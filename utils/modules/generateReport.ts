
import { join } from "jsr:@std/path@^0.225.1";
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import fg from "npm:fast-glob@3.3.2";
import { ensureDirSync, emptyDirSync, copySync } from "jsr:@std/fs@1.0.9";
import { capitalCase } from "jsr:@mesqueeb/case-anything";
import cloneJSON from "jsr:@rhy/fast-json-clone";
import { formatDuration, formatNumber } from "../utils.ts";
import { BenchmarkResultDB, GlobalResultTime, ToolKeyType, SummaryType, DetailedResultType, SummaryTableElement, SummaryTable, projectCategory, ProjectCategoryType, RuleSummaryData, ResultAggregation, ResultData, ResultAggregationByProjectCategory, SUMMARY_TABLE_KEYS, toolKey, SummaryTableKeys } from "../types.ts";
import { PROJECT_ROOT as defaultProjectRoot } from "../../config.ts";
import { OutputFormat, OutputFormatType, TableAlignType, TableCell } from "../formatters/formatters-interface.ts";
import { DocumentExporter } from "../formatters/exporter.ts";

const GLOBAL_EXECUTION_KEY = "GLOBAL";
const codingRules = JSON.parse(
    Deno.readTextFileSync(
        join(defaultProjectRoot, "utils/cogralys-cli/rules/types/allRules.json")
    )
).map((elt: [string, any]) => (elt[0].toLocaleLowerCase()));

function toTitleCase(value: string) {
    return capitalCase(value.replaceAll("_", " "));
}

function determineRuleName(benchmarkFile: string): string {
    const r = benchmarkFile.match(/benchmarkResults-([^-.]+).*?\.json$/)?.[1];
    if (!r || !codingRules.includes(r)) {
        return GLOBAL_EXECUTION_KEY;
    }
    return r;
}

interface TimeBaseData {
    user_time: number;
    system_time: number;
    cpu_percent: number;
    elapsed_time: number;
    average_shared_text_size: number;
    average_unshared_data_size: number;
    average_stack_size: number;
    average_total_size: number;
    maximum_resident_set_size: number;
    average_resident_set_size: number;
    major_pagefaults: number;
    minor_pagefaults: number;
    voluntary_context_switches: number;
    involuntary_context_switches: number;
    swaps: number;
    block_input_operations: number;
    block_output_operations: number;
    messages_sent: number;

    messages_received: number,
    signals_delivered: number;
    page_size: number;
    exit_status: number;
}

function getNotNullNumber(value: number, defaultValue: number, allowZeroValue = true): number {
    return isNaN(value) ? defaultValue : !allowZeroValue && value === 0 ? defaultValue : value;
}

function emptyTimeData(): TimeBaseData {
    return {
        user_time: 0,
        system_time: 0,
        cpu_percent: 0,
        elapsed_time: 0,
        average_shared_text_size: 0,
        average_unshared_data_size: 0,
        average_stack_size: 0,
        average_total_size: 0,
        maximum_resident_set_size: 0,
        average_resident_set_size: 0,
        major_pagefaults: 0,
        minor_pagefaults: 0,
        voluntary_context_switches: 0,
        involuntary_context_switches: 0,
        swaps: 0,
        block_input_operations: 0,
        block_output_operations: 0,
        messages_sent: 0,
        messages_received: 0,
        signals_delivered: 0,
        page_size: 0,
        exit_status: 0,
    };
}

function processResultsByLocRange(results: BenchmarkResultDB[]): {
    [key in ProjectCategoryType]: BenchmarkResultDB[];
} {
    // Categorize projects based on Lines of Code
    return {
        all: results,
        small: results.filter(r => r.scc.Code <= 10_000),
        medium: results.filter(r => r.scc.Code > 10_000 && r.scc.Code <= 30_000),
        large: results.filter(r => r.scc.Code > 30_000)
    };
}

// Result processor class
class ResultProcessor {
    constructor() {
    }

    processResultsWithLocCategories(benchmarkFile: string, ruleName: string): {
        [key in ProjectCategoryType]: {
            table: SummaryTable;
            nbProjects: number;
            totalLoC: number;
            projects: DetailedResultType[];
        }
    } {
        const results: BenchmarkResultDB[] = JSON.parse(Deno.readTextFileSync(benchmarkFile));
        const categorizedResults = processResultsByLocRange(results);

        const processCategory = (categoryResults: BenchmarkResultDB[]): {
            table: SummaryTable;
            nbProjects: number;
            totalLoC: number;
            projects: DetailedResultType[];
        } => {
            const listOfLoC: number[] = [];
            const projects: DetailedResultType[] = [];
            const summary = this.initializeSummary();
            const totalLoC = this.aggregateData(categoryResults, summary, listOfLoC, ruleName, projects);
            return {
                table: this.createResultTable(summary),
                nbProjects: categoryResults.length,
                totalLoC,
                projects: projects.sort((a, b) => a.scc.nbLoC - b.scc.nbLoC)
            };
        };

        return {
            all: processCategory(categorizedResults.all),
            small: processCategory(categorizedResults.small),
            medium: processCategory(categorizedResults.medium),
            large: processCategory(categorizedResults.large)
        };
    }

    private initializeSummary(): SummaryType {
        return {
            adactl: this.createEmptyToolSummary(),
            gnatcheck_1cores: this.createEmptyToolSummary(),
            gnatcheck_32cores: this.createEmptyToolSummary(),
            cogralys: this.createEmptyToolSummary()
        };
    }

    private createEmptyToolSummary(): GlobalResultTime {
        return {
            overheadParsing: 0,
            overheadPopulating: 0,
            analysisTime: 0,
            timeData: emptyTimeData(),
            overheadTimeData: emptyTimeData(),
            executionTime: 0,
            nbFails: 0,
            nbProjectFails: 0,
            analysisTimeValues: [],
            issuedMessage: 0
        };
    }

    private aggregateData(
        results: BenchmarkResultDB[],
        summary: SummaryType,
        listOfLoC: number[],
        ruleName: string,
        projects: DetailedResultType[]
    ): number {
        let totalLoC = 0;

        for (const result of results) {
            totalLoC += result.scc.Code;
            listOfLoC.push(result.scc.Code);
            projects.push({
                crateName: result.crateName,
                workDir: result.workDir,
                gprPath: result.gprPath,
                scc: {
                    nbLoC: result.scc.Code,
                    complexity: result.scc.Complexity,
                    nbFiles: result.scc.Count
                },
                results: {
                    adactl: { ...result.benchmarkResults.adactl.digestTime, issuedMessages: result.benchmarkResults.adactl.run.issuedMessages },
                    cogralys: { ...result.benchmarkResults.cogralys.digestTime, issuedMessages: ruleName === GLOBAL_EXECUTION_KEY ? result.benchmarkResults.cogralys.run.issuedMessages : result.benchmarkResults.cogralys.ruleResults[ruleName].issuedMessages },
                    gnatcheck_1cores: { ...result.benchmarkResults.gnatcheck_1cores.digestTime, issuedMessages: result.benchmarkResults.gnatcheck_1cores.run.issuedMessages },
                    gnatcheck_32cores: { ...result.benchmarkResults.gnatcheck_32cores.digestTime, issuedMessages: result.benchmarkResults.gnatcheck_32cores.run.issuedMessages }
                }
            });

            this.aggregateToolData(result, summary, ruleName);
        }

        return totalLoC;
    }

    private aggregateToolData(
        result: BenchmarkResultDB,
        summary: SummaryType,
        ruleName: string
    ): void {
        let nbFails: number = 0;

        for (const tool in summary) {
            summary[tool as ToolKeyType].overheadParsing += result.benchmarkResults[tool as ToolKeyType].digestTime.overheadParsing;
            summary[tool as ToolKeyType].overheadPopulating += result.benchmarkResults[tool as ToolKeyType].digestTime.overheadPopulating;
            let analysisTime = result.benchmarkResults[tool as ToolKeyType].digestTime.analysisTime;
            let executionTime = result.benchmarkResults[tool as ToolKeyType].digestTime.executionTime;
            let issuedMessage = result.benchmarkResults[tool as ToolKeyType].run.issuedMessages.maxCount;
            nbFails = result.benchmarkResults[tool as ToolKeyType].run.nbRuns - result.benchmarkResults[tool as ToolKeyType].run.nbValidRuns;
            summary[tool as ToolKeyType].nbFails += nbFails;
            summary[tool as ToolKeyType].nbProjectFails += nbFails > 0 ? 1 : 0;

            if (tool === "cogralys") {
                if (ruleName === GLOBAL_EXECUTION_KEY) {
                    executionTime = result.benchmarkResults.cogralys.digestTime.executionTime
                        + result.benchmarkResults.cogralys.digestTime.overheadParsing
                        + result.benchmarkResults.cogralys.digestTime.overheadPopulating;
                } else {
                    analysisTime = result.benchmarkResults.cogralys.ruleResults[ruleName].digestTime.analysisTime;
                    executionTime = result.benchmarkResults.cogralys.ruleResults[ruleName].digestTime.executionTime
                        + result.benchmarkResults.cogralys.digestTime.overheadParsing
                        + result.benchmarkResults.cogralys.digestTime.overheadPopulating;
                    issuedMessage = result.benchmarkResults.cogralys.ruleResults[ruleName].issuedMessages.maxCount;
                }
            }

            summary[tool as ToolKeyType].analysisTime += analysisTime;
            summary[tool as ToolKeyType].analysisTimeValues.push(analysisTime);
            summary[tool as ToolKeyType].executionTime += executionTime;
            summary[tool as ToolKeyType].issuedMessage += issuedMessage;
        }
    }

    private createResultTable(summary: SummaryType): SummaryTable {
        // Calculate percentages and prepare result table
        const fastestAnalysisTime = Math.min(
            getNotNullNumber(summary.adactl.analysisTime, Infinity, false),
            getNotNullNumber(summary.gnatcheck_1cores.analysisTime, Infinity, false),
            getNotNullNumber(summary.gnatcheck_32cores.analysisTime, Infinity, false),
            getNotNullNumber(summary.cogralys.analysisTime, Infinity, false)
        );
        const fastestExecutionTime = Math.min(
            getNotNullNumber(summary.adactl.executionTime, Infinity, false),
            getNotNullNumber(summary.gnatcheck_1cores.executionTime, Infinity, false),
            getNotNullNumber(summary.gnatcheck_32cores.executionTime, Infinity, false),
            getNotNullNumber(summary.cogralys.executionTime, Infinity, false)
        );

        const fastestOverhead = Math.min(
            getNotNullNumber(summary.adactl.overheadParsing, Infinity, false) + getNotNullNumber(summary.adactl.overheadPopulating, Infinity),
            getNotNullNumber(summary.gnatcheck_1cores.overheadParsing, Infinity, false) + getNotNullNumber(summary.gnatcheck_1cores.overheadPopulating, Infinity),
            getNotNullNumber(summary.gnatcheck_32cores.overheadParsing, Infinity, false) + getNotNullNumber(summary.gnatcheck_32cores.overheadPopulating, Infinity),
            getNotNullNumber(summary.cogralys.overheadParsing, Infinity, false) + getNotNullNumber(summary.cogralys.overheadPopulating, Infinity)
        );

        const generateEmptyToolsValues = (): SummaryTableElement => {
            const result: SummaryTableElement = {
                adactl: "",
                cogralys: "",
                gnatcheck_1cores: "",
                gnatcheck_32cores: ""
            };
            return result;
        }

        // Generate result
        const result: SummaryTable = {
            overheadParsing: generateEmptyToolsValues(),
            overheadPopulating: generateEmptyToolsValues(),
            "Relative Overhead (0 is better)": generateEmptyToolsValues(),
            analysisTime: generateEmptyToolsValues(),
            "Analysis Relative Speed (0 is better)": generateEmptyToolsValues(),
            executionTime: generateEmptyToolsValues(),
            "Execution Relative Speed (0 is better)": generateEmptyToolsValues(),
            "Nb run fails": generateEmptyToolsValues(),
            "Nb project fails": generateEmptyToolsValues(),
            "Issued Messages": generateEmptyToolsValues(),
        }

        for (const tool in summary) {
            result.overheadParsing[tool as ToolKeyType] = formatDuration(Math.floor(summary[tool as ToolKeyType].overheadParsing * 1000));
            result.overheadPopulating[tool as ToolKeyType] = formatDuration(Math.floor(summary[tool as ToolKeyType].overheadPopulating * 1000));
            result.analysisTime[tool as ToolKeyType] = formatDuration(Math.floor(summary[tool as ToolKeyType].analysisTime * 1000));
            result.executionTime[tool as ToolKeyType] = formatDuration(Math.floor(summary[tool as ToolKeyType].executionTime * 1000));
            result["Nb run fails"][tool as ToolKeyType] = summary[tool as ToolKeyType].nbFails;
            result["Nb project fails"][tool as ToolKeyType] = summary[tool as ToolKeyType].nbProjectFails;
            result["Issued Messages"][tool as ToolKeyType] = summary[tool as ToolKeyType].issuedMessage;


            if (summary[tool as ToolKeyType].analysisTime === 0) {
                result["Analysis Relative Speed (0 is better)"][tool as ToolKeyType] = ""
            } else {
                result["Analysis Relative Speed (0 is better)"][tool as ToolKeyType] = (((summary[tool as ToolKeyType].analysisTime - fastestAnalysisTime) / fastestAnalysisTime)).toLocaleString(undefined, { style: 'percent', minimumFractionDigits: 2 });
            }

            if (summary[tool as ToolKeyType].executionTime === 0) {
                result["Execution Relative Speed (0 is better)"][tool as ToolKeyType] = ""
            } else {
                result["Execution Relative Speed (0 is better)"][tool as ToolKeyType] = (((summary[tool as ToolKeyType].executionTime - fastestExecutionTime) / fastestExecutionTime)).toLocaleString(undefined, { style: 'percent', minimumFractionDigits: 2 });
            }

            if (summary[tool as ToolKeyType].overheadParsing === 0) {
                result["Relative Overhead (0 is better)"][tool as ToolKeyType] = ""
            } else {
                result["Relative Overhead (0 is better)"][tool as ToolKeyType] = ((((summary[tool as ToolKeyType].overheadParsing + summary[tool as ToolKeyType].overheadPopulating) - fastestOverhead) / fastestOverhead) || 0).toLocaleString(undefined, { style: 'percent', minimumFractionDigits: 2 });
            }
        }
        return result;
    }
}

// Main module initialization
export function initializeModule(program: Command): void {
    program
        .command("generate-report")
        .description(
            "Generate benchmark report. This script shall be called after benchmarking GNATcheck, AdaControl, and Cogralys, and after aggregating results."
        )
        .option(
            "--rootDir <string>",
            "Path to the root directory of the result files",
            defaultProjectRoot
        )
        .option(
            "-o, --output <string>",
            `Output format (Possible values: ${OutputFormat.join("|")})`,
            "cli"
        )
        .action(handleComputeResults);
}

function generateRuleSummary(resultData: ResultData, propertyKey = "analysisTime"): RuleSummaryData {
    const summary: RuleSummaryData = {
        all: {},
        small: {},
        medium: {},
        large: {}
    };

    // Initialize summary with all rules
    for (const r of Object.keys(resultData.rules).sort((a, b) => a.localeCompare(b))) {
        const ruleName = toTitleCase(r);
        for (const key of projectCategory) {
            summary[key][ruleName] = {};
        }
    }

    // Fill in the analysis times for each rule and tool
    for (const [r, ruleData] of Object.entries(resultData.rules)) {
        const ruleName = toTitleCase(r);
        for (const key of projectCategory) {
            for (const [property, byToolData] of Object.entries(ruleData[key].table)) {
                if (property === propertyKey) {
                    summary[key][ruleName] = cloneJSON(byToolData) as { [k in ToolKeyType]: string };
                    break;
                }
            }
            summary[key][ruleName]["Number of line of codes"] = formatNumber(ruleData[key].totalLoC);
            summary[key][ruleName]["Number of projects"] = formatNumber(ruleData[key].nbProjects);
        }
    }

    return summary;
}

// Command handler
function handleComputeResults(options: { rootDir: string, output: OutputFormatType }): void {
    const resultProcessor = new ResultProcessor();
    const benchmarkFiles = fg.sync(join(options.rootDir, "benchmarkResults*.json"));

    const resultData: ResultData = {
        global: {
            all: {
                table: {
                  overheadParsing: {},
                  overheadPopulating: {},
                  "Relative Overhead (0 is better)": {},
                  analysisTime: {},
                  "Analysis Relative Speed (0 is better)": {},
                  executionTime: {},
                  "Execution Relative Speed (0 is better)": {},
                  "Nb run fails": {},
                  "Nb project fails": {},
                  "Issued Messages": {}
                },
                nbProjects: 0,
                totalLoC: 0,
                projects: []
            },
            small: {
                table: {
                  overheadParsing: {},
                  overheadPopulating: {},
                  "Relative Overhead (0 is better)": {},
                  analysisTime: {},
                  "Analysis Relative Speed (0 is better)": {},
                  executionTime: {},
                  "Execution Relative Speed (0 is better)": {},
                  "Nb run fails": {},
                  "Nb project fails": {},
                  "Issued Messages": {}
                },
                nbProjects: 0,
                totalLoC: 0,
                projects: []
            },
            medium: {
                table: {
                  overheadParsing: {},
                  overheadPopulating: {},
                  "Relative Overhead (0 is better)": {},
                  analysisTime: {},
                  "Analysis Relative Speed (0 is better)": {},
                  executionTime: {},
                  "Execution Relative Speed (0 is better)": {},
                  "Nb run fails": {},
                  "Nb project fails": {},
                  "Issued Messages": {}
                },
                nbProjects: 0,
                totalLoC: 0,
                projects: []
            },
            large: {
                table: {
                  overheadParsing: {},
                  overheadPopulating: {},
                  "Relative Overhead (0 is better)": {},
                  analysisTime: {},
                  "Analysis Relative Speed (0 is better)": {},
                  executionTime: {},
                  "Execution Relative Speed (0 is better)": {},
                  "Nb run fails": {},
                  "Nb project fails": {},
                  "Issued Messages": {}
                },
                nbProjects: 0,
                totalLoC: 0,
                projects: []
            }
        },
        rules: {} as Record<string, ResultAggregation>,
        summary: {
          analysisTime: {
            all: {},
            small: {},
            medium: {},
            large: {}
          },
          overheadParsing: {
            all: {},
            small: {},
            medium: {},
            large: {}
          },
          issuedMessage: {
            all: {},
            small: {},
            medium: {},
            large: {}
          }
        }
    };

    let nbRuns = 0;

    for (const benchmarkFile of benchmarkFiles) {
        const ruleName = determineRuleName(benchmarkFile);
        const result = resultProcessor.processResultsWithLocCategories(benchmarkFile, ruleName);

        if (ruleName === GLOBAL_EXECUTION_KEY) {
            resultData.global = result;
        } else {
            resultData.rules[ruleName] = result;
            result.all.table
        }

        if (nbRuns === 0) {
            nbRuns = JSON.parse(Deno.readTextFileSync(benchmarkFile))[0]
                ?.benchmarkResults?.adactl?.run?.nbRuns || 0;
        }
    }

    const analysisTime = generateRuleSummary(resultData);
    const overheadParsing = generateRuleSummary(resultData, "overheadParsing");
    const issuedMessage = generateRuleSummary(resultData, "Issued Messages");
    resultData.summary = {
        analysisTime,
        overheadParsing,
        issuedMessage
    };

    generateReports(nbRuns, resultData, options.output, options.rootDir);
}

function generateReports(nbRuns: number, resultData: ResultData, outputFormat: OutputFormatType, rootDir: string): void {
    let resultsDir = join(rootDir, "results");
    let result = "";
    let ext = "";
    const exporter = new DocumentExporter(outputFormat);

    const generateOutput = () => {
        const output: string[] = [];

        const formatTable = (data: SummaryTable | RuleSummaryData[ProjectCategoryType], title: string): string => {
            const isSummaryTable = (data: any): data is SummaryTable => {
                return 'overheadParsing' in data;
            };
            const fistLevelKeys: (SummaryTableKeys | string)[] = Object.keys(data);
            const secondLevelKeys = Object.keys(data[fistLevelKeys[0] as keyof typeof data]);
            const headers: TableCell[] = [
                { name: "Metric", key: "metric" },
                ...secondLevelKeys.map(elt => ({ name: toTitleCase((elt as string).replace("_", " ")), key: elt as string, align: "left" as TableAlignType }))
            ];

            type TableElt = Record<'metric' | ToolKeyType | string, string | number>;

            const rows: TableElt[] = [];
            for (const header of fistLevelKeys) {
                const row: TableElt = {
                  adactl: "",
                  cogralys: "",
                  gnatcheck_1cores: "",
                  gnatcheck_32cores: "",
                  metric: header
                };
                for (const tool of secondLevelKeys) {
                    const value = isSummaryTable(data)
                    ? data[header as keyof SummaryTable][tool]
                    : data[header][tool];

                    if (!value) {
                        row[tool] = "";
                    }
                    row[tool] = value as string | number;
                    if (typeof row[tool] === "number") {
                        row[tool] = exporter.formatNumber(row[tool]);
                    }
                }
                rows.push(row);
            }

            return exporter.formatTable(headers, rows, title);
        };

        // Format category data with appropriate sectioning
        const formatCategory = (
            category: ResultAggregationByProjectCategory | SummaryTable | RuleSummaryData[ProjectCategoryType],
            rule: string,
            categoryName: string,
            sectionLevel = 1
        ) => {
            const isSummaryTable = (obj: any): obj is ResultAggregationByProjectCategory => {
                return 'table' in obj &&
                       'nbProjects' in obj &&
                       'totalLoC' in obj;
            };
            if (categoryName && categoryName.length) {
                output.push(exporter.addTitle(categoryName, sectionLevel));
            }
            const tableTitle = `${toTitleCase(rule)}: ${categoryName}`
            if (isSummaryTable(category)) {
                output.push(formatTable(category.table, tableTitle));
                output.push(exporter.bold("Number of projects:") + " " + exporter.formatNumber(category.nbProjects) + "\n");
                output.push(exporter.bold("Total lines of code:") + " " + exporter.formatNumber(category.totalLoC) + "\n");
            } else {
                output.push(formatTable(category, tableTitle));
            }
            output.push("");
        };

        output.push(exporter.documentHeader("Benchmark Results"));

        // Format global results
        output.push(exporter.addTitle("Global Results"));
        formatCategory(resultData.global.all, "Global", 'All Projects', 2);
        formatCategory(resultData.global.small, "Global", 'Small Projects (0-10k LoC)', 2);
        formatCategory(resultData.global.medium, "Global", 'Medium Projects (10-30k LoC)', 2);
        formatCategory(resultData.global.large, "Global", 'Large Projects (30k+ LoC)', 2);

        // Format rule-specific results
        output.push(exporter.addTitle("Results by Rules"));
        output.push(exporter.addTitle("Summary", 2));
        output.push(exporter.addTitle("Analysis Time", 3));

        formatCategory(resultData.summary.analysisTime.all, "Analysis Time", 'All Projects', 4);
        formatCategory(resultData.summary.analysisTime.small, "Analysis Time", 'Small Projects (0-10k LoC)', 4);
        formatCategory(resultData.summary.analysisTime.medium, "Analysis Time", 'Medium Projects (10-30k LoC)', 4);
        formatCategory(resultData.summary.analysisTime.large, "Analysis Time", 'Large Projects (30k+ LoC)', 4);

        // Format parsing overhead results
        output.push(exporter.addTitle("Parsing Overhead", 3));
        formatCategory(resultData.summary.overheadParsing.all, "Parsing Overhead", 'All Projects', 4);
        formatCategory(resultData.summary.overheadParsing.small, "Parsing Overhead", 'Small Projects (0-10k LoC)', 4);
        formatCategory(resultData.summary.overheadParsing.medium, "Parsing Overhead", 'Medium Projects (10-30k LoC)', 4);
        formatCategory(resultData.summary.overheadParsing.large, "Parsing Overhead", 'Large Projects (30k+ LoC)', 4);

        // Format issued message results
        output.push(exporter.addTitle("Issued Messages", 3));
        formatCategory(resultData.summary.issuedMessage.all, "Issued Messages", 'All Projects', 4);
        formatCategory(resultData.summary.issuedMessage.small, "Issued Messages", 'Small Projects (0-10k LoC)', 4);
        formatCategory(resultData.summary.issuedMessage.medium, "Issued Messages", 'Medium Projects (10-30k LoC)', 4);
        formatCategory(resultData.summary.issuedMessage.large, "Issued Messages", 'Large Projects (30k+ LoC)', 4);

        // Format individual rule results
        for (const [ruleName, ruleData] of Object.entries(resultData.rules).sort((a, b) => a[0].localeCompare(b[0]))) {
            output.push(exporter.addTitle(toTitleCase(ruleName), 2));
            formatCategory(ruleData.all, ruleName, 'All Projects', 3);
            formatCategory(ruleData.small, ruleName, 'Small Projects (0-10k LoC)', 3);
            formatCategory(ruleData.medium, ruleName, 'Medium Projects (10-30k LoC)', 3);
            formatCategory(ruleData.large, ruleName, 'Large Projects (30k+ LoC)', 3);
        }

        output.push(exporter.documentFooter());

        return output.join("\n");
    }

    switch (outputFormat) {
        case "cli":
            console.log(generateOutput());
            break;
        case "markdown":
        case "md":
            resultsDir = join(resultsDir, "markdown");
            result = generateOutput();
            ext = "md";
            break;
        case "typst":
            resultsDir = join(resultsDir, "typst");
            result = generateOutput();
            ext = "typ";
            break;
        case "latex":
        case "tex":
            resultsDir = join(resultsDir, "latex");
            result = generateOutput();
            ext = "tex";
            break;
        default:
            console.log(generateOutput());
            break;
    }

    ensureDirSync(resultsDir);
    emptyDirSync(resultsDir);
    const jsonOutputPath = join(rootDir, "results", "result.json");
    Deno.writeTextFileSync(jsonOutputPath, JSON.stringify(resultData, null, 2));
    console.log("The resulting data is stored in: ", jsonOutputPath);


    if (result.length) {
        if (outputFormat === "typst") {
            copySync(join(defaultProjectRoot, "utils/report/typst"), join(resultsDir, "/"), { overwrite: true });
        }

        const reportPath = join(resultsDir, "report." + ext);
        Deno.writeTextFileSync(reportPath, result);
        console.log("Report generated here: ", reportPath);

    }
}
