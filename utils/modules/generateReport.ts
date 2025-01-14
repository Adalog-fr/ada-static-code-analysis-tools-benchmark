import { join } from "jsr:@std/path@^0.225.1";
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import fg from "npm:fast-glob@3.3.2";
import { LanguageSummary } from "../scc-types.ts";
import { formatDuration } from "../utils.ts";
import { PROJECT_ROOT as defaultProjectRoot } from "../../config.ts";
import { ensureDirSync, emptyDirSync, copySync } from "jsr:@std/fs@1.0.9";
import { capitalCase } from "jsr:@mesqueeb/case-anything";
import cloneJSON from "jsr:@rhy/fast-json-clone";

const GLOBAL_EXECUTION_KEY = "GLOBAL";
const codingRules = JSON.parse(
    Deno.readTextFileSync(
        join(defaultProjectRoot, "utils/cogralys-cli/rules/types/allRules.json")
    )
).map((elt: [string, any]) => (elt[0].toLowerCase()));

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

function formatNumber(value: number, maxDigits = 0) {
    return new Intl.NumberFormat('en-GB', { maximumFractionDigits: maxDigits }).format(value)
}

// Core interfaces and types
interface StandardDeviationResult {
    value: number;
    percentage: number;
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

interface DigestTimeResult {
    overheadParsing: number;
    overheadPopulating: number;
    overheadThreshold: number;
    overhead: number;
    executionTime: number;
    analysisTime: number;
}

// Benchmark specific types
interface GlobalResultTime {
    overheadParsing: number;
    overheadPopulating: number;
    analysisTime: number;
    executionTime: number;
    timeData: TimeBaseData;
    overheadTimeData: TimeBaseData;
    nbFails: number;
    nbProjectFails: number;
    analysisTimeValues: number[];
    r2Value: number;
    mean: number;
    standardDeviation: StandardDeviationResult;
}

type ToolKey = "adactl" | "cogralys" | "gnatcheck_1cores" | "gnatcheck_32cores";
type SummaryType = Record<ToolKey, GlobalResultTime>;

interface BenchmarkResultDB {
    crateName: string;
    workDir: string;
    gprPath: string;
    benchmarkResults: {
        adactl: {
            overhead: { parsing: any };
            run: any;
            digestTime: DigestTimeResult;
        };
        gnatcheck_1cores: {
            overhead: { parsing: any };
            run: any;
            digestTime: DigestTimeResult;
        };
        gnatcheck_32cores: {
            overhead: { parsing: any };
            run: any;
            digestTime: DigestTimeResult;
        };
        cogralys: {
            overhead: {
                parsing: any;
                populatingDB: any;
            };
            run: any;
            ruleResults: { [key: string]: any };
            digestTime: DigestTimeResult;
        };
    };
    scc: Omit<LanguageSummary, 'Files'>;
}

const ProjectCategory = ["all", "small", "medium", "large"] as const;
type ProjectCategoryType = typeof ProjectCategory[number];

type RuleSummaryData = {
    [size in ProjectCategoryType]: {
        [rule: string]: {
            [tool in ToolKey | string]?: string|number;
        }
    }
};

// Statistical utility functions
function calculateStandardDeviation(values: number[]): StandardDeviationResult {
    const mean = values.reduce((a, b) => a + b, 0) / values.length;
    const squaredDifferences = values.reduce((sum, value) => sum + Math.pow(value - mean, 2), 0);
    const variance = squaredDifferences / values.length;
    const stdDev = Math.sqrt(variance);
    const stdDevPercent = mean !== 0 ? (stdDev / mean) * 100 : 0;

    return { value: stdDev, percentage: stdDevPercent };
}

function calculateR2(x: number[], y: number[]): number {
    const n = x.length;
    const sumX = x.reduce((a, b) => a + b, 0);
    const sumY = y.reduce((a, b) => a + b, 0);
    const sumXY = x.reduce((sum, xi, i) => sum + xi * y[i], 0);
    const sumX2 = x.reduce((sum, xi) => sum + xi * xi, 0);
    const sumY2 = y.reduce((sum, yi) => sum + yi * yi, 0);

    const numerator = n * sumXY - sumX * sumY;
    const denominator = Math.sqrt((n * sumX2 - sumX * sumX) * (n * sumY2 - sumY * sumY));

    return Math.pow(numerator / denominator, 2);
}

// Metric utility functions
function getMetrics(analysisTimeValues: number[], listOfLoC: number[]) {
    return {
        r2Value: calculateR2(listOfLoC, analysisTimeValues),
        mean: analysisTimeValues.reduce((a, b) => a + b, 0) / analysisTimeValues.length,
        standardDeviation: calculateStandardDeviation(analysisTimeValues)
    };
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
        small: results.filter(r => r.scc.Code <= 10000),
        medium: results.filter(r => r.scc.Code > 10000 && r.scc.Code <= 30000),
        large: results.filter(r => r.scc.Code > 30000)
    };
}

// Result processor class
class ResultProcessor {
    constructor() {
    }

    processResultsWithLocCategories(benchmarkFile: string): {
        [key in ProjectCategoryType]: {
            table: any;
            nbProjects: number;
            totalLoC: number;
        }
    } {
        const results: BenchmarkResultDB[] = JSON.parse(Deno.readTextFileSync(benchmarkFile));
        const ruleName = determineRuleName(benchmarkFile);
        const categorizedResults = processResultsByLocRange(results);

        const processCategory = (categoryResults: BenchmarkResultDB[]) => {
            const listOfLoC: number[] = [];
            const summary = this.initializeSummary();
            const totalLoC = this.aggregateData(categoryResults, summary, listOfLoC, ruleName);
            this.calculateMetrics(summary, listOfLoC);
            return {
                table: this.createResultTable(summary),
                nbProjects: categoryResults.length,
                totalLoC
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
            r2Value: 0,
            mean: 0,
            standardDeviation: { value: 0, percentage: 0 }
        };
    }

    private aggregateData(
        results: BenchmarkResultDB[],
        summary: SummaryType,
        listOfLoC: number[],
        ruleName: string
    ): number {
        let totalLoC = 0;

        for (const result of results) {
            totalLoC += result.scc.Code;
            listOfLoC.push(result.scc.Code);

            this.aggregateToolData(result, summary, ruleName);
        }

        return totalLoC;
    }

    private aggregateToolData(
        result: BenchmarkResultDB,
        summary: SummaryType,
        ruleName: string
    ): void {
        // Implementation details for data aggregation
        // This would contain the specific logic for each tool
        let nbFails: number = 0;

        for (const tool in summary) {
            summary[tool as ToolKey].overheadParsing += result.benchmarkResults[tool as ToolKey].digestTime.overheadParsing;
            summary[tool as ToolKey].overheadPopulating += result.benchmarkResults[tool as ToolKey].digestTime.overheadPopulating;
            let analysisTime = result.benchmarkResults[tool as ToolKey].digestTime.analysisTime;
            let executionTime = result.benchmarkResults[tool as ToolKey].digestTime.executionTime;
            nbFails = result.benchmarkResults[tool as ToolKey].run.nbRuns - result.benchmarkResults[tool as ToolKey].run.nbValidRuns;
            summary[tool as ToolKey].nbFails += nbFails;
            summary[tool as ToolKey].nbProjectFails += nbFails > 0 ? 1 : 0;

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
                }
            }

            summary[tool as ToolKey].analysisTime += analysisTime;
            summary[tool as ToolKey].analysisTimeValues.push(analysisTime);
            summary[tool as ToolKey].executionTime += executionTime;
        }
    }

    private calculateMetrics(summary: SummaryType, listOfLoC: number[]): void {
        for (const tool in summary) {
            summary[tool as ToolKey] = {
                ...summary[tool as ToolKey],
                ...getMetrics(summary[tool as ToolKey].analysisTimeValues, listOfLoC)
            };
        }
    }

    private createResultTable(summary: SummaryType): any {
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

        const generateEmptyToolsValues = () => {
            const result: Record<ToolKey, any> = {
              adactl: undefined,
              cogralys: undefined,
              gnatcheck_1cores: undefined,
              gnatcheck_32cores: undefined
            };
            return result;
        }

        // Generate result
        const result = {
            overheadParsing: generateEmptyToolsValues(),
            overheadPopulating: generateEmptyToolsValues(),
            "Relative Overhead (0 is better)": generateEmptyToolsValues(),
            analysisTime: generateEmptyToolsValues(),
            "Analysis Relative Speed (0 is better)": generateEmptyToolsValues(),
            "R²": generateEmptyToolsValues(),
            "mean": generateEmptyToolsValues(),
            "Standard Deviation value": generateEmptyToolsValues(),
            "Standard Deviation in %": generateEmptyToolsValues(),
            executionTime: generateEmptyToolsValues(),
            "Execution Relative Speed (0 is better)": generateEmptyToolsValues(),
            "Nb run fails": generateEmptyToolsValues(),
            "Nb project fails": generateEmptyToolsValues(),
        }

        for (const tool in summary) {
            result.overheadParsing[tool as ToolKey] = formatDuration(Math.floor(summary[tool as ToolKey].overheadParsing * 1000));
            result.overheadPopulating[tool as ToolKey] = formatDuration(Math.floor(summary[tool as ToolKey].overheadPopulating * 1000));
            result.analysisTime[tool as ToolKey] = formatDuration(Math.floor(summary[tool as ToolKey].analysisTime * 1000));
            result["R²"][tool as ToolKey] = summary[tool as ToolKey].r2Value.toFixed(3);
            result.mean[tool as ToolKey] = summary[tool as ToolKey].mean.toFixed(3);
            result["Standard Deviation value"][tool as ToolKey] = summary[tool as ToolKey].standardDeviation.value.toFixed(3);
            result["Standard Deviation in %"][tool as ToolKey] = summary[tool as ToolKey].standardDeviation.percentage.toFixed(3) + "%";
            result.executionTime[tool as ToolKey] = formatDuration(Math.floor(summary[tool as ToolKey].executionTime * 1000));
            result["Nb run fails"][tool as ToolKey] = summary[tool as ToolKey].nbFails;
            result["Nb project fails"][tool as ToolKey] = summary[tool as ToolKey].nbProjectFails;


            if (summary[tool as ToolKey].analysisTime === 0) {
                result["Analysis Relative Speed (0 is better)"][tool as ToolKey] = ""
            } else {
                result["Analysis Relative Speed (0 is better)"][tool as ToolKey] = (((summary[tool as ToolKey].analysisTime - fastestAnalysisTime) / fastestAnalysisTime)).toLocaleString(undefined, { style: 'percent', minimumFractionDigits: 2 });
            }

            if (summary[tool as ToolKey].executionTime === 0) {
                result["Execution Relative Speed (0 is better)"][tool as ToolKey] = ""
            } else {
                result["Execution Relative Speed (0 is better)"][tool as ToolKey] = (((summary[tool as ToolKey].executionTime - fastestExecutionTime) / fastestExecutionTime)).toLocaleString(undefined, { style: 'percent', minimumFractionDigits: 2 });
            }

            if (summary[tool as ToolKey].overheadParsing === 0) {
                result["Relative Overhead (0 is better)"][tool as ToolKey] = ""
            } else {
                result["Relative Overhead (0 is better)"][tool as ToolKey] = ((((summary[tool as ToolKey].overheadParsing + summary[tool as ToolKey].overheadPopulating) - fastestOverhead) / fastestOverhead) || 0).toLocaleString(undefined, { style: 'percent', minimumFractionDigits: 2 });
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
            "Generate benchmark report. This script shall be called after benchmark GNATcheck, AdaControl and Cogralys, and after aggregate-results."
        )
        .option(
            "--rootDir <string>",
            "Path to the root of the result files",
            defaultProjectRoot
        )
        .option(
            "-o, --output <string>",
            `Output fromat (Possible values: ${OutputFormat.join("|")})`,
            "cli"
        )
        .action(handleComputeResults);
}

const OutputFormat = ['cli', 'md', 'typst', 'latex'] as const;
type OutputFormatType = typeof OutputFormat[number];

type ResultAggregation = {
    [size in ProjectCategoryType]: {
        table: {
            [rule: string]: {
                [tool in ToolKey]?: string|number;
            }
        },
        nbProjects: number,
        totalLoC: number,
    }
}
type ResultData = {
    global: ResultAggregation;
    rules: Record<string, ResultAggregation>;
    summary: {
        analysisTime: RuleSummaryData;
        overheadParsing: RuleSummaryData;
    }
};

function generateRuleSummary(resultData: ResultData, propertyKey = "analysisTime"): RuleSummaryData {
    const summary: RuleSummaryData = {
      all: {},
      small: {},
        medium: {},
        large: {}
    };

    // Initialize summary with all rules
    for (const r of Object.keys(resultData.rules).sort((a,b) => a.localeCompare(b))) {
        const ruleName = toTitleCase(r);
        for (const key of ProjectCategory) {
            summary[key][ruleName] = {};
        }
    }

    // Fill in the analysis times for each rule and tool
    for (const [r, ruleData] of Object.entries(resultData.rules)) {
        const ruleName = toTitleCase(r);
        for (const key of ProjectCategory) {
            for (const [property, byToolData] of Object.entries(ruleData[key].table)) {
                if (property === propertyKey) {
                    summary[key][ruleName] = cloneJSON(byToolData) as { [k in ToolKey]: string };
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
              table: {},
              nbProjects: 0,
              totalLoC: 0
            },
            small: {
              table: {},
              nbProjects: 0,
              totalLoC: 0
            },
            medium: {
              table: {},
              nbProjects: 0,
              totalLoC: 0
            },
            large: {
              table: {},
              nbProjects: 0,
              totalLoC: 0
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
            }
        }
    };

    let nbRuns = 0;

    for (const benchmarkFile of benchmarkFiles) {
        const result = resultProcessor.processResultsWithLocCategories(benchmarkFile);
        const ruleName = determineRuleName(benchmarkFile);

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
    resultData.summary = {
        analysisTime,
        overheadParsing
    };

    generateReports(nbRuns, resultData, options.output, options.rootDir);
}

function generateReports(nbRuns: number, resultData: ResultData, outputFormat: OutputFormatType, rootDir: string): void {
    let resultsDir = join(rootDir, "results");
    let result = "";
    let ext = "";

    switch (outputFormat) {
        case "cli":
            formatResultsCLI(nbRuns, resultData);
            break;
        case "md":
            resultsDir = join(resultsDir, "markdown");
            result = formatResultsMarkdown(nbRuns, resultData);
            ext = "md";
            break;
        case "typst":
            resultsDir = join(resultsDir, "typst");
            result = formatResultsTypst(nbRuns, resultData);
            ext = "typ";
            break;
        case "latex":
            resultsDir = join(resultsDir, "latex");
            result = formatResultsLatex(nbRuns, resultData);
            ext = "tex";
            break;
        default:
            formatResultsCLI(nbRuns, resultData);
            break;
    }

    ensureDirSync(resultsDir);
    emptyDirSync(resultsDir);
    Deno.writeTextFileSync(join(rootDir, "results", "result.json"), JSON.stringify(resultData, null, 2));

    if (result.length) {
        if (outputFormat === "typst") {
            copySync(join(defaultProjectRoot, "utils/report/typst"), join(resultsDir, "/"), { overwrite: true });
        }

        Deno.writeTextFileSync(join(resultsDir, "report." + ext), result);
    }
}

// OUTPUT FORMATTER

function formatResultsCLI(nbRuns: number, resultData: ResultData): void {
    const printCategory = (category: any, categoryName?: string, headingLevel = 3) => {
        if (categoryName && categoryName.length) {
            console.log(`\n${'#'.repeat(headingLevel)} ${categoryName}`);
        }
        if ("table" in category) {
            console.table(category.table);
            console.log("\nNumber of projects:", formatNumber(category.nbProjects));
            console.log("Total number of line of codes:", formatNumber(category.totalLoC));
        } else {
            console.table(category)
        }
    };

    console.log("=== Benchmark result ===\n");
    console.log("Number of runs: ", formatNumber(nbRuns));

    console.log("\n# Global");
    printCategory(resultData.global.all);
    console.log("\n## Result by project size");
    printCategory(resultData.global.small, "Small Projects (0-10k LoC)");
    printCategory(resultData.global.medium, "Medium Projects (10k-30k LoC)");
    printCategory(resultData.global.large, "Large Projects (30k+ LoC)");

    console.log("\n# By rules");
    console.log("\n## Summary");
    console.log("\n### Analysis Time");

    printCategory(resultData.summary.analysisTime.all);
    console.log("\n#### Result by project size");
    printCategory(resultData.summary.analysisTime.small, "Small Projects (0-10k LoC)", 5);
    printCategory(resultData.summary.analysisTime.medium, "Medium Projects (10k-30k LoC)", 5);
    printCategory(resultData.summary.analysisTime.large, "Large Projects (30k+ LoC)", 5);

    console.log("\n### Parsing Overhead");

    printCategory(resultData.summary.overheadParsing.all);
    console.log("\n#### Result by project size");
    printCategory(resultData.summary.overheadParsing.small, "Small Projects (0-10k LoC)", 5);
    printCategory(resultData.summary.overheadParsing.medium, "Medium Projects (10k-30k LoC)", 5);
    printCategory(resultData.summary.overheadParsing.large, "Large Projects (30k+ LoC)", 5);

    for (const [ruleName, ruleData] of Object.entries(resultData.rules).sort((a, b) => a[0].localeCompare(b[0]))) {
        console.log(`\n## Rule: ${toTitleCase(ruleName)}`);
        printCategory(ruleData.all);
        console.log("\n### Result by project size");
        printCategory(ruleData.small, "Small Projects (0-10k LoC)", 4);
        printCategory(ruleData.medium, "Medium Projects (10k-30k LoC)", 4);
        printCategory(ruleData.large, "Large Projects (30k+ LoC)", 4);
    }
}

function formatResultsMarkdown(nbRuns: number, resultData: ResultData): string {
    const output: string[] = [];

    const formatTable = (data: any): string => {
        const headers = Object.keys(data);
        const tools = Object.keys(data[headers[0]]);

        // Create header row
        let table = '| **Metric** | **' + tools.join('** | **') + '** |\n';
        // Create separator row
        table += '|---|' + tools.map(() => '---').join('|') + '|\n';

        // Create data rows
        for (const header of headers) {
            table += `| ${header} |`;
            for (const tool of tools) {
                table += ` ${data[header][tool]} |`;
            }
            table += '\n';
        }

        return table;
    };

    const formatCategory = (category: any, categoryName?: string, headingLevel = 2) => {
        if (categoryName && categoryName.length) {
            output.push(`\n${"#".repeat(headingLevel)} ${categoryName}\n`);
        }
        if ("table" in category) {
            output.push(formatTable(category.table));
            output.push(`\n**Number of projects:** ${formatNumber(category.nbProjects)}\n`);
            output.push(`**Total number of line of codes:** ${formatNumber(category.totalLoC)}`);
        } else {
            output.push(formatTable(category));
        }
    };

    output.push("Benchmark Results\n");
    output.push(`**Number of runs:** ${formatNumber(nbRuns)}`);

    output.push("\n# Global Results");
    formatCategory(resultData.global.all, "All Projects");
    formatCategory(resultData.global.small, "Small Projects (0-10k LoC)");
    formatCategory(resultData.global.medium, "Medium Projects (10k-30k LoC)");
    formatCategory(resultData.global.large, "Large Projects (30k+ LoC)");

    output.push("\n# Results by Rules");

    output.push("\n## Summary");
    output.push("\n### Analysis Time");

    formatCategory(resultData.summary.analysisTime.all);
    output.push("\n#### Result by project size");
    formatCategory(resultData.summary.analysisTime.small, "Small Projects (0-10k LoC)", 5);
    formatCategory(resultData.summary.analysisTime.medium, "Medium Projects (10k-30k LoC)", 5);
    formatCategory(resultData.summary.analysisTime.large, "Large Projects (30k+ LoC)", 5);

    output.push("\n### Parsing Overhead");

    formatCategory(resultData.summary.overheadParsing.all);
    output.push("\n#### Result by project size");
    formatCategory(resultData.summary.overheadParsing.small, "Small Projects (0-10k LoC)", 5);
    formatCategory(resultData.summary.overheadParsing.medium, "Medium Projects (10k-30k LoC)", 5);
    formatCategory(resultData.summary.overheadParsing.large, "Large Projects (30k+ LoC)", 5);

    for (const [ruleName, ruleData] of Object.entries(resultData.rules).sort((a, b) => a[0].localeCompare(b[0]))) {
        output.push(`\n## Rule: ${toTitleCase(ruleName)}`);
        formatCategory(ruleData.all, "All Projects", 3);
        formatCategory(ruleData.small, "Small Projects (0-10k LoC)", 3);
        formatCategory(ruleData.medium, "Medium Projects (10k-30k LoC)", 3);
        formatCategory(ruleData.large, "Large Projects (30k+ LoC)", 3);
    }

    return output.join("\n");
}

function formatResultsTypst(nbRuns: number, resultData: ResultData): string {
    const output: string[] = [];

    const formatTable = (data: any): string => {
        const headers = Object.keys(data);
        const tools = Object.keys(data[headers[0]]);

        // Start table
        let table = '#pad(x: -2cm, table(\n';
        // Define columns
        table += '  columns: (auto' + ', auto'.repeat(tools.length) + '),\n';
        // Define alignment
        table += '  align: right,\n';

        // Add header row
        table += '  table.header([*Metric*], ' + tools.map(t => '[*' + t.replaceAll("_", " ") + '*]').join(', ') + '),\n';

        // Add data rows
        for (const header of headers) {
            const rowValues = tools.map(tool => `\`${data[header][tool]}\``);
            table += `  \`${header}\`, ${rowValues.join(', ')},\n`;
        }

        table += '))\n';
        return table;
    };

    const formatCategory = (category: any, categoryName?: string, headingLevel = 2) => {
        if (categoryName && categoryName.length) {
            output.push(`\n${'='.repeat(headingLevel)} ${categoryName}\n`);
        }
        if ("table" in category) {
            output.push(formatTable(category.table));
            output.push(`\n*Number of projects*: ${formatNumber(category.nbProjects)}\n`);
            output.push(`*Total number of line of codes*: ${formatNumber(category.totalLoC)}\n`);
        } else {
            output.push(formatTable(category));
        }

    };

    output.push(`#import "./modules/lib.typ": *

#show: it => basic-report(
  doc-category: "Benchmark report",
  doc-title: "Benchmark of Ada static analysis tools",
  author: "",
  affiliation: "Université de Caen Normandie, France\nAdalog SAS, SIREN 527 695 704, France",
  logo: image("assets/adalog.jpg", width: 4cm),
  logo2: image("assets/UNICAEN_LOGO.svg", width: 5cm),
  language: "en",
  it
)\n\n`);
    // output.push("= Benchmark Results\n");
    output.push(`*Number of runs*: ${formatNumber(nbRuns)}`);

    output.push("\n= Global Results");
    formatCategory(resultData.global.all, "All Projects");
    formatCategory(resultData.global.small, "Small Projects (0-10k LoC)");
    formatCategory(resultData.global.medium, "Medium Projects (10k-30k LoC)");
    formatCategory(resultData.global.large, "Large Projects (30k+ LoC)");

    output.push("\n= Results by Rules");

    output.push("\n== Summary");
    output.push("\n=== Analysis Time");

    formatCategory(resultData.summary.analysisTime.all);
    output.push("\n==== Result by project size");
    formatCategory(resultData.summary.analysisTime.small, "Small Projects (0-10k LoC)", 5);
    formatCategory(resultData.summary.analysisTime.medium, "Medium Projects (10k-30k LoC)", 5);
    formatCategory(resultData.summary.analysisTime.large, "Large Projects (30k+ LoC)", 5);

    output.push("\n=== Parsing Overhead");

    formatCategory(resultData.summary.overheadParsing.all);
    output.push("\n==== Result by project size");
    formatCategory(resultData.summary.overheadParsing.small, "Small Projects (0-10k LoC)", 5);
    formatCategory(resultData.summary.overheadParsing.medium, "Medium Projects (10k-30k LoC)", 5);
    formatCategory(resultData.summary.overheadParsing.large, "Large Projects (30k+ LoC)", 5);

    for (const [ruleName, ruleData] of Object.entries(resultData.rules).sort((a, b) => a[0].localeCompare(b[0]))) {
        output.push(`\n== Rule: ${toTitleCase(ruleName)}`);
        formatCategory(ruleData.all, "All Projects", 3);
        formatCategory(ruleData.small, "Small Projects (0-10k LoC)", 3);
        formatCategory(ruleData.medium, "Medium Projects (10k-30k LoC)", 3);
        formatCategory(ruleData.large, "Large Projects (30k+ LoC)", 3);
    }

    return output.join("\n");
}

// Function to format tables in LaTeX using NiceTabular with consistent styling and SI units
function formatResultsLatex(nbRuns: number, resultData: ResultData): string {
    const output: string[] = [];

    // Function to format table data into LaTeX NiceTabular environment
    const formatTable = (data: any, title: string): string => {
        const headers = Object.keys(data);
        const tools = Object.keys(data[headers[0]]);

        // Initialize LaTeX table structure
        let table = '\\begin{table}[h!]\n';
        table += '    \\centering\n';
        table += '    \\renewcommand{\\arraystretch}{1.5}\n';
        table += '    \\captionsetup[table]{font={bf,sf,color=gray-600,small}, skip=0pt}\n';

        // Calculate column format based on number of tools
        const colFormat = 'l' + 'c'.repeat(tools.length);

        // Start NiceTabular environment with styling
        table += `    \\hspace*{-3.1cm}\\begin{NiceTabular}{${colFormat}}[hvlines, rounded-corners=6pt, rules/color=gray-200]\n`;
        table += '    \\CodeBefore\n';
        table += '        \\rowcolor{gray-100}{1-2}\n';
        table += '        \\rowcolors{3}{}{slate-50}\n';
        table += '    \\Body\n';

        // Add caption block
        table += `        \\Block{1-${tools.length + 1}}{\\parbox{150mm}{\\RawCaption{\\captionof{table}{${title.replaceAll("_", "\\_")}}\\label{table:${title.toLowerCase().replace(/\s+/g, '_')}}}}} \\\\\n`;

        // Format header row
        table += '        \\RowStyle[bold]{\\color{gray-600}}\n';
        table += '        \\textbf{Metric}';
        for (const tool of tools) {
            table += ` & \\textbf{\\thead{${tool.replaceAll("_", "\\_")}}}`;
        }
        table += ' \\\\\n';

        // Format data rows
        table += '        \\RowStyle[nb-rows=*,color=gray-800]{}\n';
        for (const header of headers) {
            table += `        ${header.replaceAll("_", "\\_")}`;
            for (const tool of tools) {
                const value = data[header][tool];
                // Format different types of values appropriately
                if (typeof value === 'number') {
                    table += ` & \\num{${value}}`;
                } else if (value.includes('%')) {
                    table += ` & \\qty{${parseFloat(value)}}{\\percent}`;
                } else if (value.match(/^\d+$/)) {
                    table += ` & \\num{${value}}`;
                } else {
                    table += ` & ${value}`;
                }
            }
            table += ' \\\\\n';
        }

        // Close table environments
        table += '    \\end{NiceTabular}\n';
        table += '\\end{table}\n';

        return table;
    };

    // Format category data with appropriate sectioning
    const formatCategory = (category: any, rule: string, categoryName: string, sectionLevel = 1) => {
        if (categoryName && categoryName.length) {
            let sectionHead = "";
            if (sectionLevel === 1) {
                sectionHead = "section"
            } else if (sectionLevel === 2) {
                sectionHead = "subsection"
            } else if (sectionLevel === 3) {
                sectionHead = "subsubsection"
            } else if (sectionLevel === 4) {
                sectionHead = "paragraph"
            } else if (sectionLevel === 5) {
                sectionHead = "subparagraph"
            }
            const sectionCmd = '\\' + sectionHead;
            output.push(`\n${sectionCmd}{${categoryName}}\n`);
        }
        let tableTitle = `${toTitleCase(rule)}: ${categoryName}`
        if ("table" in category) {
            output.push(formatTable(category.table, tableTitle));
            output.push(`\\textbf{Number of projects:} \\num{${category.nbProjects}}\n`);
            output.push(`\\textbf{Total lines of code:} \\num{${category.totalLoC}}\n`);
        } else {
            output.push(formatTable(category, tableTitle));
        }
    };

    // Generate LaTeX document structure
    output.push(`\\documentclass{article}
\\usepackage{tabularx,makecell,floatrow,nicematrix,booktabs,xcolor,caption,siunitx}
\\usepackage[hidelinks]{hyperref}
% Tailwind colors
\\definecolor{slate-50}{HTML}{f8fafc}
\\definecolor{slate-100}{HTML}{f1f5f9}
\\definecolor{slate-200}{HTML}{e2e8f0}
\\definecolor{slate-300}{HTML}{cbd5e1}
\\definecolor{slate-400}{HTML}{94a3b8}
\\definecolor{slate-500}{HTML}{64748b}
\\definecolor{slate-600}{HTML}{475569}
\\definecolor{slate-700}{HTML}{334155}
\\definecolor{slate-800}{HTML}{1e293b}
\\definecolor{slate-900}{HTML}{0f172a}
\\definecolor{slate-950}{HTML}{020617}
\\definecolor{gray-50}{HTML}{f9fafb}
\\definecolor{gray-100}{HTML}{f3f4f6}
\\definecolor{gray-200}{HTML}{e5e7eb}
\\definecolor{gray-300}{HTML}{d1d5db}
\\definecolor{gray-400}{HTML}{9ca3af}
\\definecolor{gray-500}{HTML}{6b7280}
\\definecolor{gray-600}{HTML}{4b5563}
\\definecolor{gray-700}{HTML}{374151}
\\definecolor{gray-800}{HTML}{1f2937}
\\definecolor{gray-900}{HTML}{111827}
\\definecolor{gray-950}{HTML}{030712}
\\begin{document}
\\begin{titlepage}
   \\vspace*{\\stretch{1.0}}
   \\begin{center}
      \\Large\\textbf{Benchmark Results}\\\\
   \\end{center}
   \\vspace*{\\stretch{2.0}}
   \\textbf{Number of runs:} \\num{${nbRuns}}\n
\\end{titlepage}\n

\\tableofcontents

`)


    // Format global results
    output.push('\\section{Global Results}');
    formatCategory(resultData.global.all, "Global", 'All Projects');
    formatCategory(resultData.global.small, "Global", 'Small Projects (0-10k LoC)', 2);
    formatCategory(resultData.global.medium, "Global", 'Medium Projects (10-30k LoC)', 2);
    formatCategory(resultData.global.large, "Global", 'Large Projects (30k+ LoC)', 2);

    // Format rule-specific results
    output.push('\\section{Results by Rules}');
    output.push('\\subsection{Summary}');
    output.push('\\subsubsection{Analysis Time}');

    formatCategory(resultData.summary.analysisTime.all, "Analysis Time", 'All Projects');
    formatCategory(resultData.summary.analysisTime.small, "Analysis Time", 'Small Projects (0-10k LoC)', 4);
    formatCategory(resultData.summary.analysisTime.medium, "Analysis Time", 'Medium Projects (10-30k LoC)', 4);
    formatCategory(resultData.summary.analysisTime.large, "Analysis Time", 'Large Projects (30k+ LoC)', 4);

    // Format parsing overhead results
    output.push('\\subsubsection{Parsing Overhead}');
    formatCategory(resultData.summary.overheadParsing.all, "Parsing Overhead", 'All Projects');
    formatCategory(resultData.summary.overheadParsing.small, "Parsing Overhead", 'Small Projects (0-10k LoC)', 4);
    formatCategory(resultData.summary.overheadParsing.medium, "Parsing Overhead", 'Medium Projects (10-30k LoC)', 4);
    formatCategory(resultData.summary.overheadParsing.large, "Parsing Overhead", 'Large Projects (30k+ LoC)', 4);

    // Format individual rule results
    for (const [ruleName, ruleData] of Object.entries(resultData.rules).sort((a, b) => a[0].localeCompare(b[0]))) {
        output.push(`\\subsection{Rule: ${toTitleCase(ruleName)}}`);
        formatCategory(ruleData.all, ruleName, 'All Projects', 3);
        formatCategory(ruleData.small, ruleName, 'Small Projects (0-10k LoC)', 3);
        formatCategory(ruleData.medium, ruleName, 'Medium Projects (10-30k LoC)', 3);
        formatCategory(ruleData.large, ruleName, 'Large Projects (30k+ LoC)', 3);
    }

    // Close document
    output.push('\\end{document}\n');

    return output.join('\n');
}
