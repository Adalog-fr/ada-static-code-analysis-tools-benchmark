import { join } from "jsr:@std/path@^0.225.1";
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import fg from "npm:fast-glob@3.3.2";
import { benchmarkResultDB, AdaControlResult, CogralysResults, GNATcheckResult, detailedResultType, globalResultTime, summaryType } from "../types.ts";
import { formatDuration } from "../utils.ts";
import { PROJECT_ROOT as defaultProjectRoot } from "../../config.ts";

const OUTPUT_FILENAME = "benchmarkResults.json";
let PROJECT_ROOT: string;

 type entryData = {
    overhead: {
        parsing: AdaControlResult | GNATcheckResult;
    };
    run: AdaControlResult | GNATcheckResult;
} | CogralysResults;

// Function to calculate execution time
function calculateExecutionTime(element: entryData): globalResultTime {
    const result : globalResultTime = {
      overheadParsing: 0,
      overheadPopulating: 0,
      executionTime: 0,
      timeData: element.run.average,
      overheadTimeData: element.overhead.parsing.average
    };
    const maxOverhead = element.run.average.elapsed_time * 0.95; // Assuming overhead threshold

    let overhead = 0;

    let tmpParsingOverhead = 0;
    for (const [overheadName, value] of Object.entries(element.overhead)) {
        const currentOverhead = value.average.elapsed_time;
        if (overheadName === "parsing") {
            tmpParsingOverhead = currentOverhead;
            result.overheadParsing = currentOverhead <= maxOverhead ? currentOverhead : 0;
        } else if (overheadName === "populatingDB") {
            result.overheadPopulating = currentOverhead;
            overhead += result.overheadPopulating;
            result.overheadParsing = tmpParsingOverhead;
        }
    }
    overhead += result.overheadParsing;

    if (overhead > maxOverhead) {
        overhead = 0;
    }

    result.executionTime = element.run.average.elapsed_time - overhead;

    return result;
}

function emptyTimeData() {
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
    }
}

function getNotNullNumber(value: number, defaultValue: number, allowZeroValue = true): number {
    return isNaN(value) ? defaultValue : !allowZeroValue && value === 0 ? defaultValue : value;
}

export function initializeModule(program: Command): void {
    program
        .command("compute-results")
        .description(
            "Compute the benchmark results. This script shall be called after benchmark GNATcheck, AdaControl and Cogralys."
        )
        .option(
            "--rootDir <string>",
            "Path to the root of the result files",
            defaultProjectRoot
        )
        .action(
            (options: { rootDir: string }) => {
                PROJECT_ROOT = options.rootDir;
                // Find all benchmark result files
                const benchmarkFiles = fg.sync(join(PROJECT_ROOT, "benchmarkResults*.json"));

                for (const benchmarkFile of benchmarkFiles) {
                    const results: benchmarkResultDB[] = JSON.parse(Deno.readTextFileSync(benchmarkFile));
                    const ruleName = benchmarkFile.match(/benchmarkResults(-.*)?\.json$/)?.[1]?.substring(1) || 'global';

                    const summary: summaryType = {
                        adactl: {
                            overheadParsing: 0, overheadPopulating: 0, executionTime: 0,
                            timeData: emptyTimeData(),
                            overheadTimeData: emptyTimeData()
                        },
                        gnatcheck_1cores: {
                            overheadParsing: 0, overheadPopulating: 0, executionTime: 0,
                            timeData: emptyTimeData(),
                            overheadTimeData: emptyTimeData()
                        },
                        gnatcheck_32cores: {
                            overheadParsing: 0, overheadPopulating: 0, executionTime: 0,
                            timeData: emptyTimeData(),
                            overheadTimeData: emptyTimeData()
                        },
                        cogralys: {
                            overheadParsing: 0, overheadPopulating: 0, executionTime: 0,
                            timeData: emptyTimeData(),
                            overheadTimeData: emptyTimeData()
                        },
                    };

                    let projctsResults: detailedResultType[] = [];
                    let totalLoC = 0;

                    // Aggregate data
                    for (const result of results) {
                        const benchmarkResults = result.benchmarkResults;

                        const detailedResult: detailedResultType = {
                            crateName: result.crateName,
                            workDir: result.workDir,
                            gprPath: result.gprPath,
                            scc: {
                                loc: result.scc.Code,
                                complexity: result.scc.Complexity,
                                nbFiles: result.scc.Count
                            },
                            results: {
                                adactl: calculateExecutionTime(benchmarkResults.adactl),
                                cogralys: calculateExecutionTime(benchmarkResults.cogralys),
                                gnatcheck_1cores: calculateExecutionTime(benchmarkResults.gnatcheck_1cores),
                                gnatcheck_32cores: calculateExecutionTime(benchmarkResults.gnatcheck_32cores)
                            }
                        };
                        if (ruleName !== "global") {
                            detailedResult.results.cogralys.executionTime = benchmarkResults.cogralys.ruleResults[ruleName].average;
                        }
                        projctsResults.push(detailedResult);
                        totalLoC += result.scc.Code;

                        // Make a global aggregate result
                        // AdaControl
                        summary.adactl.overheadParsing += detailedResult.results.adactl.overheadParsing;
                        summary.adactl.overheadPopulating += detailedResult.results.adactl.overheadPopulating;
                        summary.adactl.executionTime += detailedResult.results.adactl.executionTime;

                        // Gnatcheck 1 core
                        summary.gnatcheck_1cores.overheadParsing += detailedResult.results.gnatcheck_1cores.overheadParsing;
                        summary.gnatcheck_1cores.overheadPopulating += detailedResult.results.gnatcheck_1cores.overheadPopulating;
                        summary.gnatcheck_1cores.executionTime += detailedResult.results.gnatcheck_1cores.executionTime;

                        // Gnatcheck 32 cores
                        summary.gnatcheck_32cores.overheadParsing += detailedResult.results.gnatcheck_32cores.overheadParsing;
                        summary.gnatcheck_32cores.overheadPopulating += detailedResult.results.gnatcheck_32cores.overheadPopulating;
                        summary.gnatcheck_32cores.executionTime += detailedResult.results.gnatcheck_32cores.executionTime;

                        // Cogralys
                        summary.cogralys.overheadParsing += detailedResult.results.cogralys.overheadParsing;
                        summary.cogralys.overheadPopulating += detailedResult.results.cogralys.overheadPopulating;
                        summary.cogralys.executionTime += detailedResult.results.cogralys.executionTime;
                    }

                    // Write detailed results for this rule
                    const outputFileName = ruleName === 'global'
                        ? "benchmarkResultByProject.json"
                        : `benchmarkResultByProject-${ruleName}.json`;
                    Deno.writeTextFileSync(
                        join(PROJECT_ROOT, outputFileName),
                        JSON.stringify(projctsResults, null, 2)
                    );

                    // Calculate percentages and prepare result table
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

                    const result = {
                        adactl: {
                            overheadParsing: formatDuration(summary.adactl.overheadParsing * 1000),
                            overheadPopulating: formatDuration(summary.adactl.overheadPopulating * 1000),
                            executionTime: formatDuration(summary.adactl.executionTime * 1000),
                        },
                        gnatcheck_1cores: {
                            overheadParsing: formatDuration(summary.gnatcheck_1cores.overheadParsing * 1000),
                            overheadPopulating: formatDuration(summary.gnatcheck_1cores.overheadPopulating * 1000),
                            executionTime: formatDuration(summary.gnatcheck_1cores.executionTime * 1000),
                        },
                        gnatcheck_32cores: {
                            overheadParsing: formatDuration(summary.gnatcheck_32cores.overheadParsing * 1000),
                            overheadPopulating: formatDuration(summary.gnatcheck_32cores.overheadPopulating * 1000),
                            executionTime: formatDuration(summary.gnatcheck_32cores.executionTime * 1000),
                        },
                        cogralys: {
                            overheadParsing: formatDuration(summary.cogralys.overheadParsing * 1000),
                            overheadPopulating: formatDuration(summary.cogralys.overheadPopulating * 1000),
                            executionTime: formatDuration(summary.cogralys.executionTime * 1000),
                        }
                    }

                    for (const tool in result) {
                        if (summary[tool].executionTime === 0) {
                            result[tool as string]["Fastest tool (0 is better)"] = ""
                        } else {
                            result[tool as string]["Fastest tool (0 is better)"] = (((summary[tool].executionTime - fastestExecutionTime) / fastestExecutionTime)).toLocaleString(undefined,{style: 'percent', minimumFractionDigits:2});
                        }

                        if (summary[tool].overheadParsing === 0) {
                            result[tool]["Fastest overhead (0 is better)"] = ""
                        } else {
                            result[tool]["Fastest overhead (0 is better)"] = ((((summary[tool].overheadParsing + summary[tool].overheadPopulating) - fastestOverhead) / fastestOverhead) || 0).toLocaleString(undefined,{style: 'percent', minimumFractionDigits:2});
                        }
                    }

                    // Log results for this rule
                    console.log(`\nResults for ${ruleName === 'global' ? 'global benchmark' : `rule: ${ruleName}`}:`);
                    console.table(result);
                    // console.log("Summary:", summary);
                    console.log("Number of projects:", results.length);
                    console.log("Total number of line of codes:", totalLoC);
                    console.log("-".repeat(80));
                }
            }
        );
}
