import { join } from "jsr:@std/path@^0.225.1";
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import fg from "npm:fast-glob@3.3.2";
import { benchmarkResultDB, summaryType } from "../types.ts";
import { formatDuration } from "../utils.ts";
import { PROJECT_ROOT as defaultProjectRoot } from "../../config.ts";

let PROJECT_ROOT: string;
const GLOBAL_EXECUTION_KEY = "GLOBAL";
const codingRules: string[] = JSON.parse(Deno.readTextFileSync(join(defaultProjectRoot, "utils/cogralys-cli/rules/types/allRules.json")))
.map((elt: [string, any]) => (elt[0].toLowerCase()));


/**
 * Helper function to calculate standard deviation
 * @param values
 * @returns return the standard deviation of values based on mean
 */
function calculateStandardDeviation(values: number[]): { value: number, percentage: number } {
    const mean = values.reduce((a, b) => a + b, 0) / values.length;
    // Calculate sum of squared differences from mean
    const squaredDifferences = values.reduce((sum, value) => sum + Math.pow(value - mean, 2), 0);
    // Calculate variance by dividing sum by count of values
    const variance = squaredDifferences / values.length;
    // Calculate standard deviation
    const stdDev = Math.sqrt(variance);
    // Calculate standard deviation as percentage of mean
    const stdDevPercent = mean !== 0 ? (stdDev / mean) * 100 : 0;

    // Return square root of variance (standard deviation)
    return { value: stdDev, percentage: stdDevPercent };
}

function calculateR2(x: number[], y: number[]): number {
    // Implement linear regression R² calculation
    // This is a simplified implementation
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

function getMetrics(analysisTimeValues: number[], listOfLoC: number[]): {
    r2Value: number;
    mean: number;
    standardDeviation: { value: number, percentage: number };
} {
    return {
        r2Value: calculateR2(listOfLoC, analysisTimeValues),
        mean: analysisTimeValues.reduce((a, b) => a + b, 0) / analysisTimeValues.length,
        standardDeviation: calculateStandardDeviation(analysisTimeValues)
    };
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

                const resultData: {
                    global: { table: any, nbProjects: number, totalLoC: number},
                    rules: { [key: string]: { table: any, nbProjects: number, totalLoC: number} }
                } = {
                    global: {
                        table: null,
                        nbProjects: 0,
                        totalLoC: 0
                    },
                    rules: {},
                }

                let nbRuns = 0;

                for (const benchmarkFile of benchmarkFiles) {
                    const results: benchmarkResultDB[] = JSON.parse(Deno.readTextFileSync(benchmarkFile));
                    let ruleName: string = "";
                    const r = benchmarkFile.match(/benchmarkResults-([^-.]+).*?\.json$/)?.[1];
                    const listOfLoC: number[] = [];

                    if (r) {
                        if (!codingRules.includes(r)) {
                            ruleName = GLOBAL_EXECUTION_KEY;
                        } else {
                            ruleName = r;
                        }
                    } else {
                        ruleName = GLOBAL_EXECUTION_KEY;
                    }

                    const summary: summaryType = {
                        adactl: {
                          overheadParsing: 0, overheadPopulating: 0, analysisTime: 0,
                          timeData: emptyTimeData(),
                          overheadTimeData: emptyTimeData(),
                          executionTime: 0,
                          nbFails: 0,
                          nbProjectFails: 0,
                          analysisTimeValues: [],
                          r2Value: 0,
                          mean: 0,
                          standardDeviation: {
                            value: 0,
                            percentage: 0
                          }
                        },
                        gnatcheck_1cores: {
                          overheadParsing: 0, overheadPopulating: 0, analysisTime: 0,
                          timeData: emptyTimeData(),
                          overheadTimeData: emptyTimeData(),
                          executionTime: 0,
                          nbFails: 0,
                          nbProjectFails: 0,
                          analysisTimeValues: [],
                          r2Value: 0,
                          mean: 0,
                          standardDeviation: {
                            value: 0,
                            percentage: 0
                          }
                        },
                        gnatcheck_32cores: {
                          overheadParsing: 0, overheadPopulating: 0, analysisTime: 0,
                          timeData: emptyTimeData(),
                          overheadTimeData: emptyTimeData(),
                          executionTime: 0,
                          nbFails: 0,
                          nbProjectFails: 0,
                          analysisTimeValues: [],
                          r2Value: 0,
                          mean: 0,
                          standardDeviation: {
                            value: 0,
                            percentage: 0
                          }
                        },
                        cogralys: {
                          overheadParsing: 0, overheadPopulating: 0, analysisTime: 0,
                          timeData: emptyTimeData(),
                          overheadTimeData: emptyTimeData(),
                          executionTime: 0,
                          nbFails: 0,
                          nbProjectFails: 0,
                          analysisTimeValues: [],
                          r2Value: 0,
                          mean: 0,
                          standardDeviation: {
                            value: 0,
                            percentage: 0
                          }
                        },
                    };

                    let totalLoC = 0;

                    // Aggregate data
                    for (const result of results) {
                        totalLoC += result.scc.Code;
                        listOfLoC.push(result.scc.Code);
                        let nbFails: number = 0;
                        // Make a global aggregate result
                        // AdaControl
                        summary.adactl.overheadParsing += result.benchmarkResults.adactl.digestTime.overheadParsing;
                        summary.adactl.overheadPopulating += result.benchmarkResults.adactl.digestTime.overheadPopulating;
                        summary.adactl.analysisTime += result.benchmarkResults.adactl.digestTime.analysisTime;
                        summary.adactl.analysisTimeValues.push(result.benchmarkResults.adactl.digestTime.analysisTime);
                        summary.adactl.executionTime += result.benchmarkResults.adactl.digestTime.executionTime;
                        nbFails = result.benchmarkResults.adactl.run.nbRuns - result.benchmarkResults.adactl.run.nbValidRuns;
                        summary.adactl.nbFails += nbFails;
                        summary.adactl.nbProjectFails += nbFails > 0 ? 1 : 0;

                        // Gnatcheck 1 core
                        summary.gnatcheck_1cores.overheadParsing += result.benchmarkResults.gnatcheck_1cores.digestTime.overheadParsing;
                        summary.gnatcheck_1cores.overheadPopulating += result.benchmarkResults.gnatcheck_1cores.digestTime.overheadPopulating;
                        summary.gnatcheck_1cores.analysisTime += result.benchmarkResults.gnatcheck_1cores.digestTime.analysisTime;
                        summary.gnatcheck_1cores.analysisTimeValues.push(result.benchmarkResults.gnatcheck_1cores.digestTime.analysisTime);
                        summary.gnatcheck_1cores.executionTime += result.benchmarkResults.gnatcheck_1cores.digestTime.executionTime;
                        nbFails = result.benchmarkResults.gnatcheck_1cores.run.nbRuns - result.benchmarkResults.gnatcheck_1cores.run.nbValidRuns;
                        summary.gnatcheck_1cores.nbFails += nbFails;
                        summary.gnatcheck_1cores.nbProjectFails += nbFails > 0 ? 1 : 0;

                        // Gnatcheck 32 cores
                        summary.gnatcheck_32cores.overheadParsing += result.benchmarkResults.gnatcheck_32cores.digestTime.overheadParsing;
                        summary.gnatcheck_32cores.overheadPopulating += result.benchmarkResults.gnatcheck_32cores.digestTime.overheadPopulating;
                        summary.gnatcheck_32cores.analysisTime += result.benchmarkResults.gnatcheck_32cores.digestTime.analysisTime;
                        summary.gnatcheck_32cores.analysisTimeValues.push(result.benchmarkResults.gnatcheck_32cores.digestTime.analysisTime);
                        summary.gnatcheck_32cores.executionTime += result.benchmarkResults.gnatcheck_32cores.digestTime.executionTime;
                        nbFails = result.benchmarkResults.gnatcheck_32cores.run.nbRuns - result.benchmarkResults.gnatcheck_32cores.run.nbValidRuns;
                        summary.gnatcheck_32cores.nbFails += nbFails;
                        summary.gnatcheck_32cores.nbProjectFails += nbFails > 0 ? 1 : 0;

                        // Cogralys
                        summary.cogralys.overheadParsing += result.benchmarkResults.cogralys.digestTime.overheadParsing;
                        summary.cogralys.overheadPopulating += result.benchmarkResults.cogralys.digestTime.overheadPopulating;
                        if (ruleName === GLOBAL_EXECUTION_KEY) {
                            summary.cogralys.analysisTime += result.benchmarkResults.cogralys.digestTime.analysisTime;
                            summary.cogralys.analysisTimeValues.push(result.benchmarkResults.cogralys.digestTime.analysisTime);
                            summary.cogralys.executionTime += result.benchmarkResults.cogralys.digestTime.executionTime + result.benchmarkResults.cogralys.digestTime.overheadParsing + result.benchmarkResults.cogralys.digestTime.overheadPopulating;

                            if (nbRuns === 0) {
                                nbRuns = result.benchmarkResults.adactl.run.nbRuns;
                            }
                        } else {
                            summary.cogralys.analysisTime += result.benchmarkResults.cogralys.ruleResults[ruleName].digestTime.analysisTime;
                            summary.cogralys.analysisTimeValues.push(result.benchmarkResults.cogralys.ruleResults[ruleName].digestTime.analysisTime);
                            summary.cogralys.executionTime += result.benchmarkResults.cogralys.ruleResults[ruleName].digestTime.executionTime;
                        }
                        nbFails = result.benchmarkResults.cogralys.run.nbRuns - result.benchmarkResults.cogralys.run.nbValidRuns;
                        summary.cogralys.nbFails += nbFails;
                        summary.cogralys.nbProjectFails += nbFails > 0 ? 1 : 0;
                    }

                    summary.adactl = {...summary.adactl, ...getMetrics(summary.adactl.analysisTimeValues, listOfLoC)};
                    summary.gnatcheck_1cores = {...summary.gnatcheck_1cores, ...getMetrics(summary.gnatcheck_1cores.analysisTimeValues, listOfLoC)};
                    summary.gnatcheck_32cores = {...summary.gnatcheck_32cores, ...getMetrics(summary.gnatcheck_32cores.analysisTimeValues, listOfLoC)};
                    summary.cogralys = {...summary.cogralys, ...getMetrics(summary.cogralys.analysisTimeValues, listOfLoC)};

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

                    const result = {
                        overheadParsing: {
                            adactl: formatDuration(summary.adactl.overheadParsing * 1000),
                            gnatcheck_1cores: formatDuration(summary.gnatcheck_1cores.overheadParsing * 1000),
                            gnatcheck_32cores: formatDuration(summary.gnatcheck_32cores.overheadParsing * 1000),
                            cogralys: formatDuration(summary.cogralys.overheadParsing * 1000),
                        },
                        overheadPopulating: {
                            adactl: formatDuration(summary.adactl.overheadPopulating * 1000),
                            gnatcheck_1cores: formatDuration(summary.gnatcheck_1cores.overheadPopulating * 1000),
                            gnatcheck_32cores: formatDuration(summary.gnatcheck_32cores.overheadPopulating * 1000),
                            cogralys: formatDuration(summary.cogralys.overheadPopulating * 1000),
                        },
                        "Relative Overhead (0 is better)": {},
                        analysisTime: {
                            adactl: formatDuration(summary.adactl.analysisTime * 1000),
                            gnatcheck_1cores: formatDuration(summary.gnatcheck_1cores.analysisTime * 1000),
                            gnatcheck_32cores: formatDuration(summary.gnatcheck_32cores.analysisTime * 1000),
                            cogralys: formatDuration(summary.cogralys.analysisTime * 1000),
                        },
                        "Analysis Relative Speed (0 is better)": {},
                        "R²": {
                            adactl: summary.adactl.r2Value.toFixed(3),
                            gnatcheck_1cores: summary.gnatcheck_1cores.r2Value.toFixed(3),
                            gnatcheck_32cores: summary.gnatcheck_32cores.r2Value.toFixed(3),
                            cogralys: summary.cogralys.r2Value.toFixed(3),
                        },
                        "mean": {
                            adactl: summary.adactl.mean.toFixed(3),
                            gnatcheck_1cores: summary.gnatcheck_1cores.mean.toFixed(3),
                            gnatcheck_32cores: summary.gnatcheck_32cores.mean.toFixed(3),
                            cogralys: summary.cogralys.mean.toFixed(3),
                        },
                        "Standard Deviation value": {
                            adactl: summary.adactl.standardDeviation.value.toFixed(3),
                            gnatcheck_1cores: summary.gnatcheck_1cores.standardDeviation.value.toFixed(3),
                            gnatcheck_32cores: summary.gnatcheck_32cores.standardDeviation.value.toFixed(3),
                            cogralys: summary.cogralys.standardDeviation.value.toFixed(3),
                        },
                        "Standard Deviation in %": {
                            adactl: summary.adactl.standardDeviation.percentage.toFixed(3) + "%",
                            gnatcheck_1cores: summary.gnatcheck_1cores.standardDeviation.percentage.toFixed(3) + "%",
                            gnatcheck_32cores: summary.gnatcheck_32cores.standardDeviation.percentage.toFixed(3) + "%",
                            cogralys: summary.cogralys.standardDeviation.percentage.toFixed(3) + "%",
                        },
                        executionTime: {
                            adactl: formatDuration(summary.adactl.executionTime * 1000),
                            gnatcheck_1cores: formatDuration(summary.gnatcheck_1cores.executionTime * 1000),
                            gnatcheck_32cores: formatDuration(summary.gnatcheck_32cores.executionTime * 1000),
                            cogralys: formatDuration(summary.cogralys.executionTime * 1000),
                        },
                        "Execution Relative Speed (0 is better)": {},
                        "Nb run fails": {
                            adactl: summary.adactl.nbFails,
                            gnatcheck_1cores: summary.gnatcheck_1cores.nbFails,
                            gnatcheck_32cores: summary.gnatcheck_32cores.nbFails,
                            cogralys: summary.cogralys.nbFails,
                        },
                        "Nb project fails": {
                            adactl: summary.adactl.nbProjectFails,
                            gnatcheck_1cores: summary.gnatcheck_1cores.nbProjectFails,
                            gnatcheck_32cores: summary.gnatcheck_32cores.nbProjectFails,
                            cogralys: summary.cogralys.nbProjectFails,
                        },
                    }

                    for (const tool in summary) {
                        if (summary[tool].analysisTime === 0) {
                            result["Analysis Relative Speed (0 is better)"][tool as string] = ""
                        } else {
                            result["Analysis Relative Speed (0 is better)"][tool as string] = (((summary[tool].analysisTime - fastestAnalysisTime) / fastestAnalysisTime)).toLocaleString(undefined,{style: 'percent', minimumFractionDigits:2});
                        }

                        if (summary[tool].executionTime === 0) {
                            result["Execution Relative Speed (0 is better)"][tool as string] = ""
                        } else {
                            result["Execution Relative Speed (0 is better)"][tool as string] = (((summary[tool].executionTime - fastestExecutionTime) / fastestExecutionTime)).toLocaleString(undefined,{style: 'percent', minimumFractionDigits:2});
                        }

                        if (summary[tool].overheadParsing === 0) {
                            result["Relative Overhead (0 is better)"][tool] = ""
                        } else {
                            result["Relative Overhead (0 is better)"][tool] = ((((summary[tool].overheadParsing + summary[tool].overheadPopulating) - fastestOverhead) / fastestOverhead) || 0).toLocaleString(undefined,{style: 'percent', minimumFractionDigits:2});
                        }
                    }

                    if (ruleName === GLOBAL_EXECUTION_KEY) {
                        resultData.global = {
                            table: result,
                            nbProjects: results.length,
                            totalLoC
                        }
                    } else {
                        resultData.rules[ruleName] = {
                            table: result,
                            nbProjects: results.length,
                            totalLoC
                        }
                    }
                }

                console.log("=== Benchmark result ===\n");

                console.log("Number of runs: ", nbRuns);

                console.log("\n# Global\n");
                console.table(resultData.global.table);
                console.log("\nNumber of projects:", resultData.global.nbProjects);
                console.log("Total number of line of codes:", resultData.global.totalLoC);

                console.log("\n# By rules");

                for (const [key, value] of Object.entries(resultData.rules)) {
                    console.log(`\n## ${key}\n`);
                    console.table(value.table);
                    console.log("\nNumber of projects:", value.nbProjects);
                    console.log("Total number of line of codes:", value.totalLoC);
                }
            }
        );
}
