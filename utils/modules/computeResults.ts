import { join } from "jsr:@std/path@^0.225.1";
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { benchmarkResultDB, AdaControlResult, CogralysResults, GNATcheckResult, detailedResultType, globalResultTime, summaryType } from "../types.ts";
import { formatDuration } from "../utils.ts";
import { PROJECT_ROOT } from "../../config.ts";

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

export function initializeModule(program: Command): void {
    program
        .command("compute-results")
        .description(
            "Compute the benchmark results. This script shall be called after benchmark GNATcheck, AdaControl and Cogralys."
        )
        .action(
            () => {
                const results: benchmarkResultDB[] = JSON.parse(Deno.readTextFileSync("./benchmarkResults.json"));

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

                    const detailedResult : detailedResultType = {
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

                Deno.writeTextFileSync(join(PROJECT_ROOT, "benchmarkResultByProject.json"), JSON.stringify(projctsResults, null, 2));

                // Calculate percentages
                const fastestExecutionTime = Math.min(
                    summary.adactl.executionTime,
                    summary.gnatcheck_1cores.executionTime,
                    summary.gnatcheck_32cores.executionTime,
                    summary.cogralys.executionTime
                );

                const fastestOverhead = Math.min(
                    summary.adactl.overheadParsing + summary.adactl.overheadPopulating,
                    summary.gnatcheck_1cores.overheadParsing + summary.gnatcheck_1cores.overheadPopulating,
                    summary.gnatcheck_32cores.overheadParsing + summary.gnatcheck_32cores.overheadPopulating,
                    summary.cogralys.overheadParsing + summary.cogralys.overheadPopulating
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
                    result[tool as string].slowerPercentage = (((summary[tool].executionTime - fastestExecutionTime) / fastestExecutionTime)).toLocaleString(undefined,{style: 'percent', minimumFractionDigits:2});
                    result[tool].fastestOverheadPercentage = ((((summary[tool].overheadParsing + summary[tool].overheadPopulating) - fastestOverhead) / fastestOverhead) || 0).toLocaleString(undefined,{style: 'percent', minimumFractionDigits:2});
                }

                // Log the summary table
                console.table(result);
                console.log(summary);
                console.log("Number of projects: ", results.length);
                console.log("Total number of line of codes: ", totalLoC);

            }
        );
}
