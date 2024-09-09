import { join, dirname, basename } from "jsr:@std/path@^0.225.1";
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { UnifiedCrateData, TimeDataWithCommand, TimeData, TimeDataKeyNumber, benchmarkResultDB, BenchmarkResult, AdaControlResult, CogralysResults, GNATcheckResult } from "../types.ts";
import { bytes } from 'https://esm.sh/@boywithkeyboard/bytes'
import { formatDuration } from "../utils.ts";

type globalResultTime = { overheadParsing: number, overheadPopulating: number, executionTime: number };
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
      executionTime: 0
    };
    const maxOverhead = element.run.average.elapsed_time * 0.95; // Assuming overhead threshold

    console.log("elapsed_time: ", element.run.average.elapsed_time);

    let overhead = 0;

    let tmpParsingOverhead = 0;
    for (const [overheadName, value] of Object.entries(element.overhead)) {
        const currentOverhead = value.average.elapsed_time;
        if (overheadName === "parsing") {
            console.log("currentOverhead: ", currentOverhead);

            tmpParsingOverhead = currentOverhead;
            result.overheadParsing = currentOverhead <= maxOverhead ? currentOverhead : 0;
        } else if (overheadName === "populatingDB") {
            result.overheadPopulating = currentOverhead;
            overhead += result.overheadPopulating;
            result.overheadParsing = tmpParsingOverhead;
        }
    }
    overhead += result.overheadParsing;

    console.log("overheadParsing: ", result.overheadParsing);

    if (overhead > maxOverhead) {
        overhead = 0;
    }

    result.executionTime = element.run.average.elapsed_time - overhead;

    return result;
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

                const summary: Record<string, globalResultTime> = {
                    adactl: { overheadParsing: 0, overheadPopulating: 0, executionTime: 0 },
                    gnatcheck_1cores: { overheadParsing: 0, overheadPopulating: 0, executionTime: 0 },
                    gnatcheck_32cores: { overheadParsing: 0, overheadPopulating: 0, executionTime: 0 },
                    cogralys: { overheadParsing: 0, overheadPopulating: 0, executionTime: 0 },
                };

                // Aggregate data
                for (const result of results) {
                    console.log(result.gprPath);

                    const benchmarkResults = result.benchmarkResults;

                    // AdaControl
                    console.log("adactl");

                    let tmp = calculateExecutionTime(benchmarkResults.adactl);
                    summary.adactl.overheadParsing += tmp.overheadParsing;
                    summary.adactl.overheadPopulating += tmp.overheadPopulating;
                    summary.adactl.executionTime += tmp.executionTime;

                    // Gnatcheck 1 core
                    console.log("gnatcheck_1cores");
                    tmp = calculateExecutionTime(benchmarkResults.gnatcheck_1cores);
                    summary.gnatcheck_1cores.overheadParsing += tmp.overheadParsing;
                    summary.gnatcheck_1cores.overheadPopulating += tmp.overheadPopulating;
                    summary.gnatcheck_1cores.executionTime += tmp.executionTime;

                    // Gnatcheck 32 cores
                    console.log("gnatcheck_32cores");
                    tmp = calculateExecutionTime(benchmarkResults.gnatcheck_32cores);
                    summary.gnatcheck_32cores.overheadParsing += tmp.overheadParsing;
                    summary.gnatcheck_32cores.overheadPopulating += tmp.overheadPopulating;
                    summary.gnatcheck_32cores.executionTime += tmp.executionTime;

                    // Cogralys
                    console.log("cogralys");
                    tmp = calculateExecutionTime(benchmarkResults.cogralys);
                    summary.cogralys.overheadParsing += tmp.overheadParsing;
                    summary.cogralys.overheadPopulating += tmp.overheadPopulating;
                    summary.cogralys.executionTime += tmp.executionTime;
                }

                // Calculate percentages
                const fastestExecutionTime = Math.min(
                    summary.adactl.executionTime,
                    summary.gnatcheck_1cores.executionTime,
                    summary.gnatcheck_32cores.executionTime,
                    summary.cogralys.executionTime
                );

                const fastestOverhead = Math.min(
                    summary.adactl.overheadParsing,
                    summary.gnatcheck_1cores.overheadParsing,
                    summary.gnatcheck_32cores.overheadParsing,
                    summary.cogralys.overheadParsing
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
                    result[tool].slowerPercentage = (((summary[tool].executionTime - fastestExecutionTime) / fastestExecutionTime)).toLocaleString(undefined,{style: 'percent', minimumFractionDigits:2});
                    result[tool].fastestOverheadPercentage = (((summary[tool].overheadParsing - fastestOverhead) / fastestOverhead) || 0).toLocaleString(undefined,{style: 'percent', minimumFractionDigits:2});
                }

                // Log the summary table
                console.table(result);
                console.log(summary);
            }
        );
}
