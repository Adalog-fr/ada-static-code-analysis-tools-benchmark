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

                const summary: summaryType = {
                    adactl: { overheadParsing: 0, overheadPopulating: 0, executionTime: 0 },
                    gnatcheck_1cores: { overheadParsing: 0, overheadPopulating: 0, executionTime: 0 },
                    gnatcheck_32cores: { overheadParsing: 0, overheadPopulating: 0, executionTime: 0 },
                    cogralys: { overheadParsing: 0, overheadPopulating: 0, executionTime: 0 },
                };

                let projctsResults: detailedResultType[] = [];

                // Aggregate data
                for (const result of results) {
                    console.log(result.gprPath);

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

                for (const tool in toolKey) {
                    result[tool].slowerPercentage = (((summary[tool].executionTime - fastestExecutionTime) / fastestExecutionTime)).toLocaleString(undefined,{style: 'percent', minimumFractionDigits:2});
                    result[tool].fastestOverheadPercentage = (((summary[tool].overheadParsing - fastestOverhead) / fastestOverhead) || 0).toLocaleString(undefined,{style: 'percent', minimumFractionDigits:2});
                }

                // Log the summary table
                console.table(result);
                console.log(summary);
            }
        );
}
