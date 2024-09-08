import { join, dirname, basename } from "jsr:@std/path@^0.225.1";
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import fg from "npm:fast-glob@3.3.2";
import { UnifiedCrateData, GPRProject } from "../utils.ts";
import { COGRALYS_DIR_NAME } from "../../config.ts";
import { bytes } from 'https://esm.sh/@boywithkeyboard/bytes'

const PROJECT_ROOT = "/Volumes/Data/programmation_pro/These/codegralys/bench-from-PC-Full-AMD/bench-from-gitlab/ada-static-code-analysis-benchmark"

const OUTPUT_FILENAME = "benchmarkResults.json";

/**
 * Convert a string representation of a computer size (B for Byte, M for Megabyte, etc.) into the corresponding byte
 * value.
 * @param value A size unit formatted with `du -ch`, like "7M" or "21,42G"
 * @returns Return the corresponding value in byte
 */
function parseUnitValue(value: string): number {
    const valueFormatted = value
    .replace(',', '.') // Convert ',' to '.'
    .replace(/([a-zA-Z])$/, ' $1') // Add space before unit
    .replace(/\s([KMGTPEZY])$/, (_, unit) => ` ${unit}B`) // Add 'B' to units except for 'B' itself
    .toUpperCase(); // Convert to uppercase

    return bytes(valueFormatted);
}

function parseTimeToSeconds(time: string): number {
    const parts = time.split(':').map(Number);
    if (parts.length === 3) {
      return parts[0] * 3600 + parts[1] * 60 + parts[2];
    } else if (parts.length === 2) {
      return parts[0] * 60 + parts[1];
    }
    return Number(time);
}

function interpolateLogPrefix(logPrefix: string, commandName: string, xpNum: number | string, cores: number | string, logSuffix: string) {
    return logPrefix
        .replace("$commandName", commandName)
        .replace("$xpNum", xpNum + "")
        .replace("$max_procs", cores + "")
        .replace("$logSuffix", logSuffix);
}

// Helper function to process time data
function processTimeData(timeFiles: string[], gprPath: string) {
    const timesData: TimeData[] = [];
    let count = 0;
    const sum: Record<string, number> = {};

    // Process each time file
    for (const path of timeFiles) {
        let timeData: TimeDataWithCommand;
        try {
            timeData = JSON.parse(Deno.readTextFileSync(path));
        } catch (e) {
            console.error("Error with: ", path);
            throw e;
        }
        const { command_being_timed, ...data } = timeData;

        // Check for execution errors
        if (data.exit_status !== "0") {
            console.error(`${gprPath} > '${path}': execution error with the following command => ${command_being_timed}`);
            continue;
        }

        count++;

        // Process each key-value pair in the data
        for (const [key, value] of Object.entries(data)) {
            let numValue: number = key === 'elapsed_time' ? parseTimeToSeconds(value) : parseFloat(value);
            if (!isNaN(numValue)) {
                sum[key] = (sum[key] || 0) + numValue;
            }
        }
        timesData.push(data);
    }

    // Calculate averages
    const average: Record<string, number> = Object.fromEntries(
        Object.entries(sum).map(([key, value]) => [key, value / count])
    );

    return { timesData, count, average };
}

// Function to compute Ada Control results
function computeAdaControlResults(alireTomlPath: string, gprPath: string, logPrefixTemplate: string, maxIteration: number, logSuffix: string) {
    // Generate log prefix for Ada Control
    const logPrefix = interpolateLogPrefix(logPrefixTemplate, "adactl", `(${Array.from({length: maxIteration}, (_, i) => i + 1).join('|')})`, "0", logSuffix);

    // Find and sort ADT size files
    const adtSizeFiles = fg.sync(`${PROJECT_ROOT}/${alireTomlPath}/**/${logPrefix}.size-adt.json`, { onlyFiles: true }).sort((a, b) => a.localeCompare(b));

    // Parse ADT size from the last file
    const adtSize = parseUnitValue(JSON.parse(Deno.readTextFileSync(adtSizeFiles[adtSizeFiles.length - 1])).size);

    // Find and sort time files
    const timeFiles = fg.sync(`${PROJECT_ROOT}/${alireTomlPath}/**/${logPrefix}.time.json`, { onlyFiles: true }).sort((a, b) => a.localeCompare(b));

    // Process time data
    const { timesData, count, average } = processTimeData(timeFiles, gprPath);

    // Return the results
    return {
        adtSize,
        allRuns: timesData,
        nbValidRuns: count,
        average
    };
}

// Function to compute GNATcheck results
function computeGNATcheckResults(alireTomlPath: string, gprPath: string, logPrefixTemplate: string, maxIteration: number, cores: number, logSuffix: string) {
    // Generate log prefix for GNATcheck
    const logPrefix = interpolateLogPrefix(logPrefixTemplate, "gnatcheck", `(${Array.from({length: maxIteration}, (_, i) => i + 1).join('|')})`, cores, logSuffix);

    // Find and sort time files
    const timeFiles = fg.sync(`${PROJECT_ROOT}/${alireTomlPath}/**/${logPrefix}.time.json`, { onlyFiles: true }).sort((a, b) => a.localeCompare(b));

    // Process time data
    const { timesData, count, average } = processTimeData(timeFiles, gprPath);

    // Return the results
    return {
        allRuns: timesData,
        nbValidRuns: count,
        average
    };
}

// Function to compute Cogralys results
function computeCogralysResults(alireTomlPath: string, gprPath: string, logPrefixTemplate: string, maxIteration: number): CogralysResults {
    // Define log suffixes
    const logSuffixes = ['-init', '-populate', '-run'];

    // Process data for each log suffix
    const results = logSuffixes.map(suffix => {
        // Generate log prefix for Cogralys with specific suffix
        const logPrefix = interpolateLogPrefix(logPrefixTemplate, "cogralys", `(${Array.from({length: maxIteration}, (_, i) => i + 1).join('|')})`, "", suffix);

        // Find and sort time files
        const timeFiles = fg.sync(`${PROJECT_ROOT}/${alireTomlPath}/**/${logPrefix}.time.json`, { onlyFiles: true }).sort((a, b) => a.localeCompare(b));

        // Process time data
        return processTimeData(timeFiles, gprPath);
    });

    // Construct and return the result object
    return {
        overhead: {
            parsing: results[0],
            populatingDB: results[1]
        },
        run: results[2]
    };
}

function computeResults(alireTomlPath: string, gprPath: string, maxIteration: number) {
    const gprName = basename(gprPath, ".gpr");

    const logPrefix = `$commandName-${gprName}-$xpNum-j$max_procs$logSuffix`;

    const adactlOverhead = computeAdaControlResults(alireTomlPath, gprPath, logPrefix, maxIteration, "-overhead");
    const adactlRun = computeAdaControlResults(alireTomlPath, gprPath, logPrefix, maxIteration, "");

    return {
        adactl: {
            overhead: { parsing: adactlOverhead },
            run: adactlRun
        },
        gnatcheck_1cores: {
            overhead: {
                parsing: computeGNATcheckResults(alireTomlPath, gprPath, logPrefix, maxIteration, 1, "-overhead")
            },
            run: computeGNATcheckResults(alireTomlPath, gprPath, logPrefix, maxIteration, 1, "")
        },
        gnatcheck_32cores: {
            overhead: {
                parsing: computeGNATcheckResults(alireTomlPath, gprPath, logPrefix, maxIteration, 32, "-overhead")
            },
            run: computeGNATcheckResults(alireTomlPath, gprPath, logPrefix, maxIteration, 32, "")
        },
        cogralys: computeCogralysResults(alireTomlPath, gprPath, logPrefix.replace("-j$max_procs", ""), maxIteration)
    };
}

export function initializeModule(program: Command): void {
    program
        .command("compute-results")
        .description(
            "Compute the benchmark results. This script shall be called after benchmark GNATcheck, AdaControl and Cogralys."
        )
        .option(
            "--maxIteration <number>",
            "Maximum number of iteration of the processed benchmark",
            10
        )
        .action(
            (options: { maxIteration: number }) => {
                const cratesDB: UnifiedCrateData = JSON.parse(Deno.readTextFileSync(join(PROJECT_ROOT, "cratesDB.json")));

                const result = [];
                for (const [crateName, crate] of Object.entries(cratesDB.crates)) {
                    if (crate.ignore) {
                        continue;
                    }

                    for (const project of crate.alireProjects) {
                        for (const gprProject of project.projects) {
                            if (gprProject.ignore) {
                                continue;
                            }

                            try {
                                result.push({
                                    crateName,
                                    workDir: project.alireTomlPath,
                                    gprPath: gprProject.gprPath,
                                    benchmarkResult: computeResults(project.alireTomlPath, gprProject.gprPath, options.maxIteration)
                                })
                                // TODO: add LoC and complexity for each projects
                            } catch (e) {
                                console.log(`Skip ${crateName} > ${project.alireTomlPath} > ${gprProject.gprPath} due to the following error: `, e);
                            }

                            // Deno.exit();
                        }
                    }
                }

                Deno.writeTextFileSync(join(PROJECT_ROOT, OUTPUT_FILENAME), JSON.stringify(result, null, 2));
            }
        );
}
