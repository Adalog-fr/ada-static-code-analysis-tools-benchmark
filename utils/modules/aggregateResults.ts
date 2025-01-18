import { join, dirname, basename } from "jsr:@std/path@^0.225.1";
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import fg from "npm:fast-glob@3.3.2";
import { UnifiedCrateData, TimeDataWithCommand, TimeData, TimeDataKeyNumber, BenchmarkResultDB, BenchmarkResult, AdaControlResult, CogralysResults, GNATcheckResult, StandardDeviationResult, RuleExecutionResult, DigestTimeResult, BenchResultByStep } from "../types.ts";
import { bytes } from 'https://esm.sh/@boywithkeyboard/bytes'
import { LanguageSummary } from "../scc-types.ts";
import { PROJECT_ROOT as defaultProjectRoot } from "../../config.ts";
import { parseDuration } from "../utils.ts";

const OUTPUT_FILENAME = "benchmarkResults.json";
let PROJECT_ROOT: string;

/**
 * Helper function to calculate standard deviation
 * @param values
 * @param mean
 * @returns return the standard deviation of values based on mean
 */
function calculateStandardDeviation(values: number[], mean: number): { value: number, percentage: number } {
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

/**
 * Convert a string representation of a aggregater size (B for Byte, M for Megabyte, etc.) into the corresponding byte
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

/**
 * Function to detect coding rule from file path
 * @param filePath
 * @returns name of the coding rule, null otherwise
 */
function detectCodingRule(filePath: string): string | null {
    const match = filePath.match(/-j\d+(-[^.]+)?[.]/);
    if (!match) return null;
    return match[1] ? match[1].substring(1) : null;
}

/**
 * Function to get global overhead results
 * @param alireTomlPath
 * @param gprPath
 * @param maxIteration
 * @returns
 */
function getGlobalOverhead(alireTomlPath: string, gprPath: string, maxIteration: number): {
    adactl: AdaControlResult;
    gnatcheck1: GNATcheckResult;
    gnatcheck32: GNATcheckResult;
} {
    const gprName = basename(gprPath, ".gpr");
    const logPrefix = `$commandName-${gprName}-$xpNum-j$max_procs-overhead`;

    return {
        adactl: aggregateAdaControlResults(alireTomlPath, gprPath, logPrefix, maxIteration, ""),
        gnatcheck1: aggregateGNATcheckResults(alireTomlPath, gprPath, logPrefix, maxIteration, 1, ""),
        gnatcheck32: aggregateGNATcheckResults(alireTomlPath, gprPath, logPrefix, maxIteration, 32, "")
    };
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

type entryData = {
    overhead: {
        parsing: BenchResultByStep;
        populatingDB?: BenchResultByStep;
    };
    run: BenchResultByStep;
};

//
/**
 * Function to calculate analysis time
 * @param element
 * @param overheadThreshold Assume that allowed overhead shall be lower than 95% (0.95) of the execution time.
 * Otherwise it is negligible.
 * @returns
 */
function calculateAnalysisTime(element: entryData, overheadThreshold = 0.95): DigestTimeResult {
    const result : DigestTimeResult = {
      overheadParsing: element.overhead.parsing.average.elapsed_time,
      overheadPopulating: 0,
      overheadThreshold,
      overhead: 0,
      executionTime: element.run.average.elapsed_time,
      analysisTime: 0,
    };
    const maxOverhead = element.run.average.elapsed_time * result.overheadThreshold; // Assuming overhead threshold

    let tmpParsingOverhead = 0;
    let havePopulatingDB = false;
    for (const [overheadName, value] of Object.entries(element.overhead)) {
        const currentOverhead = value.average.elapsed_time;
        if (overheadName === "parsing") {
            tmpParsingOverhead = currentOverhead;
            result.overheadParsing = currentOverhead <= maxOverhead ? currentOverhead : 0;
        } else if (overheadName === "populatingDB") {
            havePopulatingDB = true;
            result.overheadPopulating = currentOverhead;
            result.overhead += result.overheadPopulating;
            result.overheadParsing = tmpParsingOverhead;
        }
    }
    result.overhead += result.overheadParsing;

    if (havePopulatingDB) {
        result.analysisTime = result.executionTime;
    } else {
        if (result.overhead > maxOverhead) {
            result.overhead = 0;
        }
        result.analysisTime = result.executionTime - result.overhead;
    }

    return result;
}

// Helper function to process time data
function processTimeData(timeFiles: string[], gprPath: string) {
    const timesData: TimeData<number>[] = [];
    let count = 0;
    const sum: TimeData<number> = {
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
        exit_status: 0
    };

    // Initialize arrays to store values for standard deviation calculation
    const valuesForStdDev: { [K in TimeDataKeyNumber]: number[] } = {
        user_time: [],
        system_time: [],
        cpu_percent: [],
        elapsed_time: [],
        average_shared_text_size: [],
        average_unshared_data_size: [],
        average_stack_size: [],
        average_total_size: [],
        maximum_resident_set_size: [],
        average_resident_set_size: [],
        major_pagefaults: [],
        minor_pagefaults: [],
        voluntary_context_switches: [],
        involuntary_context_switches: [],
        swaps: [],
        block_input_operations: [],
        block_output_operations: [],
        messages_sent: [],
        messages_received: [],
        signals_delivered: [],
        page_size: [],
        exit_status: []
    };

    // Process each time file
    for (const path of timeFiles) {
        let timeData: TimeDataWithCommand;
        try {
            timeData = JSON.parse(Deno.readTextFileSync(path));
        } catch (e) {
            console.error("Error with: ", path);
            continue;
        }
        const { command_being_timed, ...data } = timeData;

        // Check for execution errors
        if (data.exit_status !== "0") {
            // Manage the special case of Variable Usage coding rule check on PHCpack. Yes, there is ASIS errors but I
            // it anyway in the result
            if (!gprPath.toLocaleLowerCase().includes("phcpack") || !path.toLocaleLowerCase().includes("variable_usage")) {
                console.error(`${gprPath} > '${path}': execution error with the following command => ${command_being_timed}`);
                continue;
            }
        }

        count++;

        // Process each key-value pair in the data
        const parsedData: TimeData<number> = {
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
            exit_status: 0
        };

        // Process each key-value pair in the data
        for (const [key, value] of Object.entries(data)) {
            // Parse value based on key type
            let numValue: number = key === 'elapsed_time' ? parseTimeToSeconds(value) : parseFloat(value);
            if (!isNaN(numValue)) {
                // Add to sum for average calculation
                sum[key as TimeDataKeyNumber] = (sum[key as TimeDataKeyNumber] || 0) + numValue;
                // Store value for standard deviation calculation
                valuesForStdDev[key as TimeDataKeyNumber].push(numValue);
            }
            parsedData[key as TimeDataKeyNumber] = numValue;
        }
        timesData.push(parsedData);
    }

    if (count < timeFiles.length / 2) {
        throw new Error(`Error with '${gprPath}': not enough data to compute metrics: ${count} success run for ${timeFiles.length} runs.`);
    }

    // Calculate averages
    const average: TimeData<number> = Object.fromEntries(
        Object.entries(sum).map(([key, value]) => [key, value / count])
    ) as unknown as TimeData<number>;

    // Calculate standard deviations
    const standardDeviation: TimeData<StandardDeviationResult> = {} as TimeData<StandardDeviationResult>;
    for (const key in valuesForStdDev) {
        standardDeviation[key as TimeDataKeyNumber] = calculateStandardDeviation(
            valuesForStdDev[key as TimeDataKeyNumber],
            average[key as TimeDataKeyNumber]
        );
    }

    return { timesData, count, average, standardDeviation };
}

function parseRuleExecutionTimes(logContent: string): { [rule: string]: number } {
    const lines = logContent.split('\n');
    const ruleTimeRegex = /^([^:]+) done in: (.+)$/;
    const results: { [rule: string]: number } = {};

    for (const line of lines) {
        const match = line.match(ruleTimeRegex);
        if (match && !line.startsWith('Total duration')) {
            const ruleName = match[1];
            const timeStr = match[2];
            results[ruleName] = parseDuration(timeStr) / 1000;
        }
    }
    return results;
}

// Function to aggregate Ada Control results
function aggregateAdaControlResults(alireTomlPath: string, gprPath: string, logPrefixTemplate: string, maxIteration: number, logSuffix: string): AdaControlResult {
    // Generate log prefix for Ada Control
    const logPrefix = interpolateLogPrefix(logPrefixTemplate, "adactl", `(${Array.from({ length: maxIteration }, (_, i) => i + 1).join('|')})`, "0", logSuffix);

    // Find and sort ADT size files
    const adtSizeFiles = fg.sync(`${PROJECT_ROOT}/${alireTomlPath}/**/${logPrefix}.size-adt.json`, { onlyFiles: true }).sort((a, b) => a.localeCompare(b));

    // Parse ADT size from the last file
    const adtSize = parseUnitValue(JSON.parse(Deno.readTextFileSync(adtSizeFiles[adtSizeFiles.length - 1])).size);

    // Find and sort time files
    const timeFiles = fg.sync(`${PROJECT_ROOT}/${alireTomlPath}/**/${logPrefix}.time.json`, { onlyFiles: true }).sort((a, b) => a.localeCompare(b));

    // Process time data
    const { timesData, count, average, standardDeviation } = processTimeData(timeFiles, gprPath);

    // Get number of issued messages by coding rule
    const extractIssuedMessages = (reportContent: string): number => {
        const match = reportContent.match(/Issued messages:.*?Warnings = (\d+)/);
        return match ? parseInt(match[1], 10) : 0;
    }

    const reportFiles = fg.sync(`${PROJECT_ROOT}/${alireTomlPath}/**/${logPrefix}.report`, { onlyFiles: true }).sort((a, b) => a.localeCompare(b));
    const issuedMessagesCounts = reportFiles.map(file => {
        const content = Deno.readTextFileSync(file);
        return extractIssuedMessages(content);
    });

    // Return the results
    return {
        adtSize,
        allRuns: timesData,
        nbValidRuns: count,
        nbRuns: timeFiles.length,
        average,
        standardDeviation,
        issuedMessages: {
            maxCount: Math.max(...issuedMessagesCounts),
            allCounts: issuedMessagesCounts
        }
    };
}

// Function to aggregate GNATcheck results
function aggregateGNATcheckResults(alireTomlPath: string, gprPath: string, logPrefixTemplate: string, maxIteration: number, cores: number, logSuffix: string): GNATcheckResult {
    // Generate log prefix for GNATcheck
    const logPrefix = interpolateLogPrefix(logPrefixTemplate, "gnatcheck", `(${Array.from({ length: maxIteration }, (_, i) => i + 1).join('|')})`, cores, logSuffix);

    // Find and sort time files
    const timeFiles = fg.sync(`${PROJECT_ROOT}/${alireTomlPath}/**/${logPrefix}.time.json`, { onlyFiles: true }).sort((a, b) => a.localeCompare(b));

    // Process time data
    const { timesData, count, average, standardDeviation } = processTimeData(timeFiles, gprPath);

    const countGNATcheckMessages = (reportContent: string): number => {
        const lines = reportContent.split('\n');
        return lines.filter(line =>
            line.startsWith('/') &&
            !line.includes('/adainclude/')
        ).length;
    }

    // Get number of issued messages by coding rule
    const reportFiles = fg.sync(`${PROJECT_ROOT}/${alireTomlPath}/**/${logPrefix}.report`, { onlyFiles: true }).sort((a, b) => a.localeCompare(b));
    const issuedMessagesCounts = reportFiles.map(file => {
        const content = Deno.readTextFileSync(file);
        return countGNATcheckMessages(content);
    });

    // Return the results
    return {
        allRuns: timesData,
        nbValidRuns: count,
        nbRuns: timeFiles.length,
        average,
        standardDeviation,
        issuedMessages: {
            maxCount: Math.max(...issuedMessagesCounts),
            allCounts: issuedMessagesCounts
        }
    };
}

// Function to aggregate Cogralys results
function aggregateCogralysResults(alireTomlPath: string, gprPath: string, logPrefixTemplate: string, maxIteration: number, overheadTreashold: number, codingRule?: string): CogralysResults {
    const countCogralysRuleMessages = (reportContent: string): { [rule: string]: number } => {
        const lines = reportContent.split('\n');
        const ruleCounts: { [rule: string]: number } = {};
        const pathPattern = /^\/[^:]+:\d+:\d+:\s*/;

        lines.forEach(line => {
            if (!line.startsWith('/') || line.includes('/adainclude/')) {
                return;
            }

            const content = line.replace(pathPattern, '');
            let rule: string;

            if (content.startsWith('Found:')) {
                if (content.includes('Found: USAGE')) {
                    rule = 'Variable_Usage';
                } else {
                    console.warn('Unknown "Found:" pattern:', line);
                    return;
                }
            } else {
                rule = content.trim().split(/\s*/)[0];
            }

            ruleCounts[rule] = (ruleCounts[rule] || 0) + 1;
        });

        return ruleCounts;
    }

    // Process overhead operations
    const logSuffixes = ['-init', '-populate', '-run'];
    const result: CogralysResults = {
      overhead: {
        parsing: {
          allRuns: [],
          nbValidRuns: 0,
          nbRuns: 0,
          average: {
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
            exit_status: 0
          },
          standardDeviation: {
            user_time: {
              value: 0,
              percentage: 0
            },
            system_time: {
              value: 0,
              percentage: 0
            },
            cpu_percent: {
              value: 0,
              percentage: 0
            },
            elapsed_time: {
              value: 0,
              percentage: 0
            },
            average_shared_text_size: {
              value: 0,
              percentage: 0
            },
            average_unshared_data_size: {
              value: 0,
              percentage: 0
            },
            average_stack_size: {
              value: 0,
              percentage: 0
            },
            average_total_size: {
              value: 0,
              percentage: 0
            },
            maximum_resident_set_size: {
              value: 0,
              percentage: 0
            },
            average_resident_set_size: {
              value: 0,
              percentage: 0
            },
            major_pagefaults: {
              value: 0,
              percentage: 0
            },
            minor_pagefaults: {
              value: 0,
              percentage: 0
            },
            voluntary_context_switches: {
              value: 0,
              percentage: 0
            },
            involuntary_context_switches: {
              value: 0,
              percentage: 0
            },
            swaps: {
              value: 0,
              percentage: 0
            },
            block_input_operations: {
              value: 0,
              percentage: 0
            },
            block_output_operations: {
              value: 0,
              percentage: 0
            },
            messages_sent: {
              value: 0,
              percentage: 0
            },
            messages_received: {
              value: 0,
              percentage: 0
            },
            signals_delivered: {
              value: 0,
              percentage: 0
            },
            page_size: {
              value: 0,
              percentage: 0
            },
            exit_status: {
              value: 0,
              percentage: 0
            }
          }
        },
        populatingDB: {
          allRuns: [],
          nbValidRuns: 0,
          nbRuns: 0,
          average: {
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
            exit_status: 0
          },
          standardDeviation: {
            user_time: {
              value: 0,
              percentage: 0
            },
            system_time: {
              value: 0,
              percentage: 0
            },
            cpu_percent: {
              value: 0,
              percentage: 0
            },
            elapsed_time: {
              value: 0,
              percentage: 0
            },
            average_shared_text_size: {
              value: 0,
              percentage: 0
            },
            average_unshared_data_size: {
              value: 0,
              percentage: 0
            },
            average_stack_size: {
              value: 0,
              percentage: 0
            },
            average_total_size: {
              value: 0,
              percentage: 0
            },
            maximum_resident_set_size: {
              value: 0,
              percentage: 0
            },
            average_resident_set_size: {
              value: 0,
              percentage: 0
            },
            major_pagefaults: {
              value: 0,
              percentage: 0
            },
            minor_pagefaults: {
              value: 0,
              percentage: 0
            },
            voluntary_context_switches: {
              value: 0,
              percentage: 0
            },
            involuntary_context_switches: {
              value: 0,
              percentage: 0
            },
            swaps: {
              value: 0,
              percentage: 0
            },
            block_input_operations: {
              value: 0,
              percentage: 0
            },
            block_output_operations: {
              value: 0,
              percentage: 0
            },
            messages_sent: {
              value: 0,
              percentage: 0
            },
            messages_received: {
              value: 0,
              percentage: 0
            },
            signals_delivered: {
              value: 0,
              percentage: 0
            },
            page_size: {
              value: 0,
              percentage: 0
            },
            exit_status: {
              value: 0,
              percentage: 0
            }
          }
        }
      },
      run: {
        allRuns: [],
        nbValidRuns: 0,
        nbRuns: 0,
        average: {
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
          exit_status: 0
        },
        standardDeviation: {
          user_time: {
            value: 0,
            percentage: 0
          },
          system_time: {
            value: 0,
            percentage: 0
          },
          cpu_percent: {
            value: 0,
            percentage: 0
          },
          elapsed_time: {
            value: 0,
            percentage: 0
          },
          average_shared_text_size: {
            value: 0,
            percentage: 0
          },
          average_unshared_data_size: {
            value: 0,
            percentage: 0
          },
          average_stack_size: {
            value: 0,
            percentage: 0
          },
          average_total_size: {
            value: 0,
            percentage: 0
          },
          maximum_resident_set_size: {
            value: 0,
            percentage: 0
          },
          average_resident_set_size: {
            value: 0,
            percentage: 0
          },
          major_pagefaults: {
            value: 0,
            percentage: 0
          },
          minor_pagefaults: {
            value: 0,
            percentage: 0
          },
          voluntary_context_switches: {
            value: 0,
            percentage: 0
          },
          involuntary_context_switches: {
            value: 0,
            percentage: 0
          },
          swaps: {
            value: 0,
            percentage: 0
          },
          block_input_operations: {
            value: 0,
            percentage: 0
          },
          block_output_operations: {
            value: 0,
            percentage: 0
          },
          messages_sent: {
            value: 0,
            percentage: 0
          },
          messages_received: {
            value: 0,
            percentage: 0
          },
          signals_delivered: {
            value: 0,
            percentage: 0
          },
          page_size: {
            value: 0,
            percentage: 0
          },
          exit_status: {
            value: 0,
            percentage: 0
          }
        },
        issuedMessages: {
          maxCount: 0,
          allCounts: []
        }
      },
      ruleResults: {},
      digestTime: {
        overheadParsing: 0,
        overheadPopulating: 0,
        overheadThreshold: 0,
        overhead: 0,
        executionTime: 0,
        analysisTime: 0
      }
    };
    const results = logSuffixes.map(suffix => {
        const logPrefix = interpolateLogPrefix(logPrefixTemplate, "cogralys", `(${Array.from({ length: maxIteration }, (_, i) => i + 1).join('|')})`, "", suffix);

        const timeFiles = fg.sync(`${PROJECT_ROOT}/${alireTomlPath}/**/${logPrefix}.time.json`, { onlyFiles: true }).sort((a, b) => a.localeCompare(b));
        const data = processTimeData(timeFiles, gprPath);

        const benchResult: BenchResultByStep = {
            allRuns: data.timesData,
            nbValidRuns: data.count,
            nbRuns: timeFiles.length,
            average: data.average,
            standardDeviation: data.standardDeviation,
        }

        switch(suffix) {
            case "-init":
                result.overhead.parsing = benchResult;
                break;
            case "-populate":
                result.overhead.populatingDB = benchResult;
                break;
            case "-run":
                result.run = { ...result.run, ...benchResult };
                break;
        }
    });

    // Process rule execution times from log files
    const logPrefix = interpolateLogPrefix(logPrefixTemplate, "cogralys", `(${Array.from({ length: maxIteration }, (_, i) => i + 1).join('|')})`, "", "");
    const ruleLogFiles = fg.sync(`${PROJECT_ROOT}/${alireTomlPath}/**/${logPrefix}.log`, { onlyFiles: true }).sort((a, b) => a.localeCompare(b));
    const reportFiles = fg.sync(`${PROJECT_ROOT}/${alireTomlPath}/**/${logPrefix}-run.report`, { onlyFiles: true }).sort((a, b) => a.localeCompare(b));


    // Collect execution times for each rule across all runs
    const ruleTimesMap: { [rule: string]: number[] } = {};
    const ruleMessagesMap: { [rule: string]: number[] } = {};

    ruleLogFiles.forEach(logFile => {
        const content = Deno.readTextFileSync(logFile);
        const runResults = parseRuleExecutionTimes(content);

        for (const [rule, time] of Object.entries(runResults)) {
            if (!ruleTimesMap[rule]) {
                ruleTimesMap[rule] = [];
            }
            ruleTimesMap[rule].push(time);
        }
    });

    reportFiles.forEach(reportFile => {
        const content = Deno.readTextFileSync(reportFile);
        const messageCounts = countCogralysRuleMessages(content);

        let foundCounter = 0;
        for (const [rule, count] of Object.entries(messageCounts)) {
            if (!ruleMessagesMap[rule]) {
                ruleMessagesMap[rule] = [];
            }
            ruleMessagesMap[rule].push(count);
            foundCounter += count;
        }
        result.run.issuedMessages.allCounts.push(foundCounter);
        result.run.issuedMessages.maxCount = Math.max(result.run.issuedMessages.maxCount, foundCounter);
    });

    // Calculate statistics for each rule
    const ruleResults: { [rule: string]: RuleExecutionResult } = {};

    // Combine all unique rule names from both times and messages
    const allRules = new Set([...Object.keys(ruleTimesMap), ...Object.keys(ruleMessagesMap)]);

    for (const rule of allRules) {
        const times = ruleTimesMap[rule] || [];
        const messages = ruleMessagesMap[rule] || [];
        const average = times.length > 0 ? times.reduce((a, b) => a + b, 0) / times.length : 0;

        ruleResults[rule.toLocaleLowerCase()] = {
            allRuns: times,
            nbValidRuns: times.length,
            nbRuns: ruleLogFiles.length,
            standardDeviation: calculateStandardDeviation(times, average),
            issuedMessages: {
                maxCount: messages.length > 0 ? Math.max(...messages) : 0,
                allCounts: messages
            },
            digestTime: {
                overheadParsing: result.overhead.parsing.average.elapsed_time,
                overheadPopulating: result.overhead.populatingDB.average.elapsed_time,
                overheadThreshold: overheadTreashold,
                overhead: 0,
                executionTime: result.overhead.parsing.average.elapsed_time + result.overhead.populatingDB.average.elapsed_time + average,
                analysisTime: average
            }
        };
    }

    return {
        ...result,
        ruleResults,
        digestTime: calculateAnalysisTime(result),
    };
}

function aggregateResults(alireTomlPath: string, gprPath: string, maxIteration: number, overheadTreashold: number, codingRule?: string): BenchmarkResult {
    const gprName = basename(gprPath, ".gpr");
    const ruleSuffix = codingRule ? `-${codingRule}` : '';
    const logPrefixWithoutRuleSuffix = `$commandName-${gprName}-$xpNum-j$max_procs`;
    const logPrefix = `${logPrefixWithoutRuleSuffix}${ruleSuffix}`;

    // Get global overhead (same for all rules)
    const overhead = getGlobalOverhead(alireTomlPath, gprPath, maxIteration);

    //  calculateAnalysisTime

    const adactl = {
        overhead: { parsing: overhead.adactl },
        run: aggregateAdaControlResults(alireTomlPath, gprPath, logPrefix, maxIteration, "")
    };

    const gnatcheck_1cores = {
        overhead: { parsing: overhead.gnatcheck1 },
        run: aggregateGNATcheckResults(alireTomlPath, gprPath, logPrefix, maxIteration, 1, "")
    };

    const gnatcheck_32cores = {
        overhead: { parsing: overhead.gnatcheck32 },
        run: aggregateGNATcheckResults(alireTomlPath, gprPath, logPrefix, maxIteration, 32, "")
    }

    const cogralys = aggregateCogralysResults(alireTomlPath, gprPath, logPrefixWithoutRuleSuffix.replace("-j$max_procs", "$logSuffix"), maxIteration, overheadTreashold, codingRule);

    // In the case of global run, Variable Usage rule is not controlled
    cogralys.digestTime.executionTime -= cogralys.ruleResults?.variable_usage.digestTime.analysisTime;
    cogralys.digestTime.analysisTime -= cogralys.ruleResults?.variable_usage.digestTime.analysisTime;

    const result: BenchmarkResult = {
        adactl: {
            ...adactl,
            digestTime: calculateAnalysisTime(adactl, overheadTreashold)
        },
        gnatcheck_1cores: {
            ...gnatcheck_1cores,
            digestTime: calculateAnalysisTime(gnatcheck_1cores, overheadTreashold)
        },
        gnatcheck_32cores: {
            ...gnatcheck_32cores,
            digestTime: calculateAnalysisTime(gnatcheck_32cores, overheadTreashold)
        },
        cogralys: cogralys
    };

    return result;
}

export function initializeModule(program: Command): void {
    program
        .command("aggregate-results")
        .description(
            "Aggregate the benchmark results. This script shall be called after benchmark GNATcheck, AdaControl and Cogralys."
        )
        .option(
            "--maxIteration <number>",
            "Maximum number of iteration of the processed benchmark",
            3
        )
        .option(
            "--rootDir <string>",
            "Path to the root of the result files",
            PROJECT_ROOT
        )
        .option(
            "--overhead-treashold <string>",
            "A value between 0 and 1 to define how much of the execution time is considered as overhead (default to 0.95, so 95% of the execution time maximum).",
            0.95
        )
        .action(
            (options: { maxIteration: number, rootDir: string, overheadTreashold: number }) => {
                PROJECT_ROOT = options.rootDir;

                const cratesDB: UnifiedCrateData = JSON.parse(Deno.readTextFileSync(join(PROJECT_ROOT, "cratesDB.json")));
                const resultsByRule = new Map<string, BenchmarkResultDB[]>();
                resultsByRule.set('global', []);

                for (const [crateName, crate] of Object.entries(cratesDB.crates)) {
                    if (crate.ignore) continue;

                    for (const project of crate.alireProjects) {
                        for (const gprProject of project.projects) {
                            if (gprProject.ignore) continue;

                            let sccMetrics: Omit<LanguageSummary, "Files">;
                            try {
                                const { Files: _, ...scc } = JSON.parse(
                                    Deno.readTextFileSync(
                                        join(defaultProjectRoot, dirname(gprProject.gprPath),
                                            basename(gprProject.gprPath, ".gpr") + "_scc-metrics.json")
                                    )
                                ) as LanguageSummary;
                                sccMetrics = scc;
                            } catch (e) {
                                console.log(`Skip ${crateName} > ${project.alireTomlPath} > ${gprProject.gprPath} (getting SCC data) due to error: `, e);
                                continue;
                            }

                            // Detect coding rules from existing files
                            const timeFiles = fg.sync(
                                `${PROJECT_ROOT}/${project.alireTomlPath}/**/*.time.json`,
                                { onlyFiles: true }
                            );

                            const rules = new Set(timeFiles.map(f => detectCodingRule(f)).filter(r => r !== null && r !== "overhead"));

                            try {
                                // Process global results
                                const globalResult: BenchmarkResultDB = {
                                    crateName,
                                    workDir: project.alireTomlPath,
                                    gprPath: gprProject.gprPath,
                                    benchmarkResults: aggregateResults(project.alireTomlPath, gprProject.gprPath, options.maxIteration, options.overheadTreashold),
                                    scc: sccMetrics
                                };
                                resultsByRule.get('global')!.push(globalResult);
                            } catch (e) {
                                console.log(`Skip ${crateName} > ${project.alireTomlPath} > ${gprProject.gprPath} (global run) due to error: `, e);
                            }
                            // Process rule-specific results
                            for (const rule of rules) {
                                if (!rule) continue;
                                if (!resultsByRule.has(rule)) {
                                    resultsByRule.set(rule, []);
                                }
                                try {
                                    const ruleResult: BenchmarkResultDB = {
                                        crateName,
                                        workDir: project.alireTomlPath,
                                        gprPath: gprProject.gprPath,
                                        benchmarkResults: aggregateResults(project.alireTomlPath, gprProject.gprPath, options.maxIteration, options.overheadTreashold, rule),
                                        scc: sccMetrics
                                    };
                                    resultsByRule.get(rule)!.push(ruleResult);
                                } catch (e) {
                                    console.log(`Skip ${crateName} > ${project.alireTomlPath} > ${gprProject.gprPath} (rule ${rule}) due to error: `, e);
                                }
                            }
                        }
                    }
                }

                // Write results to separate files
                for (const [rule, results] of resultsByRule.entries()) {
                    const fileName = rule === 'global' ? OUTPUT_FILENAME : OUTPUT_FILENAME.replace(".json", `-${rule}.json`);
                    Deno.writeTextFileSync(
                        join(PROJECT_ROOT, fileName),
                        JSON.stringify(results, null, 2)
                    );
                }
            }
        );
}
