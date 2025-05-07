// @deno-types="https://cdn.skypack.dev/@types/lodash?dts"
import { isPlainObject } from "https://cdn.skypack.dev/lodash-es?dts";
import { join, basename } from "jsr:@std/path@^0.225.1";
import { PROJECT_ROOT, COGRALYS_EXE_NAME } from "../config.ts";
import { Crate, ExtendedGPRProject, UnifiedCrateData } from "./types.ts";

export function collectOptionList(value: any, previous: any[]) {
  return previous.concat([value]);
}

export function exec(
  command: string | URL,
  args?: string[] | undefined,
  options?: Deno.CommandOptions | undefined,
  surroundEnvWithEnvFile?: boolean,
) {
  const process = new Deno.Command(command, {
      args,
      stdout: "piped",
      stderr: "piped",
      ...options,
  });

  const { code, stderr, stdout } = process.outputSync();

  if (code !== 0) {
      const errorOutput = new TextDecoder().decode(stderr);
      return { success: false, output: errorOutput || new TextDecoder().decode(stdout) };
  }

  return { success: true, output: new TextDecoder().decode(stdout) };
}

export function getCogralysEnginePath(pathToCogralys?: string): string {
    const paths: string[] = [];

    if (pathToCogralys) {
        paths.push(pathToCogralys);
    } else {
        paths.push(COGRALYS_EXE_NAME);
        paths.push(join(PROJECT_ROOT, "analysis-tools/cogralys-engine/bin", COGRALYS_EXE_NAME));
    }

    for (const path of paths) {
        try {
            exec(path, ["-h"]);
            return path;
        } catch (_) {
            // The path is not found or not executable
        }
    }

    throw new Error ("Unable to locate 'cogralys' command")
}

export function formatDuration(milliseconds: number): string {
  const seconds = Math.floor(milliseconds / 1000);
  const minutes = Math.floor(seconds / 60);
  const hours = Math.floor(minutes / 60);
  const days = Math.floor(hours / 24);

  const durationParts = [];
  if (days > 0) {
      durationParts.push(`${Number(days).toLocaleString(undefined, { style: 'unit', unit: 'day' })}`);
  }
  if (hours > 0) {
      durationParts.push(`${Number(hours % 24).toLocaleString(undefined, { style: 'unit', unit: 'hour' })}`);
  }
  if (minutes > 0) {
      durationParts.push(`${Number(minutes % 60).toLocaleString(undefined, { style: 'unit', unit: 'minute' })}`);
  }
  if (seconds > 0) {
      durationParts.push(`${Number(seconds % 60).toLocaleString(undefined, { style: 'unit', unit: 'second' })}`);
  }
  if (milliseconds > 0) {
      durationParts.push(`${Number(milliseconds % 1000).toLocaleString(undefined, { maximumSignificantDigits: 1, style: 'unit', unit: 'millisecond' })}`);
  }

  return durationParts.join(" ");
}

// Function to parse duration string and convert it to milliseconds
export function parseDuration(durationString: string): number {
    // Remove all spaces from input string
    const sanitizedString = durationString.replace(/\s+/g, '');

    // Initialize total milliseconds
    let totalMilliseconds = 0;

    // Regular expression to match numbers followed by units
    const durationRegex = /(\d+)(d|h|hr|hrs|min|mins|s|sec|secs|ms|millisecond|milliseconds|day|days|hour|hours|minute|minutes|second|seconds)/g;

    // Find all matches in the string
    const matches = [...sanitizedString.matchAll(durationRegex)];

    // Process each match
    matches.forEach(match => {
        // Get the numeric value and unit
        const value = parseInt(match[1]);
        const unit = match[2].toLowerCase();

        // Convert each unit to milliseconds and add to total
        switch (unit) {
            case 'd':
            case 'day':
            case 'days':
                totalMilliseconds += value * 24 * 60 * 60 * 1000;
                break;
            case 'h':
            case 'hr':
            case 'hrs':
            case 'hour':
            case 'hours':
                totalMilliseconds += value * 60 * 60 * 1000;
                break;
            case 'min':
            case 'mins':
            case 'minute':
            case 'minutes':
                totalMilliseconds += value * 60 * 1000;
                break;
            case 's':
            case 'sec':
            case 'secs':
            case 'second':
            case 'seconds':
                totalMilliseconds += value * 1000;
                break;
            case 'ms':
            case 'millisecond':
            case 'milliseconds':
                totalMilliseconds += value;
                break;
        }
    });

    // Return the total duration in milliseconds
    return totalMilliseconds;
}

/**
 * Given a number, it format it with corresponding max digits
 */
export function formatNumber(value: number | bigint | Intl.StringNumericLiteral, maxDigits = 0) {
    return new Intl.NumberFormat('en-GB', { maximumFractionDigits: maxDigits }).format(value)
}

/** Filters crates to include only those with all projects meeting complete criteria */
export function filterCompleteCrates(crates: { [key: string]: Crate }): ExtendedGPRProject[] {
    const filteredCrates: ExtendedGPRProject[] = [];

    for (const [crateName, crate] of Object.entries(crates)) {
        if (crate.ignore) {
            continue;
        }

        for (const projectInfo of crate.alireProjects) {
            for (const project of projectInfo.projects) {
                if (project.isNeo4jDbFilesComplete && project.isAdaCtlComplete && !project.ignore) {
                    filteredCrates.push({...project, crateName, alireTomlPath: projectInfo.alireTomlPath})
                }
            }
        }
    }

    return filteredCrates;
}

export function getAllIgnoredCrates(data : UnifiedCrateData): string[] {
    const result = [...data.ignoredCrates];

    for (const [_, crate] of Object.entries(data.crates)) {
        if (!crate.ignore) {
            continue;
        }
        result.push(crate.path);
    }

    return result;
}

/**
 * Function to create a block of text with a border
 * @param content The content to be displayed inside the block
 * @param borderChar The character to use for the border (default: '#')
 * @returns The formatted block as a string
 */
export function createBlock(content: string, borderChar: string = '#'): string {
    // Split the content into lines
    const lines: string[] = content.split('\n');

    // Find the maximum line length
    const maxLength: number = Math.max(...lines.map(line => line.length));

    // Calculate the total width of the block
    const totalWidth: number = maxLength + 4;

    // Create the top and bottom borders
    const border: string = borderChar.repeat(totalWidth);

    // Format each line of content
    const formattedLines: string[] = lines.map(line =>
        `${borderChar} ${line.padEnd(maxLength)} ${borderChar}`
    );

    // Combine all parts of the block
    return [
        border,
        ...formattedLines,
        border
    ].join('\n');
}

/**
 * Options for the sortKeys function.
 */
interface SortKeysOptions {
    /** If true, sort keys deeply in nested objects and arrays. */
    deep?: boolean;
    /** Custom comparison function for sorting keys. */
    compare?: (a: string, b: string) => number;
  }

  /**
   * Sorts the keys of an object or elements of an array.
   * @param input - The object or array to sort.
   * @param options - Options for sorting.
   * @returns A new object or array with sorted keys/elements.
   * @throws {TypeError} If input is not a plain object or array.
   * @see Inspired from {@link https://github.com/sindresorhus/sort-keys}
   */
export function sortKeys(
  input: Record<string, any> | any[],
  options: SortKeysOptions = {}
): Record<string, any> | any[] {
  // Check if input is a plain object or array
  if (!isPlainObject(input) && !Array.isArray(input)) {
    throw new TypeError('Expected a plain object or array');
  }

  // Destructure options with default values
  const { deep = false, compare = (a: string, b: string) => a.localeCompare(b) } = options;

  // Arrays to keep track of circular references
  const seenInput: (Record<string, any> | any[])[] = [];
  const seenOutput: (Record<string, any> | any[])[] = [];

  /**
   * Deeply sorts an array.
   * @param array - The array to sort.
   * @returns A new array with sorted elements.
   */
  const deepSortArray = (array: any[]): any[] => {
    // Check for circular references
    const seenIndex = seenInput.indexOf(array);
    if (seenIndex !== -1) {
      return seenOutput[seenIndex] as any[];
    }

    // Create a new array for the result
    const result: any[] = [];
    seenInput.push(array);
    seenOutput.push(result);

    // Map and process each item in the array
    result.push(...array.map((item: any) => {
      if (Array.isArray(item)) {
        return deepSortArray(item);
      }

      if (isPlainObject(item)) {
        return sortKeysInternal(item);
      }

      return item;
    }));

    return result;
  };

  /**
   * Internal function to sort keys of an object.
   * @param obj - The object to sort.
   * @returns A new object with sorted keys.
   */
  const sortKeysInternal = (obj: Record<string, any>): Record<string, any> => {
    // Check for circular references
    const seenIndex = seenInput.indexOf(obj);
    if (seenIndex !== -1) {
      return seenOutput[seenIndex] as Record<string, any>;
    }

    // Create a new object for the result
    const result: Record<string, any> = {};
    const keys = Object.keys(obj).sort(compare);

    seenInput.push(obj);
    seenOutput.push(result);

    // Process each key in the sorted order
    for (const key of keys) {
      const value = obj[key];
      let newValue: any;

      // Handle deep sorting for arrays and objects
      if (deep && Array.isArray(value)) {
        newValue = deepSortArray(value);
      } else {
        newValue = deep && isPlainObject(value) ? sortKeysInternal(value) : value;
      }

      // Define the property in the result object
      Object.defineProperty(result, key, {
        ...Object.getOwnPropertyDescriptor(obj, key),
        value: newValue
      });
    }

    return result;
  };

  // Handle array input
  if (Array.isArray(input)) {
    return deep ? deepSortArray(input) : [...input];
  }

  // Handle object input
  return sortKeysInternal(input);
}

/**
 * Get a list of blob pattern of benchmark log files.
 * @param crates List of crates
 * @param maxIteration Maximum number of iteration of the processed benchmark
 * @returns List of blob pattern
 */
export function getBlob(crates: { [key: string]: Crate }, maxIteration: number): string[] {
    const result: string[] = [];
    const maxIterationPattern = `{1..${maxIteration}}`;

    result.push("./benchmarkResults.json");
    result.push("./benchmarkResultByProject.json");
    result.push("./cratesDB.json");
    // `{1..1000}` is to prevent a fg bug. In the following string, I got an infinite loop if I add -j* or -j+([0-9])
    result.push(`./gnatcheck-all-${maxIterationPattern}-j{1..1000}.log`);
    result.push(`./adactl-all-${maxIterationPattern}-j0.log`);
    result.push(`./cogralys-run-all-${maxIterationPattern}.log`);


    for (const [_, crate] of Object.entries(crates)) {
        if (crate.ignore) {
            continue;
        }

        for (const project of crate.alireProjects) {
            for (const gprProject of project.projects) {
                if (gprProject.ignore) {
                    continue;
                }

                const gprName = basename(gprProject.gprPath, ".gpr");

                // gnatcheck pattern
                // result.push(`${join(crate.path)}/**/gnatcheck-${gprName}-${maxIterationPattern}-j+([0-9])(-overhead|).+(log|report|*json|time)`);
                result.push(`${join(crate.path)}/**/gnatcheck*.+(log|report|*json|time)`);
                // result.push(`${join(crate.path)}/**/gnatcheck*.report`);

                // adactl pattern
                // result.push(`${join(crate.path)}/**/adactl-${gprName}-${maxIterationPattern}-j+([0-9])(-overhead|).+(log|report|*json|time)`);
                result.push(`${join(crate.path)}/**/adactl*.+(log|report|*json|time)`);

                // cogralys pattern
                // result.push(`${join(crate.path)}/**/cogralys-${gprName}-${maxIterationPattern}(-init|-populate|-run|).+(log|report|*json|time)`);
                result.push(`${join(crate.path)}/**/cogralys-${gprName}.cypher`);
                result.push(`${join(crate.path)}/**/cogralys*.+(log|report|*json|time)`);
            }
        }
    }

    return result;
}
