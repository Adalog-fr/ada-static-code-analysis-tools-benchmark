// @deno-types="https://cdn.skypack.dev/@types/lodash?dts"
import { isPlainObject } from "https://cdn.skypack.dev/lodash-es?dts";
import { join } from "jsr:@std/path@^0.225.1";
import { PROJECT_ROOT, COGRALYS_EXE_NAME } from "../config.ts";
import { Crate, extendedGPRProject, UnifiedCrateData } from "./types.ts";

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
    const paths = [];

    if (pathToCogralys) {
        paths.push(pathToCogralys);
    } else {
        paths.push(COGRALYS_EXE_NAME);
        paths.push(join(PROJECT_ROOT, "rootfs/home/bin", COGRALYS_EXE_NAME));
    }

    for (const path of paths) {
        try {
            exec(COGRALYS_EXE_NAME);
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
      durationParts.push(`${days}d`);
  }
  if (hours > 0) {
      durationParts.push(`${hours % 24}h`);
  }
  if (minutes > 0) {
      durationParts.push(`${minutes % 60}m`);
  }
  if (seconds > 0) {
      durationParts.push(`${seconds % 60}s`);
  }
  if (milliseconds > 0) {
      durationParts.push(`${milliseconds % 1000}ms`);
  }

  return durationParts.join(" ");
}

/** Filters crates to include only those with all projects meeting complete criteria */
export function filterCompleteCrates(crates: { [key: string]: Crate }): extendedGPRProject[] {
    const filteredCrates: extendedGPRProject[] = [];

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
