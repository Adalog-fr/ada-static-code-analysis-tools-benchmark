import { PROJECT_ROOT } from "../config.ts";
import { join } from "jsr:@std/path@^0.225.1";

export function collectOptionList(value, previous) {
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
    const execName = "atgdb";
    const paths = [];

    if (pathToCogralys) {
        paths.push(pathToCogralys);
    } else {
        paths.push(execName);
        paths.push(join(PROJECT_ROOT, "rootfs/home/bin", execName));
    }

    for (const path of paths) {
        try {
            exec(execName);
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

export interface UnifiedCrateData {
    crates: { [key: string]: Crate };
    ignoredCrates: string[];
}

export interface Crate {
    path: string;
    alireProjects: CrateInfo[];
    ignore: boolean;
    ignoreReason?: string;
}

export interface CrateInfo {
    alireTomlPath: string;
    projects: GPRProject[];
}

export interface CratesInNeo4j {
    workDir: string;
    projects: GPRProject[];
    isNeo4jDbFilesFullyComplete: boolean;
    isAdaCtlComplete: boolean;
}

export interface GPRProject {
    gprPath: string;
    isNeo4jDbFilesComplete: boolean;
    isAdaCtlComplete: boolean;
    ignore: boolean;
    ignoreReason?: string;
}

export interface extendedGPRProject extends GPRProject {
    alireTomlPath: string;
    crateName: string;
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
