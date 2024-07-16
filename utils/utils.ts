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
    neo4jDbFilesPath: string;
    isNeo4jDbFilesComplete: boolean;
    isAdaCtlComplete: boolean;
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
                if (project.isNeo4jDbFilesComplete && project.isAdaCtlComplete) {
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
