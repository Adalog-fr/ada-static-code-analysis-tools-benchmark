import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { join } from "https://deno.land/std/path/mod.ts";
import { parse } from "https://deno.land/std/toml/mod.ts";
import * as log from "https://deno.land/std/log/mod.ts";
import { TaskRunner, preTaskCbType, postTaskCbType } from "../lib/taskRunner/taskRunner.ts";
import { formatDuration } from "../utils.ts";

type commandType = [string, string[]];
type taskDataType = { path: string, command: commandType };

function replaceProjectName(arr: string[], projectPath: string): string[] {
    const replacedArr: string[] = [];

    for (const item of arr) {
        // Check if the element contains "-P%PRJ%"
        if (item.includes("-P%PRJ%")) {
            // Replace "-P%PRJ%" with "-Pauie.gpr"
            replacedArr.push(item.replace("-P%PRJ%", `-P${projectPath}`));
        } else {
            // If not found, add the item as is
            replacedArr.push(item);
        }
    }

    return replacedArr;
}

interface CratesInNeo4j {
    workDir: string;
    projects: Project[];
    isNeo4jDbFilesFullyComplete: boolean;
}

interface Project {
    gprPath: string;
    neo4jDbFilesPath: string;
    isNeo4jDbFilesComplete: boolean;
}

export function initializeModule(program: Command, settings: {
    commandName: string,
    description: string,
    command: string[],
    concurrency?: number,
    logAppendMode?: boolean,
    preTaskCb?: preTaskCbType<taskDataType> | undefined,
    postTaskCb?: postTaskCbType<taskDataType, string> | undefined,
    beforeRun?: (params: { taskRunner: TaskRunner<taskDataType, string>, alireTomlPath: string[] }) => void,
}): void {
    program
        .command(settings.commandName)
        .description(
            settings.description
        )
        .option(
            "-p, --pathToCratesWithCompleteNeo4jSetup <path>",
            "File path to a json file that contains a list of all working project.",
            "/workspaces/bench-source/pathToCratesWithCompleteNeo4jSetup.json"
        )
        .option(
            "-a, --logAppendMode <path>",
            "If true, log files will be in append mode, otherwise is will overwrite the file.",
            false
        )
        .action((options: { pathToCratesWithCompleteNeo4jSetup: string, logAppendMode: boolean }) => {
            const cratesPath: CratesInNeo4j[] = JSON.parse(Deno.readTextFileSync(options.pathToCratesWithCompleteNeo4jSetup))
                .filter((e: CratesInNeo4j) => e.isNeo4jDbFilesFullyComplete);
            const alireTomlPath = cratesPath.map(elt => elt.workDir);

            // Configure logs
            log.setup({
                handlers: {
                    console: new log.handlers.ConsoleHandler("DEBUG"),

                    file: new log.handlers.FileHandler("WARNING", {
                        filename: `/workspaces/bench-source/cogralysRunBenchmarkCommand-${settings.commandName}.log`,
                        formatter: "[{levelName}] {msg}",
                        mode: options.logAppendMode ? "a" : settings.logAppendMode ? "a" : "w"
                    }),
                },

                loggers: {
                    default: {
                        level: "DEBUG",
                        handlers: ["console", "file"],
                    },

                    tasks: {
                        level: "ERROR",
                        handlers: ["console"],
                    },
                },
            });

            const logger = log.getLogger();

            const taskRunner = new TaskRunner<taskDataType, string>(settings.concurrency || 1, "./workerRunCmd.ts");
            taskRunner.preTaskCb = (task, index) => {
                logger.debug(`[${settings.commandName}] [${index}/${alireTomlPath.length}] Building project '${task.id}' in: ${task.data.path}`);
                settings.preTaskCb?.(task, index);
            }
            taskRunner.postTaskCb = (task, index) => {
                if (task.status === "failure") {
                    logger.error(`[${settings.commandName}] [${index}/${alireTomlPath.length}] '${task.id}' in '${task.task.data.path}' error: ${task.result.output || task.result}`);
                }
                settings.postTaskCb?.(task, index)
            }

            const maxProject = cratesPath.flatMap(elt => elt.projects).length;

            let i = 1;
            console.log("#!/bin/bash\n");

            for (const alireDir of cratesPath) {
                const currentCratePath = join(alireDir.workDir, "./alire.toml");
                const data = parse(Deno.readTextFileSync(currentCratePath));

                for (const project of alireDir.projects) {

                    console.log(`echo [${i}/${maxProject}] START`);
                    console.log(`cd "${alireDir.workDir}"`);


                    console.log("alr exec --", ...settings.command
                        .map(item => item.includes("%PRJ%") ?
                            item.replace("%PRJ%", project.gprPath) : item)
                        .map(item => item.includes("%UNITS%") ?
                            item.replace("%UNITS%", project.gprPath.replace(".gpr", ".units")) : item)
                    );
                    console.log(`echo [${i}/${maxProject}] END`);
                    i++;
                }
            }

            settings.beforeRun?.({ taskRunner, alireTomlPath });

            performance.mark('buildStart');
            taskRunner.terminate();
        });
}
