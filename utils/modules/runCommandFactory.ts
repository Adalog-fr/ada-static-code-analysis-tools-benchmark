import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { join } from "jsr:@std/path@^0.225.1";
import { parse } from "jsr:@std/toml@^1.0.1";
import * as log from "jsr:@std/log@^0.224.6";
import { TaskRunner, preTaskCbType, postTaskCbType } from "../lib/taskRunner/taskRunner.ts";
import { formatDuration, filterCompleteCrates, getAllIgnoredCrates } from "../utils.ts";
import { UnifiedCrateData, ExtendedGPRProject } from "../types.ts";
import { PROJECT_ROOT } from "../../config.ts";

type commandType = [string, string[]];
type taskDataType = { path: string, command: commandType };
export function initializeModule(program: Command, settings: {
    commandName: string,
    description: string,
    command: commandType,
    concurrency?: number,
    logAppendMode?: boolean,
    preTaskCb?: preTaskCbType<taskDataType> | undefined,
    postTaskCb?: postTaskCbType<taskDataType, string> | undefined,
    beforeRun?: (params: { taskRunner: TaskRunner<taskDataType, string>, alireTomlPath: string[]}) => void
 }): void {
    program
        .command(settings.commandName)
        .description(
            settings.description
        )
        .option(
            "-a, --logAppendMode",
            "If true, log files will be in append mode, otherwise it will overwrite the file.",
            false
        )
        .option(
            "-i, --ignoreMissingDependencies",
            "If true, the task runner will not throw error when there are missing dependencies.",
            false
        )
        .action((options: { logAppendMode: boolean, ignoreMissingDependencies: boolean }) => {
            const cratesDB: UnifiedCrateData = JSON.parse(Deno.readTextFileSync(join(PROJECT_ROOT, "cratesDB.json")));
            const ignoreCrate : string[] = getAllIgnoredCrates(cratesDB);
            const projects : ExtendedGPRProject[] = filterCompleteCrates(cratesDB.crates);
            const alireTomlPath: string[] = [...new Set(projects.map(elt => join(PROJECT_ROOT, elt.alireTomlPath)))];
            const knowCrates = Object.keys(cratesDB.crates);

            // Configure logs
            log.setup({
                handlers: {
                    console: new log.ConsoleHandler("DEBUG"),

                    file: new log.FileHandler("WARN", {
                        filename: join(PROJECT_ROOT, `cogralysRunCommand-${settings.commandName}.log`),
                        formatter: (entry) => `[${entry.levelName}] ${entry.msg}`,
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

            const taskRunner = new TaskRunner<taskDataType, string>(settings.concurrency || 1, "./workerRunCmd.ts", undefined, options.ignoreMissingDependencies);
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

            for (const alireDir of alireTomlPath) {
                const currentCratePath = join(alireDir, "./alire.toml");
                const data = parse(Deno.readTextFileSync(currentCratePath));

                let dependencies: string[] = [];
                if ("depends-on" in data) {
                    dependencies = data["depends-on"].map(elt => Object.keys(elt)).flat()
                    .filter(dep => knowCrates.includes(dep))
                    .filter(dep => !ignoreCrate.includes(dep));
                }

                taskRunner.addTask(<string>data.name, dependencies, { path: alireDir, command: settings.command });
            }

            settings.beforeRun?.({taskRunner, alireTomlPath});

            performance.mark('buildStart');
            taskRunner.run().then(() => {
                performance.mark('buildEnd');
                taskRunner.terminate();
                const buildDuration = performance.measure('buildDuration', 'buildStart', 'buildEnd').duration;
                logger.info(`[${settings.commandName}] Build in: ${formatDuration(buildDuration)}`);
            }).catch((e) => {
                taskRunner.terminate();
                throw e;
            });
        });
}
