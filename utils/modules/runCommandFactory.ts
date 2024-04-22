import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { join } from "https://deno.land/std/path/mod.ts";
import { parse } from "https://deno.land/std/toml/mod.ts";
import * as log from "https://deno.land/std/log/mod.ts";
import { TaskRunner, preTaskCbType, postTaskCbType } from "../lib/taskRunner/taskRunner.ts";
import { formatDuration } from "../utils.ts";

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
            "-c, --cratesPath <path>",
            "Path to a file that contains a list of know crates.",
            "/workspaces/bench-source/cratesPath.json"
        )
        .option(
            "-i, --ignoreCratesPath <path>",
            "Path to a file that contains a list of crates to ignore in dependencies.",
            "/workspaces/bench-source/unknownCrates.ignore"
        )
        .option(
            "-p, --alireTomlPath <path>",
            "File path to a json file that contains a list of all directories that contains a `alire.origin.toml`.",
            "/workspaces/bench-source/alireTomlPath.json"
        )
        .option(
            "-a, --logAppendMode <path>",
            "If true, log files will be in append mode, otherwise is will overwrite the file.",
            false
        )
        .action((options: { cratesPath: string, ignoreCratesPath: string, alireTomlPath: string, logAppendMode: boolean }) => {
            const cratesPath = JSON.parse(Deno.readTextFileSync(options.cratesPath));
            const ignoreCrate = Deno.readTextFileSync(options.ignoreCratesPath).split(/\r?\n/g).map(elt => elt.trim());
            const alireTomlPath: string[] = JSON.parse(Deno.readTextFileSync(options.alireTomlPath));
            const knowCrates = Object.keys(cratesPath);

            // Configure logs
            log.setup({
                handlers: {
                    console: new log.handlers.ConsoleHandler("DEBUG"),

                    file: new log.handlers.FileHandler("WARNING", {
                        filename: `/workspaces/bench-source/cogralysRunCommand-${settings.commandName}.log`,
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
