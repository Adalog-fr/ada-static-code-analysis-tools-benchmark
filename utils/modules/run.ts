import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { join, dirname, basename } from "jsr:@std/path@^0.225.1";
import { parse as parseToml } from "jsr:@std/toml@^1.0.1";
import * as dotenv from "jsr:@std/dotenv@^0.225.1";
import ProgressBar from "https://deno.land/x/progress@v1.3.8/mod.ts";
import * as log from "jsr:@std/log@^0.224.6";
import { TaskRunner } from "../lib/taskRunner/taskRunner.ts";
import { filterCompleteCrates, getAllIgnoredCrates, getCogralysEnginePath } from "../utils.ts";
import { UnifiedCrateData, ExtendedGPRProject } from "../types.ts";
import { PROJECT_ROOT, COGRALYS_DIR_NAME } from "../../config.ts";

type commandType = [string, string[], Record<string, string>];
type taskDataType = { path: string, command: commandType };

let defaultCogralysEnginePath = "";
try {
    defaultCogralysEnginePath = getCogralysEnginePath()
} catch (_) {
    defaultCogralysEnginePath = "NOT FOUND"
}

export function initializeModule(program: Command): void {
    program
        .command("run")
        .description(
            "Run cogralys engine to generate all db files (Neo4J queries in a JSON file). Then run command 'populate' in order to fill the database."
        )
        .option(
            "-c, --cratesPath <path>",
            "Path to a file that contains a list of known crates.",
            join(PROJECT_ROOT, "cratesPath.json")
        )
        .option("-i, --ignoredUnknownCrates <path>", "Name of the output file", join(PROJECT_ROOT, "unknownCrates.ignore"))
        .option("-v, --verbose", "Verbose mode")
        .option("-e, --execPath <path>", "Path to cogralys bin/exec file", defaultCogralysEnginePath)
        .option("-l, --log4jSettingsPath <path>", "Path to log4j settings", join(PROJECT_ROOT, "rootfs/home/bin/log4j.properties"))
        .action(
            async (
                options: { cratesPath: string, ignoredUnknownCrates: string, execPath: string, log4jSettingsPath: string, verbose: boolean }
            ) => {
                // Configure logs
                log.setup({
                    handlers: {
                        console: new log.ConsoleHandler(options.verbose ? "DEBUG" : "INFO"),

                        file: new log.FileHandler("WARN", {
                            filename: join(PROJECT_ROOT, "cogralysRunCommand-run.log"),
                            formatter: (entry) => `[${entry.levelName}] ${entry.msg}`,
                            mode: "w"
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

                const taskRunner = new TaskRunner<taskDataType, string>(8, "./workerRunCmd.ts");
                let completed = 0;
                const projectFiles: string[] = [];

                const cratesDB: UnifiedCrateData = JSON.parse(Deno.readTextFileSync(join(PROJECT_ROOT, "cratesDB.json")));
                const ignoredUnknownCrates : string[] = getAllIgnoredCrates(cratesDB);
                const projects : ExtendedGPRProject[] = filterCompleteCrates(cratesDB.crates);
                const knowCrates = projects.map(elt => elt.crateName);
                const paths = projects.map(elt => elt.alireTomlPath);

                for (const path of paths) {
                    const alireFilePath = join(PROJECT_ROOT, `${path}${path.endsWith("/") ? "alire.toml" : path.endsWith("alire.toml") ? "" : "/alire.toml"}`);
                    const alireToml = parseToml(Deno.readTextFileSync(alireFilePath));
                    const projectFilesPath = alireToml["project-files"] as string[];

                    let dependencies: string[] = [];

                    if ("depends-on" in alireToml) {
                        dependencies = alireToml["depends-on"].map(elt => Object.keys(elt)).flat()
                            .filter(dep => knowCrates.includes(dep))
                            .filter(dep => !ignoredUnknownCrates.includes(dep));
                    }

                    if (projectFilesPath) {
                        let dependsOn = [];
                        for (const filePath of projectFilesPath) {
                            const currentPath = join(dirname(alireFilePath), filePath);

                            if (currentPath.match(new RegExp(`src/(${ignoredUnknownCrates.join("|")})/`))) {
                                continue;
                            }
                            projectFiles.push(currentPath);

                            const taskId: string = <string>alireToml.name + "_" + dependsOn.length;
                            dependsOn.push(taskId)
                            taskRunner.addTask(taskId, dependencies, {
                                path: dirname(alireFilePath), command: [
                                    "deno",
                                    [
                                        "run",
                                        "--config",
                                        join(PROJECT_ROOT, "deno.jsonc"),
                                        "--allow-read",
                                        "--allow-write",
                                        "--allow-env",
                                        "--allow-run",
                                        join(PROJECT_ROOT, "utils/executeCogralysWithWatchdog.ts"),
                                        JSON.stringify({
                                            path: dirname(alireFilePath), command: [
                                            options.execPath,
                                            [
                                                "-dvx",
                                                "-p",
                                                currentPath.trim(),
                                                `@${currentPath.replace(".gpr", ".units").trim()}`
                                            ],
                                            {
                                                env: {
                                                    ...dotenv.loadSync({
                                                        envPath: join(dirname(alireFilePath), ".env"),
                                                    }),
                                                    DRY_RUN: "True",
                                                    LOGGER_CONFIG: options.log4jSettingsPath,
                                                    NEO4J_RESULT_DIR: join("." + basename(currentPath).replace(".gpr", "").trim(), COGRALYS_DIR_NAME)
                                                }
                                            },
                                        ]})
                                    ],
                                    {}
                                ],
                            });
                        }

                        taskRunner.addTask(<string>alireToml.name, [...dependencies, ...dependsOn], {
                            path: dirname(alireFilePath), command: [
                                "echo", [<string>alireToml.name, "done"], {}
                            ]
                        });
                    } else {
                        const filePath = (alireToml["name"] as string) + ".gpr";
                        const currentPath = join(dirname(alireFilePath), filePath);

                        if (!currentPath.match(new RegExp(`src/(${ignoredUnknownCrates.join("|")})/`))) {
                            projectFiles.push(currentPath);

                            taskRunner.addTask(<string>alireToml.name, dependencies, {
                                path: dirname(alireFilePath), command: [
                                    "deno",
                                    [
                                        "run",
                                        "--config",
                                        join(PROJECT_ROOT, "deno.jsonc"),
                                        "--allow-read",
                                        "--allow-write",
                                        "--allow-env",
                                        "--allow-run",
                                        join(PROJECT_ROOT, "utils/executeCogralysWithWatchdog.ts"),
                                        JSON.stringify({
                                            path: dirname(alireFilePath), command: [
                                            options.execPath,
                                            [
                                                "-dvx",
                                                "-p",
                                                currentPath.trim(),
                                                `@${currentPath.replace(".gpr", ".units").trim()}`
                                            ],
                                            {
                                                env: {
                                                    ...dotenv.loadSync({
                                                        envPath: join(dirname(alireFilePath), ".env"),
                                                    }),
                                                    DRY_RUN: "True",
                                                    LOGGER_CONFIG: options.log4jSettingsPath,
                                                    NEO4J_RESULT_DIR: join("." + basename(currentPath).replace(".gpr", "").trim(), COGRALYS_DIR_NAME)
                                                }
                                            },
                                        ]})
                                    ],
                                    {}
                                ],
                            });
                        }
                    }
                }

                const title = "generating db files:";
                const total = projectFiles.length;
                const progress: ProgressBar = new ProgressBar({
                    title,
                    total,
                });

                taskRunner.postTaskCb = (task) => {
                    progress.render(completed++);

                    const result: string = task.result?.output || task.result;

                    if (!task.result?.success || result.includes("Stack traceback:")) {
                        log.error(`'${task.id}' in '${task.task.data.path}', executing "${JSON.stringify(task.task.data.command)}", getting error: ${task.result?.output || task.result}`);
                    }
                }

                try {
                    await taskRunner.run();
                    taskRunner.terminate();
                    progress.render(completed++);
                } catch (e) {
                    taskRunner.terminate();
                    throw e;
                }
            }
        );
}
