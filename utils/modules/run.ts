import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { join, dirname, basename } from "https://deno.land/std/path/mod.ts";
import { parse as parseToml } from "https://deno.land/std/toml/mod.ts";
import * as dotenv from "https://deno.land/std/dotenv/mod.ts";
import ProgressBar from "https://deno.land/x/progress@v1.3.8/mod.ts";
import * as log from "https://deno.land/std/log/mod.ts";
import { TaskRunner } from "../lib/taskRunner/taskRunner.ts";

type commandType = [string, string[], Record<string, string>];
type taskDataType = { path: string, command: commandType };

export function initializeModule(program: Command): void {
    program
        .command("run")
        .description(
            "Run cogralys engine to generate all db files (Neo4J queries in a JSON file). Then run command 'populate' in order to fill the database."
        )
        .option(
            "-c, --cratesPath <path>",
            "Path to a file that contains a list of know crates.",
            "/workspaces/bench-source/cratesPath.json"
        )
        .option("-i, --ignoredUnknownCrates <path>", "Name of the output file", "/workspaces/bench-source/unknownCrates.ignore")
        .option("-v, --verbose", "Verbose mode")
        .option("-e, --execPath <path>", "Path to cogralys bin/exec file", "/home/devy/bin/atgdb")
        .option("-l, --log4jSettingsPath <path>", "Path to log4j settings", "/home/devy/bin/log4j.properties")
        .action(
            async (
                options: { cratesPath: string, ignoredUnknownCrates: string, execPath: string, log4jSettingsPath: string, verbose: boolean }
            ) => {
                // Configure logs
                log.setup({
                    handlers: {
                        console: new log.handlers.ConsoleHandler(options.verbose ? "DEBUG" : "INFO"),

                        file: new log.handlers.FileHandler("WARNING", {
                            filename: `/workspaces/bench-source/cogralysRunCommand-run.log`,
                            formatter: "[{levelName}] {msg}",
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
                const cratesPath = JSON.parse(Deno.readTextFileSync(options.cratesPath));
                const knowCrates = Object.keys(cratesPath);
                const paths = knowCrates.map(elt => cratesPath[elt]);
                const ignoredUnknownCrates = Deno.readTextFileSync(options.ignoredUnknownCrates).split(/\r?\n/g).map(elt => elt.trim()).filter(elt => elt.length > 0);

                for (const path of paths) {
                    const alireFilePath = `${path}${path.endsWith("/") ? "alire.toml" : path.endsWith("alire.toml") ? "" : "/alire.toml"}`;
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
                                        "/workspaces/bench-source/deno.jsonc",
                                        "--allow-read",
                                        "--allow-write",
                                        "--allow-env",
                                        "--allow-run",
                                        "/workspaces/bench-source/utils/executeCogralysWithWatchdog.ts",
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
                                                    NEO4J_RESULT_DIR: join("." + basename(currentPath).replace(".gpr", "").trim(), ".atdgb")
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
                                        "/workspaces/bench-source/deno.jsonc",
                                        "--allow-read",
                                        "--allow-write",
                                        "--allow-env",
                                        "--allow-run",
                                        "/workspaces/bench-source/utils/executeCogralysWithWatchdog.ts",
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
                                                    NEO4J_RESULT_DIR: join("." + basename(currentPath).replace(".gpr", "").trim(), ".atdgb")
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
