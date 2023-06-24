import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { parse, isGlob, basename, join, dirname, format } from "https://deno.land/std/path/mod.ts";
import { parse as parseToml } from "https://deno.land/std/toml/mod.ts";
import * as dotenv from "https://deno.land/std/dotenv/mod.ts";
import ProgressBar from "https://deno.land/x/progress@v1.3.8/mod.ts";
import * as log from "https://deno.land/std/log/mod.ts";
import fg from "npm:fast-glob@3.2.12";
import * as libgpr2 from "../lib/gpr2/libgpr2.ts";
import { TaskRunner } from "../lib/taskRunner/taskRunner.ts";

type commandType = [string, string[], Record<string, string>];
type taskDataType = { path: string, command: commandType };

function generateFromDir(paths: string[], resultingUnitKind: "unit" | "file" | "path"): string[] {
    const pathGlobs: string[] = [];
    for (const path of paths) {
        if (isGlob(path)) {
            pathGlobs.push(path);
        } else {
            pathGlobs.push(`${path}${path.endsWith("/") ? "" : "/"}*.*(ads|adb)`);
        }
    }

    const allFiles = fg
        .sync(pathGlobs, { onlyFiles: true })
        .sort((a, b) => a.localeCompare(b));
    const extensions = [".ads", ".adb"];
    const units = new Set<string>();

    for (const path of allFiles) {
        const file = parse(path);
        if (!extensions.includes(file.ext)) {
            continue;
        }

        units.add(
            resultingUnitKind === "unit" ?
                file.name.replaceAll("-", ".").toUpperCase() :
                resultingUnitKind === "file" ? file.base : format(file)
        );
    }

    return Array.from(units);
}

export function initializeModule(program: Command): void {
    program
        .command("units <paths...>")
        .description(
            "Generate an 'units.txt' file that contains unit name or file names (.ads, .adb), for every units recursively found in <path>. If option `-P` is given, then <path> is treated as paths of `.gpr` files."
        )
        .option(
            "-f, --resultingUnitKind <kind>",
            "Choose what kind of units is set into the resulting file: unit (unit name), file (file name), path (full path to the unit file).",
            "unit"
        )
        .option("-i, --ignoredUnknownCrates <path>", "Name of the output file", "/workspaces/bench-source/unknownCrates.ignore")
        .option("-o, --output <path>", "Name of the output file", "units.txt")
        .option(
            "-P, --project <type>",
            "Treat the command entries (paths) as project (.gpr) paths rather than as directories to search files (.ads, abd).",
            "gpr"
        )
        .option("-v, --verbose", "Verbose mode")
        .action(
            async (
                paths: string[],
                options: { ignoredUnknownCrates: string, resultingUnitKind: "unit" | "file" | "path"; output: string; project: string, verbose: boolean }
            ) => {
                if (!paths || paths.length === 0) {
                    console.error("Please set one or several path");
                    Deno.exit(1);
                }

                let units: string[] = [];
                const CWD = Deno.cwd();

                if (options.project) {
                    // Search for the "alire.toml" file in the specified paths
                    let projectFiles: string[] = [];

                    let correspondingAlire: Record<string, string> = {};
                    if (options.project === "alire") {
                        const ignoredUnknownCrates = Deno.readTextFileSync(options.ignoredUnknownCrates).split(/\r?\n/g).map(elt => elt.trim()).filter(elt => elt.length > 0);
                        for (const path of paths) {
                            const alireFilePath = `${path}${path.endsWith("/") ? "alire.toml" : path.endsWith("alire.toml") ? "" : "/alire.toml"}`;
                            const alireTomlFile = await fg(alireFilePath, { onlyFiles: true });
                            for (const alireFilePath of alireTomlFile) {

                                const alireToml = parseToml(Deno.readTextFileSync(alireFilePath));
                                const projectFilesPath = alireToml["project-files"] as string[];

                                if (projectFilesPath) {
                                    for (const filePath of projectFilesPath) {
                                        const currentPath = join(Deno.cwd(), dirname(alireFilePath), filePath);

                                        if (currentPath.match(new RegExp(`src/(${ignoredUnknownCrates.join("|")})`))) {
                                            continue;
                                        }
                                        projectFiles.push(currentPath);
                                        correspondingAlire[currentPath] = join(Deno.cwd(), dirname(alireFilePath));
                                    }
                                } else {
                                    const currentPath = join(Deno.cwd(), dirname(alireFilePath), (alireToml["name"] as string) + ".gpr");

                                    if (!currentPath.match(new RegExp(`src/(${ignoredUnknownCrates.join("|")})`))) {
                                        projectFiles.push(currentPath);
                                        correspondingAlire[currentPath] = join(Deno.cwd(), dirname(alireFilePath));
                                    }
                                }
                            }
                        }
                    } else if (options.project === "gpr") {
                        projectFiles = paths;
                    } else {
                        throw new Error(`Unknon project type '${options.project}'`);
                    }

                    let completed = 0;
                    const title = "generating units files:";
                    const total = projectFiles.length;
                    const progress: ProgressBar = new ProgressBar({
                        title,
                        total,
                    });

                    // Configure logs
                    log.setup({
                        handlers: {
                            console: new log.handlers.ConsoleHandler(options.verbose ? "DEBUG" : "INFO"),

                            file: new log.handlers.FileHandler("WARNING", {
                                filename: `/workspaces/bench-source/cogralysRunCommand-generate_units.log`,
                                formatter: "[{levelName}] {msg}",
                                mode: "a"
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

                    const unitSet = new Set<string>();
                    const taskRunner = new TaskRunner<taskDataType, string>(8, "./workerRunCmd.ts");

                    taskRunner.postTaskCb = (task, index) => {
                        progress.render(completed++);

                        if (!task.result?.success) {
                            log.error(task.result?.output);
                        }
                    }

                    for (const path of projectFiles) {
                        if (path in correspondingAlire) {
                            Deno.chdir(dirname(correspondingAlire[path]));

                            taskRunner.addTask(path, [], {
                                path: dirname(path), command: [
                                    "deno",
                                    [
                                        "run",
                                        "--config",
                                        "/workspaces/bench-source/deno.jsonc",
                                        "--allow-read",
                                        "--allow-write",
                                        "--allow-env",
                                        "--allow-run",
                                        "--allow-ffi",
                                        "--unstable",
                                        "/workspaces/bench-source/utils/cogralys-bench-util.ts",
                                        "units", "-P", "gpr", "-o", options.output, "-f", options.resultingUnitKind,
                                        "-i", options.ignoredUnknownCrates, path
                                    ],
                                    {
                                        env: dotenv.loadSync({
                                            envPath: join(correspondingAlire[path], ".env"),
                                        })
                                    },
                                ]
                            })

                            continue;
                        }
                        const envFilePath = join(path in correspondingAlire ? correspondingAlire[path] : dirname(path), ".env");
                        console.log("envFilePath: ", envFilePath, Deno.cwd());

                        console.log("env (before): ", JSON.stringify(Deno.env.toObject()));
                        dotenv.loadSync({
                            envPath: envFilePath,
                            export: true
                        });

                        console.log("env: ", JSON.stringify(Deno.env.toObject()));

                        Deno.chdir(dirname(path));

                        const tree = libgpr2.loadTree({ filename: path });
                        const sources = libgpr2.viewSources({
                            tree_id: tree.id,
                            view_id: tree.root_view,
                        });
                        const localUnitSet = new Set<string>();

                        if (options.resultingUnitKind !== "unit") {
                            sources.sources
                                .map((elt) => elt.path)
                                .forEach((elt) => {
                                    const unit = options.resultingUnitKind === "path" ? elt : basename(elt);
                                    unitSet.add(unit);
                                    localUnitSet.add(unit);
                                });
                        } else {
                            const unitList = libgpr2.viewUnits({
                                tree_id: tree.id,
                                view_id: tree.root_view
                            });

                            unitList.units
                                .map((elt) => elt.name)
                                .forEach((elt) => {
                                    unitSet.add(elt);
                                    localUnitSet.add(elt);
                                });
                        }
                        libgpr2.unloadTree({ tree_id: tree.id });

                        const localUnits = Array.from(localUnitSet);
                        const localUnitsList = localUnits.sort((a, b) => a.localeCompare(b)).join("\n");
                        Deno.writeTextFileSync(basename(path).replace(".gpr", `.units${options.resultingUnitKind === "unit" ? "" : options.resultingUnitKind === "path" ? "_by_path" : "_by_filename"}`), localUnitsList);

                    }
                    taskRunner.run().then(() => {
                        taskRunner.terminate();
                        progress.render(completed++);
                    }).catch((e) => {
                        taskRunner.terminate();
                        throw e;
                    });
                    units = Array.from(unitSet);
                } else {
                    units = generateFromDir(paths, options.resultingUnitKind);
                }

                const unitsList = units.sort((a, b) => a.localeCompare(b)).join("\n");
                Deno.chdir(CWD);
                Deno.writeTextFileSync(options.output, unitsList);
            }
        );
}
