import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { parse, isGlob, basename, join, dirname, format, relative } from "jsr:@std/path@^0.225.1";
import { parse as parseToml } from "jsr:@std/toml@^1.0.1";
import * as dotenv from "jsr:@std/dotenv@^0.225.1";
import ProgressBar from "https://deno.land/x/progress@v1.3.8/mod.ts";
import * as log from "jsr:@std/log@^0.224.6";
import fg from "npm:fast-glob@3.3.2";
import { TaskRunner } from "../lib/taskRunner/taskRunner.ts";
import { PROJECT_ROOT } from "../../config.ts";

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
        .option("-i, --ignoredUnknownCrates <path>", "Name of the output file", join(PROJECT_ROOT, "unknownCrates.ignore"))
        .option(
            "-P, --project <type>",
            "Treat the command entries (paths) as project (.gpr) paths rather than as directories to search files (.ads, abd).",
            "gpr"
        )
        .option("-v, --verbose", "Verbose mode")
        .action(
            async (
                paths: string[],
                options: { ignoredUnknownCrates: string, resultingUnitKind: "unit" | "file" | "path"; project: string, verbose: boolean }
            ) => {
                if (!paths || paths.length === 0) {
                    console.error("Please set one or several path");
                    Deno.exit(1);
                }
                const libgpr2 = await import("../lib/gpr2/libgpr2.ts");

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
                            console: new log.ConsoleHandler(options.verbose ? "DEBUG" : "INFO"),

                            file: new log.FileHandler("WARN", {
                                filename: join(PROJECT_ROOT,`cogralysRunCommand-generate_units.log`),
                                formatter: (logRecord) => `[${logRecord.levelName}] ${logRecord.msg}`,
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
                                        join(PROJECT_ROOT, "deno.jsonc"),
                                        "--allow-all",
                                        "--unstable-ffi",
                                        join(PROJECT_ROOT, "utils/cogralys-bench-util.ts"),
                                        "units", "-P", "gpr", "-f", options.resultingUnitKind,
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
                        // The following code in executed only if `path` is not an `alire.toml` path
                        const envFilePath = join(path in correspondingAlire ? correspondingAlire[path] : dirname(path), ".env");

                        dotenv.loadSync({
                            envPath: envFilePath,
                            export: true
                        });

                        Deno.chdir(dirname(path));

                        const tree = libgpr2.loadTree({ filename: path });
                        const sources = libgpr2.viewSources({
                            tree_id: tree.id,
                            view_id: tree.root_view,
                        });
                        const localUnitSet = new Set<string>()
                        let localUnitsList = "";

                        if (options.resultingUnitKind !== "unit") {
                            sources.sources
                                .map((elt) => elt.path)
                                .forEach((elt) => {
                                    const unit = options.resultingUnitKind === "path" ? elt : basename(elt);
                                    unitSet.add(unit);
                                    localUnitSet.add(unit);
                                });

                            localUnitsList = Array.from(localUnitSet)
                                .map(e => relative(Deno.cwd(), e))
                                .filter(e => e.toLocaleLowerCase().endsWith(".ada")
                                    || e.toLocaleLowerCase().endsWith(".adb")
                                    || e.toLocaleLowerCase().endsWith(".ads"))
                                .sort((a, b) => a.localeCompare(b))
                                .join("\n");
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
                            localUnitsList = Array.from(localUnitSet).join("\n");
                        }
                        libgpr2.unloadTree({ tree_id: tree.id });

                        Deno.writeTextFileSync(basename(path).replace(".gpr", `.units${options.resultingUnitKind === "unit" ? "" : options.resultingUnitKind === "path" ? "_by_path" : "_by_filename"}`), localUnitsList);
                    }
                    taskRunner.run().then(() => {
                        taskRunner.terminate();
                    }).catch((e) => {
                        taskRunner.terminate();
                        throw e;
                    });
                    units = Array.from(unitSet);
                } else {
                    units = generateFromDir(paths, options.resultingUnitKind);
                }

                Deno.chdir(CWD);
            }
        );
}
