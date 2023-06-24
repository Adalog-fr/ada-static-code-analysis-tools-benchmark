import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { join, relative } from "https://deno.land/std/path/mod.ts";
import { parse, stringify } from "https://deno.land/std/toml/mod.ts";

const DEFAULT_UNKNOWN_CRATES_FILENAME = "unknownCrates.json";
export function initializeModule(program: Command): void {
    program
        .command("generate-alire")
        .description(
            "Generate `alire.toml` from a liste of directory that contains a `alire.origin.toml`. It also delete existing `alire` folder and generate a '" + DEFAULT_UNKNOWN_CRATES_FILENAME + "' that contains a list of all unknown crates."
        )
        .option(
            "-c, --cratesPath <path>",
            "Path to a file that contains a list of know crates.",
            "/workspaces/bench-source/cratesPath.json"
        )
        .option("-i, --ignoredUnknownCrates <path>", "Name of the output file", "/workspaces/bench-source/unknownCrates.ignore")
        .option(
            "-p, --alireTomlPath <path>",
            "Path to a json file that contains a list of all directories that contains a `alire.origin.toml`. If the path is a directory, it assume that is a root of a crate and will try to find `alire.origin.toml` in this directory.",
            "/workspaces/bench-source/alireTomlPath.json"
        )
        .option(
            "-u, --unknownCrates <path>",
            "File path to a json file that contains a list of all unknown crtates found by processing `alire.origin.toml` files.",
            "/workspaces/bench-source/" + DEFAULT_UNKNOWN_CRATES_FILENAME
        )
        .action(
            (options: { cratesPath: string, ignoredUnknownCrates: string, alireTomlPath: string, unknownCrates: string }) => {
                const cratesPath = JSON.parse(Deno.readTextFileSync(options.cratesPath));
                const ignoredUnknownCrates = Deno.readTextFileSync(options.ignoredUnknownCrates).split(/\r?\n/g).map(elt => elt.trim()).filter(elt => elt.length > 0);
                const unknownCrates: { [key: string]: string[] } = {};
                let alireTomlPath: string[] = [];
                try {
                    alireTomlPath = JSON.parse(Deno.readTextFileSync(options.alireTomlPath));
                } catch (e) {
                    if (e.code !== "EISDIR") {
                        throw e;
                    }

                    alireTomlPath.push(options.alireTomlPath)
                }

                const processAlireToml = (alireDir: string) => {
                    const currentCratePath = join(alireDir, "./alire.origin.toml");
                    const data = parse(Deno.readTextFileSync(currentCratePath));

                    const addIgnoreWarningBuildSwitch = () => {
                        if (!("build-switches" in data)) {
                            data["build-switches"] = {};
                        }

                        if (!("*" in data["build-switches"])) {
                            data["build-switches"]["*"] = {};
                        }

                        data["build-switches"]["*"]["Compile_Checks"] = "none";

                    };

                    const alireFolderPath = join(alireDir, "alire");
                    try {
                        Deno.removeSync(alireFolderPath, { recursive: true });
                    } catch (error) {
                        if (!(error instanceof Deno.errors.NotFound)) {
                            throw error;
                        }
                    }

                    if (!("depends-on" in data)) {
                        addIgnoreWarningBuildSwitch();
                        Deno.writeTextFileSync(join(alireDir, "./alire.toml"), stringify(data));
                        return;
                    }

                    const canReplace = (version: string) => {
                        return !(version.startsWith("."));
                    };

                    const pins: { [key: string]: { path: string } } = { ...data["pins"]?.[0] };

                    for (let index = 0; index < data["depends-on"].length; index++) {
                        for (const dependency of Object.keys(data["depends-on"][index])) {
                            if (["gnat", "gnat_native"].includes(dependency)) {
                                delete data["depends-on"][index][dependency];
                                continue;
                            }

                            const value = data["depends-on"][index][dependency];
                            if (dependency in cratesPath) {
                                if (canReplace(value) && !(dependency in pins)) {
                                    pins[dependency] = { path: relative(alireDir, cratesPath[dependency]) };
                                    data["depends-on"][index][dependency] = "*";
                                }
                            } else {
                                if (ignoredUnknownCrates.includes(dependency)) {
                                    continue;
                                }
                                if (!(dependency in unknownCrates)) {
                                    unknownCrates[dependency] = [];
                                }
                                unknownCrates[dependency].push(currentCratePath);
                            }
                        }
                    }

                    data["pins"] = [pins];

                    addIgnoreWarningBuildSwitch();

                    Deno.writeTextFileSync(join(alireDir, "./alire.toml"), stringify(data));
                };

                for (const path of alireTomlPath) {
                    processAlireToml(path);
                }

                const unknownCratesSorted: { [key: string]: string[] } = {};
                Object.keys(unknownCrates).sort((a, b) => a.localeCompare(b)).map(elt => {
                    unknownCratesSorted[elt] = unknownCrates[elt];
                })

                Deno.writeTextFileSync(options.unknownCrates, JSON.stringify(unknownCratesSorted, null, 2));
            }
        );
}
