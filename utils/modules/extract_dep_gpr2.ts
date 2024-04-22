import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { parse, isGlob, basename } from "https://deno.land/std/path/mod.ts";
import fg from "npm:fast-glob@3.2.12";
import * as libgpr2 from "../lib/gpr2/libgpr2.ts";

function generateFromDir(paths: string[], setFileName: boolean): string[] {
    const pathGlobs: string[] = [];
    for (const path of paths) {
        if (isGlob(path)) {
            pathGlobs.push(path);
        } else {
            pathGlobs.push(`${path}${path.endsWith("/") ? "" : "/"}*.*(gpr)`);
        }
    }

    const allFiles = fg
        .sync(pathGlobs, { onlyFiles: true })
        .sort((a, b) => a.localeCompare(b));
    const extensions = [".gpr"];
    const units = new Set<string>();

    for (const path of allFiles) {
        const file = parse(path);
        if (!extensions.includes(file.ext)) {
            continue;
        }

        units.add(
            setFileName ? file.base : file.name.replaceAll("-", ".").toUpperCase()
        );
    }

    return Array.from(units);
}

export function initializeModule(program: Command): void {
    program
        .command("extract-dep-gpr2 <paths...>")
        .description(
            "Extract all dependencies (with clause) in all .gpr found in directory and subdirectory recursively."
        )
        // .option(
        //   "-f, --filenameUnit",
        //   "Set filenames into the output file instead of unit name."
        // )
        .option("-o, --output <path>", "Name of the output file", "/workspaces/bench-source/gpr2deps.txt")
        // .option(
        //   "-P, --project",
        //   "Treat the command entries (paths) as project (.gpr) paths rather than as directories to search files (.ads, abd)."
        // )
        .action(
            (
                paths: string[],
                options: { filenameUnit: boolean; output: string; project: boolean }
            ) => {
                if (!paths || paths.length === 0) {
                    console.error("Please set one or several path");
                    Deno.exit(1);
                }

                let units: string[] = [];
                const unitSet = new Set<string>();
                for (const path of paths) {
                    const tree = libgpr2.loadTree({ filename: path });
                    console.log("tree: ", tree);

                    // const sources = libgpr2.viewSources({
                    //     tree_id: tree.id,
                    //     view_id: tree.root_view,
                    // });
                    // if (options.filenameUnit) {
                    //     sources.sources
                    //         .map((elt) => elt.path)
                    //         .forEach((elt) => {
                    //             unitSet.add(basename(elt));
                    //         });
                    // } else {
                    //     const unitList = libgpr2.viewUnits({
                    //         tree_id: tree.id,
                    //         view_id: tree.root_view
                    //     });

                    //     unitList.units
                    //         .map((elt) => elt.name)
                    //         .forEach((elt) => {
                    //             unitSet.add(elt);
                    //         });
                    // }
                    libgpr2.unloadTree({ tree_id: tree.id });
                }
                // units = Array.from(unitSet);

                // const unitsList = units.sort((a, b) => a.localeCompare(b)).join("\n");
                // Deno.writeTextFileSync(options.output, unitsList);
            }
        );
}
