import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { join, dirname, basename } from "https://deno.land/std/path/mod.ts";
import { parse as parseToml } from "https://deno.land/std/toml/mod.ts";
import fg from "npm:fast-glob@3.2.12";

const OUTPUT_FILENAME = "pathToCratesWithCompleteNeo4jSetup.json";
export type gprProjectAndNeo4jDbFiles = { isNeo4jDbFilesComplete: boolean, gprPath: string, neo4jDbFilesPath: string }
export type alireAndGprPath = { workDir: string, projects: gprProjectAndNeo4jDbFiles[], isNeo4jDbFilesFullyComplete: boolean };

type cratesPathType = Record<string, string>;
function exploreCratesDir(cratesPath: cratesPathType, ignoredUnknownCrates: string): alireAndGprPath[] {
    const result: alireAndGprPath[] = [];
    const knowCrates = Object.keys(cratesPath);
    const paths = knowCrates.map(elt => cratesPath[elt]);

    for (const path of paths) {
        const alireFilePath = `${path}${path.endsWith("/") ? "alire.toml" : path.endsWith("alire.toml") ? "" : "/alire.toml"}`;
        const alireToml = parseToml(Deno.readTextFileSync(alireFilePath));
        const projectFilesPath = alireToml["project-files"] as string[];

        let isNeo4jDbFilesComplete = true;

        const alireAndGprPath: alireAndGprPath = { workDir: path, projects: [], isNeo4jDbFilesFullyComplete: false };

        if (projectFilesPath) {
            for (const filePath of projectFilesPath) {
                const currentPath = join(dirname(alireFilePath), filePath);

                if (currentPath.match(new RegExp(`src/(${ignoredUnknownCrates})/`))) {
                    continue;
                }

                const neo4jDbFilesPath = join(path, "." + basename(currentPath).replace(".gpr", "").trim(), ".atdgb");
                const isNeo4jDbFilesCompleteLocal = fg.sync(join(neo4jDbFilesPath, "3_*.json"), { onlyFiles: true }).length > 0;
                isNeo4jDbFilesComplete = isNeo4jDbFilesComplete && isNeo4jDbFilesCompleteLocal;
                alireAndGprPath.projects.push({
                    gprPath: currentPath,
                    neo4jDbFilesPath: neo4jDbFilesPath,
                    isNeo4jDbFilesComplete: isNeo4jDbFilesCompleteLocal
                });
            }
            alireAndGprPath.projects = alireAndGprPath.projects.sort((a, b) => a.gprPath.localeCompare(b.gprPath));
        } else {
            const filePath = (alireToml["name"] as string) + ".gpr";
            const currentPath = join(dirname(alireFilePath), filePath);
            if (!currentPath.match(new RegExp(`src/(${ignoredUnknownCrates})/`))) {
                const neo4jDbFilesPath = join(path, "." + basename(currentPath).replace(".gpr", "").trim(), ".atdgb");
                const isNeo4jDbFilesCompleteLocal = fg.sync(join(neo4jDbFilesPath, "3_*.json"), { onlyFiles: true }).length > 0;
                isNeo4jDbFilesComplete = isNeo4jDbFilesComplete && isNeo4jDbFilesCompleteLocal;
                alireAndGprPath.projects.push({
                    gprPath: currentPath,
                    neo4jDbFilesPath: neo4jDbFilesPath,
                    isNeo4jDbFilesComplete: isNeo4jDbFilesCompleteLocal
                });
            }
        }

        alireAndGprPath.isNeo4jDbFilesFullyComplete = isNeo4jDbFilesComplete;

        // Do not add ignored projets into result
        if (alireAndGprPath.projects.length > 0) {
            result.push(alireAndGprPath);
        }
    }

    return result.sort((a, b) => a.workDir.localeCompare(b.workDir));
}

export function initializeModule(program: Command): void {
    program
        .command("explore-neo4j-dirs")
        .description(
            "Generate `" + OUTPUT_FILENAME + "` that contains an array of object that contains the path to an `alire.toml` and the corresponding project files (`.gpr`). This file is used for example to run analysis (AdaControl, GNATcheck) and code metrics (tokei)."
        )
        .option(
            "-c, --cratesPath <path>",
            "Path to a file that contains a list of know crates.",
            "/workspaces/bench-source/cratesPath.json"
        )
        .option("-i, --ignoredUnknownCrates <path>", "Name of the output file", "/workspaces/bench-source/unknownCrates.ignore")
        .option(
            "-o, --output-dir <path>",
            "Diriectory were the `" + OUTPUT_FILENAME + "` will be generated.",
            "."
        )
        .action(
            (options: { cratesPath: string, ignoredUnknownCrates: string, outputDir: string }) => {

                const ignoredUnknownCrates = Deno.readTextFileSync(options.ignoredUnknownCrates).split(/\r?\n/g).map(elt => elt.trim()).filter(elt => elt.length > 0);
                const result = exploreCratesDir(JSON.parse(Deno.readTextFileSync(options.cratesPath)), ignoredUnknownCrates.join("|"));

                Deno.writeTextFileSync(join(options.outputDir, OUTPUT_FILENAME), JSON.stringify(result, null, 2));
            }
        );
}
