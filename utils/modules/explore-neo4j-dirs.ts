import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { parse, isGlob, join } from "https://deno.land/std/path/mod.ts";
import { parse as parseToml } from "https://deno.land/std/toml/mod.ts";
import fg from "npm:fast-glob@3.2.12";

const OUTPUT_FILENAME = "pathToCratesWithCompleteNeo4jSetup.json";

function exploreDirectories(rootDir: string): string[] {
    const atgdbDirs: string[] = [];

    const matches = fg.sync(rootDir, { onlyDirectories: true, dot: true });
    for (const match of matches) {
        const dir = parse(match).dir;
        const jsonFiles = fg.sync(`${dir}/.atgdb/3_*.json`, { onlyFiles: true });

        if (jsonFiles.length > 0) {
            atgdbDirs.push(dir);
        }
    }

    return atgdbDirs;
}

type alireAndGprPath = { workDir: string, gprPath: string[] };
function getCorrespondingGPRprojectsFromAlire(pathToAlireDir:string): alireAndGprPath {
    const result: alireAndGprPath = { workDir: pathToAlireDir, gprPath: [] };

    const alireFilePath = join(pathToAlireDir, "alire.toml");
    const alireToml = parseToml(Deno.readTextFileSync(alireFilePath));
    const projectFilesPath = alireToml["project-files"] as string[];

    if (projectFilesPath) {
        for (const filePath of projectFilesPath) {
            const currentPath = join(pathToAlireDir, filePath);
            result.gprPath.push(currentPath);
        }
    } else {
        const filePath = (alireToml["name"] as string) + ".gpr";
        const currentPath = join(pathToAlireDir, filePath);
        result.gprPath.push(currentPath);
    }

    result.gprPath = result.gprPath.sort((a, b) => a.localeCompare(b));

    return result;
}

export function initializeModule(program: Command): void {
    program
        .command("explore-neo4j-dirs")
        .description(
            "Generate `" + OUTPUT_FILENAME + "` that contains an array of object that contains the path to an `alire.toml` and the corresponding project files (`.gpr`). This file is used for example to run analysis (AdaControl, GNATcheck) and code metrics (tokei)."
        )
        .option(
            "-p, --path <path>",
            "The path to start the exploration.",
            "."
        )
        .option(
            "-o, --output-dir <path>",
            "Diriectory were the `" + OUTPUT_FILENAME + "` will be generated.",
            "."
        )
        .action(
            (options: { path: string, outputDir: string }) => {
                const dirPath = options.path.startsWith("/") ? options.path : join(Deno.cwd(), options.path);
                let pathGlobs: string;
                if (isGlob(dirPath)) {
                    pathGlobs = dirPath;
                } else {
                    pathGlobs = `${dirPath}/**/.atgdb`;
                }


                const paths = exploreDirectories(pathGlobs).sort((a, b) => a.localeCompare(b));
                let result: alireAndGprPath[] = [];
                for (const dir of paths) {
                    result.push(getCorrespondingGPRprojectsFromAlire(dir));
                }

                result = result.sort((a, b) => a.workDir.localeCompare(b.workDir));

                Deno.writeTextFileSync(join(options.outputDir, OUTPUT_FILENAME), JSON.stringify(result, null, 2));
            }
        );
}
