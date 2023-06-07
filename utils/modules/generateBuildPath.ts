import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { isGlob, join, dirname } from "https://deno.land/std/path/mod.ts";
import fg from "npm:fast-glob@3.2.12";

const OUTPUT_FILENAME = "alireTomlPath.json";

export function initializeModule(program: Command): void {
    program
        .command("generate-build-path")
        .description(
            "Generate `" + OUTPUT_FILENAME + "` of all directories that contains a `alire.origin.toml`. This file is used for example by `build` command to build all projects."
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
                    pathGlobs = `${dirPath}${dirPath.endsWith("/") ? "" : "/"}**/alire.origin.toml`;
                }

                const paths = fg
                    .sync(pathGlobs, { onlyFiles: true })
                    .map(elt => dirname(elt))
                    .sort((a, b) => a.localeCompare(b));

                Deno.writeTextFileSync(join(options.outputDir, OUTPUT_FILENAME), JSON.stringify(paths, null, 2));
            }
        );
}
