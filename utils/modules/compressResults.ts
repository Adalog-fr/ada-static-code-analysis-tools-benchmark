import { join } from "jsr:@std/path@^0.225.1";
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { wait } from "https://deno.land/x/wait@0.1.14/mod.ts";
import fg from "npm:fast-glob@3.3.2";
import sevenZip from "npm:7zip-min@1.4.5";
import { UnifiedCrateData } from "../types.ts";
import { PROJECT_ROOT } from "../../config.ts";
import { getBlob } from "../utils.ts";

const OUTPUT_FILENAME = "benchmark-results.7z";
const RESULT_FILE_LIST_FILENAME = "listOfAllResultFilesToCompress.txt";

export function initializeModule(program: Command): void {
    program
        .command("compress-results")
        .description(
            "Create an archive of result files"
        )
        .option(
            "--maxIteration <number>",
            "Maximum number of iteration of the processed benchmark",
            10
        )
        .action(
            (options: { maxIteration: number }) => {
                const cratesDB: UnifiedCrateData = JSON.parse(Deno.readTextFileSync(join(PROJECT_ROOT, "cratesDB.json")));
                const blobs = getBlob(cratesDB.crates, options.maxIteration);

                const filePaths = fg
                    .sync(blobs, { onlyFiles: true, dot: true, extglob: true  })
                    .sort((a: string, b: string) => a.localeCompare(b));

                const resultFileListPath = join(PROJECT_ROOT, RESULT_FILE_LIST_FILENAME);
                const outputPath = join(PROJECT_ROOT, OUTPUT_FILENAME);
                Deno.writeTextFileSync(resultFileListPath, filePaths.join("\n"));

                console.log("List of all files (" + filePaths.length + ") added to the archive listed in: ", resultFileListPath);

                const spinner = wait("Generating archive").start();

                sevenZip.cmd([
                    "a",
                    "-t7z",
                    "-mx=9",
                    "-m0=PPMd",
                    "-mmt=on",
                    "-ms=on",
                    "-spf",
                    outputPath,
                    `@${resultFileListPath}`
                ], (err: any) => {
                    spinner.succeed("Archive generated in: " + outputPath);
                    if (err) {
                        console.log("Error: ", err);
                    }
                });
            }
        );
}
