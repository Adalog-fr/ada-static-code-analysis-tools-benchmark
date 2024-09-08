import { join, basename } from "jsr:@std/path@^0.225.1";
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { wait } from "https://deno.land/x/wait/mod.ts";
import fg from "npm:fast-glob@3.3.2";
import sevenZip from "npm:7zip-min@1.4.5";
import { UnifiedCrateData, Crate } from "../types.ts";
import { PROJECT_ROOT } from "../../config.ts";

const OUTPUT_FILENAME = "benchmark-results.7z";
const RESULT_FILE_LIST_FILENAME = "listOfAllResultFilesToCompress.txt";

function getBlob(crates: { [key: string]: Crate }, maxIteration: number): string[] {
    const result: string[] = [];
    const maxIterationPattern = `{1..${maxIteration}}`;

    result.push("./benchmarkResults.json");
    result.push("./cratesDB.json");
    // `{1..1000}` is to prevent a fg bug. In the following string, I got an infinite loop if I add -j* or -j+([0-9])
    result.push(`./gnatcheck-all-${maxIterationPattern}-j{1..1000}.log`);
    result.push(`./adactl-all-${maxIterationPattern}-j0.log`);
    result.push(`./cogralys-run-all-${maxIterationPattern}.log`);


    for (const [_, crate] of Object.entries(crates)) {
        if (crate.ignore) {
            continue;
        }

        for (const project of crate.alireProjects) {
            for (const gprProject of project.projects) {
                if (gprProject.ignore) {
                    continue;
                }

                const gprName = basename(gprProject.gprPath, ".gpr");

                // gnatcheck pattern
                result.push(`${join(crate.path)}/**/gnatcheck-${gprName}-${maxIterationPattern}-j+([0-9])(-overhead|).+(log|report|*json|time)`);
                result.push(`${join(crate.path)}/**/gnatcheck*.report`);

                // adactl pattern
                result.push(`${join(crate.path)}/**/adactl-${gprName}-${maxIterationPattern}-j+([0-9])(-overhead|).+(log|report|*json|time)`);

                // cogralys pattern
                result.push(`${join(crate.path)}/**/cogralys-${gprName}-${maxIterationPattern}(-init|-populate|-run|).+(log|report|*json|time)`);
                result.push(`${join(crate.path)}/**/cogralys-${gprName}.cypher`);
            }
        }
    }

    return result;
}

export function initializeModule(program: Command): void {
    program
        .command("compress-results")
        .description(
            "Create an archive of results file"
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
