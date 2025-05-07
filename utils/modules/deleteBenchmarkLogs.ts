import { join, dirname, basename } from "jsr:@std/path@^0.225.1";
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { wait } from "https://deno.land/x/wait@0.1.14/mod.ts";
import fg from "npm:fast-glob@3.3.2";
import { UnifiedCrateData } from "../types.ts";
import { PROJECT_ROOT } from "../../config.ts";
import { getBlob } from "../utils.ts";

async function deleteFile(path: string): Promise<void> {
    try {
        if (path.endsWith('.cypher')) {
            const dirPath = dirname(path);
            await Deno.remove(dirPath, { recursive: true });
        } else {
            await Deno.remove(path);
        }
    } catch (error) {
        console.error(`Error deleting ${path}: ${error.message}`);
    }
}

export function initializeModule(program: Command): void {
    program
        .command("delete-benchmark-logs")
        .description(
            "Delete all logs related to the benchmarks."
        )
        .option(
            "--maxIteration <number>",
            "Maximum number of iteration of the processed benchmark",
            3
        )
        .option(
            "--concurrency <number>",
            "Number of concurrent deletion operations",
            50
        )
        .action(
            async (options: { maxIteration: number, concurrency: number }) => {
                const cratesDB: UnifiedCrateData = JSON.parse(Deno.readTextFileSync(join(PROJECT_ROOT, "cratesDB.json")));
                const blobs = getBlob(cratesDB.crates, options.maxIteration);

                const filePaths = fg
                    .sync(blobs, { onlyFiles: true, dot: true, extglob: true })
                    .filter(path => basename(path) !== "cratesDB.json")
                    .sort((a: string, b: string) => a.localeCompare(b));

                console.log(`Found ${filePaths.length} files to process.`);

                const spinner = wait("Deleting log files").start();

                const chunkSize = options.concurrency;
                for (let i = 0; i < filePaths.length; i += chunkSize) {
                    const chunk = filePaths.slice(i, i + chunkSize);
                    await Promise.all(chunk.map(path => deleteFile(path)));
                    spinner.text = `Deleted ${Math.min(i + chunkSize, filePaths.length)} of ${filePaths.length} files`;
                }

                spinner.succeed("Deletion process completed.");
            }
        );
}
