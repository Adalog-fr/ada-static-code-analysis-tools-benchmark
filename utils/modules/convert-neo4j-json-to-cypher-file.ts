import { join, basename } from "jsr:@std/path@^0.225.1";
import { writeAll } from "jsr:@std/io@^0.224.7";
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import fg from "npm:fast-glob@3.3.2";
import { PROJECT_ROOT, COGRALYS_DIR_NAME } from "../../config.ts";

// Function to remove double quotes from JSON keys
function removeQuotesFromKeys(jsonString: string): string {
    // Use a regular expression to match JSON keys and remove their quotes
    return jsonString.replace(/"(\w+)":/g, '$1:');
}

// Function to process JSON files and write Cypher statements in real-time
async function processCypherFiles(directoryPath: string, fileName: string) {
    // Get all json files
    const jsonPaths = fg.sync(`${directoryPath}/*.json`, { onlyFiles: true }).sort((a: string, b: string) => a.localeCompare(b));

    // Open the output file for writing
    const file = await Deno.open(join(directoryPath, fileName), { write: true, create: true, truncate: true });

    // Process each JSON file
    for (const path of jsonPaths) {
        // Read the file content
        const fileContent = await Deno.readTextFile(path);

        // Parse the JSON content
        const jsonData = JSON.parse(fileContent);

        // Extract statements from the JSON and write them to the file
        for (const item of jsonData.statements) {
            await writeAll(file, new TextEncoder().encode(":begin\n"));
            if (item.parameters) {
                // Convert parameters to JSON string and remove quotes from keys
                const paramsString = removeQuotesFromKeys(JSON.stringify(item.parameters.rows));
                await writeAll(file, new TextEncoder().encode(`:param rows => ${paramsString}\n`));
            }
            await writeAll(file, new TextEncoder().encode(item.statement));
            await writeAll(file, new TextEncoder().encode(";\n:commit\n"));
        }
    }

    // Close the file
    file.close();
}

export function initializeModule(program: Command): void {
    program
        .command("convert-neo4j-json-to-cypher-file")
        .description(
            "Transform all `.json` query files to populate the database into one `.cypher` file. This is useful for large code bases, because populating with a lot of `.json` files can crash the database."
        )
        .option(
            "-w, --workDir <string>",
            "Relative path (from " + PROJECT_ROOT + ") to the working directory of the project. It is where the `alire.toml` of the project is located"
        )
        .option(
            "-g, --gprPath <string>",
            "Relative path (from " + PROJECT_ROOT + ") to the GPR"
        )
        .action(
            async (options: { workDir: string, gprPath: string }) => {
                const gprName = basename(options.gprPath, ".gpr").trim();
                const searchPath = join(PROJECT_ROOT, options.workDir, "." + gprName, COGRALYS_DIR_NAME);
                await processCypherFiles(searchPath, `cogralys-${gprName}.cypher`);
            }
        );
}
