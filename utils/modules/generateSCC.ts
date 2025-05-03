import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { dirname, basename, join } from "jsr:@std/path@^0.225.1";
import { exec } from "../utils.ts";

export function initializeModule(program: Command): void {
    program
        .command("generate-scc-metrics <file>")
        .description(
            "Generate SCC metrics (number of lines of code, complexity, number of files, etc.). The path is the corresponding `project.units_by_path` of the project."
        )
        .action(
            (file: string) => {
                const resultFilePath = join(dirname(file), basename(file, ".units_by_path") + "_scc-metrics.json");
                const content = Deno.readTextFileSync(file).split("\n");
                if (content[0].startsWith("/")) {
                    console.error(`Ignore '${file}' because it uses absolute path from the root of the system.`);
                    Deno.exit(1);
                }
                console.log("Process: ", file);

                const result = exec("scc", [
                    "--no-cocomo",
                    "--by-file",
                    "-f",
                    "json",
                    "--include-ext",
                    "ads,adb,ada",
                    ...content
                ],
                    {
                        cwd: join(Deno.cwd(), dirname(file))
                    });

                if (!result.success) {
                    console.error(result.output);
                    Deno.exit(1);
                }

                const metrics = JSON.parse(result.output)[0];

                Deno.writeTextFileSync(resultFilePath, JSON.stringify(metrics, null, 2));
            }
        );
}
