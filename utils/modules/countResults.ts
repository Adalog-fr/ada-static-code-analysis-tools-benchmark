import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { join } from "https://deno.land/std/path/mod.ts";
import fg from "npm:fast-glob@3.2.12";

const regex = new RegExp('^[\\/\\w\\-\\.]+:\\d+:\\d+');

// Function to count lines matching the regex in a file
const countLines = (filename: string): number => {
    let count = 0;
    const file = Deno.readTextFileSync(filename);
    for (const line of file.split(/\r?\n/)) {
      if (regex.test(line.trim())) {
        count++;
      }
    }
    return count;
  };

export function initializeModule(program: Command, settings: {
    commandName: string,
    description: string,
    filePattern: string,
}): void {
    program
        .command(settings.commandName)
        .description(
            settings.description
        )
        .option(
            "-p, --path <path>",
            "Path to start the exploration of results file.",
            "/workspaces/bench-source/src"
        )
        .option(
            "-f, --filename <path>",
            "Filename or pattern of file to be considered as result file",
            settings.filePattern
        )
        .action((options: { path: string, filename: string }) => {
            const reports = fg.sync(join(options.path, "**", options.filename), { onlyFiles: true, dot: true });

            let numberOfReportedIssues = 0;
            for (const file of reports) {
                numberOfReportedIssues += countLines(file);
            }
            console.log("Number of reports: ", reports.length);

            console.log("Number of reported issues: ", numberOfReportedIssues);
        });
}
