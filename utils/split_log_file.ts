import { ensureDirSync } from "jsr:@std/fs@^1.0.2";
import { join } from "jsr:@std/path@^0.225.1";

function splitLogFile(inputFile: string): void {
  // Create 'allLogs' folder if it doesn't exist
  const outputFolder = "allLogs";
  ensureDirSync(outputFolder);

  // Read the input log file
  const logContent = Deno.readTextFileSync(inputFile);

  // Regular expression pattern to find project names and their log chunks
  const pattern = /\[ERROR\] '(.*?)' in(.*?)(?=\[ERROR\] '|$)/gs;

  // Dictionary to store log chunks for each project
  const projectLogs: Record<string, string[]> = {};

  // Find all matches in the log content and group log chunks by project name
  for (const match of logContent.matchAll(pattern)) {
    const projectName = match[1];
    const logChunk = match[2];
    if (!projectLogs[projectName]) {
      projectLogs[projectName] = [];
    }
    projectLogs[projectName].push(logChunk);
  }

  // Write grouped log chunks to separate files
  for (const [projectName, logChunks] of Object.entries(projectLogs)) {
    // Create a file name based on the project name
    const outputFile = join(outputFolder, `${projectName.replace(/ /g, '_')}.log`);

    // Combine all log chunks for the project
    const projectLogContent = logChunks.map(chunk => `[ERROR] '${projectName}' in${chunk}`).join('');

    // Write the combined log content to the output file
    Deno.writeTextFileSync(outputFile, projectLogContent);
  }
}

////////////////////
// Main execution //
////////////////////

// Input log file name
const inputLogFile = "cogralysRunCommand-run.log";

// Call the function to split the log file
splitLogFile(inputLogFile);
