import { existsSync } from "jsr:@std/fs@1.0.8";

// Usage: checkErrorInLog.ts /path/to/file.time

// Define error types for the analysis
type ErrorKind = "ASIS_BUG" | "ASIS_USAGE" | "GNAT_BUG" | "undefined";

// Define structure for the error array
type ErrorArray = { errorKind: ErrorKind, error: string, textPosition: string, diagnosis: string }[];

// Function to analyze a log file and find the first line matching the criteria
function analyzeLogFile(filePath: string): ErrorArray {
    // Read the file contents
    const fileContent = Deno.readTextFileSync(filePath);

    // Split the content into lines
    const lines = fileContent.split('\n');

    // Regular expression to match the pattern
    const pattern = /^0x\w+\s/;
    const DiagnosisPattern = /^Diagnosis\s*:\s*/;
    const textPositionPattern = /^text position\s*:\s*/;

    const result: ErrorArray = [];
    let errorKind: ErrorKind = "undefined";
    let diagnosis = "";
    let textPosition = "";

    // Iterate through each line
    for (const line of lines) {
        // Check if the line starts with the pattern
        if (errorKind !== "undefined") {
            if (pattern.test(line)) {
                // Extract the part after the pattern
                const afterPattern = line.replace(pattern, '');

                // Check if it doesn't start with a4g, asis, or Thick_Queries
                if (!afterPattern.startsWith('a4g') &&
                    !afterPattern.startsWith('asis') &&
                    !afterPattern.startsWith('Thick_Queries') &&
                    !pattern.test(afterPattern)) {
                    // Return the matching line
                    result.push({errorKind, error: afterPattern, diagnosis, textPosition });
                    errorKind = "undefined";
                    diagnosis = "";
                    textPosition = "";
                }
            } else if (!diagnosis.length && DiagnosisPattern.test(line)) {
                // Extract the part after the pattern
                diagnosis = line.replace(DiagnosisPattern, '');
            } else if (!textPosition.length && textPositionPattern.test(line)) {
                // Extract the part after the pattern
                textPosition = line.replace(textPositionPattern, '');
            }
        } else if (line.includes("ASIS BUG DETECTED")) {
            errorKind = "ASIS_BUG";
        } else if (line.includes("GNAT BUG DETECTED")) {
            errorKind = "GNAT_BUG";
        } else if (line.includes("Phase: Processing")) {
            errorKind = "ASIS_USAGE";
        }
    }

    return result;
}

//////////
// MAIN //
//////////

// Get the first command line argument
const path = Deno.args[0];

// Check if path argument is provided
if (!path) {
    console.error("Error: No path provided");
    Deno.exit(1);
}

// Check if file exists and has .time extension
if (!path.endsWith('.time') || !existsSync(path)) {
    console.error(`Error: Invalid file path or not a .time file: ${path}`);
    Deno.exit(1);
}

// Read the first line of the .time file
const timeContent = Deno.readTextFileSync(path);
const timeLines = timeContent.split('\n');
const firstLine = timeLines[0];

// Check if first line indicates non-zero status
if (!firstLine.startsWith('Command exited with non-zero status')) {
    Deno.exit(0);
}

// Convert .time path to .log path
const logPath = path.replace(/\.time$/, '.log');

// Check if .log file exists
if (!existsSync(logPath)) {
    console.error(`Error: Log file not found at path: ${logPath}`);
    Deno.exit(1);
}

// Analyze the log file and get results
const results = analyzeLogFile(logPath);

// Check for non-undefined errors
const hasErrors = results.some(result => result.errorKind !== "undefined");

// Process results based on error presence
if (hasErrors) {
    console.warn("Possible missing control due to ASIS or GNAT bug");
    Deno.exit(2);
}

// Remove the first line and write back to the file
const newContent = timeLines.slice(1).join('\n');
Deno.writeTextFileSync(path, newContent);

Deno.exit(0);
