import fg from "npm:fast-glob@3.3.2";

type ErrorKind = "ASIS_BUG" | "ASIS_USAGE" | "GNAT_BUG" | "undefined";
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

// Main function to process all .log files in a directory
function processLogFiles(directoryPath: string) {
    // Iterate through all files in the directory
    const globPattern = `${directoryPath}${directoryPath.endsWith("/") ? "" : "/"}**/*.log`;

    const paths = fg
        .sync(globPattern, { onlyFiles: true })
        .sort((a, b) => a.localeCompare(b));
    for (const entry of paths) {
        // Analyze the log file
        const results: ErrorArray = analyzeLogFile(entry);

        // Print the result
        if (results.length) {
            for (const result of results) {
                if (!(result.error in errorLogs[result.errorKind])) {
                    errorLogs[result.errorKind][result.error] = {
                        diagnosis: result.diagnosis,
                        textPosition: result.textPosition,
                        path: []
                    };
                }
                errorLogs[result.errorKind][result.error].path.push(entry)
            }
        }
    }
}

// Get the directory path from command line arguments
const directoryPath = Deno.args[0];
const errorLogs: { [key in ErrorKind]: { [key: string]: { diagnosis: string, textPosition: string, path: string[] } } } = {
    "ASIS_BUG": {},
    "ASIS_USAGE": {},
    "GNAT_BUG": {},
    "undefined": {}
};

// Check if a directory path is provided
if (!directoryPath) {
    console.error('Please provide a directory path as an argument.');
    Deno.exit(1);
}

// Process the log files
if (directoryPath.endsWith(".log")) {
    const results: ErrorArray = analyzeLogFile(directoryPath);

    // Print the result
    if (results.length) {
        for (const result of results) {
            if (!(result.error in errorLogs[result.errorKind])) {
                errorLogs[result.errorKind][result.error] = {
                    diagnosis: result.diagnosis,
                    textPosition: result.textPosition,
                    path: []
                };
            }
            errorLogs[result.errorKind][result.error].path.push(directoryPath)
        }
    }
} else {
    processLogFiles(directoryPath);
}

let nbTotalErrors = 0;
// Display the result
for (const [errorKind, errors] of Object.entries(errorLogs)) {
    console.log(`# ${errorKind}\n`);
    let nbErrors = 0;
    for (const [error, errorInfo] of Object.entries(errors)) {
        console.log(`## ${error}\n`);
        console.log(`Diagnosis: `, errorInfo.diagnosis);
        console.log(`Text position: `, errorInfo.textPosition);
        console.log(`Found in:`);

        for (const file of errorInfo.path) {
            console.log(`\t=> ${file}`);
        }
        console.log("");
        nbErrors++;
        nbTotalErrors++;
    }
    console.log(`Number of distinct errors for ${errorKind}: `, nbErrors, "\n\n**********\n");
}

console.log("Total number of distinct errors: ", nbTotalErrors);
