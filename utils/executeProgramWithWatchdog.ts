// Import required modules from Deno standard library and external sources
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";

// Define type for command arguments
type commandArg = {
    path: string, command: [string | URL,
    string[] | undefined,
    Deno.CommandOptions | undefined,
]};

// Set default timeout duration in milliseconds
const DEFAULT_TIMEOUT = 180_000; // 3 min

// Parse integer values with fallback to previous value
function parseIntCustom(value: string, previous: number): number {
    const val = parseInt(value);
    return isNaN(val) ? previous : val;
}

// Parse JSON string into commandArg type
function parseJSON(value: string): commandArg {
    return JSON.parse(value);
}

// Configure command-line argument parsing
const program = new Command()
    .option("-t, --timeout <string>", "Timeout before killing the process", parseIntCustom, DEFAULT_TIMEOUT)
    .option("-f, --file <path>", "Path to the log file to watch")
    .option("-c, --cmd <json>", "Command configuration in JSON", parseJSON)
    .option("--end-of-file-check <string>", "Success verification string")
    .parse(Deno.args);

// Extract command-line arguments
const timeout: number = program.timeout;
const command: commandArg = program.cmd;
const endOfFileCheckStr = program.endOfFileCheck.toLocaleLowerCase();

// Change current working directory
Deno.chdir(command.path);

// Read the end portion of a text file
function readEndOfTextFile(fileName: string, nBytesToRead = 21): string {
    const file = Deno.openSync(fileName, { read: true });
    const buf = new Uint8Array(nBytesToRead);
    file.seekSync(0 - nBytesToRead, Deno.SeekMode.End);
    file.readSync(buf);
    file.close();
    return new TextDecoder().decode(buf).trim();
}

// Initialize file monitoring variables
let lastModifiedTime: number | null = null;
let timer: number | undefined;

// Create/clear the output file
Deno.writeTextFileSync(program.file, "");

// Create file system watcher
const watcher = Deno.watchFs(program.file);

// Monitor file changes
async function watchFileChanges() {
    resetTimer();
    for await (const event of watcher) {
        if (event.kind === "modify") {
            lastModifiedTime = Date.now();
            resetTimer();
        }
    }
}

// Main execution function
async function main() {
    watchFileChanges();

    try {
        // Initialize the process with output redirection
        const process = new Deno.Command(command.command[0], {
            args: command.command[1],
            stdout: "piped",
            stderr: "piped",
            ...command.command[2],
        }).spawn();

        // Create separate file handles for stdout and stderr
        const stdoutFile = await Deno.open(program.file, { write: true, append: true });
        const stderrFile = await Deno.open(program.file, { write: true, append: true });

        // Create separate streams for stdout and stderr
        const stdoutStream = process.stdout.tee();
        const stderrStream = process.stderr.tee();

        // Redirect outputs to file and console
        await Promise.all([
            stdoutStream[0].pipeTo(stdoutFile.writable),
            stderrStream[0].pipeTo(stderrFile.writable),
        ]);

        // Wait for process completion
        const { success, code, signal } = await process.status;
        console.log(`Process finished - success: ${success} code: ${code} signal: ${signal}`);

        // Cleanup resources
        watcher.close();
        clearTimeout(timer);

        // Check end-of-file condition if specified
        if (code !== 0 && endOfFileCheckStr) {
            const fileContent = readEndOfTextFile(program.file, endOfFileCheckStr.length * 4);
            if (!fileContent.toLowerCase().includes(endOfFileCheckStr)) {
                throw new Error("Process failed and end-of-file check failed");
            }
        }
    } catch (error) {
        console.error("Error:", error);
        watcher.close();
        clearTimeout(timer);
        Deno.exit(1);
    }
}

// Timer reset function for deadlock detection
function resetTimer() {
    clearTimeout(timer);
    timer = setTimeout(() => {
        if (lastModifiedTime === null || Date.now() - lastModifiedTime > timeout) {
            throw new Error("Possible deadlock detected");
        }
    }, timeout);
}

// Execute main function
main();
