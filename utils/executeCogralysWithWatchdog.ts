// This script is called by `cogralys-bench-util run`
const TIMEOUT = 20000; // In ms

const [jsonString] = Deno.args;
const args: {
    path: string, command: [string | URL,
    string[] | undefined,
    Deno.CommandOptions | undefined,
]} = JSON.parse(jsonString);

Deno.chdir(args.path);

function readEndOfTextFile(fileName: string, nBytesToRead = 21): string {
    const file = Deno.openSync(fileName, {read: true});
    const buf = new Uint8Array(nBytesToRead);
    file.seekSync(0-nBytesToRead, Deno.SeekMode.End);
    file.readSync(buf);
    file.close();
    return new TextDecoder().decode(buf).trim();
}

const filePath = "./log-atgdb.log";
let lastModifiedTime: number | null = null;
let timer: number | undefined;
Deno.writeTextFileSync(filePath, "");
const watcher = Deno.watchFs(filePath);
async function watchFileChanges() {
    resetTimer();

    for await (const event of watcher) {
        if (event.kind === "modify") {
            lastModifiedTime = Date.now();
            resetTimer();
        }
    }
}

async function main() {
    // Start watching the file
    watchFileChanges();

    try {
        const process = new Deno.Command(args.command[0], {
            args: args.command[1],
            stdout: "piped",
            stderr: "piped",
            ...args.command[2],
        });

        // Execute the command
        const prog = process.output();
        prog.then(answer => {
            watcher.close();
            clearTimeout(timer);
            const stdout = new TextDecoder().decode(answer.stdout);
            if (answer.code !== 0) {
                const errorOutput = new TextDecoder().decode(answer.stderr);
                // If the end of the log file is "[Upload_Manager] End", then the program as worked properly
                // but probably has some minor ASIS errors like an inappropriate element
                if (readEndOfTextFile(filePath) !== "[Upload_Manager] End") {
                    throw new Error(errorOutput || stdout);
                } else {
                    console.log(stdout, errorOutput || "");
                }
            } else {
                console.log(stdout);
            }
        }).catch(err => {
            try {
                watcher.close();
                clearTimeout(timer);
            } catch (_) {}

            try {
                throw new Error(new TextDecoder().decode(err.stderr));
            } catch (_) {
                try {
                    throw new Error(new TextDecoder().decode(err));
                } catch (_) {
                    throw new Error(err);
                }
            }
        });
    } catch (error) {
        console.error("Error:", error);
        Deno.exit(1);
    }
}

function resetTimer() {
    clearTimeout(timer);
    timer = setTimeout(() => {
        if (lastModifiedTime === null || Date.now() - lastModifiedTime > TIMEOUT) {
            throw new Error("Possible deadlock.");
        }
    }, TIMEOUT);
}

// Start the main script
main();
