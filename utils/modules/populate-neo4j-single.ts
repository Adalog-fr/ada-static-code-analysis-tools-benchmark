import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { basename, join } from "jsr:@std/path@^0.225.1";
import fg from "npm:fast-glob@3.3.2";
import ProgressBar from "https://deno.land/x/progress@v1.3.8/mod.ts";
import * as log from "jsr:@std/log@^0.224.6";
import { TaskRunner } from "../lib/taskRunner/taskRunner.ts";
import { PROJECT_ROOT, COGRALYS_DIR_NAME } from "../../config.ts";
import { exec } from "../utils.ts";

type taskDataType = { skip: boolean } |
{ path: string, url: string, username: string, password: string } |
{ query: string, url: string, username: string, password: string };

type importMode = "json" | "cypher";

type optionType = { workDir: string, gprPath: string, host: string, username: string, password: string, mode: importMode, logPath: string, verbose: boolean };

async function populateWithJson(options: optionType) {
    const taskRunner = new TaskRunner<taskDataType, string>(navigator.hardwareConcurrency, "./workerNeo4jBolt.ts");
    let completed = 0;

    const gprDir = join(PROJECT_ROOT, options.workDir, "." + basename(options.gprPath, ".gpr").trim(), COGRALYS_DIR_NAME);
    const nodesPath = fg.sync([join(gprDir, "1_*.json")], { onlyFiles: true });
    const edgesPath = fg.sync([join(gprDir, "2_*.json")], { onlyFiles: true });
    const finalizationPath = fg.sync([join(gprDir, "3_*.json")], { onlyFiles: true }).sort((a, b) => a.localeCompare(b));

    taskRunner.addTask("init", [], {
        query: JSON.stringify({ "statements": [
            { "statement": "CREATE CONSTRAINT uniqueNodeIds IF NOT EXISTS FOR (node:Element) REQUIRE (node.node_id) IS UNIQUE;" },
            { "statement": "CREATE CONSTRAINT uniqueImportLabel IF NOT EXISTS FOR (node:`UNIQUE IMPORT LABEL`) REQUIRE (node.node_id) IS UNIQUE;" },
            // { "statement": "CALL db.awaitIndexes(30);" }
        ] }),
        url: options.host,
        username: options.username,
        password: options.password,
    });

    const nodesTasks : string[] = [];
    for (const path of nodesPath) {
        nodesTasks.push(basename(path, ".json"));
        taskRunner.addTask(nodesTasks[nodesTasks.length - 1], ["init"], {
            path,
            url: options.host,
            username: options.username,
            password: options.password,
        });
    }

    // Empty task for synchronization barrier
    taskRunner.addTask("nodes", nodesTasks, { skip: true });

    const edgesTasks : string[] = [];
    for (const path of edgesPath) {
        edgesTasks.push(basename(path, ".json"));
        taskRunner.addTask(edgesTasks[edgesTasks.length - 1], ["nodes"], {
            path,
            url: options.host,
            username: options.username,
            password: options.password,
        });
    }

    // Empty task for synchronization barrier
    taskRunner.addTask("edges", edgesTasks, { skip: true });

    // Finalization tasks
    let previousTaskId = "edges";

    for (const path of finalizationPath) {
        const taskId = basename(path, ".json");
        taskRunner.addTask(taskId, [previousTaskId], {
            path,
            url: options.host,
            username: options.username,
            password: options.password,
        });
        previousTaskId = taskId;
    }

    const title = "populating db:";
    const total = taskRunner.getNbTasks();
    const progress: ProgressBar = new ProgressBar({
        title,
        total,
        display: ":title :percent :time :completed/:total"
    });

    progress.render(0);

    taskRunner.preTaskCb = (task) => {
        log.info(`Running '${task.id}' in '${task.data?.path}'`);
    };

    taskRunner.postTaskCb = (task) => {
        if (task.id === "init") {
            log.info("DB initialized");
        }

        progress.render(completed++);

        const result: string = task.result?.output || task.result;

        if (task.status === "failure") {
            log.error(`'${task.id}' in '${task.task.data?.path || task.task.data?.query}' getting error: ${result}`);
        } else {
            log.info(`End of '${task.id}'`);
        }
    }

    try {
        await taskRunner.run();
        taskRunner.terminate();
        Deno.exit(0);
    } catch (e) {
        taskRunner.terminate();
        console.error(e);
        Deno.exit(1);
    }
}

function populateWithCypher(options: optionType) {
    const gprName = basename(options.gprPath, ".gpr").trim();
    const dbFilePath = join(PROJECT_ROOT, options.workDir, "." + gprName, COGRALYS_DIR_NAME, `cogralys-${gprName}.cypher`);

    const result = exec("cypher-shell", [
        "-a", options.host,
        "-u", options.username,
        "-p", options.password,
        "-f", dbFilePath
    ])

    if (!result.success) {
        console.error(result.output);
        Deno.exit(1);
    }
}

function checkMode(value: string) {
    if (!["json", "cypher"].includes(value)) {
        throw new Error("Unexpected value for parameter mode: " + value);
    }
    return value;
}

export function initializeModule(program: Command): void {
    program
        .command("populate-neo4j-single")
        .description(
            "Populate Neo4j database with data from a specific GPR project"
        )
        .option(
            "-w, --workDir <string>",
            "Relative path (from " + PROJECT_ROOT + ") to the working directory of the project. It is where the `alire.toml` of the project is located"
        )
        .option(
            "-g, --gprPath <string>",
            "Relative path (from " + PROJECT_ROOT + ") to the GPR file"
        )
        .option(
            "-h, --host <host:port>",
            "Host path used to communicate with the database, using HTTP requests. If you use this script inside a Docker container, and have Neo4j on your host, you can use `host.docker.internal`.",
            "127.0.0.1:7474"
        )
        .option(
            "--username <string>",
            "Username used to login to Neo4j database",
            "neo4j"
        )
        .option(
            "--password <string>",
            "Password used to login to Neo4j database",
            "auieauie"
        )
        .option(
            "-m, --mode <json|cypher>",
            "Set the import mode. 'json' to import data using `.json` files, or 'cypher' to import using a single `.cypher` file.",
            checkMode,
            "json"
        )
        .option(
            "-l, --logPath <string>",
            "Path and name of the log file",
            join(PROJECT_ROOT, `cogralysRunCommand-populate-neo4j-single.log`)
        )
        .option("-v, --verbose", "Verbose mode")
        .action(
            async (options: optionType) => {
                // Configure logs
                log.setup({
                    handlers: {
                        console: new log.ConsoleHandler(options.verbose ? "DEBUG" : "INFO"),

                        file: new log.FileHandler("INFO", {
                            filename: options.logPath,
                            formatter: (record) => `>[${record.levelName}] ${record.msg}`,
                            mode: "w"
                        }),
                    },
                    loggers: {
                        default: {
                            level: "DEBUG",
                            handlers: ["file"],
                        },
                    },
                });

                switch(options.mode) {
                    case "json":
                        await populateWithJson(options);
                        break;
                    case "cypher":
                        populateWithCypher(options);
                        break;
                }
            }
        );
}
