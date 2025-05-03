import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { TaskRunner } from "../lib/taskRunner/taskRunner.ts";

type taskDataType = { skip: boolean } |
{ path: string, url: string, username: string, password: string } |
{ query: string, url: string, username: string, password: string };

export function initializeModule(program: Command): void {
    program
        .command("clean-neo4j")
        .description(
            "Clean a Neo4j database by removing all data (nodes and edges) and all indexes and constraints"
        )
        .option(
            "-h, --host <protocol:host:port>",
            "Host path used to communicate with the database, using HTTP requests. If you use this script inside a Docker container, and have Neo4j on your host, you can use `bolt://host.docker.internal:7687`.",
            "bolt://127.0.0.1:7687"
        )
        .option(
            "--username <string>",
            "Username used to log in to Neo4j database",
            "neo4j"
        )
        .option(
            "--password <string>",
            "Password used to log in to Neo4j database",
            "auieauie"
        )
        .action(
            async (options: { host: string, username: string, password: string }) => {
                const taskRunner = new TaskRunner<taskDataType, string>(navigator.hardwareConcurrency, "./workerNeo4jBolt.ts");

                taskRunner.addTask("clean", [], {
                    query: JSON.stringify({ "statements": [
                        { "statement": "CALL apoc.periodic.iterate( 'MATCH (n) RETURN n', 'DETACH DELETE n', {batchSize:100000} )" },
                        { "statement": "CALL apoc.schema.assert({}, {})" },
                    ] }),
                    url: options.host,
                    username: options.username,
                    password: options.password,
                });

                taskRunner.postTaskCb = (task) => {
                    const result: string = task.result?.output || task.result;

                    if (task.status === "failure") {
                        console.error(`'${task.id}' in '${task.task.data?.path || task.task.data?.query}' getting error: ${result}`);
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
        );
}
