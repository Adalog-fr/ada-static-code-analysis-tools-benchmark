import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { join as joinPath } from "https://deno.land/std/path/mod.ts";
import fg from "npm:fast-glob@3.2.12";
import ProgressBar from "https://deno.land/x/progress@v1.3.8/mod.ts";
import * as log from "https://deno.land/std/log/mod.ts";
import { TaskRunner } from "../lib/taskRunner/taskRunner.ts";
import { alireAndGprPath } from "./explore-neo4j-dirs.ts";

const INPUT_FILENAME = "pathToCratesWithCompleteNeo4jSetup.json";
type taskDataType = { skip: boolean } |
{ path: string, url: string, username: string, password: string } |
{ query: string, url: string, username: string, password: string };
export function initializeModule(program: Command): void {
    program
        .command("populate-neo4j")
        .description(
            "Populate Neo4j database using `" + INPUT_FILENAME + "` as a base path to explore generated `.atgdb` directories that contains Neo4j queries."
        )
        .option(
            "-p, --path <path>",
            "The path to start the exploration.",
            "/workspaces/bench-source/pathToCratesWithCompleteNeo4jSetup.json"
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
        .option("-v, --verbose", "Verbose mode")
        .action(
            async (options: { path: string, outputDir: string, host: string, username: string, password: string, verbose: boolean }) => {
                // const files = (JSON.parse(Deno.readTextFileSync(options.path)) as alireAndGprPath[])
                // .filter(elt => elt.isNeo4jDbFilesFullyComplete)
                // .map(elt => elt.projects).flat()
                // .map((elt) => elt.neo4jDbFilesPath);
                // const nodesPath = fg.sync(files.map((elt: string) => joinPath(elt, "1_*.json")), { onlyFiles: true });
                // const edgesPath = fg.sync(files.map((elt: string) => joinPath(elt, "2_*.json")), { onlyFiles: true });

                // Configure logs
                log.setup({
                    handlers: {
                        console: new log.handlers.ConsoleHandler(options.verbose ? "DEBUG" : "INFO"),

                        file: new log.handlers.FileHandler("INFO", {
                            filename: `/workspaces/bench-source/cogralysRunCommand-populate-neo4j.log`,
                            formatter: "[{levelName}] {msg}",
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

                const taskRunner = new TaskRunner<taskDataType, string>(navigator.hardwareConcurrency, "./workerNeo4jBolt.ts");
                // const taskRunner = new TaskRunner<taskDataType, string>(navigator.hardwareConcurrency, "./workerHttpToNeo4j.ts");
                let completed = 0;

                let nodeId = 0;
                let edgeId = 0;
                const nodesDependencies: string[] = [];
                const edgesDependencies: string[] = [];

                // taskRunner.addTask("init", [], {
                //     // skip: true,
                //     query: JSON.stringify({ "statements": [{ "statement": "CREATE CONSTRAINT uniqueNodeIds IF NOT EXISTS FOR (node:Element) REQUIRE (node.node_id) IS UNIQUE;" }, { "statement": "CREATE CONSTRAINT uniqueImportLabel IF NOT EXISTS FOR (node:`UNIQUE IMPORT LABEL`) REQUIRE (node.node_id) IS UNIQUE;" }, { "statement": "CALL db.awaitIndexes(30);" }] }),
                //     url: options.host,
                //     username: options.username,
                //     password: options.password,
                // });

                // for (const path of nodesPath) {
                //     const taskId = `node_${nodeId++}`;
                //     nodesDependencies.push(taskId);
                //     taskRunner.addTask(taskId, ["init"], {
                //         path,
                //         url: options.host,
                //         username: options.username,
                //         password: options.password,
                //     });
                // }

                // taskRunner.addTask("nodes", nodesDependencies, {
                //     skip: true
                // });

                // for (const path of edgesPath) {
                //     const taskId = `edge_${edgeId++}`;
                //     edgesDependencies.push(taskId);
                //     taskRunner.addTask(taskId, ["nodes"], {
                //         path,
                //         url: options.host,
                //         username: options.username,
                //         password: options.password,
                //     });
                // }

                // taskRunner.addTask("edges", edgesDependencies, {
                //     skip: true
                // });

                // Order of nodes
                // taskRunner.addTask("finalize_0", ["edges"], {
                taskRunner.addTask("finalize_0", [], {
                    query: JSON.stringify({ "statements": [{ "statement": "MATCH (p)<-[r:IS_ENCLOSED_IN]-(e:Element)\n  WHERE r.index IS NULL\nWITH p, e, r\n  ORDER BY e.filename, e.line, e.column\nCALL {\nWITH p\nMATCH (p)<-[r2:IS_ENCLOSED_IN]-(:Element)\n  WHERE r2.index IS NOT NULL\nRETURN count(r2) as nb\n}\nWITH p, nb, collect(distinct r) as rels\nFOREACH (n IN rels | SET n.index = apoc.coll.indexOf(rels, n) + nb + 1);" }] }),
                    url: options.host,
                    username: options.username,
                    password: options.password,
                });

                // Call graph
                taskRunner.addTask("finalize_1", ["finalize_0"], {
                    query: JSON.stringify({ "statements": [{ "statement": "MATCH (callerSpec)<-[:CORRESPONDING_SPECIFICATION]-(caller:A_FUNCTION_BODY_DECLARATION|A_FUNCTION_BODY_STUB|A_PROCEDURE_BODY_DECLARATION|A_PROCEDURE_BODY_STUB|A_PROTECTED_BODY_DECLARATION|A_PROTECTED_BODY_STUB|A_TASK_BODY_DECLARATION|A_TASK_BODY_STUB|AN_ENTRY_BODY_DECLARATION|AN_EXPRESSION_FUNCTION_DECLARATION)<-[:IS_ENCLOSED_IN*]-(subProgCall:A_PROCEDURE_CALL_STATEMENT|A_FUNCTION_CALL)<-[:IS_ENCLOSED_IN]-(id:AN_IDENTIFIER)-[:CORRESPONDING_NAME_DEFINITION]->(:A_DEFINING_IDENTIFIER)-[:IS_ENCLOSED_IN]->()-[:CORRESPONDING_SPECIFICATION]->(subProg)\nMERGE (callerSpec)-[r:CALLING]->(subProg)\nON CREATE\n  SET r.nbCall = 1\nON MATCH\n  SET r.nbCall = r.nbCall + 1" }] }),
                    url: options.host,
                    username: options.username,
                    password: options.password,
                });

                // Ancestor / progenitors
                taskRunner.addTask("finalize_2", ["finalize_1"], {
                    query: JSON.stringify({ "statements": [{ "statement": `MATCH (e:A_DERIVED_RECORD_EXTENSION_DEFINITION|AN_INTERFACE_TYPE_DEFINITION|A_DERIVED_TYPE_DEFINITION|A_FORMAL_DERIVED_TYPE_DEFINITION|A_FORMAL_INTERFACE_TYPE_DEFINITION|A_PROTECTED_DEFINITION|A_TASK_DEFINITION|A_PRIVATE_EXTENSION_DEFINITION)\nMATCH (enclosingE)-[:CORRESPONDING_TYPE_DECLARATION_VIEW]->(e)\n\nOPTIONAL MATCH (e)<-[:IS_ENCLOSED_IN*]-(:AN_IDENTIFIER)-[:CORRESPONDING_NAME_DEFINITION]->()-[:IS_ENCLOSED_IN]->()-[:CORRESPONDING_TYPE_DECLARATION_VIEW]->(parent)\nOPTIONAL MATCH (enclosingParent)-[:CORRESPONDING_TYPE_DECLARATION_VIEW]->(parent)\n\nCALL apoc.do.when(\n  parent IS NOT NULL AND 'A_SUBTYPE_INDICATION' IN labels(parent),\n  'MATCH (enclosingParent)-[:CORRESPONDING_FIRST_SUBTYPE]->(realParent)-[:CORRESPONDING_TYPE_DECLARATION_VIEW]->(realTypeDecl)\n   CALL apoc.do.when(realParent IS NOT NULL AND \'AN_INTERFACE_TYPE_DEFINITION\' IN labels(realTypeDecl), \n     "CALL apoc.create.relationship(enclosingParent,\\\"IS_PROGENITOR_OF\\\",{},enclosingE) YIELD rel AS relParent RETURN relParent, enclosingParent", \n     "CALL apoc.create.relationship(enclosingParent,\\\"IS_ANCESTOR_OF\\\",{},enclosingE) YIELD rel AS relParent RETURN relParent, enclosingParent", \n     {enclosingE:enclosingE,enclosingParent:realParent, realTypeDecl:realTypeDecl}) yield value as ancestors RETURN ancestors', \n  'RETURN NULL',\n  {enclosingE:enclosingE,enclosingParent:enclosingParent}) yield value as ancestors\n\nCALL apoc.do.when(\n  parent IS NOT NULL AND 'AN_INTERFACE_TYPE_DEFINITION' IN labels(parent),\n  'CALL apoc.create.relationship(enclosingParent,"IS_PROGENITOR_OF",{},enclosingE) YIELD rel AS relParent RETURN relParent, enclosingParent', \n  'RETURN NULL',\n  {enclosingE:enclosingE, enclosingParent:enclosingParent}) yield value as interfaces\n\nCALL apoc.do.when(\n  parent IS NOT NULL AND none(label in labels(parent) WHERE label IN ['AN_INTERFACE_TYPE_DEFINITION', 'A_SUBTYPE_INDICATION']),\n  'CALL apoc.create.relationship(enclosingParent,"IS_ANCESTOR_OF",{},enclosingE) YIELD rel AS relParent RETURN relParent, enclosingParent',\n  "RETURN NULL",\n  {enclosingE:enclosingE,enclosingParent:enclosingParent}) yield value as ancestor\n\nRETURN enclosingE,\n       ancestors,\n       interfaces,\n       ancestor\nORDER BY enclosingE.filename, enclosingE.line, enclosingE.column\n` }] }),
                    url: options.host,
                    username: options.username,
                    password: options.password,
                });

                // Clean label
                taskRunner.addTask("finalize_label", ["finalize_2"], {
                    query: JSON.stringify({ "statements": [{ "statement": "MATCH (n:`UNIQUE IMPORT LABEL`) REMOVE n:`UNIQUE IMPORT LABEL`;" }] }),
                    url: options.host,
                    username: options.username,
                    password: options.password,
                });

                // Clean constraint
                taskRunner.addTask("finalize_constraint", ["finalize_label"], {
                    query: JSON.stringify({ "statements": [{ "statement": "DROP CONSTRAINT uniqueImportLabel IF EXISTS;" }] }),
                    url: options.host,
                    username: options.username,
                    password: options.password,
                });

                const title = "populating db:";
                const total = taskRunner.getNbTasks();
                const progress: ProgressBar = new ProgressBar({
                    title,
                    total,
                });


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
                } catch (e) {
                    taskRunner.terminate();
                    console.error(e);
                    Deno.exit(1);
                }

            }
        );
}
