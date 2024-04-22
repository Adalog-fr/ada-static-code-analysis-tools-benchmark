import neo4j from "https://deno.land/x/neo4j_driver_lite@5.18.0/mod.ts";
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { formatDuration } from "../utils.ts";
import * as allRules from "./allRules.ts";
import { RuleType, UnknownRuleError } from "./rules/types/rules.ts";

type AllRulesName = keyof typeof allRules;
const ruleNames: AllRulesName[] = Object.keys(allRules) as AllRulesName[];
const lowercaseRuleNames: string[] = ruleNames.map((ruleName) => ruleName.toLowerCase());

type ruleFile = Array<[string, ({ [key: string]: any })?]>;

// Parse command-line arguments
const program = new Command()
  .option("-t, --timing", "Enable/disable the computation of execution time (global and by rules)", false)
  .option("-h, --host <URI>", "Neo4j URI", "bolt://host.docker.internal:7687")
  .option("-p, --path <path>", "Path to Cypher rules", "/workspaces/bench-source/benchmark-rules")
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
        "-o, --output <string>",
        "Path to the result file",
        "cogralys.result"
    )
    .option(
        "-r, --rulePath <string>",
        "Path to a JSON file that provides all rules to analyse",
        "/workspaces/bench-source/utils/cogralys-cli/rules/types/allRules.json"
    )
  .parse(Deno.args);

const { timing, host, path: directoryPath, username, password, output: resultFile, rulePath } = program;

const ruleFileParsed: ruleFile = JSON.parse(Deno.readTextFileSync(rulePath));
const rulesToControl: RuleType[] = [];
const driver = neo4j.driver(host, neo4j.auth.basic(username, password));

// Open the result file in write mode
const file = await Deno.open(resultFile, { write: true, create: true, truncate: true });

// Create a job list with all configured rules
for (const rule of ruleFileParsed) {
    const [ruleName, ruleParams] = rule;
    if (!lowercaseRuleNames.includes(ruleName.toLowerCase())) {
        throw new UnknownRuleError(`Unknown rule "${ruleName}"`);
    }

    rulesToControl.push(allRules[ruleName as AllRulesName].initialize({
        cypherQueriesPath: directoryPath,
        timing: timing,
        resultFile: file,
        ...ruleParams,
    }))
}

// Execute rules
let totalDuration = 0;
for (const rule of rulesToControl) {
    totalDuration += await rule.executeRule(driver);
}

// Report timing, if enabled
if (timing) {
    console.log("Total duration: ", formatDuration(totalDuration));
}

await driver.close();
