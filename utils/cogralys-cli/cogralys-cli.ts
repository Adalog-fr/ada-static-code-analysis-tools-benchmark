import { join } from "jsr:@std/path@^0.225.1";
import neo4j from "npm:neo4j-driver@5.27.0";
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { formatDuration } from "../utils.ts";
import * as allRules from "./allRules.ts";
import { RuleType, UnknownRuleError } from "./rules/types/rules.ts";
import { PROJECT_ROOT } from "../../config.ts";
import { ruleNames, AllRulesName, CogralysOutputType, AllRulesNameLC } from "./cogralysCliTypes.ts";

const lowercaseRuleNames: string[] = ruleNames.map((ruleName) => ruleName.toLowerCase());

type ruleFile = Array<[string, ({ [key: string]: any })?]>;

// Parse command-line arguments
const program = new Command()
  .option("-t, --timing", "Enable/disable the computation of execution time (global and by rules)", false)
  .option("-h, --host <URI>", "Neo4j URI", "bolt://host.docker.internal:7687")
  .option("-p, --path <path>", "Path to Cypher rules", join(PROJECT_ROOT, "benchmark-rules"))
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
        "cogralys.report"
    )
    .option(
        "-u, --unitListFile <string>",
        "Path to the unit file list",
    )
    .option(
        "-r, --rulePath <string>",
        "Path to a JSON file that provides all rules to analyse",
        join(PROJECT_ROOT, "utils/cogralys-cli/rules/types/allRules.json")
    )
  .parse(Deno.args);

const { timing, host, path: directoryPath, username, password, output: resultFile, rulePath, unitListFile } = program;

if (!unitListFile || unitListFile.length === 0) {
    throw new Error("Missing option 'unitListFile'");
} else if (!unitListFile.endsWith(".units")) {
    throw new Error(`Expected ".units" file for unit list file.`);
}

const unitList = Deno.readTextFileSync(unitListFile).split("\n").map(elt => elt.trim());

const ruleFileParsed: ruleFile = JSON.parse(Deno.readTextFileSync(rulePath));
const rulesToControl: RuleType[] = [];

const driver = neo4j.driver(host, neo4j.auth.basic(username, password));
const session = driver.session({ defaultAccessMode: neo4j.session.READ });

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
        unitList,
        ...ruleParams,
    }))
}

const result: CogralysOutputType = {
  result: {
    abort_statements: {
      found: [],
      analysisTime: 0
    },
    abstract_type_declarations: {
      found: [],
      analysisTime: 0
    },
    blocks: {
      found: [],
      analysisTime: 0
    },
    constructors: {
      found: [],
      analysisTime: 0
    },
    enumeration_representation_clauses: {
      found: [],
      analysisTime: 0
    },
    renamings: {
      found: [],
      analysisTime: 0
    },
    slices: {
      found: [],
      analysisTime: 0
    },
    too_many_parents: {
      found: [],
      analysisTime: 0
    },
    variable_usage: {
      found: [],
      analysisTime: 0
    }
  },
  totalAnalysisTime: 0
}

function hasRuleName(constructor: Function): constructor is (new (...args: any[]) => RuleType) & { ruleName: string } {
    return 'ruleName' in constructor;
}

// Execute rules
let totalDuration = 0;
let totalNbFound = 0;
for (const rule of rulesToControl) {
    totalDuration += await rule.executeRule(session);
    if (hasRuleName(rule.constructor)) {
        const ruleName: AllRulesName = rule.constructor.ruleName as AllRulesName;
        result.result[ruleName.toLocaleLowerCase() as AllRulesNameLC] = rule.getReport();
        totalNbFound += result.result[ruleName.toLocaleLowerCase() as AllRulesNameLC].nbFound;
    }
}

// Report timing, if enabled
if (timing) {
    console.log("Total duration: ", formatDuration(totalDuration));
    result.totalAnalysisTime = totalDuration;
    result.totalNbFound = totalNbFound;
}

Deno.writeTextFileSync(resultFile + ".json", JSON.stringify(result, null, 2));

await driver.close();
file.close();
