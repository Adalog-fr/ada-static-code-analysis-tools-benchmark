import * as GenerateUnits from "./modules/generate_units.ts";
import * as GenerateAlire from "./modules/generateAlire.ts";
import * as CreateRunCommand from "./modules/runCommandFactory.ts";
import * as CreateRunBenchmarkCommand from "./modules/runBenchmarkCommandFactory.ts";
import * as Run from "./modules/run.ts";
import * as PopulateNeo4jSingle from "./modules/populate-neo4j-single.ts";
import * as CleanNeo4j from "./modules/clean-neo4j.ts";
import * as CountResults from "./modules/countResults.ts";
import * as UpdateCratesDBneo4jComplete from "./modules/update_cratesDBNeo4jComplete.ts";
import * as GenerateSCC from "./modules/generateSCC.ts";
import * as AddProject from "./modules/addProject.ts";
import * as convertNeo4jJsonToCypherFile from "./modules/convert-neo4j-json-to-cypher-file.ts";
import * as aggregateResults from "./modules/aggregateResults.ts";
import * as generateReport from "./modules/generateReport.ts";
import * as compressResults from "./modules/compressResults.ts";
import * as deleteBenchmarkLogs from "./modules/deleteBenchmarkLogs.ts";
import * as importMetrics from "./modules/importMetrics.ts";
import * as generateImportReport from "./modules/generateImportReport.ts";
import * as generateCvReports from "./modules/generateCvReports.ts";
import * as generateIssuedMessageReport from "./modules/issuedMessageReport.ts";

import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";

const program = new Command();

program.version("1.0.0");

GenerateUnits.initializeModule(program);
GenerateAlire.initializeModule(program);
CreateRunCommand.initializeModule(program, {
    commandName: "update-project",
    description: "Run `alr -n update` for all crates.",
    command: ["alr", ["-n", "update"]],
    concurrency: 10
});
Run.initializeModule(program);
UpdateCratesDBneo4jComplete.initializeModule(program);
PopulateNeo4jSingle.initializeModule(program);
CleanNeo4j.initializeModule(program);

CreateRunBenchmarkCommand.initializeModule(program, {
    commandName: "bench-adactl",
    description: "Run AdaControl benchmark", 
    ruleFile: "$PROJECT_ROOT/benchmark-rules/all_rules_in_one_file/_all.aru",
    command: [
        "$PROJECT_ROOT/analysis-tools/Adacontrol/bin/adactl",
        "-f",
        "$ruleFile",
        "-p",
        "%PRJ%",
        "@%UNITS%",
        "-o",
        "%log_prefix%.report",
        "-w",
        "%EXTRA_ARGS%",
    ]
})

CreateRunBenchmarkCommand.initializeModule(program, {
    commandName: "bench-gnatcheck",
    description: "Run GNATcheck benchmark",
    ruleFile: "$PROJECT_ROOT/benchmark-rules/all_rules_in_one_file/gnatcheck.rules",
    command: [
        "gnatcheck",
        " --brief",
        "-q",
        "-t",
        "-l",
        "--show-rule",
        "-o",
        "%log_prefix%.report",
        "--no_objects_dir",
        "-files=%UNITS_BY_FILENAME%",
        "-P%PRJ%",
        "%EXTRA_ARGS%",
        "-rules",
        `-from=$ruleFile`
    ]
})

CreateRunBenchmarkCommand.initializeModule(program, {
    commandName: "gnatmetrics",
    description: "Run GNATmetrics",
    ruleFile: "",
    command: [
        "/opt/gnatsas/bin/gnatmetric",
        "--generate-xml-output",
        "--generate-xml-schema",
        "--wide-character-encoding=8",
        "--files=%UNITS_BY_PATH%",
        "-P%PRJ%",
        "--output-dir=gnatmetric-%PRJ_NAME%",
        "--xml-file-name=gnatmetric-%PRJ_NAME%.xml",
    ]
})

CountResults.initializeModule(program, {
    commandName: "count-results-gnatcheck",
    description: "Count reported results analysis from GNATcheck",
    filePattern: "gnatcheck.report"
})


CountResults.initializeModule(program, {
    commandName: "count-results-adactl",
    description: "Count reported results analysis from AdaControl",
    filePattern: "adactl.report"
})

CreateRunBenchmarkCommand.initializeModule(program, {
    commandName: "bench-cogralys",
    description: "Run Cogralys benchmark",
    isCogralys: true
})

GenerateSCC.initializeModule(program);

AddProject.initializeModule(program);
convertNeo4jJsonToCypherFile.initializeModule(program);
aggregateResults.initializeModule(program);
generateReport.initializeModule(program);
compressResults.initializeModule(program);
deleteBenchmarkLogs.initializeModule(program);
importMetrics.initializeModule(program);
generateImportReport.initializeModule(program);
generateCvReports.initializeModule(program);
generateIssuedMessageReport.initializeModule(program);

program.parse(Deno.args);
