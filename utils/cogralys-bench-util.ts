import * as ExtractDependenciesInGPR from "./modules/extract_dependencies_in_gpr.ts";
import * as ExtractDependenciesInGPR2 from "./modules/extract_dep_gpr2.ts";
import * as GenerateUnits from "./modules/generate_units.ts";
import * as GetAlireDependencies from "./modules/get_alire_dependencies.ts";
import * as GenerateBuildPath from "./modules/generateBuildPath.ts";
import * as GenerateAlire from "./modules/generateAlire.ts";
import * as CreateRunCommand from "./modules/runCommandFactory.ts";
import * as CreateRunBenchmarkCommand from "./modules/runBenchmarkCommandFactory.ts";
import * as GenerateEnv from "./modules/generate_env.ts";
import * as Run from "./modules/run.ts";
import * as CogralysRun from "./modules/cogralysRun.ts";
import * as PopulateNeo4j from "./modules/populate-neo4j.ts";
import * as PopulateNeo4jSingle from "./modules/populate-neo4j-single.ts";
import * as CleanNeo4j from "./modules/clean-neo4j.ts";
import * as CountResults from "./modules/countResults.ts";
import * as UpdateCratesDBneo4jComplete from "./modules/update_cratesDBNeo4jComplete.ts";
import * as UpdateAllCratesDBneo4jComplete from "./modules/update_all_cratesDBNeo4jComplete.ts";
import * as GenerateSCC from "./modules/generateSCC.ts";
import * as AddProject from "./modules/addProject.ts";
import * as convertNeo4jJsonToCypherFile from "./modules/convert-neo4j-json-to-cypher-file.ts";
import * as aggregateResults from "./modules/aggregateResults.ts";
import * as computeResults from "./modules/computeResults.ts";
import * as compressResults from "./modules/compressResults.ts";
import * as deleteBenchmarkLogs from "./modules/deleteBenchmarkLogs.ts";

import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";

const program = new Command();

program.version("1.0.0");

ExtractDependenciesInGPR.initializeModule(program);
GenerateUnits.initializeModule(program);
GetAlireDependencies.initializeModule(program);
ExtractDependenciesInGPR2.initializeModule(program);
GenerateBuildPath.initializeModule(program);
GenerateAlire.initializeModule(program);
CreateRunCommand.initializeModule(program, {
    commandName: "update-project",
    description: "Run `alr -n update` in all directories listed into `alireTomlPath`.",
    command: ["alr", ["-n", "update"]],
    concurrency: 10
});
CreateRunCommand.initializeModule(program, {
    commandName: "build",
    description: "Run `alr -n build` in all directories listed into `alireTomlPath`.",
    command: ["alr", ["-n", "build"]]
});
GenerateEnv.initializeModule(program);
Run.initializeModule(program);
UpdateCratesDBneo4jComplete.initializeModule(program);
UpdateAllCratesDBneo4jComplete.initializeModule(program);
PopulateNeo4j.initializeModule(program);
PopulateNeo4jSingle.initializeModule(program);
CleanNeo4j.initializeModule(program);

CreateRunBenchmarkCommand.initializeModule(program, {
    commandName: "bench-adactl",
    description: "Run Adactl benchmark",
    ruleFile: "$PROJECT_ROOT/benchmark-rules/all_rules_in_one_file/_all.aru",
    command: [
        "$PROJECT_ROOT/rootfs/home/bin/adactl",
        "-f",
        "$ruleFile",
        "-p",
        "%PRJ%",
        "@%UNITS%",
        "-o",
        "adactl-%PRJ_NAME%-$xpNum-j$max_procs$logSuffix.report",
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
        "gnatcheck-%PRJ_NAME%-$xpNum-j$max_procs$logSuffix.report",
        "--no_objects_dir",
        "-files=%UNITS_BY_FILENAME%",
        "-P%PRJ%",
        "%EXTRA_ARGS%",
        "-rules",
        `-from=$ruleFile`
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

CogralysRun.initializeModule(program);

GenerateSCC.initializeModule(program);

AddProject.initializeModule(program);
convertNeo4jJsonToCypherFile.initializeModule(program);
aggregateResults.initializeModule(program);
computeResults.initializeModule(program);
compressResults.initializeModule(program);
deleteBenchmarkLogs.initializeModule(program);

program.parse(Deno.args);
