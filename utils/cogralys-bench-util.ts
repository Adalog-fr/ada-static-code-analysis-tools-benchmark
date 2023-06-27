import * as ExtractDependenciesInGPR from "./modules/extract_dependencies_in_gpr.ts";
import * as ExtractDependenciesInGPR2 from "./modules/extract_dep_gpr2.ts";
import * as GenerateUnits from "./modules/generate_units.ts";
import * as GetAlireDependencies from "./modules/get_alire_dependencies.ts";
import * as GenerateBuildPath from "./modules/generateBuildPath.ts";
import * as GenerateAlire from "./modules/generateAlire.ts";
import * as CreateRunCommand from "./modules/runCommandFactory.ts";
import * as GenerateEnv from "./modules/generate_env.ts";
import * as Run from "./modules/run.ts";

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

program.parse(Deno.args);
