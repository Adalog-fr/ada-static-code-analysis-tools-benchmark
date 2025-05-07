import { join } from "jsr:@std/path@^0.225.1";
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { sortKeys } from "../utils.ts";
import { UnifiedCrateData, GPRProject } from "../types.ts"
import { PROJECT_ROOT } from "../../config.ts";

const OUTPUT_FILENAME = "cratesDB.json";

/**
 * Adds a new GPR project to the UnifiedCrateData
 * @param cratesDB The UnifiedCrateData object
 * @param projectInfo Information about the project to add
 * @returns The updated UnifiedCrateData object
 * @throws Error if the project already exists
 */
export function addGPRProject(cratesDB: UnifiedCrateData, projectInfo: { crateName: string, cratePath: string, workDir: string, gprPath: string }): UnifiedCrateData {
    // Check if the project already exists
    if (cratesDB.crates[projectInfo.crateName]) {
        const crate = cratesDB.crates[projectInfo.crateName];
        const alireProject = crate.alireProjects.find(ap => ap.alireTomlPath === projectInfo.workDir);
        if (alireProject) {
            const gprProject = alireProject.projects.find(gp => gp.gprPath === projectInfo.gprPath);
            if (gprProject) {
                throw new Error(`Project already exists: ${projectInfo.crateName} - ${projectInfo.gprPath}`);
            }
        }
    } else {
        cratesDB.crates[projectInfo.crateName] = {
            alireProjects: [],
            path: projectInfo.cratePath,
            ignore: false,
        };
    }

    const crate = cratesDB.crates[projectInfo.crateName];

    // Find or create the alire project
    let alireProject = crate.alireProjects.find(ap => ap.alireTomlPath === projectInfo.workDir);
    if (!alireProject) {
        alireProject = {
            alireTomlPath: projectInfo.workDir,
            projects: []
        };
        crate.alireProjects.push(alireProject);
    }

    // Create the new GPR project
    const newGPRProject: GPRProject = {
        gprPath: projectInfo.gprPath,
        ignore: false,
        isNeo4jDbFilesComplete: true,
        isAdaCtlComplete: true
    };

    // Add the new GPR project
    alireProject.projects.push(newGPRProject);

    return cratesDB;
}

export function initializeModule(program: Command): void {
    program
        .command("add-project")
        .description(
            "Add a new project into the crates DB files (" + OUTPUT_FILENAME + ")." +
            "Example: cogralys-bench-util add-project -c aaa -w src/aaa -g src/aaa/aaa.gpr"
        )
        .option(
            "-c, --crateName <string>",
            "Name of the crate to add"
        )
        .option(
            "-p, --cratePath <string>",
            "Relative path (from " + PROJECT_ROOT + ") to the root of the crate. It is generally equal to `--workDir`, but it is not always the case."
        )
        .option(
            "-w, --workDir <string>",
            "Relative path (from " + PROJECT_ROOT + ") to the working directory of the project. It is where the `alire.toml` file of the project is located."
        )
        .option(
            "-g, --gprPath <string>",
            "Relative path (from " + PROJECT_ROOT + ") to the GPR."
        )
        .action(
            (options: { crateName: string, cratePath: string, workDir: string, gprPath: string }) => {
                try {
                    const cratesDB: UnifiedCrateData = JSON.parse(Deno.readTextFileSync(join(PROJECT_ROOT, OUTPUT_FILENAME)));

                    // Sort the crates DB before writing
                    const updatedCratesDB = addGPRProject(cratesDB, options);

                    // Sort the crates DB before writing
                    const sortedCratesDB = sortKeys(updatedCratesDB, { deep: true });

                    Deno.writeTextFileSync(join(PROJECT_ROOT, OUTPUT_FILENAME), JSON.stringify(sortedCratesDB, null, 2));

                    console.log(`Project added successfully: ${options.crateName} - ${options.gprPath}`);
                } catch (error) {
                    console.error(`Error: ${error.message}`);
                    Deno.exit(1);
                }
            }
        );
}
