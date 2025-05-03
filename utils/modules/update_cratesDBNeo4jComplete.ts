import { join, dirname, basename } from "jsr:@std/path@^0.225.1";
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import fg from "npm:fast-glob@3.3.2";
import { UnifiedCrateData, GPRProject } from "../types.ts";
import { PROJECT_ROOT, COGRALYS_DIR_NAME } from "../../config.ts";

const OUTPUT_FILENAME = "cratesDB.json";

/**
 * Updates the Neo4j DB files completion status for a specific GPR project
 * @param cratesDB The UnifiedCrateData object
 * @param projectInfo Information about the project to update
 * @returns The updated UnifiedCrateData object
 */
export function updateGPRProject(cratesDB: UnifiedCrateData, projectInfo: { crateName: string, workDir: string, gprPath: string }): UnifiedCrateData {
    // Get the crate
    const crate = cratesDB.crates[projectInfo.crateName];
    if (!crate) return cratesDB;

    // Find the alire project
    const alireProject = crate.alireProjects.find(ap => ap.alireTomlPath === projectInfo.workDir);
    if (!alireProject) return cratesDB;

    // Find the GPR project
    const gprProject = alireProject.projects.find(gp => gp.gprPath === projectInfo.gprPath);
    if (!gprProject) return cratesDB;

    // Update the GPR project
    updateNeo4jDbFilesStatus(gprProject);

    return cratesDB;
}

/**
 * Updates the Neo4j DB files status for a GPR project
 * @param gprProject The GPR project to update
 */
function updateNeo4jDbFilesStatus(gprProject: GPRProject): void {
    const previousCompletionState = gprProject.isNeo4jDbFilesComplete;
    const isNeo4jDbFilesCompleteLocal = checkNeo4jDbFilesCompletion(gprProject.gprPath);

    gprProject.isNeo4jDbFilesComplete = isNeo4jDbFilesCompleteLocal;

    if (previousCompletionState && !isNeo4jDbFilesCompleteLocal) {
        gprProject.ignore = true;
        gprProject.ignoreReason = "Incomplete Neo4j DB files (Cogralys fails) or AdaControl error";
    } else if (!previousCompletionState && isNeo4jDbFilesCompleteLocal) {
        gprProject.ignore = false;
        gprProject.ignoreReason = undefined;
    }
}

/**
 * Checks if the Neo4j DB files are complete for a given GPR path
 * @param gprPath The path to the GPR file
 * @returns True if the Neo4j DB files are complete, false otherwise
 */
function checkNeo4jDbFilesCompletion(gprPath: string): boolean {
    const searchPath = join(PROJECT_ROOT, dirname(gprPath), "." + basename(gprPath).replace(".gpr", "").trim(), COGRALYS_DIR_NAME, "3_*.json");
    return fg.sync(searchPath, { onlyFiles: true }).length > 0;
}

export function initializeModule(program: Command): void {
    program
        .command("update-cratesDB-neo4j-dir")
        .description(
            "Updates the crates DB files (" + OUTPUT_FILENAME + "), according to the completion status of cogralys-engine for a specific GPR project" + "This file is used for example to run analysis (AdaControl, Cogralys, GNATcheck) and code metrics (tokei/scc).\n" +
            "Example: cogralys-bench-util update-cratesDB-neo4j-dir -c aaa -w src/aaa -g src/aaa/aaa.gpr"
        )
        .option(
            "-c, --crateName <string>",
            "Name of the crate to update"
        )
        .option(
            "-w, --workDir <string>",
            "Relative path (from " + PROJECT_ROOT + ") to the working directory of the project. It is where the `alire.toml` file of the project is located"
        )
        .option(
            "-g, --gprPath <string>",
            "Relative path (from " + PROJECT_ROOT + ") to the GPR"
        )
        .action(
            (options: { crateName: string, workDir: string, gprPath: string }) => {
                const cratesDB: UnifiedCrateData = JSON.parse(Deno.readTextFileSync(join(PROJECT_ROOT, OUTPUT_FILENAME)));

                const updatedCratesDB = updateGPRProject(cratesDB, options);

                Deno.writeTextFileSync(join(PROJECT_ROOT, OUTPUT_FILENAME), JSON.stringify(updatedCratesDB, null, 2));
            }
        );
}
