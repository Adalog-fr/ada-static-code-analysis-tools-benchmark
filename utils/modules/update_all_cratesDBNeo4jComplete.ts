import { join, dirname, basename } from "jsr:@std/path@^0.225.1";
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import fg from "npm:fast-glob@3.3.2";
import { UnifiedCrateData, GPRProject, Crate } from "../utils.ts";
import { PROJECT_ROOT } from "../../config.ts";

const OUTPUT_FILENAME = "cratesDB.json";

/**
 * Updates the Neo4j DB files completion status for all GPR projects in all crates
 * @param cratesDB The UnifiedCrateData object
 * @returns The updated UnifiedCrateData object
 */
function updateAllGPRProjects(cratesDB: UnifiedCrateData): UnifiedCrateData {
    // Iterate through all crates
    for (const crateName in cratesDB.crates) {
        const crate = cratesDB.crates[crateName];
        updateCrate(crate);
    }
    return cratesDB;
}

/**
 * Updates a single crate
 * @param crate The crate to update
 */
function updateCrate(crate: Crate): void {
    // Iterate through all alire projects in the crate
    for (const alireProject of crate.alireProjects) {
        // Update each GPR project in the alire project
        alireProject.projects = alireProject.projects.map(updateNeo4jDbFilesStatus);
    }
}

/**
 * Updates the Neo4j DB files status for a GPR project
 * @param gprProject The GPR project to update
 * @returns The updated GPR project
 */
function updateNeo4jDbFilesStatus(gprProject: GPRProject): GPRProject {
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

    return gprProject;
}

/**
 * Checks if the Neo4j DB files are complete for a given GPR path
 * @param gprPath The path to the GPR file
 * @returns True if the Neo4j DB files are complete, false otherwise
 */
function checkNeo4jDbFilesCompletion(gprPath: string): boolean {
    const searchPath = join(PROJECT_ROOT, dirname(gprPath), "." + basename(gprPath).replace(".gpr", "").trim(), ".atdgb", "3_*.json");
    return fg.sync(searchPath, { onlyFiles: true }).length > 0;
}

export function initializeModule(program: Command): void {
    program
        .command("update-all-cratesDB-neo4j-dir")
        .description(
            "Updates the crates DB files (" + OUTPUT_FILENAME + ") for all GPR projects in all crates, according to the completion status of cogralys-engine" +
            "This file is used for example to run analysis (AdaControl, Cogralys, GNATcheck) and code metrics (tokei/scc).\n" +
            "Example: cogralys-bench-util update-all-cratesDB-neo4j-dir"
        )
        .action(() => {
            const cratesDB: UnifiedCrateData = JSON.parse(Deno.readTextFileSync(join(PROJECT_ROOT, OUTPUT_FILENAME)));

            const updatedCratesDB = updateAllGPRProjects(cratesDB);

            Deno.writeTextFileSync(OUTPUT_FILENAME, JSON.stringify(updatedCratesDB, null, 2));

            console.log(`Updated ${OUTPUT_FILENAME} with Neo4j DB files completion status for all GPR projects.`);
        });
}
