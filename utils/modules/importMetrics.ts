import { join, dirname, basename } from "jsr:@std/path@^0.225.1";
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { PROJECT_ROOT } from "../../config.ts";
import { LanguageSummary } from "../scc-types.ts";

/**
 * Extract the name of all units imported by the current compilation unit
 * @param unit Code to analyze
 * @returns
 */
function extractUnitsFromWithClauses(unit: string): string[] {
    // Delete comments starting with '--'
    const removeComments = (code: string): string => {
        return code
            .split("\n")
            .map((line) => {
                const commentIndex = line.indexOf("--");
                return commentIndex >= 0
                    ? line.substring(0, commentIndex)
                    : line;
            })
            .join("\n");
    };

    // Find the declarative part (before 'is' or 'body')
    const findDeclarativePart = (code: string): string => {
        const match = code.match(/^(.*?)(?:\bis\b|\bbody\b)/s);
        return match ? match[1] : code;
    };

    const cleanCode = removeComments(findDeclarativePart(unit));

    // Find all 'with' clauses
    const withPattern = /\b(?:limited\s+)?(?:private\s+)?with\s+(.*?);/gs;
    const units: string[] = [];

    let match;
    while ((match = withPattern.exec(cleanCode)) !== null) {
        const withClause = match[1];

        // Dividing the clause into individual units
        const clauseUnits = withClause
            .replace(/\n/g, " ") // Replace line breaks with spaces
            .split(",")
            .map((unit) => unit.trim().toLocaleLowerCase())
            .filter((unit) => unit.length > 0);

        units.push(...clauseUnits);
    }

    return units.sort();
}

export function generateUnitUsage(gprPath: string): void {
    const scc = JSON.parse(Deno.readTextFileSync(
        join(PROJECT_ROOT, dirname(gprPath),
            basename(gprPath, ".gpr") + "_scc-metrics.json")
    )
    ) as LanguageSummary;

    const unitUsage: { [key: string]: number } = {};

    for (const file of scc.Files) {
        const fileContent = Deno.readTextFileSync(
            join(PROJECT_ROOT, dirname(gprPath), file.Location),
        );
        const dependencies = extractUnitsFromWithClauses(fileContent);
        file.withUnit = dependencies;

        for (const unitUsed of dependencies) {
            if (!(unitUsed in unitUsage)) {
                unitUsage[unitUsed] = 0;
            }
            unitUsage[unitUsed]++;
        }
    }

    scc.unitUsage = Object.keys(unitUsage).sort().reduce(
        (obj: Record<string, number>, key) => {
            obj[key] = unitUsage[key];
            return obj;
        },
        {}
    );

    Deno.writeTextFileSync(
        join(PROJECT_ROOT, dirname(gprPath),
            basename(gprPath, ".gpr") + "_scc-metrics.json"),
        JSON.stringify(scc, null, 2)
    );

}

export function initializeModule(program: Command): void {
    program
        .command("import-unit-metrics")
        .description(
            "Generate metrics about compilation units used in `with` clauses",
        )
        .option(
            "-g, --gprPath <string>",
            "Relative path (from " + PROJECT_ROOT + ") to the GPR.",
        )
        .action((options: {
            gprPath: string;
        }) => {
            try {
                generateUnitUsage(options.gprPath)
            } catch (error) {
                console.error(`Error: ${error.message}`);
                Deno.exit(1);
            }
        });
}
