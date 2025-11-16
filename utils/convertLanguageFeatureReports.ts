import { join, dirname, basename } from "jsr:@std/path@^0.225.1";
import { PROJECT_ROOT } from "../config.ts";
import { UnifiedCrateData, LANGUAGE_FEATURE_USAGE_KEYS, LanguageFeatureUsage, LanguageFeatureUsageKey } from "./types.ts";

function parseLanguageFeatureUsageReport(reportPath: string): LanguageFeatureUsage {
    const metrics: LanguageFeatureUsage = {};
    for (const key of LANGUAGE_FEATURE_USAGE_KEYS) {
        metrics[key] = 0;
    }

    const content = Deno.readTextFileSync(reportPath);
    const lines = content.split("\n");

    const isKey = (key: string): key is LanguageFeatureUsageKey =>
        (LANGUAGE_FEATURE_USAGE_KEYS as readonly string[]).includes(key);

    for (const rawLine of lines) {
        const line = rawLine.trim();
        if (!line.length) continue;

        const match = line.match(/^([^:]+):\s*([+-]?\d+)/);
        if (!match) continue;

        const key = match[1].trim();
        const value = Number.parseInt(match[2], 10);
        if (!Number.isFinite(value) || !isKey(key)) continue;

        metrics[key] = (metrics[key] ?? 0) + value;
    }

    return metrics;
}

function main() {
    const cratesDBPath = join(PROJECT_ROOT, "cratesDB.json");
    const cratesDB: UnifiedCrateData = JSON.parse(Deno.readTextFileSync(cratesDBPath));

    for (const [crateName, crate] of Object.entries(cratesDB.crates)) {
        if (crate.ignore) continue;

        for (const project of crate.alireProjects) {
            const workDirAbs = join(PROJECT_ROOT, project.alireTomlPath);

            for (const gprProject of project.projects) {
                if (gprProject.ignore) continue;

                const gprAbs = join(PROJECT_ROOT, gprProject.gprPath);
                const gprDirAbs = join(PROJECT_ROOT, dirname(gprProject.gprPath));
                const gprName = basename(gprProject.gprPath, ".gpr");

                const reportPath = join(workDirAbs, "languageFeatureUsage.report");
                try {
                    Deno.statSync(reportPath);
                } catch (_e) {
                    console.log(`Report not found: ${reportPath} for ${crateName} / ${gprProject.gprPath}`);
                    continue;
                }

                console.log(`Converting ${reportPath} for ${crateName} / ${gprProject.gprPath}`);
                const metrics = parseLanguageFeatureUsageReport(reportPath);
                const jsonOutputPath = join(gprDirAbs, `${gprName}_languageFeatureUsage.json`);
                Deno.writeTextFileSync(jsonOutputPath, JSON.stringify(metrics, null, 2));

                try {
                    Deno.removeSync(reportPath);
                } catch (_e) {
                    // best effort
                }
            }
        }
    }
}

if (import.meta.main) {
    main();
}
