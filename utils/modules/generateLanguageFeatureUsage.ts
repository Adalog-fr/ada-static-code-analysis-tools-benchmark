import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { dirname, basename, join } from "jsr:@std/path@^0.225.1";
import * as dotenv from "jsr:@std/dotenv@^0.225.1";
import fg from "npm:fast-glob@3.3.2";
import { PROJECT_ROOT } from "../../config.ts";
import { LANGUAGE_FEATURE_USAGE_KEYS, LanguageFeatureUsage, LanguageFeatureUsageKey } from "../types.ts";

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

export function initializeModule(program: Command): void {
    program
        .command("generate-language-feature-usage")
        .description(
            "Run AdaControl traits_usage_detailed and generate language feature usage metrics as JSON next to the GPR file."
        )
        .option(
            "-w, --workDir <string>",
            "Relative path (from PROJECT_ROOT) to the working directory where alire.toml is located."
        )
        .option(
            "-g, --gprPath <string>",
            "Relative path (from PROJECT_ROOT) to the GPR file."
        )
        .action(
            async (options: { workDir: string; gprPath: string }) => {
                const { workDir, gprPath } = options;
                if (!workDir || !gprPath) {
                    console.error("Both --workDir and --gprPath must be provided.");
                    Deno.exit(1);
                }

                const workDirAbs = join(PROJECT_ROOT, workDir);
                const gprAbs = join(PROJECT_ROOT, gprPath);
                const gprDirAbs = join(PROJECT_ROOT, dirname(gprPath));
                const gprName = basename(gprPath, ".gpr");
                const unitsPath = join(gprDirAbs, `${gprName}.units`);
                const reportName = "languageFeatureUsage.report";
                const reportPath = join(workDirAbs, reportName);
                const jsonOutputPath = join(gprDirAbs, `${gprName}_languageFeatureUsage.json`);

                try {
                    Deno.statSync(unitsPath);
                } catch (_e) {
                    console.error(`Units file not found: ${unitsPath}. Please run 'cogralys-bench-util units' first.`);
                    Deno.exit(1);
                }

                // Load project-specific environment if present
                const envFile = join(workDirAbs, ".env");
                let env: Record<string, string> = {};
                try {
                    env = dotenv.loadSync({ envPath: envFile });
                } catch (_e) {
                    // .env is optional
                }

                const adactlPath = join(PROJECT_ROOT, "rootfs/home/bin/adactl");
                const ruleFilePath = join(
                    PROJECT_ROOT,
                    "benchmark-rules/language_feature_usage/traits_usage_detailed.aru",
                );

                console.log(`Running AdaControl traits_usage_detailed for ${gprPath} ...`);

                const cmd = new Deno.Command("alr", {
                    args: [
                        "exec",
                        "--",
                        adactlPath,
                        "-f",
                        ruleFilePath,
                        "-p",
                        gprAbs,
                        `@${unitsPath}`,
                        "-o",
                        reportName,
                    ],
                    cwd: workDirAbs,
                    env,
                });

                const { code, stdout, stderr } = await cmd.output();
                if (code !== 0) {
                    console.error(new TextDecoder().decode(stderr) || new TextDecoder().decode(stdout));
                    Deno.exit(code);
                }

                // Locate the generated report (in workDirAbs or subdirectories if needed)
                let finalReportPath = reportPath;
                try {
                    Deno.statSync(finalReportPath);
                } catch (_e) {
                    const matches = fg.sync("**/*languageFeatureUsage.report", {
                        cwd: workDirAbs,
                        onlyFiles: true,
                    });
                    if (!matches.length) {
                        console.error("Unable to locate languageFeatureUsage.report after AdaControl run.");
                        Deno.exit(1);
                    }
                    finalReportPath = join(workDirAbs, matches[0]);
                }

                console.log(`Parsing ${finalReportPath} ...`);
                const metrics = parseLanguageFeatureUsageReport(finalReportPath);

                Deno.writeTextFileSync(jsonOutputPath, JSON.stringify(metrics, null, 2));
                console.log(`Language feature usage metrics written to ${jsonOutputPath}`);

                try {
                    Deno.removeSync(finalReportPath);
                    console.log(`Removed report file ${finalReportPath}`);
                } catch (_e) {
                    // best effort
                }
            }
        );
}
