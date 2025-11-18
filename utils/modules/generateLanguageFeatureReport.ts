import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { join } from "@std/path/join";
import { ensureDirSync, copySync } from "jsr:@std/fs@1.0.9";
import { BenchmarkResultDB, LANGUAGE_FEATURE_USAGE_KEYS, LanguageFeatureUsage, toolKey, ToolKeyType } from "../types.ts";
import { DocumentExporter } from "../formatters/exporter.ts";
import { OutputFormat, OutputFormatType } from "../formatters/formatters-interface.ts";
import { formatNumber } from "../utils.ts";
import { PROJECT_ROOT as defaultProjectRoot } from "../../config.ts";

const TRIGGER_NUMBER = 0.7;
const MAX_LOC = 10_000;

export function initializeModule(program: Command): void {
    program
        .command("generate-language-feature-report")
        .description(
            "Generate language feature usage benchmark report. This script shall be called after aggregate-results."
        )
        .option(
            "--rootDir <string>",
            "Path to the root of the result files",
            defaultProjectRoot
        )
        .option(
            "-o, --output <string>",
            `Output format (Possible values: ${OutputFormat.join("|")})`,
            "cli"
        )
        .option(
            "-t, --trigger-number <number>",
            "Threshold (in seconds) to split fast vs normal projects",
            TRIGGER_NUMBER
        )
        .option(
            "-m, --maxLoc <number>",
            "Maximum project size (in lines of code) to consider",
            MAX_LOC
        )
        .action((options: { rootDir: string; output: OutputFormatType; triggerNumber: number; maxLoc: number }): void => {
            const jsonPath = join(options.rootDir, "benchmarkResults.json");
            const format = options.output || "cli";

            try {
                const jsonContent = Deno.readTextFileSync(jsonPath);
                const results = JSON.parse(jsonContent) as BenchmarkResultDB[];
                const analyzer = new LanguageFeaturePerformanceAnalyzer(options.triggerNumber, options.maxLoc);
                const exporter = new DocumentExporter(format);

                let output = exporter.documentHeader(
                    `Language Feature Usage vs Performance (LoC <= ${exporter.formatNumber(options.maxLoc)})`
                );

                // Intro
                output += exporter.addTitle("Overview", 1) + "\n";
                output +=
                    "This report analyzes how the usage of specific Ada language features correlates with analysis time for each tool, " +
                    "considering only projects whose size is below a configurable LoC threshold.\n\n";

                // Mathematical background (brief)
                output += exporter.addTitle("Correlation Model", 1) + "\n";
                output +=
                    "We use the Pearson correlation coefficient to measure the linear relationship between analysis time and feature usage. " +
                    "For each feature and each tool, we compute the correlation between:\n";
                output += "- raw feature count (number of occurrences)\n";
                output += "- normalized feature count (occurrences per 1k LoC)\n\n";

                // Analyze tools (skip cogralys as its analysis time is constant)
                for (const tool of toolKey) {
                    if (tool === "cogralys") {
                        continue;
                    }
                    output += analyzer.analyzeTool(results, tool, exporter);
                    output += "\n\n";
                }

                output += exporter.documentFooter();

                let resultsDir = join(options.rootDir, "results");
                let result = "";
                let ext = "";

                switch (format) {
                    case "cli":
                        console.log(output);
                        break;
                    case "markdown":
                    case "md":
                        resultsDir = join(resultsDir, "markdown");
                        result = output;
                        ext = "md";
                        break;
                    case "typst":
                        resultsDir = join(resultsDir, "typst");
                        result = output;
                        ext = "typ";
                        break;
                    case "latex":
                    case "tex":
                        resultsDir = join(resultsDir, "latex");
                        result = output;
                        ext = "tex";
                        break;
                    default:
                        console.log(output);
                        break;
                }

                ensureDirSync(resultsDir);

                if (result.length) {
                    if (format === "typst") {
                        copySync(join(defaultProjectRoot, "utils/report/typst"), join(resultsDir, "/"), { overwrite: true });
                    }

                    const reportPath = join(resultsDir, "languageFeatureReport." + ext);
                    Deno.writeTextFileSync(reportPath, result);
                    console.log("Language feature report generated here: ", reportPath);
                }
            } catch (error) {
                console.error("Error during language feature analysis:", error);
                Deno.exit(1);
            }
        });
}

interface FeatureProjectSample {
    crateName: string;
    workDir: string;
    gprPath: string;
    loc: number;
    analysisTime: {
        adactl: number;
        gnatcheck_1cores: number;
        gnatcheck_32cores: number;
    };
    features: LanguageFeatureUsage;
}

class LanguageFeaturePerformanceAnalyzer {
    private readonly triggerNumber: number;
    private readonly maxLoc: number;

    constructor(triggerNumber: number, maxLoc: number) {
        this.triggerNumber = triggerNumber;
        this.maxLoc = maxLoc;
    }

    private buildSamples(results: BenchmarkResultDB[]): FeatureProjectSample[] {
        const samples: FeatureProjectSample[] = [];

        for (const result of results) {
            if (!result.languageFeatureUsage) continue;
            if (result.scc.Code > this.maxLoc) continue;

            samples.push({
                crateName: result.crateName,
                workDir: result.workDir,
                gprPath: result.gprPath,
                loc: result.scc.Code,
                analysisTime: {
                    adactl: result.benchmarkResults.adactl.digestTime.analysisTime,
                    gnatcheck_1cores: result.benchmarkResults.gnatcheck_1cores.digestTime.analysisTime,
                    gnatcheck_32cores: result.benchmarkResults.gnatcheck_32cores.digestTime.analysisTime,
                },
                features: result.languageFeatureUsage,
            });
        }

        return samples;
    }

    private calculateCorrelation(x: number[], y: number[]): number {
        const n = x.length;
        if (!n) return 0;

        const sumX = x.reduce((a, b) => a + b, 0);
        const sumY = y.reduce((a, b) => a + b, 0);
        const sumXY = x.reduce((total, xi, i) => total + xi * y[i], 0);
        const sumX2 = x.reduce((total, xi) => total + xi * xi, 0);
        const sumY2 = y.reduce((total, yi) => total + yi * yi, 0);

        const numerator = n * sumXY - sumX * sumY;
        const denominator = Math.sqrt((n * sumX2 - sumX * sumX) * (n * sumY2 - sumY * sumY));

        return denominator === 0 ? 0 : numerator / denominator;
    }

    public analyzeTool(results: BenchmarkResultDB[], tool: ToolKeyType, exporter: DocumentExporter): string {
        const samples = this.buildSamples(results);
        if (!samples.length) {
            return `No projects with language feature data found for tool ${tool}.\n`;
        }

        const output: string[] = [];
        output.push(exporter.addTitle(`Tool: ${tool}`, 1));

        // Build vectors for each feature
        const rows: {
            feature: string;
            corrCount: number;
            corrDensity: number;
            avgCountFast: number;
            avgCountSlow: number;
        }[] = [];

        const times = samples.map((s) => s.analysisTime[tool]);
        const locs = samples.map((s) => s.loc);

        // Define fast vs slow based on triggerNumber
        const fastMask = times.map((t) => t < this.triggerNumber);

        for (const key of LANGUAGE_FEATURE_USAGE_KEYS) {
            const counts = samples.map((s) => s.features[key] ?? 0);
            const densities = counts.map((c, i) => (locs[i] > 0 ? (c / locs[i]) * 1000 : 0)); // per 1k LoC

            const corrCount = this.calculateCorrelation(times, counts);
            const corrDensity = this.calculateCorrelation(times, densities);

            // Averages per cluster (fast vs slow)
            let sumFast = 0;
            let nFast = 0;
            let sumSlow = 0;
            let nSlow = 0;

            for (let i = 0; i < samples.length; i++) {
                if (fastMask[i]) {
                    sumFast += counts[i];
                    nFast++;
                } else {
                    sumSlow += counts[i];
                    nSlow++;
                }
            }

            const avgFast = nFast ? sumFast / nFast : 0;
            const avgSlow = nSlow ? sumSlow / nSlow : 0;

            rows.push({
                feature: key,
                corrCount,
                corrDensity,
                avgCountFast: avgFast,
                avgCountSlow: avgSlow,
            });
        }

        // Sort by absolute correlation on count (descending)
        rows.sort((a, b) => Math.abs(b.corrCount) - Math.abs(a.corrCount));

        output.push(
            exporter.formatTable(
                [
                    { name: "Feature", key: "feature", align: "left" },
                    { name: "Corr(count, time)", key: "corrCount", align: "right" },
                    { name: "Corr(density, time)", key: "corrDensity", align: "right" },
                    { name: "Avg count (fast)", key: "avgFast", align: "right" },
                    { name: "Avg count (slow)", key: "avgSlow", align: "right" },
                ],
                rows.map((r) => ({
                    feature: r.feature,
                    corrCount: formatNumber(r.corrCount, 3),
                    corrDensity: formatNumber(r.corrDensity, 3),
                    avgFast: formatNumber(r.avgCountFast, 2),
                    avgSlow: formatNumber(r.avgCountSlow, 2),
                })),
                `Correlation and averages for ${tool}`,
            ),
        );

        return output.join("\n");
    }
}
