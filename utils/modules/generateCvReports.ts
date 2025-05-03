import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { join } from "@std/path/join";
import { BenchmarkResultDB, toolKey, ToolKeyType } from "../types.ts";
import { DocumentExporter } from "../formatters/exporter.ts";
import { OutputFormat, OutputFormatType } from "../formatters/formatters-interface.ts";
import { formatNumber } from "../utils.ts";
import { PROJECT_ROOT as defaultProjectRoot } from "../../config.ts";

// Interface for CV results per project
interface ProjectCVResult {
    run: number;
    overheadParsing: number;
    overheadPopulating?: number; // Overhead for populating, optional
}

// Interface for CV statistics
interface CVStatistics {
    mean: number;
    median: number;
    stdDev: number;
    min: number;
    max: number;
}

export function initializeModule(program: Command): void {
    program
        .command("generate-cv-report")
        .description("Generate per-project coefficient of variation report for benchmarks")
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
        .action((options: { rootDir: string, output: OutputFormatType }): void => {
            try {
                const jsonPath = join(options.rootDir, "benchmarkResults.json");
                const jsonContent = Deno.readTextFileSync(jsonPath);
                const results = JSON.parse(jsonContent) as BenchmarkResultDB[];
                const analyzer = new CVAnalyzer();
                const exporter = new DocumentExporter(options.output);

                const output = analyzer.generateReport(results, exporter);

                if (options.output === "cli") {
                    console.log(output);
                } else {
                    const resultsDir = join(options.rootDir, "results", options.output);
                    Deno.mkdirSync(resultsDir, { recursive: true });
                    const ext = options.output === "markdown" || options.output === "md" ? "md" :
                              options.output === "typst" ? "typ" : "tex";
                    const reportPath = join(resultsDir, `cv_report.${ext}`);
                    Deno.writeTextFileSync(reportPath, output);
                    console.log("CV report generated here:", reportPath);
                }
            } catch (error) {
                console.error("Error during analysis:", error);
                Deno.exit(1);
            }
        });
}

class CVAnalyzer {
    private calculateCV(values: number[]): number {
        if (values.length < 2) return 0;
        const mean = values.reduce((sum, val) => sum + val, 0) / values.length;
        const variance = values.reduce((sum, val) => sum + Math.pow(val - mean, 2), 0) / values.length;
        const stdDev = Math.sqrt(variance);
        return (stdDev / mean) * 100;
    }

    private calculateStatistics(values: number[]): CVStatistics {
        const mean = values.reduce((sum, val) => sum + val, 0) / values.length;
        const sortedValues = [...values].sort((a, b) => a - b);
        const median = sortedValues[Math.floor(values.length / 2)];
        const variance = values.reduce((sum, val) => sum + Math.pow(val - mean, 2), 0) / values.length;
        const stdDev = Math.sqrt(variance);

        return {
            mean,
            median,
            stdDev,
            min: sortedValues[0],
            max: sortedValues[sortedValues.length - 1]
        };
    }

    private extractProjectCV(result: BenchmarkResultDB, tool: ToolKeyType): ProjectCVResult {
        const toolResult = result.benchmarkResults[tool];

        const cvResult: ProjectCVResult = {
            run: this.calculateCV(toolResult.run.allRuns.map(run => run.elapsed_time)),
            overheadParsing: this.calculateCV(toolResult.overhead.parsing.allRuns.map(run => run.elapsed_time))
        };

        if (tool === 'cogralys' && toolResult.overhead.populatingDB) {
            cvResult.overheadPopulating = this.calculateCV(
                toolResult.overhead.populatingDB.allRuns.map(run => run.elapsed_time)
            );
        }

        return cvResult;
    }

    public generateReport(results: BenchmarkResultDB[], exporter: DocumentExporter): string {
        const output: string[] = [];

        output.push(exporter.documentHeader("Per-Project Coefficient of Variation Analysis"));
        output.push(exporter.addTitle("Project-Level Coefficient of Variation Analysis", 1));

        // Calculate CV for each project and tool
        const projectResults = new Map<ToolKeyType, ProjectCVResult[]>();

        for (const tool of toolKey) {
            const toolProjectCVs = results.map(result => ({
                projectName: result.gprPath,
                loc: result.scc.Code,
                ...this.extractProjectCV(result, tool)
            }));
            projectResults.set(tool, toolProjectCVs);
        }

        // Generate statistics for each tool
        for (const tool of toolKey) {
            const toolResults = projectResults.get(tool)!;

            output.push(exporter.addTitle(`${tool} Analysis`, 2));

            // Run phase statistics
            const runCVs = toolResults.map(r => r.run);
            const runStats = this.calculateStatistics(runCVs);

            // Parsing phase statistics
            const parsingCVs = toolResults.map(r => r.overheadParsing);
            const parsingStats = this.calculateStatistics(parsingCVs);

            // Optional populating phase statistics (Cogralys only)
            let populatingStats;
            if (tool === 'cogralys') {
                const populatingCVs = toolResults.map(r => r.overheadPopulating!).filter(cv => cv !== undefined);
                populatingStats = this.calculateStatistics(populatingCVs);
            }

            // Create statistics table
            const statsData = [
                {
                    phase: "Run",
                    mean: `${formatNumber(runStats.mean, 2)}%`,
                    median: `${formatNumber(runStats.median, 2)}%`,
                    stddev: `${formatNumber(runStats.stdDev, 2)}%`,
                    min: `${formatNumber(runStats.min, 2)}%`,
                    max: `${formatNumber(runStats.max, 2)}%`
                },
                {
                    phase: "Parsing",
                    mean: `${formatNumber(parsingStats.mean, 2)}%`,
                    median: `${formatNumber(parsingStats.median, 2)}%`,
                    stddev: `${formatNumber(parsingStats.stdDev, 2)}%`,
                    min: `${formatNumber(parsingStats.min, 2)}%`,
                    max: `${formatNumber(parsingStats.max, 2)}%`
                }
            ];

            if (populatingStats) {
                statsData.push({
                    phase: "Populating",
                    mean: `${formatNumber(populatingStats.mean, 2)}%`,
                    median: `${formatNumber(populatingStats.median, 2)}%`,
                    stddev: `${formatNumber(populatingStats.stdDev, 2)}%`,
                    min: `${formatNumber(populatingStats.min, 2)}%`,
                    max: `${formatNumber(populatingStats.max, 2)}%`
                });
            }

            output.push(exporter.formatTable(
                [
                    { name: "Phase", key: "phase", align: "left" },
                    { name: "Mean CV", key: "mean", align: "right" },
                    { name: "Median CV", key: "median", align: "right" },
                    { name: "Std Dev", key: "stddev", align: "right" },
                    { name: "Min CV", key: "min", align: "right" },
                    { name: "Max CV", key: "max", align: "right" }
                ],
                statsData,
                `CV Statistics for ${tool}`
            ));

            // List projects with unusually high CV (> mean + 2*stddev)
            const highCVThreshold = runStats.mean + 2 * runStats.stdDev;
            const highCVProjects = toolResults
                .filter(r => r.run > highCVThreshold)
                .sort((a, b) => b.run - a.run);

            if (highCVProjects.length > 0) {
                output.push(exporter.addTitle(`Projects with High Run CV (> ${formatNumber(highCVThreshold, 2)}%)`, 3));
                output.push(exporter.formatTable(
                    [
                        { name: "Project", key: "projectName", align: "left" },
                        { name: "LoC", key: "loc", align: "right" },
                        { name: "Run CV", key: "run", align: "right" }
                    ],
                    highCVProjects.map(p => ({
                        projectName: p.projectName,
                        loc: formatNumber(p.loc),
                        run: `${formatNumber(p.run, 2)}%`
                    }))
                ));
            }
        }

        output.push(exporter.documentFooter());
        return output.join('\n\n');
    }
}
