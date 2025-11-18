import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { join } from "jsr:@std/path@^0.225.1";
import { ensureDirSync } from "jsr:@std/fs@1.0.9";
import { BenchmarkResultDB, ToolKeyType, toolKey, SimilarProjectsRawData, SimilarProjectsTarget, SimilarProjectsTargetInfo } from "../types.ts";
import { PROJECT_ROOT as defaultProjectRoot } from "../../config.ts";
import { OutputFormat, OutputFormatType, TableCell } from "../formatters/formatters-interface.ts";
import { DocumentExporter } from "../formatters/exporter.ts";

const DEFAULT_TOLERANCE = 0.2; // +/- 20%
const MAX_SMALL_LOC = 10_000;
const DEFAULT_TRIGGER_NUMBER = 0.7; // seconds

interface SimilarProjectsOptions {
    rootDir: string;
    output: OutputFormatType;
    loc?: number;
    project?: string;
    gpr?: string;
    metric: "loc" | "files";
    tolerance: number;
    tool: ToolKeyType;
    triggerNumber: number;
}

interface ProjectRow {
    project: string;
    gprPath: string;
    loc: number;
    nbFiles: number;
    analysisTime: number;
    executionTime: number;
}

function loadResults(rootDir: string): BenchmarkResultDB[] {
    const jsonPath = join(rootDir, "benchmarkResults.json");
    const content = Deno.readTextFileSync(jsonPath);
    return JSON.parse(content) as BenchmarkResultDB[];
}

function selectTarget(options: SimilarProjectsOptions, results: BenchmarkResultDB[]): { targetLoc: number; targetFiles: number; targetProject?: BenchmarkResultDB } {
    if (options.loc && (options.project || options.gpr)) {
        throw new Error("Please specify only one of --loc, --project or --gpr.");
    }

    if (options.loc) {
        return { targetLoc: options.loc, targetFiles: 0 };
    }

    let target: BenchmarkResultDB | undefined;

    if (options.project) {
        target = results.find(r => r.crateName === options.project);
        if (!target) {
            throw new Error(`Project with crateName='${options.project}' not found in benchmarkResults.json`);
        }
    } else if (options.gpr) {
        // Try exact match first
        target = results.find(r => r.gprPath === options.gpr);
        if (!target) {
            // Fallback: search by suffix
            target = results.find(r => r.gprPath.endsWith(options.gpr as string));
        }
        if (!target) {
            throw new Error(`Project with gprPath='${options.gpr}' not found in benchmarkResults.json`);
        }
    } else {
        throw new Error("You must specify one of --loc, --project or --gpr.");
    }

    return { targetLoc: target.scc.Code, targetFiles: target.scc.Count, targetProject: target };
}

function computeMetric(result: BenchmarkResultDB, metric: "loc" | "files"): number {
    return metric === "loc" ? result.scc.Code : result.scc.Count;
}

function computeTiming(result: BenchmarkResultDB, tool: ToolKeyType): { analysisTime: number; executionTime: number } {
    const digest = result.benchmarkResults[tool].digestTime;
    return {
        analysisTime: digest.analysisTime,
        executionTime: digest.executionTime,
    };
}

function findSimilarProjects(options: SimilarProjectsOptions, results: BenchmarkResultDB[]): { fast: ProjectRow[]; normal: ProjectRow[]; single: ProjectRow[]; targetInfo: SimilarProjectsTargetInfo; targetRow?: ProjectRow; targetBenchmark?: BenchmarkResultDB; matchedBenchmarks: BenchmarkResultDB[] } {
    const { targetLoc, targetFiles, targetProject } = selectTarget(options, results);
    const metricTarget = options.metric === "loc" ? targetLoc : (targetFiles || 0);

    if (!metricTarget || metricTarget <= 0) {
        throw new Error("Target metric is zero or undefined; cannot search for similar projects.");
    }

    const tolerance = options.tolerance ?? DEFAULT_TOLERANCE;
    const minMetric = metricTarget * (1 - tolerance);
    const maxMetric = metricTarget * (1 + tolerance);

    const rows: ProjectRow[] = [];
    const matchedBenchmarks: BenchmarkResultDB[] = [];
    let targetRow: ProjectRow | undefined;

    if (targetProject) {
        const timing = computeTiming(targetProject, options.tool);
        targetRow = {
            project: targetProject.crateName,
            gprPath: targetProject.gprPath,
            loc: targetProject.scc.Code,
            nbFiles: targetProject.scc.Count,
            analysisTime: timing.analysisTime,
            executionTime: timing.executionTime,
        };
    }

    for (const r of results) {
        if (targetProject && r.gprPath === targetProject.gprPath) {
            continue; // skip self
        }

        const metricValue = computeMetric(r, options.metric);
        if (metricValue < minMetric || metricValue > maxMetric) {
            continue;
        }

        const timing = computeTiming(r, options.tool);

        rows.push({
            project: r.crateName,
            gprPath: r.gprPath,
            loc: r.scc.Code,
            nbFiles: r.scc.Count,
            analysisTime: timing.analysisTime,
            executionTime: timing.executionTime,
        });

        matchedBenchmarks.push(r);
    }

    const isSmall = targetLoc < MAX_SMALL_LOC;
    const targetInfo: SimilarProjectsTargetInfo = { targetLoc, targetFiles, isSmall };

    if (!isSmall) {
        return { fast: [], normal: [], single: rows, targetInfo, targetRow, targetBenchmark: targetProject, matchedBenchmarks };
    }

    const fast: ProjectRow[] = [];
    const normal: ProjectRow[] = [];

    for (const row of rows) {
        if (row.analysisTime < options.triggerNumber) {
            fast.push(row);
        } else {
            normal.push(row);
        }
    }
    return { fast, normal, single: [], targetInfo, targetRow, targetBenchmark: targetProject, matchedBenchmarks };
}

function formatWithDiff(exporter: DocumentExporter, value: number, target?: number): string {
    if (!target || target <= 0) {
        return exporter.formatNumber(value);
    }

    const diff = ((value - target) / target) * 100;
    const sign = diff >= 0 ? "+" : "";
    return `${exporter.formatNumber(value)} (${sign}${diff.toFixed(1)}%)`;
}

function formatTable(exporter: DocumentExporter, rows: ProjectRow[], title: string, targetLoc?: number, targetFiles?: number): string {
    const columns: TableCell[] = [
        { name: "Project", key: "project", align: "left" },
        { name: "GPR Path", key: "gprPath", align: "left" },
        { name: "LoC", key: "loc", align: "right" },
        { name: "Nb Files", key: "nbFiles", align: "right" },
        { name: "Analysis Time (s)", key: "analysisTime", align: "right" },
        { name: "Execution Time (s)", key: "executionTime", align: "right" },
    ];

    // Convert to generic row objects
    const data = rows.map((r) => ({
        project: r.project,
        gprPath: r.gprPath,
        loc: formatWithDiff(exporter, r.loc, targetLoc),
        nbFiles: formatWithDiff(exporter, r.nbFiles, targetFiles),
        analysisTime: exporter.formatNumber(r.analysisTime),
        executionTime: exporter.formatNumber(r.executionTime),
    }));

    return exporter.formatTable(columns, data, title);
}

export function initializeModule(program: Command): void {
    program
        .command("similar-projects")
        .description(
            "Find projects with similar size (LoC or number of files) based on benchmarkResults.json."
        )
        .option(
            "--rootDir <string>",
            "Path to the root directory of the result files",
            defaultProjectRoot,
        )
        .option(
            "-o, --output <string>",
            `Output format (Possible values: ${OutputFormat.join("|")})`,
            "cli",
        )
        .option(
            "--loc <number>",
            "Target number of lines of code (LoC)",
        )
        .option(
            "--project <string>",
            "Target project crate name (as stored in benchmarkResults.json)",
        )
        .option(
            "--gpr <string>",
            "Target project GPR path (exact or suffix)",
        )
        .option(
            "--metric <string>",
            "Similarity metric: 'loc' or 'files'",
            "loc",
        )
        .option(
            "--tolerance <number>",
            "Relative tolerance as a fraction (e.g. 0.2 for ±20%)",
            DEFAULT_TOLERANCE,
        )
        .option(
            "--tool <string>",
            "Tool used for timings (adactl|cogralys|gnatcheck_1cores|gnatcheck_32cores)",
            "adactl",
        )
        .option(
            "-t, --trigger-number <number>",
            "Threshold (seconds) to separate fast vs normal projects when target LoC < 10k",
            DEFAULT_TRIGGER_NUMBER,
        )
        .action((options: { rootDir: string; output: OutputFormatType; loc?: number; project?: string; gpr?: string; metric: string; tolerance: number; tool: string; triggerNumber: number }) => {
            try {
                const metric = options.metric === "files" ? "files" : "loc";

                const validTools: ToolKeyType[] = [...toolKey];
                const tool = validTools.includes(options.tool as ToolKeyType)
                    ? (options.tool as ToolKeyType)
                    : "adactl";

                const simOptions: SimilarProjectsOptions = {
                    rootDir: options.rootDir || defaultProjectRoot,
                    output: options.output || "cli",
                    loc: options.loc,
                    project: options.project,
                    gpr: options.gpr,
                    metric,
                    tolerance: options.tolerance ?? DEFAULT_TOLERANCE,
                    tool,
                    triggerNumber: options.triggerNumber ?? DEFAULT_TRIGGER_NUMBER,
                };

                const results = loadResults(simOptions.rootDir);
                const exporter = new DocumentExporter(simOptions.output);

                const { fast, normal, single, targetInfo, targetRow, targetBenchmark, matchedBenchmarks } = findSimilarProjects(simOptions, results);

                const toolLabel = tool;
                const metricLabel = metric === "loc" ? "LoC" : "Nb Files";
                let output = "";

                const headerTitle = `Similar projects around target ${metricLabel}: ${exporter.formatNumber(metric === "loc" ? targetInfo.targetLoc : targetInfo.targetFiles)}`;
                output += exporter.documentHeader(headerTitle);

                if (targetRow) {
                    output += exporter.addTitle("Target project", 1);
                    output += "\n" + formatTable(exporter, [targetRow], `Target project (${metricLabel}, tool ${toolLabel})`, targetInfo.targetLoc, targetInfo.targetFiles) + "\n\n";
                }

                if (targetInfo.isSmall) {
                    output += exporter.addTitle("Fast projects", 1);
                    if (fast.length) {
                        output += "\n" + formatTable(exporter, fast, `Fast projects (analysis time < ${exporter.formatNumber(simOptions.triggerNumber)}s with ${toolLabel})`, targetInfo.targetLoc, targetInfo.targetFiles) + "\n\n";
                    } else {
                        output += "\nNo fast project found in the specified range.\n\n";
                    }

                    output += exporter.addTitle("Normal projects", 1);
                    if (normal.length) {
                        output += "\n" + formatTable(exporter, normal, `Normal projects (analysis time >= ${exporter.formatNumber(simOptions.triggerNumber)}s with ${toolLabel})`, targetInfo.targetLoc, targetInfo.targetFiles) + "\n\n";
                    } else {
                        output += "\nNo normal project found in the specified range.\n\n";
                    }
                } else {
                    output += exporter.addTitle("Similar projects", 1);
                    if (single.length) {
                        output += "\n" + formatTable(exporter, single, `Projects similar to target (${metricLabel}) with tool ${toolLabel}`, targetInfo.targetLoc, targetInfo.targetFiles) + "\n\n";
                    } else {
                        output += "\nNo project found in the specified range.\n\n";
                    }
                }

                output += exporter.documentFooter();

                // Export raw subset of benchmark data (target + similar projects) as JSON
                let rawTarget: SimilarProjectsTarget;

                if (simOptions.project || simOptions.gpr) {
                    rawTarget = {
                        type: simOptions.project ? "project" : "gpr",
                        value: (simOptions.project ?? simOptions.gpr) as string,
                        benchmark: targetBenchmark ?? null,
                    };
                } else {
                    rawTarget = {
                        type: "loc",
                        value: simOptions.loc as number,
                    };
                }

                const rawData: SimilarProjectsRawData = {
                    options: {
                        metric,
                        tolerance: simOptions.tolerance,
                        tool: toolLabel,
                        triggerNumber: simOptions.triggerNumber,
                    },
                    target: rawTarget,
                    targetInfo,
                    similarProjects: matchedBenchmarks,
                };

                const sanitize = (value: string): string => {
                    return value.replace(/[^a-zA-Z0-9_-]+/g, "_").slice(0, 100);
                };

                let baseName = "similar-projects";
                if (simOptions.project) {
                    baseName += `_project-${sanitize(simOptions.project)}`;
                } else if (simOptions.gpr) {
                    baseName += `_gpr-${sanitize(simOptions.gpr)}`;
                } else if (typeof simOptions.loc === "number") {
                    baseName += `_loc-${simOptions.loc}`;
                }

                const resultsDir = join(simOptions.rootDir, "results");
                ensureDirSync(resultsDir);
                const rawJsonPath = join(resultsDir, `${baseName}.json`);
                Deno.writeTextFileSync(rawJsonPath, JSON.stringify(rawData, null, 2));
                console.log("Raw similar-projects data stored in:", rawJsonPath);

                switch (simOptions.output) {
                    case "cli":
                        console.log(output);
                        break;
                    case "markdown":
                    case "md":
                    case "typst":
                    case "latex":
                    case "tex":
                        // For non-CLI outputs, simply print to stdout as well; caller can redirect if needed.
                        console.log(output);
                        break;
                    default:
                        console.log(output);
                        break;
                }
            } catch (error) {
                console.error("Error while computing similar projects:", error);
                Deno.exit(1);
            }
        });
}
