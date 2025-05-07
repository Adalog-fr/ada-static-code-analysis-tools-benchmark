import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { join } from "@std/path/join";
import { ensureDirSync, copySync } from "jsr:@std/fs@1.0.9";
import { BenchmarkResultDB, toolKey, ToolKeyType } from "../types.ts";
import { DocumentExporter } from "../formatters/exporter.ts";
import { OutputFormat, OutputFormatType } from "../formatters/formatters-interface.ts";
import { formatNumber } from "../utils.ts";
import { PROJECT_ROOT as defaultProjectRoot } from "../../config.ts";

// Constant for trigger time threshold
const TRIGGER_NUMBER = 0.7;
const MAX_LOC = 10_000;

export function initializeModule(program: Command): void {
    program
        .command("generate-import-report")
        .description(
            "Generate import benchmark report. This script shall be called after benchmark GNATcheck, AdaControl and Cogralys, and after aggregate-results."
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
            `Trigger time threshold, where we split the groups to compare`,
            TRIGGER_NUMBER
        )
        .option(
            "-m, --maxLoc <number>",
            `Maximum size of a project (in lines of code)`,
            MAX_LOC
        )
        .action((options: { rootDir: string, output: OutputFormatType, triggerNumber: number, maxLoc: number }): void => {
            const args = Deno.args;
            if (args.length < 1) {
                program.help();
                Deno.exit(0);
            }

            const jsonPath = join(options.rootDir, "benchmarkResults.json");
            const format = options.output || 'cli';

            try {
                const jsonContent = Deno.readTextFileSync(jsonPath);
                const results = JSON.parse(jsonContent) as BenchmarkResultDB[];
                const analyzer = new PerformanceAnalyzer(options.rootDir, options.triggerNumber);
                const exporter = new DocumentExporter(format);

                let output = exporter.documentHeader(`Analysis of imports for project size < ${exporter.formatNumber(options.maxLoc)} LoC`);

                let outputSection: string[] = [];

                // Add mathematical explanation of correlation calculation
                outputSection.push(exporter.addTitle("Mathematical Background", 1));
                outputSection.push("In this report, we use various statistical measures to analyze the data. Below are the key mathematical formulae used:");

                // Pearson correlation coefficient explanation
                outputSection.push(exporter.addTitle("Pearson Correlation Coefficient", 2));
                outputSection.push("The correlation between analysis time and various metrics is calculated using the Pearson correlation coefficient formula:");
                outputSection.push(exporter.mathEquation("r = \\frac{n\\sum xy - \\sum x \\sum y}{\\sqrt{[n \\sum x^2 - (\\sum x)^2][n \\sum y^2 - (\\sum y)^2]}}"));
                outputSection.push("Where:");
                outputSection.push("- " + exporter.mathEquation("n", true) + " is the number of data points (projects)");
                outputSection.push("- " + exporter.mathEquation("\\sum xy", true) + " is the sum of the products of paired data values");
                outputSection.push("- " + exporter.mathEquation("\\sum x", true) + " is the sum of the x values (analysis times)");
                outputSection.push("- " + exporter.mathEquation("\\sum y", true) + " is the sum of the y values (metric values)");
                outputSection.push("- " + exporter.mathEquation("\\sum x^2", true) + " is the sum of squared x values");
                outputSection.push("- " + exporter.mathEquation("\\sum y^2", true) + " is the sum of squared y values");
                outputSection.push("The coefficient ranges from -1 to 1, where:");
                outputSection.push("- Values close to 1 indicate a strong positive correlation");
                outputSection.push("- Values close to -1 indicate a strong negative correlation");
                outputSection.push("- Values close to 0 indicate little to no linear correlation");

                // Standard Library Ratio explanation
                outputSection.push(exporter.addTitle("Standard Library Import Ratio", 2));
                outputSection.push("The ratio of standard library imports to total imports is calculated as:");
                outputSection.push(exporter.mathEquation("\\text{stdLibRatio} = \\frac{\\text{stdLibImports.length}}{\\text{allImports.length}}"));
                outputSection.push("This ratio helps us understand what portion of a project's dependencies comes from the standard library.");

                // Averages explanation
                outputSection.push(exporter.addTitle("Average Calculations", 2));
                outputSection.push("For a collection of projects, we calculate the average of a metric using:");
                outputSection.push(exporter.mathEquation("\\text{Average} = \\frac{\\sum_{i=1}^{n} \\text{metric}(\\text{project}_i)}{n}"));
                outputSection.push("Where $n$ is the number of projects and metric(project) is the value of the metric for a specific project.");

                // LoC vs Files ratio explanation
                outputSection.push(exporter.addTitle("LoC vs Files Ratio", 2));
                outputSection.push("We calculate the LoC vs Files ratio in two different ways:");
                outputSection.push("1. " + exporter.bold("Global ratio") + ": Total lines of code divided by total number of files across all projects:");
                outputSection.push(exporter.mathEquation("\\text{Global Ratio} = \\frac{\\sum_{i=1}^{n} \\text{LoC}_i}{\\sum_{i=1}^{n} \\text{Files}_i}"));
                outputSection.push("2. " + exporter.bold("Average of individual ratios") + ": Average of the LoC/Files ratio calculated for each project:");
                outputSection.push(exporter.mathEquation("\\text{Avg. Individual Ratio} = \\frac{\\sum_{i=1}^{n} \\frac{\\text{LoC}_i}{\\text{Files}_i}}{n}"));
                outputSection.push("These two values can differ significantly and provide different perspectives on code organization.");

                // Statistical distribution explanation
                outputSection.push(exporter.addTitle("Statistical Distribution Measures", 2));
                outputSection.push("For understanding the distribution of analysis times, we calculate:");
                outputSection.push("- " + exporter.bold("Minimum") + ": The smallest value in the dataset");
                outputSection.push("- " + exporter.bold("Q1 (First Quartile)") + ": The value below which 25% of observations are found");
                outputSection.push("- " + exporter.bold("Median (Second Quartile)") + ": The value below which 50% of observations are found");
                outputSection.push("- " + exporter.bold("Q3 (Third Quartile)") + ": The value below which 75% of observations are found");
                outputSection.push("- " + exporter.bold("Maximum") + ": The largest value in the dataset");
                outputSection.push("These statistics help us understand the spread and central tendency of the data without being overly influenced by outliers.");

                output += outputSection.join('\n\n') + "\n\n";

                for (const tool of toolKey) {
                    if (tool === "cogralys") {
                        // Skip cogralys, because analysis time is constant
                        analyzer.analyzeResults(results, tool, exporter);
                        continue;
                    }
                    output += analyzer.analyzeResults(results, tool, exporter);
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

                    const reportPath = join(resultsDir, "importReport." + ext);
                    Deno.writeTextFileSync(reportPath, result);
                    console.log("Import report generated here: ", reportPath);

                }

            } catch (error) {
                console.error("Error during analysis:", error);
                Deno.exit(1);
            }
        });
}

// Interface for project analysis results
interface ProjectAnalysis {
    crateName: string;
    workDir: string;
    gprPath: string;
    loc: number;
    nbFiles: number;
    locVsNbFilesRatio: number;
    complexity: number;
    analysisTime: {
        adactl: number;
        cogralys: number;
        gnatcheck_1cores: number;
        gnatcheck_32cores: number;
    };
    maxIssuedMessages: {
        adactl: number;
        cogralys: number;
        gnatcheck_1cores: number;
        gnatcheck_32cores: number;
    };
    imports: ImportAnalysis;
}

// Interface for import analysis data
interface ImportAnalysis {
    totalImports: number;
    stdLibImports: number;
    gnatImports: number;
    systemImports: number;
    interfaceImports: number;
    customImports: number;
    stdLibRatio: number;
    categorizedImports: Record<string, number>;
    nonCustomImports: string[];
}

// Interface for standard library categories
interface StdLibCategory {
    name: string;
    patterns: string[];
}

function computeGlobalLocVsFilesRatio(projects: ProjectAnalysis[]): number {
    let nbLoc = 0;
    let nbFiles = 0;
    for (const project of projects) {
        nbLoc += project.loc;
        nbFiles += project.nbFiles;
    }
    return nbLoc / nbFiles;
}

export class PerformanceAnalyzer {
    private readonly rootDir: string;
    private readonly triggerNumber: number;
    private readonly stdLibCategories: StdLibCategory[] = [
        {
            name: "io",
            patterns: [
                "ada.text_io",
                "ada.wide_text_io",
                "ada.wide_wide_text_io",
                "ada.sequential_io",
                "ada.direct_io",
                "ada.complex_text_io",
                "ada.float_text_io",
                "ada.float_wide_text_io",
                "ada.float_wide_wide_text_io",
                "ada.integer_text_io",
                "ada.integer_wide_text_io",
                "ada.integer_wide_wide_text_io"
            ]
        },
        {
            name: "strings",
            patterns: [
                "ada.strings",
                "ada.characters"
            ]
        },
        {
            name: "containers",
            patterns: ["ada.containers"]
        },
        {
            name: "timing",
            patterns: [
                "ada.calendar",
                "ada.real_time",
                "ada.execution_time"
            ]
        },
        {
            name: "numerics",
            patterns: ["ada.numerics"]
        },
        {
            name: "tasking",
            patterns: [
                "ada.task_",
                "ada.synchronous_",
                "ada.asynchronous_task_control",
                "ada.dispatching"
            ]
        },
        {
            name: "memory",
            patterns: [
                "ada.finalization",
                "ada.storage_io",
                "ada.unchecked_deallocation",
                "ada.unchecked_conversion"
            ]
        },
        {
            name: "system interface",
            patterns: [
                "ada.command_line",
                "ada.directories",
                "ada.environment_variables",
                "ada.interrupts"
            ]
        },
        {
            name: "exceptions",
            patterns: [
                "ada.exceptions",
                "ada.io_exceptions"
            ]
        }
    ];

    constructor(rootDir: string, triggerNumber: number) {
        this.rootDir = rootDir;
        this.triggerNumber = triggerNumber;
    }

    private analyzeImports(withUnits: string[] = []): ImportAnalysis {
        const allImports = [...new Set(withUnits)];

        const gnatImports = allImports.filter(unit => unit.startsWith("gnat."));
        const interfaceImports = allImports.filter(unit => unit.startsWith("interfaces."));
        const systemImports = allImports.filter(unit => unit.startsWith("system."));
        const stdLibImports = allImports.filter(unit => unit.startsWith("ada."));
        const customImports = allImports.filter(unit =>
            !unit.startsWith("ada.") &&
            !unit.startsWith("interfaces.") &&
            !unit.startsWith("gnat.") &&
            !unit.startsWith("system.")
        );

        const categorizedImports: Record<string, number> = {};
        this.stdLibCategories.forEach(category => {
            categorizedImports[category.name] = stdLibImports.filter(unit =>
                category.patterns.some(pattern => unit.startsWith(pattern))
            ).length;
        });

        const categorizedCount = Object.values(categorizedImports).reduce((a, b) => a + b, 0);
        categorizedImports["other_ada"] = stdLibImports.length - categorizedCount;

        return {
            totalImports: allImports.length,
            stdLibImports: stdLibImports.length,
            gnatImports: gnatImports.length,
            systemImports: systemImports.length,
            interfaceImports: interfaceImports.length,
            customImports: customImports.length,
            stdLibRatio: allImports.length ? stdLibImports.length / allImports.length : 0,
            categorizedImports,
            nonCustomImports: [...gnatImports, ...interfaceImports, ...systemImports, ...stdLibImports]
        };
    }

    public analyzeProject(result: BenchmarkResultDB): ProjectAnalysis {
        return {
            crateName: result.crateName,
            workDir: result.workDir,
            gprPath: result.gprPath,
            loc: result.scc.Code,
            locVsNbFilesRatio: result.scc.Code / result.scc.Count,
            nbFiles: result.scc.Count,
            complexity: result.scc.Complexity,
            analysisTime: {
                adactl: result.benchmarkResults.adactl.digestTime.analysisTime,
                cogralys: result.benchmarkResults.cogralys.digestTime.analysisTime,
                gnatcheck_1cores: result.benchmarkResults.gnatcheck_1cores.digestTime.analysisTime,
                gnatcheck_32cores: result.benchmarkResults.gnatcheck_32cores.digestTime.analysisTime
            },
            maxIssuedMessages: {
                adactl: result.benchmarkResults.adactl.run.issuedMessages.maxCount,
                cogralys: result.benchmarkResults.cogralys.run.issuedMessages.maxCount,
                gnatcheck_1cores: result.benchmarkResults.gnatcheck_1cores.run.issuedMessages.maxCount,
                gnatcheck_32cores: result.benchmarkResults.gnatcheck_32cores.run.issuedMessages.maxCount
            },
            imports: this.analyzeImports(result.scc.unitUsage ? Object.keys(result.scc.unitUsage) : [])
        };
    }

    // Helper method to export projects to JSON
    private exportProjectsToJson(projects: ProjectAnalysis[], tool: ToolKeyType, category: string): void {
        // Convert projects to simplified format for export
        const exportData = projects.map(p => ({
            crateName: p.crateName,
            workDir: p.workDir,
            gprPath: p.gprPath,
            loc: p.loc,
            nbFiles: p.nbFiles,
            locVsNbFilesRatio: p.locVsNbFilesRatio,
            complexity: p.complexity,
            analysisTime: p.analysisTime[tool],
            maxIssuedMessages: p.maxIssuedMessages[tool],
            imports: p.imports
        }));

        // Create filename based on tool and category
        const filename = `${tool}_${category}_projects.json`;

        // Write to file
        Deno.writeTextFileSync(
            join(this.rootDir, "results", filename),
            JSON.stringify(exportData, null, 2)
        );
    }

    private calculateCorrelation(x: number[], y: number[]): number {
        const n = x.length;
        const sumX = x.reduce((a, b) => a + b, 0);
        const sumY = y.reduce((a, b) => a + b, 0);
        const sumXY = x.reduce((total, xi, i) => total + xi * y[i], 0);
        const sumX2 = x.reduce((total, xi) => total + xi * xi, 0);
        const sumY2 = y.reduce((total, yi) => total + yi * yi, 0);

        const numerator = n * sumXY - sumX * sumY;
        const denominator = Math.sqrt((n * sumX2 - sumX * sumX) * (n * sumY2 - sumY * sumY));

        return denominator === 0 ? 0 : numerator / denominator;
    }

    private compareCategoriesAcrossTools(analysedProjects: ProjectAnalysis[], exporter: DocumentExporter): string {
        const output: string[] = [];

        // Helper function to determine if a project is "fast" for a given tool
        const isFastProject = (project: ProjectAnalysis, tool: ToolKeyType) =>
            project.analysisTime[tool] < this.triggerNumber;

        // Count projects in each category combination
        let bothNormal = 0;    // C1 for both
        let adacNormalGnatcFast = 0;  // C1 for ADAC, C2 for GNATC
        let adacFastGnatcNormal = 0;  // C2 for ADAC, C1 for GNATC
        let bothFast = 0;      // C2 for both

        const bothNormalProjects: ProjectAnalysis[] = [];
        const adacNormalGnatcFastProjects: ProjectAnalysis[] = [];
        const adacFastGnatcNormalProjects: ProjectAnalysis[] = [];
        const bothFastProjects: ProjectAnalysis[] = [];

        const bothNormalTable: string[] = [];
        const adacNormalGnatcFastTable: string[] = [];
        const adacFastGnatcNormalTable: string[] = [];
        const bothFastTable: string[] = [];

        const c1Projects: ProjectAnalysis[] = [];
        const c2Projects: ProjectAnalysis[] = [];

        analysedProjects.forEach(project => {
            const isAdacFast = isFastProject(project, 'adactl');
            const isGnatcFast = isFastProject(project, 'gnatcheck_1cores');

            if (!isAdacFast && !isGnatcFast) {
                bothNormal++;
                bothNormalProjects.push(project);
                bothNormalTable.push(project.gprPath);
                c1Projects.push(project);
            }
            else if (!isAdacFast && isGnatcFast) {
                adacNormalGnatcFast++;
                adacNormalGnatcFastProjects.push(project);
                adacNormalGnatcFastTable.push(project.gprPath);

                const c1Project = { ...project, analysisTime: { ...project.analysisTime, gnatcheck_1cores: 0, gnatcheck_32cores: 0 }, maxIssuedMessages: { ...project.maxIssuedMessages, gnatcheck_1cores: 0, gnatcheck_32cores: 0 } };
                c1Projects.push(c1Project);
                const c2Project = { ...project, analysisTime: { ...project.analysisTime, adactl: 0 }, maxIssuedMessages: { ...project.maxIssuedMessages, adactl: 0 } };
                c2Projects.push(c2Project);
            }
            else if (isAdacFast && !isGnatcFast) {
                adacFastGnatcNormal++;
                adacFastGnatcNormalProjects.push(project);
                adacFastGnatcNormalTable.push(project.gprPath);

                const c1Project = { ...project, analysisTime: { ...project.analysisTime, adactl: 0 }, maxIssuedMessages: { ...project.maxIssuedMessages, adactl: 0 } };
                c1Projects.push(c1Project);
                const c2Project = { ...project, analysisTime: { ...project.analysisTime, gnatcheck_1cores: 0, gnatcheck_32cores: 0 }, maxIssuedMessages: { ...project.maxIssuedMessages, gnatcheck_1cores: 0, gnatcheck_32cores: 0 } };
                c2Projects.push(c2Project);
            }
            else {
                bothFast++;
                bothFastProjects.push(project);
                bothFastTable.push(project.gprPath);
                c2Projects.push(project);
            }
        });

        // Add cross-tool comparison section
        output.push(exporter.addTitle("Cross-Tool Cluster Comparison", 1));
        output.push(`Comparing ${analysedProjects.length} projects distribution between AdaControl and GNATcheck:\n`);

        // Calculate and display percentages
        const total = bothNormal + adacNormalGnatcFast + adacFastGnatcNormal + bothFast;
        output.push(exporter.formatTable(
            [
                { name: "Category", key: "category", align: "left" },
                { name: "Number of Projects", key: "count", align: "right" },
                { name: "Percentage", key: "percentage", align: "right" }
            ],
            [
                { category: "C1 (Normal) for both tools", count: formatNumber(bothNormal), percentage: `${((bothNormal / total) * 100).toFixed(1)}%` },
                { category: "C1 for AdaControl, C2 (Fast) for GNATcheck", count: formatNumber(adacNormalGnatcFast), percentage: `${((adacNormalGnatcFast / total) * 100).toFixed(1)}%` },
                { category: "C2 for AdaControl, C1 for GNATcheck", count: formatNumber(adacFastGnatcNormal), percentage: `${((adacFastGnatcNormal / total) * 100).toFixed(1)}%` },
                { category: "C2 (Fast) for both tools", count: formatNumber(bothFast), percentage: `${((bothFast / total) * 100).toFixed(1)}%` }
            ],
            "Distribution of projects"
        ));

        output.push(exporter.formatTable(
            [
                { name: "AdaControl / GNATcheck", key: "category", align: "left", diagbox: { direction: "tlbr", splitChar: "/"} },
                { name: "C1 (Normal)", key: "c1", align: "right" },
                { name: "C2 (Fast)", key: "c2", align: "right" }
            ],
            [
                {
                    category: exporter.bold("C1 (Normal)"),
                    c1: `${bothNormal} (${((bothNormal / total) * 100).toFixed(1)}%)`,
                    c2: `${adacNormalGnatcFast} (${((adacNormalGnatcFast / total) * 100).toFixed(1)}%)`
                },
                {
                    category: exporter.bold("C2 (Fast)"),
                    c1: `${adacFastGnatcNormal} (${((adacFastGnatcNormal / total) * 100).toFixed(1)}%)`,
                    c2: `${bothFast} (${((bothFast / total) * 100).toFixed(1)}%)`
                }
            ],
            "2x2 Contingency Table"
        ));

        // Helper function to calculate average for a specific property across projects
        const calcAvg = (projects: ProjectAnalysis[], selector: (p: ProjectAnalysis) => number) => {
            return projects.length ? projects.reduce((sum, p) => sum + selector(p), 0) / projects.length : 0;
        };

        // Add new subsection comparing metrics across clusters
        output.push(exporter.addTitle("Cluster Metric Comparison", 2));
        output.push("Comparing key metrics across the four identified clusters:\n");

        // Complexity comparison
        output.push(exporter.addTitle("Complexity Metrics", 3));
        output.push(exporter.formatTable(
            [
                { name: "Cluster", key: "cluster", align: "left" },
                { name: "Avg. LoC", key: "loc", align: "right" },
                { name: "Avg. Files", key: "nbFiles", align: "right" },
                { name: "Avg. LoC/Files", key: "locVsNbFilesRatio", align: "right" },
                { name: "Avg. Complexity", key: "complexity", align: "right" },
                { name: "Avg. AdaControl Time", key: "adactlTime", align: "right" },
                { name: "Avg. GNATcheck Time", key: "gnatcTime", align: "right" }
            ],
            [
                {
                    cluster: "C1 (Normal) for both tools",
                    loc: formatNumber(calcAvg(bothNormalProjects, p => p.loc), 2),
                    nbFiles: formatNumber(calcAvg(bothNormalProjects, p => p.nbFiles), 2),
                    locVsNbFilesRatio: formatNumber(calcAvg(bothNormalProjects, p => p.locVsNbFilesRatio), 2),
                    complexity: formatNumber(calcAvg(bothNormalProjects, p => p.complexity), 2),
                    adactlTime: `${formatNumber(calcAvg(bothNormalProjects, p => p.analysisTime['adactl']), 3)}s`,
                    gnatcTime: `${formatNumber(calcAvg(bothNormalProjects, p => p.analysisTime['gnatcheck_1cores']), 3)}s`
                },
                {
                    cluster: "C1 for AdaControl, C2 for GNATcheck",
                    loc: formatNumber(calcAvg(adacNormalGnatcFastProjects, p => p.loc), 2),
                    nbFiles: formatNumber(calcAvg(adacNormalGnatcFastProjects, p => p.nbFiles), 2),
                    locVsNbFilesRatio: formatNumber(calcAvg(adacNormalGnatcFastProjects, p => p.locVsNbFilesRatio), 2),
                    complexity: formatNumber(calcAvg(adacNormalGnatcFastProjects, p => p.complexity), 2),
                    adactlTime: `${formatNumber(calcAvg(adacNormalGnatcFastProjects, p => p.analysisTime['adactl']), 3)}s`,
                    gnatcTime: `${formatNumber(calcAvg(adacNormalGnatcFastProjects, p => p.analysisTime['gnatcheck_1cores']), 3)}s`
                },
                {
                    cluster: "C2 for AdaControl, C1 for GNATcheck",
                    loc: formatNumber(calcAvg(adacFastGnatcNormalProjects, p => p.loc), 2),
                    nbFiles: formatNumber(calcAvg(adacFastGnatcNormalProjects, p => p.nbFiles), 2),
                    locVsNbFilesRatio: formatNumber(calcAvg(adacFastGnatcNormalProjects, p => p.locVsNbFilesRatio), 2),
                    complexity: formatNumber(calcAvg(adacFastGnatcNormalProjects, p => p.complexity), 2),
                    adactlTime: `${formatNumber(calcAvg(adacFastGnatcNormalProjects, p => p.analysisTime['adactl']), 3)}s`,
                    gnatcTime: `${formatNumber(calcAvg(adacFastGnatcNormalProjects, p => p.analysisTime['gnatcheck_1cores']), 3)}s`
                },
                {
                    cluster: "C2 (Fast) for both tools",
                    loc: formatNumber(calcAvg(bothFastProjects, p => p.loc), 2),
                    nbFiles: formatNumber(calcAvg(bothFastProjects, p => p.nbFiles), 2),
                    locVsNbFilesRatio: formatNumber(calcAvg(bothFastProjects, p => p.locVsNbFilesRatio), 2),
                    complexity: formatNumber(calcAvg(bothFastProjects, p => p.complexity), 2),
                    adactlTime: `${formatNumber(calcAvg(bothFastProjects, p => p.analysisTime['adactl']), 3)}s`,
                    gnatcTime: `${formatNumber(calcAvg(bothFastProjects, p => p.analysisTime['gnatcheck_1cores']), 3)}s`
                }
            ],
            "Average complexity metrics by cluster"
        ));

        // Import metrics comparison
        output.push(exporter.addTitle("Import Metrics", 3));
        output.push(exporter.formatTable(
            [
                { name: "Cluster", key: "cluster", align: "left" },
                { name: "Total Imports", key: "total", align: "right" },
                { name: "Std. Ada", key: "std", align: "right" },
                { name: "GNAT", key: "gnat", align: "right" },
                { name: "System", key: "sys", align: "right" },
                { name: "Custom", key: "custom", align: "right" }
            ],
            [
                {
                    cluster: "C1 (Normal) for both tools",
                    total: formatNumber(calcAvg(bothNormalProjects, p => p.imports.totalImports), 2),
                    std: formatNumber(calcAvg(bothNormalProjects, p => p.imports.stdLibImports), 2),
                    gnat: formatNumber(calcAvg(bothNormalProjects, p => p.imports.gnatImports), 2),
                    sys: formatNumber(calcAvg(bothNormalProjects, p => p.imports.systemImports), 2),
                    custom: formatNumber(calcAvg(bothNormalProjects, p => p.imports.customImports), 2)
                },
                {
                    cluster: "C1 for AdaControl, C2 for GNATcheck",
                    total: formatNumber(calcAvg(adacNormalGnatcFastProjects, p => p.imports.totalImports), 2),
                    std: formatNumber(calcAvg(adacNormalGnatcFastProjects, p => p.imports.stdLibImports), 2),
                    gnat: formatNumber(calcAvg(adacNormalGnatcFastProjects, p => p.imports.gnatImports), 2),
                    sys: formatNumber(calcAvg(adacNormalGnatcFastProjects, p => p.imports.systemImports), 2),
                    custom: formatNumber(calcAvg(adacNormalGnatcFastProjects, p => p.imports.customImports), 2)
                },
                {
                    cluster: "C2 for AdaControl, C1 for GNATcheck",
                    total: formatNumber(calcAvg(adacFastGnatcNormalProjects, p => p.imports.totalImports), 2),
                    std: formatNumber(calcAvg(adacFastGnatcNormalProjects, p => p.imports.stdLibImports), 2),
                    gnat: formatNumber(calcAvg(adacFastGnatcNormalProjects, p => p.imports.gnatImports), 2),
                    sys: formatNumber(calcAvg(adacFastGnatcNormalProjects, p => p.imports.systemImports), 2),
                    custom: formatNumber(calcAvg(adacFastGnatcNormalProjects, p => p.imports.customImports), 2)
                },
                {
                    cluster: "C2 (Fast) for both tools",
                    total: formatNumber(calcAvg(bothFastProjects, p => p.imports.totalImports), 2),
                    std: formatNumber(calcAvg(bothFastProjects, p => p.imports.stdLibImports), 2),
                    gnat: formatNumber(calcAvg(bothFastProjects, p => p.imports.gnatImports), 2),
                    sys: formatNumber(calcAvg(bothFastProjects, p => p.imports.systemImports), 2),
                    custom: formatNumber(calcAvg(bothFastProjects, p => p.imports.customImports), 2)
                }
            ],
            "Average import metrics by cluster"
        ));

        // Import categories comparison
        const categoryNames = this.stdLibCategories.map(cat => cat.name);
        const categoryColumns = [
            { name: "Cluster", key: "cluster", align: "left" as const },
            ...categoryNames.map(name => ({ name: name, key: name, align: "right" as const }))
        ];

        const categoryRows = [
            {
                cluster: "C1 (Normal) for both tools",
                ...Object.fromEntries(categoryNames.map(cat => [
                    cat,
                    formatNumber(calcAvg(bothNormalProjects, p => p.imports.categorizedImports[cat] || 0), 2)
                ]))
            },
            {
                cluster: "C1 for AdaControl, C2 for GNATcheck",
                ...Object.fromEntries(categoryNames.map(cat => [
                    cat,
                    formatNumber(calcAvg(adacNormalGnatcFastProjects, p => p.imports.categorizedImports[cat] || 0), 2)
                ]))
            },
            {
                cluster: "C2 for AdaControl, C1 for GNATcheck",
                ...Object.fromEntries(categoryNames.map(cat => [
                    cat,
                    formatNumber(calcAvg(adacFastGnatcNormalProjects, p => p.imports.categorizedImports[cat] || 0), 2)
                ]))
            },
            {
                cluster: "C2 (Fast) for both tools",
                ...Object.fromEntries(categoryNames.map(cat => [
                    cat,
                    formatNumber(calcAvg(bothFastProjects, p => p.imports.categorizedImports[cat] || 0), 2)
                ]))
            }
        ];

        output.push(exporter.addTitle("Standard Library Categories", 3));
        output.push(exporter.formatTable(
            categoryColumns,
            categoryRows,
            "Average standard library imports by category and cluster"
        ));

        // C1 vs C2 comparison section
        output.push(exporter.addTitle("C1 (Normal) vs C2 (Fast) Comparison", 2));
        output.push("Comparing metrics between normal (C1) and fast (C2) projects.\n");
        output.push("All clusters may contain AdaControl and GNATcheck.\n");

        // Complexity comparison
        output.push(exporter.addTitle("Complexity Metrics", 3));
        output.push(exporter.formatTable(
            [
                { name: "Metric", key: "metric", align: "left" },
                { name: "C1 (Normal)", key: "c1", align: "right" },
                { name: "C2 (Fast)", key: "c2", align: "right" }
            ],
            [
                {
                    metric: "Projects",
                    c1: formatNumber(c1Projects.length),
                    c2: formatNumber(c2Projects.length)
                },
                {
                    metric: "Avg. LoC",
                    c1: formatNumber(calcAvg(c1Projects, p => p.loc), 2),
                    c2: formatNumber(calcAvg(c2Projects, p => p.loc), 2)
                },
                {
                    metric: "Avg. Files",
                    c1: formatNumber(calcAvg(c1Projects, p => p.nbFiles), 2),
                    c2: formatNumber(calcAvg(c2Projects, p => p.nbFiles), 2)
                },
                {
                    metric: "Avg. LoC/Files (Global)",
                    c1: formatNumber(computeGlobalLocVsFilesRatio(c1Projects), 2),
                    c2: formatNumber(computeGlobalLocVsFilesRatio(c2Projects), 2)
                },
                {
                    metric: "Avg. LoC/Files (Computed by project)",
                    c1: formatNumber(calcAvg(c1Projects, p => p.locVsNbFilesRatio), 2),
                    c2: formatNumber(calcAvg(c2Projects, p => p.locVsNbFilesRatio), 2)
                },
                {
                    metric: "Avg. Complexity",
                    c1: formatNumber(calcAvg(c1Projects, p => p.complexity), 2),
                    c2: formatNumber(calcAvg(c2Projects, p => p.complexity), 2)
                },
                {
                    metric: "Avg. AdaControl Time",
                    c1: `${formatNumber(calcAvg(c1Projects, p => p.analysisTime['adactl']), 3)}s`,
                    c2: `${formatNumber(calcAvg(c2Projects, p => p.analysisTime['adactl']), 3)}s`
                },
                {
                    metric: "Avg. GNATcheck Time",
                    c1: `${formatNumber(calcAvg(c1Projects, p => p.analysisTime['gnatcheck_1cores']), 3)}s`,
                    c2: `${formatNumber(calcAvg(c2Projects, p => p.analysisTime['gnatcheck_1cores']), 3)}s`
                }
            ],
            "Average complexity metrics by category"
        ));

        // Import metrics comparison
        output.push(exporter.addTitle("Import Metrics", 3));
        output.push(exporter.formatTable(
            [
                { name: "Import Type", key: "type", align: "left" },
                { name: "C1 (Normal)", key: "c1", align: "right" },
                { name: "C2 (Fast)", key: "c2", align: "right" }
            ],
            [
                {
                    type: "Total Imports",
                    c1: formatNumber(calcAvg(c1Projects, p => p.imports.totalImports), 2),
                    c2: formatNumber(calcAvg(c2Projects, p => p.imports.totalImports), 2)
                },
                {
                    type: "Std. Ada",
                    c1: formatNumber(calcAvg(c1Projects, p => p.imports.stdLibImports), 2),
                    c2: formatNumber(calcAvg(c2Projects, p => p.imports.stdLibImports), 2)
                },
                {
                    type: "GNAT",
                    c1: formatNumber(calcAvg(c1Projects, p => p.imports.gnatImports), 2),
                    c2: formatNumber(calcAvg(c2Projects, p => p.imports.gnatImports), 2)
                },
                {
                    type: "System",
                    c1: formatNumber(calcAvg(c1Projects, p => p.imports.systemImports), 2),
                    c2: formatNumber(calcAvg(c2Projects, p => p.imports.systemImports), 2)
                },
                {
                    type: "Custom",
                    c1: formatNumber(calcAvg(c1Projects, p => p.imports.customImports), 2),
                    c2: formatNumber(calcAvg(c2Projects, p => p.imports.customImports), 2)
                }
            ],
            "Average import metrics by category"
        ));

        // Import categories comparison
        output.push(exporter.addTitle("Standard Library Categories", 3));

        // Create transposed table (categories as rows, C1/C2 as columns)
        output.push(exporter.formatTable(
            [
                { name: "Category", key: "category", align: "left" },
                { name: "C1 (Normal)", key: "c1", align: "right" },
                { name: "C2 (Fast)", key: "c2", align: "right" }
            ],
            categoryNames.map(category => ({
                category: category,
                c1: formatNumber(calcAvg(c1Projects, p => p.imports.categorizedImports[category] || 0), 2),
                c2: formatNumber(calcAvg(c2Projects, p => p.imports.categorizedImports[category] || 0), 2)
            })),
            "Average standard library imports by category"
        ));

        // Add unique imports subsection for C1 vs C2 comparison
        output.push(exporter.addTitle("Unique Imports Between C1 and C2", 3));
        output.push("Comparing non-custom imports that are unique to C1 (Normal) vs C2 (Fast) projects:\n");

        // Get all non-custom imports for each category
        const c1Imports = [...new Set(c1Projects.flatMap(p => p.imports.nonCustomImports))].sort();
        const c2Imports = [...new Set(c2Projects.flatMap(p => p.imports.nonCustomImports))].sort();

        // Calculate unique imports for each category
        const uniqueC1Imports = c1Imports.filter(imp => !c2Imports.includes(imp));
        const uniqueC2Imports = c2Imports.filter(imp => !c1Imports.includes(imp));

        // Display unique imports for each category
        if (uniqueC1Imports.length > 0) {
            output.push(exporter.addTitle("Unique to C1 (Normal) projects", 4));
            output.push(exporter.codeBlock(uniqueC1Imports.join('\n')));
        }

        if (uniqueC2Imports.length > 0) {
            output.push(exporter.addTitle("Unique to C2 (Fast) projects", 4));
            output.push(exporter.codeBlock(uniqueC2Imports.join('\n')));
        }

        // Add unique imports subsection
        output.push(exporter.addTitle("Unique Standard/GNAT/Interface/System Imports", 3));
        output.push("Comparing non-custom imports that are unique to each cluster:\n");

        // Get all non-custom imports for each cluster
        const bothNormalImports = [...new Set(bothNormalProjects.flatMap(p => p.imports.nonCustomImports))].sort();
        const adacNormalGnatcFastImports = [...new Set(adacNormalGnatcFastProjects.flatMap(p => p.imports.nonCustomImports))].sort();
        const adacFastGnatcNormalImports = [...new Set(adacFastGnatcNormalProjects.flatMap(p => p.imports.nonCustomImports))].sort();
        const bothFastImports = [...new Set(bothFastProjects.flatMap(p => p.imports.nonCustomImports))].sort();

        // Calculate unique imports for each cluster
        const uniqueBothNormal = bothNormalImports.filter(imp =>
            !adacNormalGnatcFastImports.includes(imp) &&
            !adacFastGnatcNormalImports.includes(imp) &&
            !bothFastImports.includes(imp));

        const uniqueAdacNormalGnatcFast = adacNormalGnatcFastImports.filter(imp =>
            !bothNormalImports.includes(imp) &&
            !adacFastGnatcNormalImports.includes(imp) &&
            !bothFastImports.includes(imp));

        const uniqueAdacFastGnatcNormal = adacFastGnatcNormalImports.filter(imp =>
            !bothNormalImports.includes(imp) &&
            !adacNormalGnatcFastImports.includes(imp) &&
            !bothFastImports.includes(imp));

        const uniqueBothFast = bothFastImports.filter(imp =>
            !bothNormalImports.includes(imp) &&
            !adacNormalGnatcFastImports.includes(imp) &&
            !adacFastGnatcNormalImports.includes(imp));

        // Display unique imports for each cluster
        if (uniqueBothNormal.length > 0) {
            output.push(exporter.addTitle("Unique to C1 (Normal) for both tools", 4));
            output.push(exporter.codeBlock(uniqueBothNormal.join('\n')));
        }

        if (uniqueAdacNormalGnatcFast.length > 0) {
            output.push(exporter.addTitle("Unique to C1 for AdaControl, C2 for GNATcheck", 4));
            output.push(exporter.codeBlock(uniqueAdacNormalGnatcFast.join('\n')));
        }

        if (uniqueAdacFastGnatcNormal.length > 0) {
            output.push(exporter.addTitle("Unique to C2 for AdaControl, C1 for GNATcheck", 4));
            output.push(exporter.codeBlock(uniqueAdacFastGnatcNormal.join('\n')));
        }

        if (uniqueBothFast.length > 0) {
            output.push(exporter.addTitle("Unique to C2 (Fast) for both tools", 4));
            output.push(exporter.codeBlock(uniqueBothFast.join('\n')));
        }

        output.push(exporter.addTitle("List of project by distribution", 2));

        output.push(exporter.addTitle("C1 (Normal) for both tools", 3));
        output.push(exporter.codeBlock(bothNormalTable.join('\n')));

        output.push(exporter.addTitle("C1 for AdaControl, C2 (Fast) for GNATcheck", 3));
        output.push(exporter.codeBlock(adacNormalGnatcFastTable.join('\n')));

        output.push(exporter.addTitle("C2 for AdaControl, C1 for GNATcheck", 3));
        output.push(exporter.codeBlock(adacFastGnatcNormalTable.join('\n')));

        output.push(exporter.addTitle("C2 (Fast) for both tools", 3));
        output.push(exporter.codeBlock(bothFastTable.join('\n')));


        return output.join('\n\n');
    }

    // Main analysis method
    public analyzeResults(results: BenchmarkResultDB[], tool: ToolKeyType, exporter: DocumentExporter): string {
        const analysedProjects = results.map(r => this.analyzeProject(r));
        const smallProjects = analysedProjects.filter(p => p.loc <= MAX_LOC);
        const largeProjects = analysedProjects.filter(p => p.loc > MAX_LOC);
        const fastProjects = smallProjects.filter(p => p.analysisTime[tool] < this.triggerNumber);
        const normalProjects = smallProjects.filter(p => p.analysisTime[tool] >= this.triggerNumber);

        // Export projects to JSON
        this.exportProjectsToJson(analysedProjects, tool, "all");
        this.exportProjectsToJson(fastProjects, tool, "fast");
        this.exportProjectsToJson(normalProjects, tool, "normal");
        this.exportProjectsToJson([...fastProjects, ...largeProjects], tool, "all-fast");
        this.exportProjectsToJson([...normalProjects, ...largeProjects], tool, "all-normal");

        const output: string[] = [exporter.addTitle(tool) + "\n"];

        // Add fast projects section
        if (fastProjects.length > 0) {
            const fastTimes = fastProjects.map(p => p.analysisTime[tool]).sort((a, b) => a - b);
            const fastStats = {
                min: fastTimes[0],
                q1: fastTimes[Math.floor(fastTimes.length / 4)],
                median: fastTimes[Math.floor(fastTimes.length / 2)],
                q3: fastTimes[Math.floor(3 * fastTimes.length / 4)],
                max: fastTimes[fastTimes.length - 1]
            };

            output.push(exporter.addTitle(`Fast Projects (< ${this.triggerNumber}s)`, 2));
            output.push(this.formatProjectStats(fastProjects, fastStats, exporter, tool));
        }

        // Add normal projects section
        if (normalProjects.length > 0) {
            const normalTimes = normalProjects.map(p => p.analysisTime[tool]).sort((a, b) => a - b);
            const normalStats = {
                min: normalTimes[0],
                q1: normalTimes[Math.floor(normalTimes.length / 4)],
                median: normalTimes[Math.floor(normalTimes.length / 2)],
                q3: normalTimes[Math.floor(3 * normalTimes.length / 4)],
                max: normalTimes[normalTimes.length - 1]
            };

            output.push(exporter.addTitle(`Normal Projects (≥ ${this.triggerNumber}s)`, 2));
            output.push(this.formatProjectStats(normalProjects, normalStats, exporter, tool));
        }

        // Calculate and add correlations
        const metrics = [
            { name: "LoC", getValue: (p: ProjectAnalysis) => p.loc },
            { name: "Number of Files", getValue: (p: ProjectAnalysis) => p.nbFiles },
            { name: "LoC / Files Ratio", getValue: (p: ProjectAnalysis) => p.locVsNbFilesRatio },
            { name: "Complexity", getValue: (p: ProjectAnalysis) => p.complexity },
            { name: "Total imports", getValue: (p: ProjectAnalysis) => p.imports.totalImports },
            { name: "Ada imports", getValue: (p: ProjectAnalysis) => p.imports.stdLibImports },
            { name: "GNAT imports", getValue: (p: ProjectAnalysis) => p.imports.gnatImports },
            { name: "Interface imports", getValue: (p: ProjectAnalysis) => p.imports.interfaceImports },
            { name: "System imports", getValue: (p: ProjectAnalysis) => p.imports.systemImports },
            { name: "Custom imports", getValue: (p: ProjectAnalysis) => p.imports.customImports },
            ...this.stdLibCategories.map(category => ({
                name: `${category.name} imports`,
                getValue: (p: ProjectAnalysis) => p.imports.categorizedImports[category.name]
            }))
        ];

        const correlations = metrics.map(metric => ({
            name: metric.name,
            value: this.calculateCorrelation(
                smallProjects.map(p => p.analysisTime[tool]),
                smallProjects.map(metric.getValue)
            )
        }));

        // Add correlations section
        output.push(exporter.addTitle("Correlations with Analysis Time", 3));
        output.push(exporter.formatTable(
            [
                { name: "Metric", key: "metric" },
                { name: "Correlation", key: "correlation", align: "right" }
            ],
            correlations.map(c => ({
                metric: c.name,
                correlation: c.value.toFixed(3)
            }))
        ));

        // Add import differences section
        const flatFastImport = fastProjects.map(e => e.imports.nonCustomImports).flat();
        const importDiff = [...new Set(
            normalProjects
                .flatMap(elt => elt.imports.nonCustomImports)
                .filter(withImport => !flatFastImport.includes(withImport))
        )].sort();

        output.push(exporter.addTitle("Unique Standard/GNAT/Interface/System Imports", 2));
        output.push("Present in normal projects but not in fast projects:");
        output.push(exporter.codeBlock(importDiff.join('\n')));

        // Add cross-tool comparison only for the first tool analysis
        if (tool === toolKey[toolKey.length - 1]) {
            output.push(this.compareCategoriesAcrossTools(analysedProjects.filter(p => p.loc <= MAX_LOC), exporter));
        }

        return output.join('\n\n');
    }

    // Format project statistics
    private formatProjectStats(projects: ProjectAnalysis[], timeStats: any, exporter: DocumentExporter, tool: ToolKeyType): string {
        const calcAvg = (selector: (p: ProjectAnalysis) => number) =>
            projects.reduce((sum, p) => sum + selector(p), 0) / projects.length;

        let output: string[] = [];

        // Basic Statistics
        output.push(exporter.addTitle("Basic Statistics", 3));
        output.push(exporter.formatTable(
            [
                { name: "Metric", key: "metric" },
                { name: "Value", key: "value", align: "right" }
            ],
            [
                { metric: "Number of projects", value: formatNumber(projects.length) },
                { metric: "Average LoC", value: formatNumber(calcAvg(p => p.loc), 2) },
                { metric: "Average number of files", value: formatNumber(calcAvg(p => p.nbFiles), 2) },
                { metric: "Average LoC/Files ratio (Global)", value: formatNumber(computeGlobalLocVsFilesRatio(projects), 2) },
                { metric: "Average LoC/Files ratio (Computed by project)", value: formatNumber(calcAvg(p => p.locVsNbFilesRatio), 2) },
                { metric: "Average complexity", value: formatNumber(calcAvg(p => p.complexity), 2) },
                { metric: "Average analysis time (" + tool + ")", value: `${formatNumber(calcAvg(p => p.analysisTime[tool]), 3)}s` }
            ]
        ));

        // Import Statistics
        output.push(exporter.addTitle("Import Statistics", 3));
        output.push(exporter.formatTable(
            [
                { name: "Category", key: "category" },
                { name: "Count", key: "count", align: "right" }
            ],
            [
                { category: "Total", count: formatNumber(calcAvg(p => p.imports.totalImports), 2) },
                { category: "Standard Ada", count: `${formatNumber(calcAvg(p => p.imports.stdLibImports), 2)} (${(calcAvg(p => p.imports.stdLibRatio) * 100).toFixed(1)}%)` },
                { category: "GNAT", count: formatNumber(calcAvg(p => p.imports.gnatImports), 2) },
                { category: "System", count: formatNumber(calcAvg(p => p.imports.systemImports), 2) },
                { category: "Interface", count: formatNumber(calcAvg(p => p.imports.interfaceImports), 2) },
                { category: "Custom", count: formatNumber(calcAvg(p => p.imports.customImports), 2) }
            ]
        ));

        // Time Distribution
        output.push(exporter.addTitle("Analysis Time Distribution", 3));
        output.push(exporter.formatTable(
            [
                { name: "Metric", key: "metric" },
                { name: "Time", key: "time", align: "right" }
            ],
            [
                { metric: "Min", time: `${timeStats.min.toFixed(3)}s` },
                { metric: "Q1", time: `${timeStats.q1.toFixed(3)}s` },
                { metric: "Median", time: `${timeStats.median.toFixed(3)}s` },
                { metric: "Q3", time: `${timeStats.q3.toFixed(3)}s` },
                { metric: "Max", time: `${timeStats.max.toFixed(3)}s` }
            ]
        ));

        // Ada Categories
        const categories = Object.entries(projects[0].imports.categorizedImports)
            .map(([category, _]) => ({
                category,
                count: formatNumber(calcAvg(p => p.imports.categorizedImports[category]), 2)
            }))
            .filter(stat => parseFloat(stat.count) > 0)
            .sort((a, b) => parseFloat(b.count) - parseFloat(a.count));

        output.push(exporter.addTitle("Ada Imports by Category", 3));
        output.push(exporter.formatTable(
            [
                { name: "Category", key: "category" },
                { name: "Count", key: "count", align: "right" }
            ],
            categories
        ));

        return output.join('\n\n');
    }
}
