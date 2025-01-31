import { BenchmarkResultDB } from "./types.ts";
import { DocumentExporter } from "./formatters/exporter.ts";
import { OutputFormatType } from "./formatters/formatters-interface.ts";
import { formatNumber } from "./utils.ts";

// Constant for trigger time threshold
const TRIGGER_NUMBER = 0.7;

// Interface for project analysis results
interface ProjectAnalysis {
    crateName: string;
    workDir: string;
    gprPath: string;
    loc: number;
    complexity: number;
    analysisTime: {
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
    incerfaceImports: number;
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

export class PerformanceAnalyzer {
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
            name: "system_interface",
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

    private analyzeImports(withUnits: string[] = []): ImportAnalysis {
        const allImports = [...new Set(withUnits)];

        const gnatImports = allImports.filter(unit => unit.startsWith("gnat"));
        const incerfaceImports = allImports.filter(unit => unit.startsWith("interfaces"));
        const systemImports = allImports.filter(unit => unit.startsWith("system"));
        const stdLibImports = allImports.filter(unit => unit.startsWith("ada."));
        const customImports = allImports.filter(unit =>
            !unit.startsWith("ada.") &&
            !unit.startsWith("interfaces") &&
            !unit.startsWith("gnat") &&
            !unit.startsWith("system")
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
            incerfaceImports: incerfaceImports.length,
            customImports: customImports.length,
            stdLibRatio: allImports.length ? stdLibImports.length / allImports.length : 0,
            categorizedImports,
            nonCustomImports: [...gnatImports, ...incerfaceImports, ...systemImports, ...stdLibImports]
        };
    }

    public analyzeProject(result: BenchmarkResultDB): ProjectAnalysis {
        return {
            crateName: result.crateName,
            workDir: result.workDir,
            gprPath: result.gprPath,
            loc: result.scc.Code,
            complexity: result.scc.Complexity,
            analysisTime: {
                adactl: result.benchmarkResults.adactl.digestTime.analysisTime,
                cogralys: result.benchmarkResults.cogralys.digestTime.analysisTime,
                gnatcheck_1cores: result.benchmarkResults.gnatcheck_1cores.digestTime.analysisTime,
                gnatcheck_32cores: result.benchmarkResults.gnatcheck_32cores.digestTime.analysisTime
            },
            imports: this.analyzeImports(result.scc.unitUsage ? Object.keys(result.scc.unitUsage) : [])
        };
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

    // Main analysis method
    public analyzeResults(results: BenchmarkResultDB[], format: OutputFormatType = 'cli'): string {
        const analysedProjects = results.map(r => this.analyzeProject(r));
        const smallProjects = analysedProjects.filter(p => p.loc <= 30_000);
        const fastProjects = smallProjects.filter(p => p.analysisTime.adactl < TRIGGER_NUMBER);
        const normalProjects = smallProjects.filter(p => p.analysisTime.adactl >= TRIGGER_NUMBER);

        const exporter = new DocumentExporter(format);
        let output: string[] = [];

        // Add fast projects section
        if (fastProjects.length > 0) {
            const fastTimes = fastProjects.map(p => p.analysisTime.adactl).sort((a, b) => a - b);
            const fastStats = {
                min: fastTimes[0],
                q1: fastTimes[Math.floor(fastTimes.length / 4)],
                median: fastTimes[Math.floor(fastTimes.length / 2)],
                q3: fastTimes[Math.floor(3 * fastTimes.length / 4)],
                max: fastTimes[fastTimes.length - 1]
            };

            output.push(exporter.addTitle(`Fast Projects (<${TRIGGER_NUMBER}s)`, 2));
            output.push(this.formatProjectStats(fastProjects, fastStats, exporter));
        }

        // Add normal projects section
        if (normalProjects.length > 0) {
            const normalTimes = normalProjects.map(p => p.analysisTime.adactl).sort((a, b) => a - b);
            const normalStats = {
                min: normalTimes[0],
                q1: normalTimes[Math.floor(normalTimes.length / 4)],
                median: normalTimes[Math.floor(normalTimes.length / 2)],
                q3: normalTimes[Math.floor(3 * normalTimes.length / 4)],
                max: normalTimes[normalTimes.length - 1]
            };

            output.push(exporter.addTitle(`Normal Projects (≥${TRIGGER_NUMBER}s)`, 2));
            output.push(this.formatProjectStats(normalProjects, normalStats, exporter));
        }

        // Calculate and add correlations
        const metrics = [
            { name: "LoC", getValue: (p: ProjectAnalysis) => p.loc },
            { name: "Complexity", getValue: (p: ProjectAnalysis) => p.complexity },
            { name: "Total imports", getValue: (p: ProjectAnalysis) => p.imports.totalImports },
            { name: "Ada imports", getValue: (p: ProjectAnalysis) => p.imports.stdLibImports },
            { name: "GNAT imports", getValue: (p: ProjectAnalysis) => p.imports.gnatImports },
            { name: "Interface imports", getValue: (p: ProjectAnalysis) => p.imports.incerfaceImports },
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
                smallProjects.map(p => p.analysisTime.adactl),
                smallProjects.map(metric.getValue)
            )
        }));

        // Add correlations section
        output.push(exporter.addTitle("Correlations with Analysis Time", 2));
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

        return output.join('\n\n');
    }

    // Format project statistics
    private formatProjectStats(projects: ProjectAnalysis[], timeStats: any, exporter: DocumentExporter): string {
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
                { metric: "Average complexity", value: formatNumber(calcAvg(p => p.complexity), 2) },
                { metric: "Average analysis time (AdaCtl)", value: `${formatNumber(calcAvg(p => p.analysisTime.adactl), 3)}s` }
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
                { category: "Interface", count: formatNumber(calcAvg(p => p.imports.incerfaceImports), 2) },
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

// Main execution block
if (import.meta.main) {
    const args = Deno.args;
    if (args.length < 1) {
        console.error("Please specify the path to the JSON results file");
        console.error("Usage: deno run main.ts <json_path> [format]");
        console.error("Available formats: cli, md, typst, latex");
        Deno.exit(1);
    }

    const jsonPath = args[0];
    const format = (args[1] as OutputFormatType) || 'cli';

    try {
        const jsonContent = Deno.readTextFileSync(jsonPath);
        const results = JSON.parse(jsonContent) as BenchmarkResultDB[];
        const analyzer = new PerformanceAnalyzer();
        const output = analyzer.analyzeResults(results, format);
        console.log(output);
    } catch (error) {
        console.error("Error during analysis:", error);
        Deno.exit(1);
    }
}
