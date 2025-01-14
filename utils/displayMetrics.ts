import { join } from "jsr:@std/path@^0.225.1";
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { BenchmarkResultDB, StandardDeviationResult, TimeData } from "./types.ts";
import { formatDuration } from "./utils.ts";

// Parse command-line arguments
const program = new Command()
    .option(
        "-o, --output <string>",
        "Path to the result file. If empty, it display in the CLI",
    )
    .option(
        "-f, --format <md|csv>",
        "Output format",
        "md"
    )
    .parse(Deno.args);

// Strategy Pattern interfaces
interface ExportStrategy {
    formatNumber(num: number | undefined, decimals?: number): string;
    formatFullTable(results: BenchmarkResultDB[]): string;
    formatHighStdDevTable?(phase: string, entries: HighStdDevEntry[]): string;
}

interface HighStdDevEntry {
    project: string;
    tool: string;
    loc: number;
    avg: string;
    stddev: number;
}

// Base class with common functionality
abstract class BaseExport implements ExportStrategy {
    abstract formatNumber(num: number | undefined, decimals?: number): string;
    abstract formatFullTable(results: BenchmarkResultDB[]): string;
    abstract formatHighStdDevTable?(phase: string, entries: HighStdDevEntry[]): string;

    protected extractToolData(project: BenchmarkResultDB) {
        return {
            adactl: this.extractPhases(project.benchmarkResults.adactl),
            gnat1: this.extractPhases(project.benchmarkResults.gnatcheck_1cores),
            gnat32: this.extractPhases(project.benchmarkResults.gnatcheck_32cores),
            cogralys: this.extractPhases(project.benchmarkResults.cogralys, true)
        };
    }

    private extractPhases(data: any, hasPopulateDB = false) {
        return {
            parsing: {
                avg: data?.overhead?.parsing?.average?.elapsed_time !== undefined ?
                    formatDuration(data.overhead.parsing.average.elapsed_time * 1000) : undefined,
                stddev: data?.overhead?.parsing?.standardDeviation?.elapsed_time?.percentage
            },
            populateDB: hasPopulateDB ? {
                avg: data?.overhead?.populatingDB?.average?.elapsed_time !== undefined ?
                    formatDuration(data.overhead.populatingDB.average.elapsed_time * 1000) : undefined,
                stddev: data?.overhead?.populatingDB?.standardDeviation?.elapsed_time?.percentage
            } : {
                avg: undefined,
                stddev: undefined
            },
            run: {
                avg: data?.run?.average?.elapsed_time !== undefined ?
                    formatDuration(data.run.average.elapsed_time * 1000) : undefined,
                stddev: data?.run?.standardDeviation?.elapsed_time?.percentage
            }
        };
    }
}

// Markdown Export Strategy
class MarkdownExport extends BaseExport {
    formatNumber(num: number | undefined, decimals = 2): string {
        if (num === undefined) return "N/A".padStart(8);
        const value = num.toFixed(decimals);
        return (num > 5 ? `**${value}**` : value).padStart(8);
    }

    formatFullTable(results: BenchmarkResultDB[]): string {
        const headers = [
            "Project Name",
            "Lines of Code",
            "Tool",
            "Parsing avg",
            "Parsing stddev%",
            "PopulateDB avg",
            "PopulateDB stddev%",
            "Run avg",
            "Run stddev%"
        ];

        let output = "# Complete Results\n\n";
        output += `| ${headers.join(" | ")} |\n`;
        output += `| ${headers.map(h => "-".repeat(h.length)).join(" | ")} |\n`;

        results.forEach(project => {
            output += this.formatProjectRows(project);
        });

        return output;
    }

    formatHighStdDevTable(phase: string, entries: HighStdDevEntry[]): string {
        let output = `\n# High Standard Deviation in ${phase} (>5%)\n\n`;
        output += "| Project Name | Lines of Code | Tool | Average | StdDev% |\n";
        output += "| ------------ | ------------- | ---- | ------- | ------- |\n";

        entries.forEach(entry => {
            output += `| ${entry.project} | ${entry.loc} | ${entry.tool} | ${entry.avg} | **${entry.stddev.toFixed(2)}** |\n`;
        });

        return output;
    }

    private formatProjectRows(project: BenchmarkResultDB): string {
        let output = "";
        const tools = this.extractToolData(project);

        let first = true;
        Object.entries(tools).forEach(([tool, phases]) => {
            const hasData = Object.values(phases).some(p => p.avg !== undefined || p.stddev !== undefined);
            if (hasData) {
                const row = [
                    first ? project.crateName : "",
                    first ? project.scc.Lines.toString() : "",
                    tool,
                    this.formatPhase(phases.parsing),
                    this.formatPhase(phases.populateDB),
                    this.formatPhase(phases.run)
                ];

                output += `| ${row.join(" | ")} |\n`;
                first = false;
            }
        });

        return output;
    }

    private formatPhase(phase: { avg: string | undefined, stddev: number | undefined }): string {
        if (phase.avg === undefined && phase.stddev === undefined) {
            return "N/A | N/A";
        }
        return `${phase.avg || "N/A"}  | ${this.formatNumber(phase.stddev)}`;
    }
}

const CSV_SEPARATOR = ";";

// CSV Export Strategy
class CSVExport extends BaseExport {
    formatNumber(num: number | undefined, decimals = 2): string {
        if (num === undefined) return "N/A";
        return num.toFixed(decimals).replace(".", ",");
    }

    formatFullTable(results: BenchmarkResultDB[]): string {
        const headers = [
            "Project Name",
            "Lines of Code",
            "Tool",
            "Parsing avg",
            "Parsing stddev%",
            "PopulateDB avg",
            "PopulateDB stddev%",
            "Run avg",
            "Run stddev%"
        ];

        let output = headers.join(CSV_SEPARATOR) + "\n";

        results.forEach(project => {
            output += this.formatProjectRows(project);
        });

        return output;
    }

    private formatProjectRows(project: BenchmarkResultDB): string {
        let output = "";
        const tools = this.extractToolData(project);

        Object.entries(tools).forEach(([tool, phases]) => {
            const hasData = Object.values(phases).some(p => p.avg !== undefined || p.stddev !== undefined);
            if (hasData) {
                const row = [
                    project.crateName,
                    project.scc.Lines.toString(),
                    tool,
                    this.formatPhase(phases.parsing),
                    this.formatPhase(phases.populateDB),
                    this.formatPhase(phases.run)
                ];

                output += row.join(CSV_SEPARATOR) + "\n";
            }
        });

        return output;
    }

    private formatPhase(phase: { avg: string | undefined, stddev: number | undefined }): string {
        if (phase.avg === undefined && phase.stddev === undefined) {
            return `N/A${CSV_SEPARATOR}N/A`;
        }
        return `${phase.avg || "N/A"}${CSV_SEPARATOR}${this.formatNumber(phase.stddev)}`;
    }
}

// Export factory
class ExportFactory {
    static createExporter(format: string): ExportStrategy {
        switch (format.toLowerCase()) {
            case 'md':
                return new MarkdownExport();
            case 'csv':
                return new CSVExport();
            default:
                throw new Error(`Unsupported format: ${format}`);
        }
    }
}

// Main execution
const results: BenchmarkResultDB[] = JSON.parse(Deno.readTextFileSync("./benchmarkResults.json"));
const exporter = ExportFactory.createExporter(program.format);

let output = exporter.formatFullTable(results);

// Add high stddev tables only for markdown format
if (program.format === 'md' && exporter.formatHighStdDevTable) {
    ['parsing', 'populateDB', 'run'].forEach(phase => {
        const entries = collectHighStdDevEntries(phase, results);
        if (entries.length > 0) {
            output += exporter.formatHighStdDevTable!(phase, entries);
        }
    });
}

// Output results
if (program.output) {
    Deno.writeTextFileSync(program.output, output);
} else {
    console.log(output);
}

function collectHighStdDevEntries(phase: string, results: BenchmarkResultDB[]): HighStdDevEntry[] {
    const entries: HighStdDevEntry[] = [];

    results.forEach(project => {
        Object.entries(project.benchmarkResults).forEach(([tool, data]) => {
            if (!data) return;

            let stddev, avg;
            if (phase === 'parsing') {
                stddev = data.overhead?.parsing?.standardDeviation?.elapsed_time?.percentage;
                avg = data.overhead?.parsing?.average?.elapsed_time;
            } else if (phase === 'populateDB') {
                stddev = data.overhead?.populatingDB?.standardDeviation?.elapsed_time?.percentage;
                avg = data.overhead?.populatingDB?.average?.elapsed_time;
            } else if (phase === 'run') {
                stddev = data.run?.standardDeviation?.elapsed_time?.percentage;
                avg = data.run?.average?.elapsed_time;
            }

            if (stddev && stddev > 5) {
                entries.push({
                    project: project.crateName,
                    tool: tool,
                    loc: project.scc.Lines,
                    avg: formatDuration(avg ? avg * 1000 : 0),
                    stddev: stddev
                });
            }
        });
    });

    return entries;
}
