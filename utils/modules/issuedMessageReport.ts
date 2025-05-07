import { join } from "jsr:@std/path@^0.225.1";
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { capitalCase } from "jsr:@mesqueeb/case-anything";
import { formatNumber } from "../utils.ts";
import { BenchmarkResultDB, toolKey } from "../types.ts";
import { PROJECT_ROOT as defaultProjectRoot } from "../../config.ts";
import { OutputFormat, OutputFormatType, TableAlignType, TableCell } from "../formatters/formatters-interface.ts";
import { DocumentExporter } from "../formatters/exporter.ts";

const GLOBAL_EXECUTION_KEY = "GLOBAL";
const codingRules = JSON.parse(
    Deno.readTextFileSync(
        join(defaultProjectRoot, "utils/cogralys-cli/rules/types/allRules.json")
    )
).map((elt: [string, any]) => (elt[0].toLocaleLowerCase()));

function toTitleCase(value: string) {
    return capitalCase(value.replaceAll("_", " "));
}

function determineRuleName(benchmarkFile: string): string {
    const r = benchmarkFile.match(/benchmarkResults-([^-.]+).*?\.json$/)?.[1];
    if (!r || !codingRules.includes(r)) {
        return GLOBAL_EXECUTION_KEY;
    }
    return r;
}

interface MessageCount {
    gprPath: string;
    maxCount: number;
    linesOfCode: number;
    fileCount: number;
}

interface ToolMessages {
    [key: string]: MessageCount[];
}

async function analyzeMessages(jsonPath: string): Promise<ToolMessages> {
    // Read and parse the JSON file
    const content = await Deno.readTextFile(jsonPath);
    const benchmarkResults: BenchmarkResultDB[] = JSON.parse(content);

    const results: ToolMessages = {};

    // Get rule name from file path
    const ruleName = determineRuleName(jsonPath);

    for (const benchmark of benchmarkResults) {
        // Check each tool
        for (const tool of toolKey) {
            if (!results[tool]) {
                results[tool] = [];
            }

            let maxCount = 0;

            // Special handling for Cogralys
            if (tool === 'cogralys') {
                if (ruleName !== GLOBAL_EXECUTION_KEY) {
                    // Get results from specific rule
                    const ruleResult = benchmark.benchmarkResults.cogralys.ruleResults[ruleName];
                    if (ruleResult && ruleResult.issuedMessages.maxCount > 0) {
                        maxCount = ruleResult.issuedMessages.maxCount;
                    }
                } else {
                    // Get results from run
                    maxCount = benchmark.benchmarkResults.cogralys.run.issuedMessages.maxCount;
                }
            } else {
                // Handle other tools
                const toolResults = benchmark.benchmarkResults[tool].run.issuedMessages.maxCount;
                if (toolResults > 0) {
                    maxCount = toolResults;
                }
            }

            // Add to results if maxCount > 0
            if (maxCount > 0) {
                results[tool].push({
                    gprPath: benchmark.gprPath,
                    maxCount: maxCount,
                    linesOfCode: benchmark.scc.Lines,
                    fileCount: benchmark.scc.Count
                });
            }
        }
    }

    return results;
}

function formatMessagesTable(messages: ToolMessages, outputFormat: OutputFormatType): string {
    const exporter = new DocumentExporter(outputFormat);

    // Prepare data for the table
    const tableData: Record<string, any>[] = [];

    // Collect all unique gprPaths and their metadata
    const projectMetadata = new Map<string, { linesOfCode: number; fileCount: number }>();
    for (const toolResults of Object.values(messages)) {
        for (const result of toolResults) {
            projectMetadata.set(result.gprPath, {
                linesOfCode: result.linesOfCode,
                fileCount: result.fileCount
            });
        }
    }

    // Create a row for each gprPath
    for (const [gprPath, metadata] of projectMetadata) {
        const row: Record<string, any> = {
            gprPath: gprPath,
            linesOfCode: metadata.linesOfCode,
            fileCount: metadata.fileCount
        };

        // Add maxCount for each tool
        for (const tool of toolKey) {
            const toolResult = messages[tool]?.find(r => r.gprPath === gprPath);
            row[tool] = toolResult ? toolResult.maxCount : 0;
        }

        tableData.push(row);
    }

    // Sort by lines of code (descending)
    tableData.sort((a, b) => b.linesOfCode - a.linesOfCode);

    // Define table columns
    const columns: TableCell[] = [
        { name: "Project", key: "gprPath", align: "left" },
        ...toolKey.map(tool => ({
            name: toTitleCase(tool),
            key: tool,
            align: "right" as TableAlignType,
            format: (value: number) => formatNumber(value)
        })),
        {
            name: "Lines of Code",
            key: "linesOfCode",
            align: "right",
            format: (value: number) => formatNumber(value)
        },
        {
            name: "Files",
            key: "fileCount",
            align: "right",
            format: (value: number) => formatNumber(value)
        },
    ];

    // Get rule name from the benchmark file
    const ruleName = determineRuleName(Deno.args[0]);
    const caption = ruleName !== GLOBAL_EXECUTION_KEY
        ? `Message counts by tool for rule: ${ruleName}`
        : "Message counts by tool";

    return exporter.formatTable(columns, tableData, caption);
}

export function initializeModule(program: Command): void {
    program
        .command("analyze-messages")
        .description(
            "Generate benchmark report. This script shall be called after benchmark GNATcheck, AdaControl and Cogralys, and after aggregate-results."
        )
        .option(
            "-f, --file <string>",
            "Path to the file to analyze",
        )
        .option(
            "-o, --output <string>",
            `Output format (Possible values: ${OutputFormat.join("|")})`,
            "cli"
        )
        .action(handler);
}

async function handler(options: { file: string, output: OutputFormatType }) {
    try {
        const messages = await analyzeMessages(options.file);
        const table = formatMessagesTable(messages, options.output);
        console.log(table);
    } catch (error) {
        console.error("Error analyzing messages:", error);
        Deno.exit(1);
    }
}

if (import.meta.main) {
    const program = new Command();
    initializeModule(program);
    program.parse(Deno.args);
}
