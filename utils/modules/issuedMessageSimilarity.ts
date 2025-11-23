import { join } from "jsr:@std/path@^0.225.1";
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { capitalCase } from "jsr:@mesqueeb/case-anything";
import { formatNumber } from "../utils.ts";
import { ResultData, ToolKeyType, toolKey } from "../types.ts";
import { PROJECT_ROOT as defaultProjectRoot } from "../../config.ts";
import { OutputFormat, OutputFormatType, TableAlignType, TableCell } from "../formatters/formatters-interface.ts";
import { DocumentExporter } from "../formatters/exporter.ts";

function toTitleCase(value: string) {
    return capitalCase(value.replaceAll("_", " "));
}

type ProjectRuleSimilarity = {
    ruleKey: string;
    ruleName: string;
    gprPath: string;
    loc: number;
    values: Record<ToolKeyType, number>;
    minDiff: number;
    closestPair: [ToolKeyType, ToolKeyType];
};

function computeProjectRuleSimilarities(
    resultData: ResultData,
    ruleFilter: string | undefined,
    maxDiff: number,
): ProjectRuleSimilarity[] {
    const rows: ProjectRuleSimilarity[] = [];
    const normalizedFilter = ruleFilter?.toLocaleLowerCase();

    for (const [ruleKey, ruleAggregation] of Object.entries(resultData.rules)) {
        if (normalizedFilter && ruleKey.toLocaleLowerCase() !== normalizedFilter) {
            continue;
        }

        const projects = ruleAggregation.all.projects;

        for (const project of projects) {
            const values = {} as Record<ToolKeyType, number>;
            const counts: { tool: ToolKeyType; count: number }[] = [];

            for (const tool of toolKey) {
                const count = project.results[tool]?.issuedMessages?.maxCount ?? 0;
                values[tool] = count;
                if (Number.isFinite(count)) {
                    counts.push({ tool, count });
                }
            }

            if (counts.length < 2) {
                continue;
            }

            // Find closest pair with non-zero difference
            let minDiff = Infinity;
            let closestPair: [ToolKeyType, ToolKeyType] | null = null;

            for (let i = 0; i < counts.length; i++) {
                for (let j = i + 1; j < counts.length; j++) {
                    const diff = Math.abs(counts[i].count - counts[j].count);
                    if (diff > 0 && diff <= maxDiff) {
                         if (diff < minDiff) {
                             minDiff = diff;
                             closestPair = [counts[i].tool, counts[j].tool];
                         }
                    }
                }
            }

            if (closestPair) {
                rows.push({
                    ruleKey,
                    ruleName: toTitleCase(ruleKey),
                    gprPath: project.gprPath,
                    loc: project.scc.nbLoC,
                    values,
                    minDiff,
                    closestPair,
                });
            }
        }
    }

    // Sort by rule name, then by minDiff (smaller diff first), then project
    rows.sort((a, b) => {
        const byRule = a.ruleName.localeCompare(b.ruleName);
        if (byRule !== 0) {
            return byRule;
        }
        const byDiff = a.minDiff - b.minDiff;
        if (Math.abs(byDiff) > 1e-6) {
            return byDiff;
        }
        return a.gprPath.localeCompare(b.gprPath);
    });

    return rows;
}

function formatSimilarityTable(
    rows: ProjectRuleSimilarity[],
    outputFormat: OutputFormatType,
    ruleFilter: string | undefined,
    maxDiff: number,
): string {
    if (rows.length === 0) {
        const rulePart = ruleFilter ? `rule='${ruleFilter}'` : "all rules";
        return `No project/rule combinations found with similar issued messages (filters: ${rulePart}, difference <= ${maxDiff} and > 0).`;
    }

    const exporter = new DocumentExporter(outputFormat);

    const columns: TableCell[] = [
        { name: "Rule", key: "rule", align: "left" },
        { name: "Project", key: "gprPath", align: "left" },
        { name: "LoC", key: "loc", align: "right", format: (value: number) => formatNumber(value) },
        ...toolKey.map(tool => ({
            name: toTitleCase(tool),
            key: tool,
            align: "right" as TableAlignType,
            format: (value: number) => formatNumber(value),
        })),
        {
            name: "Closest Pair",
            key: "pair",
            align: "left",
        },
        {
            name: "Diff",
            key: "diff",
            align: "right",
            format: (value: number) => formatNumber(value),
        },
    ];

    const tableData = rows.map(row => {
        const result: Record<string, string | number> = {
            rule: row.ruleName,
            gprPath: row.gprPath,
            loc: row.loc,
            diff: row.minDiff,
            pair: `${toTitleCase(row.closestPair[0])} vs ${toTitleCase(row.closestPair[1])}`,
        };

        for (const tool of toolKey) {
            result[tool] = row.values[tool];
        }

        return result;
    });

    const rulePart = ruleFilter ? `rule = ${ruleFilter}` : "all rules";
    const caption = `Projects with similar issued messages between tools (${rulePart}, difference <= ${maxDiff} and > 0)`;
    return exporter.formatTable(columns, tableData, caption);
}

export function initializeModule(program: Command): void {
    program
        .command("issued-message-similarity")
        .description(
            "List projects where issued messages differ slightly (but not zero) between at least two tools."
        )
        .option(
            "--rootDir <string>",
            "Root directory where results/result.json is located",
            defaultProjectRoot,
        )
        .option(
            "--rule <string>",
            "Filter on a specific rule (default: all rules)",
        )
        .option(
            "-d, --diff <number>",
            "Maximum difference between issued message counts to include (default: 10)",
            "10",
        )
        .option(
            "-o, --output <string>",
            `Output format (Possible values: ${OutputFormat.join("|")})`,
            "cli",
        )
        .action(handler);
}

async function handler(options: { rootDir: string; rule?: string; diff?: string | number; output: OutputFormatType }) {
    try {
        const maxDiff = options.diff !== undefined ? Number(options.diff) : 10;
        if (Number.isNaN(maxDiff) || maxDiff < 0) {
            console.error("Invalid difference value. It must be a non-negative number.");
            Deno.exit(1);
        }

        const resultJsonPath = join(options.rootDir, "results", "result.json");
        const content = await Deno.readTextFile(resultJsonPath);
        const resultData = JSON.parse(content) as ResultData;

        const rows = computeProjectRuleSimilarities(resultData, options.rule, maxDiff);
        const table = formatSimilarityTable(rows, options.output, options.rule, maxDiff);
        console.log(table);
    } catch (error) {
        console.error("Error generating issued message similarity report:", error);
        Deno.exit(1);
    }
}

if (import.meta.main) {
    const program = new Command();
    initializeModule(program);
    program.parse(Deno.args);
}
