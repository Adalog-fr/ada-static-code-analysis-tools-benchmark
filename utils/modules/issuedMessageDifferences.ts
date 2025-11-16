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

type ProjectRuleDiff = {
    ruleKey: string;
    ruleName: string;
    gprPath: string;
    values: Record<ToolKeyType, number>;
    min: number;
    max: number;
    diffPct: number;
};

function computeProjectRuleDiffs(
    resultData: ResultData,
    ruleFilter: string | undefined,
    thresholdPct: number,
): ProjectRuleDiff[] {
    const rows: ProjectRuleDiff[] = [];
    const normalizedFilter = ruleFilter?.toLocaleLowerCase();

    for (const [ruleKey, ruleAggregation] of Object.entries(resultData.rules)) {
        if (normalizedFilter && ruleKey.toLocaleLowerCase() !== normalizedFilter) {
            continue;
        }

        const projects = ruleAggregation.all.projects;

        for (const project of projects) {
            const values = {} as Record<ToolKeyType, number>;
            let min = Infinity;
            let max = -Infinity;
            let hasAny = false;

            for (const tool of toolKey) {
                const count = project.results[tool]?.issuedMessages?.maxCount ?? 0;
                values[tool] = count;
                if (!Number.isFinite(count)) {
                    continue;
                }
                if (count !== 0) {
                    hasAny = true;
                }
                if (count < min) {
                    min = count;
                }
                if (count > max) {
                    max = count;
                }
            }

            if (!hasAny) {
                continue;
            }
            if (min === Infinity || max === -Infinity || max === min) {
                continue;
            }

            const base = max === 0 ? 1 : max;
            const diffPct = ((max - min) / base) * 100;

            if (diffPct < thresholdPct) {
                continue;
            }

            rows.push({
                ruleKey,
                ruleName: toTitleCase(ruleKey),
                gprPath: project.gprPath,
                values,
                min,
                max,
                diffPct,
            });
        }
    }

    rows.sort((a, b) => {
        const byRule = a.ruleName.localeCompare(b.ruleName);
        if (byRule !== 0) {
            return byRule;
        }
        const byDiff = b.diffPct - a.diffPct;
        if (Math.abs(byDiff) > 1e-6) {
            return byDiff;
        }
        return a.gprPath.localeCompare(b.gprPath);
    });

    return rows;
}

function formatDifferencesTable(
    rows: ProjectRuleDiff[],
    outputFormat: OutputFormatType,
    ruleFilter: string | undefined,
    thresholdPct: number,
): string {
    if (rows.length === 0) {
        const rulePart = ruleFilter ? `rule='${ruleFilter}'` : "all rules";
        return `No project/rule combinations found with differing issued messages (filters: ${rulePart}, threshold >= ${thresholdPct.toFixed(2)}%).`;
    }

    const exporter = new DocumentExporter(outputFormat);

    const columns: TableCell[] = [
        { name: "Rule", key: "rule", align: "left" },
        { name: "Project", key: "gprPath", align: "left" },
        ...toolKey.map(tool => ({
            name: toTitleCase(tool),
            key: tool,
            align: "right" as TableAlignType,
            format: (value: number) => formatNumber(value),
        })),
        {
            name: "Min",
            key: "min",
            align: "right",
            format: (value: number) => formatNumber(value),
        },
        {
            name: "Max",
            key: "max",
            align: "right",
            format: (value: number) => formatNumber(value),
        },
        {
            name: "Diff (%)",
            key: "diffPct",
            align: "right",
            format: (value: number) => `${value.toFixed(2)}%`,
        },
    ];

    const tableData = rows.map(row => {
        const result: Record<string, string | number> = {
            rule: row.ruleName,
            gprPath: row.gprPath,
            min: row.min,
            max: row.max,
            diffPct: row.diffPct,
        };

        for (const tool of toolKey) {
            result[tool] = row.values[tool];
        }

        return result;
    });

    const rulePart = ruleFilter ? `rule = ${ruleFilter}` : "all rules";
    const caption = `Projects with differing issued messages between tools (${rulePart}, threshold >= ${thresholdPct.toFixed(2)}%)`;
    return exporter.formatTable(columns, tableData, caption);
}

export function initializeModule(program: Command): void {
    program
        .command("issued-message-diff")
        .description(
            "List projects where issued messages differ between tools for the same rule, with optional rule and percentage threshold filters."
        )
        .option(
            "-r, --root-dir <string>",
            "Root directory where results/result.json is located",
            defaultProjectRoot,
        )
        .option(
            "--rule <string>",
            "Filter on a specific rule (default: all rules)",
        )
        .option(
            "-t, --threshold <number>",
            "Minimum percentage difference between min and max issued message counts to include (default: 0)",
            "0",
        )
        .option(
            "-o, --output <string>",
            `Output format (Possible values: ${OutputFormat.join("|")})`,
            "cli",
        )
        .action(handler);
}

async function handler(options: { rootDir: string; rule?: string; threshold?: string | number; output: OutputFormatType }) {
    try {
        const thresholdValue = options.threshold !== undefined ? Number(options.threshold) : 0;
        if (Number.isNaN(thresholdValue) || thresholdValue < 0) {
            console.error("Invalid threshold value. It must be a non-negative number.");
            Deno.exit(1);
        }

        const resultJsonPath = join(options.rootDir, "results", "result.json");
        const content = await Deno.readTextFile(resultJsonPath);
        const resultData = JSON.parse(content) as ResultData;

        const rows = computeProjectRuleDiffs(resultData, options.rule, thresholdValue);
        const table = formatDifferencesTable(rows, options.output, options.rule, thresholdValue);
        console.log(table);
    } catch (error) {
        console.error("Error generating issued message differences report:", error);
        Deno.exit(1);
    }
}

if (import.meta.main) {
    const program = new Command();
    initializeModule(program);
    program.parse(Deno.args);
}
