import { Table } from "https://deno.land/x/cliffy@v1.0.0-rc.4/table/mod.ts";
import { formatNumber } from "../utils.ts";
import { FormatProvider, TableCell } from './formatters-interface.ts';

// Implementation of FormatProvider for CLI output
export class CLIFormatter implements FormatProvider {
    // Format title with specified level using '#' characters
    addTitle(title: string, level = 1): string {
        return `${('#').repeat(level)} ${title}\n`;
    }

    // Format unordered list using bullet points
    unorderedList(items: string[]): string {
        return items.map(item => `* ${item}`).join('\n');
    }

    // Format ordered list using numbers
    orderedList(items: string[]): string {
        return items.map((item, index) => `${index + 1}. ${item}`).join('\n');
    }

    // Format table data using console.table style
    formatTable(
        columns: TableCell[],
        data: Record<string, any>[] | Record<string, Record<string, any>>,
        _caption?: string
    ): string {
        // Convert object data to array format if needed
        const arrayData = Array.isArray(data)
            ? data
            : Object.entries(data).map(([key, value]) => ({
                [columns[0].key]: key,
                ...value
            }));

        const header = columns.map(col => col.name);

        const rows = arrayData.map(row => {
            return columns.map(col => {
                const value = row[col.key];
                return col.format ? col.format(value) : value;
            })
        });

        return new Table()
            .header(header)
            .body(rows)
            .padding(1)
            .indent(2)
            .border()
            .toString();
    }

    // Format metrics as key-value pairs
    formatMetrics(metrics: Record<string, string | number>): string {
        return Object.entries(metrics)
            .map(([key, value]) => `${key}: ${value}`)
            .join('\n');
    }

    // Format code block with simple backticks
    codeBlock(content: string): string {
        return `\`\`\`\n${content}\n\`\`\``;
    }

    // Format bold text using asterisks
    bold(text: string): string {
        return `**${text}**`;
    }

    // Format document header (minimal for CLI)
    documentHeader(title: string): string {
        return `=== ${title} ===\n`;
    }

    // Format document footer (empty for CLI)
    documentFooter(): string {
        return '';
    }

    formatNumber(value: number | string): string {
        if (typeof value === 'number') {
            return formatNumber(value);
        } else {
            return value;
        }
    }

    // Format mathematical equations using markdown math syntax
    mathEquation(equation: string, inline?: boolean): string {
        return inline ? `\(${equation}\)` : `$$
${equation}
$$
`;
    }
}
