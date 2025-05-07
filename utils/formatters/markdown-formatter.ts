import { formatNumber } from "../utils.ts";
import { FormatProvider, TableCell } from './formatters-interface.ts';

// Implementation of FormatProvider for Markdown output
export class MarkdownFormatter implements FormatProvider {
    // Format title with specified level using '#' characters
    addTitle(title: string, level = 1): string {
        return `${'#'.repeat(level)} ${title}\n`;
    }

    // Format unordered list using markdown bullet points
    unorderedList(items: string[]): string {
        return items.map(item => `- ${item}`).join('\n');
    }

    // Format ordered list using numbers
    orderedList(items: string[]): string {
        return items.map((item, index) => `${index + 1}. ${item}`).join('\n');
    }

    // Format table data using markdown table syntax
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

        // Create header row
        const header = `| **${columns.map(col => col.name).join('** | **')}** |`;

        // Create separator row with alignment
        const separator = `|${columns.map(col => {
            const align = col.align || 'left';
            const dash = '-'.repeat(10);
            switch (align) {
                case 'right': return `${dash}:|`;
                case 'center': return `:${dash}:|`;
                default: return `${dash}|`;
            }
        }).join('')}`;

        // Create data rows
        const rows = arrayData.map(row =>
            `| ${columns.map(col => {
                const value = row[col.key];
                return col.format ? col.format(value) : value;
            }).join(' | ')} |`
        ).join('\n');

        return [header, separator, rows].join('\n') + "\n";
    }

    // Format metrics using markdown list
    formatMetrics(metrics: Record<string, string | number>): string {
        return this.unorderedList(
            Object.entries(metrics).map(([key, value]) => `**${key}:** ${value}`)
        );
    }

    // Format code block with language specification
    codeBlock(content: string, language = ''): string {
        return `\`\`\`${language}\n${content}\n\`\`\`\n`;
    }

    // Format bold text using markdown syntax
    bold(text: string): string {
        return `**${text}**`;
    }

    // Format document header (minimal for markdown)
    documentHeader(title: string): string {
        return `# ${title}\n\n`;
    }

    // Format document footer (empty for markdown)
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
