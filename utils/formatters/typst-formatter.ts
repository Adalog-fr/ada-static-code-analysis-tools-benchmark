import { formatNumber } from "../utils.ts";
import { FormatProvider, TableColumn } from './formatters-interface.ts';

// Implementation of FormatProvider for Typst output
export class TypstFormatter implements FormatProvider {
    // Format title with specified level using '=' characters
    addTitle(title: string, level = 1): string {
        return `${'='.repeat(level)} ${title}`;
    }

    // Format unordered list using typst bullet points
    unorderedList(items: string[]): string {
        return items.map(item => `- ${item}`).join('\n');
    }

    // Format ordered list using typst numbering
    orderedList(items: string[]): string {
        return items.map(item => `+ ${item}`).join('\n');
    }

    // Format table data using typst table syntax
    formatTable(
        columns: TableColumn[],
        data: Record<string, any>[] | Record<string, Record<string, any>>,
        caption?: string
    ): string {
        // Convert object data to array format if needed
        const arrayData = Array.isArray(data)
            ? data
            : Object.entries(data).map(([key, value]) => ({
                [columns[0].key]: key,
                ...value
            }));
        let table = "";

        if (caption && caption.length) {
            table += "#figure(\npad(x: -2cm, table(\n"
        } else {
            table += '#pad(x: -2cm, table(\n';
        }

        // Start table with column definitions
        table += `  columns: (${columns.map(() => 'auto').join(', ')}),\n`;

        // Add headers
        table += `  ${columns.map(col => `[${col.name}]`).join(', ')},\n`;

        // Add data rows
        table += arrayData.map(row =>
            `  ${columns.map(col => {
                const value = row[col.key];
                return `[${col.format ? col.format(value) : value}]`;
            }).join(', ')}`
        ).join(',\n');

        table += '\n))';
        // Add caption if provided
        if (caption) {
            table += `,\ncaption: [${caption}],\n)\n`;
        }
        return table;
    }

    // Format metrics using typst definition list
    formatMetrics(metrics: Record<string, string | number>): string {
        return Object.entries(metrics)
            .map(([key, value]) => `/ ${key}: ${value}`)
            .join('\n');
    }

    // Format code block using typst raw block
    codeBlock(content: string, language = ''): string {
        return `\`\`\`${language}\n${content}\n\`\`\`\n`;
    }

    // Format bold text using typst strong markup
    bold(text: string): string {
        return `*${text}*`;
    }

    // Format document header with typst template
    documentHeader(title: string, metadata?: Record<string, string>): string {
        return `#import "./modules/lib.typ": *

#show: it => basic-report(
  doc-category: "Benchmark report",
  doc-title: "${title}",
  author: "",
  affiliation: "Université de Caen Normandie, France\nAdalog SAS, SIREN 527 695 704, France",
  logo: image("assets/adalog.jpg", width: 4cm),
  logo2: image("assets/UNICAEN_LOGO.svg", width: 5cm),
  language: "en",
  ${
    metadata ? Object.entries(metadata)
        .map(([key, value]) => `  ${key}: "${value}"`)
        .join(',\n') + ',\n' : ''
}  it
)

#show table.cell.where(y: 0): set text(weight: "bold")
`;
    }

    // Format document footer (empty for typst)
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
}
