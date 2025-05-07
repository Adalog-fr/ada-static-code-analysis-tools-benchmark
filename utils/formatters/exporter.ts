// Import format providers and types
import { FormatProvider, OutputFormatType, TableCell } from './formatters-interface.ts';
import { CLIFormatter } from './cli-formatter.ts';
import { MarkdownFormatter } from './markdown-formatter.ts';
import { TypstFormatter } from './typst-formatter.ts';
import { LaTeXFormatter } from './latex-formatter.ts';

// Export class to handle document formatting
export class DocumentExporter {
    private formatter: FormatProvider;

    // Initialize exporter with specified format
    constructor(format: OutputFormatType) {
        this.formatter = this.getFormatter(format);
    }

    // Get appropriate formatter based on output format
    private getFormatter(format: OutputFormatType): FormatProvider {
        switch (format) {
            case 'cli': return new CLIFormatter();
            case 'markdown': case 'md': return new MarkdownFormatter();
            case 'typst': return new TypstFormatter();
            case 'latex': case 'tex': return new LaTeXFormatter();
            default: throw new Error(`Unsupported format: ${format}`);
        }
    }

    // Format document with provided content
    formatDocument(content: string, title: string, metadata?: Record<string, string>): string {
        return [
            this.formatter.documentHeader(title, metadata),
            content,
            this.formatter.documentFooter()
        ].join('\n');
    }

    // Expose formatter methods
    addTitle(title: string, level?: number): string {
        return this.formatter.addTitle(title, level);
    }

    unorderedList(items: string[]): string {
        return this.formatter.unorderedList(items);
    }

    orderedList(items: string[]): string {
        return this.formatter.orderedList(items);
    }
    formatTable(
        columns: TableCell[],
        data: Record<string, any>[] | Record<string, Record<string, any>>,
        caption?: string
    ): string {
        return this.formatter.formatTable(columns, data, caption);
    }

    formatMetrics(metrics: Record<string, string | number>): string {
        return this.formatter.formatMetrics(metrics);
    }

    codeBlock(content: string, language?: string): string {
        return this.formatter.codeBlock(content, language);
    }

    bold(text: string): string {
        return this.formatter.bold(text);
    }

    formatNumber(value: string | number): string {
        return this.formatter.formatNumber(value);
    }

    documentHeader(title: string, metadata?: Record<string, string>): string {
        return this.formatter.documentHeader(title, metadata);
    }

    documentFooter(): string {
        return this.formatter.documentFooter();
    }

    mathEquation(equation: string, inline: boolean = false): string {
        return this.formatter.mathEquation(equation, inline);
    }
}
