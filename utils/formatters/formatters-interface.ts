/**
 * Define supported output format
 */

export const OutputFormat = ['cli', 'md', 'markdown', 'typst', 'latex', 'tex'] as const;
export type OutputFormatType = typeof OutputFormat[number];

export const TableAlign = ['left', 'center', 'right'] as const;
export type TableAlignType = typeof TableAlign[number];

export interface TableCell {
    name: string;
    key: string;
    align?: TableAlignType;
    format?: (value: any) => string;
    diagbox?: { direction: "tlbr" | "bltr", splitChar: string };
}

/**
 * Interface for formatting functions
 */
export interface FormatProvider {
    /**
     * Function to format title with specified level
     */
    addTitle(title: string, level?: number): string;
    /**
     * Function to format unordered list items
     */
    unorderedList(items: string[]): string;
    /**
     * Function to format ordered list items
     */
    orderedList(items: string[]): string;
    /**
     * Function to format table data
     */
    formatTable(
        columns: TableCell[],
        data: Record<string, any>[] | Record<string, Record<string, any>>,
        caption?: string
    ): string;
    /**
     * Function to format metrics (key-value pairs)
     */
    formatMetrics(metrics: Record<string, string | number>): string;
    /**
     * Function to format code block
     */
    codeBlock(content: string, language?: string): string;
    /**
     * Function to format bold text
     */
    bold(text: string): string;

    /**
     * Format a number
     */
    formatNumber(value: number | string): string;
    /**
     * Function to format a mathematical equation in LaTeX syntax
     */
    mathEquation(equation: string, inline?: boolean): string;
    /**
     * Function to format document header
     */
    documentHeader(title: string, metadata?: Record<string, string>): string;
    /**
     * Function to format document footer
     */
    documentFooter(): string;
}
