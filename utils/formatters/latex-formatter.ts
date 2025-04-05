import { FormatProvider, TableCell } from './formatters-interface.ts';

// Implementation of FormatProvider for LaTeX output
export class LaTeXFormatter implements FormatProvider {
    // Format title with specified level using LaTeX section commands
    addTitle(title: string, level = 1): string {
        const commands = ['section', 'subsection', 'subsubsection', 'paragraph', 'subparagraph'];
        return `\\${commands[level - 1]}{${this.escapeLatex(title)}}`;
    }

    // Format unordered list using LaTeX itemize environment
    unorderedList(items: string[]): string {
        return `\\begin{itemize}\n${
            items.map(item => `  \\item ${this.escapeLatex(item)}`).join('\n')
        }\n\\end{itemize}`;
    }

    // Format ordered list using LaTeX enumerate environment
    orderedList(items: string[]): string {
        return `\\begin{enumerate}\n${
            items.map(item => `  \\item ${this.escapeLatex(item)}`).join('\n')
        }\n\\end{enumerate}`;
    }

    // Format table data using LaTeX NiceTabular environment
    formatTable(
        columns: TableCell[],
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

        // Define column format based on alignment
        const colFormat = columns.map(col => {
            const align = col.align || 'left';
            switch (align) {
                case 'right': return 'r';
                case 'center': return 'c';
                default: return 'l';
            }
        }).join('');

        let table = '\\begin{table}[!ht]\n    \\centering\n';
        table += '    \\renewcommand{\\arraystretch}{1.5}\n';

        if (caption) {
            // table += `    \\caption{${this.escapeLatex(caption)}}\n`;
            table += `    \\captionsetup[table]{font={bf,sf,color=gray-600,small}, skip=0pt}\n`;
        }

        table += `    \\hspace*{-3.1cm}\\begin{NiceTabular}{${colFormat}}[hvlines, rounded-corners=6pt, rules/color=gray-200]\n`;
        table += '    \\CodeBefore\n';
        table += '        \\rowcolor{gray-100}{1-2}\n';
        table += '        \\rowcolors{3}{}{slate-50}\n';
        table += '    \\Body\n';

        // Add caption block
        if (caption) {
            table += `        \\Block{1-${colFormat.length}}{\\parbox{150mm}{\\RawCaption{\\captionof{table}{${this.escapeLatex(caption)}}\\label{table:${caption.replace(/[&%$#_{}~^\\]/g, '').toLocaleLowerCase().replace(/\s+/g, '_')}}}}} \\\\\n`;
        }

        // Add headers
        table += '        \\RowStyle[bold]{\\color{gray-600}}\n';

        const processCell = (col: TableCell): string => {
            if (col.diagbox) {
                const diagElts = col.name.split(col.diagbox.splitChar).map(elt => this.escapeLatex(elt.trim()));
                return `\\diagbox{${diagElts[0]}}{${diagElts[1]}}`
            } else {
                return `\\textbf{${this.escapeLatex(col.name)}}`
            }
        }

        table += `        ${columns.map(processCell).join(' & ')} \\\\\n`;

        // Add data rows
        table += '        \\RowStyle[nb-rows=*,color=gray-800]{}\n';
        table += arrayData.map(row =>
            `        ${columns.map(col => {
                const value = row[col.key];
                const formatted = col.format ? col.format(value) : value;
                return this.formatLatexValue(formatted);
            }).join(' & ')} \\\\\n`
        ).join('');

        table += '    \\end{NiceTabular}\n\\end{table}';
        return table;
    }

    // Format metrics using LaTeX description environment
    formatMetrics(metrics: Record<string, string | number>): string {
        return `\\begin{description}\n${
            Object.entries(metrics)
                .map(([key, value]) => `  \\item[${this.escapeLatex(key)}] ${this.formatLatexValue(value)}`)
                .join('\n')
        }\n\\end{description}`;
    }

    // Format code block using LaTeX verbatim environment
    codeBlock(content: string): string {
        return `\\begin{verbatim}\n${content}\n\\end{verbatim}`;
    }

    // Format bold text using LaTeX textbf command
    bold(text: string): string {
        return `\\textbf{${this.escapeLatex(text)}}`;
    }

    // Format document header with LaTeX preamble
    documentHeader(title: string): string {
        return `\\documentclass{article}
\\usepackage{tabularx,makecell,floatrow,nicematrix,booktabs,xcolor,caption,siunitx}
\\usepackage[hidelinks]{hyperref}
% Tailwind colors
\\definecolor{slate-50}{HTML}{f8fafc}
\\definecolor{slate-100}{HTML}{f1f5f9}
\\definecolor{slate-200}{HTML}{e2e8f0}
\\definecolor{slate-300}{HTML}{cbd5e1}
\\definecolor{slate-400}{HTML}{94a3b8}
\\definecolor{slate-500}{HTML}{64748b}
\\definecolor{slate-600}{HTML}{475569}
\\definecolor{slate-700}{HTML}{334155}
\\definecolor{slate-800}{HTML}{1e293b}
\\definecolor{slate-900}{HTML}{0f172a}
\\definecolor{slate-950}{HTML}{020617}
\\definecolor{gray-50}{HTML}{f9fafb}
\\definecolor{gray-100}{HTML}{f3f4f6}
\\definecolor{gray-200}{HTML}{e5e7eb}
\\definecolor{gray-300}{HTML}{d1d5db}
\\definecolor{gray-400}{HTML}{9ca3af}
\\definecolor{gray-500}{HTML}{6b7280}
\\definecolor{gray-600}{HTML}{4b5563}
\\definecolor{gray-700}{HTML}{374151}
\\definecolor{gray-800}{HTML}{1f2937}
\\definecolor{gray-900}{HTML}{111827}
\\definecolor{gray-950}{HTML}{030712}
\\begin{document}
\\begin{titlepage}
   \\vspace*{\\stretch{1.0}}
   \\begin{center}
      \\Large\\textbf{${title}}\\\\
   \\end{center}
   \\vspace*{\\stretch{2.0}}
\\end{titlepage}\n

\\tableofcontents

`;
    }

    // Format document footer with LaTeX end document
    documentFooter(): string {
        return '\n\\end{document}';
    }

    // Helper method to escape LaTeX special characters
    private escapeLatex(text: string): string {
        return text.replace(/[&%$#_{}~^\\]/g, '\\$&');
    }

    // Helper method to format values for LaTeX
    private formatLatexValue(value: string | number): string {
        if (typeof value === 'number') {
            return `\\num{${value}}`;
        }
        if (value.includes('%')) {
            return `\\qty{${parseFloat(value)}}{\\percent}`;
        } else if (value.match(/^\d+$/)) {
            return `\\num{${value}}`;
        }
        return this.escapeLatex(value);
    }

    formatNumber(value: number | string): string {
        if (typeof value === 'number') {
            return `\\num{${value}}`;
        } else if (value.includes('%')) {
            return `\\qty{${parseFloat(value)}}{\\percent}`;
        } else if (value.match(/^\d+$/)) {
            return `\\num{${value}}`;
        }
        return this.escapeLatex(value);
    }

    mathEquation(equation: string, inline?: boolean): string {
        return inline ? `\\(${equation}\\)` : `\\begin{equation}
${equation}
\\end{equation}
`;
    }
}
