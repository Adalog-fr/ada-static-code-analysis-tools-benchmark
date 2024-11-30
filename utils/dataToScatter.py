#!/usr/bin/python3

import json
import matplotlib.pyplot as plt
import numpy as np
import mplcursors

# Load JSON data
with open('benchmarkResultByProject.json', 'r') as f:
    data = json.load(f)

# Define a mapping between internal tool names and display names
tool_mapping = {
    'adactl': 'AdaControl',
    'cogralys': 'Cogralys',
    'gnatcheck_1cores': 'GNATcheck (1 core)',
    'gnatcheck_32cores': 'GNATcheck (32 cores)'
}

tools = list(tool_mapping.keys())
display_names = list(tool_mapping.values())
colors = ['#ef4444', '#3b82f6', '#22c55e', '#a855f7']
markers = ['o', 'o', 'o', 'o']

def create_scatter_plot(plot_type):
    plt.figure(figsize=(12, 8))

    scatter_plots = []

    for tool, display_name, color, marker in zip(tools, display_names, colors, markers):
        x = [item['scc']['loc'] for item in data]
        if plot_type == 'analysis_time':
            y = [item['results'][tool]['executionTime'] for item in data]
        elif plot_type == 'overhead':
            y = [item['results'][tool]['overheadParsing'] + item['results'][tool]['overheadPopulating'] for item in data]
        elif plot_type == 'total':
            y = [item['results'][tool]['executionTime'] +
                 item['results'][tool]['overheadParsing'] +
                 item['results'][tool]['overheadPopulating'] for item in data]

        scatter = plt.scatter(x, y, c=color, label=display_name, alpha=0.6, marker=marker, s=40, edgecolors='none')
        scatter_plots.append(scatter)

        # Filter out zero values for trend line calculation
        non_zero_mask = np.array(y) > 0
        x_non_zero = np.array(x)[non_zero_mask]
        y_non_zero = np.array(y)[non_zero_mask]
        if len(x_non_zero) > 1:  # Only calculate trend line if we have at least two non-zero points
            # Add trend line
            z = np.polyfit(np.log10(x_non_zero), np.log10(y_non_zero), 1)
            p = np.poly1d(z)
            plt.plot(x, 10**p(np.log10(x)), c=color, linestyle='-', linewidth=1, alpha=0.8)

    if plot_type == 'analysis_time':
        # Add the GNATcheck empirical trend line
        plt.axline((194, 0.013), (192845, 8), color='#14b8a6', linestyle=(5, (10, 3)), linewidth=1, label='GNATcheck empirical trend')

    plt.xscale('log')
    plt.yscale('log')

    plt.xlabel('Lines of Code', color='#475569')
    if plot_type == 'analysis_time':
        plt.ylabel('Analysis Time (seconds)', color='#475569')
    elif plot_type == 'overhead':
        plt.ylabel('Overhead (parsing + populating) (seconds)', color='#475569')
    elif plot_type == 'total':
        plt.ylabel('Execution time (analysis + parsing + populating) (seconds)', color='#475569')
    plt.legend()

    # Add grid for easier reading
    plt.grid(True, which="major", ls="-", color="#64748b", alpha=0.2)

    # Improve tick labels
    plt.xticks([10, 100, 1000, 10000, 100000, 1000000],
               ['10', '100', '1K', '10K', '100K', '1M'])
    plt.yticks([0.01, 0.1, 1, 10, 100, 1000],
               ['0.01', '0.1', '1', '10', '100', '1000'])

    # Change axis color
    plt.gca().spines['bottom'].set_color('#475569')
    plt.gca().spines['left'].set_color('#475569')
    plt.gca().tick_params(axis='x', colors='#475569')
    plt.gca().tick_params(axis='y', colors='#475569')

    # Set font to Satoshi
    plt.rcParams['font.family'] = 'Satoshi'

    plt.tight_layout()

    # Save plots without title
    if plot_type == 'analysis_time':
        base_filename = "scatterAnalysisTimeVSloc"
    elif plot_type == 'overhead':
        base_filename = "scatterOverheadVSloc"
    elif plot_type == 'total':
        base_filename = "scatterTotalTimeVSloc"

    for format in ['svg', 'eps', 'pdf', 'png']:
        plt.savefig(f"{base_filename}.{format}", format=format, bbox_inches='tight')

    # Add hover information
    cursor = mplcursors.cursor(scatter_plots, hover=True)

    @cursor.connect("add")
    def on_add(sel):
        index = sel.target.index
        item = data[index]
        tool = tools[scatter_plots.index(sel.artist)]

        hover_info = (
            f"GPR Path: {item['gprPath']}\n"
            f"Lines of Code: {item['scc']['loc']:,}\n"
            f"Complexity: {item['scc']['complexity']:,}\n"
            f"Files: {item['scc']['nbFiles']}\n"
            f"Tool: {tool_mapping[tool]}\n"
            f"Analysis Time: {item['results'][tool]['executionTime']:.2f}s\n"
            f"Overhead Parsing: {item['results'][tool]['overheadParsing']:.2f}s\n"
            f"Overhead Populating: {item['results'][tool]['overheadPopulating']:.2f}s"
        )

        sel.annotation.set_text(hover_info)
        sel.annotation.get_bbox_patch().set(fc="white", alpha=0.8)

    # Add title for display only (not in saved files)
    if plot_type == 'analysis_time':
        plt.title('Tool Performance: Analysis Time vs. Lines of Code (Log-Log Scale)')
    elif plot_type == 'overhead':
        plt.title('Tool Performance: Overhead (Parsing + Populating) vs. Lines of Code (Log-Log Scale)')
    elif plot_type == 'total':
        plt.title('Tool Performance: Execution time (Analysis + Parsing + Populating) vs. Lines of Code (Log-Log Scale)')

    plt.show()

# Create both plots
# create_scatter_plot('analysis_time')
# create_scatter_plot('overhead')
create_scatter_plot('total')
