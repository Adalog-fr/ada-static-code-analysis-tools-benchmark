import json
import os
import argparse
from pathlib import Path
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns

# Configuration
tool_mapping = {
    'adactl': 'AdaControl',
    'cogralys': 'Cogralys',
    'gnatcheck_1cores': 'GNATcheck (1 core)',
    'gnatcheck_32cores': 'GNATcheck (32 cores)'
}
tools = list(tool_mapping.keys())
display_names = list(tool_mapping.values())
colors = ['#a855f7', '#22c55e', '#ef4444', '#3b82f6']
markers_plotly = ['circle', 'diamond', 'x', 'cross']
markers_matplotlib = ['o', 'D', 'x', '+']

def load_data(root_dir):
    """Load benchmark results from JSON file."""
    json_path = Path(root_dir) / "benchmarkResults.json"
    with open(json_path, 'r') as f:
        return json.load(f)

def extract_plot_data(data):
    """Extract plotting data from benchmark results."""
    plot_data = {tool: {'x': [], 'y_value': [], 'y_percentage': [], 'names': []}
                 for tool in tools}

    for entry in data:
        loc = entry['scc']['Code']
        name = entry['crateName']

        for tool in tools:
            if tool in entry['benchmarkResults']:
                result = entry['benchmarkResults'][tool]['run']['standardDeviation']['elapsed_time']
                plot_data[tool]['x'].append(loc)
                plot_data[tool]['y_value'].append(result['value'])
                plot_data[tool]['y_percentage'].append(result['percentage'])
                plot_data[tool]['names'].append(name)

    return plot_data

def get_max_y_value(plot_data, plot_type):
    """Get maximum y value across all tools plus 10%."""
    max_y = 0
    for tool in tools:
        y_values = plot_data[tool][f'y_{plot_type}']
        if y_values:
            max_y = max(max_y, max(y_values))
    return max_y * 1.1

def format_number(n):
    """Convert number to human readable format (e.g., 1M, 30K)."""
    if n >= 1_000_000:
        return f'{n/1_000_000:.1f}M'
    if n >= 1_000:
        return f'{n/1_000:.1f}K'
    return str(int(n))

def extract_plot_data(data):
    """Extract plotting data from benchmark results."""
    metrics = {
        'run': {'path': ['run', 'standardDeviation', 'elapsed_time']},
        'parsing': {'path': ['overhead', 'parsing', 'standardDeviation', 'elapsed_time']},
        'populating': {'path': ['overhead', 'populating', 'standardDeviation', 'elapsed_time']}
    }

    plot_data = {metric: {tool: {'x': [], 'y_value': [], 'y_percentage': [], 'names': []}
                         for tool in tools}
                 for metric in metrics}

    def get_nested_value(d, path):
        """Safely get nested dictionary value"""
        for key in path:
            if key in d:
                d = d[key]
            else:
                return None
        return d

    for entry in data:
        loc = entry['scc']['Code']
        name = entry['crateName']

        for tool in tools:
            if tool in entry['benchmarkResults']:
                for metric, config in metrics.items():
                    # Skip populating metric for non-Cogralys tools
                    if metric == 'populating' and tool != 'cogralys':
                        continue

                    result = get_nested_value(entry['benchmarkResults'][tool], config['path'])
                    if result:
                        plot_data[metric][tool]['x'].append(loc)
                        plot_data[metric][tool]['y_value'].append(result['value'])
                        plot_data[metric][tool]['y_percentage'].append(result['percentage'])
                        plot_data[metric][tool]['names'].append(name)

    return plot_data

def create_static_plots(plot_data, output_dir):
    """Generate static plots using matplotlib."""
    sns.set_theme(style="whitegrid")

    # Dictionary to map metrics to their display names
    metric_names = {
        'run': 'Execution Time',
        'parsing': 'Parsing Time',
        'populating': 'Populating Time'
    }

    for metric in plot_data:
        for plot_type in ['value', 'percentage']:
            fig, ax = plt.subplots(figsize=(10, 6))
            max_y = get_max_y_value(plot_data[metric], plot_type)

            for tool, color, marker in zip(tools, colors, markers_matplotlib):
                # Skip if no data (e.g., populating for non-Cogralys tools)
                if not plot_data[metric][tool]['x']:
                    continue

                x = plot_data[metric][tool]['x']
                y = plot_data[metric][tool][f'y_{plot_type}']
                ax.scatter(x, y, c=color, marker=marker, label=tool_mapping[tool], alpha=0.7)

            ax.set_xscale('log')
            ax.set_ylim(0, max_y)

            x_ticks = [10, 100, 1000, 10000, 100000, 1000000]
            ax.set_xticks(x_ticks)
            ax.set_xticklabels([format_number(x) for x in x_ticks])

            ax.set_xlabel('Lines of Code')
            ax.set_ylabel(f'{metric_names[metric]} Standard Deviation {"(%)" if plot_type == "percentage" else "(seconds)"}')
            ax.grid(True, which="both", ls="-", alpha=0.2)
            ax.legend()

            for fmt in ['svg', 'eps', 'pdf', 'png']:
                output_path = output_dir / f'scatter_{metric}_std_{plot_type}.{fmt}'
                plt.savefig(output_path, format=fmt, bbox_inches='tight', dpi=300)

            plt.close()

def create_interactive_plot(plot_data):
    """Create interactive plot using plotly."""
    metric_names = {
        'run': 'Execution Time',
        'parsing': 'Parsing Time',
        'populating': 'Populating Time'
    }

    for metric in plot_data:
        fig = make_subplots(
            rows=1, cols=2,
            subplot_titles=(
                f'{metric_names[metric]} Standard Deviation (seconds)',
                f'{metric_names[metric]} Standard Deviation (%)'
            )
        )

        max_y_value = get_max_y_value(plot_data[metric], 'value')
        max_y_percentage = get_max_y_value(plot_data[metric], 'percentage')

        for idx, tool in enumerate(tools):
            # Skip if no data
            if not plot_data[metric][tool]['x']:
                continue

            x = plot_data[metric][tool]['x']
            y_value = plot_data[metric][tool]['y_value']
            y_percentage = plot_data[metric][tool]['y_percentage']
            names = plot_data[metric][tool]['names']

            hover_text = [
                f'Project: {name}<br>Tool: {tool_mapping[tool]}<br>LoC: {format_number(loc)}<br>'
                f'Std Dev: {val:.2f} seconds<br>Std Dev: {pct:.2f}%'
                for name, loc, val, pct in zip(names, x, y_value, y_percentage)
            ]

            # Value plot
            fig.add_trace(
                go.Scatter(
                    x=x, y=y_value,
                    name=tool_mapping[tool],
                    mode='markers',
                    marker=dict(color=colors[idx], symbol=markers_plotly[idx]),
                    hovertext=hover_text,
                    hoverinfo='text'
                ),
                row=1, col=1
            )

            # Percentage plot
            fig.add_trace(
                go.Scatter(
                    x=x, y=y_percentage,
                    name=tool_mapping[tool],
                    mode='markers',
                    marker=dict(color=colors[idx], symbol=markers_plotly[idx]),
                    hovertext=hover_text,
                    hoverinfo='text',
                    showlegend=False
                ),
                row=1, col=2
            )

        tickvals = [10, 100, 1000, 10000, 100000, 1000000]
        ticktext = [format_number(val) for val in tickvals]

        fig.update_xaxes(
            type='log',
            title_text='Lines of Code',
            ticktext=ticktext,
            tickvals=tickvals
        )

        fig.update_yaxes(type='linear', range=[0, max_y_value], col=1)
        fig.update_yaxes(type='linear', range=[0, max_y_percentage], col=2)
        fig.update_layout(height=600, width=1200, title_text=f'{metric_names[metric]} Standard Deviation')

        fig.show()

def main():
    parser = argparse.ArgumentParser(description='Generate elapsed time standard deviation scatter plots')
    parser.add_argument('--root-dir', required=True, help='Root directory containing benchmark results')
    parser.add_argument('--interactive', action='store_true', help='Show interactive plots')
    args = parser.parse_args()

    # Ensure output directory exists
    output_dir = Path(args.root_dir) / 'results' / 'graphics'
    output_dir.mkdir(parents=True, exist_ok=True)

    # Load and process data
    data = load_data(args.root_dir)
    plot_data = extract_plot_data(data)

    if args.interactive:
        create_interactive_plot(plot_data)
    else:
        create_static_plots(plot_data, output_dir)

if __name__ == "__main__":
    main()
