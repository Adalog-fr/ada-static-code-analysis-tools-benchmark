#!/usr/bin/python3

import json
import matplotlib.pyplot as plt
import numpy as np
import mplcursors
import os
import argparse
from concurrent.futures import ProcessPoolExecutor
from itertools import product

# Parse command line arguments
parser = argparse.ArgumentParser(description='Generate scatter plots from result data')
parser.add_argument('--root-dir', type=str, default='../..', help='Root working directory (default: current directory)')
args = parser.parse_args()

# Convert relative paths to absolute paths
root_dir = os.path.abspath(args.root_dir)
graphics_dir = os.path.join(root_dir, 'results', 'graphics')
result_file = os.path.join(root_dir, 'results', 'result.json')

# Load JSON data
with open(result_file, 'r') as f:
    data = json.load(f)

# Create the results/graphics directory if it doesn't exist
os.makedirs(graphics_dir, exist_ok=True)

# Define a mapping between internal tool names and display names
tool_mapping = {
    'adactl': 'AdaControl',
    'cogralys': 'Cogralys',
    'gnatcheck_1cores': 'GNATcheck (1 core)',
    'gnatcheck_32cores': 'GNATcheck (32 cores)'
}

tools = list(tool_mapping.keys())
display_names = list(tool_mapping.values())
colors = ['#a855f7', '#22c55e', '#ef4444', '#3b82f6']
markers = ['o', 'D', 'x', '+']

def get_valid_value(project, tool, value_type):
    try:
        if tool not in project['results']:
            return None
        value = project['results'][tool][value_type]
        return float(value) if value is not None else None
    except (KeyError, ValueError, TypeError):
        return None

def get_axis_ranges(y_valid, category_name):
    # Predefined x ranges based on category
    x_ranges = {
        'all': ([10, 100, 1000, 10000, 100000, 1000000],
                ['10', '100', '1K', '10K', '100K', '1M']),
        'small': ([10, 100, 1000, 10000],
                 ['10', '100', '1K', '10K']),
        'medium': ([10000, 12500, 15000, 17500, 20000],
                  ['10K', '12.5K', '15K', '17.5K', '20K']),
        'large': ([20000, 50000, 100000, 200000, 500000, 1000000],
                 ['20K', '50K', '100K', '200K', '500K', '1M'])
    }

    x_ticks, x_labels = x_ranges[category_name]

    if category_name == 'all':
        y_ticks = [0.01, 0.1, 1, 10, 100, 1000]
        y_labels = ['0.01', '0.1', '1', '10', '100', '1000']
    else:
        # Calculate y range based on data
        y_min, y_max = min(y_valid), max(y_valid)
        y_magnitude_min = 10 ** np.floor(np.log10(y_min))
        y_magnitude_max = 10 ** np.ceil(np.log10(y_max))

        y_ticks = [y_magnitude_min]
        while y_ticks[-1] < y_magnitude_max:
            y_ticks.append(y_ticks[-1] * 10)

        def format_number(n):
            if n >= 1:
                return str(int(n)) if n < 1000 else (
                    f'{int(n/1000)}K' if n < 1000000 else f'{int(n/1000000)}M'
                )
            return f'{n:.2f}'

        y_labels = [format_number(y) for y in y_ticks]

    return x_ticks, x_labels, y_ticks, y_labels

def calculate_r2(x, y):
    z = np.polyfit(np.log10(x), np.log10(y), 1)
    p = np.poly1d(z)
    y_pred = 10**p(np.log10(x))
    ss_res = np.sum((np.log10(y) - np.log10(y_pred)) ** 2)
    ss_tot = np.sum((np.log10(y) - np.mean(np.log10(y))) ** 2)
    r2 = 1 - (ss_res / ss_tot)
    return r2, z[0], z[1]

def create_scatter_plot(plot_args):
    plot_type, data_category, category_name, rule_name = plot_args
    plt.figure(figsize=(12, 8))
    scatter_plots = []
    r2_text_lines = []

    # Collect all valid points first to determine axis ranges
    all_valid_y = []

    for tool, display_name, color, marker in zip(tools, display_names, colors, markers):
        x = [project['scc']['nbLoC'] for project in data_category['projects']]

        if plot_type == 'analysis_time':
            y = [get_valid_value(project, tool, 'analysisTime') for project in data_category['projects']]
        elif plot_type == 'overhead':
            y = [
                (get_valid_value(project, tool, 'overheadParsing') or 0) +
                (get_valid_value(project, tool, 'overheadPopulating') or 0)
                for project in data_category['projects']
            ]
        elif plot_type == 'total':
            y = [get_valid_value(project, tool, 'executionTime') for project in data_category['projects']]

        # Filter out None values
        valid_points = [(x_val, y_val) for x_val, y_val in zip(x, y) if y_val is not None and y_val > 0]
        if valid_points:
            x_valid, y_valid = zip(*valid_points)
            all_valid_y.extend(y_valid)
            scatter = plt.scatter(x_valid, y_valid, c=color, label=display_name, alpha=0.6, marker=marker, s=40, edgecolors='none')
            scatter_plots.append(scatter)

            x_non_zero = np.array(x_valid)
            y_non_zero = np.array(y_valid)
            if len(x_non_zero) > 1:
                r2, a, b = calculate_r2(x_non_zero, y_non_zero)
                x_range = np.array([min(x), max(x)])
                plt.plot(x_range, 10**(a*np.log10(x_range) + b), c=color, linestyle='-', linewidth=1, alpha=0.8)

                # Add R² and equation text
                equation = f"y = {10**b:.2e}x^{a:.2f}"
                r2_line = f"{display_name}: {equation} (R² = {r2:.3f})"
                r2_text_lines.append(r2_line)

    if not all_valid_y:
        plt.close()
        return

    # Add GNATcheck empirical trend line only for global-all analysis time
    if plot_type == 'analysis_time' and category_name == 'all' and rule_name is None:
        plt.axline((194, 0.013), (192845, 8), color='#ef4444', linestyle=(5, (10, 3)), linewidth=1, label='GNATcheck empirical trend')
        r2_text_lines.append("GNATcheck empirical: y = 6.25e-5x^1.2")

    # Add R² text box
    if r2_text_lines:
        r2_text = '\n'.join(r2_text_lines)
        plt.figtext(0.02, 0.02, r2_text, fontsize=8, va='bottom', bbox=dict(facecolor='white', alpha=0.8, edgecolor='none'))

    plt.xscale('log')
    plt.yscale('log')
    plt.xlabel('Lines of Code', color='#475569')

    y_label = {
        'analysis_time': 'Analysis Time (seconds)',
        'overhead': 'Overhead (parsing + populating) (seconds)',
        'total': 'Execution time (seconds)'
    }[plot_type]
    plt.ylabel(y_label, color='#475569')

    plt.legend()
    plt.grid(True, which="major", ls="-", color="#64748b", alpha=0.2)

    x_ticks, x_labels, y_ticks, y_labels = get_axis_ranges(all_valid_y, category_name)
    plt.xticks(x_ticks, x_labels)
    plt.yticks(y_ticks, y_labels)

    plt.gca().spines['bottom'].set_color('#475569')
    plt.gca().spines['left'].set_color('#475569')
    plt.gca().tick_params(axis='x', colors='#475569')
    plt.gca().tick_params(axis='y', colors='#475569')
    plt.rcParams['font.family'] = 'Satoshi'
    plt.tight_layout()

    base_filename = f"scatter{'Rule_' + rule_name if rule_name else ''}"
    base_filename += f"_{plot_type}_{category_name}"

    save_path = os.path.join(graphics_dir, base_filename)
    for format in ['svg', 'eps', 'pdf', 'png']:
        plt.savefig(f"{save_path}.{format}", format=format, bbox_inches='tight')

    plt.close()

def generate_all_plots():
    plot_types = ['analysis_time', 'overhead', 'total']
    categories = ['all', 'small', 'medium', 'large']

    # Prepare all plot arguments
    plot_args = []

    # Global data plots
    for plot_type, category in product(plot_types, categories):
        plot_args.append((plot_type, data['global'][category], category, None))

    # Rule-specific plots
    for rule_name, rule_data in data['rules'].items():
        for plot_type, category in product(plot_types, categories):
            plot_args.append((plot_type, rule_data[category], category, rule_name))

    # Use ProcessPoolExecutor for parallel execution
    with ProcessPoolExecutor() as executor:
        list(executor.map(create_scatter_plot, plot_args))

if __name__ == '__main__':
    generate_all_plots()
