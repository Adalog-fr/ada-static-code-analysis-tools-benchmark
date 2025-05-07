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
parser.add_argument('--interactive', action='store_true', help='Enable interactive hover for global-all analysis time plot')
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

def theoretical_curve(x, a, b, c):
    """Calculate theoretical curve values."""
    return a * np.power(x, b) + c

def add_project_annotations(projects, tool='adactl'):
    """Add arrows and labels for specific projects."""
    target_gprs = {
        'src/hangman/hangman.gpr': 'Hangman',
        'src/apdf/pdf_out_gnat_w_gid.gpr': 'APDF',
        'src/aicwl/sources/aicwl.gpr': 'AICWL'
    }

    for project in projects:
        if project['gprPath'] in target_gprs and tool in project['results']:
            x = project['scc']['nbLoC']
            y = project['results'][tool]['analysisTime']

            # Configure annotation properties
            arrow_props = dict(
                arrowstyle='->',
                color='#475569',
                lw=1,
                alpha=0.6
            )

            # Adjust text position based on GPR path
            if 'hangman' in project['gprPath']:
                xy_text = (20, 0)
            elif 'aicwl' in project['gprPath']:
                xy_text = (20, 20)
            else:  # apdf
                xy_text = (20, 20)

            plt.annotate(
                target_gprs[project['gprPath']],  # Use the mapping for labels
                xy=(x, y),
                xytext=xy_text,
                textcoords='offset points',
                bbox=dict(boxstyle='round,pad=0.5', fc='white', ec='#475569', alpha=0.8),
                arrowprops=arrow_props
            )

def add_empirical_curves(x_range, cluster_points=None):
    """Add empirical curves and cluster visualization using convex hull."""
    from scipy.spatial import ConvexHull
    import numpy as np

    # Define vivid colors for the curves and clusters
    curve1_color = '#ef4444'  # Red for C1
    curve2_color = '#a855f7'  # Purple for C2

    if cluster_points:
        points = np.array(cluster_points)
        x, y = points[:, 0], points[:, 1]

        # Filter points for each cluster (0-10k LoC)
        mask_10k = x <= 10000
        x_10k, y_10k = x[mask_10k], y[mask_10k]

        # Separate points into C1 (>0.7s) and C2 (<0.7s)
        mask_c1 = (y_10k > 0.7) & (y_10k < 3)
        mask_c2 = y_10k <= 0.7

        points_c1 = np.column_stack((x_10k[mask_c1], y_10k[mask_c1]))
        points_c2 = np.column_stack((x_10k[mask_c2], y_10k[mask_c2]))

        # Function to create and plot convex hull
        def plot_cluster_hull(points, color, label):
            if len(points) < 3:
                return

            # Create convex hull
            hull = ConvexHull(points)

            # Get hull vertices
            vertices = points[hull.vertices]

            # Add first point to close the polygon
            vertices = np.vstack((vertices, vertices[0]))

            # Plot filled polygon
            plt.fill(vertices[:, 0], vertices[:, 1],
                    color=color, alpha=0.1, label=f'Cluster {label}')

            # Plot border
            plt.plot(vertices[:, 0], vertices[:, 1],
                    color=color, alpha=0.3, linewidth=1)

        # Plot clusters
        for points, color, label in [(points_c1, curve1_color, 'C1'),
                                   (points_c2, curve2_color, 'C2')]:
            if len(points) >= 3:
                plot_cluster_hull(points, color, label)

    # First equation (C1): y = 1.445e-3 × x^0.707 + 0.7
    y1 = theoretical_curve(x_range, 1.445e-3, 0.707, 0.7)
    plt.plot(x_range, y1, '--', color=curve1_color, alpha=0.8, label='Empirical curve C1')

    # Second equation (C2): y = 9.905e-4 × x^0.679 + 0.010
    y2 = theoretical_curve(x_range, 9.905e-4, 0.679, 0.010)
    plt.plot(x_range, y2, '--', color=curve2_color, alpha=0.8, label='Empirical curve C2')

    # Add equations text with cluster names
    equations_text = [
        r"Empirical curve C1: $y = 1.445 \times 10^{-3} \times x^{0.707} + 0.7$ $(R^2 = 0.535)$",
        r"Empirical curve C2: $y = 9.905 \times 10^{-4} \times x^{0.679} + 0.010$ $(R^2 = 0.283)$"
    ]
    plt.figtext(0.5, 0.95, '\n'.join(equations_text), fontsize=8, ha='center',
                bbox=dict(facecolor='white', alpha=0.8, edgecolor='none'))

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

    # Only create interactive plot for global-all analysis time
    is_interactive = args.interactive and plot_type == 'analysis_time' and category_name == 'all' and rule_name is None
    is_global_all = plot_type == 'analysis_time' and category_name == 'all' and rule_name is None

    # Create figure for regular plot
    plt.figure(figsize=(12, 8))
    scatter_plots = []

    # Store project data for hover information
    hover_data = {}

    # Collect all valid points first to determine axis ranges
    all_valid_y = []

    for tool, display_name, color, marker in zip(tools, display_names, colors, markers):
        projects = data_category['projects']
        x = [project['scc']['nbLoC'] for project in projects]

        if plot_type == 'analysis_time':
            y = [get_valid_value(project, tool, 'analysisTime') for project in projects]
        elif plot_type == 'overhead':
            y = [
                (get_valid_value(project, tool, 'overheadParsing') or 0) +
                (get_valid_value(project, tool, 'overheadPopulating') or 0)
                for project in projects
            ]
        elif plot_type == 'total':
            y = [get_valid_value(project, tool, 'executionTime') for project in projects]

        # Filter out None values and store project data for hover
        valid_points = []
        for i, (x_val, y_val) in enumerate(zip(x, y)):
            if y_val is not None and y_val > 0:
                valid_points.append((x_val, y_val))
                if is_interactive:
                    project = projects[i]
                    hover_data[(x_val, y_val, tool)] = {
                        'project': project['crateName'],
                        'loc': x_val,
                        'nbFiles': project['scc']['nbFiles'],
                        'complexity': project['scc']['complexity'],
                        'time': y_val,
                        'tool': display_name,
                        'work_dir': project['workDir'],
                        'gpr_path': project['gprPath']
                    }
                    if tool in project['results']:
                        results = project['results'][tool]
                        hover_data[(x_val, y_val, tool)].update({
                            'overhead_parsing': results.get('overheadParsing'),
                            'overhead_populating': results.get('overheadPopulating'),
                            'issued_messages': results.get('issuedMessages', {}).get('maxCount')
                        })

        if valid_points:
            x_valid, y_valid = zip(*valid_points)
            all_valid_y.extend(y_valid)
            scatter = plt.scatter(x_valid, y_valid, c=color, label=display_name,
                                alpha=0.6, marker=marker, s=40, edgecolors='none')
            scatter_plots.append(scatter)

    if not all_valid_y:
        plt.close()
        return

    # Set up plot properties for regular plot
    setup_plot_properties(all_valid_y, category_name, plot_type)

    # Add interactivity if requested
    if is_interactive and hover_data:
        setup_interactivity(scatter_plots, hover_data)

    # Save plots
    if is_interactive:
        plt.show()
    else:
        base_filename = f"scatter{'Rule_' + rule_name if rule_name else ''}"
        base_filename += f"_{plot_type}_{category_name}"

        if is_global_all:
            # Collect all points for clustering
            all_points = []
            for tool in tools:
                if tool != 'cogralys':
                    projects = data_category['projects']
                    for project in projects:
                        x_val = project['scc']['nbLoC']
                        y_val = get_valid_value(project, tool, 'analysisTime')
                        if y_val is not None and y_val > 0:
                            all_points.append((x_val, y_val))

            # Create and save plot with empirical curves and clusters
            plt.figure(figsize=(12, 8))

            # Replot all data points
            for tool, display_name, color, marker in zip(tools, display_names, colors, markers):
                projects = data_category['projects']
                x = [project['scc']['nbLoC'] for project in projects]
                y = [get_valid_value(project, tool, 'analysisTime') for project in projects]

                valid_points = [(x_val, y_val) for x_val, y_val in zip(x, y) if y_val is not None and y_val > 0]
                if valid_points:
                    x_valid, y_valid = zip(*valid_points)
                    plt.scatter(x_valid, y_valid, c=color, label=display_name,
                            alpha=0.6, marker=marker, s=40, edgecolors='none')

            # Add empirical curves and clusters
            x_range = np.logspace(1, 6, 1000)
            add_empirical_curves(x_range, cluster_points=all_points)

            # Set up plot properties
            setup_plot_properties(all_valid_y, category_name, plot_type)

            # Add annotations for specific projects
            add_project_annotations(data_category['projects'])

            # Save plot with empirical curves
            save_plot(base_filename + "_with_empirical", graphics_dir)

        else:
            save_plot(base_filename, graphics_dir)

def setup_plot_properties(all_valid_y, category_name, plot_type):
    """Set up common plot properties."""
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

def setup_interactivity(scatter_plots, hover_data):
    """Set up interactive hover functionality."""
    cursor = mplcursors.cursor(scatter_plots, hover=True)

    @cursor.connect("add")
    def on_add(sel):
        x, y = sel.target
        for (x_val, y_val, tool), data in hover_data.items():
            if abs(x - x_val) < 1e-10 and abs(y - y_val) < 1e-10:
                hover_text = [
                    f"Project: {data['project']}",
                    f"Tool: {data['tool']}",
                    f"Lines of Code: {data['loc']:,}",
                    f"Number of files: {data['nbFiles']:,}",
                    f"Complexity: {data['complexity']:,}",
                    f"Time: {data['time']:.3f}s"
                ]

                if 'overhead_parsing' in data:
                    hover_text.append(f"Overhead Parsing: {data['overhead_parsing']:.3f}s")
                if 'overhead_populating' in data:
                    hover_text.append(f"Overhead Populating: {data['overhead_populating']:.3f}s")
                if 'issued_messages' in data:
                    hover_text.append(f"Issued Messages: {data['issued_messages']}")

                sel.annotation.set_text("\n".join(hover_text))
                break

def save_plot(filename, graphics_dir):
    """Save plot in multiple formats."""
    save_path = os.path.join(graphics_dir, filename)
    for format in ['svg', 'eps', 'pdf', 'png']:
        plt.savefig(f"{save_path}.{format}", format=format, bbox_inches='tight')
    plt.close()

def generate_all_plots():
    plot_types = ['analysis_time', 'overhead', 'total']
    categories = ['all', 'small', 'medium', 'large']

    if args.interactive:
        create_scatter_plot(plot_args=(plot_types[0], data['global'][categories[0]], categories[0], None))
    else:
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
    # create_scatter_plot(('analysis_time', data['global']['all'], 'all', None))
