#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/utils/utils.sh"

# Function to display help
show_help() {
    echo "Usage: $0 [OPTIONS]"
    echo "Options:"
    echo "  -f, --format FORMAT   Output format (md, typst, tex). Default: md"
    echo "  -r, --rootDir PATH    Root directory of the result files. Default: ."
    echo "  -h, --help            Display this help message"
}

# Default output format
output_format="md"
root_dir="."

# Parse command-line options
while [[ $# -gt 0 ]]; do
    case $1 in
        -f|--format)
            output_format="$2"
            shift 2
            ;;
        -r|--rootDir)
            root_dir="$2"
            shift 2
            ;;
        -h|--help)
            show_help
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            show_help
            exit 1
            ;;
    esac
done

# Validate output format
if [[ ! "$output_format" =~ ^(md|typst|tex)$ ]]; then
    echo "Error: Invalid output format. Use md, typst, or tex."
    exit 1
fi

print_banner "Ada Benchmark Repository: Generate Report"

# Run the command with the specified output format
deno run --allow-all $SCRIPT_DIR/utils/cogralys-bench-util.ts generate-report --rootDir "$root_dir" -o "$output_format"
deno run --allow-all $SCRIPT_DIR/utils/cogralys-bench-util.ts generate-import-report --rootDir "$root_dir" -o "$output_format"
