#!/bin/bash

# Function to display usage information
usage() {
    echo "Usage: $0 [SRC_DIR]"
    echo "  SRC_DIR - Optional path to the source directory to explore"
    echo "            Default: \$CWD/../src"
    exit 1
}

# Parse command line arguments
if [ "$#" -gt 1 ]; then
    usage
fi

CWD=$(dirname "$(readlink -f "$0")")
SRC_DIR="${1:-$(join_paths "$CWD" ".." "src")}"
source "${CWD}/utils.sh"
FIND_CMD=$(get_find_command)

# Check if source directory exists
if [ ! -d "$SRC_DIR" ]; then
    echo "Error: Source directory '$SRC_DIR' not found" >&2
    exit 1
fi

# Find all .time files in subdirectories using fd
files=$($FIND_CMD -e time -t f -H -I --base-directory "$SRC_DIR" | sort -u)

total_files=$(echo "$files" | wc -l)
current=1

for time_file in $files; do
    echo "[$current/$total_files] Processing file: $time_file"
    # Extract the log prefix (path without .time extension)
    log_prefix="${time_file%.time}"

    # Call the Deno script with the time file and store its exit status
    deno run --allow-read --allow-write "${CWD}/checkErrorInLog.ts" "$time_file"
    exit_status=$?

    # Process based on exit status
    if [ $exit_status -eq 0 ]; then
        # Convert time file to JSON if no errors were found
        jc --time -p -r < "$time_file" > "$time_file.json"
    fi
    ((current++))
done
