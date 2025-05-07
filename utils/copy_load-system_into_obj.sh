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
source "./utils.sh"

# Function to clean path (remove ./ prefix if present)
clean_path() {
    echo "$1" | sed 's|^\./||'
}

# Function to copy the file and run the command
copy_and_run() {
    local relative_dir=$(clean_path "$1")
    local dir=$(join_paths "$SRC_DIR" "$relative_dir")

    # Check if 'release' directory exists in the given directory
    if [ -d "$dir/release" ]; then
        cp "$CWD/Storage_Error/load_system.adb" "$dir/release"
        cd "$dir/release"
    else
        cp "$CWD/Storage_Error/load_system.adb" "$dir"
        cd "$dir"
    fi

    if [ -f "load_system.ali" ]; then
        rm "load_system.ali"
    fi
    if [ -f "load_system.adt" ]; then
        rm "load_system.adt"
    fi

    # Run the command
    asis-gcc -c -gnatct load_system.adb
    cd - >/dev/null # Return to the previous directory
}

# Get the appropriate find command
FIND_CMD=$(get_find_command)

# Check if asis-gcc exists
if ! command_exists asis-gcc; then
    echo "Error: asis-gcc not found. Please install ASIS tools." >&2
    exit 1
fi

# Check if source directory exists
if [ ! -d "$SRC_DIR" ]; then
    echo "Error: Source directory '$SRC_DIR' not found" >&2
    exit 1
fi

# Check if Storage_Error directory and required file exist
if [ ! -d "$CWD/Storage_Error" ] || [ ! -f "$CWD/Storage_Error/load_system.adb" ]; then
    echo "Error: $CWD/Storage_Error/load_system.adb not found" >&2
    exit 1
fi

echo "Using $FIND_CMD to locate object directories..."
echo "Source directory: $SRC_DIR"

# Find all directories containing .o files
directories=$($FIND_CMD -t f -e o -H -I -x dirname {} \; --base-directory "$SRC_DIR" | sort -u)

if [ -z "$directories" ]; then
    echo "No object directories found."
    exit 0
fi

# Process each directory
total_dirs=$(echo "$directories" | wc -l)
current=1

for directory in $directories; do
    echo "[$current/$total_dirs] Processing directory: $directory"
    copy_and_run "$directory"
    ((current++))
done

echo "Processing complete. Total directories processed: $total_dirs"