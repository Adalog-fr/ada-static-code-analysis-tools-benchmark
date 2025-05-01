#!/bin/bash

cwd=$PWD

# Function to check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Function to join paths safely
join_paths() {
    local IFS="/"
    echo "$*"
}

# Function to clean path (remove ./ prefix if present)
clean_path() {
    echo "$1" | sed 's|^\./||'
}

# Function to determine which find command to use
get_find_command() {
    if command_exists fd; then
        echo "fd"
    elif command_exists fdfind; then
        echo "fdfind"
    else
        echo "Error: Neither 'fd' nor 'fdfind' command found. Please install fd-find." >&2
        exit 1
    fi
}

# Function to copy the file and run the command
copy_and_run() {
    local relative_dir=$(clean_path "$1")
    local dir=$(join_paths "src" "$relative_dir")

    # Check if 'release' directory exists in the given directory
    if [ -d "$dir/release" ]; then
        echo "obj dir: $dir/release"
        cp "$cwd/Storage_Error/load_system.adb" "$dir/release"
        cd "$dir/release"
    else
        echo "obj dir: $dir"
        cp "$cwd/Storage_Error/load_system.adb" "$dir"
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
if [ ! -d "src" ]; then
    echo "Error: 'src' directory not found" >&2
    exit 1
fi

# Check if Storage_Error directory and required file exist
if [ ! -d "Storage_Error" ] || [ ! -f "Storage_Error/load_system.adb" ]; then
    echo "Error: Storage_Error/load_system.adb not found" >&2
    exit 1
fi

echo "Using $FIND_CMD to locate object directories..."

# Find all directories containing .o files
directories=$($FIND_CMD -t f -e o -H -I -x dirname {} \; --base-directory src | sort -u)

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
