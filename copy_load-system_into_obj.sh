#!/bin/bash

# Function to copy the file and run the command
copy_and_run() {
    local dir="$1"

    # Check if 'release' directory exists in the given directory
    if [ -d "$dir/release" ]; then
        echo "obj dir: $dir/release"
        cp /workspaces/bench-source/Storage_Error/load_system.adb "$dir/release"
        cd "$dir/release"
    else
        echo "obj dir: $dir"
        cp /workspaces/bench-source/Storage_Error/load_system.adb "$dir"
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
    cd -  # Return to the previous directory
}

# Recursive function to locate 'obj' directories and process them
locate_and_process() {
    local search_dir="$1"

    # Loop through directories and subdirectories
    for dir in "$search_dir"/*; do
        if [ -d "$dir" ]; then
            if [ "$(basename "$dir")" == "obj" ]; then
                copy_and_run "$dir"
            else
                locate_and_process "$dir"
            fi
        fi
    done
}

# Start the search from the current directory
# locate_and_process "$(pwd)"

# directories=$(find . -type d -name "*obj*" -exec sh -c 'ls -1 "{}"/*.o >/dev/null 2>&1' \; -print)
directories=$(find . -type f -name "*.o" -exec dirname {} \; | sort -u)

for directory in $directories; do
  # echo "$directory"
  copy_and_run "$directory"
done

# Done.

echo "done."
