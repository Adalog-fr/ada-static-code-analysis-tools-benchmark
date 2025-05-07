#!/bin/bash

# Function to check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Check if jq is installed
if ! command_exists jq; then
    echo "Error: jq is required but not installed"
    exit 1
fi

# Check if the cratesDB.json file exists
if [ ! -f "cratesDB.json" ]; then
    echo "Error: cratesDB.json not found"
    exit 1
fi

# Get all alireTomlPaths from complete projects that are not ignored
ALIRE_PATHS=$(jq -r '
    .crates
    | to_entries[]
    | select(.value.ignore != true)
    | .value.alireProjects[]
    | select(.projects | any(.isNeo4jDbFilesComplete == true and .isAdaCtlComplete == true and .ignore != true))
    | .alireTomlPath
' cratesDB.json | sort -u)

# Count total paths
TOTAL_PATHS=$(echo "$ALIRE_PATHS" | wc -l)

# Process a single path
process_path() {
    local toml_path=$1
    local index=$2
    local total=$3

    if [ -z "$toml_path" ]; then
        return
    fi

    # Change to the directory
    cd "$toml_path" || {
        echo "[$index/$total] Error: Could not cd to $toml_path"
        return
    }

    # Run alr printenv and create .env file
    alr printenv | grep "export" | while IFS= read -r line; do
        if [[ $line =~ ^export[[:space:]]+([A-Z_]+)=(.+)$ ]]; then
            echo "${BASH_REMATCH[1]}=${BASH_REMATCH[2]}"
        fi
    done > .env

    # Return to original directory
    cd - >/dev/null
}
export -f process_path

# Process paths in parallel
index=1
echo "$ALIRE_PATHS" | parallel --bar --jobs 0 process_path {} $index $TOTAL_PATHS {#}
