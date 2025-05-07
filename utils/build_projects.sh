#!/bin/bash

# Function to handle signal (CTRL+C, CTRL+\)
function signalHandler()
{
  echo "Signal recieved, safely end the script"
  exit 1
}

trap 'signalHandler' SIGINT
trap 'signalHandler' SIGQUIT

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
CURRENT=1

# Process each path sequentially
while IFS= read -r toml_path; do
    if [ -z "$toml_path" ]; then
        continue
    fi

    echo "[$CURRENT/$TOTAL_PATHS] build $toml_path"

    # Change to the directory
    cd "$toml_path" || {
        echo "[$CURRENT/$TOTAL_PATHS] Error: Could not cd to $toml_path"
        ((CURRENT++))
        continue
    }

    # Run alr -n build
    alr -n build

    # Return to original directory
    cd - >/dev/null
    ((CURRENT++))
done <<< "$ALIRE_PATHS"
