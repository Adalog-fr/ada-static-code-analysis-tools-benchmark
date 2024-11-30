#!/bin/bash

#######################################################################################################################
#                                WARNING: DO NOT RE-RUN THIS SCRIPT ON EXISTING FILES                                 #
# This script is designed for one-time generation of TypeScript files from .cyp queries.                              #
# It provides a quick start boilerplate for new rules.                                                                #
#                                                                                                                     #
# IMPORTANT:                                                                                                          #
# - This script should only be used to create new TypeScript rule files.                                              #
# - Rerunning this script on files with custom modifications or complex queries                                       #
#   will result in an overwrite of those changes, leading to data loss.                                               #
# - Ensure that you only run this script for adding new rules to prevent                                              #
#   unintended overwrites (not run with -a / --all).                                                                  #
#######################################################################################################################

# Parse arguments to either process a single file or all files
if [ "$#" -eq 0 ]; then
  echo "Usage: $0 [--all | -a] | [filename.cyp]"
  exit 1
fi

# Define the source and target directories
source_directory="../../../../bench-source/benchmark-rules"
target_directory=".."

# Function to generate TypeScript file from a .cyp filename
generate_ts_file() {
  local cyp_file="$1"
  # Get the basename without the extension
  local base_name=$(basename "$cyp_file" .cyp)

  # Convert base_name to snake_case
  local snake_case_file_name=$(echo "$base_name" | sed -r 's/([A-Z])/_\L\1/g' | sed 's/^_//')

  # Convert snake_case to PascalCase for class names
  local pascal_case_class_name=$(echo "$snake_case_file_name" | sed -r 's/(^|_)([a-z])/\U\2/g')

  # Convert snake_case to Pascal_Snake_Case for the rule name
  local pascal_snake_case_rule_name=$(echo "$pascal_case_class_name" | sed -r 's/([A-Z])/_\1/g' | sed 's/^_//')

  # Define the target TypeScript file path
  local ts_file_path="$target_directory/${snake_case_file_name}.ts"

  # Create the TypeScript file with the corresponding content
  cat <<EOF > "$ts_file_path"
import { join } from "jsr:@std/path@^0.225.1";
import { RuleType, responseRecords, ruleConstructorParamsExtended, type Query } from "./types/rules.ts";

export default class $pascal_case_class_name extends RuleType {
    static override readonly ruleName = '$pascal_snake_case_rule_name';
    query: string;

    constructor(cypherQueriesPath: string, timing: boolean, resultFile: Deno.FsFile) {
        super(cypherQueriesPath, timing, resultFile);
        this.query = Deno.readTextFileSync(join(cypherQueriesPath, "$base_name.cyp"));
    }

    static override initialize(params: ruleConstructorParamsExtended): $pascal_case_class_name {
        return new $pascal_case_class_name(params.cypherQueriesPath, params.timing, params.resultFile);
    }

    getQuery(): Query {
      return this.query;
    }

    saveResult(records: responseRecords, file: Deno.FsFile) {
        records.forEach(elt => {
            const props = elt.get("n").properties;
            file.writeSync(new TextEncoder().encode(\`\${props.filename}:\${props.line}:\${props.column}: \${$pascal_case_class_name.ruleName}\n\`));
        })
    }
}
EOF
}

# Check if the first argument is --all or -a to process all files
if [ "$1" = "--all" ] || [ "$1" = "-a" ]; then
  # Loop through all *.cyp files in the source directory
  for cyp_filename in "$source_directory"/*.cyp; do
    generate_ts_file "$cyp_filename"
  done
else
  # Process the single file passed as an argument
  cyp_filename="$source_directory/$1"
  if [ ! -f "$cyp_filename" ]; then
    echo "File not found: $cyp_filename"
    exit 1
  fi
  generate_ts_file "$cyp_filename"
fi
