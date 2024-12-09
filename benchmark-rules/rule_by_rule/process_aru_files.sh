#!/bin/bash

# Loop through all files with .aru extension in the parent directory
for file in ../*.aru; do
    # Extract just the filename without the path
    filename=$(basename "$file")

    # Create a temporary file to store the modified content
    temp_file=$(mktemp)

    # Add "set verbose on;" at the beginning of the file
    echo -e "set verbose on;\nset timing global;\nset statistics 3;\n" > "$temp_file"

    # Append the original file content after the new line
    cat "$file" >> "$temp_file"

    # Copy the modified content to the current directory with the same filename
    cp "$temp_file" "./$filename"

    # Remove the temporary file
    rm "$temp_file"
done
