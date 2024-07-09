#!/bin/bash

# Define prefixes
PREFIXES=("gnatcheck" "adactl")

# Delete corresponding files recursively
for PREFIX in "${PREFIXES[@]}"; do
    find . -type f -name "${PREFIX}-all-*.log" -exec rm -f {} +
    find . -type f -name "${PREFIX}-*.log" -exec rm -f {} +
    find . -type f -name "${PREFIX}-*.report" -exec rm -f {} +
done
