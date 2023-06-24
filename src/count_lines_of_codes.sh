#!/bin/bash

sum=0

while IFS= read -r file; do
  count=$(tokei -t=Ada --output json "$file" | jq '.Ada.code')
  if [[ $count =~ ^[0-9]+$ ]]; then
    sum=$((sum + count))
  fi
done < merged.units_by_path

echo "Total lines of code: $sum"
