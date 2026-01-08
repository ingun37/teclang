#!/bin/bash

# Loop through all png files in the current directory
for file in *.png; do
    # Check if the file follows the pattern "number-direction.png"
    if [[ $file =~ ^([0-9]+)-([a-z]+)\.png$ ]]; then
        num="${BASH_REMATCH[1]}"
        dir="${BASH_REMATCH[2]}"

        # Construct the new name: direction-aNumber.png
        new_name="${dir}-a${num}.png"

        # Rename the file
        mv "$file" "$new_name"
        echo "Renamed: $file -> $new_name"
    fi
done