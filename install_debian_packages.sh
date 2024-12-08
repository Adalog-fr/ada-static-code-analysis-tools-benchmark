#!/bin/bash

# Read the package list and install each package
while read package; do
    sudo apt-get install -y "$package"
done < package-list.txt
