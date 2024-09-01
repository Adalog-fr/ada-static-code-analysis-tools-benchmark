#!/bin/bash

# By default, this script run 10 times the following configurations:
#  - AdaControl
#  - GNATcheck, monothread
#  - GNATcheck, 32 threads
#  - cogralys

maxIteration=10
PROJECT_ROOT=$PWD

# Function to display help information
show_help() {
    echo "Usage: $0 [--maxIteration <number>] [-h|--help]"
    echo "  --maxIteration <number>   How many times the script should run the benchmark."
    echo "  -n, --neo4jHost <URI>     Bolt URI of Neo4j database, with port (e.g.: bolt://domain.com:7687)."
    echo "  --username <userName>     Username used to login to Neo4j database."
    echo "  --password <password>     Password used to login to Neo4j database."
    echo "  -h, --help                Show help information."
}

# Parse command line arguments
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --maxIteration) maxIteration="$2"; shift 2 ;;
        -h|--help) show_help; exit 0 ;;
        -n|--neo4jHost) NEO4J_HOST="$2"; shift 2 ;;
        --username) NEO4J_USER="$2"; shift 2 ;;
        --password) NEO4J_PASS="$2"; shift 2 ;;
        *) echo "Unknown option: $1"; show_help; exit 1 ;;
    esac
done

# COMPUTE OVERHEAD #

echo " ####################"
echo " # Compute overhead #"
echo " ####################"

echo -e "\n# Compute GNATcheck overhead\n"

exit 0

# Compute overhead of AdaControl
for i in $(seq 1 $maxIteration); do
    echo "Running Adactl_benchmark.sh iteration $i/$maxIteration (overhead computation)"
    ./Adactl_benchmark.sh --xpNum "$i" -s "-overhead" --rule $PROJECT_ROOT/benchmark-rules/overheadComputation/compute_overhead.aru
done

echo -e "\n# Compute GNATcheck overhead\n"

# Loops to compute overhead of gnatcheck with -j1 and -j32
for j_option in "1" "32"; do
    for i in $(seq 1 $maxIteration); do
        echo "Running GNATcheck_benchmark.sh iteration $i/$maxIteration with $j_option thread(s) (overhead computation)"
        ./GNATcheck_benchmark.sh --xpNum "$i" -j "$j_option" -s "-overhead" --rule "$PROJECT_ROOT/benchmark-rules/overheadComputation/compute_overhead.rules" --extra-args "--rules-dir=$PROJECT_ROOT/benchmark-rules/overheadComputation"
    done
    echo ""
done

# Note: Cogralys overhead is not computed here because it is already computed in its benchmark script.

# BENCHMARK #

echo -e "\n #############"
echo " # Benchmark #"
echo " #############\n"

# Main loop to run benchmarks for Adactl_benchmark.sh
for i in $(seq 1 $maxIteration); do
    echo "Running Adactl_benchmark.sh iteration $i/$maxIteration"
    ./Adactl_benchmark.sh --xpNum "$i"
done

echo ""

# Main loops to run benchmarks for GNATcheck_benchmark.sh with -j1 and -j32
for j_option in "1" "32"; do
    for i in $(seq 1 $maxIteration); do
        echo "Running GNATcheck_benchmark.sh iteration $i/$maxIteration with $j_option thread(s)"
        ./GNATcheck_benchmark.sh --xpNum "$i" -j "$j_option"
    done
    echo ""
done

echo ""

# Main loop to run benchmarks for cogralys_benchmark.sh
for i in $(seq 1 $maxIteration); do
    echo "Running cogralys_benchmark.sh iteration $i/$maxIteration"
    ./cogralys_benchmark.sh -xpNum "$i" --neo4jHost "$neo4jHost" --username $username --password $password
done
