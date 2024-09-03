#!/bin/bash

# By default, this script run 10 times the following configurations:
#  - AdaControl
#  - GNATcheck, monothread
#  - GNATcheck, 32 threads
#  - cogralys

maxIteration=10
PROJECT_ROOT=$PWD
NEO4J_HOST="bolt://localhost:7687"
NEO4J_USER="neo4j"
NEO4J_PASS="auieauie"

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
echo -e " ####################\n"

echo -e "# Compute GNATcheck overhead"

# Compute overhead of AdaControl
for i in $(seq 1 $maxIteration); do
    echo -e "\n## Running adactl_benchmark.sh iteration $i/$maxIteration (overhead computation)\n"
    ./adactl_benchmark.sh --xpNum "$i" -s "-overhead" --rule $PROJECT_ROOT/benchmark-rules/overheadComputation/compute_overhead.aru
done

echo -e "\n# Compute GNATcheck overhead"

# Loops to compute overhead of gnatcheck with -j1 and -j32
for j_option in "1" "32"; do
    for i in $(seq 1 $maxIteration); do
        echo -e "\n## Running gnatcheck_benchmark.sh iteration $i/$maxIteration with $j_option thread(s) (overhead computation)\n"
        ./gnatcheck_benchmark.sh --xpNum "$i" -j "$j_option" -s "-overhead" --rule "$PROJECT_ROOT/benchmark-rules/overheadComputation/compute_overhead.rules" --extra-args "--rules-dir=$PROJECT_ROOT/benchmark-rules/overheadComputation"
    done
done

# Note: Cogralys overhead is not computed here because it is already computed in its benchmark script.

# BENCHMARK #

echo -e "\n #############"
echo " # Benchmark #"
echo -e " #############\n"

echo -e "# Benchmark AdaControl"

# Main loop to run benchmarks for Adactl_benchmark.sh
for i in $(seq 1 $maxIteration); do
    echo -e "\n## Running adactl_benchmark.sh iteration $i/$maxIteration\n"
    ./adactl_benchmark.sh --xpNum "$i"
done

echo -e "\n# Benchmark GNATcheck"

# Main loops to run benchmarks for GNATcheck_benchmark.sh with -j1 and -j32
for j_option in "1" "32"; do
    for i in $(seq 1 $maxIteration); do
        echo -e "\n## Running gnatcheck_benchmark.sh iteration $i/$maxIteration with $j_option thread(s)\n"
        ./gnatcheck_benchmark.sh --xpNum "$i" -j "$j_option"
    done
done

echo -e "\n# Benchmark Cogralys"

# Main loop to run benchmarks for cogralys_benchmark.sh
for i in $(seq 1 $maxIteration); do
    echo -e "\n## Running cogralys_benchmark.sh iteration $i/$maxIteration \n"
    ./cogralys_benchmark.sh -xpNum "$i" --neo4jHost "$NEO4J_HOST" --username "$NEO4J_USER" --password "$NEO4J_PASS"
done
