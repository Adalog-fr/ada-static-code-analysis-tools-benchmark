#!/bin/bash

# Default values for script parameters
PROJECT_ROOT=$PWD
NEO4J_HOST="bolt://localhost:7687"
NEO4J_USER="neo4j"
NEO4J_PASS="auieauie"
maxIteration=3
benchOnly=false
use_cache=false
MIN_LOC=0
MAX_LOC=0
skip_adactl=false
skip_gnatcheck=false
skip_cogralys=true

# Function to display help information (same as benchmark.sh)
show_help() {
    echo "Usage: $0 [OPTIONS]"
    echo "Options:"
    echo "  --maxIteration <number>          How many times the script should run the benchmark (default: 3)"
    echo "  -n, --neo4jHost <URI>            Bolt URI of Neo4j database, with port (e.g.: bolt://domain.com:7687)"
    echo "  --username <userName>            Username used to login to Neo4j database"
    echo "  --password <password>            Password used to login to Neo4j database"
    echo "  -r, --resume <step:iteration>    Resume from a specific step and iteration (e.g., 3:5)"
    echo "  --bench-only                     Skip overhead computation and run only benchmarks (default: false)"
    echo "  --use-cache                      Enable cache usage for Cogralys benchmarks (default: false)"
    echo "  --min-loc <number>               Minimum lines of code filter for projects (0 for no limit)"
    echo "  --max-loc <number>               Maximum lines of code filter for projects (0 for no limit)"
    echo "  --skip-adactl                    Skip AdaControl benchmarks (default: false)"
    echo "  --skip-gnatcheck                 Skip GNATcheck benchmarks (default: false)"
    echo "  --skip-cogralys                  Skip Cogralys benchmarks (default: true, because Cogralys already process rule by rule)"
    echo "  -h, --help                       Show help information"
}

# Parse command line arguments
args=""
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --maxIteration) maxIteration="$2"; args="$args $1 $2"; shift 2 ;;
        -h|--help) show_help; exit 0 ;;
        -n|--neo4jHost) NEO4J_HOST="$2"; args="$args $1 $2"; shift 2 ;;
        --username) NEO4J_USER="$2"; args="$args $1 $2"; shift 2 ;;
        --password) NEO4J_PASS="$2"; args="$args $1 $2"; shift 2 ;;
        --bench-only) benchOnly=true; args="$args $1"; shift ;;
        --use-cache) use_cache=true; args="$args $1"; shift ;;
        --min-loc) MIN_LOC="$2"; args="$args $1 $2"; shift 2 ;;
        --max-loc) MAX_LOC="$2"; args="$args $1 $2"; shift 2 ;;
        --skip-adactl) skip_adactl=true; shift ;;
        --skip-gnatcheck) skip_gnatcheck=true; shift ;;
        --skip-cogralys) skip_cogralys=true; shift ;;
        -r|--resume) args="$args $1 $2"; shift 2 ;;
        *) echo "Unknown option: $1"; show_help; exit 1 ;;
    esac
done

# Iterate over each rule file in the rule_by_rule directory
for rule_file in "$PROJECT_ROOT"/benchmark-rules/rule_by_rule/*.rules; do
    # Get the base name without extension
    base_name=$(basename "$rule_file" .rules)
    # Construct the corresponding AdaCtl rule file path
    adactl_file="$PROJECT_ROOT/benchmark-rules/rule_by_rule/${base_name}.aru"

    # Call benchmark.sh with the appropriate rule files
    ./benchmark.sh $args \
        --adactl-rule-file "$adactl_file" \
        --gnatcheck-rule-file "$rule_file" \
        -s $base_name \
        ${skip_adactl:+'--skip-adactl'} ${skip_gnatcheck:+'--skip-gnatcheck'} ${skip_cogralys:+'--skip-cogralys'}
done
