#!/bin/bash

# Signal handler for graceful termination
function signalHandler() {
  echo -e "\n\nInterrupted! Exiting gracefully..."
  exit 1
}

# Set up signal handlers for Ctrl+C (SIGINT) and Ctrl+\ (SIGQUIT)
trap 'signalHandler' SIGINT
trap 'signalHandler' SIGQUIT

# Source common utility functions
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/utils/utils.sh"
source "$SCRIPT_DIR/utils/check-requirements.sh"

print_banner "Ada Benchmark Repository: Benchmark All"

# Define options
OPTIONS=$(getopt -o "h" --longoptions "help,neo4j-uri:,neo4j-user:,neo4j-password:,benchmark-only:,rule-by-rule-only:,generate-report-only:,output-dir:,log-level:,log-file:,skip-requirements-check,min-loc:,max-loc:,skip-adactl,skip-gnatcheck,skip-cogralys,bench-only,use-cache,project-list:,adactl-rule-file:,gnatcheck-rule-file:,suffix:" -- "$@")
eval set -- "$OPTIONS"

# Unified show_help function
function show_help() {
  echo "Usage: $0 [options]"
  echo ""
  echo "Options:"
  echo "  -h, --help                  Show this help message and exit"
  echo "  --neo4j-uri URI             Neo4j instance URI (e.g., bolt://localhost:7687)"
  echo "  --neo4j-user USER           Neo4j username"
  echo "  --neo4j-password PASSWORD   Neo4j password"
  echo "  --benchmark-only            Run only the benchmark"
  echo "  --rule-by-rule-only         Run only the rule-by-rule benchmark"
  echo "  --generate-report-only      Run only the report generation"
  echo "  --output-dir DIR            Output directory for results"
  echo "  --log-level LEVEL           Log level (e.g., DEBUG, INFO, WARNING, ERROR)"
  echo "  --log-file FILE             Log file"
  echo "  --skip-requirements-check   Skip checking prerequisites"
  echo "  --min-loc MIN_LOC           Minimum lines of code"
  echo "  --max-loc MAX_LOC           Maximum lines of code"
  echo "  --skip-adactl               Skip AdaControl"
  echo "  --skip-gnatcheck            Skip GNATcheck"
  echo "  --skip-cogralys             Skip Cogralys"
  echo "  --bench-only                Run only the benchmark"
  echo "  --use-cache                 Use cache"
  echo "  --project-list LIST         List of projects"
  echo "  --adactl-rule-file FILE     AdaControl rule file"
  echo "  --gnatcheck-rule-file FILE  GNATcheck rule file"
  echo "  --suffix SUFFIX             Suffix"
  exit 1
}

# Parse options
while true; do
  case "$1" in
    -h|--help) show_help; shift;;
    --neo4j-uri) NEO4J_URI="$2"; shift 2;;
    --neo4j-user) NEO4J_USER="$2"; shift 2;;
    --neo4j-password) NEO4J_PASSWORD="$2"; shift 2;;
    --benchmark-only) BENCHMARK_ONLY="true"; shift;;
    --rule-by-rule-only) RULE_BY_RULE_ONLY="true"; shift;;
    --generate-report-only) GENERATE_REPORT_ONLY="true"; shift;;
    --output-dir) OUTPUT_DIR="$2"; shift 2;;
    --log-level) LOG_LEVEL="$2"; shift 2;;
    --log-file) LOG_FILE="$2"; shift 2;;
    --skip-requirements-check) SKIP_REQUIREMENTS_CHECK="true"; shift;;
    --min-loc) MIN_LOC="$2"; shift 2;;
    --max-loc) MAX_LOC="$2"; shift 2;;
    --skip-adactl) SKIP_ADACL="$2"; shift 2;;
    --skip-gnatcheck) SKIP_GNATCHECK="$2"; shift 2;;
    --skip-cogralys) SKIP_COGRALYS="$2"; shift 2;;
    --bench-only) BENCH_ONLY="$2"; shift 2;;
    --use-cache) USE_CACHE="$2"; shift 2;;
    --project-list) PROJECT_LIST="$2"; shift 2;;
    --adactl-rule-file) ADACL_RULE_FILE="$2"; shift 2;;
    --gnatcheck-rule-file) GNATCHECK_RULE_FILE="$2"; shift 2;;
    --suffix) SUFFIX="$2"; shift 2;;
    --) shift; break;;
  esac
done

# Ensure true script defaults if not set by user
: "${maxIteration:=3}"
: "${NEO4J_HOST:=bolt://localhost:7687}"
: "${NEO4J_USER:=neo4j}"
: "${NEO4J_PASS:=auieauie}"
: "${MIN_LOC:=0}"
: "${MAX_LOC:=0}"
: "${BENCH_ONLY:=false}"
: "${USE_CACHE:=false}"
: "${SKIP_ADACL:=false}"
: "${SKIP_GNATCHECK:=false}"
: "${SKIP_COGRALYS:=false}"
: "${ADACL_RULE_FILE:=}"
: "${GNATCHECK_RULE_FILE:=}"
: "${PROJECT_LIST:=}"
: "${SUFFIX:=}"
: "${output_format:=md}"
: "${root_dir:=.}"

# Check prerequisites
if [ -z "$SKIP_REQUIREMENTS_CHECK" ]; then
  if ! check_requirements; then
    if [ $? -eq 0 ]; then
      print_success "All prerequisites are met "
    else
      print_warning "Some prerequisites are not met"
      if ! confirm "Do you want to continue despite detected issues?"; then
        print_error "Benchmark canceled"
        return 1
      fi
    fi
  fi
fi

# Function to check if Neo4j is running using Cypher shell
check_neo4j_connection() {
  print_info "Checking Neo4j connection at $NEO4J_HOST..."
  
  # Attempt to connect to Neo4j using cypher-shell
  if cypher-shell -a "$NEO4J_HOST" -u "$NEO4J_USER" -p "$NEO4J_PASS" "RETURN 1;" &>/dev/null; then
    print_success "Successfully connected to Neo4j database"
    
    # Check Neo4j version
    print_info "Checking Neo4j version..."
    neo4j_version=$(cypher-shell -a "$NEO4J_HOST" -u "$NEO4J_USER" -p "$NEO4J_PASS" "CALL dbms.components() YIELD name, versions, edition UNWIND versions as version RETURN version" | grep -v "version" | tr -d " " | tr -d "\r" | tr -d "\n")
    
    # Clean the version string by removing any quotes
    clean_version=$(echo "$neo4j_version" | tr -d '"')
    
    # Extract major version number
    major_version=$(echo "$clean_version" | cut -d'.' -f1)
    
    print_info "Detected Neo4j version: $clean_version"
    
    # Check if version is greater than 5.x.y - ensure it's a number
    if [[ "$major_version" =~ ^[0-9]+$ ]] && [ "$major_version" -ge 5 ]; then
      print_success "Neo4j version $clean_version is compatible (>= 5.x.y)"
      return 0
    else
      print_error "Neo4j version $clean_version is not compatible. Version 5.x.y or higher is required."
      if ! confirm "Do you want to continue anyway? (This may cause errors)"; then
        print_error "Benchmark canceled"
        exit 1
      fi
      return 1
    fi
  else
    print_error "Failed to connect to Neo4j database at $NEO4J_HOST"
    print_error "Please ensure Neo4j is running and credentials are correct"
    if ! confirm "Do you want to continue anyway? (This may cause errors)"; then
      print_error "Benchmark canceled"
      exit 1
    fi
    return 1
  fi
}

# Check Neo4j connection before running any benchmark scripts
if [[ -n "$BENCHMARK_ONLY" || -z "$GENERATE_REPORT_ONLY" ]]; then
  check_neo4j_connection
fi

# Call sub-scripts with relevant options
if [ -n "$BENCHMARK_ONLY" ]; then
  "$SCRIPT_DIR/benchmark.sh" --neo4j-uri "$NEO4J_URI" --neo4j-user "$NEO4J_USER" --neo4j-password "$NEO4J_PASSWORD" --output-dir "$OUTPUT_DIR" --log-level "$LOG_LEVEL" --log-file "$LOG_FILE" --min-loc "$MIN_LOC" --max-loc "$MAX_LOC" --skip-adactl "$SKIP_ADACL" --skip-gnatcheck "$SKIP_GNATCHECK" --skip-cogralys "$SKIP_COGRALYS" --bench-only "$BENCH_ONLY" --use-cache "$USE_CACHE" --project-list "$PROJECT_LIST" --adactl-rule-file "$ADACL_RULE_FILE" --gnatcheck-rule-file "$GNATCHECK_RULE_FILE" --suffix "$SUFFIX"
elif [ -n "$RULE_BY_RULE_ONLY" ]; then
  "$SCRIPT_DIR/benchmark-rule-by-rule.sh" --neo4j-uri "$NEO4J_URI" --neo4j-user "$NEO4J_USER" --neo4j-password "$NEO4J_PASSWORD" --output-dir "$OUTPUT_DIR" --log-level "$LOG_LEVEL" --log-file "$LOG_FILE" --min-loc "$MIN_LOC" --max-loc "$MAX_LOC" --skip-adactl "$SKIP_ADACL" --skip-gnatcheck "$SKIP_GNATCHECK" --skip-cogralys "$SKIP_COGRALYS" --bench-only "$BENCH_ONLY" --use-cache "$USE_CACHE" --project-list "$PROJECT_LIST" --adactl-rule-file "$ADACL_RULE_FILE" --gnatcheck-rule-file "$GNATCHECK_RULE_FILE" --suffix "$SUFFIX"
elif [ -n "$GENERATE_REPORT_ONLY" ]; then
  "$SCRIPT_DIR/generate-report.sh" --output-dir "$OUTPUT_DIR" --log-level "$LOG_LEVEL" --log-file "$LOG_FILE"
else
  "$SCRIPT_DIR/benchmark.sh" --neo4j-uri "$NEO4J_URI" --neo4j-user "$NEO4J_USER" --neo4j-password "$NEO4J_PASSWORD" --output-dir "$OUTPUT_DIR" --log-level "$LOG_LEVEL" --log-file "$LOG_FILE" --min-loc "$MIN_LOC" --max-loc "$MAX_LOC" --skip-adactl "$SKIP_ADACL" --skip-gnatcheck "$SKIP_GNATCHECK" --skip-cogralys "$SKIP_COGRALYS" --bench-only "$BENCH_ONLY" --use-cache "$USE_CACHE" --project-list "$PROJECT_LIST" --adactl-rule-file "$ADACL_RULE_FILE" --gnatcheck-rule-file "$GNATCHECK_RULE_FILE" --suffix "$SUFFIX"
  "$SCRIPT_DIR/benchmark-rule-by-rule.sh" --neo4j-uri "$NEO4J_URI" --neo4j-user "$NEO4J_USER" --neo4j-password "$NEO4J_PASSWORD" --output-dir "$OUTPUT_DIR" --log-level "$LOG_LEVEL" --log-file "$LOG_FILE" --min-loc "$MIN_LOC" --max-loc "$MAX_LOC" --skip-adactl "$SKIP_ADACL" --skip-gnatcheck "$SKIP_GNATCHECK" --skip-cogralys "$SKIP_COGRALYS" --bench-only "$BENCH_ONLY" --use-cache "$USE_CACHE" --project-list "$PROJECT_LIST" --adactl-rule-file "$ADACL_RULE_FILE" --gnatcheck-rule-file "$GNATCHECK_RULE_FILE" --suffix "$SUFFIX"
  "$SCRIPT_DIR/generate-report.sh" --output-dir "$OUTPUT_DIR" --log-level "$LOG_LEVEL" --log-file "$LOG_FILE"
fi

print_success "All benchmarks and report generation completed."