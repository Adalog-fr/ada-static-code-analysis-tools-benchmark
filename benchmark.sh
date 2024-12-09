#!/bin/bash

# By default, this script run 3 times the following configurations:
#  - AdaControl
#  - GNATcheck, monothread
#  - GNATcheck, 32 threads
#  - cogralys

maxIteration=3
PROJECT_ROOT=$PWD
NEO4J_HOST="bolt://localhost:7687"
NEO4J_USER="neo4j"
NEO4J_PASS="auieauie"
adactl_rule_file=""
gnatcheck_rule_file=""
skip_adactl=false
skip_gnatcheck=false
skip_cogralys=false
logSuffix=""

# Function to display help information
show_help() {
    echo "Usage: $0 [OPTIONS]"
    echo "Options:"
    echo "  --maxIteration <number>          How many times the script should run the benchmark (default: $maxIteration)"
    echo "  -n, --neo4jHost <URI>            Bolt URI of Neo4j database, with port (e.g.: $NEO4J_HOST)"
    echo "  --username <userName>            Username used to login to Neo4j database"
    echo "  --password <password>            Password used to login to Neo4j database"
    echo "  -r, --resume <step:iteration>    Resume from a specific step and iteration (e.g., 3:5)"
    echo "  --bench-only                     Skip overhead computation and run only benchmarks (default: false)"
    echo "  --use-cache                      Enable cache usage for Cogralys benchmarks (default: false)"
    echo "  --min-loc <number>               Minimum lines of code filter for projects (0 for no limit)"
    echo "  --max-loc <number>               Maximum lines of code filter for projects (0 for no limit)"
    echo "  --adactl-rule-file <path>        Path to AdaControl rule file for benchmarks"
    echo "  --gnatcheck-rule-file <path>     Path to GNATcheck rule file for benchmarks"
    echo "  --skip-adactl                    Skip AdaControl benchmarks (default: false)"
    echo "  --skip-gnatcheck                 Skip GNATcheck benchmarks (default: false)"
    echo "  --skip-cogralys                  Skip Cogralys benchmarks (default: false)"
    echo "  --project-list <path>            File containing list of project GPR paths to analyze"
    echo "  -s, --suffix <suffix>            Add a suffix to the benchmark log files"
    echo "  -h, --help                       Show help information"
}

# Save the current state of the script. Useful to resume the script later.
save_checkpoint() {
  echo "$current_step:$current_iteration" > "$checkpoint_file"
}

# Load the script to the latest state (if exists)
load_checkpoint() {
  if [ -f "$checkpoint_file" ]; then
      IFS=':' read -r current_step current_iteration < "$checkpoint_file"
  else
      current_step=0
      current_iteration=1
  fi
}

# Function to handle signal (CTRL+C, CTRL+\)
function signalHandler()
{
  echo "Signal recieved, safely end the script"
  exit 1
}

trap 'signalHandler' SIGINT
trap 'signalHandler' SIGQUIT

benchOnly=false
use_cache=false
MIN_LOC=0
MAX_LOC=0

# Parse command line arguments
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --maxIteration) maxIteration="$2"; shift 2 ;;
        -h|--help) show_help; exit 0 ;;
        -n|--neo4jHost) NEO4J_HOST="$2"; shift 2 ;;
        --username) NEO4J_USER="$2"; shift 2 ;;
        --password) NEO4J_PASS="$2"; shift 2 ;;
        --bench-only) benchOnly=true; shift ;;
        --use-cache) use_cache=true; shift ;;
        --min-loc) MIN_LOC="$2"; shift 2 ;;
        --max-loc) MAX_LOC="$2"; shift 2 ;;
        -r|--resume)
          IFS=':' read -r resume_step resume_iteration <<< "$2"
          current_step=$resume_step
          current_iteration=$resume_iteration
          shift 2 ;;
        --adactl-rule-file) adactl_rule_file="$2"; shift 2 ;;
        --gnatcheck-rule-file) gnatcheck_rule_file="$2"; shift 2 ;;
        --skip-adactl) skip_adactl=true; shift ;;
        --skip-gnatcheck) skip_gnatcheck=true; shift ;;
        --skip-cogralys) skip_cogralys=true; shift ;;
        --project-list) projectListFile="$2"; shift 2 ;;
        -s|--suffix) logSuffix="$2"; shift 2 ;;
        *) echo "Unknown option: $1"; show_help; exit 1 ;;
    esac
done

checkpoint_file="benchmark-$maxIteration.checkpoint"

# Load the current state if not set by resume option
if [ -z "$current_step" ] || [ -z "$current_iteration" ]; then
  load_checkpoint
  if ! [ -f "$checkpoint_file" ] && [ "$benchOnly" = true ]; then
    current_step=3
  fi
fi

# COMPUTE OVERHEAD #

if [[ $current_step -ge 0 && $current_step -le 2 ]]; then
  echo " ####################"
  echo " # Compute overhead #"
  echo -e " ####################\n"

  if [ $current_step -eq 0 ]; then
    if [ "$skip_adactl" = false ]; then
      echo -e "# Compute AdaControl overhead"

      # Compute overhead of AdaControl
      for i in $(seq $current_iteration $maxIteration); do
        current_iteration=$i
        save_checkpoint
        echo -e "\n## Running adactl iteration $i/$maxIteration (overhead computation)\n"
        ./benchmark-base.sh adactl --xpNum "$i" -s "-overhead" --rule $PROJECT_ROOT/benchmark-rules/overheadComputation/compute_overhead.aru --min-loc $MIN_LOC --max-loc $MAX_LOC ${logSuffix:+-s "$logSuffix"} ${projectListFile:+--project-list "$projectListFile"}
      done
    fi

    current_step=1
    current_iteration=1
    save_checkpoint
  fi

  if [ "$skip_gnatcheck" = false ]; then
    echo -e "\n# Compute GNATcheck overhead"

    nbCores=(1 32)

    # Loops to compute overhead of gnatcheck with -j1 and -j32
    for j_option in "${nbCores[@]}"; do
      if [ $current_step -eq 2 ] && [ $j_option -eq 1 ]; then
        continue
      fi
      for i in $(seq $current_iteration $maxIteration); do
        current_iteration=$i
        save_checkpoint
        echo -e "\n## Running gnatcheck iteration $i/$maxIteration with $j_option thread(s) (overhead computation)\n"
        ./benchmark-base.sh gnatcheck --xpNum "$i" -j "$j_option" -s "-overhead" --rule "$PROJECT_ROOT/benchmark-rules/overheadComputation/compute_overhead.rules" --extra-args "--rules-dir=$PROJECT_ROOT/benchmark-rules/overheadComputation" --min-loc $MIN_LOC --max-loc $MAX_LOC ${logSuffix:+-s "$logSuffix"} ${projectListFile:+--project-list "$projectListFile"}
      done
      current_step=$((current_step+1))
      current_iteration=1
      save_checkpoint
    done
  else
    # Skip to step 3 if GNATcheck is skipped
    current_step=3
    current_iteration=1
    save_checkpoint
  fi

  # Note: Cogralys overhead is not computed here because it is already computed in its benchmark script.
fi

# BENCHMARK #

echo -e "\n #############"
echo " # Benchmark #"
echo -e " #############\n"

if [ $current_step -eq 3 ]; then
  if [ "$skip_adactl" = false ]; then

    echo -e "# Benchmark AdaControl"

    # Main loop to run benchmarks for adactl
    for i in $(seq $current_iteration $maxIteration); do
      current_iteration=$i
      save_checkpoint
      echo -e "\n## Running adactl iteration $i/$maxIteration\n"
      ./benchmark-base.sh adactl --xpNum "$i" --min-loc $MIN_LOC --max-loc $MAX_LOC ${adactl_rule_file:+--rule "$adactl_rule_file"} ${logSuffix:+-s "$logSuffix"} ${projectListFile:+--project-list "$projectListFile"}
    done
  fi
  current_step=4
  current_iteration=1
  save_checkpoint
fi

if [[ $current_step -ge 4 && $current_step -le 5 ]]; then
  if [ "$skip_gnatcheck" = false ]; then

    echo -e "\n# Benchmark GNATcheck"

    nbCores=(1 32)

    # Main loops to run benchmarks for gnatcheck with -j1 and -j32
    for j_option in "${nbCores[@]}"; do
      if [ $current_step -eq 5 ] && [ $j_option -eq 1 ]; then
        continue
      fi
      for i in $(seq $current_iteration $maxIteration); do
        current_iteration=$i
        save_checkpoint
        echo -e "\n## Running gnatcheck iteration $i/$maxIteration with $j_option thread(s)\n"
        ./benchmark-base.sh gnatcheck --xpNum "$i" -j "$j_option" --min-loc $MIN_LOC --max-loc $MAX_LOC ${gnatcheck_rule_file:+--rule "$gnatcheck_rule_file"} ${logSuffix:+-s "$logSuffix"} ${projectListFile:+--project-list "$projectListFile"}
      done
      current_step=$((current_step+1))
      current_iteration=1
      save_checkpoint
    done
  else
    # Skip to step 6 if GNATcheck is skipped
    current_step=6
    current_iteration=1
    save_checkpoint
  fi

fi

if [ "$skip_cogralys" = false ]; then
  echo -e "\n# Benchmark Cogralys"

  # Main loop to run benchmarks for cogralys
  for i in $(seq $current_iteration $maxIteration); do
    current_iteration=$i
    save_checkpoint
    echo -e "\n## Running cogralys iteration $i/$maxIteration \n"
    ./benchmark-base.sh cogralys --xpNum "$i" --neo4jHost "$NEO4J_HOST" --username "$NEO4J_USER" --password "$NEO4J_PASS" --min-loc $MIN_LOC --max-loc $MAX_LOC ${use_cache:+'--use-cache'} ${logSuffix:+-s "$logSuffix"} ${projectListFile:+--project-list "$projectListFile"}
  done

fi

rm "$checkpoint_file"
