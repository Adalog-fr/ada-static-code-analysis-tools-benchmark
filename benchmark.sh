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
    echo "  --maxIteration <number>          How many times the script should run the benchmark."
    echo "  -n, --neo4jHost <URI>            Bolt URI of Neo4j database, with port (e.g.: bolt://domain.com:7687)."
    echo "  --username <userName>            Username used to login to Neo4j database."
    echo "  --password <password>            Password used to login to Neo4j database."
    echo "  -r, --resume <step:iteration>    Resume from a specific step and iteration (e.g., 3:5)."
    echo "  -h, --help                       Show help information."
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

# Parse command line arguments
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --maxIteration) maxIteration="$2"; shift 2 ;;
        -h|--help) show_help; exit 0 ;;
        -n|--neo4jHost) NEO4J_HOST="$2"; shift 2 ;;
        --username) NEO4J_USER="$2"; shift 2 ;;
        --password) NEO4J_PASS="$2"; shift 2 ;;
        -r|--resume)
          IFS=':' read -r resume_step resume_iteration <<< "$2"
          current_step=$resume_step
          current_iteration=$resume_iteration
          shift 2 ;;
        *) echo "Unknown option: $1"; show_help; exit 1 ;;
    esac
done

checkpoint_file="benchmark-$maxIteration.checkpoint"

# Load the current state if not set by resume option
if [ -z "$current_step" ] || [ -z "$current_iteration" ]; then
    load_checkpoint
fi

# COMPUTE OVERHEAD #

if [[ $current_step -ge 0 && $current_step -le 2 ]]; then
  echo " ####################"
  echo " # Compute overhead #"
  echo -e " ####################\n"

  if [ $current_step -eq 0 ]; then
    echo -e "# Compute GNATcheck overhead"

    # Compute overhead of AdaControl
    for i in $(seq $current_iteration $maxIteration); do
      current_iteration=$i
      save_checkpoint
      echo -e "\n## Running adactl_benchmark.sh iteration $i/$maxIteration (overhead computation)\n"
      ./adactl_benchmark.sh --xpNum "$i" -s "-overhead" --rule $PROJECT_ROOT/benchmark-rules/overheadComputation/compute_overhead.aru
    done

    current_step=1
    current_iteration=1
    save_checkpoint
  fi

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
      echo -e "\n## Running gnatcheck_benchmark.sh iteration $i/$maxIteration with $j_option thread(s) (overhead computation)\n"
      ./gnatcheck_benchmark.sh --xpNum "$i" -j "$j_option" -s "-overhead" --rule "$PROJECT_ROOT/benchmark-rules/overheadComputation/compute_overhead.rules" --extra-args "--rules-dir=$PROJECT_ROOT/benchmark-rules/overheadComputation"
    done
    current_step=$((current_step+1))
    current_iteration=1
    save_checkpoint
  done

  # Note: Cogralys overhead is not computed here because it is already computed in its benchmark script.
fi

# BENCHMARK #

echo -e "\n #############"
echo " # Benchmark #"
echo -e " #############\n"

if [ $current_step -eq 3 ]; then

  echo -e "# Benchmark AdaControl"

  # Main loop to run benchmarks for Adactl_benchmark.sh
  for i in $(seq $current_iteration $maxIteration); do
    current_iteration=$i
    save_checkpoint
    echo -e "\n## Running adactl_benchmark.sh iteration $i/$maxIteration\n"
    ./adactl_benchmark.sh --xpNum "$i"
  done

  current_step=4
  current_iteration=1
  save_checkpoint
fi

if [[ $current_step -ge 4 && $current_step -le 5 ]]; then

  echo -e "\n# Benchmark GNATcheck"

  nbCores=(1 32)

  # Main loops to run benchmarks for GNATcheck_benchmark.sh with -j1 and -j32
  for j_option in "${nbCores[@]}"; do
    if [ $current_step -eq 5 ] && [ $j_option -eq 1 ]; then
      continue
    fi
    for i in $(seq $current_iteration $maxIteration); do
      current_iteration=$i
      save_checkpoint
      echo -e "\n## Running gnatcheck_benchmark.sh iteration $i/$maxIteration with $j_option thread(s)\n"
      ./gnatcheck_benchmark.sh --xpNum "$i" -j "$j_option"
    done
    current_step=$((current_step+1))
    current_iteration=1
    save_checkpoint
  done

fi

echo -e "\n# Benchmark Cogralys"

# Main loop to run benchmarks for cogralys_benchmark.sh
for i in $(seq $current_iteration $maxIteration); do
  current_iteration=$i
  save_checkpoint
  echo -e "\n## Running cogralys_benchmark.sh iteration $i/$maxIteration \n"
  ./cogralys_benchmark.sh -xpNum "$i" --neo4jHost "$NEO4J_HOST" --username "$NEO4J_USER" --password "$NEO4J_PASS"
done

rm "$checkpoint_file"
