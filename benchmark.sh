#!/bin/bash

# Currently, this script run 10 times the following configurations:
#  - AdaControl
#  - GNATcheck, monothread
#  - GNATcheck, 32 threads
#
# TODO: add cogralyis

maxIteration=10

# Function to run a benchmark script with the given parameters
function run_benchmark() {
    local script_name="$1"
    local xp_num="$2"
    local j_option="$3"
    local iteration="$4"
    local log_file="/workspaces/bench-source/${script_name}-${xp_num}-${j_option}.log"

    # Run the benchmark script with time and redirect output to the log file
    time ./${script_name} -xpNum ${xp_num} ${j_option} 2>> ${log_file}
}

# Function to clean up logs using a Deno script
function clean_logs() {
    deno run --allow-all utils/deleteBenchmarkLogs.ts
}

# Main loop to run benchmarks for Adactl_benchmark.sh
for i in {1..$maxIteration}; do
    echo "Running Adactl_benchmark.sh iteration $i/$maxIteration"
    run_benchmark "Adactl_benchmark.sh" "$i" "-j0" "$i"
    clean_logs
done

# Main loops to run benchmarks for GNATcheck_benchmark.sh with -j1 and -j32
for j_option in "-j1" "-j32"; do
    for i in {1..$maxIteration}; do
        echo "Running GNATcheck_benchmark.sh iteration $i/$maxIteration with $j_option"
        run_benchmark "GNATcheck_benchmark.sh" "$i" "$j_option" "$i"
        clean_logs
    done
done
