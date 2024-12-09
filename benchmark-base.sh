#!/bin/bash

######################
# Common Functions   #
######################

# Function to handle signal (CTRL+C, CTRL+\)
function signalHandler() {
  echo "Signal received, safely end the script"
  exit 1
}

# Function to get current date and time
get_datetime() {
    date "+%Y-%m-%d %H:%M:%S"
}

# Save checkpoint for resuming
save_checkpoint() {
    echo "$current_step:$current_project_step" > "$checkpoint_file"
}

# Load checkpoint if exists
load_checkpoint() {
    if [ -f "$checkpoint_file" ]; then
        IFS=':' read -r current_step current_project_step < "$checkpoint_file"
    else
        current_step=1
        current_project_step=1
    fi
}

########################
# Standard Functions   #
########################

process_standard_project() {
    local project_info=$1
    local project_number=$2
    local total_projects=$3
    IFS='|' read -r crateName alireTomlPath gprPath command loc <<< "$project_info"

    local base_name=$(basename "$gprPath" .gpr)
    local log_prefix="${commandName}-$base_name-$xpNum-j$max_procs$logSuffix"

    echo "[$(get_datetime)] [$project_number/$total_projects] START" | tee -a "$globalLogFilePath"
    cd "$PROJECT_ROOT/$alireTomlPath"

    if [ $current_project_step -eq 1 ]; then
        run_standard_command "$gprPath" "$log_prefix" "$command"
        current_project_step=2
        save_checkpoint
    fi
    if [ $current_project_step -eq 2 ]; then
        compute_standard_size "$gprPath" "$log_prefix"
        current_project_step=3
        save_checkpoint
    fi
    if [ $current_project_step -eq 3 ]; then
        clean_files "$alireTomlPath"
        current_project_step=1
        save_checkpoint
    fi
}

run_standard_command() {
    local gprPath=$1
    local log_prefix=$2
    local command="$3"

    echo "[$(get_datetime)] [$gprPath] Start xp" | tee -a "$globalLogFilePath"
    /usr/bin/time -v -o "$log_prefix.time" alr exec -- $command 2>&1 | tee -a "$log_prefix.log" "$globalLogFilePath"
    jc --time -p -r < "$log_prefix.time" > "$log_prefix.time.json"
    echo "[$(get_datetime)] [$gprPath] End xp" | tee -a "$globalLogFilePath"
}

compute_standard_size() {
    local gprPath=$1
    local log_prefix=$2
    echo "[$(get_datetime)] [$gprPath] Start computing ADT size" | tee -a "$globalLogFilePath"
    total_size=$(du -ch *.adt 2>/dev/null | tail -n 1 | cut -f 1)
    echo "{ \"size\": \"$total_size\" }" > "$log_prefix.size-adt.json"
    echo "[$(get_datetime)] [$gprPath] End computing ADT size" | tee -a "$globalLogFilePath"
}

clean_files() {
    local alireTomlPath=$1
    echo "[$(get_datetime)] [$alireTomlPath] Start cleaning" | tee -a "$globalLogFilePath"
    rm -f *.ali *.adt
    echo "[$(get_datetime)] [$alireTomlPath] End cleaning" | tee -a "$globalLogFilePath"
}

parse_standard_arguments() {
    while [[ "$#" -gt 0 ]]; do
        case $1 in
            --xpNum) xpNum="$2"; shift 2 ;;
            --min-loc) MIN_LOC="$2"; shift 2 ;;
            --max-loc) MAX_LOC="$2"; shift 2 ;;
            -j) max_procs="$2"; shift 2 ;;
            --rule) ruleFile="$2"; shift 2 ;;
            -s|--suffix) logSuffix="$2"; shift 2 ;;
            --extra-args) extraArgs="$2"; shift 2 ;;
            -r|--resume)
                IFS=':' read -r resume_step resume_iteration <<< "$2"
                current_step=$resume_step
                current_project_step=$resume_iteration
                resume_requested=true
                shift 2 ;;
            *) echo "Unknown option: $1"; exit 1 ;;
        esac
    done
}

########################
# Cogralys Functions   #
########################

process_cogralys_project() {
    local project_info=$1
    local project_number=$2
    local total_projects=$3
    IFS='|' read -r crateName alireTomlPath gprPath cogralys_init_args loc <<< "$project_info"

    local base_name=$(basename "$gprPath" .gpr)
    local log_prefix="cogralys-$base_name-$xpNum"

    echo "[$(get_datetime)] [$project_number/$total_projects] START processing $crateName" | tee -a "$globalLogFilePath"
    cd "$PROJECT_ROOT/$alireTomlPath"

    # If using cache and not resume, start from step 5
    if [ "$resume_requested" = false ] && [ "$use_cache" = true ]; then
        current_project_step=5
    fi

    if [ $current_project_step -le 1 ]; then
        run_cogralys_init "$gprPath" "$log_prefix" "$cogralys_init_args"
        current_project_step=2
        save_checkpoint
    fi

    if [ $current_project_step -eq 2 ]; then
        clean_files "$alireTomlPath"
        current_project_step=3
        save_checkpoint
    fi

    if [ $current_project_step -eq 3 ]; then
        update_cratesDB "$crateName" "$alireTomlPath" "$gprPath" "$log_prefix"
        current_project_step=4
        save_checkpoint
    fi

    if [ $current_project_step -eq 4 ]; then
        convert_json_to_cypher "$alireTomlPath" "$gprPath"
        current_project_step=5
        save_checkpoint
    fi

    if [ $current_project_step -eq 5 ]; then
        populate_neo4j "$alireTomlPath" "$gprPath" "$log_prefix"
        current_project_step=6
        save_checkpoint
    fi

    if [ $current_project_step -eq 6 ]; then
        run_cogralys_cli "$log_prefix"
        current_project_step=7
        save_checkpoint
    fi

    if [ $current_project_step -eq 7 ]; then
        compute_cogralys_size "$gprPath" "$log_prefix" "$alireTomlPath" "$cogralys_init_args"
        current_project_step=8
        save_checkpoint
    fi

    if [ $current_project_step -eq 8 ]; then
        clean_db "$log_prefix"
        current_project_step=1
        save_checkpoint
    fi
}

run_cogralys_init() {
    local gprPath=$1
    local log_prefix=$2
    local init_args=$3
    echo "[$(get_datetime)] [$gprPath] Start init" | tee -a "$globalLogFilePath"
    /usr/bin/time -v -o "$log_prefix-init.time" deno run $DENO_RUN_ARGS "$PROJECT_ROOT/utils/executeCogralysWithWatchdog.ts" "$init_args" 2>&1 | tee -a "$log_prefix-init.log" "$globalLogFilePath" > /dev/null
    jc --time -p -r < "$log_prefix-init.time" > "$log_prefix-init.time.json"
    echo "[$(get_datetime)] [$gprPath] End init" | tee -a "$globalLogFilePath"
}

update_cratesDB() {
    local crateName=$1
    local alireTomlPath=$2
    local gprPath=$3
    local log_prefix=$4
    echo "[$(get_datetime)] [$gprPath] Start updating cratesDB" | tee -a "$globalLogFilePath"
    deno run $DENO_RUN_ARGS "$PROJECT_ROOT/utils/cogralys-bench-util.ts" update-cratesDB-neo4j-dir -c "$crateName" -w "$alireTomlPath" -g "$gprPath" 2>&1 | tee -a "$globalLogFilePath" > /dev/null
    echo "[$(get_datetime)] [$gprPath] End updating cratesDB" | tee -a "$globalLogFilePath"
}

convert_json_to_cypher() {
    local alireTomlPath=$1
    local gprPath=$2
    echo "[$(get_datetime)] [$gprPath] Start converting to Cypher" | tee -a "$globalLogFilePath"
    deno run $DENO_RUN_ARGS "$PROJECT_ROOT/utils/cogralys-bench-util.ts" convert-neo4j-json-to-cypher-file -w "$alireTomlPath" -g "$gprPath"
    echo "[$(get_datetime)] [$gprPath] End converting to Cypher" | tee -a "$globalLogFilePath"
}

populate_neo4j() {
    local alireTomlPath=$1
    local gprPath=$2
    local log_prefix=$3
    echo "[$(get_datetime)] [$gprPath] Start populate" | tee -a "$globalLogFilePath"
    /usr/bin/time -v -o "$log_prefix-populate.time" deno run $DENO_RUN_ARGS "$PROJECT_ROOT/utils/cogralys-bench-util.ts" populate-neo4j-single -h "$NEO4J_HOST" --username "$NEO4J_USER" --password "$NEO4J_PASS" -m "cypher" -w "$alireTomlPath" -g "$gprPath" 2>&1 | tee -a "$globalLogFilePath" > /dev/null
    jc --time -p -r < "$log_prefix-populate.time" > "$log_prefix-populate.time.json"
    echo "[$(get_datetime)] [$gprPath] End populate" | tee -a "$globalLogFilePath"
}

run_cogralys_cli() {
    local log_prefix=$1
    echo "[$(get_datetime)] [$gprPath] Start run" | tee -a "$globalLogFilePath"
    /usr/bin/time -v -o "$log_prefix-run.time" deno run $DENO_RUN_ARGS "$PROJECT_ROOT/utils/cogralys-cli/cogralys-cli.ts" -t -h "$NEO4J_HOST" --username "$NEO4J_USER" --password "$NEO4J_PASS" -o "$log_prefix-run.report" 2>&1 | tee -a "$globalLogFilePath" > /dev/null
    jc --time -p -r < "$log_prefix-run.time" > "$log_prefix-run.time.json"
    echo "[$(get_datetime)] [$gprPath] End run" | tee -a "$globalLogFilePath"
}

compute_cogralys_size() {
    local gprPath=$1
    local log_prefix=$2
    local alireTomlPath=$3
    local cogralys_init_args=$4
    echo "[$(get_datetime)] [$gprPath] Start computing size metrics" | tee -a "$globalLogFilePath"

    total_size_adt=$(du -ch *.adt 2>/dev/null | tail -n 1 | cut -f 1)
    echo "{ \"size\": \"$total_size_adt\" }" > "$log_prefix.size-adt.json"

    neo4jPath=$PROJECT_ROOT/$alireTomlPath/$(echo "$cogralys_init_args" | jq -r '.command[2].env.NEO4J_RESULT_DIR')
    total_size_cogralys=$(du -ch "$neo4jPath" 2>/dev/null | tail -n 1 | cut -f 1)
    echo "{ \"size\": \"$total_size_cogralys\" }" > "$log_prefix.size-cogralys.json"

    echo "[$(get_datetime)] [$gprPath] End computing size metrics" | tee -a "$globalLogFilePath"
}

clean_db() {
    local log_prefix=$1
    echo "[$(get_datetime)] [$gprPath] Start cleaning DB" | tee -a "$globalLogFilePath"
    deno run $DENO_RUN_ARGS "$PROJECT_ROOT/utils/cogralys-bench-util.ts" clean-neo4j -h "$NEO4J_HOST" --username "$NEO4J_USER" --password "$NEO4J_PASS"
    echo "[$(get_datetime)] [$gprPath] End cleaning DB" | tee -a "$globalLogFilePath"
}

parse_cogralys_arguments() {
    while [[ "$#" -gt 0 ]]; do
        case $1 in
            --xpNum) xpNum="$2"; shift 2 ;;
            -h|--help) show_help; exit 0 ;;
            -n|--neo4jHost) NEO4J_HOST="$2"; shift 2 ;;
            --username) NEO4J_USER="$2"; shift 2 ;;
            --password) NEO4J_PASS="$2"; shift 2 ;;
            --use-cache) use_cache=true; shift ;;
            --min-loc) MIN_LOC="$2"; shift 2 ;;
            --max-loc) MAX_LOC="$2"; shift 2 ;;
            -r|--resume)
                IFS=':' read -r resume_step resume_iteration <<< "$2"
                current_step=$resume_step
                current_project_step=$resume_iteration
                resume_requested=true
                shift 2 ;;
            *) echo "Unknown option: $1"; exit 1 ;;
        esac
    done
}

####################
# Main Execution   #
####################

# Initialize common variables
resume_requested=false
use_cache=false
MIN_LOC=0
MAX_LOC=0
xpNum=0
max_procs=0
PROJECT_ROOT=$PWD
NEO4J_HOST="bolt://localhost:7687"
NEO4J_USER="neo4j"
NEO4J_PASS="auieauie"
DENO_RUN_ARGS="--config "$PROJECT_ROOT/deno.jsonc" --allow-all --unsafely-ignore-certificate-errors --unstable-ffi"
logSuffix=""
extraArgs=""
ruleFile=""

# Get benchmark type from first argument
benchmark_type=$1
shift

# Parse arguments based on benchmark type
if [[ "$benchmark_type" == "cogralys" ]]; then
    parse_cogralys_arguments "$@"
else
    case $benchmark_type in
        adactl) ruleFile="$PROJECT_ROOT/benchmark-rules/all_rules_in_one_file/_all.aru" ;;
        gnatcheck) ruleFile="$PROJECT_ROOT/benchmark-rules/all_rules_in_one_file/gnatcheck.rules" ;;
        gnatmetrics) ruleFile="" ;;
        *) echo "Unknown benchmark type: $benchmark_type"; exit 1 ;;
    esac
    parse_standard_arguments "$@"
fi

# Source the projects array
source "./${benchmark_type}_projects.sh"

# Filter projects based on LoC criteria
filtered_projects=()
for project in "${projects[@]}"; do
    IFS='|' read -r crateName alireTomlPath gprPath command loc <<< "$project"
    if [ $MIN_LOC -eq 0 ] || [ $loc -ge $MIN_LOC ]; then
        if [ $MAX_LOC -eq 0 ] || [ $loc -le $MAX_LOC ]; then
            filtered_projects+=("$project")
        else
            echo "[$(get_datetime)] Skipping $crateName (LoC: $loc > maximum: $MAX_LOC)" | tee -a "$globalLogFilePath"
        fi
    else
        echo "[$(get_datetime)] Skipping $crateName (LoC: $loc < minimum: $MIN_LOC)" | tee -a "$globalLogFilePath"
    fi
done

# Setup global variables
globalLogFilePath="$PROJECT_ROOT/${benchmark_type}-run-all-$xpNum.log"
total_projects=${#filtered_projects[@]}
checkpoint_file="$PROJECT_ROOT/benchmark-${benchmark_type}-$xpNum.checkpoint"

# Load checkpoint if not resuming
if [ "$resume_requested" = false ] && [ -f "$checkpoint_file" ]; then
    load_checkpoint
    resume_requested=true
fi

# Initialize log file
if [ "$resume_requested" = false ]; then
    echo "" > "$globalLogFilePath"
else
    echo -e "\n## RESUME ##\n" | tee -a "$globalLogFilePath"
fi

current_step=$((current_step-1))

# Setup signal handlers
trap 'signalHandler' SIGINT SIGQUIT

# Process all projects
for i in "${!filtered_projects[@]}"; do
    echo "${filtered_projects[$i]}"
    exit 0
    if [ $i -lt $current_step ]; then
        continue
    fi
    project_number=$((i+1))
    current_step=$project_number
    # Set current_project_step to 1 if it's empty or not a number
    if ! [[ "$current_project_step" =~ ^[0-9]+$ ]] ; then
        current_project_step=1
    fi
    save_checkpoint
    if [[ "$benchmark_type" == "cogralys" ]]; then
        process_cogralys_project "${filtered_projects[$i]}" "$project_number" "$total_projects"
    else
        process_standard_project "${filtered_projects[$i]}" "$project_number" "$total_projects"
    fi
done

echo "All projects ($total_projects) processed."
rm "$checkpoint_file"
