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

# Function to show help message
show_help() {
    cat << EOF
Usage: $0 <benchmark_type> [options]

Benchmark Types:
    cogralys     Run Cogralys benchmarks
    adactl       Run AdaControl benchmarks
    gnatcheck    Run GNATcheck benchmarks
    gnatmetrics  Run GNATmetric

Common Options:
    --xpNum NUMBER           Experiment number
    --min-loc NUMBER         Minimum lines of code filter
    --max-loc NUMBER         Maximum lines of code filter
    --project-list FILE      File containing list of project GPR paths to analyze
    -s, --suffix STRING      Log file suffix
    -r, --resume STEP:ITER   Resume from specific step and iteration

Standard Benchmark Options (adactl, gnatcheck, gnatmetrics):
    -j NUMBER                Number of parallel processes
    --rule FILE              Rules file path
    --extra-args STRING      Additional arguments

Cogralys Specific Options:
    -n, --neo4jHost HOST     Neo4j host address
    --username USER          Neo4j username
    --password PASS          Neo4j password
    --use-cache              Use cache for Cogralys

Example:
    $0 cogralys --xpNum 1 --min-loc 1000 --project-list projects.txt
    $0 adactl --xpNum 1 -j 4 --rule rules.aru
EOF
}

# Function to parse all arguments
parse_arguments() {
    # First argument is benchmark type
    benchmark_type=$1
    shift

    # Show help if requested
    if [[ "$1" == "-h" ]] || [[ "$1" == "--help" ]] || [[ -z "$benchmark_type" ]]; then
        show_help
        exit 0
    fi

    # Initialize default values
    xpNum=0
    MIN_LOC=0
    MAX_LOC=0
    projectListFile=""
    resume_requested=false
    current_step=1
    current_project_step=1
    logSuffix=""

    # Standard benchmark defaults
    max_procs=0
    ruleFile=""
    extraArgs=""

    # Cogralys defaults
    NEO4J_HOST="bolt://localhost:7687"
    NEO4J_USER="neo4j"
    NEO4J_PASS="auieauie"
    use_cache=false

    # Parse all arguments
    while [[ "$#" -gt 0 ]]; do
        case $1 in
            # Common arguments
            --xpNum) xpNum="$2"; shift 2 ;;
            --min-loc) MIN_LOC="$2"; shift 2 ;;
            --max-loc) MAX_LOC="$2"; shift 2 ;;
            --project-list) projectListFile="$2"; shift 2 ;;
            -s|--suffix) logSuffix="$2"; shift 2 ;;
            -r|--resume)
                IFS=':' read -r current_step current_project_step <<< "$2"
                resume_requested=true
                shift 2 ;;

            # Standard benchmark arguments
            -j)
                if [[ "$benchmark_type" != "cogralys" ]]; then
                    max_procs="$2"; shift 2
                else
                    echo "Unknown option: $1"; exit 1
                fi ;;
            --rule)
                if [[ "$benchmark_type" != "cogralys" ]]; then
                    ruleFile="$2"; shift 2
                else
                    echo "Unknown option: $1"; exit 1
                fi ;;
            --extra-args)
                if [[ "$benchmark_type" != "cogralys" ]]; then
                    extraArgs="$2"; shift 2
                else
                    echo "Unknown option: $1"; exit 1
                fi ;;

            # Cogralys specific arguments
            -n|--neo4jHost)
                if [[ "$benchmark_type" == "cogralys" ]]; then
                    NEO4J_HOST="$2"; shift 2
                else
                    echo "Unknown option: $1"; exit 1
                fi ;;
            --username)
                if [[ "$benchmark_type" == "cogralys" ]]; then
                    NEO4J_USER="$2"; shift 2
                else
                    echo "Unknown option: $1"; exit 1
                fi ;;
            --password)
                if [[ "$benchmark_type" == "cogralys" ]]; then
                    NEO4J_PASS="$2"; shift 2
                else
                    echo "Unknown option: $1"; exit 1
                fi ;;
            --use-cache)
                if [[ "$benchmark_type" == "cogralys" ]]; then
                    use_cache=true; shift
                else
                    echo "Unknown option: $1"; exit 1
                fi ;;

            # Unknown option
            *) echo "Unknown option: $1"; exit 1 ;;
        esac
    done
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
    local log_prefix="$benchmark_type-$base_name-$xpNum-j$max_procs${logSuffix:+"-$logSuffix"}"

    echo "[$(get_datetime)] [$project_number/$total_projects] START" | tee -a "$globalLogFilePath"
    cd "$PROJECT_ROOT/$alireTomlPath"

    if [ $current_project_step -eq 1 ]; then
        run_standard_command "$gprPath" "$log_prefix" "${command//%log_prefix%/$log_prefix}"
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
    # /usr/bin/time -v -o "$log_prefix.time" alr exec -- $command 2>&1 | tee -a "$log_prefix.log" "$globalLogFilePath"
    /usr/bin/time -v -o "$log_prefix.time" deno run $DENO_RUN_ARGS "$PROJECT_ROOT/utils/executeProgramWithWatchdog.ts" -f "$log_prefix.log" --end-of-file-check "execution time" -c "$command" 2>&1 | tee -a "$log_prefix.log" "$globalLogFilePath" > /dev/null
    if [ "$command" == "adactl" ]; then
      # Call the Deno script with the time file and store its exit status
      deno run $DENO_RUN_ARGS "$PROJECT_ROOT/utils/checkErrorInLog.ts" "$log_prefix.time"
      exit_status=$?
      # Process based on exit status
      if [ $exit_status -eq 0 ]; then
          # Convert time file to JSON if no errors were found
          jc --time -p -r < "$log_prefix.time" > "$log_prefix.time.json"
      fi
    else
      jc --time -p -r < "$log_prefix.time" > "$log_prefix.time.json"
    fi
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
        current_project_step=4
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
    /usr/bin/time -v -o "$log_prefix-init.time" deno run $DENO_RUN_ARGS "$PROJECT_ROOT/utils/executeProgramWithWatchdog.ts" -f "./log-atgdb.log" --end-of-file-check "[Upload_Manager] End" -c "$init_args" 2>&1 | tee -a "$log_prefix-init.log" "$globalLogFilePath" > /dev/null
    # jc --time -p -r < "$log_prefix-init.time" > "$log_prefix-init.time.json"
    # Call the Deno script with the time file and store its exit status
    deno run $DENO_RUN_ARGS "$PROJECT_ROOT/utils/checkErrorInLog.ts" "$log_prefix-init.time"
    exit_status=$?
    # Process based on exit status
    if [ $exit_status -eq 0 ]; then
        # Convert time file to JSON if no errors were found
        jc --time -p -r < "$log_prefix-init.time" > "$log_prefix-init.time.json"
    fi
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
    local dir_name=$(dirname "$gprPath")
    local base_name=$(basename "$gprPath" .gpr)
    local unitList="$PROJECT_ROOT/$dir_name/$base_name.units"
    echo "[$(get_datetime)] [$gprPath] Start run" | tee -a "$globalLogFilePath"
    /usr/bin/time -v -o "$log_prefix-run.time" deno run $DENO_RUN_ARGS "$PROJECT_ROOT/utils/cogralys-cli/cogralys-cli.ts" -t -h "$NEO4J_HOST" --username "$NEO4J_USER" --password "$NEO4J_PASS" -o "$log_prefix-run.report" -u "$unitList" 2>&1 | tee -a "$log_prefix.log" "$globalLogFilePath" > /dev/null
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

####################
# Main Execution   #
####################

# Initialize common variables
PROJECT_ROOT=$PWD
DENO_RUN_ARGS="--config "$PROJECT_ROOT/deno.jsonc" --allow-all --unsafely-ignore-certificate-errors --unstable-ffi"

# Get benchmark type and parse arguments
parse_arguments "$@"

globalLogFilePath="$PROJECT_ROOT/${benchmark_type}-run-all-$xpNum${logSuffix:+"-$logSuffix"}.log"

# Set default rule file based on benchmark type
if [[ "$benchmark_type" != "cogralys" ]] && [[ -z "$ruleFile" ]]; then
    case $benchmark_type in
        adactl) ruleFile="$PROJECT_ROOT/benchmark-rules/all_rules_in_one_file/_all.aru" ;;
        gnatcheck) ruleFile="$PROJECT_ROOT/benchmark-rules/all_rules_in_one_file/gnatcheck.rules" ;;
        gnatmetrics) ruleFile="" ;;
        *) echo "Unknown benchmark type: $benchmark_type"; exit 1 ;;
    esac
fi

# Source the projects array
source "./${benchmark_type}_projects.sh"

# Load project list if provided
selected_projects=()
if [ -n "$projectListFile" ] && [ -f "$projectListFile" ]; then
    while IFS= read -r gpr_path || [ -n "$gpr_path" ]; do
        # Trim whitespace
        gpr_path=$(echo "$gpr_path" | tr -d '[:space:]')
        [ -z "$gpr_path" ] && continue  # Skip empty lines
        # Find matching project from projects array
        for project in "${projects[@]}"; do
            IFS='|' read -r pCrateName pAlireTomlPath pGprPath pCommand pLoc <<< "$project"
            if [ "$pGprPath" = "$gpr_path" ]; then
                selected_projects+=("$project")
                break
            fi
        done
    done < "$projectListFile"
fi

# If no project list provided, use all projects
if [ ${#selected_projects[@]} -eq 0 ]; then
    selected_projects=("${projects[@]}")
fi

# Filter projects based on LoC criteria
filtered_projects=()
for project in "${selected_projects[@]}"; do
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
