#!/bin/bash

set -e  # Exit on error

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

# Function to check Python version
check_python_version() {
    local version=$(python3 --version 2>&1 | awk '{print $2}')
    local major=$(echo $version | cut -d. -f1)
    local minor=$(echo $version | cut -d. -f2)

    if [[ "$major" -ge 3 && "$minor" -ge 10 ]]; then
        print_success "$(format_text $FORMAT_BOLD $COLOR_CYAN)Python$COLOR_RESET version $version ✓"
        return 0
    else
        print_error "$(format_text $FORMAT_BOLD $COLOR_CYAN)Python$COLOR_RESET version $version is lower than 3.10 ✗"
        return 1
    fi
}

# Function to check sed version
check_sed_version() {
    if check_command sed; then
        local version=$(sed --version | head -n 1 | awk '{print $4}')
        if [[ $(echo -e "$version\n4.9" | sort -V | head -n1) != "4.9" ]]; then
            print_error "$(format_text $FORMAT_BOLD $COLOR_CYAN)sed$COLOR_RESET version $version is lower than 4.9 ✗"
            if confirm "Do you want to install sed 4.9 using the install-tools.sh script?"; then
                if [[ -f "install/install-tools.sh" ]]; then
                    (cd install && sudo ./install-tools.sh)
                else
                    print_error "The install/install-tools.sh script was not found"
                    return 1
                fi
            else
                print_warning "The benchmark might not work correctly without sed 4.9"
                return 1
            fi
        else
            print_success "$(format_text $FORMAT_BOLD $COLOR_CYAN)sed$COLOR_RESET version $version ✓"
            return 0
        fi
    else
        print_error "$(format_text $FORMAT_BOLD $COLOR_CYAN)sed$COLOR_RESET is not installed ✗"
        return 1
    fi
}

# Function to check deno version
check_deno_version() {
    if check_command deno; then
        local version=$(deno --version | head -n 1 | awk '{print $2}')
        local major=$(echo $version | cut -d. -f1)

        if [[ "$major" -lt 2 ]]; then
            print_success "$(format_text $FORMAT_BOLD $COLOR_CYAN)deno$COLOR_RESET version $version ✓"
            return 0
        else
            print_error "$(format_text $FORMAT_BOLD $COLOR_CYAN)deno$COLOR_RESET version $version is greater than or equal to 2.0.0, bugs may occur ✗"
            print_warning "It is recommended to use a version lower than 2.0.0"
            return 1
        fi
    else
        print_error "$(format_text $FORMAT_BOLD $COLOR_CYAN)deno$COLOR_RESET is not installed ✗"
        return 1
    fi
}

# Function to check and install alr if necessary
check_alr() {
    if check_command alr; then
        print_success "$(format_text $FORMAT_BOLD $COLOR_CYAN)alr$COLOR_RESET is installed ✓"
        return 0
    else
        print_error "$(format_text $FORMAT_BOLD $COLOR_CYAN)alr$COLOR_RESET is not installed ✗"
        if confirm "Do you want to install alr?"; then
            if check_command deno; then
                if [[ -f "install/install_ada_setup.ts" ]]; then
                    (cd install && deno run --allow-run --allow-write --allow-read --allow-env ./install_ada_setup.ts install -b -e .zip alire)
                    if check_command alr; then
                        print_success "$(format_text $FORMAT_BOLD $COLOR_CYAN)alr$COLOR_RESET has been successfully installed ✓"
                        return 0
                    else
                        print_error "$(format_text $FORMAT_BOLD $COLOR_CYAN)alr$COLOR_RESET installation failed ✗"
                        return 1
                    fi
                else
                    print_error "The install/install_ada_setup.ts script was not found"
                    return 1
                fi
            else
                print_error "deno is required to install alr"
                return 1
            fi
        else
            print_warning "alr is necessary to compile tools and projects"
            return 1
        fi
    fi
}

# Function to check adactl
check_adactl() {
    if [[ -x "analysis-tools/Adacontrol/bin/adactl" ]]; then
        print_success "$(format_text $FORMAT_BOLD $COLOR_CYAN)adactl$COLOR_RESET is present ✓"
        return 0
    else
        print_error "$(format_text $FORMAT_BOLD $COLOR_CYAN)adactl$COLOR_RESET is not present in analysis-tools/Adacontrol/bin/ ✗"
        if confirm "Do you want to compile it?"; then
            if check_command alr; then
                print_info "Compiling adactl..."
                (cd analysis-tools/Adacontrol && alr build)
                if [[ -x "analysis-tools/Adacontrol/bin/adactl" ]]; then
                    print_success "$(format_text $FORMAT_BOLD $COLOR_CYAN)adactl$COLOR_RESET has been successfully compiled ✓"
                    return 0
                else
                    print_error "$(format_text $FORMAT_BOLD $COLOR_CYAN)adactl$COLOR_RESET compilation failed ✗"
                    return 1
                fi
            else
                print_error "alr is required to compile adactl"
                return 1
            fi
        else
            return 1
        fi
    fi
}

# Function to check atgdb
check_atgdb() {
    if [[ -x "analysis-tools/cogralys-engine/bin/atgdb" ]]; then
        print_success "$(format_text $FORMAT_BOLD $COLOR_CYAN)atgdb$COLOR_RESET is present ✓"
        return 0
    else
        print_error "$(format_text $FORMAT_BOLD $COLOR_CYAN)atgdb$COLOR_RESET is not present in analysis-tools/cogralys-engine/bin/ ✗"
        if confirm "Do you want to compile it?"; then
            if check_command alr; then
                print_info "Compiling atgdb..."
                (cd analysis-tools/cogralys-engine && alr build)
                if [[ -x "analysis-tools/cogralys-engine/bin/atgdb" ]]; then
                    print_success "$(format_text $FORMAT_BOLD $COLOR_CYAN)atgdb$COLOR_RESET has been successfully compiled ✓"
                    return 0
                else
                    print_error "$(format_text $FORMAT_BOLD $COLOR_CYAN)atgdb$COLOR_RESET compilation failed ✗"
                    return 1
                fi
            else
                print_error "alr is required to compile atgdb"
                return 1
            fi
        else
            return 1
        fi
    fi
}

# Check required tools
check_requirements() {
    local requirements_met=0

    print_info "Checking required tools..."

    # Check SCC
    if check_command scc; then
        print_success "$(format_text $FORMAT_BOLD $COLOR_CYAN)SCC$COLOR_RESET is installed ✓"
    else
        print_error "$(format_text $FORMAT_BOLD $COLOR_CYAN)SCC$COLOR_RESET is not installed ✗"
        requirements_met=1
    fi

    # Check typst
    if check_command typst; then
        print_success "$(format_text $FORMAT_BOLD $COLOR_CYAN)typst$COLOR_RESET is installed ✓"
    else
        print_warning "$(format_text $FORMAT_BOLD $COLOR_CYAN)typst$COLOR_RESET is not installed, it is required to generate PDF reports"
        requirements_met=1
    fi

    # Check Python
    if check_command python3; then
        check_python_version || requirements_met=1
    else
        print_error "$(format_text $FORMAT_BOLD $COLOR_CYAN)Python$COLOR_RESET 3 is not installed ✗"
        requirements_met=1
    fi

    # Check sed
    check_sed_version || requirements_met=1

    # Check deno
    check_deno_version || requirements_met=1

    # Check alr
    check_alr || requirements_met=1

    # Check adactl
    check_adactl || requirements_met=1

    # Check atgdb
    check_atgdb || requirements_met=1

    # Check gnatcheck
    if check_command gnatcheck; then
        print_success "$(format_text $FORMAT_BOLD $COLOR_CYAN)gnatcheck$COLOR_RESET is installed ✓"
    else
        print_error "$(format_text $FORMAT_BOLD $COLOR_CYAN)gnatcheck$COLOR_RESET is not installed ✗"
        requirements_met=1
    fi

    # Check gnatls
    if check_command gnatls; then
        print_success "$(format_text $FORMAT_BOLD $COLOR_CYAN)gnatls$COLOR_RESET is installed ✓"
    else
        print_error "$(format_text $FORMAT_BOLD $COLOR_CYAN)gnatls$COLOR_RESET is not installed ✗"
        requirements_met=1
    fi

    # Check cypher-shell
    if check_command cypher-shell; then
        print_success "$(format_text $FORMAT_BOLD $COLOR_CYAN)cypher-shell$COLOR_RESET is installed ✓"
    else
        print_error "$(format_text $FORMAT_BOLD $COLOR_CYAN)cypher-shell$COLOR_RESET is not installed ✗"
        requirements_met=1
    fi

    # Check jq
    if check_command jq; then
        print_success "$(format_text $FORMAT_BOLD $COLOR_CYAN)jq$COLOR_RESET is installed ✓"
    else
        print_error "$(format_text $FORMAT_BOLD $COLOR_CYAN)jq$COLOR_RESET is not installed ✗"
        requirements_met=1
    fi

    # Check jc
    if check_command jc; then
        print_success "$(format_text $FORMAT_BOLD $COLOR_CYAN)jc$COLOR_RESET is installed ✓"
    else
        print_error "$(format_text $FORMAT_BOLD $COLOR_CYAN)jc$COLOR_RESET is not installed ✗"
        requirements_met=1
    fi

    # Check parallel
    if check_command parallel; then
        print_success "$(format_text $FORMAT_BOLD $COLOR_CYAN)parallel$COLOR_RESET is installed ✓"
    else
        print_error "$(format_text $FORMAT_BOLD $COLOR_CYAN)parallel$COLOR_RESET is not installed ✗"
        requirements_met=1
    fi

    # Check fd/fdfind
    if check_command fd; then
        print_success "$(format_text $FORMAT_BOLD $COLOR_CYAN)fd$COLOR_RESET is installed ✓"
    elif check_command fdfind; then
        print_success "$(format_text $FORMAT_BOLD $COLOR_CYAN)fdfind$COLOR_RESET is installed ✓"
    else
        print_error "$(format_text $FORMAT_BOLD $COLOR_CYAN)fd/fdfind$COLOR_RESET is not installed ✗"
        if confirm "Do you want to install fd-find?"; then
            sudo apt-get install fd-find
            if check_command fdfind; then
                print_success "$(format_text $FORMAT_BOLD $COLOR_CYAN)fd-find$COLOR_RESET has been successfully installed ✓"
            else
                print_error "$(format_text $FORMAT_BOLD $COLOR_CYAN)fd-find$COLOR_RESET installation failed ✗"
                requirements_met=1
            fi
        else
            requirements_met=1
        fi
    fi

    return $requirements_met
}

export -f check_python_version check_sed_version check_deno_version check_alr check_adactl check_atgdb check_requirements