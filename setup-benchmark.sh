#!/bin/bash
# Benchmark repository setup script
# Created on $(date +"%d-%m-%Y")

set -e  # Exit on error

# Signal handler for graceful termination
function signalHandler() {
  echo -e "\n\nInterrupted! Exiting gracefully..."
  exit 1
}

# Set up signal handlers for Ctrl+C (SIGINT) and Ctrl+\ (SIGQUIT)
trap 'signalHandler' SIGINT
trap 'signalHandler' SIGQUIT

# Color definitions for better readability
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to display information messages
info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

# Function to display success messages
success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

# Function to display warning messages
warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

# Function to display error messages
error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to ask for confirmation
confirm() {
    read -p "$1 (y/n): " choice
    case "$choice" in
        y|Y ) return 0 ;;
        * ) return 1 ;;
    esac
}

# Function to check if a command exists
check_command() {
    command -v "$1" >/dev/null 2>&1
}

# Function to check Python version
check_python_version() {
    local version=$(python3 --version 2>&1 | awk '{print $2}')
    local major=$(echo $version | cut -d. -f1)
    local minor=$(echo $version | cut -d. -f2)

    if [[ "$major" -ge 3 && "$minor" -ge 10 ]]; then
        success "Python version $version ✓"
        return 0
    else
        error "Python version $version is lower than 3.10 ✗"
        return 1
    fi
}

# Function to check sed version
check_sed_version() {
    if check_command sed; then
        local version=$(sed --version | head -n 1 | awk '{print $4}')
        if [[ $(echo -e "$version\n4.9" | sort -V | head -n1) != "4.9" ]]; then
            error "sed version $version is lower than 4.9 ✗"
            if confirm "Do you want to install sed 4.9 using the install-tools.sh script?"; then
                if [[ -f "install/install-tools.sh" ]]; then
                    (cd install && sudo ./install-tools.sh)
                else
                    error "The install/install-tools.sh script was not found"
                    return 1
                fi
            else
                warning "The benchmark might not work correctly without sed 4.9"
                return 1
            fi
        else
            success "sed version $version ✓"
            return 0
        fi
    else
        error "sed is not installed ✗"
        return 1
    fi
}

# Function to check deno version
check_deno_version() {
    if check_command deno; then
        local version=$(deno --version | head -n 1 | awk '{print $2}')
        local major=$(echo $version | cut -d. -f1)

        if [[ "$major" -lt 2 ]]; then
            success "deno version $version ✓"
            return 0
        else
            error "deno version $version is greater than or equal to 2.0.0, bugs may occur ✗"
            warning "It is recommended to use a version lower than 2.0.0"
            return 1
        fi
    else
        error "deno is not installed ✗"
        return 1
    fi
}

# Function to check and install alr if necessary
check_alr() {
    if check_command alr; then
        success "alr is installed ✓"
        return 0
    else
        error "alr is not installed ✗"
        if confirm "Do you want to install alr?"; then
            if check_command deno; then
                if [[ -f "install/install_ada_setup.ts" ]]; then
                    (cd install && deno run --allow-run --allow-write --allow-read --allow-env ./install_ada_setup.ts install -b -e .zip alire)
                    if check_command alr; then
                        success "alr has been successfully installed ✓"
                        return 0
                    else
                        error "alr installation failed ✗"
                        return 1
                    fi
                else
                    error "The install/install_ada_setup.ts script was not found"
                    return 1
                fi
            else
                error "deno is required to install alr"
                return 1
            fi
        else
            warning "alr is necessary to compile tools and projects"
            return 1
        fi
    fi
}

# Function to check adactl
check_adactl() {
    if [[ -x "analysis-tools/Adacontrol/bin/adactl" ]]; then
        success "adactl is present ✓"
        return 0
    else
        error "adactl is not present in analysis-tools/Adacontrol/bin/ ✗"
        if confirm "Do you want to compile it?"; then
            if check_command alr; then
                info "Compiling adactl..."
                (cd analysis-tools/Adacontrol && alr build)
                if [[ -x "analysis-tools/Adacontrol/bin/adactl" ]]; then
                    success "adactl has been successfully compiled ✓"
                    return 0
                else
                    error "adactl compilation failed ✗"
                    return 1
                fi
            else
                error "alr is required to compile adactl"
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
        success "atgdb is present ✓"
        return 0
    else
        error "atgdb is not present in analysis-tools/cogralys-engine/bin/ ✗"
        if confirm "Do you want to compile it?"; then
            if check_command alr; then
                info "Compiling atgdb..."
                (cd analysis-tools/cogralys-engine && alr build)
                if [[ -x "analysis-tools/cogralys-engine/bin/atgdb" ]]; then
                    success "atgdb has been successfully compiled ✓"
                    return 0
                else
                    error "atgdb compilation failed ✗"
                    return 1
                fi
            else
                error "alr is required to compile atgdb"
                return 1
            fi
        else
            return 1
        fi
    fi
}

# Function to install Python dependencies
install_python_deps() {
    info "Installing Python dependencies..."
    if [[ -d "utils/generateChart" ]]; then
        (cd utils/generateChart && python3 -m venv venv && source venv/bin/activate && pip install -r requirements.txt)
        success "Python dependencies installed ✓"
        return 0
    else
        error "The utils/generateChart directory was not found"
        return 1
    fi
}

# Function to configure xmlada
configure_xmlada() {
    info "Configuring xmlada..."
    if [[ -d "src/xmlada" ]]; then
        (cd src/xmlada && ./configure)
        success "xmlada configured ✓"
        return 0
    else
        error "The src/xmlada directory was not found"
        return 1
    fi
}

# Function to build libgpr2
build_libgpr2() {
    info "Building libgpr2..."
    if [[ -d "src/libgpr2" ]]; then
        (cd src/libgpr2 && alr -n build)
        success "libgpr2 built ✓"

        info "Building libgpr2-c..."
        if [[ -d "src/libgpr2/bindings/c" ]]; then
            (cd src/libgpr2/bindings/c && alr -n build)
            success "libgpr2-c built ✓"
            return 0
        else
            error "The src/libgpr2/bindings/c directory was not found"
            return 1
        fi
    else
        error "The src/libgpr2 directory was not found"
        return 1
    fi
}

# Function to configure matreshka
configure_matreshka() {
    info "Configuring matreshka..."
    if [[ -d "src/matreshka" ]]; then
        (cd src/matreshka && make config && ./configure)
        success "matreshka configured ✓"
        return 0
    else
        error "The src/matreshka directory was not found"
        return 1
    fi
}

# Function to configure zeromq_ada
configure_zeromq_ada() {
    info "Configuring zeromq_ada..."
    if [[ -d "src/zeromq_ada" ]]; then
        (cd src/zeromq_ada && ./configure)
        success "zeromq_ada configured ✓"
        return 0
    else
        error "The src/zeromq_ada directory was not found"
        return 1
    fi
}

# Function to generate .env files
generate_env() {
    info "Generating .env files..."
    if [[ -f "utils/generate_env.sh" ]]; then
        ./utils/generate_env.sh
        cp .env.example .env
        success ".env files generated ✓"
        return 0
    else
        error "The utils/generate_env.sh script was not found"
        return 1
    fi
}

# Function to build benchmark projects
build_projects() {
    info "Building projects for benchmark..."
    if [[ -f "utils/build_projects.sh" ]]; then
        ./utils/build_projects.sh
        success "Projects built ✓"
        return 0
    else
        error "The build_projects.sh script was not found"
        return 1
    fi
}

# Function to copy load-system into obj
copy_load_system() {
    info "Copying load-system into obj..."
    if [[ -f "utils/copy_load-system_into_obj.sh" ]]; then
        ./utils/copy_load-system_into_obj.sh
        success "load-system copied ✓"
        return 0
    else
        error "The copy_load-system_into_obj.sh script was not found"
        return 1
    fi
}

# Check required tools
check_requirements() {
    local requirements_met=0

    info "Checking required tools..."

    # Check SCC
    if check_command scc; then
        success "SCC is installed ✓"
    else
        error "SCC is not installed ✗"
        requirements_met=1
    fi

    # Check Python
    if check_command python3; then
        check_python_version || requirements_met=1
    else
        error "Python 3 is not installed ✗"
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
        success "gnatcheck is installed ✓"
    else
        error "gnatcheck is not installed ✗"
        requirements_met=1
    fi

    # Check gnatls
    if check_command gnatls; then
        success "gnatls is installed ✓"
    else
        error "gnatls is not installed ✗"
        requirements_met=1
    fi

    # Check cypher-shell
    if check_command cypher-shell; then
        success "cypher-shell is installed ✓"
    else
        error "cypher-shell is not installed ✗"
        requirements_met=1
    fi

    # Check jq
    if check_command jq; then
        success "jq is installed ✓"
    else
        error "jq is not installed ✗"
        requirements_met=1
    fi

    # Check jc
    if check_command jc; then
        success "jc is installed ✓"
    else
        error "jc is not installed ✗"
        requirements_met=1
    fi

    # Check parallel
    if check_command parallel; then
        success "parallel is installed ✓"
    else
        error "parallel is not installed ✗"
        requirements_met=1
    fi

    # Check fd/fdfind
    if check_command fd; then
        success "fd is installed ✓"
    elif check_command fdfind; then
        success "fdfind is installed ✓"
    else
        error "fd/fdfind is not installed ✗"
        if confirm "Do you want to install fd-find?"; then
            sudo apt-get install fd-find
            if check_command fdfind; then
                success "fd-find has been successfully installed ✓"
            else
                error "fd-find installation failed ✗"
                requirements_met=1
            fi
        else
            requirements_met=1
        fi
    fi

    return $requirements_met
}

# Main function
main() {
    echo "============================================="
    echo "  Ada Benchmark Repository Setup"
    echo "============================================="

    # Check prerequisites
    if ! check_requirements; then
        if [ $? -eq 0 ]; then
            success "All prerequisites are met ✓"
        else
            warning "Some prerequisites are not met"
            if ! confirm "Do you want to continue despite detected issues?"; then
                error "Setup canceled"
                return 1
            fi
        fi
    fi

    # Install Python dependencies
    install_python_deps || warning "Problem during Python dependencies installation"

    # Configure xmlada
    configure_xmlada || warning "Problem during xmlada configuration"

    # Build libgpr2
    build_libgpr2 || warning "Problem during libgpr2 build"

    # Configure matreshka
    configure_matreshka || warning "Problem during matreshka configuration"

    # Configure zeromq_ada
    configure_zeromq_ada || warning "Problem during zeromq_ada configuration"

    # Generate .env files
    generate_env || warning "Problem during .env files generation"

    # Build projects
    build_projects || warning "Problem during projects build"

    # Copy load-system
    copy_load_system || warning "Problem during load-system copy"

    success "Setup completed"
    echo "==================================================="
    echo "To run the benchmark later, execute: ./benchmark.sh"
    echo "Do not forget to start Neo4j DB with Neo4j Desktop!"
    echo "==================================================="
}

# Run the script
main
