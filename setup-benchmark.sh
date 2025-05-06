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

# Source common utility functions
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/utils/utils.sh"
source "$SCRIPT_DIR/utils/check-requirements.sh"

# Function to install Python dependencies
install_python_deps() {
    print_info "Installing Python dependencies..."
    if [[ -d "utils/generateChart" ]]; then
        (cd utils/generateChart && python3 -m venv venv && source venv/bin/activate && pip install -r requirements.txt)
        print_success "Python dependencies installed ✓"
        return 0
    else
        print_error "The utils/generateChart directory was not found"
        return 1
    fi
}

# Function to configure xmlada
configure_xmlada() {
    print_info "Configuring xmlada..."
    if [[ -d "src/xmlada" ]]; then
        (cd src/xmlada && ./configure)
        print_success "xmlada configured ✓"
        return 0
    else
        print_error "The src/xmlada directory was not found"
        return 1
    fi
}

# Function to build libgpr2
build_libgpr2() {
    print_info "Building libgpr2..."
    if [[ -d "src/libgpr2" ]]; then
        (cd src/libgpr2 && alr -n build)
        print_success "libgpr2 built ✓"

        print_info "Building libgpr2-c..."
        if [[ -d "src/libgpr2/bindings/c" ]]; then
            (cd src/libgpr2/bindings/c && alr -n build)
            print_success "libgpr2-c built ✓"
            return 0
        else
            print_error "The src/libgpr2/bindings/c directory was not found"
            return 1
        fi
    else
        print_error "The src/libgpr2 directory was not found"
        return 1
    fi
}

# Function to configure matreshka
configure_matreshka() {
    print_info "Configuring matreshka..."
    if [[ -d "src/matreshka" ]]; then
        (cd src/matreshka && make config && ./configure)
        print_success "matreshka configured ✓"
        return 0
    else
        print_error "The src/matreshka directory was not found"
        return 1
    fi
}

# Function to configure zeromq_ada
configure_zeromq_ada() {
    print_info "Configuring zeromq_ada..."
    if [[ -d "src/zeromq_ada" ]]; then
        (cd src/zeromq_ada && ./configure)
        print_success "zeromq_ada configured ✓"
        return 0
    else
        print_error "The src/zeromq_ada directory was not found"
        return 1
    fi
}

# Function to generate .env files
generate_env() {
    print_info "Generating .env files..."
    if [[ -f "utils/generate_env.sh" ]]; then
        ./utils/generate_env.sh
        cp .env.example .env
        print_success ".env files generated ✓"
        return 0
    else
        print_error "The utils/generate_env.sh script was not found"
        return 1
    fi
}

# Function to build benchmark projects
build_projects() {
    print_info "Building projects for benchmark..."
    if [[ -f "utils/build_projects.sh" ]]; then
        ./utils/build_projects.sh
        print_success "Projects built ✓"
        return 0
    else
        print_error "The build_projects.sh script was not found"
        return 1
    fi
}

# Function to copy load-system into obj
copy_load_system() {
    print_info "Copying load-system into obj..."
    if [[ -f "utils/copy_load-system_into_obj.sh" ]]; then
        ./utils/copy_load-system_into_obj.sh
        print_success "load-system copied ✓"
        return 0
    else
        print_error "The copy_load-system_into_obj.sh script was not found"
        return 1
    fi
}

# Main function
main() {
    print_banner "Ada Benchmark Repository Setup"

    # Check prerequisites
    if ! check_requirements; then
        if [ $? -eq 0 ]; then
            print_success "All prerequisites are met ✓"
        else
            print_warning "Some prerequisites are not met"
            if ! confirm "Do you want to continue despite detected issues?"; then
                print_error "Setup canceled"
                return 1
            fi
        fi
    fi

    # Install Python dependencies
    install_python_deps || print_warning "Problem during Python dependencies installation"

    # Configure xmlada
    configure_xmlada || print_warning "Problem during xmlada configuration"

    # Build libgpr2
    build_libgpr2 || print_warning "Problem during libgpr2 build"

    # Configure matreshka
    configure_matreshka || print_warning "Problem during matreshka configuration"

    # Configure zeromq_ada
    configure_zeromq_ada || print_warning "Problem during zeromq_ada configuration"

    # Generate .env files
    generate_env || print_warning "Problem during .env files generation"

    # Build projects
    build_projects || print_warning "Problem during projects build"

    # Copy load-system
    copy_load_system || print_warning "Problem during load-system copy"

    print_success "Setup completed"
    echo "==================================================="
    echo "To run the benchmark later, execute: ./benchmark.sh"
    echo "Do not forget to start Neo4j DB with Neo4j Desktop!"
    echo "==================================================="
}

# Run the script
main
