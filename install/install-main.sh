#!/bin/bash
# Main installation script - acts as entry point for all installation options
# Created on $(date +"%d-%m-%Y")

set -e  # Exit on error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/install-utils.sh"

print_banner "Ada Development Environment Installation"

# Check if user has root privileges
check_root

# Display menu of installation options
echo "Please select installation option:"
echo "1. Full installation on fresh Debian (includes user creation)"
echo "2. Install dependencies only (for existing users)"
echo "3. Install GNAT Pro and libraries only"
echo "4. Install everything except GNAT Pro"
echo "5. Custom installation"
echo "6. Exit"

read -p "Enter option (1-6): " option

case $option in
    1)
        bash "${SCRIPT_DIR}/install-fresh.sh"
        ;;
    2)
        bash "${SCRIPT_DIR}/install-dependencies.sh"
        ;;
    3)
        bash "${SCRIPT_DIR}/install-gnatpro.sh"
        ;;
    4)
        bash "${SCRIPT_DIR}/install-dependencies.sh"
        bash "${SCRIPT_DIR}/install-scc.sh"
        bash "${SCRIPT_DIR}/install-tools.sh"
        ;;
    5)
        echo "Select components to install:"
        echo "a. System dependencies"
        echo "b. Create users"
        echo "c. Install Go and SCC"
        echo "d. Install GNAT Pro"
        echo "e. Install tools (sed, Oh My Posh, etc.)"

        read -p "Enter components separated by space (e.g., 'a c e'): " components

        [[ $components == *"a"* ]] && bash "${SCRIPT_DIR}/install-dependencies.sh"
        [[ $components == *"b"* ]] && bash "${SCRIPT_DIR}/install-users.sh"
        [[ $components == *"c"* ]] && bash "${SCRIPT_DIR}/install-scc.sh"
        [[ $components == *"d"* ]] && bash "${SCRIPT_DIR}/install-gnatpro.sh"
        [[ $components == *"e"* ]] && bash "${SCRIPT_DIR}/install-tools.sh"
        ;;
    6)
        echo "Exiting installation."
        exit 0
        ;;
    *)
        echo "Invalid option. Exiting."
        exit 1
        ;;
esac

print_success "Installation process completed."
