#!/bin/bash
# Fresh installation script - For installing on a fresh Debian system
# Created on $(date +"%d-%m-%Y")

set -e  # Exit on error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/install-utils.sh"

print_banner "Full Installation on Fresh Debian System"
check_root

# Welcome message
echo "This script will perform a complete installation on a fresh Debian system."
echo "It will install all dependencies, create users, and set up the development environment."
echo ""
read -p "Do you want to continue? (y/n, default: y): " proceed
proceed=${proceed:-y}

if [[ "$proceed" != "y" && "$proceed" != "Y" ]]; then
    print_warning "Installation aborted by user"
    exit 0
fi

# Run all component scripts in order
print_step "Running dependency installation"
bash "${SCRIPT_DIR}/install-dependencies.sh"

print_step "Creating users"
bash "${SCRIPT_DIR}/install-users.sh"

print_step "Installing Go and SCC"
bash "${SCRIPT_DIR}/install-scc.sh"

# Ask if user wants to install GNAT Pro
read -p "Do you want to install GNAT Pro? (y/n, default: n): " install_gnat
install_gnat=${install_gnat:-n}

if [[ "$install_gnat" == "y" || "$install_gnat" == "Y" ]]; then
    print_step "Installing GNAT Pro"
    bash "${SCRIPT_DIR}/install-gnatpro.sh"
else
    print_warning "Skipping GNAT Pro installation"
fi

print_step "Installing additional tools"
bash "${SCRIPT_DIR}/install-tools.sh"

# Final setup
print_step "Setting up environment"
setup_environment_vars

print_banner "Installation Complete!"
echo "The development environment has been successfully set up."
echo ""
echo "To activate the environment, log out and log back in,"
echo "or run: source /etc/profile.d/gnatpro.sh"
echo ""
echo "If you installed GNAT Pro, the PATH has been updated to include it."
echo ""
echo "Summary of installed components:"
echo "- System dependencies"
echo "- Users: devy"
echo "- Go and SCC development tools"
if [[ "$install_gnat" == "y" || "$install_gnat" == "Y" ]]; then
    echo "- GNAT Pro and Ada development libraries"
fi
echo "- Additional tools (sed 4.9, Oh My Posh)"
echo ""
echo "To verify the installation:"
echo "- Check Go: go version"
echo "- Check SCC: scc --version"
