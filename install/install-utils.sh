#!/bin/bash
# Utilities script - Contains shared functions and variables
# Created on $(date +"%d-%m-%Y")

# Global variables
USERNAME="devy"
USER_UID=1000
USER_GID=1000
GNAT_RELEASE="gnatpro.tar.gz"
GO_VERSION="1.24.2"
SCC_VERSION="v3.5.0"

# Source common utility functions
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../utils/utils.sh"

# Variables specific to this script
# (USERNAME, USER_UID, USER_GID, GNAT_RELEASE, GO_VERSION, SCC_VERSION)

    return 0
}

# Function to check if a user exists
user_exists() {
    id -u "$1" &>/dev/null
    return $?
}

# Function to check if a group exists
group_exists() {
    getent group "$1" &>/dev/null
    return $?
}

# Function to create environment variables setup
setup_environment_vars() {
    print_step "Setting up environment variables"
    cat > /etc/profile.d/gnatpro.sh << 'EOF'
export PATH="/usr/gnat/libexec/asis-gnsa/bin:/usr/alire/bin:/usr/gnat/bin:/usr/local/go/bin:${PATH}"
export GOPATH="$HOME/go"
export PATH="$PATH:$GOPATH/bin"
EOF

    chmod +x /etc/profile.d/gnatpro.sh
    print_success "Environment variables set up successfully"
}

# Function to create symbolic links
create_symlinks() {
    print_step "Creating symbolic links"
    ln -sf /usr/bin/python3 /usr/bin/python
    print_success "Symbolic links created"
}

# Function to verify if a command exists
command_exists() {
    command -v "$1" &> /dev/null
}

# Export functions for use in other scripts
export -f print_banner print_step print_success print_error print_warning check_root
export -f is_package_installed check_file_exists user_exists group_exists
export -f setup_environment_vars create_symlinks command_exists
