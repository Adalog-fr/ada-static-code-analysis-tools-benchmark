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

# Function to display colorful step banners
print_banner() {
    echo -e "\e[1;34m==================================================================\e[0m"
    echo -e "\e[1;34m >> $1\e[0m"
    echo -e "\e[1;34m==================================================================\e[0m"
}

# Function to display steps
print_step() {
    echo -e "\e[1;36m--------------------------------------------------------------\e[0m"
    echo -e "\e[1;36m >>> $1\e[0m"
    echo -e "\e[1;36m--------------------------------------------------------------\e[0m"
}

# Function to display success message
print_success() {
    echo -e "\e[1;32m✓ $1\e[0m"
}

# Function to display error message
print_error() {
    echo -e "\e[1;31m✗ $1\e[0m"
}

# Function to display warning message
print_warning() {
    echo -e "\e[1;33m⚠ $1\e[0m"
}

# Function to check if user is root
check_root() {
    if [ "$(id -u)" -ne 0 ]; then
        print_error "This script must be run as root or with sudo"
        exit 1
    fi
}

# Function to check if a package is installed
is_package_installed() {
    dpkg -l "$1" &> /dev/null
    return $?
}

# Function to check if a file exists
check_file_exists() {
    if [ ! -f "$1" ]; then
        print_error "File $1 not found!"
        return 1
    fi
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
