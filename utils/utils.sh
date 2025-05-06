#!/bin/bash
# utils.sh - Shared utility functions

# Color variables
COLOR_INFO='\033[1;34m'
COLOR_SUCCESS='\033[1;32m'
COLOR_WARNING='\033[1;33m'
COLOR_ERROR='\033[1;31m'
COLOR_BANNER='\033[1;34m'
COLOR_STEP='\033[1;36m'
COLOR_RESET='\033[0m'

# Print functions
print_info() {
    echo -e "${COLOR_INFO}[INFO]${COLOR_RESET} $1"
}
print_success() {
    echo -e "${COLOR_SUCCESS}[SUCCESS]${COLOR_RESET} $1"
}
print_warning() {
    echo -e "${COLOR_WARNING}[WARNING]${COLOR_RESET} $1"
}
print_error() {
    echo -e "${COLOR_ERROR}[ERROR]${COLOR_RESET} $1"
}
print_banner() {
    echo -e "${COLOR_BANNER}==================================================================${COLOR_RESET}"
    echo -e "${COLOR_BANNER} >> $1${COLOR_RESET}"
    echo -e "${COLOR_BANNER}==================================================================${COLOR_RESET}"
}
print_step() {
    echo -e "${COLOR_STEP}--------------------------------------------------------------${COLOR_RESET}"
    echo -e "${COLOR_STEP} >>> $1${COLOR_RESET}"
    echo -e "${COLOR_STEP}--------------------------------------------------------------${COLOR_RESET}"
}
print_tick() {
    echo -e "${COLOR_SUCCESS}✓ $1${COLOR_RESET}"
}
print_cross() {
    echo -e "${COLOR_ERROR}✗ $1${COLOR_RESET}"
}
print_warn_icon() {
    echo -e "${COLOR_WARNING}⚠ $1${COLOR_RESET}"
}

# Check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Check if a file exists
check_file_exists() {
    if [ ! -f "$1" ]; then
        error "File $1 not found!"
        return 1
    fi
    return 0
}

# Check if the script is run as root
check_root() {
    if [ "$(id -u)" -ne 0 ]; then
        print_error "This script must be run as root or with sudo"
        exit 1
    fi
}

# Check if a package is installed (Debian/Ubuntu)
is_package_installed() {
    dpkg -l "$1" &> /dev/null
    return $?
}

# Check if a user exists
user_exists() {
    id "$1" &>/dev/null
}

# Check if a group exists
group_exists() {
    getent group "$1" &>/dev/null
}

# Function to check if a command exists
check_command() {
    command -v "$1" >/dev/null 2>&1
}

# Function to ask for confirmation
confirm() {
    read -p "$1 (y/n): " choice
    case "$choice" in
        y|Y ) return 0 ;;
        * ) return 1 ;;
    esac
}

# Export functions for use in other scripts
export -f print_info print_success print_warning print_error print_banner print_step print_tick print_cross print_warn_icon
export -f command_exists check_file_exists check_root is_package_installed user_exists group_exists check_command confirm
