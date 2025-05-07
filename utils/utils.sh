#!/bin/bash
# utils.sh - Shared utility functions

# Base color codes
COLOR_RED='\033[31m'
COLOR_GREEN='\033[32m'
COLOR_YELLOW='\033[33m'
COLOR_BLUE='\033[34m'
COLOR_PURPLE='\033[35m'
COLOR_CYAN='\033[36m'
COLOR_LIGHTGRAY='\033[37m'
COLOR_WHITE='\033[97m'
COLOR_RESET='\033[0m'

# Text formatting
FORMAT_BOLD='\033[1m'
FORMAT_DIM='\033[2m'
FORMAT_UNDERLINE='\033[4m'
FORMAT_BLINK='\033[5m'
FORMAT_REVERSE='\033[7m'
FORMAT_HIDDEN='\033[8m'

# Function to combine formatting and colors
format_text() {
    local format="$1"
    local color="$2"
    echo "${format}${color}"
}

# Predefined formatted colors
COLOR_INFO=$(format_text "${FORMAT_BOLD}" "${COLOR_BLUE}")
COLOR_SUCCESS=$(format_text "${FORMAT_BOLD}" "${COLOR_GREEN}")
COLOR_WARNING=$(format_text "${FORMAT_BOLD}" "${COLOR_YELLOW}")
COLOR_ERROR=$(format_text "${FORMAT_BOLD}" "${COLOR_RED}")
COLOR_BANNER=$(format_text "${FORMAT_BOLD}" "${COLOR_BLUE}")
COLOR_STEP=$(format_text "${FORMAT_BOLD}" "${COLOR_CYAN}")

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

# Function to join paths safely
join_paths() {
    local IFS="/"
    echo "$*"
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

# Function to determine which find command to use
get_find_command() {
    if command_exists fd; then
        echo "fd"
    elif command_exists fdfind; then
        echo "fdfind"
    else
        echo "Error: Neither 'fd' nor 'fdfind' command found. Please install fd-find." >&2
        exit 1
    fi
}

# Export functions for use in other scripts
export -f print_info print_success print_warning print_error print_banner print_step print_tick print_cross print_warn_icon join_paths join_paths
export -f command_exists check_file_exists check_root is_package_installed user_exists group_exists check_command confirm format_text get_find_command

# Export color and formatting variables
export COLOR_RED COLOR_GREEN COLOR_YELLOW COLOR_BLUE COLOR_PURPLE COLOR_CYAN COLOR_LIGHTGRAY COLOR_WHITE COLOR_RESET
export FORMAT_BOLD FORMAT_DIM FORMAT_UNDERLINE FORMAT_BLINK FORMAT_REVERSE FORMAT_HIDDEN
export COLOR_INFO COLOR_SUCCESS COLOR_WARNING COLOR_ERROR COLOR_BANNER COLOR_STEP
