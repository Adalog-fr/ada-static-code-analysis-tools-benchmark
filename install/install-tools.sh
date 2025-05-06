#!/bin/bash
# Additional tools installation script
# Created on $(date +"%d-%m-%Y")

set -e  # Exit on error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/install-utils.sh"

print_banner "Installing Additional Tools"
check_root

# Check if current sed version needs upgrading
check_sed_version() {
    print_step "Checking sed version"

    # Get current sed version
    if command_exists sed; then
        local current_sed_version=$(sed --version | head -n 1 | awk '{print $4}')

        # Compare with minimum required version (4.9)
        if [[ $(echo -e "$current_sed_version\n4.9" | sort -V | head -n1) != "4.9" ]]; then
            print_warning "Your current sed version ($current_sed_version) is older than 4.9"
            print_error "Version 4.9 or newer is REQUIRED for benchmark scripts to work correctly with the -i option"
            print_warning "It is HIGHLY RECOMMENDED to install sed 4.9 to avoid issues with benchmark scripts"
            return 1
        else
            print_success "Your sed version ($current_sed_version) is 4.9 or newer, which is compatible with benchmark scripts"
            return 0
        fi
    else
        print_error "sed command not found"
        return 1
    fi
}

# Install sed 4.9
install_sed_49() {
    print_step "Installing sed version 4.9"

    # Check current sed version
    local current_sed_version=$(sed --version | head -n 1 | awk '{print $4}')
    if [[ "$current_sed_version" == "4.9" ]]; then
        print_warning "sed 4.9 is already installed, skipping"
        return
    fi

    cd /tmp
    wget ftp://ftp.gnu.org/gnu/sed/sed-4.9.tar.xz
    tar xf sed-4.9.tar.xz
    rm sed-4.9.tar.xz
    cd sed-4.9
    ./configure
    make
    make install
    cd ..
    rm -rf sed-4.9
    print_success "sed 4.9 installed successfully"
}

# Install Oh My Posh
install_oh_my_posh() {
    local target_user="$1"

    if ! user_exists "$target_user"; then
        print_warning "User $target_user does not exist, skipping Oh My Posh installation"
        return
    fi

    print_step "Installing Oh My Posh for $target_user"

    # Check if Oh My Posh is already installed
    if su - "$target_user" -c "command -v oh-my-posh &>/dev/null"; then
        print_warning "Oh My Posh already installed for $target_user, skipping"
        return
    fi

    su - "$target_user" -c "curl -s https://ohmyposh.dev/install.sh | bash -s"
    print_success "Oh My Posh installed for $target_user"
}

# Check sed version at start
check_sed_version

# Show menu of tools to install
echo "Select which tools to install:"
echo "1. sed 4.9"
echo "2. Oh My Posh"
echo "3. All of the above"
echo "4. Skip tool installation"

read -p "Enter choice (1-4): " tool_choice

case $tool_choice in
    1)
        install_sed_49
        ;;
    2)
        read -p "Enter username to install Oh My Posh for (default: $USERNAME): " custom_username
        custom_username=${custom_username:-$USERNAME}
        install_oh_my_posh "$custom_username"
        ;;
    3)
        install_sed_49
        read -p "Enter username to install Oh My Posh for (default: $USERNAME): " custom_username
        custom_username=${custom_username:-$USERNAME}
        install_oh_my_posh "$custom_username"
        ;;
    4)
        print_warning "Skipping tool installation"
        ;;
    *)
        print_error "Invalid choice"
        exit 1
        ;;
esac

print_success "Tools installation completed"
