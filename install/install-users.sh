#!/bin/bash
# User creation script - Creates users needed for the development environment
# Created on $(date +"%d-%m-%Y")

set -e  # Exit on error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/install-utils.sh"

print_banner "Creating Users"
check_root

# Check if rootfs/home directory exists
rootfs_home_dir="${SCRIPT_DIR}/../rootfs/home"
if [[ ! -d "$rootfs_home_dir" ]]; then
    print_warning "rootfs/home directory not found at ${rootfs_home_dir}"
    print_step "Creating rootfs directory structure"
    mkdir -p "${rootfs_home_dir}"
    print_warning "Created empty rootfs/home directory. You should populate it with your configuration files."
fi

# Function to create main user if not exists
create_main_user() {
    local username="$1"
    local uid="$2"
    local gid="$3"

    if ! group_exists "$username"; then
        print_step "Creating group $username"
        groupadd -f --gid "$gid" "$username"
        print_success "Group $username created"
    else
        print_warning "Group $username already exists"
    fi

    if ! user_exists "$username"; then
        print_step "Creating user $username"
        useradd --uid "$uid" --gid "$gid" -m --no-log-init -G sudo -s /usr/bin/zsh "$username"
        echo "$username ALL=(root) NOPASSWD:ALL" >> /etc/sudoers
        print_success "User $username created with sudo access"

        # Copy all files from rootfs/home to user's home
        copy_home_files "$username"
    else
        print_warning "User $username already exists"

        # Ask if configuration files should be copied for existing user
        read -p "Copy configuration files to existing user's home? (y/n, default: n): " copy_config
        copy_config=${copy_config:-n}

        if [[ "$copy_config" == "y" || "$copy_config" == "Y" ]]; then
            copy_home_files "$username"
        fi
    fi
}

# Function to copy all files from rootfs/home to user's home directory
copy_home_files() {
    local username="$1"

    print_step "Copying all configuration files to $username's home directory"

    # Check if source directory exists
    rootfs_home_dir="${SCRIPT_DIR}/../rootfs/home"
    if [[ ! -d "$rootfs_home_dir" ]]; then
        print_warning "rootfs/home directory not found at ${rootfs_home_dir}"
        return 1
    fi

    # Copy all files and directories from rootfs/home to user's home
    shopt -s dotglob nullglob # Include hidden files and handle empty directories

    for item in "${rootfs_home_dir}"/*; do
        if [[ -e "$item" ]]; then
            item_name=$(basename "$item")

            # Copy the item
            if [[ -d "$item" ]]; then
                # For directories, use -r for recursive copy
                cp -r "$item" "/home/$username/"
                print_success "Copied directory $item_name to user's home"
            else
                # For files
                cp "$item" "/home/$username/"
                print_success "Copied file $item_name to user's home"
            fi

            # Set ownership
            chown -R "$username:$username" "/home/$username/$item_name"
        fi
    done

    shopt -u dotglob nullglob # Reset shell options

    print_success "All configuration files copied to $username's home directory"
    return 0
}

# Ask user if they want to create a new main user
read -p "Create new main user? (y/n, default: n): " create_user
create_user=${create_user:-n}

if [[ "$create_user" == "y" || "$create_user" == "Y" ]]; then
    # Prompt for username or use default
    read -p "Enter username (default: $USERNAME): " custom_username
    custom_username=${custom_username:-$USERNAME}

    # Prompt for UID or use default
    read -p "Enter UID (default: $USER_UID): " custom_uid
    custom_uid=${custom_uid:-$USER_UID}

    # Prompt for GID or use default
    read -p "Enter GID (default: $USER_GID): " custom_gid
    custom_gid=${custom_gid:-$USER_GID}

    # Create the user
    create_main_user "$custom_username" "$custom_uid" "$custom_gid"
    USERNAME="$custom_username"
else
    print_warning "Skipping main user creation"
fi

# We no longer need to create a linuxbrew user since we're using Go directly
print_step "Setting up user environment"

# Create .config directory for the user if it doesn't exist
if user_exists "$USERNAME"; then
    if [[ ! -d "/home/$USERNAME/.config" ]]; then
        mkdir -p "/home/$USERNAME/.config"
        chown -R "$USERNAME":"$USERNAME" "/home/$USERNAME/.config"
    fi
    print_success "User environment setup complete"
fi

print_success "User setup completed successfully"
