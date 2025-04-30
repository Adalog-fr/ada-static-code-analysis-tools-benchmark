#!/bin/bash
# GNAT Pro installation script - Installs GNAT Pro and required libraries
# Created on $(date +"%d-%m-%Y")

set -e  # Exit on error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/install-utils.sh"

print_banner "GNAT Pro and Ada Tools Installation"
check_root

# Define required files for ASIS and tools with simplified names
required_files=(
    "asis.tar.gz"
    "asis-tree-generator.tar.gz"
    "florist.tar.gz"
    "gnatcheck.tar.gz"
    "alire.zip"
)

# Show menu for installation options
echo "Please select what you want to install:"
echo "1. Install GNAT Pro only"
echo "2. Install Ada tools only (ASIS, Florist, etc.)"
echo "3. Install both GNAT Pro and Ada tools"
echo "4. Exit"

read -p "Enter option (1-4): " install_option

case $install_option in
    1)
        install_gnatpro=true
        install_tools=false
        ;;
    2)
        install_gnatpro=false
        install_tools=true
        ;;
    3)
        install_gnatpro=true
        install_tools=true
        ;;
    4)
        print_warning "Exiting installation"
        exit 0
        ;;
    *)
        print_error "Invalid option"
        exit 1
        ;;
esac

cwd=$PWD
# GNAT Pro installation
if [[ "$install_gnatpro" == true ]]; then
    # Check if GNAT Pro is already installed
    if [[ -d "/usr/gnat" && -f "/usr/gnat/bin/gnat" ]]; then
        print_warning "GNAT Pro appears to be already installed"
        read -p "Reinstall anyway? (y/n, default: n): " reinstall
        reinstall=${reinstall:-n}
        if [[ "$reinstall" != "y" && "$reinstall" != "Y" ]]; then
            print_warning "Skipping GNAT Pro installation"
        else
            perform_gnatpro_install=true
        fi
    else
        perform_gnatpro_install=true
    fi

    # Perform GNAT Pro installation if needed
    if [[ "$perform_gnatpro_install" == true ]]; then
        # Allow user to specify GNAT Pro release file
        read -p "Enter path to GNAT Pro release file (default: $GNAT_RELEASE): " custom_gnat_release
        custom_gnat_release=${custom_gnat_release:-$GNAT_RELEASE}
        GNAT_RELEASE="$custom_gnat_release"

        # Check if GNAT Pro file exists
        if ! check_file_exists "$GNAT_RELEASE"; then
            print_error "GNAT Pro file not found: $GNAT_RELEASE"
            exit 1
        fi

        # Install GNAT Pro
        print_step "Installing GNAT Pro"
        mkdir -p /tmp/gnat_release
        tar -xf "$GNAT_RELEASE" -C /tmp/gnat_release/ --strip-components 1
        cp ./default_install.sh /tmp/gnat_release
        cd /tmp/gnat_release
        ./default_install.sh
        cd /usr/gnat/share
        # Remove unnecessary components
        rm -rf gnatstudio gps
        print_success "GNAT Pro installed successfully"
    fi
fi

cd $cwd

# Ada tools installation
if [[ "$install_tools" == true ]]; then
    print_step "Installing ASIS and additional Ada tools"

    # Check if GNAT Pro is installed (required for tools)
    if [[ ! -d "/usr/gnat" || ! -f "/usr/gnat/bin/gnat" ]]; then
        print_error "GNAT Pro is not installed but is required for the Ada tools"
        print_error "Please install GNAT Pro first or select option 3"
        exit 1
    fi

    if [[ ":$PATH:" != *":/usr/gnat/bin:"* ]]; then
      PATH="/usr/gnat/bin:$PATH"
      echo 'export PATH="/usr/gnat/bin:$PATH"' >> /etc/profile
    fi

    # Check if required files are present
    missing_files=false
    for file in "${required_files[@]}"; do
        if ! check_file_exists "$file"; then
            print_error "Required file not found: $file"
            missing_files=true
        else
            cp "$file" /tmp/
        fi
    done

    if [[ "$missing_files" == true ]]; then
        print_error "Some required files are missing. Cannot proceed with tools installation."
        exit 1
    fi

    # Running installation commands
    print_step "Installing Ada libraries and tools"
    deno run --allow-run --allow-write --allow-read --allow-env ./install_ada_setup.ts install florist
    deno run --allow-run --allow-write --allow-read --allow-env ./install_ada_setup.ts install asis
    deno run --allow-run --allow-write --allow-read --allow-env ./install_ada_setup.ts install -b asis-tree-generator
    deno run --allow-run --allow-write --allow-read --allow-env ./install_ada_setup.ts install -b gnatcheck
    deno run --allow-run --allow-write --allow-read --allow-env ./install_ada_setup.ts install -b -e .zip alire

    # Setting up GPR_PROJECT_PATH
    export GPR_PROJECT_PATH=.:/tmp/Comps:/tmp/Asiscomps:

    deno run --allow-run --allow-write --allow-read --allow-env ./install_ada_setup.ts install -e .zip Adacontrol

    print_success "Ada tools installed successfully"
fi

# Update environment variables
setup_environment_vars

if [[ "$install_gnatpro" == true && "$install_tools" == true ]]; then
    print_success "GNAT Pro and Ada tools installation completed"
elif [[ "$install_gnatpro" == true ]]; then
    print_success "GNAT Pro installation completed"
elif [[ "$install_tools" == true ]]; then
    print_success "Ada tools installation completed"
fi
