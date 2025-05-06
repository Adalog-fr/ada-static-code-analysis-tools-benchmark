#!/bin/bash
# SCC installation script - Installs Go and SCC directly
# Created on $(date +"%d-%m-%Y")

set -e  # Exit on error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/install-utils.sh"

print_banner "Installing Go and SCC"
check_root

GO_VERSION="1.24.2"
GO_DOWNLOAD_URL="https://go.dev/dl/go${GO_VERSION}.linux-amd64.tar.gz"
GO_INSTALL_PATH="/usr/local"
SCC_VERSION="v3.5.0"
SCC_PACKAGE="github.com/boyter/scc/v3@${SCC_VERSION}"

# Check if Go is already installed
if command_exists go; then
    current_go_version=$(go version | awk '{print $3}' | sed 's/go//')
    print_warning "Go version $current_go_version is already installed"
    read -p "Reinstall Go version ${GO_VERSION}? (y/n, default: n): " reinstall_go
    reinstall_go=${reinstall_go:-n}

    if [[ "$reinstall_go" != "y" && "$reinstall_go" != "Y" ]]; then
        print_warning "Skipping Go installation"
    else
        # Remove existing Go installation if it's in /usr/local/go
        if [[ -d "${GO_INSTALL_PATH}/go" ]]; then
            print_step "Removing existing Go installation"
            rm -rf "${GO_INSTALL_PATH}/go"
            print_success "Existing Go installation removed"
        fi

        install_go=true
    fi
else
    install_go=true
fi

# Install Go if needed
if [[ "$install_go" == true ]]; then
    print_step "Installing Go version ${GO_VERSION}"

    # Download Go
    cd /tmp
    if [[ ! -f "go${GO_VERSION}.linux-amd64.tar.gz" ]]; then
        wget -O "go${GO_VERSION}.linux-amd64.tar.gz" "${GO_DOWNLOAD_URL}"
    fi

    # Extract to /usr/local
    tar -C "${GO_INSTALL_PATH}" -xzf "go${GO_VERSION}.linux-amd64.tar.gz"
    rm "go${GO_VERSION}.linux-amd64.tar.gz"

    # Set up Go environment variables
    cat > /etc/profile.d/go.sh << EOF
export PATH=\$PATH:${GO_INSTALL_PATH}/go/bin:/root/go/bin
EOF
    chmod +x /etc/profile.d/go.sh

    # Source the environment variables
    source /etc/profile.d/go.sh

    print_success "Go version ${GO_VERSION} installed successfully"
fi

# Install SCC
print_step "Installing SCC version ${SCC_VERSION}"

# Check if SCC is already installed
if command_exists scc; then
    current_scc_version=$(scc --version 2>&1 | grep -o "v[0-9]\+\.[0-9]\+\.[0-9]\+" | head -n 1)
    print_warning "SCC version $current_scc_version is already installed"
    read -p "Reinstall SCC version ${SCC_VERSION}? (y/n, default: n): " reinstall_scc
    reinstall_scc=${reinstall_scc:-n}

    if [[ "$reinstall_scc" != "y" && "$reinstall_scc" != "Y" ]]; then
        print_warning "Skipping SCC installation"
        exit 0
    fi
fi

# Make sure Go is in PATH for this script
export PATH=$PATH:${GO_INSTALL_PATH}/go/bin

# Install SCC using Go
go install "${SCC_PACKAGE}"

# Check if installation was successful
if [[ -f "/root/go/bin/scc" ]]; then
    # Make SCC available for all users
    cp /root/go/bin/scc /usr/local/bin/
    chmod +x /usr/local/bin/scc
    print_success "SCC installed successfully"
else
    print_error "Failed to install SCC"
    exit 1
fi

# Create environment variables for user setup
for user_home in /home/*; do
    user=$(basename "$user_home")

    if [[ "$user" != "lost+found" && -d "$user_home" ]]; then
        # Create Go directory for user
        mkdir -p "${user_home}/go/bin"
        chown -R ${user}:${user} "${user_home}/go"

        # Add Go binaries to user's PATH
        if [[ -f "${user_home}/.bashrc" ]]; then
            if ! grep -q "export PATH=\$PATH:${GO_INSTALL_PATH}/go/bin" "${user_home}/.bashrc"; then
                echo "export PATH=\$PATH:${GO_INSTALL_PATH}/go/bin:~/go/bin" >> "${user_home}/.bashrc"
            fi
        fi

        if [[ -f "${user_home}/.zshrc" ]]; then
            if ! grep -q "export PATH=\$PATH:${GO_INSTALL_PATH}/go/bin" "${user_home}/.zshrc"; then
                echo "export PATH=\$PATH:${GO_INSTALL_PATH}/go/bin:~/go/bin" >> "${user_home}/.zshrc"
            fi
        fi
    fi
done

print_success "Go and SCC setup completed for all users"
