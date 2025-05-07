#!/bin/bash
# Linuxbrew installation script
# Created on $(date +"%d-%m-%Y")

set -e  # Exit on error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/install-utils.sh"

print_banner "Installing Linuxbrew"
check_root

# Check if linuxbrew user exists
if ! user_exists "linuxbrew"; then
    print_error "Linuxbrew user does not exist. Please run install-users.sh first."
    read -p "Create linuxbrew user now? (y/n): " create_now
    if [[ "$create_now" == "y" || "$create_now" == "Y" ]]; then
        bash "${SCRIPT_DIR}/install-users.sh"
    else
        exit 1
    fi
fi

# Check if Linuxbrew is already installed
if [[ -d "/home/linuxbrew/.linuxbrew" && -f "/home/linuxbrew/.linuxbrew/bin/brew" ]]; then
    print_warning "Linuxbrew appears to be already installed"
    read -p "Reinstall anyway? (y/n, default: n): " reinstall
    reinstall=${reinstall:-n}
    if [[ "$reinstall" != "y" && "$reinstall" != "Y" ]]; then
        print_warning "Skipping Linuxbrew installation"
        exit 0
    fi
fi

# Install Linuxbrew
print_step "Installing Linuxbrew"
su - linuxbrew -c "sh -c \"$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh)\""
print_success "Linuxbrew installed successfully"

# Install scc via Homebrew
print_step "Installing scc via Homebrew"
cat > /tmp/install_brew.sh << 'EOF'
#!/bin/bash
export PATH="/home/linuxbrew/.linuxbrew/bin:${PATH}"
export HOMEBREW_PREFIX="/home/linuxbrew/.linuxbrew"
export HOMEBREW_CELLAR="/home/linuxbrew/.linuxbrew/Cellar"
export HOMEBREW_REPOSITORY="/home/linuxbrew/.linuxbrew/Homebrew"
export MANPATH="/home/linuxbrew/.linuxbrew/share/man:${MANPATH}"
export INFOPATH="/home/linuxbrew/.linuxbrew/share/info:${INFOPATH}"
brew install scc
EOF
chmod +x /tmp/install_brew.sh

# Find an appropriate user to run brew install
if user_exists "$USERNAME"; then
    su - "$USERNAME" -c "/tmp/install_brew.sh"
else
    su - linuxbrew -c "/tmp/install_brew.sh"
fi

print_success "SCC installed via Homebrew"

# Install Oh My Posh if main user exists
if user_exists "$USERNAME"; then
    print_step "Installing Oh My Posh for $USERNAME"
    su - "$USERNAME" -c "curl -s https://ohmyposh.dev/install.sh | bash -s"
    print_success "Oh My Posh installed for $USERNAME"
fi

# Set up environment variables
setup_environment_vars

print_success "Linuxbrew setup completed"
