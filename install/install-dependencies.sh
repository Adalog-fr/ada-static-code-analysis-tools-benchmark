#!/bin/bash
# Dependencies installation script - Installs all required packages
# Created on $(date +"%d-%m-%Y")

set -e  # Exit on error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/install-utils.sh"

print_banner "Installing System Dependencies"
check_root

# Install base dependencies
print_step "Installing base dependencies (python:3.10)"
apt-get update -y
apt-get install -y --no-install-recommends vim make libc6-dev python3.10 python3-pip python3-venv

# Install Ada development dependencies
print_step "Installing dependencies for compiling Ada files"
apt-get install -y build-essential libssl-dev \
    unzip liblzma-dev libcurl4-openssl-dev \
    default-libmysqlclient-dev \
    mariadb-client libmariadbclient-dev-compat \
    sqlite3 libsqlite3-dev \
    postgresql-client libpq-dev \
    libgtk-3-dev \
    libfuse-dev \
    lua5.4 liblua5.4-dev liblua5.3-dev \
    liblapack3 libblas3 libblas-dev \
    libbullet-dev libfreetype6-dev libexpat1-dev \
    libcanberra-pulse \
    libfbclient2 \
    libcsfml-dev \
    libglfw3-dev \
    libgmp-dev \
    libgsl-dev \
    libhidapi-dev \
    libpython3-dev \
    librtmidi-dev \
    libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev \
    libzmq5 libzmq3-dev \
    libx11-dev \
    re2c \
    unixodbc-dev \
    zlib1g-dev \
    libgnutls28-dev liblapack-dev \
    libmpfr-dev libcanberra-dev libusb-1.0-0-dev \
    zsh sudo \
    jc \
    iputils-ping iproute2 git time jq unzip libgmp-dev curl wget parallel fd-find \
    gcc libssl-dev libffi-dev python3-dev

# Install Neo4J
print_step "Installing Neo4J: cypher-shell"
if ! command_exists cypher-shell; then
    wget --no-check-certificate https://dist.neo4j.org/cypher-shell/cypher-shell_5.23.0_all.deb
    apt install -y ./cypher-shell_5.23.0_all.deb
    rm cypher-shell_5.23.0_all.deb
    print_success "Neo4J cypher-shell installed"
else
    print_warning "Neo4J cypher-shell already installed, skipping"
fi

# Install Deno
print_step "Installing Deno"
if ! command_exists deno; then
    curl -fsSL https://deno.land/x/install/install.sh | DENO_INSTALL=/usr/local sh
    deno upgrade 1.46.3
    print_success "Deno installed"
else
    print_warning "Deno already installed, skipping"
fi

# Create python symlink
create_symlinks

print_success "All dependencies installed successfully"
