# Ada Development Environment Installation Scripts

This project provides a modular set of installation scripts for setting up an Ada development environment. The scripts are designed to be flexible, allowing installation on a fresh Debian system or on an existing system with users already set up.

## Overview

The installation is divided into several modular scripts:

- `install-main.sh` - Main entry point that presents installation options
- `install-utils.sh` - Utility functions and shared variables
- `install-dependencies.sh` - Installs all system dependencies
- `install-users.sh` - Creates main user (devy) with configuration files
- `install-scc.sh` - Installs Go and SCC (Source Code Counter) directly
- `install-gnatpro.sh` - Installs GNAT Pro and related libraries (optional)
- `install-tools.sh` - Installs additional tools (sed 4.9, Oh My Posh)
- `install-fresh.sh` - Performs a complete installation on a fresh system

## Prerequisites

- A Debian-based Linux distribution
- Root privileges (sudo)
- Required files if installing GNAT Pro (must be in the same directory as the scripts with exactly these names):
  - `gnatpro.tar.gz` (GNAT Pro release)
  - `asis.tar.gz`
  - `asis-tree-generator.tar.gz`
  - `florist.tar.gz`
  - `gnatcheck.tar.gz`
  - `alire.zip`
  - `adasubst.zip`
  - `adadep.zip`
  - `Comps.zip`
  - `Asiscomps.zip`
  - `Adacontrol.zip`

## Installation Options

### 1. Full Installation on Fresh Debian

This option will:
- Install all dependencies
- Create the main user (devy)
- Install Go and SCC
- Optionally install GNAT Pro
- Install additional tools

```bash
sudo ./install-main.sh
# Select option 1
```

### 2. Install Dependencies Only

This option installs only the system dependencies required for Ada development:

```bash
sudo ./install-main.sh
# Select option 2
```

### 3. Install GNAT Pro and/or Ada Tools

This option provides a menu to install:
- GNAT Pro only
- Ada tools only (if GNAT Pro is already installed)
- Both GNAT Pro and Ada tools

```bash
sudo ./install-main.sh
# Select option 3
```

### 4. Install Everything Except GNAT Pro

This option installs all components except GNAT Pro:

```bash
sudo ./install-main.sh
# Select option 4
```

### 5. Custom Installation

This option allows you to select specific components to install:

```bash
sudo ./install-main.sh
# Select option 5
```

## Running Individual Scripts

You can also run individual scripts directly:

```bash
sudo ./install-dependencies.sh
sudo ./install-users.sh
sudo ./install-scc.sh
sudo ./install-gnatpro.sh
sudo ./install-tools.sh
```

## User Configuration

The `install-users.sh` script will:
1. Create the main user (devy by default)
2. Copy **all** files and directories from `../rootfs/home/` to the user's home directory

This allows you to prepare a complete user environment including:
- Shell configuration files (`.zshrc`, `.bashrc`, etc.)
- Custom scripts (e.g., in a `bin/` directory)
- Configuration files for various tools
- Any other files or directories you want in the user's home

If the `../rootfs/home/` directory doesn't exist, the script will create an empty one and notify you to populate it.

## After Installation

After installation, either log out and log back in, or run:

```bash
source /etc/profile.d/gnatpro.sh
```

to activate the environment.

## Additional Tools

The `install-tools.sh` script installs several utilities:

### sed 4.9
This script installs sed version 4.9 to replace the system version. This is **required** for benchmark scripts to work correctly, particularly with the `-i` (in-place editing) option. Older versions of sed have critical bugs that will cause benchmark scripts to fail.

The script will check your current sed version and warn you if it's older than 4.9. If you plan to run benchmark scripts, installing sed 4.9 is not optional but mandatory.

### Oh My Posh
Oh My Posh is an optional prompt theme engine that makes your terminal more informative and visually appealing. It adds git status, execution time, and other useful information to your command prompt. While not essential for development, it's recommended for a better terminal experience but entirely optional.

You can choose to install both, either, or neither of these tools when running the script.

## Customization

You can modify the `install-utils.sh` script to change default variables such as:

- `USERNAME` - Default username (default: devy)
- `USER_UID` - Default user UID (default: 1000)
- `USER_GID` - Default user GID (default: 1000)
- `GNAT_RELEASE` - GNAT Pro release filename (default: gnatpro.tar.gz)
- `GO_VERSION` - Go version to install (default: 1.24.2)
- `SCC_VERSION` - SCC version to install (default: v3.5.0)

## Troubleshooting

If you encounter issues:

1. Check if all required files are present
2. Verify that you have sufficient permissions
3. Check installation logs for errors
4. Try running specific components individually to isolate the issue

## File Structure for GNAT Pro Installation

To install GNAT Pro and Ada tools, you must place the following files in the same directory as the installation scripts:

```
Adacontrol.zip
Asiscomps.zip
Comps.zip
adadep.zip
adasubst.zip
alire.zip
asis-tree-generator.tar.gz
asis.tar.gz
florist.tar.gz
gnatcheck.tar.gz
gnatpro.tar.gz
```

The file names must match exactly as shown above. If your original files have different names (e.g., version numbers), you'll need to rename them before running the installation.
