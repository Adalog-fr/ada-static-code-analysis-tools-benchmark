#!/bin/bash

set -e

####################
#                  #
# Global variables #
#                  #
####################

# Define prefix variable with default value
prefix=${prefix:-"/usr/gnat"}
tmpWorkdir=${tmpWorkdir:-"/tmp"}

# Define function to strip and unzip archives
unzip-strip() (
    set -eu
    local archive=$1
    local destdir=${2:-}
    shift; shift || :
    local tmpdir=$(mktemp -d)
    trap 'rm -rf -- "$tmpdir"' EXIT
    unzip -qd "$tmpdir" -- "$archive"
    shopt -s dotglob
    local files=("$tmpdir"/*) name i=1
    if (( ${#files[@]} == 1 )) && [[ -d "${files[0]}" ]]; then
        name=$(basename "${files[0]}")
        files=("$tmpdir"/*/*)
    else
        name=$(basename "$archive"); name=${archive%.*}
        files=("$tmpdir"/*)
    fi
    if [[ -z "$destdir" ]]; then
        destdir=./"$name"
    fi
    while [[ -f "$destdir" ]]; do destdir=${destdir}-$((i++)); done
    mkdir -p "$destdir"
    cp -ar "$@" -t "$destdir" -- "${files[@]}"
)

############################
#                          #
# Install all dependencies #
#                          #
############################

# Define function to install a package
install_package() {
    pkg_name="$1"
    prefix="$2"
    archive_extension="${3:-.tar.gz}"  # Optional parameter with default value
    archive_path="$tmpWorkdir/${pkg_name}${archive_extension}"

    if [ -f "${archive_path}" ]; then
        cd "$tmpWorkdir"
        mkdir "${pkg_name}"
        case "${archive_extension}" in
            ".tar.gz")
                tar -xf "${archive_path}" -C "${pkg_name}/" --strip-components 1
                ;;
            ".zip")
                unzip-strip "${archive_path}" "${pkg_name}/"
                ;;
        esac

        cd "${pkg_name}"

        # Custom logic for different packages
        case "${pkg_name}" in
            "florist" | "gtkada")
                ./configure --prefix="${prefix}"
                make
                ;;
            "asis")
                if [ -f "patch.sh" ]; then
                    echo "[ASIS] patch.sh exists. Executing..."
                    sh patch.sh
                fi
                make all prefix="${prefix}"
                ;;
            "gnatcoll-core")
                make prefix="${prefix}" setup
                make prefix="${prefix}" build
                ;;
            "gnatcoll-bindings")
                # GMP
                cd gmp
                ./setup.py build
                ./setup.py install --prefix="${prefix}"
                cd ..
                # iconv
                cd iconv
                ./setup.py build
                ./setup.py install --prefix="${prefix}"
                cd ..
                ;;
            "adasubst")
                gprbuild -P build.gpr
                cp adasubst "${prefix}/bin/"
                ;;
            "adadep")
                cd src
                for file in *.adb; do
                    # Create a temporary file
                    temp_file=$(mktemp)

                    # Read the file line by line, removing lines containing "pragma No_Return"
                    while IFS= read -r line; do
                        if [[ "$line" != *"pragma No_Return"* ]]; then
                            echo "$line" >> "$temp_file"
                        fi
                    done < "$file"

                    # Overwrite the original file with the modified contents
                    mv "$temp_file" "$file"
                done
                gprbuild -P build.gpr
                cp adadep "${prefix}/bin/"
                cd ..
                ;;
        esac

        # Install if not specific packages
        if [[ "${pkg_name}" != @(gnatcoll-bindings|adadep|adasubst) ]]; then
            make install
        fi
    fi
}

# Define function to install package without building
install_package_no_build() {
    pkg_name="$1"
    target_dir="$2"
    archive_extension="${3:-.tar.gz}"  # Optional parameter with default value
    archive_path="$tmpWorkdir/${pkg_name}${archive_extension}"

    if [ -f "${archive_path}" ]; then
        cd "$tmpWorkdir"

        case "${archive_extension}" in
            ".tar.gz")
                tar -xf "${archive_path}" -C "${target_dir}/" --strip-components 1
                ;;
            ".zip")
                unzip-strip "${archive_path}" "${target_dir}/"
                ;;
        esac
    fi
}

# Install base packages

install_package "florist" "${prefix}"
install_package "asis" "${prefix}"
install_package_no_build "asistools" "${prefix}"
install_package_no_build "asis-tree-generator" "${prefix}"
install_package_no_build "gnatcheck" "${prefix}"
install_package_no_build "alire" "${prefix}" ".zip"
install_package "gtkada" "${prefix}"
install_package "gnatcoll-core" "${prefix}"
install_package "gnatcoll-bindings" "${prefix}"
install_package "adadep" "${prefix}" ".zip"
install_package "adasubst" "${prefix}" ".zip"
# libadalang not installed

# Complete setup

echo "Add the following paths to PATH: $prefix/libexec/asis-gnsa/bin:$prefix/bin:"
echo -e "PATH=\"$prefix/libexec/asis-gnsa/bin:$prefix/bin:\$PATH\"\nOS=\"unix\"" >> /etc/environment

# SHALL BE the last instruction!
# unset prefix;
