#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2024 - 2025 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only

# this script contains functions for each implementation, to ensure that they
# can be run.

set -eo pipefail

cd "$(dirname "$(realpath "$0")")"

source common.sh

# shorthand wrapper for command -v <some command> &>/dev/null
cmd_exists() {
    command -v "$1" &>/dev/null
}

# create a wrapper function to use the best available "run as root" command
# prefer sudo over doas, doas over pkexec, and pkexec over crashing
if [ "$EUID" -eq 0 ]; then
    # we're already root
    as_root() { "$@"; }
elif cmd_exists sudo; then
    as_root() { sudo "$@"; }
elif cmd_exists doas; then
    as_root() { doas "$@"; }
elif cmd_exists pkexec; then
    as_root() { pkexec "$@"; }
else
    # fail if attempting to run as root at this point.
    as_root() {
        printf 'Not running as root, and none of sudo, doas, or pkexec ' >&2
        printf 'could be found in PATH. Aborting!\n' >&2
        exit 2
    }
fi

is_installed () {
    [ "$(
        dpkg-query -W -f '${db:Status-Status}' "$1" 2>/dev/null
    )" = 'installed' ]
}


declare -a to_install

# register listed packages for installation if not already installed
apt_wrapper() {
    for pkg in "$@"; do
        # `|| :` ensures that it won't exit on failure here
        is_installed "$pkg" || to_install+=("$pkg");
    done
}

# Install registered packages - should be called before non-apt commands to
# ensure dependencies are installed, and should be called at the end of the
# script. This allows consecutive calls to apt to be combined
apt_install() {
    if [ "${#to_install}" -gt 0 ]; then
        as_root apt-get install -qy "${to_install[@]}"
    fi
    to_install=()
}

# First argument is a command. If that command is not found, the rest of
# the arguments are passed to apt_wrapper. As a special case, if only one arg 
# was passed, it's assumed to be both the command and the package name.
apt_if() {
    cmd="$1"
    if [ "$#" -gt 1 ]; then shift; fi
    if ! cmd_exists "$cmd"; then apt_wrapper "$@"; fi
}

# takes a url as an argument, and downloads it unless the output already exists
wget_if() {
    # substitute %NN with \xNN, then echo -e to process backslash escapes
    # thanks to https://stackoverflow.com/a/6265305 for pointing me to that
    local url_decoded
    url_decoded="$(echo -e "${1//%/\\x}")"

    # ${FOO##pattern}" greedily matches pattern at the start of FOO, removing
    # the matching component.
    # https://www.cyberciti.biz/tips/bash-shell-parameter-substitution-2.html
    # This removes everything before the last slash in the url_decoded value
    # and runs wget if it doesn't exist
    if ! [ -f "${url_decoded##*/}" ]; then
        apt_if wget
        apt_wrapper ca-certificates
        apt_install
        wget -nv "$1"
    fi
}

# checks if command listed in first argument exists
# if not, invoke cargo with the remaining arguments to install it
cargo_wrapper() {
    apt_if cargo
    apt_wrapper ca-certificates
    apt_install

    # if CARGO_HOME isn't defined, and its default location doesn't exist,
    # set it to RUNNERS/cargo to avoid polluting the user's home directory.
    if [[ "${CARGO_HOME+defined}" != defined && ! -e ~/.cargo ]]; then
        export CARGO_HOME="$PWD/cargo"
    fi

    if ! cmd_exists "$1"; then
        shift
        cargo install --root . --locked "$@"
    fi
}

# this script assumes the presence of GNU coreutils on a Debian-based distro
if [[ "$(uname -mo)" != 'x86_64 GNU/Linux' || ! -e /etc/debian_version ]]; then
    printf 'Unsupported system!\n' >&2
    exit 1
fi

# sh is always available, or else we're doomed.
sh_dependencies() { cmd_exists sh; }

# trivial cases - if command does not exist, install it as an apt package
bf_dependencies() { apt_if beef; }
dc_dependencies() { apt_if dc; }
elixir_dependencies() { apt_if elixir; }
forth_dependencies() { apt_if pforth; }
fortran_dependencies() { apt_if gfortran; }
go_dependencies() { apt_if gccgo; }
haskell_dependencies() { apt_if ghc; }
jq_dependencies() { apt_if jq; }
kotlin_dependencies() { apt_if kotlin; }
ocaml_dependencies() { apt_if ocaml; }
octave_dependencies() { apt_if octave; }
perl_dependencies() { apt_if perl; }
lisp_dependencies() { apt_if clisp; }
python_dependencies() { apt_if python3; }
ruby_dependencies() { apt_if ruby ; }
rust_dependencies() { apt_if rustc; }
scala_dependencies() { apt_if scala; }
typescript_dependencies() { apt_if ts-node; }

# getting a bit more complex here - the package name does not match the
# command name, but it's still pretty simple.
algol_68_dependencies() { apt_if a68g algol68g; }
awk_dependencies() { apt_if awk mawk; } # required in Debian 12, still check
cobol_dependencies() { apt_if cobc gnucobol; }
cpp_dependencies() { apt_if c++ g++; }
erlang_dependencies() { apt_if escript erlang-base; }
java_dependencies() { apt_if javac default-jdk-headless; }
javascript_dependencies() { apt_if node nodejs; }
lua_dependencies() { apt_if lua lua5.2; }
pascal_dependencies() { apt_if fpc fp-compiler; }
php_dependencies() { apt_if php php-cli; }
r_dependencies() { apt_if r r-cran-littler; }
scheme_dependencies() { apt_if csi chicken-bin; }

# these need more than just a single package
c_dependencies() {
    apt_if cc tcc # if no C compiler is installed, install gcc
    apt_wrapper libc6-dev # need to have C standard library headers
}
csharp_dependencies() {
    apt_if mcs mono-mcs
    apt_if cli mono-runtime
}
objective-c_dependencies() {
    apt_wrapper gcc gobjc gnustep-make make libgnustep-base-dev
}
vala_dependencies() {
    apt_if cc tcc
    apt_if valac
}
x86-64_linux_asm_dependencies() {
    apt_if ld binutils
    apt_if nasm nasm
}

# a couple which need to be installed from git with cargo
babalang_dependencies() {
    cargo_wrapper babalang --git https://github.com/RocketRace/babalang
}

fender_dependencies() {
    cargo_wrapper fender --git https://github.com/FenderLang/Fender
}

# the remaining ones have more complexity for various reasons

d_dependencies() {
    apt_wrapper libc6-dev # need to have C standard library headers
    # ldc2 shells out to `cc`, but uses gcc-specific flags, so make sure that
    # `cc` is actually GCC
    apt_if gcc 
    apt_if ldc2 ldc
    apt_install
    ln -sf "$(type -p gcc)" bin/cc
}

# for dependencies with hard-coded versions to download, store them here to
# make it easier to change in the future

PWSH_V='7.5.4'
NIM_V='2.2.6'
ODIN_V='dev-2025-12'
ROCKSTAR_COMMIT='c6c53db'
ZIG_V='0.15.2'
WASMTIME_V='40.0.0'
CFUNGE_V='1,001'

# we need to pull the source for the interpreter and build it locally
befunge_dependencies() {
    # do nothing if cfunge is already in PATH
    if cmd_exists cfunge; then return 0; fi
    apt_wrapper cmake make gcc tar gzip libc6-dev libbsd-dev
    apt_install
    # split the second half of the URL into a var to fit within 80 columns
    local asset_path
    asset_path="archive/refs/tags/$CFUNGE_V.tar.gz"
    wget_if "https://github.com/VorpalBlade/cfunge/$asset_path"
    
    # create local install prefix at colortest/RUNNERS/cfunge
    local pfx
    pfx="$PWD/cfunge"
    mkdir -p cfunge

    # extract source to cfunge_src directory
    mkdir -p cfunge_src
    pushd cfunge_src &>/dev/null
    tar --strip-components=1 -xf "../$CFUNGE_V.tar.gz"

    # install to the local install prefix
    mkdir -p build
    cd build
    cmake ../ -DCMAKE_INSTALL_PREFIX="$pfx"
    make
    make install
    popd &>/dev/null

    # link into colortest/RUNNERS/bin
    ln -s ../cfunge/bin/cfunge bin/cfunge
}

nim_dependencies() {
    apt_if gcc
    apt_wrapper libc6-dev
    if cmd_exists nim; then return 0; fi
    apt_if xz xz-utils
    apt_install
    wget_if "https://nim-lang.org/download/nim-$NIM_V.tar.xz"
    mkdir -p nim-build
    pushd nim-build &>/dev/null
    tar --strip-components=1 -xf "../nim-$NIM_V.tar.xz"
    sh build.sh
    sh install.sh ..
    cd ../bin
    ln -s ../nim/bin/nim
    popd &>/dev/null
}

# compile the odin compiler and symlink it into the PATH
odin_dependencies() {
    if cmd_exists odin; then return 0; fi
    apt_wrapper libc6-dev llvm-dev
    apt_if llvm-as llvm
    apt_if clang
    apt_install
    # download and compile Odin version
    wget_if "https://github.com/odin-lang/Odin/archive/refs/tags/$ODIN_V.tar.gz"
    mkdir -p odin
    pushd odin &>/dev/null
    tar --strip-components=1 -xf "../$ODIN_V.tar.gz"
    ./build_odin.sh release
    popd &>/dev/null
    ln -s ../odin/odin bin/odin
}

# the former rockstar reference implementation, satriani, must be run with the
# cwd set to the directory containing it. It also annoyingly prints out a
# message that "(program returned no output)" after running a program.
# To properly use it for colortest, the line printing that needs to be commented
# out.
rockstar_dependencies() {
    if cmd_exists satriani-wrapper; then return 0; fi
    apt_if yarnpkg
    apt_if node nodejs
    apt_if git
    apt_install
    # clone the official rockstar repo
    git clone https://github.com/RockstarLang/rockstar
    pushd rockstar/ &>/dev/null
    # switch to a commit where the following sed command is known to work
    git checkout "$ROCKSTAR_COMMIT"
    cd satriani
    # use this sed command to comment out the annoying line
    sed -i '/program returned no output/s#^#//#' rockstar.js
    # install dependencies and build the language grammar with pegjs
    yarnpkg install
    yarnpkg pegjs
    popd &>/dev/null
    # create the satriani-wrapper script, if needed
    cat >bin/satriani-wrapper <<EOF
#!/bin/sh
# satriani must be run from within its directory, so the first step is to
# save the absolute path of the command to a variable
if [ \$# -ne 1 ]; then
    printf 'Must pass 1 argument to the command line!\n' >&2
    exit 2
fi

program="\$(realpath "\$1")"
cd "\$(dirname "\$(realpath "\$0")")/../rockstar/satriani" || exit 4
shift

exec node rockstar "\$program" "\$@"
EOF
    chmod +x bin/satriani-wrapper
}

# for the last couple, just download the pre-built executables and symlink them
# into the PATH
powershell_dependencies() {
    # do nothing if pwsh is already in PATH
    if cmd_exists pwsh; then return 0; fi
    # packages needed to download and extract PowerShell's archive
    apt_if gzip # required in debian, check anyway just in case
    apt_if tar # required in debian, check anyway just in case
    # PowerShell needs a bunch of libs, all but one of which start with "lib"
    apt_wrapper libc6 libgcc-s1 libgssapi-krb5-2 libicu76 libssl3 libstdc++6 zlib1g
    apt_install

    if [ ! -f powershell/pwsh ]; then
        mkdir -p powershell
        pushd powershell &>/dev/null
        # split the second half of the URL into a var to fit within 80 columns
        local asset_path
        asset_path="download/v$PWSH_V/powershell-$PWSH_V-linux-x64.tar.gz"
        wget_if "https://github.com/PowerShell/PowerShell/releases/$asset_path"
        tar -xzf "powershell-$PWSH_V-linux-x64.tar.gz"
        # mark powershell as executable (it's not already in 7.4.6 tarball)
        chmod +x pwsh
        # link powershell within the bin directory
        cd ../bin
        ln -s ../powershell/pwsh pwsh
        popd &>/dev/null
    fi
}

wasm_dependencies() {
    # do nothing if wasmtime is already in path
    if cmd_exists wasmtime; then return 0; fi
    apt_if tar
    apt_if xz xz-utils
    apt_install
    mkdir -p wasmtime
    pushd wasmtime &>/dev/null
    local repo
    repo='bytecodealliance/wasmtime'
    local filename
    filename="wasmtime-v$WASMTIME_V-x86_64-linux.tar.xz"
    local url_path
    url_path="$repo/releases/download/v$WASMTIME_V/$filename"
    wget_if "https://github.com/$url_path"
    tar --strip-components=1 -xJf "$filename"
    cd ../bin
    ln -s ../wasmtime/wasmtime wasmtime
    popd &>/dev/null
}

zig_dependencies() {
    # do nothing if zig is already in PATH
    if cmd_exists zig; then return 0; fi
    # packages needed to download and extract Zig's archive
    apt_if tar
    apt_if xz xz-utils
    apt_install
    mkdir -p zig
    pushd zig &>/dev/null
    wget_if "https://ziglang.org/download/$ZIG_V/zig-linux-x86_64-$ZIG_V.tar.xz"
    tar --strip-components=1 -xJf "zig-linux-x86_64-$ZIG_V.tar.xz"
    cd ../bin
    ln -s ../zig/zig zig
    popd &>/dev/null
}

resolve_all() {
    for lang in "${colortest_implementations[@]}"; do
        "${lang}_dependencies"
    done
}

# if run directly without arguments, resolve all dependencies
# if run directly with arguments, assume each argument is an implementation,
# and resolve its dependencies
if [ "$#" -eq 0 ]; then
    resolve_all
else
    for lang in "$@"; do
        "${lang}_dependencies"
    done
fi
apt_install
