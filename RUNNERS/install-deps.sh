#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2024 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only

# this script contains functions for each implementation, to ensure that they
# can be run.

set -eo pipefail

cd "$(dirname "$(realpath "$0")")"

source common.sh

# this script assumes the presence of GNU coreutils on a Debian-based distro
if [[ "$(uname -mo)" != 'x86_64 GNU/Linux' || ! -e /etc/debian_version ]]; then
    printf 'Unsupported system!\n' >&2
    exit 1
fi

# trivial cases - these can be handled with one apt_wrapper call on Debian 12
algol_68_dependencies() { apt_wrapper a68g algol68g; }
bf_dependencies() { apt_wrapper beef beef; }
c_dependencies() { apt_wrapper cc gcc; }
cobol_dependencies() { apt_wrapper cobc gnucobol; }
cpp_dependencies() { apt_wrapper c++ g++; }
erlang_dependencies() { apt_wrapper escript erlang-base; }
forth_dependencies() { apt_wrapper gforth gforth; }
fortran_dependencies() { apt_wrapper gfortran gfortran; }
go_dependencies() { apt_wrapper gccgo gccgo; }
haskell_dependencies() { apt_wrapper ghc ghc; }
java_dependencies() { apt_wrapper javac default-jdk-headless; }
javascript_dependencies() { apt_wrapper node nodejs; }
kotlin_dependencies() { apt_wrapper kotlinc kotlin; }
lisp_dependencies() { apt_wrapper clisp clisp; }
lua_dependencies() { apt_wrapper lua lua5.2; }
nim_dependencies() { apt_wrapper nim nim; }
ocaml_dependencies() { apt_wrapper ocamlc ocaml; }
octave_dependencies() { apt_wrapper octave octave; }
pascal_dependencies() { apt_wrapper fpc fp-compiler; }
perl_dependencies() { apt_wrapper perl perl; }
php_dependencies() { apt_wrapper php php-cli; }
python_dependencies() { apt_wrapper python3 python3; }
r_dependencies() { apt_wrapper r r-cran-littler; }
ruby_dependencies() { apt_wrapper ruby ruby; }
scala_dependencies() { apt_wrapper scalac scala; }
typescript_dependencies() { apt_wrapper ts-node ts-node; }
vala_dependencies() { apt_wrapper valac valac;}

# these 2 are required parts of Debian as of version 12, but check just in case
awk_dependencies() { apt_wrapper awk mawk; }
sh_dependencies() { apt_wrapper sh dash; }

# a few with more than 1 dependency, but still fairly trivial
csharp_dependencies() {
    apt_wrapper mcs mono-mcs
    apt_wrapper cli mono-runtime
}
d_dependencies() {
    apt_wrapper ldc2 ldc
    apt_wrapper cc gcc
}
x86-64_linux_asm_dependencies() {
    apt_wrapper ld binutils
    apt_wrapper nasm nasm
}

# rustc is in the Debian 12 repos, but Fender has a dependency that needs a
# newer version of it, so for the sake of consistency, install the latest
# version of rustup for anything that uses rust
rust_dependencies() { rustup_install; }

# a couple which need to be installed from git with cargo
babalang_dependencies() {
    cargo_wrapper babalang --git https://github.com/RocketRace/babalang
}

fender_dependencies() {
    cargo_wrapper fender --git https://github.com/FenderLang/Fender
}

# the remaining ones have more complexity for various reasons

# gcc is used to compile Objective-C as well, but needs an extra package to be
# able to do that. Try to parse in objective-c mode to see if it's supported
objective-c_dependencies() {
    apt_wrapper gcc gcc
    if ! printf 'int main(void){}' | gcc -xobjective-c -E - &>/dev/null; then
        as_root apt-get install -qy gobjc
    fi
}

# for those with hard-coded versions to download, store them here to make it
# easier to change in the future

PWSH_V='7.4.2'
ODIN_V='dev-2024-01'
ROCKSTAR_COMMIT='c6c53db'
ZIG_V='0.12.0'
CFUNGE_V='1,001'

# we need to pull the source for the interpreter and build it locally
befunge_dependencies() {
    # do nothing if cfunge is already in PATH
    if cmd_exists cfunge; then return 0; fi
    apt_wrapper cmake cmake
    apt_wrapper make make
    apt_wrapper cc gcc
    apt_wrapper wget wget
    apt_wrapper tar tar
    apt_wrapper gzip gzip

    # split the second half of the URL into a var to fit within 80 columns
    local asset_path
    asset_path="archive/refs/tags/$CFUNGE_V.tar.gz"
    wget "https://github.com/VorpalBlade/cfunge/$asset_path"
    
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

# compile the odin compiler and symlink it into the PATH
odin_dependencies() {
    if cmd_exists odin; then return 0; fi
    apt_wrapper wget wget
    apt_wrapper llvm-as llvm
    apt_wrapper clang clang
    # download and compile Odin version
    wget "https://github.com/odin-lang/Odin/archive/refs/tags/$ODIN_V.tar.gz" \
        -O "Odin-$ODIN_V.tar.gz"
    mkdir -p odin
    pushd odin &>/dev/null
    tar --strip-components=1 -xf "../Odin-$ODIN_V.tar.gz"
    ./build_odin.sh
    popd &>/dev/null
    ln -s ../odin/odin bin/odin
}

# the rockstar reference implementation, satriani, must be run with the cwd set
# to the directory containing it. It also annoyingly prints out a message that
# "(program returned no output)" after running a program. To properly use it
# for colortest, the line printing that needs to be commented out.
rockstar_dependencies() {
    if cmd_exists satriani-wrapper; then return 0; fi
    apt_wrapper yarnpkg yarnpkg
    apt_wrapper node nodejs
    apt_wrapper git git
    # clone the official rockstar repo
    git clone https://github.com/RockstarLang/rockstar
    pushd rockstar/satriani &>/dev/null
    # switch to a commit where the following sed command is known to work
    git checkout "$ROCKSTAR_COMMIT"
    # use this sed command to comment out the annoying line
    sed -i '/program returned no output/s#^#//#' rockstar.js
    # install dependencies and build the language grammer with pegjs
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
    apt_wrapper gzip gzip
    apt_wrapper tar tar
    apt_wrapper wget wget
    # PowerShell needs a bunch of libs, all but one of which start with "lib"
    for lib in c6 gcc-s1 gssapi-krb5-2 icu72 ssl3 stdc++6; do
        if ! dpkg --list "lib$lib" &>/dev/null; then
            as_root apt-get install -qy "lib$lib"
        fi
    done
    if ! dpkg --list zlib1g &>/dev/null; then
        as_root apt-get install -qy zlib1g
    fi

    if [ ! -f powershell/pwsh ]; then
        mkdir -p powershell
        pushd powershell &>/dev/null
        # split the second half of the URL into a var to fit within 80 columns
        local asset_path
        asset_path="download/v$PWSH_V/powershell-$PWSH_V-linux-x64.tar.gz"
        wget "https://github.com/PowerShell/PowerShell/releases/$asset_path"
        tar -xzf "powershell-$PWSH_V-linux-x64.tar.gz"
        # link powershell within the bin directory
        cd ../bin
        ln -s ../powershell/pwsh pwsh
        popd &>/dev/null
    fi
}

zig_dependencies() {
    # do nothing if zig is already in PATH
    if cmd_exists zig; then return 0; fi
    # packages needed to download and extract Zig's archive
    apt_wrapper tar tar
    apt_wrapper wget wget
    apt_wrapper xz xz-utils
    mkdir -p zig
    pushd zig &>/dev/null
    wget "https://ziglang.org/download/$ZIG_V/zig-linux-x86_64-$ZIG_V.tar.xz"
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
