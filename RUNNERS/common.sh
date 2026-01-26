#shellcheck shell=bash
# SPDX-FileCopyrightText: 2024 - 2026 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only

# This is meant to be sourced by install-deps.sh and run-version.sh to ensure
# that some common variables and functions are defined properly, and the
# colortest/RUNNERS/bin directory exists.

# THESE FUNCTIONS ARE NOT MEANT TO HANDLE UNTRUSTED PARAMETERS!
# * They may pass along the parameters to executables to install software.
# * They may attempt to run commands as root.
# * They may try to download commands.

# bash array containing all implementations
# shellcheck disable=2034 # Shellcheck thinks that this is unused. It's not.
colortest_implementations=(
    algol_68 awk babalang befunge bf c cobol cpp csharp d dc elixir erlang
    fender forth fortran go haskell ial java javascript jq kotlin lisp lua nim
    objective-c ocaml octave odin pascal perl php powershell python r rockstar
    ruby rust scala scheme sh typescript vala wasm x86-64_linux_asm zig
)

rundir="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"
mkdir -p "$rundir/bin"

pathappend() {
    if [ "$#" -ne 1 ]; then printf 'internal error\n'>&2; exit 1; fi
    # if the parameter isn't in the PATH, append it
    case ":$PATH:" in
        *":$1:"*) : ;; # do nothing
        *) PATH="$PATH:$1" ;;
    esac
}

pathappend "$rundir/bin"
pathappend "$rundir/cargo/bin"

if [ "${RUSTUP_HOME+set}" != 'set' ] && ! [ -e "$HOME/.rustup" ]; then
    export RUSTUP_HOME="$rundir/rustup"
fi

if [ "${CARGO_HOME+set}" != 'set' ] && ! [ -e "$HOME/.cargo" ]; then
    export CARGO_HOME="$rundir/cargo"
fi
