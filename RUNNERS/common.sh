#shellcheck shell=bash
# SPDX-FileCopyrightText: 2024 - 2025 Eli Array Minkoff
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
    fender forth fortran go haskell java javascript jq kotlin lisp lua nim
    objective-c ocaml octave odin pascal perl php powershell python r rockstar
    ruby rust scala scheme sh typescript vala wasm x86-64_linux_asm zig
)

basedir="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"


# if the argument isn't in the PATH, it's appended to it
ensure_in_path() {
    case ":$PATH:" in
        *"$1"*) : ;; # do nothing
        *) PATH="$PATH:$1" ;;
    esac
}

# if something is installed with cargo, install it here
export CARGO_HOME="${CARGO_HOME-$basedir/cargo}"

# shorthand wrapper for command -v <some command> &>/dev/null
cmd_exists() {
    command -v "$1" &>/dev/null
}

ensure_in_path "$basedir/bin"
ensure_in_path "$basedir/cargo/bin"
