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

bindir="$(dirname "$(realpath "${BASH_SOURCE[0]}")")/bin"

mkdir -p "$bindir"
# if the bindir isn't in the PATH, append it
case ":$PATH:" in
    *":$bindir:"*) : ;; # do nothing
    *) PATH="$PATH:$bindir" ;;
esac
