#shellcheck shell=bash
# SPDX-FileCopyrightText: 2024 Eli Array Minkoff
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

# ensure the bin dir exists
mkdir -p "$basedir/bin"

# shorthand wrapper for command -v <some command> &>/dev/null
cmd_exists() {
    command -v "$1" &>/dev/null
}

# if the argument isn't in the PATH, it's appended to it
ensure_in_path() {
    case ":$PATH:" in
        *"$1"*) : ;; # do nothing
        *) PATH="$PATH:$1" ;;
    esac
}

# if something is installed with cargo, install it here
export CARGO_HOME="${CARGO_HOME-$basedir/cargo}"

if ! { cmd_exists rustup && cmd_exists rustc ; }; then 
    # if we install rust with rustup, keep it local to this project.
    export RUSTUP_HOME="${RUSTUP_HOME-$basedir/rustup}"
fi

ensure_in_path "$basedir/bin"
ensure_in_path "$basedir/cargo/bin"

# create a wrapper function to use the best available "run as root" command
# prefer sudo over doas, doas over pkexec, and pkexec over piping to su
if [ "$EUID" -eq 0 ]; then
    as_root() {
        # we're already root
        "$@"
    }
elif cmd_exists sudo; then
    as_root() {
        sudo "$@"
    }
elif cmd_exists doas; then
    as_root() {
        doas "$@"
    }
elif cmd_exists pkexec; then
    as_root() {
        pkexec "$@"
    }
else
    # fail if attempting to run as root at this point.
    as_root() {
        printf 'Not running as root, and none of sudo, doas, or pkexec ' >&2
        printf 'could be found in PATH. Aborting!\n' >&2
        exit 2
    }
fi

# checks if command listed in first argument exists
# if not, call `apt-get install -qy` with the remaining arguments
apt_wrapper() {
    if ! cmd_exists "$1"; then
        shift
        as_root apt-get install -qy "$@"
    fi
}

# non-interactively install rust tooling using rustup if needed
rustup_install() {
    apt_wrapper cc gcc
    apt_wrapper curl curl
    if ! cmd_exists rustup; then
        # the rustup command from rustup.rs, with -s -- -y appended to make it
        # non-interactive
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    fi
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
        apt_wrapper wget wget ca-certificates
        wget "$1"
    fi
}

# checks if command listed in first argument exists
# if not, invoke cargo with the remaining arguments to install it
cargo_wrapper() {
    cmd="$1"
    shift
    if ! cmd_exists "$cmd"; then
        if ! cmd_exists cargo; then
            rustup_install
        fi
        cargo install "$@"
    fi
}

