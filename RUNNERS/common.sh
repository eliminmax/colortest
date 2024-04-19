#shellcheck shell=bash

# bash array containing all implementations
colortest_implementations=(
    algol_68 awk babalang befunge bf c cobol cpp csharp d erlang fender forth
    fortran go haskell java javascript kotlin lisp lua nim objective-c ocaml
    octave odin pascal perl php powershell python r rockstar ruby rust scala sh
    typescript vala x86-64_linux_asm zig
)

basedir="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"

# ensure the bin dir exists
mkdir -p "$basedir/bin"

# if the argument isn't in the PATH, it's appended to it
ensure_in_path() {
    case ":$PATH:" in
        *"$1"*) : ;; # do nothing
        *) PATH="$PATH:$1" ;;
    esac
}

ensure_in_path "$basedir/bin"
ensure_in_path "$basedir/cargo/bin"

# if something is installed with cargo, install it here
export CARGO_HOME="${CARGO_HOME-$basedir/cargo}"
# if we run rustup, keep it local to this project
export RUSTUP_HOME="${RUSTUP_HOME-$basedir/rustup}"

# shorthand wrapper for command -v <some command> &>/dev/null
cmd_exists() {
    command -v "$1" &>/dev/null
}

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
    as_root() {
        set +x
        echo "$@" | su - -c sh
        set -x
    }
fi

# checks if command listed in first argument exists
# if not, install the package specified in the second argument to install it
apt_wrapper() {
    if ! cmd_exists "$1"; then
        as_root apt-get install -q "$2"
    fi
}

rustup_install() {
    apt_wrapper curl curl
    if ! cmd_exists rustc; then
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
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

