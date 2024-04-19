#!/bin/sh
set -e
cd "$(dirname "$(realpath "$0")")/.."

if ! command -v fd >/dev/null; then
    printf 'Missing dependency: fd (https://github.com/sharkdp/fd)\n' >&2
    exit 2
elif ! command -v tar >/dev/null; then
    printf 'Missing dependency: tar (https://www.gnu.org/software/tar/)\n' >&2
elif ! command -v podman >/dev/null; then
    printf 'Missing dependency: podman (https://podman.io/)\n'
fi

# The following command will generate a tarball called .files.tar
# with colortest_output, RUNNERS/install-deps.sh, RUNNERS/run-version.sh,
# and all colortest source files. The reason I'm using fd instead of find is
# that fd defaults to excluding files specified in .gitignore, so it will omit
# compiled executables and any intermidiate compiler artifacts, as long as the
# gitignore is properly set up.
fd --type file '^colortest\.' --exact-depth 2 -X \
    tar cf .files.tar ./colortest_output \
    ./RUNNERS/install-deps.sh \
    ./RUNNERS/run-version.sh

# now build the test
podman build --tag=colortester -f Containerfile .
