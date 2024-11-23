#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2024 Eli Array Minkoff
#
# SPDX-License-Identifier: CC0-1.0

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

tar_flags=(
    --numeric-owner --owner=1000 --group=1000 # set owner to colortester
    --mtime=@1645900384 # don't care about mtime, set to time of first commit
    --sort=name # keeps the order consistent
    --no-acls --no-xattrs --no-selinux # keep extra info out.
)

# The following command will generate a tarball called .files.tar
# with colortest_output, RUNNERS/install-deps.sh, RUNNERS/run-version.sh,
# and all colortest source files. The reason I'm using fd instead of find is
# that fd defaults to excluding files specified in .gitignore, so it will omit
# compiled executables and any intermidiate compiler artifacts, as long as the
# gitignore is properly set up.
fd --type file '^colortest\.' --exact-depth 2 --print0 |\
    xargs -0 tar "${tar_flags[@]}" -cf .files.tar \
        ./colortest_output \
        ./RUNNERS/install-deps.sh \
        ./RUNNERS/common.sh \
        ./RUNNERS/run-version.sh

# now build the test
podman pull docker.io/library/debian:latest
podman build --tag=colortester -f Containerfile .
