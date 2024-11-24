#!/bin/sh

# SPDX-FileCopyrightText: 2024 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only

cd "$(dirname "$(realpath "$0")")" || exit 4

podman_img=localhost/colortester:latest
if podman image exists "$podman_img"; then
    # if container is older than most recently changed file, rebuild it
    # shellcheck disable=1083 # curly braces are intentionally literal
    container_time="$(podman image list "$podman_img" --format {{.CreatedAt}})"
    # date doesn't understand the format output, and messing with LC_ALL
    # doesn't seem to change it. Cut off the time zone field, and it will work.
    container_ts="$(date -d"${container_time% UTC}" +%s)"
    
    # stat -c%W prints only UNIX timestamp, sort -un sorts numerically,
    # removing duplicates, and tail -n1 grabs the last line
    latest_ts="$(
        fd --exclude=.gitignore --exclude=.test_assets\
            --type file . .. -X stat -c%W | sort -un | tail -n1
    )"
    if [ "$container_ts" -lt "$latest_ts" ]; then
        ./build_test_container.sh
    fi
else
    # rebuild the container if it doesn't exist at all
    ./build_test_container.sh
fi


mkdir -p results
date +%s >results/start-time

podman run --name 'colortester_FULL_RUN' \
    --rm -a stdout -a stderr "$podman_img" \
    sh -c 'RUNNERS/install-deps.sh && RUNNERS/run-version.sh -a' \
    >results/FULL_RUN.stdout 2>results/FULL_RUN.stderr &

test_single () {
    podman run --name "colortester_$1" \
        --rm -a stdout -a stderr "$podman_img" \
        sh -c "RUNNERS/install-deps.sh $1 && RUNNERS/run-version.sh -t $1" \
        >"results/$1.stdout" 2>"results/$1.stderr" &
}

list_colortest_implementations () (
    cd ..
    fd -tf '^colortest\.' --exact-depth 2 \
        --strip-cwd-prefix \
        --exclude '*.license' \
        -x dirname | sort
)

if [ "$#" -gt 0 ]; then
    if ! { [ "$#" -eq 1 ] && [ "$1" = "FULL_RUN" ]; }; then
        for i in "$@"; do test_single "$i"; done
    fi
else
    for i in $(list_colortest_implementations); do test_single "$i"; done
fi

wait # without any arguments, waits for all background processes to finish
date +%s >results/finish-time
