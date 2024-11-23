#!/bin/sh

# SPDX-FileCopyrightText: 2024 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only

cd "$(dirname "$(realpath "$0")")" || exit 4

podman_img=localhost/colortester:latest
if podman image exists "$podman_img"; then
    container_time="$(podman image list $podman_img --format {{.CreatedAt}})"
    # date doesn't understand the format output, and messing with LC_ALL
    # doesn't seem to change it. Cut off the time zone field, and it will work.
    container_ts="$(date -d"${container_time% UTC}" +%s)"
    # if container is older than latest commit, rebuild it
    if [ "$container_ts" -lt "$(git log -1 --format=%at)" ]; then
        ./build_test_container.sh
    fi
else
    # rebuild the container if it doesn't exist at all
    ./build_test_container.sh
fi


mkdir -p results
date +%s >results/start-time

podman run --rm -a stdout -a stderr "$podman_img" \
    sh -c 'RUNNERS/install-deps.sh && RUNNERS/run-version.sh -a' \
    >results/FULL_RUN.stdout 2>results/FULL_RUN.stderr &

test_single () {
    podman run --rm -a stdout -a stderr "$podman_img" \
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
    for i in "$@"; do test_single "$i"; done
else
    for i in $(list_colortest_implementations); do test_single "$i"; done
fi

wait # without any arguments, waits for all background processes to finish
date +%s >results/finish-time
