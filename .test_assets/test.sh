#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2024 - 2025 Eli Array Minkoff
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


(
    podman run --name 'colortester_FULL_RUN' \
        --rm -a stdout -a stderr "$podman_img" \
        sh -c 'RUNNERS/install-deps.sh && RUNNERS/run-version.sh -a' \
        >results/FULL_RUN.stdout 2>results/FULL_RUN.stderr
    echo "$?" > results/FULL_RUN.status
) &

run_test () {
    local install_cmd
    local run_cmd
    install_cmd="RUNNERS/install-deps.sh $1"
    run_cmd="RUNNERS/run-version.sh -t $1"
    # remove trailing " FULL_RUN" from locals if present
    install_cmd="${install_cmd% FULL_RUN}"
    run_cmd="${run_cmd% FULL_RUN}"

    podman run --name "colortester-$1" --rm \
        -a stdout -a stderr "$podman_img" \
        sh -c "$install_cmd && $run_cmd" \
        >"results/$1.stdout" \
        2>"results/$1.stderr"
    result="$?"
    echo "$result" >"results/$1.status"
    printf '>> completed %s with exit code %d\n' "$1" "$result" >&2
}

list_colortest_implementations () (
    cd ..
    fd -tf '^colortest\.' --exact-depth 2 \
        --strip-cwd-prefix \
        --exclude '*.license' \
        -x dirname | sort
)

run_tests() {
    # run all tests in the background
    for test in "$@"; do run_test "$test" & done
    # wait for all tests to finish
    wait

    local -i failures=0
    local -i successes=0
    local -i result
    for test in "$@"; do
        result="$(cat "results/$test.status")"
        if [ "$result" -ne 0 ]; then
            failures+=1
            printf '> %s failed with code %d.\n' "$test" "$result"
        else
            successes+=1
            printf '> %s succeeded.\n' "$test"
        fi
    done
    printf '%d/%d runs passed.\n' "$successes" "$#"
    [ "$failures" -eq 0 ]
}

mkdir -p results
# clean up leftovers from previous runs
find results -type f -exec rm '{}' +

date +%s >results/start-time
if [ "$#" -gt 0 ]; then
    run_tests "$@"
else
    # shellcheck disable=2046 # word splitting is intended
    run_tests FULL_RUN $(list_colortest_implementations)
fi
date +%s >results/finish-time
