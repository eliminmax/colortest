#!/bin/sh

cd "$(dirname "$(realpath "$0")")" || exit 4

mkdir -p results
date +%s >results/start-time

podman run --rm -a stdout -a stderr colortester:latest \
    sh -c 'RUNNERS/install-deps.sh && RUNNERS/run-version.sh -a' \
    >results/FULL_RUN.stdout 2>results/FULL_RUN.stderr &

list_colortest_implementations () {
    old_pwd="$PWD"
    cd ..
    fd -tf '^colortest\.' --exact-depth 2 \
        --strip-cwd-prefix \
        --exclude '*.license' \
        -x dirname | sort
    cd "$old_pwd"
}

for i in $(list_colortest_implementations); do
    podman run --rm -a stdout -a stderr colortester:latest \
        sh -c "RUNNERS/install-deps.sh $i && RUNNERS/run-version.sh -t $i" \
        >"results/$i.stdout" 2>"results/$i.stderr" &
done

wait
date +%s >results/finish-time
