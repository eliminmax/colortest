#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2024 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only

# this script contains functions to compile each version

set -eo pipefail


# the reason for the order of the below few commands is that shellcheck doesn't
# understand that RUNNERS/common.sh sourced from the colortest directory and
# common.sh sourced from the colortest/RUNNERS directory are one and the same.

# cd to colortest/RUNNERS directory to source common.sh, then go to colortest
cd "$(dirname "$(realpath "$0")")"
source common.sh
cd ..

run_version() {
    pushd "$1" &>/dev/null
    case "$1" in
        # scripting languages - these need to be passed to an interpreter
        'algol_68')   a68g colortest.a68              ;;
        'awk')        awk -f colortest.awk            ;;
        'babalang')   babalang colortest.baba         ;;
        'befunge')    cfunge colortest.bf             ;;
        'bf')         beef colortest.bf               ;;
        'dc')         dc colortest.dc                 ;;
        'elixir')     elixir colortest.exs            ;;
        'erlang')     escript colortest.erl           ;;
        'fender')     fender colortest.fndr           ;;
        'forth')      gforth colortest.fth -e bye     ;;
        'jq')         jq -njf colortest.jq            ;;
        'javascript') node colortest.js               ;;
        'lisp')       clisp colortest.lisp            ;;
        'lua')        lua colortest.lua               ;;
        'perl')       perl colortest.pl               ;;
        'php')        php colortest.php               ;;
        'powershell') pwsh colortest.ps1              ;;
        'python')     python3 colortest.py            ;;
        'r')          r colortest.r                   ;;
        'rockstar')   satriani-wrapper colortest.rock ;;
        'ruby')       ruby colortest.rb               ;;
        'scheme')     csi -script colortest.sld       ;;
        'sh')         sh colortest.sh                 ;;
        'typescript') ts-node colortest.ts            ;;
        'wasm')       wasmtime colortest.wat          ;;
        # GNU Octave throws an error on exit if XDG_DATA_HOME is missing
        # without -q, it throws a warning if the DISPLAY variable is unset
        'octave')
            mkdir -p "${XDG_DATA_HOME-$HOME/.local/share}"
            octave -q colortest.m
        ;;

        # for the remaining ones, first compile them silently, then run them
        'c')
            cc colortest.c -o colortest &>/dev/null && \
            ./colortest
        ;;
        'cobol')
            cobc -x colortest.cbl &>/dev/null && \
            ./colortest
        ;;
        'cpp')
            c++ colortest.cpp -o colortest &>/dev/null && \
            ./colortest
        ;;
        'csharp')
            mcs colortest.cs &>/dev/null && \
            cli colortest.exe 
        ;;
        'd')
            ldc2 colortest.d &>/dev/null && \
            ./colortest 
        ;;
        'fortran')
            gfortran colortest.f90 -o colortest &>/dev/null && \
            ./colortest 
        ;;
        'go')
            gccgo colortest.go -o colortest &>/dev/null && \
            ./colortest 
        ;;
        'haskell')
            ghc colortest.hs &>/dev/null && \
            ./colortest 
        ;;
        'java')
            javac colortest.java &>/dev/null && \
            java colortest 
        ;;
        'kotlin')
            kotlinc colortest.kt &>/dev/null && \
            kotlin ColortestKt 
        ;;
        'nim')
            nim c colortest.nim &>/dev/null && \
            ./colortest 
        ;;
        'objective-c')
            # shellcheck disable=2046 # Word splitting is required here
            gcc $(gnustep-config --objc-flags) \
                colortest.m -o colortest       \
                $(gnustep-config --base-libs) &>/dev/null && \
            ./colortest 
        ;;
        'ocaml')
            ocamlc colortest.ml -o colortest &>/dev/null && \
            ./colortest 
        ;;
        'odin')
            odin build colortest.odin -file &>/dev/null && \
            ./colortest 
        ;;
        'pascal')
            fpc colortest.pas &>/dev/null && \
            ./colortest 
        ;;
        'rust')
            rustc colortest.rs &>/dev/null && \
            ./colortest 
        ;;
        'scala')
            scalac colortest.scala &>/dev/null && \
            scala colortest 
        ;;
        'vala')
            valac colortest.vala &>/dev/null && \
            ./colortest 
        ;;
        'x86-64_linux_asm')
			nasm -f elf64 -o colortest.o colortest.asm
			ld colortest.o -o colortest
            ./colortest
        ;;
        'zig')
            zig build-exe colortest.zig &>/dev/null && \
            ./colortest 
        ;;
        *) printf "Unrecognized implementation: '%s'.\n" "$1" >&2; return 1 ;;
    esac
    popd &>/dev/null
}

test_implementation() {
    apt_wrapper diff diffutils
    local fails
    local tests
    fails=0
    tests=0
    for lang in "$@"; do
        printf 'Testing \e[1;32m%s\e[m...\n' "$lang" >&2
        ((tests+=1))
        if ! { run_version "$lang" |  diff --brief - colortest_output; }; then
            printf '\e[1;31m%s\e[m failed!\n' "$lang" >&2
            ((fails+=1))
        fi
    done
    if [ "$fails" -gt 0 ]; then
        printf '%d tests failed out of %d total.\n' "$fails" "$tests" >&2
        return 1
    else
        printf 'All %d tests passed!\n' "$tests" >&2
        return 0
    fi
}

# if it's a single argument, then unless it's a flag, assume it's
# a colortest implementation and try to run it.
if [[ "$#" -eq 1 ]]; then
    case "$1" in
        -[at]|--test-all|--test)
            test_implementation "${colortest_implementations[@]}"; exit ;;
        -*) printf 'Unrecognized option: %s\n' "$1" >&2 ; exit 2 ;;
        *) run_version "$1" ;;
    esac
else
    declare -a filtered_args
    to_test=0
    while [ "$#" -gt 0 ]; do
        case "$1" in
            -t|--test) to_test=1; shift ;;
            -a|--test-all)
                test_implementation "${colortest_implementations[@]}"; exit ;;
            -*) printf 'Unrecognized option: %s\n' "$1" >&2 ; exit 2 ;;
            *) filtered_args+=("$1"); shift ;;
        esac
    done
    if [ "$to_test" -eq 1 ]; then
        test_implementation "${filtered_args[@]}"
    else
        for lang in "${filtered_args[@]}"; do
            printf '########### \e[1;32m%s\e[m ###########\n' >&2 "$lang"
            run_version "$lang"
        done
    fi
fi
