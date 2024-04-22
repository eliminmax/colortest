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


# Utility function to compile an implementation if a colortest binary is not
# present. It runs its arguments as a command if a file called `colortest`
# with no extension is not present, supressing compiler output to stdout.
# either way, it then runs it
comp_check() {
    if [ ! -e colortest ]; then "$@" >/dev/null; fi
    ./colortest
}

run_version() {
    pushd "$1" &>/dev/null
    case "$1" in
        # scripting languages - these need to be passed to an interpreter
        'algol_68')   a68g colortest.a68              ;;
        'awk')        awk -f colortest.awk            ;;
        'babalang')   babalang colortest.baba         ;;
        'befunge')    cfunge colortest.befunge        ;;
        'bf')         beef colortest.bf               ;;
        'erlang')     escript colortest.erl           ;;
        'fender')     fender colortest.fndr           ;;
        'forth')      gforth colortest.fth -e bye     ;;
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
        'sh')         sh colortest.sh                 ;;
        'typescript') ts-node colortest.ts            ;;
        # GNU Octave throws an error on exit if XDG_DATA_HOME is missing
        # without -q, it throws a warning if the DISPLAY variable is unset
        'octave')
            mkdir -p "${XDG_DATA_HOME-$HOME/.local/share}"
            octave -q colortest.m
        ;;

        # for the remaining ones, first check if they've been compiled.
        # if not, compile them, then either way, run them.
        # for most of them, the comp_check function does that automatically
        # for the rest (mainly the CLR and JVM languages), they are handled
        # manually at the end
        'c')            comp_check cc colortest.c -o colortest              ;;
        'cobol')        comp_check cobc -x colortest.cbl                    ;;
        'cpp')          comp_check c++ colortest.cpp -o colortest           ;;
        'd')            comp_check ldc2 colortest.d                         ;;
        'fortran')      comp_check gfortran colortest.f90 -o colortest      ;;
        'go')           comp_check gccgo colortest.go -o colortest          ;;
        'haskell')      comp_check ghc colortest.hs                         ;;
        'nim')          comp_check nim c colortest.nim                      ;;
        'objective-c')  comp_check gcc colortest.m -o colortest             ;;
        'ocaml')        comp_check ocamlc colortest.ml -o colortest         ;;
        'odin')         comp_check odin-wrapper build colortest.odin -file  ;;
        'pascal')       comp_check fpc colortest.pas                        ;;
        'rust')         comp_check rustc colortest.rs                       ;;
        'vala')         comp_check valac colortest.vala                     ;;
        'zig')          comp_check zig build-exe colortest.zig              ;;
        # the remaining are different enough not to be able to use comp_check
        'csharp')
            if ! [ -f colortest.exe ]; then mcs colortest.cs; fi
            cli ./colortest.exe
        ;;
        'java') 
            if ! [ -e colortest.class ]; then javac colortest.java; fi
            java colortest
        ;;
        'kotlin')
            if ! [ -e ColortestKt.class ]; then kotlinc colortest.kt; fi
            kotlin ColortestKt
        ;;
        'scala') 
            if ! [ -e colortest.class ]; then scalac colortest.scala; fi
            scala colortest
        ;;
        # this one could technically use comp_check with eval, but this is a
        # cleaner approach in my opinion.
        'x86-64_linux_asm')
            if ! [ -e colortest ]; then
                nasm -f elf64 -o colortest.o colortest.asm
                ld colortest.o -o colortest
            fi
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
        printf 'Testing \e[1;32m%s\e[m...\n' "$lang"
        ((tests+=1))
        if ! { run_version "$lang" | diff --brief - colortest_output; }; then
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
            printf '########### \e[1;32m%s\e[m ###########\n' "$lang"
            run_version "$lang"
        done
    fi
fi
