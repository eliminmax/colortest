<!--
SPDX-FileCopyrightText: 2024 Eli Array Minkoff

SPDX-License-Identifier: CC0-1.0
-->

# How to run each implementation

These instructions assume that you are on an amd64 Debian 12 "Bookworm" system.

They can be adapted to other systems and platforms if you know what you're doing.

Each one of these was tested on a fresh Debian GNU/Linux Bookworm podman container, made from the following Dockerfile:

```dockerfile
FROM debian:latest

RUN apt-get update && \
    apt-get install -y git && \
    apt-get clean && \
    git clone https://github.com/eliminmax/colortest.git /colortest

ENTRYPOINT /bin/bash
```

<!-- vim-markdown-toc GFM -->

* [Common Steps](#common-steps)
* [Algol 68](#algol-68)
* [AWK](#awk)
* [Babalang](#babalang)
* [Befunge](#befunge)
* [Brainfuck](#brainfuck)
* [C](#c)
* [C](#c-1)
* [C++](#c-2)
* [Cobol](#cobol)
* [D](#d)
* [Erlang](#erlang)
* [Fender](#fender)
* [Forth](#forth)
* [Fortran](#fortran)
* [Go](#go)
* [Haskell](#haskell)
* [Java](#java)
* [JavaScript](#javascript)
* [Kotlin](#kotlin)
* [Lisp](#lisp)
* [Lua](#lua)
* [Nim](#nim)
* [Objecive-C](#objecive-c)
* [Odin](#odin)
* [OCaml](#ocaml)
* [Octave](#octave)
* [Pascal](#pascal)
* [Perl](#perl)
* [PHP](#php)
* [PowerShell](#powershell)
* [Python](#python)
* [R](#r)
* [Rockstar](#rockstar)
* [Ruby](#ruby)
* [Rust](#rust)
* [Scala](#scala)
* [Shell Script](#shell-script)
* [TypeScript](#typescript)
* [Vala](#vala)
* [x86_64 assembly](#x86_64-assembly)
* [Zig](#zig)

<!-- vim-markdown-toc -->

## Common Steps

Before anything language-specific, install `git` and clone the repo, then `cd` into it:

```bash
apt update
apt install -y git
git clone https://github.com/eliminmax/colortest.git
cd colortest
```

## Algol 68

```bash
# install dependencies
apt install -y algol68g
# switch to directory
cd algol68
# run the code
a68g colortest.a68
```

## AWK

```bash
# awk is already installed
# switch to directory
cd awk
# run the code
awk -f colortest.awk
```

## Babalang

```bash
# install dependencies
apt install -y curl gcc
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
source "$HOME/.cargo/env"
cargo install --git https://github.com/RocketRace/babalang
# switch to directory
cd babalang
# run the code
babalang colortest.baba
```

## Befunge

```sh
apt install -y wget cmake
dldir="$(mktemp -d)"
pushd "$dldir"
wget 'https://github.com/VorpalBlade/cfunge/archive/refs/tags/1,001.tar.gz'
tar xzf '1,001.tar.gz'
mkdir build
cd build
cmake ../cfunge-1-001/
make
make install
popd
rm -rf "$dldir"
cd /colortest/befunge
cfunge colortest.be
```

## Brainfuck

```bash
# install dependencies
apt install -y beef
# switch to directory
cd bf
# run the code
beef colortest.bf
```

## C

```bash
# install dependencies
apt install -y gcc
# switch to directory
cd c
# compile the code
cc colortest.c -o colortest
# run the code
./colortest
```

## C#

```bash
# install dependencies
apt install -y mono-mcs
# switch to directory
cd csharp
# compile the code
mcs colortest.cs
# run the code
cli colortest.exe
```

## C++

```bash
# install dependencies
apt install -y g++
# switch to directory
cd cpp
# compile the code
g++ colortest.cpp -o colortest
# run the code
./colortest
```

## Cobol

```bash
# install dependencies
apt install -y gnucobol
# switch to directory
cd cobol
# compile the code
cobc -x colortest.cbl
# run the code
./colortest
```

## D

```bash
# install dependencies
apt install -y ldc gcc
# switch to directory
cd d
# compile the code
ldc2 colortest.d
# run the code
./colortest
```

## Erlang

```bash
# install dependencies
apt install -y erlang-base
# switch to directory
cd erlang
# run the code
escript colortest
```

## Fender

```bash
# install dependencies
apt install -y curl gcc
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
source "$HOME/.cargo/env"
cargo install --git https://github.com/FenderLang/Fender
# switch to directory
cd fender
# run the code
fender colortest.fndr
```

## Forth

```bash
# install dependencies
apt install -y gforth
# switch to directory
cd forth
# run the code
gforth colortest.fth -e bye
```

## Fortran

```bash
# install dependencies
apt install -y gfortran
# switch to directory
cd fortran
# compile the code
gfortran colortest.f90 -o colortest
# run the code
./colortest
```

## Go

```bash
# install dependencies
apt install -y gccgo
# switch to directory
cd go
# compile the code
gccgo colortest.go -o colortest
# run the code
./colortest
```

## Haskell

```bash
# install dependencies
apt install -y ghc
# switch to directory
cd haskell
# compile the code
ghc colortest.hs
# run the code
./colortest
```

## Java

```bash
# install dependencies
apt install -y openjdk-17-jdk-headless
# switch to directory
cd java
# compile the code
javac colortest.java
# run the code
java colortest
```

## JavaScript

```bash
# install dependencies
apt install -y nodejs
# switch to directory
cd javascript
# run the code
node colortest.js
```

## Kotlin

```bash
# install dependencies
apt install -y kotlin
# switch to directory
cd kotlin
# compile the code
kotlinc colortest.kt -include-runtime -d colortest.jar
# run the code
kotlin colortest.jar
```

## Lisp

```bash
# install dependencies
apt install -y clisp
# switch to directory
cd lisp
# run the code
clisp colortest.lisp
```

## Lua

```bash
# install dependencies
apt install -y lua5.2
# switch to directory
cd lua
# run the code
lua colortest.lua
```

## Nim

```bash
# install dependencies
apt install -y nim
# switch to directory
cd nim
# compile the code
nim c colortest.nim
# run the code
./colortest
```

## Objecive-C

```bash
# install dependencies
apt install -y gobjc
# switch to directory
cd objective-c
# compile the code
gcc colortest.m -o colortest
# run the code
./colortest
```

## Odin

```bash
# install dependencies
apt install -y wget llvm clang
# download and extract Odin dev-2024-01
cd $(mktemp -d)
wget https://github.com/odin-lang/Odin/archive/refs/tags/dev-2024-01.tar.gz -O Odin-dev-2024-01.tgz
tar xf Odin-dev-2024-01.tgz
mv Odin-dev-2024-01 /opt/odin
cd /opt/odin
# build odin
./build_odin.sh
# switch to directory
cd odin
# compile the code
/opt/odin/odin build colortest.odin -file
# run the code
./colortest
```

## OCaml

```bash
# install dependencies
apt install -y ocaml
# switch to directory
cd ocaml
# compile the code
ocamlc colortest.ml -o colortest
# run the code
./colortest
```

## Octave

```bash
# install dependencies
apt install -y octave --no-install-recommends
# switch to directory
cd octave
# run the code
octave colortest.m 2>/dev/null
```

## Pascal

```bash
# install dependencies
apt install -y fp-compiler-3.2.2
# switch to directory
cd pascal
# compile the code
fpc colortest.pas
# run the code
./colortest
```

## Perl

```bash
# perl is already installed
# switch to directory
cd perl
# run the code
perl colortest.pl
```

## PHP

```bash
# install dependencies
apt install -y php-cli
# switch to directory
cd php
# run the code
php colortest.php
```

## PowerShell

```bash
# install dependencies
apt install -y wget
wget -q https://packages.microsoft.com/config/debian/11/packages-microsoft-prod.deb
dpkg -i packages-microsoft-prod.deb
apt update
apt install powershell
# switch to directory
cd powershell
# run the code
pwsh colortest.ps1
```

## Python

```bash
# install dependencies
apt install -y python3
# switch to directory
cd python
# run the code
python3 colortest.py
```

## R

```bash
# install dependencies
apt install -y r-cran-littler
# switch to directory
cd r
# run the code
r colortest.r
```

## Rockstar

*Note: this one's messy. Really messy.*

```bash
# install dependencies
apt install -y nodejs npm
git clone https://github.com/RockstarLang/rockstar /rockstar
cd /rockstar
# switch to a commit that's known to work with this hackery
git checkout 3b9aab47485e6745d3bb76d9e7f13152e72c3973
# disable the message that "program returned no output"
sed -i '/program returned no output/s/^/\/\//' /rockstar/satriani/rockstar.js
cd satriani
npm install -g yarn
yarn install
yarn pegjs
node rockstar /colortest/rockstar/colortest.rock
```

## Ruby

```bash
# install dependencies
apt install -y ruby
# switch to directory
cd ruby
# run the code
ruby colortest.rb
```

## Rust

```bash
# install dependencies
apt install -y rustc
# switch to directory
cd rust
# compile the code
rustc colortest.rs
# run the code
./colortest
```

## Scala

```bash
# install dependencies
apt install -y scala
# switch to directory
cd scala
# compile the code
scalac colortest.scala
# run the code
scala colortest
```

## Shell Script

```bash
# at least 2 different POSIX shells are already installed
# switch to directory
cd sh
# run the code
sh colortest.sh
```

## TypeScript

```bash
# install dependencies
apt install -y ts-node
# switch to directory
cd typescript
# run the code
ts-node colortest.js
```

## Vala

```bash
# install dependencies
apt install -y valac
# switch to directory
cd vala
# compile the code
valac colortest.vala
# run the code
colortest
```

## x86_64 assembly

```bash
# install dependencies
apt install -y binutils nasm
# switch to directory
cd x86-64_linux_asm
# compile the code
nasm -felf64 colortest.asm
# link the code
ld colortest.o -o colortest
# run the code on amd64 systems
./colortest
```

## Zig

```bash
# install dependencies
apt install -y xz-utils wget
pushd /opt
wget https://ziglang.org/download/0.11.0/zig-linux-x86_64-0.11.0.tar.xz
tar xf zig-linux-x86_64-0
PATH="$PATH:/opt/zig-linux-x86_64-0.11.0"
# switch to directory
popd
cd zig
# compile the code
zig build-exe colortest.zig
# run the code
./colortest
```
