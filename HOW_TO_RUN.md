<!--
SPDX-FileCopyrightText: 2024 Eli Array Minkoff

SPDX-License-Identifier: CC0-1.0
-->

# How to run each implementation

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

```sh
apt update
apt install -y git
git clone https://github.com/eliminmax/colortest.git /colortest
```

## Algol 68

```sh
# install dependencies
apt install -y algol68g
# switch to directory
cd /colortest/algol68
# run the code
a68g colortest.a68
```

## AWK

```sh
# awk is already installed
# switch to directory
cd /colortest/awk
# run the code
awk -f colortest.awk
```

## Babalang

```sh
# install dependencies
apt install -y curl gcc
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
source /root/.cargo/env
cargo install --git https://github.com/RocketRace/babalang
# switch to directory
cd /colortest/babalang
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

```sh
# install dependencies
apt install -y beef
# switch to directory
cd /colortest/bf
# run the code
beef colortest.bf
```

## C

```sh
# install dependencies
apt install -y gcc
# switch to directory
cd /colortest/c
# compile the code
cc colortest.c -o colortest
# run the code
./colortest
```

## C#

```sh
# install dependencies
apt install -y mono-mcs
# switch to directory
cd /colortest/csharp
# compile the code
mcs colortest.cs
# run the code
cli colortest.exe
```

## C++

```sh
# install dependencies
apt install -y g++
# switch to directory
cd /colortest/cpp
# compile the code
g++ colortest.cpp -o colortest
# run the code
./colortest
```

## Cobol

```sh
# install dependencies
apt install -y gnucobol
# switch to directory
cd /colortest/cobol
# compile the code
cobc -x colortest.cbl
# run the code
./colortest
```

## D

```sh
# install dependencies
apt install -y ldc gcc
# switch to directory
cd /colortest/d
# compile the code
ldc2 colortest.d
# run the code
./colortest
```

## Erlang

```sh
# install dependencies
apt install -y erlang-base
# switch to directory
cd /colortest/erlang
# run the code
escript colortest
```

## Fender

```sh
# install dependencies
apt install -y curl gcc
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
source /root/.cargo/env
cargo install --git https://github.com/FenderLang/Fender
# switch to directory
cd /colortest/fender
# run the code
fender colortest.fndr
```

## Forth

```sh
# install dependencies
apt install -y gforth
# switch to directory
cd /colortest/forth
# run the code
gforth colortest.fth -e bye
```

## Fortran

```sh
# install dependencies
apt install -y gfortran
# switch to directory
cd /colortest/fortran
# compile the code
gfortran colortest.f90 -o colortest
# run the code
./colortest
```

## Go

```sh
# install dependencies
apt install -y gccgo
# switch to directory
cd /colortest/go
# compile the code
gccgo colortest.go -o colortest
# run the code
./colortest
```

## Haskell

```sh
# install dependencies
apt install -y ghc
# switch to directory
cd /colortest/haskell
# compile the code
ghc colortest.hs
# run the code
./colortest
```

## Java

```sh
# install dependencies
apt install -y openjdk-17-jdk-headless
# switch to directory
cd /colortest/java
# compile the code
javac colortest.java
# run the code
java colortest
```

## JavaScript

```sh
# install dependencies
apt install -y nodejs
# switch to directory
cd /colortest/javascript
# run the code
node colortest.js
```

## Kotlin

```sh
# install dependencies
apt install -y kotlin
# switch to directory
cd /colortest/kotlin
# compile the code
kotlinc colortest.kt
# run the code
kotlin ColortestKt
```

## Lisp

```sh
# install dependencies
apt install -y clisp
# switch to directory
cd /colortest/lisp
# run the code
clisp colortest.lisp
```

## Lua

```sh
# install dependencies
apt install -y lua5.2
# switch to directory
cd /colortest/lua
# run the code
lua colortest.lua
```

## Nim

```sh
# install dependencies
apt install -y nim
# switch to directory
cd /colortest/nim
# compile the code
nim c colortest.nim
# run the code
./colortest
```

## Objecive-C

```sh
# install dependencies
apt install -y gobjc
# switch to directory
cd /colortest/objective-c
# compile the code
gcc colortest.m -o colortest
# run the code
./colortest
```

## Odin

```sh
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
cd /colortest/odin
# compile the code
/opt/odin/odin build colortest.odin -file
# run the code
./colortest
```

## OCaml

```sh
# install dependencies
apt install -y ocaml
# switch to directory
cd /colortest/ocaml
# compile the code
ocamlc colortest.ml -o colortest
# run the code
./colortest
```

## Octave

```sh
# install dependencies
apt install -y octave --no-install-recommends
# switch to directory
cd /colortest/octave
# run the code
octave colortest.m 2>/dev/null
```

## Pascal

```sh
# install dependencies
apt install -y fp-compiler-3.2.2
# switch to directory
cd /colortest/pascal
# compile the code
fpc colortest.pas
# run the code
./colortest
```

## Perl

```sh
# perl is already installed
# switch to directory
cd /colortest/perl
# run the code
perl colortest.pl
```

## PHP

```sh
# install dependencies
apt install -y php-cli
# switch to directory
cd /colortest/php
# run the code
php colortest.php
```

## PowerShell

```sh
# install dependencies
apt install -y wget
wget -q https://packages.microsoft.com/config/debian/11/packages-microsoft-prod.deb
dpkg -i packages-microsoft-prod.deb
apt update
apt install powershell
# switch to directory
cd /colortest/powershell
# run the code
pwsh colortest.ps1
```

## Python

```sh
# install dependencies
apt install -y python3
# switch to directory
cd /colortest/python
# run the code
python3 colortest.py
```

## R

```sh
# install dependencies
apt install -y r-cran-littler
# switch to directory
cd /colortest/r
# run the code
r colortest.r
```

## Rockstar

*Note: this one's messy. Really messy.*

```sh
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

```sh
# install dependencies
apt install -y ruby
# switch to directory
cd /colortest/ruby
# run the code
ruby colortest.rb
```

## Rust

```sh
# install dependencies
apt install -y rustc
# switch to directory
cd /colortest/rust
# compile the code
rustc colortest.rs
# run the code
./colortest
```

## Scala

```sh
# install dependencies
apt install -y scala
# switch to directory
cd /colortest/scala
# compile the code
scalac colortest.scala
# run the code
scala colortest
```

## Shell Script

```sh
# at least 2 different POSIX shells are already installed
# switch to directory
cd /colortest/sh
# run the code
sh colortest.sh
```

## TypeScript

```sh
# install dependencies
apt install -y ts-node
# switch to directory
cd /colortest/typescript
# run the code
ts-node colortest.js
```

## Vala

```sh
# install dependencies
apt install -y valac
# switch to directory
cd /colortest/vala
# compile the code
valac colortest.vala
# run the code
colortest
```
## x86_64 assembly

```sh
# install dependencies
apt install -y binutils nasm
# switch to directory
cd /colortest/x86-64_linux_asm
# compile the code
nasm -felf64 colortest.scala
# link the code
ld colortest.o -o colortest
# run the code
./colortest
```

## Zig

```sh
# install dependencies
apt install -y xz-utils wget
cd opt
wget https://ziglang.org/download/0.11.0/zig-linux-x86_64-0.11.0.tar.xz
tar xf zig-linux-x86_64-0
PATH=$PATH:/opt/zig-linux-x86_64-0.11.0
# switch to directory
cd /colortest/zig
# compile the code
zig build-exe colortest.zig
# run the code
./colortest
```
