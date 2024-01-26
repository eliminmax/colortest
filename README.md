# Colortest

A series of tiny, non-interactive programs written in various languages that all
output a specific, strictly-defined pattern of ANSI escape codes and
whitespace to create a 256-color test pattern, and generate it following a specific procedure.

Each program is entirely my original work unless otherwise noted. My understanding and comfort with the languages I've used varies greatly.

## Languages

At the current moment, I've implemented it in the following languages:

* AWK
* Algol 68
* Babalang (an esoteric programming language inspired by the puzzle game Baba is you. The specification is [here](https://esolangs.org/wiki/Babalang) and the official interpreter is available [here](https://github.com/RocketRace/babalang)
* Brainfuck
* C
* C#
* C++
* Cobol
* D
* Erlang (via Escript)
* Fender (a language created by 3 of my fellow Champlain College students in my senior year, available [here](https://github.com/FenderLang/Fender))
* Forth
* Fortran
* Go
* Haskell
* Java
* JavaScript (via Node.js)
* Kotlin
* Lisp (Common Lisp)
* Lua
* Nim
* Objecive-C *(lightly tweaked C implementation)*
* Odin
* OCaml
* Octave (the GNU project's MATLAB-like analytical language)
* Pascal (via FreePascal) ([in memory of Niklaus Wirth](https://www.theregister.com/2024/01/04/niklaus_wirth_obituary/))
* Perl
* PHP
* PowerShell
* Python
* R
* Rockstar (an esoteric programming language based on 80s metal and power ballads. Specification and reference implementation are [here](https://github.com/RockstarLang/rockstar))
* Ruby
* Rust
* Scala
* Shell Script (should work in any POSIX shell)
* TypeScript *(lightly tweaked JavaScript implementation)* (via ts-node)
* Vala
* x86_64 assembly (NASM Intel-style syntax, 64-bit Linux System calls)
* Zig

For instructions on how to run any specific implementation on Debian GNU/Linux 12 (Bookworm), see HOW_TO_RUN.md. All languages have been run in a Debian 12 Podman container with the instructions in that document.

I am experienced with some of those languages, and others I learned barely enough to implement this program as I was working on them.

## Questions and Answers

### Why?

Why not? It's what I find fun, and [it's not the first time](https://github.com/eliminmax/eli-bash-colors) I've messed with ANSI escape sequences in the terminal.

It also gives me a sense of how different languages are designed.

## The details

The output of the program should look like this:
![Screenshot of Colortest output](https://github.com/eliminmax/colortest/blob/main/colortest_output.png)

The following is the pattern itself, with the ASCII escape character
replaced by '␛':

```text

␛[48;5;0m  ␛[48;5;1m  ␛[48;5;2m  ␛[48;5;3m  ␛[48;5;4m  ␛[48;5;5m  ␛[48;5;6m  ␛[48;5;7m  ␛[48;5;8m  ␛[48;5;9m  ␛[48;5;10m  ␛[48;5;11m  ␛[48;5;12m  ␛[48;5;13m  ␛[48;5;14m  ␛[48;5;15m  ␛[0m

␛[48;5;16m  ␛[48;5;17m  ␛[48;5;18m  ␛[48;5;19m  ␛[48;5;20m  ␛[48;5;21m  ␛[0m  ␛[48;5;52m  ␛[48;5;53m  ␛[48;5;54m  ␛[48;5;55m  ␛[48;5;56m  ␛[48;5;57m  ␛[0m  ␛[48;5;88m  ␛[48;5;89m  ␛[48;5;90m  ␛[48;5;91m  ␛[48;5;92m  ␛[48;5;93m  ␛[0m
␛[48;5;22m  ␛[48;5;23m  ␛[48;5;24m  ␛[48;5;25m  ␛[48;5;26m  ␛[48;5;27m  ␛[0m  ␛[48;5;58m  ␛[48;5;59m  ␛[48;5;60m  ␛[48;5;61m  ␛[48;5;62m  ␛[48;5;63m  ␛[0m  ␛[48;5;94m  ␛[48;5;95m  ␛[48;5;96m  ␛[48;5;97m  ␛[48;5;98m  ␛[48;5;99m  ␛[0m
␛[48;5;28m  ␛[48;5;29m  ␛[48;5;30m  ␛[48;5;31m  ␛[48;5;32m  ␛[48;5;33m  ␛[0m  ␛[48;5;64m  ␛[48;5;65m  ␛[48;5;66m  ␛[48;5;67m  ␛[48;5;68m  ␛[48;5;69m  ␛[0m  ␛[48;5;100m  ␛[48;5;101m  ␛[48;5;102m  ␛[48;5;103m  ␛[48;5;104m  ␛[48;5;105m  ␛[0m
␛[48;5;34m  ␛[48;5;35m  ␛[48;5;36m  ␛[48;5;37m  ␛[48;5;38m  ␛[48;5;39m  ␛[0m  ␛[48;5;70m  ␛[48;5;71m  ␛[48;5;72m  ␛[48;5;73m  ␛[48;5;74m  ␛[48;5;75m  ␛[0m  ␛[48;5;106m  ␛[48;5;107m  ␛[48;5;108m  ␛[48;5;109m  ␛[48;5;110m  ␛[48;5;111m  ␛[0m
␛[48;5;40m  ␛[48;5;41m  ␛[48;5;42m  ␛[48;5;43m  ␛[48;5;44m  ␛[48;5;45m  ␛[0m  ␛[48;5;76m  ␛[48;5;77m  ␛[48;5;78m  ␛[48;5;79m  ␛[48;5;80m  ␛[48;5;81m  ␛[0m  ␛[48;5;112m  ␛[48;5;113m  ␛[48;5;114m  ␛[48;5;115m  ␛[48;5;116m  ␛[48;5;117m  ␛[0m
␛[48;5;46m  ␛[48;5;47m  ␛[48;5;48m  ␛[48;5;49m  ␛[48;5;50m  ␛[48;5;51m  ␛[0m  ␛[48;5;82m  ␛[48;5;83m  ␛[48;5;84m  ␛[48;5;85m  ␛[48;5;86m  ␛[48;5;87m  ␛[0m  ␛[48;5;118m  ␛[48;5;119m  ␛[48;5;120m  ␛[48;5;121m  ␛[48;5;122m  ␛[48;5;123m  ␛[0m

␛[48;5;124m  ␛[48;5;125m  ␛[48;5;126m  ␛[48;5;127m  ␛[48;5;128m  ␛[48;5;129m  ␛[0m  ␛[48;5;160m  ␛[48;5;161m  ␛[48;5;162m  ␛[48;5;163m  ␛[48;5;164m  ␛[48;5;165m  ␛[0m  ␛[48;5;196m  ␛[48;5;197m  ␛[48;5;198m  ␛[48;5;199m  ␛[48;5;200m  ␛[48;5;201m  ␛[0m
␛[48;5;130m  ␛[48;5;131m  ␛[48;5;132m  ␛[48;5;133m  ␛[48;5;134m  ␛[48;5;135m  ␛[0m  ␛[48;5;166m  ␛[48;5;167m  ␛[48;5;168m  ␛[48;5;169m  ␛[48;5;170m  ␛[48;5;171m  ␛[0m  ␛[48;5;202m  ␛[48;5;203m  ␛[48;5;204m  ␛[48;5;205m  ␛[48;5;206m  ␛[48;5;207m  ␛[0m
␛[48;5;136m  ␛[48;5;137m  ␛[48;5;138m  ␛[48;5;139m  ␛[48;5;140m  ␛[48;5;141m  ␛[0m  ␛[48;5;172m  ␛[48;5;173m  ␛[48;5;174m  ␛[48;5;175m  ␛[48;5;176m  ␛[48;5;177m  ␛[0m  ␛[48;5;208m  ␛[48;5;209m  ␛[48;5;210m  ␛[48;5;211m  ␛[48;5;212m  ␛[48;5;213m  ␛[0m
␛[48;5;142m  ␛[48;5;143m  ␛[48;5;144m  ␛[48;5;145m  ␛[48;5;146m  ␛[48;5;147m  ␛[0m  ␛[48;5;178m  ␛[48;5;179m  ␛[48;5;180m  ␛[48;5;181m  ␛[48;5;182m  ␛[48;5;183m  ␛[0m  ␛[48;5;214m  ␛[48;5;215m  ␛[48;5;216m  ␛[48;5;217m  ␛[48;5;218m  ␛[48;5;219m  ␛[0m
␛[48;5;148m  ␛[48;5;149m  ␛[48;5;150m  ␛[48;5;151m  ␛[48;5;152m  ␛[48;5;153m  ␛[0m  ␛[48;5;184m  ␛[48;5;185m  ␛[48;5;186m  ␛[48;5;187m  ␛[48;5;188m  ␛[48;5;189m  ␛[0m  ␛[48;5;220m  ␛[48;5;221m  ␛[48;5;222m  ␛[48;5;223m  ␛[48;5;224m  ␛[48;5;225m  ␛[0m
␛[48;5;154m  ␛[48;5;155m  ␛[48;5;156m  ␛[48;5;157m  ␛[48;5;158m  ␛[48;5;159m  ␛[0m  ␛[48;5;190m  ␛[48;5;191m  ␛[48;5;192m  ␛[48;5;193m  ␛[48;5;194m  ␛[48;5;195m  ␛[0m  ␛[48;5;226m  ␛[48;5;227m  ␛[48;5;228m  ␛[48;5;229m  ␛[48;5;230m  ␛[48;5;231m  ␛[0m

␛[48;5;232m  ␛[48;5;233m  ␛[48;5;234m  ␛[48;5;235m  ␛[48;5;236m  ␛[48;5;237m  ␛[48;5;238m  ␛[48;5;239m  ␛[48;5;240m  ␛[48;5;241m  ␛[48;5;242m  ␛[48;5;243m  ␛[48;5;244m  ␛[48;5;245m  ␛[48;5;246m  ␛[48;5;247m  ␛[48;5;248m  ␛[48;5;249m  ␛[48;5;250m  ␛[48;5;251m  ␛[48;5;252m  ␛[48;5;253m  ␛[48;5;254m  ␛[48;5;255m  ␛[0m

```

The MD5 checksum of the output should be `5ee6c8ad78719bc2a515fbee5957ba06`. *Note: I am aware that it is trivial to find MD5 hash collisions, and would not rely on MD5 for security under any circumstance. Given the assumption that it looks right, and I just want to validate that nothing is off about the whitespace, I think it should be fine.*

Each implementation consists of 3 parts, each of which begins with a specific comment and follows the same general structure across implementations, though there is some flexibility depending on the specifics of the language.

*Note that in the following pseudocode,* `print` *is assumed not to add a trailing newline,* `for` *loops work the way they do in C, and within strings,* `\e` *represents the ASCII escape character and* `\n` *represents the ASCII newline character.*

Part 1 begins with the following comment:

> Print the first 16 colors - these vary by terminal configuration

The general structure of part 1 can be expressed with the following pseudocode:

```
print("\n")
for (i=0; i < 16; i++) {
  print("\e[48;5;{i}m  ")
}
print("\e[0m\n\n")
```

Part 2 begins with the following comment:

> Print the 6 sides of the color cube - these are more standardized,
> but the order is a bit odd, thus the need for this trickery

The general structure of part 2 can be expressed with the following pseudocode:

```
for (i=16; i < 52; i+=6) {
  for (ii=0; ii < 6; ii++) {
    print("\e[48;5;{i+ii}m  ")
  }
  print("\e[0m  ")
  for (ii=36; ii < 42; ii++) {
    print("\e[48;5;{i+ii}m  ")
  }
  print("\e[0m  ")
  for (ii=72; ii < 78; ii++) {
    print("\e[48;5;{i+ii}m  ")
  }
  print("\e[0m\n")
}
print("\n")
for (i=124; i < 160; i+=6) {
  for (ii=0; ii < 6; ii++) {
    print("\e[48;5;{i+ii}m  ")
  }
  print("\e[0m  ")
  for (ii=36; ii < 42; ii++) {
    print("\e[48;5;{i+ii}m  ")
  }
  print("\e[0m  ")
  for (ii=72; ii < 78; ii++) {
    print("\e[48;5;{i+ii}m  ")
  }
  print("\e[0m\n")
}
print("\n")
```

Part 3 begins with the following comment:

> Finally, the 24 grays

The general structure of part 3 can be expressed with the following pseudocode:

```
for (i=232; i < 256; i++) {
  print("\e[48;5;{i}m  ")
}
print("\e[0m\n\n")
```
