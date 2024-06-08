<!--
SPDX-FileCopyrightText: 2024 Eli Array Minkoff

SPDX-License-Identifier: CC0-1.0
-->

# How to run each implementation

In the `RUNNERS` directory, there are 2 executable bash scripts -
`install-deps.sh` and `run-version.sh`. Both of them can take any number of
colortest implementations as arguments. (The name used for each implementation
is the name of its directory). If not running as root, they may try to run
`apt-get` using one of `sudo`, `doas`, or `pkexec`, or quick.

If `run-version.sh` is passed the `-t` or `--test` flag, it will `diff` the
output of each implementation ran against the `colortest_output` file, and
print status message to `stderr`, exiting with exit code 1 if any tests fail.
If passed the `-a` or `--test-all` flag, it ignores any other arguments, and
acts as though it had been provided all implementations, once each, as well as
the `-t` flag.

Running `install-deps.sh` will non-interactively install dependencies for any
implementations listed as arguments. If none are listed, it will install
dependencies for all implementations. It will only run on systems where running
`uname -mo` results in `x86_64 GNU/Linux`, or systems which do not have a file
at `/etc/debian_version`. If you want to edit it to handle other cases, feel
free to open a pull request. Wherever possible, it uses packages found in the
Debian 12 "Bookworm" repositories, except for Rust, as some interpreters for
lesser-known languages are written in Rust, and at least one of them (Fender)
needs a newer Rust compiler than the one Debian provides.

You can alternatively run each implementation manually using a similar process
to the one used in those scripts, but the process for that is not documented.
