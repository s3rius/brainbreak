[![Coverage Status](https://img.shields.io/coveralls/github/s3rius/brainbreak?style=for-the-badge)](https://coveralls.io/github/s3rius/brainbreak)
[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/s3rius/brainbreak/BrainBreak%20tests?style=for-the-badge)](https://github.com/s3rius/brainbreak/actions)
[![Haskell version](https://img.shields.io/badge/haskell-lts--16.31-green?style=for-the-badge)](https://github.com/commercialhaskell/stackage-snapshots/blob/master/lts/16/31.yaml)

# brainBreak

Simple BrainFuck toolkit written in haskell.

This project covers 3 things:
* [REPL](#REPL);
* [Interpreter](#Interpreter);
* [Compiler](#Compiler).

You can check it by your self.

# Instalation
You need to install [Stack](https://docs.haskellstack.org/en/stable/README/) before continue.
Sine you've got the Stack,  you can simply install this project by running:
```bash
git clone https://github.com/s3rius/brainbreak.git
cd brainbreak
stack install
```

Additionally for compilation you need to have different compilers installed.
For `C++` compilation you need to install `clang++`.

# REPL
[![asciicast](https://asciinema.org/a/k2YQQUaJdYsEjIOd0oJZxRMw9.svg)](https://asciinema.org/a/k2YQQUaJdYsEjIOd0oJZxRMw9?autoplay=1)
You can just call `bb` to enter REPL.
It works as ipython. Every time you hit the Enter button, code evaluates.
It's simple as is.

REPL itself inspired by the [IPython](https://ipython.org/) project and looks similar to it.
Addition commands:
```
In [0]: ++++

# Print current REPL state.
In [1]: :state
Current index: 0
Offset from start: 0
part of curren buffer:
[0,0,0,0,0,4,0,0,0,0,0]

# Print current buffer as list of integers.
In [2]: :buf
[0,0,0,0,0,4,0,0,0,0,0]

# Print current buffer as characters.
In [3]: :bufc
"\NUL\NUL\NUL\NUL\NUL\EOT\NUL\NUL\NUL\NUL\NUL"
```

# Interpreter
[![asciicast](https://asciinema.org/a/nElf1qUhNde6xancoMsV20WZ9.svg)](https://asciinema.org/a/nElf1qUhNde6xancoMsV20WZ9?autoplay=1)
To run brainbreak as interpreter you need to provide a `brainFuck` file.
```bash
bb -i /path/to/file.bf
```
This command will optimize and evaluate the code from file.

# Compiler
[![asciicast](https://asciinema.org/a/NzCJc6t23TDSWxLO1QqQTNM79.svg)](https://asciinema.org/a/NzCJc6t23TDSWxLO1QqQTNM79?autoplay=1)
To compile your bf files you need to provide input and output files.
The backend option can be omitted.

```bash
bb -i /path/to/input.bf -o /path/to/output

# If you want to change compilation backend you can add option
bb -i /path/to/input.bf -o /path/to/output -b Cpp
```
