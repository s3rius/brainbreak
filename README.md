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
[![asciicast](https://asciinema.org/a/myeE4cxgOblxB5QRWdIFw65zW.svg)](https://asciinema.org/a/myeE4cxgOblxB5QRWdIFw65zW?autoplay=1)
You can just call `bb` to enter REPL.
It works as ipython. Every time you hit the Enter button, code evaluates.
It's simple as is.

REPL itself inspired by the [IPython](https://ipython.org/) project and looks similar to it.

# Interpreter
[![asciicast](https://asciinema.org/a/Fi2TuDXSLVjqFjsKocHpz6gW3.svg)](https://asciinema.org/a/Fi2TuDXSLVjqFjsKocHpz6gW3?autoplay=1)
To run brainbreak as interpreter you need to provide a `brainFuck` file.
```bash
bb -i /path/to/file.bf
```
This command will optimize and evaluate the code from file.

# Compiler
[![asciicast](https://asciinema.org/a/klCIiJH1m846nuRfyuHRqZmJV.svg)](https://asciinema.org/a/klCIiJH1m846nuRfyuHRqZmJV?autoplay=1)

To compile your bf files you need to provide input and output files.
The backend option can be omitted.

```bash
bb -i /path/to/input.bf -o /path/to/output

# If you want to change compilation backend you can add option
bb -i /path/to/input.bf -o /path/to/output -b Cpp
```
