Lorax Programming Language
==========================
Compiler for Lorax, a language focused on making tree operations simple. Authors: Doug Beinstock (dmb2168), Chris D'Angelo (cd2665), Zhaarn Maheswaran (zsm2103), Tim Paine (tkp2108), Kira Whitehouse (kbw2116)

Quick Start
===============
```
$ make
$ echo "int main() { print("hello, world\n"); return 0; }" > hello.lrx
$ ./lorax -c < hello.lrx > hello.c
$ gcc -g -Wall hello.c
$ ./a.out
$ hello, world
$
```
Compiler Flags
==============
* `-a` Print the Abstract Syntax Tree digested source code.
* `-t` Print an alphabetical list of the symbol table created from source code.
* `-s` Run Semantic Analysis on source code.
* `-c` Compile source code. Output to standard out.

Running Tests
=============
```
$ make
$ ./testall.sh
$
```
Examples
========
If you're interested in some real world examples of the lorax language check out the `examples`
directory.
