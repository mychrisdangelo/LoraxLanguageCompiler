Lorax Programming Language
==========================
Compiler for Lorax, a language focused on making tree operations simple. Authors: Doug Beinstock (dmb2168), Chris D'Angelo (cd2665), Zhaarn Maheswaran (zsm2103), Tim Paine (tkp2108), Kira Whitehouse (kbw2116)

Requirements
===========
[OCaml](http://ocaml.org/), [Unix](http://www.ubuntu.com/), [gcc](http://gcc.gnu.org/)
Quick Start
===============
```
$ cat hello.lrx
$ int main() { print("hello, world\n"); }
$ make
$ ./lorax -b hello.lrx
$ ./a.out
$ hello, world
$
```
Compiler Flags
==============
* `-a` Print the Abstract Syntax Tree digested source code.
* `-t` Print an alphabetical list of the symbol table created from source code.
* `-s` Run Semantic Analysis on source code.
* `-c` Compile source code to target c language. Default to stdout, or written to filename present in third command line argument.
* `-b` Compile source code to binary ouput. By default to a.out, or the filename present in third command line argument.

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

User Guides
===========
[Language Reference Manual](http://bit.ly/theloraxmanual), [Lorax Language Presentation](http://bit.ly/theloraxpresentation)
