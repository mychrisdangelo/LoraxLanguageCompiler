Lorax Programming Language
==========================
Compiler for Lorax, a language focused on making tree operations simple.

Authors
========
Doug Beinstock (dmb2168)
Chris D'Angelo (cd2665)
Zhaarn Maheswaran (zsm2103)
Tim Paine (tkp2108)
Kira Whitehouse (kbw2116)

Getting Started
===============
```
$ make
$ echo "main() { print("hello, world\n"); return 0; }" > hello.lrx
$ ./lorax -c < hello.lrx
$ gcc hello.c
$ ./a.out
$ hello, world
$
```
Running Tests
=============
```
$ make
$ ./testall.sh
$
```
Examples
========

If you're interested in some real world examples check out the ./examples
directory.
