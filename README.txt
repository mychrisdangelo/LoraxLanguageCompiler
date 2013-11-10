Lorax Programming Language

<<<<<<< HEAD
PLT group project

CFG
=========

tree -> expr[]; | expr[nodes];

nodes -> expr, nodes | expr

expr -> tree | ...
=======
Authors:

Doug Beinstock (dmb2168)
Chris D'Angelo (cd2665)
Zhaarn Maheswaran (zsm2103)
Tim Paine (tkp2108)
Kira Whitehouse (kbw2116)

Getting Started:

$ make
$ echo "main() { print("hello, world\n"); }" > hello.lrx
$ lorax -c < hello.lrx
$ g++ hello.cpp
$ ./a.out
$ hello, world
$
>>>>>>> master
