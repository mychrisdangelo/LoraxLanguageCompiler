#ifndef __LRXLIB_H__
#define __LRXLIB_H__

#include <stdint.h>

int TREE_SIZE = 10; //arbitrary multiplier for tree size

enum lrx_primitives {
  FLOAT,
  INT,
  BOOL,
  CHAR

};


struct lrx_tree {
  
  char *id; //not sure if this is needed?
  void *data;
  int bfactor;

};

/**
 *Function to initialize a lorax_tree type in C. Expects 3 parameters:
 * id: the identifier in lrx program given to the tree
 * bfactor: the branching factor of the tree
 * type: the data type of the tree
 *
 * mallocs a lrx_tree structure, sets the data to 0, and returns a pointer. In the future
 * this will probably need to be wrapped into some sort of smart pointer structure.
 */
struct lrx_tree *construct_tree( const char *id, const int bfactor, enum lrx_primitives type);



#endif
