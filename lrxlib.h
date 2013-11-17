#ifndef __LRXLIB_H__
#define __LRXLIB_H__

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

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
  enum lrx_primitives type;

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
struct lrx_tree *construct_tree( const char *id, const int bfactor, enum lrx_primitives type)
{

  struct lrx_tree *temp = (struct lrx_tree *) malloc( sizeof( struct lrx_tree ) );
  if( temp == NULL ) {
    printf("ERROR.");
  }

  temp->id = (char *)malloc( strlen(id) );
  if( temp->id == NULL ) {
    printf("ERROR.");
  }
  strncpy( temp->id, id, strlen(id) );

  switch(type) {

  case FLOAT:
    temp->data = (float *)malloc( sizeof(float) * (bfactor * TREE_SIZE));

  case INT: 
    temp->data = (int16_t *)malloc( sizeof(int16_t) * (bfactor * TREE_SIZE) );

  case BOOL: case CHAR:
    temp->data = (char *)malloc( sizeof(char) * (bfactor * TREE_SIZE) );

  }
  if( temp->data == NULL ) {
    printf("ERROR.");
  }

  temp->bfactor = bfactor;

  temp->type = type;

  return temp;

}




#endif
