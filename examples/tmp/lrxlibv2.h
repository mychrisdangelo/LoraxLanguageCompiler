#ifndef __LRXLIBV2_H__
#define __LRXLIBV2_H__

/**
 *Lorax C Library to integrate with Lorax Language.
 * Authors:  Doug Bienstock
 *
 * TODO: more complex memory management scenarios. ( Child, and dereference )
 *
 */


#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <math.h>


#define TRUE 1
#define FALSE 0


//enumeration constants for tree types. Mimics polymorphic property of trees
typedef enum  {
  FLOAT,
  INT,
  BOOL,
  CHAR,
  STRING

} lrx_primitives;

//enumeration constants for valid tree comparison operators
typedef enum {
	GT,
	GTE,
	LT,
	LTE,
	EQL,
	NEQL
} comparisons;

/*
 * Lorax Tree Structure.
 * data: Pointer to the node's data item.
 * bfactor: The branching factor of the tree.
 * size: The number of data elements in the tree.
 * type: The data type this tree holds.
 * children: an array of pointers to this node's children. SIZE MUST BE EQUAL to the branching factor
 *
 */
struct lrx_node {

  void *data;
  int bfactor;
  lrx_primitives type;
  struct lrx_node **children;

};

struct lrx_node *lrx_construct_tree( const void *data, const int bfactor, const lrx_primitives type, struct lrx_node **children );

/**
*path is an array of ints representing the path from the root to take. this can be taken
*directly fromt the integers supplied to the % operator.
*/
struct lrx_node *lrx_construct_subtree( struct lrx_node *root, int *path );

//deconstructor built without integration with smart pointers. parameter likely to change
void lrx_destroy_tree( struct lrx_node *root );

struct lrx_node *lrx_tree_concat( struct lrx_node *left, struct lrx_node *right );

void pint_tree( const struct lrx_node *root );

int lrx_tree_compare( const struct lrx_node *left, const struct lrx_node *right, const comparisons operation );


