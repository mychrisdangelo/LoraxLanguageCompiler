#ifndef __LRXLIB_H__
#define __LRXLIB_H__

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

int TREE_SIZE = 10; //arbitrary multiplier for tree size

enum lrx_primitives {
  FLOAT,
  INT,
  BOOL,
  CHAR,
  STRING

};


struct lrx_tree {

  void **data;
  int bfactor;
  int size;
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
struct lrx_tree *construct_tree( const int bfactor,  enum lrx_primitives type)
{

  struct lrx_tree *temp = (struct lrx_tree *) malloc( sizeof( struct lrx_tree ) );
  if( temp == NULL ) {
    printf("ERROR.");
  }

  //  temp->id = (char *)malloc( strlen(id) );
  //if( temp->id == NULL ) {
  // printf("ERROR.");
  //}
  //  strncpy( temp->id, id, strlen(id) );

  temp->bfactor = bfactor;

  temp->type = type;



  return temp;

}

int set_tree( const struct lrx_tree *t, const void *data_args, const int data_size ) {

  switch(t->type) {

  case FLOAT:
    *(t->data) = (float *)malloc( sizeof(float) * ( data_size));

  case INT: 
    *(t->data) = (int16_t *)malloc( sizeof(int16_t) * (data_size) );

  case BOOL: case CHAR: case STRING:
    *(t->data) = (char *)malloc( sizeof(char) * (data_size) );

  }
  
  if( *(t->data) == NULL ) {
    printf("ERROR.");
  }
  
  if( memcpy( t->data, data_args, data_size ) == NULL ) {
    return 1;
  }
  
  temp->size = data_size;

  return 0;
}
// 
//  * concatenation IS ALMOST DONE.
//  * TODO: split off concatenation into functions by the type
//  * of the tree. This handles the void * problem
//  *struct lrx_tree *tree_concat( struct lrx_tree *t1, struct lrx_tree *t2 ) {
//  *  //check branching factor
//  *  if( t1->bfactor < t2->bfactor ) {
//  *  return NULL;
//  *  }
//  *
//  *  
//  *  struct lrx_tree *temp = construct_tree( t1->bfactor, t1->type ); //initialize the new tree struct
//  *  
//  *
//  */* 
//   * * calculate the size of the new buffer by
//    ** calculating the heights of each tree and
// *   * summing it. Initialize a temp buffer to hold
// *   * the new data structure
// *  
// *  void *buffer;
// *  int typesize;
// *  int size =  + (log2( t1->size ) / log2( t1->bfactor ) ) + (log2( t2->size ) / log2( t2->bfactor ) );
// *
//   switch(t1->type) {
//   case INT:
//     int temp[size];
//     buffer = temp;
//     typesize = sizeof(int);
// 
//   case STRING: case BOOL: case CHAR:
//     char temp[size];
//     buffer = temp;
//     typesize = sizeof(char);
// 
//   case FLOAT:
//     float temp[size];
//     buffer = temp;
//     typesize = sizeof(float);
// 
//   }
// 
//   /*
//    *
//    * Find where to insert the second tree
//    *
//   
//   void *root = t1->data[0];
//   int i;
//   for( i = 1; i < t1->bfactor; i++ ) {
//     if( t1->data[i] == NULL ) {
//       break;
//     }
//   }
// 
//   memcpy( buffer, t1->data, t1->size );
//   memcpy( buffer[i], t2->data[0], typesize );
// 
//   int j;
//   for ( j = 1; j < t2->size; j + t2->bfactor ) {
//     int k;
//     i = j * t1->bfactor + 1; 
//     for( k = j; k < t2->bfactor; k++ ) {
//       memcpy( buffer[i], t2->data[k], typesize ); 
//       i++;
//     }
//     int diff;
//     if( ( diff = t1->bfactor - t2->bfactor ) != 0 ) {
//       int m;
//       for( m = 0; m < diff; m++ ) {
// 	memcpy( buffer[i], 0, typesize );
// 	i++;
//       }
//     }
//   }
//     
// 
//   set_tree( temp, buffer, size );
// 
//   return temp;
// 
// }

void print_tree( struct lrx_tree *t, enum lrx_primitives type) {
	char *fmt;
	int step;
	switch(type) {
		case STRING: case CHAR: case BOOL:
			fmt = "%s";
			step = sizeof(char);
			break;
		case INT:
			fmt = "%d";
			step = sizeof(int);
			break;
		case FLOAT:
			fmt = "%f";
			step = sizeof(float);
			break;
		default:
			printf("Error\n");
	}
	int i;
	for(i = 0; i < t->size; i+step)
	{
		printf( fmt, *(t->data)+i );
	}
	
}
	
  

#endif
