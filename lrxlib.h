#ifndef __LRXLIB_H__
#define __LRXLIB_H__

/**
 *Lorax C Library to integrate with Lorax Language.
 * Authors: Zhaarn Maheswaran, Doug Bienstock
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
 * data: Pointer to an array of the tree's data items.
 * bfactor: The branching factor of the tree.
 * size: The number of data elements in the tree.
 * type: The data type this tree holds.
 *
 */
struct lrx_tree {

  void **data;
  int bfactor;
  int size;
  lrx_primitives type;

};

int _lrx_check_equals( struct lrx_tree *t1, struct lrx_tree *t2 );
void _lrx_tree_memcpy( void *b1, void *b2, int size, lrx_primitives type );
int set_tree( struct lrx_tree *t, const void *data_args, const int data_size );



/**
 *Function to initialize a lorax_tree type in C. Expects 2 parameters:
 * bfactor: the branching factor of the tree
 * type: the data type of the tree
 *
 * Trees are initialized dynamically onto the heap. This function only reserves memory
 * for the tree structure NOT the data items.
 */
struct lrx_tree *construct_tree( const int bfactor,  lrx_primitives type)
{

  struct lrx_tree *temp = (struct lrx_tree *) malloc( sizeof( struct lrx_tree ) );
  if( temp == NULL ) {
    printf("ERROR.");
  }

  temp->data = (void *)malloc( sizeof(void *));

  temp->bfactor = bfactor;

  temp->type = type;

  temp->size = 0;

  return temp;

}

/**
*
* Function to capture the % operation when used WITH ASSIGNMENT. 
* function returns a pointer to a new tree structure with data items
* assigned to the subtree given by the % operator. This function expects
* 2 parameters:
* t: the tree that the % operation is being performed on.
* index: the index into the tree t that marks the start of the subtree referenced in the % operation
*
*Note this function is to be used only within assignment. When multiple % symbols are strung
*together it is expected that the ocaml will string these together into a single index
*/
struct lrx_tree *get_subtree( struct lrx_tree *t, int index ) {
 
  struct lrx_tree *temp = construct_tree( t->bfactor, t->type );

  int size = t->size - index;

  void *buffer;
  int typesize;
  switch(t->type) {
  case BOOL:
    buffer = (short *)malloc(sizeof(short) * size);
    typesize = sizeof(short);
    break;
  case INT:
    buffer = (int16_t *)malloc(sizeof(int16_t)*size);
    typesize = sizeof(int16_t);
    break;
  case CHAR: case STRING:
    buffer = (char *)malloc(sizeof(char)*size);
    typesize = sizeof(char);
    break;
  case FLOAT:
    buffer = (float *)malloc(sizeof(float)*size);
    typesize = sizeof(float);
    break;
  }

  _lrx_tree_memcpy( buffer, *(t->data)+(index*typesize), size, t->type);
  
  set_tree( temp, buffer, typesize );
  
  free(buffer);
  
  return temp;
}


//deconstructor
void destroy_tree( struct lrx_tree *t ) {
  free( *(t->data) );
  free( t );
}

/**
 * Function to set the data items in an already initialized tree. Expects 3 parameters:
 * t: The tree to set data items for.
 * data_args: an array containing the data for tree t
 * data_size: The number of data items in the data_args array
 * 
 * This function will malloc() the required amount of space for data_size elements
 * and copy the data items from data_args into the newly malloc'd space. Sets the
 * tree's data pointer to point at this array and also sets the tree's size.
 * A tree's size should always be equal to exactly the number of data items.
 */
int set_tree( struct lrx_tree *t, const void *data_args, const int data_size ) {


  switch(t->type) {

  case FLOAT:
    *(t->data) =  (float *)malloc( sizeof(float) * ( data_size));
    break;
	
  case INT: 
    *(t->data) = (int16_t *)malloc( sizeof(int16_t) * (data_size) );
    break;
  case BOOL:
    *(t->data) = (short *)malloc( sizeof(short) * (data_size) );
    break;
  case CHAR: case STRING:
    *(t->data) = (char *)malloc( sizeof(char) * (data_size) );
    break;
	
  }
  
  if( *(t->data) == NULL ) {
    printf("ERROR.");
  }
 
  _lrx_tree_memcpy( *(t->data), (void *)data_args, data_size, t->type);

  
  t->size = data_size;

  return 0;
}


/**
 *Tree concatentation. Expects 2 arguments:
 * t1: The tree to append to
 * t2: The tree that is being appended
 *
 * Function calculates the new size of the tree and copies the data from
 * t2 to t1 according to the rules specified in the LRM. Returns a pointer
 * to a NEW tree structure with the concatenated data.
 *
 *TODO: size may actually be larger than the number of data items, fix this.
 */
struct lrx_tree *tree_concat( struct lrx_tree *t1, struct lrx_tree *t2 ) {
  
   struct lrx_tree *temp = construct_tree( t1->bfactor, t1->type ); //initialize the new tree struct
   
 
  
   //the size of the new tree is determined by adding the heights of t1 and t2. This gaurantees all data will fit
   //todo: check this????
   int size =  (log2( t1->size ) / log2( t1->bfactor ) ) + (log2( t2->size ) / log2( t2->bfactor ) );

  void *buffer;
  int typesize;
  switch(t1->type) {
  case BOOL:
    buffer = (short *)malloc(sizeof(short)*size);
    typesize = sizeof(short);
    break;
  case INT:
    buffer = (int16_t *)malloc(sizeof(int16_t)*size);
    typesize = sizeof(int16_t);
    break;
  case CHAR: case STRING:
    buffer = (char *)malloc(sizeof(char)*size);
    typesize = sizeof(char);
    break;
  case FLOAT:
    buffer = (float *)malloc(sizeof(float)*size);
    typesize = sizeof(float);
    break;
  }



   _lrx_tree_memcpy( buffer, t1->data, t1->size, t1->type );
  
  	/**
  	* loop to find the insertion point for the concatenation.
  	* Will break if it finds an available child node, otherwise
  	* the loop will continue and find the first available spot or
  	* posisbly go on to a new depth level
  	*/
   int i;
   for( i = 1; i < t1->size; i++ ) {
     if( *(t1->data)+(i*typesize) == NULL ) {
       break;
     }
   }
   
  
   _lrx_tree_memcpy( buffer+(i*typesize), *(t2->data), 1, t2->type );

  int j;
  for ( j = 1; j < t2->size; j += t2->bfactor ) {
    int k;
    i = j * t1->bfactor + 1; 
    for( k = j; k < t2->bfactor; k++ ) {
      _lrx_tree_memcpy( buffer+(i*typesize), *(t2->data)+(k*typesize), 1,t2->type ); 
      i++;
    }
    int diff;
    if( ( diff = t1->bfactor - t2->bfactor ) != 0 ) {
      int m;
      for( m = 0; m < diff; m++ ) { //if bfactor is less, then we need to pad the new tree with some NULLS
		_lrx_tree_memcpy( buffer+i,NULL, 1, t2->type );
		i++;
      }
    }
  }
  
  
  set_tree( temp, buffer, size );
  
  free(buffer);
  return temp;
  
}

/**
 * Special memcpy function to cope with the polymorphic nature
 * of Lorax Tree data types.
 */
void _lrx_tree_memcpy( void *buffer, void *data, const int size, lrx_primitives type ) {
	
  switch( type ) {
  case INT:
    memcpy( (int16_t *)buffer, (int16_t *)data, size * sizeof(int16_t) );
    break;
  case BOOL:
    memcpy( (short *)buffer, (short *)data, size * sizeof(short) );
    break;
  case STRING: case CHAR:
    memcpy( (char *)buffer, (char *)data, size );
    break;
    
  case FLOAT:
    memcpy( (float *)buffer, (float *)data, size * sizeof(float) );
    break;
  }
}

/**
 *
 *Function to print the elements of the tree.
 * Prints in pre-order style.
 *
 */
void print_tree( struct lrx_tree *t) {
  char *fmt;
  int i;
  void *lrx_data = *(t->data);
  switch(t->type) {
  case BOOL:
    fmt = "%s";
    for( i = 0; i < t->size; i++ )
      {
	short temp = *((short *)lrx_data+i);
	if( temp == TRUE ) {
	  printf("TRUE");
	} else {
	  printf("FALSE");
	}
      }
    break;
  case STRING:
    fmt = "%s";
    char *temp = (char *)lrx_data;
    printf( fmt, temp );
    break;
  case CHAR:
    fmt = "%c";
    for(i = 0; i < t->size; i++ )
      {
	char temp = *((char *)lrx_data+i);
	printf( fmt, temp );
      }

    break;
  case INT:
    fmt = "%hd";

    for(i = 0; i < t->size; i++ )
      {
	int16_t temp = *((int16_t *)lrx_data+i);
	printf( fmt, temp );
      }

    break;
  case FLOAT:
    fmt = "%f";

    for(i = 0; i < t->size; i++ )
      {
	float temp = *((float *)lrx_data+i);
	printf( fmt, temp );
      }

    break;
  default:
    printf("Error\n");
    break;
  }
  
}

/**
 * Compares two trees accroding to rules set in the LRM.
 * Expects 3 parameters:
 *  t1: The first tree.
 * t2: The second tree.
 * operation: The comparison operation to perform.
 *
 * Returns 0 if t1 does not compare to t2, returns non-zero if comparison matches.
 */
int lrx_tree_compare( struct lrx_tree *t1, struct lrx_tree *t2, comparisons operation ) {
	int equality = 0;
	switch(operation) {
		case GT:
			equality = t1->size > t2->size;
			break;
		case GTE:
			equality = t1->size >= t2->size;
			break;
		case LT:
			equality =  t1->size < t2->size;
			break;
		case LTE:
			equality =  t1->size <= t2->size;
			break;
		case EQL:
			if( t1->bfactor != t2->bfactor || t1->size != t2->size) {
				break;
			}
			equality = _lrx_check_equals( t1, t2 );
			break;
		case NEQL:
			equality = !_lrx_check_equals( t1, t2 );
			break;
		}	
	return equality;

}

/**
 * internal function to do equality checking.
 * Equality checking requires a little more work so it was moved to
 * a separate function.
 */
int _lrx_check_equals( struct lrx_tree *t1, struct lrx_tree *t2 ) {
	int i;
	int equality = 1;
	for( i = 0; i < t1->size; i++ ) {
		switch(t1->type) {
		case BOOL:
		  equality = *(((short *)*(t1->data))+i) == *(((short *)*(t2->data))+i);
		  break;
		case INT:
		  equality = *(((int16_t *)*(t1->data))+i) == *(((int16_t *)*(t2->data))+i);
		  break;
		case CHAR: case STRING:
		  equality = *(((char *)*(t1->data))+i) == *(((char *)*(t2->data))+i);	
		  break;
		case FLOAT:
		  equality = *(((float *)*(t1->data))+i) == *(((float *)*(t2->data))+i);
		  break;
		}
	}
	return equality;
}
#endif

