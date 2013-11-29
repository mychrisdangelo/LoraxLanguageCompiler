#define DO_NOT_COMPILE_THIS_FILE_UNTIL_CG_TEAM_IS_READY
#ifndef DO_NOT_COMPILE_THIS_FILE_UNTIL_CG_TEAM_IS_READY

#ifndef __LRXLIB_H__
#define __LRXLIB_H__

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

/*
* INVARIANT: tree data array is initialized to fit EXACTLY the number of data elements the tree
* was initialized with.
*
*/


typedef enum  {
  FLOAT,
  INT,
  BOOL,
  CHAR,
  STRING

} lrx_primitives;

typedef enum {
	GT,
	GTE,
	LT,
	LTE,
	EQL,
	NEQL
} comparisons;

struct lrx_tree {

  void **data;
  int bfactor;
  int size;
  lrx_primitives type;

};

void _lrx_tree_memcpy( void *b1, void *b2, int size, lrx_primitives type );

/**
 *Function to initialize a lorax_tree type in C. Expects 3 parameters:
 * id: the identifier in lrx program given to the tree
 * bfactor: the branching factor of the tree
 * type: the data type of the tree
 *
 * mallocs a lrx_tree structure, sets the data to 0, and returns a pointer. In the future
 * this will probably need to be wrapped into some sort of smart pointer structure.
 */
struct lrx_tree *construct_tree( const int bfactor,  lrx_primitives type)
{

  struct lrx_tree *temp = (struct lrx_tree *) malloc( sizeof( struct lrx_tree ) );
  if( temp == NULL ) {
    printf("ERROR.");
  }

  temp->bfactor = bfactor;

  temp->type = type;

  return temp;

}



int set_tree( struct lrx_tree *t, const void *data_args, const int data_size ) {

  switch(t->type) {

  case FLOAT:
    *(t->data) = (float *)malloc( sizeof(float) * ( data_size));
    break;
	
  case INT: 
    *(t->data) = (int16_t *)malloc( sizeof(int16_t) * (data_size) );
    break;
	
  case BOOL: case CHAR: case STRING:
    *(t->data) = (char *)malloc( sizeof(char) * (data_size) );
    break;
	
  }
  
  if( *(t->data) == NULL ) {
    printf("ERROR.");
  }
  
  if( memcpy( *(t->data), data_args, data_size ) == NULL ) {
    return 1;
  }
  
  t->size = data_size;

  return 0;
}


/*
*concatenation IS ALMOST DONE.
*TODO: split off concatenation into functions by the type
*of the tree. This handles the void * problem
*/
struct lrx_tree *tree_concat( struct lrx_tree *t1, struct lrx_tree *t2 ) {
   /*check branching factor
	* We shouldn't do any checking in the library. Function of check.ml!
	* if( t1->bfactor < t2->bfactor ) {
	*  return NULL;
	* }
	*/
  
   struct lrx_tree *temp = construct_tree( t1->bfactor, t1->type ); //initialize the new tree struct
   
 
	/* 
	* calculate the size of the new buffer by
	* calculating the heights of each tree and
	* summing it. Initialize a temp buffer to hold
	* the new data structure
	*/
   void *buffer;
   int typesize;
   int size =  (log2( t1->size ) / log2( t1->bfactor ) ) + (log2( t2->size ) / log2( t2->bfactor ) );
  /*
   *
   * Find where to insert the second tree
   */
  

   switch(t1->type) {
   case INT:
  
     int16_t temp_buff[size];
     buffer = int16_t[size];
     typesize = sizeof(int16_t);
     break;
     

  case STRING: case BOOL: case CHAR:
    char temp_buff[size];
   buffer = temp_buff;
    typesize = sizeof(char);
    break;
    
   case FLOAT:
     float temp_buff[size];
     buffer = temp_buff;
     typesize = sizeof(float);
     break;

  }

	


   _lrx_tree_memcpy( buffer, t1->data, t1->size, t1->type );
  
   int i;
   for( i = 1; i < t1->bfactor; i++ ) {
     if( t1->data[i] == NULL ) {
       break;
     }
   }
   
  
   _lrx_tree_memcpy( buffer+i, t2->data, typesize, t2->type );

  int j;
  for ( j = 1; j < t2->size; j += t2->bfactor ) {
    int k;
    i = j * t1->bfactor + 1; 
    for( k = j; k < t2->bfactor; k++ ) {
      _lrx_tree_memcpy( buffer+i, t2->data+k, typesize,t2->type ); 
      i++;
    }
    int diff;
    if( ( diff = t1->bfactor - t2->bfactor ) != 0 ) {
      int m;
      for( m = 0; m < diff; m++ ) {
	_lrx_tree_memcpy( buffer+i, 0, typesize, t2->type );
	i++;
      }
    }
  }
  
  
  set_tree( temp, buffer, size );
  
  return temp;
  
}

void _lrx_tree_memcpy( void *buffer, void *data, const int size, lrx_primitives type ) {
	
  switch( type ) {
  case INT:
    memcpy( (int *)buffer, (int *)data, size );
    break;
    
  case BOOL: case STRING: case CHAR:
    memcpy( (char *)buffer, (char *)data, size );
    break;
    
  case FLOAT:
    memcpy( (float *)buffer, (float *)data, size );
    break;
  }
}


void print_tree( struct lrx_tree *t) {
  char *fmt;
  int step;
  switch(t->type) {
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
    break;
  }
  int i;
  for(i = 0; i < t->size; i += step)
    {
      printf( fmt, *(t->data)+i );
    }
  
}

int lrx_tree_compare( struct lrx_tree *t1, struct lrx_tree *t2, comparisions operation ) {
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

int _lrx_check_equals( struct lrx_tree *t1, struct lrx_tree *t2 ) {
	int i;
	int equals = 1;
	for( i = 0; i < t1->size; i++ ) {
		switch(t1->type) {
			case INT:
				equality = (int16_t)*(*(t1->data)+i) == (int16_t)*(*(t2->data)+i);
				break;
			case CHAR: case STRING: case BOOL:
				equality = (char)*(*(t1->data)+i) == (char)*(*(t2->data)+i);
				break;
			case FLOAT:
				equality = (float)*(*(t1->data)+i) == (float)*(*(t2->data)+i);
				break;
			}
	}
	return equality;
}
#endif

// DO_NOT_COMPILE_THIS_FILE_UNTIL_CG_TEAM_IS_READY
#endif 
