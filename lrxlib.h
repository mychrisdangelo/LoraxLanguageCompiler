/*
 * Authors:
 * Kira Whitehouse
 * Chris D'Angelo
 * Doug Bienstock
 */
//TODO: CHANGE TO 16 BIT INTEGERS

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <stdint.h>
#define false 0
#define true !false

//#define LRXDEBUG
#ifdef LRXDEBUG
#define LrxLog( ... ) fprintf(stderr, __VA_ARGS__ )
#else
#define LrxLog( ... )
#endif

typedef enum {
	_GT_,
	_GTE_,
	_LT_,
	_LTE_,
	_EQ_,
	_NEQ_,
} Comparator;

typedef enum {
    _BOOL_,
    _INT_,
    _FLOAT_,
    _CHAR_,
    _STRING_
} Atom;

typedef int bool;

typedef union Root {
    char char_root;
    int16_t int_root;
    bool bool_root;
    float float_root;
} Root;


typedef struct tree{
    int degree;
    Atom datatype;

    Root root;

    /* array of children, which are themselves tree pointers */
    struct tree **children;
    struct tree *parent;

    /* leaf == childless */
    bool leaf;
    /* isNull == has been declared but not defined */ 
    bool is_null;
    /* reference count (smart pointer) */
    int *count;
} tree;

int lrx_print_bool(bool b) {
    if (b) {
        fprintf(stdout, "true");
    } else {
        fprintf(stdout, "false");
    }
    return 0;
}

// TODO: the following functions are not implemented
int lrx_print_tree(struct tree *t) {
    LrxLog("I am in print tree.\n");

    //Occurs when tree is imbalanced (one child is instantiated and not the others)
    if(t == NULL){
        fprintf(stdout, "null");
        return 0;
    }

    LrxLog("datatype: %d\n", t->datatype);
    switch (t->datatype){
        case _INT_:
            fprintf(stdout, "%hd", t->root.int_root);
            LrxLog("%hd\n", t->root.int_root);
            break;

        case _FLOAT_:
            fprintf(stdout, "%f", t->root.float_root); 
            break;

        case _CHAR_: 
        case _STRING_:
            fprintf(stdout, "%c", t->root.char_root); 
            break;

        case _BOOL_:
            lrx_print_bool(t->root.bool_root);
            break;

    }

    if(!t->leaf){
        int i;
        if( t->datatype != _STRING_ )  {
            fprintf(stdout, "[");
        }
        for(i = 0; i < t->degree; ++i){
//        	if( t->children[i] == NULL && t->c
            LrxLog("print iter: %d\n", i);
            if (t->children[i] == NULL && t->degree == 1 && (t->datatype == _CHAR_ || t->datatype == _STRING_)) {
                break;
            }
	        lrx_print_tree(t->children[i]);
	        
            if (t->datatype != _STRING_ && i != t->degree - 1){
                fprintf(stdout, ",");
            }
        }
        if( t->datatype != _STRING_ ) {
           fprintf(stdout, "]");
    	}
    }

    return 0;

}

void lrx_destroy_tree(struct tree *t)
{
    if(t == NULL){
        return;
    }

    if(*(t->count) <= 1){

        if(t->leaf){
            free(t->children);
            free(t->count);
            free(t);
            return;
        }

        int i;
        for(i = 0; i < t->degree; ++i){
            lrx_destroy_tree(t->children[i]);
        }
        free(t->children);
        free(t->count);
        free(t);
    }
    else{
        *(t->count) -= 1;
    }
}

struct tree *lrx_declare_tree(Atom type, int deg) {

    assert(deg > 0);

    struct tree *t = (struct tree *)malloc(sizeof(struct tree));
    int *cnt = (int *)malloc(sizeof(int));

    assert(t != NULL);
    assert(cnt != NULL);
    *cnt = 1;
    t->degree = deg;
    t->datatype = type;
    t->count = cnt;

    switch(type){
        case _BOOL_:
            t->root.bool_root = false;
            break;

        case _INT_:
            t->root.int_root = 0;
            break;

        case _FLOAT_:
            t->root.float_root = 0.0;
            break;

        case _CHAR_: case _STRING_:
        	if( t->degree == 1 ) {
                LrxLog("Declare string\n");
        		t->datatype = _STRING_;
        	}        
            t->root.char_root = '\0';
            break;
    }

    t->is_null = true;
    t->leaf = true;
    t->children = (struct tree **)malloc(sizeof(struct tree *) * t->degree);
    memset((t->children), 0, sizeof(struct tree*) * t->degree);
    t->parent = NULL;
    return t;
}

struct tree *lrx_define_tree(struct tree *t, void *root_data, struct tree **children)
{

    /* set root data */
    switch(t->datatype){
        case _BOOL_:
            t->root.bool_root = *((bool *)root_data);
            break;

        case _INT_:
            t->root.int_root = *((int16_t *)root_data);
            break;

        case _FLOAT_:
            t->root.float_root = *((float *)root_data);
            break;

        case _CHAR_: 
        case _STRING_:
            t->root.char_root = *((char *)root_data);
            break;
    }

    t->is_null = false;

    if(children == NULL)
        return t;

    /* set pointers to children */
    int num_children = t->degree;
    int i;
    for(i = 0; i < num_children; ++i) {
        LrxLog("Iter i: %d\n", i);

        if(children[i] != NULL){
            children[i]->parent = t;
            t->children[i] = children[i];
        }
	}
    t->leaf = false;  

    return t;
}

/* data = t@; */
bool *lrx_access_data_at_bool (struct tree **t)
{
    assert(*t != NULL);
    return &(*t)->root.bool_root;
}


int16_t *lrx_access_data_at_int (struct tree **t)
{
    assert(*t != NULL);
    return &((*t)->root.int_root);
}

float *lrx_access_data_at_float (struct tree **t)
{
    assert(*t != NULL);
    return &(*t)->root.float_root;
}
char *lrx_access_data_at_char (struct tree **t)
{
    assert(*t != NULL);
    return &(*t)->root.char_root;
}

/* t@ = data */
bool lrx_assign_data_at_bool (struct tree **t, const bool data)
{
    assert(*t != NULL);
    return (*t)->root.bool_root = data;
}
int lrx_assign_data_at_int (struct tree **t, const int16_t data)
{
    assert(*t != NULL);
    return (*t)->root.int_root = data;
}
float lrx_assign_data_at_float (struct tree **t, const float data)
{
    assert(t != NULL);
    return (*t)->root.float_root = data;
}
char lrx_assign_data_at_char (struct tree **t, const char data)
{
    assert(t != NULL);
    return (*t)->root.char_root = data;
}

/* t1 = t2%0 */
struct tree **lrx_access_child (struct tree **t, const int child)
{
    assert(*t);
    assert(child < (*t)->degree);
    /* ptr to the parent's ptr to it's children */
    return &((*t)->children[child]);
}

/* t1 = t2. Lhs is the tree pointer we need without dereference */
struct tree **lrx_assign_tree_direct(struct tree **lhs, struct tree **rhs)
{

    if(lhs == rhs)
        return lhs;
    if(lhs && rhs && *rhs && *lhs)
        assert((*lhs)->degree == (*rhs)->degree);
    
    // if(!rhs || !*rhs)
    //     fprintf(stderr, "rhs is null\n");
    // if(!lhs || !*lhs)
    //     fprintf(stderr, "lhs is null\n");

    lrx_destroy_tree(*lhs);
    *lhs = *rhs;
    if(*rhs)
        *((*rhs)->count) += 1;
/*

    lhs->root = rhs->root;
    lhs->leaf = rhs->leaf;
    lhs->children = rhs->children;
*/
    return lhs;
}

/* t%0 = t1 
   tree<int> t(1);
   t%0
   tmp = t%0

*/
   /*
struct tree *lrx_assign_tree_with_dereference(struct tree *t1, int child, struct tree *t2)
{
    //here t1 is a leaf node
    assert(child < t1->degree);

    struct tree *access = lrx_access_child(t1, child);

    if(access && t2) {
      //  we are at an internal node 
        return lrx_assign_tree_direct(access, t2);
    }
    else {
     //   we are at a leaf node 
        t1->children[child] = t2;
        t1->leaf = false;
        return t1;
    }
}
*/

/** concatenation
* appends t2 to the first available child sport in t1
* if no such spot is available
*/
struct tree *lrx_add_trees(struct tree *t1, struct tree*t2)
{
  //base case
  if( t1 == NULL ) {
  	t1 = t2;
  	return t1;
  }  
  
  //otherwise, BFS
  int qSize = t1->degree * t1->degree;
  struct tree *q[ qSize ];
  int front =0;
  int back = 0;
  q[ back ] = t1;
  back =  (back+1) % qSize;
  while( q[ front ] != NULL ) {
  	struct tree *t = q[ front ];
  	q[ front ] = NULL;
  	front = (front+1) % qSize;
  	int i;
  	for( i = 0; i < t->degree; i++ ) {
  		if( t->children[i] == NULL ) {
  			if( t->leaf ) t->leaf = !t->leaf;
  			t->children[i] = t2;
  			return t1;
  		}
  		q[ back ] = t->children[i];
  		back = (back+1) % qSize;
  	}
  }
  return NULL;
}
  
  



/*
*  MUTATOR
*  goes to t's parent and sets its entry in t->children to NULL
*  sets t->parent to NULL
*  returns t
*  simply returns t if t is a root
* 
*/
struct tree *lrx_pop_tree(struct tree *t)
{
	if( t-> parent == NULL ) {
		//NOTE: the reference count of t will need to be decremented here.
		struct tree *temp = lrx_declare_tree( t->datatype, t->degree );
		switch(t->datatype) {
		case _INT_:
			lrx_define_tree( temp, &(t->root.int_root), t->children );
			break;
		case _BOOL_:
			lrx_define_tree( temp, &(t->root.bool_root), t->children );
			break;
		case _FLOAT_:
			lrx_define_tree( temp, &(t->root.float_root), t->children );
			break;
		case _CHAR_: case _STRING_:
			lrx_define_tree( temp, &(t->root.char_root), t->children );
			break;
		}
		
		
		t = NULL;
		return temp;
		
	}
	
	struct tree *parent = t->parent;
	int i;
	for( i = 0; i < parent->degree; i++ ) {
		if( parent->children[i] == t ) {
			parent->children[i] = NULL;
			break;
		}
	}
	
	t->parent = NULL;
	return t;
}


struct tree *lrx_get_root(struct tree *t)
{
	if( t->parent == NULL ) {
		return t;
	}
	return lrx_get_root( t->parent );
}

struct tree *lrx_get_parent( struct tree *t ) {
	return t->parent;
}

int _lrx_count_nodes( struct tree *t ) {
	int count = 0;
	int i;
	if( t == NULL ) {
		return 0;
	}
	if( t->leaf) {
		return 1;
	}
	count += 1;
	for( i = 0; i < t->degree; i++ ) {
		count +=  _lrx_count_nodes( t->children[i] );
	}
	return count;
}
	
int _lrx_check_equals(struct tree *lhs, struct tree *rhs ) {
    if(lhs == NULL && rhs == NULL)
        return true;
    if(lhs == NULL || rhs == NULL)
        return false;

	int equals = 1;
	if( lhs->datatype != rhs->datatype || lhs->degree != rhs->degree ) return !equals;

	switch( lhs->datatype ) {
		case _INT_:
			equals = lhs->root.int_root == rhs->root.int_root;
			break;
		case _BOOL_:
			equals = lhs->root.bool_root == rhs->root.bool_root;
			break;
		case _FLOAT_:
			equals = lhs->root.float_root == rhs->root.float_root;
			break;
		case _CHAR_: 
        case _STRING_:
			equals = lhs->root.char_root == rhs->root.char_root;
			break;
	}
	
	if( !equals ) return equals;	
	
	int i;
	for( i = 0; i < lhs->degree; i++ ) {
		equals = _lrx_check_equals( lhs->children[i], rhs->children[i] );
		if( !equals ) return equals;
	}
	
	return equals;
}
		
//TODO: equals and not equals
bool lrx_compare_tree( struct tree *lhs, struct tree *rhs, Comparator comparison ) {
	int lhs_nodes = _lrx_count_nodes( lhs );
	int rhs_nodes = _lrx_count_nodes( rhs );
	int value;

    LrxLog("%d vs %d\n", lhs_nodes, rhs_nodes);
    LrxLog("Comparator = %d\n", comparison);
    #ifdef LRXDEBUG
    lrx_print_tree(lhs);
    printf("\n");
    lrx_print_tree(rhs);
    printf("\n");
    #endif
	
	switch(comparison) {

    	case _LT_:
    		value = lhs_nodes < rhs_nodes;
    		break;
    	case _LTE_:
    		value = lhs_nodes <= rhs_nodes;
    		break;
    	case _GT_:
    		value = lhs_nodes > rhs_nodes;
    		break;
    	case _GTE_:
    		value = lhs_nodes >= rhs_nodes;
    		break;
    	case _EQ_:
    		value = _lrx_check_equals( lhs, rhs );
    		break;
    	case _NEQ_:
     		value = !_lrx_check_equals( lhs, rhs );            
            break;

	}
	
	return value;	
}


int lrx_get_degree(struct tree *t)
{
    return t->degree;
}


/*
???
empty function??
degree

*/






