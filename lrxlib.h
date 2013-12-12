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
#include <stdint.h>
#define false 0
#define true !false

// #define LRXDEBUG
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
    //Occurs when tree is imbalanced (one child is instantiated and not the others)
    if(t == NULL){
        fprintf(stdout, "null");
        return 0;
    }

    switch (t->datatype){
        case _INT_:
            fprintf(stdout, "%hd", t->root.int_root);
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

    if(t->leaf){
        free(t->children);
        free(t);
        return;
    }

    int i;
    for(i = 0; i < t->degree; ++i)
        lrx_destroy_tree(t->children[i]);

    free(t->children);
    free(t);
}

struct tree *lrx_declare_tree(Atom type, int deg) {

    assert(deg > 0);

    struct tree *t = (struct tree *)malloc(sizeof(struct tree));
    assert(t != NULL);
    t->degree = deg;
    t->datatype = type;


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
        		t->datatype = _STRING_;
        	}        
            t->root.char_root = '\0';
            break;
    }


    t->leaf = true;
    t->children = (struct tree **)malloc(sizeof(struct tree *) * t->degree);
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

    if(children == NULL)
        return t;

    /* set pointers to children */
    int num_children = t->degree;
    int i;
    for(i = 0; i < num_children; ++i) {
    	struct tree *temp = children[i];
        if (temp){
    	   temp->parent = t;	
        }
        t->children[i] = temp; 
	}
    t->leaf = false;  

    return t;
}

/* data = t@; */
bool lrx_access_data_at_bool (struct tree *t)
{
    assert(t != NULL);
    return t->root.bool_root;
}


int16_t lrx_access_data_at_int (struct tree *t)
{
    assert(t != NULL);
    return t->root.int_root;
}

float lrx_access_data_at_float (struct tree *t)
{
    assert(t != NULL);
    return t->root.float_root;
}
char lrx_access_data_at_char (struct tree *t)
{
    assert(t != NULL);
    return  t->root.char_root;
}

/* t@ = data */
bool lrx_assign_data_at_bool (struct tree *t, const bool data)
{
    assert(t != NULL);
    return t->root.bool_root = data;
}
int lrx_assign_data_at_int (struct tree *t, const int16_t data)
{
    assert(t != NULL);
    return t->root.int_root = data;
}
float lrx_assign_data_at_float (struct tree *t, const float data)
{
    assert(t != NULL);
    return t->root.float_root = data;
}
char lrx_assign_data_at_char (struct tree *t, const char data)
{
    assert(t != NULL);
    return t->root.char_root = data;
}

/* t1 = t2%0 */
struct tree *lrx_access_child (struct tree *t, const int child)
{
    assert(t);
    assert(child < t->degree);
    struct tree **children = t->children;
    return *(children + child);
}

/* t1 = t2. Lhs is the tree pointer we need without dereference */
struct tree **lrx_assign_tree_direct(struct tree **lhs, struct tree **rhs)
{
 	
    assert((*lhs)->degree == (*rhs)->degree);
    assert((*lhs) != (*rhs));

    lrx_destroy_tree(*lhs);
    *lhs = *rhs;
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
        we are at an internal node 
        return lrx_assign_tree_direct(access, t2);
    }
    else {
        we are at a leaf node 
        t1->children[child] = t2;
        t1->leaf = false;
        return t1;
    }
}*/


/* breadth first search 
struct tree *lrx_add_trees(struct tree *t1, struct tree*t2)
{
  struct tree *t;

}


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
    	case _NEQ_:
     
            assert(0);
            break;

	}
	
	return value;	
}





/*
???
empty function??
degree

*/






