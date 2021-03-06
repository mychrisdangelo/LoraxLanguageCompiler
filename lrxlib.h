/*
 * Authors:
 * Kira Whitehouse
 * Chris D'Angelo
 * Doug Bienstock
 * Tim Paine
 */

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
    _STRING_ /* when tree is char type with degree = 1 */
} Atom;

typedef int bool;

typedef union Root {
    char char_root;
    int int_root;
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

int lrx_print_tree(struct tree *t) {
    // Occurs when tree is imbalanced (one child is instantiated and not the others)
    if (t == NULL) {
        fprintf(stdout, "null");
        return 0;
    }

    LrxLog("datatype: %d\n", t->datatype);
    switch (t->datatype){
        case _INT_:
            fprintf(stdout, "%d", t->root.int_root);
            LrxLog("%d\n", t->root.int_root);
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

    if (t->children) {
        int i;
        if (t->datatype != _STRING_ ) {
            fprintf(stdout, "[");
        }
        for (i = 0; i < t->degree; ++i) {

            if (t->children[i] == NULL && t->degree == 1 && (t->datatype == _CHAR_ || t->datatype == _STRING_)) {
                break;
            }
	        lrx_print_tree(t->children[i]);
	        
            if (t->datatype != _STRING_ && i != t->degree - 1){
                fprintf(stdout, ",");
            }
        }
        if (t->datatype != _STRING_) {
           fprintf(stdout, "]");
        }
    }
    return 0;
}

void lrx_destroy_add_tree(struct tree *t) {
    if (t == NULL){
        return;
    }
    
    if (t->children){
        int i;
        for(i = 0; i < t->degree; ++i){
            lrx_destroy_add_tree(t->children[i]);
        }
        free(t->children);
    }
    
    free(t->count);
    free(t);
}

void lrx_destroy_tree(struct tree *t) {
    
    if (t == NULL) {
        return;
    }

    *(t->count) -= 1;
    if (*(t->count) == 0) {

         if (t->children){
            int i;
            for (i = 0; i < t->degree; ++i){
                lrx_destroy_tree(t->children[i]);
            }
            free(t->children);
         }
                  
          free(t->count);
          free(t);
        
    }
}

struct tree *lrx_declare_tree(Atom type, int deg) {
    assert(deg >= 0);
    struct tree *t = (struct tree *)malloc(sizeof(struct tree));
    assert(t);

    t->degree = deg;
    t->datatype = type;
    t->count = (int *)malloc(sizeof(int));
    assert(t->count);
    *(t->count) = 1;

    switch (type) {
        case _BOOL_:
            t->root.bool_root = false;
            break;
        case _INT_:
            t->root.int_root = 0;
            break;
        case _FLOAT_:
            t->root.float_root = 0.0;
            break;
        case _CHAR_: 
        case _STRING_:
        	if (t->degree == 1) {
                LrxLog("Declare string\n");
        		t->datatype = _STRING_;
        	}        
            t->root.char_root = '\0';
            break;
    }

    t->is_null = true;
    t->leaf = true;
    if (t->degree > 0) {
        t->children = (struct tree **)malloc(sizeof(struct tree *) * t->degree);
        assert(t->children);
        memset((t->children), 0, sizeof(struct tree*) * t->degree);
    }
    
    t->parent = NULL;
    return t;
}

struct tree *lrx_define_tree(struct tree *t, void *root_data, struct tree **children){
    /* set root data */
    switch (t->datatype){
        case _BOOL_:
            t->root.bool_root = *((bool *)root_data);
            break;

        case _INT_:
            t->root.int_root = *((int *)root_data);
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

    if (children == NULL){
        return t;
    }

    /* set pointers to children */
    int num_children = t->degree;
    int i;
    int null = 0;
    for (i = 0; i < num_children; ++i) {
        if (children[i] != NULL){
            children[i]->parent = t;
            *(children[i]->count) += 1;
            t->children[i] = children[i];
        }
        else {
            null +=1;
        }
	}
    if(null != num_children) {
        t->leaf = false;  
    }

    return t;
}

/* data = t@; */
bool *lrx_access_data_at_bool (struct tree **t) {
    assert(*t != NULL);
    return &(*t)->root.bool_root;
}

int *lrx_access_data_at_int (struct tree **t) {
    assert(*t != NULL);
    return &((*t)->root.int_root);
}

float *lrx_access_data_at_float (struct tree **t) {
    assert(*t != NULL);
    return &(*t)->root.float_root;
}

char *lrx_access_data_at_char (struct tree **t) {
    assert(*t != NULL);
    return &(*t)->root.char_root;
}

/* t@ = data */
bool lrx_assign_data_at_bool (struct tree **t, const bool data) {
    assert(*t != NULL);
    return (*t)->root.bool_root = data;
}

int lrx_assign_data_at_int (struct tree **t, const int data) {
    assert(*t != NULL);
    return (*t)->root.int_root = data;
}

float lrx_assign_data_at_float (struct tree **t, const float data) {
    assert(t != NULL);
    return (*t)->root.float_root = data;
}

char lrx_assign_data_at_char (struct tree **t, const char data) {
    assert(t != NULL);
    return (*t)->root.char_root = data;
}

/* t1 = t2%0 */
struct tree **lrx_access_child (struct tree **t, const int child) {
    assert(*t);
    assert(child < (*t)->degree);

    /* ptr to the parent's ptr to it's children */
    return &((*t)->children[child]);
}

/* t1 = t2. Lhs is the tree pointer we need without dereference */
struct tree **lrx_assign_tree_direct(struct tree **lhs, struct tree **rhs) {
    if(lhs == rhs)
        return lhs;
    if(lhs && rhs && *rhs && *lhs){
        if((*rhs)->degree == 0) {
            int lhs_degree = (*lhs)->degree;
            (*rhs)->degree = lhs_degree;
            (*rhs)->children = (struct tree **)malloc(sizeof(struct tree *) * lhs_degree);
            assert((*rhs)->children);
            memset(((*rhs)->children), 0, sizeof(struct tree*) * lhs_degree);
        }
        assert((*lhs)->degree == (*rhs)->degree);
    }

    if(*lhs){
        if((*lhs)->parent){
            ((*lhs)->parent)->leaf = false;
        }
    }
       
    lrx_destroy_tree(*lhs);
    *lhs = *rhs;
    if(*rhs){
        if((*rhs)->count)
            *((*rhs)->count) += 1;
    }

    return lhs;
}

int _lrx_count_nodes( struct tree *t ) {
    int count = 0;
    int i;
    if(t == NULL ) {
        return 0;
    }
    if(t->leaf) {
        return 1;
    }
    count += 1;
    for(i = 0; i < t->degree; i++) {
        count +=  _lrx_count_nodes( t->children[i] );
    }
    return count;
}


void lrx_copy_construct_tree(struct tree **target, struct tree **source, 
    int depth, int *insert, struct tree ***position) {

    void *root;
    switch((*source)->datatype){
        case _BOOL_:
            root = &(*source)->root.bool_root;
            break;

        case _INT_:
            root = &(*source)->root.int_root;
            break;

        case _FLOAT_:
            root = &(*source)->root.float_root;
            break;
        case _CHAR_: 
        case _STRING_:
            root = &(*source)->root.char_root;            
            break;
    }

    int degree = (*source)->degree;
    struct tree *children[degree];

    int i;
    for (i = 0; i < degree; ++i) {
        children[i] = NULL;

        if (!(*source)->leaf && (*source)->children && (*source)->children[i] != NULL){
            struct tree *child = lrx_declare_tree((*source)->datatype, degree);
            lrx_copy_construct_tree(&child, &(*source)->children[i], depth + 1, insert, position);
            children[i] = child;
        }
        else if (depth < *insert){
            *insert = depth;
            (*target)->leaf = false;
            *position = &((*target)->children[i]);
        }
    }
    *target = lrx_define_tree(*target, root, children);
}

/** concatenation
* appends t2 to the first available child sport in t1
* if no such spot is available
*/
void lrx_add_trees(struct tree **target, struct tree **lhs, struct tree **rhs) {
    if (lhs && rhs && *rhs && *lhs) {
        assert((*lhs)->datatype == (*rhs)->datatype);

        int rhs_degree = (*rhs)->degree;
        int lhs_degree = (*lhs)->degree;
        if (rhs_degree == 0 && lhs_degree == 0){
            (*rhs)->degree = 1;
            (*lhs)->degree = 1;
        }
        if (rhs_degree == 0) {
           (*rhs)->degree = (*lhs)->degree;

             (*rhs)->children = (struct tree **)malloc(sizeof(struct tree *) * (*rhs)->degree);
             assert((*rhs)->children);
             memset(((*rhs)->children), 0, sizeof(struct tree *) * (*rhs)->degree);
        }
        if (lhs_degree == 0) {
            (*lhs)->degree = (*rhs)->degree;
            (*lhs)->children = (struct tree **)malloc(sizeof(struct tree *) * (*lhs)->degree);
            assert((*lhs)->children);
            memset(((*lhs)->children), 0, sizeof(struct tree *) * (*lhs)->degree);

            (*target)->degree = (*rhs)->degree;
            (*target)->children = (struct tree **)malloc(sizeof(struct tree *) * (*lhs)->degree);
            assert((*target)->children);
            memset(((*target)->children), 0, sizeof(struct tree *) * (*lhs)->degree);
        }
        assert((*lhs)->degree == (*rhs)->degree);
    }

    /* copy construct lhs */
    int max_nodes_lhs = _lrx_count_nodes(*lhs);
    struct tree **pos;
    lrx_copy_construct_tree(target, lhs, 0, &max_nodes_lhs, &pos);
    
    /* copy construct rhs */
    struct tree **trash;
    int max_nodes_rhs = _lrx_count_nodes(*rhs);
    struct tree *rhs_copy = lrx_declare_tree((*rhs)->datatype, (*rhs)->degree); /* Ir_Decl */
    lrx_copy_construct_tree(&rhs_copy, rhs, max_nodes_rhs, &max_nodes_rhs, &trash);

    *pos = rhs_copy;
}

struct tree **lrx_get_root(struct tree **t){
	if ((*t)->parent == NULL) {
		return t;
	}
	return lrx_get_root(&(*t)->parent);
}

struct tree **lrx_get_parent(struct tree **t) {
    assert(t && *t);
	return &((*t)->parent);
}
	
int _lrx_check_equals(struct tree *lhs, struct tree *rhs ) {
    if (lhs == NULL && rhs == NULL)
        return true;
    if (lhs == NULL || rhs == NULL)
        return false;

	int equals = 1;
	if (lhs->datatype != rhs->datatype || lhs->degree != rhs->degree) return !equals;

	switch (lhs->datatype) {
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
	
	if (!equals) return equals;	
	
	int i;
	for (i = 0; i < lhs->degree; i++) {
		equals = _lrx_check_equals(lhs->children[i], rhs->children[i]);
		if (!equals) return equals;
	}
	
	return equals;
}
		
bool lrx_compare_tree(struct tree *lhs, struct tree *rhs, Comparator comparison) {
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
	
	switch (comparison) {
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

int lrx_get_degree(struct tree **t) {
    return (*t)->degree;
}