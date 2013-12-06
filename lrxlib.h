/*
 * Authors:
 * Kira Whitehouse
 * Chris D'Angelo
 * Doug Bienstock
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#define false 0
#define true !false

typedef enum {
  _BOOL_,
  _INT_,
  _FLOAT_,
  _CHAR_
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

      bool leaf;
} tree;

int lrx_print_bool(bool b) {
  if (b) {
    fprintf(stderr, "true");
  } else {
    fprintf(stderr, "false");
  }
  return 0;
}

// TODO: the following functions are not implemented
int lrx_print_tree(struct tree *t) {
  if(t == NULL){
    fprintf(stderr, "this should be IMPOSSIBLE!");
    return -1;
  }

  switch (t->datatype){
    case _INT_:
      fprintf(stderr, "%d ", (int)(t->root.int_root));
      
      if(!t->leaf){
        int i;
        for(i = 0; i < t->degree; ++i){
            //fprintf(stderr, "%d\n", t->children[i]->root.int_root);

            lrx_print_tree(t->children[i]);
            //fprintf(stderr, "recursive print\n");
        }
      }
      break;
      default:
      printf("default");

  }
  return 0;

}

void lrx_destroy_tree(struct tree *t)
{
    if(t == NULL){
      fprintf(stderr, "t is null?!?!");
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

struct tree *lrx_declare_tree(Atom type, int deg){

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
          fprintf(stderr, "root: %d\n", t->root.int_root);

          break;

        case _FLOAT_:
          t->root.float_root = 0.0;
          break;
  
        case _CHAR_:
          t->root.char_root = '\0';
          break;
    }


    t->leaf = true;
    t->children = (struct tree **)malloc(sizeof(struct tree *) * t->degree);
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
          t->root.int_root = *((int *)root_data);
          printf("define root: %d\n", t->root.int_root);
          break;

        case _FLOAT_:
          t->root.float_root = *((float *)root_data);
          break;
  
        case _CHAR_:
          t->root.char_root = *((char *)root_data);
          break;
    }

    if(children == NULL)
      return t;

    /* set pointers to children */
    int num_children = t->degree;
    int i;
    for(i = 0; i < num_children; ++i)
        t->children[i] = children[i]; 
    t->leaf = false;  
    fprintf(stderr, "we are no longer a leaf\n");

    return t;
}

/* data = t@; */
bool lrx_access_data_at_bool (struct tree *t)
{
  return false;
}
int lrx_access_data_at_int (struct tree *t)
{
  return 0;
}
float lrx_access_data_at_float (struct tree *t)
{
  return 0.0;
}
char lrx_access_data_at_char (struct tree *t)
{
  return '\0';
}

/* t@ = data */
bool lrx_assign_data_at_bool (struct tree *t, int at)
{
  return false;
}
int lrx_assign_data_at_int (struct tree *t, int at)
{
  return 0;
}
float lrx_assign_data_at_float (struct tree *t, int at)
{
  return 0.0;
}
char lrx_assign_data_at_char (struct tree *t, int at)
{
  return '\0';
}

/* t1 = t2%0 */
struct tree *lrx_access_child (struct tree *t, int child)
{
  assert(child < t->degree);
  struct tree **children = t->children;
  return *(children + child);
}

/* t1 = t2%0 */
struct tree *lrx_assign_tree (struct tree *t1, struct tree *t2)
{
  assert(t1->degree == t2->degree);
  t1 = t2;
  return t1;
}











