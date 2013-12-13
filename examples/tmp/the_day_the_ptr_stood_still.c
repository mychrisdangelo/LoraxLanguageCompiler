#include "lrxlib.h"
int main();

int main()
{
tree * t_1 = lrx_declare_tree(_INT_, 2); /* Ir_Decl */
tree * s_1 = lrx_declare_tree(_INT_, 2); /* Ir_Decl */
tree * __tmp_tree_datatype_int_degree_2_19 = lrx_declare_tree(_INT_, 2); /* Ir_Decl */
int __tmp_int_17 = 0; /* Ir_Decl */
int __tmp_int_18 = 0; /* Ir_Decl */
int __tmp_int_16 = 0; /* Ir_Decl */
tree * __tmp_tree_datatype_int_degree_2_25 = lrx_declare_tree(_INT_, 2); /* Ir_Decl */
tree * __tmp_tree_datatype_int_degree_2_22 = lrx_declare_tree(_INT_, 2); /* Ir_Decl */
tree * __tmp_tree_datatype_int_degree_2_7 = lrx_declare_tree(_INT_, 2); /* Ir_Decl */
int __tmp_int_5 = 0; /* Ir_Decl */
int __tmp_int_6 = 0; /* Ir_Decl */
int __tmp_int_4 = 0; /* Ir_Decl */
tree * __tmp_tree_datatype_int_degree_2_13 = lrx_declare_tree(_INT_, 2); /* Ir_Decl */
tree * __tmp_tree_datatype_int_degree_2_10 = lrx_declare_tree(_INT_, 2); /* Ir_Decl */
tree * __tmp_tree_datatype_int_degree_2_3 = lrx_declare_tree(_INT_, 2); /* Ir_Decl */
int __tmp_int_2 = 0; /* Ir_Decl */
int __tmp_int_1 = 0; /* Ir_Decl */
int __tmp_int_0 = 0; /* Ir_Decl */

__tmp_int_17 = 4;

__tmp_int_18 = 5;

__tmp_int_16 = 3;

int *__tmp_int_23 = &__tmp_int_17; /* Ir_Ptr */
tree * __tmp_tree_datatype_int_degree_2_24[2]; /* Ir_Leaf */
__tmp_tree_datatype_int_degree_2_24[1] = NULL; /* c_of_leaf */
__tmp_tree_datatype_int_degree_2_24[0] = NULL; /* c_of_leaf */

lrx_define_tree(__tmp_tree_datatype_int_degree_2_25, __tmp_int_23, __tmp_tree_datatype_int_degree_2_24);

int *__tmp_int_20 = &__tmp_int_18; /* Ir_Ptr */
tree * __tmp_tree_datatype_int_degree_2_21[2]; /* Ir_Leaf */
__tmp_tree_datatype_int_degree_2_21[1] = NULL; /* c_of_leaf */
__tmp_tree_datatype_int_degree_2_21[0] = NULL; /* c_of_leaf */

lrx_define_tree(__tmp_tree_datatype_int_degree_2_22, __tmp_int_20, __tmp_tree_datatype_int_degree_2_21);

tree * __tmp_tree_datatype_int_degree_2_26[2]; /* Ir_Child_Array */
__tmp_tree_datatype_int_degree_2_26[1] = NULL; /* c_of_leaf */
__tmp_tree_datatype_int_degree_2_26[0] = NULL; /* c_of_leaf */
/* Filling with NULL preemptively */
__tmp_tree_datatype_int_degree_2_26[0] = __tmp_tree_datatype_int_degree_2_25; /* Ir_Internal */
__tmp_tree_datatype_int_degree_2_26[1] = __tmp_tree_datatype_int_degree_2_22; /* Ir_Internal */
int *__tmp_int_27 = &__tmp_int_16; /* Ir_Ptr */
lrx_define_tree(__tmp_tree_datatype_int_degree_2_19, __tmp_int_27, __tmp_tree_datatype_int_degree_2_26);

lrx_assign_tree_direct(&t_1, &__tmp_tree_datatype_int_degree_2_19);

__tmp_int_5 = 7;

__tmp_int_6 = 8;

__tmp_int_4 = 6;

int *__tmp_int_11 = &__tmp_int_5; /* Ir_Ptr */
tree * __tmp_tree_datatype_int_degree_2_12[2]; /* Ir_Leaf */
__tmp_tree_datatype_int_degree_2_12[1] = NULL; /* c_of_leaf */
__tmp_tree_datatype_int_degree_2_12[0] = NULL; /* c_of_leaf */

lrx_define_tree(__tmp_tree_datatype_int_degree_2_13, __tmp_int_11, __tmp_tree_datatype_int_degree_2_12);

int *__tmp_int_8 = &__tmp_int_6; /* Ir_Ptr */
tree * __tmp_tree_datatype_int_degree_2_9[2]; /* Ir_Leaf */
__tmp_tree_datatype_int_degree_2_9[1] = NULL; /* c_of_leaf */
__tmp_tree_datatype_int_degree_2_9[0] = NULL; /* c_of_leaf */

lrx_define_tree(__tmp_tree_datatype_int_degree_2_10, __tmp_int_8, __tmp_tree_datatype_int_degree_2_9);

tree * __tmp_tree_datatype_int_degree_2_14[2]; /* Ir_Child_Array */
__tmp_tree_datatype_int_degree_2_14[1] = NULL; /* c_of_leaf */
__tmp_tree_datatype_int_degree_2_14[0] = NULL; /* c_of_leaf */
/* Filling with NULL preemptively */
__tmp_tree_datatype_int_degree_2_14[0] = __tmp_tree_datatype_int_degree_2_13; /* Ir_Internal */
__tmp_tree_datatype_int_degree_2_14[1] = __tmp_tree_datatype_int_degree_2_10; /* Ir_Internal */
int *__tmp_int_15 = &__tmp_int_4; /* Ir_Ptr */
lrx_define_tree(__tmp_tree_datatype_int_degree_2_7, __tmp_int_15, __tmp_tree_datatype_int_degree_2_14);

lrx_assign_tree_direct(&s_1, &__tmp_tree_datatype_int_degree_2_7);

__tmp_int_2 = 0;

tree ***childrenz = lrx_access_child(&t_1, __tmp_int_2);

lrx_assign_tree_direct(*childrenz, &s_1);

lrx_print_tree(t_1);

return __tmp_int_0;

}