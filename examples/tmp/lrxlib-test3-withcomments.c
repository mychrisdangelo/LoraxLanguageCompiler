// Lorax Program 
//
// int main()
// {
// 	tree <char>t(2);
// 	t = 'a'['b', 'c'];
// 	print(t);
// }

//
// Compiler Output
//
#include "lrxlib.h"
int main();

int main()
{
tree * t_1 = lrx_declare_tree(_CHAR_, 2); /* Ir_Decl */
tree * __tmp_tree_datatype_char_degree_2_5 = lrx_declare_tree(_CHAR_, 2); /* Ir_Decl */
char __tmp_char_3 = '\0'; /* Ir_Decl */
char __tmp_char_4 = '\0'; /* Ir_Decl */
char __tmp_char_2 = '\0'; /* Ir_Decl */
tree * __tmp_tree_datatype_char_degree_2_8 = lrx_declare_tree(_CHAR_, 2); /* Ir_Decl */
tree * __tmp_tree_datatype_char_degree_2_11 = lrx_declare_tree(_CHAR_, 2); /* Ir_Decl */
int __tmp_int_1 = 0; /* Ir_Decl */
int __tmp_int_0 = 0; /* Ir_Decl */

__tmp_char_3 = 'b';
__tmp_char_4 = 'c';
__tmp_char_2 = 'a';

char *__tmp_char_6 = &__tmp_char_4; /* Ir_Ptr */
tree * __tmp_tree_datatype_char_degree_2_7[2]; /* Ir_Leaf */
__tmp_tree_datatype_char_degree_2_7[1] = NULL; /* Ir_Leaf */
__tmp_tree_datatype_char_degree_2_7[0] = NULL; /* Ir_Leaf */

lrx_define_tree(__tmp_tree_datatype_char_degree_2_8, __tmp_char_6, __tmp_tree_datatype_char_degree_2_7) /* Ir_Tree_Literal */;

char *__tmp_char_9 = &__tmp_char_3; /* Ir_Ptr */
tree * __tmp_tree_datatype_char_degree_2_10[2]; /* Ir_Leaf */
__tmp_tree_datatype_char_degree_2_10[1] = NULL; /* Ir_Leaf */
__tmp_tree_datatype_char_degree_2_10[0] = NULL; /* Ir_Leaf */

lrx_define_tree(__tmp_tree_datatype_char_degree_2_11, __tmp_char_9, __tmp_tree_datatype_char_degree_2_10) /* Ir_Tree_Literal */;

tree * __tmp_tree_datatype_char_degree_2_12[2]; /* Ir_Child_Array */
__tmp_tree_datatype_char_degree_2_12[0] = __tmp_tree_datatype_char_degree_2_8; /* Ir_Internal */
__tmp_tree_datatype_char_degree_2_12[1] = __tmp_tree_datatype_char_degree_2_11; /* Ir_Internal */
char *__tmp_char_13 = &__tmp_char_2; /* Ir_Ptr */
lrx_define_tree(__tmp_tree_datatype_char_degree_2_5, __tmp_char_13, __tmp_tree_datatype_char_degree_2_12) /* Ir_Tree_Literal */;

lrx_assign_tree_direct(&t_1, &__tmp_tree_datatype_char_degree_2_5);

lrx_print_tree(t_1);

return __tmp_int_0;

}
