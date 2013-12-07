#include "lrxlib.h"
int main();

int main()
{
tree * t_1 = lrx_declare_tree(_CHAR_, 2);
tree * __tmp_tree_datatype_char_degree_2_16 = lrx_declare_tree(_CHAR_, 2);
tree * __tmp_tree_datatype_char_degree_2_6 = lrx_declare_tree(_CHAR_, 2);
char __tmp_char_4 = '\0';
char __tmp_char_5 = '\0';
char __tmp_char_3 = '\0';
tree * __tmp_tree_datatype_char_degree_2_9 = lrx_declare_tree(_CHAR_, 2);
tree * __tmp_tree_datatype_char_degree_2_12 = lrx_declare_tree(_CHAR_, 2);
char __tmp_char_15 = '\0';
char __tmp_char_2 = '\0';
tree * __tmp_tree_datatype_char_degree_2_19 = lrx_declare_tree(_CHAR_, 2);
int __tmp_int_1 = 0;
int __tmp_int_0 = 0;

__tmp_char_4 = 'j';

__tmp_char_5 = 'o';

__tmp_char_3 = 'i';

char *__tmp_char_7 = &__tmp_char_5;
tree * __tmp_tree_datatype_char_degree_2_8[2];
__tmp_tree_datatype_char_degree_2_8[1] = NULL;
__tmp_tree_datatype_char_degree_2_8[0] = NULL;

lrx_define_tree(__tmp_tree_datatype_char_degree_2_9, __tmp_char_7, __tmp_tree_datatype_char_degree_2_8);

char *__tmp_char_10 = &__tmp_char_4;
tree * __tmp_tree_datatype_char_degree_2_11[2];
__tmp_tree_datatype_char_degree_2_11[1] = NULL;
__tmp_tree_datatype_char_degree_2_11[0] = NULL;

lrx_define_tree(__tmp_tree_datatype_char_degree_2_12, __tmp_char_10, __tmp_tree_datatype_char_degree_2_11);

tree * __tmp_tree_datatype_char_degree_2_13[2];
__tmp_tree_datatype_char_degree_2_13[0] = __tmp_tree_datatype_char_degree_2_9;
__tmp_tree_datatype_char_degree_2_13[1] = __tmp_tree_datatype_char_degree_2_12;
char *__tmp_char_14 = &__tmp_char_3;
lrx_define_tree(__tmp_tree_datatype_char_degree_2_6, __tmp_char_14, __tmp_tree_datatype_char_degree_2_13);

__tmp_char_15 = 'k';

__tmp_char_2 = 'h';

char *__tmp_char_17 = &__tmp_char_15;
tree * __tmp_tree_datatype_char_degree_2_18[2];
__tmp_tree_datatype_char_degree_2_18[1] = NULL;
__tmp_tree_datatype_char_degree_2_18[0] = NULL;

lrx_define_tree(__tmp_tree_datatype_char_degree_2_19, __tmp_char_17, __tmp_tree_datatype_char_degree_2_18);

tree * __tmp_tree_datatype_char_degree_2_20[2];
__tmp_tree_datatype_char_degree_2_20[0] = __tmp_tree_datatype_char_degree_2_6;
__tmp_tree_datatype_char_degree_2_20[1] = __tmp_tree_datatype_char_degree_2_19;
char *__tmp_char_21 = &__tmp_char_2;
lrx_define_tree(__tmp_tree_datatype_char_degree_2_16, __tmp_char_21, __tmp_tree_datatype_char_degree_2_20);

lrx_assign_tree_direct( &t_1, &__tmp_tree_datatype_char_degree_2_16);

lrx_print_tree(t_1);
lrx_destroy_tree(t_1);

return __tmp_int_0;

}
