#include "lrxlib.h"
int main();

int main()
{
tree * t_1 = lrx_declare_tree(_CHAR_, 2);
tree * __tmp_tree_datatype_char_degree_2_32 = lrx_declare_tree(_CHAR_, 2);
tree * __tmp_tree_datatype_char_degree_2_25 = lrx_declare_tree(_CHAR_, 2);
char __tmp_char_4 = '\0';
tree * __tmp_tree_datatype_char_degree_2_19 = lrx_declare_tree(_CHAR_, 2);
tree * __tmp_tree_datatype_char_degree_2_9 = lrx_declare_tree(_CHAR_, 2);
char __tmp_char_7 = '\0';
char __tmp_char_8 = '\0';
char __tmp_char_6 = '\0';
tree * __tmp_tree_datatype_char_degree_2_15 = lrx_declare_tree(_CHAR_, 2);
tree * __tmp_tree_datatype_char_degree_2_12 = lrx_declare_tree(_CHAR_, 2);
char __tmp_char_18 = '\0';
char __tmp_char_5 = '\0';
tree * __tmp_tree_datatype_char_degree_2_22 = lrx_declare_tree(_CHAR_, 2);
char __tmp_char_3 = '\0';
tree * __tmp_tree_datatype_char_degree_2_28 = lrx_declare_tree(_CHAR_, 2);
char __tmp_char_31 = '\0';
char __tmp_char_2 = '\0';
tree * __tmp_tree_datatype_char_degree_2_35 = lrx_declare_tree(_CHAR_, 2);
int __tmp_int_1 = 0;
int __tmp_int_0 = 0;

__tmp_char_4 = 'c';

__tmp_char_7 = 'f';

__tmp_char_8 = 'g';

__tmp_char_6 = 'e';

char *__tmp_char_13 = &__tmp_char_7;
tree * __tmp_tree_datatype_char_degree_2_14[2];
__tmp_tree_datatype_char_degree_2_14[1] = NULL;
__tmp_tree_datatype_char_degree_2_14[0] = NULL;

lrx_define_tree(__tmp_tree_datatype_char_degree_2_15, __tmp_char_13, __tmp_tree_datatype_char_degree_2_14);

char *__tmp_char_10 = &__tmp_char_8;
tree * __tmp_tree_datatype_char_degree_2_11[2];
__tmp_tree_datatype_char_degree_2_11[1] = NULL;
__tmp_tree_datatype_char_degree_2_11[0] = NULL;

lrx_define_tree(__tmp_tree_datatype_char_degree_2_12, __tmp_char_10, __tmp_tree_datatype_char_degree_2_11);

tree * __tmp_tree_datatype_char_degree_2_16[2];
__tmp_tree_datatype_char_degree_2_16[0] = __tmp_tree_datatype_char_degree_2_15;
__tmp_tree_datatype_char_degree_2_16[1] = __tmp_tree_datatype_char_degree_2_12;
char *__tmp_char_17 = &__tmp_char_6;
lrx_define_tree(__tmp_tree_datatype_char_degree_2_9, __tmp_char_17, __tmp_tree_datatype_char_degree_2_16);

__tmp_char_18 = 'h';

__tmp_char_5 = 'd';

char *__tmp_char_20 = &__tmp_char_18;
tree * __tmp_tree_datatype_char_degree_2_21[2];
__tmp_tree_datatype_char_degree_2_21[1] = NULL;
__tmp_tree_datatype_char_degree_2_21[0] = NULL;

lrx_define_tree(__tmp_tree_datatype_char_degree_2_22, __tmp_char_20, __tmp_tree_datatype_char_degree_2_21);

tree * __tmp_tree_datatype_char_degree_2_23[2];
__tmp_tree_datatype_char_degree_2_23[0] = __tmp_tree_datatype_char_degree_2_9;
__tmp_tree_datatype_char_degree_2_23[1] = __tmp_tree_datatype_char_degree_2_22;
char *__tmp_char_24 = &__tmp_char_5;
lrx_define_tree(__tmp_tree_datatype_char_degree_2_19, __tmp_char_24, __tmp_tree_datatype_char_degree_2_23);

__tmp_char_3 = 'b';

char *__tmp_char_26 = &__tmp_char_4;
tree * __tmp_tree_datatype_char_degree_2_27[2];
__tmp_tree_datatype_char_degree_2_27[1] = NULL;
__tmp_tree_datatype_char_degree_2_27[0] = NULL;

lrx_define_tree(__tmp_tree_datatype_char_degree_2_28, __tmp_char_26, __tmp_tree_datatype_char_degree_2_27);

tree * __tmp_tree_datatype_char_degree_2_29[2];
__tmp_tree_datatype_char_degree_2_29[0] = __tmp_tree_datatype_char_degree_2_28;
__tmp_tree_datatype_char_degree_2_29[1] = __tmp_tree_datatype_char_degree_2_19;
char *__tmp_char_30 = &__tmp_char_3;
lrx_define_tree(__tmp_tree_datatype_char_degree_2_25, __tmp_char_30, __tmp_tree_datatype_char_degree_2_29);

__tmp_char_31 = 'i';

__tmp_char_2 = 'a';

char *__tmp_char_33 = &__tmp_char_31;
tree * __tmp_tree_datatype_char_degree_2_34[2];
__tmp_tree_datatype_char_degree_2_34[1] = NULL;
__tmp_tree_datatype_char_degree_2_34[0] = NULL;

lrx_define_tree(__tmp_tree_datatype_char_degree_2_35, __tmp_char_33, __tmp_tree_datatype_char_degree_2_34);

tree * __tmp_tree_datatype_char_degree_2_36[2];
__tmp_tree_datatype_char_degree_2_36[0] = __tmp_tree_datatype_char_degree_2_25;
__tmp_tree_datatype_char_degree_2_36[1] = __tmp_tree_datatype_char_degree_2_35;
char *__tmp_char_37 = &__tmp_char_2;
lrx_define_tree(__tmp_tree_datatype_char_degree_2_32, __tmp_char_37, __tmp_tree_datatype_char_degree_2_36);

lrx_assign_tree_direct(&t_1, &__tmp_tree_datatype_char_degree_2_32);

lrx_print_tree(t_1);

return __tmp_int_0;

}