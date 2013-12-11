#include "lrxlib.h"
int main();

int main()
{
tree * t_1 = lrx_declare_tree(_INT_, 1);
tree * __tmp_tree_datatype_int_degree_0_3 = lrx_declare_tree(_INT_, 0);
int __tmp_int_2 = 0;
int __tmp_int_1 = 0;
int __tmp_int_0 = 0;

__tmp_int_2 = 5;

tree * __tmp_tree_datatype_int_degree_0_4[0];
int *__tmp_int_5 = &__tmp_int_2;
lrx_define_tree(__tmp_tree_datatype_int_degree_0_3, __tmp_int_5, __tmp_tree_datatype_int_degree_0_4);

lrx_assign_tree_direct(&t_1, &__tmp_tree_datatype_int_degree_0_3);

lrx_print_tree(t_1);

return __tmp_int_0;

}