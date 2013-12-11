#include "lrxlib.h"
int main();

int main()
{
int i_2 = 0;
int j_2 = 0;
int __tmp_int_1 = 0;
int __tmp_int_7 = 0;
int __tmp_int_6 = 0;
int __tmp_int_5 = 0;
int __tmp_int_4 = 0;
bool __tmp_bool_3 = false;
int __tmp_int_2 = 0;
int __tmp_int_0 = 0;

__tmp_int_1 = 0;

i_2 = __tmp_int_1;

goto __LABEL_1;
__LABEL_0:
fprintf(stdout, "%d", j_2);

j_2 = i_2;

fprintf(stdout, "%d", j_2);

__tmp_int_4 = 1;

__tmp_int_5 = i_2 + __tmp_int_4;

i_2 = __tmp_int_5;

__LABEL_1:
__tmp_int_2 = 3;

__tmp_bool_3 = i_2 < __tmp_int_2;

if(__tmp_bool_3) goto __LABEL_0;
return __tmp_int_0;

}