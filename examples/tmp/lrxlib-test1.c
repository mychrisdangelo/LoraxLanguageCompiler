#include "lrxlib.h"

int main(int argc, char **argv)
{
        tree *t, *t1, *t2;

        t = lrx_declare_tree(_INT_, 2);
        t1 = lrx_declare_tree(_INT_, 2);
        t2 = lrx_declare_tree(_INT_, 2);

        int a, b, c;
        int *i, *j, *k;

        a = 3; b = 4; c = 5;
        i = &a; j = &b; k = &c;

        lrx_define_tree(t, i, NULL);
        lrx_define_tree(t1, j, NULL);

        struct tree *children[2];// = malloc(sizeof(struct tree *) * 2);
        children[0] = t;
        children[1] = t1;

        lrx_define_tree(t2, k, children);

        lrx_print_tree(t2);

        lrx_destroy_tree(t2);

        //free(children);

        return 0;

}
