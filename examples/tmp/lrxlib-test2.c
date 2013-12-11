#include "lrxlib.h"

int main(int argc, char **argv)
{
        tree *t, *t1, *t2, *t3, *t4, *t5;

        t = lrx_declare_tree(_INT_, 2);
        t1 = lrx_declare_tree(_INT_, 2);
        t2 = lrx_declare_tree(_INT_, 2);
        t3 = lrx_declare_tree(_INT_, 2);
        t4 = lrx_declare_tree(_INT_, 2);
        t5 = lrx_declare_tree(_INT_, 2);

        int a, b, c, d, e, f;
        int *i, *j, *k, *l, *m, *n;

        a = 3; b = 4; c = 5; d = 6; e = 7; f = 15;
        i = &a; j = &b; k = &c; l = &d; m = &e; n = &f;

        lrx_define_tree(t, i, NULL);
        lrx_define_tree(t1, j, NULL);
        lrx_define_tree(t3, l, NULL);
        lrx_define_tree(t5, n, NULL);

        struct tree *children[2];
        children[0] = t;
        children[1] = t1;
        lrx_define_tree(t2, k, children);

        children[0] = t2;
        children[1] = t3;
        lrx_define_tree(t4, m, children);

        fprintf(stdout, "Here is a tree 7[5[3,4], 6]\n");
        lrx_print_tree(t4);
        fprintf(stdout, "\n\n");


        /*try accessing a child*/
        struct tree *child = lrx_access_child(t4, 0);
        fprintf(stdout, "Here is the 0th child of the above tree.\n");
        lrx_print_tree(child);
        fprintf(stdout, "\n\n");

        /*try assigning a child*/
        fprintf(stdout, "Assign a tree 15[] as 6's child.\n");
        child = lrx_access_child(t4, 1);
        lrx_assign_child(child,0,t5);
        lrx_print_tree(t4);
        fprintf(stdout, "\n\n");

        /*tree accessing data*/
        fprintf(stdout, "Accessing data at tree's root\n");
        int z = lrx_access_data_at_int(t4); 
        fprintf(stdout, "%d\n\n", z);

        /*try assigning data*/
        fprintf(stdout, "Change the original tree's root from 7 to 100.\n");
        lrx_assign_data_at_int(t4, 100);
        lrx_print_tree(t4);
        fprintf(stdout, "\n\n");



        //pass in root node, all children will be destroyed
        lrx_destroy_tree(t4);

        return 0;

}
