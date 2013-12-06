#include "lrxlib.h"
#include <stdint.h>

int main() {

  int16_t buff1[5] = { 1, 2, 3, 4 , 5};

  float buff2[5] = {1.5, 2.5, 3.5, 4.5, 5.5 };

  char buff3[5] = {'h','e','l','l','o'};

  char buff4[5] = {'w','o','r','l','d'};

  short buff5[5] = {TRUE, TRUE, TRUE, FALSE, TRUE };

  struct lrx_tree *tree1 = construct_tree( 2, INT );
  set_tree( tree1, buff1, 5 );

  struct lrx_tree *tree2 = construct_tree( 2, FLOAT );
  set_tree( tree2, buff2, 5 );

  struct lrx_tree *tree3 = construct_tree( 2, CHAR );
  set_tree( tree3, buff3, 5 );

  struct lrx_tree *tree4 = construct_tree( 2, STRING );
  set_tree( tree4, buff4, 5 );

  struct lrx_tree *tree5 = construct_tree( 2, BOOL );
  set_tree( tree5, buff5, 5 );

  printf("Printing int: ");
  print_tree( tree1 );
  printf("\n");

  printf("Printing float: ");
  print_tree( tree2 );
  printf("\n");

  printf("printing char: ");
  print_tree( tree3 );
  printf("\n");

  printf("printing string: ");
  print_tree( tree4 );
  printf("\n");

  printf("printing bool: ");
  print_tree( tree5 );
  printf("\n");

  return 0;
}
