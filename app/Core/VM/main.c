#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int runVM(void** bytecode, int lengthb, void** constants, int lengthc) {
  printf("%d\n", lengthb);
  for (int i = 0; i < lengthb; i++) {
    int op = ((int**)bytecode)[i][0];
    int arg = ((int**)bytecode)[i][1];
    printf("%d: %d\n", op, arg);
  }
  
  for (int i = 0; i < lengthc; i++) {
    char* c = ((char**)constants)[i];
    printf("%s\n", c);
  }
  return 0;
} 
