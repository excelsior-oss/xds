/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrMemoryManager.c                                         *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This file contains implementation of memory manager.       *|
|*                                                                            *|
\******************************************************************************/

#include <stdlib.h>
#include <string.h>

#include "xdrIncl.h"
#include "xdrTrace.h"
#include "xdrMemoryManager.h"



void * xdrMemoryManager_Alloc   (int size){
  void * ptr;

  if((ptr = malloc(size)) == NULL){
    perror("malloc");
    for(;;);
  };

  return memset(ptr, 0, size);
};



void * xdrMemoryManager_ReAlloc (void * ptr, int size){
  void * new;
  if((new = realloc(ptr, size)) == NULL){
    perror("realloc");
    for(;;);
  };
};


void   xdrMemoryManager_Free    (void * ptr){
  free(ptr);
};

