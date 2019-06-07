#include <stdlib.h>
#include "rcsup.h"
#include "rcerr.h"

void *Malloc(int size)
{
void *p;
   if (!(p=malloc(size)))
      fatal_error(MSG_ALLOC);
   return p;
}

void *Realloc(void *ptr,int size)
{
void *p;
   if (!(p=realloc(ptr,size)))
      fatal_error(MSG_ALLOC);
   return p;
}

void Free(void *ptr)
{
   free(ptr);
}
