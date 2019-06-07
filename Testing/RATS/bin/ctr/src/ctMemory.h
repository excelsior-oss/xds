/***    Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   Memory manager for CTRout(CityRout) - Compiler Testing Routine
                                                 Alexs: 11-Nov-96
*/
#ifndef __ctMemory_h
#define __ctMemory_h

#include <stdlib.h>

#define allocate(type)        ((type*)Memory_allocate(sizeof(type)))
#define _allocate(type, size)  ((type*)Memory_allocate(size))

extern long   Memory_Init(void);
extern void * Memory_allocate(long size);
extern void   Memory_Final(long id);


#endif /* __ctMemory_h */
