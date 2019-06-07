/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrMemoryManager.h                                         *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This header file contains interface to memory manager.     *|
|*                                                                            *|
\******************************************************************************/

#ifndef _xdrMemoryManager_h
#define _xdrMemoryManager_h


#define xdrMM_AllocRec(type)             ((type*)(xdrMemoryManager_Alloc(sizeof(type))))

#define xdrMM_AllocArr(type, num)        ((type*)(xdrMemoryManager_Alloc(sizeof(type)*(num))))
#define xdrMM_ReAllocArr(type, ptr, num) ((type*)(xdrMemoryManager_ReAlloc((ptr), (sizeof(type)*(num)))))

#define xdrMM_Alloc(size)                xdrMemoryManager_Alloc(size)
#define xdrMM_ReAlloc(ptr, size)         xdrMemoryManager_ReAlloc((ptr), (size))

#define xdrMM_Free(ptr)                  xdrMemoryManager_Free(ptr)

/*
#define xdrMM_Mark(ptr)                  xdrMemoryManager_Mark(ptr)
#define xdrMM_UnMark(ptr)                xdrMemoryManager_UnMark(ptr)
*/

extern void * xdrMemoryManager_Alloc   (int size);
extern void * xdrMemoryManager_ReAlloc (void * ptr, int size);
extern void   xdrMemoryManager_Free    (void * ptr);

/*
extern void   xdrMemoryManager_Mark    (void * ptr);
extern void   xdrMemoryManager_UnMark  (void * ptr);
*/

#endif