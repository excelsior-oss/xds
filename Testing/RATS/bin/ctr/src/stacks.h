/***    Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
     LIFO  Alexs: 11-Nov-96
*/

#ifndef __stacks_h
#define __stacks_h


typedef struct Stack_t Stack_t;  /* hidden type */


extern Stack_t  *   Stack_Create    ( long magic );
extern void         Stack_Destroy   ( Stack_t * s, long magic );

extern void         Stack_Push      ( Stack_t * s, long magic, void * data );
extern void     *   Stack_Pop       ( Stack_t * s, long magic );
extern void     *   Stack_Top       ( Stack_t * s, long magic );

#endif /* __stacks_h */
