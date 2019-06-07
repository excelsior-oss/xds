/***    Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
     FIFO  Den: 10-Aug-96
*/

#ifndef __lists_h
#define __lists_h


typedef struct List_t List_t;  /* hidden type */


extern List_t * List_Create ( long magic );
extern void List_Destroy ( List_t * l, long magic );

extern void List_Insert ( List_t * l, long magic, void * data );

extern void * List_GetFirst     ( List_t * l, long magic );
extern void * List_GetNext      ( List_t * l, long magic );
extern long   List_SaveState    ( List_t * l, long magic );
extern void   List_RestoreState ( List_t * l, long magic, long state );


#endif /* __lists_h */
