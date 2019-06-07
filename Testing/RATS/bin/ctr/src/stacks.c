/***    Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
     LIFO  Alexs: 11-Nov-96
*/

#include "ctAssert.h"
#include "ctMemory.h"
#include "stacks.h"


static char * inv_magic  = "\"Invalid magic\"";
static char * null_stack = "\"NULL stack\"";


typedef struct stack_node_t{
  struct stack_node_t * down;
  void * data;
} stack_node_t;

struct Stack_t{
  long magic;
  stack_node_t * down;
};


/*------------------------------------------------------*/
Stack_t * Stack_Create( long magic ){
  Stack_t * s;

  s = allocate( Stack_t );
  s->magic = magic;
  return s;
};

/*------------------------------------------------------*/
void Stack_Push( Stack_t * s, long magic, void * data ){
  stack_node_t * n = NULL;

  ASSERTM( s->magic == magic, inv_magic );
  ASSERTM( s, null_stack );

  if (!data) return;

  n = allocate( stack_node_t );
  n->data = data;
  n->down = s->down;
  s->down = n;
};

/*------------------------------------------------------*/
void * Stack_Pop( Stack_t * s, long magic ){
  void         * data = NULL;
  stack_node_t * n    = NULL;

  ASSERTM( s->magic == magic, inv_magic );
  ASSERTM( s, null_stack );

  if ( s->down ){
    n       = s->down;
    data    = n->data;
    s->down = n->down;
  };
  return data;
};

/*------------------------------------------------------*/
void * Stack_Top( Stack_t * s, long magic ){

  ASSERTM( s->magic == magic, inv_magic );
  ASSERTM( s, null_stack );
  return ( s->down ) ? s->down->data : NULL;
};

/*------------------------------------------------------*/
void Stack_Destroy( Stack_t * s, long magic ){

  ASSERTM( s->magic == magic, inv_magic );
  ASSERTM( s, null_stack );

  while( Stack_Pop(s, magic) );
};
