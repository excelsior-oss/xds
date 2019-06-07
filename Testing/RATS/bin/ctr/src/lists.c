/***    Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
     FIFO  Den: 10-Aug-96
*/

#include <stdlib.h>

#include "lists.h"
#include "ctAssert.h"
#include "ctMemory.h"

static char * inv_magic = "\"Invalid magic\"";
static char * inv_state = "\"Invalid state\"";
static char * null_list = "\"NULL list\"";


struct list_node_t
{
  struct list_node_t * next;
  void * data;
};

typedef struct list_node_t list_node_t;


struct List_t
{
  long magic;
  list_node_t * first;
  list_node_t * last;
  list_node_t * curr;
};



List_t * List_Create ( long magic )
{
  List_t * l;
  l = allocate(  List_t );
  l->first = 0;
  l->last = 0;
  l->curr = 0;
  l->magic = magic;
  return l;
}

void List_Destroy ( List_t * l, long magic )
{
  ASSERTM ( l, null_list );
  ASSERTM ( l->magic == magic, inv_magic );
}

void List_Insert ( List_t * l, long magic, void * data )
{
  list_node_t * n;

  ASSERTM ( l, null_list );
  ASSERTM ( l->magic == magic, inv_magic );
  n = allocate( list_node_t );
  n->data = data;
  n->next = 0;
  if ( ! ( l->last ) )
  {
    l->first = l->last = l->curr = n;
  }
  else
  {
    l->last->next = n;
    l->last = n;
  }
}

void * List_GetFirst( List_t * l, long magic )
{
  ASSERTM ( l, null_list );
  ASSERTM ( l->magic == magic, inv_magic );
  l->curr = l->first;
  return l->curr ? l->curr->data : 0 ;
}

void * List_GetNext( List_t * l, long magic )
{
  ASSERTM ( l, null_list );
  ASSERTM ( l->magic == magic, inv_magic );
  l->curr = l->curr ? l->curr->next : 0;
  return l->curr ? l->curr->data : 0 ;
}



long List_SaveState ( List_t * l, long magic ){
  list_node_t * n;
  long state = 0;

  ASSERTM ( l, null_list );
  ASSERTM ( l->magic == magic, inv_magic );

  if ( l->curr ){
    state = 1;
    n = l->first;
    while ( n != l->curr ){
      state++;
      n = n->next;
    };
  };
  return state;
};

void List_RestoreState ( List_t * l, long magic, long state ){

  ASSERTM ( l, null_list );
  ASSERTM ( l->magic == magic, inv_magic );
  ASSERTM ( state >= 0, inv_state );

  if ( state ){
    l->curr = l->first;
    state--;
    while ( l->curr && state-- ) l->curr = l->curr->next;
    ASSERTM ( l->curr, inv_state );
  }else{
    l->curr = NULL;
  };
};

