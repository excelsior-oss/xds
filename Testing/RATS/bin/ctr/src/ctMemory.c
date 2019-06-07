/***    Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   Memory manager for CTRout(CityRout) - Compiler Testing Routine
                                                 Alexs: 11-Nov-96
*/

#include "ctAssert.h"
#include <stdlib.h>

#define STARTMANAGERNUMBER 0


typedef struct mem_node_t{
  void * ptr;
  struct mem_node_t * next;
} mem_node_t;

typedef struct man_node_t{
  long id;
  struct man_node_t * next;
} man_node_t;


static man_node_t   * managers = NULL;
static mem_node_t   * blocks   = NULL;
static mem_node_t   * blocks_tail   = NULL;
static long           man_id   = STARTMANAGERNUMBER;

long Memory_Init(void){
  man_node_t * m = NULL;

  if ( managers ){
    m = managers;
    while( m->next ) m = m->next;
    m->next = (man_node_t*) malloc ( sizeof(man_node_t) );
    m->next->id = ++man_id;
    m->next->next = NULL;
    m = m->next;
  }else{
    m = (man_node_t*) malloc (sizeof(man_node_t));
    m->id = ++man_id;
    m->next = NULL;
    managers = m;
  };
  return m->id;
};


void * Memory_allocate(long size){
  void * mem;
  mem_node_t * m;
  int count =0;

  mem = malloc( size );
  while ( (((char *)mem)[--size] = 0), size );

  if ( blocks ){
	m = blocks_tail;
	m->next = (mem_node_t*) malloc ( sizeof(*(m->next)) );
    m->next->ptr = mem;
    m->next->next = NULL;
	blocks_tail = blocks_tail->next;
  }else{
    blocks = (mem_node_t*) malloc ( sizeof(*blocks) );
    blocks->ptr = mem;
    blocks_tail = mem;
    blocks->next = NULL;
  };
  return mem;
};


void Memory_Final(long id){
  man_node_t * m   = NULL, * m1;
  mem_node_t * mem = NULL, * mem1;
  long         collect = 1;

  ASSERT(managers);
  m = managers;
  while( m && m->id != id ) m = m->next;
  ASSERT( m );
  m->id = STARTMANAGERNUMBER;
  m = managers;
  while( m ){
    collect = collect && m->id == STARTMANAGERNUMBER;
    m = m->next;
  };
  if ( collect ){
    m = managers; mem = blocks; man_id = STARTMANAGERNUMBER;
    managers = NULL; blocks = NULL;
    m1 = m;
    while( m1 ){
      m = m1;
      m1 = m1->next;
      free(m);
    };

    mem1 = mem;
    while( mem1 ){
      mem = mem1;
      mem1 = mem1->next;
      free(mem->ptr);
      free(mem);
    };
  };
};
