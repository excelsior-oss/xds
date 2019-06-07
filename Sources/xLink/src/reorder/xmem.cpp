
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "xdefs.h"

open_namespace

#include "messages.h"
#include "xmem.h"

/*----------------------------------------------------------------------------*/

static unsigned long MemoryUsed    = 0;
static unsigned long Overhead      = 0;


/*----------------------------------------------------------------------------*/

static void* reserveMem = NULL;
static int reserveSize = 0;

Bool reserveMemory(int size) {
    if (reserveMem != NULL) {
        return false;
    }
    reserveMem = malloc(size);
    if (reserveMem == NULL) {
        return false;
    }
    reserveSize = size;
    return true;
}

/*----------------------------------------------------------------------------*/

#if (defined(xos_LINUX) && defined(SHARED_LIBRARY)) || defined(HEAP_DUMP)

void * operator new(size_t n) { 
  return xalloc(n); 
}

void operator delete(void* p) { 
  return xfree(p); 
}

void * operator new[] (size_t n) { 
  return xalloc(n); 
}

void operator delete[] (void* p) { 
  return xfree(p); 
}

#endif

/*----------------------------------------------------------------------------*/


#if defined(xos_LINUX) && defined(SHARED_LIBRARY)


// in Linux, the memory allocated in a shared library is not freed
// when the library is unloaded

// therefore, to prevent memory leaks without an explicit deallocation
// we support a double-linked list of references to allocated objects

// when the library is unloaded, we call the routine free_heap() which 
// uses that list to free all the memory occupied by these objects


/*----------------------------------------------------------------------------*/


typedef struct block_list {
    block_list * next, *prev;
} block_list;


static block_list list_head_obj = {
    &list_head_obj,
    &list_head_obj
};


static const int block_list_size = sizeof(struct block_list);


static block_list *list_head = &list_head_obj;


#ifdef DEBUG
static int malloc_count = 0;
#endif


void* xalloc (int _n)
{
    if(!_n)
        return NULL;

    int n = _n + block_list_size;

    block_list *block = (block_list *)malloc(n);

    if(!block)
        Message(xFATAL, msgINSUFFICIENT_MEMORY);

#ifdef DEBUG
    MemoryUsed += n;
    if(nn >= 65536)
        printf("%d bytes allocated, %d bytes memory used\n", n, MemoryUsed);
#endif

    block->next = list_head->next;
    block->prev = list_head;

    list_head->next->prev = block;
    list_head->next = block;

#ifdef DEBUG
    malloc_count++;
#endif

    return (void *)(((char *)block) + block_list_size);
}


void xfree (void *p)
{
    if(!p)
        return;

    block_list *block = (block_list *)(((char *)p) - block_list_size);

    ASSERT((block != NULL) && (block != list_head));

    block->next->prev = block->prev;
    block->prev->next = block->next;

    free(block);

#ifdef DEBUG
    malloc_count--;
#endif
}


void free_heap() {
    block_list *block = list_head->next;

    while(block != list_head) {
      block_list *dying = block;

      block = block->next;
      free(dying);

#ifdef DEBUG
      malloc_count--;
#endif
    }

    list_head->next = list_head;
    list_head->prev = list_head;

#ifdef DEBUG
    if(malloc_count != 0) {
      printf("INTERNAL ERROR: malloc_count=%d\n", malloc_count);
      ASSERT(0);
    }
#endif
}


#else // defined(xos_LINUX) && defined(SHARED_LIBRARY)

/*----------------------------------------------------------------------------*/

#ifdef HEAP_DUMP

typedef struct _block_header { 
  int size; 
  const char *file; int line;
  _block_header *prev; 
  _block_header *next;  
} block_header;

static block_header *heap = 0;

static void dump_mem(int d = 0) {
  printf("%ld (overhead %ld) bytes memory used\n", MemoryUsed, Overhead);

  if(d) {
    for(; heap; heap = heap->next) {
      printf("%s:%d %d\n", heap->file, heap->line, heap->size);
    }
  }
}


void* xallocImpl (int n, const char *file, int line)
{
    void * p;

    if (! n) return NULL;
    
    p = malloc (sizeof(block_header) + n);
    if (!p && (reserveMem != NULL)) {
        printf("Using reserved %d bytes for allocation of %d bytes\n", reserveSize, n);
        if ((sizeof(block_header) + n) <= reserveSize) {
            p = reserveMem;
        } else {
            free(reserveMem);
            p = malloc (sizeof(block_header) + n);
        }
        reserveMem = NULL;
    }

    if (! p) { 
      dump_mem(1);
      printf("Failed to allocate %d bytes in %s:%d\n", n, file, line);
      Message(xFATAL, msgINSUFFICIENT_MEMORY);
    }

    MemoryUsed += sizeof(block_header) + n;
    Overhead   += sizeof(block_header);

    //printf("%d: bytes allocated, ", n);
    //dump_mem();

    block_header* header = (block_header*)p;
                        
    header->size = n;
    header->file = file;
    header->line = line;
    header->next = heap;
    header->prev = 0;
    if(heap) heap->prev = header;
    heap = header;

    return header + 1; 
}


void xfree (void * p)
{
    if(p) {      
      block_header *header = ((block_header*)p) - 1;

      if(header->prev) header->prev->next = header->next; else heap = header->next;
      if(header->next) header->next->prev = header->prev;

      MemoryUsed -= sizeof(block_header) + header->size;
      Overhead   -= sizeof(block_header);

      free (header);
    }    
}

/*----------------------------------------------------------------------------*/

#else // HEAP_DUMP

void* xalloc (int n)
{
    void * p;

    if (! n) {
        return NULL;
    }

    p = malloc (n);
    if (!p && (reserveMem != NULL)) {
//        printf("Using reserved %d bytes for allocation of %d bytes\n", reserveSize, n);
        if (n <= reserveSize) {
            p = reserveMem;
        } else {
            free(reserveMem);
            p = malloc (n);
        }
        reserveMem = NULL;
    }

    if (! p) {
//        printf("Failed to allocate %d bytes\n", n);
        Message(xFATAL, msgINSUFFICIENT_MEMORY);
    }
#ifdef DEBUG
    MemoryUsed += n;
    if(n >= 65536) printf("%d bytes allocated, %d bytes memory used\n",n, MemoryUsed);
#endif
    return p;
}


void xfree (void * p)
{
    free (p);
}


#endif // HEAP_DUMP
#endif // xos_LINUX


/*----------------------------------------------------------------------------*/

#ifdef HEAP_DUMP

void * xreallocImpl (void * p, int o, int len, const char *file, int line)
{
    void * q;

    q = xallocImpl (len, file, line);
    if (p && o)
        memcpy (q, p, o);
    xfree (p);
    return q;
}

#else

void * xrealloc (void * p, int o, int len)
{
    void * q;

    q = xalloc (len);
    if (p && o)
        memcpy (q, p, o);
    xfree (p);
#ifdef DEBUG
    MemoryUsed += len - o;
    if(len >= 65536) printf("%d to %d reallocated, %d bytes memory used\n", o, len, MemoryUsed);
#endif
    return q;
}

#endif

/*----------------------------------------------------------------------------*/

char * dup (const char * p, int n)
{
    char *  q;

    if (p == NULL)
        return NULL;
    q = (char *) xalloc (n + 1);
    memcpy (q, p, n);
    q [n] = '\0';
    return  q;
}

/*----------------------------------------------------------------------------*/

#define BLOCK_SIZE  16384
#define MAX_SIZE    1024

int AFUsedMem = 0;
int AFBusyMem = 0;
int AFGapMem  = 0;

#define ALIGNMENT   4

int alignSize (int size) {
    return (size + ALIGNMENT-1) & ~(ALIGNMENT-1);
}

class memoryBlock * blocks[MAX_SIZE/ALIGNMENT + 1];

class memoryBlock {
  private:
    memoryBlock * next;
    byte        * freeMem;
    int           freeSize;
    int           quantum;

  public:
    memoryBlock (int _quantum) {
        ASSERT ((_quantum > 0) && ((_quantum % ALIGNMENT) == 0));
        quantum  = _quantum;

        freeMem = (byte *) xalloc (BLOCK_SIZE + ALIGNMENT);
        if (!freeMem)
            Message(xFATAL, msgINSUFFICIENT_MEMORY);

        int gapsize = ALIGNMENT - (((int)freeMem) & (ALIGNMENT-1));

        freeMem  = freeMem + gapsize;
        freeSize = ((BLOCK_SIZE - gapsize)/quantum)*quantum;

        AFBusyMem += (BLOCK_SIZE + ALIGNMENT);
        AFGapMem  += ((BLOCK_SIZE + ALIGNMENT) - freeSize);

        next = blocks[quantum/ALIGNMENT];
        blocks[quantum/ALIGNMENT] = this;
    }

    int getFreeSize () {
        return freeSize;
    }

    byte * allocate (int size) {
        ASSERT ((size > 0) && ((size % quantum) == 0) && (size <= freeSize));
        byte * mem = freeMem;
        freeMem   += size;
        freeSize  -= size;
        AFUsedMem += size;
        return mem;
    }

    Bool busy () {
        return (freeSize == 0);
    }
};

void * allocateForever (int _size)
{
    if (_size > MAX_SIZE) {
        return xalloc (_size);
    }
    ASSERT (_size > 0);

    int size = alignSize (_size);
    AFGapMem += (size - _size);

    int root = size/ALIGNMENT;
    if ((blocks[root] == NULL) || (blocks[root]->busy()))
        return (void *) ((new memoryBlock (size))->allocate(size));
    else
        return (void *) (blocks[root]->allocate(size));
}

void InitMem () {
    for (int i = 0; i < (MAX_SIZE/ALIGNMENT + 1); i++) {
        blocks[i] = NULL;
    }
}


char * dup2AF (const char * str, int len)
{
    if (!str)
        return NULL;

    char * str2 = (char *) allocateForever (len + 1);

    memcpy (str2, str, len);
    str2 [len] = '\0';

    return str2;
}


char * dup2AF (const char * str)
{
    if (!str)
        return NULL;
    return dup2AF (str, strlen (str));
}


/*
#define QUANTUM        4
#define MEMORY_BLOCK  -1

class memoryBlock * blocks[MAX_SIZE/QUANTUM + 1];

class memoryBlock {
  private:
    memoryBlock * next;
    memoryBlock * prev;

    byte        * freeMem;
    int           freeSize;

  public:
    memoryBlock (int root) {
        if (root != MEMORY_BLOCK) {
            // fake block
            ASSERT ((root >= 0) && (root <= MAX_SIZE/QUANTUM));
            freeMem  = NULL;
            freeSize = 0;
            next = this;
            prev = this;
            return;
        }
        freeMem = (byte *) xalloc (BLOCK_SIZE);
        if (!freeMem)
            Message(xFATAL, msgINSUFFICIENT_MEMORY);

        freeSize   = BLOCK_SIZE;
        AFBusyMem += BLOCK_SIZE;

        next = NULL;
        prev = NULL;
        tie ();
    }

    int getFreeSize () {
        return freeSize;
    }

    byte * allocate (int size) {
        if (freeMem == NULL) {
            ASSERT (next != this);
            return next->allocate (size);
        }
        ASSERT ((size > 0) && (size <= freeSize));
        byte * mem = freeMem;
        freeMem   += size;
        freeSize  -= size;
        AFUsedMem += size;
        untie ();
        tie ();
        return mem;
    }

    Bool rootBusy (void) {
        ASSERT (freeMem == NULL);
        return (next == this);
    }

    void tie (void) {
        ASSERT ((next == NULL) && (prev == NULL));
        int root;
        if (freeSize > MAX_SIZE)
            root = MAX_SIZE/QUANTUM;
        else
            root = freeSize/QUANTUM;
        ASSERT (blocks[root] != NULL);
        next = blocks [root]->next;
        prev = blocks [root];
        blocks [root]->next->prev = this;
        blocks [root]->next = this;
    }

    void untie (void) {
        ASSERT (freeMem != NULL);
        ASSERT ((next != NULL) && (prev != NULL));
        prev->next = next;
        next->prev = prev;
        next = NULL;
        prev = NULL;
    }
};

void * allocateForever (int size)
{
    if (size > MAX_SIZE) {
        return xalloc (size);
    }

    ASSERT (size > 0);
    int root = (size + QUANTUM-1)/QUANTUM;
    while (root <= MAX_SIZE/QUANTUM) {
        if (! blocks[root]->rootBusy()) {
            return blocks[root]->allocate(size);
        }
        root++;
    }
    return (new memoryBlock (MEMORY_BLOCK))->allocate(size);
}

void InitMem () {
    for (int i = 0; i <= MAX_SIZE/QUANTUM; i++) {
        blocks[i] = new memoryBlock (i);
    }
}
*/

close_namespace

