
#ifndef XMEM_H
#define XMEM_H

#include <string.h>

#include "xdebug.h"
#include "strenc.h"

#if (defined(xos_LINUX) && defined(SHARED_LIBRARY)) || defined(HEAP_DUMP)

extern void * operator new(size_t n);
extern void   operator delete(void* p);
extern void * operator new[] (size_t n);
extern void   operator delete[] (void* p);

#endif


#if defined(xos_LINUX) && defined(SHARED_LIBRARY)
extern void free_heap();
#endif


#ifdef HEAP_DUMP

#define xalloc(n) xallocImpl((n), __FILE__, __LINE__)
#define xrealloc(p, o, n) xreallocImpl((p), (o), (n), __FILE__, __LINE__)

extern void * xallocImpl   (int n, const char* file, int line);
extern void * xreallocImpl (void * p, int o, int n, const char* file, int line);

#else

extern void * xalloc   (int n);
extern void * xrealloc (void * p, int o, int n);

#endif

extern void   xfree    (void * p);

extern char * dup      (const char *, int);

extern Bool reserveMemory(int size);

template<class Element> class GrowingArray {
  private:
    Element * data;
    unsigned  size;
    unsigned  count;
    unsigned  granularity;

  public:
    GrowingArray (unsigned initialCapacity, unsigned growingGranularity) :
        data        (0),
        size        (initialCapacity),
        count       (0),
        granularity (growingGranularity)
    {
        if (size != 0)
            data = (Element *) xalloc (size * sizeof (Element));
    }

    GrowingArray () :
        data        (0),
        size        (0),
        count       (0),
        granularity (0)
    {
    }

    void addElement (Element e) {
        if (count == size) {
            if (size == 0) {
                size = (granularity == 0) ? 16 : granularity;
                data = (Element *) xalloc (size * sizeof (Element));
            } else {
                unsigned newsize = (granularity == 0) ? size*2 : size + granularity;
                data = (Element *) xrealloc (data, size*sizeof (Element), newsize*sizeof (Element));
                size = newsize;
            }
        }
        ASSERT (count < size);
        data [count++] = e;
    }

    unsigned getCount () {
        return count;
    }

    Element operator [] (int index) {
        ASSERT ((index >= 0) && ((dword)index < count));
        return data [index];
    }

    void Clean () {
        count = 0;
    }

    ~GrowingArray () {
        if (data)
            xfree (data);
    }
};

/*----------------------------------------------------------------------------*/

#define Grow(n, size, delta, table, type, ptr)                                \
        if (n == size) {                                                      \
                        size += (size < delta) ? delta : size;                \
                        table = (type *) xrealloc (table, n * sizeof (type),  \
                                                  size * sizeof (type));      \
                }                                                             \
                ptr = & (table [n ++]);


/*----------------------------------------------------------------------------*/
/*                   Storage implementation                                   */
/*----------------------------------------------------------------------------*/

#ifdef HEAP_DUMP
#define newStorage(...) (new Storage(__VA_ARGS__, __FILE__, __LINE__))
#else
#define newStorage(...) (new Storage(__VA_ARGS__))
#endif

class Storage {
  private:
    dword   Len;

#ifdef HEAP_DUMP
    const char *file;
    int line;
#endif

  public:
    byte  * Ptr;
    dword   Index;

#ifdef HEAP_DUMP

    Storage (dword len, const char *file_, int line_)
    {
        file = file_;
        line = line_;

        Ptr   = (byte *) xallocImpl(len, file, line);
        Len   = len;
        memset(Ptr, 0x00, len);
        Index = 0;

    }

    Storage (byte * data, dword len, const char *file_, int line_)
    {
        file = file_;
        line = line_;

        Ptr   = data;
        Len   = len;
        Index = len;
    }

    inline void Check (int len)
    {
        if (Index + len >= Len) {
            dword new_len;
            for(new_len = Len; Index + len >= new_len; new_len *= 2);
            Ptr = (byte *) xreallocImpl (Ptr, Len, new_len, file, line);
            Len = new_len;
        }
    }

#else

    Storage (dword len)
    {
        Ptr   = (byte *) xalloc(len);
        Len   = len;
        memset(Ptr, 0x00, len);
        Index = 0;
    }

    Storage (byte * data, dword len)
    {
        Ptr   = data;
        Len   = len;
        Index = len;
    }

    inline void Check (int len)
    {
        if (Index + len >= Len) {
            dword new_len;
            for(new_len = Len; Index + len >= new_len; new_len *= 2);
            Ptr = (byte *) xrealloc (Ptr, Len, new_len);
            Len = new_len;
        }
    }

#endif

    ~Storage ()
    {
        xfree(Ptr);
    }

    inline void ZeroBytes (int len)
    {
        Check(len);
        memset (Ptr + Index, 0, len);
        Index += len;
    }

    inline void PutB (dword b)
    {
        ASSERT(b < 0x100);
        Check(1);
        * (byte *)(Ptr + Index) = (byte) b;
        Index += 1;
    }

    inline void Put2 (dword w)
    {
        ASSERT(w < 0x10000);
        Check(2);
        * (word *) (Ptr + Index) = (word) w;
        Index += 2;
    }

    inline void Put4 (dword dw)
    {
        Check(4);
        * (dword *) (Ptr + Index) = dw;
        Index += 4;
    }

    inline void PutS (const void * data, dword len)
    {
        Check(len);
        memcpy (Ptr + Index, data, len);
        Index += len;
    }

    inline void PutEncryptedS (const void * data, dword len, byte key)
    {
        Check(len);
        byte * s = (byte *)data;
        for(dword i=0; i<len; i++) {
            PutB(encryptByte(s[i], key));
        }
    }

    inline void PutName (const char * str)
    {
        dword len = strlen (str);
        ASSERT (len < 0x100);
        Check (len + 1);
        PutB  (len);
        PutS  (str, len);
    }

    inline void PutLongName (const char * str)
    {
        dword len = strlen (str);
        ASSERT (len < 0x10000);
        Check (len + 2);
        Put2  (len);
        PutS  (str, len);
    }

    inline void PutAlignedName (const char * str, dword len)
    {
        dword alen = (len + 3) & (~3);
        Check (alen + 4);
        Put4  (alen);
        PutS  (str, len);
        if (len != alen) {
            ZeroBytes (alen - len);
        }
    }

    inline void PutEncryptedAlignedName (const char * str, dword len, byte key)
    {
        dword alen = (len + 3) & (~3);
        Check (alen + 4);
        Put4  (alen);
        PutEncryptedS (str, len, key);
        if (len != alen) {
            ZeroBytes (alen - len);
        }
    }

    inline void Align2 (void)
    {
        if (Index & 1)
            PutB (0x00);
    }
    inline void Align4 (void)
    {
        int len = 4 - (Index & 3);
        if (len != 4) {
            ZeroBytes  (len);
        }
    }

    inline void Set4 (dword index, dword dw)
    {
        ASSERT (index + 4 <= Index);
        * (dword *) (Ptr + index) = dw;
    }

    inline dword GetPos () const
    {
        return Index;
    }

    inline byte * GetData () const
    {
        return Ptr;
    }

    inline dword GetLen () const
    {
        return Index;
    }
};

/*----------------------------------------------------------------------------*/
/*                   Allocate-forever heap                                    */
/*----------------------------------------------------------------------------*/

extern void * allocateForever (int size);

extern int AFUsedMem;
extern int AFBusyMem;
extern int AFGapMem;

extern void InitMem ();

class ForeverObject {
  public:
    void *operator new (size_t size) {
        return allocateForever (size);
    }
  
    void operator delete (void *) {
        ASSERT_FALSE ();
    }
};

extern char * dup2AF (const char * str, int len);
extern char * dup2AF (const char * str);

#endif
