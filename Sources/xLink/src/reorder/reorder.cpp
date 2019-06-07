
#include <stdio.h>
#include <string.h>

#include "xdefs.h"
#include "xmem.h"
#include "parser.h"
#include "xdebug.h"
#include "messages.h"
#include "avl.h"
#include "list.h"

#define MAX_LINE_LEN   16384
#define MAX_ID_LEN     16384

///////////////////////////////////////////////////////////////////////////////

class Section;
class Segment;

char * ComponentName;

class Image {
  private:
    dword imageBase;
    dword actualImageBase;
    dword size;

    List<Section *> * sections;

    dword getSize ();

    dword * pageFaults;
    int nPageFaults;
    int pageFaults_size;

    friend class ReorderSectionIterator;

  public:
    Image (dword imagebase) :
        imageBase (imagebase),
        actualImageBase (0),
        size (0),
        pageFaults (NULL),
        nPageFaults (0),
        pageFaults_size (0)
    {
        sections = new List<Section *> ();
    }

    void addSection (Section * sec) {
        sections -> Add (sec);
    }

    void setActualImageBase (dword va) {
        actualImageBase = va;
        if (actualImageBase != imageBase) {
            Message (xWARNING, msgCOMPONENT_RELOCATION);
        }
    }

    void touch (dword va) {
        if (actualImageBase == 0) {
            Message (xFATAL, msgCOMPONENT_NOT_FOUND, ComponentName);
        }

        if ((va < actualImageBase) ||
            (va > actualImageBase + getSize ()))
        {
            return;
        }

        if (pageFaults_size == 0) {
            pageFaults_size = 1024;
            nPageFaults     = 0;
            pageFaults      = (dword *) xalloc (pageFaults_size*sizeof(dword));
        }
        if (pageFaults_size == nPageFaults) {
            pageFaults_size += pageFaults_size;
            pageFaults       = (dword *) xrealloc (pageFaults,
                                                   nPageFaults*sizeof(dword),
                                                   pageFaults_size*sizeof(dword));
        }
        pageFaults [nPageFaults++] = va;
    }

    void Reorder ();
    void WriteLinkInfo (char * fname);
};

///////////////////////////////////////////////////////////////////////////////

class MemoryRegion {
  private:
    dword start, end;

  public:
    MemoryRegion (dword _start, dword _end) : start (_start), end (_end) {}

    friend int operator <  (const MemoryRegion r1, const MemoryRegion r2);
    friend int operator >  (const MemoryRegion r1, const MemoryRegion r2);
    friend int operator == (const MemoryRegion r1, const MemoryRegion r2);
};

int operator < (const MemoryRegion r1, const MemoryRegion r2) {
    return (r1.end <= r2.start);
}

int operator > (const MemoryRegion r1, const MemoryRegion r2) {
    return (r2.end <= r1.start);
}

int operator == (const MemoryRegion r1, const MemoryRegion r2) {
    return ((r1.start >= r2.start) && (r1.start < r2.end)) ||
           ((r2.start >= r1.start) && (r2.start < r1.end));
}

///////////////////////////////////////////////////////////////////////////////

class Section {
  private:
    AVLTree <MemoryRegion, Segment *> * segments;

    List<Segment *> * ordered_segments;

    dword va, size;
    char * name;

  public:
    Section (char * _name) :
        va (0xFFFFFFFF),
        size (0)
    {
        name = dup (_name, strlen (_name));
        segments = new AVLTree<MemoryRegion, Segment *> ();
        ordered_segments = new List<Segment *> ();
    }

    inline dword Section :: getVA () {
        return va;
    }

    inline dword Section :: getSize () {
        return size;
    }

    void addSegment (Segment * seg);
    void reorderTouchedSegment (dword touch);

    void WriteLinkInfo (FILE * f);
};

///////////////////////////////////////////////////////////////////////////////

class Segment {
  private:
    dword   va;
    dword   size;
    char  * filename;
    char  * keyName;

  public:
    Segment (dword _va, dword _size, char * _filename, char * _keyName) :
        va       (_va),
        size     (_size)
    {
        filename = strdup (_filename);
        keyName  = strdup (_keyName);
    }

    inline dword getSize () {
        return size;
    }

    inline dword getVA () {
        return va;
    }

    inline char * getFilename () {
        return filename;
    }

    inline char * getKeyName () {
        return keyName;
    }

    void WriteLinkInfo (FILE * f);
};

Image * image;

///////////////////////////////////////////////////////////////////////////////

class GetImageSizeIterator : public ListIterator<Section *> {
  private:
    dword imageSize;
    dword imageBase;

  public:
    GetImageSizeIterator (dword _imageBase) : imageSize (0), imageBase (_imageBase) {}

    void iter (Section * sec) {
        dword sz = sec -> getSize ();
        if (sz != 0) {
            sz += (sec -> getVA () - imageBase);
            if (sz > imageSize)
                imageSize = sz;
        }
    }

    dword getSize () {
        return imageSize;
    }
};

dword Image :: getSize () {
    if (size == 0) {
        GetImageSizeIterator getsz (imageBase);
        sections -> Iterate (&getsz);
        size = getsz.getSize();
    }
    return size;
}

///////////////////////////////////////////////////////////////////////////////

void Section :: addSegment (Segment * seg) {
    dword seg_va = seg->getVA();
    dword seg_sz = seg->getSize();

    if (va > seg_va)
        va = seg_va;

    dword approx_size = (seg_va + seg_sz - va);
    if (size < approx_size)
        size = approx_size;

    MemoryRegion region (seg_va, seg_va + seg_sz);
    segments -> Insert (region, seg);
}

///////////////////////////////////////////////////////////////////////////////

void ReadSegmentsFromSection (TextFileParser * parser, char * secname)
{
    Section * sec = new Section (secname);
    image -> addSection (sec);

    for (;;) {
        parser -> readLine ();

        ASSERT (! parser -> eof ());

        parser -> skipSpaces ();

        char * directive = parser -> peekToken ();
        if ((directive != NULL) && (directive [0] == '@')) {
            parser -> expect ("@EndSection");
            return;
        }

        dword va = parser -> getHex ();

        parser -> skipSpaces ();
        dword len = parser -> getHex ();

        char filename [MAX_ID_LEN];
        parser -> skipSpaces ();
        char * _filename = parser -> getQuoted (filename, sizeof (filename));
        ASSERT (_filename != NULL);

        parser -> expect ("#");

        char segKeyName [MAX_ID_LEN];
        parser -> skipSpaces ();
        char * _segKeyName = parser -> getQuoted (segKeyName, sizeof(segKeyName));
        ASSERT (_segKeyName != NULL);

        Segment * seg = new Segment (va, len, filename, segKeyName);
        sec -> addSegment (seg);
    }
}

void ReadSegments (TextFileParser * parser)
{
    for (;;) {
        parser -> readLine ();

        ASSERT (!parser -> eof ());

        char * directive = parser -> peekToken ();
        ASSERT (directive != NULL);

        if (!strcmp (directive, "@Section")) {
            parser -> getToken ();
            char * secname = parser -> getToken ();
            ReadSegmentsFromSection (parser, secname);
        } else if (!strcmp (directive, "@EndSegments")) {
            return;
        } else {
            Message (xFATAL, msgUNKNOWN_DIRECTIVE, directive);
        }
    }
}


void ReadLinkInfo (char * fname)
{
    TextFileParser * parser = new TextFileParser (fname, MAX_LINE_LEN);

    for (;;) {
        parser -> readLine ();

        if (parser -> eof ())
            break;

        parser -> skipSpaces ();

        char * directive = parser -> peekToken ();

        if (!strcmp (directive, "@Segments")) {
            parser -> getToken ();
            ASSERT (image != NULL);
            ReadSegments (parser);
        } else if (!strcmp (directive, "@ImageBase")) {
            parser -> getToken ();
            dword imagebase = parser -> getHex();
            image = new Image (imagebase);
        } else {
            Message (xFATAL, msgUNKNOWN_DIRECTIVE, directive);
        }
    }

    delete parser;
}

///////////////////////////////////////////////////////////////////////////////

void ReadComponents (TextFileParser * parser, char * compname)
{
    for (;;) {
        parser -> readLine ();

        if (parser -> eof ())
            return;

        parser -> skipSpaces ();

        char * directive = parser -> peekToken ();

        if (directive == NULL) 
            continue;

        if (directive [0] == '@')
            return;

        char * imagename = parser -> getQuoted ();
        ASSERT (imagename != NULL);
        imagename = dup (imagename, strlen (imagename));

        parser -> skipSpaces ();
        dword imageLocation = parser -> getHex ();


        if (!stricmp (imagename, compname)) {
            image -> setActualImageBase (imageLocation);
        }
    }
}


void ReadPageFaults (TextFileParser * parser)
{
    for (;;) {
        parser -> readLine ();
        if (parser -> eof ())
            return;

        parser -> skipSpaces ();

        char * directive = parser -> peekToken ();

        if (directive == NULL) 
            continue;

        if (directive [0] == '@')
            return;

        dword pc = parser -> getHex ();
        parser -> skipSpaces ();
        dword va = parser -> getHex ();

        image -> touch (va);
    }
}


void ReadProfileData (char * fname, char *compname)
{
    ComponentName = strdup (compname);

    TextFileParser * parser = new TextFileParser (fname);

    parser -> readLine ();
    parser -> expect ("# JET Application profile data v1");

    for (;;) {
        parser -> readLine ();

        if (parser -> eof ())
            break;

        parser -> skipSpaces ();

        for (;;) {
            char * directive = parser -> peekToken ();

            if (directive == NULL)
                break;

            if (!strcmp (directive, "@JETComponents")) {
                parser -> getToken ();
                ReadComponents (parser, compname);
            } else if (!strcmp (directive, "@PageFaults")) {
                parser -> getToken ();
                ReadPageFaults (parser);
            } else {
                break;
            }
        }
    }
}

///////////////////////////////////////////////////////////////////////////////

#define PAGESIZE 4096

dword pageLO (dword va) {
    return (va / PAGESIZE * PAGESIZE);
}

dword pageHI (dword va) {
    return pageLO (va) + PAGESIZE;
}

int TouchedSegsSize        = 0;
int UntouchedSegsSize      = 0;

void Section :: reorderTouchedSegment (dword touch)
{
    ASSERT (size != 0);
    ASSERT (va   != 0);

    if ((touch < va) || (touch > va + size))
        return;

    MemoryRegion touchSite (touch, touch+1);

    Segment * seg = segments -> FindAndRemove (touchSite);
    if (seg != 0) {
        TouchedSegsSize += seg->getSize();
        ordered_segments -> Add (seg);
    }
}


class ReorderSectionIterator : public ListIterator<Section *> {
  private:
    Image * image;

  public:
    ReorderSectionIterator (Image * _image) : image(_image) {}

    void iter (Section * sec) {
        if (sec -> getSize () != 0) {
            for (int i = 0; i < image->nPageFaults; i++)
                sec -> reorderTouchedSegment (image->pageFaults [i] - image->actualImageBase + image->imageBase);
        }
    }
};

void Image :: Reorder ()
{
    ReorderSectionIterator reord (this);
    sections->Iterate (&reord);
}

///////////////////////////////////////////////////////////////////////////////

void Segment :: WriteLinkInfo (FILE * f)
{
    fprintf (f, " %10X %4X \"%s\"#\"%s\"\n", va, size, filename, keyName);
}

class SegmentLinkInfoWriterFromTree : public TreeIterator<MemoryRegion, Segment *> {
  FILE * f;

  public:
    SegmentLinkInfoWriterFromTree (FILE * _f) : f(_f) {}

    virtual void iter (MemoryRegion, Segment * seg) {
        seg -> WriteLinkInfo (f);
    }
};

class SegmentLinkInfoWriterFromList : public ListIterator<Segment *> {
  FILE * f;

  public:
    SegmentLinkInfoWriterFromList (FILE * _f) : f(_f) {}

    virtual void iter (Segment * seg) {
        seg -> WriteLinkInfo (f);
    }
};


void Section :: WriteLinkInfo (FILE * f)
{
    fprintf (f, "@Section %s\n", name);

    SegmentLinkInfoWriterFromList wrl (f);
    ordered_segments -> Iterate (&wrl);

    SegmentLinkInfoWriterFromTree wrt (f);
    segments -> Iterate (&wrt);

    fprintf (f, "@EndSection\n");
}

class SectionLinkInfoWriter : public ListIterator<Section *> {
  FILE * f;

  public:
    SectionLinkInfoWriter (FILE * _f) : f(_f) {}

    virtual void iter (Section * sec) {
        sec -> WriteLinkInfo (f);
    }
};


static void OutStatistics (void)
{
    switch (TotalErrors) {
        case 0: 
            printf ("No errors, ");
            break;
        case 1:
            printf ("1 error, ");
            break;
        default:
            printf ("%d errors, ", TotalErrors);
            break;
    }
    switch (TotalWarnings) {
        case 0:
            puts ("no warnings\n");
            break;
        case 1:
            puts ("1 warning\n");
            break;
        default:
            printf ("%d warnings\n", TotalWarnings);
            break;
    }
}

void Image :: WriteLinkInfo (char * fname)
{
    FILE * f = fopen (fname, "w");

    fprintf (f, "@ImageBase %X\n", imageBase);
    fprintf (f, "@Segments\n");

    SectionLinkInfoWriter wr (f);
    sections -> Iterate (&wr);

    fprintf (f, "@EndSegments\n");
    fclose (f);

    Message (xMESSAGE, msgNUMBER_OF_PAGEFAULTS, nPageFaults);

    OutStatistics ();
}

///////////////////////////////////////////////////////////////////////////////

int main (int argc, char ** argv)
{
    puts ("");
    puts ("Excelsior Link Info Reorder Utility Version 1.1");
    puts ("Copyright (c) Excelsior, 2002-2007. All rights reserved.");
    fflush (stdout);

    if (argc != 5) {
        printf ("\n"
                "Usage: xreorder <input link info> <profile data> <component name>\n"
                "                <output link info>\n");
        return 1;
    }
    initOS ();

    ReadLinkInfo    (argv [1]);
    ReadProfileData (argv [2], argv[3]);
    image -> Reorder ();
    image -> WriteLinkInfo (argv [4]);
    return 0;
}
