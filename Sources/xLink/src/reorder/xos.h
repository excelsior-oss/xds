
#ifndef _XOS_H_
#define _XOS_H_

#include "xdefs.h"

// The time in milliseconds from 01.01.1970
struct FileTime {
    dword fileTimeLO;
    dword fileTimeHI;
};

#define file_attribute_exists           0x00000001
#define file_attribute_readable         0x00000002
#define file_attribute_hidden           0x00000004
#define file_attribute_directory        0x00000008


class OSFile {
  public:
    virtual Bool OpenRead (const char * name, byte * &data, unsigned long &size) = 0;
    virtual ~OSFile () {}
};

class OSDir {
  public:
    virtual const char* nextFile() = 0;
    virtual void startEnumeration() = 0;
    virtual void endEnumeration() = 0;
    virtual ~OSDir () {}
};

#define FMODE_EXECUTABLE    0775       /* r-x r-x r-x */

class OSInterface {
  public:
    virtual OSFile * File () = 0;
    virtual char FileSep() = 0;

    virtual const char* GetCmdLine () = 0;

    virtual char * GetFullPath (const char * pathname) = 0;
    virtual void ChMod (const char * filename, int mode) = 0;
    virtual void RemoveFile (const char * filename) = 0;

    virtual OSDir * Dir (const char* name) = 0;

    virtual Bool  getLastModifyTime (const char * filename, struct FileTime *t) = 0;
    virtual dword getFileAttributes (const char * filename) = 0;
    virtual dword getFileLength(const char * filename) = 0;

    virtual unichar * localStringToUnicode (const char * mbsString) = 0;
};

extern OSInterface * OS;

extern void initOS ();

#endif
