
#include <stdio.h>
#include <stdlib.h>

#include "xdefs.h"

open_namespace

#include "xos.h"
#include "messages.h"
#include "xmem.h"

OSInterface * OS = NULL;

//----------------------------------------------------------------------------

class CFile : public OSFile {
  private:
     byte * rawdata;

  public:
    CFile () {
        rawdata = NULL;
    }

    virtual Bool OpenRead (const char * filename, byte * &data, unsigned long &size)
    {
        FILE * fp = fopen (filename, "rb");
        if (fp == NULL) {
            Message(xERROR, msgUNABLE_TO_OPEN_FILE, filename);
            return false;
        }
        fseek (fp, 0L, SEEK_END);
        size = ftell (fp);
        if (size == 0) {
            Message(xERROR, msgEMPTY_FILE, filename);
            fclose (fp);
            return false;
        }
        fseek (fp, 0L, SEEK_SET);
        data = (byte *) xalloc (size);
        if (fread (data, 1, size, fp) != size) {
            fclose (fp);
            Message(xERROR, msgUNABLE_TO_READ_FILE, filename);
            return false;
        }
        fclose (fp);
        rawdata =  data;
        return true;
    }

    virtual ~CFile ()
    {
        if (rawdata)
            xfree (rawdata);
    }
};

//----------------------------------------------------------------------------

#if defined(xos_LINUX)

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <iconv.h>
#include <errno.h>
#include <glob.h>
#include <limits.h>

#define SEARCH_MASK "*"

class Linux_OSDir : public OSDir {
  private:
    char* name;
    glob64_t globbuf;
    int index;

  public:
    Linux_OSDir (const char* name) {
        this->name = dup(name, strlen(name)+1);
        index = INT_MAX;
        memset (&globbuf, 0, sizeof(globbuf));
    }

    virtual void startEnumeration() {
        char* mask = (char*) xalloc (strlen(name) + 1 + strlen(SEARCH_MASK) + 1);
        sprintf (mask, "%s/%s", name, SEARCH_MASK);
        int res = glob64(mask, 0, NULL, &globbuf);
        xfree (mask);
        index = (res == 0) ? 0 : INT_MAX;
    }

    virtual const char* nextFile() {
        if (index >= globbuf.gl_pathc) {
            return NULL;
        }

        char * relName = basename(globbuf.gl_pathv[index]);
        index++;

        return relName;
    }

    virtual void endEnumeration() {
        if (index != INT_MAX) {
            globfree64(&globbuf);
        }
    }

    virtual ~Linux_OSDir () {
        xfree(name);
    }
};

class LINUX_Interface : public OSInterface {
  private:

    /* Counts the length of unicode string */
    int wcsLen (const unichar * str)
    {
        int len = 0;
        while (str [len] != 0)
            len++;

        return len;
    }

  public:
    virtual OSFile * File ()
    {
        return new CFile ();
    }

    virtual char FileSep() {
        return '/';
    }

    virtual OSDir * Dir (const char* name)
    {
        return new Linux_OSDir(name);
    }

    virtual const char* GetCmdLine () {
        return NULL;
    }

    virtual char * GetFullPath (const char * pathname)
    {
        if (pathname[0] != '/') {
            char buf [256];
            if (getcwd (buf, sizeof(buf))) {
                char * fp = (char *) xalloc (strlen (buf) + 1 + strlen (pathname) + 1);
                strcpy (fp, buf);
                strcat (fp, "/");
                strcat (fp, pathname);
                return fp;
            }
        }
        return strdup (pathname);
    }
    virtual void ChMod (const char * filename, int mode)
    {
        chmod (filename, mode);
    }
    virtual void RemoveFile (const char * filename)
    {
        chmod  (filename, 0777); // rwx rwx rwx
        remove (filename);
    }

    virtual Bool getLastModifyTime (const char * filename, struct FileTime *t)
    {
        struct stat sb;
        if (stat(filename, &sb) == 0) {
            long long tm = 1000 * (long long) sb.st_mtime;
            t->fileTimeLO = (dword) (tm & 0xFFFFFFFF);
            t->fileTimeHI = (dword) (tm >> 32);
            return true;
        }
        return false;
    }

    virtual dword getFileAttributes (const char * filename)
    {
        struct stat sb;
        dword attr = 0;
        if (stat(filename, &sb) == 0) {
            attr |= file_attribute_exists;
            if (access(filename, R_OK) == 0) {
                attr |= file_attribute_readable;
            }
            if ((sb.st_mode & S_IFDIR) != 0) {
                attr |= file_attribute_directory;
            }
        }
        return attr;
    }

    virtual dword getFileLength (const char * filename)
    {
        struct stat sb;
        if (stat(filename, &sb) == 0) {
            return sb.st_size;
        }
        return 0;
    }

    virtual unichar * localStringToUnicode (const char * str)
    {
        int len = strlen(str);

        size_t wlen = mbstowcs (NULL, str, 0);
        ASSERT (wlen >= 0);
        wlen++;

        wchar_t * wstr = (wchar_t *) xalloc (wlen*sizeof(wchar_t));

        if (mbstowcs (wstr, str, wlen) < 0) {
            ASSERT_FALSE();
        }

        iconv_t conv = iconv_open ("UNICODELITTLE", "WCHAR_T");
        ASSERT (conv != ((iconv_t)-1));

        unichar * ustr = (unichar *) xalloc (wlen*sizeof(unichar));

        wchar_t * inbuf = wstr;
        size_t inbytesleft = wlen*sizeof(wchar_t);
        unichar * outbuf = ustr;
        size_t outbytesleft = wlen*sizeof(unichar);

        while (inbytesleft > 0) {

            if (iconv (conv, (char **)(&inbuf), &inbytesleft, (char **)(&outbuf), &outbytesleft) == ((size_t)(-1))) {
                printf ("errno=%d\n", errno);
                ASSERT_FALSE();
            }
        }
        ASSERT (wlen == (wcsLen(ustr)+1));

        if (iconv_close(conv) != 0) {
            ASSERT_FALSE();
        }

        xfree (wstr);

        return ustr;
    }
};


void initOS ()
{
    OS = new LINUX_Interface ();
}

#endif /* xos_LINUX */

//----------------------------------------------------------------------------

#if defined(xos_OS2)

#include <io.h>

class OS2_Interface : public OSInterface {
  public:
    virtual OSFile * File ()
    {
        return new CFile ();
    }

    virtual char FileSep() {
        return '\\';
    }

    virtual OSDir * Dir (const char*)
    {
        ASSERT_FALSE(); // not implemented
        return NULL;
    }

    virtual const char* GetCmdLine () {
        return NULL;
    }

    virtual char * GetFullPath (const char * pathname)
    {
        char * buf = (char *) xalloc (260);
        char * abspath = _fullpath (buf, pathname, 260);
        ASSERT (abspath != NULL);
        return abspath;
    }
    virtual void ChMod (const char * filename, int mode)
    {
        chmod (filename, mode);
    }
    virtual void RemoveFile (const char * filename)
    {
        remove (filename);
    }

    virtual Bool getLastModifyTime (const char * filename, struct FileTime *t)
    {
        struct stat sb;
        if (stat(filename, &sb) == 0) {
            long long tm = 1000 * (long long) sb.st_mtime;
            t->fileTimeLO = (dword) (tm & 0xFFFFFFFF);
            t->fileTimeHI = (dword) (tm >> 32);
            return true;
        }
        return false;
    }

    virtual dword getFileAttributes (const char * filename)
    {
        struct stat sb;
        dword attr = 0;
        if (stat(filename, &sb) == 0) {
            attr |= (file_attribute_exists | file_attribute_readable);
            if ((sb.st_mode & _S_IFDIR) != 0) {
                attr |= file_attribute_directory;
            }
        }
        return attr;
    }

    virtual dword getFileLength (const char * filename)
    {
        struct stat sb;
        if (stat(filename, &sb) == 0) {
            return sb.st_size;
        }
        return 0;
    }

    virtual unichar * localStringToUnicode (const char * str)
    {
        ASSERT (sizeof(wchar_t) == sizeof(unichar));

        int len = strlen(str);

        size_t wlen = mbstowcs (NULL, str, len + 1);
        ASSERT (wlen >= 0);

        unichar * wstr = (unichar *) xalloc ((wlen + 1) * sizeof(unichar));
        if (mbstowcs ((wchar_t *)wstr, str, wlen + 1) < 0) {
            ASSERT_FALSE();
        }

        return wstr;
    }
};


void initOS ()
{
    OS = new OS2_Interface ();
}

#endif /* xos_OS2 */

//----------------------------------------------------------------------------
#if defined(xos_WINNT)

#include <windows.h>
#include <io.h>

class Win32MappingFile : public OSFile {
  private:
    HANDLE hFile;
    HANDLE hFileMapping;
    LPVOID lpFileBase;

  public:
    Win32MappingFile () {
    }

    virtual Bool OpenRead (const char * filename, byte * &data, unsigned long &size)
    {
        BY_HANDLE_FILE_INFORMATION FileInformation;

        hFile = CreateFile (filename, GENERIC_READ, FILE_SHARE_READ, NULL,
                            OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

        if (hFile == INVALID_HANDLE_VALUE ||
           !GetFileInformationByHandle (hFile, & FileInformation))
        {
            Message(xERROR, msgUNABLE_TO_OPEN_FILE, filename);
            return false;
        }
        if (FileInformation.nFileSizeHigh) {
            Message(xERROR, msgFILE_TOO_LONG, filename);
            return false;
        }
        hFileMapping = CreateFileMapping (hFile, NULL, PAGE_READONLY,
                                          0, 0, NULL);
        if (hFileMapping == 0) {
            CloseHandle (hFile);
            Message(xERROR, msgUNABLE_TO_READ_FILE, filename);
            return false;
        }
        lpFileBase = MapViewOfFile (hFileMapping, FILE_MAP_READ, 0, 0, 0);
        if (lpFileBase == 0) {
            Message(xERROR, msgUNABLE_TO_READ_FILE, filename);
            CloseHandle (hFileMapping);
            CloseHandle (hFile);
            return false;
        }

        data = (byte *) lpFileBase;
        size = FileInformation.nFileSizeLow;
        return true;
    }

    virtual ~Win32MappingFile ()
    {
        UnmapViewOfFile (lpFileBase);
        CloseHandle (hFileMapping);
        CloseHandle (hFile);
    }
};

//----------------------------------------------------------------------------

#define SEARCH_MASK  "*.*"

class Win32_OSDir : public OSDir {
  private:
    char* name;
    HANDLE hFind;
    WIN32_FIND_DATA findData;
    Bool findNext;

  public:
    Win32_OSDir (const char* name) {
        this->name = dup(name, strlen(name)+1);
        findNext = false;
    }

    virtual void startEnumeration() {
        char* mask = (char*) xalloc (strlen(name) + 1 + strlen(SEARCH_MASK) + 1);
        sprintf (mask, "%s\\%s", name, SEARCH_MASK);
        hFind = FindFirstFile (mask, &findData);
        xfree (mask);
        findNext = false;
    }

    virtual const char* nextFile() {
        do {
            if (findNext) {
                ASSERT (hFind != INVALID_HANDLE_VALUE);
                if (!FindNextFile (hFind, &findData)) {
                    return NULL;
                }
            } else {
                if (hFind == INVALID_HANDLE_VALUE) {
                    return NULL;
                }
            }
            findNext = true;
        } while (!strcmp(findData.cFileName, ".") ||
                 !strcmp(findData.cFileName, ".."));
        return findData.cFileName;
    }

    virtual void endEnumeration() {
        if (hFind != INVALID_HANDLE_VALUE) {
            FindClose (hFind);
        }
    }

    virtual ~Win32_OSDir () {
        xfree(name);
    }
};


class Win32_Interface : public OSInterface {
  public:
    virtual OSFile * File ()
    {
        return new Win32MappingFile ();
    }

    virtual char FileSep() {
        return '\\';
    }

    virtual OSDir * Dir (const char* name)
    {
        return new Win32_OSDir(name);
    }

    virtual const char* GetCmdLine ()
    {
        return GetCommandLine();
    }

    virtual char * GetFullPath (const char * pathname)
    {
        char * buf = (char *) xalloc (MAX_PATH);
        char * abspath = _fullpath (buf, pathname, MAX_PATH);
        ASSERT (abspath != NULL);
        return abspath;
    }

    virtual void ChMod (const char * filename, int mode)
    {
        chmod (filename, mode);
    }

    virtual void RemoveFile (const char * filename)
    {
        SetFileAttributes (filename, FILE_ATTRIBUTE_NORMAL);
        remove (filename);
    }

    virtual Bool getLastModifyTime (const char * filename, struct FileTime *t)
    {
        WIN32_FIND_DATA fd;
        HANDLE h = FindFirstFile(filename, &fd);
        if (h != INVALID_HANDLE_VALUE) {
            FindClose(h);
            /* Perform 64-bit division.
               (a + b*2^32) DIV c =
                  = (b DIV c)*2^32 + (b MOD c)*(2^32 DIV c) +
                            ((b MOD c) * (2^32 MOD c) + a) DIV c;

               a = LO
               b = HI
               c = 10000
            */
            dword a = fd.ftLastWriteTime.dwLowDateTime;
            dword b = fd.ftLastWriteTime.dwHighDateTime;
            dword c = 10000;

            dword b_div_c = (b / c);
            dword b_mod_c = (b % c);
            dword base_div_c = (1 + (0xFFFFFFFF - c + 1) / c);
            dword base_mod_c = (    (0xFFFFFFFF - c + 1) % c);

            dword lo1 = b_mod_c * base_div_c;

            dword q1 = b_mod_c * base_mod_c;
            ASSERT (q1 < c*c);

            dword lo2 = (a >= c*c) ? ((a - c*c + q1) / c) + c :
                                     ((a       + q1) / c);

            if ((lo1 + lo2) < lo1) {
                // overflow in LO
                t->fileTimeHI = b_div_c + 1;
            } else {
                t->fileTimeHI = b_div_c;
            }
            t->fileTimeLO = lo1 + lo2;

            /* Subtract 11644473600000 */
            t->fileTimeHI -= 2711;

            if (t->fileTimeLO < 817260544)
                t->fileTimeHI -= 1;

            t->fileTimeLO -= 817260544;

            return true;
        }
        return false;
    }

    virtual dword getFileAttributes (const char * filename)
    {
        DWORD a = GetFileAttributes(filename);
        if (a != 0xffffffff) {
            return file_attribute_exists | file_attribute_readable |
                   ((a & FILE_ATTRIBUTE_HIDDEN) ? file_attribute_hidden : 0) |
                   ((a & FILE_ATTRIBUTE_DIRECTORY) ? file_attribute_directory : 0);
        }
        return 0;
    }

    virtual dword getFileLength (const char * filename)
    {
        WIN32_FIND_DATA fd;
        HANDLE h = FindFirstFile(filename, &fd);
        if (h != INVALID_HANDLE_VALUE) {
            FindClose(h);
            return (fd.nFileSizeHigh != 0) ? UINT_MAX : fd.nFileSizeLow;
        }
        return 0;
    }

    virtual unichar * localStringToUnicode (const char * mbsString)
    {
        ASSERT (mbsString != NULL);

        int len = MultiByteToWideChar (CP_ACP, 0, mbsString, -1, NULL, 0);

        unichar * wcsString = (unichar *) xalloc (len*sizeof(unichar));

        if (MultiByteToWideChar (CP_ACP, 0, mbsString, -1, (wchar_t *)wcsString, len) != len)
        {
            ASSERT_FALSE();
        }

        return wcsString;
    }
};


void initOS ()
{
    OS = new Win32_Interface ();
}

#endif /* xos_WINNT */

close_namespace

