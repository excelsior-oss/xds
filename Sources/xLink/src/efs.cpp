#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stddef.h>

#include <vector>
#include <string>

#include "xdefs.h"

open_namespace

#include "efs.h"
#include "idents.h"
#include "messages.h"
#include "xdebug.h"
#include "xmem.h"
#include "struct.h"
#include "jet.h"
#include "parser.h"

using namespace std;


/*----------------------------------------------------------------------------*/
/*                        Embedded File System                                */
/*----------------------------------------------------------------------------*/


static Segment * EFSDescSeg  = NULL;
static Segment * EFSDirSeg   = NULL;
static Segment * EFSNamesSeg = NULL;
static CustomDataSourceSegment * EFSDataSeg  = NULL;

static Storage * EFSDescStorage  = NULL;
static Storage * EFSDirStorage   = NULL;
static Storage * EFSNamesStorage = NULL;

class EFSDataContainer;

static EFSDataContainer* dataContainer = NULL;

class EFS_IR_Directory;
class EFS_IR_MountPoint;
class EFS_IR;

static EFS_IR * EFS_ir = NULL;

static int EFS_curFileIndex = 0;
static int EFS_nFiles       = 0;


class EFSHostFileDataSource : public DataSource {
private:
    dword length;
    std::string hostFileName;

public:
    EFSHostFileDataSource (const char* hostFileName_, dword length_)
        : length(length_)
        , hostFileName(hostFileName_)
    {}

    virtual int getLen() {
        return length;
    }

    virtual void copyDataTo(byte* dest) {
        OSFile * file = OS->File();
        byte * data = NULL;
        unsigned long size = 0;

        if (!file->OpenRead (hostFileName.c_str(), /* & */ data, /* & */ size)) {
            Message(xFATAL, msgUNABLE_TO_READ_FILE, hostFileName.c_str());
        }

        if (size > length) {
            // korova v puti mogla podrasti, ignore remaining data
            size = length;
        }
        memcpy (dest, data, size);
        delete file;
    }
};


class EFSDataFile {
private:
    std::string dataFileName;
    FILE* file;

public:
    EFSDataFile (const char* dataFileName_)
        : dataFileName(dataFileName_)
    {}

    FILE* getFile() {
        return file;
    }

    const char* getName() const {
        return dataFileName.c_str();
    }

    void open() {
        file = fopen (getName(), "rb");
        if (file == NULL) {
            Message(xFATAL, msgUNABLE_TO_OPEN_FILE, getName());
        }
    }

    void close() {
        if (file != NULL) {
            fclose(file);
        }
    }
};


class EFSDataFileDataSource : public DataSource {
private:
    EFSDataFile* dataFile;
    dword offs;
    dword length;

public:
    EFSDataFileDataSource (EFSDataFile* dataFile_, dword offs_, dword length_)
        : dataFile(dataFile_)
        , offs(offs_)
        , length(length_)
    {}

    virtual int getLen() {
        return length;
    }

    virtual void copyDataTo(byte* dest) {
        if (length != 0) {
            fseek (dataFile->getFile(), offs, SEEK_SET);

            if (fread (dest, length, 1, dataFile->getFile()) != 1) {
                VerboseMessage ("Trying to read %s offs %d length %d\n", dataFile->getName(), offs, length);
                Message(xFATAL, msgUNABLE_TO_READ_FILE, dataFile->getName());
            }
        }
    }
};


class EFSDataContainer : public DataSource {
    typedef std::vector<DataSource*> DataSources;
    DataSources dataSources;

    typedef std::vector<EFSDataFile*> DataFiles;
    DataFiles dataFiles;

    dword length;

public:
    EFSDataContainer()
        : length(0)
    {}

    virtual ~EFSDataContainer() {
        for (size_t i = 0; i < dataSources.size(); i++) {
            delete dataSources[i];
        }
        for (size_t i = 0; i < dataFiles.size(); i++) {
            delete dataFiles[i];
        }
    }

    void addDataSource(DataSource* ds) {
        dataSources.push_back(ds);
        length += ds->getLen();
    }

    void addDataFile(EFSDataFile* df) {
        dataFiles.push_back(df);
    }

    virtual int getLen() {
        return length;
    }

    virtual void copyDataTo(byte* dest) {
        // open data files
        for (size_t i = 0; i < dataFiles.size(); i++) {
            dataFiles[i]->open();
        }
        // copy data from all data sources into destination
        int offs = 0;
        for (size_t i = 0; i < dataSources.size(); i++) {
            DataSource* ds = dataSources[i];
            int l = ds->getLen();
            ASSERT (l >= 0);
            ASSERT (offs + l <= length);
            ds->copyDataTo(dest + offs);
            offs += l;
        }
        ASSERT (offs == length);
        // close data files
        for (size_t i = 0; i < dataFiles.size(); i++) {
            dataFiles[i]->close();
        }
        // encrypt data
        if ((xEncryptStrings != ENCRYPTION_KEY_NO_ENCRYPTION_VALUE) &&
            (xEncryptStrings != ENCRYPTION_KEY_UNDEFINED_VALUE))
        {
                encryptData((char *)dest, (char *)dest, length, (byte) xEncryptStrings);
        }
    }


    void writeDataTo(FILE* outDataFile, const char* outDataFileName) {
        // open data files
        for (size_t i = 0; i < dataFiles.size(); i++) {
            dataFiles[i]->open();
        }
        // write data from all data sources
        int offs = 0;
        for (size_t i = 0; i < dataSources.size(); i++) {
            DataSource* ds = dataSources[i];
            int l = ds->getLen();
            ASSERT (l >= 0);
            ASSERT (offs + l <= length);
            byte* buf = (byte*) xalloc(l);
            ds->copyDataTo(buf);
            if (fwrite (buf, l, 1, outDataFile) != 1) {
                fclose (outDataFile);
                Message(xFATAL, msgUNABLE_TO_WRITE_FILE, outDataFileName);
            }
            xfree (buf);
            offs += l;
        }
        ASSERT (offs == length);
        // close data files
        for (size_t i = 0; i < dataFiles.size(); i++) {
            dataFiles[i]->close();
        }
    }
};


class EFS_IR_File {
  private:
    char * efsFileName;
    dword lastModifiedTimeLO, lastModifiedTimeHI;
    dword attrs;
    dword data_offs;
    dword length;
    EFS_IR_Directory * dir;

  public:
    EFS_IR_File () :
        dir(NULL),
        efsFileName (NULL),
        lastModifiedTimeLO (0), lastModifiedTimeHI (0),
        attrs (0),
        data_offs (0),
        length (0)
    {}

    ~EFS_IR_File ();

    void read (TextFileParser* parser, EFSDataFile* dataFile);
    void doEnum (const char* shortFileName, const char * hostFileName);
    void write (dword efile_offs);
    void serialize (FILE * f, int identation);
};


class EFS_IR_Directory {
  private:
    int nFilesInDir;
    EFS_IR_File * files;

  public:
    EFS_IR_Directory () : nFilesInDir (0), files (NULL) {}

    ~EFS_IR_Directory ();

    void read (TextFileParser* parser, EFSDataFile* dataFile);
    void doEnum (const char * hostDirName);
    void write (Storage * edir_buf, dword edir_offs);
    void serialize (FILE * f, int identation);
};


EFS_IR_File :: ~EFS_IR_File () {
    xfree (efsFileName);
    delete dir;
}


void EFS_IR_File :: read (TextFileParser * parser, EFSDataFile* dataFile) {
    parser -> readLine ();
    ASSERT (! parser -> eof ());

    parser -> skipSpaces ();
    char * id = parser -> getToken();

    Bool isDir;
    Bool packed;
    Bool doEnum = false;
    Bool noData = false;
    if (!strcmp (id, "FILE")) {
        isDir  = false;
        packed = false;
    } else if (!strcmp (id, "FILE_NODATA")) {
        isDir  = false;
        packed = false;
        noData = true;
    } else if (!strcmp (id, "DIR")) {
        isDir = true;
        packed = false;
    } else if (!strcmp (id, "PACKED_FILE")) {
        isDir = false;
        packed = true;
    } else if (!strcmp (id, "PACKED_DIR")) {
        isDir = true;
        packed = true;
    } else if (!strcmp (id, "ENUM_DIR_RECURSIVE")) {
        isDir = true;
        packed = false;
        doEnum = true;
    } else {
        parser -> expect ("FILE or DIR or PACKED_FILE or PACKED_DIR or ENUM_DIR_RECURSIVE"); // fatal error
        ASSERT_FALSE();
    }

    parser -> skipSpaces ();
    char _efsFileName [1024];
    char * _efsFileName2 = parser -> getQuoted (_efsFileName, 1024);
    efsFileName = processDefsAndDup(_efsFileName2);

    if (packed) {
        parser -> skipSpaces ();
        attrs = parser -> getHex() | fs_notiteratable;

        parser -> skipSpaces ();
        lastModifiedTimeLO = parser -> getHex();

        parser -> skipSpaces ();
        lastModifiedTimeHI = parser -> getHex();

        if (!isDir) {
            ASSERT (dataFile != NULL);

            parser -> skipSpaces ();
            dword packed_offs = parser -> getHex();

            parser -> skipSpaces ();
            length = parser -> getHex();

            data_offs = dataContainer->getLen();

            if (length != 0) {
                DataSource* ds = new EFSDataFileDataSource(dataFile, packed_offs, length);
                dataContainer->addDataSource(ds);
            }

        } else {
            dir = new EFS_IR_Directory();
            dir -> read (parser, dataFile);
        }
    } else {
        parser -> skipSpaces ();
        char _hostFileName [1024];
        char * _hostFileName2 = parser -> getQuoted (_hostFileName, 1024);
        char * hostFileName = processDefsAndDup(_hostFileName2);

        struct FileTime tm;
        if (OS -> getLastModifyTime(hostFileName, &tm)) {
            lastModifiedTimeLO = tm.fileTimeLO;
            lastModifiedTimeHI = tm.fileTimeHI;
        } else {
            lastModifiedTimeLO = 0;
            lastModifiedTimeHI = 0;
        }

        dword osAttrs = OS -> getFileAttributes (hostFileName);
        if ((osAttrs & file_attribute_exists) != 0) {
            attrs |= fs_exists;
        }
        if ((osAttrs & file_attribute_readable) != 0) {
            attrs |= fs_readable;
        }
        if ((osAttrs & file_attribute_hidden) != 0) {
            attrs |= fs_hidden;
        }

        parser -> skipSpaces ();
        char * extAttr = parser->peekToken();
        if ((extAttr != NULL) && !strcmp(extAttr, "HIDDEN")) {
            attrs |= fs_notiteratable;
        }

        if (!isDir) {
            if (!noData) {
                length = OS->getFileLength(hostFileName);
                data_offs = dataContainer->getLen();

                if (length != 0) {
                    DataSource* ds = new EFSHostFileDataSource(hostFileName, length);
                    dataContainer->addDataSource(ds);
                }
            }
        } else {
            attrs |= (fs_directory | fs_readable);

            dir = new EFS_IR_Directory();

            if (doEnum) {
                dir -> doEnum (hostFileName);
            } else {
                dir -> read (parser, dataFile);
            }
        }

        xfree (hostFileName);
    }
}


void EFS_IR_File :: doEnum (const char* shortFileName, const char * hostFileName) {
    efsFileName = dup(shortFileName, strlen(shortFileName)+1);

    struct FileTime tm;
    if (OS -> getLastModifyTime(hostFileName, &tm)) {
        lastModifiedTimeLO = tm.fileTimeLO;
        lastModifiedTimeHI = tm.fileTimeHI;
    } else {
        lastModifiedTimeLO = 0;
        lastModifiedTimeHI = 0;
    }

    dword osAttrs = OS -> getFileAttributes (hostFileName);
    if ((osAttrs & file_attribute_exists) != 0) {
        attrs |= fs_exists;
    }
    if ((osAttrs & file_attribute_readable) != 0) {
        attrs |= fs_readable;
    }
    if ((osAttrs & file_attribute_hidden) != 0) {
        attrs |= fs_hidden;
    }

    if (osAttrs & file_attribute_directory) {
        attrs |= (fs_directory | fs_readable);

        dir = new EFS_IR_Directory();
        dir -> doEnum (hostFileName);
    } else {
        length = OS->getFileLength(hostFileName);
        data_offs = dataContainer->getLen();

        if (length != 0) {
            DataSource* ds = new EFSHostFileDataSource(hostFileName, length);
            dataContainer->addDataSource(ds);
        }
    }
}


void EFS_IR_File :: write (dword efile_offs) {
    struct EFile * efile = (struct EFile *) (EFSDescStorage -> Ptr + efile_offs);

    addFixup (EFSDescSeg,
              FIXUP_ADDRESS32,
              efile_offs + offsetof (struct EFile, name),
              TK_SEG,
              EFSNamesSeg,
              EFSNamesStorage->GetPos());

    unichar * efsFileNameU = OS -> localStringToUnicode (efsFileName);
    EFSNamesStorage -> PutS (efsFileNameU, (wcsLen(efsFileNameU) + 1)*sizeof(unichar));

    efile -> attrs              = attrs;
    efile -> lastModifiedTimeLO = lastModifiedTimeLO;
    efile -> lastModifiedTimeHI = lastModifiedTimeHI;
    efile -> length             = length;

    if (dir == NULL) {
        addFixup (EFSDescSeg,
                  FIXUP_ADDRESS32,
                  efile_offs + offsetof (struct EFile, data),
                  TK_SEG,
                  EFSDataSeg,
                  data_offs);
    } else {
        dword dir_offs = EFSDirStorage->GetPos();

        addFixup (EFSDescSeg,
                  FIXUP_ADDRESS32,
                  efile_offs + offsetof (struct EFile, data),
                  TK_SEG,
                  EFSDirSeg,
                  dir_offs);

        EFSDirStorage -> ZeroBytes (sizeof(struct EDirectory));

        dir -> write (EFSDirStorage, dir_offs);
    }
}


void EFS_IR_File :: serialize (FILE * f, int identation) {
    if (dir == NULL) {
        fprintf (f, "%*cPACKED_FILE \"%s\" %x %x %x %x %x\n", identation, ' ',
                 efsFileName, attrs, lastModifiedTimeLO, lastModifiedTimeHI,
                 data_offs, length);
    } else {
        fprintf (f, "%*cPACKED_DIR \"%s\" %x %x %x\n", identation, ' ',
                 efsFileName, attrs, lastModifiedTimeLO, lastModifiedTimeHI);

        dir -> serialize (f, identation + EFS_IDENTATION);
    }
}


EFS_IR_Directory :: ~EFS_IR_Directory () {
    delete[] files;
}


void EFS_IR_Directory :: read (TextFileParser * parser, EFSDataFile* dataFile) {
    parser -> readLine ();
    ASSERT (! parser -> eof ());

    parser -> skipSpaces ();
    nFilesInDir = parser -> getDec();

    files = new EFS_IR_File [nFilesInDir];

    for (int i = 0; i < nFilesInDir; i++)
        files[i].read (parser, dataFile);

    EFS_nFiles += nFilesInDir;
}

void EFS_IR_Directory :: doEnum (const char * hostDirName)
{
    OSDir * osdir = OS->Dir(hostDirName);

    osdir -> startEnumeration();
    nFilesInDir = 0;
    while (osdir -> nextFile())
        nFilesInDir++;
    osdir -> endEnumeration();

    files = new EFS_IR_File [nFilesInDir];

    osdir -> startEnumeration();
    for (int i = 0; i < nFilesInDir; i++) {
        const char* fname = osdir -> nextFile();
        ASSERT (fname != NULL);

        char * path = (char *) xalloc (strlen(hostDirName) + 1 + strlen(fname) + 1);
        sprintf (path, "%s%c%s", hostDirName, OS->FileSep(), fname);
        files[i].doEnum (fname, path);
        xfree (path);
    }
    osdir -> endEnumeration();

    EFS_nFiles += nFilesInDir;
}


void EFS_IR_Directory :: write (Storage * edir_buf, dword edir_offs) {
    struct EDirectory * edir = (struct EDirectory *) (edir_buf -> Ptr + edir_offs);

    int firstFileIndex = EFS_curFileIndex;

    edir -> firstFileIndex = firstFileIndex;
    edir -> nFiles         = nFilesInDir;

    dword efile_offs = EFSDescStorage->GetPos();
    EFSDescStorage->ZeroBytes (nFilesInDir*sizeof(struct EFile));

    EFS_curFileIndex += nFilesInDir;
    ASSERT (EFS_curFileIndex <= EFS_nFiles);

    for (int i = 0; i < nFilesInDir; i++)
        files[i].write (efile_offs + i*sizeof(struct EFile));
}


void EFS_IR_Directory :: serialize (FILE * f, int identation) {
    fprintf (f, "%*c%d\n", identation, ' ', nFilesInDir);

    for (int i = 0; i < nFilesInDir; i++)
        files[i].serialize (f, identation);
}


class EFS_IR_MountPoint {
  private:
    char * hostDir;
    EFS_IR_Directory * dir;

  public:
    EFS_IR_MountPoint () : hostDir (NULL), dir (NULL) {}

    ~EFS_IR_MountPoint () {
        xfree (hostDir);
        delete dir;
    }

    void read (TextFileParser * parser, EFSDataFile* dataFile) {
        parser -> readLine ();
        ASSERT (! parser -> eof ());

        char _hostDir [1024];
        parser -> skipSpaces ();
        char * _hostDir2 = parser -> getQuoted (_hostDir, 1024);
        hostDir = dup (_hostDir2, strlen(_hostDir2));

        dir = new EFS_IR_Directory ();
        dir -> read (parser, dataFile);
    }

    void write (dword mp_offs) {

        addFixup (EFSDescSeg,
                  FIXUP_ADDRESS32,
                  mp_offs + offsetof (struct EMountPoint, hostDir),
                  TK_SEG,
                  EFSNamesSeg,
                  EFSNamesStorage->GetPos());

        unichar * hostDirU = OS -> localStringToUnicode (hostDir);
        EFSNamesStorage -> PutS (hostDirU, (wcsLen(hostDirU) + 1)*sizeof(unichar));

        dir -> write (EFSDescStorage, mp_offs + offsetof (struct EMountPoint, efsDir));
    }

    void serialize (FILE * f) {
        fprintf (f, "\"%s\"\n", hostDir);
        dir -> serialize (f, EFS_IDENTATION);
    }
};


class EFS_IR {
  private:
    std::vector<EFS_IR_MountPoint*> mountPoints;

  public:
    EFS_IR () {
        EFSNamesStorage = newStorage (4096);
        dataContainer = new EFSDataContainer();
    }

    ~EFS_IR () {
        for (size_t i = 0; i < mountPoints.size(); i++)
            delete mountPoints[i];
    }

    void read (const char * efsDescFile, const char * dataFileName) {
        TextFileParser * parser = new TextFileParser (efsDescFile, 16384);

        parser -> readLine ();
        ASSERT (! parser -> eof ());

        parser -> skipSpaces ();
        int nMountPoints = parser -> getDec();

        EFSDataFile* dataFile = NULL;

        parser -> skipSpaces ();
        char * token = parser->peekToken();
        if ((token != NULL) && !strcmp (token, "PACKED")) {
            dataFile = new EFSDataFile(dataFileName);
            dataContainer->addDataFile(dataFile);
        }

        for (int i = 0; i < nMountPoints; i++) {
            EFS_IR_MountPoint * mp = new EFS_IR_MountPoint();

            mp -> read (parser, dataFile);

            mountPoints.push_back(mp);
        }

        delete parser;
    }


    void write () {
        new OBJFile ("EMBEDDED FILE SYSTEM");

        EFSDescSeg  = new Segment (NAMES.Str2Index("EFS Desc"),        DATA,  DATA,  false, 0, 4);
        EFSDirSeg   = new Segment (NAMES.Str2Index("EFS Directories"), DATA,  DATA,  false, 0, 4);
        EFSNamesSeg = new Segment (NAMES.Str2Index("EFS Names"),       DATA,  DATA,  false, 0, 1);
        EFSDataSeg  = new CustomDataSourceSegment (NAMES.Str2Index("EFS Data"), CONST, CONST, false, 0, 1);

        EFSDescStorage  = newStorage (1024);
        EFSDirStorage   = newStorage (1024);

        dword NMountPoints = mountPoints.size();

        NewPublicName (NAMES.Str2Index("LINK_EmbeddedFileSystem"), EFSDescSeg, 0, T_DATA);

        struct EDesc desc;
        desc.magic        = EFS_MAGIC;
        desc.version      = EFS_VERSION;
        desc.mounted      = 0;
        desc.names        = NULL;
        desc.namesLength  = 0; 
        desc.nMountPoints = NMountPoints;
        desc.mountPoints  = NULL;
        desc.nFiles       = EFS_nFiles;
        desc.files        = NULL;

        EFSDescStorage -> PutS (&desc, sizeof(desc));
        
        dword mountPoints_offs = EFSDescStorage -> GetPos();
        EFSDescStorage -> ZeroBytes (NMountPoints*sizeof(struct EMountPoint));

        addFixup (EFSDescSeg,
                  FIXUP_ADDRESS32,
                  offsetof (struct EDesc, mountPoints),
                  TK_SEG,
                  EFSDescSeg,
                  mountPoints_offs);

        addFixup (EFSDescSeg,
                  FIXUP_ADDRESS32,
                  offsetof (struct EDesc, files),
                  TK_SEG,
                  EFSDescSeg,
                  EFSDescStorage->GetPos());

        addFixup (EFSDescSeg,
                  FIXUP_ADDRESS32,
                  offsetof (struct EDesc, names),
                  TK_SEG,
                  EFSNamesSeg,
                  0);

        for (size_t i = 0; i < mountPoints.size(); i++) {
            mountPoints[i]->write (mountPoints_offs + i*sizeof(struct EMountPoint));
        }

        ASSERT (EFS_curFileIndex == EFS_nFiles);

        // write length of names segment
        EFSDescStorage -> Set4(offsetof (struct EDesc, namesLength), EFSNamesStorage -> GetLen());
        // encrypt names and data
        if ((xEncryptStrings != ENCRYPTION_KEY_NO_ENCRYPTION_VALUE) &&
            (xEncryptStrings != ENCRYPTION_KEY_UNDEFINED_VALUE)) {
                encryptData((char *)EFSNamesStorage -> Ptr, (char *)EFSNamesStorage -> Ptr, EFSNamesStorage -> GetLen(), (byte) xEncryptStrings);
        }

        EFSDescSeg  -> setText (EFSDescStorage  -> GetData(), EFSDescStorage  -> GetLen());
        EFSDirSeg   -> setText (EFSDirStorage   -> GetData(), EFSDirStorage   -> GetLen());
        EFSNamesSeg -> setText (EFSNamesStorage -> GetData(), EFSNamesStorage -> GetLen());

        EFSDataSeg -> setDataSource(dataContainer);

        VerboseMessage ("EFS: .desc %d, .dir %d, .names %d, .data %d bytes\n",
                        EFSDescStorage  -> GetLen(), EFSDirStorage -> GetLen(),
                        EFSNamesStorage -> GetLen(), dataContainer -> getLen());
    }


    void serialize (const char * outFileName, const char * dataFileName) {
        FILE * f = fopen (outFileName, "w");
        if (f == NULL) {
            Message(xFATAL, msgUNABLE_TO_OPEN_FILE, outFileName);
        }

        fprintf (f, "%d PACKED\n", mountPoints.size());

        for (size_t i = 0; i < mountPoints.size(); i++) {
            mountPoints[i]->serialize (f);
        }
        fclose (f);

        FILE * dataFile = fopen (dataFileName, "wb");
        if (dataFile == NULL) {
            Message(xFATAL, msgUNABLE_TO_OPEN_FILE, dataFileName);
        }
        if (dataContainer->getLen() != 0) {
            dataContainer->writeDataTo(dataFile, dataFileName);
        }
        fclose (dataFile);

        Message(xMESSAGE, msgPACKED_EFS_WRITTEN, outFileName, dataFileName);
    }
};


void ReadEmbeddedFS (const char * efsDescFile, const char * dataFileName)
{
    if (EFS_ir == NULL) {
        EFS_ir = new EFS_IR();
    }
    EFS_ir -> read (efsDescFile, dataFileName);
}


void FormEmbeddedFS (void)
{
    if (EFS_ir == NULL) {
        if (xJetComponent) {
            new OBJFile ("EMBEDDED FILE SYSTEM");
            EFSDescSeg = new Segment (NAMES.Str2Index("EFS Desc"), DATA, DATA, false, sizeof(struct EDesc), 4);
            NewPublicName (NAMES.Str2Index("LINK_EmbeddedFileSystem"), EFSDescSeg, 0, T_DATA);

            struct EDesc * desc = (struct EDesc *) (EFSDescSeg -> getText());
            desc->magic        = EFS_MAGIC;
            desc->version      = EFS_VERSION;
            desc->mounted      = 0;
            desc->names        = NULL;
            desc->namesLength  = 0; 
            desc->nMountPoints = 0;
            desc->mountPoints  = NULL;
            desc->nFiles       = 0;
            desc->files        = NULL;
        }
        return;
    }

    EFS_ir -> write();
}


void SerializeEmbeddedFS (const char * efsDescFile, const char * dataFileName)
{
    if (EFS_ir != NULL) {
        EFS_ir -> serialize(efsDescFile, dataFileName);
    }
}


/*----------------------------------------------------------------------------*/

close_namespace

