
#include <string.h>
#include <errno.h>

#include "xdefs.h"

open_namespace

#include "xos.h"
#include "struct.h"
#include "idents.h"
#include "xmem.h"
#include "messages.h"

#include "readedf.h"
#include "reader.h"
#include "efs.h"

#include "xshell.h"

#define CPB_OPTIONS_DECLARATION
#include "cpb.h"

#include "args.h"

Bool xDoDebug              = false;
Bool xSmart                = true;
Bool xFixed                = false;
Bool xExtraAlignment       = false;
Bool xDLLFlag              = false;
Bool xCreateTimeDateStamps = false;
Bool xImpLibFlag           = false;
Bool xUseOrdFlag           = false;
Bool xDoMapFile            = false;
Bool xMapByName            = false;
Bool xWasOutputName        = false;
Bool xUseShell             = false;
Bool xNoEntryPoint         = false;
Bool xWasEntryPoint        = false;
Bool xWasImageBase         = false;
Bool xWasStub              = false;
Bool xVerbose              = false;
Bool xEILImpLib            = false;
Bool xWritableCodeSection  = false;
Bool xStrictLinkToDLL      = false;
Bool xMeasureTime          = false;
Bool xNoExportNames        = false;
Bool xNoConsistencyInfo    = false;
Bool xSilent               = false;
Bool xWriteLinkInfo        = false;
Bool xReadLinkInfo         = false;
Bool xSparse               = false;
Bool xAddImportJumpsCodeviewPublic = false;
Bool xEmitNullCheckInfo    = false;
Bool xUseProgramInterpreter= false;
Bool xLargeAddressAware    = false;
Bool xNoLink               = false;
Bool xJetComponent         = false;
Bool xAutoImageBase        = false;
Bool xEmitDynamicLookupTable = false;
Bool xEmitStackTraceInfo   = false;
Bool xNoDynstrSizeCheck    = false;
Bool xPrintSectionSizes    = false;
Bool xWriteGCCImportLibrary= false;
Bool xSplitTypesTable      = false;


#define DEFAULT_IMAGEBASE_PE   0x00400000
#define DEFAULT_OBJOFFSET_PE   0x00001000

#define DEFAULT_IMAGEBASE_LX            0
#define DEFAULT_OBJOFFSET_LX   0x00010000

#define DEFAULT_IMAGEBASE_ELF  0x08048000
#define DEFAULT_OBJOFFSET_ELF  0x00001000

#if (xDEFAULT_IMAGE_FORMAT == xPE_IMAGE_FORMAT)
dword xImageBase    = DEFAULT_IMAGEBASE_PE;
dword xObjectOffset = DEFAULT_OBJOFFSET_PE;
#elif (xDEFAULT_IMAGE_FORMAT == xLX_IMAGE_FORMAT)
dword xImageBase    = DEFAULT_IMAGEBASE_LX;
dword xObjectOffset = DEFAULT_OBJOFFSET_LX;
#elif (xDEFAULT_IMAGE_FORMAT == xELF_IMAGE_FORMAT)
dword xImageBase    = DEFAULT_IMAGEBASE_ELF;
dword xObjectOffset = DEFAULT_OBJOFFSET_ELF;
#endif

dword xFileAlign    = 0x00000200;
dword xSystem       = xSUBSYSTEM_CUI;
dword xStackSize    = 0x00010000;
dword xStackCommit  = 0x00002000;
dword xHeapSize     = 0x00002000;
dword xHeapCommit   = 0x00001000;
dword xOSmajor      = 4;
dword xOSminor      = 0;
dword xWarningLevel = DEFAULT_WARNING_LEVEL;
dword xOptStrLevel  = DEFAULT_OPTSTR_LEVEL;
dword xVCode        = 0;
dword xEncryptStrings = ENCRYPTION_KEY_UNDEFINED_VALUE;
dword xMemReserveSize = 0;


char * xOutputFileName = NULL;
char * xOutputLibName  = NULL;
char * xMapFileName    = NULL;
char * xStubFileName   = NULL;
const char * xEntryPointFile = NULL;

char * xWriteLinkInfoFile = NULL;
char * xReadLinkInfoFile  = NULL;

char * xProgramInterpreter         = NULL;
char * xAutoImageBasePrevComponent = NULL;
char * xDynamicLookupTable         = NULL;
char * xSectionSizesLog            = NULL;

dword xIMAGE_FORMAT = xDEFAULT_IMAGE_FORMAT;

//-----------------------------------------------------------------------------

struct Job * JobList = NULL;
struct Job * LastJob = NULL;
struct Job * CurJob  = NULL;

void NewJob (int kind, char *param) {
    ASSERT ((kind == JOB_READ_FILE) || (kind == JOB_READ_FROM_RESOURCE));

    struct Job * j = (struct Job *) allocateForever (sizeof (struct Job));
    j -> kind      = kind;
    j -> parameter = dup (param, strlen (param));
    j -> next      = NULL;

    j -> xSmart           = xSmart;
    j -> xStrictLinkToDLL = xStrictLinkToDLL;

    if (LastJob) {
        LastJob -> next = j;
        LastJob = j;
    } else
        JobList = LastJob = j;
}

struct Job * getNextJob () {
    if (!CurJob) {
        if (!JobList) return NULL;
        CurJob = JobList;
    } else
        CurJob = CurJob -> next;

    if (CurJob) {
        xSmart           = CurJob -> xSmart;
        xStrictLinkToDLL = CurJob -> xStrictLinkToDLL;
    }
    return CurJob;
}

//-----------------------------------------------------------------------------

#define COMMENT_CHAR ';'

static class Option * Options;

//-----------------------------------------------------------------------------

class Option : public ForeverObject {
    public:
        const char   * name;
        class Option * next;

        Option() {}
        Option (const char *Name) {  
            this -> name = Name;
            this -> next = Options;
            Options      = this;
        }
        virtual void Interpret(char *value) = 0;
};

//-----------------------------------------------------------------------------

class BooleanOption : public Option {
  private:
    Bool * flag;
    Bool   BoolOptionType;
  public:
    BooleanOption (const char *Name, Bool * flag, Bool BoolOptionType = true) : Option(Name)
    {  
       this -> flag = flag;
       this -> BoolOptionType = BoolOptionType;
    }
    void Interpret(char *)
    {
       *flag = BoolOptionType;
    }
};

//-----------------------------------------------------------------------------

class BoolIdOption : public Option {
  private:
    Bool  * flag;
    char ** id;
  public:
    BoolIdOption(const char *Name, Bool * flag, char ** id) : Option (Name)
    {  
       this -> flag = flag;
       this -> id   = id;
    }
    void Interpret(char *value)
    {
       *flag = true;
       *id   = ((value == NULL) ? NULL : dup(value, strlen(value)));
    }
};

//-----------------------------------------------------------------------------

dword ParseNumber (char *value) {
    char *end;
    dword n;

    if (*value == 'x' || *value == 'X')
        n = strtoul (value + 1, &end, 16);
    else if ((*value == '0') && ((value [1] == 'x') || (value [1] == 'X')))
        n = strtoul (value + 2, &end, 16);
    else
        n = strtoul (value, &end, 10);
    if ((*end == 'k') || (*end == 'K'))
        n *= 1024;
    else if ((*end == 'm') || (*end == 'M'))
        n *= 1024 * 1024;
    else if (*end != 0)
        Message(xFATAL, msgINVALID_NUMBER, value);

    return n;
}


class NumericOption : public Option {
  protected:
    dword * num;
  public: 
    NumericOption() {}
    NumericOption(const char *Name, dword * num) : Option (Name)
    {  
       this->num = num;
    }
    void Interpret(char *value)
    {
        if ((value == NULL) || (*value == 0))
            return;

        * num = ParseNumber (value);
    }
};

//-----------------------------------------------------------------------------

class EDFReadOption : public Option {
  private:
    Bool * flag;
  public:
    EDFReadOption (const char *Name, Bool * flag) : Option (Name)
    {  
       this -> flag = flag;
    }
    void Interpret(char *value)
    {
        if (flag != NULL)
            *flag = true;
        if((value != NULL) && (*value != 0))
            ReadEdf(HasExtension (value) ? value : MakeExtension (value, "edf"));
    }
};

//-----------------------------------------------------------------------------

class ImageFormatOption : public Option {
  public:
    ImageFormatOption(const char *Name) : Option (Name) {}

    void Interpret(char *value)
    {
       if (value == NULL) return;
       if (!strcmpIC (value, "pe"))
       {
          xIMAGE_FORMAT = xPE_IMAGE_FORMAT;
          if (!xWasImageBase) xImageBase = DEFAULT_IMAGEBASE_PE;
          xObjectOffset = DEFAULT_OBJOFFSET_PE;

       } else if (!strcmpIC(value, "lx"))
       {
          xIMAGE_FORMAT = xLX_IMAGE_FORMAT;
          if (!xWasImageBase) xImageBase = DEFAULT_IMAGEBASE_LX;
          xObjectOffset = DEFAULT_OBJOFFSET_LX;

       } else if (!strcmpIC(value, "elf"))
       {
          xIMAGE_FORMAT = xELF_IMAGE_FORMAT;
          if (!xWasImageBase) xImageBase = DEFAULT_IMAGEBASE_ELF;
          xObjectOffset = DEFAULT_OBJOFFSET_ELF;

       } else {
          Message(xFATAL, msgINVALID_IMAGE_FORMAT, value);
       }
    }
};

//-----------------------------------------------------------------------------

class HelpOption : public Option {
  public:
    HelpOption(const char *Name) : Option (Name) {}

    void Interpret(char *) {
       ShowHelp();
    }
};

//-----------------------------------------------------------------------------

class ShellOption : public Option {
  private:
    Bool * flag;
  public:
    ShellOption(const char *Name, Bool * flag) : Option (Name)
    {  
       this -> flag = flag;
    }
    void Interpret(char *) {
       *flag = (ConnectShell() != 0) ? true : false;
    }
};

//-----------------------------------------------------------------------------

class ExportOption : public Option {
  public:
    ExportOption (const char *Name) : Option (Name) {}

    void Interpret(char *value)
    {
       if((value == NULL) || (*value == 0))
          Message(xFATAL, msgINVALID_PARAMETER, value);

       while (*value != 0)
       {
          char * iname, * ord;
          word ordinal = 0;

          if ((*value == ',') || (*value == '.') || (*value == '='))
             Message(xFATAL, msgINVALID_EXPORT_OPTION, value);

          char* buf = (char *) xalloc (strlen(value)+1);
          int i = 0;
          for (; (*value != 0) && (*value != ','); value++, i++) {
             buf [i] = *value;
          }
          buf [i] = '\0';

          iname = strchr (buf, '=');
          if (iname != NULL) {
             *iname ++ = '\0';
             if (*iname == 0) Message(xFATAL, msgINVALID_EXPORT_OPTION, value);
          } else
             iname = buf;
          ord = strchr (buf, '.');
          if (ord) {
             *ord ++ = '\0';
             if (*ord == 0) Message(xFATAL, msgINVALID_EXPORT_OPTION, value);
             char * end;
             ordinal = (word)strtoul (ord, & end, 10);
             if (*end != 0) Message(xFATAL, msgINVALID_EXPORT_OPTION, value);
          }
          ident extname = NAMES.Str2Index (buf,   strlen (buf));
          ident intname = NAMES.Str2Index (iname, strlen (iname));
          NewExport (extname, intname, NULL, 0, ordinal, EFLAG_SOURCE_CMDLINE, INVALID_ID);

          xfree (buf);
       }
    }
};

//-----------------------------------------------------------------------------

class SysOption : public Option {
  public:
    SysOption (const char *Name) : Option (Name) {}

    void Interpret(char *value)
    {
       char * ver;
       if ((value == NULL) || (*value == 0)) return;
       ver = strchr (value, ',');
       if (ver != NULL) {
          if(xIMAGE_FORMAT == xPE_IMAGE_FORMAT) {
             char *end;
             *ver++ = 0;
             if (*ver == 0) Message(xFATAL, msgINVALID_OS_VERSION, "");
             xOSmajor = xOSminor = 0;
             xOSmajor = strtoul (ver, & end, 10);
             if (end != NULL) {
               if (*end != '.') Message(xFATAL, msgINVALID_OS_VERSION, ver);
               xOSminor = strtoul (end + 1, &end, 10);
               if (*end != 0) Message(xFATAL, msgINVALID_OS_VERSION, ver);
             }
          } else {
             Message(xFATAL, msgINVALID_SYSTEM, value);
          }
       }
       if (xIMAGE_FORMAT == xPE_IMAGE_FORMAT) {
          if (!strcmpIC(value, "w")) {
             xSystem = xSUBSYSTEM_GUI;
          } else if (!strcmpIC(value, "c")) {
             xSystem = xSUBSYSTEM_CUI;
          } else {
             Message(xFATAL, msgINVALID_SYSTEM, value);
          }
       } else if(xIMAGE_FORMAT == xLX_IMAGE_FORMAT) {
          if (!strcmpIC(value, "pm")){
             xSystem = xSUBSYSTEM_GUI;
          } else if (!strcmpIC(value, "vio")) {
             xSystem = xSUBSYSTEM_CUI;
          } else if (!strcmpIC(value, "novio")) {
             xSystem = xSUBSYSTEM_FS;
          } else {
            Message(xFATAL, msgINVALID_SYSTEM, value);
          }
       }
    }
};

//-----------------------------------------------------------------------------

class EntryOption : public Option {
  public:
    EntryOption(const char *Name) : Option (Name) {}

    void Interpret(char *value)
    {
        if ((value == NULL) || (*value == 0))
            Message(xFATAL, msgINVALID_ENTRY_OPTION);

        if (xWasEntryPoint) {
            Message(xWARNING, msgIGNORED_ENTRY_POINT, xEntryPointFile, "command line");
            xfree (EntryPoint);
        }

        setEntryPoint (FIXUP_FAR16_32,
                       TK_ID,
                       (void *) NAMES.Str2Index (value),
                       0,
                       "command line");
    }
};

//-----------------------------------------------------------------------------

class ImplibOption : public Option {
  public:
    ImplibOption(const char *Name) : Option (Name) {}

    void Interpret(char *value)
    {
       xImpLibFlag = true;
       if ((value != NULL) && (*value != 0))
          xOutputLibName = dup (value, strlen (value));
    }
};

//-----------------------------------------------------------------------------

class AlignOption : public NumericOption {
  public:
    AlignOption(const char *Name, dword * num) : NumericOption(Name, num) {}

    void Interpret(char *value)
    {
       NumericOption :: Interpret(value);
       Bool found;
       for (int i = 0; i < 31; i ++)
          if ((dword) (1 << i) == xFileAlign) {
             found = true;
             break;
          }
       if (!found)
          Message(xFATAL, msgINVALID_OPTION_VALUE, "/ALIGN", xFileAlign);
    }
};

//-----------------------------------------------------------------------------

class ImageBaseOption : public NumericOption {
  public:
    ImageBaseOption(const char *Name, dword * num) : NumericOption(Name, num) {}

    void Interpret(char *value)
    {
       NumericOption :: Interpret(value);
       xWasImageBase = true;
    }
};

//-----------------------------------------------------------------------------

class NumLevelOption : public NumericOption {
  private:
    dword maxLevel;
  public:
    NumLevelOption(const char *Name, dword * num, dword _maxLevel) : NumericOption(Name, num),
                                                               maxLevel (_maxLevel) {}

    void Interpret(char *value)
    {
       dword numSaved = (* num);
       NumericOption :: Interpret(value);
       if ((* num) > maxLevel) {
          char b [1024];
          strcpy (b, "/");
          strcat (b, name);
          str2UpperCase (b);
          Message(xMESSAGE, msgOPTION_SKIPPED, b);
          * num = numSaved;
       }
    }
};

//-----------------------------------------------------------------------------

class VerboseOption : public Option {
    public:
        VerboseOption (const char *Name) : Option (Name) {}

        void Interpret (char *value)
        {
            if ((value == NULL) || (*value == '\0'))
                xVerbose = true;
            else
                SetVerboseMask (value);
        }
};

//-----------------------------------------------------------------------------

#define CONFIG_DELIMITER   ':'

class ConfigOption : public Option {
    public:
        ConfigOption (const char *Name) : Option (Name) {}

        void Interpret (char *value)
        {

            if ((value == NULL) || (*value == '\0'))
                Message(xFATAL, msgINVALID_PARAMETER, value);

            char *d = strchr (value, CONFIG_DELIMITER);
            if (!d)
                Message(xFATAL, msgINVALID_PARAMETER, value);

            * (d++) = '\0';

            for (int i = 0; i < CPB_OPTIONS; i++) {
                if (strcmpIC (value, CPBKeys [i].keyName) == 0) {
                    switch (CPBKeys[i].optionType) {
                        case CPB_KEY_TYPE_CARD32: NewCPBOption (i, ParseNumber (d));
                                                  break;

                        case CPB_KEY_TYPE_STRING: NewCPBOption (i, d);
                                                  break;

                        case CPB_KEY_TYPE_ENCRYPTEDSTRING: 
                                                  NewEncryptedCPBOption(i, d);
                                                  break;
                        default:
                            ASSERT_FALSE ();
                    }
                    return;
                }
            }

            * (--d) = CONFIG_DELIMITER;
            Message(xFATAL, msgINVALID_PARAMETER, value);
        }
};

//-----------------------------------------------------------------------------

#define DEFINE_DELIMITER   ':'
#define DEFINE_SIGN        '$'


class Define;
static Define * DefList = NULL;


class Define {
  private:
    char * name;
    char * value;
    Define * next;

  public:
    Define (const char * name, const char * value) {
        this -> name  = dup(name,  strlen(name)+1);
        this -> value = dup(value, strlen(value)+1);

        this -> next = DefList;
        DefList = this;
    }

    static const char * get(const char * name) {
        for (Define * d = DefList; d != NULL; d = d->next) {
            if (!strcmp (d->name, name)) {
                return d->value;
            }
        }
        return NULL;
    }

};


class DefineOption : public Option {
    public:
        DefineOption (const char *name) : Option (name) {}

        void Interpret (char *value)
        {
            if ((value == NULL) || (*value == '\0'))
                Message(xFATAL, msgINVALID_PARAMETER, value);

            char *d = strchr (value, DEFINE_DELIMITER);
            if (!d)
                Message(xFATAL, msgINVALID_PARAMETER, value);

            * (d++) = '\0';
            new Define (value, d);
            * (--d) = DEFINE_DELIMITER;
        }
};


const char * getDefine (const char *defName) {
    return Define :: get(defName);
}


char * processDefsAndDup (const char *str)
{
    char * s = dup (str, strlen(str)+1);
    char * cur = s;

    for (;;) {
        char * d = strchr (cur, DEFINE_SIGN);
        if (!d) {
            return s;
        }

        char * end = NULL;
        if ((d[0] == DEFINE_SIGN) && (d[1] == '(') &&
            ((end = strchr (d, ')')) != NULL))
        {
            *end = '\0';
            const char * val = getDefine (d + 2);
            if (val != NULL) {
                // define found, perform substitution
                *d   = '\0';
                char * buf = (char *) xalloc (strlen(s)+strlen(val)+strlen(end+1)+1);
                sprintf (buf, "%s%s%s", s, val, end+1);
                cur = buf + strlen(s) + strlen(val);
                xfree (s);
                s = buf;
            } else {
                *end = ')';
                cur = end + 1;
            }
        } else {
            cur = d + 1;
        }
    }
}

//-----------------------------------------------------------------------------

class ReserveMemoryOption : public NumericOption {
  public:
    ReserveMemoryOption(const char *Name, dword * num) : NumericOption(Name, num) {}

    void Interpret(char *value)
    {
       NumericOption :: Interpret(value);
       if (!reserveMemory(xMemReserveSize)) {
          Message(xFATAL, msgINVALID_OPTION_VALUE, "/ReserveMemory", xMemReserveSize);
       }
    }
};

//-----------------------------------------------------------------------------

class EmbeddedFileSysOption : public Option {
  public:
    EmbeddedFileSysOption (const char *Name) : Option (Name)
    {}

    void Interpret (char *value)
    {
        if((value != NULL) && (*value != 0)) {
            ReadEmbeddedFS(value, MakeExtension (value, "efsdata"));
        }
    }
};

//-----------------------------------------------------------------------------

class PackEmbeddedFileSysOption : public Option {
  public:
    PackEmbeddedFileSysOption (char *Name) : Option (Name)
    {}

    void Interpret (char *value)
    {
        if((value != NULL) && (*value != 0)) {
            SerializeEmbeddedFS (value, MakeExtension (value, "efsdata"));
            xNoLink = true;
        }
    }
};


//-----------------------------------------------------------------------------

class JetComponentOption : public Option {
  public:
    JetComponentOption (const char *Name) : Option (Name) {}

    void Interpret(char *value)
    {
       xJetComponent = true;

       if ((value == NULL) || (*value == 0)) return;
       char* minorVer = strchr (value, '.');
       ASSERT (minorVer != NULL);
       *minorVer++ = 0;

       char *end = NULL;

       int major = strtoul (value, &end, 10);
       ASSERT (*end == 0);
       ASSERT ((major >= 0) && (major <= 0xFF));

       int minor = strtoul (minorVer, &end, 10);
       ASSERT (*end == 0);
       ASSERT ((minor >= 0) && (minor <= 0xFF));

       ASSERT (VerStamp != NULL);
       VerStamp->setVersion ((byte)minor, (byte)major);
    }
};

//-----------------------------------------------------------------------------

class JetEditionOption : public Option {
  public:
    JetEditionOption (const char *Name) : Option (Name) {}

    void Interpret(char *value)
    {
       xJetComponent = true;

       if ((value == NULL) || (*value == 0)) return;

       ASSERT (VerStamp != NULL);

       if (!strcmpIC(value, "evaluation")) {
           VerStamp->setEdition (JET_EDITION_EVALUATION);
       } else if (!strcmpIC(value, "professional")) {
           VerStamp->setEdition (JET_EDITION_PROFESSIONAL);
       } else if (!strcmpIC(value, "standard")) {
           VerStamp->setEdition (JET_EDITION_STANDARD);
       } else if (!strcmpIC(value, "enterprise")) {
           VerStamp->setEdition (JET_EDITION_ENTERPRISE);
       } else if (!strcmpIC(value, "embedded")) {
           VerStamp->setEdition (JET_EDITION_EMBEDDED);
       } else if (!strcmpIC(value, "embedded_evaluation")) {
           VerStamp->setEdition (JET_EDITION_EMBEDDED_EVALUATION);
       } else {
           Message(xFATAL, msgINVALID_PARAMETER, value);
       }
    }
};

//-----------------------------------------------------------------------------

class LinkFileAsRDataOption : public Option {
  public:
    LinkFileAsRDataOption (const char *Name) : Option (Name) {}

    void Interpret(char *value)
    {
        if ((value == NULL) || (*value == '\0'))
            Message(xFATAL, msgINVALID_PARAMETER, value);

        char *d = strchr (value, CONFIG_DELIMITER);
        if (!d)
            Message(xFATAL, msgINVALID_PARAMETER, value);

        * (d++) = '\0';

        LinkFileIntoSegment (CONST, NAMES.Str2Index(value), d);
    }
};


//-----------------------------------------------------------------------------

static char * cutSpaces(char * in)
{
    char * out;
  
    out = in;
    while(*out == ' ') out++;
    in = in + strlen(in) - 1;
    while(in != out && *in == ' ') in --;
    *(in+1) = 0;
    return out;
}

void ReadResponseFile(const char *filename)
{
    if ((filename == NULL) || (*filename == 0)) return;
    FILE * fp = fopen (filename, "r");
    if (fp == NULL) Message(xFATAL, msgUNABLE_TO_OPEN_FILE_MSG, filename, strerror(errno));

    fseek (fp, 0L, SEEK_END);
    int size = ftell (fp);
    fseek (fp, 0L, SEEK_SET);
    if (size != 0) {
        char* buf = (char*) xalloc (size + 1);
        for (;;) {
           memset (buf, 0, size + 1);
           if (! fgets (buf, size, fp)) break;
           buf[size] = 0;
           int n = strlen (buf);
           if (n && (buf [n - 1] == '\r' || buf [n - 1] == '\n'))
              buf [n - 1] = 0;
           if (buf[0] != COMMENT_CHAR) {
              ParseArgument (cutSpaces(buf));
           }
        }
        xfree (buf);
    }
    fclose (fp);
}

//-----------------------------------------------------------------------------

void ParseArgument(char *Arg)
{
    if ((Arg == NULL) || (*Arg == 0)) return;
    if (*Arg == '@') {
        Arg++;
        ReadResponseFile(HasExtension (Arg) ? Arg : MakeExtension (Arg, "lnk"));
    }
    else if ((*Arg == '-')
#ifndef xos_LINUX
             || (*Arg == '/')
#endif
            )
    {
        char * value = strchr(Arg+1, '=');
        if (value == NULL) value = strchr(Arg+1, ':');
        if (value != NULL) *value++ = 0;
        Option * opt = Options;
        Bool option_found = false;
        while (opt != NULL) {
            if (!strcmpIC (Arg+1, opt->name)) {
                opt->Interpret (value);
                option_found = true;
                break;
            } else
                opt = opt->next;
        }
        if (!option_found)
            Message(xFATAL, msgINVALID_OPTION, Arg);
    } else if (!strcmp(Arg, "?")) {
        ShowHelp();
    } else if (*Arg == '*') {
        NewJob (JOB_READ_FROM_RESOURCE, Arg);
    } else {
        if (xOutputFileName == NULL)
            xOutputFileName = MakeExtension(Arg, "");
        NewJob (JOB_READ_FILE, HasExtension (Arg) ? Arg : MakeExtension (Arg, "obj"));
    }
}

//-----------------------------------------------------------------------------

void ParseCommandLine (const char* cmdLine, Bool skipZeroArg)
{
    Bool skipArg = skipZeroArg;
    Bool quoted = false;
    char* buf = (char *) xalloc (strlen(cmdLine) + 1);

    while (*cmdLine != 0) {
        // skip spaces
        while((*cmdLine != 0) && (*cmdLine==' '))
            cmdLine++;

        // extract one argument
        int pos = 0;
        for (;;) {
            char ch = *cmdLine;
            if ((ch == 0) || (!quoted && (ch == ' '))) {
                break;
            }
            if ((ch == '\\') && (cmdLine[1] == '\"')) {
                buf[pos++] = '\"';
                cmdLine++;
            } else if (ch == '\"') {
                quoted = !quoted;
            } else {
                buf[pos++] = ch;
            }
            cmdLine++;
        }
        buf[pos++] = 0;

        // parse argument
        if (!skipArg) {
            ParseArgument(buf);
        } else {
            skipArg = false;
        }
    }

    xfree(buf);
}


//-----------------------------------------------------------------------------

void ShowHelp (void)
{
    puts ("Usage: xlink [options] [@xxxx] file1 file2 ... lib1 lib2 ...\n"
          "@xxxx indicates use response file xxxx\n"
          "Options:\n"
          "    /ALIGN=#[k]                Specify file alignment (default 0x00000200)\n"
          "    /BASE=#[k]                 Specify image base\n"
          "    /D[EBUG]                   Full symbolic debug information\n"
          "    /DLL[=edffile]             Create dynamic link library\n"
          "    /ENTRY=id                  Specify entry point/DLL initialization routine\n"
          "    /EXP[ORT]=id[.#][=id],...  Specify exported names\n"
          "                               (with an optional ordinal and internal name)\n"
          "    /EXTRA[ALIGNMENT]          Align all at a 16-byte boundary, if necessary\n"
          "    /F[IXED]                   Do not produce relocation info\n"
          "    /HEAPCOMMIT=#[k]           Specify heap commit size (default 0x00001000)\n"
          "    /HEAP[SIZE]=#[k]           Specify maximum heap size (default 0x00002000)\n"
          "    /IMAGE[FORMAT]=format      Specify image format\n"
          "                               (valid values are \"PE\", \"LX\" and \"ELF\"\n"
          "                               default is host platform's native format)\n"
          "    /IMPLIB[=libfile]          Create import library\n"
          "    /M[AP][=mapfile]           Create map file\n"
          "    /NAME=file                 Specify output file name\n"
          "    /NOENTRY                   Do not set entry point\n"
          "    /[NO]SMART                 Turn smart linking for subsequent files on/off\n"
          "                               (default on)\n"
          "    /STACKCOMMIT=#[k]          Specify stack commit size (default 0x00002000)\n"
          "    /STACK[SIZE]=#[k]          Specify maximum stack size (default 0x00010000)\n"
          "    /STUB=xxxx                 Use stub file xxxx\n"
          "    /SYS=C|W[,#[.#]]           Win32: Create console or windows application\n"
          "                               (and specify major and minor OS version)\n"
          "    /SYS=NOVIO|VIO|PM          OS/2: Create NOVIO, VIO or PM application\n"
          "    /TIME[STAMPS]              Create time and date stamps\n"
          "    /WARN=#                    Specify warning level (0-3)\n"
          "    /SPARSE                    Link in a \"sparse\" mode for page fault profiling\n"
          "    /WRITELINKINFO[=file]      Write link information to the file\n"
          "    /READLINKINFO=file         Read link information from the file\n"
          "-option is also accepted"
         );
    VerboseHelp ();
    ASSERT (!xSilent);
    exit(0);
}

//-----------------------------------------------------------------------------

void InitOptions(void)
{
    // Set defaults

    // Create options
    Options = NULL;

    new BooleanOption("d",              &xDoDebug);
    new BooleanOption("debug",          &xDoDebug);

    new BooleanOption("smart",          &xSmart);
    new BooleanOption("nosmart",        &xSmart, false);

    new BooleanOption("f",              &xFixed);
    new BooleanOption("fixed",          &xFixed);

    new BooleanOption("extra",          &xExtraAlignment);
    new BooleanOption("extraalignment", &xExtraAlignment);

    new BooleanOption("time",           &xCreateTimeDateStamps);
    new BooleanOption("timestamps",     &xCreateTimeDateStamps);

    new BooleanOption("useord",         &xUseOrdFlag);
    new BooleanOption("noentry",        &xNoEntryPoint);

    new BooleanOption("eilimplib",           &xEILImpLib);
    new BooleanOption("writablecodesection", &xWritableCodeSection);
    new BooleanOption("silent",              &xSilent);
    new BooleanOption("noexportnames",       &xNoExportNames);
    new BooleanOption("noconsistencyinfo",   &xNoConsistencyInfo);
    new BooleanOption("measuretime",         &xMeasureTime);

    new BooleanOption("strictlinktodll",     &xStrictLinkToDLL);
    new BooleanOption("nostrictlinktodll",   &xStrictLinkToDLL, false);
    new BooleanOption("sparse",              &xSparse);
    new BooleanOption("emitnullcheckinfo",   &xEmitNullCheckInfo);
    new BooleanOption("emitstacktraceinfo",  &xEmitStackTraceInfo);
    new BooleanOption("largeaddressaware",   &xLargeAddressAware);
    new BooleanOption("nodynstrsizecheck",   &xNoDynstrSizeCheck);

    new BooleanOption("addimportjumpscodeviewpublic", &xAddImportJumpsCodeviewPublic);

    new BooleanOption("writegccimportlibrary", &xWriteGCCImportLibrary);

    new BooleanOption("splittypestable", &xSplitTypesTable);

    new BoolIdOption("m",    &xDoMapFile,     &xMapFileName);
    new BoolIdOption("map",  &xDoMapFile,     &xMapFileName);

    new BoolIdOption("name", &xWasOutputName, &xOutputFileName);
    new BoolIdOption("stub", &xWasStub,       &xStubFileName);

    new BoolIdOption("writelinkinfo", &xWriteLinkInfo, &xWriteLinkInfoFile);
    new BoolIdOption("writeli",       &xWriteLinkInfo, &xWriteLinkInfoFile);
    new BoolIdOption("readlinkinfo",  &xReadLinkInfo,  &xReadLinkInfoFile);
    new BoolIdOption("readli",        &xReadLinkInfo,  &xReadLinkInfoFile);

    new BoolIdOption("programinterpreter", &xUseProgramInterpreter, &xProgramInterpreter);

    new BoolIdOption("autoimagebase",          &xAutoImageBase,          &xAutoImageBasePrevComponent);
    new BoolIdOption("emitdynamiclookuptable", &xEmitDynamicLookupTable, &xDynamicLookupTable);

    new BoolIdOption("printsectionsizes",      &xPrintSectionSizes,      &xSectionSizesLog);

    new NumericOption("heap",      &xHeapSize);
    new NumericOption("heapsize",  &xHeapSize);

    new NumericOption("heapcommit",  &xHeapCommit);

    new NumericOption("stack",     &xStackSize);
    new NumericOption("stacksize", &xStackSize);

    new NumericOption("stackcommit", &xStackCommit);
    new NumericOption("vcode",       &xVCode);

    new NumLevelOption   ("warn",    &xWarningLevel, MAX_WARNING_LEVEL);
    new NumLevelOption   ("optstr",  &xOptStrLevel, MAX_OPTSTR_LEVEL);

    new NumLevelOption   ("zimer", &xEncryptStrings, ENCRYPTION_KEY_MAX_VALUE);

    new EDFReadOption    ("dll", &xDLLFlag);
    new EDFReadOption    ("edf", NULL);

    new ImageFormatOption("image");
    new ImageFormatOption("imageformat");
    new EntryOption      ("entry");

    new ImageBaseOption  ("base",  &xImageBase);
    new AlignOption      ("align", &xFileAlign);
    new ShellOption      ("shell", &xUseShell);

    new SysOption        ("sys");
    new ExportOption     ("exp");
    new ExportOption     ("export");
    new ImplibOption     ("implib");
    new VerboseOption    ("verbose");
    new ConfigOption     ("config");
    new DefineOption     ("define");

    new ReserveMemoryOption ("reservememory", &xMemReserveSize);

    new EmbeddedFileSysOption ("embeddedfilesys");
    new PackEmbeddedFileSysOption ("packembeddedfilesys");

    new JetComponentOption("jetcomponent");
    new JetEditionOption("jetedition");

    new LinkFileAsRDataOption("linkfileasrdata");

    new HelpOption("h");
    new HelpOption("help");
    new HelpOption("?");
}

//-----------------------------------------------------------------------------

close_namespace
