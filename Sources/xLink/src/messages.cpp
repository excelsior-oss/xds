#include <string.h>

#include "xdefs.h"

open_namespace

#include "args.h"
#include "xdebug.h"
#include "messages.h"
#include "xshell.h"
#include "xmem.h"
#include "idents.h"

int      TotalErrors   = 0;
int      TotalWarnings = 0;

struct msg{
  short   level;
  char  * text;
};

struct msg Messages[MESSAGE_TYPES] = {
  {1, "Unable to open file %s" },
  {1, "Unable to write file %s" },
  {1, "Unable to read file %s" },
  {1, "Insufficient memory" },
  {1, "Invalid system %s" },
  {1, "Invalid OS version %s" },
  {1, "Invalid number %s" },
  {1, "Invalid parameter - %s" },
  {1, "Invalid /ENTRY option" },
  {1, "Invalid %s value - 0x%X" },
  {1, "Unrecognized option %s - option ignored" },
  {1, "No file(s) specified" },
  {1, "No DLL initializing routine" },
  {1, "No program entry point" },
  {3, "Name %s was declared in %s and in %s" },
  {3, "Name %s was redeclared in %s" },
  {1, "Illegal RES file %s - must be 32-bit Microsoft format" },
  {1, "Illegal file format - %s" },
  {1, "Empty file %s" },
  {1, "File %s too long" },
  {1, "Duplicate definition for export name %s" },
  {1, "Entry point from %s ignored, used from %s" },
  {1, "File %s - illegal CPU type 0x%x" },
  {1, "File %s - cannot initialize BSS segment %s" },
  {1, "File %s - invalid weak external %s" },
  {1, "File %s - illegal symbol index %d" },
  {1, "File %s - unsupported fixup type %d" },
  {1, "File %s - invalid section %d" },
  {1, "File %s - bad storage class %d" },
  {1, "File %s - not expected end of file" },
  {1, "File %s (%d:%d) - identifier expected" },
  {1, "File %s (%d:%d) - string isn't closed" },
  {1, "File %s (%d:%d) - bad ordinal number" },
  {1, "File %s (%d:%d) - ordinal number expected" },
  {1, "File %s (%d:%d) - end of file expected" },
  {1, "File %s - unknown COMDAT length prefix %X" },
  {1, "File %s - invalid entry point" },
  {1, "File %s group %s - local groups not supported" },
  {1, "File %s - unsupported group type %X" },
  {1, "File %s - unknown common type %X" },
  {1, "File %s - too much data for segment %s" },
  {1, "File %s - LINNUM segment not specified" },
  {1, "File %s - fixups in BSS segment %s" },
  {1, "File %s - FIXUP without LEDATA" },
  {1, "File %s - unsupported OMF record type" },
  {1, "Illegal record length" },
  {1, "Record too long in file %s" },
  {1, "File %s - unknown record type %02X " },
  {1, "File %s - internal name %s not found" },
  {1, "File %s - duplicate definition for export name %s" },
  {1, "File %s - unresolved segment %s" },
  {1, "Invalid fixup target - file %s, fixup offset %4X" },
  {1, "Invalid fixup for a flat memory model" },
  {1, "Name '%s' not found" },
  {1, "Name '%s' not found (referenced in  %s)" },
  {1, "Invalid entry point target" },
  {1, "Illegal stub file %s" },
  {1, "File %s - unhandled EXE type or invalid .EXE" },
  {1, "File %s - unable to find EXPORT section" },
  {1, "Invalid option %s - option skipped" },
  {1, "Invalid image format \"%s\"" },
  {1, "Unable to mix debug information from modules - %s (%s), %s (%s)" },
  {1, "Duplicate debug information for module %s" },
  {1, "File %s - duplicate resource name" },
  {1, "There is no debug information - /DEBUG option is ignored" },
  {1, "File %s: invalid file format" },
  {1, "File %s: expected ')'" },
  {1, "FULNAM comment w/o MODNAM comments in %s" },
  {1, "Too many exported functions: %d" },
  {1, "Invalid /EXPORT option: %s" },
  {1, "Duplicate ordinals for names: %s, %s" },
  {1, "Cannot find array element type: %s" },
  {1, "Too many types for CODEVIEW debug info format: use HLL/EDIF   debug info" },
  {1, "Incompatibe object file versions: %s and %s"},
  {1, "Fixup in rdata section"},
  {1, "Unable to load resource %s (error code %d)"},
  {1, "Required COFF library %s member missing: %s"},
  {1, "Invalid COFF library: %s"},
  {1, "File %s: Incorrect %s record format: %s"},
  {1, "File %s: Invalid 0-terminating string in record at %X, at offset %X"},
  {1, "Illegal file format - %s: remove outdated .obj files" },
  {1, "Name '%s' not found (exported as %s)" },
  {2, "Obsolete record %s in file %s" },
  {1, "File %s: Unsupported XOMF format version %d (expected version %d)"},
  {1, "File %s (%d:%d) - %s expected"},
  {1, "Unable to create debug info: Reordering optimization employed"},
  {1, "Unable to allocate virtual address space: executable image is too big"},
  {1, "Resulting image is too big: it may not be executed on some systems"},
  {1, "File %s - unhandled ELF type or invalid file format" },
  {1, "Packed EFS %s successfully written (data file %s)." },
  {1, "Common object files %s and %s do not match." },
  {1, "ELF format does not support resources. Remove all *.res files from link set." },
  {1, "Unable to open file %s (%s)" },
  {1, "Types table overflow: too many (64K+) types defined in %s."},
  {1, "Unreferenced types table in %s"},                                   // msgUNREFERENCED_TYPES_TABLE
  {1, "Option \"%s\" should be specified before the option \"-config=%s:<value>\""},       // msgOPTION_REQUIRED_BEFORE_CONFIG
};

char * ERROR_MESSAGE_BUF  = NULL;
int    ERROR_MESSAGE_SIZE = 0;

jmp_buf ABORT_JMP_BUF;

void putMsg(int number, dword code, char * text, dword level) {
    if ((code == xWARNING) && (level > xWarningLevel)) return;

    if (xSilent) {
        int textLen = strlen (text);
        if (ERROR_MESSAGE_BUF) {
            ERROR_MESSAGE_BUF  = (char *) xrealloc (ERROR_MESSAGE_BUF, ERROR_MESSAGE_SIZE, textLen + ERROR_MESSAGE_SIZE + 1);
            ERROR_MESSAGE_SIZE = textLen + ERROR_MESSAGE_SIZE + 1;
        } else {
            ERROR_MESSAGE_SIZE = textLen + 2;
            ERROR_MESSAGE_BUF  = (char *) xalloc (ERROR_MESSAGE_SIZE);
            *ERROR_MESSAGE_BUF = '\0';
        }
        strcat (ERROR_MESSAGE_BUF, text);
        strcat (ERROR_MESSAGE_BUF, "\n");
    } else if (xUseShell) {
        SendError (MSG_SEVERE, number, -1, -1, "", text);
    } else {
        printf("%s (%d): ", code == xFATAL   ? "Fatal error" :
                            code == xERROR   ? "Error"       :
                            code == xWARNING ? "Warning"     : "Message", number);
        puts (text);
    }
    if (code == xFATAL) {
        if (xSilent)
            longjmp (ABORT_JMP_BUF, ERROR_EXIT);
        else
            exit (255);
    } else if (code == xERROR) {
        TotalErrors ++;
    } else if (code == xWARNING) {
        TotalWarnings ++;
    } else if (code != xMESSAGE) {
        ASSERT_FALSE();
    }
}


void Message(int code, int number,...) {
  char           buf [1024];
  va_list        vl;

  va_start(vl, number);
  vsprintf(buf, Messages[number].text, vl);
  putMsg(number, code, buf, Messages[number].level);
  va_end(vl);
}

void VerboseMessage(const char *fmt_str,...)
{
    if (xVerbose && !xSilent) {
       va_list        vl;
     
       va_start(vl, fmt_str);
       vprintf(fmt_str, vl);
       va_end(vl);
    }
}

int VerboseMask = 0;

void SetVerboseMask (char *infoName)
{
    if (strcmpIC (infoName, "sectionSize") == 0)
        VerboseMask |= INFO_SECTIONSIZE;
    else if (strcmpIC (infoName, "relocStat") == 0)
        VerboseMask |= INFO_RELOCSTAT;
    else if (strcmpIC (infoName, "OMFRead") == 0)
        VerboseMask |= INFO_OMFREAD;
    else if (strcmpIC (infoName, "SymbolDeclaration") == 0)
        VerboseMask |= INFO_SYMBOLDECLARATION;
    else if (strcmpIC (infoName, "TypeDesc") == 0)
        VerboseMask |= INFO_TYPEDESC;
    else if (strcmpIC (infoName, "DebugInfo") == 0)
        VerboseMask |= INFO_DEBUGINFO;
    else if (strcmpIC (infoName, "EILRead") == 0)
        VerboseMask |= INFO_EILREAD;
    else if (strcmpIC (infoName, "MemUsage") == 0)
        VerboseMask |= INFO_MEMUSAGE;
    else if (strcmpIC (infoName, "ImageCreation") == 0)
        VerboseMask |= INFO_IMAGECREATION;
    else if (strcmpIC (infoName, "JExport") == 0)
        VerboseMask |= INFO_JEXPORT;
    else if (strcmpIC (infoName, "JavaStrings") == 0)
        VerboseMask |= INFO_JAVASTRINGS;
    else if (strcmpIC (infoName, "UnusedDLLImport") == 0)
        VerboseMask |= INFO_UNUSEDDLLIMPORT;
    else if (strcmpIC (infoName, "StringOpt") == 0)
        VerboseMask |= INFO_STRINGOPT;
    else if (strcmpIC (infoName, "StrOptProgress") == 0)
        VerboseMask |= INFO_STROPTPROGRESS;
    else if (strcmpIC (infoName, "VirtualAddresses") == 0)
        VerboseMask |= INFO_VIRTUALADDRESSES;
    else if (strcmpIC (infoName, "ReadLinkInfo") == 0)
        VerboseMask |= INFO_READLINKINFO;
    else if (strcmpIC (infoName, "XOMFRead") == 0)
        VerboseMask |= INFO_XOMFREAD;
    else if (strcmpIC (infoName, "NullChecks") == 0)
        VerboseMask |= INFO_NULLCHECKS;
    else if (strcmpIC (infoName, "ELFRead") == 0)
        VerboseMask |= INFO_ELFREAD;
    else if (strcmpIC (infoName, "ZIPRead") == 0)
        VerboseMask |= INFO_ZIPREAD;
    else
        Message (xERROR, msgINVALID_PARAMETER, infoName);
}

void VerboseHelp ()
{
    if (xVerbose) {

        puts ("\n"
              " /verbose[=<verbose option>]           \n"
              " <verbose option> ::= sectionSize      \n"
              "                    | relocStat        \n"
              "                    | OMFRead          \n"
              "                    | XOMFRead         \n"
              "                    | SymbolDeclaration\n"
              "                    | TypeDesc         \n"
              "                    | DebugInfo        \n"
              "                    | EILRead          \n"
              "                    | MemUsage         \n"
              "                    | ImageCreation    \n"
              "                    | JExport          \n"
              "                    | JavaStrings      \n"
              "                    | UnusedDLLImport  \n"
              "                    | StringOpt        \n"
              "                    | StrOptProgress   \n"
              "                    | VirtualAddresses \n"
              "                    | ReadLinkInfo     \n"
              "                    | NullChecks       \n"
              "                    | ELFRead          \n"
              "                    | ZIPRead          \n"
             );
    }
}


void VerboseMessage (int infoKind, const char *fmt_str,...)
{
    if (!xSilent && ((VerboseMask & infoKind) != 0)) {
       va_list        vl;
     
       va_start(vl, fmt_str);
       vprintf(fmt_str, vl);
       va_end(vl);
    }
}

Bool IsPrintable (int infoKind)
{
    return ((VerboseMask & infoKind) != 0);
}

close_namespace

