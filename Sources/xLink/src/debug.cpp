#include <stdlib.h>
#include <string.h>

#include "xdefs.h"

open_namespace

#include "struct.h"
#include "debug.h"

#include "dbghll.h"
#include "dbg99.h"
#include "dbgcv.h"
#include "dbgedif.h"

#include "xmem.h"
#include "xdebug.h"
#include "messages.h"
#include "idents.h"
#include "writer.h"

byte  DebugFormat = DBG_FMT_NO;

const char * FirstHLL4Module = NULL;
const char * FirstNB99Module = NULL;
const char * FirstCVModule   = NULL;
const char * FirstEDIFModule = NULL;

Storage * DebugInfo = NULL;

word NDebugInfoModules = 0;
struct DebugInfoModule * DebugInfoModules = NULL;
struct DebugInfoModule * LastDebugInfoModule = NULL;

char *UNDEFINED_MODULE_NAME = "???";

static void NewDebugInfoModule (const char *modulename, Segment * first_seg, Segment * last_seg) {
    if (first_seg->address == (last_seg->address + last_seg->getLen())) {
        return;
    }

    VerboseMessage(INFO_DEBUGINFO, "DEBUGInfo: Module %s\n", modulename);
    struct DebugInfoModule * mod = (struct DebugInfoModule *) xalloc(sizeof(struct DebugInfoModule));
    mod -> next = NULL;
    ASSERT (NDebugInfoModules < 0xFFFF);
    mod -> no   = (word) (++NDebugInfoModules);
    mod -> file = first_seg->file;
    mod -> name = modulename;
    mod -> first_seg = first_seg;
    mod ->  last_seg = last_seg;

    mod -> name_offs     = mod -> name_len     =
    mod -> linenums_offs = mod -> linenums_len =
    mod -> align_offs    = mod -> align_len    = 0;

    if (LastDebugInfoModule) {
        LastDebugInfoModule->next = mod;
        LastDebugInfoModule       = mod;
    } else {
        DebugInfoModules = LastDebugInfoModule = mod;
    }
}

const char * GetModuleName (Segment *s) {
    const char * name = s->file->getFilename();
    if (name)
        return name;
    else
        return UNDEFINED_MODULE_NAME;
}

static void CollectDebugInfoModules (struct collection *CodeCollection) {
    ident BEGIN_TEXT = NAMES.Str2Index("_BEGIN");
    for (;CodeCollection; CodeCollection = CodeCollection->next) {
        if (CodeCollection->name == BEGIN_TEXT)
            continue;
        /* Iterate all segments of collection and determine modules taking a part in it */
        Segment * first_seg = CodeCollection->segs;
        Segment * last_seg  = CodeCollection->segs;
        const char * mod_name = GetModuleName (first_seg);
        for (Segment * cur_seg = CodeCollection->segs; cur_seg; cur_seg = cur_seg->link) {
            if (strcmp(mod_name, GetModuleName(cur_seg))) {
                NewDebugInfoModule (mod_name, first_seg, last_seg);
                first_seg = cur_seg;
                mod_name  = GetModuleName (first_seg);
            }
            last_seg = cur_seg;
        }
        NewDebugInfoModule(mod_name, first_seg, last_seg);
    }
}


void CreateDebugInfo() {
    if (DebugFormat == DBG_FMT_EDIF) {
        VerboseMessage("Creating EDIF Debug Info\n");
        CreateEDIFDebugInfo ();
        return;
    }

    CollectDebugInfoModules (code);
    CollectDebugInfoModules (code16);

    if(DebugFormat == DBG_FMT_CV) {
        VerboseMessage("Creating CodeView Debug Info\n");
        CreateCVDbg();
    } else if (DebugFormat == DBG_FMT_HLL4) {
        VerboseMessage("Creating HLL4 Debug Info\n");
        CreateHllDbg();
    } else if (DebugFormat == DBG_FMT_NB99) {
        VerboseMessage("Creating NB99 Debug Info\n");
        CreateNB99Dbg();
    } else {
        ASSERT_FALSE();
    }

    struct DebugInfoModule * dbgMod = DebugInfoModules;
    while (dbgMod) {
        struct DebugInfoModule * temp = dbgMod;
        dbgMod = dbgMod -> next;
        xfree (temp);
    }
}


void CheckDebugInfoFormat ()
{
    const char * firstModule [MAX_DBG_FORMATS];
    const char * formatName  [MAX_DBG_FORMATS];

    int dbgFormats = 0;
    if (DebugFormat & DBG_FMT_HLL4) {
        firstModule [dbgFormats] = FirstHLL4Module;
        formatName  [dbgFormats] = "HLL4";
        dbgFormats++;
    }
    if (DebugFormat & DBG_FMT_CV) {
        firstModule [dbgFormats] = FirstCVModule;
        formatName  [dbgFormats] = "CV";
        dbgFormats++;
    }
    if (DebugFormat & DBG_FMT_NB99) {
        firstModule [dbgFormats] = FirstNB99Module;
        formatName  [dbgFormats] = "NB99";
        dbgFormats++;
    }
    if (DebugFormat & DBG_FMT_EDIF) {
        firstModule [dbgFormats] = FirstEDIFModule;
        formatName  [dbgFormats] = "EDIF";
        dbgFormats++;
    }
    if (dbgFormats > 1) {
        xDoDebug = false;
        Message (xWARNING, msgUNABLE_TO_MIX_DBG_INFO, firstModule[0], formatName[0], firstModule[1], formatName[1]);
    } else if (dbgFormats == 0) {
        ASSERT (DebugFormat == DBG_FMT_NO);
        xDoDebug = false;
        Message (xWARNING, msgNO_DEBUG_INFO);
    }
}

close_namespace

