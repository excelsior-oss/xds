#include <string.h>

#include "xdefs.h"

open_namespace

#include "struct.h"
#include "xdebug.h"
#include "idents.h"
#include "messages.h"
#include "xmem.h"
#include "debug.h"

#include "writer.h"

#ifdef DBG_NB99
#include "dbg99.h"
#else
#include "dbghll.h"
#endif

static dword           NDirEntries = 0;
static dword           DirInfoIdx  = 0;

struct subsect_desc {
  word  type;
  word  index;
  dword offset;
  dword len;
};

static struct subsect_desc * pSUB_SECT_ARR = NULL;

static void addSubSectRecord(word  type,
                             word  index,
                             dword offset,
                             dword len)
{
  pSUB_SECT_ARR[NDirEntries].type   = type;
  pSUB_SECT_ARR[NDirEntries].index  = index;
  pSUB_SECT_ARR[NDirEntries].offset = offset;
  pSUB_SECT_ARR[NDirEntries].len    = len;

  NDirEntries++;
};


static void CreateModules(void) {
  Segment  *  s;
  OBJFile     *  f;
  Segment     ** types;
  Segment     ** symbols;
  struct linnum  ** linnums;
  dword             types_len;
  dword             symbols_len;

  Bool                   used;


  /* Collect debug segments */
  for (f = FileList; f; f = f -> next) {
        types   = & (f -> types);
        symbols = & (f -> symbols);
        linnums = & (f -> linnums);
        types_len = 0; symbols_len = 0;

        used = false;
        for (s = f -> segs; s; s = s -> next) {
          if (s -> isProcessed ()){
                used = true;
          }else if (s -> name == SYMBOLS_OMF){
                * symbols = s;
                symbols_len += s->getLen();
                symbols = & (s -> link);
          }else if (s -> name == TYPES_OMF){
                types_len += s->getLen();
                * types = s;
                types = & (s -> link);
          };
        };
        if(used){
          if(!types_len)   f -> types   = NULL;
          if(!symbols_len) f -> symbols = NULL;
        }else{
          f -> types = f -> symbols = NULL;
          f -> linnums  = NULL;
        };
  };
};

static void processSymbol(Segment * seg){
    struct fixup  * fixup;
    dword           addr;
    dword           vadr, offset;
    word            object;
    int n;

    for (n = seg -> nfixups, fixup = seg -> fixups; n; n--, fixup++) {
        if (SymbolInExe(fixup, &addr)) {
            vadr = addr;
            object = getObjectNumber(vadr);
            offset = getOffset(vadr, object);
            byte* segText = seg->getText();
            *(dword *)(segText + fixup -> offset)     += offset;
            *(word  *)(segText + fixup -> offset + 4)  = object;
        }
    }
};


#ifdef DBG_NB99
void CreateNB99Dbg()
#else
void CreateHllDbg()
#endif
{

  dword subsection_begin;
  dword vadr;
  word  object;
  Segment     * s;
  struct pub     * pub;
  struct linnum  * linnum;
  struct DebugInfoModule * mod;

  DebugInfo = newStorage(65536);

  CreateModules();

  /* Set signature */
#ifdef DBG_NB99
  DebugInfo->PutS("NB99", 4);
#else
  DebugInfo->PutS("NB04", 4);
#endif

  /* Store index of Directory Information pointer */
  DirInfoIdx = DebugInfo->Index;
  DebugInfo->Index += 4;

  /* Allocate array of Subsection Descriptors */
  pSUB_SECT_ARR = (struct subsect_desc *) xalloc(NDebugInfoModules * 5 * 12);

  /* Set subsections */
  for (mod = DebugInfoModules; mod; mod = mod -> next) {
#ifndef DBG_NB99
        if (strlen(mod->name) >= 256) {
            // skip module with too long name
            continue;
        }
#endif
        /* Set code subsection(s) */
          subsection_begin = DebugInfo->Index;
          vadr   = mod->first_seg->address;

          object = getObjectNumber(vadr);
          DebugInfo->Put2(object);
          DebugInfo->Put4(getOffset(vadr, object));
          DebugInfo->Put4(mod->last_seg->address - mod->first_seg->address
                          + mod->last_seg->getLen());
          DebugInfo->Put2(0);
          DebugInfo->Put2(0);
          DebugInfo->PutB(1);
          DebugInfo->PutB(0);
          DebugInfo->PutS("HL",2);
          DebugInfo->Put2(0x0400);
#ifdef DBG_NB99
          DebugInfo->PutLongName(mod->name);
#else
          DebugInfo->PutName(mod->name);
#endif
          addSubSectRecord(0x101, mod -> no, subsection_begin, DebugInfo->Index - subsection_begin);

        /* Set public subsection */
        if(mod -> file -> publics) {
          subsection_begin = DebugInfo->Index;
          for(pub = mod -> file -> publics; pub; pub = pub -> next) {
                nameInfo * name = (nameInfo *) (NAMES.getInfo (pub -> name));
                if (name && name->seg && name -> seg -> isProcessed()
                         && (pub->seg == name->seg)
#ifdef DBG_NB99
                    )
#else
                    && (NAMES.Index2StrLen (pub -> name) < 256) )
#endif
                {
                     if (pub -> seg != name -> seg) {
                          VerboseMessage ("symbol: %s\n", NAMES.Index2Str(pub -> name));
                          VerboseMessage ("duplicate in: %s and %s\n", mod->file->getFilename(), name->seg->file->getFilename());
                     }
                     ASSERT (pub -> seg == name -> seg);
                     vadr = pub -> seg -> address + pub -> offset;
                     ASSERT (vadr == (dword) (name -> offset));
                     object = getObjectNumber(vadr);
                     DebugInfo->Put4(getOffset(vadr, object));
                     DebugInfo->Put2( object);
                     DebugInfo->Put2 (0); // type index
#ifdef DBG_NB99
                     DebugInfo->PutLongName(NAMES.Index2Str(pub -> name));
#else
                     DebugInfo->PutName(NAMES.Index2Str(pub -> name));
#endif
                };
          };
          if(DebugInfo->Index > subsection_begin) addSubSectRecord(0x102, mod -> no, subsection_begin, DebugInfo->Index - subsection_begin);
        };

        /* Set types subsection */
        if(mod -> file -> types) {
            subsection_begin = DebugInfo->Index;
            for(s = mod -> file -> types; s; s = s -> link){
                if(s->getLen() > 0) {
                    DebugInfo->PutS(s->getText(), s->getLen());
                }
            }
            addSubSectRecord(0x103, mod -> no, subsection_begin, DebugInfo->Index - subsection_begin);
        }

        /* Set symbols subsection */
        if(mod -> file -> symbols) {
            subsection_begin = DebugInfo->Index;
            for(s = mod -> file -> symbols; s; s = s -> link){
                if(s->getLen() > 0){
                    processSymbol(s);
                    DebugInfo->PutS(s->getText(), s->getLen());
                }
            }
            addSubSectRecord(0x104, mod -> no, subsection_begin, DebugInfo->Index - subsection_begin);
        }

        /* Set linnums subsection */
        if(mod -> file -> linnums){
          subsection_begin = DebugInfo->Index;
          for(linnum = mod -> file -> linnums; linnum; linnum = linnum -> next){
                if(linnum -> seg){
                  if(linnum -> seg -> isProcessed ()){
            if(*(word *)(linnum -> text) == 0){
              /* First (special) record */
              *(word  *)(linnum -> text + 6) = getObjectNumber(linnum -> seg -> address);
              *(dword *)(linnum -> text + 8) = linnum -> seg -> address;
            };
                        DebugInfo->PutS(linnum -> text, linnum -> length);
                  };
                }else{
                  DebugInfo->PutS(linnum -> text, linnum -> length);
                };
          };
          addSubSectRecord(0x10B, mod -> no, subsection_begin, DebugInfo->Index - subsection_begin);
        };
  };

  /* Set pointer to Directory Information */
  *(dword *)(DebugInfo->Ptr + DirInfoIdx) = DebugInfo->Index;

  /* Set Directory Information */
  DebugInfo->Put2( 8);
  DebugInfo->Put2( 12);
  DebugInfo->Put4(NDirEntries);

  /* Set subsection descriptors array */
  DebugInfo->PutS((byte*)pSUB_SECT_ARR, NDirEntries * 12);
  xfree (pSUB_SECT_ARR);

  /* Set signature */
#ifdef DBG_NB99
  DebugInfo->PutS("NB99", 4);
#else
  DebugInfo->PutS("NB04", 4);
#endif

  /* Set length */
  DebugInfo->Put4(DebugInfo->Index + 4);

}

close_namespace

