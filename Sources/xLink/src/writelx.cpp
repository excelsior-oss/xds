#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <ctype.h>

#include "xdefs.h"

open_namespace

#include "struct.h"
#include "idents.h"
#include "messages.h"
#include "xdebug.h"
#include "debug.h"
#include "implib.h"
#include "xmem.h"
#include "xlx.h"

#include "writelx.h"
#include "writer.h"

/*----------------------------------------------------------------------------*/
/*         Default Stub Code                                                                                                      */
/*----------------------------------------------------------------------------*/

byte DefaultLXStub [DefaultLXStubSize] = {
                0x4D, 0x5A, 0x80, 0x00, 0x01, 0x00, 0x00, 0x00,
                0x04, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x00, 0x00,
                0xB8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                0x0E, 0x1F, 0xBA, 0x0E, 0x00, 0xB4, 0x09, 0xCD,
                0x21, 0xB8, 0x01, 0x4C, 0xCD, 0x21, 

                0x54, 0x68, 
                0x69, 0x73, 0x20, 0x70, 0x72, 0x6f, 0x67, 0x72, 
                0x61, 0x6d, 0x20, 0x63, 0x61, 0x6e, 0x6e, 0x6f, 
                0x74, 0x20, 0x62, 0x65, 0x20, 0x72, 0x75, 0x6e, 
                0x20, 0x69, 0x6e, 0x20, 0x44, 0x4f, 0x53, 0x20, 
                0x73, 0x65, 0x73, 0x73, 0x69, 0x6f, 0x6e, 0x0D, 
                0x0A, 0x24, 0x00, 0x90, 0x90, 0x90, 0x90, 0x90
/*
                0x74, 0x68,
                0x69, 0x73, 0x20, 0x69, 0x73, 0x20, 0x61, 0x20,
                0x57, 0x69, 0x6E, 0x64, 0x6F, 0x77, 0x73, 0x20,
                0x4E, 0x54, 0x20, 0x63, 0x68, 0x61, 0x72, 0x61,
                0x63, 0x74, 0x65, 0x72, 0x2D, 0x6D, 0x6F, 0x64,
                0x65, 0x20, 0x65, 0x78, 0x65, 0x63, 0x75, 0x74,
                0x61, 0x62, 0x6C, 0x65, 0x0D, 0x0A, 0x24, 0x00
*/
};

/*----------------------------------------------------------------------------*/
/*                Globals                                                                                                                         */
/*----------------------------------------------------------------------------*/
#define ALIGN(n,align) (((n)+(align)-1)&~((align)-1))

#define PAGE_OFFSET_SHIFT 9
#define PAGE_OFFSET_ALIGN (1<<9)

#define QUANTUM         0x100
#define RAW_QUANTUM 0x1000

static FILE     * fp;

#define DOS_HDR (*pDOS_HDR)

static struct e32_exe OS2_HDR;
#define pOS2_HDR (&OS2_HDR)

static struct o32_map * pOBJ_PAGE_TBL  = NULL;
static dword OBJ_PAGE_TBL_SIZE = QUANTUM;
static dword OBJ_PAGE_TBL_IDX  = 0;
static dword PAGE_DATA_OFFSET  = 0;

static struct o32_obj * pOBJ_TBL  = NULL;
static dword OBJ_TBL_SIZE         = QUANTUM;
static dword OBJ_TBL_IDX          = 0;
static dword NEXT_OBJ_ADDRESS     = 0;

static dword * pFIXUP_PAGE_TBL     = NULL;
static dword   FIXUP_PAGE_TBL_SIZE = QUANTUM;
static dword   FIXUP_PAGE_TBL_IDX  = 0;

static byte  * pFIXUP_RECORD_TBL      = NULL;
static dword   FIXUP_RECORD_TBL_SIZE  = RAW_QUANTUM;
static dword   FIXUP_RECORD_TBL_LEN   =  0;

static byte              * pIMPORT_PROC_TBL       = NULL;
static dword   IMPORT_PROC_TBL_SIZE   = RAW_QUANTUM;
static dword   IMPORT_PROC_TBL_LEN        = 0;

static byte              * pIMPORT_MOD_TBL                = NULL;
static dword   IMPORT_MOD_TBL_SIZE        = RAW_QUANTUM;
static dword   IMPORT_MOD_TBL_LEN         = 0;

static byte              * pRESIDENT_NAME_TBL     = NULL;
static dword   RESIDENT_NAME_TBL_SIZE = RAW_QUANTUM;
static dword   RESIDENT_NAME_TBL_LEN  = 0;

static byte              * pENTRY_TBL                     = NULL;
static dword   ENTRY_TBL_SIZE             = RAW_QUANTUM;
static dword   ENTRY_TBL_LEN              = 0;


static struct rsrc32 * pRESOURCE_TBL              = NULL;
static dword   RESOURCE_TBL_SIZE          = QUANTUM;
static dword   RESOURCE_TBL_IDX           = 0;
static byte  * pRESOURCES                 = NULL;
static dword   RESOURCES_SIZE             = RAW_QUANTUM;
static dword   RESOURCES_LEN              = 0;



struct page_src{
  struct page_src * next;
  byte            * ptr;
  dword             address;
  word              size;
  dword             number;
};

struct page_src * pPAGE_SRC = NULL;


struct object_src{
  struct object_src * next;
  byte              * ptr;
  dword               size;
  dword               address;
  dword               flags;
  dword               pagemap;
  dword               mapsize;
  dword               number;
};

struct object_src * pOBJECT_SRC = NULL;


/* Resident tables lengthes */

dword ObjectTableLen                               = 0,
                          ObjectPageTableLen                       = 0,
                          ResourceTableLen                                 = 0,
                          ResidentNameTableLen                     = 0,
                          EntryTableLen                                    = 0,
                          ModuleFormatDirectivesTableLen   = 0,
                          FixupPageTableLen                        = 0,
                          FixupRecordTableLen                      = 0,
                          ImportModuleTableLen                     = 0,
                          ImportProcTableLen                       = 0,
                          PerPageChecksumTableLen                  = 0;


/*----------------------------------------------------------------------------*/
static dword nbytes = 0;
static char output_file_name[100] = "";

static void write_file(FILE * fp, void * buf, dword size){
  nbytes += size;
  if (fwrite (buf, size, 1, fp) != 1){
        fclose (fp);
        Message(xFATAL, msgUNABLE_TO_WRITE_FILE, output_file_name);
  };
};

/*----------------------------------------------------------------------------*/

static void align_file(FILE * fp, dword alignment, dword init_offset){
  byte * filler;
  dword len;

  len = ALIGN(nbytes - init_offset, alignment) - (nbytes - init_offset);
  if (len){
        filler = (byte *) xalloc (len);
        memset (filler, 0, len);
        write_file(fp, filler, len);
        xfree(filler);
  };
};


/*----------------------------------------------------------------------------*/
#define resize(table,add_len) resize_f(&p##table,&table##_SIZE,table##_LEN,add_len)
void resize_f(byte ** ptr, dword * size, dword len, dword add_len){
  dword new_size;

  new_size = *size;
  while(len + add_len > new_size) new_size += RAW_QUANTUM;
  if(new_size != *size){
    *ptr = (byte *) xrealloc(*ptr, *size, new_size);    
    *size = new_size;                                           
  };
};


/*----------------------------------------------------------------------------*\
**                                                                                                                                                        **
**       Add page to object page table ("pOBJ_PAGE_TBL"),                         **
**       set page attributes and return logical page number                                       **
**                                                                                                                                                        **
\*----------------------------------------------------------------------------*/
static dword addPage(byte  * src_ptr,
                     dword   address,
                     dword   size,
                     word    flags)
{
  struct page_src * p;

  if (OBJ_PAGE_TBL_IDX >= OBJ_PAGE_TBL_SIZE){
        pOBJ_PAGE_TBL = (struct o32_map *) xrealloc(pOBJ_PAGE_TBL,
                                 OBJ_PAGE_TBL_SIZE * sizeof(struct o32_map),
                                 (OBJ_PAGE_TBL_SIZE + QUANTUM) * sizeof(struct o32_map));
        OBJ_PAGE_TBL_SIZE += QUANTUM;
  };
  if (flags == VALID){
        PUTPAGEIDX(pOBJ_PAGE_TBL[OBJ_PAGE_TBL_IDX], PAGE_DATA_OFFSET);
        PAGE_DATA_OFFSET += ALIGN(size, PAGE_OFFSET_ALIGN) >> PAGE_OFFSET_SHIFT;
        if (pPAGE_SRC){
          p = pPAGE_SRC;
          while(p -> next) p = p -> next;
          p -> next = (struct page_src *) xalloc(sizeof(struct page_src));
          p = p -> next;
        }else{
          p = pPAGE_SRC = (struct page_src *) xalloc(sizeof(struct page_src));
        };
        p -> ptr         = src_ptr;
        p -> address = address;
        p -> size        = size;
        p -> next        = NULL;
        p -> number  = OBJ_PAGE_TBL_IDX + 1;
  }else{
        PUTPAGEIDX(pOBJ_PAGE_TBL[OBJ_PAGE_TBL_IDX], 0);
  };
  PUTPAGESIZ(pOBJ_PAGE_TBL[OBJ_PAGE_TBL_IDX], size);
  PAGEFLAGS(pOBJ_PAGE_TBL[OBJ_PAGE_TBL_IDX]) = flags;

  ObjectPageTableLen += sizeof(struct o32_map);
  return ++OBJ_PAGE_TBL_IDX;
};

/*----------------------------------------------------------------------------*\
**                                                                                                                                                        **
**       Add object to object table ("pOBJ_TBL"),                                 **
**       set object attributes and return logical object number                                   **
**                                                                                                                                                        **
\*----------------------------------------------------------------------------*/

static dword addObject(byte * src_ptr,
                                                           dword size,
                                                           dword address,
                                                           dword flags,
                                                           dword pagemap,
                                                           dword mapsize){

  struct object_src * p;

  if (OBJ_TBL_IDX >= OBJ_TBL_SIZE){
        pOBJ_TBL = (struct o32_obj *) xrealloc(pOBJ_TBL,
                                                OBJ_TBL_SIZE * sizeof(struct o32_obj),
                                                (OBJ_TBL_SIZE + QUANTUM) * sizeof(struct o32_obj));
        OBJ_TBL_SIZE += QUANTUM;
  };
  O32_SIZE(pOBJ_TBL[OBJ_TBL_IDX])        = size;
  O32_BASE(pOBJ_TBL[OBJ_TBL_IDX])        = address;
  O32_FLAGS(pOBJ_TBL[OBJ_TBL_IDX])       = flags;
  O32_PAGEMAP(pOBJ_TBL[OBJ_TBL_IDX]) = pagemap;
  O32_MAPSIZE(pOBJ_TBL[OBJ_TBL_IDX]) = mapsize;
  ASSERT(NEXT_OBJ_ADDRESS == address);
  NEXT_OBJ_ADDRESS = ALIGN(address + size, xObjectOffset);

  if (pOBJECT_SRC){
        p = pOBJECT_SRC;
        while(p -> next) p = p -> next;
        p -> next = (struct object_src *) xalloc(sizeof(struct object_src));
        p = p -> next;
  }else{
    p = pOBJECT_SRC = (struct object_src *) xalloc(sizeof(struct object_src));
  };
  p -> ptr         = src_ptr;
  p -> address = address;
  p -> size    = size;
  p -> pagemap = pagemap;
  p -> mapsize = mapsize;
  p -> flags   = flags;
  p -> number  = OBJ_TBL_IDX + 1;
  p -> next    = NULL;


  ObjectTableLen += sizeof(struct o32_obj);
  return ++OBJ_TBL_IDX;
};


/*----------------------------------------------------------------------------*/

static void addResidentName(const char * name, word ordinal){
  byte len;
  int j;

  len = strlen(name);
  resize(RESIDENT_NAME_TBL, len + 3);

  /* Set name length */
  pRESIDENT_NAME_TBL[RESIDENT_NAME_TBL_LEN++] = len;

  /* Set name */
  for(j = 0; j < len ; j++, RESIDENT_NAME_TBL_LEN++){
        pRESIDENT_NAME_TBL[RESIDENT_NAME_TBL_LEN] = name[j];
  };

  /* Set ordinal */
  *(word *)(pRESIDENT_NAME_TBL + RESIDENT_NAME_TBL_LEN) = ordinal;
  RESIDENT_NAME_TBL_LEN += 2;
};

static dword addBundleHeader(byte type, word object){
  dword cnt_offset;

  resize(ENTRY_TBL, 1 + 1 + (object ? 2 : 0));

  /* Store cnt address */
  cnt_offset = ENTRY_TBL_LEN;
  pENTRY_TBL[cnt_offset] = 0; ENTRY_TBL_LEN++;

  /* Set bundle type */
  pENTRY_TBL[ENTRY_TBL_LEN++] = type;

  /* Set object number */
  if(object){
        *(word *)(pENTRY_TBL + ENTRY_TBL_LEN) = object;
        ENTRY_TBL_LEN += 2;
  };

  return cnt_offset;
};

static void addUnusedEntriesBundle(dword * cnt_offset_ptr,
                                   word    last_ord,
                                   word    curr_ord,
                                   word /* curr_object */)
{
  word   entries_num, bundles_num;
  dword  cnt_offset;

  if(last_ord + 1 < curr_ord){
        entries_num = curr_ord - last_ord - 1;
        bundles_num  = entries_num / 255;
        entries_num = entries_num % 255;
        bundles_num  += entries_num ? 1 : 0;
        for(; bundles_num > 0; bundles_num--){
          cnt_offset = addBundleHeader(0, 0);
          pENTRY_TBL[cnt_offset] = 255;
        };
        if(entries_num) pENTRY_TBL[cnt_offset] = (byte) entries_num;
        *cnt_offset_ptr = 0;
  };
};

static void addEntry(dword * cnt_offset_ptr,
                      word   last_object,
                      word   curr_object,
                      byte   flags,
                     dword   vadr)
{
  if((curr_object != last_object) || (pENTRY_TBL[*cnt_offset_ptr] == 255)){
        *cnt_offset_ptr = addBundleHeader(3, curr_object);
  };
  pENTRY_TBL[*cnt_offset_ptr]++;
  resize(ENTRY_TBL, 5);
  pENTRY_TBL[ENTRY_TBL_LEN++] = flags;
  *(dword *)(pENTRY_TBL + ENTRY_TBL_LEN) = vadr - O32_BASE(pOBJ_TBL[curr_object-1]);
  ENTRY_TBL_LEN += 4;
};

dword LX_getOffset(dword vadr, word object){
  return vadr - O32_BASE(pOBJ_TBL[object-1]);
};

word LX_getObjectNumber(dword vadr){
  dword obj_num, i;
  struct object_src * object_src;

  for(obj_num = 0; obj_num < OBJ_TBL_IDX; obj_num++){
        if(O32_BASE(pOBJ_TBL[obj_num]) <= vadr &&
       (O32_BASE(pOBJ_TBL[obj_num]) + O32_SIZE(pOBJ_TBL[obj_num])) >= vadr)
        {
      for(i = 0, object_src = pOBJECT_SRC; i < OBJ_TBL_IDX; i++, object_src = object_src->next){
                if(object_src->number == obj_num + 1){
                  return obj_num + 1;
                };
          };
        };
  };
  /* There is no such object */
  ASSERT_FALSE();
  return 0;
};


static void CreateResidentNameAndEntryTables(void){
  Export * s;
  word    last_ord = 0, l, curr_object, last_object = 0;
  dword   vadr;
  dword   cnt_offset = 0;
  char    name[256], * p, * p1;

  /* Set module name */
  strcpy(name, xOutputFileName);
  p = strchr(name, '.');
  if(p) *p = 0;
  p = strrchr (name, '\\');
  if (!p) p = strrchr (name, '/');
  if (!p)
    p = name;
  else
    p ++;
  StrippedOutputFileName = p;
  for(p1 = p; *p1; p1++) *p1 = toupper(*p1);
  addResidentName(p, 0);

  /* Process export list */
  if ((Exports != NULL) && (MaxOrdinal != 0)) {

      /* Set exported item names */
      for(l = 0; l < NumberOfExports; l++)
                addResidentName(NAMES.Index2Str (ExportsTable[l] -> extname), ExportsTable[l] -> ordinal);
      /* Set exported item entries */
      l = 0;
      while(l < NumberOfExports) {
                s = ExportsTable[l];
                vadr = s -> seg ? s -> seg -> address + s -> offset :
                       ((nameInfo *) (NAMES.getInfo(s -> intname))) -> offset;
                curr_object = LX_getObjectNumber(vadr);
                addUnusedEntriesBundle(&cnt_offset, last_ord, s -> ordinal, curr_object);
                if(!cnt_offset){
                  cnt_offset = addBundleHeader(3, curr_object);
                  last_object = curr_object;
                };
                addEntry(&cnt_offset, last_object, curr_object, 3, vadr);
                last_object = curr_object;
                last_ord = s -> ordinal;
                l++;
      }
  }

  /* Close Resident Name Table */
  *(pRESIDENT_NAME_TBL + RESIDENT_NAME_TBL_LEN++) = 0;

  /* Close Entry Table */
  *(pENTRY_TBL + ENTRY_TBL_LEN++) = 0;


  /* Set Resident Name Table Length */
  ResidentNameTableLen = RESIDENT_NAME_TBL_LEN;

  /* Set Entry Table Length */
  EntryTableLen = ENTRY_TBL_LEN;
}

/*----------------------------------------------------------------------------*/

const char* getModuleNameNoExt(ident dllname) {
    const char* name = NAMES.Index2Str (dllname);
    const char* extpos = strchr(name, '.');
    size_t len = extpos ? extpos - name : strlen(name);
    ident id = NAMES.Str2Index (name, len);
    return NAMES.Index2Str(id);
}


static void CreateImportTables(void){
  int                   i, j, k;
  struct dll  * dll;
  byte                  len;

  /* Create import module name table */
  for(i = 0, dll = dlls; i < Ndlls; i++, dll++){
        const char* name = getModuleNameNoExt (dll -> name);
        len = strlen(name);
        resize(IMPORT_MOD_TBL, len + 1);
        pIMPORT_MOD_TBL[IMPORT_MOD_TBL_LEN++] = len;
        for(j = 0; j < len ; j++){
          pIMPORT_MOD_TBL[IMPORT_MOD_TBL_LEN++] = name[j];
        };
  };
  E32_IMPMODCNT(OS2_HDR) = Ndlls;
  ImportModuleTableLen = IMPORT_MOD_TBL_LEN;

    /* Create import procedure name table */
    for(i = 0; i < NumberOfImportedNames; i++) {
        importNameInfo * n = (importNameInfo *) (NAMES.getInfo (ImportedNames[i]));
        if (n && ((n -> kind & (K_USED | K_MASK | K_BY_ORDINAL)) == K_USED + K_IMPORT))
        {
            ident impname = n -> getName ();
            const char* name = NAMES.Index2Str (impname);
            len  = NAMES.Index2StrLen (impname);
            resize (IMPORT_PROC_TBL, len + 1);
            pIMPORT_PROC_TBL [IMPORT_PROC_TBL_LEN++] = len;
            for (k = 0; k < len ; k++) {
                pIMPORT_PROC_TBL[IMPORT_PROC_TBL_LEN++] = name[k];
            }
        }
    }
    ImportProcTableLen = IMPORT_PROC_TBL_LEN;
}

static Bool compare_names(byte * name_in_tabel, const char * zero_term_name){
  if(*name_in_tabel == strlen(zero_term_name)){
        for(int i = 0; i < *name_in_tabel; i++){
          if(zero_term_name[i] != name_in_tabel[i+1]) return false;
        };
        return true;
  };
  return false;
};

static word getOrdIdxByModName(ident modname){
  const char* name = getModuleNameNoExt (modname);
  word   ord;
  byte * ptr;

  for(ord = 0, ptr = pIMPORT_MOD_TBL; ord < E32_IMPMODCNT(OS2_HDR); ord++, ptr += *ptr + 1){
        if(compare_names(ptr, name)) return ord + 1;
  };
  ASSERT_FALSE();
  return 0;
};

static dword getNameOffsetByProcName(const char * name){
  dword  offset;

  for(offset = 0; offset < IMPORT_PROC_TBL_LEN; offset += *(pIMPORT_PROC_TBL + offset) + 1){
        if(compare_names(pIMPORT_PROC_TBL + offset, name)) return offset;
  };
  ASSERT_FALSE();
  return 0;
};


/*----------------------------------------------------------------------------*/
static void addFixupRecord(struct r32_rlc * fixup, dword len){
  resize(FIXUP_RECORD_TBL, len);
  memcpy(pFIXUP_RECORD_TBL + FIXUP_RECORD_TBL_LEN, fixup, len);
  FIXUP_RECORD_TBL_LEN += len;
};

#define is_inside(p,l,a,s)                   \
                (p <= (a + s - 1)) &&        \
                ((p + l) > (a + s - 1)) ||   \
                (p <= a) &&                  \
                ((p + l) > a)

static void CreateFixupTables(void){

  struct r32_rlc       fixup;
  struct impVarFixup * ivf;
/*
  dword * fix_adr;
  dword   nfix;
*/
  struct page_src    *   page_src;
  struct object_src  *   object_src;
  dword                  fixup_record_offset;
  dword                  page_number = 1;
  int                    int_fixup_num = 0;
  dword                  obj_num, i;
  dword                  fixup_target_address, fix_trg_addr;
  struct fixupFAR16_xx * far_fixup;
  dword                  additive_value;
  dword                  fixed_up_data_size;
  Bool                   prev_int_fixup = false;


  if(NFixups || ImpVarFixups){
        /* Iterate valid pages and form fixup tables */
        for(page_src = pPAGE_SRC; page_src; page_src = page_src -> next, page_number++){

          ASSERT(page_number == page_src -> number);

          fixup_record_offset = FIXUP_RECORD_TBL_LEN;

          /* Iterate import fixups to look for current page fixups */
          for(ivf = ImpVarFixups; ivf; ivf = ivf -> next){
                fixed_up_data_size = 4; /* be careful in the future:
                                                                   now only such fixups are known, that
                                                                   have such fixed up data size */
                if(is_inside(page_src -> address, page_src -> size, ivf -> vadr, fixed_up_data_size)){
                  /* This is current page fixup */
/*
                  (* Exclude this fixup from full fixup list *)

                        for(fix_adr = Fix, nfix = NFixups; nfix; nfix--, fix_adr++){
                          if(*fix_adr == ivf -> vadr){
                                *fix_adr = 0;
                                break;
                          };
                        };
*/

                  /* Process fixup */
                  /* Other fixup types is unknown for me */
                  ASSERT((ivf -> kind == FIXUP_SELFRELOFFS32) ||
                         (ivf -> kind == FIXUP_ADDRESS32) ||
                         (ivf -> kind == FIXUP_FAR16_16));

                  /* Known fixup types are 32-bit offset and 32-bit self relative offset */
                  switch(ivf -> kind){
                        case FIXUP_SELFRELOFFS32:
                          NR_STYPE(fixup) = NRSOFF32;
                          break;
                        case FIXUP_ADDRESS32:
                          NR_STYPE(fixup) = NROFF32;
                          break;
                        case FIXUP_FAR16_16:
                        NR_STYPE(fixup) = NRSPTR + NRALIAS;
                          break;
                  };

                  /* Set fixup source offset */
                  R32_SOFF(fixup) = ivf -> vadr - page_src -> address;
                  additive_value = *(dword *)(page_src -> ptr + R32_SOFF(fixup));

                  /* Functions or variables may be imported by name or by ordinal */
                  NR_FLAGS(fixup) = ivf -> name -> kind & K_BY_ORDINAL ? NRRORD : NRRNAM;
                  NR_FLAGS(fixup) |= NR32BITOFF + NR16OBJMOD + (additive_value ? NRADD + NR32BITADD : 0);


                  /* Set ordinal index into the Import Module Name Table */
                  R32_MODORD(fixup) = getOrdIdxByModName(ivf -> name -> getModuleName());

                  /* Set ordinal number or offset into Import Procedure Name Table */
                  if(ivf -> name -> byOrdinal ()){
                        R32_PROCORD(fixup) = ivf -> name -> getOrdinal ();
                  }else{
                        R32_PROCOFF32(fixup) = getNameOffsetByProcName(NAMES.Index2Str(ivf -> name -> getName()));
                  };

                  addFixupRecord(&fixup, RNAMSIZE32);

                  /* Set additive value */
                  if (additive_value) addFixupRecord((struct r32_rlc *) &additive_value, 4);
                };
          };

          /* Iterate selector fixups to look for current page fixups */
          for(far_fixup = FAR16_xxFixups; far_fixup; far_fixup = far_fixup -> next){
                switch(far_fixup -> kind){
                  case FIXUP_FAR16_32:
                        fixed_up_data_size = 6;
                        break;
                  case FIXUP_FAR16_16:
                        fixed_up_data_size = 4;
                        break;
                };
                if(is_inside(page_src -> address, page_src -> size, far_fixup -> source, fixed_up_data_size)){
                  /* This is current page fixup */

                  /* Process fixup */

                  /* Set fixup type and source offset */
                  switch(far_fixup -> kind){
                        case FIXUP_FAR16_32:
                          NR_STYPE(fixup) = NRPTR48;
                          R32_SOFF(fixup) = far_fixup -> source - page_src -> address;
                          break;
                        case FIXUP_FAR16_16:
                          NR_STYPE(fixup) = NRSSEG + NRALIAS;
                          R32_SOFF(fixup) = far_fixup -> source - page_src -> address + 2;
                          break;
                  };

                  /* Fixup is internal */
                  NR_FLAGS(fixup) = NRRINT + NR32BITOFF + NR16OBJMOD;

                  /* Set object ordinal index and target offset */
          fixup_target_address = far_fixup -> target;
                  for(obj_num = 0; obj_num < OBJ_TBL_IDX; obj_num++){
            if(O32_BASE(pOBJ_TBL[obj_num]) <= fixup_target_address &&
               (O32_BASE(pOBJ_TBL[obj_num]) + O32_SIZE(pOBJ_TBL[obj_num]) - sizeof(dword)) >= fixup_target_address)
                        {
                          R32_OBJNO(fixup) = obj_num + 1;
                          for(i = 0, object_src = pOBJECT_SRC; i < OBJ_TBL_IDX; i++, object_src = object_src ->next){
                                if(object_src -> number == obj_num + 1){
                                  R32_OBJNO(fixup) = obj_num + 1;
                                  switch(far_fixup -> kind) {
                                      case FIXUP_FAR16_32:
                                          R32_OFFSET32(fixup) = fixup_target_address - O32_BASE(pOBJ_TBL[obj_num]);
                                          break;
                                      case FIXUP_FAR16_16:
                                          break;
                                  }
                                  break;
                                };
                          };
                          ASSERT(i < OBJ_TBL_IDX);
                          break;
                        };
                  };
                  ASSERT(obj_num < OBJ_TBL_IDX);

                  switch(far_fixup -> kind){
                        case FIXUP_FAR16_32:
                          addFixupRecord(&fixup, 10);
                          break;
                        case FIXUP_FAR16_16:
                          addFixupRecord(&fixup, 6);
                          break;
                  };
                };
          };


          /* Iterate internal fixups to look for current page fixups */
      for(; int_fixup_num < NFixups; int_fixup_num++){
        if(prev_int_fixup) int_fixup_num--;
                fixed_up_data_size = 4;
        if(is_inside(page_src -> address, page_src -> size, Fix[int_fixup_num], fixed_up_data_size)){
                  /* This is current page fixup */
          prev_int_fixup = (Fix[int_fixup_num] + fixed_up_data_size - page_src -> address) > page_src -> size;

                  /* Process fixup */

                  /* Fixup type is 32-bit offset */
                  NR_STYPE(fixup) = NROFF32;

                  /* Fixup is internal */
                  NR_FLAGS(fixup) = NRRINT + NR32BITOFF + NR16OBJMOD;

                  /* Set fixup source offset */
                  R32_SOFF(fixup) = Fix[int_fixup_num] - page_src -> address;

                  /* Set object ordinal index and target offset */
          fix_trg_addr = fixup_target_address = *(dword *)
            (page_src -> ptr + (Fix[int_fixup_num] - page_src -> address));
          for(i = 0; i < NFix; i++){
            if(fixup_target_address == FixTrg[i]){
              fixup_target_address = FixRTrg[i];
              break;
            };
          };
                  for(obj_num = 0; obj_num < OBJ_TBL_IDX; obj_num++){
                        if(O32_BASE(pOBJ_TBL[obj_num]) <= fixup_target_address &&
                           (O32_BASE(pOBJ_TBL[obj_num]) + O32_SIZE(pOBJ_TBL[obj_num]) - sizeof(dword)) >= fixup_target_address)
                        {
                          R32_OBJNO(fixup) = obj_num + 1;
                          for(i = 0, object_src = pOBJECT_SRC; i < OBJ_TBL_IDX; i++, object_src = object_src ->next){
                                if(object_src -> number == obj_num + 1){
                  R32_OFFSET32(fixup) = fix_trg_addr - O32_BASE(pOBJ_TBL[obj_num]);
                                  break;
                                };
                          };
                          ASSERT(i < OBJ_TBL_IDX);
                          break;
                        };
                  };
          ASSERT(obj_num < OBJ_TBL_IDX);

                  addFixupRecord(&fixup, RINTSIZE32);
          if(prev_int_fixup){
            int_fixup_num++;
            break;
          };
        }else{
          break;
        };
      };

          /* Set first fixup record offset to fixup page table */
          if(FIXUP_PAGE_TBL_IDX >= FIXUP_PAGE_TBL_SIZE){
                pFIXUP_PAGE_TBL = (dword *) xrealloc(pFIXUP_PAGE_TBL,
                                                                   FIXUP_PAGE_TBL_SIZE * sizeof(dword),
                                                                   (FIXUP_PAGE_TBL_SIZE + QUANTUM) * sizeof(dword));
                FIXUP_PAGE_TBL_SIZE += QUANTUM;
          };
          pFIXUP_PAGE_TBL[FIXUP_PAGE_TBL_IDX++] = fixup_record_offset;
        };

        /* Close Fixup Page Table */
        if(FIXUP_PAGE_TBL_IDX >= FIXUP_PAGE_TBL_SIZE){
          pFIXUP_PAGE_TBL = (dword *) xrealloc(pFIXUP_PAGE_TBL,
                                                                 FIXUP_PAGE_TBL_SIZE * sizeof(dword),
                                                                 (FIXUP_PAGE_TBL_SIZE + QUANTUM) * sizeof(dword));
          FIXUP_PAGE_TBL_SIZE += QUANTUM;
        };
        pFIXUP_PAGE_TBL[FIXUP_PAGE_TBL_IDX++] = FIXUP_RECORD_TBL_LEN;

        /* Set lengths of fixup tables  */
        FixupPageTableLen       = FIXUP_PAGE_TBL_IDX * sizeof(dword);
        FixupRecordTableLen = FIXUP_RECORD_TBL_LEN;
  }else{
        /* No fixups */
        pFIXUP_PAGE_TBL = (dword *) xrealloc(pFIXUP_PAGE_TBL,
                                                           FIXUP_PAGE_TBL_SIZE * sizeof(dword),
                                                           (OBJ_PAGE_TBL_IDX + 1) * sizeof(dword));
        FIXUP_PAGE_TBL_SIZE = FIXUP_PAGE_TBL_IDX = OBJ_PAGE_TBL_IDX + 1;
        memset(pFIXUP_PAGE_TBL, 0, FIXUP_PAGE_TBL_IDX * sizeof(dword));
        FixupPageTableLen = FIXUP_PAGE_TBL_IDX * sizeof(dword);
  };
};

/*----------------------------------------------------------------------------*/

static void addResource (byte * src_ptr, dword size) {
    resize(RESOURCES, size);
    memcpy(pRESOURCES + RESOURCES_LEN, src_ptr, size);
    RESOURCES_LEN += size;
}

static void addResourceRecord(word  type,
                              word  name,
                              dword size,
                              word  object,
                              dword offset)
{
    if (RESOURCE_TBL_IDX >= RESOURCE_TBL_SIZE) {
        pRESOURCE_TBL = (struct rsrc32 *) xrealloc(pRESOURCE_TBL, RESOURCE_TBL_SIZE * sizeof(struct rsrc32),
                                                      (RESOURCE_TBL_SIZE + QUANTUM) * sizeof(struct rsrc32));
        RESOURCE_TBL_SIZE += QUANTUM;
    }
    pRESOURCE_TBL[RESOURCE_TBL_IDX].type   = type;
    pRESOURCE_TBL[RESOURCE_TBL_IDX].name   = name;
    pRESOURCE_TBL[RESOURCE_TBL_IDX].cb     = size;
    pRESOURCE_TBL[RESOURCE_TBL_IDX].obj    = object;
    pRESOURCE_TBL[RESOURCE_TBL_IDX].offset = offset;

    RESOURCE_TBL_IDX++;
}


#define RESOURCE_TYPE_TO_EXCLUDE      22

static void CreateResources (void)
{
    ResourceDirectory * res_dir;
    Resource          * res;

    dword            res_len = 0;
    word     object = OBJ_TBL_IDX + 1;
    dword    offset = 0, base;
    dword    page_number = 0;
    dword    pagemap;

    if (numberOfResourceDirectories == 0) return;

    base = NEXT_OBJ_ADDRESS;
    for(res_dir = resourceDirectoryList; res_dir; res_dir = res_dir -> next) {
        if (res_dir -> typeID != RESOURCE_TYPE_TO_EXCLUDE)
            for(res = res_dir -> resList; res; res = res -> next) {
                addResourceRecord(res_dir -> typeID,
                                  res -> resourceID,
                                  res -> datasize,
                                  object,
                                  offset);
                addResource(res -> data, res -> datasize);
                offset = offset + res -> datasize;
            }
    }
    res_len = offset;
    pagemap = OBJ_PAGE_TBL_IDX + 1;
    while (res_len > 0) {
        addPage(pRESOURCES + page_number * OBJPAGELEN,
                base + page_number * OBJPAGELEN,
                (res_len > OBJPAGELEN) ? OBJPAGELEN : res_len,
                VALID);
        page_number++;
        if(res_len <= OBJPAGELEN)
            break;
        res_len -= OBJPAGELEN;
    }
    ASSERT( object == addObject(pRESOURCES, offset, base,
                                OBJREAD + NSSHARED + OBJRSRC + OBJBIGDEF + NSDISCARD,
                                pagemap, page_number));
    ResourceTableLen = RESOURCE_TBL_IDX * sizeof(struct rsrc32);
}

/*----------------------------------------------------------------------------*/
static void CreateDataObject(void){
  dword data_len;
  dword page_number = 0;
  dword pagemap;

  data_len = DataLen;
  pagemap = OBJ_PAGE_TBL_IDX + 1;
  while(data_len > 0){
        addPage(((byte*)DataPtr) + page_number * OBJPAGELEN,
                        DataStart + page_number * OBJPAGELEN,
                        (data_len > OBJPAGELEN) ? OBJPAGELEN : data_len,
                        VALID);
        page_number++;
        if(data_len <= OBJPAGELEN)
                break;
        data_len -= OBJPAGELEN;
  };

  addObject(DataPtr, DataLen, DataStart, OBJREAD + OBJWRITE + OBJBIGDEF,
                                                                 pagemap, page_number);
};


/*----------------------------------------------------------------------------*/
static void CreateBSSObject(void){

  if (BSSLen)
        addObject(NULL, BSSLen, BSSStart, OBJREAD + OBJWRITE + OBJBIGDEF,
                          OBJ_PAGE_TBL_IDX + 1, 0);
};

/*----------------------------------------------------------------------------*/
static void CreateStackObject(void){

  E32_STACKOBJ(OS2_HDR) = addObject(NULL, xStackSize, NEXT_OBJ_ADDRESS,
                                                                        OBJREAD + OBJWRITE + OBJBIGDEF,
                                                                        OBJ_PAGE_TBL_IDX + 1, 0);
  E32_ESP(OS2_HDR) = xStackSize;
  E32_STACKSIZE(OS2_HDR) = xStackSize;
};

/*----------------------------------------------------------------------------*/
static void CreateCode16Object(void){
  dword code_len;
  dword page_number = 0;
  dword pagemap;

  code_len = Code16Len;
  pagemap = OBJ_PAGE_TBL_IDX + 1;
  while(code_len > 0){
        addPage(((byte*)Code16Ptr) + page_number * OBJPAGELEN,
                Code16Start + page_number * OBJPAGELEN,
                (code_len > OBJPAGELEN) ? OBJPAGELEN : code_len,
                VALID);
        page_number++;
        if(code_len <= OBJPAGELEN)
                break;
        code_len -= OBJPAGELEN;
  }

  addObject(Code16Ptr, Code16Len, Code16Start,
                        OBJREAD + NSEXRD + OBJALIAS16,
                        pagemap, page_number);
};


/*----------------------------------------------------------------------------*/
static void CreateCodeObject(void){
  dword code_len;
  dword page_number = 0;
  dword pagemap, start, code_object_number;

  code_len = CodeLen;
  pagemap = OBJ_PAGE_TBL_IDX + 1;
  while(code_len > 0){
        addPage(((byte*)CodePtr) + page_number * OBJPAGELEN,
                CodeStart + page_number * OBJPAGELEN,
                (code_len > OBJPAGELEN) ? OBJPAGELEN : code_len,
                VALID);
        page_number++;
        if(code_len <= OBJPAGELEN)
                break;
        code_len -= OBJPAGELEN;
  };

  code_object_number = addObject(CodePtr, CodeLen, CodeStart,
                                 OBJREAD + NSEXRD + OBJBIGDEF,
                                 pagemap, page_number);


    /* Calculate entry point object and offset */

    if (xWasEntryPoint) {
        ASSERT (EntryPoint != NULL);

        ident i;
        switch (EntryPoint->k_target) {
            case TK_SEG:
                start = ((Segment *) EntryPoint->target) -> address;
                break;
            case TK_ID:
                i = (ident) (EntryPoint->target);
                start = ((nameInfo *) (NAMES.getInfo (i))) -> offset;
                break;
            default:
                Message (xFATAL, msgINVALID_ENTRY_POINT_TARGET);
        }
        start += EntryPoint->fx_offset;

        E32_EIP(OS2_HDR) = start - CodeStart;
        E32_STARTOBJ(OS2_HDR) = code_object_number;
    }
}

/*----------------------------------------------------------------------------*/

void WriteLX (char * name, int debug, int dll){
  dword currentOffset = 0;
  struct page_src * page_src;

  NEXT_OBJ_ADDRESS = xImageBase + xObjectOffset;

  currentOffset = sizeof(struct e32_exe);

  /* Allocate and clean tables, clean headers */
        pOBJ_TBL = (struct o32_obj *) xalloc(OBJ_TBL_SIZE * sizeof(struct o32_obj));
        memset(pOBJ_TBL, 0, OBJ_TBL_SIZE * sizeof(struct o32_obj));

        pOBJ_PAGE_TBL = (struct o32_map *) xalloc(OBJ_PAGE_TBL_SIZE * sizeof(struct o32_map));
        memset(pOBJ_PAGE_TBL, 0, OBJ_PAGE_TBL_SIZE * sizeof(struct o32_map));

        pFIXUP_PAGE_TBL = (dword *) xalloc(FIXUP_PAGE_TBL_SIZE * sizeof(dword));
        memset(pFIXUP_PAGE_TBL, 0, FIXUP_PAGE_TBL_SIZE * sizeof(dword));

        pFIXUP_RECORD_TBL = (byte *) xalloc(FIXUP_RECORD_TBL_SIZE);
        memset(pFIXUP_RECORD_TBL, 0, FIXUP_RECORD_TBL_SIZE);

        pIMPORT_MOD_TBL = (byte *) xalloc(IMPORT_MOD_TBL_SIZE);
        memset(pIMPORT_MOD_TBL, 0, IMPORT_MOD_TBL_SIZE);

        pIMPORT_PROC_TBL = (byte *) xalloc(IMPORT_PROC_TBL_SIZE);
        memset(pIMPORT_PROC_TBL, 0, IMPORT_PROC_TBL_SIZE);

        pRESIDENT_NAME_TBL = (byte *) xalloc(RESIDENT_NAME_TBL_SIZE);
        memset(pRESIDENT_NAME_TBL, 0, RESIDENT_NAME_TBL_SIZE);

        pENTRY_TBL = (byte *) xalloc(ENTRY_TBL_SIZE);
        memset(pENTRY_TBL, 0, ENTRY_TBL_SIZE);

        pRESOURCE_TBL = (struct rsrc32 *) xalloc(RESOURCE_TBL_SIZE * sizeof(struct rsrc32));
        memset(pRESOURCE_TBL, 0, RESOURCE_TBL_SIZE * sizeof(struct rsrc32));
        pRESOURCES = (byte *) xalloc(RESOURCES_SIZE);
        memset(pRESOURCES, 0, RESOURCES_SIZE);


        memset(pOS2_HDR, 0, sizeof(struct e32_exe));


  /* Set general features */
        /* Set magic number "LX" */
        E32_MAGIC1(OS2_HDR) = E32MAGIC1;
        E32_MAGIC2(OS2_HDR) = E32MAGIC2;

        /* Set byte and word orders */
        E32_BORDER(OS2_HDR) = E32LEBO;
        E32_WORDER(OS2_HDR) = E32LEWO;

        /* Set format level */
        E32_LEVEL(OS2_HDR) = E32LEVEL;

        /* Set cpu and os type */
        E32_CPU(OS2_HDR) = E32CPU386;
        E32_OS(OS2_HDR)  = 1; /* OS/2 */

        /* Set module version */
        E32_VER(OS2_HDR) = 0;

        /* Set module flags */
        E32_MFLAGS(OS2_HDR) = (dll ? E32NOTP : 0) +
                              (dll && xWasEntryPoint ? E32LIBINIT + E32LIBTERM : 0) +
                              (xSystem == xSUBSYSTEM_GUI ? E32PMAPI : xSystem == xSUBSYSTEM_CUI ? E32PMW: E32NOPMW);

        /* Set page size and offset shift */
        E32_PAGESIZE(OS2_HDR)  = OBJPAGELEN;
        E32_PAGESHIFT(OS2_HDR) = PAGE_OFFSET_SHIFT;


  /* Create objects, pages and tables */
        /* Create objects */
        if(CodeLen)   CreateCodeObject();
        if(Code16Len) CreateCode16Object();
        if(DataLen)   CreateDataObject();
        if(BSSLen)        CreateBSSObject();
        if(!dll)          CreateStackObject();

        /* Create resources */
        CreateResources();

        /* Create import tables */
        CreateImportTables();

        /* Create fixup tables */
        CreateFixupTables();

        /* Create resident name and entry tables */
        CreateResidentNameAndEntryTables();

  /* Set atrtributes of unused features */
        /* Set module format directive table offset and number of entries */
        E32_DIRTAB(OS2_HDR) = 0;
        E32_DIRCNT(OS2_HDR) = 0;

        /* Set per-page checksum table offset */
        E32_PAGESUM(OS2_HDR) = 0;

        /* Set preload pages number */
        E32_PRELOAD(OS2_HDR) = 0;

        /* Set non-resident name table offset, number of entries and checksum */
        E32_NRESTAB(OS2_HDR)   = 0;
        E32_CBNRESTAB(OS2_HDR) = 0;
        E32_NRESSUM(OS2_HDR)   = 0;

        /* Set instance pages in preload and demand sections */
        E32_INSTPRELOAD(OS2_HDR) = 0;
        E32_INSTDEMAND(OS2_HDR)  = 0;

  /* Set atrtributes of used features */
        /* Set number of pages in module */
        E32_MPAGES(OS2_HDR) = OBJ_PAGE_TBL_IDX;

        /* Set loader section size and checksum */
        E32_LDRSIZE(OS2_HDR) = ObjectTableLen                             +
                                                   ObjectPageTableLen                     +
                                                   ResourceTableLen                       +
                                                   ResidentNameTableLen                   +
                                                   EntryTableLen                                  +
                                                   ModuleFormatDirectivesTableLen +
                                                   /*
                                                   FixupPageTableLen                      +
                                                   FixupRecordTableLen                    +
                                                   */
                                                   ImportModuleTableLen                   +
                                                   ImportProcTableLen                     +
                                                   PerPageChecksumTableLen;
        E32_LDRSUM(OS2_HDR)  = 0;

        /* Set fixup section size and checksum */
        E32_FIXUPSIZE(OS2_HDR) = FixupPageTableLen +
                                                         FixupRecordTableLen +
                                                         ImportModuleTableLen +
                                                         ImportProcTableLen;
        E32_FIXUPSUM(OS2_HDR)  = 0;

        /* Set table offsets and other table attributes */
          /* Set object table offset and number of objects */
          /* written as 1st */
          E32_OBJTAB(OS2_HDR) = currentOffset;
          E32_OBJCNT(OS2_HDR) = OBJ_TBL_IDX;
          currentOffset += ObjectTableLen;

          /* Set object page table offset */
          /* written as 2nd */
          E32_OBJMAP(OS2_HDR) = currentOffset;
          currentOffset += ObjectPageTableLen;

          /* Set resource table offset and number of entries */
          /* written as 3rd */
          if (ResourceTableLen){
                E32_RSRCTAB(OS2_HDR) = currentOffset;
                E32_RSRCCNT(OS2_HDR) = RESOURCE_TBL_IDX;
                currentOffset += ResourceTableLen;
          }else{
                E32_RSRCTAB(OS2_HDR) = 0;
                E32_RSRCCNT(OS2_HDR) = 0;
          };

          /* Set resident table offset */
          /* written as 4th */
          E32_RESTAB(OS2_HDR) = currentOffset;
          currentOffset += ResidentNameTableLen;

          /* Set resident table offset */
          /* written as 5th */
          E32_ENTTAB(OS2_HDR) = currentOffset;
          currentOffset += EntryTableLen;

          /* Set fixup page table and fixup record tabel offsets */
          /* written as 6th */
          E32_FPAGETAB(OS2_HDR) = currentOffset;
          currentOffset += FixupPageTableLen;
          /* written as 7th */
          E32_FRECTAB(OS2_HDR)  = currentOffset;
          currentOffset += FixupRecordTableLen;

          /* Set import module name table offset */
          /* written as 9th */
          E32_IMPMOD(OS2_HDR) = currentOffset;
          currentOffset += ImportModuleTableLen;

          /* Set import procedure name table offset */
          /* written as 7th */
          E32_IMPPROC(OS2_HDR) = currentOffset;
          currentOffset += ImportProcTableLen;

          /* Set data pages offset */
          E32_DATAPAGE(OS2_HDR) = ALIGN(currentOffset + StubSize, PAGE_OFFSET_ALIGN);

        /* Write pages */


        /* Create debug information and set its offset and length */
        /* written as last */
        if(debug){
          currentOffset = ALIGN(currentOffset + StubSize, PAGE_OFFSET_ALIGN);
          /* Calculate debug info offset */
          for(page_src = pPAGE_SRC; page_src; page_src = page_src ->next){
                currentOffset += ALIGN(page_src -> size, PAGE_OFFSET_ALIGN);
          };
          CreateDebugInfo();
          E32_DEBUGINFO(OS2_HDR) = currentOffset;
          E32_DEBUGLEN(OS2_HDR)  = DebugInfo->Index;
        }else{
          E32_DEBUGINFO(OS2_HDR) = 0;
          E32_DEBUGLEN(OS2_HDR)  = 0;
        };


/* А теперь записать все это на диск */
  fp = fopen (name, "wb");
  if (fp == NULL) Message(xFATAL, msgUNABLE_TO_OPEN_FILE, name);
  strcpy(output_file_name, name);

  /* Write stub */
  write_file(fp, Stub, StubSize);

  /* Write LX header */
  write_file(fp, pOS2_HDR, sizeof(struct e32_exe));

  /* Write object table - 1st */
  write_file(fp, pOBJ_TBL, ObjectTableLen);

  /* Write object page table - 2nd */
  write_file(fp, pOBJ_PAGE_TBL, ObjectPageTableLen);

  /* Write resource table - 3rd */
  if (ResourceTableLen)
        write_file(fp, pRESOURCE_TBL, ResourceTableLen);

  /* Write resident name tabel - 5th */
  if (ResidentNameTableLen)
        write_file(fp, pRESIDENT_NAME_TBL, ResidentNameTableLen);

  /* Write resident name tabel - 6th */
  if (EntryTableLen)
        write_file(fp, pENTRY_TBL, EntryTableLen);


  /* Write fixup page table - 7th */
  write_file(fp, pFIXUP_PAGE_TBL, FixupPageTableLen);

  /* Write fixup record table - 8th */
  if (FixupRecordTableLen)
        write_file(fp, pFIXUP_RECORD_TBL, FixupRecordTableLen);

  /* Write import module name table - 9th */
  if (ImportModuleTableLen)
        write_file(fp, pIMPORT_MOD_TBL, ImportModuleTableLen);

  /* Write import procedure name table - 7th */
  if (ImportProcTableLen)
        write_file(fp, pIMPORT_PROC_TBL, ImportProcTableLen);

  /* Align file on PAGE_OFFSET_ALIGN */
  align_file(fp, PAGE_OFFSET_ALIGN, 0);


  /* Write pages */
  for(page_src = pPAGE_SRC; page_src; page_src = page_src ->next){
        write_file(fp, page_src -> ptr, page_src -> size);
        align_file(fp, PAGE_OFFSET_ALIGN, 0);
  };


  /* Write debug information */
  /* written as last */
  if(debug){
        write_file(fp, DebugInfo->Ptr, DebugInfo->Index);
  };

  if (fclose (fp)) Message(xFATAL, msgUNABLE_TO_WRITE_FILE, name);
};

close_namespace

