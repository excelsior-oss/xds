
#include "xdefs.h"

open_namespace

#include "struct.h"
#include "xdebug.h"
#include "idents.h"

char ASSERT_MESSAGE_BUF [1024] = "";

/*
void DumpAllNames(void) {
        int i, index;
        struct Name * q;

        printf("----------------------------------------------------------------------------\n");
        printf(" NAMES DUMP\n");

        for (index = 0; index < NamesHeadSize; index ++)
                for (i = NNames [index], q = NamesTable [index]; i; q ++, i --) {
                        printf("index = <%d> i = <%d> name = <%s> kind = <%hd> ",index, i, NAMES.Index2Str(q->name),q->kind);
                        if(q->seg) {
                            printf("seg.name = <%s> ",NAMES.Index2Str(q->seg->name));
                            if(q->seg->file && q->seg->file->filename)
                                 printf("seg.file = <%s>\n", q->seg->file->filename);
                            else
                                 printf("seg.file=0\n");
                        } else
                            printf("seg=0\n");
                }
        printf("----------------------------------------------------------------------------\n");
}

void DumpAllExports(void) {
        int i;
        Export * e;

        printf("----------------------------------------------------------------------------\n");
        printf(" EXPORTS DUMP\n");

        printf("NExports = %d\n",NExports);
        for (i = 0, e = Exports ; (i < NExports) && e; i ++, e = e->next) {
            printf("extname = <%s> intname = <%s> offset = <%u> ordinal = <%hd>\n\t exp_def_src = <%hd> modname = <%d> name_exported = <%d>\n",
                    NAMES.Index2Str(e->extname), NAMES.Index2Str(e->intname), e->offset, e->ordinal, e->exp_def_src, e->modname, (int)(e->name_exported));
            if(e->seg) {
                    printf("\tseg.name = <%s> ",NAMES.Index2Str(e->seg->name));
                    if(e->seg->file && e->seg->file->filename)
                         printf("seg.file = <%s>\n", e->seg->file->filename);
                     else
                         printf("seg.file=0\n");
            } else
                     printf("\tseg=0\n");
        }
        printf("Actual Num Of Exports = %d\n",i);
        printf("----------------------------------------------------------------------------\n");
}

void DumpExportsTable(void) {
        int i;
        Export * e;

        printf("----------------------------------------------------------------------------\n");
        printf(" EXPORTS TABLE DUMP\n");

        for (i = 0; i < NExps; i ++) {
            e = table[i];
            printf("extname = <%s> intname = <%s> offset = <%u> ordinal = <%hd>\n\t exp_def_src = <%hd> modname = <%d>\n",
                    NAMES.Index2Str(e->extname), NAMES.Index2Str(e->intname), e->offset, e->ordinal, e->exp_def_src, e->modname);
            if(e->seg) {
                    printf("\tseg.name = <%s> ",NAMES.Index2Str(e->seg->name));
                    if(e->seg->file && e->seg->file->filename)
                         printf("seg.file = <%s>\n", e->seg->file->filename);
                     else
                         printf("seg.file=0\n");
            } else
                     printf("\tseg=0\n");
        }
        printf("----------------------------------------------------------------------------\n");
}
*/
/*
void PrintFixup (struct fixup * Fixup)
{
    printf("\t\tFixup: ");
    if(Fixup->kind & FX_SELFRELATIVE)
         printf("FX_SELFRELATIVE ");
    switch(Fixup->kind & ~FX_SELFRELATIVE) {
      case FX_LOW8:
         printf("FX_LOW8 ");
         break;
      case FX_OFFSET16:
         printf("FX_OFFSET16 ");
         break;
      case FX_SEL16:
         printf("FX_SEL16 ");
         break;
      case FX_FAR16_16:
         printf("FX_FAR16_16 ");
         break;
      case FX_HIGH8:
         printf("FX_HIGH8 ");
         break;
      case FX_OFFSET16LR:
         printf("FX_OFFSET16LR ");
         break;
      case FX_OFFSET32:
         printf("FX_OFFSET32 ");
         break;
      case FX_FAR16_32:
         printf("FX_FAR16_32 ");
         break;
      case FX_OFFSET32_LR:
         printf("FX_OFFSET32_LR ");
         break;
      case FX_TDINDEX16:
         printf("FX_TDINDEX16 ");
         break;
      case FX_SECREL32:
         printf("FX_SECREL32 ");
         break;
      case FX_OFFSET32NB:
         printf("FX_OFFSET32NB ");
         break;
    }
    switch(Fixup->k_target) {
       case TK_SEG          :
         printf("TK_SEG %s ", NAMES.Index2Str(((struct seg *)(Fixup->target))->name));
         break;
       case TK_GROUP        :
         printf("TK_GROUP %s ", NAMES.Index2Str(((struct group *)(Fixup->target))->name));
         break;
       case TK_ID           :
         printf("TK_ID %s ", NAMES.Index2Str((int)Fixup->target));
         break;
       case TK_FWD_SEG      :
         printf("TK_FWD_SEG %s ", NAMES.Index2Str(((struct seg *)(Fixup->target))->name));
         break;
    }
    printf("offset=%d fx_offset=%d\n", Fixup->offset, Fixup->fx_offset);
}

void DumpAllFixups(void)
{
   OBJFile      * f;
   struct seg   * s;
   int i;

   for (f = FileList; f; f = f->next) {
       printf("File: %s\n",f->filename);
       for(s = f->segs; s; s = s->next) {
           printf("\tSegment: %s\n",NAMES.Index2Str(s->name));
           for(i = 0; i < s->nfixups; i++) {
               PrintFixup(s->fixups + i);
           }
       }
   }
}
*/
clock_t startTimer () {
    if (xMeasureTime)
        return clock();
    else
        return 0;
}

#define SEC_PER_MIN 60

void stopTimer (char * message, clock_t startTime) {
    if (xMeasureTime) {
        clock_t clk = clock() - startTime;
        clock_t sec = clk/CLOCKS_PER_SEC;
        clock_t min = sec/SEC_PER_MIN;

        clk -= sec*CLOCKS_PER_SEC;
        sec -= min*SEC_PER_MIN;

        fprintf (stdout, message, min, sec, clk, CLOCKS_PER_SEC);
        fflush  (stdout);
    }
}
/*
void DumpImpVarFixups () 
{
    struct impVarFixup * ivf = ImpVarFixups;
    printf ("--------------------------  ImpVarFixups --------------------------\n");
    while (ivf != NULL) {
        printf ("%s %d -> %d (kind %d)\n", NAMES.Index2Str (ivf->name->name), ivf->vadr, *(ivf->address), ivf->kind);
        ivf = ivf -> next;
    }
}
*/

close_namespace

