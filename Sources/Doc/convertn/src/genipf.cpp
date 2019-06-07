#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "basedefs.h"
#include "scanner.h"
#include "ir.h"
#include "generate.h"

static FILE *outfile = 0;
static char outname[256];

static void ChangeExt(char name[], const char ext[]) {
    char *extpos;
    extpos = strrchr(name,'.');
    if (!extpos||strchr(extpos,'\\')||strchr(extpos,'/'))
        extpos = name+strlen(name);
    strcpy(extpos,ext);
}

void OpenOutput(const char name[], const char ext []) {
    strcpy(outname,name);
    ChangeExt(outname,ext);
    if ((outfile = fopen(outname,"w")) == NULL)
        FatalError("Can not open output file");
}

void WriteOutput(const char text []) {
    fputs(text, outfile);
    fflush(outfile);
}

void CloseOutput(void) {
    fclose(outfile);
}

static int offset  = 0;
int autowrap = 1; // Temporarily set to 0 when generating headings and indexes

static void NL(void) {
    if (offset) { WriteOutput("\n"); offset = 0; }
}

static void NLPut(const char s[]) {
    if (offset) { WriteOutput("\n"); offset = 0; }
    WriteOutput(s);
    offset += strlen(s);
}

static void Put(const char s[]) {
    WriteOutput(s);
    offset += strlen(s);
}

static void Translate(char text[], int verb = 0) {  // should be optimized
    char *scan;
    for(scan = text;*scan;scan++) {
        switch (*scan) {
        case ':': Put("&colon."); continue;
        case '&': Put("&amp."); continue;
        case '.': Put("&period."); continue;
        case ' ':
            if ((offset >= 72) && autowrap && !verb) { NL(); continue; }
            else break;
        }
        fputc(*scan,outfile); offset++;
    }
}

int par = 0;
int table = 0;

static void CheckPar(void) {
    if (par) {par = 0; NLPut(":p."); }
}

static void SwitchFont(int from, int to) {
    switch(from) {
    case f_rm:   break;
    case f_bf:   if (table) Put(":ehp8."); else Put(":ehp2."); break;
    case f_sf:   break;
    case f_it:   if (table) Put(":ehp9."); else Put(":ehp1."); break;
    case f_sl:   break;
    case f_sc:   break;
    case f_tt:   if (!table) Put(":font facename=default size=0x0."); break;
    case f_math: if (!table) Put(":font facename=default size=0x0."); break;
    case f_tab:  Put(":font facename=default size=0x0."); break;
    }
    switch(to) {
    case f_rm:   break;
    case f_bf:   if (table) Put(":hp8."); else Put(":hp2."); break;
    case f_sf:   break;
    case f_it:   if (table) Put(":hp9."); else Put(":hp1."); break;
    case f_sl:   break;
    case f_sc:   break;
    case f_tt:   if (!table) Put(":font facename=Courier size=18x10."); break;
    case f_math: if (!table) Put(":font facename='Tms Rmn' size=18x10."); break;
    case f_tab:  Put(":font facename='System Monospaced' size=18x10."); break;
    }
}

void IRNode::genIPF(void) {
    IRNode *scan;
    CheckPar();
    for(scan = this->son; scan; scan = scan->next) {
        if (scan->face != curface) {
            SwitchFont(curface,scan->face);
            curface = scan->face;
        }
        scan->genIPF();
    }
}

static void GenTables(void) {
    char s[256];
    IRFloat *scan;
    if (!IRTable::lastnumber) return;
    NLPut(":h1.Tables");
    for (scan = IRFloat::first; scan; scan = scan->next) {
        if (scan->code != e_table) continue;
        autowrap = 0;
        par = 0;
        sprintf(s,":h2 id='table%d'.Table %d. ",scan->number,scan->number);
        NLPut(s);
        scan->caption.genIPF();
        NL();
        autowrap = 1;
        par = 1;
        scan->IRNode::genIPF();
    }
}

static void GenFigures(void) {
    char s[256];
    IRFloat *scan;
    if (!IRFigure::lastnumber) return;
    NLPut(":h1.Figures");
    for (scan = IRFloat::first; scan; scan = scan->next) {
        if (scan->code != e_figure) continue;
        autowrap = 0;
        par = 0;
        sprintf(s,":h2 id='figure%d'.Figure %d. ",scan->number,scan->number);
        NLPut(s);
        scan->caption.gentext(Put);
        NL();
        autowrap = 1;
        par = 1;
        scan->IRNode::genIPF();
    }
}

void IRRoot::genIPF(void) {
    OpenOutput(this->name,".ipf");
    curface = t_rm;
    cursize = t_normalsize;
    Put(":userdoc.");
    NLPut(":title.");
    title.genIPF();
    NLPut(":docprof toc=1234.");
    NLPut(":h1.Disclaimer");
    NLPut(":p.This on-line document was generated automatically from");
    NLPut("its printed version LaTeX source.");
    NLPut(":h1.Title Page");
    NL();
    par = 1;
    this->IRNode::genIPF();
    GenTables();
    GenFigures();
    NLPut(":euserdoc.");
    NL();
    CloseOutput();
}

void IRRoot::genEXT(void) {
    IRHeading* scan;
    String* pub;
    char s[256];

    OpenOutput(this->name,".ext");
    for (scan = IRHeading::getfirst(); scan; scan = scan->next) {
        for (pub = scan->publist.first; pub; pub = pub->next) {
			if(multipart)
               sprintf(s, "\\extern{%s}{%s}[%d]{%d}", name, pub->string, scan->part, scan->id);
			else
               sprintf(s, "\\extern{%s}{%s}{%d}", name, pub->string, scan->id);
            Put(s);
            NL();
        }
    }
    CloseOutput();
}

void IRPlain::genIPF(void) {
    char *t = this->text;
    if (par) {
        if (!this->verb) {
            while(isspace((unsigned char)*t)) t++;    // Skip leading spaces, if any
            if (!*t) return;
        }
        CheckPar();
    }
    Translate(t);
}

void IRCommand::genIPF(void) {
    char buf[256];
    switch (code) {
    case t_par:
        par = 1; break;
    case t_newline:
        NLPut(".br"); NL(); break;
    case t_copyright:
        Put("(c)");
    case t_hyphen:
        Put("&hyphen."); break;
    case t_endash:
        Put("&endash."); break;
    case t_emdash:
        Put("&emdash."); break;
    case t_TeX:
        Put("TeX"); break;
    case t_LaTeX:
        Put("LaTeX"); break;
    case t_times:
        Put("&dot."); break;
    case t_textbackslash:
    case t_backslash:
        Put("\\"); break;
    case t_surd:
        Put("&sqrt."); break;
    case t_pi:
        Put("&pi."); break;
    case t_leq:
        Put("<="); break;           // !!!
    default:
        Put("?");
        Warning("Symbol '%s' is unsupported",VisToken(code,buf));
        break;
    }
}

void IRItem::genIPF(void) {
    par = 0;
    if (son) {
        NLPut(":dt.");
        this->IRNode::genIPF();
        NLPut(":dd.");
    }
    else
        NLPut(":li.");
}

void IRVerbatim::genIPF(void) {
    String *scan;
    for (scan = list.first; scan; scan = scan->next)
        Translate(scan->string,1);
}

void IREnv::genIPF(void) {
    par = 0;
    switch (this->code) {
    case e_document:
        this->IRNode::genIPF();
        break;
    case e_verbatim:
        NLPut(":xmp.");
        NL();
        this->IRNode::genIPF();
        Put(":exmp.");
        NL();
        break;
    case e_itemize:
        NLPut(":ul.");
        this->IRNode::genIPF();
        NLPut(":eul.");
        NL();
        par = 1;
        break;
    case e_enumerate:
        NLPut(":ol.");
        this->IRNode::genIPF();
        NLPut(":eol.");
        NL();
        par = 1;
        break;
    case e_description:
        NLPut(":dl compact break=all tsize=3.");
        this->IRNode::genIPF();
        NLPut(":edl.");
        NL();
        par = 1;
        break;
    case e_center:
    case e_flushleft:
        NLPut(":p.");
        this->IRNode::genIPF();                     // !!!
        par = 1;
        break;
    case e_flushright:
        Warning("The \\flushright environment is unsupported if IPF");
        NLPut(":p.");
        this->IRNode::genIPF();
        par = 1;
        break;
    case e_tabbing:
    case e_tabular:
    case e_tabularw:
        NLPut(":p.:hp9.The appearance of the following table is a known");
        NLPut("problem and will be improved in the final release.:ehp9.");
    default:
        par = 1;
        this->IRNode::genIPF();
        par = 1;
        break;
    }
}

void IRTable::genIPF(void) {
    char s[256];
    // !!! CheckPar?
    sprintf(s,":link reftype=hd refid='table%d'.",this->number);
    Put(s);
    sprintf(s,"Table %d. ",this->number); Put(s);
    this->caption.genIPF();
    Put(":elink.");
}

void IRFigure::genIPF(void) {
    char s[256];
    // !!! CheckPar?
    sprintf(s,":link reftype=hd refid='figure%d'.",this->number);
    Put(s);
    sprintf(s,"Figure %d. ",this->number); Put(s);
    this->caption.genIPF();
    Put(":elink.");
}

void IRPopUp::genIPF(void) {
	Error("Pop-ups are not supported in IPF");
}


static int cellwidth;

static void AddLen(const char text[]) {
    cellwidth += strlen(text);
}

void IRTabular::genIPF(void) {
    IRNode *row,*cell;
//    int width;
    int colwidth[32], colno;
    char s[256],w[8];
    int face = curface;

    par = 0;

    for (colno = 0; colno < 32; colno++) colwidth[colno] = -1;
    for (row = this->son; row; row = row->next) {
        colno = 0;
        for (cell = row->son; cell; cell = cell->next) {
            cellwidth = 0;
            cell->gentext(AddLen);
            if (cellwidth > colwidth[colno])
                colwidth[colno] = cellwidth;
            colno++;
        }
    }

    NL();
    SwitchFont(curface,f_tab); curface = f_rm;
    strcpy(s,":table rules=none frame=rules cols='");
    for (colno = 0; colwidth[colno] >= 0; colno++) {
        sprintf(w,"%d ",colwidth[colno]);
        strcat(s,w);
    }
    strcat(s,"'.");
    NLPut(s);

    table = 1;
    for (row = this->son; row; row = row->next) {
        NLPut(":row.");
        for (cell = row->son; cell; cell = cell->next) {
            NLPut(":c.");
            cell->genIPF();
        }
    }
    table = 0;
    NLPut(":etable."); NL();
    SwitchFont(f_tab,face);
    curface = face;
    par = 1;
}

void IRHeading::genIPF(void) {
    IRHeading *link;
    char s[256];
    String *scan;
    par = 0;
    if ((this->level <= 4) && this->prev && (this->prev->level == this->level-1)) {
        NLPut(":ul compact.");
        for (link = this; link; link = link->next) {
            if (link->level < this->level) break;
            if (link->level > this->level) continue;
            sprintf(s, ":li.:link reftype=hd res=%d.",link->id);
            NLPut(s);
            link->IRNode::genIPF();
            Put(":elink.");
        }
        NLPut(":eul.");
    }
    if (this->publist.first)
        sprintf(s,":h%d res=%d global id='%s'.",
                  this->level,this->id,this->publist.first->string);
    else
        sprintf(s,":h%d res=%d.",this->level,this->id);
    NLPut(s);
    autowrap = 0;
    this->IRNode::genIPF();
    for (scan = indlist.first; scan; scan = scan->next) {
        if (scan->next->string[0]) {
            sprintf(s,":i2 global refid='%s'.",scan->string);
            NLPut(s);
            scan = scan->next;
            Translate(scan->string,1);
        }
        else {
            sprintf(s,":i1 global id='%s'.",scan->string);
            NLPut(s);
            Translate(scan->string,1);
            scan = scan->next;
        }
    }
    autowrap = 1;
    NL();
    par = 1;
}

void IRRef::genIPF(void) {
    char s[64];
    IRHeading *h;
    IRExtern *e;
    IRFloat *f;

    CheckPar();
    h = IRHeading::find(this->label);
    if (h) {
        sprintf(s, ":link reftype=hd res=%d.",h->id);
        Put(s);
        if (son) this->IRNode::genIPF(); else h->IRNode::genIPF();
        Put(":elink.");
        return;
    }
    e = IRExtern::find(this->label);
    if (e) {
        sprintf(s, ":link reftype=hd refid='%s' database='%s'.",e->label, e->file);
        Put(s);
        if (son) this->IRNode::genIPF(); else Put("!!!");
        Put(":elink.");
        return;
    }
    f = IRFloat::find(this->label);
    if (f) {
        sprintf(s, ":link reftype=hd refid='table%d'.",f->number);
        Put(s);
        if (son) this->IRNode::genIPF();
        else {
            sprintf(s, "%d. ",f->number);
            Put(s);
            f->caption.genIPF();
        }
        Put(":elink.");
        return;
    }
    Warning("Unresolved reference - '%s'",this->label);
    Put("???");
}

void IRFootnote::genIPF() {
    Put("(");
    this->IRNode::genIPF();
    Put(")");
}

void IRHRef::genIPF() {
    int face = curface;
    SwitchFont(curface,f_tt); curface = f_tt;
    this->IRNode::genIPF();
    SwitchFont(curface,face); curface = face;
}

void IRGraphics::genIPF(void) {
    Put(":artwork name='");
    Put(this->name);
    Put(".bmp'.");
}


