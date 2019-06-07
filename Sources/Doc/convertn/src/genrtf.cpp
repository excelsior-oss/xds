#include <stdio.h>
#include <string.h>

#include "basedefs.h"
#include "scanner.h"
#include "ir.h"
#include "generate.h"

static int offset  = 0;

static int pars = 0;   // par bound state:
                       // 0 - not at par boundary
                       // 1 - at end of par
                       // 2 - at beginning of par

                                // Paragraph properties
char align = 'l';               // alignment - l,r,c, or j
int fi, li, ri = 0;             // paragraph indents
int f  = 0;                     // font #
int fs = 20;                    // font size
int keepn = 0;                  // keep with next

static char Align(int env) {
    switch (env) {
    case e_center:      return 'c';
    case e_flushleft:   return 'l';
    case e_flushright:  return 'r';
    default:            return 'l';
    }
}

static void Put(const char text[], int nl = 0) {
    char s[256];
    if (pars == 1) {
        WriteOutput("\\par\n");
        pars = 2;
    }
    if (pars == 2) {
        while (*text == ' ') text++;
        if (!*text) return;
        pars = nl = 0;
        sprintf(s,"\\pard \\plain \\q%c \\sb96 \\fi%d \\li%d \\tx%d \\f%d \\fs%d ",
                align,fi,li,li,f,fs);
        WriteOutput(s);
		if (keepn) WriteOutput("\\keepn ");
    }
    if (nl && offset) { WriteOutput("\n"); offset = 0; }
    WriteOutput(text);
    offset += strlen(text);
}

static void NL(void) { Put("",1); }

static void NLPut(const char text[]) { Put(text,1); }

static void Par(void) {if (!pars) pars = 1;}

static int Alt2Win[0x80] = {
        0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, // 0x80
        0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF,
        0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, // 0x90
        0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF,
        0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, // 0xA0
        0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF,
        0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, // 0xB0
        0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 
        0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, // 0xC0
        0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 
        0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, // 0xD0
        0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 
        0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, // 0xE0
        0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF,
        0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, // 0xF0
        0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F
        };

static void Translate(char text[], int verb = 0) {  // should be optimized
    char buf[BUFLEN*5], *pos;
    char s[8];
        unsigned char ch;
        int cyr = 0;
    pos = &buf[0];
    for(;*text;text++) {
                ch = *text;
        if ((ch) < 0x80) {
                        if (cyr) {
                                cyr = 0;
                                *pos++ = '}';
                        }
            switch (ch) {
            case '\\':
            case '{':
            case '}':
                *pos++ = '\\';
                break;
            case '\n':
                if (!verb) Error("EOL in not-verbatim");
                continue;
            }
            *pos++ = *text;
        }
        else {
                        if (!cyr) {
                                cyr = 1;
                                strcpy(pos,"{\\f32\\lang1049");
                                pos += 14;
                        }
            sprintf(s,"\\'%x",Alt2Win[ch-0x80]);
            strcpy(pos,s);
            pos += 4;
        }
    }
    *pos = '\0';
    Put(buf);
        if (cyr) Put("}");
}

static void TranslateCntLine(const char text[]) {
    char buf[BUFLEN], *pos;
        unsigned char ch;
    pos = &buf[0];
    for(;*text;text++) {
                ch = *text;
        if ((ch) < 0x80) 
            *pos++ = *text;       
        else 
            *pos++ = Alt2Win[ch-0x80];
    }
    *pos = '\0';
    Put(buf);
}

void IRNode::genRTF(void) {
    IRNode *scan;
    for(scan = this->son; scan; scan = scan->next) {
        if (scan->face != curface) {
            switch (curface) {
            case t_rm: break;
            case t_bf: Put("\\b0 "); break;
            case t_sf: break;
            case t_it: Put("\\i0 "); break;
            case t_sl: break;
            case t_sc: Put("\\scaps0 ");
            case t_tt: break;
            }
            switch (scan->face) {
            case t_rm: Put("\\f0 "); break;
            case t_bf: Put("\\f0 \\b "); break;
            case t_sf: Put("\\f1 ");break;
            case t_it: Put("\\f0 \\i "); break;
            case t_sl: break;
            case t_sc: Put("\\f0 \\scaps "); break;
            case t_tt: Put("\\f2 "); break;
            }
            curface = scan->face;
        }
        if (scan->size != cursize) {
            switch (scan->size) {
            case t_tiny: Put("\\fs12 "); break;
            case t_scriptsize: Put("\\fs14 "); break;
            case t_footnotesize: Put("\\fs16 "); break;
            case t_small: Put("\\fs18 "); break;
            case t_normalsize: Put("\\fs20 "); break;
            case t_large: Put("\\fs24 "); break;
            case t_Large: Put("\\fs28 "); break;
            case t_LARGE: Put("\\fs32 "); break;
            case t_huge: Put("\\fs40 "); break;
            case t_Huge: Put("\\fs48 ");  break;
            }
            cursize = scan->size;
        }
        scan->genRTF();
    }
}

static void GenContents(void) {
    IRHeading* scan;
    char s[32];
    pars = 0;
    int level;

    // Generate collapsed contents
    NLPut("#{\\footnote contents}");
    NLPut("\\pard \\keepn \\plain \\sb144 \\sa144 \\fs28 \\b \\cf10 ");
    IRRoot::title.genRTF();
    Put("\\line"); NL();
    Put("\\plain \\fs16 {\\uldb Expand}{\\v expanded_contents}");
    Par();
    for (scan = IRHeading::getfirst(); scan; scan = scan->next) {
        if ((scan->level > 1) || (!scan->toc)) continue;
        Put("{\\uldb ");
        scan->IRNode::genRTF();
        Put("}");
        sprintf(s, "{\\v topic%d}",scan->id);
            Put(s);
        Par();
    }
    NLPut("\\page");

    NLPut("#{\\footnote expanded_contents}");
    NLPut("\\pard \\keepn \\plain \\sb144 \\sa144 \\fs28 \\b \\cf10 ");
    IRRoot::title.genRTF();
    Put("\\line"); NL();
    Put("\\plain \\fs16 {\\uldb Collapse}{\\v contents}\\par");
    NLPut("\\pard \\plain \\sb96 \\fs20 {\\uldb Disclaimer}{\\v disclaimer}\\par");
    NLPut("\\pard \\plain \\sb96 \\fs20 {\\uldb Title Page}{\\v title}");
    NL();
    for (scan = IRHeading::getfirst(); scan; scan = scan->next) {
        Par();
        if (scan->level > 3) continue;
        // This section is hidden?
        if (!scan->toc) {
            // Yes, skip all with greater level
            for (level = scan->level;
                 (scan->next) && (scan->next->level > level);
                  scan = scan->next);
            continue;
        }
        switch (scan->level) {
        case 1:  Put("\\sa144 \\sb144 \\fs28 \\b \\i "); break;
        case 2:  Put("\\sb120 \\fs20 "); break;
        case 3:  Put("\\fs20 "); break;
        default: Put("\\fs20 "); break;
        }
        sprintf(s, "\\li%d ", (scan->level-1)*360);
        Put(s);
        Put("{\\uldb ");
        scan->IRNode::genRTF();
        Put("}");
        sprintf(s, "{\\v topic%d}",scan->id);
            Put(s);
    }
    NLPut("\\page");
}

static void GenTables(void) {
    char s[256];
    IRFloat *scan;
    if (!IRFloat::first) return;
//    NLPut(":h1.Tables");

    for (scan = IRFloat::first; scan; scan = scan->next) {
		if (scan->type != 'T') continue;
        Par();
        NLPut("\\page");
        sprintf(s,"${\\footnote Table %d. ",scan->number);
        NLPut(s);
        scan->caption.genRTF();
        Put("}");
        sprintf(s,"#{\\footnote table%d}",scan->number);
        NLPut(s);
        NLPut("+{\\footnote 0}");
        NLPut("\\pard \\keepn \\plain \\fs28 \\b \\cf10 ");
        sprintf(s,"Table %d. ",scan->number);
        Put(s);
        scan->caption.genRTF();
        Par();
        scan->IRNode::genRTF();
    }
}

static void GenFigures(void) {
    char s[256];
    IRFloat *scan;
    if (!IRFloat::first) return;
//    NLPut(":h1.Tables"); !!!

    for (scan = IRFloat::first; scan; scan = scan->next) {
		if (scan->type != 'F') continue;
        Par();
        NLPut("\\page");
        sprintf(s,"${\\footnote Figure %d. ",scan->number);
        NLPut(s);
        scan->caption.genRTF();
        Put("}");
        sprintf(s,"#{\\footnote figure%d}",scan->number);
        NLPut(s);
        NLPut("+{\\footnote 0}");
        NLPut("\\pard \\keepn \\plain \\fs28 \\b \\cf10 ");
        sprintf(s,"Figure %d. ",scan->number);
        Put(s);
        scan->caption.genRTF();
        Par();
        scan->IRNode::genRTF();
    }
}

static void GenPopUps(void) {
    char s[256];
    IRFloat *scan;
    String *pub;
    if (!IRFloat::first) return;
//    NLPut(":h1.Tables"); !!!
    for (scan = IRFloat::first; scan; scan = scan->next) {
		if (scan->type != 'P') continue;
        Par();
        NLPut("\\page");
        sprintf(s,"#{\\footnote popup%d}",scan->number);
        NLPut(s);
        for (pub = scan->publist.first; pub; pub = pub->next) {
            NLPut("#{\\footnote "); Put(pub->string); Put("}");
        }
/* !!!
        NLPut("\\pard \\keepn \\plain \\fs28 \\b \\cf10 ");
        sprintf(s,"Figure %d. ",scan->number);
        Put(s);
        scan->caption.genRTF();
*/
		keepn = 1;
		pars = 2; Put("\\b ");
        scan->caption.genRTF();
        Par();
        scan->IRNode::genRTF();
		keepn = 0;
    }
}

void IRRoot::genRTF(void) {
    OpenOutput(this->name,".rtf");
    curface = t_rm;
    cursize = t_normalsize;
    pars = 0;
    Put  ("{\\rtf1\\ansi\\deff0\\deflang1033");
    NLPut("{\\fonttbl");
    Put("{\\f0\\fswiss\\fcharset0\\fprq2 Arial}");
    Put("{\\f1\\froman\\fcharset0\\fprq2 Times New Roman;}");
    Put("{\\f2\\fmodern\\fcharset0\\fprq1 Courier New}");
        Put("{\\f3\\froman\\fcharset2\\fprq2 Symbol;}");
    Put("{\\f32\\fswiss\\fcharset204\\fprq2 Arial CYR;}");
    Put("{\\f33\\froman\\fcharset204\\fprq2 Times New Roman CYR;}");
    Put("{\\f34\\fmodern\\fcharset204\\fprq1 Courier New CYR}");
        Put("}");
    NLPut("{\\colortbl;");
    Put("\\red0\\green0\\blue0;");
    Put("\\red0\\green0\\blue127;");
    Put("\\red0\\green127\\blue0;");
    Put("\\red0\\green127\\blue127;");
    Put("\\red127\\green0\\blue0;");
    Put("\\red127\\green0\\blue127;");
    Put("\\red127\\green127\\blue0;");
    Put("\\red127\\green127\\blue127;");
    Put("\\red63\\green63\\blue63;");
    Put("\\red0\\green0\\blue255;");
    Put("\\red0\\green255\\blue0;");
    Put("\\red0\\green255\\blue255;");
    Put("\\red255\\green0\\blue0;");
    Put("\\red255\\green0\\blue255;");
    Put("\\red255\\green255\\blue0;");
    Put("\\red255\\green255\\blue255;}");

    GenContents();
    NLPut("\\pard \\plain \\sb96 \\fs20");
    NLPut("${\\footnote Disclaimer}");
    NLPut("#{\\footnote disclaimer}");
    NLPut("This on-line document was generated automatically from ");
    NLPut("its printed version LaTeX source. \\par");
    NLPut("\\page");

    NLPut("\\pard \\plain \\sb96 ");
    NLPut("${\\footnote Title Page}");
    NLPut("#{\\footnote title}");
        NLPut("+{\\footnote 0}");
    NL();
    this->IRNode::genRTF();
    GenTables();
    GenFigures();
	GenPopUps();
    NLPut("}");
    NL();
    CloseOutput();
}

void IRRoot::genCNT(int cntlevel) {
    IRHeading* scan;
    char s[256];

    cntlevel--;
    pars = 0;
    OpenOutput(this->name,".cnt");
    Put(":Title "); title.gentext(TranslateCntLine); NL();
    Put(":Base "); Put(this->name); Put(".hlp"); NL();
    if (cntlevel > 0) {
        sprintf(s, "%d ", cntlevel);
        NLPut(s); title.gentext(TranslateCntLine); NL();
    }
    for (scan = IRHeading::getfirst(); scan; scan = scan->next) {
        if ((scan->level > 4) || (!scan->toc)) continue;
        sprintf(s, "%d ", scan->level+cntlevel);
        Put(s);
        scan->IRNode::gentext(TranslateCntLine);
        if ((scan->level < 4) &&
            (scan->next) &&
            (scan->next->level > scan->level)) {
            NL();
            sprintf(s, "%d ", scan->level+cntlevel+1);
            Put(s);
            scan->IRNode::gentext(TranslateCntLine);
        }
        if (scan->publist.first)
            sprintf(s, "=%s",scan->publist.first->string);
        else
            sprintf(s, "=topic%d",scan->id);
        Put(s);
        NL();
    }
    CloseOutput();
}

void IRPlain::genRTF(void) {
    Translate(this->text);
}

void IRCommand::genRTF(void) {
    char buf[256];
    switch (this->code) {
    case t_par:
        Par();
        break;
    case t_newline:
        NLPut("\\line "); NL(); break;
    case t_copyright:
        Put("\\'a9"); break;
    case t_hyphen:
        Put("-"); break;
    case t_endash:
        Put("\\'96"); break;
    case t_emdash:
        Put("\\'97"); break;
    case t_lsquo:
        Put("\\'91"); break;
    case t_rsquo:
        Put("\\'92"); break;
    case t_ldquo:
        Put("\\'93"); break;
    case t_rdquo:
        Put("\\'94"); break;
    case t_TeX:
        Put("TeX"); break;
    case t_LaTeX:
        Put("LaTeX"); break;
    case t_textbackslash:
    case t_backslash:
        Put("\\\\"); break;
    case t_surd:
        Put("{\\f3 \\'d6}"); break;
    case t_times:
        Put("{\\f3 \\'b4}"); break;
    case t_leq:
        Put("{\\f3 \\'a3}"); break;
    case t_neq:
        Put("{\\f3 \\'b9}"); break;
    case t_subset:
        Put("{\\f3 \\'cc}"); break;
    case t_subseteq:
        Put("{\\f3 \\'cd}"); break;
    case t_supseteq:
        Put("{\\f3 \\'ca}"); break;
    case t_rightarrow:
        Put("{\\f3 \\'ae}"); break;
    case t_pi:
        Put("{\\f3 p}"); break;
    case t_exp:
        Put("{\\f1 exp}"); break;
    case t_ln:
        Put("{\\f1 ln}"); break;
    case t_Diamond:
        Put("{\\f3 \\'e0}"); break;
    case t_circ:
        Put("{\\f1 \\'b7}"); break;
    case t_ldots:
        Put("..."); break;
    default:
        Put("?");
        Warning("Symbol '%s' is unsupported",VisToken(code,buf));
        break;
    }
}

int listenv = e_unknown;
int itemnum;

void IRItem::genRTF(void) {
    char s[8];
    fi = -240;
    Par();
    switch (listenv) {
    case e_itemize:
        Put("\\'95 \\tab ");
        break;
    case e_enumerate:
        sprintf(s,"%d.",itemnum++);
        Put(s);
        break;
    case e_description:
        if (son) this->IRNode::genRTF();
        Par();
        break;
    default:
        FatalError("\\item outside list environment");
    }
    fi = 0;
}

void IRVerbatim::genRTF(void) {
    String *scan = list.first;
    while (scan) {
        NL();
        Translate(scan->string,1);
        if ((scan = scan->next) != 0) Put("\\line");
    }
}

void IREnv::genRTF(void) {
    int  le = listenv;
    int  in = itemnum;
    char al = align;

    switch (this->code) {
    case e_document:
        this->IRNode::genRTF();
        break;
    case e_verbatim:
        Par();
        NLPut("\\keep {\\f2 ");
        this->IRNode::genRTF();
        Put("}");
        Par();
        break;
    case e_itemize:
    case e_enumerate:
    case e_description:
        li += 360;
        listenv = this->code;
        itemnum = 1;
        this->IRNode::genRTF();
        listenv = le;
        itemnum = in;
        li -= 360;
        Par();
        break;
    case e_center:
    case e_flushleft:
    case e_flushright:
        align = Align(this->code);
        Par();
        this->IRNode::genRTF();
        align = al;                 // Restore alignment
        Par();
        break;
    default:
        Par();
        this->IRNode::genRTF();
        Par();
        break;
    }
}

void IRTable::genRTF(void) {
    char al = align;
    char s[256];
    align = 'l';
    Par();
    sprintf(s,"{\\uldb Table %d. ",this->number);
    Put(s);
    this->caption.genRTF();
    Put("}");
    sprintf(s, "{\\v table%d}",this->number);
    Put(s);
    align = al;
}

void IRFigure::genRTF(void) {
    char al = align;
    char s[256];
    align = 'l';
    Par();
    sprintf(s,"{\\uldb Figure %d. ",this->number);
    Put(s);
    this->caption.genRTF();
    Put("}");
    sprintf(s, "{\\v figure%d}",this->number);
    Put(s);
    align = al;
}

void IRPopUp::genRTF(void) {} // Nothing is generated inline

static int cellwidth;

static void AddLen(const char text[]) {
    cellwidth += strlen(text);
}

void IRTabular::genRTF(void) {
    char al = align;
    IRNode *row,*cell;
    int edge, colwidth[32], colno;
    char s[256],w[16];

    align = 'l';

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

    strcpy(s,"\\trowd \\trqc");
    edge = 0;
    for (colno = 0; colwidth[colno] >= 0; colno++) {
        edge += colwidth[colno];
        sprintf(w,"\\cellx%d ",edge*180);
        strcat(s,w);
    }
    NLPut(s);

    for (row = this->son; row; row = row->next) {
        NLPut("\\intbl ");
        for (cell = row->son; cell; cell = cell->next) {
            cell->genRTF();
            Put("\\cell ");
        }
        Put("\\row ");
    }
    NL();
    pars = 2;
    align = al;
}

void IRHeading::genRTF(void) {
    IRHeading *link;
    char s[256];
    String* scan;
    int super = 0;              // Supertopics existense flag
    int sub = 0;                // Subtopics existense flag
    int level;
    int t;

    Par();
    if (this->level <= 4) {
        if (this->prev && 
			this->prev->minitoc &&
			(this->prev->level == this->level-1)) {
            // Generate list of subtopics
            pars = 1;
            Put("\\tx360 "); NL();
            for (link = this; link; link = link->next) {
                if (link->level < this->level) break;
                if (link->level > 4) continue;
                if (link->level > this->level) continue;
                Put("\\tab ");
                Put("{\\uldb ");
                link->IRNode::genRTF();
                Put("}");
                sprintf(s, "{\\v topic%d}",link->id);
                Put(s);
                Put("\\line"); NL();
            }
        }
        NLPut("\\page");        // End previous topic
        sprintf(s, "#{\\footnote navigate%d}",this->id);
        NLPut(s);
        pars = 2;
        Put("\\tx180 \\tx360 \\tx540 \\tx720 \\tx900 \\tx1080 \\tx1260 \\tx1440 "); NL();
        Put("\\b {\\uldb "); IRRoot::title.genRTF(); Put("}{\\v contents}\\b0");
        for (level = 1; level < this->level; level++) {
            super++;
            if ((link = this->super(level)) != 0) {
                Put("\\line "); NL();
                for (t = 0; t < level; t++) Put("\\tab ");
                Put("{\\uldb "); link->IRNode::genRTF(); Put("}");
                sprintf(s, "{\\v topic%d}",link->id);
                Put(s);
            }
        }
        Put("\\line"); NL();
        for (t = 0; t < this->level; t++) Put("\\tab ");
        this->IRNode::genRTF();
        for (link = this->next; link; link = link->next) {
            if (link->level <= this->level) break;
            if (link->level > 4) continue;
            if (link->level > this->level+1) continue;
            if (!link->toc) continue;
            sub++;
            Put("\\line"); NL();
            for (t = 0; t <= this->level; t++) Put("\\tab ");
            Put("{\\uldb ");
            link->IRNode::genRTF();
            Put("}");
            sprintf(s, "{\\v topic%d}",link->id);
            Put(s);
        }
        Par();
        NLPut("\\page");        // End a navigate topic
/*
        if (sub||super) {
            sprintf(s,"!{\\footnote ChangeButtonBinding(Navigate,PopupId(navigate%d))}",this->id);
            NLPut(s);
        }
        else NLPut("!{\\footnote DisableButton(Navigate))}");
*/
        NLPut("${\\footnote ");
        this->IRNode::genRTF();
        Put("}");
        sprintf(s, "#{\\footnote topic%d}",this->id);
        NLPut(s);
        for (scan = publist.first; scan; scan = scan->next) {
            NLPut("#{\\footnote "); Put(scan->string); Put("}");
        }
        NLPut("+{\\footnote 0}");
        for (scan = indlist.first; scan; scan = scan->next) {
            NLPut("K{\\footnote ");
            Translate(scan->string,1);
            scan = scan->next;
            if (scan->string[0]) {
                Put(" - ");
                Translate(scan->string,1);
            }
            Put("}");
        }
        NLPut("\\pard \\keepn \\plain \\sb144 \\sa144 \\fs28 \\b \\cf10 ");
        this->IRNode::genRTF();
        if (sub||super) {
            Put("\\line");
            NLPut("\\plain \\fs16");
            sprintf(s, "{\\ul Where Am I?}{\\v navigate%d}",this->id);
            Put(s);
        }
    }
    else {
        NLPut("\\pard \\ql \\sa144 \\sb196 \\plain \\fs24 \\b ");
        this->IRNode::genRTF();
    }
    Par();
}

void IRRef::genRTF(void) {
    char s[64];
    IRHeading* h;
    IRExtern *e;
    IRFloat *f;
    h = IRHeading::find(this->label);
    if (h) {
        Put("{\\uldb ");
        if (son) this->IRNode::genRTF(); else h->IRNode::genRTF();
        Put("}");
        sprintf(s, "{\\v topic%d}",h->id);
        Put(s);
        return;
    }
    e = IRExtern::find(this->label);
    if (e) {
        sprintf(s, "{\\strike %s}{\\v %s@%s}", e->label, e->label, e->file );
        Put(s);
        return;
    }
    f = IRFloat::find(this->label);
    if (f) {
        if (f->type == 'T') {
            Put("{\\uldb ");
            if (son) this->IRNode::genRTF();
            else {
                sprintf(s, "%d. ",f->number);
                Put(s);
                f->caption.genRTF();
            }
            Put("}");
            sprintf(s, "{\\v table%d}",f->number);
            Put(s);
            return;
        }
        else if (f->type == 'F') {
            Put("{\\uldb ");
            if (son) this->IRNode::genRTF();
            else {
                sprintf(s, "%d. ",f->number);
                Put(s);
                f->caption.genRTF();
            }
            Put("}");
            sprintf(s, "{\\v figure%d}",f->number);
            Put(s);
            return;
        }
        else if (f->type == 'P') {
            Put("{\\ul ");
            if (son) this->IRNode::genRTF();
            else {
                sprintf(s, "%d. ",f->number);
                Put(s);
                f->caption.genRTF();
            }
            Put("}");
            sprintf(s, "{\\v popup%d}",f->number);
            Put(s);
            return;
        }
        FatalError("Unknown float type in reference");
    }
    WarningAt(this->pos,"Unresolved reference - '%s'",this->label);
    Put("???");
}

void IRFootnote::genRTF() {
    Put("(");
    this->IRNode::genRTF();
    Put(")");
}

void IRHRef::genRTF() {
    this->IRNode::genRTF();
}


void IRGraphics::genRTF(void) {
    Put("\\{bmc ");
    Put(this->name);
    Put(".bmp\\}");
}

