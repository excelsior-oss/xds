#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "basedefs.h"
#include "scanner.h"
#include "ir.h"
#include "generate.h"

typedef int (*IsTerm)(int);     // 0/1 = parameter is curr. scope terminator

// Function prototypes

void        Expect(int t);
void        Check(int t);
IRGroup*    Group(void);
IRNode*     Environment(void);
IRTable*    Table(void);
IRFigure*   Figure(void);
IRPopUp*    PopUp(void);
void        Caption(void);
IRTabular*  Tabular(int code);
IRHeading*  Heading(int level);
void        Label(int pub);
void        Extern(void);
IRRef*      Ref(void);
void        Index(void);
IRPlain*    Verb(void);
IRFootnote* Footnote(void);
IRGraphics* IncludeGraphics(void);
void        NewCommand(void);
void        UserCommand(void);
void        NewEnvironment(void);
IRNode*     UserEnvironment(void);
void        Ignore(int opt, int req);
IRNode*     Parse(void);
IRNode*     Scope(IsTerm isterm);

int IsRBrace(int token)   {return token == t_rbrace;}
int IsRBracket(int token) {return token == t_rbracket;}
int IsMath(int token)     {return token == t_math;}
int IsEnd(int token)      {return token == t_end;}
int IsEOF(int token)      {return token == t_eof;}

void Expect(int t) {
    GetToken();
    if (token != t) Error("Expected token %d",t);
}

void Check(int t) {
    if (token != t) Error("Expected token %d",t);
}

static void AppendToPlain(const char text[]) {
   strncat(plain, text, BUFLEN-strlen(plain)-1);
}

void Input(void) {
    char *p;
    Expect(t_lbrace);
    GetString('}');
    Expect(t_rbrace);
    if ((p = strrchr(plain,'\\')) == 0) p = plain;
    if (strchr(p,'.') == 0) strcat(plain,".tex");
    Source(plain);
}

void Include(void) {
    Expect(t_lbrace);
    GetString('}');
    Expect(t_rbrace);
    strcat(plain,".tex");
    Source(plain);
}

#define m_normal 0
#define m_math 1
#define m_LR 2

int curmode = m_normal;

IRGroup* Group(void) {
    IRGroup *g;
    int b = brackets;
    brackets = 0;
    GetToken();
    g = new IRGroup;
    g->adopt(Scope(IsRBrace));
    brackets = b;
    GetToken();
    return g;
}

IRNode* Math(int term) {
    IRNode* n;
    int mode = curmode;
    int face = IRNode::curface;
    curmode = m_math;
    IRNode::curface = f_math;
    GetToken();
    n = new IRNode;
    n->adopt(Scope(IsMath));
    curmode = mode;
    IRNode::curface = face;
    GetToken();
    return n;
}


IRNode* Environment(void) {
    int en;
    IREnv *e;
    StringList list;
    Expect(t_lbrace);
    Expect(t_plain);

    en = FindEN(plain);
    if (en == e_unknown) return UserEnvironment();

    e = new IREnv(en);
    Expect(t_rbrace);
    switch (e->code) {
    case e_verbatim:
        GetVerbatim(list);
        e->adopt(new IRVerbatim(list));
        break;
    case e_table:
        e->adopt(Table());
        break;
    case e_figure:
        e->adopt(Figure());
        break;
    case e_popup:
        e->adopt(PopUp());
        break;
    case e_tabular:
        return Tabular(e_tabular);
    case e_document:
    case e_center:
    case e_flushleft:
    case e_flushright:
    case e_enumerate:
    case e_itemize:
    case e_description:
        GetToken();
        e->adopt(Scope(IsEnd));
        Expect(t_lbrace);
        Expect(t_plain);
        if (e->code != FindEN(plain))
            FatalError("Bad environment nesting");
//        printf("\nEnvironment '%s' ends",plain);
        Expect(t_rbrace);
        SkipSpaces();
        break;
    default:
        Error("Environment %d not supported", en);
    }
    GetToken();
    return e;
}

IRFloat* curfloat = 0;

IRTable* Table(void) {
    IRTable *t = new IRTable;
    SkipSpaces();
    brackets++;
    GetToken();
    if (token == t_lbracket) {
        GetToken();
        if (token == t_plain) GetToken();
        Check(t_rbracket);
        SkipSpaces();
        brackets--;
        GetToken();
    }
    else
        brackets--;
    if (curfloat) Error("One float inside another");
    curfloat = t;
    t->adopt(Scope(IsEnd));
    curfloat = 0;
    Expect(t_lbrace);
    Expect(t_plain);
    if (FindEN(plain) != e_table)
        FatalError("Bad environment nesting");
    Expect(t_rbrace);
    SkipSpaces();
    return t;
}

IRFigure* curfigure = 0;

IRFigure* Figure(void) {
    IRFigure *f = new IRFigure;
    SkipSpaces();
    brackets++;
    GetToken();
    if (token == t_lbracket) {
        GetToken();
        if (token == t_plain) GetToken();
        Check(t_rbracket);
        SkipSpaces();
        brackets--;
        GetToken();
    }
    else
        brackets--;
    if (curfloat) Error("One float inside another");
    curfloat = f;
    f->adopt(Scope(IsEnd));
    curfloat = 0;
    Expect(t_lbrace);
    Expect(t_plain);
    if (FindEN(plain) != e_figure)
        FatalError("Bad environment nesting");
    Expect(t_rbrace);
    SkipSpaces();
    return f;
}

IRPopUp* curpopup = 0;

IRPopUp* PopUp(void) {
    IRPopUp *p = new IRPopUp;
    SkipSpaces();
    GetToken();
    if (curfloat) Error("One float inside another");
    curfloat = p;
    p->adopt(Scope(IsEnd));
    curfloat = 0;
    Expect(t_lbrace);
    Expect(t_plain);
    if (FindEN(plain) != e_popup)
        FatalError("Bad environment nesting");
    Expect(t_rbrace);
    SkipSpaces();
    return p;
}


void Caption(void) {
    Expect(t_lbrace);
    GetToken();
    if (!curfloat) Error("\\caption outside float");
    curfloat->caption.adopt(Scope(IsRBrace));
}

int IsCellTerm(int token) {
    return (token == t_tab) ||
           (token == t_newline) ||
           (token == t_tabularnewline) ||
           (token == t_end);
}

IRCol* Cols(void) {
    char *scan;
    IRCol *first = new IRCol;
    IRCol *col = first;
    IRNode *width, *colsep;
    Check(t_plain);
    for (scan = plain; *scan; ) {
        switch (*scan) {
        case '|':
            col->rborder++;
            scan++;
            break;
        case 'l':
        case 'r':
        case 'c':
            col->next = new IRCol;
            col = col->next;
            col->align = *scan;
            scan++;
            break;
        case 'p':
            col->next = new IRCol;
            col = col->next;
            col->align = 'l';
            if (*(++scan))
                Error("Expected '{' after 'p'");
            Expect(t_lbrace);
            GetToken();
            width = Scope(IsRBrace);
            GetToken();
            if (token == t_rbrace) return first;
            if (token == t_plain) scan = plain;
            else 
                Error("Cannot parse column descriptor");
            break;
        case '@':
            if (*(++scan))
                Error("Expected '{' after '@'");
            Expect(t_lbrace);
            GetToken();
            colsep = Scope(IsRBrace);
            GetToken();
            if (token == t_rbrace) return first;
            if (token == t_plain) scan = plain;
            else
                Error("Cannot parse column descriptor");
            break;
        default:
            Error("Unrecognized character in column descriptor: %s", scan);
            break;
        }                 
    }
    Expect(t_rbrace);
    if (!first->next)
        Error("no columns defined");
    return first;
}

IRTabular* Tabular(int code) {
    IRTabular *t = new IRTabular(code);
    IRNode *row,*lastrow,*cell,*lastcell;
    IRRowSep *rowsep;
    int face = IRNode::curface;
   
    brackets++;
    GetToken();
    if (token == t_lbracket) {
        Expect(t_plain);
        if ((plain[0] != 't' && plain[0] != 'b') ||
            (plain[1] != '\0'))
            Error("Expected 't' or 'b'");
        Expect(t_rbracket);
        brackets--;
        GetToken();
    }
    else brackets--;
    Check(t_lbrace);
    GetToken();
    t->cols = Cols();
    t->rowseps = new IRRowSep;
    rowsep = t->rowseps;
    row = lastrow = 0;
    for (; token != t_end; ) {
        SkipSpaces();
        GetToken();
        if (!row) {
            for (;;) {
                if (token == t_hline) {
                    rowsep->border = ~0;
                }
                else if (token == t_cline) {
                    Expect(t_lbrace);
                    Expect(t_plain);
                    Expect(t_rbrace);
                }
                else if (token != t_par) break;
                GetToken();
            }
            rowsep->next = new IRRowSep;
            rowsep = rowsep->next;
        }
        if (token == t_multicolumn) {
            int span = 0;
            char *p;
            IRCol *cols;
            char align;
            int lb = 0, rb = 0;
            Expect(t_lbrace);
            Expect(t_plain);
            for (p=plain; *p; p++) {
                if ((*p < '0') || (*p > '9'))
                    Error("Digit expected");
                span = span*10+(*p-'0');
            }
            Expect(t_rbrace);
            Expect(t_lbrace);
            GetToken();
            cols = Cols();
            lb = cols->rborder;
            rb = cols->next->rborder;
            align = cols->next->align;
            Expect(t_lbrace);
            GetToken();
            cell = new IRCell(span,align,lb,rb);
            cell->adopt(Scope(IsRBrace));
            SkipSpaces();
            GetToken();
        }
        else {
            cell = new IRCell;
            cell->adopt(Scope(IsCellTerm));
        }
        if (!row) {
            if ((token == t_end) && (!cell->son)) break;
            row = new IRNode;
            row->adopt(cell);
        }
        else
            lastcell->append(cell);
        lastcell = cell;
        if (token != t_tab) {
            if (row) {
                if (lastrow) lastrow->append(row);
                else t->adopt(row);
                lastrow = row;
                row = 0;
            }
       }
    }
    Expect(t_lbrace);
    Expect(t_plain);
    if (code != FindEN(plain))
        FatalError("Bad environment nesting");
    Expect(t_rbrace);
    SkipSpaces();
    GetToken();
//    t->face = f_tab;
    IRNode::curface = face;
    return t;
}

IRItem* Item(void) {
    IRItem *item = new IRItem;
    brackets++;
    GetToken();
    if (token == t_lbracket) {
        GetToken();
        item->adopt(Scope(IsRBracket));
        SkipSpaces();
        brackets--;
        GetToken();
    }
    else
        brackets--;
    return item;
}

void Title(void) {
    Expect(t_lbrace);
    GetToken();
    IRRoot::title.adopt(Scope(IsRBrace)); // !!!
}

//
// Headings and cross-references
//

IRHeading* Heading(int level) {
    IRHeading *h = new IRHeading(level,!asterisk);
    if (asterisk) Warning("No TOC Entry");
//    printf("\nHeading level %d",level);
    Expect(t_lbrace);
    GetToken();
    h->adopt(Scope(IsRBrace));
    SkipSpaces();
    GetToken();
    return h;
}

void GetLabel(char buf [], unsigned maxlen) {
    buf[0] = '\0';
    for(;;) {
        GetToken();
        if ((token == t_rbrace) || (token == t_rbracket)) break;
        switch (token) {
        case t_plain:
            if (strlen(buf)+strlen(plain) >= maxlen)
                Error("Label too long");
            strcat(buf,plain);
            break;
        case t_usercommand:
            UserCommand();
            break;
        default:
            break;
        }
    }
}

void Label(void) {
    char buf[256];
    Expect(t_lbrace);
    GetLabel(buf,256);
    curfloat ? IRFloat::addlabel(buf) : IRHeading::addlabel(buf);
    SkipSpaces();
}

IRPublic* Public(void) {
    char buf[256];
    IRPublic *p;
    Expect(t_lbrace);
    GetLabel(buf,256);
    curfloat ? IRFloat::addpublic(buf) : IRHeading::addpublic(buf);
    p = new IRPublic(buf);
    SkipSpaces();
    GetToken();
    return p;
}


void Extern(void) {
    char file[256];
    char label[256];
    int  part,id;

    Expect(t_lbrace);
    GetString('}');
    strncpy(file,plain,256);
    Expect(t_rbrace);
    Expect(t_lbrace);
    GetString('}');
    strncpy(label,plain,256);
    Expect(t_rbrace);
    brackets++;
    GetToken();
    if (token == t_lbracket) {
        GetString(']');
        part = strtoul(plain,0,10);
        Expect(t_rbracket);
        brackets--;
        Expect(t_lbrace);
        GetString('}');
        id = strtoul(plain,0,10);
        new IRExtern(file,label,id,part);
    }
    else
    {  
        brackets--;
        GetString('}');
        id = strtoul(plain,0,10);
        new IRExtern(file,label,id);
    }
    Expect(t_rbrace);
    GetToken();
}

IRRef* Ref(void) {
    char label[256];
    IRRef *r = new IRRef;
    brackets++;
    GetToken();
    if (token == t_lbracket) {
        GetToken();
        r->adopt(Scope(IsRBracket));
        brackets--;
        GetToken();
    }
    else
        brackets--;
    Check(t_lbrace);
    GetLabel(label,256);
    r->setlabel(label);
    GetToken();
    return r;
}

char IndexBuf[256];
int  indexpos;

void AppendIndex(const char str[]) {
    int l = strlen(str);
    if (indexpos+l >= 256)
        Error("Index too long");
    strcat(IndexBuf,str);
    indexpos += l;
}

void Index(void) {
    IRNode *n;
    char buf [256];
    char *primary;
    char *secondary = 0;
    char *p,*q;
    int copy = 1;   // copy flag
    int level = 1;  // index level

    Expect(t_lbrace);
    GetToken();
    n = new IRNode;
    n->adopt(Scope(IsRBrace));
    indexpos = 0; IndexBuf[0] = '\0';
    n->gentext(AppendIndex);
    q = &buf[0];
    primary = q;
    for(p = &IndexBuf[0]; *p; *p++) { //??
        switch (*p) {
        case '"':
            p++;
            if (!*p)
                Error("\\index argument ended with the quote character");
            break;
        case '\\':
            *q++ = *p++;
            if (!*p)
                Error("\\index argument ended with the escape character");
            break;
        case '@':
            copy = 0;   // !!! the soritng key will be included
            break;
        case '!':
            if (level >= 2)
                Error("Indexes of three or more levels are not supported");
            level++;
            *q++ = '\0';
            secondary = q;
            copy = 1;
            continue;   // !!! "!" won't be copied
        }
        if (copy) *q++ = *p;
    }
    *q = '\0';
    if (!secondary) secondary = q;
    IRHeading::addindex(primary,secondary);
    SkipSpaces();
    GetToken();
}

/*
void Index(void) {
    StringList list;
    char buf [256];
    char *p,*q;
    Expect(t_lbrace);
    GetParameter(list);
    Expect(t_rbrace);
    if (list.first->next)
        Error("\\index argument should reside on a single line");
    q = &buf[0];
    for(p = list.first->string; *p; *p++) {//??
        switch (*p) {
        case '"':
            p++;
            if (!*p)
                Error("\\index argument ended with the quote character");
            break;
        case '\\':
            *q++ = *p++;
            if (!*p)
                Error("\\index argument ended with the escape character");
            break;
        }
        *q++ = *p;
    }
    IRHeading::addindex(buf);
    SkipSpaces();
    GetToken();
}
*/

IRPlain* Verb(void) {
    IRPlain *p;
    GetVerb();
    p = new IRPlain(plain,1);
    p->face = t_tt;
    GetToken();
    return p;
}

IRFootnote* Footnote(void) {
    IRFootnote *f;
    int face = IRNode::curface;
    int size = IRNode::cursize;
    IRNode::curface = t_rm;
    IRNode::cursize = t_footnotesize;
    Expect(t_lbrace);
    GetToken();
    f = new IRFootnote;
    f->adopt(Scope(IsRBrace));
    GetToken();
    IRNode::curface = face;
    IRNode::cursize = size;
    return f;
}

IRHRef* HRef(int text) {
    IRHRef *hr;
    IRNode *n = new IRNode;
    Expect(t_lbrace);
    GetToken();
    n->adopt(Scope(IsRBrace));
    plain[0] = 0;
    n->gentext(&AppendToPlain);
    hr = new IRHRef(plain);
    GetToken();
    if (text) {
        Check(t_lbrace);
        GetToken();
        hr->adopt(Scope(IsRBrace));
        GetToken();
    }
    return hr;
}


//
// User-defined commands
//

void NewCommand(void) {
    char name [32];
    int  nop = 0;
    UCmd *uc;

    Expect(t_lbrace);
    GetToken();
    if ((token != t_usercommand) && (token != t_unknowncw))
        Error("Command expected");
        //!!!!!!    Expect(t_unknowncw);            // raise some sort of scanner flag here?
    strcpy(name,cw);
    Expect(t_rbrace);
    SkipSpaces();
    brackets++;
    GetToken();
    brackets--;
    if (token==t_lbracket) {
        GetString(']');
        if((plain[0] < '0') || (plain[0] > '9') || (plain[1] != '\0'))
            Error("Digit expected");
        nop = plain[0]-'0';
        brackets++;
        GetToken();
        brackets--;
        Check(t_rbracket);
        SkipSpaces();
        GetToken();
    }
    uc = new UCmd(name,nop);
    Check(t_lbrace);
    GetParameter(*uc);
    Expect(t_rbrace);
    SkipSpaces();
}

void UserCommand(void) {
    int i;
    for (i=1 ; i<=command->nop; i++) {
        SkipSpaces();
        Expect(t_lbrace);
        GetParameter(command->param[i]);
        Expect(t_rbrace);
    }
    Source(command);
}

void NewEnvironment(void) {
    char name [32];
    int  nop = 0;
    char *opt = 0;   // Default value for optional argument
    UEnv *ue;
    UCmd *uc;

    Expect(t_lbrace);
    GetToken();
    if (token != t_plain)
        Error("Environment name expected");
    if (strlen(plain) >= 32)
        Error("Environment name too long");
    strcpy(name,plain);
    Expect(t_rbrace);
    SkipSpaces();
    brackets++;
    GetToken();
    brackets--;
    if (token==t_lbracket) {
        GetString(']');
        if((plain[0] < '1') || (plain[0] > '9') || (plain[1] != '\0'))
            Error("Digit from 1 to 9 expected");
        nop = plain[0]-'0';
        brackets++;
        GetToken();
        brackets--;
        Check(t_rbracket);
        SkipSpaces();
        brackets++;
        GetToken();
        brackets--;
        if (token==t_lbracket) {
            GetString(']');
            opt = new char[strlen(plain)+1];
            strcpy(opt,plain);
            brackets++;
            GetToken();
            brackets--;
            Check(t_rbracket);
            SkipSpaces();
            brackets++;
            GetToken();
        }
    }

    uc = new UCmd("**DUMMY**",nop);
    fprintf(stderr, "parsing begin\n");
    Check(t_lbrace);
    GetParameter(*uc);
    Expect(t_rbrace);
    ue = new UEnv(name,nop,opt,uc);
    SkipSpaces();
    fprintf(stderr, "parsing end\n");
    Expect(t_lbrace);
    GetParameter(*ue->end);
    Expect(t_rbrace);
    SkipSpaces();
}


IRNode* UserEnvironment(void) {
    IRNode* n = new IRNode();
    UEnv *ue = UEnv::find(plain);
    char* ename = new char[strlen(plain)+1];
    char* term;
    StringList list;
    int i;

    strcpy(ename,plain);

    Expect(t_rbrace);

    if (ue) {
        printf("UE found\n");
        for (i=1 ; i<=ue->nop; i++) {
            SkipSpaces();
            Expect(t_lbrace);
            GetParameter(ue->begin->param[i]);
            Expect(t_rbrace);
        }
        term = new char[strlen(ename)+strlen("\\end{}")+1];
        sprintf(term, "\\end{%s}", ename);
        Prefetch(list, term);
        list.last->next = ue->end->first;
        list.last = ue->end->last;
        Source(&list);
        Source(ue->begin);
        GetToken();
        n->adopt(Scope(IsEnd));
        Expect(t_lbrace);
        Expect(t_plain);
        if (strcmp(ename,plain))
            FatalError("Bad environment nesting");SkipSpaces();
        Expect(t_rbrace);
    }
    else {
        GetToken();
        n->adopt(Scope(IsEnd));
        Expect(t_lbrace);
        Expect(t_plain);
        if (strcmp(ename,plain))
            FatalError("Bad environment nesting");
        Expect(t_rbrace);
        SkipSpaces();
    }
    GetToken();
    delete [] ename;
    return n;
}


void NewIf(void) {
    GetToken();
    if (token != t_unknowncw)
        Error("Unknown \\if expected");
    AddIf(cw);
}

int iflevel = 0;

void If(void) {
    iflevel++;
    if (!ifvalue) SkipFalse(1);
}

void Else(void) {
    if (!iflevel) Error("Unexpected \\else");
    SkipFalse(0);
}

void Fi(void) {
    iflevel--;
}

IRGraphics* IncludeGraphics() {
    int i;
    IRNode *n = new IRNode;
    IRGraphics *g;
    char optparam[1024] = "";
    brackets++;
    GetToken();
    if (token == t_lbracket) {
        GetToken();
        if (token == t_plain) {
            strncpy(optparam,plain,1024);
            GetToken();
        }
        Check(t_rbracket);
        GetToken();
    }
    brackets--;
    Check(t_lbrace);
    GetToken();
    n->adopt(Scope(IsRBrace));
    plain[0] = 0;
    n->gentext(&AppendToPlain);
    for (i = strlen(plain); i>=0; i--) {
        if (plain[i]=='.') {
            plain[i]=0;
            break;
        }
        if ((plain[i]=='\\') || (plain[i]=='/')) {
            break;
        }
    }
    g = new IRGraphics(plain,optparam);
    GetToken();
    return g;
}

void Ignore(int opt, int req) {
    int i;
    StringList dummy;
    brackets++;
    GetToken();
    for (i=1; i <= opt; i++) {
        if (token != t_lbracket) break;
        GetToken();
        Scope(IsRBracket);
        GetToken();
    }
    brackets--;
    for (i=1; i <= req; i++) {
        Check(t_lbrace);
        GetParameter(dummy);
        Expect(t_rbrace);
        GetToken();
    }
}

//
// Main parse function
//

IRNode* Parse(void) {
    IRPlain *p;
    IRCommand *c;

    switch (token) {
    case t_eof:
        FatalError("Unexpected end of file");
    case t_rbrace:
        FatalError("Unexpected end of group");
    case t_macropar:
        FatalError("Unexpected macro parameter");
    case t_tab:
        FatalError("Unexpected cell delimiter");
    case t_tie:
        GetToken();
        return new IRCommand(t_tie);
    case t_wordspace:
    case t_negthinspace:
    case t_thinspace:
    case t_italicsspace:
    case t_mediumspace:
    case t_thickspace:
    case t_eosspace:
        p = new IRPlain(" ");
        GetToken();
        return p;
/*
    case t_accent:
    case t_beginformula:
    case t_endformula:
    case t_tabp:
    case t_hyphenation:
    case t_tabl:
    case t_tabe:
    case t_tabg:
    case t_begindispformula:
    case t_enddispformula:
    case t_backaccent:
*/
    case t_input:
        Input();
        break;
    case t_include:
        Include();
        break;
    case t_plain:
        p = new IRPlain(plain);
        GetToken();
        return p;
    case t_par:
        GetToken();
        return new IRCommand(t_par);
    case t_lbrace:
        return Group();
    case t_math:
        return Math(token);
    case t_superscript:
    case t_subscript:
        if (curmode != m_math) Error("\"^\" or \"_\" in non-math mode");
        break;
    case t_begin:
        return Environment();
    case t_end:
        FatalError("Unexpected end of environment");
    case t_item:
        return Item();
    case t_title:
        Title(); break;
    case t_part:
    case t_chapter:
    case t_section:
    case t_subsection:
    case t_subsubsection:
    case t_paragraph:
    case t_subparagraph:
        return Heading(token-t_part);
    case t_appendix:
        break;
    case t_label:
        Label(); break;
    case t_public:
        return Public(); break;
    case t_extern:
        Extern(); break;
    case t_ref:
        return Ref();
    case t_index:
        Index(); return 0;
    case t_caption:
        Caption(); break;
    case t_verb:
        return Verb();
    case t_newline:
        SkipSpaces();
        GetToken();
        return new IRCommand(t_newline);
    case t_mbox:
        break;                     // mbox == group !!!
    case t_footnote:
        return Footnote();
    case t_url:
        return HRef(0);
    case t_href:
        return HRef(1);
    case t_rm:
    case t_bf:
    case t_sf:
    case t_it:
    case t_sl:
    case t_sc:
    case t_tt:
        IRNode::curface = token; break;
    case t_mdseries:
        IRNode::curface = t_rm; break;
    case t_bfseries:
        IRNode::curface = t_bf; break;
    case t_em:
        switch(IRNode::curface) {
        case t_rm: IRNode::curface = t_it; break;
        case t_it: IRNode::curface = t_rm; break;
        default:   Warning("\\em in neither \\rm nor \\it text"); break;
        }
        break;
    case t_tiny:
    case t_scriptsize:
    case t_footnotesize:
    case t_small:
    case t_normalsize:
    case t_large:
    case t_Large:
    case t_LARGE:
    case t_huge:
    case t_Huge:
        IRNode::curface = t_rm;
        IRNode::cursize = token;
        break;
    case t_newcommand:
    case t_renewcommand:
        NewCommand(); break;
    case t_usercommand:
        UserCommand(); break;
    case t_newenvironment:
    case t_renewenvironment:
        NewEnvironment(); break;
    case t_newif:
        NewIf(); break;
    case t_if:
        If(); break;
    case t_else:
        Else(); break;
    case t_fi:
        Fi(); break;
    case t_true:
    case t_false:
        break;
    case t_includegraphics:
        return IncludeGraphics();
    case t_nominitoc:
        IRHeading::nominitoc(); break;
    case t_ignorable:
        Ignore(ignorable->opt,ignorable->req); return 0;
    case t_unknowncw:
        Warning("Unknown control word: '\\%s'",cw);
        break;
    default:
        if (token >= firstspecsymbol) {
            c = new IRCommand(token);
            GetToken();
            return c;
        }
        else {
            char buf[256];
            Warning("token '%s' ignored",VisToken(token,buf)); break;
        }
    }
    GetToken();
    return 0;
}

typedef int (*IsTerm)(int);

IRNode* Scope(IsTerm isterm) {
    IRNode* first = 0;
    IRNode* last;
    int face = IRNode::curface;
    int size = IRNode::cursize;

    while (!isterm(token))
        if((first = Parse()) != 0) break;
    last = first;
    while (!isterm(token)) {
        last = last->append(Parse());
    }
    IRNode::curface = face;
    IRNode::cursize = size;
    return first;
}

IRRoot* BuildIR(char *name, int multipart, int javahelp, int htmlhelp, char* stylesheet) {
    IRRoot *root = new IRRoot(name, multipart, javahelp, htmlhelp, stylesheet);
    root->adopt(Scope(IsEOF));
    return root;
}
