#ifndef __linux__
    // unused, and not exists
    #include <direct.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/stat.h>

#include "basedefs.h"
#include "scanner.h"

#include "tables.cpp"

// Static class members

UCmd *UCmd::list = 0;
UEnv *UEnv::list = 0;
ICmd *ICmd::list = 0;

//
// Function prototypes
//

static void PushState();
static void PopState(void);
int FindCW(const char text[]);
int FindCS(const char symbol);
int FindEN(const char text[]);
int FindIf(const char text[]);
char GetChar(void);
void Warning(const char msg[]);
void Warning(const char msg[], const int n);
void Warning(const char msg[], const char s[]);
void Error(const char msg[]);
void Error(const char msg[], const int n);
void Error(const char msg[], const char s[]);
void FatalError(const char msg[]);


/* Current input stream state  */

char curname[260];              // Current file name
FILE *curfile = 0;              // Current file handle
UCmd *curcmd = 0;               // Current command
int  curmpl = 0;                // Current macro parameter level
StringList *curlist = 0;        // Current string list
int  curline;                   // Current line number
char buffer[BUFLEN], *curpos;   // Current line buffer and position

int  progress = 0;

void Progress() {
    if (progress && !curlist) {
        fprintf(stderr,"%5d %-70.70s\r",curline,curname);
        fflush(stderr);
    }
}

struct State {
    char curname[260];
    FILE *curfile;
    UCmd *curcmd;
    int  curmpl;
    StringList *curlist;
    int  curline;
    char buffer [BUFLEN];
    int  curpos;
    State *prev;
};

State *StateList = NULL;

static void PushState() {
    State *s = new State;
    strcpy(s->curname,curname);
    s->curfile = curfile;
    s->curcmd  = curcmd;
    s->curmpl  = curmpl;
    s->curlist = curlist;
    s->curline = curline;
    strcpy(s->buffer,buffer);
    s->curpos  = curpos-buffer;
    s->prev = StateList;
    StateList = s;
}

static void PopState(void) {
    State *s = StateList;
    strcpy(curname,s->curname);
    curfile = s->curfile;
    curcmd  = s->curcmd;
    curmpl  = s->curmpl;
    curlist = s->curlist;
    curline = s->curline;
    strcpy(buffer, s->buffer);
    curpos = buffer+s->curpos;
    StateList = s->prev;
    delete s;
    if (!curlist) Progress();
}

Pos::Pos(void) {
    State *scan;
    if (curfile) {
        if (curlist == 0) {
            strcpy(this->name = new char[strlen(curname)+1], curname);
            this->line = curline;
            this->pos  = curpos-buffer;
        }
        else {
            for(scan = StateList; scan->curlist; scan = scan->prev);
            strcpy(this->name = new char[strlen(scan->curname)+1], curname);
            this->line = scan->curline;
            this->pos  = scan->curpos;
        }
    }
    else {
    this->name = 0;
    this->line = 0;
    this->pos  = 0;
    }
}

static UCmd* DrawCommand(void) {
    int l;
    UCmd* c  = curcmd;
    State* s = StateList;
    for (l=0;l<curmpl;) {
        if (!s)
            Error("Macro parameter outside macro definition");
        s = s->prev;
        if (s->curcmd != c) {c = s->curcmd; l++;}
    }
    return c;
}

int  token;
char plain[BUFLEN];
char cw[256];
UCmd *command;
int  asterisk;
ICmd *ignorable;
int  ifvalue;

struct {
    char name[32];
    int  value;
} IfTable [256];

int IfCount = 0;

void AddIf(const char text[]) {
    if ((text[0] != 'i') ||
        (text[1] != 'f') ||
        (strlen(cw)<=2)) Error("\\if??? expected");
    strcpy(IfTable[IfCount].name,text);
    IfTable[IfCount].value = 0;
    IfCount++;
    strcpy(IfTable[IfCount].name,text+2);
    strcat(IfTable[IfCount].name,"true");
    IfTable[IfCount].value = 1;
    IfCount++;
    strcpy(IfTable[IfCount].name,text+2);
    strcat(IfTable[IfCount].name,"false");
    IfTable[IfCount].value = 0;
    IfCount++;
    if (strcmp(text,"ifonline") == 0) {
        IfTable[IfCount-3].value = 1;
        IfTable[IfCount-1].value = 1;
    }
}

int skipping = 0;

int FindIf(const char text []) {
    int i;

    for (i = 0; i < IfCount; i++)
        if (strcmp(IfTable[i].name,text) == 0)
            switch (i%3) {
            case 0:
                if (!skipping) ifvalue = IfTable[i].value;
                return t_if;
            case 1:
                if (!skipping) IfTable[i-1].value = IfTable[i].value;
                return t_true;
            case 2:
                if (!skipping) IfTable[i-2].value = IfTable[i].value;
                return t_false;
            }
    return 0;
}

void SkipFalse(int else_allowed) {
    int lev = 0;
    skipping = 1;
    for(;;) {
        while (*curpos != '\\') {
            if (*curpos == '%') *curpos = '\0';
            GetChar();
        }
        GetToken();
        if (token == t_if) ++lev;
        else if ((token == t_fi) && (!lev--)) break;
        else if (token == t_else)
            if (!lev) {
                if (else_allowed) break;
                else Error("Unmatched \\else");
            }
    }
    skipping = 0;
}

int FindCW(const char text[]) {
    int i;
    for (i=0; CWTable[i].token != t_unknowncw; i++)
        if (strcmp(CWTable[i].text,text) == 0) return CWTable[i].token;
    if ((command   = UCmd::find(text)) != 0) return t_usercommand;
    if ((i = FindIf(text)) != 0) return i;
    if ((ignorable = ICmd::find(text)) != 0) return t_ignorable;
    return t_unknowncw;
}

int FindCS(const char symbol) {
    int i;
    char s[2];
    for (i=0; CSTable[i].token != t_unknowncs; i++)
        if (CSTable[i].symbol==symbol) return CSTable[i].token;
    s[0]=symbol; s[1]='\0';
    Warning("Unknown control symbol: %s",s);
    return t_unknowncs;
}

char* VisToken(int token, char buf []) {
    int i;
    strcpy(buf,"**BAD**");
    if (token <= t_plain)
        switch (token) {
        case t_unknowncw: strcpy(buf, "**unknowncw**"); break;
        case t_unknowncs: strcpy(buf, "**unknowncs**"); break;
        case t_ignorable: strcpy(buf, "**ignorable**"); break;
        case t_eof:       strcpy(buf,"**EOF**"); break;
        case t_plain:     strcpy(buf,"**plain text**"); break;
        }
    else if (token < t_firstcs) {
        buf[1] = '\0';
        switch (token) {
        case t_lbrace:      buf[0] = '{'; break;
        case t_rbrace:      buf[0] = '}'; break;
        case t_macropar:    buf[0] = '#'; break;
        case t_math:        buf[0] = '$'; break;
        case t_superscript: buf[0] = '^'; break;
        case t_subscript:   buf[0] = '_'; break;
        case t_tab:         buf[0] = '&'; break;
        case t_tie:         buf[0] = '~'; break;
        case t_lbracket:    buf[0] = '['; break;
        case t_rbracket:    buf[0] = ']'; break;
        }
    }
    else if (token < t_firstcw) {
        for (i=0; CSTable[i].token != t_unknowncs; i++) {
            if (CSTable[i].token == token) {
                buf[0] = '\\';
                buf[1] = CSTable[i].symbol;
                buf[2] = '\0';
                break;
            }
        }
    }
    else
        for (i=0; CWTable[i].token != t_unknowncw; i++)
            if (CWTable[i].token == token) {
                buf[0] = '\\';
                strcpy(buf+1, CWTable[i].text);
                break;
            }
    return &buf[0];
}

static int Command(void) {
    int i;
    int t;

    for (i = 0; isalpha((unsigned char)*curpos); i++) {
        cw[i] = *curpos; GetChar();
    }
    if (i) {
        if ((asterisk = (*curpos == '*')) != 0) GetChar();
        while (isspace((unsigned char)*curpos)) GetChar();
        cw[i] = '\0';
        return FindCW(cw);
    }
    else
        if (strchr("%{}#$^_&~",*curpos)) {
            plain[0] = *curpos; plain[1]='\0';
            GetChar();
            return t_plain;
        }
        else {
            t = FindCS(*curpos);
            GetChar();
            return t;
        }
}

void Source(const char name[]) {
    FILE *f = fopen(name,"r");
    if (!f) FatalError("Can not open source file");
    PushState();
    strcpy(curname,name);
    curfile = f;
    curline = 0;
    buffer[0] = '\0';
    curpos = &buffer[0];
    GetChar(); //!!!
}

void Source(UCmd *command) {
    PushState();
    curlist = curcmd = command;
    curlist->reset();
    curline = 0;
    buffer[0] = '\0';
    curpos = &buffer[0];
    GetChar(); // !!!
}

void Source(StringList *list) {
    PushState();
    curlist = list;
    curlist->reset();
    curline = 0;
    buffer[0] = '\0';
    curpos = &buffer[0];
    GetChar(); // !!!
}

static void MacroPar(void) {
    int n = *curpos-'0';
    UCmd* c;
    if ((n < 1) || (n > 9))
        Error("Digit expected");
    c = DrawCommand();
    if (!c)
        Error("Macro parameter outside macro definition");
    if (n > c->nop)
        Error("Wrong macro parameter number");
    GetChar();
    PushState();
    curmpl++;
    curlist = &(c->param[n]);
    curlist->reset();
    curline = 0;
    buffer[0] = '\0';
    curpos = &buffer[0];
    GetChar();   // !!!
}

int GetLine() {
    if (curlist) {
        if (!curlist->getline(buffer)) {
            PopState(); return 0;
        }
    }
    if (!curlist) {
        if (!curfile) {
            buffer[0] = '\0';
            curpos = &buffer[0];
            return 0;
        }
        while (fgets(buffer,BUFLEN,curfile) == 0) {
            if (feof(curfile)) {
                fclose(curfile);
                curfile = 0;
                if (StateList) PopState();
                else {buffer[0] = '\0'; curpos = &buffer[0];}
                return 0;
            }
            else FatalError("Error reading file");
        }
    }
    curpos = &buffer[0];
    if (!(curline % 256)) Progress();
    curline++;
    return 1;
}

char GetChar() {
    char *eol;

    if (*curpos) {curpos++; return *curpos;}

    while (!*curpos) {
        if (!GetLine()) {if (curfile) continue; else return '\0';}
        for (;;) {
            if (*curpos == '\n') break;           // Line is empty
            if (*curpos == '%') {                 // Line is pure comment
                if (!GetLine()) {if (curfile) break; else return '\0';}
                continue;
            }
            if (!isspace((unsigned char)*curpos)) {              // Line is not empty
                if ((eol=strchr(curpos,'\n')) != 0) *eol=' ';
                break;
            }
            ++curpos;
        }
    }
    return *curpos;
}

static void CheckChar(void) {
    while (!*curpos) { GetChar(); if (!curfile) return;}
}

void SkipSpaces(void) {
    CheckChar();
    while (*curpos == ' ') GetChar();
}

int brackets = 0;

void GetToken() {
    int plain_l;
    int wasspace;
    int done;

    do {
        CheckChar();
        done = 1;
        if (*curpos == '%') { *curpos = '\0'; GetChar();}
        switch (*curpos) {
        case '\0': token = t_eof; return;
        case '%':  Error("Unexpected comment"); return;
        case '\\': GetChar(); token = Command(); return;
        case '#':  GetChar(); MacroPar(); done = 0; break;
        case '\n': GetChar(); token = t_par; return;
        case '{':  GetChar(); token = t_lbrace; return;
        case '}':  GetChar(); token = t_rbrace; return;
        case '$':  GetChar(); token = t_math; return;
        case '^':  GetChar(); token = t_superscript; return;
        case '_':  GetChar(); token = t_subscript; return;
        case '&':  GetChar(); token = t_tab; return;
        case '~':  GetChar(); token = t_tie; return;
        case '[':
            if (brackets) {
                GetChar(); token = t_lbracket; return;
            }
            break;
        case ']':
            if (brackets) {
                GetChar(); token = t_rbracket; return;
            }
            break;
        case '-':
            if      (GetChar() != '-') token = t_hyphen;
            else if (GetChar() != '-') token = t_endash;
            else {   GetChar();        token = t_emdash; }
            return;
        case '`':
            if      (GetChar() != '`') token = t_lsquo;
            else {   GetChar();        token = t_ldquo; }
            return;
        case '\'':
            if      (GetChar() != '\'') token = t_rsquo;
            else {   GetChar();         token = t_rdquo; }
            return;
        default: break;
        }
    } while (!done);

    plain_l = 0; wasspace = 0;
    for (;;) {
        if (plain_l >= BUFLEN) Error("Plain text too long");
        CheckChar();
        plain[plain_l++] = *curpos;
        if (*curpos == ' ')
            do ; while (GetChar() == ' ');
        else
            GetChar();
        CheckChar();
        if (*curpos == '\\') {  // !!!
            break;
        }
        else {
            if (brackets)
                if (strchr("\n%{}#$^_&~-`'[]",*curpos)) break; else;
            else
                if (strchr("\n\\%{}#$^_&~-`'",*curpos)) break;
        }
    }
    plain[plain_l] = '\0';
    token = t_plain;
}

void GetString(char term) {
    char *end;

    if ((end = strchr(curpos,term)) != 0) {
        strncpy(plain,curpos,end-curpos);
        plain[end-curpos] = '\0';
        curpos = end;
    }
    else Error("String expected");
}

void GetVerb(void) {
    char *end;

    if ((end = strchr(curpos+1,*curpos)) != 0) {
        strncpy(plain,curpos+1,end-curpos-1);
        plain[end-curpos-1] = '\0';
        curpos = end+1;
    }
    else Error("\\verb terminator expected");
}

void GetParameter(StringList &list) {
    int l = 0;          // braces level
    char *scan;

    list.clear();

    for(;;) {
        for (scan = curpos; *scan; scan++) {
            switch(*scan) {
            case '\\': scan++; break;
            case '{':  l++;    break;
            case '}':
                if (!l--) {
                    list.append(curpos,scan-curpos);
                    curpos = scan;
// !!!                    GetChar();
                    return;
                }
                break;
            }
        }
        list.append(curpos,scan-curpos);
        if (!GetLine()) FatalError("Unexpected end of file");
    }
}

void GetVerbatim(StringList &list) {
    const char term[] = "\\end{verbatim}";
    char *scan;

    // If there is no nonspace characters on the line following the
    // \begin{verbatim} command, then literal text effectively begins
    // on the next line.

    for (scan = curpos; *scan; scan++)
        if (!isspace((unsigned char)*scan)) break;
    if (!*scan) GetLine();

    for(;;) {
        if ((scan = strstr(curpos,term)) != 0) {
            if (scan-curpos > 0)
                list.append(curpos,scan-curpos);
            curpos = scan+strlen(term);
            return;
        }
        list.append(curpos,strlen(curpos));
        if (!GetLine()) FatalError("Unexpected end of file");
    }
}

void Prefetch(StringList &list, const char term[]) {
    char *scan;
    for(;;) {
        if ((scan = strstr(curpos,term)) != 0) {
            if (scan-curpos > 0)
                list.append(curpos,scan-curpos);
            curpos = scan;
            return;
        }
        list.append(curpos,strlen(curpos));
        if (!GetLine()) FatalError("Unexpected end of file");
    }
}


void InitScan(const char name[]) {
    struct stat st;
    if (!fstat(fileno(stderr), &st))
        progress = st.st_mode & S_IFCHR;
    if ((curfile = fopen(name,"r")) == NULL)
        FatalError("InitScan: Can not open input file");
    strcpy(curname,name);
    curline = 0;
    buffer[0] = '\0';
    curpos = &buffer[0];
    GetChar();   // !!!
}

//
// Error messages and warnings
//

static void PrintBuf(void) {
    fprintf(stderr,"\n%s",buffer);
}

static void PrintPos(int err = 0) {
    if (curfile) {
        printf("\n(%s %d:%d) ",curname,curline,curpos-buffer);
        if (err) fprintf(stderr,"\n(%s %d:%d) ",curname,curline,curpos-buffer);
    }
    else {
        printf("\n(************ *:*) ");
        if (err) fprintf(stderr,"\n(************ *:*) ");
    }
}

static void PrintPos(Pos* pos, int err = 0) {
    if (pos->name) {
        printf("\n(%s %d:%d) ",pos->name,pos->line,pos->pos);
        if (err) fprintf(stderr,"\n(%s %d:%d) ",pos->name,pos->line,pos->pos);
    }
    else {
        printf("\n(************ *:*) ");
        if (err) fprintf(stderr,"\n(************ *:*) ");
    }
}

void Warning(const char msg[]) {
    PrintPos(); printf(msg);
}

void Warning(const char msg[], const int n) {
    PrintPos(); printf(msg,n);
}

void Warning(const char msg[], const char s[]) {
    PrintPos(); printf(msg,s);
}

void WarningAt(Pos *pos, const char msg[], const char s[]) {
    PrintPos(pos); printf(msg,s);
}

void Error(const char msg[]) {
    fflush(stdout);
    PrintBuf();
    PrintPos(1); fprintf(stderr,msg);
    exit(1);
}

void Error(const char msg[], const int n) {
    fflush(stdout);
    PrintBuf();
    PrintPos(1); fprintf(stderr,msg,n);
    exit(1);
}

void Error(const char msg[], const char s[]) {
    fflush(stdout);
    PrintBuf();
    PrintPos(1); fprintf(stderr,msg,s);
    exit(1);
}

void FatalError(const char msg[]) {
    fflush(stdout);
    fprintf(stderr,"\n(%s %d:%d) FATAL: %s",curname,curline,curpos-buffer-1,msg);
    exit(1);
}


