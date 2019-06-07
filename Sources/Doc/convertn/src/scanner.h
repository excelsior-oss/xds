class Pos {
public:
    char *name;
    int line;
    int pos;
    Pos(void);
};

struct String {
    char *string;
    String *next;
    String(const char *str, size_t len) {
        string = new char[len+1];
        memcpy(string,str,len);
        string[len] = '\0';
        next = 0;
    }
    ~String() {
        delete [] string;
    }
};

struct StringList {
    String *first;
    String *last;
    String *scan;
    StringList() { first = last = scan = 0; }
    void append(String* sp) {
        if (!last) first = last = sp; else last = last->next = sp;
    }
    void append(const char *str, size_t len) {
        String *sp = new String(str,len);
        append(sp);
    }
    void clear (void) {
        String *curr, *next;
        for (curr = first; curr; curr = next) {
            next = curr->next;
            delete curr;
        }
        first = last = scan = 0;
    }
    void reset (void) { scan = first; }
    char* getline (char buffer[]) {
        if (scan) {
            strcpy(buffer,scan->string);
            scan = scan->next;
            return buffer;
        }
        return 0;
    }
    int find(char string[]) {
        for (scan = first; scan; scan = scan->next)
            if (strcmp(scan->string,string) == 0)
                return 1;
        return 0;
    }
};

class UCmd : public StringList {
    static UCmd *list;           // list of all commands
    UCmd *prev;                  // previous command in list
    char name[32];               // command name (without leading '\')
public:
    int nop;                     // number of parameters
    StringList param[10];         // parameters
    UCmd(const char name[], int nop) {
        strcpy(this->name,name);
        this->nop  = nop;
        this->prev = this->list;
        this->list = this;
    }
    static UCmd* find (const char name[]) {
        UCmd *scan;
        for (scan = UCmd::list; scan; scan = scan->prev)
            if (strcmp(name,scan->name) == 0) return scan;
        return 0;
    }
};

// User-defined environment

class UEnv { 
    static UEnv *list;           // list of all environments
    UEnv *prev;                  // previous environment in list
    char name[32];               // environment name 
public:
    int nop;                     // number of parameters
    char *opt;                   // default value of the optional parameter
    StringList param[9];         // parameters
    UCmd       *begin;           // begin
    StringList *end;             // end
    UEnv(const char name[], int nop, char *opt, UCmd *uc) {
        strcpy(this->name,name);
        this->nop   = nop;
        this->opt   = opt;
        this->begin = uc;
        this->end   = new StringList;
        this->prev  = this->list;
        this->list  = this;
    }
    static UEnv* find (const char name[]) {
        UEnv *scan;
        for (scan = UEnv::list; scan; scan = scan->prev)
            if (strcmp(name,scan->name) == 0) return scan;
        return 0;
    }
};

// Ignorable commands

class ICmd : public String {
    static ICmd *list;           // list of all ignorable commands
    ICmd *prev;                  // previous command in list
public:
    int opt;                     // number of optional parameters
    int req;                     // number of required parameters
    ICmd(char name[], size_t len, int opt, int req) : String(name,len) {
        this->opt = opt;
        this->req = req;
        this->prev = this->list;
        this->list = this;
    }
    static ICmd* find (const char name[]) {
        ICmd *scan;
        for (scan = ICmd::list; scan; scan = scan->prev)
            if (strcmp(name,scan->string) == 0) return scan;
        return 0;
    }
};

extern int  token;              // last read token
extern char plain[BUFLEN];      // last read plain text
extern char cw[256];            // last read control word
extern UCmd *command;           // last read user command
extern int  asterisk;           // last read command asterisk mode
extern ICmd *ignorable;         // last read ignorable command
extern int  ifvalue;            // last read \if value

extern int brackets;            // if brackets!=0, GetToken recognizes []

void SkipSpaces(void);
void GetToken(void);
void GetString(char term);
void GetIndex(void);
void GetVerb(void);
void GetParameter(StringList &list);     // change to global?
void GetVerbatim(StringList &list);
void Prefetch(StringList &list, const char term[]);

int  FindEN(const char name[]);
void AddIf (const char text[]);
void SkipFalse(int else_allowed);

void Source(const char name[]);
void Source(UCmd *command);
void Source(StringList *list);

void InitScan(const char name[]);

// Warnings and errors

char* VisToken(int token, char buf []);

void Warning(const char msg[]);
void Warning(const char msg[], const int n);
void Warning(const char msg[], const char s[]);
void WarningAt(Pos *pos, const char msg[], const char s[]);

void Error(const char msg[]);
void Error(const char msg[], const int n);
void Error(const char msg[], const char s[]);
void FatalError(const char msg[]);


