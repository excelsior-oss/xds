/*
 * ir.h - LaTeX document internal representation
 *
 * Note: basedefs.h, scanner.h and string.h has to be #included
 *       *before* this file!
 */

class IRNode;

typedef void (*Write) (const char[]);  // Used by 'gentext' virtual member function
typedef void (*Walk) (IRNode*);        // Used by 'walktree' member function

class IRNode {
public:
    static char *name;              // Output file name (w/o path and ext)
    static int multipart;
    static int javahelp;        // Patch against JavaHelp bugs
    static int htmlhelp;
    static char *stylesheet;
    static int curpart;
    static int curface;
    static int cursize;
    IRNode *parent;             // Links in the tree
    IRNode *prev;
    IRNode *next;
    IRNode *son;
    int part;                   // Part number
    int face;
    int size;
    IRNode (void) {
        parent = prev = next = son = 0;
        part = curpart; face = curface; size = cursize;
    }
    static void setname(char *name) {
        strcpy(IRNode::name = new char[strlen(name)+1],name);
    }
    void forcefont (void) {
        face = curface; size = cursize;
    }
    IRNode* append(IRNode *n) {
    // If 'n' != 0, appends 'n' to the list 'this' terminates.
    // then Returns 'n'. Otherwise returns 'this'.
        if (n) { this->next = n; n->prev = this; return n; }
        else return this;
    }
    IRNode* adopt(IRNode *n) {
    // Adopts list 'n' starts to 'this'. Returns 'this'.
        IRNode *scan;
    //        if (this->son) abort();
        this->son = n;
        for (scan = n; scan; scan = scan->next) scan->parent = this;
        return this;
    }
    void walktree(int fromlevel, int tolevel, Walk walk) {
    // Walks subtree starting from 'this'. 'walk' is called for
    // nodes which level is in range [fromlevel,tolevel].
    // 'this' is considered level 0.
    // Examples:
    //   n->walktree(1,1,w) walks direct descendants of 'n' only
    //   n->walktree(0,1000,w) walks 'n' with all descendants
        IRNode *scan;
        if (fromlevel <= 0 ) walk(this);
        if (tolevel > 0)
            for (scan = this->son; scan; scan = scan->next)
                scan->walktree(fromlevel-1, tolevel-1, walk);
    }
    virtual void gentext(Write w);
    virtual void genIPF(void);
    virtual void genRTF(void);
    virtual void genHTML(void);
};

class IRRoot: public IRNode {
    void genFrameset(int php);
public:
    static IRNode title;                        // Title subtree
    IRRoot(char *name, int multipart, int javahelp, int htmlhelp, char* stylesheet) {
        IRNode::setname(name);
        IRNode::multipart = multipart;
        IRNode::javahelp = javahelp;        // Patch against JavaHelp bugs
        IRNode::htmlhelp = htmlhelp;
        IRNode::stylesheet = stylesheet;
    }
    void genIPF(void);
    void genRTF(void);
    void genCNT(int level);                     // Starting level
    void genHTML(void);
    void genHTML(int multipart, int frames, int plain, int javahelp, int htmlhelp, int eclipse);
    void genEXT(void);
    void genXML(void);
};

class IRPlain : public IRNode {
public:
    char *text;
    int  verb;
    IRPlain(const char text[], int verb = 0) {
        strcpy(this->text = new char[strlen(text)+1],text);
        this->verb = verb;
    }
    void gentext(Write w);
    void genIPF(void);
    void genRTF(void);
    void genHTML(void);
};

class IRCommand : public IRNode {
    int code;
public:
    IRCommand(int code) {
        this->code = code;
        if (javahelp && (code == t_par)) {
            size = t_normalsize;
            face = t_rm;
        }
    }
    void gentext(Write w);
    void genIPF(void);
    void genRTF(void);
    void genHTML(void);
};

class IRGroup : public IRNode {
};

class IRFootnote : public IRNode {
public:
    void genIPF(void);
    void genRTF(void);
    void genHTML(void);
};

class IRHRef : public IRNode {
public:
    char *url;
    IRHRef (char *url) {
        strcpy(this->url = new char[strlen(url)+1],url);
    }
    void genIPF(void);
    void genRTF(void);
    void genHTML(void);
};

class IRVerbatim : public IRNode {
    StringList list;
public:
    IRVerbatim(StringList &list) { this->list = list; }
    void gentext(Write w);
    void genIPF(void);
    void genRTF(void);
    void genHTML(void);
};

class IREnv : public IRNode {
public:
    int code;
    IREnv(int code) {
        this->code = code;
        if (javahelp) {
            size = t_normalsize;
            face = t_rm;
        }
    }
    void genIPF(void);
    void genRTF(void);
    void genHTML(void);
};

class IRFloat : public IREnv {
public:
    static IRFloat *first, *last;
    IRFloat *next;
    int     number;
    IRNode  caption;
    char    type;
    StringList lablist;          // list of cross-reference labels
    StringList publist;          // list of public labels
    StringList indlist;          // list of index entries

    IRFloat(int envid, char type) : IREnv(envid) {
         this->next   = 0;
         this->number = 0;
         this->type   = type;
         if (last) {
             last->next   = this;
             last = this;
         }
         else {
             first = last = this;
         }
    }
    // Add xref label
    static void addlabel(char label[]) {
        last->lablist.append(label,strlen(label));
    }
    // Add public label
    static void addpublic(char label[]) {
        last->publist.append(label,strlen(label));
    }
    // Add index entry
    static void addindex(char key[]) {
        last->indlist.append(key,strlen(key));
    }
    // Find heading with specified label
    static IRFloat* find(char label[]) {
        IRFloat *scan;
        for(scan = IRFloat::first; scan; scan = scan->next) {
            if (scan->lablist.find(label)) return scan;
            if (scan->publist.find(label)) return scan;
        }
        return 0;
    }
};

class IRTable : public IRFloat {
public:
    static int lastnumber;
    IRTable(void) : IRFloat(e_table,'T') {
        this->number = ++lastnumber;
    }
    void genIPF(void);
    void genRTF(void);
    void genHTML(void);
};

class IRFigure : public IRFloat {
public:
    static int lastnumber;
    IRFigure(void) : IRFloat(e_figure,'F') {
        this->number = ++lastnumber;
    }
    void genIPF(void);
    void genRTF(void);
    void genHTML(void);
};

class IRPopUp : public IRFloat {
public:
    static int lastnumber;
    IRPopUp(void) : IRFloat(e_popup,'P') {
        this->number = ++lastnumber;
    }
    void genIPF(void);
    void genRTF(void);
    void genHTML(void);
};

struct IRCol {
    char align;
    int rborder;
    IRCol *next;    
    IRCol(void) {
        this->align   = 0;
        this->rborder = 0;
        this->next    = 0;
    }
};

struct IRRowSep {
    int border;
    IRRowSep *next;
    IRRowSep(void) {
        this->border = 0;
        this->next   = 0;
    }
};

class IRCell : public IRNode {
    int span;
    char align;
    int lb;
    int rb;
public:
    IRCell(void) {
        this->span = 1;
        this->align = 0;
        this->lb = -1;
        this->rb = -1;
    }
    IRCell(int span, char align, int lb, int rb) {
        this->span = span;
        this->align = align;
        this->lb = lb;
        this->rb = rb;
    }
/*    void gentext(Write w);
    void genIPF(void);
    void genRTF(void);
*/
    void genHTML(char align, int tb, int rb, int bb, int lb);
};

class IRTabular : public IREnv {
public:
    IRCol *cols;
    IRRowSep *rowseps;
    IRTabular(int code) : IREnv(code) {cols = 0;}
    void gentext(Write w);
    void genIPF(void);
    void genRTF(void);
    void genHTML(void);
};


class IRItem : public IRNode {
public:
    void gentext(Write w);
    void genIPF(void);
    void genRTF(void);
    void genHTML(void);
};

class IRHeading : public IRNode {
    static int lastid;
    static IRHeading *first, *last;
    static int toplevel;
public:
    int id;
    int level;                   // Nesting level (1-based)
    int toc;                     // 0/1 - not/generate TOC entry
    int minitoc;                 // 0/1 - not/generate subtopic list
    IRHeading *prev, *next;      // links in headings list
    StringList lablist;          // list of cross-reference labels
    StringList publist;          // list of public labels
    StringList indlist;          // list of index entries
    IRHeading(int level, int toc) {
        this->id    = this->lastid++;
        this->prev  = this->last;
        this->next  = 0;
        if (this->last) this->last->next = this;
        else {this->first = this; toplevel = level;}
        this->last  = this;
        this->level = level-toplevel+1;
        if (this->level <= multipart) {curpart++; part++;}
        this->toc     = toc;
        this->minitoc = 1;
    }
    // Turn off minitoc flag
    static void nominitoc(void) {
        last->minitoc = 0;
    }
    // Add xref label
    static void addlabel(char label[]) {
        last->lablist.append(label,strlen(label));
    }
    // Add public label
    static void addpublic(char label[]) {
        last->publist.append(label,strlen(label));
    }
    // Add index entry
    static void addindex(char primary[], char secondary[]) {
        last->indlist.append(primary,strlen(primary));
        last->indlist.append(secondary,strlen(secondary));
    }
    // Find heading with specified label
    static IRHeading* find(char label[]) {
        IRHeading *scan;
        for(scan = IRHeading::last; scan; scan = scan->prev) {
            if (scan->lablist.find(label)) return scan;
            if (scan->publist.find(label)) return scan;
        }
        return 0;
    }
    static IRHeading* getfirst(void) {
        return first;
    }
    // Find outer heading with specified level
    IRHeading* super(int level) {
        IRHeading *scan;
        if (level < this->level)
            for(scan = this->prev; scan; scan = scan->prev)
                if (scan->level == level) return scan;
        return 0;
    }
    void genIPF(void);
    void genRTF(void);
    void genHTML(void);
};

class IRPublic : public IRNode {
public:
    char *label;
    IRPublic(char label[]) {
        this->label = new char[strlen(label)+1];
        strcpy(this->label,label);
    }
//    void genIPF(void);
//    void genRTF(void);
//    void genHTML(void);
};

class IRRef : public IRNode {
public:
    char *label;
    Pos* pos;
    IRRef(void) {
        this->label = 0;
        this->pos = new Pos;
    }
    void setlabel(char label[]) {
        this->label = new char[strlen(label)+1];
        strcpy(this->label,label);
    }
    void gentext(Write w);
    void genIPF(void);
    void genRTF(void);
    void genHTML(void);
};

class IRExtern {
    static IRExtern *last;
public:
    IRExtern *prev;
    char *file;
    char *label;
    int  id;
    int part;
    IRExtern(char file[], char label[], int id) {
        strcpy(this->file  = new char[strlen(file)+1],file);
        strcpy(this->label = new char[strlen(label)+1],label);
        this->id = id;
        this->part = -1;
        this->prev = this->last;
        this->last = this;
    }
    IRExtern(char file[], char label[], int id, int part) {
        strcpy(this->file  = new char[strlen(file)+1],file);
        strcpy(this->label = new char[strlen(label)+1],label);
        this->id = id;
        this->part = part;
        this->prev = this->last;
        this->last = this;
    }
    static IRExtern* find(char label[]) {
        IRExtern *scan;
        for(scan = IRExtern::last; scan; scan = scan->prev)
            if (!strcmp(scan->label,label)) return scan;
        return 0;
    }
};

class IRGraphics : public IRNode {
public:
    char *name;
    char *optparam;
    IRGraphics(char name[], char optparam[]) {
        char *scan;
        int state;
        this->name = new char[strlen(name)+1];
        strcpy(this->name,name);
        this->optparam = new char[strlen(optparam)+2];
        strcpy(this->optparam,optparam);
        state = 1;
        for(scan = this->optparam; *scan; scan++) {
            switch (*scan) {
                case '=':
                    if (state == 1) {
                        *scan = 0;
                        state = 2;
                    }
                    break;
                case ',':
                    if (state == 2) {
                        *scan = 0;
                        state = 1;
                    }
                    break;
            }
        }
        *(++scan) = 0;
    }
    char* param(char name[]) {
        char *scan;
        for (scan=optparam; *scan; scan++) {
           if (!strcmp(name,scan)) return scan+strlen(scan)+1;
           scan = scan+strlen(scan)+1;
           scan = scan+strlen(scan)+1;
        }
        return 0;
    }
    void gentext(Write w);
    void genIPF(void);
    void genRTF(void);
    void genHTML(void);
};

