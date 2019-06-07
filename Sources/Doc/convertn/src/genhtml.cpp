#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "basedefs.h"
#include "scanner.h"
#include "ir.h"
#include "generate.h"

int offset = 0;

static int frames     = 0;
static int navigation = HTMLNAV_FULL;
static int javahelp   = 0;
static int htmlhelp   = 0;
static int eclipse    = 0;
static char *catchPut = NULL;
static char *titleText = NULL;

static int pars = 0;   // par bound state:
                       // 0 - not at par boundary
                       // 1 - at end of par
                       // 2 - at beginning of par

static char align = 'l';      // alignment - l,r, or c

static char Align(int env) {
    switch (env) {
    case e_center:      return 'c';
    case e_flushleft:   return 'l';
    case e_flushright:  return 'r';
    default:            return 'l';
    }
}

static void Put(const char text[], int nl = 0) {
    if (catchPut) {
        strcpy(catchPut + strlen(catchPut), text);
        return;
    }
    if (pars == 1) {
        WriteOutput("\n<P");
        switch (align) {
        case 'r' : WriteOutput(" ALIGN=RIGHT>"); break;
        case 'c' : WriteOutput(" ALIGN=CENTER>"); break;
        default  : WriteOutput(">"); break;
        }
        pars = 2;
    }
    if (pars == 2) {
        while (*text == ' ') text++;
        if (!*text) return;
        pars = nl = 0;
    }
    if (nl && offset) { WriteOutput("\n"); offset = 0; }
    WriteOutput(text);
    offset += strlen(text);
}

static void NL(void) { Put("",1); }

static void NLPut(const char text[]) { Put(text,1); }

struct Tag {
    char *name;
    Tag *prev;
    static Tag* last;
    Tag (const char name[]) {
        strcpy(this->name  = new char[strlen(name)+1],name);
        this->prev = this->last;
        this->last = this;
    }
    ~Tag () {
        this->last = this->prev;
        delete[] this->name;
    }
};

Tag* Tag::last = 0;

static void OpenTag (const char name[], const char attr[] = 0) {
    Tag *tag = new Tag(name);
//    printf("<%s>\n",name);
    Put("<"); Put(name); if (attr) {Put(" "); Put(attr);} Put(">");
}

static void CloseTag (const char name[]) {
	while (Tag::last) {
        // printf("</%s>\n",Tag::last->name);
	    Put("</"); Put(Tag::last->name); Put(">");
		if (!strcmp(name, Tag::last->name)) {
			delete Tag::last;
			return;
		} else {
            Warning("Forced close tag ", Tag::last->name);
			delete Tag::last;
		}
	}

//    if (!Tag::last || strcmp(name, Tag::last->name)) {
//        if (Tag::last)
//            Warning("%s is at stack top", Tag::last->name);
//        else
//            Error("stack empty");
//        Error("Unbalanced tag %s:", name);
//        return;
//    }
//    delete Tag::last;
////    printf("--</%s>\n",name);
//    Put("</"); Put(name); Put(">");
}

static int IsTagOpen(const char name[]) {
    return Tag::last ? !strcmp(name, Tag::last->name) : 0;
}

static void Translate(char text[], int verb = 0) {  // should be optimized
    char buf[BUFLEN], *pos;
//    char s[8];
    pos = &buf[0];
    for(;*text;text++) {
        switch (*text) {
        case ' ': if (verb) {
                    strcpy(pos,"&nbsp;"); pos+=6;
                  }
                  else
                    *pos++ = ' ';
                  break;
        case '<': strcpy(pos,"&lt;");   pos += 4; break;
        case '>': strcpy(pos,"&gt;");   pos += 4; break;
        case '&': strcpy(pos,"&amp;");  pos += 5; break;
        case '"': strcpy(pos,"&quot;"); pos += 6; break;
        default : *pos++ = *text; break;
        }
    }
    *pos = '\0';
    Put(buf);
}

static void SwitchFont(int fromface, int toface, int fromsize, int tosize) {
    if (fromface != toface) {
        switch(fromface) {
        case f_rm:   break;
        case f_bf:   CloseTag("B"); break;
        case f_sf:   break;
        case f_it:   CloseTag("I"); break;
        case f_sl:   Put("</I></B>"); break;
        case f_sc:   break;
        case f_tt:   Put("</TT>"); break;
    //!!!    case f_math: break;
    //!!!    case f_tab:  break;
        }
    }
    if (fromsize != tosize) {
        if (Tag::last && !strcmp("FONT", Tag::last->name)) {
            CloseTag("FONT");
        }
        switch (tosize) {
        case t_tiny:         OpenTag("FONT", "SIZE=1"); break;
        case t_scriptsize:   OpenTag("FONT", "SIZE=1"); break;
        case t_footnotesize: OpenTag("FONT", "SIZE=2"); break;
        case t_small:        OpenTag("FONT", "SIZE=2"); break;
        case t_normalsize:   break;
        case t_large:        OpenTag("FONT", "SIZE=4"); break;
        case t_Large:        OpenTag("FONT", "SIZE=5"); break;
        case t_LARGE:        OpenTag("FONT", "SIZE=6"); break;
        case t_huge:         OpenTag("FONT", "SIZE=7"); break;
        case t_Huge:         OpenTag("FONT", "SIZE=7"); break;
        }
    }
    if (fromface != toface) {
        switch(toface) {
        case f_rm:   break;
        case f_bf:   OpenTag("B"); break;
        case f_sf:   break;
        case f_it:   OpenTag("I"); break;
        case f_sl:   Put("<B><I>"); break;
        case f_sc:   break;
        case f_tt:   Put("<TT>"); break;
    // !!!    case f_math: break;
    // !!!    case f_tab:  break;
        }
    }
}

static void SwitchSize(int from, int to) {
    if (from != t_normalsize)
        CloseTag("FONT");
    switch (to) {
    case t_tiny:         OpenTag("FONT", "SIZE=1"); break;
    case t_scriptsize:   OpenTag("FONT", "SIZE=1"); break;
    case t_footnotesize: OpenTag("FONT", "SIZE=2"); break;
    case t_small:        OpenTag("FONT", "SIZE=2"); break;
    case t_normalsize:   break;
    case t_large:        OpenTag("FONT", "SIZE=4"); break;
    case t_Large:        OpenTag("FONT", "SIZE=5"); break;
    case t_LARGE:        OpenTag("FONT", "SIZE=6"); break;
    case t_huge:         OpenTag("FONT", "SIZE=7"); break;
    case t_Huge:         OpenTag("FONT", "SIZE=7"); break;
    }
}

static void GenLink(char *text, int from, int to) {
    char s[256];
    if (from == to)
        sprintf(s, "<A HREF=\"#%s\"",text);
    else if (!IRNode::multipart)
        if (!frames)
            sprintf(s, "<A HREF=\"#%s\"",text);
        else
            sprintf(s, "<A HREF=\"%sbdy.html#%s\"",IRNode::name,text);
    else
        sprintf(s, "<A HREF=\"%s%03.3d.html#%s\"",IRNode::name,to,text);
    Put(s);
    if (frames) Put(" TARGET=body>"); else Put(">");
}

static void GenLink(int id, int from, int to) {
    char s[8];
    sprintf(s, "%04.4d",id);
    GenLink(s,from,to);
}

void IRNode::genHTML(void) {
    IRNode *scan;
    int saveface = curface;
    int savesize = cursize;
    for(scan = this->son; scan; scan = scan->next) {
        SwitchFont(curface,scan->face, cursize,scan->size);
        curface = scan->face;
        cursize = scan->size;
        scan->genHTML();
    }
    SwitchFont(curface,saveface,cursize,savesize);
    curface = saveface;
    cursize = savesize;
}

static void GenContentsHeader(void){
    NLPut("<A NAME=\"contents\"><P ALIGN=CENTER><NOBR>");
    NL(); GenLink("title",-1,0); IRRoot::title.genHTML(); Put("</A>");
    Put("</A>"); Put("</NOBR></P>"); NL();
}


static void GenContentsNoScript(void) {
    IRHeading* scan;
    pars = 0;
    int prevlevel;
    int level;
//    int leveldif;
//    char s[32];
//    int i;

//    GenContentsHeader();

    NLPut("<P>");   
    NLPut("<HR>");
    NLPut("<TABLE BORDER=0 CELLSPACING=3 CELLPADDING=0>");
    if (!eclipse) {
        NLPut("<TR><TD COLSPAN=4><A NAME=\"contents\"><B><FONT SIZE=\"+1\">Table Of Contents</FONT></B></A></TD></TR>");
    }
    NLPut("<TR>");
    NLPut("<TD WIDTH=32><FONT COLOR=\"#FFFFFF\">00</FONT></TD>");
    NLPut("<TD WIDTH=32><FONT COLOR=\"#FFFFFF\">00</FONT></TD>");
    NLPut("<TD WIDTH=32><FONT COLOR=\"#FFFFFF\">00</FONT></TD>");
    NLPut("<TD><FONT COLOR=\"#FFFFFF\">0000000000000000000000</FONT></TD>");
    NLPut("</TR>");
    prevlevel = 1;
    for (scan = IRHeading::getfirst(); scan; scan = scan->next) {
        if (scan->level > 3) continue;
        // This section is hidden?
        if (!scan->toc) {
            // Yes, skip all with greater level
            for (level = scan->level;
                 (scan->next) && (scan->next->level > level);
                  scan = scan->next);
            continue;
        }

        NLPut("<TR>");
        switch (scan->level) {
        case 1:  Put("<TD COLSPAN=4>"); Put("<B>"); break;
        case 2:  Put("<TD COLSPAN=1></TD><TD COLSPAN=3>"); SwitchSize(t_normalsize,t_small); break;
        case 3:  Put("<TD COLSPAN=2></TD><TD COLSPAN=2>"); SwitchSize(t_normalsize,t_small); break;
        default: Put(""); break;
        }
        GenLink(scan->id,-1,scan->part);
        scan->IRNode::genHTML();
        Put("</A>");
        switch (scan->level) {
        case 1:  Put("</B>"); break;
        case 2:  SwitchSize(t_small,t_normalsize); break;
        case 3:  SwitchSize(t_small,t_normalsize); break;
        default: Put(""); break;
        }
        prevlevel = scan->level;
        NLPut("</TD></TR>");
    }
    NLPut("</TABLE>");
    NLPut("<HR>");
}

static void OpenHTML(char *name, char *suffix, IRNode *title, int frameset = 0) {
    OpenOutput(name,suffix);
    if (frameset)
        Put("<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 4.01 Frameset//EN\">"); 
    else
        Put("<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 4.01 Transitional//EN\">"); 
    NL();
    OpenTag("HTML"); NL();
    OpenTag("HEAD"); NL();
    if (eclipse) {
        Put("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=cp866\">");
        NL();
    }

    if (IRNode::stylesheet) {
        Put("<LINK REL=\"stylesheet\" HREF=\"");
        Put(IRNode::stylesheet);
        Put("\" TYPE=\"text/css\">");
        NL();
    }
    OpenTag("TITLE"); title->IRNode::genHTML(); CloseTag("TITLE"); NL();
    CloseTag("HEAD"); NL();
    if (!frameset) {
        if (IRNode::stylesheet) 
            OpenTag("BODY");
        else
            OpenTag("BODY","BGCOLOR=\"#FFFFFF\""); 
        NL();
    }
    if (eclipse && !titleText) {
        // Remember 1st title string to use in eclipse toc:
        catchPut = (char*)malloc(1024);
        catchPut[0] = 0;
        title->IRNode::genHTML();
        titleText = catchPut;
        catchPut = NULL;
    }
}


static char header[256];

static void PutHeaderInContents(const char text[]) {
   char *d = header+strlen(header);
   const char* s;
   for (s=text; *s; s++) {
     if (*s=='"') *d++ = '\\';
     *d++ = *s;
   }
   *d = '\0';
}

static void GenJSContentsEntry(int i, IRHeading* scan) {
    char link[256], s[256], h[256];

    if (!IRNode::multipart)
        sprintf(link, "%sbdy.html#%04.4d",IRNode::name, scan->id);
    else
        sprintf(link, "%s%03.3d.html#%04.4d",IRNode::name,scan->part,scan->id);

    header[0] = '\0';
    scan->IRNode::gentext(&PutHeaderInContents);

    if(scan->level >= 2)
      sprintf(h,"<FONT SIZE=2>%s</FONT>",header);
    else
      sprintf(h,"%s",header);
    if( (scan->next) && (scan->level < scan->next->level) )
    {
       sprintf(s,"db[%d] = new dbRecord(true,\"%s\",\"%s\",%d,\"\")",
        i, h, link, scan -> level - 1);
       NLPut(s);
    }
    else
    {
       sprintf(s,"db[%d] = new dbRecord(false,\"%s\",\"%s\",%d,\"\")",
        i, h, link, scan -> level - 1);
       NLPut(s);

    }
}

static void GenJSContents(char *name, char *suffix) {
    int i;
    int level;
    IRHeading* scan;

    OpenOutput(name,suffix);
    Put("<!--"); NL();

//------------Generate Contents
// Create array object containing outline content and attributes.
// To adapt outline for your use, modify this table.
// Start the array with [1], and continue without gaps to your last item.
// The order of the five parameters:
//    1. Boolean (true or false) whether _next_ item is indented.
//    2. String to display in outline entry (including <FONT> or style tags).
//    3. URL of link for outline entry; Use empty string ("") for no link
//    4. Integer of indentation level (0 is leftmost margin level)
//    5. String for status line during onMouseOver (apostrophes require \\')

    for(i=1, scan = IRHeading::getfirst(); scan; scan=scan->next)
    {
        // This section is hidden?
        if (!scan->toc) {
            // Yes, skip all with greater level
            for (level = scan->level;
                (scan->next) && (scan->next->level > level);
                scan = scan->next);
            continue;
        }
        GenJSContentsEntry(i,scan);
        i++;
    }
    NLPut("// -->");
    CloseOutput();
}


static void OpenScriptContentsHTML(char *name, char *suffix, IRNode *title, int body = 1) {
    OpenOutput(name,suffix);
    Put  ("<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">");
    OpenTag("HTML");
    NLPut("<HEAD>");
    NLPut("<TITLE>"); title->genHTML(); Put("</TITLE>");
    NLPut("<SCRIPT TYPE=\"text/javascript\" SRC=\"head1.js\">");
    NLPut("</SCRIPT>");
    NLPut("<SCRIPT TYPE=\"text/javascript\" SRC=\"");Put(name); Put("cnt.js\">");
    NLPut("</SCRIPT>");
    NLPut("<SCRIPT TYPE=\"text/javascript\" SRC=\"head2.js\">");
    NLPut("</SCRIPT>");
    NLPut("</HEAD>");
    if (body) {
        if (IRNode::stylesheet)
            OpenTag("BODY");
        else
            OpenTag("BODY","BGCOLOR=\"#FFFFFF\"");
        NL();
    }
}


static void GenHtmlHelpTOC(char *name, char *suffix) {
    int level;
    int prevlevel;
    IRHeading* scan;
    char link [256];

    OpenOutput(name,suffix);
    OpenTag("HTML");
    OpenTag("HEAD");
    CloseTag("HEAD");
    OpenTag("BODY");
    prevlevel = 0;
    for (scan = IRHeading::getfirst(); scan; scan=scan->next)
    {
        // This section is hidden?
        if (!scan->toc || scan->level > 3) {
            // Yes, skip all with greater level
            for (level = scan->level;
                (scan->next) && (scan->next->level > level);
                scan = scan->next);
            continue;
        }
        if (scan->level > prevlevel+1) Error("JOPA");
        if (scan->level > prevlevel) {
            OpenTag("UL");
            prevlevel = scan->level;
        }
        else {
            for (; prevlevel > scan->level; prevlevel--)
                CloseTag("UL");
        }
        NLPut("<LI>");
        OpenTag("OBJECT", "type=\"text/sitemap\"");
        NLPut("<param name=\"Name\" value=\"");
        header[0] = 0;
        scan->IRNode::gentext(&PutHeaderInContents);
        Put(header); Put("\">");

        if (!IRNode::multipart)
            sprintf(link, "%s.html#%04.4d",IRNode::name, scan->id);
        else
            sprintf(link, "%s%03.3d.html#%04.4d",IRNode::name,scan->part,scan->id);
        NLPut("<param name=\"Local\" value=\"");
        Put(link); Put("\">");
        CloseTag("OBJECT");
    }
    for (; prevlevel; prevlevel--) CloseTag("UL");
    CloseTag("BODY");
    CloseTag("HTML");
    CloseOutput();
}


static void WriteIdent(int level) {
	for (int ident=0; ident<level; ++ident) {
        WriteOutput("    ");
	}
}

static void UnquoteHeader() {
    // replace all '\"' in header with '&quot;'
    for (int pos = sizeof(header)-7; pos >=0; --pos) {
        if (header[pos] == '\\' && header[pos+1] == '"') {
            memmove(header + pos + 4, header + pos, sizeof(header) - pos - 5);
            strncpy(header + pos, "&quot;", 6);
        }
    }
    header[sizeof(header)-1] = 0;
}


static void GenEclipseTocXml(char *name, char *suffix) {
    int level;
    int prevlevel;
    IRHeading* scan;
    char link [256];

    OpenOutput(name,suffix);
	WriteOutput("<?xml version=\"1.0\" encoding=\"CP866\"?>\n");
    WriteOutput("<?NLS TYPE=\"org.eclipse.help.toc\"?>\n");
	WriteOutput("\n");
    char * label = "Help tpoic";
    if (titleText && titleText[0]) {
        label = titleText;
    }
    sprintf(link, "<toc label=\"%s\" link_to=\"toc.xml#%s\">\n", label, IRNode::name);
    free(titleText);
    titleText = NULL;

	WriteOutput(link);

    prevlevel = 0;
	int closetopic = false;
    for (scan = IRHeading::getfirst(); scan; scan=scan->next)
    {
        // This section is hidden?
        if (!scan->toc || scan->level > 3 || scan->level > prevlevel+1) {
            // Yes, skip all with greater or wrong level
            for (level = scan->level;
                (scan->next) && (scan->next->level > level);
                scan = scan->next);
            continue;
        }

		if (scan->level > prevlevel) {
			if (closetopic) {
                WriteOutput(" >\n");
			}
            prevlevel = scan->level;
        }
        else {
			if (closetopic) {
                WriteOutput(" />\n");
			}
			for (; prevlevel > scan->level; prevlevel--) {
				WriteIdent(prevlevel-1);
                WriteOutput("</topic>\n");
			}
        }

		//    <topic label="Some Topic"  href="dvx005.html#0010"> :
		WriteIdent(scan->level);
        WriteOutput("<topic label=\"");
        header[0] = 0;
        scan->IRNode::gentext(&PutHeaderInContents);
        UnquoteHeader();
        WriteOutput(header); 
		WriteOutput("\"  href=\"");

        if (!IRNode::multipart)
            sprintf(link, "html/%s.html#%04.4d",IRNode::name, scan->id);
        else
            sprintf(link, "html/%s%03.3d.html#%04.4d",IRNode::name,scan->part,scan->id);
		WriteOutput(link);
		WriteOutput("\"");
		closetopic = true;
    }
	if (closetopic) {
        WriteOutput(" />\n");
	}
	for (prevlevel--; prevlevel>0; prevlevel--) {
		WriteIdent(prevlevel);
        WriteOutput("</topic>\n");
	}
	WriteOutput("</toc>\n");
    CloseOutput();
}



static void GenHtmlHelpAliases(char *name, char *suffix) {
    IRHeading* scan;
    char s[256];
    char u[256];
    char *p;
    OpenOutput(name,suffix);
    for (scan = IRHeading::getfirst(); scan; scan = scan->next) {
        scan->publist.reset();
        while (scan->publist.getline(s)) {
            Put("IDH_");
            for (p = s; *p; p++) {
                if (!isalnum(*p)) {
                    switch(*p) {
                    case '_': 
                        break;
                    // on crashes - add bad chars as a cases below to replace them with '_'
                    case '.':  
                    case ':':
                    case '#':
                    case '*':
                    case '/':
                    case ' ':
                        *p = '_';
                        break;
                    default: 
                        Error("Cannot map \"%s\" to a C id (try to add the bad char into case in genhtml.cpp?)", s);
                    }
                }
            }
            Put(s);
            Put("=");
            if (IRNode::multipart)
                sprintf(u, "%s%03.3d.html#%s",IRNode::name,scan->part,s);
            else
                sprintf(u, "%s.html#%s",IRNode::name,s);
            Put(u);

            NLPut("IDH_");
            Put(s); 
            Put("_topic=");
            if (IRNode::multipart)
                sprintf(u, "%s%03.3d.html#%04.4d",IRNode::name,scan->part,scan->id);
            else
                sprintf(u, "%s.html#%04.4d",IRNode::name,scan->id);
            Put(u);
            NL();
        }
    }
    CloseOutput();
}


/*

<HTML>
<HEAD>
</HEAD>
  <BODY>
<OBJECT type="text/site properties">
        <param name="ImageType" value="Folder">
</OBJECT>
<UL>
<LI> <OBJECT type="text/sitemap">
      <param name="Name" value="MiKTeX FAQ (14 July 2002)">
      <param name="Local" value="index.html">
    </OBJECT><UL><LI> <OBJECT type="text/sitemap">
      <param name="Name" value="About this document">
      <param name="Local" value="index.html#d0e40">
    </OBJECT><LI> <OBJECT type="text/sitemap">
      <param name="Name" value="General">
      <param name="Local" value="ar01s02.html">
    </OBJECT><UL><LI> <OBJECT type="text/sitemap">
      <param name="Name" value="Installation">
      <param name="Local" value="ar01s03.html">
    </OBJECT><LI> <OBJECT type="text/sitemap">
      <param name="Name" value="Getting Started">
      <param name="Local" value="ar01s04.html">
    </OBJECT><LI> <OBJECT type="text/sitemap">
      <param name="Name" value="Getting Help">
      <param name="Local" value="ar01s05.html">
    </OBJECT></UL><LI> <OBJECT type="text/sitemap">
      <param name="Name" value="Maintenance">
      <param name="Local" value="ar01s06.html">
    </OBJECT><LI> <OBJECT type="text/sitemap">
      <param name="Name" value="Packages">
      <param name="Local" value="ar01s07.html">
    </OBJECT></UL></UL>
</BODY>
</HTML>

*/

static void CloseHTML(int frameset = 0) {
/*
    int i;
    for (i = 0; i < 20; i++) NLPut("<BR>"); 
*/
    pars = 0;
    NL(); 
    if (!frameset) { CloseTag("BODY"); NL(); }
    CloseTag("HTML"); NL();
    CloseOutput();
}

void IRRoot::genFrameset(int php) {
    char *ext;
    if (php) ext = ".php"; else ext = ".html";
    OpenHTML(this->name, ext, &this->title, 1);
    NLPut("<FRAMESET COLS=\"30%,*\">");
    NLPut("  <FRAME NAME=contents SRC=\"");
    Put(this->name); Put("cnt.html\">");
    NLPut("  <FRAME NAME=body SRC=\"");
    Put(this->name);

    if (php) {
        Put("<?php");
        if (multipart) {
            NLPut("if (isset($_GET['goto'])) {");
            NLPut("    $found = TRUE;");
            NLPut("    switch ($_GET['goto']) {");
            IRHeading *h = IRHeading::getfirst();
            while (h) {
                char l[256];
                h->publist.reset();
                while (h->publist.getline(l)) {
                    char p[256];
                    sprintf(p, "%03.3d",h->part);
                    NLPut("        case \""); Put(l); Put("\" : "); 
                    Put("echo '"); Put(p); Put("'; break;");
                }
                h = h->next;
            }
            NLPut("        default: echo '000'; $found = FALSE; break;");
            NLPut("    }");
            NLPut("    echo '.html';");
            NLPut("    if ($found) {");
            NLPut("        echo '#'.$_GET['goto'];");
            NLPut("    }");
            NLPut("}");
            NLPut("else");
            NLPut("    echo '000.html';");
        }
        else
            NLPut("    echo 'bdy.html#'.$_GET['goto'];");
        NLPut("?>");
    }
    else {
        if (multipart) Put("000.html"); else Put("bdy.html");
    }
    Put("\">"); NL();
    OpenTag("NOFRAMES");
    NLPut("<H1>"); this->title.IRNode::genHTML(); Put("</H1>");
    NLPut("<P>A frames-capable browser is required to view this document.</P>");
    CloseTag("NOFRAMES");
    NLPut("</FRAMESET>");
    CloseHTML(1);
}

void IRRoot::genHTML(void) {
    curface = t_rm;
    cursize = t_normalsize;
    curpart = 0;
    pars = 0;
    if (frames) {
        genFrameset(0);
        if (!(javahelp||htmlhelp||eclipse)) genFrameset(1);
        GenJSContents(this->name,"cnt.js");
        OpenScriptContentsHTML(this->name, "cnt.html", &this->title);
        GenContentsHeader();
        NLPut("<NOSCRIPT>");
        GenContentsNoScript();
        NLPut("</NOSCRIPT>");
        NLPut("<SCRIPT TYPE=\"text/javascript\" SRC=\"body.js\">");
        NLPut("</SCRIPT>");
    }
    else {
        OpenHTML(this->name, ".html", &this->title);
    }
    if (multipart) {
        CloseHTML();
        OpenHTML(this->name, "000.html", &this->title);
    }
    else if (frames) {
        CloseHTML();
        OpenHTML(this->name, "bdy.html", &this->title);
    }
    NLPut("<A NAME=\"title\">&nbsp;</A>");
    pars = 1;
    this->IRNode::genHTML();
    CloseHTML();
    if (htmlhelp) {
        GenHtmlHelpTOC(this->name,".hhc");
        GenHtmlHelpAliases(this->name,".hha");
    } 
    if (eclipse) {
        GenEclipseTocXml(this->name,"_toc.xml");
    }
}

void IRRoot::genHTML(int m, int f, int n, int j, int hh, int ecl) {
    multipart  = m;
    frames     = f;
    navigation = n;
    javahelp   = j;
    htmlhelp   = hh;
    eclipse    = ecl;
    this->genHTML();
}

void IRPlain::genHTML(void) {
    Translate(this->text, this->verb);
}

void IRCommand::genHTML(void) {
    char buf[256];
    switch (code) {
    case t_tie:
        Put("&nbsp;"); break;
    case t_par:
        pars = 1; break;
    case t_newline:
        Put("<BR>"); NL(); break;
    case t_hyphen:
        Put("-"); break;
    case t_endash:
        Put(javahelp ? "--" : "&ndash;"); break;
    case t_emdash:
        Put(javahelp ? "---" : "&mdash;"); break;
    case t_lsquo:
        Put(javahelp ? "`" : "&lsquo;"); break;
    case t_rsquo:
        Put(javahelp ? "'" : "&rsquo;"); break;
    case t_ldquo:
        Put(javahelp ? "\"" : "&ldquo;"); break;
    case t_rdquo:
        Put(javahelp ? "\"" : "&rdquo;"); break;
    case t_copyright:
        Put("&copy;"); break;
    case t_TeX:
        Put("TeX"); break;
    case t_LaTeX:
        Put("LaTeX"); break;
    case t_times:
        Put("&middot;"); break;
    case t_textbackslash:
    case t_backslash:
        Put("\\"); break;
    case t_ldots:
        Put("..."); break;
    case t_surd:
        Put(javahelp ? "V" : "&radic;"); break;
    case t_subset:
        Put(javahelp ? "&lt;" : "&sub;"); break;
    case t_subseteq:
        Put(javahelp ? "&lt;=" : "&sube;"); break;
    case t_supset:
        Put(javahelp ? "&gt;" : "&sup;"); break;
    case t_supseteq:
        Put(javahelp ? "&gt;=" : "&supe;"); break;
    case t_leq:
        Put(javahelp ? "&lt;=" : "&le;"); break;
    case t_geq:
        Put(javahelp ? "&gt;=" : "&ge;"); break;
    case t_neq:
        Put(javahelp ? "!=" : "&ne;"); break;
    case t_pi:
        Put(javahelp ? "pi" : "&pi;"); break;
    case t_exp:
        Put("exp "); break;
    case t_ln:
        Put("ln "); break;
    case t_Diamond:
        Put("&diams;"); break;
    case t_circ:
        Put("&ordm;"); break;
    case t_rhd:
        Put("&#9658;"); break;
    default:
        Put("?");
        printf("\n------- code= %d\n", code);
        Warning("Symbol '%s' is unsupported",VisToken(code,buf));
        break;
    }
}

void IRItem::genHTML(void) {
    pars = 0;
    if (son) {
        if (IsTagOpen("DD")) CloseTag("DD");
        NL(); OpenTag("DT");
        this->IRNode::genHTML();
        CloseTag("DT");
        NL(); OpenTag("DD");
    }
    else {
        if (IsTagOpen("LI")) CloseTag("LI");
        NL(); OpenTag("LI");
    }
}

void IRVerbatim::genHTML(void) {
    String *scan;
    for (scan = list.first; scan; scan = scan->next)
        Translate(scan->string,1);
}

void IREnv::genHTML(void) {
    char al = align;

    switch (this->code) {
    case e_document:
        this->IRNode::genHTML();
        break;
    case e_verbatim:
        pars = 1;
        NLPut("<PRE>");
        this->IRNode::genHTML();
        Put("</PRE>");
        NL();
        break;
    case e_itemize:
        pars = 0;
        NL(); OpenTag("UL"); NL();
        this->IRNode::genHTML();
        pars = 0;
        if (IsTagOpen("LI")) CloseTag("LI");
        NL(); CloseTag("UL"); NL();
        break;
    case e_enumerate:
        pars = 0;
        NL(); OpenTag("OL"); NL();
        this->IRNode::genHTML();
        pars = 0;
        if (IsTagOpen("LI")) CloseTag("LI");
        NL(); CloseTag("OL"); NL();
        break;
    case e_description:
        pars = 0;
        NL(); OpenTag("DL"); NL();
        this->IRNode::genHTML();
        pars = 0;
        if (IsTagOpen("DD")) CloseTag("DD");
        else if (IsTagOpen("LI")) CloseTag("LI");
        NL(); CloseTag("DL"); NL();
        break;
    case e_center:
    case e_flushleft:
    case e_flushright:
        pars = 1;
        align = Align(this->code);
        this->IRNode::genHTML();
        align = al;
        pars = 1;
        break;
    case e_tabbing:
    case e_tabular:
    case e_tabularw:
    default:
        this->IRNode::genHTML();
        break;
    }
}

void IRTable::genHTML(void) {
    char s[200];
    sprintf(s,"<P ALIGN=CENTER><B><A NAME=table%d>Table %d. </A></B>", this->number, this->number);
    NLPut(s);
    this->caption.genHTML();
    NL();
    this->IRNode::genHTML();
    NL();
}

void IRFigure::genHTML(void) {
    char s[200];
    sprintf(s,"<P ALIGN=CENTER><B><A NAME=figure%d>Figure %d. </A></B>", this->number, this->number);
    NLPut(s);
    this->caption.genHTML();
    NL();
    this->IRNode::genHTML();
    NL();
}

void IRPopUp::genHTML(void) {
    Error("Pop-ups are not supported in HTML");
}

void IRTabular::genHTML(void) {
    IRNode *row;
    IRCell *cell;
    IRCol *col;
    IRRowSep *rowsep;
    int lb,rb,tb,bb,tbmask,bbmask;
    NLPut("<TABLE CELLSPACING=0>");
    rowsep = this->rowseps;
    tbmask = rowsep->border;
    for (row = this->son; row; row = row->next) {
        rowsep = rowsep->next;
        bbmask = rowsep->border;
//        printf("bbmask=%d\n",bbmask);
        NL(); OpenTag("TR"); NL();
        col = this->cols;
        lb = col->rborder;
        for (cell = (IRCell*)(row->son); cell; cell = (IRCell*)(cell->next)) {
//            printf("%d %d\n",tbmask, bbmask);
            col = col->next;
            if (!col) 
                break;
            rb = col->rborder;
            tb = tbmask&1;
            bb = bbmask&1;
            tbmask>>=1;
            bbmask>>=1;
            cell->genHTML(col->align,tb,rb,bb,lb);
            lb = 0;
        }
        NL(); CloseTag("TR"); NL();
        tbmask = 0;
    }
    NLPut("</TABLE>");
}

void IRCell::genHTML(char align, int tb, int rb, int bb, int lb) {
    char attr[256], span[16];
    if (this->align) align = this->align;
//    if (this->lb >= 0) lb = this->lb; !!!
    if (this->rb >= 0) rb = this->rb;
    switch (align) {
    case 'l': attr[0] = 0; break;
    case 'r': strcpy(attr, "ALIGN=RIGHT"); break;
    case 'c': strcpy(attr, "ALIGN=CENTER"); break;
    default:  Error("Unknown alignment specifier"); break;
    }
    if (this->span > 1) {
        strcat(attr," COLSPAN=");
        sprintf(span, "%d", this->span);
        strcat(attr,span);
    }
    strcat(attr," STYLE=\"padding:0px 3px;");

    if (lb+rb+tb+bb) {
        strcat(attr,"border-style:");
        if (tb>1) strcat(attr," double");
        else if (tb) strcat(attr," solid");
        else strcat(attr," none");
        if (rb>1) strcat(attr," double");
        else if (rb) strcat(attr," solid");
        else strcat(attr," none");
        if (bb>1) strcat(attr," double");
        else if (bb) strcat(attr," solid");
        else strcat(attr," none");
        if (lb>1) strcat(attr," double");
        else if (lb) strcat(attr," solid");
        else strcat(attr," none");

        strcat(attr,";border-width:");
        if (tb>1) strcat(attr," 3px");
        else if (tb) strcat(attr," 1px");
        else strcat(attr," 0px");
        if (rb>1) strcat(attr," 3px");
        else if (rb) strcat(attr," 1px");
        else strcat(attr," 0px");
        if (bb>1) strcat(attr," 3px");
        else if (bb) strcat(attr," 1px");
        else strcat(attr," 0px");
        if (lb>1) strcat(attr," 3px");
        else if (lb) strcat(attr," 1px");
        else strcat(attr," 0px");
    }
    strcat(attr,"\"");
    OpenTag ("TD",attr);
    if (this->son)
        this->IRNode::genHTML();
    else
        Put("&nbsp;");
    if (curface != f_rm) {
        SwitchFont(curface,f_rm,cursize,cursize);
        curface = f_rm;
    }
    CloseTag("TD"); NL();
}

void IRHeading::genHTML(void) {
    char s[256], p[256];
    IRHeading *link;
    int l;
    int genLabels = 1;  

    if ((this->id==0) && !frames && (navigation!=HTMLNAV_NONE)) 
        GenContentsNoScript();

    if ((navigation==HTMLNAV_FULL) && !this->prev && !frames) {
        NLPut("<HR>");
        NLPut("<A NAME=\"root\">");
        NL(); GenLink("contents",0,-1); Put("<IMG ALT=\"Contents\" SRC=\"root.gif\" BORDER=0></A>");
        NLPut("</A>");
    }

    if ( ( (navigation==HTMLNAV_FULL) ||
           (this->multipart && this->prev && this->prev->part != this->part)
         ) &&
		 ( !this->prev ||
           ( (this->prev->level <= 3) &&
             (this->prev->level == this->level-1) )
		 )
       ) {
        NLPut("<UL>");
        for (link = this; link; link = link->next) {
            if (link->level < this->level) break;
            if (link->level > this->level) continue;
            NLPut("<LI>");
            GenLink(link->id,this->prev?this->prev->part:0,link->part);
            link->IRNode::genHTML();
            Put("</A>");
        }
        NLPut("</UL>");
    }

    if (this->multipart) {
        if (this->part != this->curpart) {
            CloseHTML();
            sprintf(s,"%03.3d.html",this->part);
            OpenHTML(this->name,s,this);
            this->curpart = this->part;
        }
    }

    if (!pars) pars = 1;
    if ((navigation==HTMLNAV_FULL) && this->level <= 4) Put("<HR>");

    if ((navigation==HTMLNAV_FULL)) {
        /* Open labels */
        sprintf(s,"<A NAME=\"%04.4d\">",this->id); NLPut(s);
        this->publist.reset();
        while (this->publist.getline(p)) {
            Put("<A NAME=\""); Put(p); Put("\">");
        }

        if (!frames) {
            NL();
            GenLink("contents",this->part,-1);
            Put("<IMG ALT=\"Contents\" SRC=\"root.gif\" BORDER=0></A>");
        }

        if (this->level > 1) {
            for (l = this->level-1; l; l--) {
                link = this->super(l);
                if (link) {
                    NL();
                    GenLink(link->id,this->part,link->part);
                    Put("<IMG ALT=\"Super\" SRC=\"super.gif\" BORDER=0></A>");
                    break;
                }
            }
            if (!link)
                NLPut("<IMG ALT=\"   \" SRC=\"empty.gif\" BORDER=0>");
        }
        else {
            NL(); GenLink("root",this->part,0); Put("<IMG ALT=\"Super\" SRC=\"super.gif\" BORDER=0></A>");
        }

        if (this->prev) {
            NL();
            GenLink(this->prev->id,this->part,this->prev->part);
            Put("<IMG ALT=\"Prev\" SRC=\"prev.gif\" BORDER=0></A>");
        }
        else
            NLPut("<IMG ALT=\"   \" SRC=\"empty.gif\" BORDER=0>");

        if (this->next) {
            NL();
            GenLink(this->next->id,this->part,this->next->part);
            Put("<IMG ALT=\"Next\" SRC=\"next.gif\" BORDER=0></A>");
        }
        else
            NLPut("<IMG ALT=\"   \" SRC=\"empty.gif\" BORDER=0>");

        /* Close labels */
        this->publist.reset();
        while (this->publist.getline(p)) {
            Put("</A>");
        }
        Put("</A>");

        /* No need to generate them again */
        genLabels = 0;
        NLPut("<BR>");
    }
    sprintf(s,"H%d>",this->level);
    NLPut("<"); Put(s);

    if (genLabels) {
        sprintf(p,"<A NAME=\"%04.4d\">",this->id); NLPut(p);
        this->publist.reset();
        while (this->publist.getline(p)) {
            Put("<A NAME=\""); Put(p); Put("\">");
        }
    }

    this->IRNode::genHTML();

    if (genLabels) {
        this->publist.reset();
        while (this->publist.getline(p)) {
            Put("</A>");
        }
        Put("</A>");
    }
    Put("</"); Put(s); NL();
}

void IRRef::genHTML(void) {
    char s[200];
    IRHeading *h;
    IRExtern *e;
    IRFloat *f;

//!!!    CheckPar();
    h = IRHeading::find(this->label);
    if (h) {
        GenLink(h->id,this->part,h->part);
        if (son) this->IRNode::genHTML(); else h->IRNode::genHTML();
        Put("</A>");
        return;
    }

    e = IRExtern::find(this->label);
    if (e) {
	  if(frames)
	  {
       if(e->part != -1)
		   sprintf(s, "<A HREF=\"%s%03.3d.html#%04.4d\" TARGET=body OnClick=\"window.open(\'%scnt.html\',\'contents\');\" >%s",e->file, e->part, e->id, e->file, e->label);
       else
          sprintf(s, "<A HREF=\"%s.html#%04.4d\" TARGET=body OnClick=\"window.open(\'%scnt.html\',\'contents\');\" >%s", e->file, e->id, e->file, e->label);
	  } else
	  {
       if(e->part != -1)
          sprintf(s, "<A HREF=\"%s%03.3d.html#%04.4d\">%s", e->file, e->part, e->id, e->label);
       else
          sprintf(s, "<A HREF=\"%s.html#%04.4d\">%s", e->file, e->id, e->label);
	  }
        Put(s);
        if (son) this->IRNode::genHTML();
        Put("</A>");
        return;
    }

    f = IRTable::find(this->label);
    if (f) {
        sprintf(s, "<A HREF=#table%d>",f->number);
        Put(s);
        if (son) this->IRNode::genHTML();
        else {
            sprintf(s, "%d. ",f->number);
            Put(s);
            f->caption.genHTML();
        }
        Put("</A>");
        return;
    }
    Warning("Unresolved reference - '%s'",this->label);
    Put("???");
}

void IRFootnote::genHTML(void) {
    Put("&nbsp;/");
    this->IRNode::genHTML();
    Put("/ ");
}

void IRHRef::genHTML(void) {
    Put("<A HREF=\"");
    Put(this->url);
    Put("\">");
    if (this->son) 
        this->IRNode::genHTML();
    else
        Put(this->url);
    Put("</A>");
}

void IRGraphics::genHTML(void) {
    char *ext = this->param("htmlext");
    char *alt = this->param("htmlalt");
    Put("<IMG ALT=\"");
    Put(alt ? alt : "");
    Put("\" SRC=\"");
    Put(this->name);
    Put(".");
    Put(ext ? ext : "jpg");
    Put("\" HSPACE=0 VSPACE=0 BORDER=0>");
}


