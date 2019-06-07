
#include <stdio.h>
#include <string.h>

#include "basedefs.h"
#include "scanner.h"
#include "ir.h"
#include "generate.h"

static int offset = 0;

static void Put(const char text[]) {
    WriteOutput(text);
    offset += strlen(text);
}

static void XMLPut(const char text[]) {
    char *buf = new char[strlen(text)+1];
    strcpy(buf,text);
    char *p, *q, c;
    for (p = q = buf; *p; q++) {
        if (!*q) {
            WriteOutput(p);
            offset += strlen(p);
            p = q;
        }
        if (*q && strchr("\"<>&", *q)) {
            c = *q;
            *q = 0;
            WriteOutput(p);
            offset += strlen(p);
            p = q; p++;
            switch (c) {
            case '"':
                WriteOutput("&quot;"); break;
            case '<':
                WriteOutput("&lt;"); break;
            case '>':
                WriteOutput("&gt;"); break;
            case '&':
                WriteOutput("&amp;"); break;
            default:
                FatalError("Unknown markup symbol"); break;
            }
            offset++;
        }
    }
}


static void NLPut(const char text[]) {
    if (offset) { WriteOutput("\n"); offset = 0; }
    Put(text);
}

static void NL(void) { NLPut(""); }


static void OpenXML(char *name, char *suffix) {
    OpenOutput(name,suffix);
    Put("<?xml version='1.0' encoding='ISO-8859-1'  ?>");
}

static void GenMap(void) {
    IRHeading* scan;
    char s[256];
    char u[256];
    NLPut("<!DOCTYPE map");
    NLPut("  PUBLIC \"-//Sun Microsystems Inc.//DTD JavaHelp Map Version 1.0//EN\"");
    NLPut("         \"http://java.sun.com/products/javahelp/map_1_0.dtd\">");
    NL();
    NLPut("<map version=\"1.0\">");
    for (scan = IRHeading::getfirst(); scan; scan = scan->next) {
        scan->publist.reset();
        while (scan->publist.getline(s)) {
            NLPut("<mapID target=\"");
            Put(s);
            Put("\" url=\"");
            if (IRNode::multipart)
                sprintf(u, "%s%03.3d.html#%s",IRNode::name,scan->part,s);
            else
                sprintf(u, "%s.html#%s",IRNode::name,s);
            Put(u);
            Put("\" />");

            NLPut("<mapID target=\"");
            Put(s); 
            Put(".topic\" url=\"");
            if (IRNode::multipart)
                sprintf(u, "%s%03.3d.html#%04.4d",IRNode::name,scan->part,scan->id);
            else
                sprintf(u, "%s.html#%04.4d",IRNode::name,scan->id);
            Put(u);
            Put("\" />");

        }
        NLPut("<mapID target=\"");
        sprintf(s, "%04.4d", scan->id);
        Put(s);
        Put("\" url=\"");
        if (IRNode::multipart) 
            sprintf(s, "%s%3.3d.html#%04.4d",IRNode::name,scan->part,scan->id);
        else
            sprintf(s, "%s.html#%04.4d",IRNode::name,scan->id);
        Put(s);
        Put("\" />");
    }
    NLPut("<mapID target=\"help.Titlebar.Image\"   url=\"HelpIcon.gif\" />");
    NLPut("</map>");
}

static void GenTOC(void) {
    IRHeading* scan;
    int prevlevel;
    int level;
    int diff;
    char s[256];

    NLPut("<!DOCTYPE toc");
    NLPut("  PUBLIC \"-//Sun Microsystems Inc.//DTD JavaHelp TOC Version 1.0//EN\"");
    NLPut("         \"http://java.sun.com/products/javahelp/toc_1_0.dtd\">");

    NLPut("<toc version=\"1.0\">");
    NLPut("<tocitem text=\"");
    IRRoot::title.gentext(XMLPut);
    Put("\">");
    prevlevel = 0;
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
        // Close this and lower level items, if any
        for (diff = prevlevel-scan->level+1; diff>0; diff--) {
            Put("</tocitem>");
            NL();
        }
        NLPut("<tocitem text=\"");
        scan->IRNode::gentext(XMLPut);
        Put("\" target=\"");
        sprintf(s, "%04.4d", scan->id);
        Put(s);
        Put("\">");
        prevlevel = scan->level;
    }
    // Close last
    for (diff = prevlevel; diff>0; diff--) {
        NLPut("</tocitem>");
        NL();
    }
    // Close title item
    NLPut("</tocitem>");
    NLPut("</toc>");
}

static void GenIndex(void) {
    IRHeading *scan;
    String *item;
    char s[256];
    NLPut("<!DOCTYPE index");
    NLPut("  PUBLIC \"-//Sun Microsystems Inc.//DTD JavaHelp Index Version 1.0//EN\"");
    NLPut("         \"http://java.sun.com/products/javahelp/index_1_0.dtd\">");
    NLPut("<index version=\"1.0\">");

    for (scan = IRHeading::getfirst(); scan; scan = scan->next) {
        for (item = scan->indlist.first; item; item = item->next) {
            NLPut("<indexitem text=\"");
            Put(item->string);
            Put("\" target=\"");
            sprintf(s, "%04.4d", scan->id);
            Put(s);
            Put("\" />");

            item = item->next;

            /*
            if (scan->string[0]) {
                Put(" - ");
                Translate(scan->string,1);
            }
            Put("}");
            */
        }
    }

    NLPut("</index>");
}

/*
<?xml version='1.0' encoding='ISO-8859-1'  ?>
<!DOCTYPE index
  PUBLIC "-//Sun Microsystems Inc.//DTD JavaHelp Index Version 1.0//EN"
         "http://java.sun.com/products/javahelp/index_1_0.dtd">

<index version="1.0">
   <indexitem text="Create Project Wizard">
      <indexitem text="adding existing sources to a bean project" target="proj.existingbeans"/>
      <indexitem text="naming a source file location" target="proj.appletwizard"/>
   </indexitem>
   <indexitem text="naming a project" target="proj.projectwizard"/>
   <indexitem text="performance analysis, see 'profiler'"/>
   <indexitem text="source directory" target="proj.generaldisplay"/>
   <indexitem text="standalone project">
      <indexitem text="creating" target="proj.createstandalone"/>
      <indexitem text="running" target="proj.run"/>
      <indexitem text="specifying program arguments" target="debug.arguments"/>
   </indexitem>
   <indexitem text="starting debugging" target="debug.start"/>
</index>
*/

static void GenHelpSet(void) {
    NLPut("<!DOCTYPE helpset");
    NLPut("  PUBLIC \"-//Sun Microsystems Inc.//DTD JavaHelp HelpSet Version 1.0//EN\"");
    NLPut("         \"http://java.sun.com/products/javahelp/helpset_1_0.dtd\">");
    NLPut("<helpset version=\"2.0\">");
    
    NLPut("  <title>");
    IRRoot::title.gentext(XMLPut);
    Put("</title>");

    NLPut("  <maps>");
    NLPut("    <homeID>top</homeID>");
    NLPut("    <mapref location=\"");
    Put(IRNode::name);
    Put(".jhm\"/>");
    NLPut("  </maps>");

    NLPut("  <view>");
    NLPut("    <name>TOC</name>");
    NLPut("    <label>Contents</label>");
    NLPut("    <type>javax.help.TOCView</type>");
    NLPut("    <data>");
    Put(IRNode::name);
    Put(".xml");
    Put("</data>");
    NLPut("  </view>");

/* !!! Index must be sorted 
    NLPut("<view>");
    NLPut("  <name>Index</name>");
    NLPut("  <label>index</label>");
    NLPut("  <type>javax.help.IndexView</type>");
    NLPut("  <data>");
    Put(IRNode::name);
    Put(".jdx");
    Put("</data>");
    NLPut("</view>");
*/

    NLPut("  <view>");
    NLPut("    <name>Search</name>");
    NLPut("    <label>Search</label>");
    NLPut("    <type>javax.help.SearchView</type>");
    NLPut("    <data engine=\"com.sun.java.help.search.DefaultSearchEngine\">");
    NLPut("      JavaHelpSearch");
    NLPut("    </data>");
    NLPut("  </view>");

    NLPut("  <presentation default=\"true\" displayviews=\"true\" displayviewimages=\"false\">");
    NLPut("     <name>Help Window</name>");
    NLPut("     <size width=\"880\" height=\"580\" />");
    NLPut("     <location x=\"80\" y=\"50\" />");
    NLPut("     <image>help.Titlebar.Image</image>");
    NLPut("  </presentation>");
    
    NLPut("</helpset>");
}

/*
<?xml version='1.0' encoding='ISO-8859-1' ?>
<!DOCTYPE helpset
  PUBLIC "-//Sun Microsystems Inc.//DTD JavaHelp HelpSet Version 1.0//EN"
         "http://java.sun.com/products/javahelp/helpset_1_0.dtd">

<helpset version="1.0">

  <!-- title -->
  <title>MyHelpSet test - Help</title>

  <!-- maps -->
  <maps>
     <homeID>top</homeID>
     <mapref location="Map.jhm"/>
  </maps>

  <!-- views -->
  <view>
    <name>TOC</name>
    <label>Table Of Contents</label>
    <type>javax.help.TOCView</type>
    <data>MyHelpTOC.xml</data>
  </view>

  <view>
    <name>Index</name>
    <label>Index</label>
    <type>javax.help.IndexView</type>
    <data>MyHelpIndex.xml</data>
  </view>

  <view>
    <name>Search</name>
    <label>Search</label>
    <type>javax.help.SearchView</type>
    <data engine="com.sun.java.help.search.DefaultSearchEngine">
      JavaHelpSearch
    </data>
  </view>
</helpset>
*/

void IRRoot::genXML(void) {
    OpenXML(this->name, ".xml");
    GenTOC();
    CloseOutput();
    OpenXML(this->name, ".jhm");
    GenMap();
    CloseOutput();
/*
    OpenXML(this->name, ".jdx");
    GenIndex();
    CloseOutput();
*/
    OpenXML(this->name, ".hs");
    GenHelpSet();
    CloseOutput();
}
