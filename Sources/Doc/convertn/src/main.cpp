#include <string.h>
#include <stdio.h>
#include <ctype.h>

#include "system.h"

#include "basedefs.h"
#include "scanner.h"
#include "ir.h"
#include "parser.h"

#ifdef __linux__
  #define EXEEXT ""
#else
  #define EXEEXT ".exe"
#endif

// Static members of IR classes

char* IRNode::name;
int IRNode::multipart = 0;
int IRNode::javahelp  = 0;
int IRNode::htmlhelp  = 0;
char* IRNode::stylesheet = 0;
int IRNode::curpart = 0;
int IRNode::curface = t_rm;
int IRNode::cursize = t_normalsize;
IRNode IRRoot::title;
IRFloat* IRFloat::first = 0;
IRFloat* IRFloat::last  = 0;
int IRTable::lastnumber = 0;
int IRFigure::lastnumber = 0;
int IRPopUp::lastnumber = 0;
int IRHeading::lastid = 0;
IRHeading* IRHeading::first = 0;
IRHeading* IRHeading::last  = 0;
int IRHeading::toplevel = 0;
IRExtern*  IRExtern::last = 0;

static void ExtractName(char to[], const char from[]) {
    const char *p,*q;
    p = strrchr(from,'\\');
    if (p) p++; else p = from;
    q = strrchr(p,'.');
    if (q) strncpy(to,p,q-p);
    else   strcpy(to,p);
}

static void ReadIniFile(char name[]) {
    FILE *cfgfile = fopen(name,"r");
    char buf[256];
    char *scan;
    char cmd[256];     // command name
    int  opt;          // number of optional arguments
    int  req;          // number of required arguments
    if (cfgfile) {
        for (;;) {
            if (!fgets(buf,256,cfgfile)) {
                if (!feof(cfgfile))
                    Error("Error %d reading .ini file",ferror(cfgfile));
                break;
            }
            for (scan = &buf[0]; isspace(*scan); scan++);
            if ((*scan == '\n') || (*scan == '\0')) continue;
            if (*scan++ != '\\') Error("'\\' expected");
            if (sscanf(scan,"%s %d %d",cmd,&opt,&req) != 3)
                Error("Error in configuration file");
            new ICmd(cmd,strlen(cmd),opt,req);
        }
        fclose(cfgfile);
    }
}

char myname  [512];
char inname  [512];
char outname [512];

int main (int argc, char *argv[]) {
    unsigned rc = LocateSelf(myname,512);
    int input   = 0;
    int output  = 0;
    int genipf  = 1;
    int genrtf  = 0;
    int gencnt  = 0;
    int genhtml = 0;
    int genjh   = 0;
    int genhh   = 0;
    int genecl  = 0;
    int genext  = 0;
    int frames  = 0;
    int HTMLnav = HTMLNAV_FULL;
    int cntlevel  = 1;
    int multipart = 0;
    char *stylesheet = 0;
    char ext[5] = ".ipf";
    int arg;
    IRRoot *Root;

    fprintf(stderr,"LaTeX Converter v1.01\n");
    fprintf(stderr,"Copyright (c) 1999-2003 Excelsior, LLC. All Rights Reserved.\n");

    if (rc) Error("Failed to locate executable, errorcode is %u",rc);

    /* Maybe it will better, if we will read any ini file under UNIX,
     * even if its extension not in lowercase?..
     */
    strcpy(myname+strlen(myname)-strlen(EXEEXT),".ini");
    ReadIniFile(myname);

    for (arg = 1; arg < argc; arg++) {
        if ((argv[arg][0]=='-') || (argv[arg][0]=='/')) {
            switch (argv[arg][1]) {
            case 'i': genipf++; break;
            case 'r':
                genrtf  = 1; genipf--;
                if (argv[arg][2] == 'c') {
                    gencnt = 1;
                    if (isdigit(argv[arg][3]))
                        cntlevel = argv[arg][3] - '0';
                }
                break;
            case 'h':
                genhtml = 1; genipf--;
                if (argv[arg][2] == 'f') frames = 1;
                else if (argv[arg][2] == 'c') HTMLnav = HTMLNAV_TOCONLY;
                else if (argv[arg][2] == 'p') HTMLnav = HTMLNAV_NONE;
                break;
            case 'j': genjh = 1; break;
            case 'H': genhh = 1; break;
            case 'E': genecl= 1; break;
            case 'm':
                if (argv[arg][2] == '3') multipart = 3;
                else if (argv[arg][2] == '2') multipart = 2;
                else multipart = 1;
                break;
            case 'e': genext = 1; break;
            case 's': stylesheet = ""; break;
            case 'S': if ((arg+1 == argc) || (argv[arg+1][0]=='-'))
                          FatalError("Stylesheet name expected after '-S'");
                      arg++;
                      stylesheet = argv[arg];
                      break;
            default:
                FatalError("Unknown option");
            }
        }
        else {
            if (input) FatalError("Too many input files specified");
            input = arg;
        }
    }
    gencnt = gencnt && genrtf;
    if (stylesheet && !stylesheet[0]) {
        if (genjh)
            stylesheet = "javahelp.css";
        else
            stylesheet = "default.css";
    }

    if (!input) {
        fprintf(stderr,"\nUsage:\n");
        fprintf(stderr,"\n    convert [options] file [more options]\n");
        fprintf(stderr,"\nOptions:\n");
        fprintf(stderr,"\n    -i         emit IPF (default)");
        fprintf(stderr,"\n    -r[c[#]]   emit RTF [ and CNT [ top level # ] ]");
        fprintf(stderr,"\n    -h[f|c|p]  emit HTML file(s) [ with frames | toc only | plain ]");
        fprintf(stderr,"\n    -j         emit JavaHelp files");
        fprintf(stderr,"\n    -e         emit extern file\n");
        fprintf(stderr,"\n               HTML specific:\n");
        fprintf(stderr,"\n    -H         emit HtmlHelp files");
        fprintf(stderr,"\n    -E         emit Eclipse .xml and .html files (use with -hc -m)");
        fprintf(stderr,"\n    -m[n]      multipart output [at level n]");
        fprintf(stderr,"\n    -s         link to stylesheet 'default.css'");
        fprintf(stderr,"\n               ('javahelp.css' if -j is present)");
        fprintf(stderr,"\n    -S NAME    link to stylesheet NAME\n\n");
        return 1;
    }

    strcpy(inname,argv[input]);

    InitScan(inname);
    GetToken();
    ExtractName(outname,inname);
    Root = BuildIR(outname,multipart,genjh,genhh|genecl,stylesheet);
    fprintf(stderr,"\nGenerating output files\n");
    if (genipf)  Root->genIPF();
    if (genrtf)  Root->genRTF();
    if (gencnt)  Root->genCNT(cntlevel);
    if (genhtml) Root->genHTML(multipart,frames,HTMLnav,genjh,genhh,genecl);
    if (genjh)   Root->genXML();
    if (genext)  Root->genEXT();
    return 0;
}
