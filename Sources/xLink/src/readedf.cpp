/******************************************************************************\
*                                                                              *
*        Name                   :  readdef.c                                   *
*        Creation date          :  6.02.97.                                    *
*        Author                 :  Alexs                                       *
*        Description            :  contains routines for reading definitions of*
*                                  dynamic linking libraries.                  *
*        List of exported                                                      *
*        routines               :  ReadEdf                                     *
*                                                                              *
\******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "xdefs.h"

open_namespace

#include "xos.h"
#include "struct.h"
#include "idents.h"
#include "messages.h"
#include "xmem.h"
#include "readedf.h"
#include "reader.h"

/******************************************************************************\
   Global constants and variables
\******************************************************************************/

#define SPACE           0x20
#define TAB             0x09
#define COMMENT_CHAR    ';'

byte * rawdata;
unsigned long rawdataSize;
unsigned long f_pos;

char   name[1024] = "";                  /* Name of parsed file */
int    line = 0;                         /* Parsed line number */
int    pos  = 0;                         /* Current position in parsed line */
char   buf[4096];                        /* Parsed line buffer */

char   identbuf[4096] = "";              /* Buffer for identifiers */
int    identidx    = 0;                  /* Length of identifier in 'ident' */

Bool   eof = false;                      /* Indicates End Of File */



/******************************************************************************\
  Name          :   readStr
  Type          :   Function
  Description   :   Function reads string (till '\n') from file
                    and puts it to '*buf'. If read string is greater than
                    buffer size (which is determined by '*size'), buffer is
                    resized, its new size and address are assigned to '*size'
                    and '*buf' accordingly.
                    If End Of File is reached it returns 'true', else - 'false'.
\******************************************************************************/
Bool readStr() {
  if (f_pos == rawdataSize)
     return true;

  int start_pos = f_pos;

  while ((f_pos < rawdataSize) && (rawdata[f_pos] != '\r')
                               && (rawdata[f_pos] != '\n'))
        f_pos ++;
  memcpy(buf, rawdata + start_pos, f_pos - start_pos);
  buf[f_pos - start_pos] = '\0';
  while ((f_pos < rawdataSize) && ((rawdata[f_pos] == '\r')
                               ||  (rawdata[f_pos] == '\n')))
        f_pos ++;
  line++;
  return false;
};

/******************************************************************************\
  Name          :   skip_blanks
  Type          :   Function
  Description   :   Function skips spaces, tabs and comments from 'buf',
                    which is current parsed line, and reads new 
                    line if nessesary.
\******************************************************************************/
void skip_blanks(int break_if_eof){
  do{
    if (!buf[pos])
      do{
        eof = readStr();
        if (eof) {
          if (break_if_eof)
            Message(xFATAL, msgNOT_EXPECT_EOF, name);
          return;
        }
        pos = 0;
      }while (buf[0] == COMMENT_CHAR);
    while (buf[pos] &&
           ((buf[pos] == SPACE) ||
            (buf[pos] == TAB))
          ) pos++;
  } while(!buf[pos] && !eof);
};

/******************************************************************************\
  Name          :   get_ident
  Type          :   Function
  Description   :   Function gets identifier from current position in 'buf'
                    (buf[pos]) and puts it to 'identbuf' (identidx is identifier
                    length).
\******************************************************************************/
#define isidentchar(c) ((c) != SPACE && (c) != TAB && (c) != 0 && (c) != '=' && (c) != '(' && (c) != ')')
void get_ident(int break_if_no_ident){
  identidx = 0;
  if (isalpha(buf[pos]) || buf[pos] == '_')
    while(buf[pos] && isidentchar(buf[pos]))
      identbuf[identidx++] = buf[pos++];
  if (identidx){
    identbuf[identidx]  = 0;
  }else if (break_if_no_ident){
    Message(xFATAL, msgIDENT_EXPECT, name, line, pos + 1);
  };
};

/******************************************************************************\
  Name            :   strequ
  Type            :   Macro
  Description :   Macro returns non-zero value if 'str1' and 'str2'
                                  are equal strings and zero in other case.
\******************************************************************************/
#define strequ(str1, str2) !strcmp(str1, str2)


/******************************************************************************\
  Name            :   ReadEdf
  Type            :   Function
  Description :   Function parsed definition file defined by 'edfname'.
                                  Library name is skipped.
                                  NewExport called for every exported name
\******************************************************************************/
void ReadEdf(char * edfname){

  char * end;
  int    i;

  ident  intname, extname, modname;
  word   ordinal;

  OSFile * file = OS->File ();
  if (! file -> OpenRead (edfname, rawdata, rawdataSize))
      return;

  strcpy(name, edfname);
  buf[0] = 0;


  /*-------------------------*/
  /*  Parse LIBRARY section  */
  /*-------------------------*/
  skip_blanks(false);
  get_ident(false);
  if ( strequ(identbuf, "LIBRARY") ){
        skip_blanks(true);

    /* Library name is skipped */
    get_ident(true);
    if (buf[pos] == '.'){
      pos++;
      get_ident(true);
    };
    skip_blanks(false);
    get_ident(false);
  };

  /*-----------------------------*/
  /*  Parse DESCRIPTION section  */
  /*-----------------------------*/
  if ( identidx && strequ(identbuf, "DESCRIPTION") ){
    skip_blanks(false);
    if (!eof){
      if (buf[pos] == '"'){
        i = pos++; identidx = 0;
        while(buf[pos] != '"'){
          if (!buf[pos]){
            pos = 0;
            eof = readStr();
            if (eof)
              Message(xFATAL, msgSTR_NOT_CLOSED, name, line, i + 1);
          }else{
            identbuf[identidx++] = buf[pos++];
          };
        };
        pos++; identbuf[identidx] = 0;
        Description = dup(identbuf, identidx);
        skip_blanks(false);
      };
      get_ident(false);
    };
  };

  /*-------------------------*/
  /*  Parse EXPORTS section  */
  /*-------------------------*/
  if ( identidx && strequ(identbuf, "EXPORTS") ){
    EXPORT_SOURCE_MASK &= EMASK_SOURCE_EDF;
    skip_blanks(false);
    for(;!eof;){
      /* Determine external name */
      get_ident(false);
      if (identidx){
        extname = NAMES.Str2Index(identbuf, identidx);
        skip_blanks(false);

        if (buf[pos] == '='){
          pos++;
          skip_blanks(false);

          /* Determine module name */
          if (buf[pos] == '('){
            pos++;
            skip_blanks(false);
            get_ident(true);
            strcat(identbuf, ".dll");
            modname = NAMES.Str2Index(identbuf, (identidx + 4) );
            skip_blanks(false);
            if (buf[pos] == ')'){
              pos++;
              skip_blanks(false);
            }else{
              Message(xFATAL, msgEXPECT_RBRACKET, name);
            };
          }else{
            modname = INVALID_ID;
          };

          /* Determine internal name */
          get_ident(true);
          intname = NAMES.Str2Index(identbuf, identidx);
          skip_blanks(false);
        }else{
          intname = extname;
          modname = INVALID_ID;
        };

        /* Determine ordinal number */
        if (buf[pos] == '@'){
          i = pos + 1;
          ordinal = (word) strtoul (buf+pos+1, &end, 10);
          if (end == (buf+pos+1)){
              Message(xFATAL, msgORDINAL_EXPECT, name, line, i + 1);
          }
          pos = end - buf;
        }else{
          ordinal = 0;
        };

        NewExport(extname, intname, NULL, 0, ordinal,
                  EFLAG_SOURCE_EDFMOD, modname);

        if (modname != INVALID_ID)
            NewImportName (intname, K_IMPORT | K_IMPFUNC | K_USED, modname, /*FIXME*/(int)intname);

        skip_blanks(false);
      }else{
        skip_blanks(false);
        break;
      };
    };
  };
  if (!eof) Message(xFATAL, msgEOF_EXPECT, name, line, pos + 1);

  delete file;
}

close_namespace

