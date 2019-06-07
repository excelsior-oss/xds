/***    Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   Lexical analizier for CTRout(CityRout) - Compiler Testing Routine
                                                       Den: 9-Aug-96
*/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <limits.h>
#include <string.h>

#include "ctScan.h"
#include "ctLimits.h"
#include "ctAssert.h"
#include "ctMagic.h"
#include "ctMemory.h"


#define EOI 0
#define NUM_BASE 10

DeclareMagic(CTSCAN_MAGIC);

#define MAGIC_ASSERT(x) ASSERT ( ( (x)->magic == CTSCAN_MAGIC ) );

struct  Scan_Scanner_t {
  FILE * src;
  char buff [ IO_BUFFER_SIZE + 1 ];
  long idx, datalen;
  long line;
  char                  Ch, nxtCh;
  Scan_Symbol_t         sym;
  long                  IntVal;
  char                  Str [ MAX_STRING_LENGTH + 1 ];
  ErrCodes_ErrorCode_t  error;
  long magic;
  char * idents [ MAX_VARS_NUMBER ]; /* pseudo hash */
  long ident_idx;
};

  void  Scan_ResetIdents  ( Scan_Scanner_t * s )
  {
    s->ident_idx = 0;
  }


  Scan_Symbol_t  Scan_Symbol ( Scan_Scanner_t * s )
  {
    MAGIC_ASSERT(s);
    return s->sym;
  }

  char *  Scan_StringValue ( Scan_Scanner_t * s )
  {
    MAGIC_ASSERT(s);
    ASSERT ( s->sym == sym_Stringval );
    return s->Str;
  }

  long Scan_IdentName ( Scan_Scanner_t * s )
  {
    MAGIC_ASSERT(s);
    ASSERT ( s->sym == sym_Ident );
    return s->IntVal;
  }

  long  Scan_IntegerValue( Scan_Scanner_t * s )
  {
    MAGIC_ASSERT(s);
    ASSERT ( s->sym == sym_Integerval );
    return s->IntVal;
  }

  long  Scan_Line( Scan_Scanner_t * s )
  {
    MAGIC_ASSERT(s);
    return s->line;
  }

  ErrCodes_ErrorCode_t  Scan_ErrorCode( Scan_Scanner_t * s )
  {
    MAGIC_ASSERT(s);
    ASSERT ( s->sym == sym_ERROR );
    return s->error;
  }


  static void skipCh ( Scan_Scanner_t * s )
  {
    s->Ch = s->nxtCh;
    if ( s->Ch == EOI ) return;
    if ( s->Ch == '\n' ) s->line ++ ;
    if ( s->datalen == s->idx )
    {
      if ( feof ( s->src ) ) { s->nxtCh=EOI; return; }
      s->datalen = fread ( s->buff, 1, IO_BUFFER_SIZE, s->src );
      ASSERT ( s->datalen != 0 );
      s->idx = 0;
    }
    s->nxtCh = s->buff [ s->idx ++ ];
  }

  static void SkipWhiteSpaces(Scan_Scanner_t * s)
  {
    int lev;

    while ( 1 )
    {
      switch ( s->Ch )
      {
        case ' '  :
        case '\f' :
        case '\n' :
        case '\r' :
        case '\t' :
        case '\v' :  while ( isspace ( s->Ch ) ) skipCh(s);  break;
        case '-'  :  if ( s->nxtCh == '-' ) while ( s->Ch != '\n' && s->Ch != EOI ) skipCh(s);
                     else return;
                     break;
        case '('  :  if ( s->nxtCh == '*' )
                     {
                       skipCh(s);
                       skipCh(s);
                       lev = 1;
                       while ( lev )
                       {
                         if ( s->Ch == '*' && s->nxtCh == ')' )
                         {
                            skipCh(s);
                            lev --;
                         } else if ( s->Ch == '(' && s->nxtCh == '*' )
                         {
                            skipCh(s);
                            lev ++;
                         }
                         skipCh(s);
                       }
                     } else return;
                     break;
        default   :  return;
      }     /* switch */
    }       /* while */
  }


    struct { Scan_Symbol_t sym; char * str; } reswrds [ ] =
    {
              sym_And,                "AND",
              sym_Begin,              "BEGIN",
              sym_Bypass,             "BYPASS",
              sym_Cfgtemplate,        "CFGTEMPLATE",
              sym_Comment,            "COMMENT",
              sym_Compile,            "COMPILE",
              sym_Compileerror,       "COMPILE_ERROR",
              sym_Compileok,          "COMPILE_OK",
              sym_Deftemplate,        "DEFTEMPLATE",
              sym_Div,                "DIV",
              sym_Do,                 "DO",
              sym_Else,               "ELSE",
              sym_Elsif,              "ELSIF",
              sym_End,                "END",
              sym_Error,              "ERROR",
              sym_Exit,               "EXIT",
              sym_Generate,           "EXPECT",
              sym_Extension,          "EXTENSION",
              sym_False,              "FALSE",
              sym_Feature,            "FEATURE",
              sym_First,              "FIRST",
              sym_For,                "FOR",
              sym_Foreach,            "FOREACH",
              sym_Generate,           "GENERATE",
              sym_If,                 "IF",
              sym_In,                 "IN",
              sym_Integer,            "INTEGER",
              sym_Loop,               "LOOP",
              sym_Nofeature,          "NOFEATURE",
              sym_Ok,                 "OK",
              sym_Or,                 "OR",
              sym_Outputs,            "OUTPUT",
              sym_Procedure,          "PROCEDURE",
              sym_Return,             "RETURN",
              sym_Run,                "RUN",
              sym_Runerror,           "RUN_ERROR",
              sym_Runok,              "RUN_OK",
              sym_Size,               "SIZE",
              sym_Stdin,              "STDIN",
              sym_Stdout,             "STDOUT",
              sym_String,             "STRING",
              sym_Suffix,             "SUFFIX",
              sym_Template,           "TEMPLATE",
              sym_Testprefix,         "TESTPREFIX",
              sym_Text,               "TEXT",
              sym_Then,               "THEN",
              sym_To,                 "TO",
              sym_True,               "TRUE",
              sym_Var,                "VAR",
              sym_Write,              "WRITE",
              sym_Writeln,            "WRITELN"
    };


  Scan_Symbol_t LookForReserved ( char * name )
  {
    long len, i, res, lt, rt;
    len = sizeof ( reswrds ) / sizeof ( reswrds[0] );
    i = len;
    res = -1;
    lt  = i + 1;
    rt = 0;
    do
    {
      if ( res < 0 )
      {
        rt = lt / 2;
        lt = lt - rt;
        i = i - rt;
      }
      else
      {
        lt = rt / 2;
        rt = rt - lt;
        i = i + lt;
      }
      res = strcmp ( name, reswrds[i].str );
      if ( ! res ) return reswrds[i].sym;
    } while ( lt > 1 || rt > 1);
    return sym_Ident;
  }


  void Scan_Skip( Scan_Scanner_t * s )
  {
    char tCh;
    long dig, i;
    char ident [ MAX_IDENT_LENGTH ];
    MAGIC_ASSERT(s);
    s->Str [ 0 ] = 0;
    s->IntVal = 0;
    if ( s->sym == sym_ERROR ) return;
    SkipWhiteSpaces(s);
    switch ( s->Ch )
    {
      case EOI  :  s->sym = sym_Eof;                  break;
      case '+'  :  s->sym = sym_Plus;      skipCh(s);  break;
      case '-'  :  s->sym = sym_Minus;     skipCh(s);  break;
      case '*'  :  s->sym = sym_Mul;       skipCh(s);  break;
      case '?'  :  s->sym = sym_Question;  skipCh(s);  break;
      case '('  :  s->sym = sym_Lparen;    skipCh(s);  break;
      case ')'  :  s->sym = sym_Rparen;    skipCh(s);  break;
      case '['  :  s->sym = sym_Lbrack;    skipCh(s);  break;
      case ']'  :  s->sym = sym_Rbrack;    skipCh(s);  break;
      case '#'  :  s->sym = sym_Ne;        skipCh(s);  break;
      case '&'  :  s->sym = sym_And;       skipCh(s);  break;
      case ','  :  s->sym = sym_Comma;     skipCh(s);  break;
      case ';'  :  s->sym = sym_Semicolon; skipCh(s);  break;
      case '='  :  s->sym = sym_Eq;        skipCh(s);  break;
      case ':'  :  s->sym = ( s->nxtCh == '=' )
                          ? ( skipCh(s), sym_Becomes )
                          : ( sym_Colon             );
                   skipCh(s);
                   break;
      case '<'  :  s->sym = ( s->nxtCh == '=' )
                          ? ( skipCh(s), sym_Le )
                          : ( sym_Lt           );
                   skipCh(s);
                   break;
      case '>'  :  s->sym = ( s->nxtCh == '=' )
                       ? ( skipCh(s), sym_Ge )
                       : ( sym_Gt           );
                   skipCh(s);
                   break;
      case '\"' :
      case '\'' :  tCh = s->Ch; i = 0; skipCh(s);
                   while( s->Ch != tCh )
                   {
                     if ( s->Ch == EOI )
                     {
                       s->sym = sym_ERROR;
                       s->error = scerr_UnterminatedString;
                       return;
                     }
                     if ( i < MAX_STRING_LENGTH ) s->Str [ i++ ] = s->Ch;
                     skipCh (s);
                   }
                   s->Str [ i ] = 0;
                   s->sym = sym_Stringval;
                   skipCh(s);
                   break;
      case '0'  :
      case '1'  :
      case '2'  :
      case '3'  :
      case '4'  :
      case '5'  :
      case '6'  :
      case '7'  :
      case '8'  :
      case '9'  :  while ( isdigit ( s->Ch ) )
                   {
                      dig = s->Ch - '0';
                      if ( ( s->IntVal > ( LONG_MAX / NUM_BASE ) ) || ( ( s->IntVal *= NUM_BASE ) > LONG_MAX - dig ) )
                      {
                        s->sym = sym_ERROR;
                        s->error = scerr_IntegerOverflow;
                        return;
                      }
                      s->IntVal += dig;
                      skipCh(s);
                   }
                   s->sym = sym_Integerval;
                   break;
      default   :  if ( isalpha ( s->Ch ) || s->Ch == '_' )
                   {
                     i = 0;
                     while ( isalnum ( s->Ch ) || s->Ch == '_' )
                     {
                       if ( i < MAX_IDENT_LENGTH ) ident [ i++ ] = /*toupper */( s->Ch );
                       skipCh(s);
                     }
                     ident [ i ] = 0;
                     if ( sym_Ident == ( s->sym = LookForReserved ( ident ) ) )
                     {
                       for ( i = 0 ; (i<s->ident_idx) && (strcmp(ident,s->idents[i])) ; i++ );
                       if ( i >= s->ident_idx )
                       {
                         ASSERTM( s->ident_idx < MAX_VARS_NUMBER, "\"Maximum number of variables exceeded\"" );
                         s->idents [ s->IntVal =  s->ident_idx++ ] =
                         strcpy ( _allocate(char, strlen(ident)), ident );
                       }
                       else
                       {
                         s->IntVal = i;
                       }
                     }
                   } else
                   {
                     s->sym = sym_ERROR;
                     s->error = scerr_UnkownCharacter;
                     return;
                   }
                   break;
    }
  }


  Scan_Scanner_t * Scan_Open ( char *name )
  {
    Scan_Scanner_t * s;
    FILE * src;

    if ( NULL == ( src = fopen ( name, "r" ) ) ) return 0;

    s = allocate( Scan_Scanner_t );
    s->src = src;
    s->line = 1;
    s->datalen = 0;
    s->idx = 0;
    s->Ch = ' ';
    s->nxtCh = ' ';
    s->magic = SetMagic(CTSCAN_MAGIC);
    s->ident_idx = 0;
    skipCh(s);
    skipCh(s);
    Scan_Skip(s);

    return s;
  }


  void Scan_Close ( Scan_Scanner_t * s )
  {
    MAGIC_ASSERT(s);
    s -> magic = 0;
    fclose ( s -> src );
  }

char * IdentName(Scan_Scanner_t * s, long i){
  ASSERTM(i<s->ident_idx,"\"Index out of range\"");
  return s->idents[i];
}


long Scan_GetIdName( Scan_Scanner_t * s, char * name ){
  long i = 0;

  MAGIC_ASSERT(s);
  while( i < s->ident_idx && strcmp( s->idents[i], name ) ) i++;
  return i > s->ident_idx ? -1 : i;
};


long Scan_SetIdName( Scan_Scanner_t * s, char * name ){
  long i = 0;

  MAGIC_ASSERT(s);
  if ( sym_Ident == LookForReserved ( name ) ){
    for ( i = 0 ; (i<s->ident_idx) && (strcmp(name,s->idents[i])) ; i++ );
    if ( i >= s->ident_idx ){
      ASSERTM( s->ident_idx < MAX_VARS_NUMBER, "\"Maximum number of variables exceeded\"" );
      s->idents [ s->ident_idx ] = strcpy ( _allocate(char,strlen(name)), name );
      return s->ident_idx++;
    }else{
      return i;
    };
  }else{
    return -1;
  };
};


char * Scan_SymName(Scan_Symbol_t s){
  char*p;
  switch ( s )
  {
    case sym_Stdin:           p="STDIN";  break;
    case sym_Stdout:          p="STDOUT"; break;
    case sym_Outputs:         p="OUTPUTS"; break;
    case sym_Text:            p="TEXT";   break;

    case sym_Testprefix:      p="TESTPREFIX";   break;
    case sym_Template:        p="TEMPLATE";     break;
    case sym_Deftemplate:     p="DEFTEMPLATE";  break;
    case sym_Cfgtemplate:     p="CFGTEMPLATE";  break;
    case sym_Suffix:          p="SUFFIX";       break;
    case sym_Begin:           p="BEGIN";        break;
    case sym_End:             p="END";          break;
    case sym_Comment:         p="COMMENT";      break;
    case sym_Compile:         p="COMPILE";      break;
    case sym_Generate:        p="GENERATE";     break;
    case sym_Bypass:          p="BYPASS";       break;
    case sym_For:             p="FOR";          break;
    case sym_Foreach:         p="FOREACH";      break;
    case sym_In:              p="IN";           break;
    case sym_Do:              p="DO";           break;
    case sym_To:              p="TO";           break;
    case sym_If:              p="IF";           break;
    case sym_Elsif:           p="ELSIF";        break;
    case sym_Else:            p="ELSE";         break;
    case sym_Then:            p="THEN";         break;
    case sym_Eq:              p="=";            break;
    case sym_Ne:              p="#";            break;
    case sym_Gt:              p=">";            break;
    case sym_Lt:              p="<";            break;
    case sym_Ge:              p=">=";           break;
    case sym_Le:              p="<=";           break;
    case sym_Becomes:         p=":=";           break;
    case sym_Ident:           p="<<IDENT>>";    break;
    case sym_Stringval:       p="<<STRING>>";   break;
    case sym_Integerval:      p="<<INTEGER>>";  break;
    case sym_Semicolon:       p=";";            break;
    case sym_Comma:           p=",";            break;
    case sym_Colon:           p=":";            break;
    case sym_Question:        p="?";            break;
    case sym_Lparen:          p="(";            break;
    case sym_Lbrack:          p="[";            break;
    case sym_Rparen:          p=")";            break;
    case sym_Rbrack:          p="]";            break;
    case sym_First:           p="FIRST";        break;
    case sym_Feature:         p="FEATURE";      break;
    case sym_Nofeature:       p="NOFEATURE";    break;
    case sym_Size:            p="SIZE";         break;
    case sym_Mul:             p="*";            break;
    case sym_Div:             p="DIV";          break;
    case sym_And:             p="&";            break;
    case sym_Plus:            p="+";            break;
    case sym_Minus:           p="-";            break;
    case sym_Or:              p="OR";           break;
    case sym_Eof:             p="<<EOF>>";      break;
    case sym_Run:             p="RUN";          break;
    case sym_Runok:           p="RUN_OK";       break;
    case sym_Compileok:       p="COMPILE_OK";   break;
    case sym_Compileerror:    p="COMPILE_ERROR";break;
    case sym_Runerror:        p="RUN_ERROR";    break;
    case sym_Var:             p="VAR";          break;
    case sym_Write:           p="WRITE";        break;
    case sym_Writeln:         p="WRITELN";      break;
    default:   printf(" {%d}\n",s); ASSERT(0);
  }
  return p;
}
