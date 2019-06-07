/***    Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   Lexical analizier for CTROUT(CityRout) - Compiler Testing Routine
                                                       Den: 9-Aug-96
*/
#ifndef __ctScan_h
#define __ctScan_h

#include "ctErrCodes.h"


  typedef enum
  {
              sym_And,
              sym_Becomes,
              sym_Begin,
              sym_Bypass,
              sym_Cfgtemplate,
              sym_Colon,
              sym_Comma,
              sym_Comment,
              sym_Compile,
              sym_Compileerror,
              sym_Compileok,
              sym_Deftemplate,
              sym_Div,
              sym_Do,
              sym_ERROR,
              sym_Elsif,
              sym_Else,
              sym_End,
              sym_Eof,
              sym_Eq,
              sym_Error,
              sym_Exit,
              sym_Extension,
              sym_False,
              sym_Feature,
              sym_First,
              sym_For,
              sym_Foreach,
              sym_Ge,
              sym_Generate,
              sym_Gt,
              sym_Ident,
              sym_If,
              sym_In,
              sym_Integer,
              sym_Integerval,
              sym_Lbrack,
              sym_Le,
              sym_Loop,
              sym_Lparen,
              sym_Lt,
              sym_Minus,
              sym_Mul,
              sym_Ne,
              sym_Nofeature,
              sym_Ok,
              sym_Or,
              sym_Outputs,
              sym_Plus,
              sym_Procedure,
              sym_Question,
              sym_Rbrack,
              sym_Return,
              sym_Rparen,
              sym_Run,
              sym_Runerror,
              sym_Runok,
              sym_Semicolon,
              sym_Size,
              sym_Stdin,
              sym_Stdout,
              sym_String,
              sym_Stringval,
              sym_Suffix,
              sym_Template,
              sym_Testprefix,
              sym_Text,
              sym_Then,
              sym_To,
              sym_True,
              sym_Var,
              sym_Write,
              sym_Writeln
  } Scan_Symbol_t;


  typedef struct Scan_Scanner_t Scan_Scanner_t;  /* hidden type */


  extern Scan_Scanner_t *      Scan_Open         ( char *name );
  extern void                  Scan_Close        ( Scan_Scanner_t * s );

  extern void                  Scan_Skip         ( Scan_Scanner_t * s );

  extern Scan_Symbol_t         Scan_Symbol       ( Scan_Scanner_t * s );
  extern char *                Scan_StringValue  ( Scan_Scanner_t * s );
  extern long                  Scan_IdentName    ( Scan_Scanner_t * s );
  extern long                  Scan_GetIdName    ( Scan_Scanner_t * s, char * name );
  extern long                  Scan_SetIdName    ( Scan_Scanner_t * s, char * name );
  extern long                  Scan_IntegerValue ( Scan_Scanner_t * s );
  extern long                  Scan_Line         ( Scan_Scanner_t * s );
  extern ErrCodes_ErrorCode_t  Scan_ErrorCode    ( Scan_Scanner_t * s );
  void                         Scan_ResetIdents  ( Scan_Scanner_t * s );
  extern char *                Scan_SymName      ( Scan_Symbol_t s);


#endif /* __ctScan_h */
