/***	Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   Error manager for CTRout(CityRout) - Compiler Testing Routine
												 Alexs: 4-Nov-96
*/

#include <stdio.h>
#include <stdlib.h>

#include "ctErrors.h"

void Errors_Error(ErrCodes_ErrorCode_t error, long lineno, char * additional_info, FILE * report){
  char * error_text;

  switch (error){

        case scerr_TooManyOutFiles :
		 error_text =			   "Too many outfiles";
		 break;
        case scerr_Duplicate_Stdin :
		 error_text =			   "Duplicate Stdin";
		 break;
        case scerr_Duplicate_Stdout :
		 error_text =			   "Duplicate Stdout";
		 break;

	case scerr_UnterminatedString:
		 error_text =			   "Unterminated string";
		 break;
	case scerr_UnterminatedComment:
		 error_text =			   "Unterminated comment";
		 break;
	case scerr_UnkownCharacter:
		 error_text =			   "Unkown character";
		 break;
	case scerr_IntegerOverflow:
		 error_text =			   "Integer overflow";
		 break;
	case prerr_Expected:
		 error_text =			   "Expected";
		 break;
	case prerr_UndeclaredIdent:
		 error_text =			   "Undeclared identifier";
		 break;
	case prerr_DuplicateIdent:
		 error_text =			   "Duplicate identifier";
		 break;
	case prerr_ActualParamMustBeLValue:
		 error_text =			   "Actual param must be lvalue";
		 break;
	case inerr_TypeMismatch:
		 error_text =			   "Type mismatch";
		 break;
	case inerr_IllegalSliceIdxs:
		 error_text =			   "Illegal slice indexes";
		 break;
	case inerr_BadConstant:
		 error_text =			   "Bad constant";
		 break;
	case inerr_ExitWithoutLoop:
		 error_text =			   "Exit without loop";
		 break;
	case inerr_IllegalAssignment:
		 error_text =			   "Illegal assignment";
		 break;
	case inerr_TooFewValues:
		 error_text =			   "Too few values";
		 break;
	case inerr_ReturnWithoutCall:
		 error_text =			   "Return without call";
		 break;
	case exerr_CantOpenFile:
		 error_text =			   "Can't open file";
		 break;
	case exerr_CantCloseFile:
		 error_text =			   "Can't close file";
		 break;
	case exerr_CantWriteToFile:
		 error_text =			   "Can't write to file";
		 break;
	case exerr_NoCompilerCmd:
		 error_text =              "There is no compile command";
		 break;
	case exerr_NoCCompilerCmd:
		 error_text =              "There is no C compile command";
		 break;
	case exerr_NoMakerCmd:
		 error_text =              "There is no make command";
		 break;
	case exerr_NoRunnerCmd:
		 error_text =              "There is no run command";
		 break;
	case exerr_NoAssemblerCmd:
		 error_text =              "There is no assemble command";
		 break;
	case exerr_NoLinkerCmd:
        	 error_text =              "There is no link command";
		 break;
	case cfgwrn_SyntaxError:
        	 error_text =              "Syntax error";
		 break;
	case undefined_error: default:
		 error_text =			   "Undefined error";
  };
  printf("\nError [%d]: %s %s\n", lineno, error_text, additional_info?additional_info:"");
  if ( report ) {
	fprintf(report, "\nError [%d]: %s %s\n", lineno, error_text, additional_info?additional_info:"");
    fclose(report);
  }
  exit(1);
};

void Errors_Warning(ErrCodes_WarningCode_t warning, long lineno, char * additional_info, FILE * report){
  char * warning_text;

  switch (warning){
	case cfgwrn_UndefinedSizeArgument:
		 warning_text = 			 "Undefined SIZE argument";
		 break;
	case undefined_warning: default:
		 warning_text = 			 "Undefined warnign";
  };
  printf("\nWarnign [%d]: %s %s\n", lineno, warning_text, additional_info?additional_info:"");
  if ( report ) fprintf(report, "\nWarnign [%d]: %s %s\n", lineno, warning_text, additional_info?additional_info:"");
};

