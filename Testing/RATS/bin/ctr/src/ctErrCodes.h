/***	Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   Error codes for CTRout(CityRout) - Compiler Testing Routine
												 Alexs: 4-Nov-96
*/
#ifndef __ctErrorCodes
#define __ctErrorCodes

  typedef enum
  {
    scerr_TooManyOutFiles,
	scerr_Duplicate_Stdin,
	scerr_Duplicate_Stdout,
	scerr_UnterminatedString,
	scerr_UnterminatedComment,
	scerr_UnkownCharacter,
	scerr_IntegerOverflow,

	prerr_Expected,
	prerr_UndeclaredIdent,
	prerr_DuplicateIdent,
	prerr_ActualParamMustBeLValue,
	inerr_TypeMismatch,
	inerr_IllegalSliceIdxs,
	inerr_BadConstant,
	inerr_ExitWithoutLoop,
	inerr_IllegalAssignment,
	inerr_TooFewValues,
	inerr_ReturnWithoutCall,

	exerr_CantOpenFile,
	exerr_CantCloseFile,
	exerr_CantWriteToFile,

	exerr_NoCompilerCmd,
	exerr_NoCCompilerCmd,
	exerr_NoLinkerCmd,
	exerr_NoMakerCmd,
	exerr_NoRunnerCmd,
	exerr_NoAssemblerCmd,

    cfgwrn_SyntaxError,

	undefined_error
  } ErrCodes_ErrorCode_t;

  typedef enum
  {
	cfgwrn_UndefinedSizeArgument,

	undefined_warning
  } ErrCodes_WarningCode_t;


#endif /* __ctErrorCodes */
