/***	Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   Configurator for CTROUT(CityRout) - Compiler Testing Routine
											   Alexs: 18-Nov-96
*/

#ifndef __ctCfg_h
#define __ctCfg_h


  typedef struct Cfg_Configurator_t Cfg_Configurator_t;  /* hidden type */


  #define ctCfg_TestCompile 0
  #define ctCfg_TestRun     1
  #define ctCfg_TestAll     2
  #define ctCfg_TestComment 3
  #define ctCfg_TestCompare 4

  #define ctCfg_CleanNone   0
  #define ctCfg_CleanAll    1
  #define ctCfg_CleanPassed 2

  #define ctCfg_StdIOFmtCrLf 0
  #define ctCfg_StdIOFmtLf   1
  


  extern Cfg_Configurator_t * Cfg_Init		( char * name );
  extern void		      Cfg_Final 	( Cfg_Configurator_t * c );

  extern long		      Cfg_Size		( Cfg_Configurator_t * c, char * string );
  extern long	              Cfg_Feature	( Cfg_Configurator_t * c, char * string );
  extern long                 Cfg_Nofeature ( Cfg_Configurator_t * c, char * string );

  extern char               * Cfg_GetFileName    ( Cfg_Configurator_t * c, char * name, char * ext, int mode );

  extern char               * Cfg_GetLinkerCmd   ( Cfg_Configurator_t * c );
  extern char               * Cfg_GetMakerCmd    ( Cfg_Configurator_t * c );
  extern char               * Cfg_GetRunnerCmd   ( Cfg_Configurator_t * c );
  extern char               * Cfg_GetCompilerCmd ( Cfg_Configurator_t * c );
  extern char               * Cfg_GetCompiler1Cmd( Cfg_Configurator_t * c );
  extern char               * Cfg_GetCCompilerCmd( Cfg_Configurator_t * c );
  extern char               * Cfg_GetAssemblerCmd( Cfg_Configurator_t * c );

  extern long                 Cfg_GetRunCodeShift     ( Cfg_Configurator_t * c );
  extern long                 Cfg_GetCompileCodeShift ( Cfg_Configurator_t * c );

  extern long                 Cfg_GetMakeOkCode    ( Cfg_Configurator_t * c );
  extern long                 Cfg_GetCompileOkCode ( Cfg_Configurator_t * c );
  extern long                 Cfg_GetRunOkCode     ( Cfg_Configurator_t * c );

  extern long                 Cfg_GetClean         ( Cfg_Configurator_t * c );

  extern long                 Cfg_GetTest          ( Cfg_Configurator_t * c );
  extern long                 Cfg_GetTestCompile   ( Cfg_Configurator_t * c );
  extern long                 Cfg_GetTestRun       ( Cfg_Configurator_t * c );

  extern char              *  Cfg_GetInetAddress   ( Cfg_Configurator_t * c );
  extern long                 Cfg_GetRemote        ( Cfg_Configurator_t * c );

  extern long                 Cfg_GetStdIOFmt      ( Cfg_Configurator_t * c );

#endif /* __ctCfg_h */

