/***	Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   Configurator for CTROUT(CityRout) - Compiler Testing Routine
											   Alexs: 18-Nov-96
*/

#include "ctAssert.h"
#include "ctMagic.h"
#include "ctErrors.h"
#include "ctStrs.h"
#include "ctMemory.h"
#include "ctScan.h"
#include "lists.h"
#include "ctCfg.h"
#include "ctNode.h"

#include "string.h"

DeclareMagic(CTCFG_MAGIC);
DeclareMagic(SizeMagic);
DeclareMagic(FeatureMagic);
DeclareMagic(NofeatureMagic);
DeclareMagic(LookupMagic);

#define MAGIC_ASSERT(x) 	  ASSERT ( ( (x)->magic == CTCFG_MAGIC ) );
#define Symbol			  (Scan_Symbol(s))
#define NextSymbol		  (Scan_Skip(s),((Symbol == sym_ERROR)?(ERROR(Scan_ErrorCode(s),0),0):Symbol))
#define Expected(what)		  ((Symbol == what)?(Symbol):(ERROR(prerr_Expected, Scan_SymName(what)),0))
#define SkipExpected(what)	  ((Symbol == what)?(NextSymbol):(ERROR(prerr_Expected, Scan_SymName(what)),0))
#define ERROR(no, addinfo)        Errors_Error((no), Scan_Line(s), addinfo, NULL)
#define WARNING(no, addinfo)      Errors_Warning((no), 153, addinfo, NULL)
#define String(str) 		  (ctStrs_COPY((str), Scan_StringValue(s)))
#define Integer 		  (Scan_IntegerValue(s))
#define IdentName		  (Scan_IdentName(s))

#define nothing_number 0
#define path_number    1
#define name_number    2
#define ext_number     3

struct Cfg_Configurator_t{

  long		 magic;

  List_t  *  features;
  List_t  *  nofeatures;
  List_t  *  sizes;
  List_t  *  lookups;

  char    *  compiler_cmd,
	  *  compiler1_cmd,
	  *  ccompiler_cmd,
	  *  assembler_cmd,
	  *  maker_cmd,
	  *  runner_cmd,
	  *  linker_cmd,
	  *  inet_addr;

  long       compile_ok_code, run_ok_code, make_ok_code,
             compile_code_shift, run_code_shift, clean,
             test, test_compile, test_run, remote, stdiofmt;


  struct {
	char * name_pattern;
	long   order[4];
  } fs;

};


typedef struct size_node_t{
  char * string;
  long	 size;
} size_node_t;

typedef struct lookup_node_t{
  char * ext;
  char * path;
} lookup_node_t;


static char default_path[] = "";
static char default_name[] = "%s\\%s.%s";
static char default_ext [] = "mod";


/*---------------------------------------------------------*/
Cfg_Configurator_t * Cfg_Init( char * name ){
  Cfg_Configurator_t * cfg = NULL;
  Scan_Scanner_t     * s   = NULL;
  long		       go_on = 1;
  long		       path_pattern, name_pattern, pathid, nameid, extid,
                       linker_cmdid, compiler_cmdid, compiler1_cmdid, ccompiler_cmdid, 
		       assembler_cmdid, maker_cmdid, runner_cmdid,
		       make_ok_codeid, test_compileid, test_runid, testid, cleanid, 
                       allid, compareid, noneid, passedid,
                       compile_code_shiftid, run_code_shiftid, inet_addrid, remoteid,
					   stdiofmt, stdiofmtcrlf, stdiofmtlf;

  size_node_t	 * size        = NULL;
  lookup_node_t  * lookup      = NULL;
  char		 * feature     = NULL;
  char		 * nofeature   = NULL;
  long		   order_count = 0;

  if ( NULL == ( s = Scan_Open( name ) ) ) return NULL;
  path_pattern         = Scan_SetIdName( s, "PATH" );
  name_pattern         = Scan_SetIdName( s, "NAME" );
  pathid               = Scan_SetIdName( s, "path" );
  nameid               = Scan_SetIdName( s, "name" );
  extid                = Scan_SetIdName( s, "ext" );
  linker_cmdid         = Scan_SetIdName( s, "LINKER" );
  compiler_cmdid       = Scan_SetIdName( s, "COMPILER" );
  compiler1_cmdid      = Scan_SetIdName( s, "COMPILER1" );
  ccompiler_cmdid      = Scan_SetIdName( s, "CCOMPILER" );
  maker_cmdid          = Scan_SetIdName( s, "MAKER" );
  assembler_cmdid      = Scan_SetIdName( s, "ASSEMBLER" );
  runner_cmdid         = Scan_SetIdName( s, "RUNNER" );
  make_ok_codeid       = Scan_SetIdName( s, "MAKE_OK" );
  compile_code_shiftid = Scan_SetIdName( s, "COMPILE_CODE_SHIFT" );
  run_code_shiftid     = Scan_SetIdName( s, "RUN_CODE_SHIFT" );
  test_compileid       = Scan_SetIdName( s, "TEST_COMPILE" );
  test_runid           = Scan_SetIdName( s, "TEST_RUN" );
  testid               = Scan_SetIdName( s, "TEST" );
  cleanid              = Scan_SetIdName( s, "CLEAN" );
  allid                = Scan_SetIdName( s, "ALL" );
  noneid               = Scan_SetIdName( s, "NONE" );
  passedid             = Scan_SetIdName( s, "PASSED" );
  compareid            = Scan_SetIdName( s, "COMPARE" );
  inet_addrid          = Scan_SetIdName( s, "INET_ADDRESS" );
  remoteid             = Scan_SetIdName( s, "REMOTE" );
  stdiofmt             = Scan_SetIdName( s, "STDIOFMT" );
  stdiofmtcrlf         = Scan_SetIdName( s, "STDIOFMTCRLF" );
  stdiofmtlf           = Scan_SetIdName( s, "STDIOFMTLF" );


  cfg = allocate( Cfg_Configurator_t );

  cfg->magic = SetMagic(CTCFG_MAGIC);
  SetMagic(SizeMagic);
  SetMagic(FeatureMagic);
  SetMagic(NofeatureMagic);
  SetMagic(LookupMagic);

  cfg->features 	= List_Create( FeatureMagic );
  cfg->nofeatures	= List_Create( NofeatureMagic );
  cfg->sizes		= List_Create( SizeMagic );
  cfg->lookups		= List_Create( LookupMagic );
  cfg->fs.name_pattern = NULL;
  cfg->linker_cmd      = NULL;
  cfg->compiler_cmd    = NULL;
  cfg->compiler1_cmd   = NULL;
  cfg->ccompiler_cmd   = NULL;
  cfg->assembler_cmd   = NULL;
  cfg->maker_cmd       = NULL;
  cfg->runner_cmd      = NULL;
  cfg->inet_addr       = NULL;
  cfg->compile_ok_code = 0;
  cfg->run_ok_code     = 0;
  cfg->remote          = 0;
  cfg->clean           = 0;


  cfg->fs.order[0]	   = path_number;
  cfg->fs.order[1]	   = name_number;
  cfg->fs.order[2]	   = ext_number;
  cfg->fs.order[3]	   = nothing_number;

  while(go_on){
	switch ( Symbol ){
	  case sym_Size 	:
			 NextSymbol;
			 Expected(sym_Stringval);
			 size = allocate( size_node_t );
			 String( size->string );
			 NextSymbol;
			 SkipExpected( sym_Eq );
			 Expected(sym_Integerval);
			 size->size = Integer;
			 NextSymbol;
			 List_Insert( cfg->sizes, SizeMagic, size );
			 break;
	  case sym_Feature	:
			 NextSymbol;
			 Expected(sym_Stringval);
			 String( feature );
			 NextSymbol;
			 List_Insert( cfg->features, FeatureMagic, feature );
			 feature = NULL;
			 break;
	  case sym_Nofeature:
			 NextSymbol;
			 Expected(sym_Stringval);
			 String( nofeature );
			 NextSymbol;
			 List_Insert( cfg->nofeatures, NofeatureMagic, nofeature );
			 nofeature = NULL;
			 break;
	  case sym_Ident:
			 if( IdentName == path_pattern ){
			   NextSymbol;
			   Expected(sym_Stringval);
			   lookup = allocate( lookup_node_t );
			   String( lookup->ext );
			   NextSymbol;
			   SkipExpected(sym_Eq);
			   Expected(sym_Stringval);
			   String( lookup->path );
			   List_Insert( cfg->lookups, LookupMagic, lookup );
			   NextSymbol;
			 }else if ( IdentName == name_pattern ){
			   NextSymbol;
			   SkipExpected(sym_Eq);
			   Expected(sym_Stringval);
			   String( cfg->fs.name_pattern );
			   NextSymbol;
			   do{
				 if ( order_count > 2 ) break;
				 SkipExpected(sym_Comma);
				 Expected(sym_Ident);
				 if ( IdentName == pathid ){
				   cfg->fs.order[order_count++] = path_number;
				 }else if ( IdentName == nameid ){
				   cfg->fs.order[order_count++] = name_number;
				 }else if ( IdentName == extid ){
				   cfg->fs.order[order_count++] = ext_number;
				 }else{
				   ERROR(cfgwrn_SyntaxError,0);
				 };
				 cfg->fs.order[order_count]   = nothing_number;
				 NextSymbol;
			   }while ( Symbol == sym_Comma );
			 }else if ( IdentName == linker_cmdid ){
			   NextSymbol;
			   SkipExpected(sym_Eq);
			   Expected(sym_Stringval);
			   String( cfg->linker_cmd );
			   NextSymbol;
			 }else if ( IdentName == maker_cmdid ){
			   NextSymbol;
			   SkipExpected(sym_Eq);
			   Expected(sym_Stringval);
			   String( cfg->maker_cmd );
			   NextSymbol;
			 }else if ( IdentName == assembler_cmdid ){
			   NextSymbol;
			   SkipExpected(sym_Eq);
			   Expected(sym_Stringval);
			   String( cfg->assembler_cmd );
			   NextSymbol;
			 }else if ( IdentName == runner_cmdid ){
			   NextSymbol;
			   SkipExpected(sym_Eq);
			   Expected(sym_Stringval);
			   String( cfg->runner_cmd );
			   NextSymbol;
			   break;
			 }else if ( IdentName == compiler_cmdid ){
			   NextSymbol;
			   SkipExpected(sym_Eq);
			   Expected(sym_Stringval);
			   String( cfg->compiler_cmd );
			   NextSymbol;
			   break;
			 }else if ( IdentName == compiler1_cmdid ){
			   NextSymbol;
			   SkipExpected(sym_Eq);
			   Expected(sym_Stringval);
			   String( cfg->compiler1_cmd );
			   NextSymbol;
			   break;
			 }else if ( IdentName == ccompiler_cmdid ){
			   NextSymbol;
			   SkipExpected(sym_Eq);
			   Expected(sym_Stringval);
			   String( cfg->ccompiler_cmd );
			   NextSymbol;
			   break;
		         }else if ( IdentName == compile_code_shiftid ){
			   NextSymbol;
			   SkipExpected(sym_Eq);
			   Expected(sym_Integerval);
		           cfg->compile_code_shift = Integer;
			   NextSymbol;
		         }else if ( IdentName == make_ok_codeid ){
			   NextSymbol;
			   SkipExpected(sym_Eq);
			   Expected(sym_Integerval);
 		           cfg->make_ok_code = Integer;
			   NextSymbol;
		         }else if ( IdentName == run_code_shiftid ){
			   NextSymbol;
			   SkipExpected(sym_Eq);
			   Expected(sym_Integerval);
			   cfg->run_code_shift = Integer;
			   NextSymbol;
		         }else if ( IdentName == test_compileid ){
			   NextSymbol;
			   SkipExpected(sym_Eq);
			   Expected(sym_Integerval);
 		           cfg->test_compile = Integer;
			   NextSymbol;
		         }else if ( IdentName == test_runid ){
			   NextSymbol;
			   SkipExpected(sym_Eq);
			   Expected(sym_Integerval);
		           cfg->test_run = Integer;
			   NextSymbol;
		         }else if ( IdentName == cleanid ){
			   NextSymbol;
			   SkipExpected(sym_Eq);
                           Expected(sym_Ident);
			   if(IdentName == allid){
                             cfg->clean = ctCfg_CleanAll;
			   }else if(IdentName == noneid){
                             cfg->clean = ctCfg_CleanNone;
			   }else if(IdentName == passedid){
                             cfg->clean = ctCfg_CleanPassed;
			   }else{
                             Expected(sym_Ident);
			   };
			   NextSymbol;

				}

				// Start STDIOFMT option parsing
				else if (IdentName == stdiofmt)
				{
					NextSymbol;
					SkipExpected (sym_Eq);
					Expected (sym_Ident);
					if (IdentName == stdiofmtcrlf)
						cfg->stdiofmt = ctCfg_StdIOFmtCrLf;
					else if (IdentName == stdiofmtlf)
						cfg->stdiofmt = ctCfg_StdIOFmtLf;
					else
						Expected(sym_Ident);
					NextSymbol;
				}
				// Finish STDIOFMT option parsing
		 
				else if ( IdentName == testid ){
			   NextSymbol;
			   SkipExpected(sym_Eq);
			   if(Symbol == sym_Run){
                             cfg->test = ctCfg_TestRun;
			   }else if(Symbol == sym_Compile){
                             cfg->test = ctCfg_TestCompile;
			   }else if(Symbol == sym_Comment){
                             cfg->test = ctCfg_TestComment;
			   }else if(Symbol == sym_Ident && IdentName == allid){
                             cfg->test = ctCfg_TestAll;
			   }else if(Symbol == sym_Ident && IdentName == compareid){
                             cfg->test = ctCfg_TestCompare;
			   }else{
                             Expected(sym_Run);
			   };
			   NextSymbol;
		         }else if ( IdentName == inet_addrid ){
			   NextSymbol;
			   SkipExpected(sym_Eq);
			   Expected(sym_Stringval);
			   String( cfg->inet_addr );
			   NextSymbol;
		         }else if ( IdentName == remoteid ){
			   NextSymbol;
			   SkipExpected(sym_Eq);
			   Expected(sym_Integerval);
		           cfg->remote = Integer;
			   NextSymbol;
		         }else{
			   ERROR(cfgwrn_SyntaxError,0);
		         };
	  		 break;
	  case sym_Compileok:
			 NextSymbol;
			 SkipExpected(sym_Eq);
			 Expected(sym_Integerval);
			 cfg->compile_ok_code = Integer;
			 NextSymbol;
			 break;
	  case sym_Runok:
			 NextSymbol;
			 SkipExpected(sym_Eq);
			 Expected(sym_Integerval);
			 cfg->run_ok_code = Integer;
			 NextSymbol;
			 break;
	  case sym_Eof:
			 go_on = 0;
			 break;
	  default:
			 ERROR(cfgwrn_SyntaxError,0);
	};
  };
  Scan_Close( s );

  if ( !cfg->fs.name_pattern ) ctStrs_COPY(cfg->fs.name_pattern, default_name);

  return cfg;
};

/*---------------------------------------------------------*/
void Cfg_Final( Cfg_Configurator_t * c ){

  MAGIC_ASSERT( c );
};

/*---------------------------------------------------------*/
long Cfg_Size( Cfg_Configurator_t * c, char * string ){
  size_node_t		 * size 	 = NULL;

  MAGIC_ASSERT( c );

  size = (size_node_t*)List_GetFirst( c->sizes, SizeMagic );
  while( size ){
	if ( strcmp( size->string, string ) == 0 ) return size->size;
	size = (size_node_t*)List_GetNext( c->sizes, SizeMagic );

  };
  WARNING( cfgwrn_UndefinedSizeArgument, 0 );
  return 0;
};

/*---------------------------------------------------------*/
long Cfg_Feature( Cfg_Configurator_t * c, char * string ){
  char * feature = NULL;

  MAGIC_ASSERT( c );

  feature = (char*)List_GetFirst( c->features, FeatureMagic );
  while( feature ){
	if ( strcmp( feature, string ) == 0 ) return 1;
	feature = (char*)List_GetNext( c->features, FeatureMagic );
  };
  return 0;
};

/*---------------------------------------------------------*/
long Cfg_Nofeature( Cfg_Configurator_t * c, char * string ){
  char * nofeature = NULL;

  MAGIC_ASSERT( c );

  nofeature = (char*)List_GetFirst( c->nofeatures, NofeatureMagic );
  while( nofeature ){
	if ( strcmp( nofeature, string ) == 0 ) return 1;
	nofeature = (char*)List_GetNext( c->nofeatures, NofeatureMagic );
  };
  return 0;
};

/*---------------------------------------------------------*/
char * Cfg_GetExePath( Cfg_Configurator_t * c , char * str){
  lookup_node_t * lookup = NULL;

  str = NULL;
  lookup = List_GetFirst( c->lookups, LookupMagic );
  while( lookup ){
	if ( strcmp( "exe", lookup->ext ) == 0 ){
	  ctStrs_COPY( str, lookup->path );
	  break;
	};
	lookup =  List_GetNext( c->lookups, LookupMagic );
  };
  if ( !str ) ctStrs_COPY( str, default_path );

  return str; 
}
/*---------------------------------------------------------*/
/*
  mode
   0  - ordinary
   1  - for text , stdout , stdin
   2  - without path

*/

char * Cfg_GetFileName ( Cfg_Configurator_t * c, char * name, char * ext , int mode){
  char * filename = NULL;
  char * buff	  = NULL;
  char * path	  = NULL;
  char * ext1     = NULL;
  long	 i = 0;
  lookup_node_t * lookup = NULL;

  MAGIC_ASSERT(c);

 
  ext = ext ? ext : default_ext;

  ctStrs_COPY( ext1 , ext);

  if (mode == 1){
    ext = "exe"; 
  }

  lookup = List_GetFirst( c->lookups, LookupMagic );
  while( lookup ){
	if ( strcmp( ext, lookup->ext ) == 0 ){
	  ctStrs_COPY( path, lookup->path );
	  break;
	};
	lookup =  List_GetNext( c->lookups, LookupMagic );
  };
  if ( !path ) ctStrs_COPY( path, default_path );

#ifdef ctHostOS_UNIX
  strcat( path , "/" );
#else 
  strcat( path , "\\" );
#endif

  ctStrs_COPY( ext , ext1);

  i = strlen(c->fs.name_pattern)+strlen(name)+strlen(ext)+strlen(path)+1;
  filename = _allocate( char, i );
  buff	   = _allocate( char, i );

  i = 0;
  strcpy( buff, c->fs.name_pattern );
  while( c->fs.order[i] != nothing_number ){
	switch( c->fs.order[i++] ){
	  case path_number:
                        if ( mode != 2 ){
                            sprintf( filename, buff, path, "%s", "%s" );
                	}else{
                    	    sprintf( filename, buff, "", "%s", "%s" );
                        }
			break;
	  case name_number:
                        sprintf( filename, buff, name, "%s", "%s" );
                        break;
	  case ext_number:
                        sprintf( filename, buff, ext, "%s", "%s" );
                        break;
	};
	strcpy( buff, filename );
  };
#ifdef ctHostOS_UNIX
  if(ext==NULL || ext[0]=='\000') {
    int filenameLen = strlen(filename);
    if(filename[filenameLen-1]=='.') {
	filename[filenameLen-1]='\000';
    }
  }
#endif
  return filename;
};

/*---------------------------------------------------------*/
char * Cfg_GetCompilerCmd( Cfg_Configurator_t * c ){

  MAGIC_ASSERT(c);
  return c->compiler_cmd;
};

/*---------------------------------------------------------*/
char * Cfg_GetCompiler1Cmd( Cfg_Configurator_t * c ){

  MAGIC_ASSERT(c);
  return c->compiler1_cmd;
};

/*---------------------------------------------------------*/
char * Cfg_GetCCompilerCmd( Cfg_Configurator_t * c ){

  MAGIC_ASSERT(c);
  return c->ccompiler_cmd;
};

/*---------------------------------------------------------*/
char * Cfg_GetLinkerCmd( Cfg_Configurator_t * c ){

  MAGIC_ASSERT(c);
  return c->linker_cmd;
};

/*---------------------------------------------------------*/
char * Cfg_GetMakerCmd( Cfg_Configurator_t * c ){

  MAGIC_ASSERT(c);
  return c->maker_cmd;
};

/*---------------------------------------------------------*/
char * Cfg_GetRunnerCmd( Cfg_Configurator_t * c ){

  MAGIC_ASSERT(c);
  return c->runner_cmd;
};

/*---------------------------------------------------------*/
char * Cfg_GetAssemblerCmd( Cfg_Configurator_t * c ){

  MAGIC_ASSERT(c);
  return c->assembler_cmd;
};

/*---------------------------------------------------------*/
long Cfg_GetCompileOkCode( Cfg_Configurator_t * c ){

  MAGIC_ASSERT(c);
  return c->compile_ok_code;
};

/*---------------------------------------------------------*/
long Cfg_GetRunOkCode( Cfg_Configurator_t * c ){

  MAGIC_ASSERT(c);
  return c->run_ok_code;
};

/*---------------------------------------------------------*/
long Cfg_GetMakeOkCode( Cfg_Configurator_t * c ){

  MAGIC_ASSERT(c);
  return c->make_ok_code;
};

/*---------------------------------------------------------*/
long Cfg_GetRunCodeShift( Cfg_Configurator_t * c ){

  MAGIC_ASSERT(c);
  return c->run_code_shift;
};

/*---------------------------------------------------------*/
long Cfg_GetCompileCodeShift( Cfg_Configurator_t * c ){

  MAGIC_ASSERT(c);
  return c->compile_code_shift;
};

/*---------------------------------------------------------*/
long Cfg_GetTestCompile( Cfg_Configurator_t * c ){

  MAGIC_ASSERT(c);
  return c->test_compile;
};

/*---------------------------------------------------------*/
long Cfg_GetTestRun( Cfg_Configurator_t * c ){

  MAGIC_ASSERT(c);
  return c->test_run;
};

/*---------------------------------------------------------*/
long Cfg_GetClean( Cfg_Configurator_t * c ){

  MAGIC_ASSERT(c);
  return c->clean;
};

/*---------------------------------------------------------*/
long Cfg_GetTest( Cfg_Configurator_t * c ){

  MAGIC_ASSERT(c);
  return c->test;
};

/*---------------------------------------------------------*/
char * Cfg_GetInetAddress( Cfg_Configurator_t * c ){

  MAGIC_ASSERT(c);
  return c->inet_addr;
};

/*---------------------------------------------------------*/
long Cfg_GetRemote( Cfg_Configurator_t * c ){

  MAGIC_ASSERT(c);
  return c->remote;
};

/*---------------------------------------------------------*/
long Cfg_GetStdIOFmt( Cfg_Configurator_t * c ){

  MAGIC_ASSERT(c);
  return c->stdiofmt;
};
  
/*---------------------------------------------------------*/
