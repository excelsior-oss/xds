/***    Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   Executor for CTROUT(CityRout) - Compiler Testing Routine
                                                                                   Alexs: 18-Nov-96
*/

#include "ctAssert.h"
#include "ctSpawn.h"
#include "ctErrCodes.h"
#include "ctErrors.h"
#include "ctLimits.h"
#include "ctMemory.h"
#include "ctMagic.h"
#include "ctStrs.h"
#include "ctExec.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>

#if defined(ctHostOS_WIN) || defined(ctHostOS_OS2)
   #include "direct.h"
#endif

#ifdef ctHostOS_UNIX
   #include <unistd.h>
#else
   #include <io.h>
#endif

#include <errno.h>
#include <sys/stat.h>

DeclareMagic(CTEXEC_MAGIC);
DeclareMagic(TemplateMagic);


#define MAX_ARG_NUM                       100

#define MAGIC_ASSERT(x)                   ASSERT ( ( (x)->magic == CTEXEC_MAGIC ) );
#define currCh                                    pattern[i]
#define nextCh                                    pattern[i+1]
#define ERROR(no, addinfo)        Errors_Error((no), 153, addinfo, e->report)
#define outstr(str)                       fwrite( (str), 1, strlen((str)), m->report)
#define outresult(str)                    outstr(splitter);outstr(m->templatename);outstr("( ");outstr(e->curr_template_name);outstr(" )");outstr((str))
#define outcodes(got,expected,shift) sprintf(text,"Got code     : %d\nExpected code: %d\nCode shift   : %d\n",(got),(expected),(shift));outstr(text);

static char empty_string[] = "";
static char splitter[] = "--------------------------------------------------------------------------------\n";
static char ExpectedSuccessfulCompilation[] = ": expected successful compilation.\n\n";
static char ExpectedSuccessfulExecution[]       = ": expected successful execution.\n\n";
static char ExpectedCompilationError[]          = ": expected compilation error.\n\n";
static char ExpectedExecutionError[]            = ": expected execution error.\n\n";
static char ExpectedSuccessfulLinking[]         = ": expected successful linking.\n\n";
static char WrongExecutionCode[]                        = ": wrong execution code.\n\n";
static char WrongCompilationCode[]                      = ": wrong compilation code.\n\n";
static char NotEqualStdOut[]                    = ": wrong stdout.\n\n";
static char NotEqualOut[]                       = ": wrong out.\n\n";



#define  BUFF_COUNT  512
/*------------------------------------------------------------------*/
int isEqualOut(char *name1, char *name2){
  int handle1,handle2;
  int len1,len2;
  char  buff1[BUFF_COUNT];
  char  buff2[BUFF_COUNT];

                  
  if ( (handle1 = open(name1,O_RDONLY) ) == -1 ) {
      perror("Error in isEqualOut");
      return 0;
  };
  if ((handle2 = open(name2,O_RDONLY)) ==-1 ){
      perror("Error in isEqualOut ");
      return 0;
  };

  while ( (len1 = read(handle1,buff1,BUFF_COUNT))!=0 ) {
        len2 = read(handle2,buff2,BUFF_COUNT);
        if (( len2 !=len1 ) ||
                ( memcmp(buff1,buff2,(len1>len2?len2:len1))!=0 ) ){
      close(handle1);
      close(handle2);
          return 0;
        }
  };
 close(handle1);
 close(handle2);
 return 1;
};
/*------------------------------------------------------------------*/
void append_str( char ** to, char * from ){
  char * tmp = NULL;

  if ( *to ){
        tmp = _allocate(char, strlen(from) + strlen(*to) + 1 );
        tmp[0] = 0;
        sprintf( tmp, "%s%s", *to, from );
  }else{
    tmp = _allocate(char, strlen(from) + 1 );
        tmp[0] = 0;
        sprintf( tmp, "%s", from );
  };
  *to = tmp;
};

/*------------------------------------------------------------------*/
void Exec_Clean( Exec_Executor_t * e ,node_module_t * m){
  long i;

  i = Cfg_GetClean(e->cfg);
  if ((i == ctCfg_CleanAll) || ((i == ctCfg_CleanPassed) && (!e->error))){
    for(i = 0; e->filenamelist[i]; i++) remove(e->filenamelist[i]);
    for(i = 0; i<=m->outputcnt; i++) {
            remove(m->outputfiles[i].outfile);
    };
    for(i = 0; i<=m->textcnt; i++) {
            remove(m->textfiles[i]);
    };
    remove(m->stdinfile);
    remove(m->stdoutfile);
    remove(m->stdtxtfile);
    remove(m->execfile);
  };
};

/*------------------------------------------------------------------*/
char * Exec_GenerateText( Exec_Executor_t * e , node_module_t * m, char * pattern ){
  long i = 0, j = 0, k = 0;
  long l = 0, sign = 0;
  char name[MAX_IDENT_LENGTH + 1];
  long idname;
  node_var_t * var = NULL;
  char str[100] = "";
  char * text = NULL;

  while( currCh ){
        if ( currCh == '<' && nextCh == '.' ){
          i+= 2; k = 0;
          while( currCh ){
                if ( currCh == '.' && nextCh == '>' ){
                  name[k] = 0;
                  i+= 2;
                  break;
                }else{
                  if ( k < MAX_IDENT_LENGTH ) name[k++] = currCh;
                  i++;
                };
          };
          idname = Scan_GetIdName( m->scan, name );
          if ( idname < 0 ) ERROR(prerr_UndeclaredIdent, name);
          var = NULL;
          if ( m->locals ){
                var = VL_Get( m->locals, idname );
          };
          if ( !var && m->globals ){
                var = VL_Get( m->globals, idname );
          };
          if ( !var ) ERROR(prerr_UndeclaredIdent, name);

      append_str( &text, e->buff ); e->buff[0] = 0;

          if ( var->val->vtag == val_int ){
                l = 99; str[l--] = 0;
        k = var->val->_.integer; sign = k < 0;
                if (sign) k = -k;
                while( k ){
                  str[l--] = (k % 10) + '0'; k = k / 10;
                };
                if (sign) str[l] = '-';
                else      l++;
                j = 0;
                while ( l < 99 ) str[j++] = str[l++];
                str[j] = 0;
                append_str( &text, str );
          }else if ( var->val->vtag == val_str ) {
        append_str( &text, var->val->_.string );
          }else{
                ERROR(inerr_TypeMismatch, name);
          };
          j = 0;
        }else{
          e->buff[j++] = currCh;
          e->buff[j]   = 0;
          i++;
        };
  };
  if ( j ) append_str( &text, e->buff );
  e->buff[0]=0;
  if ( text == NULL ) {
        append_str(&text,"");
  };
  return text;
};

/*------------------------------------------------------------------*/
char * MakeName( char * testprefix, long number, long length ){
  char * name = NULL;
  char   str[100];
  long   l, j;

  l = 99;
  str[l--] = 0;
  while( (0 < length--) || number ){
        str[l--] = (number % 10) + '0';
        number = number / 10;
  };
  l++; j = 0;
  while ( l < 99 ) str[j++] = str[l++];
  str[j] = 0;
  name = _allocate(char, strlen(testprefix) + strlen(str) + 1 );
  sprintf( name, "%s%s", testprefix, str );
  printf("%s\r", name);
  return name;
};

/*------------------------------------------------------------------*/
void Exec_Generate( Exec_Executor_t * e , node_module_t * m ){
  node_template_t * template = NULL;
  node_var_t      * var          = NULL;
  char            * text         = NULL;
  FILE            * hand         = NULL;
  char            * filename = NULL;
  unsigned long     count    = 0;
  char            * fullname = NULL;
  char            * name         = NULL;
  long              filenamelistcnt = 0;
  long              namelistcnt = 0;
  int               outputcnt=0,textcnt=0;

  MAGIC_ASSERT(e);

  while(e->filenamelist[filenamelistcnt]) filenamelistcnt++;
  name = MakeName( m->testprefix, e->number++, 4 );
  if(m->firstName[0] == 0){
    strcpy(m->firstName, name);
  };
  strcpy(m->lastName, name);
  ctStrs_COPY( e->curr_template_name, name );

  var = VL_Get( m->globals, Scan_GetIdName( m->scan, "name" ) );
  var->val->vtag = val_str; ctStrs_COPY( var->val->_.string, name );

  var = VL_Get( m->globals, Scan_GetIdName( m->scan, "header" ) );
  var->val->vtag = val_str; 
  if (!m->comment)
    ctStrs_COPY(var->val->_.string, "");
  else 
    ctStrs_COPY(var->val->_.string, m->comment);

  template = TL_GetFirst( m->templates );
  while( template ){
    var = VL_Get( m->globals, Scan_GetIdName( m->scan, "suffix" ) );
    var->val->vtag = val_str; ctStrs_COPY(var->val->_.string, template->suffix ? template->suffix : empty_string );
    fullname = _allocate(char, strlen(name) + strlen(var->val->_.string) + 1 );
    sprintf( fullname, "%s%s", name, var->val->_.string );
    text = Exec_GenerateText( e, m, template->pattern );
    if ( template->ttag == tmpl_cfgtemplate ){
      m->compile_options = text;
    }else{
      if (( template->ttag == tmpl_text_template )||( template->ttag == tmpl_stdout_template )||( template->ttag == tmpl_stdin_template )){
          filename = Cfg_GetFileName( e->cfg, fullname, template->extension , 1 );
      } else {
          filename = Cfg_GetFileName( e->cfg, fullname, template->extension , 0 );
      };

          if ((Cfg_GetStdIOFmt (e->cfg) == ctCfg_StdIOFmtLf) && ((template->ttag == tmpl_stdout_template) || (template->ttag == tmpl_stdin_template))) {
                  if ( NULL == ( hand = fopen( filename, "wb" ) ) ) ERROR(exerr_CantOpenFile, filename);
          } else {
                  if ( NULL == ( hand = fopen( filename, "w" ) ) ) ERROR(exerr_CantOpenFile, filename);
          };
                
          count = strlen( text );
      if ( count != fwrite( text, 1, count, hand ) ) ERROR(exerr_CantWriteToFile, filename);
      if ( fclose( hand ) ) ERROR(exerr_CantCloseFile, filename );
      ASSERT( filenamelistcnt < MAX_ARG_NUM );
      e->filenamelist[filenamelistcnt] = NULL;
      if(( template->ttag == tmpl_template ) ||
         ( template->ttag == tmpl_deftemplate )){
              append_str( e->filenamelist + (filenamelistcnt++), filename );
      };
      if (template->ttag == tmpl_text_template ){
                 ctStrs_COPY( m->textfiles[textcnt] , filename);
                 textcnt++;
      };
      if ( template->ttag == tmpl_outputs_template ){
                 text=Exec_GenerateText( e, m, m->outputfiles[outputcnt].patterntxt );
                 ctStrs_COPY( m->outputfiles[outputcnt].txtfile ,text );
                 ctStrs_COPY( m->outputfiles[outputcnt].outfile , filename);
                 outputcnt++;
      };
      if ( template->ttag == tmpl_stdin_template ){
        strcpy( m->stdinfile , filename);
      };
      if ( template->ttag == tmpl_stdout_template ){
        strcpy( m->stdtxtfile , filename);
        filename = Cfg_GetFileName( e->cfg, fullname, "ooo" , 1 );
        strcpy( m->stdoutfile , filename);
      };
      if ( template->ttag == tmpl_template ){
        ASSERT( namelistcnt < MAX_ARG_NUM );
        e->namelist[namelistcnt] = NULL;
        append_str( e->namelist + (namelistcnt++), fullname );
      };
    };
        template = TL_GetNext( m->templates );
  };
  e->filenamelist[filenamelistcnt] = NULL;
  e->namelist[namelistcnt] = NULL;
};

/*------------------------------------------------------------------*/
long Exec_Compile( Exec_Executor_t * e, node_module_t * m, long action ){
  char ** filenamelist = NULL;
  long    filenamelistcnt = 0;
  char *  cmd = NULL;
  long    compile_code;
  char    text[200];
  long    i, j = 0;


  MAGIC_ASSERT(e);


  e->filenamelist[0] = NULL;
  Exec_Generate( e, m );

  switch (action){
    case Exec_DoNone: return 0;
    case Exec_DoCompare:
      cmd = Cfg_GetCompiler1Cmd( e->cfg );
      if ( !cmd ) ERROR(exerr_NoCompilerCmd,0);

      filenamelist = _allocate(char*, sizeof(char*) * MAX_ARG_NUM );
      while( cmd[j] ){
        i = 0;
        while( cmd[j] && cmd[j] != ' ' ) text[i++] = cmd[j++];
        text[i] = 0;
        append_str( filenamelist + (filenamelistcnt++), text );
        ASSERT( filenamelistcnt < MAX_ARG_NUM );
        filenamelist[filenamelistcnt] = NULL;
        while( cmd[j] == ' ' ) j++;
      };

      if ( m->compile_options ) {
        j = i = 0;
        while(m->compile_options[i]){
          while( m->compile_options[i] && m->compile_options[i] <= ' ' ) i++;
          while( m->compile_options[i] > ' ' ){
            text[j++] = m->compile_options[i++];
          };
          text[j] = 0;
          if ( j ){
            j = 0;
            append_str( filenamelist + (filenamelistcnt++), text );
            ASSERT( filenamelistcnt < MAX_ARG_NUM );
            filenamelist[filenamelistcnt] = NULL;
          };
        };
      };

      for (i = 0; e->filenamelist[i]; i++){
        append_str( filenamelist + (filenamelistcnt++), e->filenamelist[i] );
        ASSERT( filenamelistcnt < MAX_ARG_NUM );
        filenamelist[filenamelistcnt] = NULL;
      };

      compile_code = Spawn_Spawn( filenamelist[0], filenamelist, m->logFD1 );
  };


  filenamelist = NULL;
  filenamelistcnt = 0;
  cmd = NULL;
  j = 0;

  m->firstName[0] = 0;

  cmd = Cfg_GetCompilerCmd( e->cfg );
  if ( !cmd ) ERROR(exerr_NoCompilerCmd,0);

  filenamelist = _allocate(char*, sizeof(char*) * MAX_ARG_NUM );
  while( cmd[j] ){
    i = 0;
    while( cmd[j] && cmd[j] != ' ' ) text[i++] = cmd[j++];
    text[i] = 0;
    append_str( filenamelist + (filenamelistcnt++), text );
    ASSERT( filenamelistcnt < MAX_ARG_NUM );
    filenamelist[filenamelistcnt] = NULL;
    while( cmd[j] == ' ' ) j++;
  };

  if ( m->compile_options ) {
    j = i = 0;
    while(m->compile_options[i]){
      while( m->compile_options[i] && m->compile_options[i] <= ' ' ) i++;
      while( m->compile_options[i] > ' ' ){
        text[j++] = m->compile_options[i++];
      };
      text[j] = 0;
      if ( j ){
        j = 0;
        append_str( filenamelist + (filenamelistcnt++), text );
        ASSERT( filenamelistcnt < MAX_ARG_NUM );
        filenamelist[filenamelistcnt] = NULL;
      };
    };
  };

  for (i = 0; e->filenamelist[i]; i++){
    append_str( filenamelist + (filenamelistcnt++), e->filenamelist[i] );
    ASSERT( filenamelistcnt < MAX_ARG_NUM );
    filenamelist[filenamelistcnt] = NULL;
  };

  compile_code = Spawn_Spawn( filenamelist[0], filenamelist, m->logFD );
  return compile_code;
};

/*------------------------------------------------------------------*/
long Exec_Run( Exec_Executor_t * e, node_module_t * m, long action ){
  char *  cmd = NULL;
  char    text[200];
  long    i, j;
  long    namelistcnt;
  long    run_code;
  char ** namelist;
  char  * filename = NULL;
  char    currdir[1000] , * exepath=NULL;

  int     inhandle;
  int     outhandle;
  int     instream;
#ifndef ctHostOS_OS2
  FILE   *outstream;
#else
  int     outstream;
#endif

  MAGIC_ASSERT(e);

  Exec_CompileBool( e, m, 1, action);
  if ( e->error ) return 1;

  if(action != Exec_DoRun) return 0;

  cmd = Cfg_GetMakerCmd( e->cfg );
  if ( !cmd ) ERROR(exerr_NoMakerCmd,0);

  namelist = _allocate(char*, sizeof(char*) * MAX_ARG_NUM );
  namelist[0] = NULL;
  
  j = 0;
  namelistcnt = 0;
  while( cmd[j] ){
        i = 0;
        while( cmd[j] && cmd[j] != ' ' ) text[i++] = cmd[j++];
        text[i] = 0;
        if ( text[0] != 0 )
                append_str( namelist + (namelistcnt++), text );
        ASSERT( namelistcnt < MAX_ARG_NUM );
        namelist[namelistcnt] = NULL;
        while( cmd[j] == ' ' ) j++;
  };

  i = 0;
  if ( namelistcnt != 0 ){
          while( e->namelist[i] ){
                  namelist[namelistcnt++] = e->namelist[i++];
                  namelist[namelistcnt] = NULL;
          };
          run_code = Spawn_Spawn( namelist[0], namelist, -1 );
          if ( run_code != Cfg_GetMakeOkCode( e->cfg ) ){
                  outresult( ExpectedSuccessfulLinking );
                  e->error = 1;
                  e->namelist[0] = NULL;
                  return run_code;
          }
  }else{
     while( e->namelist[i] ){ i++; };    
  }  
  namelist[ 0 ] = e->namelist[i-1];

  e->namelist[0] = Cfg_GetFileName( e->cfg, namelist[0], 
#ifndef ctHostOS_UNIX
  "exe"
#else
  ""
#endif
  , 2 );
  e->namelist[1] = NULL;
  ctStrs_COPY( m->execfile,Cfg_GetFileName( e->cfg, namelist[0], "exe" , 0 ));

        
  if (m->needstdin){
    inhandle=dup(0);
    if ((instream = open(m->stdinfile,O_RDONLY )) == NULL) {
        perror("Stdin error ");
        e->error = 1;
        return -1;
    };
    dup2(instream,0);
  };
  if (m->needstdout){
    outhandle = dup(1);
#ifndef ctHostOS_OS2
    if ((outstream = freopen(m->stdoutfile,"w+",stdout))== NULL){
#else
    if ((outstream = open(m->stdoutfile, O_RDWR | O_CREAT ))== NULL){
#endif
      perror("Stdout error");
      e->error = 1;
      return -1;
    }
  };

  if(Cfg_GetRemote( e->cfg )){
    run_code = RemoteSpawn_Spawn( e->cfg, "tmp/ch/test.o", "test");
  }else{
    getcwd( currdir , 1000);
    exepath = Cfg_GetExePath ( e->cfg , exepath );
    i = strlen( exepath );
    if (i && ( ( exepath[i-1] == '\\' ) || ( exepath[i-1] == '/') ) )
      exepath[i-1] = 0;
    chdir( exepath  );
    
    namelist = NULL;
    namelistcnt = 0;
    cmd = NULL;
    j = 0;

    cmd = Cfg_GetRunnerCmd( e->cfg );
    if ( !cmd ) ERROR(exerr_NoRunnerCmd,0);
    
    namelist = _allocate(char*, sizeof(char*) * MAX_ARG_NUM );
    while( cmd[j] ){
        i = 0;
        while( cmd[j] && cmd[j] != ' ' ) text[i++] = cmd[j++];
        text[i] = 0;
        append_str( namelist + (namelistcnt++), text );
        ASSERT( namelistcnt < MAX_ARG_NUM );
        namelist[namelistcnt] = NULL;
        while( cmd[j] == ' ' ) j++;
    };
      
    for (i = 0; e->namelist[i]; i++){
        append_str( namelist + (namelistcnt++), e->namelist[i] );
        ASSERT( namelistcnt < MAX_ARG_NUM );
        namelist[namelistcnt] = NULL;
    };
    run_code = Spawn_Spawn( namelist[0], namelist, -1 );

/* 
    {
-- version for CPS-1750
      char tmpRun[] = "run.exe";
      char *args[2];
      args[0] = &tmpRun;
      args[1] = e->namelist[0];

      char tmpRun[] = "simmips.exe";
      char cpu[]    = "-cpu=r4000";
      char endian[] = "-L";
      char *args[4];
      args[0] = &tmpRun;
      args[1] = &cpu;
      args[2] = &endian;
      args[3] = e->namelist[0];
//      run_code = Spawn_Spawn( strcat (tmpRun, e->namelist[0]), e->namelist, -1 );
      run_code = Spawn_Spawn( tmpRun, args, -1 );
    }
*/
    chdir( currdir );
  };
  e->namelist[0] = NULL;

  if( run_code == Cfg_GetRunCodeShift(e->cfg)  ){
      for (i=0; i<m->outputcnt; i++){
           if ( isEqualOut(m->outputfiles[i].outfile,m->outputfiles[i].txtfile) != 0 ){
                outresult( NotEqualStdOut );
            e->error = 1;
           };
      };
  };

  if (m->needstdout){
#ifndef ctHostOS_OS2
      fclose(outstream);
#else
      close(outstream);
#endif
      dup2(outhandle,1);
      close(outhandle);
      if ( isEqualOut(m->stdoutfile,m->stdtxtfile) == 0 ){
        outresult( NotEqualOut );
        e->error = 1;
      };
  };
  if (m->needstdin){
      dup2(inhandle,0);
      close(instream);
      close(inhandle);
  };
 
  return run_code;
};

/*------------------------------------------------------------------*/
Exec_Executor_t * Exec_Init( Cfg_Configurator_t * c ){
  Exec_Executor_t * e = NULL;

  e = allocate( Exec_Executor_t );
  e->magic         = SetMagic(CTEXEC_MAGIC);
  e->cfg           = c;
  e->buff          = _allocate(char, MAX_STRING_LENGTH + 1 );
  e->number        = 0;
  e->namelist      = _allocate(char*, sizeof(char*) * MAX_ARG_NUM );
  e->filenamelist  = _allocate(char*, sizeof(char*) * MAX_ARG_NUM );
  e->namelist[0]   = NULL;
  e->filenamelist[0] = NULL;
  e->error           = 0;
  e->report          = NULL;

  return e;
};

/*------------------------------------------------------------------*/
void Exec_Final( Exec_Executor_t * e ){

  MAGIC_ASSERT(e);
};


/*------------------------------------------------------------------*/
void Exec_RunBool( Exec_Executor_t * e , node_module_t * m, long result, long action ){
  long   run_code;
  long   run_ok_code;


  MAGIC_ASSERT(e); e->report = m->report;

  run_code = Exec_Run( e, m, action );
  if ( !e->error && action == Exec_DoRun){
    run_ok_code = Cfg_GetRunOkCode( e->cfg );
    if ( result ){
      if ( run_code != run_ok_code ){
        outresult( ExpectedSuccessfulExecution );
        e->error = 1;
      };
    }else if ( run_code == run_ok_code ){
      outresult( ExpectedExecutionError );
      e->error = 1;
    };
  };
  if(action == Exec_DoRun) Exec_Clean(e,m);
};

/*------------------------------------------------------------------*/
void Exec_RunInt( Exec_Executor_t * e , node_module_t * m, long result, long action ){
  long   run_code;
  long   run_code_shift;
  char   text[200];

  MAGIC_ASSERT(e); e->report = m->report;
  run_code = Exec_Run( e, m, action );
  if ( !e->error && action == Exec_DoRun){
    run_code_shift = Cfg_GetRunCodeShift( e->cfg );
    if ( run_code != ( run_code_shift + result ) ){
      outresult( WrongExecutionCode );
      outcodes( run_code, result, run_code_shift );
      e->error = 1;
    };
  };
  if(action == Exec_DoRun) Exec_Clean(e,m);
};

/*------------------------------------------------------------------*/
void Exec_CompileBool( Exec_Executor_t * e , node_module_t * m, long result, long action ){
  long   compile_code;
  long   compile_ok_code;

  MAGIC_ASSERT(e); e->report = m->report;
  compile_code = Exec_Compile( e, m, action );
  e->error = 0;
  if(action != Exec_DoNone){
    compile_ok_code = Cfg_GetCompileOkCode( e->cfg );
    if ( result ){
          if ( compile_code != compile_ok_code ){
            outresult( ExpectedSuccessfulCompilation );
            e->error = 1;
          };
    }else if ( compile_code == compile_ok_code ){
          outresult( ExpectedCompilationError );
          e->error = 1;
    };
  };
  if(action == Exec_DoCompile) Exec_Clean(e,m);
};

/*------------------------------------------------------------------*/
void Exec_CompileInt( Exec_Executor_t * e , node_module_t * m, long result, long action ){
  long   compile_code;
  long   compile_code_shift;
  char   text[200];

  MAGIC_ASSERT(e); 
  e->report = m->report;
  compile_code = Exec_Compile( e, m, action );
  e->error = 0;
  if(action != Exec_DoNone){
    compile_code_shift = Cfg_GetCompileCodeShift( e->cfg );
    if ( compile_code != ( compile_code_shift + result ) ){
          outresult( WrongCompilationCode );
          outcodes( compile_code, result, compile_code_shift );
          e->error = 1;
    };
  };
  if(action == Exec_DoCompile) Exec_Clean(e,m);
};

/*------------------------------------------------------------------*/
long Exec_GetTestsCount( Exec_Executor_t * e){
	return e->number;
}
