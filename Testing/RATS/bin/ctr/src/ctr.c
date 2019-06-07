#ifdef ctHostOS_OS2
  #include <io.h>
#else
  #include <stdio.h>
#endif

#include <stdlib.h>

#include "ctNode.h"
#include "ctParse.h"
#include "ctInterpret.h"
#include "ctMemory.h"


int main(int argc, char *argv[])
{
  Parse_Parser_t   * p;
  Interpret_Interpereter_t * i;
  node_module_t    * m;
  long               mem_man;
  int logFD, logFD1;

  int j, l;
  char number[100];
  char *cntFname;
  FILE * cntfile;

 
  FILE		   * commentFile = NULL;
  long               commentFlag = 0;

  if ( argc != 3 && argc != 4 ) { printf("Usage: ctr templatename reportname [commentname]\n"); exit(1); }

  if ( argc == 4 ){
    for(j = nnumbers = 0; argv[3][j];){
      for(l = 0; argv[3][j] && argv[3][j] != ','; j++, l++) number[l] = argv[3][j];
      number[l] = 0; j += argv[3][j] != 0;
      numbers[nnumbers++] = atol(number);
    };
  };

/*
  if(argc == 4 && (commentFile = fopen(argv[3], "w")) == NULL){
    printf("Can't open file %s\n", argv[3]);
    exit(1);
  };
*/

  if((logFD = creat("comp.log", 0644)) == -1 || (logFD1 = creat("comp1.log", 0644)) == -1){
    printf("Can't open file comp.log\n");
    exit(1);
  };

  mem_man = Memory_Init();
  p = Parse_Init( argv[1], argv[2] );

  if (!p) { printf("Can't open %s\n",argv[1]); exit(1); }

 
 i = Interpret_Init();


  while (m = Parse_Module( p )){
    m->logFD  = logFD;
    m->logFD1 = logFD1;
    Interpret_Module( i, m );
    if(commentFile && m->firstName[0]){
      commentFlag = 1;
      fprintf(commentFile, "%s - %s:\t%s\n", m->firstName, m->lastName, m->comment);
    };
  };

  
  /* Create <report_file_name>.cnt file with tests count inside */

  cntFname = _allocate( char, strlen(argv[2] + 5));
  sprintf(cntFname, "%s.cnt", argv[2]);
  if ( NULL == ( cntfile = fopen( cntFname, "w" ) ) ) { 
	  printf("Can't open .cnt file %s. No .cnt file will be created.\n",cntFname);
  } else {
	  fprintf(cntfile, "Total tests: %d\n", Interpret_GetTestsCount(i));
	  fclose(cntfile);
  }
	   
 
  if(commentFile){
    fclose(commentFile);
    if(!commentFlag) remove(argv[3]);
  };
/***    Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   ASSERTIONs  Den: 10-Aug-96
*/

#ifndef __ctAssert_h
#define __ctAssert_h


#ifdef CT_ASSERTIONS_OFF

#define ASSERT(x) ((void)0)
#define ASSERTM(x,message) ((void)0)

#else

#include <stdio.h>
#include <stdlib.h>
#define ASSERT(x) ((x)?(0):(fprintf(stderr,"\nInternal error: ASSERTion failed, file %s(compiled %s at %s) line # %d\n", __FILE__, __DATE__, __TIME__, __LINE__),exit(1)))
#define ASSERTM(x,message) ((x)?(0):(fprintf(stderr,"\nInternal error: ASSERTion failed(%s), file %s(compiled %s at %s) line # %d\n", message, __FILE__, __DATE__, __TIME__, __LINE__),exit(1)))

#endif



#endif __ctAssert_h

  Parse_Final( p );
  Interpret_Final( i );
/*  Memory_Final(mem_man);*/

  return 0;
}

