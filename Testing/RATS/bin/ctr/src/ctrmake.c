#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ctCfg.h"
#include "ctSpawn.h"
#include "ctNode.h"

#define MAX_ARGS    20
#define MAX_ARG_LEN 65


#define modeC	0
#define modeNC	1


typedef struct tagArgDesc{
  int  args_num;
  char args[MAX_ARGS][MAX_ARG_LEN];
} ArgDesc;


#define argsAS		0
#define argsCCOMP	1
#define argsLINK	2
#define argsOBJ		3
#define argsLIB		4



ArgDesc args[5] = {
  {0},
  {0},
  {0},
  {0},
  {0}
};


int getArgIndex(char * key){
  if(!strcmp(key, "as"))    return argsAS;
  if(!strcmp(key, "ccomp")) return argsCCOMP;
  if(!strcmp(key, "link"))  return argsLINK;
  if(!strcmp(key, "obj"))   return argsOBJ;
  if(!strcmp(key, "lib"))   return argsLIB;

  printf("Undefined key: %s\n", key);
  exit(1);
};

void getArgs(char * argv[]){
  int i = 2, idx = -1, j;


  if(argv[i][0] != '='){
    printf("Bad argument: %s\n", argv[i]);
    exit(1);
  };

  while(argv[i] && argv[i][0] == '='){
    if(idx == argsOBJ){
      printf("Bad argument: %s\n", argv[i]);
      exit(1);
    };
    idx = getArgIndex(argv[i++]+1);
    while(argv[i] && argv[i][0] != '='){
      strcpy(args[idx].args[args[idx].args_num++], argv[i]);
      i++;
    };
  };

  if(!args[argsOBJ].args_num){
    printf("Nothing to do (absent '=obj')\n");
    exit(1);
  };
};


void help(void){
  printf("Usage: ctrmake (-c|-nc) options\n"
         "Options:\n"
         "  =as    assembler_options\n"
         "  =ccomp compiler_options\n"
         "  =link  link_options\n"
         "  =lib   libraries\n"
         "  =obj   objects\n");
  exit(1);
};



int main(int argc, char *argv[])
{
  int    mode;
  char * arglist[100];
  char * tmparglist[100];
  char * exename = NULL;
  char   option[512] = "";
  long   i, arglistcnt = 0, tmparglistcnt = 0;
  long   ret_code;
  long   mem_man;


  Cfg_Configurator_t * cfg = NULL;

  if (argc < 4) help();

  cfg = Cfg_Init( "ctr.cfg" );
  if ( !cfg ) { printf("Can't open ctr.cfg\n"); exit(1); }

  mem_man = Memory_Init();


  if ( strcmp( argv[1], "-c" ) == 0 ){
    mode = modeC;
  }else if ( strcmp( argv[1], "-nc" ) == 0 ){
    mode = modeNC;
  }else{
    help();
  };

  getArgs(argv);

  switch(mode){
    case modeC:
      arglist[arglistcnt++] = Cfg_GetCCompilerCmd(cfg);
      exename = Cfg_GetFileName( cfg, argv[argc-1], "exe" ,0);
      sprintf( option, "/Fe%s", exename );
      arglist[arglistcnt++] = option;
      for(i = 0; i < args[argsCCOMP].args_num; i++){
        arglist[arglistcnt++] = args[argsCCOMP].args[i];
      };
      for(i = 0; i < args[argsOBJ].args_num; i++){
        arglist[arglistcnt++] = Cfg_GetFileName( cfg, args[argsOBJ].args[i], "c" ,0);
      };
      arglist[arglistcnt++] = "/link";
      for(i = 0; i < args[argsLIB].args_num; i++){
        arglist[arglistcnt++] = args[argsLIB].args[i];
      };
      arglist[arglistcnt]   = NULL;
      break;
    case modeNC:
      arglist[arglistcnt++] = Cfg_GetLinkerCmd(cfg);
      for(i = 0; i < args[argsLINK].args_num; i++){
        arglist[arglistcnt++] = args[argsLINK].args[i];
      };
      for(i = 0; i < args[argsOBJ].args_num; i++){
        arglist[arglistcnt++] = Cfg_GetFileName( cfg, args[argsOBJ].args[i], "obj", 0);
      };
      for(i = 0; i < args[argsLIB].args_num; i++){
        arglist[arglistcnt++] = args[argsLIB].args[i];
      };
      exename = Cfg_GetFileName( cfg, argv[argc-1], "exe" ,0 );
      sprintf( option, "/NAME=%s", exename );
      arglist[arglistcnt++] = option;
      arglist[arglistcnt]   = NULL;
      break;
  };

  ret_code = Spawn_Spawn( arglist[0], arglist, -1 );
/*  Memory_Final(mem_man); */
  return ret_code;
}
