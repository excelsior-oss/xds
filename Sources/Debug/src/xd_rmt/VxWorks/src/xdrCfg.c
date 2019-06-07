/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrCfg.c                                                   *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This file contains implementation of configuration module. *|
|*                                                                            *|
\******************************************************************************/

#include <vxWorks.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>

#include "xdrIncl.h"
#include "xdrTrace.h"
#include "xdrCfg.h"
#include "xdrTypes.h"
#include "xdrAssert.h"


/*----------------------------------------------------------------------------*/
#define cfg_error(str)            {printf("Configuration module: %s [%d]\n", (str), i); return False;}
#define cfgError_BadFormat        "bad arguments format"
#define cfgError_UnknownOption    "unknown option"
#define cfgError_IncompatibleType "incompatible type"

/*----------------------------------------------------------------------------*/
typedef struct xdrCfg_tagOption{
  int  type;
  char name[xdrCfg_OptionNameLen+1];
  union{
    int  integer;
    int  boolean;
    char string[xdrCfg_OptionValueLen+1];
  } value;
} xdrCfg_Option;


xdrCfg_Option xdrCfg_Options[] = {
  {xdrCfg_OptionType_Integer, "qsize", {256}},
  {xdrCfg_OptionType_None}
};

/*-------------------------------------------------------*/
static xdrCfg_Option * check_name(char * name){
  int i;

  for(i = 0; xdrCfg_Options[i].type != xdrCfg_OptionType_None && strcmp(name, xdrCfg_Options[i].name) != 0; i++);
  if(xdrCfg_Options[i].type == xdrCfg_OptionType_None) return NULL;
  return &xdrCfg_Options[i];
}


/*----------------------------------------------------------------------------*/
/* Procedure to get option values */
void xdrCfg_GetOptionValue(char * name, int type, void * val){
  xdrCfg_Option * opt;

  opt = check_name(name);
  ASSERT(opt != NULL && opt->type == type);
  if(opt->type == xdrCfg_OptionType_Integer){
    *(int*)val = opt->value.integer;
  }else if(opt->type == xdrCfg_OptionType_Boolean){
    *(int*)val = opt->value.boolean;
  }else if(opt->type == xdrCfg_OptionType_String){
    strcpy((char*)val, opt->value.string);
  }else{
    ASSERT(False);
  };
};

/*-------------------------------------------------------*/
/* Initialization procedure, also parses start arguments.
   Returns True if initialization was succesfully completed,
   and False otherwise.
*/
int xdrCfg_Init(char * args){
  int i, n, v;
  char name[xdrCfg_OptionNameLen+1], val[xdrCfg_OptionValueLen+1];
  xdrCfg_Option * opt;

  if(args == NULL) return True;

  i = 0;
  while(args[i] != 0){
    if(!isalpha(args[i])) cfg_error(cfgError_BadFormat);
    n = 0;
    while(isalnum(args[i])){
      if(n < xdrCfg_OptionNameLen) name[n++] = args[i];
      i++;
    };
    name[n] = 0;
    opt = check_name(name);
    if(opt == NULL) cfg_error(cfgError_UnknownOption);
    if(args[i] == '-'){
      i++;
      if(opt->type != xdrCfg_OptionType_Boolean) cfg_error(cfgError_IncompatibleType);
      opt->value.boolean = False;
      printf("Cfg: %s is False\n", name);
    }else if(args[i] == '+'){
      i++;
      if(opt->type != xdrCfg_OptionType_Boolean) cfg_error(cfgError_IncompatibleType);
      opt->value.boolean = True;
      printf("Cfg: %s is True\n", name);
    }else if(args[i] == '='){
      i++;
      v = 0;
      if(isalpha(args[i])){
        if(opt->type != xdrCfg_OptionType_String) cfg_error(cfgError_IncompatibleType);
        while(isalpha(args[i])){
          if(v < xdrCfg_OptionValueLen) opt->value.string[v++] = args[i];
          i++;
        };
        opt->value.string[v] = 0;
        printf("Cfg: %s is %s\n", name, opt->value.string);
      }else if(isdigit(args[i])){
        while(isdigit(args[i])){
          if(v < xdrCfg_OptionValueLen) val[v++] = args[i];
          i++;
        };
        val[v] = 0;
        opt->value.integer = atol(val);
        printf("Cfg: %s is %s\n", name, val);
      }else cfg_error(cfgError_BadFormat);
    }else cfg_error(cfgError_BadFormat);
    if(args[i] == ';'){
      i++;
    }else if(args[i] != 0) cfg_error(cfgError_BadFormat);
  };
  return True;
};

