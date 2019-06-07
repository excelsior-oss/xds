/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrAssert.c                                                *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This file contains implementation of assertion facilities. *|
|*                                                                            *|
\******************************************************************************/

#include <stdio.h>

#include "xdrIncl.h"
#include "xdrTrace.h"
#include "xdrAssert.h"


int xdrAssert_Failure(
  char * msg, 
  char * file, 
  char * date, 
  char * time, 
  int    line
){
  if(msg == NULL){
    printf("Failure in file %s line %d (compiled at date %s time %s)\n", file, line, date, time);
  }else{
    printf("Failure in file %s line %d (compiled at date %s time %s): %s\n", file, line, date, time, msg);
  };
  exit(1);
};
