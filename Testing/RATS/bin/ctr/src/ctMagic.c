/***    Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   Magic generator for CTRout(CityRout) - Compiler Testing Routine
                                                   Alexs: 4-Nov-96
*/

#include "ctMagic.h"
#include "ctAssert.h"

long magic = NO_MAGIC + 1;

long SetMagic_F(long * x){
  ASSERTM( magic != NO_MAGIC, "\"Out of MAGIC\"");
  return *x = magic++;
};
