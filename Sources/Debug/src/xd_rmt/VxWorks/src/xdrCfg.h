/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrCfg.h                                                   *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This header file contains interface to configuration       *|
|*                 module.                                                    *|
|*                                                                            *|
\******************************************************************************/

#ifndef _xdrCfg_h
#define _xdrCfg_h


#define xdrCfg_OptionNameLen  63
#define xdrCfg_OptionValueLen 63

#define xdrCfg_OptionType_None    0
#define xdrCfg_OptionType_Integer 1
#define xdrCfg_OptionType_Boolean 2
#define xdrCfg_OptionType_String  3



/* Procedure to get option values */
extern void xdrCfg_GetOptionValue(char * name, int type, void * val);

/* Initialization procedure, also parses start arguments.
   Returns True if initialization was succesfully completed,
   and False otherwise.
*/
extern int xdrCfg_Init(char * args);

#endif