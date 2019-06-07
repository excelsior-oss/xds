
#ifndef _XDEBUG_H_
#define _XDEBUG_H_

#define ASSERT_ON

#ifdef ASSERT_ON

#include <stdlib.h>
#include <stdio.h>
#include <setjmp.h>
#include <time.h>

#include "messages.h"
#include "args.h"

extern char ASSERT_MESSAGE_BUF [];

#define ASSERT(c)  if (!(c)){                                                                           \
                     if (xSilent) {                                                                     \
                        sprintf (ASSERT_MESSAGE_BUF, "ASSERT: file %s, line %d\n", __FILE__, __LINE__); \
                        longjmp (ABORT_JMP_BUF, ASSERT_EXIT);                                          \
                     } else {                                                                           \
                        printf("ASSERT: file %s, line %d\n", __FILE__, __LINE__);                       \
                        exit(1);                                                                        \
                     }                                                                                  \
                   };

#define ASSERT_FALSE() {                                                                                \
                     if (xSilent) {                                                                     \
                        sprintf (ASSERT_MESSAGE_BUF, "ASSERT: file %s, line %d\n", __FILE__, __LINE__); \
                        longjmp (ABORT_JMP_BUF, ASSERT_EXIT);                                          \
                     } else {                                                                           \
                        printf("ASSERT: file %s, line %d\n", __FILE__, __LINE__);                       \
                        exit(1);                                                                        \
                     }                                                                                  \
                   };
#else
#define ASSERT(c)
#define ASSERT_FALSE()
#endif

// extern void DumpAllNames(void);
// extern void DumpAllExports(void);
// extern void DumpExportsTable(Export **table, int NExps);
extern void PrintFixup (struct fixup * Fixup);
extern void DumpAllFixups(void);

extern clock_t startTimer ();
extern void    stopTimer  (char * message, clock_t startTime);

//void DumpImpVarFixups ();

#endif
