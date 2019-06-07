
#ifndef _XDEBUG_H_
#define _XDEBUG_H_

#include <stdlib.h>
#include <stdio.h>

#define ASSERT(c)  if (!(c)){                                                                           \
                        printf("ASSERT: file %s, line %d\n", __FILE__, __LINE__);                       \
                        exit(1);                                                                        \
                   };

#define ASSERT_FALSE() {                                                                                \
                        printf("ASSERT: file %s, line %d\n", __FILE__, __LINE__);                       \
                        exit(1);                                                                        \
                   };

#endif