/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrTransportTCP.h                                          *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This header file contains interface to TCP/IP transport    *|
|*                 module (initialization function).                          *|
|*                                                                            *|
\******************************************************************************/

#ifndef _xdrTransportTCP_h
#define _xdrTransportTCP_h

#include "xdrTransports.h"

extern xdrTransports_InitTransport xdrTransportTCP_Init;
extern xdrTransports_InitTransport xdrTransportTCP_Final;


#endif