#ifndef _xdTransportTCP_H
#define _xdTransportTCP_H

#include "xdTypes.h"
#include "xdTransport.h"

extern int xdTransportTCP_Accept(xdTransportDesc ** desc); 
/* returns pipe ID */


extern xdTransportDesc * xdTransportTCP_Init(void);
/* returns TCP transport descriptor */


extern void xdTransportTCP_Final(xdTransportDesc ** desc);

#endif
