#ifndef _xdTransport_H
#define _xdTransport_H

typedef void xdTransportDesc;

typedef int (*xdTransport_Accept)(xdTransportDesc ** desc); 
/* returns pipe ID */


typedef xdTransportDesc * (*xdTransport_Init)(void);
/* returns transport descriptor */


typedef void (*xdTransport_Final)(xdTransportDesc ** desc);



#endif
