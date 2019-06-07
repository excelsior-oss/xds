/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrMagic.h                                                 *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This header file contains magic numbers for various        *|
|*                 structures.                                                *|
|*                                                                            *|
\******************************************************************************/

#ifndef _xdrMagic_h
#define _xdrMagic_h


#define xdrMagic_Base                 0xDEADFACE
                            
#define xdrMagic_ClientDescriptor     (xdrMagic_Base + 1)
#define xdrMagic_TransportDescriptor  (xdrMagic_Base + 2)


#endif