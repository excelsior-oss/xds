/******************************************************************************\
|*                                                                            *|
|*  File        :  xdrTransports.h                                            *|
|*  Author      :  AlexS                                                      *|
|*  Description :  This header file contains interface to common transport    *|
|*                 module (descriptors and initialization, finalization and   *|
|*                 channel managment functions).                              *|
|*                                                                            *|
|*  Notes       :  To include transport define corresponding flag:            *|
|*                     TCP/IP    xdrIncl_TransportTCP                          *|
|*                                                                            *|
|*                                                                            *|
|*                                                                            *|
|*                                                                            *|
|*                                                                            *|
\******************************************************************************/


#ifndef _xdrTransports_h
#define _xdrTransports_h


/*----------------------------------------------------------------------------*/
/* Transport descriptor declaration */
typedef struct xdrTransports_tagTransportDescriptor xdrTransports_TransportDescriptor;




/*-------------------------------------------------------*/
/* Transport initialization procedure prototype.
   This procedure initialize transport, fills its descriptor, and returns
   result (True or False).
*/
typedef int xdrTransports_InitTransport (xdrTransports_TransportDescriptor * desc);




/*-------------------------------------------------------*/
/* Transport finalization procedure prototype.
   This procedure finalize transport and returns result (True or False).
*/
typedef int xdrTransports_FinalTransport(xdrTransports_TransportDescriptor * desc);




/*-------------------------------------------------------*/
/* Transport types */
#define xdrTransports_None 0
#define xdrTransports_TCP  1




/*-------------------------------------------------------*/
/* Transport descriptor definition */
struct xdrTransports_tagTransportDescriptor{
  int magic; /* Magic number (see xdrMagic.h) */

  struct{
    int                            transport_type; /* Transport type           */
    int                            initialized;    /* True if transport is     *\ 
                                                   \* initialized              */
    xdrTransports_InitTransport  * init;           /* Initialization procedure */
    xdrTransports_FinalTransport * final;          /* Finalization procedure   */
  } Header;

  union{
    struct{
    } None;

    struct{
      int taskID;
      int socketID;
    } TCP;
  } Body;
};





/*-------------------------------------------------------*/
/* Transports array, ended by NONE transport descriptor
   (transport_type == xdrTransports_None).
*/
extern xdrTransports_TransportDescriptor xdrTransports_Transports[];





/*-------------------------------------------------------*/
/* Transports initialization procedure.
   Returns True if initialization was succesfully completed,
   and False otherwise.
*/
extern int xdrTransports_Init(void);





/*-------------------------------------------------------*/
/* Transports finalization procedure.
   Returns True if finalization was succesfully completed,
   and False otherwise.
*/
extern int xdrTransports_Final(void);






/*----------------------------------------------------------------------------*/
/* Pipe descriptor definition */
typedef struct xdrTransports_tagPipeDescriptor{
  int pipeID; 
} xdrTransports_PipeDescriptor;


#define DummyChannel 0

/*-------------------------------------------------------*/
/* Pipe descriptor creation procedure. 
   Creates and fills pipe descriptor.
*/
extern xdrTransports_PipeDescriptor * xdrTransports_CreatePipeDescriptor(int pipeID);



/*-------------------------------------------------------*/
/* Pipe descriptor destroing procedure.
   Destroies pipe descriptor.
*/
extern void xdrTransports_DestroyPipeDescriptor(xdrTransports_PipeDescriptor * desc);



/*-------------------------------------------------------*/
/* Kernel notification procedure.
   Creates and fills pipe descriptor, notifies the kernel about accepted
   connection.
*/
extern void xdrTransports_NotifyKernel(
  int                                 msgID, 
  xdrTransports_TransportDescriptor * senderDesc,
  int                                 arg0
);

#define xdrTransports_MsgConnectionRequest  0  /* Create and fill pipe 
                                                  descriptor, notifie the 
                                                  kernel about accepted
                                                  connection. 'arg0' is pipe
                                                  identifier. */
#define xdrTransports_MsgTransportFailure   1  /* Notify the kernel about 
                                                  transport failure */




/*-------------------------------------------------------*/
/* Read and write functions (like standard ANSI C functions) */
extern int xdrTransports_Write(
  xdrTransports_PipeDescriptor * pipeDesc,
  int                            channelNo,
  char                         * buffer,
  int                            nbytes
);





/*-------------------------------------------------------*/
extern int xdrTransports_Read(
  xdrTransports_PipeDescriptor * pipeDesc,
  int                            channelNo,
  char                         * buffer,
  int                            maxbytes
);


#endif