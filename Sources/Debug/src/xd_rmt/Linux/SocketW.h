#ifndef SocketW_H_
#define SocketW_H_
#include "X2C.h"

#include <sys/socket.h>

typedef unsigned UINT;

typedef X2C_CARD16 USHORT;

typedef X2C_CARD32 ULONG;

typedef X2C_CHAR UCHAR;

typedef X2C_CARD32 BOOL;

typedef int INT;

typedef X2C_INT16 SHORT;

typedef X2C_INT32 LONG;

typedef X2C_CHAR * PSTR;

typedef PSTR * PPSTR;

typedef X2C_LOC * PBYTE;

typedef PBYTE PCBYTE;

typedef X2C_CARD32 * p_u_long;

typedef unsigned SOCKET;

#define INVALID_SOCKET X2C_max_longcard

#define SOCKET_ERROR (-1)

#define SOCK_STREAM 1

#define SOCK_DGRAM 2

#define SOCK_RAW 3

#define SOCK_RDM 4

#define SOCK_SEQPACKET 5

typedef struct sockaddr SOCKADDR;

typedef struct sockaddr * PSOCKADDR;

union in_addr;


union in_addr {
   struct {
      X2C_CHAR s_b1;
      X2C_CHAR s_b2;
      X2C_CHAR s_b3;
      X2C_CHAR s_b4;
   } _;
   struct {
      X2C_CARD16 s_w1;
      X2C_CARD16 s_w2;
   } _0;
   X2C_CARD32 S_addr;
   X2C_CARD32 s_addr;
   struct {
      X2C_CHAR s_net;
      X2C_CHAR s_host;
      X2C_CHAR s_lh;
      X2C_CHAR s_impno;
   } _1;
   struct {
      X2C_CARD16 s_dummy;
      X2C_CARD16 s_imp;
   } _2;
};

#define AF_UNSPEC 0

#define AF_UNIX 1

#define AF_INET 2

#define AF_IMPLINK 3

#define AF_PUP 4

#define AF_CHAOS 5

#define AF_IPX 6

#define AF_NS 6

#define AF_ISO 7

#define AF_OSI 7

#define AF_ECMA 8

#define AF_DATAKIT 9

#define AF_CCITT 10

#define AF_SNA 11

#define AF_DECnet 12

#define AF_DLI 13

#define AF_LAT 14

#define AF_HYLINK 15

#define AF_APPLETALK 16

#define AF_NETBIOS 17

#define AF_VOICEVIEW 18

#define AF_MAX 19

extern X2C_CARD32 IN_CLASSA(X2C_CARD32);

#define IN_CLASSA_NET 0x0FF000000

#define IN_CLASSA_NSHIFT 24

#define IN_CLASSA_HOST 0xFFFFFF 

#define IN_CLASSA_MAX 128

extern X2C_CARD32 IN_CLASSB(X2C_CARD32);

#define IN_CLASSB_NET 0x0FFFF0000

#define IN_CLASSB_NSHIFT 16

#define IN_CLASSB_HOST 0xFFFF 

#define IN_CLASSB_MAX 65536

extern X2C_CARD32 IN_CLASSC(X2C_CARD32);

#define IN_CLASSC_NET 0x0FFFFFF00

#define IN_CLASSC_NSHIFT 8

#define IN_CLASSC_HOST 0xFF 

#define INADDR_ANY 0x0 

#define INADDR_LOOPBACK 0x7F000001 

#define INADDR_BROADCAST 0x0FFFFFFFF

#define INADDR_NONE 0x0FFFFFFFF

#define PF_UNSPEC 0

#define PF_UNIX 1

#define PF_INET 2

#define PF_IMPLINK 3

#define PF_PUP 4

#define PF_CHAOS 5

#define PF_NS 6

#define PF_IPX 6

#define PF_ISO 7

#define PF_OSI 7

#define PF_ECMA 8

#define PF_DATAKIT 9

#define PF_CCITT 10

#define PF_SNA 11

#define PF_DECnet 12

#define PF_DLI 13

#define PF_LAT 14

#define PF_HYLINK 15

#define PF_APPLETALK 16

#define PF_VOICEVIEW 18

#define PF_MAX 19

struct sockaddr_in;


struct sockaddr_in {
   X2C_INT16 sin_family;
   X2C_CARD16 sin_port;
   union in_addr sin_addr;
   X2C_CHAR sin_zero[8];
};

typedef struct sockaddr_in SOCKADDR_IN;

typedef struct sockaddr_in * PSOCKADDR_IN;

struct hostent;


struct hostent {
   PSTR h_name;
   PPSTR h_aliases;
   X2C_INT16 h_addrtype;
   X2C_INT16 h_length;
   PPSTR h_addr_list;
};

typedef struct hostent * p_hostent;

typedef struct hostent HOSTENT;

typedef struct hostent * PHOSTENT;

extern X2C_CARD16 htons(X2C_CARD16);

#define closesocket close

extern p_hostent gethostbyname(X2C_CHAR []);


#endif /* SocketW_H_ */
