
#ifndef XDEFS_H
#define XDEFS_H

#if defined(__IBMCPP__)
#define XCDECL _Optlink
#define XEXPORT
#elif defined (_MSC_VER)
#define XCDECL   __cdecl
#define XEXPORT  __declspec(dllexport) __stdcall
#elif defined(xos_LINUX)
#define XCDECL
#define XEXPORT extern
#else
#define XCDECL
#define XEXPORT
#endif

#define far
#define near

#ifdef xos_LINUX
  #define open_namespace namespace xlink_namespace {
  #define close_namespace } // xlink_namespace
  #define from_namespace xlink_namespace::
#else
  #define open_namespace
  #define close_namespace
  #define from_namespace
#endif

/*----------------------------------------------------------------------------*/

typedef unsigned char  byte;
typedef unsigned short word;
typedef unsigned       dword;

typedef unsigned short unichar;

typedef char           Bool;

#define false   0
#define true    1

#ifndef max
#define max(a,b)    (((a) > (b)) ? (a) : (b))
#endif

/*----------------------------------------------------------------------------*/

#endif
