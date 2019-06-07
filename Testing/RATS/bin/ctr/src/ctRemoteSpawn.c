/***    Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   Remote Spawn for CTROUT(CityRout) - Compiler Testing Routine
                                                Alexs: 3-Feb-98
*/

#include "ctAssert.h"
#include "ctCfg.h"

#ifdef ctHostOS_Unix

#include <sys/types.h>
#include <stdio.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <string.h>
#include <fcntl.h>

#define BUFF_SIZE 20 * 1024

#define xGet(x) if(read(sock, &(x), sizeof(x)) != sizeof(x)){printf("Fatal error 1\n"); return;}
#define xSend(var) write(sock, &(var), sizeof(var))


static long remoteSpawn1(int sock, char * path, char * entry){
  char buff[BUFF_SIZE], name[100];
  long ret_code, len;
  int fd;

  if((fd = open(path, O_RDONLY, 0)) == -1){
    printf("File %s not found\n", path);
    return -1;
  }else{
    /* Read object module */
    len = read(fd, &buff, BUFF_SIZE);
    close(fd);

    /* Send object module length */
    xSend(len);

    /* Get confirmation */
    xGet(ret_code);
    if(!ret_code){
      printf("Fatal error 2\n");
      return -1;
    };

    /* Send object module */
    write(sock, buff, len);

    /* Get confirmation */
    xGet(ret_code);
    if(!ret_code){
      printf("Fatal error 2\n");
      return -1;
    };

    /* Send symbol name */
    sprintf(buff, "_%s", entry);
/*    buff[strlen(buff)-2] = 0; */
    write(sock, buff, strlen(buff));

    /* Get return code */
    xGet(ret_code);
    return ret_code;
  };
};


static long remoteSpawn(int sock, char * path, char * entry){
  long ret_code = 153;

  /* Get return code */
  xGet(ret_code);
  return ret_code;
};

long RemoteSpawn_Spawn(Cfg_Configurator_t * cfg, char * path, char * entry){
  struct sockaddr_in server_address;
  int sock;
  long ret_code;

  sock = socket(AF_INET, SOCK_STREAM, 0);

  server_address.sin_family = AF_INET;
  server_address.sin_port   = 5001;
  server_address.sin_addr.s_addr = inet_addr(Cfg_GetInetAddress(cfg));

  connect(sock, (struct sockaddr *)&server_address, sizeof(server_address));

  ret_code = remoteSpawn(sock, path, entry);
  close(sock);
  return ret_code;

};

#else
long RemoteSpawn_Spawn(Cfg_Configurator_t * cfg, char * path, char * entry){
  ASSERT(0);

};

#endif