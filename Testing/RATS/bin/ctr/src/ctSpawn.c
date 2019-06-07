/***    Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   Spawn for CTROUT(CityRout) - Compiler Testing Routine
                                        Alexs: 23-Nov-96
*/

#include "ctSpawn.h"

#ifdef ctHostOS_UNIX

#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

#else

#include <process.h>

#endif

long Spawn_Spawn( char *path, char * args[], int fd )

#ifdef ctHostOS_UNIX
{
	pid_t pid;
	int stat;
    int fp;

	pid=fork();
	if (pid<0) return -1;
    if (!pid) { /* child */
		if(fd != -1){
			dup2(fd, 1);
			dup2(fd, 2);
		};
		execv(path,(char**)args);
		exit(255);
	}
    else { /* parent */
		pid=waitpid(pid,&stat,0);
		if (WIFEXITED(stat)) return WEXITSTATUS(stat);
		return 255;
	}
}

#else
{
  return spawnvp( P_WAIT, path, args );
};
#endif
