#include <windows.h>
#include "profapi.h"

#if 1

HANDLE	profapi_lib;

int	(* pLoadDebugInfo)	(char * name);
void	(* pClearDebugInfo)	(void);
int	(* pGetSnapshots)	(void);
char *	(* pComponentName)	(int nCom);
int	(* pN_Parts)		(int nCom);
int	(* pComponentSnapshots) (int nCom);
int	(* pGetUnknownParts)	(int nCom);
char *	(* pPublicName)		(int nCom, int nPublic);
int	(* pPublicSnapshots)	(int nCom, int nPublic);
void	(* pPublicAttr)		(int nCom, int nPublic, unsigned * addr, unsigned * length);
char *	(* pModuleName)		(int nCom, int nModule);
char *	(* pSourceName)		(int nCom, int nModule);
int	(* pModuleSnapshots)	(int nCom, int nModule);
int	(* pLineSnapshots)	(int nCom, int nModule, int nLine);
int	(* pN_Proc)		(int nCom, int nModule);
char *	(* pProcName)		(int nCom, int nModule, int nProc);
BOOL	(* pProcSnapshots)	(int nCom, int nModule, int nProc);
BOOL	(* pProcBounds)		(int nCom, int nModule, int nProc, int * begin, int * end);
unsigned (* pUtility)           (void);
BOOL    (* pProcInfo)           (int nCom, int nModule, int nProc, PROFDATA * info);
int     (* pN_Call)		(int nCom, int nModule, int nProc);
int     (* pCallCount)          (int nCom, int nModule, int nProc, int nCall);
BOOL    (* pCallPlace)          (int nCom, int nModule, int nProc, int nCall, int * c, int * m, int * l);
BOOL	(* pGetExecutionTime)	(__int64 * time);

int	LoadDebugInfo (char * name)
{
	return pLoadDebugInfo (name);
}
void	ClearDebugInfo	(void)
{
	pClearDebugInfo();
}

int	GetSnapshots (void)
{
	return pGetSnapshots ();
}

char *	ComponentName (int nCom)
{
	return pComponentName (nCom);
}

int	N_Parts (int nCom)
{
	return pN_Parts (nCom);
}

int	ComponentSnapshots (int nCom)
{
	return pComponentSnapshots (nCom);
}


char *	PublicName (int nCom, int nPublic)
{
	return pPublicName (nCom, nPublic);
}

int	PublicSnapshots (int nCom, int nPublic)
{
	return pPublicSnapshots (nCom, nPublic);
}

void	PublicAttr (int nCom, int nPublic, unsigned * addr, unsigned * length)
{
	pPublicAttr (nCom, nPublic, addr, length);
}

int	GetUnknownParts (int nCom)
{
	return pGetUnknownParts (nCom);
}

char *	ModuleName (int nCom, int nModule)
{
	return pModuleName (nCom, nModule);
}

char *	SourceName (int nCom, int nModule)
{
	return pSourceName (nCom, nModule);
}

int	ModuleSnapshots (int nCom, int nModule)
{
	return pModuleSnapshots (nCom, nModule);
}

int	N_Proc	(int nCom, int nModule)
{
	return pN_Proc (nCom, nModule);
}

char *	ProcName (int nCom, int nModule, int nProc)
{
	return pProcName (nCom, nModule, nProc);
}

BOOL	ProcSnapshots (int nCom, int nModule, int nProc)
{
	return pProcSnapshots (nCom, nModule, nProc);
}

BOOL	ProcBounds (int nCom, int nModule, int nProc, int * begin, int * end)
{
	return pProcBounds (nCom, nModule, nProc, begin, end);
}

int	LineSnapshots (int nCom, int nModule, int nLine)
{
	return pLineSnapshots (nCom, nModule, nLine);
}

unsigned Utility        (void)
{
        return pUtility();
}

BOOL    ProcInfo        (int nCom, int nModule, int nProc, PROFDATA * info)
{
        return pProcInfo(nCom,nModule,nProc,info);
}

int     N_Call		(int nCom, int nModule, int nProc)
{
        return pN_Call(nCom,nModule,nProc);
}

int     CallCount        (int nCom, int nModule, int nProc, int nCall)
{
        return pCallCount(nCom,nModule,nProc,nCall);
}

BOOL    CallPlace        (int nCom, int nModule, int nProc, int nCall,  int * c, int * m, int * l)
{
        return pCallPlace(nCom,nModule,nProc,nCall,c,m,l);
}

BOOL	GetExecutionTime(__int64 * time)
{
	return pGetExecutionTime(time);
}


BOOL	Profapi_init (void)
{
	char s [2000];
	char * p, * q;
	if (profapi_lib) return TRUE;

	GetModuleFileName (0, s, sizeof (s));
	p = s;
	for (q = s; *q; q++)
		if (*q=='\\') p = q+1;

	strcpy (p, "xprofapi.dll");
	profapi_lib = LoadLibrary (s);
	if (!profapi_lib) return FALSE;

	pLoadDebugInfo	 = (int	   (*) (char *))		GetProcAddress (profapi_lib, "LoadDebugInfo");
	pClearDebugInfo	 = (void   (*) (void))			GetProcAddress (profapi_lib, "ClearDebugInfo");
	pGetSnapshots	 = (int	   (*) (void))			GetProcAddress (profapi_lib, "GetSnapshots");
	pComponentName	 = (char * (*) (int))			GetProcAddress (profapi_lib, "ComponentName");
	pN_Parts	 = (int    (*) (int))			GetProcAddress (profapi_lib, "N_Parts");
	pComponentSnapshots = (int (*) (int))			GetProcAddress (profapi_lib, "ComponentSnapshots");
	pGetUnknownParts =  (int   (*) (int))			GetProcAddress (profapi_lib, "GetUnknownParts");
	pPublicName	 = (char * (*) (int, int))		GetProcAddress (profapi_lib, "PublicName");
	pPublicSnapshots = (int    (*) (int, int))		GetProcAddress (profapi_lib, "PublicSnapshots");
	pPublicAttr	 = (void   (*) (int, int, unsigned*, unsigned*)) GetProcAddress (profapi_lib, "PublicAttr");
	pModuleName	 = (char * (*) (int, int))			GetProcAddress (profapi_lib, "ModuleName");
	pSourceName	 = (char * (*) (int, int))			GetProcAddress (profapi_lib, "SourceName");
	pModuleSnapshots = (int	   (*) (int, int))			GetProcAddress (profapi_lib, "ModuleSnapshots");
	pN_Proc		 = (int	   (*) (int, int))			GetProcAddress (profapi_lib, "N_Proc");
	pProcName	 = (char * (*) (int, int, int))		GetProcAddress (profapi_lib, "ProcName");
	pProcSnapshots	 = (BOOL   (*) (int, int, int))		GetProcAddress (profapi_lib, "ProcSnapshots");
	pProcBounds	 = (BOOL   (*) (int, int, int, int*, int*))	GetProcAddress (profapi_lib, "ProcBounds");
	pLineSnapshots	 = (int	   (*) (int, int, int))		 GetProcAddress (profapi_lib, "LineSnapshots");
	pUtility         = (unsigned (*) (void))                 GetProcAddress (profapi_lib, "Utility");
	pProcInfo        = (BOOL   (*) (int,int,int,PROFDATA *)) GetProcAddress (profapi_lib, "ProcInfo");
	pN_Call		 = (int    (*) (int,int,int))            GetProcAddress (profapi_lib, "N_Call");
	pCallCount       = (int    (*) (int,int,int,int))        GetProcAddress (profapi_lib, "CallCount");
	pCallPlace       = (BOOL   (*) (int,int,int,int, int *,int *,int *)) GetProcAddress (profapi_lib, "CallPlace");
	pGetExecutionTime= (BOOL   (*) (__int64 * time))	 GetProcAddress (profapi_lib, "GetExecutionTime");

	if (
		!pLoadDebugInfo
	||	!pClearDebugInfo
	||	!pGetSnapshots
	||	!pComponentName
	||	!pN_Parts
	||	!pComponentSnapshots
	||	!pGetUnknownParts
	||	!pPublicName
	||	!pPublicSnapshots
	||	!pPublicAttr
	||	!pModuleName
	||	!pSourceName
	||	!pModuleSnapshots
	||	!pN_Proc
	||	!pProcName
	||	!pProcSnapshots
	||	!pProcBounds
	||	!pLineSnapshots
        ||      !pUtility              
        ||      !pProcInfo
        ||      !pN_Call
        ||      !pCallCount
        ||      !pCallPlace
	||	!pGetExecutionTime)
				return FALSE;
	return TRUE;
}

#endif