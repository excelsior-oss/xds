typedef int BOOL;
 
#define LDI_Error                   0  /* Error */
#define LDI_OpenErrorProfilerData   -1 /* Trace file open error */
#define LDI_ReadErrorProfilerData   -2 /* Profiler Trace file read error */
#define LDI_ReadDebugInfo           -3 /* Debugging information read error */
#define LDI_WrongFormatProfilerData -4 /* Wrong format or trace file */
#define LDI_IsNot_XDS_ProfilerTraceFile -5 /* Is not XDS profiler trace file */

#define UT_UNKNOWN             0
#define UT_TRACE_EXECUTION     1
#define UT_TRACE_MEMORY        2
#define UT_TRACE_CALLS_PROFILE 3

typedef struct {
//    ip                : ADDRESS;
      unsigned ip;
      unsigned count;
      void * next;
} CALL;

typedef struct {
      unsigned pure_dur_lo;
      unsigned pure_dur_hi;
      unsigned dirty_dur_lo;
      unsigned dirty_dur_hi;
      unsigned total_entry_count;
      unsigned norec_entry_count;
} PROFDATA;

BOOL	Profapi_init	(void);

unsigned Utility        (void);
int	LoadDebugInfo	(char * name);
void	ClearDebugInfo	(void);
int	GetSnapshots	(void);
char *	ComponentName	(int nCom);
int	N_Parts		(int nCom);
int	ComponentSnapshots (int nCom);
int	GetUnknownParts (int nCom);
char *	PublicName	(int nCom, int nPublic);
int	PublicSnapshots (int nCom, int nPublic);
void	PublicAttr	(int nCom, int nPublic, unsigned * addr, unsigned * length);
char *	ModuleName	(int nCom, int nModule);
char *	SourceName	(int nCom, int nModule);
int	ModuleSnapshots (int nCom, int nModule);
int	LineSnapshots   (int nCom, int nModule, int nLine);
int	N_Proc		(int nCom, int nModule);
char *	ProcName	(int nCom, int nModule, int nProc);
BOOL	ProcSnapshots	(int nCom, int nModule, int nProc);
BOOL    ProcInfo        (int nCom, int nModule, int nProc, PROFDATA * info);
BOOL	ProcBounds	(int nCom, int nModule, int nProc, int * begin, int * end);
int     N_Call		(int nCom, int nModule, int nProc);
int     CallCount       (int nCom, int nModule, int nProc, int nCall);
BOOL    CallPlace       (int nCom, int nModule, int nProc, int nCall, int * c, int * m, int * l);
BOOL	GetExecutionTime(__int64 * time);
