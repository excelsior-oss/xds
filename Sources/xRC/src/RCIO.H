void ParsePath(const char *paths);
void FileIni(const char *iname,const char *oname,int progress);
int IsShell(void);

int InputIsH(void);
int InputRead(char *buf,int len);
void InputInclude(const char *name,int sys);
char *InputGetName(void);
int InputGetLineNumber(void);

void OutputWrite(const void *buf,int len);
void OutputClose(void);
void OutputDelete(void);
long OutputGetPos(void);
void OutputSetPos(long pos);

void AuxOpen(const char *name);
void AuxSetPos(long pos);
int AuxRead(char *buf,int len);
void AuxClose(void);

