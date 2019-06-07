# Microsoft Developer Studio Generated NMAKE File, Format Version 40001
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=xrc - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to xrc - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "xrc - Win32 Release" && "$(CFG)" != "xrc - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "xrc.mak" CFG="xrc - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "xrc - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "xrc - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "xrc - Win32 Debug"
CPP=cl.exe
RSC=rc.exe

SRCDIR=.

!IF  "$(CFG)" == "xrc - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=..\workplace
INTDIR=..\ch

ALL : "$(OUTDIR)\xrc.exe"

CLEAN : 
	-@erase "$(OUTDIR)\xrc.exe"
	-@erase "$(INTDIR)\rcscan.obj"
	-@erase "$(INTDIR)\RCEXPR.OBJ"
	-@erase "$(INTDIR)\rcio.obj"
	-@erase "$(INTDIR)\rcerr.obj"
	-@erase "$(INTDIR)\rcsup.obj"
	-@erase "$(INTDIR)\rcparse.obj"
	-@erase "$(INTDIR)\rcstb.obj"
	-@erase "$(INTDIR)\xrc.obj"
	-@erase "$(INTDIR)\xrc.res"
	-@erase "$(INTDIR)\WINMAKE.OBJ"
	-@erase ".\XRCMSG.h"
	-@erase ".\XRCMSG.rc"
	-@erase ".\MSG00001.bin"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
CPP_PROJ=/nologo /MT /W3 /EHsc /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE"\
 /D"_CRT_SECURE_NO_DEPRECATE" /D"STRSAFE_NO_DEPRECATE"  \
 /Fp"$(INTDIR)/xrc.pch" /Fo"$(INTDIR)/" /c 
CPP_OBJS=$(INTDIR)/
CPP_SBRS=
# ADD BASE RSC /l 0x419 /d "NDEBUG"
# ADD RSC /l 0x419 /d "NDEBUG"
RSC_PROJ=/l 0x419 /fo"$(INTDIR)/xrc.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/xrc.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib version.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib version.lib /nologo\
 /subsystem:console /incremental:no /pdb:"$(OUTDIR)/xrc.pdb" /machine:I386\
 /out:"$(OUTDIR)/xrc.exe" 
LINK32_OBJS= \
	"$(INTDIR)/rcscan.obj" \
	"$(INTDIR)/RCEXPR.OBJ" \
	"$(INTDIR)/rcio.obj" \
	"$(INTDIR)/rcerr.obj" \
	"$(INTDIR)/rcsup.obj" \
	"$(INTDIR)/rcparse.obj" \
	"$(INTDIR)/rcstb.obj" \
	"$(INTDIR)/xrc.obj" \
	"$(INTDIR)/WINMAKE.OBJ" \
	"$(INTDIR)/xrc.res"

"$(OUTDIR)\xrc.exe" : "$(OUTDIR)" "$(INTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "xrc - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=..\workplace
INTDIR=..\ch

ALL : "$(OUTDIR)\xrc.exe" "$(OUTDIR)\xrc.bsc"

CLEAN : 
	-@erase "$(OUTDIR)\xrc.exe"
	-@erase "$(OUTDIR)\xrc.pdb"
	-@erase "$(OUTDIR)\xrc.ilk"
	-@erase "$(OUTDIR)\xrc.bsc"
	-@erase "$(INTDIR)\vc90.pdb"
	-@erase "$(INTDIR)\vc90.idb"
	-@erase "$(INTDIR)\rcerr.sbr"
	-@erase "$(INTDIR)\rcsup.sbr"
	-@erase "$(INTDIR)\xrc.sbr"
	-@erase "$(INTDIR)\RCEXPR.SBR"
	-@erase "$(INTDIR)\rcstb.sbr"
	-@erase "$(INTDIR)\rcparse.sbr"
	-@erase "$(INTDIR)\rcscan.sbr"
	-@erase "$(INTDIR)\rcio.sbr"
	-@erase "$(INTDIR)\WINMAKE.SBR"
	-@erase "$(INTDIR)\rcio.obj"
	-@erase "$(INTDIR)\rcerr.obj"
	-@erase "$(INTDIR)\rcsup.obj"
	-@erase "$(INTDIR)\xrc.obj"
	-@erase "$(INTDIR)\RCEXPR.OBJ"
	-@erase "$(INTDIR)\rcstb.obj"
	-@erase "$(INTDIR)\rcparse.obj"
	-@erase "$(INTDIR)\rcscan.obj"
	-@erase "$(INTDIR)\xrc.res"
	-@erase "$(INTDIR)\WINMAKE.OBJ"
	-@erase ".\XRCMSG.h"
	-@erase ".\XRCMSG.rc"
	-@erase ".\MSG00001.bin"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /FR /YX /c
CPP_PROJ=/nologo /MTd /W3 /Gm /EHsc /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE"\
 /D"_CRT_SECURE_NO_DEPRECATE" /D"STRSAFE_NO_DEPRECATE"  \
 /FR"$(INTDIR)/" /Fp"$(INTDIR)/xrc.pch" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=$(INTDIR)/
CPP_SBRS=$(INTDIR)/
# ADD BASE RSC /l 0x419 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/xrc.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/xrc.bsc" 
BSC32_SBRS= \
	"$(INTDIR)/rcerr.sbr" \
	"$(INTDIR)/rcsup.sbr" \
	"$(INTDIR)/xrc.sbr" \
	"$(INTDIR)/RCEXPR.SBR" \
	"$(INTDIR)/rcstb.sbr" \
	"$(INTDIR)/rcparse.sbr" \
	"$(INTDIR)/rcscan.sbr" \
	"$(INTDIR)/rcio.sbr" \
	"$(INTDIR)/WINMAKE.SBR"

"$(OUTDIR)\xrc.bsc" : "$(OUTDIR)" "$(INTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib version.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib version.lib /nologo\
 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)/xrc.pdb" /debug\
 /machine:I386 /out:"$(OUTDIR)/xrc.exe" 
LINK32_OBJS= \
	"$(INTDIR)/rcio.obj" \
	"$(INTDIR)/rcerr.obj" \
	"$(INTDIR)/rcsup.obj" \
	"$(INTDIR)/xrc.obj" \
	"$(INTDIR)/RCEXPR.OBJ" \
	"$(INTDIR)/rcstb.obj" \
	"$(INTDIR)/rcparse.obj" \
	"$(INTDIR)/rcscan.obj" \
	"$(INTDIR)/WINMAKE.OBJ" \
	"$(INTDIR)/xrc.res"

"$(OUTDIR)\xrc.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.c{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Target

# Name "xrc - Win32 Release"
# Name "xrc - Win32 Debug"

!IF  "$(CFG)" == "xrc - Win32 Release"

!ELSEIF  "$(CFG)" == "xrc - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=$(SRCDIR)\xrc.c
DEP_CPP_XRC_C=\
	"$(SRCDIR)\rcerr.h"\
	"$(SRCDIR)\rcio.h"\
	"$(SRCDIR)\rcsup.h"\
	"$(SRCDIR)\rcstb.h"\
	"$(SRCDIR)\RCSCAN.H"\
	"$(SRCDIR)\rcparse.h"\
	"$(SRCDIR)\KEYS.H"\
	"$(SRCDIR)\DLMS.H"\
	".\XRCMSG.h"\
	

!IF  "$(CFG)" == "xrc - Win32 Release"


"$(INTDIR)\xrc.obj" : $(SOURCE) $(DEP_CPP_XRC_C) "$(INTDIR)" ".\XRCMSG.h"


!ELSEIF  "$(CFG)" == "xrc - Win32 Debug"


"$(INTDIR)\xrc.obj" : $(SOURCE) $(DEP_CPP_XRC_C) "$(INTDIR)" ".\XRCMSG.h"

"$(INTDIR)\xrc.sbr" : $(SOURCE) $(DEP_CPP_XRC_C) "$(INTDIR)" ".\XRCMSG.h"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(SRCDIR)\rcio.c
DEP_CPP_RCIO_=\
	{$(INCLUDE)}"\sys\TYPES.H"\
	{$(INCLUDE)}"\sys\STAT.H"\
	"$(SRCDIR)\winmake.h"\
	"$(SRCDIR)\rcio.h"\
	"$(SRCDIR)\rcerr.h"\
	"$(SRCDIR)\rcsup.h"\
	".\XRCMSG.h"\
	

!IF  "$(CFG)" == "xrc - Win32 Release"


"$(INTDIR)\rcio.obj" : $(SOURCE) $(DEP_CPP_RCIO_) "$(INTDIR)" ".\XRCMSG.h"


!ELSEIF  "$(CFG)" == "xrc - Win32 Debug"


"$(INTDIR)\rcio.obj" : $(SOURCE) $(DEP_CPP_RCIO_) "$(INTDIR)" ".\XRCMSG.h"

"$(INTDIR)\rcio.sbr" : $(SOURCE) $(DEP_CPP_RCIO_) "$(INTDIR)" ".\XRCMSG.h"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(SRCDIR)\rcparse.c
DEP_CPP_RCPAR=\
	"$(SRCDIR)\rcparse.h"\
	"$(SRCDIR)\rcexpr.h"\
	"$(SRCDIR)\rcio.h"\
	"$(SRCDIR)\RCSCAN.H"\
	"$(SRCDIR)\rcsup.h"\
	"$(SRCDIR)\rcerr.h"\
	"$(SRCDIR)\KEYS.H"\
	"$(SRCDIR)\DLMS.H"\
	".\XRCMSG.h"\
	

!IF  "$(CFG)" == "xrc - Win32 Release"


"$(INTDIR)\rcparse.obj" : $(SOURCE) $(DEP_CPP_RCPAR) "$(INTDIR)" ".\XRCMSG.h"


!ELSEIF  "$(CFG)" == "xrc - Win32 Debug"


"$(INTDIR)\rcparse.obj" : $(SOURCE) $(DEP_CPP_RCPAR) "$(INTDIR)" ".\XRCMSG.h"

"$(INTDIR)\rcparse.sbr" : $(SOURCE) $(DEP_CPP_RCPAR) "$(INTDIR)" ".\XRCMSG.h"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(SRCDIR)\rcerr.c
DEP_CPP_RCERR=\
	"$(SRCDIR)\winmake.h"\
	"$(SRCDIR)\rcio.h"\
	"$(SRCDIR)\rcerr.h"\
	".\XRCMSG.h"\
	

!IF  "$(CFG)" == "xrc - Win32 Release"


"$(INTDIR)\rcerr.obj" : $(SOURCE) $(DEP_CPP_RCERR) "$(INTDIR)" ".\XRCMSG.h"


!ELSEIF  "$(CFG)" == "xrc - Win32 Debug"


"$(INTDIR)\rcerr.obj" : $(SOURCE) $(DEP_CPP_RCERR) "$(INTDIR)" ".\XRCMSG.h"

"$(INTDIR)\rcerr.sbr" : $(SOURCE) $(DEP_CPP_RCERR) "$(INTDIR)" ".\XRCMSG.h"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(SRCDIR)\rcscan.c
DEP_CPP_RCSCA=\
	"$(SRCDIR)\rcerr.h"\
	"$(SRCDIR)\rcio.h"\
	"$(SRCDIR)\rcsup.h"\
	"$(SRCDIR)\rcstb.h"\
	"$(SRCDIR)\rcexpr.h"\
	"$(SRCDIR)\RCSCAN.H"\
	"$(SRCDIR)\KEYS.H"\
	"$(SRCDIR)\DLMS.H"\
	".\XRCMSG.h"\
	

!IF  "$(CFG)" == "xrc - Win32 Release"


"$(INTDIR)\rcscan.obj" : $(SOURCE) $(DEP_CPP_RCSCA) "$(INTDIR)" ".\XRCMSG.h"


!ELSEIF  "$(CFG)" == "xrc - Win32 Debug"


"$(INTDIR)\rcscan.obj" : $(SOURCE) $(DEP_CPP_RCSCA) "$(INTDIR)" ".\XRCMSG.h"

"$(INTDIR)\rcscan.sbr" : $(SOURCE) $(DEP_CPP_RCSCA) "$(INTDIR)" ".\XRCMSG.h"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(SRCDIR)\rcstb.c
DEP_CPP_RCSTB=\
	"$(SRCDIR)\rcsup.h"\
	"$(SRCDIR)\rcerr.h"\
	"$(SRCDIR)\rcstb.h"\
	".\XRCMSG.h"\
	

!IF  "$(CFG)" == "xrc - Win32 Release"


"$(INTDIR)\rcstb.obj" : $(SOURCE) $(DEP_CPP_RCSTB) "$(INTDIR)" ".\XRCMSG.h"


!ELSEIF  "$(CFG)" == "xrc - Win32 Debug"


"$(INTDIR)\rcstb.obj" : $(SOURCE) $(DEP_CPP_RCSTB) "$(INTDIR)" ".\XRCMSG.h"

"$(INTDIR)\rcstb.sbr" : $(SOURCE) $(DEP_CPP_RCSTB) "$(INTDIR)" ".\XRCMSG.h"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(SRCDIR)\rcsup.c
DEP_CPP_RCSUP=\
	"$(SRCDIR)\rcsup.h"\
	"$(SRCDIR)\rcerr.h"\
	".\XRCMSG.h"\
	

!IF  "$(CFG)" == "xrc - Win32 Release"


"$(INTDIR)\rcsup.obj" : $(SOURCE) $(DEP_CPP_RCSUP) "$(INTDIR)" ".\XRCMSG.h"


!ELSEIF  "$(CFG)" == "xrc - Win32 Debug"


"$(INTDIR)\rcsup.obj" : $(SOURCE) $(DEP_CPP_RCSUP) "$(INTDIR)" ".\XRCMSG.h"

"$(INTDIR)\rcsup.sbr" : $(SOURCE) $(DEP_CPP_RCSUP) "$(INTDIR)" ".\XRCMSG.h"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(SRCDIR)\XRCMSG.MC

!IF  "$(CFG)" == "xrc - Win32 Release"

# Begin Custom Build - Message compile
InputDir=.
InputPath=$(SRCDIR)\XRCMSG.MC
InputName=XRCMSG

BuildCmds= \
	mc $(InputPath) \
	

"$(InputDir)\$(InputName).h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"$(InputDir)\$(InputName).rc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"$(InputDir)\MSG00001.BIN" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "xrc - Win32 Debug"

# Begin Custom Build - Message compile
InputDir=.
InputPath=$(SRCDIR)\XRCMSG.MC
InputName=XRCMSG

BuildCmds= \
	mc $(InputPath) \
	

"$(InputDir)\$(InputName).h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"$(InputDir)\$(InputName).rc" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"$(InputDir)\MSG00001.BIN" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(SRCDIR)\xrc.rc
DEP_RSC_XRC_R=\
	".\XRCMSG.rc"\
	".\MSG00001.bin"\
	

"$(INTDIR)\xrc.res" : $(SOURCE) $(DEP_RSC_XRC_R) "$(INTDIR)" ".\XRCMSG.rc"\
 ".\MSG00001.bin"
   $(RSC) $(RSC_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=$(SRCDIR)\RCEXPR.C
DEP_CPP_RCEXP=\
	"$(SRCDIR)\rcexpr.h"\
	"$(SRCDIR)\RCSCAN.H"\
	"$(SRCDIR)\rcerr.h"\
	"$(SRCDIR)\KEYS.H"\
	"$(SRCDIR)\DLMS.H"\
	".\XRCMSG.h"\
	

!IF  "$(CFG)" == "xrc - Win32 Release"


"$(INTDIR)\RCEXPR.OBJ" : $(SOURCE) $(DEP_CPP_RCEXP) "$(INTDIR)" ".\XRCMSG.h"


!ELSEIF  "$(CFG)" == "xrc - Win32 Debug"


"$(INTDIR)\RCEXPR.OBJ" : $(SOURCE) $(DEP_CPP_RCEXP) "$(INTDIR)" ".\XRCMSG.h"

"$(INTDIR)\RCEXPR.SBR" : $(SOURCE) $(DEP_CPP_RCEXP) "$(INTDIR)" ".\XRCMSG.h"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(SRCDIR)\WINMAKE.C
DEP_CPP_WINMA=\
	"$(SRCDIR)\winmake.h"\
	

!IF  "$(CFG)" == "xrc - Win32 Release"


"$(INTDIR)\WINMAKE.OBJ" : $(SOURCE) $(DEP_CPP_WINMA) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "xrc - Win32 Debug"


"$(INTDIR)\WINMAKE.OBJ" : $(SOURCE) $(DEP_CPP_WINMA) "$(INTDIR)"

"$(INTDIR)\WINMAKE.SBR" : $(SOURCE) $(DEP_CPP_WINMA) "$(INTDIR)"


!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
