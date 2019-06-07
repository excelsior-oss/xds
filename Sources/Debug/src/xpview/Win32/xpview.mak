# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=xpview - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to xpview - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "xpview - Win32 Release" && "$(CFG)" != "xpview - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "xpview.mak" CFG="xpview - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "xpview - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "xpview - Win32 Debug" (based on "Win32 (x86) Application")
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
# PROP Target_Last_Scanned "xpview - Win32 Debug"
RSC=rc.exe
MTL=mktyplib.exe
CPP=cl.exe

!IF  "$(CFG)" == "xpview - Win32 Release"

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
OUTDIR=..\..\..\workplace
INTDIR=.\Release

ALL : "$(OUTDIR)\xpview.exe"

CLEAN : 
	-@erase ".\Release\xpview.exe"
	-@erase ".\Release\prof.obj"
	-@erase ".\Release\profapi.obj"
	-@erase ".\Release\mdi.obj"
	-@erase ".\Release\prof.res"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /MT /W3 /EHsc /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /D"_CRT_SECURE_NO_DEPRECATE" /D"STRSAFE_NO_DEPRECATE"  \
 /Fp"$(INTDIR)/xpview.pch" /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x419 /d "NDEBUG"
# ADD RSC /l 0x419 /d "NDEBUG"
RSC_PROJ=/l 0x419 /fo"$(INTDIR)/prof.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/xpview.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib comctl32.lib /nologo /subsystem:windows /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib comctl32.lib /nologo\
 /subsystem:windows /incremental:no /pdb:"$(OUTDIR)/xpview.pdb" /machine:I386\
 /out:"$(OUTDIR)/xpview.exe" 
LINK32_OBJS= \
	"$(INTDIR)/prof.obj" \
	"$(INTDIR)/profapi.obj" \
	"$(INTDIR)/mdi.obj" \
	"$(INTDIR)/prof.res"

"$(OUTDIR)\xpview.exe" : "$(OUTDIR)" "$(INTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "xpview - Win32 Debug"

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
OUTDIR=..\..\..\workplace
INTDIR=.\Debug

ALL : "$(OUTDIR)\xpview.exe"

CLEAN : 
	-@erase ".\Debug\vc40.pdb"
	-@erase ".\Debug\vc40.idb"
	-@erase ".\Debug\xpview.exe"
	-@erase ".\Debug\profapi.obj"
	-@erase ".\Debug\mdi.obj"
	-@erase ".\Debug\prof.obj"
	-@erase ".\Debug\prof.res"
	-@erase ".\Debug\xpview.ilk"
	-@erase ".\Debug\xpview.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /MTd /W3 /Gm /EHsc /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D"_CRT_SECURE_NO_DEPRECATE" /D"STRSAFE_NO_DEPRECATE"  \
 /Fp"$(INTDIR)/xpview.pch" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x419 /d "_DEBUG"
# ADD RSC /l 0x419 /d "_DEBUG"
RSC_PROJ=/l 0x419 /fo"$(INTDIR)/prof.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/xpview.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib comctl32.lib /nologo /subsystem:windows /debug /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib comctl32.lib /nologo\
 /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)/xpview.pdb" /debug\
 /machine:I386 /out:"$(OUTDIR)/xpview.exe" 
LINK32_OBJS= \
	"$(INTDIR)/profapi.obj" \
	"$(INTDIR)/mdi.obj" \
	"$(INTDIR)/prof.obj" \
	"$(INTDIR)/prof.res"

"$(OUTDIR)\xpview.exe" : "$(OUTDIR)" "$(INTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

# Name "xpview - Win32 Release"
# Name "xpview - Win32 Debug"

!IF  "$(CFG)" == "xpview - Win32 Release"

!ELSEIF  "$(CFG)" == "xpview - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\Src\profapi.c
DEP_CPP_PROFA=\
	".\Src\profapi.h"\
	

"$(INTDIR)\profapi.obj" : $(SOURCE) $(DEP_CPP_PROFA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Src\prof.rc
DEP_RSC_PROF_=\
	".\Src\vsize.cur"\
	".\Src\hsize.cur"\
	".\Src\handcls.cur"\
	".\Src\modules.ico"\
	".\Src\module.ico"\
	".\Src\modtext.ico"\
	

!IF  "$(CFG)" == "xpview - Win32 Release"


"$(INTDIR)\prof.res" : $(SOURCE) $(DEP_RSC_PROF_) "$(INTDIR)"
   $(RSC) /l 0x419 /fo"$(INTDIR)/prof.res" /i "Src" /d "NDEBUG" $(SOURCE)


!ELSEIF  "$(CFG)" == "xpview - Win32 Debug"


"$(INTDIR)\prof.res" : $(SOURCE) $(DEP_RSC_PROF_) "$(INTDIR)"
   $(RSC) /l 0x419 /fo"$(INTDIR)/prof.res" /i "Src" /d "_DEBUG" $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\Src\prof.c

!IF  "$(CFG)" == "xpview - Win32 Release"

DEP_CPP_PROF_C=\
	".\Src\profapi.h"\
	".\src\mdi.h"\
	
NODEP_CPP_PROF_C=\
	".\Src\mod"\
	

"$(INTDIR)\prof.obj" : $(SOURCE) $(DEP_CPP_PROF_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "xpview - Win32 Debug"

DEP_CPP_PROF_C=\
	".\Src\profapi.h"\
	".\src\mdi.h"\
	

"$(INTDIR)\prof.obj" : $(SOURCE) $(DEP_CPP_PROF_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\Src\mdi.c
DEP_CPP_MDI_C=\
	".\src\mdi.h"\
	

"$(INTDIR)\mdi.obj" : $(SOURCE) $(DEP_CPP_MDI_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
# End Target
# End Project
################################################################################
