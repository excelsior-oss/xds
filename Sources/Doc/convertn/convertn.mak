# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=convertn - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to convertn - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "convertn - Win32 Release" && "$(CFG)" !=\
 "convertn - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "convertn.mak" CFG="convertn - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "convertn - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "convertn - Win32 Debug" (based on "Win32 (x86) Console Application")
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
# PROP Target_Last_Scanned "convertn - Win32 Debug"
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "convertn - Win32 Release"

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
OUTDIR=.\workplace
INTDIR=.\ch\Release
# Begin Custom Macros
ProjDir=.
TargetName=convertn
# End Custom Macros

ALL : "$(OUTDIR)\convertn.exe"

CLEAN : 
	-@erase "$(INTDIR)\genhtml.obj"
	-@erase "$(INTDIR)\genipf.obj"
	-@erase "$(INTDIR)\genrtf.obj"
	-@erase "$(INTDIR)\gentext.obj"
	-@erase "$(INTDIR)\genxml.obj"
	-@erase "$(INTDIR)\main.obj"
	-@erase "$(INTDIR)\parser.obj"
	-@erase "$(INTDIR)\scanner.obj"
	-@erase "$(INTDIR)\system.obj"
	-@erase "$(OUTDIR)\convertn.exe"
	-@erase ".\convertn"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE"\
 /Fp"$(INTDIR)/convertn.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_SRC=.\src/
CPP_OBJS=.\ch\Release/
CPP_SBRS=.\ch\Release/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/convertn.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo\
 /subsystem:console /incremental:no /pdb:"$(OUTDIR)/convertn.pdb" /machine:I386\
 /out:"$(OUTDIR)/convertn.exe" 
LINK32_OBJS= \
	"$(INTDIR)\genhtml.obj" \
	"$(INTDIR)\genipf.obj" \
	"$(INTDIR)\genrtf.obj" \
	"$(INTDIR)\gentext.obj" \
	"$(INTDIR)\genxml.obj" \
	"$(INTDIR)\main.obj" \
	"$(INTDIR)\parser.obj" \
	"$(INTDIR)\scanner.obj" \
	"$(INTDIR)\system.obj"

"$(OUTDIR)\convertn.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<


!ELSEIF  "$(CFG)" == "convertn - Win32 Debug"

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
OUTDIR=.\workplace
INTDIR=.\ch\Debug
# Begin Custom Macros
ProjDir=.
TargetName=convertn
# End Custom Macros

ALL : "$(OUTDIR)\convertn.exe" "$(OUTDIR)\convertn.bsc"

CLEAN : 
	-@erase "$(INTDIR)\genhtml.obj"
	-@erase "$(INTDIR)\genhtml.sbr"
	-@erase "$(INTDIR)\genipf.obj"
	-@erase "$(INTDIR)\genipf.sbr"
	-@erase "$(INTDIR)\genrtf.obj"
	-@erase "$(INTDIR)\genrtf.sbr"
	-@erase "$(INTDIR)\gentext.obj"
	-@erase "$(INTDIR)\gentext.sbr"
	-@erase "$(INTDIR)\genxml.obj"
	-@erase "$(INTDIR)\genxml.sbr"
	-@erase "$(INTDIR)\main.obj"
	-@erase "$(INTDIR)\main.sbr"
	-@erase "$(INTDIR)\parser.obj"
	-@erase "$(INTDIR)\parser.sbr"
	-@erase "$(INTDIR)\scanner.obj"
	-@erase "$(INTDIR)\scanner.sbr"
	-@erase "$(INTDIR)\system.obj"
	-@erase "$(INTDIR)\system.sbr"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(OUTDIR)\convertn.bsc"
	-@erase "$(OUTDIR)\convertn.exe"
	-@erase "$(OUTDIR)\convertn.ilk"
	-@erase "$(OUTDIR)\convertn.pdb"
	-@erase ".\convertn"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /FR /YX /c
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE"\
 /FR"$(INTDIR)/" /Fp"$(INTDIR)/convertn.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/"\
 /c 
CPP_SRC=.\src/
CPP_OBJS=.\ch\Debug/
CPP_SBRS=.\ch\Debug/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/convertn.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\genhtml.sbr" \
	"$(INTDIR)\genipf.sbr" \
	"$(INTDIR)\genrtf.sbr" \
	"$(INTDIR)\gentext.sbr" \
	"$(INTDIR)\genxml.sbr" \
	"$(INTDIR)\main.sbr" \
	"$(INTDIR)\parser.sbr" \
	"$(INTDIR)\scanner.sbr" \
	"$(INTDIR)\system.sbr"

"$(OUTDIR)\convertn.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo\
 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)/convertn.pdb" /debug\
 /machine:I386 /out:"$(OUTDIR)/convertn.exe" 
LINK32_OBJS= \
	"$(INTDIR)\genhtml.obj" \
	"$(INTDIR)\genipf.obj" \
	"$(INTDIR)\genrtf.obj" \
	"$(INTDIR)\gentext.obj" \
	"$(INTDIR)\genxml.obj" \
	"$(INTDIR)\main.obj" \
	"$(INTDIR)\parser.obj" \
	"$(INTDIR)\scanner.obj" \
	"$(INTDIR)\system.obj"

"$(OUTDIR)\convertn.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

{$(CPP_SRC)}.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

{$(CPP_SRC)}.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

{$(CPP_SRC)}.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

{$(CPP_SRC)}.c{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

{$(CPP_SRC)}.cpp{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

{$(CPP_SRC)}.cxx{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Target

# Name "convertn - Win32 Release"
# Name "convertn - Win32 Debug"

!IF  "$(CFG)" == "convertn - Win32 Release"

!ELSEIF  "$(CFG)" == "convertn - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=$(CPP_SRC)\scanner.cpp

!IF  "$(CFG)" == "convertn - Win32 Release"

DEP_CPP_SCANN=\
	"$(CPP_SRC)\basedefs.h"\
	"$(CPP_SRC)\scanner.h"\
	"$(CPP_SRC)\tables.cpp"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\scanner.obj" : $(SOURCE) $(DEP_CPP_SCANN) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "convertn - Win32 Debug"

DEP_CPP_SCANN=\
	"$(CPP_SRC)\basedefs.h"\
	"$(CPP_SRC)\scanner.h"\
	"$(CPP_SRC)\tables.cpp"\
	{$(INCLUDE)}"\sys\STAT.H"\
	

"$(INTDIR)\scanner.obj" : $(SOURCE) $(DEP_CPP_SCANN) "$(INTDIR)"

"$(INTDIR)\scanner.sbr" : $(SOURCE) $(DEP_CPP_SCANN) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(CPP_SRC)\parser.cpp
DEP_CPP_PARSE=\
	"$(CPP_SRC)\basedefs.h"\
	"$(CPP_SRC)\generate.h"\
	"$(CPP_SRC)\ir.h"\
	"$(CPP_SRC)\scanner.h"\
	

!IF  "$(CFG)" == "convertn - Win32 Release"


"$(INTDIR)\parser.obj" : $(SOURCE) $(DEP_CPP_PARSE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "convertn - Win32 Debug"


"$(INTDIR)\parser.obj" : $(SOURCE) $(DEP_CPP_PARSE) "$(INTDIR)"

"$(INTDIR)\parser.sbr" : $(SOURCE) $(DEP_CPP_PARSE) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(CPP_SRC)\main.cpp
DEP_CPP_MAIN_=\
	"$(CPP_SRC)\basedefs.h"\
	"$(CPP_SRC)\ir.h"\
	"$(CPP_SRC)\parser.h"\
	"$(CPP_SRC)\scanner.h"\
	"$(CPP_SRC)\system.h"\
	

!IF  "$(CFG)" == "convertn - Win32 Release"


"$(INTDIR)\main.obj" : $(SOURCE) $(DEP_CPP_MAIN_) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "convertn - Win32 Debug"


"$(INTDIR)\main.obj" : $(SOURCE) $(DEP_CPP_MAIN_) "$(INTDIR)"

"$(INTDIR)\main.sbr" : $(SOURCE) $(DEP_CPP_MAIN_) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(CPP_SRC)\gentext.cpp
DEP_CPP_GENTE=\
	"$(CPP_SRC)\basedefs.h"\
	"$(CPP_SRC)\ir.h"\
	"$(CPP_SRC)\scanner.h"\
	

!IF  "$(CFG)" == "convertn - Win32 Release"


"$(INTDIR)\gentext.obj" : $(SOURCE) $(DEP_CPP_GENTE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "convertn - Win32 Debug"


"$(INTDIR)\gentext.obj" : $(SOURCE) $(DEP_CPP_GENTE) "$(INTDIR)"

"$(INTDIR)\gentext.sbr" : $(SOURCE) $(DEP_CPP_GENTE) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(CPP_SRC)\genrtf.cpp
DEP_CPP_GENRT=\
	"$(CPP_SRC)\basedefs.h"\
	"$(CPP_SRC)\generate.h"\
	"$(CPP_SRC)\ir.h"\
	"$(CPP_SRC)\scanner.h"\
	

!IF  "$(CFG)" == "convertn - Win32 Release"


"$(INTDIR)\genrtf.obj" : $(SOURCE) $(DEP_CPP_GENRT) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "convertn - Win32 Debug"


"$(INTDIR)\genrtf.obj" : $(SOURCE) $(DEP_CPP_GENRT) "$(INTDIR)"

"$(INTDIR)\genrtf.sbr" : $(SOURCE) $(DEP_CPP_GENRT) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(CPP_SRC)\genipf.cpp
DEP_CPP_GENIP=\
	"$(CPP_SRC)\basedefs.h"\
	"$(CPP_SRC)\generate.h"\
	"$(CPP_SRC)\ir.h"\
	"$(CPP_SRC)\scanner.h"\
	

!IF  "$(CFG)" == "convertn - Win32 Release"


"$(INTDIR)\genipf.obj" : $(SOURCE) $(DEP_CPP_GENIP) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "convertn - Win32 Debug"


"$(INTDIR)\genipf.obj" : $(SOURCE) $(DEP_CPP_GENIP) "$(INTDIR)"

"$(INTDIR)\genipf.sbr" : $(SOURCE) $(DEP_CPP_GENIP) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(CPP_SRC)\system.cpp

!IF  "$(CFG)" == "convertn - Win32 Release"


"$(INTDIR)\system.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "convertn - Win32 Debug"


"$(INTDIR)\system.obj" : $(SOURCE) "$(INTDIR)"

"$(INTDIR)\system.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(CPP_SRC)\genhtml.cpp
DEP_CPP_GENHT=\
	"$(CPP_SRC)\basedefs.h"\
	"$(CPP_SRC)\generate.h"\
	"$(CPP_SRC)\ir.h"\
	"$(CPP_SRC)\scanner.h"\
	

!IF  "$(CFG)" == "convertn - Win32 Release"


"$(INTDIR)\genhtml.obj" : $(SOURCE) $(DEP_CPP_GENHT) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "convertn - Win32 Debug"


"$(INTDIR)\genhtml.obj" : $(SOURCE) $(DEP_CPP_GENHT) "$(INTDIR)"

"$(INTDIR)\genhtml.sbr" : $(SOURCE) $(DEP_CPP_GENHT) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=$(CPP_SRC)\genxml.cpp
DEP_CPP_GENXM=\
	"$(CPP_SRC)\basedefs.h"\
	"$(CPP_SRC)\generate.h"\
	"$(CPP_SRC)\ir.h"\
	"$(CPP_SRC)\scanner.h"\
	

!IF  "$(CFG)" == "convertn - Win32 Release"


"$(INTDIR)\genxml.obj" : $(SOURCE) $(DEP_CPP_GENXM) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "convertn - Win32 Debug"


"$(INTDIR)\genxml.obj" : $(SOURCE) $(DEP_CPP_GENXM) "$(INTDIR)"

"$(INTDIR)\genxml.sbr" : $(SOURCE) $(DEP_CPP_GENXM) "$(INTDIR)"


!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
