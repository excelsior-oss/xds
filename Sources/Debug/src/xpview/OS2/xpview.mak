#===================================================================
#
#   Template Make file
#   Copyright 1991 IBM Corporation
#
#===================================================================

# Compile switches  that are enabled
# /c      compile don't link
# /Gm+    use the multi-threaded libraries
# /ss     allow  "//" for comment lines
# /Ms     use the system calling convention and not _optlink as the default
# /Gd-    disable optimization
# /Se     allow C Set/2  extensions

CC         = icc /c /Gd- /Se /Re /Ms /Gm+ /Ti+ /O- /Oi- /Si+
LFLAGS   = /NOE  /ALIGN:16 /EXEPACK /M /BASE:0x10000 /NOD /NOI
LINK    = LINK386  $(LFLAGS)
LIBS  = DDE4MBS + os2386 + xprofapi.lib

ALL_OBJ = obj\xpview.obj obj\headwin.obj obj\sf_lib.obj

all: xpview.exe

obj\xpview.obj: xpview.cpp xpview.h profile.h headwin.h
  $(CC) -Foobj\xpview.obj xpview.cpp

obj\headwin.obj: headwin.cpp headwin.h
  $(CC) -Foobj\headwin.obj headwin.cpp

obj\sf_lib.obj: sf_lib.cpp sf_lib.h
  $(CC) -Foobj\sf_lib.obj sf_lib.cpp

res\xpview.res: res\xpview.rc xpview.h
  cd res
  rc -r -i .. xpview.rc
  cd ..

obj\xpview.lnk: xpview.mak
    echo $(ALL_OBJ)       >  xpview.lnk
    echo xpview.exe       >> xpview.lnk
    echo xpview           >> xpview.lnk
    echo $(LIBS)          >> xpview.lnk
    echo xpview.def       >> xpview.lnk


xpview.exe: $(ALL_OBJ) xpview.def obj\xpview.lnk res\xpview.res
    $(LINK) /Debug @xpview.lnk
    rc res\xpview.res xpview.exe


