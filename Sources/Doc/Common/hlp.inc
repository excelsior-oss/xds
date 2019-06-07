# common part of makefiles for all XDS on-line docs (Windows Help 4 format)

CONVERTN = ..\convertn\workplace\convertn.exe

COMMON = ..\Common\back.tex ..\Common\def.tex ..\Common\title.tex ..\Common\xdsbook.tex
EUDIR   = ..\workplace
ENDUSER = $(EUDIR)\$(TARGET).hlp $(EUDIR)\$(TARGET).cnt

!ifndef CNTLEVEL
CNTLEVEL = 2
!endif

!ifndef SRCDIR
SRCDIR = .
!endif


hlp: $(EUDIR) $(ENDUSER)

$(EUDIR):
        mkdir $(EUDIR)

$(EUDIR)\$(TARGET).hlp: $(TARGET).hlp
	copy $(TARGET).hlp $(EUDIR)

$(EUDIR)\$(TARGET).cnt: $(TARGET).cnt
	copy $(TARGET).cnt $(EUDIR)

$(TARGET).hlp: $(TARGET).rtf $(TARGET).hpj
	hcrtf -xn $(TARGET).hpj

$(TARGET).cnt $(TARGET).rtf: $(SOURCE) $(COMMON)
	$(CONVERTN) -rc$(CNTLEVEL) $(SRCDIR)\$(TARGET).tex > $(TARGET).rlg
