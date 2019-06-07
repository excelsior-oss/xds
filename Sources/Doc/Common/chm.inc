# common part of makefiles for all Excelsior on-line docs (HtmlHelp format)

CONVERTN = ..\convertn\workplace\convertn.exe

COMMON = ..\Common\back.tex ..\Common\def.tex ..\Common\title.tex ..\Common\xdsbook.tex

!ifndef EUDIR
EUDIR   = ..\workplace
!endif

!ifndef PICDIRS
PICDIRS = .
!endif

!ifndef SRCDIR
SRCDIR = .
!endif


ENDUSER = $(EUDIR)\$(TARGET).chm

hh: $(ENDUSER)

$(ENDUSER): $(TARGET).chm
	copy $(TARGET).chm $(EUDIR)

$(TARGET).chm: $(TARGET).hhc
        hhc $(TARGET).hhp

$(TARGET).hhc: $(SOURCE) $(COMMON)
	$(CONVERTN) -H -hp -m2 -s $(SRCDIR)\$(TARGET).tex > $(TARGET).clg
