# common part of makefiles for all XDS on-line docs (HTML format)

CONVERTN = ..\convertn\workplace\convertn.exe

COMMON = ..\Common\back.tex ..\Common\def.tex ..\Common\title.tex ..\Common\xdsbook.tex
EUDIR   = ..\workplace\html
ENDUSER = $(EUDIR)\$(TARGET).html

!ifndef PICDIRS
PICDIRS = .
!endif

!ifndef SRCDIR
SRCDIR = .
!endif

html: $(ENDUSER)

$(ENDUSER): $(TARGET).html
        if not exist $(EUDIR) mkdir $(EUDIR)
	move $(TARGET)*.html $(EUDIR) 
	move $(TARGET)*.js $(EUDIR) 
	copy ..\Common\gifs\*.gif $(EUDIR) 
	copy ..\Common\gifs\*.js $(EUDIR) 
	-for %a in ($(PICDIRS)) do mkdir $(EUDIR)\%a
	for %a in ($(PICDIRS)) do xcopy /Y %a\*.gif $(EUDIR)\%a
	for %a in ($(PICDIRS)) do xcopy /Y %a\*.jpg $(EUDIR)\%a

$(TARGET).html: $(SOURCE) $(COMMON)
	$(CONVERTN) -hf -m $(SRCDIR)\$(TARGET).tex > $(TARGET).hlg
