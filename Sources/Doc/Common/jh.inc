# common part of makefiles for all XDS on-line docs (HTML format)

CONVERTN = ..\convertn\workplace\convertn.exe

COMMON = ..\Common\back.tex ..\Common\def.tex ..\Common\title.tex ..\Common\xdsbook.tex
EUDIR   = ..\workplace\help
ENDUSER = $(EUDIR)\$(TARGET).hs

!ifndef PICDIRS
PICDIRS = .
!endif

!ifndef SRCDIR
SRCDIR = .
!endif

jh: $(ENDUSER)

$(ENDUSER): $(TARGET).hs
	copy $(TARGET)*.html $(EUDIR)
	copy $(TARGET).hs $(EUDIR)
	copy $(TARGET).jhm $(EUDIR)
#	copy $(TARGET).jdx $(EUDIR)
	copy $(TARGET).xml $(EUDIR)
	-for %a in ($(PICDIRS)) do mkdir $(EUDIR)\%a
	for %a in ($(PICDIRS)) do xcopy /Y %a\*.gif $(EUDIR)\%a
	for %a in ($(PICDIRS)) do xcopy /Y %a\*.jpg $(EUDIR)\%a

$(TARGET).hs: $(SOURCE) $(COMMON)
	$(CONVERTN) -j -hp -m2 -S javahelp.css $(SRCDIR)\$(TARGET).tex > $(TARGET).jlg
