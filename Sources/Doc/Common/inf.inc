# common part of makefiles for all OS/2 XDS on-line docs

COMMON = ..\back.tex ..\def.tex ..\title.tex ..\xdsbook.tex
EUDIR   = ..\..\enduser\xds\doc
ENDUSER = $(EUDIR)\$(TARGET).inf

inf: $(ENDUSER)

$(ENDUSER): $(TARGET).inf
	copy $(TARGET).inf $(EUDIR)

$(TARGET).inf: $(TARGET).ipf 
	ipfc /inf /l:ENU /d:1 /c:850 $(TARGET).ipf

$(TARGET).ipf: $(SOURCE) $(COMMON)
	converto -i $(TARGET).tex > $(TARGET).ilg
