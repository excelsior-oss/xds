# common part of makefiles for all PDF manuals

LATEX	= pdflatex -c-style-errors -halt-on-error -interaction=batchmode  
MKIDX	= makeindex

COMMON = ..\Common\back.tex ..\Common\def.tex ..\Common\title.tex ..\Common\xdsbook.tex
EUDIR   = ..\workplace
ENDUSER = $(EUDIR)\$(TARGET).pdf 

!ifndef SRCDIR
SRCDIR = .
!endif

!ifndef CNTLEVEL
CNTLEVEL = 2
!endif

pdf: $(ENDUSER)

$(EUDIR)\$(TARGET).pdf: $(TARGET).pdf 
	copy $(TARGET).pdf $(EUDIR)

$(TARGET).pdf: $(TARGET).ind

$(TARGET).ind: $(TARGET).idx
        copy $(TARGET).idx $(TARGET).ids
        $(MKIDX) $(TARGET).idx
        $(LATEX) $(SRCDIR)\$(TARGET).tex
        copy $(TARGET).ids $(TARGET).idx
        del $(TARGET).ids

$(TARGET).idx: $(SOURCE) $(COMMON)
        $(LATEX) $(SRCDIR)\$(TARGET).tex
