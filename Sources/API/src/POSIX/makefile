# Copyrighti (c) XDS Team 1997. All rights reserved.
#
# $RCSfile: makefile,v $ $Revision: 1.1.1.1 $ $Date: 1997/10/20 19:32:35 $
#

XDS_HOME = /usr/local/xdsn

LIBDIR = ./lib
SYMDIR = ./sym
XDS_LIBDIR = $(XDS_HOME)/lib/X86
XDS_SYMDIR = $(XDS_HOME)/sym/X86

POSIX_LIB = libmac_posix.a

RM = rm -f
INSTALL = install -c
MKDIR = mkdir -p
CP = cp

all:	libs syms

$(LIBDIR):
	$(MKDIR) $(LIBDIR)
	
$(SYMDIR):
	$(MKDIR) $(SYMDIR)
	$(CP) $(XDS_SYMDIR)/xmRTS.sym $(SYMDIR)

libs:	$(LIBDIR) $(LIBDIR)/$(POSIX_LIB)

$(LIBDIR)/$(POSIX_LIB): syms
	cd mac; xc =p libmac_posix.prj

syms:	$(SYMDIR) posix.prj
	$(MKDIR) $(LIBDIR) $(SYMDIR)
	xc =p posix.prj

install: all
	$(INSTALL) -d $(XDS_SYMDIR)/posix
	$(INSTALL) -d $(XDS_LIBDIR)/posix
	$(INSTALL) -m 644 $(LIBDIR)/$(POSIX_LIB) $(XDS_LIBDIR)/posix
	$(INSTALL) -m 644 sym/*.sym $(XDS_SYMDIR)/posix

clean:
	cd sym; $(RM) *.sym
	$(RM) $(LIBDIR)/$(POSIX_LIB)
	cd mac; make clean -f makefile.lib

