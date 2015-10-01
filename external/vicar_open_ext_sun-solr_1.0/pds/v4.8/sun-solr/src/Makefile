#
#*******************************************************************************
#                                                                              *
# Component:                                                                   *
#                                                                              *
#    Makefile for tbtool                                                       *
#*******************************************************************************
#

CFLAGS = -g
DEFINES = -D SUN_UNIX -D _NO_PROTO -D PDS_TOOLBOX
ODLC_LABLIB_LIB = lib/odlc_lablib.a

TBTOOL_DIR = legacy/tbtool
SHARED_DIR = legacy/shared

LABLIB_DIR = lablib
ODLC_DIR = odlc

PDS_LABLIB_OBJ = $(LABLIB_DIR)/label.o \
	$(LABLIB_DIR)/labutil.o \
	$(LABLIB_DIR)/fiolib.o \
	$(LABLIB_DIR)/errorlib.o
PDS_ODLCLIB_OBJ = $(ODLC_DIR)/a_nodes.o \
	$(ODLC_DIR)/ao_nodes.o\
	$(ODLC_DIR)/p_nodes.o \
	$(ODLC_DIR)/v_nodes.o \
	$(ODLC_DIR)/lexan.o \
	$(ODLC_DIR)/parser.o \
	$(ODLC_DIR)/parsact.o \
	$(ODLC_DIR)/cvtvalue.o \
	$(ODLC_DIR)/fmtvalue.o \
	$(ODLC_DIR)/toolout.o \
	$(ODLC_DIR)/wrtlabel.o \
	$(ODLC_DIR)/rdlabel.o \
	$(ODLC_DIR)/prtlabel.o \
	$(ODLC_DIR)/comments.o \
	$(ODLC_DIR)/utillib.o \
	$(ODLC_DIR)/syslib.o

TBTOOL_OBJ = $(SHARED_DIR)/formtype.o $(SHARED_DIR)/search.o $(SHARED_DIR)/labtool.o

LABLIB_LIB_SOURCE = $(LABLIB_DIR)/label.c
ODLC_LIB_SOURCE = $(ODLC_DIR)/a_nodes.c
PDS_LIBRARIES = $(ODLC_LABLIB_LIB)
PDS_INCLUDES = -I$(ODLC_DIR) -I$(TBTOOL_DIR) -I$(SHARED_DIR) -I$(LABLIB_DIR)

LINK_START = if [ -f $(TBTOOL_DIR)/$(@F).c ]; then echo Making $@; cc $(CFLAGS) -o $@ $(TBTOOL_DIR)/$(@F).o 
LINK_END = $(PDS_LIBRARIES) -lm; else exit 0; fi

#.SILENT:
.KEEP_STATE:

default: 
	if [ -f $(ODLC_LIB_SOURCE) ]; then make lib/odlc;  fi
	if [ -f $(LABLIB_LIB_SOURCE) ]; then make lib/lablib; fi
	if [ -f $(TBTOOL_DIR)/tbtool.c ]; then make bin/tbtool; fi

lib/lablib: $(PDS_LABLIB_OBJ)
	if [ -f $(LABLIB_DIR)/label.c ]; then \
		ar rcv $(ODLC_LABLIB_LIB) $(PDS_LABLIB_OBJ); \
#		ranlib $(LABLIB_LIB);\
	fi

lib/odlc: $(PDS_ODLCLIB_OBJ)
	if [ -f $(ODLC_DIR)/parser.c ]; then \
		ar rcv $(ODLC_LABLIB_LIB) $(PDS_ODLCLIB_OBJ); \
#		ranlib $(ODLC_LIB); \
	fi

bin/tbtool: $(PDS_LIBRARIES) $(TBTOOL_DIR)/$$(@F).o $(TBTOOL_OBJ)
	$(LINK_START) $(TBTOOL_OBJ) $(LINK_END)

.c.o:
	if [ -f $*.c ]; then \
	     echo Making $@; \
	     cc $(CFLAGS) $(PDS_INCLUDES) $(DEFINES) -c $@ -o $*.o $*.c; \
	else \
		exit 0; \
	fi

clean:
	touch $(TBTOOL_DIR)/*.o $(SHARED_DIR)/*.o $(ODLC_DIR)/*.o $(LABLIB_DIR)/*.o $(ODLC_LABLIB_LIB) bin/tbtool
	-rm $(TBTOOL_DIR)/*.o $(SHARED_DIR)/*.o $(ODLC_DIR)/*.o $(LABLIB_DIR)/*.o $(ODLC_LABLIB_LIB) bin/tbtool
