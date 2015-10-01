# SunOS Makefile for PDS Label Library
#
CFLAGS =
DEFINES = -DSUN_UNIX -DPDS_TOOLBOX

PDS_LABEL_OBJ =	label.o \
                labutil.o \
		syslib.o \
		utillib.o \
		fiolib.o \
		errorlib.o \
                a_nodes.o \
	        p_nodes.o \
	        v_nodes.o \
	        lexan.o \
                parser.o \
	        parsact.o \
	        cvtvalue.o \
	        fmtvalue.o \
	        toolout.o \
	        wrtlabel.o \
                rdlabel.o \
	        prtlabel.o \
                comments.o

all: label_demo

label_demo: label_de.o \
	    pdslabel.a 
	cc $(CFLAGS) $(DEFINES) -o label_de \
	   label_de.o \
	   pdslabel.a -lm

.PRECIOUS: pdslabel.a

pdslabel.a: $(PDS_LABEL_OBJ)
	ar rcv pdslabel.a $(PDS_LABEL_OBJ)
	-ranlib pdslabel.a

.c.o:
	cc $(CFLAGS) $(DEFINES) -c -o $@ $<



