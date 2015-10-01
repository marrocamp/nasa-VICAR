
FLAGS       =  -DSUN_UNIX -DLINUX -DPDS_TOOLBOX

# Change this to the path where your ODL library, header, and source
# files are located in your directory.
OAL_DIR    =  oal
LABLIB3_DIR = lablib3
ODLC_DIR = odlc
TC_DIR = legacy/table-check
SHARED_DIR = legacy/shared

INC_PATH  = -I$(TC_DIR) -I$(SHARED_DIR) -I$(OAL_DIR) -I$(LABLIB3_DIR) -I$(ODLC_DIR)
LIB_PATH  = -Llib

LIBS      =   -lm -loal_others_c

#
#        Translator Options
#
#CCOPTS      = -c -O $(INC_PATH)
CCOPTS = -c -O $(INC_PATH)
LDOPTS      = $(LIB_PATH)


#
#        Translator Definitions
#
CC        = gcc
LD        = gcc

#
#        Utility Definitions
#
COPY_CMD  = cp
DEL_CMD   = rm
MOVE_CMD  = mv

.c.o:
	$(CC) $(FLAGS) $(CCOPTS) $< -o $@
#
#
#
#        Application Targets and Dependencies
#
all:      bin/table_check

clean:
	- $(DEL_CMD) $(TC_DIR)/*.o
	- $(DEL_CMD) bin/table_check

#
#        List the project object modules
#
OBJECTFILES1 = $(TC_DIR)/asctv.o $(TC_DIR)/bintv.o $(SHARED_DIR)/formtype.o $(TC_DIR)/table_check.o $(TC_DIR)/tb_utils.o $(TC_DIR)/tverr.o $(TC_DIR)/tvutil.o 

OBJECTFILES = $(OBJECTFILES1)

#
#        List the project dependencies
#


$(TC_DIR)/asctv.o: $(TC_DIR)/asctv.c $(TC_DIR)/values.h $(TC_DIR)/table_check.h $(TC_DIR)/tverr.h $(TC_DIR)/tvutil.h

$(TC_DIR)/bintv.o:$(TC_DIR)/bintv.c $(TC_DIR)/values.h $(TC_DIR)/table_check.h $(TC_DIR)/tverr.h $(TC_DIR)/tvutil.h $(SHARED_DIR)/formtype.h

$(SHARED_DIR)/formtype.o: $(SHARED_DIR)/formtype.c $(SHARED_DIR)/formtype.h $(SHARED_DIR)/formtype2.h $(ODLC_DIR)/pdsdef.h

$(TC_DIR)/table_check.o: $(TC_DIR)/table_check.c $(TC_DIR)/values.h $(TC_DIR)/table_check.h $(TC_DIR)/tvutil.h $(TC_DIR)/tverr.h $(TC_DIR)/tb_utils.h $(OAL_DIR)/oal.h $(LABLIB3_DIR)/lablib3.h

$(TC_DIR)/tb_utils.o: $(TC_DIR)/tb_utils.c $(TC_DIR)/tb_utils.h

$(TC_DIR)/tverr.o: $(TC_DIR)/tverr.c $(TC_DIR)/tverr.h

$(TC_DIR)/tvutil.o: $(TC_DIR)/tvutil.c $(TC_DIR)/values.h $(TC_DIR)/table_check.h $(TC_DIR)/tverr.h $(TC_DIR)/tvutil.h

#
#        Application link line
#
bin/table_check: $(OBJECTFILES) 
	$(LD) $(OBJECTFILES) $(LDOPTS) $(LIBS) -o $@

