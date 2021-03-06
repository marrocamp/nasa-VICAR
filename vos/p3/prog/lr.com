$!****************************************************************************
$!
$! Build proc for MIPL module lr
$! VPACK Version 1.8, Friday, January 31, 1997, 17:40:47
$!
$! Execute by entering:		$ @lr
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   PDF         Only the PDF file is created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module lr ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to lr.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("lr.imake") .nes. ""
$   then
$      vimake lr
$      purge lr.bld
$   else
$      if F$SEARCH("lr.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake lr
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @lr.bld "STD"
$   else
$      @lr.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create lr.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack lr.com -
	-s lr.f -
	-p lr.pdf -
	-i lr.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create lr.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
      IMPLICIT NONE
C
      INTEGER*4    I, ILOC, INUNIT, J, NBI,LBANDS,LSAMPS,
     1             NLI, NSI, STATUS, OUTUNIT, ISAMPS, LCNTR, 
     2             IBVAL(250),LGBVAL(250),LPBVAL(1024),ICNT,IDEF,
     3             JSTART,NCHAN,JCOUNT
      REAL*8       LGBSUM(250), ASAMPS(250), 
     1             LBMEAN(250),LSMEAN, LPSUM(1024), LPMEAN(1024)
      REAL         BUFIN(256000) 
      CHARACTER*3  INORG
      CHARACTER*4  FORMAT
C
C*****INITIALIZE VARIABLES
      DATA IBVAL,LGBVAL/250*0,250*0/
      DATA LGBSUM,ASAMPS,LBMEAN,LSMEAN 
     1     /250*0.0D0,250*0.0D0,250*0.0D0,0.0D0/
C
C  OPEN INPUT FILES 
C
      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,'OPEN_ACT','SA','IO_ACT','SA',
     &            'U_FORMAT','REAL',' ')
C
      CALL XVGET(INUNIT,STATUS,'NL',NLI,'NS',NSI,'NB',NBI,'ORG',
     &           INORG,'FORMAT',FORMAT,' ')
      CALL XVPARM('JSTART',JSTART,ICNT,IDEF,' ')
      CALL XVPARM('NCHAN',NCHAN,ICNT,IDEF,' ')
      IF(NCHAN.EQ.0)NCHAN=NBI
C
C*****INITIALIZE THE COUNTERS AND ADDITIONAL PARAMETERS     
      ISAMPS=NSI*NCHAN
      LBANDS=NCHAN+1
      LSAMPS=LBANDS*NSI
      LCNTR=0
C
C*****OPEN OUTPUT DATA FILE 
      CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
      CALL XVOPEN(OUTUNIT,STATUS,'OPEN_ACT','SA','IO_ACT','SA',
     &            'O_FORMAT','REAL','U_NB',LBANDS,
     &            'U_FORMAT','REAL','OP','WRITE','U_ORG','BIL',' ')
C
      CALL XVMESSAGE('USE FIT3D TO CONVERT YOUR OUTPUT DATA TO BYTE ',
     +               ' ')
      CALL XVMESSAGE('FOR DISPLAY OR TO SCALE YOUR OUTPUT DATA',' ')
      CALL XVMESSAGE(' ',' ')
C
C*****FIRST PASS - CALCULATE THE BAND AND SCENE MEANS 
C
      CALL XVMESSAGE(' First pass of two',' ')
      DO I = 1,NLI
        JCOUNT=JSTART 
        DO J = 1,NCHAN  
          ILOC=(NSI * (J-1)) + 1
          CALL XVREAD(INUNIT,BUFIN(ILOC),STATUS,'LINE',I,
     &                'SAMP',1,'NSAMPS',NSI,'BAND',JCOUNT,' ')
          JCOUNT = JCOUNT + 1
        ENDDO
        CALL LOGST1(BUFIN, NSI, NCHAN, ISAMPS, LGBSUM, 
     1              LGBVAL)
        LCNTR=LCNTR+1
      ENDDO
      CALL LSTAT2(LGBSUM, NSI, NCHAN, ASAMPS, LCNTR, LGBVAL, 
     1            LBMEAN, LSMEAN)
C
C*****SECOND PASS CALCULATE STATS ON LOG RESIDUAL DATA FOR 
C*****BYTE REDUCTION 
C
      LCNTR=0
      CALL XVMESSAGE(' Second pass of two',' ')
      DO I= 1,NLI
        JCOUNT=JSTART 
        DO J = 1,NCHAN  
          ILOC=(NSI * (J-1)) + 1
          CALL XVREAD(INUNIT,BUFIN(ILOC),STATUS,'LINE',I,
     &                'SAMP',1,'NSAMPS',NSI,'BAND',JCOUNT,' ')
          JCOUNT = JCOUNT + 1
        ENDDO
      CALL LOGRES(BUFIN, NSI, NCHAN, LSAMPS, LBMEAN, 
     1           LSMEAN, LPSUM, LPMEAN, LPBVAL, LBANDS, 
     2           LCNTR)
        DO J = 1,LBANDS 
          ILOC=(NSI * (J-1)) + 1
          CALL XVWRIT(OUTUNIT,BUFIN(ILOC),STATUS,' ')
        ENDDO
        LCNTR=LCNTR+1
      ENDDO
C
C  CLOSE FILES
C
      CALL XVCLOSE(INUNIT,STATUS,' ')
      CALL XVCLOSE(OUTUNIT,STATUS,' ')
      RETURN
      END
C
C*****SUBROUTINE TO FIND THE LOG SUM OF EACH BAND OF REAL 
C*****DATA 
C 
C*****CHECK IBVAL IS RESET TO ZERO IF CALCULATING REAL DATA AND 
C*****BYTE DATA STATS 
C 
      SUBROUTINE LOGST1(BUFIN, NSI, NCHAN, ISAMPS, LGBSUM, LGBVAL)
      REAL BUFIN(ISAMPS)
      REAL*8 LGBSUM(NCHAN)
      INTEGER LGBVAL(NCHAN)
C 
      JSTART = 1
      JEND = NSI
      DO I = 1, NCHAN
        DO J = JSTART, JEND
          IF (BUFIN(J) .GT. 0.0) THEN 
            LGBSUM(I) = (DLOG(DBLE(BUFIN(J)))) + LGBSUM(I)
          END IF 
          IF (BUFIN(J) .EQ. 0.0) LGBVAL(I) = LGBVAL(I) + 1
        ENDDO 
        JSTART = JSTART + NSI
        JEND = JEND + NSI
      ENDDO 
      RETURN 
      END 
C 
C*****SUBROUTINE TO CALCULATE THE LOG MEAN OF EACH BAND OF REAL 
C*****DATA 
      SUBROUTINE LSTAT2(LGBSUM, NSI, NCHAN, ASAMPS, LCNTR, LGBVAL, 
     1           LBMEAN, LSMEAN)
      REAL*8 LGBSUM(NCHAN), LBMEAN(NCHAN), ASAMPS(NCHAN), LSMEAN, 
     1       LSSUM
      INTEGER LGBVAL(NCHAN)
C 
C*****INITIALIZE ASAMPS,LBMEAN AND CALCULATE ASAMPS, THE NUMBER OF 
C*****GOOD VALUES PER CHANNEL 
C 
      DO I = 1, NCHAN
        ASAMPS(I) = 0.0D0
        ASAMPS(I) = (DFLOAT((NSI*LCNTR) - LGBVAL(I)))
        LBMEAN(I) = 0.0D0
      ENDDO 
C 
C*****CALCULATE THE LOG MEAN OF EACH BAND FROM THE LOG SUM OF EACH 
C*****BAND 
C 
      DO I = 1, NCHAN
        LBMEAN(I) = LGBSUM(I) / ASAMPS(I)
      ENDDO 
C 
C*****INITIALIZE LSSUM 
      LSSUM = 0.0D0
C 
C*****CALCULATE THE OVERALL LOG SCENE MEAN 
      DO I = 1, NCHAN
        LSSUM = LBMEAN(I) + LSSUM
      ENDDO 
      LSMEAN = 0.0D0
      LSMEAN = LSSUM / (DFLOAT(NCHAN))
C
      RETURN 
      END 

C*****SUBROUTINE TO CALCULATE LOG RESIDUALS 
      SUBROUTINE LOGRES(BUFIN, NSI, NCHAN, LSAMPS, LBMEAN, 
     1           LSMEAN, LPSUM, LPMEAN, LPBVAL, LBANDS,
     2           LCNTR)
      REAL*8 LBMEAN(NCHAN), LPSUM(NSI), LPMEAN(NSI), GBANDS, 
     1       LSMEAN 
      REAL   BUFIN(LSAMPS) 
      INTEGER LPBVAL(NSI)
C
C*****INITIALIZE REAL*8 VARIABLES
      DO I = 1, NSI
        LPSUM(I) = 0.0D0
        LPMEAN(I) = 0.0D0
        LPBVAL(I) = 0
      ENDDO
C
C*****INITIALIZE EXTRA CHANNEL
      K = (NCHAN*NSI) + 1
      DO I = 1, NSI
        BUFIN(K) = 0.0
        K = K + 1
      ENDDO
C  
C*****SET UP COUNTERS 
      K = 1
C*****CALCULATE THE SUM OF EACH PIXEL FOR NCHAN 
      DO J = 1, NSI
        L = J
        DO I = 1, NCHAN
          IF (BUFIN(L) .GT. 0.0) THEN 
            LPSUM(J) = (DLOG(DBLE(BUFIN(L)))) + LPSUM(J)
          ENDIF  
          IF (BUFIN(L) .LE. 0.0) THEN 
            LPBVAL(J) = LPBVAL(J) + 1
          ENDIF 
          L = L + NSI
        ENDDO 
      ENDDO 
C 
C*****CALCULATE THE MEAN OF EACH PIXEL FOR NCHAN 
      DO I = 1, NSI
        GBANDS = 0.0D0
        GBANDS = (DFLOAT(NCHAN - LPBVAL(I)))
        IF (GBANDS.LE.0.0D0)THEN
          LPMEAN(I) = 0.0D0
        ELSE   
          LPMEAN(I) = LPSUM(I) / GBANDS
        ENDIF
      ENDDO
C     
C*****SUBTRACT OFF THE LOG PIXEL MEAN AND LOG BAND MEAN AND ADD 
C*****THE LOG SCENE MEAN 
C 
      DO I = 1, NCHAN
        DO J = 1, NSI
          IF (BUFIN(K) .GT. 0.0 .AND. LPMEAN(J) .GT. 0.D0) THEN 
            BUFIN(K) =SNGL((DLOG(DBLE(BUFIN(K)))) - LBMEAN(I) 
     1                 - LPMEAN(J) + LSMEAN)
          ELSEIF(BUFIN(K) .LE. 0.0 .OR. LPMEAN(J) .EQ. 0.0D0)
     1    THEN 
            BUFIN(K) = 0.0
          ENDIF  
          K = K + 1
        ENDDO
      ENDDO
C 
C*****PUT THE LOG PIXEL MEAN INTO NCHAN+1 
      K = (NSI*NCHAN) + 1
      DO I = 1, NSI
        BUFIN(K) = SNGL(LPMEAN(I))
        K = K + 1
      ENDDO
C 
      RETURN 
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create lr.pdf
process help=*
PARM INP    TYPE=STRING     
PARM OUT    TYPE=STRING
PARM JSTART TYPE=INTEGER DEFAULT=1
PARM NCHAN  TYPE=INTEGER DEFAULT=0
END-PROC
.HELP
     This program calculates the log residual values of the input data.
     The algorithm used is given in Roberts et al. 1985. Comparison 
     of various techniques for calibration of AIS data: Proc. Second 
     Airborne Imaging Spectrometer Data Analysis Workshop. May 6-8, 
     JPL Pub. 86-35, p.21-30.
     The maximum number of channels it will take is 250.
     the maximum number of pixels in a channels it will take is 1024.
     Cognizant programmer S. J. Hook extn 4-0974 section 326.
.LEVEL1
.VARIABLE INP
Input dataset
.VARIABLE OUT
Ouput dataset 
.VARIABLE JSTART
Start channel 
.VARIABLE NCHAN
Number of channels
.LEVEL2
.VARIABLE INP
The input dataset
.VARIABLE OUT
The output dataset is a BIL file with one more channel than the 
input. The additional channel is the log pixel mean which equates 
to the albedo.
.VARIABLE JSTART
The start channel within the dataset from which you want to start 
processing.
.VARIABLE NCHAN
The number of channels from the start channel that you want to process.
$ Return
$!#############################################################################
$Imake_File:
$ create lr.imake
#define  PROGRAM   lr

#define MODULE_LIST lr.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
