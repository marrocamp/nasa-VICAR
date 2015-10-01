C$Procedure TLE2SPK ( Read two-line element set and create type 10 SPK )

      SUBROUTINE TLE2SPK (INPFN, OBIDVL, CNIDVL, FRNMVL, SGIDVL, HANDLE)

C$ Abstract
C
C     This routine is a module of the MKSPK program. It creates an SPK 
C     file from a file containing the NORAD "two-line element sets".
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     MKSPK User's Guide
C
C$ Keywords
C
C     None.
C
C$ Declarations 

      IMPLICIT       NONE
      
      INCLUDE        'mkspk.inc'

      CHARACTER *(*)        INPFN
      CHARACTER *(*)        FRNMVL
      CHARACTER *(*)        SGIDVL
    
      INTEGER               OBIDVL 
      INTEGER               CNIDVL
      INTEGER               HANDLE

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     INPFN      I   Input file name
C     FRNMVL     I   Reference frame name of output SPK
C     OBIDVL     I   NORAD satellite code   
C     CNIDVL     I   Center ID NAIF code
C     SGIDVL     I   Segment identifier
C     HANDLE     I   Handle of an SPK file open for writing.
C
C$ Detailed_Input
C
C     INPFN       is the name of input file containing the NORAD
C                 "two-line element sets"
C
C     FRNMVL      is the reference frame that output states are 
C                 referenced to. It must be 'J2000'.
C
C     OBIDVL      is the NORAD code of the object whose states
C                 are to be recorded in an SPK file.
C
C     CNIDVL      is the NAIF ID for the center body. It must be 399
C                 corresponding to Earth.
C
C     SGIDVL      is identifier of segment stored in output file.
C
C     HANDLE      is the file handle of an SPK file that has been
C                 opened for writing.
C
C$ Detailed_Output
C
C     None.       The data input is stored in an SPK segment in the
C                 DAF connected to the input HANDLE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the center body of the motion is not the Earth, then 
C        the error 'SPICE(INCONSISTCENTERID)' will be signalled.      
C
C     2) If the reference frame is not 'J2000', then the error 
C        'SPICE(INCONSISTFRAME)' will be signalled.      
C
C     3) If code of requested space object is not found in the 
C        input file, then the error 'SPICE(NOTLEDATAFOROBJECT)'
C        will be signalled.
C
C     4) If second line of two-line element records does not exist, 
C        then the error 'SPICE(NOTEXISTSECONDLINE)' will be signalled.
C
C     5) If second line of two-line element record contains incorrect
C        object code, then the error 'SPICE(INCONSISTSECONDLINE)'
C        will be signalled.
C  
C     6) If any one of the required geophysical constants was not 
C        found in the POOL, then the error SPICE(MISSINGGEOCONSTS)
C        will be signalled.
C
C$ Files
C
C     This routine read text data from the input data file INPFN
C     containing two-line element set file in standard text format.
C
C     Leapsecond Kernel (LSK) file must be loaded before running
C     this routine.
C
C     A geophysical constants file for the Earth must be loaded
C     before runnung this routine.
C
C        The geophysical constants kernel should contain 
C        the following variables:
C
C        BODY399_J2 --- J2 gravitational harmonic for earth
C        BODY399_J3 --- J3 gravitational harmonic for earth
C        BODY399_J4 --- J4 gravitational harmonic for earth
C        BODY399_KE --- Square root of the GM for earth where GM
C                       is expressed in earth radii cubed
C                       per minutes squared
C        BODY399_ER --- Equatorial radius of the earth in km
C        BODY399_S0 --- Low altitude bound for atmospheric model in km
C        BODY399_Q0 --- High altitude bound for atmospheric model in km
C        BODY399_AE --- Distance units/earth radius (normally 1).
C
C
C     The program creates SPK file connected to HANDLE.
C     This file must be opened for writing before running this
C     routine.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     E.D. Wright    (NAIF)
C     N.G. Khavenson (IKI RAS, Russia)
C     B.V. Semenov   (NAIF)
C     W.L. Taber     (NAIF)
C
C$ Version
C
C-    Version 2.2.0, 06-MAR-2009 (BVS).
C
C        Encapsulated forward/backward propagation period in COVPAD
C        variable.
C
C-    Version 2.1.0, 18-MAR-2005 (EDW).
C
C        Corrected a logic error that prevent processing of TLEs
C        for vehicles with ID codes four characters or shorter.
C
C-    Version 2.0.0, 06-APR-2004 (EDW).
C
C        Modified algorithm to call ZZGETELM, a modified version of 
C        GETELM. ZZGETELM returns a flag and explanation string
C        for any TLE processing error.
C
C        Correct a typo:
C
C           ENDT   = EPOCHS(I)
C
C        to
C
C           ENDT   = EPOCHS(PUT)
C
C        This type could cause a TLE segment summary to report the
C        wrong end time for the segment data.
C
C-    Version 1.0.1, 27-JAN-2000 (BVS).
C
C        Added a little better error message for the case when 
C        geophysical constants weren't loaded.
C
C-    Version 1.0.0, 22-NOV-1999 (NGK).
C
C        Initial release based on the  MKSPK10 utility program
C        Version 1.0.0, 18-JUL-1997 (WLT)
C
C-& 

C$ Index_Entries
C
C     Creates an SPK file from a file containing the NORAD
C     "two-line element sets. 
C
C-&                       

C                              
C     SPICELIB functions
C

      INTEGER               RTRIM
      DOUBLE PRECISION      SPD
      LOGICAL               ISORDV
      LOGICAL               RETURN

C
C     Local variables
C
       
C
C     Size WDSIZE, LINLEN, FILSIZE are defined in include file.
C

      CHARACTER*( WDSIZE )  CODE
      CHARACTER*( WDSIZE )  CHOSE1
      CHARACTER*( WDSIZE )  CHOSE2
      CHARACTER*( LINLEN )  LINES  ( 2 )
      CHARACTER*( LINLEN )  ERROR  ( 2 )
      CHARACTER*( FILSIZ )  OUTFN

      CHARACTER *(*)        FRAME
      PARAMETER           ( FRAME = 'J2000' )
    
      INTEGER               CENTER
      PARAMETER           ( CENTER = 399 ) 
 
      INTEGER               OBJECT
      INTEGER               FRCODE
      INTEGER               FRAMID

      LOGICAL               OK
 
C
C
C     The following integers are used to mark the various
C     slots in the array for geophysical constants.
C
C        J2
C        J3
C        J4
C        KE
C        QO
C        SO
C        ER
C        AE
C
      INTEGER               START
      PARAMETER           ( START  = 0 )
 
      INTEGER               J2
      PARAMETER           ( J2     = START  + 1 )
 
      INTEGER               J3
      PARAMETER           ( J3     = J2     + 1 )
 
      INTEGER               J4
      PARAMETER           ( J4     = J3     + 1 )
 
      INTEGER               KE
      PARAMETER           ( KE     = J4     + 1 )
 
      INTEGER               QO
      PARAMETER           ( QO     = KE     + 1 )
 
      INTEGER               SO
      PARAMETER           ( SO     = QO     + 1 )
 
      INTEGER               ER
      PARAMETER           ( ER     = SO     + 1 )
 
      INTEGER               AE
      PARAMETER           ( AE     = ER     + 1 )
 
      INTEGER               NGEOCN
      PARAMETER           ( NGEOCN = AE )
 
C
C     An enumeration of the various components of the
C     elements array---ELEMS
C
C        KNDT20
C        KNDD60
C        KBSTAR
C        KINCL
C        KNODE0
C        KECC
C        KOMEGA
C        KMO
C        KNO
C
      INTEGER               KNDT20
      PARAMETER           ( KNDT20 = START  + 1 )
 
      INTEGER               KNDD60
      PARAMETER           ( KNDD60 = KNDT20 + 1 )
 
      INTEGER               KBSTAR
      PARAMETER           ( KBSTAR = KNDD60 + 1 )
 
      INTEGER               KINCL
      PARAMETER           ( KINCL  = KBSTAR + 1 )
 
      INTEGER               KNODE0
      PARAMETER           ( KNODE0 = KINCL  + 1 )
 
      INTEGER               KECC
      PARAMETER           ( KECC   = KNODE0 + 1 )
 
      INTEGER               KOMEGA
      PARAMETER           ( KOMEGA = KECC   + 1 )
 
      INTEGER               KMO
      PARAMETER           ( KMO    = KOMEGA + 1 )
 
      INTEGER               KNO
      PARAMETER           ( KNO    = KMO    + 1 )
 
      INTEGER               KEPOCH
      PARAMETER           ( KEPOCH = KNO    + 1 )
 
      INTEGER               NELEMS
      PARAMETER           ( NELEMS = KEPOCH )
 
C
C     The next set of parameters govern how many items will
C     go into a segment.
C
      INTEGER               MAXEPC
      PARAMETER           ( MAXEPC = 5000 )
 
      INTEGER               MAXELM
      PARAMETER           ( MAXELM = NELEMS * MAXEPC )
 
      INTEGER               EPCRM
      PARAMETER           ( EPCRM  = MAXEPC + 1      )
 
      INTEGER               ELMRM
      PARAMETER           ( ELMRM  = MAXELM + NELEMS )
 
      DOUBLE PRECISION      BEGINT
      DOUBLE PRECISION      ELEMS  ( ELMRM  )
      DOUBLE PRECISION      ENDT
      DOUBLE PRECISION      EPOCHS ( EPCRM  )
      DOUBLE PRECISION      GEOPHS ( NGEOCN )
      DOUBLE PRECISION      COVPAD
 
      INTEGER               I
      INTEGER               J
      INTEGER               N
 
      LOGICAL               EOF
      LOGICAL               FMODEL
      LOGICAL               FOUND
 
      INTEGER               IORDER ( MAXEPC )
      INTEGER               ORDVEC ( MAXEPC )
      INTEGER               TRASH
      INTEGER               PUT
      INTEGER               K

      CHARACTER*(FILSIZ)    GEOLST
      CHARACTER*(1)         TYPE

C     
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'TLE2SPK' ) 
      END IF

C
C     The number of seconds for which propagation will be allowed
C     before the first TLE epoch and after the last TLE epoch,
C     nominally set to one half day.
C
      COVPAD = SPD()/2.0D0

C
C     Get filename of output file.
C
      CALL DAFHFN ( HANDLE, OUTFN )       

C
C     Check center ID. 
C
      IF ( CNIDVL .NE. CENTER ) THEN

C
C        The center body is not the Earth. Complain. KCENID is defined
C        in include file.
C
         CALL DAFCLS ( HANDLE )
         CALL DELFIL ( OUTFN )
         CALL SETMSG ( 'Processing of two-line element data '     //
     .                 'requires the setup file keyword ''#'' to '//
     .                 'be set to #.'                             )
         CALL ERRINT ( '#', CENTER                                )
         CALL ERRCH  ( '#', KCENID                                ) 
         CALL SIGERR ( 'SPICE(INCONSISTCENTERID)'                 )
 
      END IF
   
C
C     Check reference frame. 
C
      CALL NAMFRM ( FRNMVL, FRCODE )
      CALL NAMFRM ( FRAME,  FRAMID )
      
      IF ( FRCODE .NE. FRAMID )  THEN

C
C        The frame is not J2000. Complain. KRFRNM is defined in include
C        file.
C
         CALL DAFCLS ( HANDLE )
         CALL DELFIL ( OUTFN )
         CALL SETMSG ( 'Processing of two-line element data '     //
     .                 'requires the setup file keyword ''#'' to '//
     .                 'be set to ''#''.'                         )
         CALL ERRCH ( '#', FRAME                                  )
         CALL ERRCH ( '#', KRFRNM                                 ) 
         CALL SIGERR ( 'SPICE(INCONSISTFRAME)'                    )
 
      END IF
     
C  
C     Convert object ID to string code that allows to chose the object
C     data from input file.
C     
      CALL INTSTR ( OBIDVL, CODE )
      
C
C     Initialize CHOSE1 and CHOSE2 to seven characters, with
C     the TLE line ID as the first character. 
C
      CHOSE1 = '1'
      CHOSE2 = '2'

C
C     Write the ID string CODE to CHOSE1 and CHOSE2 so that the
C     last RTRIM(CODE) characters of the CHOSE1(1:7) and
C     CHOSE2(1:7) contain the code.
C
      CHOSE1( 7 - RTRIM(CODE) + 1: 7 ) =  CODE(1:RTRIM(CODE))
      CHOSE2( 7 - RTRIM(CODE) + 1: 7 ) =  CODE(1:RTRIM(CODE))

C
C     Form standard negative object ID.
C 
      OBJECT = -100000 - OBIDVL

C
C     Read first line from TLE data file
C
      CALL RDTEXT   ( INPFN, LINES(1), EOF )
 
C
C     Check the NORAD ID value matches Find the first requested 
C     line of the TLE file.
C
      DO WHILE ( LINES(1)(1:7) .NE. CHOSE1(1:7) .AND. .NOT. EOF )
 
         CALL RDTEXT ( INPFN, LINES(1), EOF )
 
      END DO

      IF ( EOF ) THEN
 
C
C        Requested data did not found. Complain.
C 
         CALL DAFCLS ( HANDLE )
         CALL DELFIL ( OUTFN )
         CALL SETMSG ( 'No data for the object with NORAD ID # '     //
     .                 'were found in the input two-line element '   //
     .                 'file.'                                       )  
         CALL ERRINT ( '#', OBIDVL                                   ) 
         CALL SIGERR ( 'SPICE(NOTLEDATAFOROBJECT)'                   )

      END IF

C
C     Check whether all geophysical constants needed for the SGP4 
C     propagation model are present in the kernel pool.
C 
      GEOLST = ' '
      
      CALL DTPOOL ( 'BODY399_J2', FOUND, N, TYPE )
      IF ( (.NOT. FOUND) .OR. (N .NE. 1) .OR. (TYPE .NE. 'N') ) THEN
         CALL SUFFIX ( ' BODY399_J2,', 0, GEOLST )
      END IF
            
      CALL DTPOOL ( 'BODY399_J3', FOUND, N, TYPE )
      IF ( (.NOT. FOUND) .OR. (N .NE. 1) .OR. (TYPE .NE. 'N') ) THEN
         CALL SUFFIX ( ' BODY399_J3,', 0, GEOLST )
      END IF
      
      CALL DTPOOL ( 'BODY399_J4', FOUND, N, TYPE )
      IF ( (.NOT. FOUND) .OR. (N .NE. 1) .OR. (TYPE .NE. 'N') ) THEN
         CALL SUFFIX ( ' BODY399_J4,', 0, GEOLST )
      END IF
      
      CALL DTPOOL ( 'BODY399_KE', FOUND, N, TYPE )
      IF ( (.NOT. FOUND) .OR. (N .NE. 1) .OR. (TYPE .NE. 'N') ) THEN
         CALL SUFFIX ( ' BODY399_KE,', 0, GEOLST )
      END IF
      
      CALL DTPOOL ( 'BODY399_QO', FOUND, N, TYPE )
      IF ( (.NOT. FOUND) .OR. (N .NE. 1) .OR. (TYPE .NE. 'N') ) THEN
         CALL SUFFIX ( ' BODY399_QO,', 0, GEOLST )
      END IF
      
      CALL DTPOOL ( 'BODY399_SO', FOUND, N, TYPE )
      IF ( (.NOT. FOUND) .OR. (N .NE. 1) .OR. (TYPE .NE. 'N') ) THEN
         CALL SUFFIX ( ' BODY399_SO,', 0, GEOLST )
      END IF
      
      CALL DTPOOL ( 'BODY399_ER', FOUND, N, TYPE )
      IF ( (.NOT. FOUND) .OR. (N .NE. 1) .OR. (TYPE .NE. 'N') ) THEN
         CALL SUFFIX ( ' BODY399_ER,', 0, GEOLST )
      END IF
      
      CALL DTPOOL ( 'BODY399_AE', FOUND, N, TYPE )
      IF ( (.NOT. FOUND) .OR. (N .NE. 1) .OR. (TYPE .NE. 'N') ) THEN
         CALL SUFFIX ( ' BODY399_AE,', 0, GEOLST )
      END IF
      
      IF ( GEOLST .NE. ' ' ) THEN
      
C
C        On of the geophysical constants was not found or wasn't 
C        of the right type. Complain.
C 
         CALL DAFCLS ( HANDLE )
         CALL DELFIL ( OUTFN )
         CALL SETMSG ( 'The following geophysical constants were '//
     .                 'not provided to the program or their '    //
     .                 'values were not scalar DP numbers: #. '   //
     .                 'Check whether the name of a geophysical ' //
     .                 'constants PCK file was provided in the '  //
     .                 'setup file keyword ''#'', and if so, '    //
     .                 'whether the file contains appropriate '   //
     .                 'values for the keywords listed above. '   ) 
         CALL ERRCH  ( '#', GEOLST(:RTRIM(GEOLST)-1)              ) 
         CALL ERRCH  ( '#', PCKFIL                                ) 
         CALL SIGERR ( 'SPICE(MISSINGGEOCONSTS)'                  )
         
      END IF
      
C
C     Fetch the geophysical constants needed for the SGP4 propagation
C     model from the kernel pool.
C
      CALL BODVAR ( 399, 'J2', N, GEOPHS ( J2 ) )
      CALL BODVAR ( 399, 'J3', N, GEOPHS ( J3 ) )
      CALL BODVAR ( 399, 'J4', N, GEOPHS ( J4 ) )
      CALL BODVAR ( 399, 'KE', N, GEOPHS ( KE ) )
      CALL BODVAR ( 399, 'QO', N, GEOPHS ( QO ) )
      CALL BODVAR ( 399, 'SO', N, GEOPHS ( SO ) )
      CALL BODVAR ( 399, 'ER', N, GEOPHS ( ER ) )
      CALL BODVAR ( 399, 'AE', N, GEOPHS ( AE ) )
  
C
C     Read next line of found TLE data.
C
      CALL RDTEXT (  INPFN, LINES(2), EOF )

      IF (  EOF ) THEN

C
C        Next line does not exist. Complain.
C 
         CALL DAFCLS ( HANDLE )
         CALL DELFIL ( OUTFN  )
         CALL SETMSG ( 'Second line of two-line element '  // 
     .                 'data for object # does not exist'   ) 
         CALL ERRINT ( '#', OBIDVL                          ) 
         CALL SIGERR ( 'SPICE(NOTEXISTSECONDLINE)'          )
 
      END IF

C
C     Track the number of TLE's read, I.
C
      I      = 0
      J      = 1 - NELEMS
      FMODEL = .TRUE.
 
      DO WHILE ( LINES(1)(1:7) .EQ. CHOSE1(1:7) .AND. .NOT. EOF )
 
         I = I + 1
         J = J + NELEMS

C
C        Try to process the TLE. If an error occurs during processing,
C        ZZGETELM will have value .FALSE. and ERROR will contain a
C        description of the error.
C
         CALL ZZGETELM ( 1950, LINES, EPOCHS(I), ELEMS(J), OK, ERROR(2))

C
C        If we find an error, signal a standard SPICE error.
C
         IF ( .NOT. OK ) THEN

            CALL DAFCLS ( HANDLE )
            CALL DELFIL ( OUTFN )
            
            CALL SETMSG ( 'Error in TLE set #1. #2' )
            CALL ERRINT ( '#1', I         )
            CALL ERRCH  ( '#2', ERROR(2)  )
            CALL SIGERR ( 'SPICE(BADTLE)' )

         END IF

C
C        If we fill up the EPOCHS buffer, then we need to complete this
C        segment. If the TLE read failed, no reason to evaluate this
C        block. Wait for the next iteration of the loop.
C
         IF ( I .EQ. MAXEPC .AND. OK ) THEN   

C
C        It may occasionally happen that the epochs may are out 
C        of order and may contain duplicate entries.  We need to 
C        sort them, shift duplicates to the end of the EPOCHS array
C        and then rearrange the elements.  Getting the correct 
C        order is easy. Just get an order vector.
C
            CALL ORDERD ( EPOCHS, MAXEPC, IORDER )

C
C           Re-arrange the order vector so that duplicates are placed
C           at the end of the array.
C
            TRASH       = MAXEPC
            PUT         = 1
            ORDVEC(PUT) = IORDER(1)
 
            DO K = 2, MAXEPC
 
               IF ( EPOCHS( ORDVEC(PUT) ) .EQ. EPOCHS( IORDER(K) ) )
     .         THEN
                  ORDVEC(TRASH) = IORDER(K)
                  TRASH         = TRASH - 1
               ELSE
                  PUT         = PUT + 1
                  ORDVEC(PUT) = IORDER(K)
               END IF
 
            END DO
 
            CALL REORDD ( ORDVEC, MAXEPC,         EPOCHS )
            CALL REORBD ( ORDVEC, MAXEPC, NELEMS, ELEMS  )
 
            IF ( FMODEL ) THEN
C
C              We shall allow propagation backwards and forward by
C              COVPAD seconds.
C
               BEGINT = EPOCHS(1) - COVPAD
               FMODEL = .FALSE.
 
            ELSE
               BEGINT = EPOCHS(1)
            END IF
 
C
C           Assign the final epoch time.
C
            ENDT   = EPOCHS(PUT)

C
C           Report that we write next SPK segment.
C                    
            CALL TOSTDO ( ' '                            )
            CALL TOSTDO ( 'Buffer is filled, writing SPK segment...')
 
            CALL SPKW10 ( HANDLE,  OBJECT,  CNIDVL, FRNMVL,
     .                    BEGINT,  ENDT,    SGIDVL,
     .                    GEOPHS,  PUT,     ELEMS,  EPOCHS )

C
C           We will continue processing of the input data and therefore
C           we need to achieve continuity of the data between segments.
C           To do that, we move one record from the end of the buffer
C           to the beginning of the buffer and reset all indexes
C           correspondingly.
C           
            EPOCHS(1) = EPOCHS(PUT)
 
            DO I = 1, NELEMS
               ELEMS(I) = ELEMS( (PUT-1)*10 + I )
            END DO
 
            I = 1
            J = 1
 
         END IF
 
C
C        Try to read the next TLE set.        
C
         CALL RDTEXT (  INPFN, LINES(1), EOF )
         CALL RDTEXT (  INPFN, LINES(2), EOF )

      END DO

C
C     We either ran out of file, or we ran out of elements.
C     In either case if there are any remaining element sets
C     to be written, now is the time to do it.
C
      IF ( I .GT. 1 ) THEN
 
C
C        It may occasionally happen that the epochs may are out 
C        of order and may contain duplicate entries.  We need to 
C        sort them, shift duplicates to the end of the EPOCHS array
C        and then rearrange the elements.  Getting the correct 
C        order is easy. Just get an order vector.
C
         CALL ORDERD ( EPOCHS, I, IORDER )

C
C        Re-arrange the order vector so that duplicates are placed
C        at the end of the array.
C
         TRASH       = I
         PUT         = 1
         ORDVEC(PUT) = IORDER(1)
 
         DO K = 2, I
 
            IF ( EPOCHS( ORDVEC(PUT) ) .EQ. EPOCHS( IORDER(K) ) )
     .      THEN
               ORDVEC(TRASH) = IORDER(K)
               TRASH         = TRASH - 1
            ELSE
               PUT         = PUT + 1
               ORDVEC(PUT) = IORDER(K)
            END IF
 
         END DO
 
 
         IF ( ISORDV(ORDVEC,I) ) THEN
 
            LINES(1) = 'Found # duplicates'
            CALL REPMI ( LINES(1), '#', I-TRASH, LINES(1) )
 
            CALL TOSTDO ( ' ' )
            CALL TOSTDO ( 'The order vector is ready to go. ' )
            CALL TOSTDO ( LINES(1) )
            CALL TOSTDO ( ' ' )
         ELSE
            CALL TOSTDO ( ' ' )
            CALL TOSTDO ( 'A bug exists in the order vector ' )
            CALL TOSTDO ( 'construction code.' )
            CALL TOSTDO ( ' ' )
         END IF
 
         CALL REORDD ( ORDVEC, I,         EPOCHS )
         CALL REORBD ( ORDVEC, I, NELEMS, ELEMS  )
 
         IF ( FMODEL ) THEN
            BEGINT = EPOCHS(1) - COVPAD
            FMODEL = .FALSE.
         ELSE
            BEGINT = EPOCHS(1)
         END IF

C
C        Since this is the last segment we shall create we
C        will allow the state to be propagated forward beyond
C        the last epoch by COVPAD seconds.
C
         ENDT   = EPOCHS(PUT) + COVPAD

         CALL TOSTDO ( ' '                        )
         CALL TOSTDO ( 'Writing  SPK segment...'  )
 
         CALL SPKW10 ( HANDLE,  OBJECT,  CNIDVL, FRNMVL,
     .                 BEGINT,  ENDT,    SGIDVL,
     .                 GEOPHS,  PUT,     ELEMS,  EPOCHS )
 
      ELSE
 
         PUT = 1
 
         IF ( FMODEL ) THEN
            BEGINT = EPOCHS(1) - COVPAD
            FMODEL = .FALSE.
         ELSE
            BEGINT = EPOCHS(1)
         END IF
 
         ENDT   = EPOCHS(PUT) + COVPAD
         CALL TOSTDO ( ' '                       )
         CALL TOSTDO ( 'Writing  SPK segment...' )
 
         CALL SPKW10 ( HANDLE,  OBJECT,  CNIDVL, FRNMVL,
     .                 BEGINT,  ENDT,    SGIDVL,
     .                 GEOPHS,  PUT,     ELEMS,  EPOCHS )
 
      END IF
 
      CALL CLTEXT ( INPFN )
      CALL CHKOUT ( 'TLE2SPK' ) 
   
      END
