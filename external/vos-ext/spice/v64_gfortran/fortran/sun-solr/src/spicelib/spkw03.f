C$Procedure SPKW03 ( Write SPK segment, type 3 )
 
      SUBROUTINE SPKW03 (  HANDLE,  BODY,    CENTER,  FRAME,
     .                     FIRST,   LAST,    SEGID,   INTLEN,
     .                     N,       POLYDG,  CDATA,   BTIME )
 
C$ Abstract
C
C    Write a type 3 segment to an SPK file.
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
C     NAIF_IDS
C     SPC
C     SPK
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations
 
      INTEGER               HANDLE
      INTEGER               BODY
      INTEGER               CENTER
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      CHARACTER*(*)         SEGID
      DOUBLE PRECISION      INTLEN
      INTEGER               N
      INTEGER               POLYDG
      DOUBLE PRECISION      CDATA (*)
      DOUBLE PRECISION      BTIME
 
C$ Brief_I/O
C
C   Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of SPK file open for writing.
C     BODY       I   NAIF code for ephemeris object.
C     CENTER     I   NAIF code for the center of motion of the body.
C     FRAME      I   Reference frame name.
C     FIRST      I   Start time of interval covered by segment.
C     LAST       I   End time of interval covered by segment.
C     SEGID      I   Segment identifier.
C     INTLEN     I   Length of time covered by record.
C     N          I   Number of records in segment.
C     POLYDG     I   Chebyshev polynomial degree.
C     CDATA      I   Array of Chebyshev coefficients.
C     BTIME      I   Begin time of first record.
C
C$ Detailed_Input
C
C     HANDLE         DAF handle of an SPK file to which a type 3 segment
C                    is to be added.  The SPK file must be open for
C                    writing.
C
C     BODY           NAIF integer code for an ephemeris object whose
C                    state relative to another body is described by the
C                    segment to be created.
C
C     CENTER         NAIF integer code for the center of motion of the
C                    object identified by BODY.
C
C     FRAME          NAIF name for a reference frame relative to which
C                    the state information for BODY is specified.
C
C     FIRST,
C     LAST           Start and stop times of the time interval over
C                    which the segment defines the state of body.
C
C     SEGID          Segment identifier.  An SPK segment identifier may
C                    contain up to 40 characters.
C
C     INTLEN         Length of time, in seconds, covered by each set of
C                    Chebyshev polynomial coefficients (each logical
C                    record).  Each set of Chebyshev coefficents must
C                    cover this fixed time interval, INTLEN.
C
C     N              Number of sets of Chebyshev polynomial coefficients
C                    for coordinates and their derivatives (number of
C                    logical records) to be stored in the segment.
C                    There is one set of Chebyshev coefficients for each
C                    time period.
C
C     POLYDG         Degree of each set of Chebyshev polynomials.
C
C     CDATA          Array containing all the sets of Chebyshev
C                    polynomial coefficients to be placed in the
C                    segment of the SPK file.  The coefficients are
C                    stored in CDATA in order as follows:
C
C                       the (degree + 1) coefficients for the first
C                       coordinate of the first logical record
C
C                       the coefficients for the second coordinate
C
C                       the coefficients for the third coordinate
C
C                       the coefficients for the derivative of the first
C                       coordinate
C
C                       the coefficients for the derivative of the
C                       second coordinate
C
C                       the coefficients for the derivative of the third
C                       coordinate
C
C                       the coefficients for the first coordinate for
C                       the second logical record, ...
C
C                       and so on.
C
C
C     BTIME          Begin time (seconds past J2000 TDB) of first set
C                    of Chebyshev polynomial coefficients (first
C                    logical record).
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the number of sets of coefficients is not positive
C        'SPICE(NUMCOEFFSNOTPOS)' is signalled.
C
C     2) If the interval length is not positive, 'SPICE(INTLENNOTPOS)'
C        is signalled.
C
C     3) If the name of the reference frame is not recognized,
C        'SPICE(INVALIDREFFRAME)' is signalled.
C
C     4) If segment stop time is not greater then the begin time,
C        'SPICE(BADDESCRTIMES)' is signalled.
C
C     5) If the start time of the first record is not less than
C        or equal to the descriptor begin time, 'SPICE(BADDESCRTIMES)'
C        is signalled.
C
C     6) If the end time of the last record is not greater than
C        or equal to the descriptor end time, 'SPICE(BADDESCRTIMES)' is
C        signalled.
C
C$ Files
C
C     A new type 3 SPK segment is written to the SPK file attached
C     to HANDLE.
C
C$ Particulars
C
C     This routine writes an SPK type 3 data segment to the designated
C     SPK file, according to the format described in the SPK Required
C     Reading.
C
C     Each segment can contain data for only one target, central body,
C     and reference frame.  The Chebyshev polynomial degree and length
C     of time covered by each logical record are also fixed.  However,
C     an arbitrary number of logical records of Chebyshev polynomial
C     coefficients can be written in each segment.  Minimizing the
C     number of segments in an SPK file will help optimize how the SPICE
C     system accesses the file.
C
C$ Examples
C
C     Suppose that you have sets of Chebyshev polynomial coefficients
C     in an array CDATA pertaining to the position of the moon (NAIF ID
C     = 301), relative to the Earth-moon barycenter (NAIF ID = 3), in
C     the J2000 reference frame, and want to put these into a type 2
C     segment in an existing SPK file.  The following code could be used
C     to add one new type 2 segment.  To add multiple segments, put the
C     call to SPKW02 in a loop.
C
C     C
C     C      First open the SPK file and get a handle for it.
C     C
C            CALL DAFOPW ( SPKNAM, HANDLE )
C
C     C
C     C      Create a segment identifier.
C     C
C            SEGID = 'MY_SAMPLE_SPK_TYPE_3_SEGMENT'
C
C     C
C     C      Write the segment.
C
C            SUBROUTINE SPKW03 ( HANDLE, 301,    3,      'J2000',
C          .                     FIRST,  LAST,   SEGID,  INTLEN,
C          .                     N,      POLYDG, CDATA,  BTIME )
C
C     C
C     C      Close the file.
C     C
C            CALL DAFCLS ( HANDLE )
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
C     K.S. Zukor (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 30-OCT-2006 (BVS)
C
C        Removed restriction that the input reference frame should be
C        inertial by changing the routine that determins the frame ID
C        from the name from IRFNUM to NAMFRM.
C
C-    SPICELIB Version 1.0.1, 19-SEP-2006 (EDW)
C
C        Corrected typo in the section name ("Example" to "Examples").
C
C-    SPICELIB Version 1.0.0, 01-AUG-1995 (KSZ)
C
C-&
 
C$ Index_Entries
C
C     write spk type_3 data segment
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local Parameters
C
C     DTYPE is the SPK data type.
C
      INTEGER               DTYPE
      PARAMETER           ( DTYPE   =   3 )
C
C     ND is the number of double precision components in an SPK
C     segment descriptor. SPK uses ND = 2.
C
      INTEGER               ND
      PARAMETER           ( ND      =   2 )
C
C     NI is the number of integer components in an SPK segment
C     descriptor. SPK uses NI = 6.
C
      INTEGER               NI
      PARAMETER           ( NI      =   6 )
C
C     NS is the size of a packed SPK segment descriptor.
C
      INTEGER               NS
      PARAMETER           ( NS      =   5 )
C
C     SIDLEN is the maximum number of characters allowed in an
C     SPK segment identifier.
C
      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN  =  40 )
 
C
C     Local variables
C
      CHARACTER*(SIDLEN)    ETSTR
      CHARACTER*(SIDLEN)    NETSTR
 
      DOUBLE PRECISION      DCD   ( ND )
      DOUBLE PRECISION      DESCR ( NS )
      DOUBLE PRECISION      LTIME
      DOUBLE PRECISION      MID
      DOUBLE PRECISION      NUMREC
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      RSIZE
 
      INTEGER               I
      INTEGER               ICD   ( NI )
      INTEGER               K
      INTEGER               NINREC
      INTEGER               REFCOD
 
C
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKW03' )
      END IF
 
C
C     The number of sets of coefficients must be positive.
C
      IF ( N .LE. 0 ) THEN
 
         CALL SETMSG ( 'The number of sets of coordinate'  //
     .                 'coeffcients is not positive. N = #'        )
         CALL ERRINT ( '#', N                                      )
         CALL SIGERR ( 'SPICE(NUMCOEFFSNOTPOS)'                    )
         CALL CHKOUT ( 'SPKW03'                                    )
         RETURN
 
      END IF
 
C
C    The interval length must be positive.
C
      IF ( INTLEN .LE. 0 ) THEN
 
         CALL SETMSG ( 'The interval length is not positive.'  //
     .                 'N = #'                                     )
         CALL ERRDP  ( '#', INTLEN                                 )
         CALL SIGERR ( 'SPICE(INTLENNOTPOS)'                       )
         CALL CHKOUT ( 'SPKW03'                                    )
         RETURN
 
      END IF
 
C
C     Get the NAIF integer code for the reference frame.
C
      CALL NAMFRM ( FRAME, REFCOD )
 
      IF ( REFCOD .EQ. 0 ) THEN
 
         CALL SETMSG ( 'The reference frame # is not supported.'   )
         CALL ERRCH  ( '#', FRAME                                  )
         CALL SIGERR ( 'SPICE(INVALIDREFFRAME)'                    )
         CALL CHKOUT ( 'SPKW03'                                    )
         RETURN
 
      END IF
 
C
C     The segment stop time must be greater than the begin time.
C
      IF ( FIRST .GT. LAST ) THEN
 
         CALL SETMSG ( 'The segment descriptor start time: # is ' //
     .                 'greater than the segment end time: #'      )
         CALL ETCAL  ( FIRST, ETSTR                                )
         CALL ERRCH  ( '#',   ETSTR                                )
         CALL ETCAL  ( LAST,  NETSTR                               )
         CALL ERRCH  ( '#',   NETSTR                               )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'SPKW03'                                    )
         RETURN
 
      END IF
 
C
C     The begin time of the first record must be less than or equal
C     to the begin time of the segment.
C
      IF ( FIRST .LT. BTIME ) THEN
 
         CALL SETMSG ( 'The segment descriptor start time: # is ' //
     .                 'less than the beginning time of the segment ' //
     .                 'data: #'                                   )
         CALL ETCAL  ( FIRST, ETSTR                                )
         CALL ERRCH  ( '#',   ETSTR                                )
         CALL ETCAL  ( BTIME, ETSTR                                )
         CALL ERRCH  ( '#',   ETSTR                                )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'SPKW03'                                    )
         RETURN
 
      END IF
 
C
C     The end time of the final record must be greater than or
C     equal to the end time of the segment.
C
      LTIME = BTIME + N * INTLEN
      IF ( LAST .GT. LTIME ) THEN
 
         CALL SETMSG ( 'The segment descriptor end time: # is ' //
     .                 'greater than the end time of the segment' //
     .                 'data: #'                                   )
         CALL ETCAL  ( LAST,  ETSTR                                )
         CALL ERRCH  ( '#',   ETSTR                                )
         CALL ETCAL  ( LTIME, ETSTR                                )
         CALL ERRCH  ( '#',   ETSTR                                )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'SPKW03'                                    )
         RETURN
 
      END IF
 
C
C     Now check the validity of the segment identifier.
C
      CALL CHCKID ( 'SPK segment identifier', SIDLEN, SEGID )
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKW03' )
         RETURN
      END IF
 
C
C     Store the start and end times to be associated
C     with this segment.
C
      DCD(1) = FIRST
      DCD(2) = LAST
 
C
C     Create the integer portion of the descriptor.
C
      ICD(1) = BODY
      ICD(2) = CENTER
      ICD(3) = REFCOD
      ICD(4) = DTYPE
 
C
C     Pack the segment descriptor.
C
      CALL DAFPS ( ND, NI, DCD, ICD, DESCR )
 
C
C     Begin a new segment of SPK type 3 form:
C
C        Record 1
C        Record 2
C        ...
C        Record N
C        INIT       ( initial epoch of first record )
C        INTLEN     ( length of interval covered by each record )
C        RSIZE      ( number of data elements in each record )
C        N          ( number of records in segment )
C
C     Each record will have the form:
C
C        MID        ( midpoint of time interval )
C        RADIUS     ( radius of time interval )
C        X coefficients, Y coefficients, Z coefficients
C        X' coefficients, Y' coefficents, Z' coefficients
C
      CALL DAFBNA ( HANDLE, DESCR, SEGID )
 
C
C     Calculate the number of Chebyshev coefficients in a record.
C
      NINREC = (POLYDG + 1) * 6
 
C
C     Fill segment with N records of data.
C
      DO I = 1, N
 
C
C        Calculate the midpoint and radius of the time of each
C        record, and put that at the beginning of each record.
C
         RADIUS = INTLEN / 2
         MID    = BTIME + RADIUS + ( I - 1 ) * INTLEN
 
         CALL DAFADA ( MID,    1 )
         CALL DAFADA ( RADIUS, 1 )
 
C
C        Put one set of coefficients into the segment.
C
         K = 1 + ( I - 1 ) * NINREC
 
         CALL DAFADA ( CDATA(K), NINREC )
 
      END DO
 
C
C     Store the initial epoch of the first record.
C
      CALL DAFADA ( BTIME, 1 )
 
C
C     Store the length of interval covered by each record.
C
      CALL DAFADA ( INTLEN, 1 )
 
C
C     Store the size of each record (total number of array elements).
C
      RSIZE = 2 + NINREC
      CALL DAFADA ( RSIZE, 1 )
 
C
C     Store the number of records contained in the segment.
C
      NUMREC = N
      CALL DAFADA ( NUMREC, 1 )
 
C
C     End this segment.
C
      CALL DAFENA
 
      CALL CHKOUT ( 'SPKW03' )
      RETURN
      END
