C$Procedure  ZZGEOSEG ( Find segments used to compute geometric state )
 
      SUBROUTINE ZZGEOSEG ( TARGET, ET, OBSRVR, N, HANLST, DSCLST )
 
C$ Abstract
C
C     Return the list of handles and segment descriptors identifying
C     the SPK segments used to produce a specified geometric state
C     vector.
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
C     SPK
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations
 
      IMPLICIT NONE
      
      INTEGER               MAXSEG
      PARAMETER           ( MAXSEG = 200 )
      
      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = 5 )
      
      INTEGER               TARGET
      DOUBLE PRECISION      ET
      INTEGER               OBSRVR
      INTEGER               N
      INTEGER               HANLST ( * )
      DOUBLE PRECISION      DSCLST ( DSCSIZ, * )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TARGET     I   Target body.
C     ET         I   Target epoch.
C     OBSRVR     I   Observing body.
C     N          O   Number of segments used to construct state.
C     HANLST     O   List of SPK handles.
C     DSCLST     O   List of SPK segment descriptors.
C
C$ Detailed_Input
C
C     TARGET      is the standard NAIF ID code for a target body.
C
C     ET          is the epoch (ephemeris time) for which the time
C                 interval of continuous state data is to be found.
C
C     OBSRVR      is the standard NAIF ID code for an observing body.
C
C
C$ Detailed_Output
C
C     N           is the number of segments used to construct the 
C                 geometric state vector defined by TARGET, ET, and 
C                 OBSRVR. If state cannot be computed, N is set to 
C                 zero.
C
C     HANLST      is a list of handles of SPK files containing the
C                 segments used to construct the specified state.
C
C     DSCLST      is a list of descriptors of the SPK segments used to 
C                 construct the specified state.
C
C$ Parameters
C
C     MAXSEG      is the maximum number of segments that may be
C                 required to construct a state.  Normally, a state
C                 can be constructed using four or fewer segments.
C
C$ Exceptions
C
C     1) If the number of segments required to construct the specified
C        state exceeds MAXSEG, the error SPICE(PARAMETERTOOSMALL) is
C        signaled.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     The method used to build a list of segments required to construct
C     a state vector is basically that used in SPKGEO.
C
C$ Examples
C
C     See the routine SPKBRK.
C
C$ Restrictions
C
C     1) The parameter MAXSEG limits the number of segments that can
C        be used to build a state vector.  Normally, the number of
C        segments will be four or fewer, while MAXSEG is at least 200,
C        so overflow is not likely.
C
C$ Literature_References
C
C     NAIF Document 168.0, "S- and P- Kernel (SPK) Specification and
C     User's Guide"
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    Beta Version 2.0.0, 14-SEP-2000 (NJB)
C
C        Interface was changed to return N=0 instead of signaling an
C        for cases when state could not be computed.
C
C-    Beta Version 1.0.0, 29-APR-1997 (NJB)  
C
C-&


C
C     SPICELIB functions
C
      INTEGER               ISRCHI
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
C     CHLEN is the maximum length of a chain.  That is,
C     it is the maximum number of bodies in the chain from
C     the target or observer to the SSB.
C
      INTEGER               CHLEN
      PARAMETER           ( CHLEN = MAXSEG/2 )
      
      INTEGER               ND
      PARAMETER           ( ND    = 2 )
      
      INTEGER               NI
      PARAMETER           ( NI    = 6 )
      
      INTEGER               SSB 
      PARAMETER           ( SSB   = 0 )

C
C     Local variables
C
      CHARACTER*(40)        IDENT
 
      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      DESCR  ( DSCSIZ )
 
      INTEGER               COBS    
      INTEGER               CTARG  ( CHLEN )
      INTEGER               CTPOS
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               NCT
      
      LOGICAL               FOUND
      
C
C     Save everything to prevent potential memory problems in f2c'ed
C     version.
C
      SAVE

      
      
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZGEOSEG' )
      END IF
      
C
C     Basically, we mimic the logic in SPKGEO.  But we don't need to
C     actually compute any states, and we do need to keep track of
C     every segment we look up, including those for the observer.
C
C     To start out, the segment list is empty.
C
      N = 0
         
C
C     We take care of the obvious case first.  If TARGET and OBSRVR are 
C     the same we can just fill in zero.
C
      IF ( TARGET .EQ. OBSRVR ) THEN
 
         CALL CHKOUT ( 'ZZGEOSEG' )
         RETURN
 
      END IF
      
C
C     CTARG contains the integer codes of the bodies in the
C     target body chain, beginning with TARGET itself and then
C     the successive centers of motion.
C
C     COBS will contain the centers of the observing body.
C
C     Then we follow the chain, filling up CTARG as we go.  We use
C     SPKSFS to search through loaded files to find the first segment 
C     applicable to CTARG(1) and time ET.  Then we get its center
C     CTARG(2).
C
C     We repeat the process for CTARG(2) and so on, until
C     there is no data found for some CTARG(I) or until we
C     reach the SSB.
C
C     Next, we find centers and states in a similar manner
C     for the observer.  It's a similar construction as
C     described above, but COBS is overwritten with each new center,
C     beginning at OBSRVR.  However, we stop when we encounter
C     a common center of motion, that is when COBS is equal
C     to CTARG(I) for some I.
C
C     CTPOS is the position in CTARG of the common node.
C
C     Fill in CTARG until no more data is found or until we reach the 
C     SSB.  
C
C     Note the check for FAILED in the loop.  If SPKSFS happens to fail 
C     during execution, and the current error handling action is to NOT 
C     abort, then FOUND may be stuck at TRUE, CTARG(I) will never
C     become zero, and the loop would otherwise execute indefinitely.
C
C
      I        =  1
      CTARG(I) =  TARGET
      FOUND    = .TRUE.
 
      DO WHILE (           FOUND
     .            .AND.  ( I        .LT. CHLEN  )
     .            .AND.  ( CTARG(I) .NE. OBSRVR )
     .            .AND.  ( CTARG(I) .NE. SSB    )  )
 
C
C        Find a file and segment that has state
C        data for CTARG(I).
C
         CALL SPKSFS ( CTARG(I), ET, HANDLE, DESCR, IDENT, FOUND )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGEOSEG' )
            RETURN
         END IF
         
         IF ( FOUND ) THEN
C
C           DESCR designates a segment giving the state of CTARG(I) 
C           relative to some center of motion.  This new center goes in
C           CTARG(I+1).  The handle and descriptor get added to our
C           lists.
C
            I = I + 1
            N = N + 1
            
            IF ( N .GT. MAXSEG ) THEN
            
               CALL SETMSG ( 'Segment list is full, requires at # ' //
     .                       'least segments.'                       )
               CALL ERRINT ( '#',  MAXSEG + 1                        )
               CALL SIGERR ( 'SPICE(PARAMETERTOOSMALL)'              )
               CALL CHKOUT ( 'ZZGEOSEG'                              )
               RETURN
                                                     
            END IF
            
            HANLST ( N ) =  HANDLE
            CALL MOVED ( DESCR, DSCSIZ, DSCLST(1,N) )
            
C
C           The center of motion of COBS becomes the new COBS.
C
            CALL DAFUS ( DESCR, ND, NI, DC, IC )
 
            CTARG(I) = IC(2)
 
         END IF
 
      END DO
 
C
C     NCT is the number of elements in CTARG,
C     the chain length. 
C
      NCT = I
 
C
C     Now follow the observer's chain.  Assign
C     the first values for COBS and SOBS.
C
      COBS = OBSRVR
 
C
C     Perhaps we have a common node already.
C     If so it will be the last node on the
C     list CTARG.
C
C     We let CTPOS will be the position of the common
C     node in CTARG if one is found.  It will
C     be zero if COBS is not found in CTARG.
C
      IF ( CTARG(NCT) .EQ. COBS ) THEN
         CTPOS  = NCT
      ELSE
         CTPOS  = 0
      END IF
 
C
C     Repeat the same loop as above, but each time
C     we encounter a new center of motion, check to
C     see if it is a common node.  
C
      FOUND  = .TRUE.
 
      DO WHILE (           FOUND
     .            .AND.  ( COBS  .NE. SSB )
     .            .AND.  ( CTPOS .EQ. 0   )  )
 
C
C        Find a file and segment that has state
C        data for COBS.
C
         CALL SPKSFS ( COBS, ET, HANDLE, DESCR, IDENT, FOUND )
 
C
C        Check failed.  We don't want to loop indefinitely.
C
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGEOSEG' )
            RETURN
         END IF
         
         
         IF ( FOUND ) THEN
C
C           Add the handle and descriptor of the new segment to
C           our lists.
C        
            N = N + 1
            
            IF ( N .GT. MAXSEG ) THEN
            
               CALL SETMSG ( 'Segment list is full, requires at # ' //
     .                       'least segments.'                       )
               CALL ERRINT ( '#',  MAXSEG + 1                        )
               CALL SIGERR ( 'SPICE(PARAMETERTOOSMALL)'              )
               CALL CHKOUT ( 'ZZGEOSEG'                              )
               RETURN
                                                     
            END IF
            
            HANLST ( N ) =  HANDLE
            CALL MOVED ( DESCR, DSCSIZ, DSCLST(1,N) )
            
C
C           The center of motion of COBS becomes the new COBS.
C
            CALL DAFUS ( DESCR, ND, NI, DC, IC )
 
            COBS = IC(2)
 
C
C           See if the new center is a common node. If not, repeat the 
C           loop.
C
            CTPOS = ISRCHI ( COBS, NCT, CTARG )
 
         END IF
 
      END DO
 
C
C     If CTPOS is zero at this point, it means we
C     have not found a common node though we have
C     searched through all the available data.
C
      IF ( CTPOS .EQ. 0 ) THEN
 
         N = 0
 
      END IF
      
      CALL CHKOUT ( 'ZZGEOSEG' )
      RETURN
      END
      
