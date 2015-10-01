C$Procedure ZZHOLDD ( Private --- hold a scalar DP )

      SUBROUTINE ZZHOLDD ( OP, VALUE )

C$ Abstract
C
C     SPICE private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due to the
C     volatile nature of this routine.
C
C     Persistently store a double precision value or retrieve a 
C     stored double precision value.
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
C     None.
C
C$ Keywords
C
C     STORE_VALUE
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         OP
      DOUBLE PRECISION      VALUE

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     OP         I   String name of operation to execute
C     VALUE     I-O  Double precision value returned or to store
C
C$ Detailed_Input
C
C     OP          The scalar string name of the operation to execute.
C                 Proper values of OP:
C
C                    'PUT'   store a double precision value for later
C                            use
C
C                    'GET'   retrieve a stored double precision value
C
C                    'RESET' reset function to require a PUT prior
C                            to a subsequent GET.
C
C     VALUE       The scalar double precision value to store; 
C                 corresponding to a 'PUT' OP.
C
C$ Detailed_Output
C
C     VALUE       The scalar double precision value returned; 
C                 corresponding to a 'GET' OP. The value is that stored 
C                 by the previous 'PUT' operation.
C
C$ Parameters
C
C    None.
C
C$ Exceptions
C
C     1)  The error SPICE(ZZHOLDNOPUT) signals if a 'GET' operation
C         precedes any 'PUT' operation.
C
C     2)  The error SPICE(UNKNOWNOP) signals if the value of OP is
C         neither 'GET', 'PUT', or 'RESET'.
C
C$ Files
C
C    None.
C
C$ Particulars
C
C     This routine simply stores a double precision value for
C     later retrieval. The value stored persists in memory until
C     overwritten by a subsequent 'PUT' operation.
C     
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine 
C     specific arithmetic implementation. 
C
C     Store values using ZZHOLDD then attempt to retrieve the values.
C
C           PROGRAM ZZHOLDD_T
C           
C           IMPLICIT NONE
C           
C           DOUBLE PRECISION      VAL
C     
C     C
C     C     Set a default value for VAL.
C     C
C           VAL = 0.D0
C     
C     C
C     C     Store 941.0 in ZZHOLDD.
C     C
C           CALL ZZHOLDD ( 'PUT', 941.D0 )
C     
C     C
C     C     Retrieve 941.0 to VAL.
C     C
C           CALL ZZHOLDD ( 'GET', VAL )
C     
C     C
C     C     Output VAL. It should have value 941.0.
C     C
C           WRITE (*,*) VAL
C     
C     
C     C
C     C     Another 'PUT' 'GET' cycle.
C     C
C           CALL ZZHOLDD ( 'PUT', 830.D0 )
C     
C     C
C     C     Output VAL. It should have value 830.0.
C     C
C           CALL ZZHOLDD ( 'GET', VAL )
C           
C           WRITE (*,*) VAL
C     
C     
C           END
C
C   The program outputs (OS X Intel run):
C     
C       941.  
C       830.
C
C    As expected.
C
C$ Restrictions
C
C     Code logic enforces the requirement at least one 'PUT' operation
C     occurs before a 'GET'. You can't 'GET' until at least one 'PUT'.
C     'RESET' returns the routine to the state requiring a 'PUT'.
C
C$ Literature_References
C
C    None.
C
C$ Author_and_Institution
C
C    E.D. Wright    (JPL)
C
C$ Version
C
C-   SPICELIB Version 1.0.0  16-FEB-2010 (EDW) 
C
C-&

C$ Index_Entries
C
C    store a double precision value
C    retrieve a stored double precision value
C
C-&

      LOGICAL               EQSTR

      DOUBLE PRECISION      S_VALUE

      LOGICAL               FIRST

      SAVE                  S_VALUE
      SAVE                  FIRST

      DATA                  FIRST  / .TRUE. /

      IF ( EQSTR( OP, 'GET' ) ) THEN

C
C        Retrieve a stored double precision value. Signal
C        an error if a "GET" call occurs prior to a "PUT."
C
         IF ( FIRST ) THEN
            CALL CHKIN  ( 'ZZHOLDD'                    )
            CALL SETMSG ( 'ZZHOLDD GET called without '
     .            //      'PUT initialization. Either the '
     .            //      'first GET call of program run or '
     .            //      'first GET call after RESET.' )
            CALL SIGERR ( 'SPICE(ZZHOLDNOPUT) '         )
            CALL CHKOUT ( 'ZZHOLDD'                     )
            RETURN
         END IF

         VALUE = S_VALUE

      ELSE IF ( EQSTR( OP, 'PUT' ) ) THEN

C
C        Store a value for later use. Set FIRST to false
C        so subsequent "GET" calls will work.
C
         IF ( FIRST ) THEN
            FIRST = .FALSE.
         END IF
 
         S_VALUE = VALUE

      ELSE IF ( EQSTR( OP, 'RESET' ) ) THEN

C
C        Reset FIRST forcing a PUT before an further GET.
C
         FIRST = .TRUE.

      ELSE

C
C        'OP' not "PUT," "RESET" or "GET." Signal an error.
C
         CALL CHKIN  ( 'ZZHOLDD'                  ) 
         CALL SETMSG ( 'Unknown operation ''#''. ' 
     .            //   'Routine supports only GET, PUT and RESET.')
         CALL ERRCH  ( '#', OP                    )
         CALL SIGERR ('SPICE(UNKNOWNOP)'          )
         CALL CHKOUT ( 'ZZHOLDD'                  )
         RETURN

      END IF
      
      END

