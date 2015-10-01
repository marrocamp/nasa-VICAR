C$Procedure ZZPHSH (Private---kernel pool hash function)
 
      INTEGER FUNCTION ZZPHSH ( WORD, M, M2 )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This is an umbrella routine for the kernel pool hash function.
C     It should never be called directly.
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
C      None.
C
C$ Keywords
C
C       PRIVATE UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         WORD
      INTEGER               M
      INTEGER               M2
 
C$ Brief_I/O
C
C      VARIABLE  I/O  Entry point
C      --------  ---  --------------------------------------------------
C      WORD       I   ZZHASH
C      M          I   ZZSHSH
C
C      The function returns zero.
C
C$ Detailed_Input
C
C     See individual entry points.
C
C$ Detailed_Output
C
C     The function ZZPHSH should never be called. However, it returns
C     the value zero.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This routine is an umbrella for the kernel pool hash function
C     ZZHASH, ZZHASH2 and the set up routine ZZSHSH.
C
C$ Examples
C
C     To make use of the ZZHAS hash function you must first call ZZSHSH
C     somewhere in your program. The value returned by ZZSHSH has
C     no meaning.  You can assign it to any temporary variable you
C     happen to have lying around.
C
C        I = ZZSHSH ( M )
C
C           ...any other set up code...
C
C        LOOKAT = ZZHASH ( WORD )
C
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 21-NOV-2006 (EDW)(BVS)
C
C        Replaced ICHAR('\\') expression with parameter
C        BSLASH, the parameter set to the ASCII value
C        of the backslash character, 92.
C
C-    SPICELIB Version 1.1.0, 14-SEP-2005 (EDW)
C
C        Added function ZZHASH2. Operation matches
C        that of ZZHASH with the exception that ZZHASH2
C        accepts the divisor value, M, as an input.
C
C-    SPICELIB Version 1.0.0, 20-SEP-1995 (WLT)
C
C-&
 
C
C     Entry Points
C
      INTEGER               ZZSHSH
      INTEGER               ZZHASH
      INTEGER               ZZHASH2
C
C     Local Variables.
C
      INTEGER               I
      INTEGER               DIVISR
      INTEGER               VAL ( 0:128 )
      INTEGER               F
      INTEGER               BASE
      INTEGER               BLANK
      INTEGER               LENGTH

      INTEGER               BSLASH
      PARAMETER           ( BSLASH = 92 )
 
 
      LOGICAL               FIRST
      SAVE
 
      DATA                  FIRST / .TRUE. /
C
C     We do not diagnose a bogus call since this is a private routine.
C
      ZZPHSH = 0
      RETURN
 
 
 
C$Procedure      ZZSHSH (Private---Set up hash function)
 
      ENTRY ZZSHSH ( M )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This routine sets up the kernel pool hash function.  Call it
C     once per program execution.
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
C      None.
C
C$ Keywords
C
C       PRIVATE UTILITY
C
C$ Declarations
C
C     INTEGER               M
C
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      M          I   Modulus used for the hash function
C
C      The function returns 0.
C
C$ Detailed_Input
C
C     M           is the modulus of the hashing function.  It is
C                 recommended that this be a prime number.
C
C$ Detailed_Output
C
C     The function returns the value zero (0).
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This entry point sets up the modulus used for hashing input
C     strings.  It should be called once by an initialization
C     branch of the kernel pool.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 21-NOV-2006 (EDW)(BVS)
C
C        Replaced ICHAR('\\') expression with parameter
C        BSLASH, the parameter set to the ASCII value
C        of the backslash character, 92.
C
C-    SPICELIB Version 1.1.0, 06-JUL-2005 (EDW)
C
C        Added punctuation marks to array of allowed
C        characters. The function can process any
C        character with ASCII decimal value 33 to 122.
C
C-    SPICELIB Version 1.0.0, 20-SEP-1995 (WLT)
C
C-&
 
      DIVISR = M
 
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         BASE  = 68
         BLANK = ICHAR( ' ' )
 
         DO I = 0, 128
            VAL(I) = 0
         END DO
 
         VAL(ICHAR('0')) = 1
         VAL(ICHAR('1')) = 2
         VAL(ICHAR('2')) = 3
         VAL(ICHAR('3')) = 4
         VAL(ICHAR('4')) = 5
         VAL(ICHAR('5')) = 6
         VAL(ICHAR('6')) = 7
         VAL(ICHAR('7')) = 8
         VAL(ICHAR('8')) = 9
         VAL(ICHAR('9')) = 10
         VAL(ICHAR('A')) = 11
         VAL(ICHAR('B')) = 12
         VAL(ICHAR('C')) = 13
         VAL(ICHAR('D')) = 14
         VAL(ICHAR('E')) = 15
         VAL(ICHAR('F')) = 16
         VAL(ICHAR('G')) = 17
         VAL(ICHAR('H')) = 18
         VAL(ICHAR('I')) = 19
         VAL(ICHAR('J')) = 20
         VAL(ICHAR('K')) = 21
         VAL(ICHAR('L')) = 22
         VAL(ICHAR('M')) = 23
         VAL(ICHAR('N')) = 24
         VAL(ICHAR('O')) = 25
         VAL(ICHAR('P')) = 26
         VAL(ICHAR('Q')) = 27
         VAL(ICHAR('R')) = 28
         VAL(ICHAR('S')) = 29
         VAL(ICHAR('T')) = 30
         VAL(ICHAR('U')) = 31
         VAL(ICHAR('V')) = 32
         VAL(ICHAR('W')) = 33
         VAL(ICHAR('X')) = 34
         VAL(ICHAR('Y')) = 35
         VAL(ICHAR('Z')) = 36
         VAL(ICHAR('-')) = 37
         VAL(ICHAR('_')) = 38
         VAL(ICHAR('.')) = 39
         VAL(ICHAR('/')) = 40
         VAL(ICHAR('!')) = 41
         VAL(ICHAR('@')) = 42
         VAL(ICHAR('#')) = 43
         VAL(ICHAR('$')) = 44
         VAL(ICHAR('%')) = 45
         VAL(ICHAR('^')) = 46
         VAL(ICHAR('&')) = 47
         VAL(ICHAR('*')) = 48
         VAL(ICHAR('(')) = 49
         VAL(ICHAR(')')) = 50
         VAL(ICHAR('+')) = 51
         VAL(ICHAR('=')) = 52
         VAL(ICHAR('[')) = 53
         VAL(ICHAR('{')) = 54
         VAL(ICHAR(']')) = 55
         VAL(ICHAR('}')) = 56
         VAL(ICHAR('|')) = 57
         VAL(BSLASH    ) = 58
         VAL(ICHAR(':')) = 59
         VAL(ICHAR(';')) = 60
         VAL(ICHAR('<')) = 61
         VAL(ICHAR(',')) = 62
         VAL(ICHAR('>')) = 63
         VAL(ICHAR('?')) = 64

C
C        Note, ICHAR('''') returns the ASCII 
C        value for the single quote -> '
C
         VAL(ICHAR(''''))= 65
         VAL(ICHAR('"')) = 66
         VAL(ICHAR('`')) = 67
         VAL(ICHAR('~')) = 68
 
 
         VAL(ICHAR('a')) = VAL(ICHAR('A'))
         VAL(ICHAR('b')) = VAL(ICHAR('B'))
         VAL(ICHAR('c')) = VAL(ICHAR('C'))
         VAL(ICHAR('d')) = VAL(ICHAR('D'))
         VAL(ICHAR('e')) = VAL(ICHAR('E'))
         VAL(ICHAR('f')) = VAL(ICHAR('F'))
         VAL(ICHAR('g')) = VAL(ICHAR('G'))
         VAL(ICHAR('h')) = VAL(ICHAR('H'))
         VAL(ICHAR('i')) = VAL(ICHAR('I'))
         VAL(ICHAR('j')) = VAL(ICHAR('J'))
         VAL(ICHAR('k')) = VAL(ICHAR('K'))
         VAL(ICHAR('l')) = VAL(ICHAR('L'))
         VAL(ICHAR('m')) = VAL(ICHAR('M'))
         VAL(ICHAR('n')) = VAL(ICHAR('N'))
         VAL(ICHAR('o')) = VAL(ICHAR('O'))
         VAL(ICHAR('p')) = VAL(ICHAR('P'))
         VAL(ICHAR('q')) = VAL(ICHAR('Q'))
         VAL(ICHAR('r')) = VAL(ICHAR('R'))
         VAL(ICHAR('s')) = VAL(ICHAR('S'))
         VAL(ICHAR('t')) = VAL(ICHAR('T'))
         VAL(ICHAR('u')) = VAL(ICHAR('U'))
         VAL(ICHAR('v')) = VAL(ICHAR('V'))
         VAL(ICHAR('w')) = VAL(ICHAR('W'))
         VAL(ICHAR('x')) = VAL(ICHAR('X'))
         VAL(ICHAR('y')) = VAL(ICHAR('Y'))
         VAL(ICHAR('z')) = VAL(ICHAR('Z'))
 
      END IF
 
      ZZSHSH = 0
      RETURN
 
 
 
C$Procedure      ZZHASH (Private --- Hash function)
 
      ENTRY ZZHASH ( WORD )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This routine computes the hash value associated with a kernel
C     pool variable name.
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
C      None.
C
C$ Keywords
C
C       PRIVATE UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         WORD
C
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      WORD       I   A left justified string of characters.
C
C      The function returns the hash value associated with WORD.
C
C$ Detailed_Input
C
C     WORD        is a left justified string of characters.  Nominally
C                 this is the name of some kernel pool variable.
C
C$ Detailed_Output
C
C     The function returns the hash value of WORD
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This routine computes the hash value of a left justified
C     string of characters.  It is critical that the string be
C     left justified.  All non-left justified strings map to the
C     same value 0.
C
C$ Examples
C
C     See POOL.
C
C$ Restrictions
C
C     1) If the has value calculates to a negative value, an error
C        signals. Such a signal should never occur.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 06-JUL-2005 (EDW)
C
C        Added error test to catch non-positive hash values. 
C
C-    SPICELIB Version 1.0.0, 20-SEP-1995 (WLT)
C
C-&

      F      = 0
      LENGTH = LEN( WORD )
 
      DO I = 1, LENGTH
 
         IF ( ICHAR(WORD(I:I)) .EQ. BLANK ) THEN
            ZZHASH = MOD ( F*BASE, DIVISR ) + 1
            RETURN
         END IF
 
         F = VAL(  MIN( 128, ICHAR(WORD(I:I)) )  ) + F * BASE
         F = MOD( F, DIVISR )
 
      END DO
 
      ZZHASH = MOD( F*BASE, DIVISR ) + 1

C
C     A non-positive value for ZZHASH indicates a serious problem.
C
      IF( ZZHASH .LT. 0 ) THEN
      
         CALL SETMSG( 'The ZZHASH function calculated a non-positive '
     .            //  'value for string $1. Contact NAIF' )
         CALL ERRCH  ( '$1', WORD  )
         CALL SIGERR( 'SPICE(BUG)' )
         RETURN

      END IF

      RETURN


C$Procedure ZZHASH2 (Private --- Hash function)

      ENTRY ZZHASH2 ( WORD, M2 )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This routine computes the hash value corresponding to an string
C     given a particular  divisor value (M2).
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
C      None.
C
C$ Keywords
C
C       PRIVATE UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         WORD
C
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      WORD       I   A left justified string of characters.
C      M2         I   Modulus used for the hash function
C
C      The function returns the hash value associated with WORD.
C
C$ Detailed_Input
C
C     WORD        is a left justified string of characters. 
C
C     M2          the modulus of the hashing function. This value
C                 defines the spread of the hash values, that
C                 spread covering the interval [0, M2-1]. The larger 
C                 the value, the less the chance of a hash key
C                 collision. The user should always chose a prime
C                 for M2.
C
C$ Detailed_Output
C
C     The function returns the hash value of WORD as computed using
C     M2 as the M divisor.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This routine computes the hash value of a left justified
C     string of characters.  It is critical that the string be
C     left justified.  All non-left justified strings map to the
C     same value 0.
C
C$ Examples
C
C     1) If the has value calculates to a negative value, an error 
C        signals. Such a signal should never occur.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C      E.D. Wright     (JPL)
C
C$ Literature_References
C
C     1)  Knuth, Donald E. "The Art of Computer Programming, Volume
C         3/Sorting and Searching 2nd Edition" 1997, pp 513-521.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 14-SEP-2005 (EDW)
C
C-&

      F      = 0
      LENGTH = LEN( WORD )
 
      DO I = 1, LENGTH
 
         IF ( ICHAR(WORD(I:I)) .EQ. BLANK ) THEN
            ZZHASH2 = MOD ( F*BASE, M2 ) + 1
            RETURN
         END IF
 
         F = VAL(  MIN( 128, ICHAR(WORD(I:I)) )  ) + F * BASE
         F = MOD( F, M2 )
 
      END DO
 
      ZZHASH2  = MOD( F*BASE, M2 ) + 1

C
C     A non-positive value for ZZHASH2 indicates a serious problem.
C
      IF( ZZHASH2 .LT. 0 ) THEN
      
         CALL SETMSG( 'The ZZHASH2 function calculated a non-positive '
     .            //  'value for string $1. Contact NAIF' )
         CALL ERRCH  ( '$1', WORD  )
         CALL SIGERR( 'SPICE(BUG)' )
         RETURN

      END IF

      RETURN
      END


