C$Procedure      PARCML ( Parse command line)

      SUBROUTINE PARCML( LINE, MAXKEY, CLKEYS, CLFLAG, CLVALS, FOUND )

C$ Abstract
C
C     This routine parses "command-line" looking line and returns
C     values of requested keys.
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
C     None.
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         LINE
      INTEGER               MAXKEY
      CHARACTER*(*)         CLKEYS ( * )
      LOGICAL               CLFLAG ( * )
      CHARACTER*(*)         CLVALS ( * )
      LOGICAL               FOUND

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LINE       I   Input line.
C     MAXKEY     I   Number of keys.
C     CLKEYS     I   Keys.
C     CLFLAG     O   "Key-found" flags.
C     CLVALS     O   Key values.
C     FOUND      O   Flag indicating that at least one key was found.
C
C$ Detailed_Input
C
C     LINE        Input line in a format "-key value -key value ..."
C
C     MAXKEY      Total number of keys to look for.
C
C     CLKEYS      Keys to look for; uppercased.
C
C$ Detailed_Output
C
C     CLFLAG      Flags set TRUE if corresponding key was found.
C
C     CLVALS      Values key; if key wasn't found, value set to
C                 blank string.
C
C     FOUND       .TRUE. if at least one key was found.
C                 Otherwise -- .FALSE.
C
C$ Parameters
C
C     TBD.
C
C$ Exceptions
C
C     TBD
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     TBD
C
C$ Examples
C
C     Let CLKEYS be
C
C        CLKEYS(1) = '-SETUP'
C        CLKEYS(2) = '-TO'
C        CLKEYS(3) = '-FROM'
C        CLKEYS(4) = '-HELP'
C
C     then:
C
C     line '-setup my.file -from utc -to sclk'
C     will be parsed as
C
C        CLFLAG(1) = .TRUE.       CLVALS(1) = 'my.file'
C        CLFLAG(2) = .TRUE.       CLVALS(2) = 'utc'
C        CLFLAG(3) = .TRUE.       CLVALS(3) = 'sclk'
C        CLFLAG(4) = .FALSE.      CLVALS(4) = ' '
C        FOUND = .TRUE.
C
C     line '-setup my.file -setup your.file'
C     will be parsed as
C
C        CLFLAG(1) = .TRUE.       CLVALS(1) = 'your.file'
C        CLFLAG(2) = .FALSE.      CLVALS(2) = ' '
C        CLFLAG(3) = .FALSE.      CLVALS(3) = ' '
C        CLFLAG(4) = .FALSE.      CLVALS(4) = ' '
C        FOUND = .TRUE.
C
C     line '-setup my.file -SeTuP your.file'
C     will be parsed as
C
C        CLFLAG(1) = .TRUE.       CLVALS(1) = 'your.file'
C        CLFLAG(2) = .FALSE.      CLVALS(2) = ' '
C        CLFLAG(3) = .FALSE.      CLVALS(3) = ' '
C        CLFLAG(4) = .FALSE.      CLVALS(4) = ' '
C        FOUND = .TRUE.
C
C     line '-help'
C     will be parsed as
C
C        CLFLAG(1) = .FALSE.      CLVALS(1) = ' '
C        CLFLAG(2) = .FALSE.      CLVALS(2) = ' '
C        CLFLAG(3) = .FALSE.      CLVALS(3) = ' '
C        CLFLAG(4) = .TRUE.       CLVALS(4) = ' '
C        FOUND = .TRUE.
C
C     and so on.
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
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    Alpha Version 1.0.0, 12-SEP-2008 (BVS)
C
C
C-&
      INTEGER               LLSIZE
      PARAMETER           ( LLSIZE = 1024 )

      INTEGER               I
      INTEGER               PCLIDX
      INTEGER               CLIDX
      INTEGER               BEGPOS
      INTEGER               ENDPOS

      CHARACTER*(LLSIZE)    ULINE
      CHARACTER*(LLSIZE)    HLINE
      CHARACTER*(LLSIZE)    LNGWD
      CHARACTER*(LLSIZE)    HLNGWD
      CHARACTER*(LLSIZE)    HKEY

C
C     Save everything to prevent potential memory problems in f2c'ed
C     version.
C
      SAVE

C
C     SPICELIB functions.
C
      INTEGER               ISRCHC
      INTEGER               RTRIM
      INTEGER               POS
      LOGICAL               RETURN

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PARCML' )
      END IF

C
C     Command line parse loop. Set initial values to blanks.
C
      DO I = 1, MAXKEY
         CLFLAG( I ) = .FALSE.
         CLVALS( I ) = ' '
      END DO

      FOUND = .FALSE.

      HLINE  = LINE
      PCLIDX = 0
      CLIDX  = 0

      DO WHILE ( HLINE .NE. ' ' )

C
C        Get next word, uppercase it.
C
         CALL NEXTWD ( HLINE, LNGWD, HLINE )
         CALL UCASE  ( LNGWD, HLNGWD )
         CLIDX = ISRCHC( HLNGWD, MAXKEY, CLKEYS )

C
C        Is the token that we found a command line key?
C
         IF ( CLIDX .NE. 0 ) THEN

C
C           Is it the first key that we have found?
C
            IF ( PCLIDX .NE. 0 ) THEN

C
C              It's not. Save value of the previous key. Compute begin
C              and end position of substring that contains this
C              value.
C
               CALL UCASE( LINE, ULINE )
               BEGPOS =
     .            POS(ULINE, CLKEYS(PCLIDX)(:RTRIM(CLKEYS(PCLIDX))), 1)
     .            + RTRIM( CLKEYS(PCLIDX) )

               HKEY = ' ' // CLKEYS(CLIDX)(:RTRIM(CLKEYS(CLIDX)))
               ENDPOS = POS(ULINE, HKEY(:RTRIM(HKEY)+1), BEGPOS )

C
C              Extract the value, left-justify and RTRIM it. Set
C              "value present" flag to .TRUE.
C
               CLVALS(PCLIDX) = LINE(BEGPOS:ENDPOS)
               CALL LJUST( CLVALS(PCLIDX), CLVALS(PCLIDX) )
               CLVALS(PCLIDX) = CLVALS(PCLIDX)(:RTRIM(CLVALS(PCLIDX)))

               CLFLAG(PCLIDX) = .TRUE.

C
C              Check whether we already parsed the whole line.
C
               IF ( HLINE .NE. ' ' ) THEN

C
C                 We are not at the end of the command line. There is
C                 more stuff to parse and we put this stuff to
C                 the HLINE.
C
                  HLINE = LINE(ENDPOS+1+RTRIM(CLKEYS(CLIDX)):)

               END IF

C
C              Now reset our line and previous index.
C
               LINE  = LINE(ENDPOS+1:)

            END IF

C
C           Save current key index in as previous.
C
            PCLIDX = CLIDX

         END IF

      END DO

C
C     We need to save the last value.
C
      IF ( PCLIDX .NE. 0 ) THEN

         FOUND = .TRUE.
C
C        Save the last value.
C
         CLFLAG(PCLIDX) = .TRUE.

         IF ( RTRIM(LINE) .GT. RTRIM(CLKEYS(PCLIDX)) ) THEN

C
C           Compute begin position of, extract, left justify and
C           RTRIM the last value.
C
            CALL UCASE( LINE, ULINE )
            BEGPOS =
     .         POS(ULINE,CLKEYS(PCLIDX)(:RTRIM(CLKEYS(PCLIDX))), 1)
     .          + RTRIM( CLKEYS(PCLIDX) )

            CLVALS(PCLIDX) = LINE(BEGPOS:)
            CALL LJUST( CLVALS(PCLIDX), CLVALS(PCLIDX) )
            CLVALS(PCLIDX) = CLVALS(PCLIDX)(:RTRIM(CLVALS(PCLIDX)))

         ELSE

C
C           The key is the last thing on the line. So, it's value
C           is blank.
C
            CLVALS(PCLIDX) = ' '

         END IF

      END IF

      CALL CHKOUT ( 'PARCML' )
      RETURN
      END

