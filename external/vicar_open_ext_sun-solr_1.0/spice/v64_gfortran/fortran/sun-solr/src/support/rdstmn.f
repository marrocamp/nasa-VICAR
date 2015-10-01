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
 
      SUBROUTINE RDSTMN (PRMPT, DELIM, STMT)
      IMPLICIT NONE
 
      CHARACTER*(*)      PRMPT
      CHARACTER*(1)      DELIM
      CHARACTER*(*)      STMT
 
C
C  Read a statement entered on one or more lines.
C
C  VARIABLE      I/O            DESCRIPTION
C   PRMPT        I      Prompt for input. If PRMPT is not blank,
C                          the cursor is positioned one space after the
C                          last non-blank character. Successive lines
C                          are indented by the length of PRMPT.
C   DELIM         I      Statement delimiter. RDSTMN will continue
C                          to read until the either the delimiter or
C                          a blank line is entered.
C   STMT          O      The statement entered, up to but not
C                          including the delimiter. If RDSTMN is
C                          terminated by the entry of a blank line,
C                          STMT is blank.
C
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
 
C
C  7 February 1986, I.M. Underwood
C
C-
 
 
 
C
C     SPICELIB functions
C
      INTEGER               RTRIM
      LOGICAL               BATCH
 
C
C     Local variables
C
      CHARACTER*(1)         SPACE
      CHARACTER*(1)         TAB
      CHARACTER*(132)       BLANK
      SAVE                  BLANK
 
      INTEGER               PRLEN
      CHARACTER*(132)       LINE
      CHARACTER*(132)       MYPRM
      INTEGER               END
 
      DATA                  BLANK / ' ' /
 
 
C
C     Read the first statement. Use the prompt. Return immediately
C     if a blank line or an error is encountered.
C
      IF ( BATCH() ) THEN
         STMT = ' '
         RETURN
      END IF
 
      PRLEN  = RTRIM (PRMPT) + 1
      MYPRM  = PRMPT
      LINE   = ' '
      SPACE  = ' '
      TAB    = CHAR(9)
 
      CALL PROMPT ( MYPRM(1:PRLEN), LINE )
 
      IF ( LINE .EQ. ' ' ) THEN
         STMT = ' '
         RETURN
      ELSE
         STMT = LINE
      END IF
 
C
C     Get rid of any of those nasty old tabs.
C
      CALL REPLCH ( LINE, TAB, SPACE, LINE )
 
 
C
C     Read succeeding lines. Indent to the length of the original
C     prompt. Add the input line to the current statement. Quit when:
C
C            - A delimiter is encountered. (Return the statement
C              up to the delimiter.)
C
C            - A blank line or an error is encountered. (Return
C              a blank statement.)
C
      DO WHILE ( INDEX ( STMT, DELIM ) .EQ. 0 )
 
         CALL PROMPT ( BLANK(1:PRLEN),   LINE )
         CALL REPLCH ( LINE, TAB, SPACE, LINE )
 
         IF ( LINE .EQ. ' ' ) THEN
            STMT = ' '
            RETURN
         ELSE
            CALL SUFFIX ( LINE, 1, STMT )
         END IF
 
      END DO
 
C
C     If we made it to here, we encountered a delimiter. Take the
C     entire statement up to the character before the delimiter.
C
      END        = INDEX ( STMT, DELIM )
      STMT(END:) = ' '
 
      RETURN
      END
