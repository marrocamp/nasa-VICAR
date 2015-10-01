C$Procedure ZZBODKER ( Private --- Process Body-Name Kernel Pool Maps )
 
      SUBROUTINE ZZBODKER ( NAMES,
     .                      NORNAM,
     .                      CODES,
     .                      NVALS,
     .                      ORDNOM,
     .                      ORDCOD,
     .                      NOCDS,
     .                      EXTKER  )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This routine processes the kernel pool vectors NAIF_BODY_NAME
C     and NAIF_BODY_CODE into the formatted lists required by ZZBODTRN
C     to successfully compute code-name mappings.
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
C
C$ Keywords
C
C     BODY
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE               'zzbodtrn.inc'
 
      CHARACTER*(MAXL)      NAMES  ( NROOM )
      CHARACTER*(MAXL)      NORNAM ( NROOM )
      INTEGER               CODES  ( NROOM )
      INTEGER               NVALS
      INTEGER               ORDNOM ( NROOM )
      INTEGER               ORDCOD ( NROOM )
      INTEGER               NOCDS
      LOGICAL               EXTKER
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAMES      O   Array of kernel pool assigned names.
C     NORNAM     O   Array of normalized kernel pool assigned names.
C     CODES      O   Array of ID codes for NAMES/NORNAM.
C     NVALS      O   Length of NAMES, NORNAM, CODES, and ORDNOM arrays.
C     ORDNOM     O   Order vector for NORNAM.
C     ORDCOD     O   Modified order vector for CODES.
C     NOCDS      O   Length of ORDCOD array.
C     EXTKER     O   Logical indicating presence of kernel pool names.
C     MAXL       P   Maximum length of body name strings.
C     NROOM      P   Maximum length of kernel pool data vectors.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     NAMES     the array of highest precedent names extracted
C               from the kernel pool vector NAIF_BODY_NAME.  This
C               array is parallel to NORNAM and CODES.
C
C     NORNAM    the array of highest precedent names extracted
C               from the kernel pool vector NAIF_BODY_NAME.  After
C               extraction, each entry is converted to uppercase,
C               and groups of spaces are compressed to a single
C               space.  This represents the canonical member of the
C               equivalence class each parallel entry in NAMES
C               belongs.
C
C     CODES     the array of highest precedent codes extracted
C               from the kernel pool vector NAIF_BODY_CODE.  This
C               array is parallel to NAMES and NORNAM.
C
C     NVALS     the number of items contained in NAMES, NORNAM,
C               CODES and ORDNOM.
C
C     ORDNOM    the order vector of indexes for NORNAM.  The set
C               of values NORNAM( ORDNOM(1) ), NORNAM( ORDNOM(2) ),
C               ... forms an increasing list of name values.
C
C     ORDCOD    the modified ordering vector of indexes into
C               CODES.  The list CODES( ORDCOD(1) ),
C               CODES( ORDCOD(2) ), ... , CODES( ORDCOD(NOCDS) )
C               forms an increasing non-repeating list of integers.
C               Moreover, every value in CODES is listed exactly
C               once in this sequence.
C
C     NOCDS     the number of indexes listed in ORDCOD.  This
C               value will never exceed NVALS.
C
C     EXTKER    is a logical that indicates to the caller whether
C               any kernel pool name-code maps have been defined.
C               If EXTKER is .FALSE., then the kernel pool variables
C               NAIF_BODY_CODE and NAIF_BODY_NAME are empty and
C               only the built-in and ZZBODDEF code-name mappings
C               need consideration.  If .TRUE., then the values
C               returned by this module need consideration.
C
C$ Parameters
C
C     MAXL        is the maximum length of a body name.  Defined in
C                 the include file 'zzbodtrn.inc'.
C
C     NROOM       is the maximum number of kernel pool data items
C                 that can be processed from the NAIF_BODY_CODE
C                 and NAIF_BODY_NAME lists.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) The error SPICE(MISSINGKPV) is signaled when one of the
C        NAIF_BODY_CODE and NAIF_BODY_NAME keywords is present in the
C        kernel pool and the other is not.
C
C     2) The error SPICE(KERVARTOOBIG) is signaled if one or both of
C        the NAIF_BODY_CODE and NAIF_BODY_NAME kernel pool vectors
C        have a cardinality that exceeds NROOM.
C
C     3) The error SPICE(BADDIMENSIONS) is signaled if the cardinality
C        of the NAIF_BODY_CODE and NAIF_BODY_NAME kernel pool vectors do
C        not match.
C
C     4) The error SPICE(BLANKNAMEASSIGNED) is signaled if an entry
C        in the NAIF_BODY_NAME kernel pool vector is a blank string.
C        ID codes may not be assigned to a blank string.
C
C$ Particulars
C
C     This routine examines the contents of the kernel pool, ingests
C     the contents of the NAIF_BODY_CODE and NAIF_BODY_NAME keywords,
C     and produces the order vectors and name/code lists that ZZBODTRN
C     requires to resolve code to name and name to code mappings.
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
C     F.S. Turner    (JPL)
C     E.D. Wright    (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 23-AUG-2002 (EDW) (FST)
C
C-&
 
C
C     SPICELIB Functions
C
      LOGICAL               RETURN
 
C
C     Local Parameters
C
      INTEGER               KEYLEN
      PARAMETER           ( KEYLEN = 32 )
 
C
C     Local Variables
C
      CHARACTER*(KEYLEN)    NBC
      CHARACTER*(KEYLEN)    NBN
      CHARACTER*(1)         TYPE   ( 2 )
 
      INTEGER               I
      INTEGER               J
      INTEGER               NSIZ   ( 2 )
      INTEGER               NUM    ( 2 )
 
      LOGICAL               DROP   ( NROOM )
      LOGICAL               FOUND
      LOGICAL               PLFIND ( 2 )
      LOGICAL               REMDUP
 
C
C     Saved Variables
C
      SAVE                  NBC
      SAVE                  NBN
 
C
C     Data Statements
C
      DATA                  NBC    / 'NAIF_BODY_CODE' /
      DATA                  NBN    / 'NAIF_BODY_NAME' /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZBODKER' )
      END IF
 
C
C     Until the code below proves otherwise, we shall assume
C     we lack kernel pool name/code mappings.
C
      EXTKER = .FALSE.
 
C
C     Check for the external body ID variables in the kernel pool.
C
      CALL GCPOOL ( NBN, 1, NROOM, NUM(1), NAMES, PLFIND(1) )
      CALL GIPOOL ( NBC, 1, NROOM, NUM(2), CODES, PLFIND(2) )
 
C
C     Examine PLFIND(1) and PLFIND(2) for problems.
C
      IF ( PLFIND(1) .NEQV. PLFIND(2) ) THEN
 
C
C        If they are not both present or absent, signal an error.
C
         CALL SETMSG ( 'The kernel pool vector, #, used in '
     .   //            'mapping between names and ID-codes '
     .   //            'is absent, while # is not.  This is '
     .   //            'often due to an improperly constructed '
     .   //            'text kernel.  Check loaded kernels for '
     .   //            'these keywords.'                         )
 
         IF ( PLFIND(1) ) THEN
            CALL ERRCH ( '#', NBC )
            CALL ERRCH ( '#', NBN )
         ELSE
            CALL ERRCH ( '#', NBN )
            CALL ERRCH ( '#', NBC )
         END IF
 
         CALL SIGERR ( 'SPICE(MISSINGKPV)' )
         CALL CHKOUT ( 'ZZBODKER'          )
         RETURN
 
      ELSE IF ( .NOT. PLFIND(1) ) THEN
 
C
C        Return if both keywords are absent.
C
         CALL CHKOUT ( 'ZZBODKER' )
         RETURN
 
      END IF
 
C
C     If we reach here, then both kernel pool variables are present.
C     Perform some simple sanity checks on their lengths.
C
      CALL DTPOOL ( NBN, FOUND, NSIZ(1), TYPE(1) )
      CALL DTPOOL ( NBC, FOUND, NSIZ(2), TYPE(2) )
 
      IF ( ( NSIZ(1) .GT. NROOM ) .OR. ( NSIZ(2) .GT. NROOM ) ) THEN
 
         CALL SETMSG ( 'The kernel pool vectors used to '
     .   //            'define the names/ID-codes mapping'
     .   //            'exceeds the max size. The size of '
     .   //            'the NAME vector is #1. The size of '
     .   //            'the CODE vector is #2. The max '
     .   //            'number allowed of elements is #3.'   )
         CALL ERRINT ( '#1', NSIZ(1)                         )
         CALL ERRINT ( '#2', NSIZ(2)                         )
         CALL ERRINT ( '#3', NROOM                           )
         CALL SIGERR ( 'SPICE(KERVARTOOBIG)'                 )
         CALL CHKOUT ( 'ZZBODKER'                            )
         RETURN
 
      ELSE IF ( NSIZ(1) .NE. NSIZ(2) ) THEN
 
         CALL SETMSG ( 'The kernel pool vectors used for '
     .   //            'mapping between names and ID-codes '
     .   //            'are not the same size.  The size '
     .   //            'of the name vector, NAIF_BODY_NAME '
     .   //            'is #. The size of the ID-code '
     .   //            'vector, NAIF_BODY_CODE is #. You '
     .   //            'need to examine the ID-code kernel '
     .   //            'you loaded and correct the mismatch.' )
         CALL ERRINT ( '#', NSIZ(1)                           )
         CALL ERRINT ( '#', NSIZ(2)                           )
         CALL SIGERR ( 'SPICE(BADDIMENSIONS)'                 )
         CALL CHKOUT ( 'ZZBODKER'                             )
         RETURN
 
      END IF
 
C
C     Compute the canonical member of the equivalence class of NAMES,
C     NORNAM.  This normalization compresses groups of spaces into a
C     single space, left justifies the string, and uppercases the
C     contents.  While passing through the NAMES array, look for any
C     blank strings and signal an appropriate error.
C
      NVALS = NUM(1)
 
      DO I = 1, NVALS
 
C
C        Check for blank strings.
C
         IF ( NAMES(I) .EQ. ' ' ) THEN
 
            CALL SETMSG ( 'An attempt to assign the code, #, to '
     .      //            'a blank string was made.  Check loaded '
     .      //            'text kernels for a blank string in '
     .      //            'the NAIF_BODY_NAME array.'               )
            CALL ERRINT ( '#', I                                    )
            CALL SIGERR ( 'SPICE(BLANKNAMEASSIGNED)'                )
            CALL CHKOUT ( 'ZZBODKER'                                )
            RETURN
 
         END IF
 
C
C        Compute the canonical member of the equivalence class.
C
         CALL LJUST  (         NAMES(I),  NORNAM(I) )
         CALL UCASE  (         NORNAM(I), NORNAM(I) )
         CALL CMPRSS ( ' ', 1, NORNAM(I), NORNAM(I) )
 
      END DO
 
C
C     Determine a preliminary order vector for NORNAM.
C
      CALL ORDERC ( NORNAM, NVALS, ORDNOM )
 
C
C     We are about to remove duplicates.  Make some initial
C     assumptions, no duplicates exist in NORNAM.
C
      DO I = 1, NVALS
         DROP(I) = .FALSE.
      END DO
 
      REMDUP = .FALSE.
 
C
C     ORDERC clusters duplicate entries in NORNAM together.
C     Use this fact to locate duplicates on one pass through
C     NORNAM.
C
      DO I = 1, NVALS - 1
 
         IF ( NORNAM( ORDNOM(I) ) .EQ. NORNAM( ORDNOM(I+1) ) ) THEN
 
C
C           We have at least one duplicate to remove.
C
            REMDUP = .TRUE.
 
C
C           If the normalized entries are equal, drop the one with
C           the lower index in the NAMES array.  Entries defined
C           later in the kernel pool have higher precedence.
C
            IF ( ORDNOM(I) .LT. ORDNOM(I+1) ) THEN
               DROP( ORDNOM(I) )   = .TRUE.
            ELSE
               DROP( ORDNOM(I+1) ) = .TRUE.
            END IF
 
         END IF
 
      END DO
 
C
C     If necessary, remove duplicates.
C
      IF ( REMDUP ) THEN
 
C
C        Sweep through the DROP array, compressing off any elements
C        that are to be dropped.
C
         J = 0
 
         DO I = 1, NVALS
            IF ( .NOT. DROP(I) ) THEN
               J         = J+1
               NAMES (J) = NAMES (I)
               NORNAM(J) = NORNAM(I)
               CODES (J) = CODES (I)
            END IF
         END DO
 
C
C        Adjust NVALS to compensate for the number of elements that
C        were compressed off the list.
C
         NVALS = J
 
      END IF
 
C
C     Compute the order vectors that ZZBODTRN requires.
C
      CALL ZZBODINI ( NAMES,  NORNAM, CODES, NVALS,
     .                ORDNOM, ORDCOD, NOCDS         )
 
C
C     We're on the home stretch if we make it to this point.
C     Set EXTKER to .TRUE., check out and return.
C
      EXTKER = .TRUE.
 
      CALL CHKOUT ( 'ZZBODKER' )
      RETURN
 
      END
