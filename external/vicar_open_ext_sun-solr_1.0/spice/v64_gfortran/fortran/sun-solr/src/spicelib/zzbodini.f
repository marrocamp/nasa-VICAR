C$Procedure ZZBODINI ( Private --- Body-Code Initialization )
 
      SUBROUTINE ZZBODINI (  NAMES,
     .                       NORNAM,
     .                       CODES,
     .                       NVALS,
     .                       ORDNOM,
     .                       ORDCOD,
     .                       NOCDS   )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Initialize the two order vectors. This routine should be called
C     by ZZBODTRN only.
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
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE              'zzbodtrn.inc'
 
      CHARACTER*(MAXL)      NAMES  ( * )
      CHARACTER*(MAXL)      NORNAM ( * )
      INTEGER               CODES  ( * )
      INTEGER               NVALS
      INTEGER               ORDNOM ( * )
      INTEGER               ORDCOD ( * )
      INTEGER               NOCDS
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAMES      I   Array of kernel pool assigned names.
C     NORNAM     I   Array of normalized kernel pool assigned names.
C     CODES      I   Array of ID codes for NAMES/NORNAM.
C     NVALS      I   Length of NAMES, NORNAM, CODES, and ORDNOM arrays.
C     ORDNOM     O   Order vector for NORNAM.
C     ORDCOD     O   Modified order vector for CODES.
C     NOCDS      O   Length of ORDCOD array.
C     MAXL       P   Maximum length of body name strings.
C
C$ Detailed_Input
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
C$ Detailed_Output
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
C               value will never exceed NVALS.C
C
C$ Parameters
C
C     MAXL        is the maximum length of a body name.  Defined in
C                 the include file 'zzbodtrn.inc'.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This is a utility routine used for initializing the ordering
C     vectors that point to the recognized names and codes used by
C     the private routine ZZBODTRN.
C
C$ Examples
C
C     See the routine ZZBODTRN.
C
C$ Restrictions
C
C     1) This routine is intended only for use by ZZBODTRN.
C
C     2) NAMES and NORNAM must contain only unique entries.
C        If duplicate entries exist, this routine may not
C        perform as expected.
C
C     3) This routine relies rather heavily on the implementation of
C        BSCHOI.  The specification of BSCHOI requires an order vector
C        as input, however it turns out that a generalization of an
C        order vector (as defined by this routine) will work as well.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov       (JPL)
C     M.J. Spencer       (JPL)
C     W.L. Taber         (JPL)
C     F.S. Turner        (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.0.0, 23-AUG-2002 (FST)
C
C        Implemented changes to support the new precedence
C        system.
C
C        Altered the calling sequence of ZZBODINI to remove
C        unused arguments.  This routine also no longer computes
C        NORNAM from NAMES, since it is used in a more general
C        capacity.
C
C        Updated module header and comments to document additional
C        assumptions this module now makes about its inputs.
C
C        This routine is now error free.
C
C-    SPICELIB Version 2.1.1, 07-MAR-2002 (EDW)
C
C        Modified error logic to allow duplicate
C        NAME -> CODE mappings without signaling an error.
C        The mapping operation is a no-op, but might
C        cause a user problems if an error signals.
C
C-    SPICELIB Version 2.1.0, 12-AUG-2001 (EDW)
C
C        Modified logic for all ZZBOD routines to function with
C        equivalence class concept. A body name now exists
C        as a member of an equivalence class named by the
C        normalized form of the body name. To facilitate this
C        concept, an addition name vector, NORNAM, and
C        order vector, ORDNOM, now exist.
C
C-    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS) (WLT)
C
C        Renamed to ZZBODINI and filled out the comments on what this
C        routine does and how it works.
C
C-&
 
C
C     Local Variables
C
      INTEGER              I
      INTEGER              N
 
C
C     Create the order vectors ORDCOD and ORDNOM.
C
      CALL ORDERC ( NORNAM, NVALS, ORDNOM )
      CALL ORDERI ( CODES , NVALS, ORDCOD )
 
C
C     Remove duplicate entries in the code order table. The entry that
C     points to the highest entry in CODES should remain.
C
      N = 1
      I = 2
 
C
C     Now for some very funky maneuvering.  We are going to take our
C     order vector for the id-codes and modify it!
C
C     Here's what is true now.
C
C       CODES(ORDCOD(1)) <= CODES(ORDCOD(2)) <=...<= CODES(ORDCOD(NVALS)
C
C     For each element such that CODES(ORDCOD(I)) = CODES(ORDCOD(I+1))
C     we are going to "shift" the items ORDCOD(I+1), ORDCOD(I+2), ...
C     left by one.  We will then repeat the test and shift as needed.
C     When we get done we will have a possibly shorter array ORDCOD
C     and the array will satisfy
C
C       CODES(ORDCOD(1)) < CODES(ORDCOD(2)) < ... < CODES(ORDCOD(NVALS)
C
C     We can still use the resulting "ordered vector" (as opposed to
C     order vector) in the BSCHOI routine because it only relies
C     upon the indexes to ORDCOD and not to CODES itself.  This is
C     making very heavy use of the implementation of BSCHOI but we
C     are going to let it go for the moment because this is a private
C     routine.
C
      DO WHILE ( I .LE. NVALS )
 
         IF ( CODES(ORDCOD(I)) .EQ. CODES(ORDCOD(N)) ) THEN
 
            IF ( ORDCOD(I) .GT. ORDCOD(N) ) THEN
 
               ORDCOD(N) = ORDCOD(I)
 
            END IF
 
         ELSE
 
            N         = N + 1
            ORDCOD(N) = ORDCOD(I)
 
         END IF
 
         I = I + 1
 
      END DO
 
      NOCDS = N
 
      RETURN
      END
