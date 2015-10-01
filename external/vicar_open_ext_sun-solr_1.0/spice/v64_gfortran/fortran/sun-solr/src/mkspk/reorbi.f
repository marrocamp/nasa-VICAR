C$Procedure      REORBI ( Reorder a blocks of double precisions )
 
      SUBROUTINE REORBI ( ORDVEC, N, BSIZE, DATA )
 
C$ Abstract
C
C     Reorder the fixed size contiguous blocks of data in an
C     array of integers as specified by an  order vector
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
C       UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
      INTEGER               ORDVEC ( * )
      INTEGER               N
      INTEGER               BSIZE
      INTEGER               DATA   ( * )
 
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      ORDVEC     I   is an order vector
C      N          I   is the number of order vector elements
C      BSIZE      I   is the size of the blocks in DATA
C      DATA      I/O  array of contiguous fixed size blocks of integers
C
C$ Detailed_Input
C
C     ORDVEC      is an order vector that tells how the blocks
C                 of data should be re-arranged. ORDVEC might
C                 be produced by calling ORDERI with inputs of
C                 integer keys.
C
C     N           is the number of values in the order vector and the
C                 number of contiguous blocks in the array DATA.
C
C     BSIZE       is the size of each block of data in DATA
C
C     DATA        is an array containing N*BSIZE integers
C                 The data is regarded as being arranged
C                 in blocks of data as shown below:
C
C                 Block 1       integer (1,     1 )
C                               integer (2,     1 )
C                               integer (3,     1 )
C                                 .
C                                 .
C                                 .
C                               integer (BSIZE, 1 )
C
C                 Block 2       integer (1,     2 )
C                               integer (2,     2 )
C                               integer (3,     2 )
C                                 .
C                                 .
C                                 .
C                               integer (BSIZE, 2 )
C
C                 Block 3       integer (1,     3 )
C
C                        etc.
C
C                 The goal in calling this routine is to move the
C                 blocks around in the array while keeping the
C                 data items in each so that the blocks are in
C                 the order prescribed by the order vector.
C
C
C$ Detailed_Output
C
C     DATA        is the input array after the blocks have been move
C                 into the order prescribed by the order vector.
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
C     This routine rearranges blocks of data in a double precision
C     array in the same way that REORDD rearranges individual
C     elements of a double precision array.
C
C$ Examples
C
C     Suppose that you have a collection of line-pixel locations and an
C     array of corresponding names of the objects at those locations
C     as shown below.
C
C        NAMES(1)         LINPIX( 1 )
C                         LINPIX( 2 )
C
C        NAMES(2)         LINPIX( 3 )
C                         LINPIX( 4 )
C
C        NAMES(3)         LINPIX( 5 )
C                         LINPIX( 6 )
C
C        NAMES(4)         LINPIX( 7 )
C                         LINPIX( 8 )
C
C                            .
C                            .
C                            .
C
C     But that the names are not in increasing order. To arrange the
C     names and line pixel pairs so that the names are in order and the
C     relationship between names index and line/pixesl indices are
C     maintained you could perform the following sequence of
C     subroutine calls.
C
C        CALL ORDERC ( NAMES,  N,    ORDVEC )
C        CALL REORDC ( ORDVEC, N,    NAMES  )
C        CALL REORBI ( ORDVEC, N, 6, LINPIX )
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
C-    SPICELIB Version 1.0.0, 28-JUN-1994 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Reorder a integer array by blocks.
C     Reorder the blocks of a integer array.
C
C-&
 
C
C     Local variables
C
      INTEGER               START
      INTEGER               INDEX
      INTEGER               HOLD
      INTEGER               I
 
      INTEGER               TEMP
 
 
C
C     If the array doesn't have at least two elements, don't bother.
C
      IF ( N .LT. 2 ) THEN
           RETURN
      END IF
 
C
C     START is the position in the order vector that begins the
C     current cycle. When all the switches have been made, START
C     will point to the end of the order vector.
C
      START = 1
 
      DO WHILE ( START .LT. N )
 
C
C        Begin with the element of input vector specified by
C        ORDVEC(START). Move it to the correct position in the
C        array, after saving the element it replaces to TEMP.
C        HOLD indicates the position of the array element to
C        be moved to its new position. After the element has
C        been moved, HOLD indicates the position of an available
C        space within the array.  Note we need to do this for
C        each element of the block.  We do the first BSIZE-1 items
C        first and do the last item of the block separately so
C        that we can "tag" the elements of the order vector to
C        indicate we've touched them.
C
         DO I = 1, BSIZE-1
 
            INDEX = START
            TEMP  = DATA (BSIZE*(INDEX-1) + I )
            HOLD  = ORDVEC(INDEX)
 
 
C
C           Keep going until HOLD points to the first array element
C           moved during the current cycle. This ends the cycle.
C
            DO WHILE ( HOLD .NE. START )
               DATA (BSIZE*(INDEX-1) + I )  =  DATA (BSIZE*(HOLD-1)+I)
               INDEX                        =  HOLD
               HOLD                         =  ORDVEC(HOLD)
            END DO
 
C
C           The last element in the cycle is restored from TEMP.
C
            DATA (BSIZE*(INDEX-1)+I) =  TEMP
 
         END DO
 
C
C        Now for the last item of the block:
C
C        As each slot in the output array is filled in, the sign
C        of the corresponding element in the order vector is changed
C        from positive to negative. This way, we know which elements
C        have already been ordered when looking for the beginning of
C        the next cycle.
C
 
         INDEX = START
         TEMP  = DATA (BSIZE*INDEX)
         HOLD  = ORDVEC(INDEX)
 
         DO WHILE ( HOLD .NE. START )
            DATA (INDEX*BSIZE)  =  DATA (HOLD*BSIZE)
            INDEX               =  HOLD
            HOLD                =  ORDVEC(HOLD)
            ORDVEC(INDEX)       = -ORDVEC(INDEX)
         END DO
 
C
C        The last element in the cycle is restored from TEMP.
C
         DATA (INDEX*BSIZE) =  TEMP
         ORDVEC(HOLD)       = -ORDVEC(HOLD)
 
 
C
C        Begin the next cycle at the next element in the order
C        vector with a positive sign. (That is, the next one
C        that hasn't been moved.)
C
         DO WHILE ( ORDVEC(START) .LT. 0  .AND.  START .LT. N )
            START = START + 1
         END DO
 
      END DO
 
C
C     Restore the original signs of the elements of the order vector,
C     in case the vector is to be used again with another array.
C
      DO INDEX = 1, N
        ORDVEC(INDEX) = IABS ( ORDVEC(INDEX) )
      END DO
 
      RETURN
      END
