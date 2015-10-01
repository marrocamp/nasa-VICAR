C$Procedure      DASA2L ( DAS, address to physical location )
 
      SUBROUTINE DASA2L (  HANDLE,  TYPE,    ADDRSS,
     .                     CLBASE,  CLSIZE,  RECNO,  WORDNO  )
 
C$ Abstract
C
C     Map a DAS address to a physical location in the DAS file
C     it refers to.
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
C     DAS
C
C$ Keywords
C
C     DAS
C     FILES
C     TRANSFORMATION
C     UTILITY
C
C$ Declarations
 
 
      INTEGER               HANDLE
      INTEGER               TYPE
      INTEGER               ADDRSS
      INTEGER               CLBASE
      INTEGER               CLSIZE
      INTEGER               RECNO
      INTEGER               WORDNO
 
      INTEGER               CHAR
      PARAMETER           ( CHAR   =  1  )
 
      INTEGER               DP
      PARAMETER           ( DP     =  2  )
 
      INTEGER               INT
      PARAMETER           ( INT    =  3  )
 
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   DAS file handle.
C     TYPE       I   Data type specifier.
C     ADDRSS     I   DAS address of a word of data type TYPE.
C     CLBASE,
C     CLSIZE     O   Cluster base record number and size.
C     RECNO,
C     WORDNO     O   Record/word pair corresponding to ADDRSS.
C     CHAR       P   Parameter indicating character data type.
C     DP         P   Parameter indicating double precision data type.
C     INT        P   Parameter indicating integer data type.
C
C$ Detailed_Input
C
C     HANDLE         is the file handle of an open DAS file.
C
C     TYPE           is a data type specifier.  TYPE may be any of
C                    the parameters
C
C                       CHAR
C                       DP
C                       INT
C
C                    which indicate `character', `double precision',
C                    and `integer' respectively.
C
C
C     ADDRSS         is the address in a DAS of a word of data
C                    type TYPE.  For each data type (double precision,
C                    integer, or character), addresses range
C                    from 1 to the maximum current value for that type,
C                    which is available from DAFRFR.
C
C$ Detailed_Output
C
C     CLBASE,
C     CLSIZE         are, respectively, the base record number and
C                    size, in records, of the cluster containing the
C                    word corresponding to ADDRSS.  The cluster spans
C                    records numbered CLBASE through CLBASE +
C                    CLSIZE - 1.
C
C     RECNO,
C     WORD           are, respectively, the number of the physical
C                    record and the number of the word within the
C                    record that correspond to ADDRSS.  Word numbers
C                    start at 1 and go up to NC, ND, or NI in
C                    character, double precision, or integer records
C                    respectively.
C
C$ Parameters
C
C     CHAR,
C     DP,
C     INT            are data type specifiers which indicate
C                    `character', `double precision', and `integer'
C                    respectively.  These parameters are used in
C                    all DAS routines that require a data type
C                    specifier as input.
C
C$ Exceptions
C
C     1)  If TYPE is not recognized, the error SPICE(DASINVALIDTYPE)
C         will be signalled.
C
C     2)  ADDRSS must be between 1 and LAST inclusive, where LAST
C         is last address in the DAS for a word of the specified
C         type.  If ADDRSS is out of range, the error
C         SPICE(DASNOSUCHADDRESS) will be signalled.
C
C     3)  If this routine fails to find directory information for
C         the input address, the error SPICE(NOSUCHRECORD) will be
C         signalled.
C
C     4)  If the input handle is invalid, the error will be diagnosed
C         by routines called by this routine.
C
C
C     If any of the above exceptions occur, the output arguments may
C     contain bogus information.
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     The DAS architecture allows a programmer to think of the data
C     within a DAS file as three one-dimensional arrays:  one of
C     double precision numbers, one of integers, and one of characters.
C     This model allows a programmer to ask the DAS system for the
C     `nth double precision number (or integer, or character) in the
C     file'.
C
C     DAS files are Fortran direct access files, so to find the
C     `nth double precision number', you must have the number of the
C     record containing it and the `word number', or position, within
C     the record of the double precision number.  This routine finds
C     the record/word number pair that specify the physical location
C     in a DAS file corresponding to a DAS address.
C
C     As opposed to DAFs, the mapping of addresses to physical locations
C     for a DAS file depends on the organization of data in the file.
C     Given a fixed set of DAS format parameters, the physical location
C     of the nth double precision number can depend on how many integer
C     and character records have been written prior to the record
C     containing that double precision number.
C
C     The cluster information output from this routine allows the
C     caller to substantially reduce the number of directory reads
C     required to read a from range of addresses that spans
C     multiple physical records; the reading program only need call
C     this routine once per cluster read, rather than once per
C     physical record read.
C
C$ Examples
C
C     1)  Use this routine to read integers from a range of
C         addresses.  This is done in the routine DASRDI.
C
C            C
C            C     Decide how many integers to read.
C            C
C                  NUMINT = LAST - FIRST + 1
C                  NREAD  = 0
C
C            C
C            C     Find out the physical location of the first
C            C     integer.  If FIRST is invalid, DASA2L will take care
C            C     of the problem.
C            C
C
C                  CALL DASA2L (  HANDLE,  INT,     FIRST,
C                 .               CLBASE,  CLSIZE,  RECNO,  WORDNO  )
C
C            C
C            C     Read as much data from record RECNO as necessary.
C            C
C                  N  =  MIN ( NUMINT,  NWI - WORDNO + 1 )
C
C                  CALL DASRRI ( HANDLE, RECNO, WORDNO, WORDNO + N-1,
C                 .              DATA                                 )
C
C                  NREAD  =  N
C                  RECNO  =  RECNO + 1
C
C            C
C            C     Read from as many additional records as necessary.
C            C
C                  DO WHILE ( NREAD .LT. NUMINT )
C            C
C            C        At this point, RECNO is the correct number of the
C            C        record to read from next.  CLBASE is the number
C            C        of the first record of the cluster we're about
C            C        to read from.
C            C
C
C                     IF (  RECNO  .LT.  ( CLBASE + CLSIZE )  ) THEN
C            C
C            C           We can continue reading from the current
C            C           cluster.
C            C
C                        N  =  MIN ( NUMINT - NREAD,  NWI )
C
C                        CALL DASRRI (  HANDLE,
C                 .                     RECNO,
C                 .                     1,
C                 .                     N,
C                 .                     DATA ( NREAD + 1 )   )
C
C                        NREAD   =   NREAD + N
C                        RECNO   =   RECNO + 1
C
C
C                     ELSE
C            C
C            C           We must find the next integer cluster to
C            C           read from.  The first integer in this
C            C           cluster has address FIRST + NREAD.
C            C
C                        CALL DASA2L ( HANDLE,
C                 .                    INT,
C                 .                    FIRST + NREAD,
C                 .                    CLBASE,
C                 .                    CLSIZE,
C                 .                    RECNO,
C                 .                    WORDNO  )
C
C                     END IF
C
C                  END DO
C
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
C     K.R. Gehringer (JPL)
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.1 20-NOV-2001 (NJB)
C
C        Comment fix:  diagram showing directory record pointers
C        incorrectly showed element 2 of the record as a backward
C        pointer.  The element is actually a forward pointer.
C
C-    SPICELIB Version 1.2.0 03-JUL-1996 (NJB)
C
C        Bug fix:  calculation to determine whether file is segregated
C        has been fixed.
C
C-    SPICELIB Version 1.1.1 19-DEC-1995 (NJB)
C
C        Corrected title of permuted index entry section.
C
C-    SPICELIB Version 1.1.0, 03-NOV-1995 (NJB)
C
C        Re-written to optimize address calculations for segregated,
C        read-only files.
C
C-    SPICELIB Version 1.0.1, 26-OCT-1993 (KRG)
C
C        Fixed a typo in the $ Brief_I/O section of the header.
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     map DAS logical address to physical location
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.2.0 03-JUL-1996 (NJB)
C
C        Bug fix:  calculation to determine whether file is segregated
C        has been fixed.  An incorrect variable name used in a bound
C        calculation resulted in an incorrect determination of whether
C        a file was segregated, and caused arithmetic overflow for
C        files with large maximum addresses.  
C
C        In the previous version, the number of DAS words in a cluster
C        was incorrectly calculated as the product of the maximum
C        address of the cluster's data type and the number of words of
C        that data type in a DAS record.  The correct product involves
C        the number of records in the cluster and the number of words of
C        that data type in a DAS record.
C
C-    SPICELIB Version 1.1.0, 03-NOV-1995 (NJB)
C
C        Re-written to optimize address calculations for segregated,
C        read-only files.
C
C-    SPICELIB Version 1.0.1, 26-OCT-1993 (KRG)
C
C        Fixed a typo in the $ Brief_I/O section of the header.
C
C        Removed references to specific DAS file open routines in the
C        $ Detailed_Input section of the header. This was done in order
C        to minimize documentation changes if the DAS open routines ever
C        change.
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT)
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               ISRCHI
 
C
C     Local parameters
C
 
C
C     Words per data record, for each data type:
C
      INTEGER               NWC
      PARAMETER           ( NWC = 1024 )
 
      INTEGER               NWD
      PARAMETER           ( NWD =  128 )
 
      INTEGER               NWI
      PARAMETER           ( NWI =  256 )
 
C
C     Directory pointer locations
C
      INTEGER               BWDLOC
      PARAMETER           ( BWDLOC = 1 )
 
      INTEGER               FWDLOC
      PARAMETER           ( FWDLOC = 2 )
 
C
C     Directory address range locations
C
      INTEGER               CHRRNG
      PARAMETER           ( CHRRNG =          3 )
 
      INTEGER               DPRNG
      PARAMETER           ( DPRNG  = CHRRNG + 2 )
 
      INTEGER               INTRNG
      PARAMETER           ( INTRNG = DPRNG  + 2 )
 
C
C     Indices of lowest and highest addresses in a `range array':
C
      INTEGER               LOW
      PARAMETER           ( LOW  = 1 )
 
      INTEGER               HIGH
      PARAMETER           ( HIGH = 2 )
 
C
C     Location of first type descriptor
C
      INTEGER               BEGDSC
      PARAMETER           ( BEGDSC = 9 )
 
C
C     Access word length
C
      INTEGER               ACCLEN
      PARAMETER           ( ACCLEN = 10 )
 
C
C     File table size
C
      INTEGER               MAXFIL
      PARAMETER           ( MAXFIL = 20 )
 
C
C     Local variables
C
      CHARACTER*(ACCLEN)    ACCESS
 
      INTEGER               CURTYP
      INTEGER               DIRREC ( NWI )
      INTEGER               DSCLOC
      INTEGER               FIDX
      INTEGER               FREE
      INTEGER               HIADDR
      INTEGER               I
      INTEGER               LSTREC ( 3 )
      INTEGER               LSTWRD ( 3 )
      INTEGER               MXADDR
      INTEGER               MXCLRC
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NDIRS
      INTEGER               NEXT   ( 3 )
      INTEGER               NFILES
      INTEGER               NREC
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               NTYPES
      INTEGER               NW     ( 3 )
      INTEGER               NXTREC
      INTEGER               PREV   ( 3 )
      INTEGER               PRVHAN
      INTEGER               PRVTYP
      INTEGER               RANGE  ( 2 )
      INTEGER               RNGLOC ( 3 )
      INTEGER               TBBASE ( 3, MAXFIL )
      INTEGER               TBHAN  (    MAXFIL )
      INTEGER               TBMXAD ( 3, MAXFIL )
      INTEGER               TBSIZE ( 3, MAXFIL )
      INTEGER               UNIT
 
      LOGICAL               FAST
      LOGICAL               FIRST
      LOGICAL               KNOWN
      LOGICAL               RDONLY
      LOGICAL               TBFAST ( MAXFIL )
      LOGICAL               SAMFIL
 
 
 
C
C     Saved variables
C
      SAVE
 
C
C     Initial values
C
C
C     NEXT and PREV map the DAS data type codes to their
C     successors and predecessors, respectively.
C
      DATA                  NEXT   /  2,   3,   1   /
      DATA                  PREV   /  3,   1,   2   /
 
      DATA                  NW     /  NWC,    NWD,   NWI    /
      DATA                  RNGLOC /  CHRRNG, DPRNG, INTRNG /
 
      DATA                  FIRST  / .TRUE. /
 
      DATA                  NFILES / 0 /
 
C
C     Discovery check-in is used in this routine.
C
C
C     DAS files have the following general structure:
C
C           +------------------------+
C           |      file record       |
C           +------------------------+
C           |    reserved records    |
C           |                        |
C           +------------------------+
C           |     comment records    |
C           |                        |
C           |                        |
C           |                        |
C           +------------------------+
C           | first data directory   |
C           +------------------------+
C           |      data records      |
C           |                        |
C           |                        |
C           |                        |
C           |                        |
C           +------------------------+
C                       .
C                       .
C           +------------------------+
C           | last data directory    |
C           +------------------------+
C           |     data records       |
C           |                        |
C           |                        |
C           +------------------------+
C
C
C        Within each DAS data record, word numbers start at one and
C        increase up to NWI, NWD, or NWC:  the number of words in an
C        integer, double precision, or character data record.
C
C
C           +--------------------------------+
C           |       |       |   ...  |       |
C           +--------------------------------+
C               1      2                NWD
C
C           +--------------------------------+
C           |   |   |       ...          |   |
C           +--------------------------------+
C             1   2                       NWI
C
C           +------------------------------------+
C           | | |           ...                | |
C           +------------------------------------+
C            1 2                               NWC
C
C
C        Directories are single records that describe the data
C        types of data records that follow.  The directories
C        in a DAS file form a doubly linked list:  each directory
C        contains forward and backward pointers to the next and
C        previous directories.
C
C        Each directory also contains, for each data type, the lowest
C        and highest logical address occurring in any of the records
C        described by the directory.
C
C        Following the pointers and address range information is
C        a sequence of data type descriptors.  These descriptors
C        indicate the data type of data records following the
C        directory record.  Each descriptor gives the data type
C        of a maximal set of contiguous data records, all having the
C        same type.  By `maximal set' we mean that no data records of
C        the same type bound the set of records in question.
C
C        Pictorially, the structure of a directory is as follows:
C
C           +----------------------------------------------------+
C           | <pointers> | <address ranges> | <type descriptors> |
C           +----------------------------------------------------+
C
C        where the <pointers> section looks like
C
C           +-----------------------------------------+
C           | <backward pointer> | <forward pointer>  |
C           +-----------------------------------------+
C
C        the <address ranges> section looks like
C
C           +-------------------------------------------+
C           | <char range> | <d.p. range> | <int range> |
C           +-------------------------------------------+
C
C        and each range looks like one of:
C
C           +------------------------------------------------+
C           | <lowest char address> | <highest char address> |
C           +------------------------------------------------+
C
C           +------------------------------------------------+
C           | <lowest d.p. address> | <highest d.p. address> |
C           +------------------------------------------------+
C
C           +------------------------------------------------+
C           | <lowest int address>  | <highest int address>  |
C           +------------------------------------------------+
C
C        The type descriptors implement a run-length encoding
C        scheme.  The first element of the series of descriptors
C        occupies two integers:  it contains a type code and a count.
C        The rest of the descriptors are just signed counts; the data
C        types of the records they describe are deduced from the sign
C        of the count and the data type of the previous descriptor.
C        The method of finding the data type for a given descriptor
C        in terms of its predecessor is as follows:  if the sign of a
C        descriptor is positive, the type of that descriptor is the
C        successor of the type of the preceding descriptor in the
C        sequence of types below.  If the sign of a descriptor is
C        negative, the type of the descriptor is the predecessor of the
C        type of the preceding descriptor.
C
C           C  -->  D  -->  I  -->  C
C
C        For example, if the preceding type is `I', and a descriptor
C        contains the number 16, the type of the descriptor is `C',
C        whereas if the descriptor contained the number -800, the type
C        of the descriptor would be `D'.
C
 
 
C
C     Make sure the data type is valid.
C
      IF (  ( TYPE .LT. CHAR ) .OR. ( TYPE .GT. INT )  ) THEN
 
         CALL CHKIN  ( 'DASA2L'                            )
         CALL DASHLU (  HANDLE, UNIT                       )
         CALL SETMSG ( 'Invalid data type: #.  File was #' )
         CALL ERRINT ( '#',  TYPE                          )
         CALL ERRFNM ( '#',  UNIT                          )
         CALL SIGERR ( 'SPICE(DASINVALIDTYPE)'             )
         CALL CHKOUT ( 'DASA2L'                            )
         RETURN
 
      END IF
 
 
C
C     Decide whether we're looking at the same file as we did on
C     the last call.
C
      IF ( FIRST ) THEN
 
         SAMFIL = .FALSE.
         FAST   = .FALSE.
         PRVHAN =  HANDLE
         FIRST  = .FALSE.
 
      ELSE
 
         SAMFIL =  HANDLE .EQ. PRVHAN
         PRVHAN =  HANDLE
 
      END IF
 
C
C     We have a special case if we're looking at a `fast' file
C     that we saw on the last call.  When we say a file is fast,
C     we're implying that it's open for read access only and that it's
C     segregated.  In this case, we can do an address calculation
C     without looking up any information from the file.
C
      IF ( SAMFIL .AND. FAST ) THEN
 
         CLBASE  =  TBBASE ( TYPE, FIDX )
         CLSIZE  =  TBSIZE ( TYPE, FIDX )
         MXADDR  =  TBMXAD ( TYPE, FIDX )
         HIADDR  =  CLSIZE * NW(TYPE)
 
C
C        Make sure that ADDRSS points to an existing location.
C
         IF (  ( ADDRSS .LT. 1 ) .OR. ( ADDRSS .GT. MXADDR )  ) THEN
 
            CALL CHKIN  ( 'DASA2L'                                     )
            CALL DASHLU (  HANDLE, UNIT                                )
            CALL SETMSG ( 'ADDRSS was #; valid range for type # is '  //
     .                    '# to #.  File was #'                        )
            CALL ERRINT ( '#',  ADDRSS                                 )
            CALL ERRINT ( '#',  TYPE                                   )
            CALL ERRINT ( '#',  1                                      )
            CALL ERRINT ( '#',  MXADDR                                 )
            CALL ERRFNM ( '#',  UNIT                                   )
            CALL SIGERR ( 'SPICE(DASNOSUCHADDRESS)'                    )
            CALL CHKOUT ( 'DASA2L'                                     )
            RETURN
 
         END IF
 
 
      ELSE
C
C        If the current file is not the same one we looked at on the
C        last call, find out whether the file is on record in our file
C        table.  Add the file to the table if necessary.  Bump the
C        oldest file in the table if there's no room.
C
         IF ( .NOT. SAMFIL ) THEN
 
            FIDX  =  ISRCHI ( HANDLE, NFILES, TBHAN )
            KNOWN =  FIDX .GT. 0
 
            IF ( KNOWN ) THEN
C
C              The file is in our list.
C
               FAST = TBFAST(FIDX)
 
               IF ( FAST ) THEN
C
C                 This is a segregated, read-only file.  Look up the
C                 saved information we'll need to calculate addresses.
C
                  CLBASE = TBBASE( TYPE, FIDX )
                  CLSIZE = TBSIZE( TYPE, FIDX )
                  MXADDR = TBMXAD( TYPE, FIDX )
                  HIADDR = CLSIZE * NW(TYPE)
 
C
C                 Make sure that ADDRSS points to an existing location.
C
                  IF (     ( ADDRSS .LT. 1      )
     .                .OR. ( ADDRSS .GT. MXADDR )  ) THEN
 
                     CALL CHKIN  ( 'DASA2L'                          )
                     CALL DASHLU (  HANDLE, UNIT                     )
                     CALL SETMSG ( 'ADDRSS was #; valid range for ' //
     .                             ' type # is # to #.  File was #'  )
                     CALL ERRINT ( '#',  ADDRSS                      )
                     CALL ERRINT ( '#',  TYPE                        )
                     CALL ERRINT ( '#',  1                           )
                     CALL ERRINT ( '#',  MXADDR                      )
                     CALL ERRFNM ( '#',  UNIT                        )
                     CALL SIGERR ( 'SPICE(DASNOSUCHADDRESS)'         )
                     CALL CHKOUT ( 'DASA2L'                          )
                     RETURN
 
                  END IF
 
               END IF
C
C              FAST is set.
C
 
            END IF
C
C           KNOWN is set.
C
 
         END IF
 
C
C        SAMFIL, FAST, and KNOWN are set.  If the file is the same one
C        we saw on the last call, the state variables FAST, and KNOWN
C        retain their values from the previous call.
C
C        FIDX is set at this point only if we're looking at a known
C        file.
C
C        Unless the file is recognized and known to be a fast file, we
C        look up all metadata for the file.
C
         IF ( .NOT. ( KNOWN .AND. FAST )  ) THEN
 
 
            IF ( .NOT. KNOWN ) THEN
C
C              This file is not in our list.  If the list is not full,
C              append the file to the list.  If the list is full,
C              replace the oldest (first) file with this one.
C
               IF ( NFILES .LT. MAXFIL ) THEN
                  NFILES  =  NFILES + 1
                  FIDX    =  NFILES
               ELSE
                  FIDX    =  1
               END IF
 
 
               TBHAN(FIDX)  =  HANDLE
 
C
C              Find out whether the file is open for read or write
C              access.  We consider the file to be `slow' until we find
C              out otherwise.  The contents of the arrays TBHIGH,
C              TBBASE, TBSIZE, and TBMXAD are left undefined for slow
C              files.
C
               CALL DASHAM ( HANDLE, ACCESS )
 
               RDONLY        =  ACCESS .EQ. 'READ'
               FAST          =  .FALSE.
               TBFAST(FIDX)  =  FAST
 
C
C              We'll set the flag KNOWN at the end of the outer IF
C              block.
C
            ELSE
C
C              We set RDONLY to .FALSE. for any known file that is
C              not fast.  It's actually possible for a read-only file
C              to be unsegregated, but this is expected to be a rare
C              case, one that's not worth complicating this routine
C              further for.
C
               RDONLY = .FALSE.
 
            END IF
 
C
C           RDONLY is set.
C
C           FIDX is now set whether or not the current file is known.
C
C           Get the number of reserved records, comment records, and
C           the current last address of the data type TYPE from the
C           file  summary.
C
            CALL DASHFS ( HANDLE,
     .                    NRESVR,
     .                    NRESVC,
     .                    NCOMR,
     .                    NCOMC,
     .                    FREE,
     .                    TBMXAD(1,FIDX),
     .                    LSTREC,
     .                    LSTWRD )
 
            MXADDR = TBMXAD( TYPE, FIDX )
 
C
C           Make sure that ADDRSS points to an existing location.
C
            IF (  ( ADDRSS .LT. 1 ) .OR. ( ADDRSS .GT. MXADDR )  ) THEN
 
               CALL CHKIN  ( 'DASA2L'                          )
               CALL DASHLU (  HANDLE, UNIT                     )
               CALL SETMSG ( 'ADDRSS was #; valid range for ' //
     .                       ' type # is # to #.  File was #'  )
               CALL ERRINT ( '#',  ADDRSS                      )
               CALL ERRINT ( '#',  TYPE                        )
               CALL ERRINT ( '#',  1                           )
               CALL ERRINT ( '#',  MXADDR                      )
               CALL ERRFNM ( '#',  UNIT                        )
               CALL SIGERR ( 'SPICE(DASNOSUCHADDRESS)'         )
               CALL CHKOUT ( 'DASA2L'                          )
               RETURN
 
            END IF
 
C
C           Find out which directory describes the cluster containing
C           this word.  To do this, we must traverse the directory
C           list.  The first directory record comes right after the
C           last comment record.  (Don't forget the file record when
C           counting the predecessors of the directory record.)
C
C           Note that we don't need to worry about not finding a
C           directory record that contains the address we're looking
C           for, since we've already checked that the address is in
C           range.
C
C           Keep track of the number of directory records we see.  We'll
C           use this later to determine whether we've got a segregated
C           file.
C
            NREC  =  NRESVR + NCOMR + 2
            NDIRS =  1
 
            CALL DASRRI ( HANDLE,
     .                    NREC,
     .                    RNGLOC(TYPE),
     .                    RNGLOC(TYPE)+1,
     .                    RANGE )
 
 
            DO WHILE ( RANGE(HIGH) .LT. ADDRSS )
C
C              The record number of the next directory is the forward
C              pointer in the current directory record.  Update NREC
C              with this pointer.  Get the address range for the
C              specified type covered by this next directory record.
C
               CALL DASRRI ( HANDLE, NREC, FWDLOC, FWDLOC, NXTREC )
 
               NREC  = NXTREC
               NDIRS = NDIRS + 1
 
               CALL DASRRI ( HANDLE,
     .                       NREC,
     .                       RNGLOC(TYPE),
     .                       RNGLOC(TYPE)+1,
     .                       RANGE           )
 
            END DO
 
C
C           NREC is now the record number of the directory that contains
C           the type descriptor for the address we're looking for.
C
C           Our next task is to find the descriptor for the cluster
C           containing the input address.  To do this, we must examine
C           the directory record in `left-to-right' order.  As we do so,
C           we'll keep track of the highest address of type TYPE
C           occurring in the clusters whose descriptors we've seen.
C           The variable HIADDR will contain this address.
C
            CALL DASRRI ( HANDLE, NREC, 1, NWI, DIRREC )
 
C
C           In the process of finding the physical location
C           corresponding to ADDRSS, we'll find the record number of the
C           base of the cluster containing ADDRSS.  We'll start out by
C           initializing this value with the number of the first data
C           record of the next cluster.
C
            CLBASE  =  NREC + 1
 
C
C           We'll initialize HIADDR with the value preceding the lowest
C           address of type TYPE described by the current directory.
C
            HIADDR  =  DIRREC(RNGLOC(TYPE)) - 1
 
C
C           Initialize the number of records described by the last seen
C           type descriptor.  This number, when added to CLBASE, should
C           yield the number of the first record of the current cluster;
C           that's why it's initialized to 0.
C
            CLSIZE  =  0
 
C
C           Now find the descriptor for the cluster containing ADDRSS.
C           Read descriptors until we get to the one that describes the
C           record containing ADDRSS.  Keep track of descriptor data
C           types as we go.  Also count the descriptors.
C
C           At this point, HIADDR is less than ADDRSS, so the loop will
C           always be executed at least once.
C
            PRVTYP  =  PREV ( DIRREC(BEGDSC) )
            DSCLOC  =  BEGDSC + 1
 
            DO WHILE ( HIADDR .LT. ADDRSS )
C
C              Update CLBASE so that it is the record number of the
C              first record of the current cluster.
C
               CLBASE = CLBASE + CLSIZE
 
C
C              Find the type of the current descriptor.
C
               IF ( DIRREC(DSCLOC) .GT. 0 ) THEN
                  CURTYP  =   NEXT( PRVTYP )
               ELSE
                  CURTYP  =   PREV( PRVTYP )
               END IF
 
C
C              Forgetting to update PRVTYP is a Very Bad Thing (VBT).
C
               PRVTYP = CURTYP
 
C
C              If the current descriptor is of the type we're interested
C              in, update the highest address count.
C
               IF ( CURTYP .EQ. TYPE ) THEN
                  HIADDR = HIADDR + ( NW(TYPE) * ABS( DIRREC(DSCLOC) ) )
               END IF
 
C
C              Compute the number of records described by the current
C              descriptor.  Update the descriptor location.
C
               CLSIZE  =  ABS (  DIRREC( DSCLOC )  )
               DSCLOC  =  DSCLOC + 1
 
            END DO
 
C
C           If we have an unknown read-only file, see whether the file
C           is segregated.  If it is, we'll be able to compute
C           addresses much faster for subsequent reads to this file.
C
            IF (  RDONLY  .AND.  ( .NOT. KNOWN )  ) THEN
 
               IF ( NDIRS .EQ. 1 ) THEN
C
C                 If this file is segregated, there are at most three
C                 cluster descriptors, and each one points to a cluster
C                 containing all records of the corresponding data type.
C                 For each data type having a non-zero maximum address,
C                 the size of the corresponding cluster must be large
C                 enough to hold all addresses of that type.
C
                  NTYPES = 0
 
                  DO I = 1, 3
 
                     IF ( TBMXAD(I,FIDX) .GT. 0 ) THEN
                        NTYPES = NTYPES + 1
                     END IF
 
                  END DO
 
C
C                 Now look at the first NTYPES cluster descriptors,
C                 collecting cluster bases and sizes as we go.
C
                  MXCLRC  =  NREC   + 1
                  PRVTYP  =  PREV ( DIRREC(BEGDSC) )
                  DSCLOC  =  BEGDSC + 1
                  FAST    = .TRUE.
 
                  DO WHILE (       ( DSCLOC .LE. (BEGDSC+NTYPES) )
     .                       .AND.   FAST                            )
C
C                    Find the type of the current descriptor.
C
                     IF ( DIRREC(DSCLOC) .GT. 0 ) THEN
                        CURTYP  =   NEXT( PRVTYP )
                     ELSE
                        CURTYP  =   PREV( PRVTYP )
                     END IF
 
                     PRVTYP                 =  CURTYP
                     TBBASE( CURTYP, FIDX ) =  MXCLRC
                     TBSIZE( CURTYP, FIDX ) =  ABS ( DIRREC( DSCLOC ) )
                     MXCLRC                 =  MXCLRC
     .                                       + TBSIZE (CURTYP, FIDX )
 
                     FAST    =       TBMXAD( CURTYP, FIDX )
     .                         .LE.  TBSIZE( CURTYP, FIDX ) * NW(CURTYP)
 
                     DSCLOC  =  DSCLOC + 1
 
                  END DO
C
C                 FAST is set.
C
 
               ELSE
C
C                 The file has more than one directory record.
C
                  FAST = .FALSE.
 
               END IF
C
C              If the file was unknown, readonly, and had one directory
C              record, we determined whether it was a fast file.
C
C
            ELSE
C
C              The file was already known and wasn't fast, or is not
C              readonly.
C
               FAST = .FALSE.
 
            END IF
C
C           FAST is set.
C
 
         END IF
C
C        This is the end of the `.NOT. ( KNOWN .AND. FAST )' case.
C
C        At this point, we've set or looked up CLBASE, CLSIZE, MXADDR,
C        and HIADDR.
C
C        If the file was unknown, we set TBHAN, TBRDON, and TBFAST.
C        If the file was unknown and turned out to be fast, we set
C        TBBASE, TBSIZE, TBHIGH, and TBMXAD as well.
C
C        At this point, it's safe to indicate that the file is known.
C
         KNOWN  =  .TRUE.
 
      END IF
 
C
C     At this point,
C
C        -- CLBASE is properly set:  it is the record number of the
C           first record of the cluster containing ADDRSS.
C
C        -- CLSIZE is properly set:  it is the size of the cluster
C           containing ADDRSS.
C
C        -- HIADDR is the last logical address in the cluster
C           containing ADDRSS.
C
C     Now we must find the physical record and word corresponding
C     to ADDRSS.  The structure of the cluster containing ADDRSS and
C     HIADDR is shown below:
C
C        +--------------------------------------+
C        |                                      |  Record # CLBASE
C        +--------------------------------------+
C                           .
C                           .
C                           .
C        +--------------------------------------+
C        |      |ADDRSS|                        |  Record # RECNO
C        +--------------------------------------+
C                           .
C                           .
C                           .
C        +--------------------------------------+  Record #
C        |                               |HIADDR|
C        +--------------------------------------+  CLBASE + CLSIZE - 1
C
C
C
      RECNO  =     ( CLBASE + CLSIZE - 1 )
     .          -  ( HIADDR - ADDRSS     ) / NW(TYPE)
 
      WORDNO =    ADDRSS    -   (  (ADDRSS-1) / NW(TYPE) ) * NW(TYPE)
 
      RETURN
      END
