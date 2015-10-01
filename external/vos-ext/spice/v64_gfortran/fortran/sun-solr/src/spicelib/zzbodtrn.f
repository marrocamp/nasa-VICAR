C$Procedure ZZBODTRN ( Private --- Body name and code translation )
 
      SUBROUTINE ZZBODTRN ( NAME, CODE, FOUND )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This is the umbrella routine that contains entry points to
C     translate between body names and NAIF integer codes, and
C     for definition of new name/code pairs.
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
 
      CHARACTER*(*)         NAME
      INTEGER               CODE
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME      I/O  ZZBODN2C, ZZBODDEF, ZZBODC2N
C     CODE      I/O  ZZBODC2N, ZZBODDEF, ZZBODN2C
C     FOUND      O   ZZBODN2C and ZZBODC2N
C     MAXL       P   (All)
C     MAXP       P   ZZBODDEF
C
C$ Detailed_Input
C
C     See the entry points for a discussion of their arguments.
C
C$ Detailed_Output
C
C     See the entry points for a discussion of their arguments.
C
C$ Parameters
C
C     MAXL       is the maximum length of a body name.  Defined in
C                the include file 'zzbodtrn.inc'.
C
C     MAXP       is the maximum number of additional names that may
C                be added via the ZZBODDEF interface.  Defined in
C                the include file 'zzbodtrn.inc'.
C
C$ Exceptions
C
C     1) The error SPICE(BOGUSENTRY) is signaled if ZZBODTRN
C        is called directly.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     ZZBODTRN should never be called, instead access the entry
C     points:
C
C        ZZBODN2C      Body name to code
C
C        ZZBODC2N      Body code to name
C
C        ZZBODDEF      Body name/code definition
C
C        ZZBODKIK      Force an examination of the kernel pool
C                      variables, subsequent processing and
C                      the generation of any error messages
C                      resultant from the processing.
C
C        ZZBODRST      Reset the mappings provided via the ZZBODDEF
C                      interface.
C
C     ZZBODN2C and ZZBODC2N perform translations between body names
C     and their corresponding integer codes used in SPK and PCK files
C     and associated routines.  A default set of name/code
C     pairs are automatically defined during the first call to
C     any of the entry points.  Additional name/code pairs may
C     be defined via ZZBODDEF for two purposes:
C
C        1) to associate another, perhaps more familiar or
C           abbreviated name with a previously defined body
C           integer code
C
C        2) to define a new body integer code and name
C
C     Each body name maps to a unique integer code, but more than
C     one name may map to a code.  Associating more than one
C     integer code with a particular name creates ambiguity.
C     Therefore the name-code mapping system establishes a
C     clearly defined precedence structure that assures at any
C     given instant only one code is assigned to a particular
C     name.
C
C     Entries provided via the kernel pool variables are examined
C     first to resolve name-code mappings.  The last listed entries
C     in the kernel pool arrays NAIF_BODY_CODE and NAIF_BODY_NAME
C     resolve any ambiguities that occur.  For example, consider
C     the following text kernel excerpt:
C
C        \begindata
C
C           NAIF_BODY_NAME += 'NAME'
C           NAIF_BODY_CODE += 1000
C
C           NAIF_BODY_NAME += 'NAME'
C           NAIF_BODY_CODE += 1001
C
C        \begintext
C
C     If, after loading this kernel, the following calls are made:
C
C        CALL ZZBODN2C ( 'NAME', CODE,  NAMFND )
C
C        CALL ZZBODC2N ( 1000,   NAME0, FND000 )
C        CALL ZZBODC2N ( 1001,   NAME1, FND001 )
C
C     The values of CODE, NAMFND, NAME0, FND000, NAME1, and FND001
C     will be:
C
C        NAMFND = .TRUE.,  CODE  = 1001
C        FND000 = .FALSE., NAME0 remains unchanged
C        FND001 = .TRUE.,  NAME1 = 'NAME'
C
C     FND000 is .FALSE., because this name-code mapping is masked
C     by the higher precedent 'NAME' <-> 1001 mapping.
C
C     If the name-code mapping is not resolved by the entries
C     provided in the kernel pool, the values assigned via the
C     ZZBODDEF interface are examined next.  As with the kernel
C     pool, the last assignment made via the ZZBODDEF interface
C     has the highest precedence.  Lastly, if the name-code
C     mapping is not resolved by the contents of ZZBODDEF, the
C     built-in mappings are examined.  In actuality, the built-in
C     mappings represent an initial state of the ZZBODDEF listings.
C     As changes are made to this listing, the original mappings
C     are discarded.
C
C     For the case in which multiple names map to a single code, a
C     ZZBODC2N call returns the name last assigned to that code - a
C     LIFO situation.
C
C$ Examples
C
C     1) The following code fragment shows SPKEZ compute the state
C        (position and velocity) of Jupiter as seen from the Galileo
C        Orbiter.  It requires the NAIF integer codes of the target
C        and observer, so we use ZZBODN2C to convert names to integer
C        codes for those bodies.
C
C           CALL ZZBODN2C ( 'JUPITER',         TARGET, FOUND )
C
C           CALL ZZBODN2C ( 'GALILEO ORBITER', OBSRVR, FOUND )
C
C           CALL SPKEZ    ( TARGET, EPOCH, FRAME, ABCORR,
C          .                OBSRVR, STATE, LT             )
C
C
C     2) This example assumes ZZBODDEF has not been called.
C        Thus, only the set of default name/code pairs has been
C        defined.
C
C        Given these names, ZZBODN2C returns the following codes:
C
C           Name                         Code    Found?
C           ------------------------   ------    ------
C           'EARTH'                       399    Yes
C           '  Earth '                    399    Yes
C           'EMB'                           3    Yes
C           'Solar System Barycenter'       0    Yes
C           'SolarSystemBarycenter'         -    No
C           'SSB'                           0    Yes
C           'Voyager 2'                   -32    Yes
C           'U.S.S. Enterprise'             -    No
C           ' '                             -    No
C           'Halley's Comet'                -    No
C
C        and, given these codes, ZZBODC2N returns the following
C        names:
C
C           Code        Name                        Found?
C           -------     -------------------         ------
C           399         'EARTH'                     Yes
C             0         'SOLAR SYSTEM BARYCENTER'   Yes
C             3         'EARTH BARYCENTER'          Yes
C           -77         'GALILEO ORBITER'           Yes
C            11          -                          No
C            -1         'GEOTAIL'                   Yes
C
C     3) This example shows the method to define a name/code pair.
C        You may associate a new name with a previously defined
C        code:
C
C           CALL ZZBODDEF ( 'JB', 5 )
C
C        You may also define the name and integer code for a new
C        body:
C
C           CALL ZZBODDEF ( 'Asteroid Frank', 20103456 )
C
C        After these calls to ZZBODDEF, ZZBODN2C would return
C        the following translations:
C
C           Name                         Code    Found?
C           ------------------------   ------    ------
C           'JB'                            5    Yes
C           'Jupiter Barycenter'            5    Yes
C           'ASTEROID FRANK'         20103456    Yes
C           'ASTEROIDFRANK'                 -    No
C           'Frank'                         -    No
C
C        and ZZBODC2N returns these translations:
C
C           Code        Name                     Found?
C           -------     -------------------      ------
C                  5    'JB'                     Yes
C           20103456    'Asteroid Frank'         Yes
C
C        ZZBODC2N exactly returns the string as used in the
C        body name/ID mapping definition.
C
C     4) To use an external IDs kernel, simply load via a FURNSH
C        call.
C
C           CALL FURNSH ( 'ids.ker' )
C
C        With ids.ker listing data such as:
C
C           \begintext
C
C           Define an additional set of body, ID code mappings.
C
C           \begindata
C
C           NAIF_BODY_CODE  = ( 22, 23, 24, 25 )
C
C           NAIF_BODY_NAME  = ( 'LARRY', 'MOE', 'CURLEY', 'SHEMP' )
C
C        Which maps the names defined in NAIF_BODY_NAME
C        to the corresponding index of NAIF_BODY_CODE, i.e.
C        LARRY -> 22, MOE -> 23, etc, and the IDs in NAIF_BODY_CODE
C        map to the corresponding index of NAIF_BODY_NAME.
C
C        NOTE:  When using an external NAME-ID kernel, all ID codes
C        MUST be listed in the kernel variable NAIF_BODY_CODE, and
C        all names MUST be listed in the kernel variable
C        NAIF_BODY_NAME.
C
C     5) Suppose you ran the utility program SPACIT to summarize
C        an SPK ephemeris file and the following data was output
C        to the terminal screen.
C
C           ----------------------------------------------------------
C           Segment identifier: JPL archive 21354
C           Body        : -77                         Center     : 399
C           From        : 1990 DEC 08 18:00:00.000
C           To          : 1990 DEC 10 21:10:00.000
C           Reference   : DE-200                      SPK Type    :1
C           ----------------------------------------------------------
C
C        You could write a program to translate the body codes
C        shown in the SPACIT output:
C
C           CALL ZZBODC2N ( -77, BODY,   FOUND )
C           CALL ZZBODC2N ( 399, CENTER, FOUND )
C
C           IF ( FOUND ) THEN
C
C              WRITE ( *,* ) 'BODY:    -77 = ', BODY
C              WRITE ( *,* ) 'CENTER:  399 = ', CENTER
C
C           END IF
C
C        You could also read the body and center codes directly from
C        the SPK files, using the appropriate DAF routines, and then
C        translate them, as above.
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
C     N.J. Bachman   (JPL)
C     J.E. McLean    (JPL)
C     H.A. Neilan    (JPL)
C     B.V. Semenov   (JPL)
C     M.J. Spencer   (JPL)
C     W.L. Taber     (JPL)
C     F.S. Turner    (JPL)
C     E.D. Wright    (JPL)
C     K.S. Zukor     (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.3.0, 05-MAR-2009 (NJB)
C
C        Bug fixes: the entry points ZZBODN2C, ZZBODC2N, and ZZBODKIK
C        now keep track of whether their kernel pool look-ups
C        succeeded. If not, a kernel pool lookup is attempted on the
C        next call to any entry point that calls ZZBODKER.
C
C-    SPICELIB Version 4.0.2, 19-SEP-2006 (EDW)
C
C        Added text to previously empty Declarations section.
C
C-    SPICELIB Version 4.0.1, 17-APR-2003 (EDW)
C
C        Corrected typo in header docs.
C
C-    SPICELIB Version 4.0.0, 23-AUG-2002 (FST)
C
C        Cleaned up  ZZBODTRN routine/entry point source code
C        and private subroutines used exclusively by ZZBODTRN
C        to process name-code mappings.
C
C        ZZBODLST has been removed from this umbrella and
C        added to the ZZBODBLT umbrella.
C
C        The built-in (permanent collection) of name-code
C        mappings has been moved from this umbrella into
C        the ZZBODBLT umbrella.  The collection is retrieved
C        from the entry point ZZBODGET in ZZBODBLT.
C
C        See the Revisions section below for details.
C
C-    SPICELIB Version 3.2.0, 14-AUG-2002 (EDW)
C
C        Added the ZZBODKIK entry point.
C
C        Moved the NAIF_BODY_NAME/CODE to subroutine
C        ZZBODKER. No change in logic.
C
C        Added logic to enforce the precedence masking;
C        logic removes duplicate assignments of ZZBODDEF.
C        Removed the NAMENOTUNIQUE error block.
C
C-    SPICELIB Version 3.1.5, 27-NOV-2001 (EDW)
C
C        Added to the collection:
C        -200   CONTOUR
C        -146   LUNAR-A
C        -135   DRTS-W
C
C        Added the subroutine ZZBODLST as an entry point.
C        The routine outputs the current name-ID mapping
C        list to some output device.
C
C-    SPICELIB Version 3.1.0, 17-OCT-2001 (EDW)
C
C        To improve clarity, the BEGXX block initialization now
C        exists in the include file zzbodtrn.inc.
C
C        Removed the comments concerning the 851, 852, ... temporary
C        codes.
C
C        Set the WNAMES assignment to NAIF_BODY_CODE, NAIF_BODY_NAME
C        as a DATA statement.
C
C        Edited headers to match information in naif_ids required
C        reading.
C
C        Edited headers, removed typos and bad grammar, clarified
C        descriptions.
C
C        Added to the collection
C        -41    MARS EXPRESS, MEX
C        -44    BEAGLE 2, BEAGLE2
C        -70    DEEP IMPACT IMPACTOR SPACECRAFT
C        -94    MO, MARS OBSERVER
C        -140   DEEP IMPACT FLYBY SPACECRAFT
C        -172   SLCOMB, STARLIGHT COMBINER
C        -205   SLCOLL, STARLIGHT COLLECTOR
C        -253   MER-A
C        -254   MER-B
C
C        Corrected typo, vehicle -188 should properly be MUSES-C,
C        previous versions listed the name as MUSES-B.
C
C        Removed from collection
C        -84    MARS SURVEYOR 01 LANDER
C        -154   EOS-PM1
C        -200   PLUTO EXPRESS 1, PEX1
C        -202   PLUTO EXPRESS 2, PEX2
C
C-    SPICELIB Version 3.0.0, 29-MAR-2000 (WLT)
C
C        The ID codes for Cluster 1, 2, 3 and 4 were added.  The
C        ID coded for Pluto Express were removed.  The ID codes
C        for Pluto-Kuiper Express, Pluto-Kuiper Express Simulation
C        and Contour were added.
C
C-    SPICELIB Version 2.0.0, 26-JAN-1998 (EDW)
C
C        The Galileo probe ID -228 replaces the incorrect ID -344.
C        DSS stations 5 through 65 added to the collection.
C
C        Added to the collection
C        -107   TROPICAL RAINFALL MEASURING MISSION, TRMM
C        -154,  EOS-PM1
C        -142   EOS-AM1
C        -151   AXAF
C        -1     GEOTAIL
C        -13    POLAR
C        -21    SOHO
C        -8     WIND
C        -25    LUNAR PROSPECTOR, LPM
C        -116   MARS POLAR LANDER, MPL
C        -127   MARS CLIMATE ORBITER, MCO
C        -188   MUSES-C
C        -97    TOPEX/POSEIDON
C        -6     PIONEER-6, P6
C        -7     PIONEER-7, P7
C        -20    PIONEER-8, P8
C        -23    PIONEER-10, P10
C        -24    PIONEER-11, P11
C        -178   NOZOMI, PLANET-B
C        -79    SPACE INFRARED TELESCOPE FACILITY, SIRTF
C        -29    STARDUST, SDU
C        -47    GENESIS
C        -48    HUBBLE SPACE TELESCOPE, HST
C        -200   PLUTO EXPRESS 1, PEX1
C        -202   PLUTO EXPRESS 2, PEX2
C        -164   YOHKOH, SOLAR-A
C        -165   MAP
C        -166   IMAGE
C        -53    MARS SURVEYOR 01 ORBITER
C         618   PAN
C         716   CALIBAN
C         717   SYCORAX
C        -30    DS-1 (low priority)
C        -58    HALCA
C        -150   HUYGEN PROBE, CASP
C        -55    ULS
C
C        Modified ZZBODC2N and ZZBODN2C so the user may load an
C        external IDs kernel to override or supplement the standard
C        collection.  The kernel must be loaded prior a call to
C        ZZBODC2N or ZZBODN2C.
C
C-    SPICELIB Version 1.1.0, 22-MAY-1996 (WLT)
C
C        Added the id-code for Comet Hyakutake, Comet Hale-Bopp,
C        Mars 96, Cassini Simulation, MGS Simulation.
C
C-    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS)
C
C        Renamed umbrella subroutine and entry points to
C        correspond private routine convention (ZZ...). Added IDs for
C        tracking stations Goldstone (399001), Canberra (399002),
C        Madrid (399003), Usuda (399004).
C
C-    Beta Version 2.2.0, 01-AUG-1995 (HAN)
C
C        Added the IDs for Near Earth Asteroid Rendezvous (-93),
C        Mars Pathfinder (-53), Ulysses (-55), VSOP (-58),
C        Radioastron (-59), Cassini spacecraft (-82), and Cassini
C        Huygens probe (-150).
C        Mars Observer (-94) was replaced with Mars Global
C        Surveyor (-94).
C
C-    Beta Version 2.1.0, 15-MAR-1995 (KSZ) (HAN)
C
C        Two Shoemaker Levy 9 fragments were added, Q1 and P2
C        (IDs 50000022 and 50000023). Two asteroids were added,
C        Eros and Mathilde (IDs 2000433 and 2000253). The
C        Saturnian satellite Pan (ID 618) was added.
C
C-    Beta Version 2.0.0, 03-FEB-1995 (NJB)
C
C        The Galileo probe (ID -344) has been added to the permanent
C        collection.
C
C-    Beta Version 1.0.0, 29-APR-1994 (MJS)
C
C        SPICELIB symbol tables are no longer used. Instead, two order
C        vectors are used to index the NAMES and CODES arrays. Also,
C        this version does not support reading body name ID pairs from a
C        file.
C
C-    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT)
C
C       The body id's for the Uranian satellites discovered by Voyager
C       were modified to conform to those established by the IAU
C       nomenclature committee.  In addition the id's for Gaspra and
C       Ida were added.
C
C-    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT)
C
C       Some items previously considered errors were removed
C       and some minor modifications were made to improve the
C       robustness of the routines.
C
C-    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM)
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 4.0.0, 23-AUG-2002 (FST)
C
C        For clarity, some variable names have changed.  The
C        mappings from the old names to the new are provided
C        below:
C
C           Old      New     Function
C           ---      ---     --------
C           NAMES    DEFNAM  Name definition as provided with ZZBODDEF
C           NORNAM   DEFNOR  Normalized name definitions
C           CODES    DEFCOD  Integer codes mapping to entries in DEFNAM
C           ORDCOD   DEFOCD  "Modified" order vector for DEFCOD
C           ORDNOM   DEFONR  Order vector for DEFNOR
C           NNAM     DEFSIZ  Size of DEFNAM, DEFNOR, DEFCOD, and DEFONR
C           NCOD     DEFOSZ  Size of DEFOCD
C
C           CVALS    KERNAM  Name definition as provided from pool
C           CVLNOM   KERNOM  Normalized name definitions
C           IVALS    KERCOD  Integer codes mapping to entries in KERNAM
C           XORDCD   KEROCD  "Modified" order vector for KERCOD
C           XORNOM   KERONR  Order vector for KERNOR
C           NUM(1)   DEFSIZ  Size of KERNAM, KERNOR, KERCOD, and KERONR
C           NUM(2)   DEFOSZ  Size of KEROCD
C
C        The reason for changing the names in this fashion,
C        is simply that these are two instances of variables
C        that have the same properties and utility.  The first
C        set implements the ZZBODDEF style mappings, and the
C        second implements the kernel pool style mappings.
C
C        ZZBODDEF now properly signals an error when a caller
C        attempts to use it to assign a blank string an ID code.
C        This should have never been allowed, but somehow
C        slipped by in previous versions.
C
C        The argument lists for ZZBODKER and ZZBODINI have
C        changed as of previous versions.  Some arguments
C        were removed, as they were no longer necessary.
C
C        ZZBODINI no longer normalizes the input name array;
C        rather it simply computes the order vector for the
C        normalized array input and the "modified" order
C        vector for the input code array.  This was done to
C        save from unnecessarily recomputing the normalization
C        array.
C
C        An additional umbrella has been added to the set of
C        modules of which ZZBODTRN makes use: ZZBODBLT.  This
C        umbrella houses the data statements that used to be
C        present in this module, which defines the "built-in"
C        name-code mappings.  These mappings, as of the changes
C        in N0053, store the mappings the define the initial
C        state of the DEF* arrays.  It contains two entry
C        points:
C
C           ZZBODGET    retrieve the initial values of DEFNAM,
C                       DEFNOR, DEFCOD, and DEFSIZ.
C
C           ZZBODLST    dump the "built-in" codes to a device.
C
C        ZZBODLST used to be present in this umbrella, but the
C        creation of ZZBODBLT made moving it there the logical
C        choice.
C
C        The entry point ZZBODRST has been added to the
C        ZZBODTRN umbrella.  This entry point resets the
C        state of the DEF* arrays to their initial values.
C        This effectively resets any changes made via the
C        ZZBODDEF interface.  It does not effect the kernel
C        pool mappings.
C
C        To support ZZBODRST, a logical BODCHG has been added
C        to the list of saved variables.  This variable
C        indicates when ZZBODDEF has been used to change the
C        built-in body list.
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               BSCHOC
      INTEGER               BSCHOI
 
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local Parameters
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
C
C     Local Variables
C
      CHARACTER*(MAXL)      DEFNAM  ( MAXE )
      CHARACTER*(MAXL)      DEFNOR  ( MAXE )
      CHARACTER*(MAXL)      KERNAM  ( NROOM )
      CHARACTER*(MAXL)      KERNOR  ( NROOM )
      CHARACTER*(MAXL)      TMPNAM
      CHARACTER*(WDSIZE)    WNAMES  ( 2 )
 
      INTEGER               CODIDX
      INTEGER               DEFCOD  ( MAXE )
      INTEGER               DEFOCD  ( MAXE )
      INTEGER               DEFONR  ( MAXE )
      INTEGER               DEFOSZ
      INTEGER               DEFSIZ
      INTEGER               I
      INTEGER               INDEX
      INTEGER               J
      INTEGER               KERCOD  ( NROOM )
      INTEGER               KEROCD  ( NROOM )
      INTEGER               KERONR  ( NROOM )
      INTEGER               KEROSZ
      INTEGER               KERSIZ
      INTEGER               NWATCH
 
      LOGICAL               BODCHG
      LOGICAL               EXTKER
      LOGICAL               FIRST
      LOGICAL               NODATA
      LOGICAL               UPDATE
 
C
C     Save all variables.
C
      SAVE
 
C
C     Data statements.
C
      DATA BODCHG            / .FALSE. /
      DATA FIRST             / .TRUE.  /
      DATA EXTKER            / .FALSE. /
      DATA NODATA            / .TRUE.  /
      DATA WNAMES            / 'NAIF_BODY_NAME', 'NAIF_BODY_CODE' /
 
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'ZZBODTRN'          )
         CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
         CALL CHKOUT ( 'ZZBODTRN'          )
      END IF
 
      RETURN
 
 
 
 
C$Procedure ZZBODN2C ( Private --- Body name to code )
 
      ENTRY ZZBODN2C ( NAME, CODE, FOUND )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Translate a body name to the corresponding SPICE integer code.
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
C     CONVERSION
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C     INTEGER               CODE
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       I   Body name to be translated.
C     CODE       O   Integer code for that body.
C     FOUND      O   True if translated, otherwise false.
C     MAXL       P   Max name length.
C
C$ Detailed_Input
C
C     NAME       is an arbitrary name of a body which could be
C                a planet, satellite, barycenter, spacecraft,
C                asteroid, comet, or other ephemeris object.
C
C                Case and leading and trailing blanks in a name
C                are not significant.  However, when a name consists
C                of more than one word, they must be separated by
C                at least one blank, i.e., all of the following
C                strings are equivalent names:
C
C                   'JUPITER BARYCENTER'
C                   'Jupiter Barycenter'
C                   'JUPITER BARYCENTER   '
C                   'JUPITER    BARYCENTER'
C                   '   JUPITER BARYCENTER'
C
C                However, 'JUPITERBARYCENTER' is not equivalent to
C                the names above.
C
C                When ignoring trailing blanks, NAME must have fewer
C                than MAXL characters.
C
C$ Detailed_Output
C
C     CODE       is the NAIF or user defined integer code for the
C                named body.
C
C     FOUND      return as true if NAME has a translation.
C                Otherwise, FOUND returns as false.
C
C$ Parameters
C
C     MAXL       is the maximum length of a body name.  Defined in
C                the include file 'zzbodtrn.inc'.
C
C$ Exceptions
C
C     Errors may be signaled by routines in the call tree of this
C     routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     ZZBODN2C is one of three related entry points,
C
C        ZZBODN2C      Body name to code
C
C        ZZBODC2N      Body code to name
C
C        ZZBODDEF      Body name/code definition
C
C     ZZBODN2C and ZZBODC2N perform translations between body names
C     and their corresponding integer codes used in SPK and PCK files
C     and associated routines.  A default set of name/code
C     pairs are automatically defined during the first call to
C     any of the entry points.  Additional name/code pairs may
C     be defined via ZZBODDEF.
C
C$ Examples
C
C     See the Examples section of the ZZBODTRN umbrella header.
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
C     N.J. Bachman   (JPL)
C     J.E. McLean    (JPL)
C     B.V. Semenov   (JPL)
C     M.J. Spencer   (JPL)
C     W.L. Taber     (JPL)
C     F.S. Turner    (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.1.0, 05-MAR-2009 (NJB)
C
C        Bug fix: this routine now keeps track of whether its
C        kernel pool look-up succeeded. If not, a kernel pool
C        lookup is attempted on the next call to any entry 
C        point that calls ZZBODKER.
C
C-    SPICELIB Version 4.0.0, 23-AUG-2002 (FST)
C
C        Cleaned up module header and source.  See the Revisions
C        section of ZZBODTRN for detailed update information.
C
C-    SPICELIB Version 3.1.0, 12-FEB-2001 (EDW)
C
C        Added logic to ensure the routine returns the NAME string
C        in the same format as when defined (case and space).
C        Added logic to handle error response in ZZBODINI.
C
C        To improve clarity, the BEGXX block initialization now
C        exists in the include file zzbodtrn.inc.
C
C        Removed the comments concerning the 851, 852, ... temporary
C        codes.
C
C        Set the WNAMES assignment to NAIF_BODY_CODE, NAIF_BODY_NAME
C        as a DATA statement.
C
C        Edited headers, removed typos and bad grammar, clarified
C        descriptions.
C
C-    SPICELIB Version 3.0.0, 29-MAR-2000 (WLT)
C
C        The ID codes for Cluster 1, 2, 3 and 4 were added.  The
C        ID coded for Pluto Express were removed.  The ID codes
C        for Pluto-Kuiper Express, Pluto-Kuiper Express Simulation
C        and Contour were added.
C
C-    SPICELIB Version 2.0.0, 21-JAN-1999 (EDW)
C
C        Added code to use the external name/ID kernel.
C
C-    SPICELIB Version 1.1.0, 29-FEB-1996 (WLT)
C
C        Added the id-code for Comet Hyakutake, Comet Hale-Bopp.
C
C-    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS)
C
C        Renamed to ZZBODN2C (BVS)
C
C-    Beta Version 1.0.0, 29-APR-1994 (MJS)
C
C        SPICELIB symbol tables are no longer used. Instead, two order
C        vectors are used to index the NAMES and CODES arrays.
C
C-    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT)
C
C       The body id's for the Uranian satellites discovered by Voyager
C       were modified to conform to those established by the IAU
C       nomenclature committee.  In addition the id's for Gaspra and
C       Ida were added.
C
C-    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT)
C
C       Items previously considered errors were downgraded
C       to simply be exceptions.  Any NAME is a legitimate input now.
C       If its not in the table, the FOUND flag is just set to .FALSE.
C
C-    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZBODN2C' )
      END IF
 
C
C     Assume we will not find the code we seek.
C
      FOUND = .FALSE.
 
C
C     On the first pass through the umbrella's entry point,
C     initialize the ZZBODDEF arrays and set the kernel pool
C     watchers.
C
      IF ( FIRST ) THEN
 
C
C        Populate the initial values of the DEFNAM, DEFNOR,
C        and DEFCOD arrays from the built-in code list.
C
         CALL ZZBODGET ( MAXE, DEFNAM, DEFNOR, DEFCOD, DEFSIZ )
 
C
C        ZZBODGET may signal an error if the toolkit is improperly
C        configured.  Check FAILED() and return if this occurs.
C
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODN2C' )
            RETURN
         END IF
 
C
C        Produce the initial order ZZBODDEF order vectors.
C
         CALL ZZBODINI ( DEFNAM, DEFNOR, DEFCOD, DEFSIZ,
     .                   DEFONR, DEFOCD, DEFOSZ          )
 
C
C        Set up the watchers for the kernel pool name-code mapping
C        variables.
C
         NWATCH    = 2
         CALL SWPOOL ( 'ZZBODTRN', NWATCH, WNAMES )
 
C
C        SWPOOL may signal an error if any difficulties arise in
C        setting the watcher.  Check FAILED() and return if this
C        occurs.
C
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODN2C' )
            RETURN
         END IF
 
C
C        Set FIRST to .FALSE., so this initialization block is
C        not repeated.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     Check for updates to the kernel pool variables.  Note:
C     the first call to CVPOOL after initialization always returns
C     .TRUE. for UPDATE.  This ensures that any initial
C     assignments are properly processed.
C
      CALL CVPOOL ( 'ZZBODTRN', UPDATE )
 
      IF ( UPDATE .OR. NODATA ) THEN
 
         CALL ZZBODKER ( KERNAM, KERNOR, KERCOD, KERSIZ,
     .                   KERONR, KEROCD, KEROSZ, EXTKER  )

         IF ( FAILED() ) THEN

            NODATA = .TRUE.

            CALL CHKOUT ( 'ZZBODN2C' )
            RETURN

         END IF

         NODATA = .FALSE.
 
      END IF
 
C
C     Compute the canonical member of the equivalence class
C     for the input argument NAME.  This will enable a quick
C     search through KERNOR and DEFNOR to locate the desired
C     code.
C
      CALL LJUST  (         NAME,   TMPNAM )
      CALL UCASE  (         TMPNAM, TMPNAM )
      CALL CMPRSS ( ' ', 1, TMPNAM, TMPNAM )
 
C
C     If necessary, first examine the contents of the kernel pool
C     name-code mapping list.
C
      IF ( EXTKER ) THEN
 
         I = BSCHOC ( TMPNAM, KERSIZ, KERNOR, KERONR )
 
C
C        If we obtained a match, copy the relevant code to the
C        output argument and return.
C
         IF ( I .NE. 0 ) THEN
            CODE  = KERCOD(I)
            FOUND = .TRUE.
            CALL CHKOUT ( 'ZZBODN2C' )
            RETURN
         END IF
 
      END IF
 
C
C     If we reach here, either the kernel pool mapping list was
C     blank or there was no mapping that matched.  Check the
C     ZZBODDEF mappings for a match.
C
      I = BSCHOC ( TMPNAM, DEFSIZ, DEFNOR, DEFONR )
 
      IF ( I .NE. 0 ) THEN
         CODE  = DEFCOD(I)
         FOUND = .TRUE.
      END IF
 
      CALL CHKOUT ( 'ZZBODN2C' )
      RETURN
 
 
 
 
C$Procedure ZZBODC2N ( Private --- Body code to name )
 
      ENTRY ZZBODC2N ( CODE, NAME, FOUND )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Translate the integer code of a body into a common name for
C     that body.
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
C     CONVERSION
C
C$ Declarations
C
C     INTEGER               CODE
C     CHARACTER*(*)         NAME
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     CODE       I   Integer code to be translated.
C     NAME       O   Common name for the body identified by CODE.
C     FOUND      O   True if translated, otherwise false.
C     MAXL       P   Max name length.
C
C$ Detailed_Input
C
C     CODE       is an integer code for a body ---
C                a planet, satellite, barycenter, spacecraft,
C                asteroid, comet, or other ephemeris object.
C
C$ Detailed_Output
C
C     NAME       is the common name of the body identified by CODE.
C                If CODE has more than one translation, then the
C                most recently defined NAME corresponding to CODE
C                is returned.  The routine returns NAME in the exact
C                format (case and blanks) as used when defining
C                the name/code pair.
C
C     FOUND      returns as true if NAME has a translation.
C                Otherwise, FOUND returns as false.
C
C$ Parameters
C
C     MAXL       is the maximum length of a body name.  Defined in
C                the include file 'zzbodtrn.inc'.
C$ Exceptions
C
C     Errors may be signaled by routines in the call tree of this
C     routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     ZZBODC2N is one of three related entry points,
C
C        ZZBODN2C      Body name to code
C
C        ZZBODC2N      Body code to name
C
C        ZZBODDEF      Body name/code definition
C
C     ZZBODN2C and ZZBODC2N perform translations between body names
C     and their corresponding integer codes used in SPK and PCK files
C     and associated routines.  A default set of name/code
C     pairs are automatically defined during the first call to
C     any of the entry points.  Additional name/code pairs may
C     be defined via ZZBODDEF.
C
C     For the case in which multiple names map to a single code, a
C     ZZBODC2N call returns the name last assigned to that code - a
C     LIFO situation.
C
C$ Examples
C
C     See Examples section of ZZBODTRN umbrella header.
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
C     N.J. Bachman   (JPL)
C     J.E. McLean    (JPL)
C     B.V. Semenov   (JPL)
C     M.J. Spencer   (JPL)
C     W.L. Taber     (JPL)
C     F.S. Turner    (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.1.0, 05-MAR-2009 (NJB)
C
C        Bug fix: this routine now keeps track of whether its
C        kernel pool look-up succeeded. If not, a kernel pool
C        lookup is attempted on the next call to any entry 
C        point that calls ZZBODKER.
C
C-    SPICELIB Version 4.0.0, 23-AUG-2002 (FST)
C
C        Cleaned up module header and source code.  See the Revisions
C        section of ZZBODTRN for detailed update information.
C
C-    SPICELIB Version 3.2.0, 19-JUL-2002 (EDW)
C
C        Added logic to enforce the precedence masking.
C
C-    SPICELIB Version 3.1.0, 5-SEP-2001 (EDW)
C
C        Added logic to ensure the routine returns the NAME string
C        in the same format as when defined (case and space).
C        Added logic to handle error response in ZZBODINI.
C
C        To improve clarity, the BEGXX block initialization now
C        exists in the include file zzbodtrn.inc.
C
C        Removed the comments concerning the 851, 852, ... temporary
C        codes.
C
C        Set the WNAMES assignment to NAIF_BODY_CODE, NAIF_BODY_NAME
C        as a DATA statement.
C
C        Edited headers, removed typos and bad grammar, clarified
C        descriptions.
C
C-    SPICELIB Version 3.0.0, 29-MAR-2000 (WLT)
C
C        The ID codes for Cluster 1, 2, 3 and 4 were added.  The
C        ID coded for Pluto Express were removed.  The ID codes
C        for Pluto-Kuiper Express, Pluto-Kuiper Express Simulation
C        and Contour were added.
C
C-    SPICELIB Version 2.0.0, 21-JAN-1999 (EDW)
C
C        Added code to use the external name/ID kernel.
C
C-    SPICELIB Version 1.1.0, 29-FEB-1996 (WLT)
C
C        Added the id-code for Comet Hyakutake, Comet Hale-Bopp.
C
C-    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS)
C
C        Renamed to ZZBODC2N (BVS)
C
C-    Beta Version 1.0.0, 29-APR-1994 (MJS)
C
C        SPICELIB symbol tables are no longer used. Instead, two order
C        vectors are used to index the NAMES and CODES arrays.
C
C-    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT)
C
C       The body id's for the Uranian satellites discovered by Voyager
C       were modified to conform to those established by the IAU
C       nomenclature committee.  In addition the id's for Gaspra and
C       Ida were added.
C
C-    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT)
C
C       Checks to see the input integer code can be represented
C       as a character string were removed along with the exceptions
C       associated with these checks.  It is now the responsibility
C       of a maintenance programmer to make sure MAXL is large
C       enough to allow any integer to be converted to a string
C       representation.
C
C-    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM)
C
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZBODC2N' )
      END IF
 
C
C     Assume we will not find the name we seek.
C
      FOUND = .FALSE.
 
C
C     On the first pass through the umbrella's entry point,
C     initialize the ZZBODDEF arrays and set the kernel pool
C     watchers.
C
      IF ( FIRST ) THEN
 
C
C        Populate the initial values of the DEFNAM, DEFNOR,
C        and DEFCOD arrays from the built-in code list.
C
         CALL ZZBODGET ( MAXE, DEFNAM, DEFNOR, DEFCOD, DEFSIZ )
 
C
C        ZZBODGET may signal an error if the toolkit is improperly
C        configured.  Check FAILED() and return if this occurs.
C
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODC2N' )
            RETURN
         END IF
 
C
C        Produce the initial order ZZBODDEF order vectors.
C
         CALL ZZBODINI ( DEFNAM, DEFNOR, DEFCOD, DEFSIZ,
     .                   DEFONR, DEFOCD, DEFOSZ          )
 
C
C        Set up the watchers for the kernel pool name-code mapping
C        variables.
C
         NWATCH    = 2
         CALL SWPOOL ( 'ZZBODTRN', NWATCH, WNAMES )
 
C
C        SWPOOL may signal an error if any difficulties arise in
C        setting the watcher.  Check FAILED() and return if this
C        occurs.
C
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODC2N' )
            RETURN
         END IF
 
C
C        Set FIRST to .FALSE., so this initialization block is
C        not repeated.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     Check for updates to the kernel pool variables.  Note:
C     the first call to CVPOOL after initialization always returns
C     .TRUE. for UPDATE.  This ensures that any initial
C     assignments are properly processed.
C
      CALL CVPOOL ( 'ZZBODTRN', UPDATE )
 
      IF ( UPDATE .OR. NODATA ) THEN
 
         CALL ZZBODKER ( KERNAM, KERNOR, KERCOD, KERSIZ,
     .                   KERONR, KEROCD, KEROSZ, EXTKER  )

         IF ( FAILED() ) THEN

            NODATA = .TRUE.

            CALL CHKOUT ( 'ZZBODC2N' )
            RETURN

         END IF

         NODATA = .FALSE.
 
      END IF
 
C
C     If necessary, first examine the contents of the kernel pool
C     name-code mapping list.
C
      IF ( EXTKER ) THEN
 
C
C        Search the list of codes, KERCOD, using the
C        modified order vector KEROCD.
C
         I = BSCHOI ( CODE, KEROSZ, KERCOD, KEROCD )
 
C
C        If we obtained a match, copy the original name to the
C        output argument and return.
C
         IF ( I .NE. 0 ) THEN
            NAME  = KERNAM(I)
            FOUND = .TRUE.
            CALL CHKOUT ( 'ZZBODC2N' )
            RETURN
         END IF
 
      END IF
 
C
C     If we reach here, either the kernel pool mapping list was
C     blank or there was no mapping that matched.  Check the
C     ZZBODDEF mappings for a match.
C
      I = BSCHOI ( CODE, DEFOSZ, DEFCOD, DEFOCD )
 
C
C     If we find a match, verify that it is not masked by
C     a kernel pool entry before returning.
C
      IF ( I .NE. 0 ) THEN
 
         IF ( EXTKER ) THEN
 
C
C           Only bother performing this check if there are actually
C           mappings present in the kernel pool lists.
C
            CALL LJUST  (         DEFNAM(I), TMPNAM )
            CALL UCASE  (         TMPNAM,    TMPNAM )
            CALL CMPRSS ( ' ', 1, TMPNAM,    TMPNAM )
 
            J = BSCHOC ( TMPNAM, KERSIZ, KERNOR, KERONR )
 
            IF ( J .NE. 0 ) THEN
 
C
C              If a match has occurred, then set FOUND to .FALSE.,
C              as the contents of the kernel pool have higher
C              precedence than any entries in the ZZBODDEF mapping
C              list.
C
               FOUND = .FALSE.
 
            ELSE
 
C
C              No match for DEFNAM(I) in the kernel pool mapping list.
C              Return the name.
C
               NAME  = DEFNAM(I)
               FOUND = .TRUE.
 
            END IF
 
         ELSE
 
C
C           No kernel pool mappings were defined, simply return
C           return the name.
C
            NAME  = DEFNAM(I)
            FOUND = .TRUE.
 
         END IF
 
      END IF
 
      CALL CHKOUT ( 'ZZBODC2N' )
      RETURN
 
 
 
 
C$Procedure ZZBODDEF ( Private --- Body name/code definition )
 
      ENTRY ZZBODDEF ( NAME, CODE )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Define a body name/code pair for later translation by
C     ZZBODN2C or ZZBODC2N.
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
C     CONVERSION
C
C$ Declarations
C
C     CHARACTER*(*)         NAME
C     INTEGER               CODE
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       I   Common name of some body.
C     CODE       I   Integer code for that body.
C     MAXL       P   Max name length and max number of digits in code.
C     MAXP       P   Maximum number of name/code pair definitions.
C
C$ Detailed_Input
C
C     NAME       is an arbitrary name of a body which could be
C                a planet, satellite, barycenter, spacecraft,
C                asteroid, comet, or other ephemeris object.
C
C                The case and positions of blanks in a name
C                are significant. ZZBODC2N returns the exact
C                string (case and space) last mapped to a code.
C                When a name is made up of more than one word,
C                the words require separation by at least one blank,
C                i.e., all of the following strings belong to
C                the same equivalence class:
C
C                   'JUPITER BARYCENTER'
C                   'Jupiter Barycenter'
C                   'JUPITER BARYCENTER   '
C                   'JUPITER    BARYCENTER'
C                   '   JUPITER BARYCENTER'
C
C                However, 'JUPITERBARYCENTER' is not equivalent to
C                the names above.
C
C                When ignoring trailing blanks, NAME must have fewer
C                than MAXL characters.
C
C     CODE       is the integer code for the named body.
C
C                CODE may already have a name as defined by a
C                previous call to ZZBODDEF or as part of the set of
C                default definitions.  That previous definition
C                remains and a translation of that name still
C                returns the same CODE.  However, future translations
C                of CODE will give the new NAME instead of the
C                previous one.  This feature is useful for assigning
C                a more familiar or abbreviated name to a body.
C                For example, in addition to the default name for
C                body 5, 'JUPITER BARYCENTER', you could define the
C                abbreviation 'JB' to mean 5.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     MAXL        is the maximum length of a body name.  Defined in
C                 the include file 'zzbodtrn.inc'.
C
C     MAXP        is the maximum number of additional names that may
C                 be added via the ZZBODDEF interface.  Defined in
C                 the include file 'zzbodtrn.inc'.
C
C$ Exceptions
C
C     1) If the maximum number of definitions is exceeded, a the
C        error SPICE(TOOMANYPAIRS) is signaled.
C
C     2) If an attempt to assign a blank string an ID code is made,
C        the error SPICE(BLANKNAMEASSIGNED) is signaled.
C
C     3) Routines in the call tree of this routine may signal
C        errors.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     ZZBODDEF is one of three related entry points,
C
C        ZZBODN2C      Body name to code
C
C        ZZBODC2N      Body code to name
C
C        ZZBODDEF      Body name/code definition
C
C     ZZBODN2C and ZZBODC2N perform translations between body names
C     and their corresponding integer codes used in SPK and PCK files
C     and associated routines.  A default set of name/code
C     pairs are automatically defined during the first call to
C     any of the entry points.  Additional name/code pairs may
C     be defined via ZZBODDEF for two purposes:
C
C        1.  to associate another, perhaps more familiar or
C            abbreviated name with a previously defined body integer
C            code or
C
C        2.  to define a new body integer code and name,
C
C     Each body has a unique integer code, but may have several
C     names.  Thus you may associate more than one name with
C     a particular integer code.  However, associating more
C     than one integer code with a particular name creates ambiguity.
C     Therefore, once a name has been defined, it may not be redefined
C     with a different integer code.
C
C     For example, Europa is the name of the second satellite of
C     Jupiter, and has the NAIF integer code 502.  Thus (EUROPA, 502)
C     is one of the default definitions.  Europa is also the name
C     of an asteroid.  Suppose you were able to associate the asteroid
C     integer code with the name EUROPA.  Then when you call ZZBODN2C to
C     translate the name EUROPA, which code should be returned?  That
C     of the asteroid or 502?
C
C     ZZBODDEF prevents this ambiguity by signalling an error
C     if the specified name has already been defined with a
C     different code.  In the case of EUROPA, you may want to use the
C     name ASTEROID EUROPA.  The set of default definitions are listed
C     in DATA statements in the umbrella routine ZZBODTRN for easy
C     reference.
C
C$ Examples
C
C     See the Examples section of the ZZBODTRN umbrella header.
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
C     J.E. McLean    (JPL)
C     B.V. Semenov   (JPL)
C     W.L. Taber     (JPL)
C     F.S. Turner    (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.0.1, 17-APR-2003 (EDW)
C
C        Correct typo in header docs.
C
C     SPICELIB Version 4.0.0, 23-AUG-2002 (FST)
C
C        Cleaned up module header and source code.  See the Revisions
C        section of ZZBODTRN for detailed update information.
C
C        Added the error SPICE(BLANKNAMEASSIGNED), when the caller
C        attempts to assign an ID code to a blank string.
C
C-    SPICELIB Version 1.3.0, 14-AUG-2002 (EDW)
C
C        Added logic to enforce the precedence masking;
C        logic removes duplicate assignments of ZZBODDEF.
C        Removed the NAMENOTUNIQUE error block.
C
C-    SPICELIB Version 1.2.0, 5-SEP-2001 (EDW)
C
C        Added logic to ensure the routine returns the NAME string
C        in the same format as when defined (case and space).
C        Added logic to handle error response from ZZBODINI.
C
C        To improve clarity, the BEGXX block initialization now
C        exists in the include file zzbodtrn.inc.
C
C        Removed the comments concerning the 851, 852, ... temporary
C        codes.
C
C        Set the WNAMES assignment to NAIF_BODY_CODE, NAIF_BODY_NAME
C        as a DATA statement.
C
C        Edited headers, removed typos and bad grammar, clarified
C        descriptions.
C
C-    SPICELIB Version 1.1.0, 29-FEB-1996 (WLT)
C
C        Added the id-code for Comet Hyakutake, Comet Hale-Bopp.
C
C-    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS)
C
C        Renamed to ZZBODDEF (BVS). More careful checking for overflow
C        of the recognized names is now performed.
C
C-    Beta Version 1.0.0, 29-APR-1994 (MJS)
C
C        SPICELIB symbol tables are no longer used. Instead, two order
C        vectors are used to index the NAMES and CODES arrays.
C
C-    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT)
C
C       The body id's for the Uranian satellites discovered by Voyager
C       were modified to conform to those established by the IAU
C       nomenclature committee.  In addition the id's for Gaspra and
C       Ida were added.
C
C-    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT)
C
C       Checks to see an integer code can be represented
C       as a character string were removed along with the exceptions
C       associated with these checks.  It is now the responsibility
C       of a maintenance programmer to make sure MAXL is large
C       enough to allow any integer to be converted to a string
C       representation.
C
C-    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZBODDEF' )
      END IF
 
C
C     On the first pass through the umbrella's entry point,
C     initialize the ZZBODDEF arrays and set the kernel pool
C     watchers.
C
      IF ( FIRST ) THEN
 
C
C        Populate the initial values of the DEFNAM, DEFNOR,
C        and DEFCOD arrays from the built-in code list.
C
         CALL ZZBODGET ( MAXE, DEFNAM, DEFNOR, DEFCOD, DEFSIZ )
 
C
C        ZZBODGET may signal an error if the toolkit is improperly
C        configured.  Check FAILED() and return if this occurs.
C
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODDEF' )
            RETURN
         END IF
 
C
C        Produce the initial order ZZBODDEF order vectors.
C
         CALL ZZBODINI ( DEFNAM, DEFNOR, DEFCOD, DEFSIZ,
     .                   DEFONR, DEFOCD, DEFOSZ          )
 
C
C        Set up the watchers for the kernel pool name-code mapping
C        variables.
C
         NWATCH    = 2
         CALL SWPOOL ( 'ZZBODTRN', NWATCH, WNAMES )
 
C
C        SWPOOL may signal an error if any difficulties arise in
C        setting the watcher.  Check FAILED() and return if this
C        occurs.
C
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODDEF' )
            RETURN
         END IF
 
C
C        Set FIRST to .FALSE., so this initialization block is
C        not repeated.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     Begin by verifying that the user is not attempting to assign
C     a blank string a code.
C
      IF ( NAME .EQ. ' ' ) THEN
 
         CALL SETMSG ( 'An attempt to assign the code, #, to '
     .   //            'a blank string was made.  Check loaded '
     .   //            'text kernels for a blank string in '
     .   //            'the NAIF_BODY_NAME array.'               )
         CALL ERRINT ( '#', I                                    )
         CALL SIGERR ( 'SPICE(BLANKNAMEASSIGNED)'                )
         CALL CHKOUT ( 'ZZBODDEF'                                )
         RETURN
 
      END IF
 
C
C     Compute the normalization of NAME.  This will allow simple
C     searches through the existing mapping list.
C
      CALL LJUST  (         NAME,   TMPNAM )
      CALL UCASE  (         TMPNAM, TMPNAM )
      CALL CMPRSS ( ' ', 1, TMPNAM, TMPNAM )
 
C
C     Determine if we are going to replace an entry currently
C     present in the DEF* lists.
C
      INDEX = BSCHOC ( TMPNAM, DEFSIZ, DEFNOR, DEFONR )
 
      IF ( INDEX .NE. 0 ) THEN
 
C
C        We are going to replace an existing entry.  There are
C        two possible ways in which a replace operation can
C        happen:
C
C           1) The caller is attempting to replace the highest
C              precedent name-code mapping for a particular
C              ID code.  When this happens, we need only change
C              the entry in DEFNAM at position INDEX.  The user
C              is simply changing the name.
C
C           2) The caller is attempting to change the code
C              associated with a name, bump a lower precedence
C              name-code mapping to highest precedence, or some
C              combination of the two.
C
C        See if we should handle 1) first.
C
         CODIDX = BSCHOI ( CODE, DEFOSZ, DEFCOD, DEFOCD )
 
C
C        If CODIDX matches INDEX, then we simply have to replace
C        the entry in DEFNAM and return.
C
         IF ( CODIDX .EQ. INDEX ) THEN
 
C
C           We altered the built-in body list.  Set BODCHG to
C           .TRUE.
C
            BODCHG           = .TRUE.
 
            DEFNAM ( INDEX ) = NAME
 
            CALL CHKOUT ( 'ZZBODDEF' )
            RETURN
 
         END IF
 
C
C        At this point we have to replace all of the values
C        for the mapping defined at the INDEX position in
C        DEFNAM, DEFNOR, and DEFCOD.  This will require
C        recomputing the order vectors.  First compress
C        out the existing entry.
C
         DO I = INDEX+1, DEFSIZ
 
            DEFNAM( I-1 ) = DEFNAM( I )
            DEFNOR( I-1 ) = DEFNOR( I )
            DEFCOD( I-1 ) = DEFCOD( I )
 
         END DO
 
      ELSE
 
C
C        We need to add this entry to the list.  See if there
C        is room; signal an error and return if there is not.
C
         IF ( DEFSIZ .GE. MAXE ) THEN
 
            CALL SETMSG ( 'There is no room available for adding '
     .      //            '''#''  to the list of name/code pairs. '
     .      //            'The number of names that can be '
     .      //            'supported is #.  This number has been '
     .      //            'reached. '                               )
            CALL ERRCH  ( '#', NAME                                 )
            CALL ERRINT ( '#', DEFSIZ                               )
            CALL SIGERR ( 'SPICE(TOOMANYPAIRS)'                     )
            CALL CHKOUT ( 'ZZBODDEF'                                )
            RETURN
 
         END IF
 
C
C        If we reach here, then there is room in the list.
C        Increase it's size counter.
C
         DEFSIZ = DEFSIZ + 1
 
      END IF
 
C
C     We are changing the body list, inform ZZBODRST by setting BODCHG
C     to .TRUE.
C
      BODCHG = .TRUE.
 
C
C     Now, we need to add the new entry on to the end of the
C     DEFNAM, DEFNOR, and DEFCOD lists.
C
      DEFNAM ( DEFSIZ ) = NAME
      DEFNOR ( DEFSIZ ) = TMPNAM
      DEFCOD ( DEFSIZ ) = CODE
 
C
C     Compute the new order vectors.
C
      CALL ZZBODINI ( DEFNAM, DEFNOR, DEFCOD, DEFSIZ,
     .                DEFONR, DEFOCD, DEFOSZ          )
 
      CALL CHKOUT ( 'ZZBODDEF' )
      RETURN
 
 
 
 
C$Procedure ZZBODKIK ( Private --- Run the kernel read block )
 
      ENTRY ZZBODKIK ( )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This routine executes the kernel pool read instructions
C     if necessary.
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
C     NONE.
C
C$ Keywords
C
C     BODY MAPPING
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     None.
C
C$ Detailed_Input
C
C     NONE.
C
C$ Detailed_Output
C
C     NONE.
C
C$ Parameters
C
C     NONE.
C
C$ Exceptions
C
C     NONE.
C
C$ Files
C
C     NONE.
C
C$ Particulars
C
C     This entry point provides a mechanism to allow a caller
C     to force the examination of the kernel pool variables that
C     define name-code mappings.  This is useful, if once a new
C     mapping is defined, diagnostics at the time of definition
C     are useful.  The way the system performs otherwise, the
C     diagnostics are not provided until a name-code conversion
C     is attempted.
C
C$ Examples
C
C     See ZZLDKER for sample usage.
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
C     N.J. Bachman   (JPL)
C     F.S. Turner    (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.1.0, 05-MAR-2009 (NJB)
C
C        Bug fix: this routine now keeps track of whether its
C        kernel pool look-up succeeded. If not, a kernel pool
C        lookup is attempted on the next call to any entry 
C        point that calls ZZBODKER.
C
C-    SPICELIB Version 4.0.2, 19-SEP-2006 (EDW)
C
C        Added text to previously empty Declarations section.
C
C-    SPICELIB Version 4.0.0, 23-AUG-2002 (FST)
C
C        Added checks to watchers and the initialization
C        block.
C
C-    SPICELIB Version 1.0.0, 16-JUN-2002 (EDW)
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZBODKIK' )
      END IF
 
C
C     On the first pass through the umbrella's entry point,
C     initialize the ZZBODDEF arrays and set the kernel pool
C     watchers.
C
      IF ( FIRST ) THEN
 
C
C        Populate the initial values of the DEFNAM, DEFNOR,
C        and DEFCOD arrays from the built-in code list.
C
         CALL ZZBODGET ( MAXE, DEFNAM, DEFNOR, DEFCOD, DEFSIZ )
 
C
C        ZZBODGET may signal an error if the toolkit is improperly
C        configured.  Check FAILED() and return if this occurs.
C
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODKIK' )
            RETURN
         END IF
 
C
C        Produce the initial order ZZBODDEF order vectors.
C
         CALL ZZBODINI ( DEFNAM, DEFNOR, DEFCOD, DEFSIZ,
     .                   DEFONR, DEFOCD, DEFOSZ          )
 
C
C        Set up the watchers for the kernel pool name-code mapping
C        variables.
C
         NWATCH    = 2
         CALL SWPOOL ( 'ZZBODTRN', NWATCH, WNAMES )
 
C
C        SWPOOL may signal an error if any difficulties arise in
C        setting the watcher.  Check FAILED() and return if this
C        occurs.
C
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODKIK' )
            RETURN
         END IF
 
C
C        Set FIRST to .FALSE., so this initialization block is
C        not repeated.
C
         FIRST = .FALSE.
 
      END IF
 
 
C
C     Check for updates to the kernel pool variables. Note:
C     the first call to CVPOOL after initialization always
C     returns .TRUE. for UPDATE.  This ensures that any
C     initial assignments are properly processed.
C
      CALL CVPOOL ( 'ZZBODTRN', UPDATE )
 
      IF ( UPDATE .OR. NODATA ) THEN
 
         CALL ZZBODKER ( KERNAM, KERNOR, KERCOD, KERSIZ,
     .                   KERONR, KEROCD, KEROSZ, EXTKER  )

         IF ( FAILED() ) THEN

            NODATA = .TRUE.

            CALL CHKOUT ( 'ZZBODKIK' )
            RETURN

         END IF

         NODATA = .FALSE.

 
      END IF
 
      CALL CHKOUT ( 'ZZBODKIK' )
      RETURN
 
 
 
 
C$Procedure ZZBODRST ( Private --- Body List Reset )
 
      ENTRY ZZBODRST ( )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This routine resets the built-in body list, removing any
C     assignments or alterations made by the ZZBODDEF entry point.
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
C     BODY
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     None.
C
C$ Detailed_Input
C
C     None.
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
C     1) Routines in the call tree of this routine may signal errors.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     ZZBODRST resets the built-in body name-code mapping list.  This
C     list may only be modified by ZZBODDEF.  Further, any assignments
C     made through the kernel pool mechanism remain unaltered as a
C     result of invoking this routine.
C
C$ Examples
C
C     None.
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
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.0.0, 26-AUG-2002 (FST)
C
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZBODRST' )
      END IF
 
C
C     On the first pass through the umbrella's entry point,
C     initialize the ZZBODDEF arrays and set the kernel pool
C     watchers.
C
      IF ( FIRST ) THEN
 
C
C        Populate the initial values of the DEFNAM, DEFNOR,
C        and DEFCOD arrays from the built-in code list.
C
         CALL ZZBODGET ( MAXE, DEFNAM, DEFNOR, DEFCOD, DEFSIZ )
 
C
C        ZZBODGET may signal an error if the toolkit is improperly
C        configured.  Check FAILED() and return if this occurs.
C
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODRST' )
            RETURN
         END IF
 
C
C        Produce the initial order ZZBODDEF order vectors.
C
         CALL ZZBODINI ( DEFNAM, DEFNOR, DEFCOD, DEFSIZ,
     .                   DEFONR, DEFOCD, DEFOSZ          )
 
C
C        Set up the watchers for the kernel pool name-code mapping
C        variables.
C
         NWATCH    = 2
         CALL SWPOOL ( 'ZZBODTRN', NWATCH, WNAMES )
 
C
C        SWPOOL may signal an error if any difficulties arise in
C        setting the watcher.  Check FAILED() and return if this
C        occurs.
C
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZBODRST' )
            RETURN
         END IF
 
C
C        Set FIRST to .FALSE., so this initialization block is
C        not repeated.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     See if the body list needs to be reset.
C
      IF ( BODCHG ) THEN
 
         BODCHG = .FALSE.
 
C
C        Fetch the initial body name-code mapping list.  Note:
C        we need not check FAILED() here, because if an error
C        had occurred due to the improper specification of MAXE
C        it would have been signaled already to the user.
C
         CALL ZZBODGET ( MAXE, DEFNAM, DEFNOR, DEFCOD, DEFSIZ )
 
C
C        Prepare the order vectors.
C
         CALL ZZBODINI ( DEFNAM, DEFNOR, DEFCOD, DEFSIZ,
     .                   DEFONR, DEFOCD, DEFOSZ          )
 
 
      END IF
 
      CALL CHKOUT ( 'ZZBODRST' )
      RETURN
 
      END
