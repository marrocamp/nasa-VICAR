C$Procedure ILUMIN ( Illumination angles )
 
      SUBROUTINE ILUMIN ( METHOD, TARGET, ET,     FIXREF,  
     .                    ABCORR, OBSRVR, SPOINT, TRGEPC,  
     .                    SRFVEC, PHASE,  SOLAR,  EMISSN  )
 
C$ Abstract
C
C     Find the illumination angles (phase, solar incidence, and
C     emission) at a specified surface point of a target body.
C
C     This routine supersedes ILLUM.
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
C     FRAMES
C     NAIF_IDS
C     PCK
C     SPK
C     TIME
C
C$ Keywords
C
C     GEOMETRY
C     MOSPICE
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE               'zzabcorr.inc'

      CHARACTER*(*)         METHOD
      CHARACTER*(*)         TARGET
      DOUBLE PRECISION      ET
      CHARACTER*(*)         FIXREF
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      PHASE
      DOUBLE PRECISION      SOLAR
      DOUBLE PRECISION      EMISSN
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     METHOD     I   Computation method.
C     TARGET     I   Name of target body.
C     ET         I   Epoch in ephemeris seconds past J2000 TDB.
C     FIXREF     I   Body-fixed, body-centered target body frame.
C     ABCORR     I   Desired aberration correction.
C     OBSRVR     I   Name of observing body.
C     SPOINT     I   Body-fixed coordinates of a target surface point.
C     TRGEPC     O   Target surface point epoch.
C     SRFVEC     O   Vector from observer to target surface point.
C     PHASE      O   Phase angle at the surface point.
C     SOLAR      O   Solar incidence angle at the surface point.
C     EMISSN     O   Emission angle at the surface point.
C
C$ Detailed_Input
C
C
C     METHOD      is a short string providing parameters defining
C                 the computation method to be used. Parameters
C                 include, but are not limited to, the shape model
C                 used to represent the surface of the target body.
C
C                 The only choice currently supported is
C
C                    'Ellipsoid'        The illumination angle
C                                       computation uses a triaxial
C                                       ellipsoid to model the surface
C                                       of the target body. The
C                                       ellipsoid's radii must be
C                                       available in the kernel pool.
C
C                 Neither case nor white space are significant in 
C                 METHOD. For example, the string ' eLLipsoid ' is 
C                 valid.                 
C
C
C     TARGET      is the name of the target body. TARGET is
C                 case-insensitive, and leading and trailing blanks in
C                 TARGET are not significant. Optionally, you may
C                 supply a string containing the integer ID code for
C                 the object. For example both 'MOON' and '301' are
C                 legitimate strings that indicate the Moon is the
C                 target body.
C
C     ET          is the epoch, expressed as seconds past J2000 TDB,
C                 for which the apparent illumination angles at the
C                 specified surface point on the target body, as seen
C                 from the observing body, are to be computed.
C
C
C     FIXREF      is the name of the body-fixed, body-centered
C                 reference frame associated with the target body. The
C                 input surface point SPOINT and the output vector
C                 SRFVEC are expressed relative to this reference
C                 frame. The string FIXREF is case-insensitive, and
C                 leading and trailing blanks in FIXREF are not
C                 significant.
C
C
C     ABCORR      is the aberration correction to be used in computing
C                 the position and orientation of the target body and
C                 the location of the Sun.
C         
C                 For remote sensing applications, where the apparent
C                 illumination angles seen by the observer are desired,
C                 normally either of the corrections 
C              
C                    'LT+S' 
C                    'CN+S'
C     
C                 should be used. These and the other supported options
C                 are described below. ABCORR may be any of the 
C                 following:
C
C                    'NONE'     No aberration correction.
C
C                 Let LT represent the one-way light time between the
C                 observer and SPOINT (note: NOT between the observer
C                 and the target body's center). The following values
C                 of ABCORR apply to the "reception" case in which
C                 photons depart from SPOINT at the light-time
C                 corrected epoch ET-LT and *arrive* at the observer's
C                 location at ET:
C
C                    'LT'       Correct both the position of SPOINT as
C                               seen by the observer, and the position
C                               of the Sun as seen by the target, for
C                               light time.
C
C                    'LT+S'     Correct both the position of SPOINT as
C                               seen by the observer, and the position
C                               of the Sun as seen by the target, for
C                               light time and stellar aberration.
C
C                    'CN'       Converged Newtonian light time
C                               correction. In solving the light time
C                               equations for target and the Sun, the
C                               "CN" correction iterates until the
C                               solution converges.
C
C                    'CN+S'     Converged Newtonian light time and
C                               stellar aberration corrections. This
C                               option produces a solution that is at
C                               least as accurate at that obtainable
C                               with the 'LT+S' option. Whether the
C                               'CN+S' solution is substantially more
C                               accurate depends on the geometry of the
C                               participating objects and on the
C                               accuracy of the input data. In all
C                               cases this routine will execute more
C                               slowly when a converged solution is
C                               computed.
C
C                 Neither case nor white space are significant in
C                 ABCORR. For example, the string 
C
C                   'Lt + s'
C
C                 is valid.
C                               
C
C     OBSRVR      is the name of the observing body. The observing body
C                 is an ephemeris object: it typically is a spacecraft,
C                 the earth, or a surface point on the earth. OBSRVR is
C                 case-insensitive, and leading and trailing blanks in
C                 OBSRVR are not significant. Optionally, you may
C                 supply a string containing the integer ID code for
C                 the object. For example both 'MOON' and '301' are
C                 legitimate strings that indicate the Moon is the
C                 observer.
C
C                 OBSRVR may be not be identical to TARGET.
C
C
C     SPOINT      is a surface point on the target body, expressed in
C                 Cartesian coordinates, relative to the body-fixed
C                 target frame designated by FIXREF.
C
C                 SPOINT need not be visible from the observer's
C                 location at the epoch ET.
C
C                 The components of SPOINT have units of km.
C
C
C$ Detailed_Output
C
C
C     TRGEPC      is the "surface point epoch." TRGEPC is defined as
C                 follows: letting LT be the one-way light time between
C                 the observer and the input surface point SPOINT,
C                 TRGEPC is either the epoch ET-LT or ET depending on
C                 whether the requested aberration correction is,
C                 respectively, for received radiation or omitted. LT
C                 is computed using the method indicated by ABCORR.
C
C                 TRGEPC is expressed as seconds past J2000 TDB.
C
C
C     SRFVEC      is the vector from the observer's position at ET to
C                 the aberration-corrected (or optionally, geometric)
C                 position of SPOINT, where the aberration corrections
C                 are specified by ABCORR. SRFVEC is expressed in the
C                 target body-fixed reference frame designated by
C                 FIXREF, evaluated at TRGEPC.
C  
C                 The components of SRFVEC are given in units of km.
C
C                 One can use the SPICELIB function VNORM to obtain the
C                 distance between the observer and SPOINT:
C
C                    DIST = VNORM ( SRFVEC )
C
C                 The observer's position OBSPOS, relative to the
C                 target body's center, where the center's position is
C                 corrected for aberration effects as indicated by
C                 ABCORR, can be computed via the call:
C
C                    CALL VSUB ( SPOINT, SRFVEC, OBSPOS )
C
C                 To transform the vector SRFVEC to a time-dependent
C                 reference frame REF at ET, a sequence of two frame
C                 transformations is required. For example, let MFIX
C                 and MREF be 3x3 matrices respectively describing the
C                 target body-fixed to J2000 frame transformation at
C                 TRGEPC and the J2000 to (time-dependent frame) REF
C                 transformation at ET, and let XFORM be the 3x3 matrix
C                 representing the composition of MREF with MFIX. Then
C                 SRFVEC can be transformed to the result REFVEC as
C                 follows:
C
C                     CALL PXFORM ( FIXREF,  'J2000', TRGEPC, MFIX   )
C                     CALL PXFORM ( 'J2000', REF,     ET,     MREF   )
C                     CALL MXM    ( MREF,    MFIX,            XFORM  )
C                     CALL MXV    ( XFORM,   SRFVEC,          REFVEC )
C
C
C     PHASE       is the phase angle at SPOINT, as seen from OBSRVR at
C                 time ET. This is the angle between the negative of
C                 the vector SRFVEC and the SPOINT-Sun vector at
C                 TRGEPC. Units are radians. The range of PHASE is
C                 [0, pi].  
C
C     SOLAR       is the solar incidence angle at SPOINT, as seen from
C                 OBSRVR at time ET. This is the angle between the
C                 surface normal vector at SPOINT and the SPOINT-Sun
C                 vector at TRGEPC. Units are radians. The range of
C                 SOLAR is [0, pi].  
C
C     EMISSN      is the emission angle at SPOINT, as seen from OBSRVR
C                 at time ET. This is the angle between the surface
C                 normal vector at SPOINT and the negative of the
C                 vector SRFVEC. Units are radians. The range of EMISSN
C                 is [0, pi]. 
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C
C     1)  If the specified aberration correction is relativistic or
C         calls for stellar aberration but not light time correction,
C         the error SPICE(NOTSUPPORTED) is signaled. If the specified
C         aberration correction is any other unrecognized value, the
C         error will be diagnosed and signaled by a routine in the call
C         tree of this routine.
C
C     2)  If either the target or observer input strings cannot be
C         converted to an integer ID code, the error
C         SPICE(IDCODENOTFOUND) is signaled.
C
C     3)  If OBSRVR and TARGET map to the same NAIF integer ID code,
C         the error SPICE(BODIESNOTDISTINCT) is signaled.
C
C     4)  If the input target body-fixed frame FIXREF is not
C         recognized, the error SPICE(NOFRAME) is signaled. A frame
C         name may fail to be recognized because a required frame
C         specification kernel has not been loaded; another cause is a
C         misspelling of the frame name.
C
C     5)  If the input frame FIXREF is not centered at the target body,
C         the error SPICE(INVALIDFRAME) is signaled.
C
C     6)  If the input argument METHOD is not recognized, the error
C         SPICE(INVALIDMETHOD) is signaled.
C
C     7)  If the target and observer have distinct identities but are
C         at the same location (for example, the target is Mars and the
C         observer is the Mars barycenter), the error
C         SPICE(NOSEPARATION) is signaled.
C
C     8)  If insufficient ephemeris data have been loaded prior to
C         calling ILUMIN, the error will be diagnosed and signaled by a
C         routine in the call tree of this routine. Note that when
C         light time correction is used, sufficient ephemeris data must
C         be available to propagate the states of observer, target, and
C         the Sun to the solar system barycenter.
C
C     9)  If the computation method specifies an ellipsoidal target
C         shape and triaxial radii of the target body have not been
C         loaded into the kernel pool prior to calling ILUMIN, the
C         error will be diagnosed and signaled by a routine in the call
C         tree of this routine.
C
C     10) The target must be an extended body: if any of the radii of
C         the target body are non-positive, the error will be
C         diagnosed and signaled by routines in the call tree of this
C         routine.
C
C     11) If PCK data specifying the target body-fixed frame
C         orientation have not been loaded prior to calling ILUMIN,
C         the error will be diagnosed and signaled by a routine in the
C         call tree of this routine.
C
C
C$ Files
C
C     Appropriate kernels must be loaded by the calling program before
C     this routine is called.
C
C     The following data are required:
C
C        - SPK data: ephemeris data for target, observer, and the
C          Sun must be loaded. If aberration corrections are used, the
C          states of target, observer, and the Sun relative to the
C          solar system barycenter must be calculable from the
C          available ephemeris data. Typically ephemeris data are made
C          available by loading one or more SPK files via FURNSH.
C
C        - PCK data: if the target body shape is modeled as an
C          ellipsoid, triaxial radii for the target body must be loaded
C          into the kernel pool. Typically this is done by loading a
C          text PCK file via FURNSH.
C
C        - Further PCK data: rotation data for the target body must be
C          loaded. These may be provided in a text or binary PCK file.
C
C        - Frame data: if a frame definition is required to convert the
C          observer and target states to the body-fixed frame of the
C          target, that definition must be available in the kernel
C          pool. Typically the definition is supplied by loading a
C          frame kernel via FURNSH.
C
C     In all cases, kernel data are normally loaded once per program
C     run, NOT every time this routine is called.
C
C
C$ Particulars
C
C
C     The term "illumination angles" refers to following set of
C     angles:
C
C
C        phase angle              Angle between the vectors from the
C                                 surface point to the observer and
C                                 from the surface point to the Sun.
C
C        solar incidence angle    Angle between the surface normal at
C                                 the specified surface point and the
C                                 vector from the surface point to the
C                                 Sun.
C
C        emission angle           Angle between the surface normal at
C                                 the specified surface point and the
C                                 vector from the surface point to the
C                                 observer.
C 
C     The diagram below illustrates the geometric relationships
C     defining these angles. The labels for the solar incidence,
C     emission, and phase angles are "s.i.", "e.", and "phase".
C
C
C                                                      *
C                                                     Sun
C
C                    surface normal vector
C                              ._                 _.
C                              |\                 /|  Sun vector
C                                \    phase      /
C                                 \   .    .    /
C                                 .            .
C                                   \   ___   /
C                              .     \/     \/
C                                    _\ s.i./
C                             .    /   \   /
C                             .   |  e. \ /
C         *             <--------------- *  surface point on
C      viewing            vector            target body
C      location           to viewing
C      (observer)         location
C
C
C     Note that if the target-observer vector, the target normal vector
C     at the surface point, and the target-sun vector are coplanar,
C     then phase is the sum of incidence and emission. This is rarely
C     true; usually
C
C        phase angle  <  solar incidence angle + emission angle
C
C     All of the above angles can be computed using light time
C     corrections, light time and stellar aberration corrections, or
C     no aberration corrections. In order to describe apparent
C     geometry as observed by a remote sensing instrument, both
C     light time and stellar aberration corrections should be used.
C     
C     The way aberration corrections are applied by this routine
C     is described below.
C
C        Light time corrections
C        ======================
C
C           Observer-target surface point vector
C           ------------------------------------
C
C           Let ET be the epoch at which an observation or remote
C           sensing measurement is made, and let ET - LT ("LT" stands
C           for "light time") be the epoch at which the photons
C           received at ET were emitted from the surface point SPOINT.
C           Note that the light time between the surface point and
C           observer will generally differ from the light time between
C           the target body's center and the observer.
C
C
C           Target body's orientation
C           -------------------------
C
C           Using the definitions of ET and LT above, the target body's
C           orientation at ET - LT is used. The surface normal is
C           dependent on the target body's orientation, so the body's
C           orientation model must be evaluated for the correct epoch.
C
C
C           Target body -- Sun vector
C           -------------------------
C
C           The surface features on the target body near SPOINT will
C           appear in a measurement made at ET as they were at ET-LT.
C           In particular, lighting on the target body is dependent on
C           the apparent location of the Sun as seen from the target
C           body at ET-LT. So, a second light time correction is used
C           to compute the position of the Sun relative to the surface
C           point.
C
C
C        Stellar aberration corrections
C        ==============================
C
C        Stellar aberration corrections are applied only if
C        light time corrections are applied as well.
C
C           Observer-target surface point body vector
C           -----------------------------------------
C
C           When stellar aberration correction is performed, the
C           direction vector SRFVEC is adjusted so as to point to the
C           apparent position of SPOINT: considering SPOINT to be an
C           ephemeris object, SRFVEC points from the observer's
C           position at ET to the light time and stellar aberration
C           corrected position of SPOINT.
C
C           Target body-Sun vector
C           ----------------------
C
C           The target body-Sun vector is the apparent position of the
C           Sun, corrected for light time and stellar aberration, as
C           seen from the target body at time ET-LT.  
C
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine 
C     specific arithmetic implementation. 
C
C     1) Find the phase, solar incidence, and emission angles at the
C        sub-solar and sub-spacecraft points on Mars as seen from the
C        Mars Global Surveyor spacecraft at a specified UTC time. Use
C        light time and stellar aberration corrections.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C           KPL/MK
C
C           File: mgs_example.tm
C
C           This meta-kernel is intended to support operation of SPICE
C           example programs. The kernels shown here should not be
C           assumed to contain adequate or correct versions of data
C           required by SPICE-based user applications.
C
C           In order for an application to use this meta-kernel, the
C           kernels referenced here must be present in the user's
C           current working directory.
C
C           The names and contents of the kernels referenced
C           by this meta-kernel are as follows:
C
C              File name                     Contents
C              ---------                     --------
C              de418.bsp                     Planetary ephemeris
C              pck00008.tpc                  Planet orientation and
C                                            radii
C              naif0008.tls                  Leapseconds
C              mgs_ext13_ipng_mgs95j.bsp     MGS ephemeris
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'de418.bsp',
C                                  'pck00008.tpc',
C                                  'naif0008.tls',
C                                  'mgs_ext13_ipng_mgs95j.bsp'  )
C           \begintext
C
C
C        Example code begins here.
C
C           PROGRAM ANGLES
C           IMPLICIT NONE
C     C
C     C     SPICELIB functions
C     C
C           DOUBLE PRECISION      DPR
C     C
C     C     Local parameters
C     C
C           CHARACTER*(*)         META
C           PARAMETER           ( META   = 'mgs_example.tm' )
C
C           INTEGER               NAMLEN
C           PARAMETER           ( NAMLEN = 32 )
C
C           INTEGER               TIMLEN
C           PARAMETER           ( TIMLEN = 25 )
C
C           INTEGER               CORLEN
C           PARAMETER           ( CORLEN = 5 )
C     C
C     C     Local variables
C     C
C           CHARACTER*(CORLEN)    ABCORR
C           CHARACTER*(NAMLEN)    OBSRVR
C           CHARACTER*(NAMLEN)    TARGET
C           CHARACTER*(TIMLEN)    UTC
C
C           DOUBLE PRECISION      ET
C           DOUBLE PRECISION      SRFVEC ( 3 )
C           DOUBLE PRECISION      SSCEMI
C           DOUBLE PRECISION      SSCPHS
C           DOUBLE PRECISION      SSCPT  ( 3 )
C           DOUBLE PRECISION      SSCSOL
C           DOUBLE PRECISION      SSLEMI
C           DOUBLE PRECISION      SSLPHS
C           DOUBLE PRECISION      SSLSOL
C           DOUBLE PRECISION      SSOLPT ( 3 )
C           DOUBLE PRECISION      TRGEPC
C
C     C
C     C     Load kernel files.
C     C
C           CALL FURNSH ( META )
C     C
C     C     Convert the UTC request time string to seconds past 
C     C     J2000 TDB.
C     C
C           UTC = '2004 JAN 1 12:00:00'
C
C           CALL UTC2ET ( UTC, ET )
C
C     C
C     C     Assign observer and target names. The acronym MGS
C     C     indicates Mars Global Surveyor. See NAIF_IDS for a
C     C     list of names recognized by SPICE. Also set the
C     C     aberration correction flag.
C     C
C           TARGET = 'Mars'
C           OBSRVR = 'MGS'
C           ABCORR = 'CN+S'
C     C
C     C     Find the sub-solar point on the Earth as seen from
C     C     the MGS spacecraft at ET. Use the "near point: ellipsoid"
C     C     style of sub-point definition. This makes it easy
C     C     to verify the solar incidence angle.
C     C
C           CALL SUBSLR ( 'Near point: ellipsoid',
C          .              TARGET,  ET,      'IAU_MARS',
C          .              ABCORR,  OBSRVR,  SSOLPT, TRGEPC, SRFVEC )
C     C
C     C     Now find the sub-spacecraft point.
C     C
C           CALL SUBPNT ( 'Near point: ellipsoid',
C          .              TARGET,  ET,     'IAU_MARS',
C          .              ABCORR,  OBSRVR, SSCPT,   TRGEPC, SRFVEC )
C     C
C     C     Find the phase, solar incidence, and emission
C     C     angles at the sub-solar point on the Earth as seen
C     C     from MGS at time ET.
C     C
C           CALL ILUMIN ( 'Ellipsoid', TARGET, ET,     'IAU_MARS',
C          .              ABCORR,      OBSRVR, SSOLPT, TRGEPC,
C          .              SRFVEC,      SSLPHS, SSLSOL, SSLEMI    )
C     C
C     C     Do the same for the sub-spacecraft point.
C     C
C           CALL ILUMIN ( 'Ellipsoid', TARGET, ET,     'IAU_MARS',
C          .              ABCORR,      OBSRVR, SSCPT,  TRGEPC,
C          .              SRFVEC,      SSCPHS, SSCSOL, SSCEMI    )
C     C
C     C     Convert the angles to degrees and write them out.
C     C
C           SSLPHS = DPR() * SSLPHS
C           SSLSOL = DPR() * SSLSOL
C           SSLEMI = DPR() * SSLEMI
C
C           SSCPHS = DPR() * SSCPHS
C           SSCSOL = DPR() * SSCSOL
C           SSCEMI = DPR() * SSCEMI
C
C           WRITE (*,*) ' '
C           WRITE (*,*) 'UTC epoch is ', UTC
C           WRITE (*,*) ' '
C           WRITE (*,*) 'Illumination angles at the sub-solar point:'
C           WRITE (*,*) ' '
C           WRITE (*,*) 'Phase angle           (deg.): ', SSLPHS
C           WRITE (*,*) 'Solar incidence angle (deg.): ', SSLSOL
C           WRITE (*,*) 'Emission angle        (deg.): ', SSLEMI
C           WRITE (*,*) ' '
C           WRITE (*,*) 'The solar incidence angle should be 0.'
C           WRITE (*,*) 'The emission and phase angles should be equal.'
C
C           WRITE (*,*) ' '
C           WRITE (*,*) 'Illumination angles at the sub-s/c point:'
C           WRITE (*,*) ' '
C           WRITE (*,*) 'Phase angle           (deg.): ', SSCPHS
C           WRITE (*,*) 'Solar incidence angle (deg.): ', SSCSOL
C           WRITE (*,*) 'Emission angle        (deg.): ', SSCEMI
C           WRITE (*,*) ' '
C           WRITE (*,*) 'The emission angle should be 0.'
C           WRITE (*,*) 'The solar incidence and phase angles should '
C          .//          'be equal.'
C           END
C
C
C     When this program was executed on a PC/Linux/g77 platform,
C     the output was:
C
C        UTC epoch is 2004 JAN 1 12:00:00
C
C        Illumination angles at the sub-solar point:
C
C        Phase angle           (deg.):   115.542001
C        Solar incidence angle (deg.):   3.20530645E-15
C        Emission angle        (deg.):   115.542001
C
C        The solar incidence angle should be 0.
C        The emission and phase angles should be equal.
C
C        Illumination angles at the sub-s/c point:
C
C        Phase angle           (deg.):   62.0840031
C        Solar incidence angle (deg.):   62.0840031
C        Emission angle        (deg.):   6.46461886E-11
C
C        The emission angle should be 0.
C        The solar incidence and phase angles should be equal.
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 17-MAY-2010 (NJB) 
C
C        Bug fix: ILUMIN now returns immediately if a target
C        radius lookup fails.
C
C-    SPICELIB Version 1.0.1, 06-FEB-2009 (NJB) 
C
C        Typo correction: changed FIXFRM to FIXREF in header
C        documentation. Meta-kernel name suffix was changed to
C        ".tm" in header code example.
C
C-    SPICELIB Version 1.0.0, 02-MAR-2008 (NJB)
C
C-&
 
C$ Index_Entries
C
C     illumination angles
C     lighting angles
C     phase angle
C     solar incidence angle
C     emission angle
C
C-&
 
C$ Revisions
C
C     None.
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      TOUCHD
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP
      
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME  =  'ILUMIN' )

C
C     This value will become system-dependent when systems
C     using 128-bit d.p. numbers are supported by SPICELIB.
C     CNVLIM, when added to 1.0D0, should yield 1.0D0. 
C
      DOUBLE PRECISION      CNVLIM
      PARAMETER           ( CNVLIM = 1.D-17 )
     
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MAXITR
      PARAMETER           ( MAXITR =  5 )

      INTEGER               SUN
      PARAMETER           ( SUN    = 10 )
      
C
C     Local variables
C
      CHARACTER*(LNSIZE)    LOCMTH
      CHARACTER*(LNSIZE)    PRVMTH
      CHARACTER*(CORLEN)    PRVCOR

      DOUBLE PRECISION      CORPOS ( 3 )
      DOUBLE PRECISION      CORVJ2 ( 3 )
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      ETDIFF
      DOUBLE PRECISION      J2POS  ( 3 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LTDIFF
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      OBSPOS ( 3 )
      DOUBLE PRECISION      OFFOBS ( 3 )
      DOUBLE PRECISION      OFFSUN ( 3 )
      DOUBLE PRECISION      PREVET
      DOUBLE PRECISION      PREVLT
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RANGE
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SLT
      DOUBLE PRECISION      SSBOST ( 6 )
      DOUBLE PRECISION      SSBTST ( 6 )
      DOUBLE PRECISION      STLOFF ( 3 )
      DOUBLE PRECISION      SUBVEC ( 3 )
      DOUBLE PRECISION      SUBVJ2 ( 3 )
      DOUBLE PRECISION      SUNPOS ( 3 )
      DOUBLE PRECISION      TPOS   ( 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      XFORM  ( 3, 3 )

      INTEGER               CENTER
      INTEGER               I
      INTEGER               N
      INTEGER               NITR
      INTEGER               OBSCDE
      INTEGER               REFCDE
      INTEGER               TRGCDE
      INTEGER               TYPE
      INTEGER               TYPEID
      
      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               ELIPSD
      LOGICAL               FIRST
      LOGICAL               FND
      LOGICAL               USECN
      LOGICAL               USELT
      LOGICAL               USESTL
      LOGICAL               XMIT

C
C     Saved variables
C
      SAVE                  ELIPSD
      SAVE                  FIRST
      SAVE                  PRVCOR
      SAVE                  PRVMTH
      SAVE                  USECN
      SAVE                  USELT
      SAVE                  USESTL

C
C     Note: XMIT need not be saved, since it's used only 
C     for error checking when an aberration correction flag
C     is parsed.
C

C
C     Initial values
C
      DATA                  ELIPSD  / .TRUE. /
      DATA                  FIRST   / .TRUE. /
      DATA                  PRVCOR  / ' '    /
      DATA                  PRVMTH  / 'Ellipsoid' /

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( RNAME )
  

      IF (  FIRST  .OR.  ( ABCORR .NE. PRVCOR )  ) THEN
C
C        The aberration correction flag differs from the value it
C        had on the previous call, if any. Analyze the new flag.
C
         CALL ZZPRSCOR ( ABCORR, ATTBLK )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

C
C        The aberration correction flag is recognized; save it.
C
         PRVCOR = ABCORR

C
C        Set logical flags indicating the attributes of the requested
C        correction:
C
C           XMIT is .TRUE. when the correction is for transmitted
C           radiation.
C
C           USELT is .TRUE. when any type of light time correction
C           (normal or converged Newtonian) is specified.
C
C           USECN indicates converged Newtonian light time correction.
C
C           USESTL indicates stellar aberration corrections.
C
C
C        The above definitions are consistent with those used by
C        ZZPRSCOR.
C 
         XMIT    =  ATTBLK ( XMTIDX )
         USELT   =  ATTBLK ( LTIDX  )
         USECN   =  ATTBLK ( CNVIDX )
         USESTL  =  ATTBLK ( STLIDX )
C
C        Reject an aberration correction flag calling for transmission
C        corrections.
C
         IF ( XMIT ) THEN

            CALL SETMSG ( 'Aberration correction flag # calls for '
     .      //            'transmission-style corrections.'        )
            CALL ERRCH  ( '#', ABCORR                              )
            CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                    )
            CALL CHKOUT ( RNAME                                    )
            RETURN

         END IF

C
C        Reject an aberration correction flag calling for stellar 
C        aberration but not light time correction.
C
         IF (  USESTL  .AND.  ( .NOT. USELT )  ) THEN

            CALL SETMSG ( 'Aberration correction flag # calls for '
     .      //            'stellar aberration but not light time '
     .      //            'corrections. This combination is not '
     .      //            'expected.'                              )
            CALL ERRCH  ( '#', ABCORR                              )
            CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                    )
            CALL CHKOUT ( RNAME                                    )
            RETURN

         ELSE IF ( ATTBLK(RELIDX) ) THEN
C
C           Also reject flags calling for relativistic corrections.
C
            CALL SETMSG ( 'Aberration correction flag # calls for '
     .      //            'relativistic light time correction.'    )
            CALL ERRCH  ( '#', ABCORR                              )
            CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                    )
            CALL CHKOUT ( RNAME                                    )
            RETURN

         END IF

      END IF

C
C     Obtain integer codes for the target and observer.
C 
      CALL BODS2C ( TARGET, TRGCDE, FND )
            
      IF ( .NOT. FND ) THEN
      
         CALL SETMSG ( 'The target, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', TARGET                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( RNAME                                      )
         RETURN
      
      END IF
      
      
      CALL BODS2C ( OBSRVR, OBSCDE, FND )
      
      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'The observer, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', OBSRVR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( RNAME                                      )
         RETURN
      
      END IF
      
C
C     Check the input body codes. If they are equal, signal
C     an error.
C
      IF ( OBSCDE .EQ. TRGCDE ) THEN
 
         CALL SETMSG ( 'In computing the sub-solar point, '   
     .   //            'the observing body and target body are the '
     .   //            'same. Both are #.'                          )
         CALL ERRCH  ( '#',  OBSRVR                                 )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'                   )
         CALL CHKOUT ( RNAME                                        )
         RETURN
 
      END IF

C
C     Determine the attributes of the frame designated by FIXREF.
C
      CALL NAMFRM ( FIXREF, REFCDE )

      CALL FRINFO ( REFCDE, CENTER, TYPE, TYPEID, FND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF

      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'Reference frame # is not recognized by ' //
     .                 'the SPICE frame subsystem. Possibly '    //
     .                 'a required frame definition kernel has ' //
     .                 'not been loaded.'                        )
         CALL ERRCH  ( '#',  FIXREF                              )
         CALL SIGERR ( 'SPICE(NOFRAME)'                          )
         CALL CHKOUT ( RNAME                                     )
         RETURN

      END IF

C
C     Make sure that FIXREF is centered at the target body's center.
C
      IF ( CENTER .NE. TRGCDE ) THEN

         CALL SETMSG ( 'Reference frame # is not centered at the ' 
     .   //            'target body #. The ID code of the frame '
     .   //            'center is #.'                             )
         CALL ERRCH  ( '#',  FIXREF                               )
         CALL ERRCH  ( '#',  TARGET                               )
         CALL ERRINT ( '#',  CENTER                               )
         CALL SIGERR ( 'SPICE(INVALIDFRAME)'                      )
         CALL CHKOUT ( RNAME                                      )
         RETURN

      END IF

C
C     If necessary, parse the method specification. PRVMTH
C     and the derived flags NEAR and ELIPSD start out with
C     valid values. PRVMTH records the last valid value of
C     METHOD; ELIPSD is the corresponding shape flag.
C
      IF ( METHOD .NE. PRVMTH ) THEN         
C
C        Parse the computation method specification. Work with a local
C        copy of the method specification that contains no leading or
C        embedded blanks.
C
         CALL CMPRSS ( ' ', 0, METHOD, LOCMTH )
         CALL UCASE  ( LOCMTH,         LOCMTH )

C
C        Check the shape specification.
C

         IF ( LOCMTH .NE. 'ELLIPSOID'  ) THEN

            CALL SETMSG ( 'Computation method argument was <#>; this ' 
     .      //            'string must specify a supported shape '
     .      //            'model and computation type. See the '
     .      //            'header of SUBSLR for details.'            )
            CALL ERRCH  ( '#',  METHOD                               )
            CALL SIGERR ( 'SPICE(INVALIDMETHOD)'                     )
            CALL CHKOUT ( RNAME                                      )
            RETURN

         END IF

C
C        At this point the method specification has passed our tests.
C        Use the flag ELIPSD to indicate that the shape is modeled as
C        an ellipsoid (which is true, for now).
C
         ELIPSD = .TRUE.
C
C        Save the current value of METHOD.
C
         PRVMTH = METHOD

      END IF

C
C     Get the sign S prefixing LT in the expression for TRGEPC.
C     When light time correction is not used, setting S = 0
C     allows us to seamlessly set TRGEPC equal to ET.
C
      IF ( USELT ) THEN
         S = -1.D0         
      ELSE
         S =  0.D0
      END IF
 
C
C     Determine the position of the observer in target body-fixed
C     coordinates. This is a first estimate.
C
C         -  Call SPKEZP to compute the position of the target body as
C            seen from the observing body and the light time (LT)
C            between them. We request that the coordinates of POS be
C            returned relative to the body fixed reference frame
C            associated with the target body, using aberration
C            corrections specified by the input argument ABCORR.
C
C         -  Call VMINUS to negate the direction of the vector (OBSPOS)
C            so it will be the position of the observer as seen from
C            the target body in target body fixed coordinates.
C
C            Note that this result is not the same as the result of
C            calling SPKEZP with the target and observer switched. We
C            computed the vector FROM the observer TO the target in
C            order to get the proper light time and stellar aberration
C            corrections (if requested). Now we need the inverse of
C            that corrected vector in order to compute the sub-solar
C            point.
C
      CALL SPKEZP ( TRGCDE, ET, FIXREF, ABCORR, OBSCDE, TPOS, LT )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF

C
C     Negate the target's position to obtain the position of the
C     observer relative to the target.
C
      CALL VMINUS ( TPOS, OBSPOS )

      RANGE  = VNORM ( OBSPOS )

      IF ( RANGE .EQ. 0.D0 ) THEN
C
C        We've already ensured that observer and target are
C        distinct, so this should be a very unusual occurrence.
C
         CALL SETMSG ( 'Observer-target distance is zero. '
     .   //            'Observer is #; target is #.'       )
         CALL ERRCH  ( '#', OBSRVR                         )
         CALL ERRCH  ( '#', TARGET                         )
         CALL SIGERR ( 'SPICE(NOSEPARATION)'               )
         CALL CHKOUT ( RNAME                               )
         RETURN

      END IF

C
C     Make a first estimate of the light time and target epoch. Note
C     that TRGEPC will equal ET if we're performing an uncorrected
C     computation, since in that case, S will be zero.
C
      CALL VSUB ( SPOINT, OBSPOS, SRFVEC )

      DIST   = VNORM ( SRFVEC )
      LT     = DIST /  CLIGHT()

      TRGEPC = ET + S*LT

C
C     If we're using light time corrections, refine our light time,
C     target epoch, and observer position estimates.
C
      IF ( USELT ) THEN 
C
C        We'll now make improved light time, target epoch, and observer
C        position estimates using the previous estimates. The number of
C        iterations depends on the light time correction type.  
C
         IF ( USECN ) THEN
            NITR = MAXITR
         ELSE
            NITR = 1
         END IF

C
C        Get the J2000-relative state of the observer relative to
C        the solar system barycenter at ET.
C
         CALL SPKSSB ( OBSCDE, ET, 'J2000', SSBOST )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

C
C        Initialize the variables required to evaluate the 
C        loop termination condition.
C
         I      = 0
         LTDIFF = 1.D0
         ETDIFF = 1.D0
         PREVLT = LT
         PREVET = TRGEPC

         DO WHILE (       ( I      .LT.   NITR                ) 
     .              .AND. ( LTDIFF .GT. ( CNVLIM * ABS(LT) )  )
     .              .AND. ( ETDIFF .GT.   0.D0                )  )
C
C           Get the J2000-relative state of the target relative to
C           the solar system barycenter at the target epoch.
C
            CALL SPKSSB ( TRGCDE, TRGEPC, 'J2000', SSBTST )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

C
C           Find the position of the observer relative to the target.
C           Convert this vector from the J2000 frame to the target
C           frame at TRGEPC.
C
            CALL VSUB   ( SSBOST,  SSBTST, J2POS         )
            CALL PXFORM ( 'J2000', FIXREF, TRGEPC, XFORM )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

            CALL MXV ( XFORM, J2POS, OBSPOS )

C
C           If we're using stellar aberration corrections, adjust the
C           observer position to account for the stellar aberration
C           correction applicable to SPOINT.
C
            IF ( USESTL ) THEN
C
C              We want to apply the stellar aberration correction that
C              applies to our current estimate of the sub-solar point
C              location, NOT the correction for the target body's
C              center. In most cases the two corrections will be
C              similar, but they might not be---consider the case of a
C              highly prolate target body where the observer is close
C              to one "end" of the body.
C
C              Find the vector from the observer to the estimated
C              sub-solar point. Find the stellar aberration offset
C              STLOFF for this vector. Note that all vectors are
C              expressed relative to the target body-fixed frame at
C              TRGEPC. We must perform our corrections in an inertial
C              frame.
C
               CALL VSUB ( SPOINT, OBSPOS, SUBVEC )

               CALL MTXV ( XFORM,  SUBVEC, SUBVJ2 )

C
C              Note that we don't handle the transmission
C              case here.
C
               CALL STELAB ( SUBVJ2, SSBOST(4), CORVJ2 )

               CALL MXV  ( XFORM,  CORVJ2, CORPOS )
               CALL VSUB ( CORPOS, SUBVEC, STLOFF )

C
C              In principle, we want to shift the target body position
C              relative to the solar system barycenter by STLOFF, but
C              we can skip this step and just re-compute the observer's
C              location relative to the target body's center by
C              subtracting off STLOFF.
C             
               CALL VSUB ( OBSPOS, STLOFF, VTEMP  )
               CALL VEQU ( VTEMP,          OBSPOS )

            END IF

            DIST = VDIST ( OBSPOS, SPOINT )

C
C           Compute a new light time estimate and new target epoch.
C
            LT      =  DIST / CLIGHT()
            TRGEPC  =  ET   + S*LT

C
C           At this point, we have new estimates of the sub-solar point
C           SPOINT, the observer altitude DIST, the target epoch TRGEPC,
C           and the position of the observer relative to the target
C           OBSPOS.
C
C           We use the d.p. identity function TOUCHD to force the
C           compiler to create double precision arguments from the
C           differences LT-PREVLT and TRGEPC-PREVET. Some compilers
C           will perform extended-precision register arithmetic, which
C           can prevent a difference from rounding to zero. Simply
C           storing the result of the subtraction in a double precision
C           variable doesn't solve the problem, because that variable
C           can be optimized out of existence.
C
            LTDIFF  =   ABS( TOUCHD(LT     - PREVLT) )
            ETDIFF  =   ABS( TOUCHD(TRGEPC - PREVET) )
            PREVLT  =   LT
            PREVET  =   TRGEPC        
            I       =   I + 1

         END DO
                 
      END IF
 
C
C     Find the body-fixed position of the Sun as seen from the target
C     at TRGEPC.
C
      CALL SPKEZP ( SUN, TRGEPC, FIXREF, ABCORR, TRGCDE, SUNPOS, SLT )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF

C
C     Now we'll modify the target-Sun vector to take into account the
C     offset between the target center and the surface point of
C     interest; we want the vector to point from the surface point to
C     Sun.
C
      CALL VSUB ( SUNPOS, SPOINT, OFFSUN )
 
C
C     Let OFFOBS be the offset observer position: this vector
C     points from SPOINT to the observer.
C     
      CALL VSUB   ( SPOINT, OBSPOS, SRFVEC )
      CALL VMINUS ( SRFVEC, OFFOBS )

C
C     Find the surface normal at SPOINT. This computation depends
C     on target body shape model.  
C
      IF ( ELIPSD ) THEN
C
C        We'll need the radii of the target body.
C
         CALL BODVCD ( TRGCDE, 'RADII', 3, N, RADII )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

         CALL SURFNM ( RADII(1), RADII(2), RADII(3), SPOINT, NORMAL )
       
      ELSE
C
C        We've already checked the computation method input argument,
C        so we don't expect to arrive here. This code is present for
C        safety.
C      
         CALL SETMSG ( 'The computation method # was not recognized. ' )
         CALL ERRCH  ( '#',  METHOD                                    )
         CALL SIGERR ( 'SPICE(INVALIDMETHOD)'                          )
         CALL CHKOUT ( RNAME                                           )
         RETURN
         
      END IF

C
C     Find the illumination angles. VSEP will give us angular
C     separation in radians.
C
      PHASE   =  VSEP ( OFFSUN, OFFOBS )
      SOLAR   =  VSEP ( NORMAL, OFFSUN )
      EMISSN  =  VSEP ( NORMAL, OFFOBS )

C
C     TRGEPC and SRFVEC have already been set.
C
      CALL CHKOUT ( RNAME )
      RETURN
      END
