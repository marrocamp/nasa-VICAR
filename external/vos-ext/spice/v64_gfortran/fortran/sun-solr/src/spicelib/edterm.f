C$Procedure EDTERM ( Ellipsoid terminator )
 
      SUBROUTINE EDTERM ( TRMTYP, SOURCE, TARGET, ET,      
     .                    FIXFRM, ABCORR, OBSRVR, NPTS, 
     .                    TRGEPC, OBSPOS, TRMPTS        )
  
C$ Abstract
C
C     Compute a set of points on the umbral or penumbral terminator of
C     a specified target body, where the target shape is modeled as an
C     ellipsoid.
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
C     ELLIPSES
C
C$ Keywords
C
C     BODY
C     GEOMETRY
C     MATH
C
C$ Declarations
 
      IMPLICIT NONE

      CHARACTER*(*)         TRMTYP
      CHARACTER*(*)         SOURCE
      CHARACTER*(*)         TARGET
      DOUBLE PRECISION      ET
      CHARACTER*(*)         FIXFRM
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      INTEGER               NPTS
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      OBSPOS ( 3 )
      DOUBLE PRECISION      TRMPTS ( 3, NPTS )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TRMTYP     I   Terminator type.
C     SOURCE     I   Light source.
C     TARGET     I   Target body.
C     ET         I   Observation epoch.
C     FIXFRM     I   Body-fixed frame associated with target.
C     ABCORR     I   Aberration correction.
C     OBSRVR     I   Observer.
C     NPTS       I   Number of points in terminator set.
C     TRGEPC     O   Epoch associated with target center.
C     OBSPOS     O   Position of observer in body-fixed frame.
C     TRMPTS     O   Terminator point set.
C   
C$ Detailed_Input
C
C     TRMTYP      is a string indicating the type of terminator to
C                 compute:  umbral or penumbral.  The umbral terminator
C                 is the boundary of the portion of the ellipsoid
C                 surface in total shadow. The penumbral terminator is
C                 the boundary of the portion of the surface that is
C                 completely illuminated.  Note that in astronomy
C                 references, the unqualified word "terminator" refers
C                 to the umbral terminator.  Here, the unqualified 
C                 word refers to either type of terminator.
C
C                 Possible values of TRMTYP are
C
C                    'UMBRAL' 
C                    'PENUMBRAL'
C
C                 Case and leading or trailing blanks in TRMTYP are
C                 not significant.
C
C
C     SOURCE      is the name of the body acting as a light source.
C                 SOURCE is case-insensitive, and leading and trailing
C                 blanks in TARGET are not significant. Optionally, you
C                 may supply a string containing the integer ID code
C                 for the object.  For example both 'SUN' and '10' are
C                 legitimate strings that indicate the Sun is the light
C                 source.
C
C                 This routine assumes that a kernel variable
C                 representing the light source's radii is present in
C                 the kernel pool.  Normally the kernel variable would
C                 be defined by loading a PCK file.
C
C                 The shape of the light source is always modeled as a
C                 sphere, regardless of whether radii defining a
C                 triaxial ellipsoidal shape model are available in the
C                 kernel pool.  The maximum radius of the body is used
C                 as the radius of the sphere.
C
C
C     TARGET      is the name of the target body.  TARGET is
C                 case-insensitive, and leading and trailing blanks in
C                 TARGET are not significant. Optionally, you may
C                 supply a string containing the integer ID code for
C                 the object.  For example both 'MOON' and '301' are
C                 legitimate strings that indicate the moon is the
C                 target body.
C
C                 This routine assumes that a kernel variable
C                 representing the target's radii is present in the
C                 kernel pool.  Normally the kernel variable would be
C                 defined by loading a PCK file.
C
C
C     ET          is the epoch of participation of the observer,
C                 expressed as ephemeris seconds past J2000 TDB: ET is
C                 the epoch at which the observer's position is
C                 computed.
C
C                 When aberration corrections are not used, ET is also
C                 the epoch at which the position and orientation of the
C                 target body and position of the light source are
C                 computed.
C
C                 When aberration corrections are used, ET is the epoch
C                 at which the observer's position relative to the solar
C                 system barycenter is computed; in this case the
C                 position and orientation of the target body are
C                 computed at ET-LT or ET+LT, where LT is the one-way
C                 light time between the target body's center and the
C                 observer, and the sign applied to LT depends on the
C                 selected correction. See the description of ABCORR
C                 below for details.
C
C
C     FIXFRM      is the name of the reference frame relative to which
C                 the output terminator points are expressed. This must
C                 a body-centered, body-fixed frame associated with the
C                 target.  The frame's axes must be compatible with the
C                 triaxial ellipsoidal shape model associated with the
C                 target body (normally provide via a PCK): this
C                 routine assumes that the first, second, and third
C                 axis lengths correspond, respectively, to the x, y,
C                 and z-axes of the frame designated by FIXFRM.
C
C                 FIXFRM may refer to a built-in frame (documented in
C                 the Frames Required Reading) or a frame defined by a
C                 loaded frame kernel (FK).
C
C                 The orientation of the frame designated by FIXFRM is
C                 evaluated at epoch of participation of the target
C                 body.  See the descriptions of ET and ABCORR for
C                 details.
C
C
C     ABCORR      indicates the aberration correction to be applied
C                 when computing the observer-target position, the
C                 orientation of the target body, and the target-
C                 source position vector.  ABCORR may be any of
C                 the following.
C
C                    'NONE'     Apply no correction.  Compute the 
C                               terminator points using the position
C                               of the light source and target, and
C                               the orientation of the target, at ET.
C
C                 Let LT represent the one-way light time between the
C                 observer and the target body's center. The following
C                 values of ABCORR apply to the "reception" case in
C                 which photons depart from the target body's center at
C                 the light-time corrected epoch ET-LT and *arrive* at
C                 the observer's location at ET:
C
C
C                    'LT'       Correct for one-way light time (also
C                               called "planetary aberration") using a
C                               Newtonian formulation. This correction
C                               yields the location of the terminator
C                               points at the approximate time they
C                               emitted photons arriving at the
C                               observer at ET (the difference between
C                               light time to the target center and
C                               light time to the terminator points
C                               is ignored).
C 
C                               The light time correction uses an
C                               iterative solution of the light time
C                               equation. The solution invoked by the
C                               'LT' option uses one iteration.
C
C                               The target position as seen by the
C                               observer, the position of the light
C                               source as seen from the target at
C                               ET-LT, and the rotation of the target
C                               body, are corrected for light time.
C
C                    'LT+S'     Correct for one-way light time and
C                               stellar aberration using a Newtonian
C                               formulation. This option modifies the
C                               positions obtained with the 'LT' option
C                               to account for the observer's velocity
C                               relative to the solar system
C                               barycenter.  This correction also
C                               applies to the position of the light
C                               source relative to the target.  The
C                               result is the apparent terminator as
C                               seen by the observer.
C
C                    'CN'       Converged Newtonian light time
C                               correction.  In solving the light time
C                               equation, the 'CN' correction iterates
C                               until the solution converges.  The
C                               position and rotation of the target
C                               body and the position of the light
C                               source relative to the target are
C                               corrected for light time.
C
C                    'CN+S'     Converged Newtonian light time
C                               and stellar aberration corrections.
C
C
C     OBSRVR      is the name of the observing body.  This is typically
C                 a spacecraft, the Earth, or a surface point on the
C                 Earth.  OBSRVR is case-insensitive, and leading and
C                 trailing blanks in OBSRVR are not significant.
C                 Optionally, you may supply a string containing the
C                 integer ID code for the object.  For example both
C                 'EARTH' and '399' are legitimate strings that indicate
C                 the Earth is the observer.
C
C                    
C     NPTS        is the number of terminator points to compute.
C
C     
C$ Detailed_Output
C
C     TRGEPC      is the "target epoch."  TRGEPC is defined as follows:
C                 letting LT be the one-way light time between the
C                 target center and observer, TRGEPC is either the
C                 epoch ET-LT or ET depending on whether the requested
C                 aberration correction is, respectively, for received
C                 radiation or omitted.  LT is computed using the
C                 method indicated by ABCORR.
C
C                 TRGEPC is expressed as seconds past J2000 TDB.
C
C
C     OBSPOS      is the vector from the center of the target body at
C                 epoch TRGEPC to the observer at epoch ET.  OBSPOS is
C                 expressed in the target body-fixed reference frame
C                 FIXFRM, which is evaluated at TRGEPC.
C
C                 OBSPOS is returned to simplify various related
C                 computations that would otherwise be cumbersome.  For
C                 example, the vector XVEC from the observer to the
C                 Ith terminator point can be calculated via the call
C
C                    CALL VMINUS ( TRMPTS(1,I), OBSPOS, XVEC )
C
C                 The components of OBSPOS are given in units of km.
C
C
C     TRMPTS      is an array of points on the umbral or penumbral
C                 terminator of the ellipsoid, as specified by the
C                 input argument TRMTYP.  The Ith point is contained in
C                 the array elements
C
C                     TRMPTS(J,I),  J = 1, 2, 3
C
C                 Each terminator point is the point of tangency of a
C                 plane that is also tangent to the light source. These
C                 associated points of tangency on the light source
C                 have uniform distribution in longitude when expressed
C                 in a cylindrical coordinate system whose Z-axis is
C                 OBSPOS.  The magnitude of the separation in longitude
C                 between the tangency points on the light source is
C
C                    2*Pi / NPTS 
C
C                 If the target is spherical, the terminator points
C                 also are uniformly distributed in longitude in the
C                 cylindrical system described above.  If the target is
C                 non-spherical, the longitude distribution of the
C                 points generally is not uniform.
C                                        
C                 The terminator points are expressed in the body-fixed
C                 reference frame designated by FIXFRM.  Units are km.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input frame name FIXFRM cannot be mapped
C         to a frame ID code, the error SPICE(NOTRANSLATION) is
C         signaled.
C
C     2)  If the target name TARGET cannot be mapped
C         to a body ID code, the error SPICE(NOTRANSLATION) is
C         signaled.
C
C     3)  If the frame designated by FIXFRM is not centered
C         on the target, the error SPICE(INVALIDFIXFRM) is
C         signaled.
C
C     4)  If the terminator type is not recognized, the error
C         will be diagnosed by a routine in the call tree of
C         this routine.
C
C     5)  If the set size NPTS is not at least 1, the error
C         will be diagnosed by a routine in the call tree of
C         this routine.
C
C     6)  If any of the ellipsoid's semi-axis lengths is non-positive,
C         the error will be diagnosed by a routine in the call tree of
C         this routine.
C
C     7)  If the light source has non-positive radius, the error
C         will be diagnosed by a routine in the call tree of
C         this routine.
C
C     8)  If the light source intersects the smallest sphere
C         centered at the origin and containing the ellipsoid, the
C         error will be diagnosed by a routine in the call tree of
C         this routine.
C
C     9)  If radii for the target body or light source are not
C         available in the kernel pool, the error will be diagnosed by
C         a routine in the call tree of this routine.  If radii are
C         available but either body does not have three radii, the
C         error SPICE(INVALIDCOUNT) will be signaled.
C
C     10) If any SPK look-up fails, the error will be diagnosed by
C         a routine in the call tree of this routine.
C
C$ Files
C
C     Appropriate SPK, PCK, and frame kernels must be loaded by the
C     calling program before this routine is called. 
C
C     The following data are required:
C
C        - SPK data: ephemeris data for target, observer, and light
C          source must be loaded. If aberration corrections are used,
C          the states of all three objects relative to the solar system
C          barycenter must be calculable from the available ephemeris
C          data. Typically ephemeris data are made available by loading
C          one or more SPK files via FURNSH.
C
C        - PCK data: triaxial radii for the target body and
C          the light source must be loaded into the kernel pool.
C          Typically this is done by loading a text PCK file via
C          FURNSH.
C
C        - Further PCK data:  rotation data for the target body must
C          be loaded.  These may be provided in a text or binary PCK
C          file. 
C
C        - Frame data:  if a frame definition is required to convert
C          the observer and target states to the target body-fixed
C          frame designated by FIXFRM, that definition must be
C          available in the kernel pool.  Typically the definitions of
C          frames not already built-in to SPICE are supplied by loading
C          a frame kernel.
C
C     In all cases, kernel data are normally loaded once per program
C     run, NOT every time this routine is called.
C
C$ Particulars
C
C     This routine models the boundaries of shadow regions on an
C     ellipsoidal target body "illuminated" by a spherical light
C     source.  Light rays are assumed to travel along straight lines;
C     refraction is not modeled.
C
C     Points on the target body's surface at which the entire cap of
C     the light source is visible are considered to be completely
C     illuminated. Points on the target's surface at which some portion
C     (or all) of the cap of the light source are blocked are
C     considered to be in partial (or total) shadow.
C
C     In this routine, we use the term "umbral terminator" to denote
C     the curve ususally called the "terminator":  this curve is the
C     boundary of the portion of the target body's surface that lies in
C     total shadow. We use the term "penumbral terminator" to denote
C     the boundary of the completely illuminated portion of the
C     surface.
C
C     In general, the terminator on an ellipsoid is a more complicated
C     curve than the limb (which is always an ellipse).  Aside from
C     various special cases, the terminator does not lie in a plane.
C
C     However, the condition for a point X on the ellipsoid to lie on
C     the terminator is simple:  a plane tangent to the ellipsoid at X
C     must also be tangent to the light source.  If this tangent plane
C     does not intersect the vector from the center of the ellipsoid to
C     the center of the light source, then X lies on the umbral
C     terminator; otherwise X lies on the penumbral terminator.
C
C$ Examples
C
C     1)  Compute a set of umbral terminator points on the Moon.
C         Perform a consistency check using the solar incidence angle
C         at each point.  We expect to see a solar incidence angle of
C         approximately 90 degrees.  Since the solar incidence angle is
C         measured between the local outward normal and the direction
C         to the Sun, the solar incidence angle at an umbral terminator
C         point should exceed 90 degrees by approximately the angular
C         radius of the Sun.
C
C         This program loads SPICE kernels via a meta-kernel.  The '
C         contents of the meta-kernel used to produce the results shown
C         below

C
C               \begindata
C
C                  KERNELS_TO_LOAD = ( 'naif0008.tls'
C                                      'pck00008.tpc'
C                                      'de405s.bsp'  )
C               \begintext
C
C
C         Program source code:
C
C            PROGRAM EX1
C            IMPLICIT NONE
C
C            DOUBLE PRECISION      DPR
C            DOUBLE PRECISION      VDIST
C
C            CHARACTER*(*)         META
C            PARAMETER           ( META   = 'ex1_meta.ker' )
C
C            INTEGER               NPTS
C            PARAMETER           ( NPTS   = 3 )
C
C            INTEGER               CORLEN
C            PARAMETER           ( CORLEN = 5 )
C
C            INTEGER               BDNMLN
C            PARAMETER           ( BDNMLN = 36 )
C
C            INTEGER               FRNMLN
C            PARAMETER           ( FRNMLN = 32 )
C
C            INTEGER               TIMLEN
C            PARAMETER           ( TIMLEN = 50 )
C
C            CHARACTER*(CORLEN)    ABCORR
C            CHARACTER*(FRNMLN)    FRAME
C            CHARACTER*(BDNMLN)    SOURCE
C            CHARACTER*(BDNMLN)    TARGET
C            CHARACTER*(BDNMLN)    OBSRVR
C            CHARACTER*(TIMLEN)    UTC
C
C            DOUBLE PRECISION      ANGRAD
C            DOUBLE PRECISION      EMISSN
C            DOUBLE PRECISION      ET
C            DOUBLE PRECISION      LAT
C            DOUBLE PRECISION      LON
C            DOUBLE PRECISION      LT
C            DOUBLE PRECISION      OBSPOS ( 3 )
C            DOUBLE PRECISION      PHASE
C            DOUBLE PRECISION      RADIUS
C            DOUBLE PRECISION      SOLAR
C            DOUBLE PRECISION      SRCPOS ( 3 )
C            DOUBLE PRECISION      SRCRAD ( 3 )
C            DOUBLE PRECISION      TRGEPC
C            DOUBLE PRECISION      TRMPTS ( 3, NPTS )
C
C            INTEGER               I
C            INTEGER               N
C
C
C            CALL FURNSH ( META )
C
C            UTC    = '2007 FEB 3 00:00:00.000'
C
C            CALL STR2ET ( UTC, ET )
C
C            OBSRVR = 'EARTH'
C            TARGET = 'MOON'
C            SOURCE = 'SUN'
C            FRAME  = 'IAU_MOON'
C            ABCORR = 'LT+S'
C
C            CALL EDTERM ( 'UMBRAL', SOURCE, TARGET,
C           .              ET,       FRAME,  ABCORR,
C           .              OBSRVR,   NPTS,   TRGEPC,
C           .              OBSPOS,   TRMPTS         )
C
C      C
C      C     Find the angular radius of the Sun as
C      C     seen from the target.  First, look up
C      C     the target-sun vector.
C      C
C            CALL SPKPOS ( SOURCE, TRGEPC, FRAME,
C           .              ABCORR, TARGET, SRCPOS, LT )
C
C      C
C      C     Look up the radii of the Sun.
C      C
C            CALL BODVRD ( SOURCE, 'RADII', 3, N, SRCRAD )
C
C            DO I = 1, NPTS
C
C               WRITE (*,*) ' '
C
C               CALL RECLAT ( TRMPTS(1,I), RADIUS, LON, LAT )
C
C               WRITE (*,*) 'Terminator point ', I, ':'
C               WRITE (*,*) '   Radius                     (km):  ',
C           .                   RADIUS
C               WRITE (*,*) '   Planetocentric longitude   (deg): ',
C           .                   LON*DPR()
C               WRITE (*,*) '   Planetocentric latitude    (deg): ',
C           .                   LAT*DPR()
C
C      C
C      C        Find the illumination angles at the
C      C        Ith terminator point.
C      C
C               CALL ILLUM  ( TARGET, ET,         ABCORR,
C           .                 OBSRVR, TRMPTS(1,I), PHASE,
C           .                 SOLAR,  EMISSN              )
C
C               WRITE (*,*)
C           .               '   Solar incidence angle      (deg): ',
C           .                   SOLAR*DPR()
C
C      C
C      C        Find the angular radius of the Sun as seen from
C      C        the terminator point.
C      C
C               ANGRAD = ASIN (   SRCRAD(1)
C           .                   / VDIST ( SRCPOS,TRMPTS(1,I) )  )
C
C      C
C      C        Display the solar incidence angle after
C      C        subtracting the angular radius of the Sun
C      C        as seen from the terminator point.  The
C      C        result should be approximately 90 degrees.
C      C
C               WRITE (*, '(1X,A,2PE22.14)')
C           .               '   Minus Sun''s ' //
C           .               'angular radius (deg): ',
C           .               (SOLAR-ANGRAD) * DPR()
C
C            END DO
C
C            END
C
C
C         When executed, this program produces the output shown
C         below.  Note that the results may vary slightly from one
C         computing platform to another.  Results are dependent on
C         the kernels used as well as the hardware and system software
C         running on the host system.
C     
C
C            Terminator point  1:
C               Radius                     (km):    1737.4
C               Planetocentric longitude   (deg):  -95.0845526
C               Planetocentric latitude    (deg):   0.00405276211
C               Solar incidence angle      (deg):   90.2697657
C               Minus Sun's angular radius (deg):   90.0000000000000E+00
C
C            Terminator point  2:
C               Radius                     (km):    1737.4
C               Planetocentric longitude   (deg):   84.2280921
C               Planetocentric latitude    (deg):   59.9957555
C               Solar incidence angle      (deg):   90.2697657
C               Minus Sun's angular radius (deg):   90.0000000000000E+00
C
C            Terminator point  3:
C               Radius                     (km):    1737.4
C               Planetocentric longitude   (deg):   87.2164179
C               Planetocentric latitude    (deg):  -59.9795505
C               Solar incidence angle      (deg):   90.2697657
C               Minus Sun's angular radius (deg):   90.0000000000000E+00
C
C
C$ Restrictions
C
C     1) This routine models light paths as straight lines.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 03-FEB-2007 (NJB)
C
C-&
 
C$ Index_Entries
C
C     find terminator on ellipsoid
C     find umbral terminator on ellipsoid
C     find penumbral terminator on ellipsoid
C
C-&
 

C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local parameters
C
      
C
C     Local variables
C
      DOUBLE PRECISION      LTSRC
      DOUBLE PRECISION      LTTARG
      DOUBLE PRECISION      R
      DOUBLE PRECISION      SRCPOS ( 3 )
      DOUBLE PRECISION      SRCRAD ( 3 )
      DOUBLE PRECISION      TRGPOS ( 3 )
      DOUBLE PRECISION      TRGRAD ( 3 )

      INTEGER               N
      INTEGER               CENTER
      INTEGER               CLSSID
      INTEGER               FRCLAS
      INTEGER               FRCODE
      INTEGER               TRGID

      LOGICAL               FOUND

C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'EDTERM' )

C
C     Get the input frame code and frame info.
C
      CALL NAMFRM ( FIXFRM, FRCODE )

      IF ( FRCODE .EQ. 0 ) THEN
         
         CALL SETMSG ( 'Input frame # has no associated frame '
     .   //            'ID code.'                               )
         CALL ERRCH  ( '#',  FIXFRM                             )
         CALL SIGERR ( 'SPICE(NOTRANSLATION)'                   )
         CALL CHKOUT ( 'EDTERM'                                 )
         RETURN

      END IF

      CALL FRINFO ( FRCODE, CENTER, FRCLAS, CLSSID, FOUND )

      IF ( .NOT. FOUND ) THEN
         
         CALL SETMSG ( 'Input frame # has associated frame '
     .   //            'ID code #, but no info was found '
     .   //            'by FRINFO for this frame.'              )
         CALL ERRCH  ( '#',  FIXFRM                             )
         CALL ERRINT ( '#',  FRCODE                             )
         CALL SIGERR ( 'SPICE(BUG)'                             )
         CALL CHKOUT ( 'EDTERM'                                 )
         RETURN

      END IF

C
C     Get the ID code of the target.
C
      CALL BODS2C ( TARGET, TRGID, FOUND )

      IF ( .NOT. FOUND ) THEN
         
         CALL SETMSG ( 'Input target # has no associated body '
     .   //            'ID code.'                               )
         CALL ERRCH  ( '#',  TARGET                             )
         CALL SIGERR ( 'SPICE(NOTRANSLATION)'                   )
         CALL CHKOUT ( 'EDTERM'                                 )
         RETURN

      END IF

C
C     If the frame is not centered on the target, reject it.
C
      IF ( CENTER .NE. TRGID ) THEN

         CALL SETMSG ( 'Input frame # is not centered on target '
     .   //            'body #. This frame must be a body-fixed '
     .   //            'frame associated with the target.'       )
         CALL ERRCH  ( '#',  FIXFRM                              )
         CALL ERRCH  ( '#',  TARGET                              )
         CALL SIGERR ( 'SPICE(INVALIDFIXFRM)'                    )
         CALL CHKOUT ( 'EDTERM'                                  )
         RETURN

      END IF
 
C
C     Look up the radii associated with the target body.
C
      CALL BODVRD ( TARGET, 'RADII', 3, N, TRGRAD )

      IF ( N .NE. 3 ) THEN

         CALL SETMSG ( 'Three radii are required for the target '
     .   //            'body''s (#) shape model, but # were found.' )
         CALL ERRCH  ( '#',  TARGET                                 )
         CALL ERRINT ( '#',  N                                      )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                        )
         CALL CHKOUT ( 'EDTERM'                                     )
         RETURN

      END IF

C
C     Look up the radii associated with the light source.
C
      CALL BODVRD ( SOURCE, 'RADII', 3, N, SRCRAD )

      IF ( N .NE. 3 ) THEN

         CALL SETMSG ( 'Three radii are required for the light '
     .   //            'source''s (#) shape model, but # were found.' )
         CALL ERRCH  ( '#',  SOURCE                                   )
         CALL ERRINT ( '#',  N                                        )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                          )
         CALL CHKOUT ( 'EDTERM'                                       )
         RETURN

      END IF

      R = MAX ( SRCRAD(1), SRCRAD(2), SRCRAD(3) )

C
C     Look up the observer-target vector and the target-source vector.
C     Also set the output OBSPOS.
C
      CALL SPKPOS   ( TARGET, ET,     FIXFRM, ABCORR,  
     .                OBSRVR, TRGPOS, LTTARG         )

      CALL ZZCOREPC ( ABCORR, ET,     LTTARG, TRGEPC )

      CALL VMINUS   ( TRGPOS, OBSPOS )

      CALL SPKPOS   ( SOURCE, TRGEPC, FIXFRM, ABCORR, 
     .                TARGET, SRCPOS, LTSRC          )

C
C     We're ready to compute the terminator.
C
      CALL ZZEDTERM ( TRMTYP, TRGRAD(1), TRGRAD(2), TRGRAD(3),  
     .                R,      SRCPOS,    NPTS,      TRMPTS     )    


      CALL CHKOUT ( 'EDTERM' )
      RETURN
      END
