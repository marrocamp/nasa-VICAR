C$Procedure ZZGFRRU ( Private - GF, range rate utility routine )
 
      SUBROUTINE ZZGFRRU ( TARGET, ABCORR, OBSRVR, REFVAL, 
     .                     ET,     DT,     DECRES, LSSTHN, RVL )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This is the umbrella routine for the entry points needed by
C     GFEVNT in order to find range rate events.
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
C     RANGE RATE
C     EPHEMERIS
C     GEOMETRY
C     SEARCH
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE 'zzabcorr.inc'

      CHARACTER*(*)         TARGET
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      DT
      LOGICAL               DECRES
      LOGICAL               LSSTHN
      DOUBLE PRECISION      RVL
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     TARGET     I   Name of the target body
C     ABCORR     I   Aberration correction flag
C     OBSRVR     I   Name of the observing body
C     REFVAL     I   Reference value
C     ET         I   Ephemeris seconds past J2000 TDB.
C     DT         I   Interval from ET for derivative calculation.
C     DECRES     O   .TRUE. if range rate is decreasing, .FALSE.
C                    otherwise.
C     LSSTHN     O   .TRUE. if range rate is less than REFVAL, 
C                    .FALSE. otherwise.
C     RVL        O   Range rate at time ET.
C
C$ Detailed_Input
C
C     TARGET   the string name of a target body.  Optionally, you may
C              supply the integer ID code for the object as an
C              integer string.  For example both 'MOON' and '301'
C              are legitimate strings that indicate the moon is the
C              target body.
C
C              The target and observer define a position vector
C              that points from the observer to the target.
C
C     ABCORR   the string description of the aberration corrections to 
C              apply to the state evaluations to account for one-way 
C              light time and stellar aberration.
C
C              Any aberration correction accepted by the SPICE 
C              routine SPKEZR is accepted here. See the header
C              of SPKEZR for a detailed description of the 
C              aberration correction options. For convenience,
C              the options are listed below:
C
C                 'NONE'     Apply no correction. Returns the "true"
C                            geometric state.
C
C                 'LT'       "Reception" case:  correct for
C                            one-way light time using a Newtonian
C                            formulation.
C
C                 'LT+S'     "Reception" case:  correct for
C                            one-way light time and stellar
C                            aberration using a Newtonian
C                            formulation.
C
C                 'CN'       "Reception" case:  converged
C                            Newtonian light time correction.
C
C                 'CN+S'     "Reception" case:  converged
C                            Newtonian light time and stellar
C                            aberration corrections.
C
C                 'XLT'      "Transmission" case:  correct for
C                            one-way light time using a Newtonian
C                            formulation.
C
C                 'XLT+S'    "Transmission" case:  correct for
C                            one-way light time and stellar
C                            aberration using a Newtonian
C                            formulation.
C
C                 'XCN'      "Transmission" case:  converged
C                            Newtonian light time correction.
C
C                 'XCN+S'    "Transmission" case:  converged
C                            Newtonian light time and stellar
C                            aberration corrections.
C
C              The ABCORR string lacks sensitivity to case, leading 
C              and trailing blanks.
C
C     OBSRVR   the string name of an observing body.  Optionally, you
C              may supply the ID code of the object as an integer
C              string. For example, both 'EARTH' and '399' are
C              legitimate strings to indicate the observer as Earth.
C
C     REFVAL   the reference range rate (in km/sec) value against
C              which to compare the range rate of the oberrver-target
C              vector.
C
C     ET       time in TDB seconds past J2000 at which to calculate 
C              the value of or characteristic of the range rate of
C              the observer-target vector.
C
C     DT       a scalar double precision value representing half the 
C              interval in TDB seconds separating the evaluation
C              epochs; the evaluations occur at epochs 
C              (ET + DT) and (ET - DT).
C
C              DT may be negative but must be non-zero.
C
C     For more information, see individual entry points.
C
C$ Detailed_Output
C
C     LSSTHN   is .TRUE. if the range rate between the two bodies is 
C              less than the reference range rate value REFVAL at
C              time ET. Otherwise it is .FALSE..
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine serves as the umbrella routine for 4 entry points
C     needed by GFEVNT in solving for range rate conditions.
C
C     The 4 entry points are
C
C        ZZGFRRIN --- an initialization routine that must be called
C                     prior to attempting to solve for any range
C                     rate event.
C
C        ZZGFRRUR --- updates reference value, REFVAL.
C
C        ZZGFRRDC --- determines whether or not range rate is
C                     decreasing at some time.
C
C        ZZGFRRGQ --- returns the range rate of the two objects
C                     of concern as a function of ET.
C
C        ZZGFRRLT --- determines whether or not range rate is
C                     less than REFVAL
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     ZZGFRRIN must be called prior to use of any of the other
C     entry points (think constructor).
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C     L.S. Elson     (JPL)
C
C$ Version
C
C-    SPICELIB version 1.0.0 09-OCT-2009 (LSE)(EDW)
C
C-&

C$ Index_Entries
C
C     find range rate events    
C
C-&  
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      VDOT

      LOGICAL               FAILED 
      LOGICAL               RETURN
 


C
C     Local Variables
C
 
      DOUBLE PRECISION      RVEL
 
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      DRVEL
      DOUBLE PRECISION      SRHAT  ( 6 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      DFDT   ( 6 )
      DOUBLE PRECISION      STATES ( 6, 2 )

      INTEGER               N
      
      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               FOUND


C
C     Saved Variables
C
      CHARACTER*(CORLEN)    S_ABCO
      SAVE                  S_ABCO

      INTEGER               S_TARG
      SAVE                  S_TARG
 
      CHARACTER*(32)        S_REF
      SAVE                  S_REF
 
      INTEGER               S_OBS
      SAVE                  S_OBS
 
      DOUBLE PRECISION      S_VREF
      SAVE                  S_VREF
 
      DOUBLE PRECISION      S_DT
      SAVE                  S_DT

      RETURN
 
 
 
C$Procedure ZZGFRRIN ( Private - GF, range rate initialization routine )
 
      ENTRY ZZGFRRIN ( TARGET, ABCORR, OBSRVR, REFVAL, DT )

C$ Abstract
C
C     This is the initialization entry point used for describing
C     the event that is to be solved for by ZZGFSOLV.
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
C     RANGE RATE
C     EPHEMERIS
C     GEOMETRY
C     SEARCH
C
C$ Declarations
C
C      CHARACTER*(*)         TARGET
C      CHARACTER*(*)         ABCORR
C      CHARACTER*(*)         OBSRVR
C      DOUBLE PRECISION      REFVAL
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     TARGET     I   Name of the target body
C     ABCORR     I   Aberration correction flag
C     OBSRVR     I   Name of the observing body
C     REFVAL     I   Reference value
C     DT         I   Interval from ET for derivative calculation.
C
C$ Detailed_Input
C
C     TARGET   the string name of a target body.  Optionally, you may
C              supply the integer ID code for the object as an
C              integer string.  For example both 'MOON' and '301'
C              are legitimate strings that indicate the moon is the
C              target body.
C
C              The target and observer define a position vector
C              that points from the observer to the target.
C
C     ABCORR   the string description of the aberration corrections to 
C              apply to the state evaluations to account for one-way 
C              light time and stellar aberration.
C
C              Any aberration correction accepted by the SPICE 
C              routine SPKEZR is accepted here. See the header
C              of SPKEZR for a detailed description of the 
C              aberration correction options. For convenience,
C              the options are listed below:
C
C                 'NONE'     Apply no correction. Returns the "true"
C                            geometric state.
C
C                 'LT'       "Reception" case:  correct for
C                            one-way light time using a Newtonian
C                            formulation.
C
C                 'LT+S'     "Reception" case:  correct for
C                            one-way light time and stellar
C                            aberration using a Newtonian
C                            formulation.
C
C                 'CN'       "Reception" case:  converged
C                            Newtonian light time correction.
C
C                'CN+S'     "Reception" case:  converged
C                            Newtonian light time and stellar
C                            aberration corrections.
C
C                 'XLT'      "Transmission" case:  correct for
C                            one-way light time using a Newtonian
C                            formulation.
C
C                 'XLT+S'    "Transmission" case:  correct for
C                            one-way light time and stellar
C                            aberration using a Newtonian
C                            formulation.
C
C                 'XCN'      "Transmission" case:  converged
C                            Newtonian light time correction.
C
C                 'XCN+S'    "Transmission" case:  converged
C                            Newtonian light time and stellar
C                            aberration corrections.
C
C              The ABCORR string lacks sensitivity to case, leading 
C              and trailing blanks.
C
C     OBSRVR   the string name of an observing body.  Optionally, you
C              may supply the ID code of the object as an integer
C              string. For example, both 'EARTH' and '399' are
C              legitimate strings to indicate the observer as Earth.
C
C     REFVAL   the reference range rate (in km/sec) value against
C              which to compare the range rate of the oberrver-target
C              vector.
C
C     DT         a scalar double precision value representing half the 
C                interval in TDB seconds separating the evaluation
C                epochs; the evaluations occur at epochs 
C                (ET + DT) and (ET - DT).
C
C                DT may be negative but must be non-zero.
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
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
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
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C     L.S. Elson     (JPL)
C
C$ Version
C
C-    SPICELIB version 1.0.0 09-OCT-2009 (LSE)(EDW)
C     
C-&

C$ Index_Entries
C
C     range rate initialization routine.     
C
C-& 
 
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFRRIN' )

C
C     Find NAIF IDs for TARGET and OBSRVR.
C
      CALL BODS2C ( TARGET, S_TARG, FOUND )

      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'The target object, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', TARGET                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFRRIN'                                 )
         RETURN

      END IF
 

      CALL BODS2C ( OBSRVR, S_OBS, FOUND )

      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'The observer, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE toolkit. '           )
         CALL ERRCH  ( '#', OBSRVR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFRRIN'                                 )
         RETURN

      END IF

C
C     Make sure the observer and target are distinct.
C
      IF ( S_TARG .EQ. S_OBS ) THEN

         CALL SETMSG ( 'The observer and target must be '
     .   //            'distinct objects, but are not: '
     .   //            'OBSRVR = #; TARGET = #.'           )
         CALL ERRCH  ( '#', OBSRVR                         )
         CALL ERRCH  ( '#', TARGET                         )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'          )
         CALL CHKOUT ( 'ZZGFRRIN'                          )
         RETURN

      END IF

C
C     Squeeze all blanks out of the aberration correction
C     string; ensure the string is in upper case.
C     
      CALL CMPRSS ( ' ', 0, ABCORR, S_ABCO )
      CALL UCASE  ( S_ABCO,         S_ABCO )

C
C     Check the aberration correction. If SPKEZR can't handle it,
C     neither can we.
C     
      CALL ZZVALCOR ( S_ABCO, ATTBLK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFRRIN' )
         RETURN
      END IF

C
C     Save the reference value.
C
      S_VREF = REFVAL
      S_REF  = 'J2000'
      S_DT   = DT
 
      CALL CHKOUT ( 'ZZGFRRIN' )
      RETURN



C$Procedure ZZGFRRUR ( Private - GF, range rate update reference value )
 
      ENTRY ZZGFRRUR ( REFVAL )
 
C$ Abstract
C
C     This is the entry point used for updating the reference
C     value.
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
C     RANGE RATE
C     EPHEMERIS
C     GEOMETRY
C     SEARCH
C
C$ Declarations
C
C      DOUBLE PRECISION      REFVAL
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     REFVAL     I   Reference value
C
C$ Detailed_Input
C
C     REFVAL     the reference range rate (in km/sec) value against
C                which to compare the range rate of the oberrver-target
C                vector.
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
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
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
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C     L.S. Elson     (JPL)
C
C$ Version
C
C-    SPICELIB version 1.0.0 09-JUN-2009 (LSE)(EDW)
C
C-&

C$ Index_Entries
C
C     range rate update reference value.     
C
C-& 

      S_VREF  =  REFVAL
      RETURN



C$Procedure ZZGFRRDC (  Private - GF, when range rate is decreasing )
 
      ENTRY ZZGFRRDC ( ET, DECRES )
 
C$ Abstract
C
C     Computes whether or not the range rate between the observer
C     and the target is decreasing at time ET.
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
C     RANGE RATE
C     EPHEMERIS
C     GEOMETRY
C     SEARCH
C
C$ Declarations
C
C     DOUBLE PRECISION      ET
C     LOGICAL               DECRES
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Ephemeris seconds past J2000 TDB.
C     DECRES     O   .TRUE. if range rate is decreasing, .FALSE. 
C                    otherwise.
C
C$ Detailed_Input
C
C     ET         time in seconds past J2000 at which to calculate
C                whether the range rate of the observer-target vector
C                is decreasing.
C
C$ Detailed_Output
C
C     DECRES     is .TRUE. if the range rate between the objects
C                is decreasing.  Otherwise it is .FALSE.
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
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
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C     L.S. Elson     (JPL)
C
C$ Version
C
C-    SPICELIB version 1.0.0 09-OCT-2009 (LSE)(EDW)
C     
C-&

C$ Index_Entries
C
C     when range rate is decreasing    
C
C-& 

C
C     Standard SPICE error handling.
C
 
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'ZZGFRRDC' ) 
      END IF
 
      N  = 6

C
C     The range rate of interest is of S_TARG relative to the S_OBS.
C     The function requires the acceleration of S_TARG relative
C     to S_OBS.
C

      CALL SPKEZ (S_TARG, ET-S_DT, S_REF, S_ABCO, S_OBS, 
     .                                            STATES(1,1), LT)
      CALL SPKEZ (S_TARG, ET+S_DT, S_REF, S_ABCO, S_OBS, 
     .                                            STATES(1,2), LT)

C
C     Approximate the derivative of the position and valocity by 
C     finding the derivative of a quadratic approximating function.
C
C        DFDT(1) = Vx
C        DFDT(2) = Vy
C        DFDT(3) = Vz
C        DFDT(4) = Ax
C        DFDT(5) = Ay
C        DFDT(6) = Az
C
      CALL QDERIV ( N, STATES(1,1), STATES(1,2), S_DT, DFDT )

      CALL SPKEZ ( S_TARG, ET, S_REF, S_ABCO, S_OBS, STATE, LT )

      IF (  FAILED() ) THEN
         CALL CHKOUT (  'ZZGFRRDC' )
         RETURN
      END IF

C
C        d ||r||     ^
C        ------- = < r, v >
C        dt
C
C         2            ^          ^
C        d ||r||   < d r, v > + < r, d v >
C        ------- =   ---             ---
C          2         
C        dt          dt              dt
C
      CALL DVHAT ( STATE, SRHAT )

      DRVEL  =  VDOT( DFDT(4), SRHAT ) + VDOT( STATE(4), SRHAT(4) )
      DECRES =  DRVEL .LT. 0.D0 

      CALL CHKOUT ( 'ZZGFRRDC' ) 
      RETURN


 
C$Procedure ZZGFRRGQ ( Private - GF, get range rate between two bodies )
 
      ENTRY ZZGFRRGQ ( ET, RVL )
 
C$ Abstract
C
C     Determine the range rate between the centers of the two
C     bodies.
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
C     RANGE RATE
C     EPHEMERIS
C     GEOMETRY
C     SEARCH
C
C$ Declarations
C
C      DOUBLE PRECISION      ET
C      DOUBLE PRECISION      RVL
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Ephemeris seconds past J2000 TDB.
C     RVL        O   Range rate at time ET.
C
C$ Detailed_Input
C
C     ET         time in ephemeris seconds past J2000 when the range 
C                rate between the two bodies is to be computed.
C
C$ Detailed_Output
C
C     RVL        is the range rate of S_TARG as seen from S_OBS at 
C                time ET.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
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
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C     L.S. Elson     (JPL)
C
C$ Version
C
C-    SPICELIB version 1.0.0 09-JUN-2009 (LSE)(EDW)
C
C-&

C$ Index_Entries
C
C     get range rate between two bodies    
C
C-& 

      CALL ZZGFRRQ ( ET, S_TARG, S_OBS, S_ABCO, RVL )

      RETURN




C$Procedure ZZGFRRLT ( Private - GF, range rate < reference )
 
      ENTRY ZZGFRRLT ( ET, LSSTHN )
 
C$ Abstract
C
C     Determine whether or not the range rate between the two
C     bodies is less than the reference value.
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
C     RANGE RATE
C     EPHEMERIS
C     GEOMETRY
C     SEARCH
C
C$ Declarations
C
C      DOUBLE PRECISION      ET
C      LOGICAL               LSSTHN
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Ephemeris seconds past J2000 TDB.
C     LSSTHN     O   .TRUE. if the range rate is less than 
C                    REFVAL, .FALSE. otherwise.
C
C$ Detailed_Input
C
C     ET       is the time in second past J2000 at which one wants
C              to determine if the range rate between the
C              two bodies is less than the reference value.
C
C$ Detailed_Output
C
C     LSSTHN   is .TRUE. if the range rate between the two bodies is 
C              less than the reference range rate value S_VREF at
C              time ET. Otherwise it is .FALSE..
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
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
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C     L.S. Elson     (JPL)
C
C$ Version
C
C-    SPICELIB version 1.0.0 09-JUN-2009 (LSE)(EDW)
C
C-&

C$ Index_Entries
C
C     range rate less than a value     
C
C-& 
 
      CALL ZZGFRRQ ( ET, S_TARG, S_OBS, S_ABCO, RVEL )
 
      IF ( RVEL .LT. S_VREF ) THEN
         LSSTHN = .TRUE.
      ELSE
         LSSTHN = .FALSE.
      END IF
 
      RETURN
      END
