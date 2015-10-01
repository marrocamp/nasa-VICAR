C$ Abstract
C
C     Include Section:  SPKDIFF Global Parameters
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
C$ Author_and_Institution
C
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    Version 1.0.0, 20-JUL-2006 (BVS).
C
C-&
 
C
C     Program name and version.
C
      CHARACTER*(*)         PGMNAM
      PARAMETER           ( PGMNAM = 'spkdiff' )

      CHARACTER*(*)         PGMVER
      PARAMETER           ( PGMVER = '1.0.0, July 20, 2006' )

C
C     Command line keys.
C
      INTEGER               MAXKEY
      PARAMETER           ( MAXKEY = 17 )

      CHARACTER*(*)         SB1KEY
      PARAMETER           ( SB1KEY = '-b1' )

      CHARACTER*(*)         SC1KEY
      PARAMETER           ( SC1KEY = '-c1' )

      CHARACTER*(*)         FR1KEY
      PARAMETER           ( FR1KEY = '-r1' )

      CHARACTER*(*)         SB2KEY
      PARAMETER           ( SB2KEY = '-b2' )

      CHARACTER*(*)         SC2KEY
      PARAMETER           ( SC2KEY = '-c2' )

      CHARACTER*(*)         FR2KEY
      PARAMETER           ( FR2KEY = '-r2' )

      CHARACTER*(*)         KERKEY
      PARAMETER           ( KERKEY = '-k' )

      CHARACTER*(*)         BEGKEY
      PARAMETER           ( BEGKEY = '-b' )

      CHARACTER*(*)         ENDKEY
      PARAMETER           ( ENDKEY = '-e' )

      CHARACTER*(*)         NSTKEY
      PARAMETER           ( NSTKEY = '-n' )

      CHARACTER*(*)         STPKEY
      PARAMETER           ( STPKEY = '-s' )

      CHARACTER*(*)         FMTKEY
      PARAMETER           ( FMTKEY = '-f' )

      CHARACTER*(*)         TYPKEY
      PARAMETER           ( TYPKEY = '-t' )

      CHARACTER*(*)         USGKEY
      PARAMETER           ( USGKEY = '-usage' )

      CHARACTER*(*)         UKEY
      PARAMETER           ( UKEY   = '-u' )

      CHARACTER*(*)         HLPKEY
      PARAMETER           ( HLPKEY = '-help' )

      CHARACTER*(*)         HKEY
      PARAMETER           ( HKEY   = '-h' )

      CHARACTER*(*)         STSVAL
      PARAMETER           ( STSVAL = 'stats' )

      CHARACTER*(*)         DMPVAL
      PARAMETER           ( DMPVAL = 'dump' )

      CHARACTER*(*)         DVFVAL
      PARAMETER           ( DVFVAL = 'dumpvf' )

      CHARACTER*(*)         BSCVAL
      PARAMETER           ( BSCVAL = 'basic' )

C
C     Max and min number states that the program can handle.
C
      INTEGER               MAXITR
      PARAMETER           ( MAXITR  = 1000000 )
 
      INTEGER               MINITR
      PARAMETER           ( MINITR  =       2 )
 
C
C     Default number states.
C
      INTEGER               DEFITR
      PARAMETER           ( DEFITR  = 1000 )

C
C     Line size parameters.
C
      INTEGER               SWDSIZ
      PARAMETER           ( SWDSIZ = 16 )
      
      INTEGER               WRDSIZ
      PARAMETER           ( WRDSIZ = 32 )
      
      INTEGER               LINSIZ
      PARAMETER           ( LINSIZ = 1024 )
      
      INTEGER               DSPSIZ
      PARAMETER           ( DSPSIZ = 80 )

C
C     Version/usage display parameters.
C
      INTEGER               VERCNT
      PARAMETER           ( VERCNT = 3 )
      
      INTEGER               USGCNT
      PARAMETER           ( USGCNT = 23 )

      INTEGER               INFCNT
      PARAMETER           ( INFCNT = 21 )

C
C     Maximum segment buffer size for Nat's ZZGEOSEG.
C
      INTEGER               MAXSEG
      PARAMETER           ( MAXSEG = 200 )
      
C
C     DAF descriptor size and component counts.
C
      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = 5 )

      INTEGER               ND
      PARAMETER           ( ND     = 2 )
 
      INTEGER               NI
      PARAMETER           ( NI     = 6 )

C
C     Cell lower boundary.
C 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

C
C     Maximum allowed number of coverage windows.
C
      INTEGER               WINSIZ
      PARAMETER           ( WINSIZ = 60000 )

C
C     Smallest allowed step.
C
      DOUBLE PRECISION      STPTOL
      PARAMETER           ( STPTOL = 1.D-8 )

