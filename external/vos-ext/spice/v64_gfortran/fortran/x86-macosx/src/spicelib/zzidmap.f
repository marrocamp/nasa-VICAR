C$Procedure ZZIDMAP ( Private --- SPICE body ID/name assignments )

      SUBROUTINE  ZZIDMAP( BLTCOD, BLTNAM )
      IMPLICIT NONE

C$ Abstract
C
C     The default SPICE body/ID mapping assignments available
C     to the SPICE library.
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
C     naif_ids.req
C
C$ Keywords
C
C     Body mappings.
C
C$ Declarations

      INCLUDE              'zzbodtrn.inc'

      INTEGER              BLTCOD(NPERM)
      CHARACTER*(MAXL)     BLTNAM(NPERM)

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     BLTCOD     O  List of default integer ID codes
C     BLTNAM     O  List of default names
C     NPERM      P  Number of name/ID mappings
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     BLTCOD     The array of NPERM elements listing the body ID codes.
C
C     BLTNAM     The array of NPERM elements listing the body names 
C                corresponding to the ID entry in BLTCOD
C
C$ Parameters
C
C     NPERM      The length of both BLTCOD, BLTNAM
C                (read from zzbodtrn.inc).
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
C     Each ith entry of BLTCOD maps to the ith entry of BLTNAM.
C
C$ Examples
C
C     Simple to use, a call the ZZIDMAP returns the arrays defining the
C     name/ID mappings.
C
C
C        INCLUDE            'zzbodtrn.inc'
C
C        INTEGER             ID  ( NPERM )
C        CHARACTER*(MAXL)    NAME( NPERM )
C
C        CALL ZZIDMAP( ID, NAME )
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
C     E.D. Wright, Thu May 20 07:57:58 2010 (JPL)
C
C$ Version
C
C-    SPICELIB 1.0.7 20-MAY-2010 (EDW)
C
C        Edit to vehicle ID list to correct -76 not in proper
C        numerical (descending) order.
C
C     Added:
C
C               -5   AKATSUKI
C               -5   VCO
C             -121   BEPICOLOMBO
C             -177   GRAIL-A
C             -181   GRAIL-B
C             -202   MAVEN
C             -205   SOIL MOISTURE ACTIVE AND PASSIVE
C             -205   SMAP
C             -362   RADIATION BELT STORM PROBE A
C             -362   RBSP_A
C             -363   RADIATION BELT STORM PROBE B
C             -363   RBSP_B
C              550   HERSE
C              653   AEGAEON
C          1000093   TEMPEL_1
C          2000021   LUTETIA
C          2004179   TOUTATIS
C
C-    SPICELIB 1.0.6 08-APR-2009 (EDW)
C
C     Added:
C
C               -5   PLC
C               -5   PLANET-C
C              -68   MMO
C              -68   MERCURY MAGNETOSPHERIC ORBITER
C              -69   MPO
C              -69   MERCURY PLANETARY ORBITER
C          2002867   STEINS
C             -140   EPOCH
C             -140   DIXI
C
C-    SPICELIB 1.0.5 09-JAN-2008 (EDW)
C
C     Added:
C
C              -18   LCROSS
C              -29   NEXT
C              -86   CH1
C              -86   CHANDRAYAAN-1
C             -131   KAGUYA
C             -140   EPOXI
C             -151   CHANDRA
C             -187   SOLAR PROBE
C              636   AEGIR
C              637   BEBHIONN
C              638   BERGELMIR
C              639   BESTLA
C              640   FARBAUTI
C              641   FENRIR
C              642   FORNJOT
C              643   HATI
C              644   HYROKKIN
C              645   KARI
C              646   LOGE
C              647   SKOLL
C              648   SURTUR
C              649   ANTHE
C              650   JARNSAXA
C              651   GREIP
C              652   TARQEQ
C              809   HALIMEDE
C              810   PSAMATHE
C              811   SAO
C              812   LAOMEDEIA
C              813   NESO
C
C     NAIF modified the Jovian system listing to conform to the
C     current (as of this date) name/body mapping.
C
C              540   MNEME
C              541   AOEDE
C              542   THELXINOE
C              543   ARCHE
C              544   KALLICHORE
C              545   HELIKE
C              546   CARPO
C              547   EUKELADE
C              548   CYLLENE
C              549   KORE
C
C     Removed assignments:
C
C             -172   SPACETECH-3 COMBINER
C             -174   PLUTO-KUIPER EXPRESS
C             -175   PLUTO-KUIPER EXPRESS SIMULATION
C             -205   SPACETECH-3 COLLECTOR
C              514   1979J2
C              515   1979J1
C              516   1979J3
C              610   1980S1
C              611   1980S3
C              612   1980S6
C              613   1980S13
C              614   1980S25
C              615   1980S28
C              616   1980S27
C              617   1980S26
C              706   1986U7
C              707   1986U8
C              708   1986U9
C              709   1986U4
C              710   1986U6
C              711   1986U3
C              712   1986U1
C              713   1986U2
C              714   1986U5
C              715   1985U1
C              718   1986U10
C              901   1978P1
C
C     Spelling correction:
C              
C        MAGACLITE to MEGACLITE
C
C     Rename:
C              
C        ERRIAPO to ERRIAPUS
C        STV-1 to STV51
C        STV-2 to STV52
C        STV-3 to STV53
C 
C
C-    SPICELIB 1.0.4 01-NOV-2006 (EDW)
C
C     NAIF removed several provisional name/ID mappings from
C     the Jovian system listing:
C
C     539         'HEGEMONE'              JXXXIX
C     540         'MNEME'                 JXL
C     541         'AOEDE'                 JXLI
C     542         'THELXINOE'             JXLII
C     543         'ARCHE'                 JXLIII
C     544         'KALLICHORE'            JXLIV
C     545         'HELIKE'                JXLV
C     546         'CARPO'                 JXLVI
C     547         'EUKELADE'              JXLVII
C     548         'CYLLENE'               JXLVIII
C
C     The current mapping set for the range 539-561:
C
C              540   ARCHE
C              541   EUKELADE
C              546   HELIKE
C              547   AOEDE
C              548   HEGEMONE
C              551   KALLICHORE
C              553   CYLLENE
C              560   CARPO
C              561   MNEME
C
C     The new mapping leaves the IDs 539, 542-545, 549, 550, 552, 
C     554-559 unassigned.
C
C     Added:
C
C              635   DAPHNIS
C              722   FRANCISCO
C              723   MARGARET
C              724   FERDINAND
C              725   PERDITA
C              726   MAB
C              727   CUPID
C              -61   JUNO
C              -76   MSL
C              -76   MARS SCIENCE LABORATORY
C             -212   STV-1
C             -213   STV-2
C             -214   STV-3
C              902   NIX
C              903   HYDRA
C             -85    LRO
C             -85    LUNAR RECON ORBITER
C             -85    LUNAR RECONNAISSANCE ORBITER
C
C     Spelling correction
C
C              632   METHODE to METHONE
C
C-    SPICELIB 1.0.3 14-NOV-2005 (EDW)
C
C     Added:
C
C              539   HEGEMONE
C              540   MNEME       
C              541   AOEDE     
C              542   THELXINOE     
C              543   ARCHE 
C              544   KALLICHORE   
C              545   HELIKE  
C              546   CARPO
C              547   EUKELADE 
C              548   CYLLENE
C              631   NARVI 
C              632   METHODE 
C              633   PALLENE
C              634   POLYDEUCES 
C          2025143   ITOKAWA 
C              -98   NEW HORIZONS
C             -248   VENUS EXPRESS, VEX
C             -500   RSAT, SELENE Relay Satellite, SELENE Rstar, Rstar
C             -502   VSAT, SELENE VLBI Radio Satellite, 
C                    SELENE VRAD Satellite, SELENE Vstar
C           399064   DSS-64
C
C      Change in spelling:
C
C              623   SUTTUNG to SUTTUNGR
C              627   SKADI   to SKATHI
C              630   THRYM   to THRYMR
C
C-    SPICELIB 1.0.2 20-DEC-2004 (EDW)
C
C     Added:
C
C           Due to the previous definition of Parkes with DSS-05,
C           the Parkes ID remains 399005.
C
C             -486   HERSCHEL
C             -489   PLANCK
C           399049   DSS-49
C           399055   DSS-55
C             -203   DAWN
C          1000012   67P/CHURYUMOV-GERASIMENKO (1969 R1)
C          1000012   CHURYUMOV-GERASIMENKO
C          398989    NOTO
C             -84    PHOENIX
C            -131    SELENE
C            -238    SMART-1, S1, SM1, SMART1
C            -130    HAYABUSA
C
C-    SPICELIB 1.0.1 19-DEC-2003 (EDW)
C
C     Added:
C              -79   SPITZER
C          2000216   KLEOPATRA
C
C-    SPICELIB 1.0.0 27-JUL-2003 (EDW)
C
C     Added:
C              -47   GNS
C              -74   MRO
C              -74   MARS RECON ORBITER
C             -130   MUSES-C
C             -142   TERRA
C             -154   AQUA
C             -159   EUROPA ORBITER
C             -190   SIM
C             -198   INTEGRAL
C             -227   KEPLER
C             -234   STEREO AHEAD
C             -235   STEREO BEHIND
C             -253   OPPORTUNITY
C             -254   SPIRIT
C              528   AUTONOE      
C              529   THYONE       
C              530   HERMIPPE        
C              531   AITNE
C              532   EURYDOME
C              533   EUANTHE        
C              534   EUPORIE         
C              535   ORTHOSIE          
C              536   SPONDE          
C              537   KALE        
C              538   PASITHEE
C              619   YMIR
C              620   PAALIAQ
C              621   TARVOS
C              622   IJIRAQ
C              623   SUTTUNG
C              624   KIVIUQ
C              625   MUNDILFARI
C              626   ALBIORIX
C              627   SKADI
C              628   ERRIAPO
C              629   SIARNAQ
C              630   THRYM
C              718   PROSPERO
C              719   SETEBOS
C              720   STEPHANO
C              721   TRINCULO
C           398990   NEW NORCIA
C          2431011   DACTYL
C          2000001   CERES
C          2000004   VESTA
C
C     Renamed:
C
C              -25   LPM to
C              -25   LP
C
C             -180   MUSES-C to
C             -130   MUSES-B
C
C             -172   STARLIGHT COMBINER to
C             -172   SPACETECH-3 COMBINER
C
C             -205   STARLIGHT COLLECTOR to
C             -205   SPACETECH-3 COLLECTOR
C
C      Removed:
C             -172   SLCOMB
C
C
C-&

C$ Index_Entries
C
C     body ID mapping
C
C-&

C
C     A script generates this file. Do not edit by hand.
C     Edit the creation script to modify the contents of
C     ZZIDMAP.
C

      BLTCOD(1) =   0
      BLTNAM(1) =  'SSB'

      BLTCOD(2) =   0
      BLTNAM(2) =  'SOLAR SYSTEM BARYCENTER'

      BLTCOD(3) =   1
      BLTNAM(3) =  'MERCURY BARYCENTER'

      BLTCOD(4) =   2
      BLTNAM(4) =  'VENUS BARYCENTER'

      BLTCOD(5) =   3
      BLTNAM(5) =  'EMB'

      BLTCOD(6) =   3
      BLTNAM(6) =  'EARTH MOON BARYCENTER'

      BLTCOD(7) =   3
      BLTNAM(7) =  'EARTH-MOON BARYCENTER'

      BLTCOD(8) =   3
      BLTNAM(8) =  'EARTH BARYCENTER'

      BLTCOD(9) =   4
      BLTNAM(9) =  'MARS BARYCENTER'

      BLTCOD(10) =   5
      BLTNAM(10) =  'JUPITER BARYCENTER'

      BLTCOD(11) =   6
      BLTNAM(11) =  'SATURN BARYCENTER'

      BLTCOD(12) =   7
      BLTNAM(12) =  'URANUS BARYCENTER'

      BLTCOD(13) =   8
      BLTNAM(13) =  'NEPTUNE BARYCENTER'

      BLTCOD(14) =   9
      BLTNAM(14) =  'PLUTO BARYCENTER'

      BLTCOD(15) =   10
      BLTNAM(15) =  'SUN'

      BLTCOD(16) =   199
      BLTNAM(16) =  'MERCURY'

      BLTCOD(17) =   299
      BLTNAM(17) =  'VENUS'

      BLTCOD(18) =   399
      BLTNAM(18) =  'EARTH'

      BLTCOD(19) =   301
      BLTNAM(19) =  'MOON'

      BLTCOD(20) =   499
      BLTNAM(20) =  'MARS'

      BLTCOD(21) =   401
      BLTNAM(21) =  'PHOBOS'

      BLTCOD(22) =   402
      BLTNAM(22) =  'DEIMOS'

      BLTCOD(23) =   599
      BLTNAM(23) =  'JUPITER'

      BLTCOD(24) =   501
      BLTNAM(24) =  'IO'

      BLTCOD(25) =   502
      BLTNAM(25) =  'EUROPA'

      BLTCOD(26) =   503
      BLTNAM(26) =  'GANYMEDE'

      BLTCOD(27) =   504
      BLTNAM(27) =  'CALLISTO'

      BLTCOD(28) =   505
      BLTNAM(28) =  'AMALTHEA'

      BLTCOD(29) =   506
      BLTNAM(29) =  'HIMALIA'

      BLTCOD(30) =   507
      BLTNAM(30) =  'ELARA'

      BLTCOD(31) =   508
      BLTNAM(31) =  'PASIPHAE'

      BLTCOD(32) =   509
      BLTNAM(32) =  'SINOPE'

      BLTCOD(33) =   510
      BLTNAM(33) =  'LYSITHEA'

      BLTCOD(34) =   511
      BLTNAM(34) =  'CARME'

      BLTCOD(35) =   512
      BLTNAM(35) =  'ANANKE'

      BLTCOD(36) =   513
      BLTNAM(36) =  'LEDA'

      BLTCOD(37) =   514
      BLTNAM(37) =  'THEBE'

      BLTCOD(38) =   515
      BLTNAM(38) =  'ADRASTEA'

      BLTCOD(39) =   516
      BLTNAM(39) =  'METIS'

      BLTCOD(40) =   517
      BLTNAM(40) =  'CALLIRRHOE'

      BLTCOD(41) =   518
      BLTNAM(41) =  'THEMISTO'

      BLTCOD(42) =   519
      BLTNAM(42) =  'MAGACLITE'

      BLTCOD(43) =   520
      BLTNAM(43) =  'TAYGETE'

      BLTCOD(44) =   521
      BLTNAM(44) =  'CHALDENE'

      BLTCOD(45) =   522
      BLTNAM(45) =  'HARPALYKE'

      BLTCOD(46) =   523
      BLTNAM(46) =  'KALYKE'

      BLTCOD(47) =   524
      BLTNAM(47) =  'IOCASTE'

      BLTCOD(48) =   525
      BLTNAM(48) =  'ERINOME'

      BLTCOD(49) =   526
      BLTNAM(49) =  'ISONOE'

      BLTCOD(50) =   527
      BLTNAM(50) =  'PRAXIDIKE'

      BLTCOD(51) =   528
      BLTNAM(51) =  'AUTONOE'

      BLTCOD(52) =   529
      BLTNAM(52) =  'THYONE'

      BLTCOD(53) =   530
      BLTNAM(53) =  'HERMIPPE'

      BLTCOD(54) =   531
      BLTNAM(54) =  'AITNE'

      BLTCOD(55) =   532
      BLTNAM(55) =  'EURYDOME'

      BLTCOD(56) =   533
      BLTNAM(56) =  'EUANTHE'

      BLTCOD(57) =   534
      BLTNAM(57) =  'EUPORIE'

      BLTCOD(58) =   535
      BLTNAM(58) =  'ORTHOSIE'

      BLTCOD(59) =   536
      BLTNAM(59) =  'SPONDE'

      BLTCOD(60) =   537
      BLTNAM(60) =  'KALE'

      BLTCOD(61) =   538
      BLTNAM(61) =  'PASITHEE'

      BLTCOD(62) =   539
      BLTNAM(62) =  'HEGEMONE'

      BLTCOD(63) =   540
      BLTNAM(63) =  'MNEME'

      BLTCOD(64) =   541
      BLTNAM(64) =  'AOEDE'

      BLTCOD(65) =   542
      BLTNAM(65) =  'THELXINOE'

      BLTCOD(66) =   543
      BLTNAM(66) =  'ARCHE'

      BLTCOD(67) =   544
      BLTNAM(67) =  'KALLICHORE'

      BLTCOD(68) =   545
      BLTNAM(68) =  'HELIKE'

      BLTCOD(69) =   546
      BLTNAM(69) =  'CARPO'

      BLTCOD(70) =   547
      BLTNAM(70) =  'EUKELADE'

      BLTCOD(71) =   548
      BLTNAM(71) =  'CYLLENE'

      BLTCOD(72) =   549
      BLTNAM(72) =  'KORE'

      BLTCOD(73) =   550
      BLTNAM(73) =  'HERSE'

      BLTCOD(74) =   699
      BLTNAM(74) =  'SATURN'

      BLTCOD(75) =   601
      BLTNAM(75) =  'MIMAS'

      BLTCOD(76) =   602
      BLTNAM(76) =  'ENCELADUS'

      BLTCOD(77) =   603
      BLTNAM(77) =  'TETHYS'

      BLTCOD(78) =   604
      BLTNAM(78) =  'DIONE'

      BLTCOD(79) =   605
      BLTNAM(79) =  'RHEA'

      BLTCOD(80) =   606
      BLTNAM(80) =  'TITAN'

      BLTCOD(81) =   607
      BLTNAM(81) =  'HYPERION'

      BLTCOD(82) =   608
      BLTNAM(82) =  'IAPETUS'

      BLTCOD(83) =   609
      BLTNAM(83) =  'PHOEBE'

      BLTCOD(84) =   610
      BLTNAM(84) =  'JANUS'

      BLTCOD(85) =   611
      BLTNAM(85) =  'EPIMETHEUS'

      BLTCOD(86) =   612
      BLTNAM(86) =  'HELENE'

      BLTCOD(87) =   613
      BLTNAM(87) =  'TELESTO'

      BLTCOD(88) =   614
      BLTNAM(88) =  'CALYPSO'

      BLTCOD(89) =   615
      BLTNAM(89) =  'ATLAS'

      BLTCOD(90) =   616
      BLTNAM(90) =  'PROMETHEUS'

      BLTCOD(91) =   617
      BLTNAM(91) =  'PANDORA'

      BLTCOD(92) =   618
      BLTNAM(92) =  'PAN'

      BLTCOD(93) =   619
      BLTNAM(93) =  'YMIR'

      BLTCOD(94) =   620
      BLTNAM(94) =  'PAALIAQ'

      BLTCOD(95) =   621
      BLTNAM(95) =  'TARVOS'

      BLTCOD(96) =   622
      BLTNAM(96) =  'IJIRAQ'

      BLTCOD(97) =   623
      BLTNAM(97) =  'SUTTUNGR'

      BLTCOD(98) =   624
      BLTNAM(98) =  'KIVIUQ'

      BLTCOD(99) =   625
      BLTNAM(99) =  'MUNDILFARI'

      BLTCOD(100) =   626
      BLTNAM(100) =  'ALBIORIX'

      BLTCOD(101) =   627
      BLTNAM(101) =  'SKATHI'

      BLTCOD(102) =   628
      BLTNAM(102) =  'ERRIAPUS'

      BLTCOD(103) =   629
      BLTNAM(103) =  'SIARNAQ'

      BLTCOD(104) =   630
      BLTNAM(104) =  'THRYMR'

      BLTCOD(105) =   631
      BLTNAM(105) =  'NARVI'

      BLTCOD(106) =   632
      BLTNAM(106) =  'METHONE'

      BLTCOD(107) =   633
      BLTNAM(107) =  'PALLENE'

      BLTCOD(108) =   634
      BLTNAM(108) =  'POLYDEUCES'

      BLTCOD(109) =   635
      BLTNAM(109) =  'DAPHNIS'

      BLTCOD(110) =   636
      BLTNAM(110) =  'AEGIR'

      BLTCOD(111) =   637
      BLTNAM(111) =  'BEBHIONN'

      BLTCOD(112) =   638
      BLTNAM(112) =  'BERGELMIR'

      BLTCOD(113) =   639
      BLTNAM(113) =  'BESTLA'

      BLTCOD(114) =   640
      BLTNAM(114) =  'FARBAUTI'

      BLTCOD(115) =   641
      BLTNAM(115) =  'FENRIR'

      BLTCOD(116) =   642
      BLTNAM(116) =  'FORNJOT'

      BLTCOD(117) =   643
      BLTNAM(117) =  'HATI'

      BLTCOD(118) =   644
      BLTNAM(118) =  'HYROKKIN'

      BLTCOD(119) =   645
      BLTNAM(119) =  'KARI'

      BLTCOD(120) =   646
      BLTNAM(120) =  'LOGE'

      BLTCOD(121) =   647
      BLTNAM(121) =  'SKOLL'

      BLTCOD(122) =   648
      BLTNAM(122) =  'SURTUR'

      BLTCOD(123) =   649
      BLTNAM(123) =  'ANTHE'

      BLTCOD(124) =   650
      BLTNAM(124) =  'JARNSAXA'

      BLTCOD(125) =   651
      BLTNAM(125) =  'GREIP'

      BLTCOD(126) =   652
      BLTNAM(126) =  'TARQEQ'

      BLTCOD(127) =   653
      BLTNAM(127) =  'AEGAEON'

      BLTCOD(128) =   799
      BLTNAM(128) =  'URANUS'

      BLTCOD(129) =   701
      BLTNAM(129) =  'ARIEL'

      BLTCOD(130) =   702
      BLTNAM(130) =  'UMBRIEL'

      BLTCOD(131) =   703
      BLTNAM(131) =  'TITANIA'

      BLTCOD(132) =   704
      BLTNAM(132) =  'OBERON'

      BLTCOD(133) =   705
      BLTNAM(133) =  'MIRANDA'

      BLTCOD(134) =   706
      BLTNAM(134) =  'CORDELIA'

      BLTCOD(135) =   707
      BLTNAM(135) =  'OPHELIA'

      BLTCOD(136) =   708
      BLTNAM(136) =  'BIANCA'

      BLTCOD(137) =   709
      BLTNAM(137) =  'CRESSIDA'

      BLTCOD(138) =   710
      BLTNAM(138) =  'DESDEMONA'

      BLTCOD(139) =   711
      BLTNAM(139) =  'JULIET'

      BLTCOD(140) =   712
      BLTNAM(140) =  'PORTIA'

      BLTCOD(141) =   713
      BLTNAM(141) =  'ROSALIND'

      BLTCOD(142) =   714
      BLTNAM(142) =  'BELINDA'

      BLTCOD(143) =   715
      BLTNAM(143) =  'PUCK'

      BLTCOD(144) =   716
      BLTNAM(144) =  'CALIBAN'

      BLTCOD(145) =   717
      BLTNAM(145) =  'SYCORAX'

      BLTCOD(146) =   718
      BLTNAM(146) =  'PROSPERO'

      BLTCOD(147) =   719
      BLTNAM(147) =  'SETEBOS'

      BLTCOD(148) =   720
      BLTNAM(148) =  'STEPHANO'

      BLTCOD(149) =   721
      BLTNAM(149) =  'TRINCULO'

      BLTCOD(150) =   722
      BLTNAM(150) =  'FRANCISCO'

      BLTCOD(151) =   723
      BLTNAM(151) =  'MARGARET'

      BLTCOD(152) =   724
      BLTNAM(152) =  'FERDINAND'

      BLTCOD(153) =   725
      BLTNAM(153) =  'PERDITA'

      BLTCOD(154) =   726
      BLTNAM(154) =  'MAB'

      BLTCOD(155) =   727
      BLTNAM(155) =  'CUPID'

      BLTCOD(156) =   899
      BLTNAM(156) =  'NEPTUNE'

      BLTCOD(157) =   801
      BLTNAM(157) =  'TRITON'

      BLTCOD(158) =   802
      BLTNAM(158) =  'NEREID'

      BLTCOD(159) =   803
      BLTNAM(159) =  'NAIAD'

      BLTCOD(160) =   804
      BLTNAM(160) =  'THALASSA'

      BLTCOD(161) =   805
      BLTNAM(161) =  'DESPINA'

      BLTCOD(162) =   806
      BLTNAM(162) =  'GALATEA'

      BLTCOD(163) =   807
      BLTNAM(163) =  'LARISSA'

      BLTCOD(164) =   808
      BLTNAM(164) =  'PROTEUS'

      BLTCOD(165) =   809
      BLTNAM(165) =  'HALIMEDE'

      BLTCOD(166) =   810
      BLTNAM(166) =  'PSAMATHE'

      BLTCOD(167) =   811
      BLTNAM(167) =  'SAO'

      BLTCOD(168) =   812
      BLTNAM(168) =  'LAOMEDEIA'

      BLTCOD(169) =   813
      BLTNAM(169) =  'NESO'

      BLTCOD(170) =   999
      BLTNAM(170) =  'PLUTO'

      BLTCOD(171) =   901
      BLTNAM(171) =  'CHARON'

      BLTCOD(172) =   902
      BLTNAM(172) =  'NIX'

      BLTCOD(173) =   903
      BLTNAM(173) =  'HYDRA'

      BLTCOD(174) =   -1
      BLTNAM(174) =  'GEOTAIL'

      BLTCOD(175) =   -5
      BLTNAM(175) =  'AKATSUKI'

      BLTCOD(176) =   -5
      BLTNAM(176) =  'VCO'

      BLTCOD(177) =   -5
      BLTNAM(177) =  'PLC'

      BLTCOD(178) =   -5
      BLTNAM(178) =  'PLANET-C'

      BLTCOD(179) =   -6
      BLTNAM(179) =  'P6'

      BLTCOD(180) =   -6
      BLTNAM(180) =  'PIONEER-6'

      BLTCOD(181) =   -7
      BLTNAM(181) =  'P7'

      BLTCOD(182) =   -7
      BLTNAM(182) =  'PIONEER-7'

      BLTCOD(183) =   -8
      BLTNAM(183) =  'WIND'

      BLTCOD(184) =   -12
      BLTNAM(184) =  'VENUS ORBITER'

      BLTCOD(185) =   -12
      BLTNAM(185) =  'P12'

      BLTCOD(186) =   -12
      BLTNAM(186) =  'PIONEER 12'

      BLTCOD(187) =   -13
      BLTNAM(187) =  'POLAR'

      BLTCOD(188) =   -18
      BLTNAM(188) =  'MGN'

      BLTCOD(189) =   -18
      BLTNAM(189) =  'MAGELLAN'

      BLTCOD(190) =   -18
      BLTNAM(190) =  'LCROSS'

      BLTCOD(191) =   -20
      BLTNAM(191) =  'P8'

      BLTCOD(192) =   -20
      BLTNAM(192) =  'PIONEER-8'

      BLTCOD(193) =   -21
      BLTNAM(193) =  'SOHO'

      BLTCOD(194) =   -23
      BLTNAM(194) =  'P10'

      BLTCOD(195) =   -23
      BLTNAM(195) =  'PIONEER-10'

      BLTCOD(196) =   -24
      BLTNAM(196) =  'P11'

      BLTCOD(197) =   -24
      BLTNAM(197) =  'PIONEER-11'

      BLTCOD(198) =   -25
      BLTNAM(198) =  'LP'

      BLTCOD(199) =   -25
      BLTNAM(199) =  'LUNAR PROSPECTOR'

      BLTCOD(200) =   -27
      BLTNAM(200) =  'VK1'

      BLTCOD(201) =   -27
      BLTNAM(201) =  'VIKING 1 ORBITER'

      BLTCOD(202) =   -29
      BLTNAM(202) =  'STARDUST'

      BLTCOD(203) =   -29
      BLTNAM(203) =  'SDU'

      BLTCOD(204) =   -29
      BLTNAM(204) =  'NEXT'

      BLTCOD(205) =   -30
      BLTNAM(205) =  'VK2'

      BLTCOD(206) =   -30
      BLTNAM(206) =  'VIKING 2 ORBITER'

      BLTCOD(207) =   -30
      BLTNAM(207) =  'DS-1'

      BLTCOD(208) =   -31
      BLTNAM(208) =  'VG1'

      BLTCOD(209) =   -31
      BLTNAM(209) =  'VOYAGER 1'

      BLTCOD(210) =   -32
      BLTNAM(210) =  'VG2'

      BLTCOD(211) =   -32
      BLTNAM(211) =  'VOYAGER 2'

      BLTCOD(212) =   -40
      BLTNAM(212) =  'CLEMENTINE'

      BLTCOD(213) =   -41
      BLTNAM(213) =  'MEX'

      BLTCOD(214) =   -41
      BLTNAM(214) =  'MARS EXPRESS'

      BLTCOD(215) =   -44
      BLTNAM(215) =  'BEAGLE2'

      BLTCOD(216) =   -44
      BLTNAM(216) =  'BEAGLE 2'

      BLTCOD(217) =   -46
      BLTNAM(217) =  'MS-T5'

      BLTCOD(218) =   -46
      BLTNAM(218) =  'SAKIGAKE'

      BLTCOD(219) =   -47
      BLTNAM(219) =  'PLANET-A'

      BLTCOD(220) =   -47
      BLTNAM(220) =  'SUISEI'

      BLTCOD(221) =   -47
      BLTNAM(221) =  'GNS'

      BLTCOD(222) =   -47
      BLTNAM(222) =  'GENESIS'

      BLTCOD(223) =   -48
      BLTNAM(223) =  'HUBBLE SPACE TELESCOPE'

      BLTCOD(224) =   -48
      BLTNAM(224) =  'HST'

      BLTCOD(225) =   -53
      BLTNAM(225) =  'MARS PATHFINDER'

      BLTCOD(226) =   -53
      BLTNAM(226) =  'MPF'

      BLTCOD(227) =   -53
      BLTNAM(227) =  'MARS ODYSSEY'

      BLTCOD(228) =   -53
      BLTNAM(228) =  'MARS SURVEYOR 01 ORBITER'

      BLTCOD(229) =   -55
      BLTNAM(229) =  'ULYSSES'

      BLTCOD(230) =   -58
      BLTNAM(230) =  'VSOP'

      BLTCOD(231) =   -58
      BLTNAM(231) =  'HALCA'

      BLTCOD(232) =   -59
      BLTNAM(232) =  'RADIOASTRON'

      BLTCOD(233) =   -61
      BLTNAM(233) =  'JUNO'

      BLTCOD(234) =   -66
      BLTNAM(234) =  'VEGA 1'

      BLTCOD(235) =   -67
      BLTNAM(235) =  'VEGA 2'

      BLTCOD(236) =   -68
      BLTNAM(236) =  'MMO'

      BLTCOD(237) =   -68
      BLTNAM(237) =  'MERCURY MAGNETOSPHERIC ORBITER'

      BLTCOD(238) =   -69
      BLTNAM(238) =  'MPO'

      BLTCOD(239) =   -69
      BLTNAM(239) =  'MERCURY PLANETARY ORBITER'

      BLTCOD(240) =   -70
      BLTNAM(240) =  'DEEP IMPACT IMPACTOR SPACECRAFT'

      BLTCOD(241) =   -74
      BLTNAM(241) =  'MRO'

      BLTCOD(242) =   -74
      BLTNAM(242) =  'MARS RECON ORBITER'

      BLTCOD(243) =   -76
      BLTNAM(243) =  'MSL'

      BLTCOD(244) =   -76
      BLTNAM(244) =  'MARS SCIENCE LABORATORY'

      BLTCOD(245) =   -77
      BLTNAM(245) =  'GLL'

      BLTCOD(246) =   -77
      BLTNAM(246) =  'GALILEO ORBITER'

      BLTCOD(247) =   -78
      BLTNAM(247) =  'GIOTTO'

      BLTCOD(248) =   -79
      BLTNAM(248) =  'SPITZER'

      BLTCOD(249) =   -79
      BLTNAM(249) =  'SPACE INFRARED TELESCOPE FACILITY'

      BLTCOD(250) =   -79
      BLTNAM(250) =  'SIRTF'

      BLTCOD(251) =   -81
      BLTNAM(251) =  'CASSINI ITL'

      BLTCOD(252) =   -82
      BLTNAM(252) =  'CAS'

      BLTCOD(253) =   -82
      BLTNAM(253) =  'CASSINI'

      BLTCOD(254) =   -84
      BLTNAM(254) =  'PHOENIX'

      BLTCOD(255) =   -85
      BLTNAM(255) =  'LRO'

      BLTCOD(256) =   -85
      BLTNAM(256) =  'LUNAR RECON ORBITER'

      BLTCOD(257) =   -85
      BLTNAM(257) =  'LUNAR RECONNAISSANCE ORBITER'

      BLTCOD(258) =   -86
      BLTNAM(258) =  'CH1'

      BLTCOD(259) =   -86
      BLTNAM(259) =  'CHANDRAYAAN-1'

      BLTCOD(260) =   -90
      BLTNAM(260) =  'CASSINI SIMULATION'

      BLTCOD(261) =   -93
      BLTNAM(261) =  'NEAR EARTH ASTEROID RENDEZVOUS'

      BLTCOD(262) =   -93
      BLTNAM(262) =  'NEAR'

      BLTCOD(263) =   -94
      BLTNAM(263) =  'MO'

      BLTCOD(264) =   -94
      BLTNAM(264) =  'MARS OBSERVER'

      BLTCOD(265) =   -94
      BLTNAM(265) =  'MGS'

      BLTCOD(266) =   -94
      BLTNAM(266) =  'MARS GLOBAL SURVEYOR'

      BLTCOD(267) =   -95
      BLTNAM(267) =  'MGS SIMULATION'

      BLTCOD(268) =   -97
      BLTNAM(268) =  'TOPEX/POSEIDON'

      BLTCOD(269) =   -98
      BLTNAM(269) =  'NEW HORIZONS'

      BLTCOD(270) =   -107
      BLTNAM(270) =  'TROPICAL RAINFALL MEASURING MISSION'

      BLTCOD(271) =   -107
      BLTNAM(271) =  'TRMM'

      BLTCOD(272) =   -112
      BLTNAM(272) =  'ICE'

      BLTCOD(273) =   -116
      BLTNAM(273) =  'MARS POLAR LANDER'

      BLTCOD(274) =   -116
      BLTNAM(274) =  'MPL'

      BLTCOD(275) =   -121
      BLTNAM(275) =  'BEPICOLOMBO'

      BLTCOD(276) =   -127
      BLTNAM(276) =  'MARS CLIMATE ORBITER'

      BLTCOD(277) =   -127
      BLTNAM(277) =  'MCO'

      BLTCOD(278) =   -130
      BLTNAM(278) =  'MUSES-C'

      BLTCOD(279) =   -130
      BLTNAM(279) =  'HAYABUSA'

      BLTCOD(280) =   -131
      BLTNAM(280) =  'SELENE'

      BLTCOD(281) =   -131
      BLTNAM(281) =  'KAGUYA'

      BLTCOD(282) =   -135
      BLTNAM(282) =  'DRTS-W'

      BLTCOD(283) =   -140
      BLTNAM(283) =  'EPOCH'

      BLTCOD(284) =   -140
      BLTNAM(284) =  'DIXI'

      BLTCOD(285) =   -140
      BLTNAM(285) =  'EPOXI'

      BLTCOD(286) =   -140
      BLTNAM(286) =  'DEEP IMPACT FLYBY SPACECRAFT'

      BLTCOD(287) =   -142
      BLTNAM(287) =  'TERRA'

      BLTCOD(288) =   -142
      BLTNAM(288) =  'EOS-AM1'

      BLTCOD(289) =   -146
      BLTNAM(289) =  'LUNAR-A'

      BLTCOD(290) =   -150
      BLTNAM(290) =  'CASSINI PROBE'

      BLTCOD(291) =   -150
      BLTNAM(291) =  'HUYGENS PROBE'

      BLTCOD(292) =   -150
      BLTNAM(292) =  'CASP'

      BLTCOD(293) =   -151
      BLTNAM(293) =  'AXAF'

      BLTCOD(294) =   -151
      BLTNAM(294) =  'CHANDRA'

      BLTCOD(295) =   -154
      BLTNAM(295) =  'AQUA'

      BLTCOD(296) =   -159
      BLTNAM(296) =  'EUROPA ORBITER'

      BLTCOD(297) =   -164
      BLTNAM(297) =  'YOHKOH'

      BLTCOD(298) =   -164
      BLTNAM(298) =  'SOLAR-A'

      BLTCOD(299) =   -165
      BLTNAM(299) =  'MAP'

      BLTCOD(300) =   -166
      BLTNAM(300) =  'IMAGE'

      BLTCOD(301) =   -177
      BLTNAM(301) =  'GRAIL-A'

      BLTCOD(302) =   -178
      BLTNAM(302) =  'PLANET-B'

      BLTCOD(303) =   -178
      BLTNAM(303) =  'NOZOMI'

      BLTCOD(304) =   -181
      BLTNAM(304) =  'GRAIL-B'

      BLTCOD(305) =   -183
      BLTNAM(305) =  'CLUSTER 1'

      BLTCOD(306) =   -185
      BLTNAM(306) =  'CLUSTER 2'

      BLTCOD(307) =   -187
      BLTNAM(307) =  'SOLAR PROBE'

      BLTCOD(308) =   -188
      BLTNAM(308) =  'MUSES-B'

      BLTCOD(309) =   -190
      BLTNAM(309) =  'SIM'

      BLTCOD(310) =   -194
      BLTNAM(310) =  'CLUSTER 3'

      BLTCOD(311) =   -196
      BLTNAM(311) =  'CLUSTER 4'

      BLTCOD(312) =   -198
      BLTNAM(312) =  'INTEGRAL'

      BLTCOD(313) =   -200
      BLTNAM(313) =  'CONTOUR'

      BLTCOD(314) =   -202
      BLTNAM(314) =  'MAVEN'

      BLTCOD(315) =   -203
      BLTNAM(315) =  'DAWN'

      BLTCOD(316) =   -205
      BLTNAM(316) =  'SOIL MOISTURE ACTIVE AND PASSIVE'

      BLTCOD(317) =   -205
      BLTNAM(317) =  'SMAP'

      BLTCOD(318) =   -212
      BLTNAM(318) =  'STV51'

      BLTCOD(319) =   -213
      BLTNAM(319) =  'STV52'

      BLTCOD(320) =   -214
      BLTNAM(320) =  'STV53'

      BLTCOD(321) =   -226
      BLTNAM(321) =  'ROSETTA'

      BLTCOD(322) =   -227
      BLTNAM(322) =  'KEPLER'

      BLTCOD(323) =   -228
      BLTNAM(323) =  'GLL PROBE'

      BLTCOD(324) =   -228
      BLTNAM(324) =  'GALILEO PROBE'

      BLTCOD(325) =   -234
      BLTNAM(325) =  'STEREO AHEAD'

      BLTCOD(326) =   -235
      BLTNAM(326) =  'STEREO BEHIND'

      BLTCOD(327) =   -236
      BLTNAM(327) =  'MESSENGER'

      BLTCOD(328) =   -238
      BLTNAM(328) =  'SMART1'

      BLTCOD(329) =   -238
      BLTNAM(329) =  'SM1'

      BLTCOD(330) =   -238
      BLTNAM(330) =  'S1'

      BLTCOD(331) =   -238
      BLTNAM(331) =  'SMART-1'

      BLTCOD(332) =   -248
      BLTNAM(332) =  'VEX'

      BLTCOD(333) =   -248
      BLTNAM(333) =  'VENUS EXPRESS'

      BLTCOD(334) =   -253
      BLTNAM(334) =  'OPPORTUNITY'

      BLTCOD(335) =   -253
      BLTNAM(335) =  'MER-1'

      BLTCOD(336) =   -254
      BLTNAM(336) =  'SPIRIT'

      BLTCOD(337) =   -254
      BLTNAM(337) =  'MER-2'

      BLTCOD(338) =   -362
      BLTNAM(338) =  'RADIATION BELT STORM PROBE A'

      BLTCOD(339) =   -362
      BLTNAM(339) =  'RBSP_A'

      BLTCOD(340) =   -363
      BLTNAM(340) =  'RADIATION BELT STORM PROBE B'

      BLTCOD(341) =   -363
      BLTNAM(341) =  'RBSP_B'

      BLTCOD(342) =   -486
      BLTNAM(342) =  'HERSCHEL'

      BLTCOD(343) =   -489
      BLTNAM(343) =  'PLANCK'

      BLTCOD(344) =   -500
      BLTNAM(344) =  'RSAT'

      BLTCOD(345) =   -500
      BLTNAM(345) =  'SELENE Relay Satellite'

      BLTCOD(346) =   -500
      BLTNAM(346) =  'SELENE Rstar'

      BLTCOD(347) =   -500
      BLTNAM(347) =  'Rstar'

      BLTCOD(348) =   -502
      BLTNAM(348) =  'VSAT'

      BLTCOD(349) =   -502
      BLTNAM(349) =  'SELENE VLBI Radio Satellite'

      BLTCOD(350) =   -502
      BLTNAM(350) =  'SELENE VRAD Satellite'

      BLTCOD(351) =   -502
      BLTNAM(351) =  'SELENE Vstar'

      BLTCOD(352) =   -502
      BLTNAM(352) =  'Vstar'

      BLTCOD(353) =   -550
      BLTNAM(353) =  'MARS-96'

      BLTCOD(354) =   -550
      BLTNAM(354) =  'M96'

      BLTCOD(355) =   -550
      BLTNAM(355) =  'MARS 96'

      BLTCOD(356) =   -550
      BLTNAM(356) =  'MARS96'

      BLTCOD(357) =   50000001
      BLTNAM(357) =  'SHOEMAKER-LEVY 9-W'

      BLTCOD(358) =   50000002
      BLTNAM(358) =  'SHOEMAKER-LEVY 9-V'

      BLTCOD(359) =   50000003
      BLTNAM(359) =  'SHOEMAKER-LEVY 9-U'

      BLTCOD(360) =   50000004
      BLTNAM(360) =  'SHOEMAKER-LEVY 9-T'

      BLTCOD(361) =   50000005
      BLTNAM(361) =  'SHOEMAKER-LEVY 9-S'

      BLTCOD(362) =   50000006
      BLTNAM(362) =  'SHOEMAKER-LEVY 9-R'

      BLTCOD(363) =   50000007
      BLTNAM(363) =  'SHOEMAKER-LEVY 9-Q'

      BLTCOD(364) =   50000008
      BLTNAM(364) =  'SHOEMAKER-LEVY 9-P'

      BLTCOD(365) =   50000009
      BLTNAM(365) =  'SHOEMAKER-LEVY 9-N'

      BLTCOD(366) =   50000010
      BLTNAM(366) =  'SHOEMAKER-LEVY 9-M'

      BLTCOD(367) =   50000011
      BLTNAM(367) =  'SHOEMAKER-LEVY 9-L'

      BLTCOD(368) =   50000012
      BLTNAM(368) =  'SHOEMAKER-LEVY 9-K'

      BLTCOD(369) =   50000013
      BLTNAM(369) =  'SHOEMAKER-LEVY 9-J'

      BLTCOD(370) =   50000014
      BLTNAM(370) =  'SHOEMAKER-LEVY 9-H'

      BLTCOD(371) =   50000015
      BLTNAM(371) =  'SHOEMAKER-LEVY 9-G'

      BLTCOD(372) =   50000016
      BLTNAM(372) =  'SHOEMAKER-LEVY 9-F'

      BLTCOD(373) =   50000017
      BLTNAM(373) =  'SHOEMAKER-LEVY 9-E'

      BLTCOD(374) =   50000018
      BLTNAM(374) =  'SHOEMAKER-LEVY 9-D'

      BLTCOD(375) =   50000019
      BLTNAM(375) =  'SHOEMAKER-LEVY 9-C'

      BLTCOD(376) =   50000020
      BLTNAM(376) =  'SHOEMAKER-LEVY 9-B'

      BLTCOD(377) =   50000021
      BLTNAM(377) =  'SHOEMAKER-LEVY 9-A'

      BLTCOD(378) =   50000022
      BLTNAM(378) =  'SHOEMAKER-LEVY 9-Q1'

      BLTCOD(379) =   50000023
      BLTNAM(379) =  'SHOEMAKER-LEVY 9-P2'

      BLTCOD(380) =   1000001
      BLTNAM(380) =  'AREND'

      BLTCOD(381) =   1000002
      BLTNAM(381) =  'AREND-RIGAUX'

      BLTCOD(382) =   1000003
      BLTNAM(382) =  'ASHBROOK-JACKSON'

      BLTCOD(383) =   1000004
      BLTNAM(383) =  'BOETHIN'

      BLTCOD(384) =   1000005
      BLTNAM(384) =  'BORRELLY'

      BLTCOD(385) =   1000006
      BLTNAM(385) =  'BOWELL-SKIFF'

      BLTCOD(386) =   1000007
      BLTNAM(386) =  'BRADFIELD'

      BLTCOD(387) =   1000008
      BLTNAM(387) =  'BROOKS 2'

      BLTCOD(388) =   1000009
      BLTNAM(388) =  'BRORSEN-METCALF'

      BLTCOD(389) =   1000010
      BLTNAM(389) =  'BUS'

      BLTCOD(390) =   1000011
      BLTNAM(390) =  'CHERNYKH'

      BLTCOD(391) =   1000012
      BLTNAM(391) =  '67P/CHURYUMOV-GERASIMENKO (1969 R1)'

      BLTCOD(392) =   1000012
      BLTNAM(392) =  'CHURYUMOV-GERASIMENKO'

      BLTCOD(393) =   1000013
      BLTNAM(393) =  'CIFFREO'

      BLTCOD(394) =   1000014
      BLTNAM(394) =  'CLARK'

      BLTCOD(395) =   1000015
      BLTNAM(395) =  'COMAS SOLA'

      BLTCOD(396) =   1000016
      BLTNAM(396) =  'CROMMELIN'

      BLTCOD(397) =   1000017
      BLTNAM(397) =  'D''ARREST'

      BLTCOD(398) =   1000018
      BLTNAM(398) =  'DANIEL'

      BLTCOD(399) =   1000019
      BLTNAM(399) =  'DE VICO-SWIFT'

      BLTCOD(400) =   1000020
      BLTNAM(400) =  'DENNING-FUJIKAWA'

      BLTCOD(401) =   1000021
      BLTNAM(401) =  'DU TOIT 1'

      BLTCOD(402) =   1000022
      BLTNAM(402) =  'DU TOIT-HARTLEY'

      BLTCOD(403) =   1000023
      BLTNAM(403) =  'DUTOIT-NEUJMIN-DELPORTE'

      BLTCOD(404) =   1000024
      BLTNAM(404) =  'DUBIAGO'

      BLTCOD(405) =   1000025
      BLTNAM(405) =  'ENCKE'

      BLTCOD(406) =   1000026
      BLTNAM(406) =  'FAYE'

      BLTCOD(407) =   1000027
      BLTNAM(407) =  'FINLAY'

      BLTCOD(408) =   1000028
      BLTNAM(408) =  'FORBES'

      BLTCOD(409) =   1000029
      BLTNAM(409) =  'GEHRELS 1'

      BLTCOD(410) =   1000030
      BLTNAM(410) =  'GEHRELS 2'

      BLTCOD(411) =   1000031
      BLTNAM(411) =  'GEHRELS 3'

      BLTCOD(412) =   1000032
      BLTNAM(412) =  'GIACOBINI-ZINNER'

      BLTCOD(413) =   1000033
      BLTNAM(413) =  'GICLAS'

      BLTCOD(414) =   1000034
      BLTNAM(414) =  'GRIGG-SKJELLERUP'

      BLTCOD(415) =   1000035
      BLTNAM(415) =  'GUNN'

      BLTCOD(416) =   1000036
      BLTNAM(416) =  'HALLEY'

      BLTCOD(417) =   1000037
      BLTNAM(417) =  'HANEDA-CAMPOS'

      BLTCOD(418) =   1000038
      BLTNAM(418) =  'HARRINGTON'

      BLTCOD(419) =   1000039
      BLTNAM(419) =  'HARRINGTON-ABELL'

      BLTCOD(420) =   1000040
      BLTNAM(420) =  'HARTLEY 1'

      BLTCOD(421) =   1000041
      BLTNAM(421) =  'HARTLEY 2'

      BLTCOD(422) =   1000042
      BLTNAM(422) =  'HARTLEY-IRAS'

      BLTCOD(423) =   1000043
      BLTNAM(423) =  'HERSCHEL-RIGOLLET'

      BLTCOD(424) =   1000044
      BLTNAM(424) =  'HOLMES'

      BLTCOD(425) =   1000045
      BLTNAM(425) =  'HONDA-MRKOS-PAJDUSAKOVA'

      BLTCOD(426) =   1000046
      BLTNAM(426) =  'HOWELL'

      BLTCOD(427) =   1000047
      BLTNAM(427) =  'IRAS'

      BLTCOD(428) =   1000048
      BLTNAM(428) =  'JACKSON-NEUJMIN'

      BLTCOD(429) =   1000049
      BLTNAM(429) =  'JOHNSON'

      BLTCOD(430) =   1000050
      BLTNAM(430) =  'KEARNS-KWEE'

      BLTCOD(431) =   1000051
      BLTNAM(431) =  'KLEMOLA'

      BLTCOD(432) =   1000052
      BLTNAM(432) =  'KOHOUTEK'

      BLTCOD(433) =   1000053
      BLTNAM(433) =  'KOJIMA'

      BLTCOD(434) =   1000054
      BLTNAM(434) =  'KOPFF'

      BLTCOD(435) =   1000055
      BLTNAM(435) =  'KOWAL 1'

      BLTCOD(436) =   1000056
      BLTNAM(436) =  'KOWAL 2'

      BLTCOD(437) =   1000057
      BLTNAM(437) =  'KOWAL-MRKOS'

      BLTCOD(438) =   1000058
      BLTNAM(438) =  'KOWAL-VAVROVA'

      BLTCOD(439) =   1000059
      BLTNAM(439) =  'LONGMORE'

      BLTCOD(440) =   1000060
      BLTNAM(440) =  'LOVAS 1'

      BLTCOD(441) =   1000061
      BLTNAM(441) =  'MACHHOLZ'

      BLTCOD(442) =   1000062
      BLTNAM(442) =  'MAURY'

      BLTCOD(443) =   1000063
      BLTNAM(443) =  'NEUJMIN 1'

      BLTCOD(444) =   1000064
      BLTNAM(444) =  'NEUJMIN 2'

      BLTCOD(445) =   1000065
      BLTNAM(445) =  'NEUJMIN 3'

      BLTCOD(446) =   1000066
      BLTNAM(446) =  'OLBERS'

      BLTCOD(447) =   1000067
      BLTNAM(447) =  'PETERS-HARTLEY'

      BLTCOD(448) =   1000068
      BLTNAM(448) =  'PONS-BROOKS'

      BLTCOD(449) =   1000069
      BLTNAM(449) =  'PONS-WINNECKE'

      BLTCOD(450) =   1000070
      BLTNAM(450) =  'REINMUTH 1'

      BLTCOD(451) =   1000071
      BLTNAM(451) =  'REINMUTH 2'

      BLTCOD(452) =   1000072
      BLTNAM(452) =  'RUSSELL 1'

      BLTCOD(453) =   1000073
      BLTNAM(453) =  'RUSSELL 2'

      BLTCOD(454) =   1000074
      BLTNAM(454) =  'RUSSELL 3'

      BLTCOD(455) =   1000075
      BLTNAM(455) =  'RUSSELL 4'

      BLTCOD(456) =   1000076
      BLTNAM(456) =  'SANGUIN'

      BLTCOD(457) =   1000077
      BLTNAM(457) =  'SCHAUMASSE'

      BLTCOD(458) =   1000078
      BLTNAM(458) =  'SCHUSTER'

      BLTCOD(459) =   1000079
      BLTNAM(459) =  'SCHWASSMANN-WACHMANN 1'

      BLTCOD(460) =   1000080
      BLTNAM(460) =  'SCHWASSMANN-WACHMANN 2'

      BLTCOD(461) =   1000081
      BLTNAM(461) =  'SCHWASSMANN-WACHMANN 3'

      BLTCOD(462) =   1000082
      BLTNAM(462) =  'SHAJN-SCHALDACH'

      BLTCOD(463) =   1000083
      BLTNAM(463) =  'SHOEMAKER 1'

      BLTCOD(464) =   1000084
      BLTNAM(464) =  'SHOEMAKER 2'

      BLTCOD(465) =   1000085
      BLTNAM(465) =  'SHOEMAKER 3'

      BLTCOD(466) =   1000086
      BLTNAM(466) =  'SINGER-BREWSTER'

      BLTCOD(467) =   1000087
      BLTNAM(467) =  'SLAUGHTER-BURNHAM'

      BLTCOD(468) =   1000088
      BLTNAM(468) =  'SMIRNOVA-CHERNYKH'

      BLTCOD(469) =   1000089
      BLTNAM(469) =  'STEPHAN-OTERMA'

      BLTCOD(470) =   1000090
      BLTNAM(470) =  'SWIFT-GEHRELS'

      BLTCOD(471) =   1000091
      BLTNAM(471) =  'TAKAMIZAWA'

      BLTCOD(472) =   1000092
      BLTNAM(472) =  'TAYLOR'

      BLTCOD(473) =   1000093
      BLTNAM(473) =  'TEMPEL_1'

      BLTCOD(474) =   1000093
      BLTNAM(474) =  'TEMPEL 1'

      BLTCOD(475) =   1000094
      BLTNAM(475) =  'TEMPEL 2'

      BLTCOD(476) =   1000095
      BLTNAM(476) =  'TEMPEL-TUTTLE'

      BLTCOD(477) =   1000096
      BLTNAM(477) =  'TRITTON'

      BLTCOD(478) =   1000097
      BLTNAM(478) =  'TSUCHINSHAN 1'

      BLTCOD(479) =   1000098
      BLTNAM(479) =  'TSUCHINSHAN 2'

      BLTCOD(480) =   1000099
      BLTNAM(480) =  'TUTTLE'

      BLTCOD(481) =   1000100
      BLTNAM(481) =  'TUTTLE-GIACOBINI-KRESAK'

      BLTCOD(482) =   1000101
      BLTNAM(482) =  'VAISALA 1'

      BLTCOD(483) =   1000102
      BLTNAM(483) =  'VAN BIESBROECK'

      BLTCOD(484) =   1000103
      BLTNAM(484) =  'VAN HOUTEN'

      BLTCOD(485) =   1000104
      BLTNAM(485) =  'WEST-KOHOUTEK-IKEMURA'

      BLTCOD(486) =   1000105
      BLTNAM(486) =  'WHIPPLE'

      BLTCOD(487) =   1000106
      BLTNAM(487) =  'WILD 1'

      BLTCOD(488) =   1000107
      BLTNAM(488) =  'WILD 2'

      BLTCOD(489) =   1000108
      BLTNAM(489) =  'WILD 3'

      BLTCOD(490) =   1000109
      BLTNAM(490) =  'WIRTANEN'

      BLTCOD(491) =   1000110
      BLTNAM(491) =  'WOLF'

      BLTCOD(492) =   1000111
      BLTNAM(492) =  'WOLF-HARRINGTON'

      BLTCOD(493) =   1000112
      BLTNAM(493) =  'LOVAS 2'

      BLTCOD(494) =   1000113
      BLTNAM(494) =  'URATA-NIIJIMA'

      BLTCOD(495) =   1000114
      BLTNAM(495) =  'WISEMAN-SKIFF'

      BLTCOD(496) =   1000115
      BLTNAM(496) =  'HELIN'

      BLTCOD(497) =   1000116
      BLTNAM(497) =  'MUELLER'

      BLTCOD(498) =   1000117
      BLTNAM(498) =  'SHOEMAKER-HOLT 1'

      BLTCOD(499) =   1000118
      BLTNAM(499) =  'HELIN-ROMAN-CROCKETT'

      BLTCOD(500) =   1000119
      BLTNAM(500) =  'HARTLEY 3'

      BLTCOD(501) =   1000120
      BLTNAM(501) =  'PARKER-HARTLEY'

      BLTCOD(502) =   1000121
      BLTNAM(502) =  'HELIN-ROMAN-ALU 1'

      BLTCOD(503) =   1000122
      BLTNAM(503) =  'WILD 4'

      BLTCOD(504) =   1000123
      BLTNAM(504) =  'MUELLER 2'

      BLTCOD(505) =   1000124
      BLTNAM(505) =  'MUELLER 3'

      BLTCOD(506) =   1000125
      BLTNAM(506) =  'SHOEMAKER-LEVY 1'

      BLTCOD(507) =   1000126
      BLTNAM(507) =  'SHOEMAKER-LEVY 2'

      BLTCOD(508) =   1000127
      BLTNAM(508) =  'HOLT-OLMSTEAD'

      BLTCOD(509) =   1000128
      BLTNAM(509) =  'METCALF-BREWINGTON'

      BLTCOD(510) =   1000129
      BLTNAM(510) =  'LEVY'

      BLTCOD(511) =   1000130
      BLTNAM(511) =  'SHOEMAKER-LEVY 9'

      BLTCOD(512) =   1000131
      BLTNAM(512) =  'HYAKUTAKE'

      BLTCOD(513) =   1000132
      BLTNAM(513) =  'HALE-BOPP'

      BLTCOD(514) =   9511010
      BLTNAM(514) =  'GASPRA'

      BLTCOD(515) =   2431010
      BLTNAM(515) =  'IDA'

      BLTCOD(516) =   2431011
      BLTNAM(516) =  'DACTYL'

      BLTCOD(517) =   2000001
      BLTNAM(517) =  'CERES'

      BLTCOD(518) =   2000004
      BLTNAM(518) =  'VESTA'

      BLTCOD(519) =   2000021
      BLTNAM(519) =  'LUTETIA'

      BLTCOD(520) =   2000216
      BLTNAM(520) =  'KLEOPATRA'

      BLTCOD(521) =   2000433
      BLTNAM(521) =  'EROS'

      BLTCOD(522) =   2000253
      BLTNAM(522) =  'MATHILDE'

      BLTCOD(523) =   2002867
      BLTNAM(523) =  'STEINS'

      BLTCOD(524) =   2009969
      BLTNAM(524) =  '1992KD'

      BLTCOD(525) =   2009969
      BLTNAM(525) =  'BRAILLE'

      BLTCOD(526) =   2004015
      BLTNAM(526) =  'WILSON-HARRINGTON'

      BLTCOD(527) =   2004179
      BLTNAM(527) =  'TOUTATIS'

      BLTCOD(528) =   2025143
      BLTNAM(528) =  'ITOKAWA'

      BLTCOD(529) =   398989
      BLTNAM(529) =  'NOTO'

      BLTCOD(530) =   398990
      BLTNAM(530) =  'NEW NORCIA'

      BLTCOD(531) =   399001
      BLTNAM(531) =  'GOLDSTONE'

      BLTCOD(532) =   399002
      BLTNAM(532) =  'CANBERRA'

      BLTCOD(533) =   399003
      BLTNAM(533) =  'MADRID'

      BLTCOD(534) =   399004
      BLTNAM(534) =  'USUDA'

      BLTCOD(535) =   399005
      BLTNAM(535) =  'DSS-05'

      BLTCOD(536) =   399005
      BLTNAM(536) =  'PARKES'

      BLTCOD(537) =   399012
      BLTNAM(537) =  'DSS-12'

      BLTCOD(538) =   399013
      BLTNAM(538) =  'DSS-13'

      BLTCOD(539) =   399014
      BLTNAM(539) =  'DSS-14'

      BLTCOD(540) =   399015
      BLTNAM(540) =  'DSS-15'

      BLTCOD(541) =   399016
      BLTNAM(541) =  'DSS-16'

      BLTCOD(542) =   399017
      BLTNAM(542) =  'DSS-17'

      BLTCOD(543) =   399023
      BLTNAM(543) =  'DSS-23'

      BLTCOD(544) =   399024
      BLTNAM(544) =  'DSS-24'

      BLTCOD(545) =   399025
      BLTNAM(545) =  'DSS-25'

      BLTCOD(546) =   399026
      BLTNAM(546) =  'DSS-26'

      BLTCOD(547) =   399027
      BLTNAM(547) =  'DSS-27'

      BLTCOD(548) =   399028
      BLTNAM(548) =  'DSS-28'

      BLTCOD(549) =   399033
      BLTNAM(549) =  'DSS-33'

      BLTCOD(550) =   399034
      BLTNAM(550) =  'DSS-34'

      BLTCOD(551) =   399042
      BLTNAM(551) =  'DSS-42'

      BLTCOD(552) =   399043
      BLTNAM(552) =  'DSS-43'

      BLTCOD(553) =   399045
      BLTNAM(553) =  'DSS-45'

      BLTCOD(554) =   399046
      BLTNAM(554) =  'DSS-46'

      BLTCOD(555) =   399049
      BLTNAM(555) =  'DSS-49'

      BLTCOD(556) =   399053
      BLTNAM(556) =  'DSS-53'

      BLTCOD(557) =   399054
      BLTNAM(557) =  'DSS-54'

      BLTCOD(558) =   399055
      BLTNAM(558) =  'DSS-55'

      BLTCOD(559) =   399061
      BLTNAM(559) =  'DSS-61'

      BLTCOD(560) =   399063
      BLTNAM(560) =  'DSS-63'

      BLTCOD(561) =   399064
      BLTNAM(561) =  'DSS-64'

      BLTCOD(562) =   399065
      BLTNAM(562) =  'DSS-65'

      BLTCOD(563) =   399066
      BLTNAM(563) =  'DSS-66'



      RETURN
      END

