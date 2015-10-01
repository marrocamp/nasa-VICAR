$!****************************************************************************
$!
$! Build proc for MIPL module createfile
$! VPACK Version 1.8, Thursday, January 09, 1997, 16:32:28
$!
$! Execute by entering:		$ @createfile
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module createfile ***"
$!
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Repack .or. Create_PDF .or. Create_Test .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to createfile.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("createfile.imake") .nes. ""
$   then
$      vimake createfile
$      purge createfile.bld
$   else
$      if F$SEARCH("createfile.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake createfile
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @createfile.bld "STD"
$   else
$      @createfile.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create createfile.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack createfile.com -
	-i createfile.imake -
	-p createfile.pdf -
	-t tstcreatefile.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create createfile.imake
#define  PROCEDURE createfile
#define R2LIB 
$ Return
$!#############################################################################
$PDF_File:
$ create createfile.pdf
PROCEDURE help=*
  PARM INPUT    TYPE=STRING
BODY
refgbl  $syschar
write "CREATEFILE version 1-3-97"
!
!  IF  VMS 
if ($syschar(1) = "VAX_VMS")
  DCL open/write FILENAMES &INPUT
  DCL close FILENAMES
!
!      UNIX: COPY the old empty file.
else
  USH cp /dev/null  &INPUT

end-if  

!# annot function="VICAR Utilities"
!# annot keywords=("empty file")

END-PROC
.TITLE
TAE proc to create an empty file
.help
PURPOSE:

CREATEFILE creates an empty file with the specified name.

EXECUTION:
 This utility can be called from a VICAR procedure using the syntax:

 createfile filename 

 where filename is the name to be given to the empty file created.
 
REVISION HISTORY:
   06-1-89  HBM   Initial release.
   01-2-97  SP    Made a UNIX version and a TCL procedure to call either the
                  DCL version or the UNIX version.
.level1
.vari input
Name to be given to the 
file created
.level2
.vari input
INPUT may be specified with or without a directory and may use the ~username 
notation. If specified without a directory (pathname), the current working 
directory is used.  
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstcreatefile.pdf
procedure
!
! To get a full log, you have to do tricks, such as cutting from an xterm window
!or running the UNIX 'script' cmd since type and cat do not go into session.log.
!
refgbl $echo
refgbl $syschar
local  diskdir string
body
let _onfail="continue"
let $echo="yes"
if ($syschar(1) = "UNIX")
  defcmd typeit "ush cat"
else
  defcmd typeit "dcl type"
end-if
!
createfile add1.dat1
typeit     add1.dat1
! See if it works if file already exists.
createfile add1.dat1
typeit     add1.dat1
!
end-proc
$ Return
$!#############################################################################
