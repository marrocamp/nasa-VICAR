$!****************************************************************************
$!
$! Build proc for MIPL module flibmb
$! VPACK Version 1.5, Friday, March 19, 1993, 19:46:44
$!
$! Execute by entering:		$ @flibmb
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
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
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module flibmb ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("flibmb.imake") .nes. ""
$   then
$      vimake flibmb
$      purge flibmb.bld
$   else
$      if F$SEARCH("flibmb.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake flibmb
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @flibmb.bld "STD"
$   else
$      @flibmb.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create flibmb.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack flibmb.com -
	-s flibmb.c -
	-i flibmb.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create flibmb.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>

#define C_MASK     0x7f000000
#define F_MASK     0x00ffffff
#define S_MASK     0x80000000
#define NORMALIZED 0x00800000

float flibmb_ (ibm_in)
float *ibm_in;
{
  unsigned long  c2_part, f_part, sign_bit;
  unsigned final = 0; /* Prepared for ORing */
  long c_part;
  float swapped;
  
  /* First, reverse byte order */
  swapped = intibm(ibm_in);

  /* Now, extract fields */

  c2_part = ( (unsigned long) swapped & C_MASK );
  f_part = ( (unsigned long) swapped & F_MASK );
  sign_bit = ( (unsigned long) swapped & S_MASK );

  c_part = c2_part;

  c_part >>= 24;           /* Make it usable */
  c_part -= 64;

  /* Normalize mantissa */

  while ( (f_part & NORMALIZED) != NORMALIZED ) {
    c_part--;
    f_part <<= 1; /* Shift one left and check again */
  }

  c_part +=  128;
  c_part <<= 23;
  
  final |= (sign_bit | c_part | f_part);
  
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create flibmb.imake

#define SUBROUTINE flibmb

#define MODULE_LIST flibmb.c

#define P3_SUBLIB

#define USES_C

$ Return
$!#############################################################################
