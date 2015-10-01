/**********************************************************************
 * Component                                                          *
 *    Include file convert.h                                          *
 * Used By                                                            *
 *    PDS CONVERTLIB software.                                        *
 * Detailed Description                                               *
 *    Defines symbols, macros, flags, typedefs, and structures        *
 *    used by PDS CONVERT software.                                   *
 * Author and Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 * Version and Date                                                   *
 *    1.3  March 23, 1992                                             *
 * Change History                                                     *
 *    DPB   07-05-90   Original code.                                 *
 *    DPB   08-17-90   Removed all symbols and typedefs.              *
 *    MDD   09-20-90   Added ifdefs for use on Sun                    *
 *    MDD   03-23-92   Modified function prototypes.                  *
 **********************************************************************/

/*--------------------------------------------------------------------*/
/*                       CONVERT Function Prototypes                  */
/*--------------------------------------------------------------------*/

#ifndef SUN_UNIX

char *cvt_dos_to_unix_file_spec (char *);
char *cvt_dos_to_vax_file_spec (char *);
char *cvt_unix_to_dos_file_spec (char *);
char *cvt_unix_to_vax_file_spec (char *);
char *cvt_vax_to_dos_file_spec (char *);
char *cvt_vax_to_unix_file_spec (char *);

#else

char *cvt_dos_to_unix_file_spec ();
char *cvt_dos_to_vax_file_spec ();
char *cvt_unix_to_dos_file_spec ();
char *cvt_unix_to_vax_file_spec ();
char *cvt_vax_to_dos_file_spec ();
char *cvt_vax_to_unix_file_spec ();

#endif

/*--------------------------------------------------------------------*/
/*                      End:  "convert.h"                             */
/*--------------------------------------------------------------------*/

