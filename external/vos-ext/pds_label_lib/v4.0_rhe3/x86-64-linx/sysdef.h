/**********************************************************************
 * Component                                                          *
 *    Include file sysdef.h                                           *
 * Used By                                                            *
 *    PDS syslib software.                                            *
 * Detailed Description                                               *
 *    Defines symbols, macros, flags, typedefs, and structures        *
 *    used by PDS syslib software.                                    *
 * Author and Institution                                             *
 *    Herbert C. Gamble / J.P.L.                                      *
 *    David P. Bernath / J.P.L.                                       *
 * Version and Date                                                   *
 *    2.2   July 9, 1991                                              *
 * Change History                                                     *
 *    HCG   09-01-90   Original code.                                 *
 *    DPB   10-04-90   Added header, and added sys_exit_system.       *
 *    HCG   10-23-90   Deleted symbol definitions.                    *
 *    DPB   07-09-91   Added sys_get_path.                            *
 **********************************************************************/ 

/*---------------------------------------------------------------------------*/
/*                       SYSLIB Include Statements                           */
/*---------------------------------------------------------------------------*/

#ifdef VAX
#include <descrip>
#include <processes>
#endif

#ifdef MSDOS_TC
#include <process.h>
#endif

#ifdef SUN_UNIX
#endif

/*---------------------------------------------------------------------------*/
/*                        SYSLIB Define Statements                           */
/*---------------------------------------------------------------------------*/

#ifdef VAX
#endif

#ifdef MSDOS_TC
#endif

#ifdef SUN_UNIX
#endif

/*---------------------------------------------------------------------------*/
/*                       SYSLIB Function Prototypes                          */
/*---------------------------------------------------------------------------*/

#ifndef SUN_UNIX

LOGICAL      sys_change_directory (char *directory);
LOGICAL      sys_check_directory_integrity (char *directory,
                                            char *fname,
                                            LOGICAL write_flag);
LOGICAL      sys_copy_file (char *source_file, char *dest_file);
LOGICAL      sys_do_command (char *os_command);
LOGICAL      sys_delete_file (char *file_name);
char        *sys_get_ascii_date (void);
STRING_LIST *sys_get_directory_list (char *directory);
char        *sys_get_current_directory (void);
struct tm   *sys_get_date (void);
STRING_LIST *sys_get_file_list (char *dir_mask);
char        *sys_get_path (char *fname);
char        *sys_get_user_id (void);
char        *sys_make_temp_fname (char *directory);

#else

LOGICAL      sys_change_directory ();
LOGICAL      sys_check_directory_integrity ();
LOGICAL      sys_copy_file ();
LOGICAL      sys_delete_file ();
LOGICAL      sys_do_command ();
char        *sys_get_ascii_date ();
STRING_LIST *sys_get_directory_list ();
char        *sys_get_current_directory ();
struct tm   *sys_get_date ();
STRING_LIST *sys_get_file_list ();
char        *sys_get_path ();
char        *sys_get_user_id ();
char        *sys_make_temp_fname ();


#endif

