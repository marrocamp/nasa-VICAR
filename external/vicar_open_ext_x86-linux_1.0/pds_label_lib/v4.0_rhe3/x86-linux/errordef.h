/**********************************************************************
 * Component                                                          *
 *    Include file errordef.h                                         *
 * Used By                                                            *
 *    PDS ERRORLIB software.                                          *
 * Detailed Description                                               *
 *    Defines symbols, macros, flags, typedefs, and structures        *
 *    used by PDS ERRORLIB software.                                  *
 * Author and Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 * Version and Date                                                   *
 *    1.6   June 12, 1991                                             *
 * Change History                                                     *
 *    DPB   07-05-90   Original code.                                 *
 *    DPB   08-17-90   Removed all symbols and typedefs.              *
 *    DPB   08-29-90   Added err_append_stderr_messages prototype.    *
 *    DPB   08-31-90   Added the non-ANSI standard ``#ifdef".         *
 *    MDD   03-14-91   Added err_write_to_stdout                      *
 *    KLM   03-25-91   Added err_write_to_file                        *
 *    DPB   06-12-91   Added err_object_message                       *
 **********************************************************************/

/*--------------------------------------------------------------------*/
/*                    ERRORLIB Function Prototypes                    */
/*--------------------------------------------------------------------*/

    /*----------------------------------------------------------------*/
    /*  These are the function prototypes that are used by all ANSI   */
    /*  standard C compilers.  At this time, SUN systems running UNIX */
    /*  do not allow this type of prototype declaration.              */
    /*----------------------------------------------------------------*/

#ifndef SUN_UNIX

void err_append_message (int severity, 
                         char *message);

void err_deallocate_list (ERROR_LIST *message_ptr);

void err_deallocate_message (ERROR_LIST *message_ptr);

LOGICAL err_keyword_message (int severity, 
                             char *keyword_name,
                             int line_number,
                             int value_count,
                             char *text);

LOGICAL err_object_message (int severity,
                            char *object_class,
                            int object_line_number,
                            char *text);

void err_write_to_file (FILE *file_ptr,
                        LOGICAL print_severity);

    /*----------------------------------------------------------------*/
    /*  These are the function prototypes that are used by C          */
    /*  compilers that do not follow the ANSI standard for the        */
    /*  declaration of function prototypes.                           */
    /*----------------------------------------------------------------*/

#else

void err_append_message ();
void err_deallocate_list ();
void err_deallocate_message ();
LOGICAL err_keyword_message ();
LOGICAL err_object_message ();
void err_write_to_file ();

#endif

/*--------------------------------------------------------------------*/
/*                       End:  "errordef.h"                           */
/*--------------------------------------------------------------------*/
    

