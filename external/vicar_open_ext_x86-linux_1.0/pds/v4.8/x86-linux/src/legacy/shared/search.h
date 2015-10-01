/**********************************************************************
 * Component                                                          *
 *    Include file search.h                                           *
 * Used By                                                            *
 *    PDS SEARCHLIB software.                                         *
 * Detailed Description                                               *
 *    Defines symbols, macros, flags, typedefs, and structures        *
 *    used by PDS SEARCHLIB software.                                 *
 * Author and Institution                                             *
 *    Marti D. DeMore/ J.P.L.                                         *
 * Version and Date                                                   *
 *    1.3   March 23, 1992                                            *
 * Change History                                                     *
 *    MDD   08-05-90   Original code.                                 *
 *    DPB   08-31-90   Added the non-ANSI standard ``#ifdef".         *
 *    MDD   03-23-92   Modified function prototypes                   * 
 **********************************************************************/

/*--------------------------------------------------------------------*/
/*                      SEARCH Function Prototypes                    */
/*--------------------------------------------------------------------*/

    /*----------------------------------------------------------------*/
    /*  These are the function prototypes that are used by all ANSI   */
    /*  standard C compilers.  At this time, SUN systems running UNIX */
    /*  do not allow this type of prototype declaration.              */
    /*----------------------------------------------------------------*/

#ifndef SUN_UNIX

long search_string_array (char * [], long, char *);
LOGICAL search_string_list_fwd (STRING_LIST *, char *, long *, long *,
			        LOGICAL);
LOGICAL search_string_list_bwd (STRING_LIST *, char *, long *, long *,
			        LOGICAL);
           
    /*----------------------------------------------------------------*/
    /*  These are the function prototypes that are used by C          */
    /*  compilers that do not follow the ANSI standard for the        */
    /*  declaration of function prototypes.                           */
    /*----------------------------------------------------------------*/

#else

long search_string_array ();
LOGICAL search_string_list_fwd ();
LOGICAL search_string_list_bwd ();
           
#endif

/*--------------------------------------------------------------------*/
/*                        End:  "search.h"                            */
/*--------------------------------------------------------------------*/

