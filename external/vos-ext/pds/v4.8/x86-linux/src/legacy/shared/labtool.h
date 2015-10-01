
/**********************************************************************
 * Component                                                          *
 *    Include file labtool.h                                          *
 * Used By                                                            *
 *    Software that is needed by high-level label routines            *
 * Detailed Description                                               *
 *    Defines symbols, macros, flags, typedefs, and structures        *
 *    used by PDS labtool software                                    *
 * Author and Institution                                             *
 *    Marti D. DeMore/ JPL                                            *
 * Version and Date                                                   *
 *    1.1   March 23, 1992                                            *
 * Change History                                                     *
 *    MDD   09-03-91   Original code.                                 *
 *    MDD   03-23-92   Modified function prototypes.                  *
 **********************************************************************/

#define PDS_LAB_ADD             0
#define PDS_LAB_CHANGE_NAME     1
#define PDS_LAB_REMOVE          2
#define PDS_LAB_CHANGE_VALUE    3

/*--------------------------------------------------------------------*/
/*                     Labtool Function Prototypes                    */
/*--------------------------------------------------------------------*/

    /*----------------------------------------------------------------*/
    /*  These are the function prototypes that are used by all ANSI   */
    /*  standard C compilers.  At this time, SUN systems running UNIX */
    /*  do not allow this type of prototype declaration.              */
    /*----------------------------------------------------------------*/

#ifndef SUN_UNIX

LOGICAL lt_global_keyword_change (AGGREGATE, char *, int, char *, char *);
LOGICAL lt_move_keyword (AGGREGATE, char *, char *, int, char *, int,
                         char *, char *, int, LOGICAL);
LOGICAL lt_replace_keyword (AGGREGATE, char *, char *, int, char *, int,
                            char *);
LOGICAL lt_get_pointer (AGGREGATE, char *, int, POINTER_INFO *);
LOGICAL lt_add_pointer (AGGREGATE, char *, POINTER_INFO *);
AGGREGATE lt_read_expanded_label (char *);

    /*----------------------------------------------------------------*/
    /*  These are the function prototypes that are used by C          */
    /*  compilers that do not follow the ANSI standard for the        */
    /*  declaration of function prototypes.                           */
    /*----------------------------------------------------------------*/

#else

LOGICAL lt_global_keyword_change ();
LOGICAL lt_move_keyword ();
LOGICAL lt_replace_keyword ();
LOGICAL lt_get_pointer ();
LOGICAL lt_add_pointer ();
AGGREGATE lt_read_expanded_label ();

#endif

