/**********************************************************************
 * Component                                                          *
 *    Include file ddaccess.h                                         *
 * Used By                                                            *
 *    Software that needs to access the data dictionary               *
 * Detailed Description                                               *
 *    Defines symbols, macros, flags, typedefs, and structures        *
 *    used by PDS DD access software.                                 *
 * Author and Institution                                             *
 *    Marti D. DeMore/ JPL                                            *
 * Version and Date                                                   *
 *    1.1   March 23, 1992                                            *
 * Change History                                                     *
 *    MDD   04-05-91   Original code.                                 *
 *    MDD   03-23-92   Modified function prototypes.                  *
 **********************************************************************/ 

/*--------------------------------------------------------------------*/
/*                      DD Access Function Prototypes                 */
/*--------------------------------------------------------------------*/

    /*----------------------------------------------------------------*/
    /*  These are the function prototypes that are used by all ANSI   */
    /*  standard C compilers.  At this time, SUN systems running UNIX */
    /*  do not allow this type of prototype declaration.              */
    /*----------------------------------------------------------------*/

#ifndef SUN_UNIX                             

void dd_cleanup_defs (void);
char *dd_get_data_type (char *);
#ifdef LV_DDICT                        /* This is for the data dictionary*/
LOGICAL dd_get_definition (FILE *, char *, char *, int);/*instead of lvtool*/
#else
LOGICAL dd_get_definition (char *, char *, int);
#endif
void dd_get_all_definitions (STRING_LIST *, char *, int);
char *dd_get_object_class (char *);
char *dd_get_description (char *, char *);
STRING_LIST *dd_get_id_members (char *);
LOGICAL dd_get_object_definition (char *, int);
LOGICAL dd_get_length_range (char *, long *, long *);
STRING_LIST *dd_get_optional_members (char *);
STRING_LIST *dd_get_optional_objects (char *);
STRING_LIST *dd_get_required_members (char *);
STRING_LIST *dd_get_required_objects (char *);
char *dd_get_standard_value_type (char *);
STRING_LIST *dd_get_standard_values (char *);
LOGICAL dd_get_value_range (char *, double *, double *);
LOGICAL dd_init (char *);
LOGICAL dd_name_exists (char *, char *);
char *dd_get_object_class (char *);
LOGICAL dd_search_index (char *, char *, long *, long *);
ERROR_LIST *dd_unalias (AGGREGATE);

    /*----------------------------------------------------------------*/
    /*  These are the function prototypes that are used by C          */
    /*  compilers that do not follow the ANSI standard for the        */
    /*  declaration of function prototypes.                           */
    /*----------------------------------------------------------------*/

#else

void dd_cleanup_defs ();
char *dd_get_data_type ();
LOGICAL dd_get_definition ();
void dd_get_all_definitions ();
char *dd_get_description ();
STRING_LIST *dd_get_id_members ();                                       
char *dd_get_object_class ();
LOGICAL dd_get_object_definition ();
LOGICAL dd_get_length_range ();                                      
STRING_LIST *dd_get_optional_members ();
STRING_LIST *dd_get_optional_objects ();
STRING_LIST *dd_get_required_members ();
STRING_LIST *dd_get_required_objects ();
char *dd_get_standard_value_type ();
STRING_LIST *dd_get_standard_values ();
LOGICAL dd_get_value_range ();
LOGICAL dd_init ();
LOGICAL dd_name_exists ();
LOGICAL dd_search_index ();
ERROR_LIST *dd_unalias ();

#endif
