/**********************************************************************
 * Component                                                          *
 *    Include file index.h                                            *
 * Used By                                                            *
 *    Software that needs to manipulate indices.                      *
 * Detailed Description                                               *
 *    Defines symbols, macros, flags, typedefs, and structures        *
 *    used by PDS index software.                                     *
 * Author and Institution                                             *
 *    Marti D. DeMore/ JPL                                            *
 * Version and Date                                                   *
 *    2.0   March 16, 1992                                            *
 * Change History                                                     *
 *    MDD   03-14-91   Original code.                                 *
 *    MDD   03-16-92   Removed DD specific stuff.                     *
 **********************************************************************/ 

        /*------------------------------------------------------------*/
        /*  The INDEX_ENTRY typedef is used for storage of entries    */
        /*  in an ODL definition file.                                */
        /*------------------------------------------------------------*/

typedef struct index_entry
{
   char *name;
   char type [PDS_MAXLINE + 1];    
   long byte_offset;
   long size;
   struct index_entry *right_child;
   struct index_entry *left_child;
} INDEX_ENTRY;


/*--------------------------------------------------------------------*/
/*                        Index Function Prototypes                   */
/*--------------------------------------------------------------------*/

    /*----------------------------------------------------------------*/
    /*  These are the function prototypes that are used by all ANSI   */
    /*  standard C compilers.  At this time, SUN systems running UNIX */
    /*  do not allow this type of prototype declaration.              */
    /*----------------------------------------------------------------*/

#ifndef SUN_UNIX                             

INDEX_ENTRY *idx_cleanup_index (INDEX_ENTRY *);
INDEX_ENTRY *idx_insert_index_entry (INDEX_ENTRY *, INDEX_ENTRY *);
LOGICAL idx_search_index (INDEX_ENTRY *, char *, long *, long *);

    /*----------------------------------------------------------------*/
    /*  These are the function prototypes that are used by C          */
    /*  compilers that do not follow the ANSI standard for the        */
    /*  declaration of function prototypes.                           */
    /*----------------------------------------------------------------*/

#else

INDEX_ENTRY *idx_cleanup_index ();
INDEX_ENTRY *idx_insert_index_entry ();
LOGICAL idx_search_index ();

#endif




