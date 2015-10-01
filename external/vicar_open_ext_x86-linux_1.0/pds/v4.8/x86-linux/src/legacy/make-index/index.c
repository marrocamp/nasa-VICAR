/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Component                                                           * 
 *    Library index.c                                                  *
 * Abstract                                                            *
 *    Common ODL definition index routines.                            *
 * Detailed Description                                                *
 *    The index file is a library of routine designed to help build    *
 *    and access indices into ODL definition files.                    *
 * Internal References                                                 *
 *    idx_cleanup_index                                                *
 *    idx_insert_index_entry                                           *
 *    idx_search_index                                                 *
 * Authors and Institutions                                            *
 *    Marti D. DeMore / J.P.L.                                         *
 * Version and Date                                                    *
 *    1.0   March 16, 1992                                             *
 * Change History                                                      *
 *    MDD   03-16-92   Original library.                               *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "pdsdef.h"
#include "index.h"
#include "errordef.h"


/**********************************************************************
 *$Component                                                          *
 *    INDEX_ENTRY *idx_cleanup_index (index_root)                     *
 *$Abstract                                                           *
 *    Frees memory pointed to by the index root.                      *
 *$Keywords                                                           *
 *    INDEX                                                           *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    index_root:                                                     *
 *        The index_root variable is a pointer to the root of a       *
 *        sorted tree of index entries.                               *
 *$Returns                                                            *
 *    index_root:                                                     *
 *        The index_root variable is a pointer to the root of a       *
 *        sorted tree of index entries.                               *
 *$Detailed_Description                                               *
 *    The idx_cleanup_index routine frees the memory pointed to by the*
 *    index root. This routine always returns a NULL index entry      *
 *    pointer.                                                        *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    2.0   March 16, 1992                                            *
 *$Change_History                                                     *
 *    MDD   03-16-92   Changed name and moved from ddindex.c          *
 **********************************************************************/

INDEX_ENTRY *idx_cleanup_index (index_root)

INDEX_ENTRY *index_root;

{
   if (index_root != NULL)
   {
      idx_cleanup_index (index_root -> left_child);
      idx_cleanup_index (index_root -> right_child);
      Lemme_Go(index_root -> name);
      Lemme_Go(index_root);
   }
   return (NULL);
}


/**********************************************************************
 *$Component                                                          *
 *    INDEX_ENTRY *idx_insert_index_entry (index_root, new_entry)     *
 *$Abstract                                                           *
 *    Adds a new index entry to the sorted tree of index entries      *
 *$Keywords                                                           *
 *    INDEX                                                           *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    index_root:                                                     *
 *        The index_root variable is a pointer to the root of a       *
 *        sorted tree of index entries.                               *
 *    new_entry:                                                      *
 *        The new_entry variable is a pointer to a new index entry    *
 *        to be added to the sorted tree of index entries.            *
 *$Returns                                                            *
 *    index_root:                                                     *
 *        The index_root variable is a pointer to the root of a       *
 *        sorted tree of index entries.                               *
 *$Detailed_Description                                               *
 *    The idx_insert_index_entry routine adds a new index entry to the*
 *    sorted tree of index entries. It locates the proper entry       *
 *    position using recursive calls.                                 *
 *$External_References                                                *
 *    None                                                            *
 *$Error_Handling                                                     *
 *    If an entry already exists which is the same as the new entry,  *
 *    this routine appends a message to the global list of messages   *
 *    and does not insert the new entry into the tree.                *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    2.1   October 19, 1992                                          *
 *$Change_History                                                     *
 *    MDD   03-21-91   Original generation.                           *
 *    MDD   03-16-92   Changed name and moved from ddindex.c          *
 *    MDD   10-19-92   Added free statements for duplicates not       *
 *                     inserted into tree.                            *
 **********************************************************************/

INDEX_ENTRY *idx_insert_index_entry (index_root, new_entry)

INDEX_ENTRY *index_root;
INDEX_ENTRY *new_entry;

{
   char *err_msg;
   int compare;

   /** BEGIN **/
   /*-------------------------------------------------------------------*/
   /** IF this subtree is empty THEN                                   **/
   /**    set the root of this subtree to the new entry                **/
   /*-------------------------------------------------------------------*/

   if (index_root == NULL)
   {
      index_root = new_entry;
   }
   /*-------------------------------------------------------------------*/
   /** ELSE                                                            **/
   /*-------------------------------------------------------------------*/

   else
   {
      /*----------------------------------------------------------------*/
      /** compare the new entry to the root                            **/
      /** IF the new entry is the same THEN                            **/
      /*----------------------------------------------------------------*/

      compare = strcmp (index_root -> name, new_entry -> name);
      if (compare == 0)
      {

         /*-------------------------------------------------------------*/
         /** compare the type of the new entry to the root             **/
         /** IF they are the same THEN                                 **/
         /**    print a warning and do not add the new entry           **/
         /*-------------------------------------------------------------*/

         compare = strcmp (index_root -> type, new_entry -> type);
         if (compare == 0)
         {
            Malloc_String(err_msg, PDS_MAXLINE);
            sprintf (err_msg, "Duplicate definition for %s %s", 
                         new_entry -> type, new_entry -> name);
	    err_append_message (WARNING, err_msg);
            Lemme_Go(err_msg);
            Lemme_Go(new_entry->name);
            Lemme_Go(new_entry);
         }
         /*-------------------------------------------------------------*/
         /** ENDIF they are the same...                                **/
         /*-------------------------------------------------------------*/
      }

      /*----------------------------------------------------------------*/
      /** ENDIF the root is the same...                                **/
      /** IF the root is greater than the new entry THEN               **/
      /**    insert it in the left subtree                             **/
      /*----------------------------------------------------------------*/

      if (compare > 0)
      {
         index_root -> left_child = 
                idx_insert_index_entry (index_root -> left_child, new_entry);
      }
      /*----------------------------------------------------------------*/
      /** ENDIF the root is greater...                                 **/
      /** IF the root is less than the new entry                       **/
      /**    insert the new entry in the right subtree                 **/
      /*----------------------------------------------------------------*/

      if (compare < 0)
      {
         index_root -> right_child = 
                idx_insert_index_entry (index_root -> right_child, new_entry);
      }
      /*----------------------------------------------------------------*/
      /** ENDIF the root is less...                                    **/
      /*----------------------------------------------------------------*/
   }
   /*-------------------------------------------------------------------*/
   /** ENDIF this subtree is empty                                     **/
   /*-------------------------------------------------------------------*/

   return (index_root);

   /** END idx_insert_index_entry **/
}             


/**********************************************************************
 *$Component                                                          *
 *    LOGICAL *idx_search_index (index_root, search_name, def_offset, *
 *                               def_size)                            *
 *$Abstract                                                           *
 *    Searches an index for the given entry.                          *
 *$Keywords                                                           *
 *    INDEX                                                           *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    index_root:                                                     *
 *        The index_root variable is a pointer to the root of a       *
 *        sorted tree of index entries.                               *
 *    search_name:                                                    *
 *        The search_name variable is a character string containing   *
 *        the name of an item to be searched for in an index tree.    *
 *$Outputs                                                            *
 *    def_offset:                                                     *
 *        The def_offset variable is an integer representing the byte *
 *        offset of an ODL definition in a definition file for        *
 *        which an index has been created, i.e., a help definition    *
 *        file or a data dictionary definition file.                  *
 *    def_size:                                                       *
 *        The def_size variable is an integer representing the        *
 *        size in bytes of an ODL definition in a definition file for *
 *        which an index has been created, i.e., a help definition    *
 *        file or a data dictionary definition file.                  *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure.       *
 *$Detailed_Description                                               *
 *    The idx_search_index routine is a recursive procedure to search *
 *    a binary tree for the given index entry.  It is assumed that    *
 *    the tree consists of INDEX_ENTRY structures, each of which      *
 *    stores the location and size of one ODL definition in a file    *
 *    of many ODL definitions.                                        *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.0   March 16, 1992                                            *
 *$Change_History                                                     *
 *    MDD   03-16-92   Original generation.                           *
 **********************************************************************/

LOGICAL idx_search_index (index_root, search_name, def_offset, def_size)

INDEX_ENTRY *index_root;
char *search_name;
long *def_offset;
long *def_size;

{
    int compare;

/** BEGIN **/

    /*------------------------------------------------------------------*/
    /** IF the index tree is empty THEN                                **/
    /**    return FAILURE                                              **/
    /*------------------------------------------------------------------*/

    if (index_root == NULL)
       return (FALSE);

    /*------------------------------------------------------------------*/
    /** ELSE IF the root is the node we are looking for THEN           **/
    /**    set output values and return SUCCESS                        **/
    /*------------------------------------------------------------------*/

    else if ((compare = strcmp (index_root -> name, search_name)) == 0)
    {
       *def_offset = index_root -> byte_offset;
       *def_size = index_root -> size;
       return (TRUE);
    }

    /*------------------------------------------------------------------*/
    /** ELSE IF the root is less than the node we want THEN            **/
    /**    search the right subtree                                    **/
    /*------------------------------------------------------------------*/

    else if (compare < 0)
       return (idx_search_index (index_root -> right_child, search_name, 
                                 def_offset, def_size));

    /*------------------------------------------------------------------*/
    /** ELSE                                                           **/
    /**    search the left subtree                                     **/
    /*------------------------------------------------------------------*/

    else
       return (idx_search_index (index_root -> left_child, search_name, 
                                 def_offset, def_size));

    /*------------------------------------------------------------------*/
    /** ENDIF                                                          **/
    /*------------------------------------------------------------------*/

/** END **/
}

