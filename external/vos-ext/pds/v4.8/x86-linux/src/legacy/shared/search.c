
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Component                                                           * 
 *    Library searchlib.c                                              *
 * Abstract                                                            *
 *    Routines for searching text and trees                            *
 * Detailed Description                                                *
 *    The searchlib is a library of subroutines used by the            *
 *    PDS software to perform a variety of searches of text lists and  *
 *    trees.                                                           *
 * Internal References                                                 *
 *    search_string_array                                              *
 *    search_string_list_fwd                                           *
 *    search_string_list_bwd                                           *
 * Authors and Institutions                                            *
 *    Marti D. DeMore / J.P.L.                                         *
 * Version and Date                                                    *
 *    3.2   May 14, 1991                                               *
 * Change History                                                      *
 *    MDD   08-05-90   Original library, including search_string_list_ *
 *                     fwd.                                            *
 *    MDD   08-17-90   Added search_string_list_bwd                    *
 *    MDD   10-02-90   Humanized error messages                        *
 *    KLM   05-14-91   Added search_string_array                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "pdsdef.h"
#include "search.h"
#include "errordef.h"
#include "utildef.h"

/**********************************************************************
 *$Component                                                          *
 *    long search_string_array (string_array, list_size, text)        *
 *$Abstract                                                           *
 *    Binary search for an array of strings.                          *
 *$Keywords                                                           *
 *    SEARCHLIB                                                       *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    string_array:                                                   *
 *        The string_array structure is a general purpose array of    *
 *        string pointers.                                            *
 *    list_size:                                                      *
 *        The list_size variable is an integer containing the number  *
 *        of items in a single-dimensional array of items.            *
 *    text:                                                           *
 *        The text variable is a character string that                *
 *        may contain zero or more characters.                        *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    item_position:                                                  *
 *        The item_position variable is an array index.  It contains  *
 *        the location of a certain item, such as the current item    *
 *        being processed, or an item being searched for.             *
 *$Detailed_Description                                               *
 *    The search_string_array routine performs a binary search on     *
 *    an array of character strings and attempts to locate the string *
 *    passed in as input ``text''.  If the string is found, this      *
 *    routine returns the array index of the string.  If the string   *
 *    is not found, it returns PDS_SEARCH_FAIL.                       *
 *$External_References                                                *
 *    None                                                            *
 *$Limitations                                                        *
 *    This routine is designed to handle an array of character        *
 *    pointers, NOT a two dimensional array of characters.            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore / J.P.L.                                        *
 *$Version_and_Date                                                   *
 *    1.1   March 17, 1992                                            *
 *$Change_History                                                     *
 *    MDD   02-20-91   Original Code                                  *
 *    MDD   03-17-92   The great int -> long conversion               *
 **********************************************************************/

long search_string_array (string_array, list_size, text)

char *string_array [];
long list_size;
char *text;
{
   long lower = 0;
   long upper = list_size;
   long mid;
   int compare;
   LOGICAL text_found = FALSE;
         
   while (!text_found && lower <= upper)
   {
      mid = (upper + lower) / 2;
      compare = strcmp (text, string_array [mid]);
      if (compare == 0)
         text_found = TRUE;
      else if (compare < 0)
         upper = mid - 1;
      else
         lower = mid + 1;
   }
   if (text_found)
      return (mid);
   else
      return ((long) PDS_SEARCH_FAIL);
}



/**********************************************************************
 *$Component                                                          *
 *    LOGICAL search_string_list_bwd (text_ptr, text, row,            *
 *                                    column, find_again)             *
 *$Abstract                                                           *
 *    Search a string list (backward) for a string.                   *
 *$Keywords                                                           *
 *    SEARCHLIB                                                       *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    text_ptr:                                                       *
 *        The text_ptr variable is a pointer to a linked              *
 *        list of lines of ordered text.                              *
 *    text:                                                           *
 *        The text variable is a character string that                *
 *        may contain zero or more characters.                        *
 *    row:                                                            *
 *        The row variable is a number corresponding to               *
 *        a row location in a screen, window, or linked list of text. *
 *    column:                                                         *
 *        The column variable is a number corresponding to a          *
 *        column location in a screen, window, or linked list of text.*
 *    find_again:                                                     *
 *        The find_again variable is a TRUE/FALSE flag indicating     *
 *        whether a text search should be performed as an initial     *
 *        search (find_again = FALSE) or as a ``search again''        *
 *        (find_again = TRUE).                                        *
 *$Outputs                                                            *
 *    row:                                                            *
 *        The row variable is a number corresponding to               *
 *        a row location in a screen, window, or linked list of text. *
 *    column:                                                         *
 *        The column variable is a number corresponding to a          *
 *        column location in a screen, window, or linked list of text.*
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose              *
 *        variable which indicates success or failure,                *
 *        generally as a flag returned by a software component.       *
 *$Detailed_Description                                               *
 *    The search_string_list_bwd  subroutine searches a linked        *
 *    list of strings (in the backward direction) for a text string.  *
 *    It will begin its search at the row and column                  *
 *    location passed to this routine (if finding for the first time) *
 *    or one character in front of the last row and column location   *
 *    returned (if finding again). The text input is ignored when     *
 *    finding again.  The row and column location output              *
 *    variables are reset to the location of the                      *
 *    text string, if it is found, or both are set to STOP_SEARCH_FLAG*
 *    if the string was not found.  Zero is returned if the string    *
 *    was not found, and non-zero if it was.                          *
 *$External_References                                                *
 *    None                                                            *
 *$Error_Handling                                                     *
 *    This routine adds an error message to the global list if it     *
 *    receives a bad argument or runs out of memory. It then stops    *
 *    its search.                                                     *
 *$Limitations                                                        *
 *    In order to use this routine to search from the very end of     *
 *    a block of text, the caller must determine the last row and     *
 *    column location and pass them in.  This routine is not able     *
 *    to automatically start from the end of a list.                  *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore / J.P.L.                                        *
 *$Version_and_Date                                                   *
 *    1.4   March 17, 1992                                            *
 *$Change_History                                                     *
 *    MDD   08-06-90   Original Code                                  *
 *    MDD   08-13-90   Added error calls and changed parameter lists  *
 *                        for the SUN compiler                        *
 *    MDD   05-29-91   Deleted use of pds_out_of_memory               *
 *    MDD   10-21-91   Fixed malloc, free, and sys_exit_system calls  *
 *    MDD   03-17-92   The great int -> long conversion               *
 **********************************************************************/

LOGICAL search_string_list_bwd (text_ptr, text,	row, column, find_again)

STRING_LIST *text_ptr;
char        *text;
long        *row;
long        *column;
LOGICAL     find_again;

{
   long temp_row = 0;
   LOGICAL found = FALSE;
   char *column_ptr;
   static char *savetext = NULL;
   static long saverow;
   static long savecol;

   /*-----------------------------------------------------------------*/
   /** BEGIN                                                         **/
   /**    IF finding again THEN                                      **/
   /**       restore old location                                    **/
   /**       backup to previous row if necessary                     **/
   /**    ELSE                                                       **/
   /**       save new string for subsequent calls                    **/
   /*-----------------------------------------------------------------*/

   if (find_again)
   {
      *row = saverow;
      *column = savecol - 1;
      if (*column == 0) (*row)--;
   }
   else
   {
      Lemme_Go(savetext);
      Malloc_String(savetext, (int) String_Size (text));
      strcpy (savetext, text);
   }
   /*-----------------------------------------------------------------*/
   /**    ENDIF finding again                                        **/
   /**    IF row or column is out of bounds THEN                     **/
   /**       set text pointer to null to stop search                 **/
   /*-----------------------------------------------------------------*/

   if (*row <= 0 || *column < 0)
   {
      err_append_message (ERROR1, 
                          "Internal -- Invalid argument sent to search routine");
      text_ptr = NULL;
   }
   /*-----------------------------------------------------------------*/
   /**    ENDIF                                                      **/
   /**    locate current row and column pointers at starting row     **/
   /**        and column                                             **/
   /*-----------------------------------------------------------------*/

   for (temp_row = 1; text_ptr && temp_row != *row; temp_row++)
   {
      text_ptr = text_ptr -> next;
   }
   if (text_ptr)                                        
   {
      if (*column == 0)
      {
	 *column = (long) strlen (text_ptr -> text);
      }
      else if (*column >= (long) strlen (text_ptr -> text))
      {
	 text_ptr = text_ptr -> next;
	 *column = 1;
	 temp_row++;
      }
   }
   if (text_ptr) column_ptr = &(text_ptr -> text [*column - 1]);

   /*-----------------------------------------------------------------*/
   /** WHILE there are more rows of text and string is not found DO  **/
   /*-----------------------------------------------------------------*/

   while (text_ptr && !found)
   {
      /*--------------------------------------------------------------*/
      /** WHILE string is not found DO                               **/
      /**   compare text at column pointer to string                 **/
      /**   subtract from column pointer                             **/
      /*--------------------------------------------------------------*/

      while (!found && column_ptr >= text_ptr -> text)
      {
	 if (*column_ptr == savetext [0])
	    found = !(strncmp (column_ptr, savetext, (strlen (savetext))));
	 if (!found) --column_ptr;
      }
      /*--------------------------------------------------------------*/
      /** ENDWHILE                                                   **/
      /** IF string was not found THEN                               **/
      /**    advance pointers to end of previous row                 **/
      /*--------------------------------------------------------------*/

      if (!found)
      {
	 text_ptr = text_ptr -> prev;
	 temp_row--;
	 if (text_ptr) column_ptr = &(text_ptr -> text
					   [strlen (text_ptr -> text) - 1]);
      }
      /*--------------------------------------------------------------*/
      /** ENDIF string was not found                                 **/
      /*--------------------------------------------------------------*/
   }
   /*-----------------------------------------------------------------*/
   /** ENDWHILE there are more rows...                               **/
   /** set row and column fields                                     **/
   /*-----------------------------------------------------------------*/

   if (found)
   {
      *row = temp_row;
      *column = (long) (strlen (text_ptr -> text) - strlen (column_ptr)) + 1;
   }
   else
   {
      *row = STOP_SEARCH_FLAG;
      *column = STOP_SEARCH_FLAG;
   }
   /*-----------------------------------------------------------------*/
   /** save row and column fields                                    **/
   /** RETURN success or failure                                     **/
   /*-----------------------------------------------------------------*/

    saverow = *row;
    savecol = *column;
    return (found);

   /** END search_string_list_bwd                                    **/
}


/**********************************************************************
 *$Component                                                          *
 *    LOGICAL search_string_list_fwd (text_ptr, text, row,            *
 *                                    column, find_again)             *
 *$Abstract                                                           *
 *    Search a string list (forward) for a string.                    *
 *$Keywords                                                           *
 *    SEARCHLIB                                                       *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    text_ptr:                                                       *
 *        The text_ptr variable is a pointer to a linked              *
 *        list of lines of ordered text.                              *
 *    text:                                                           *
 *        The text variable is a character string that                *
 *        may contain zero or more characters.                        *
 *    row:                                                            *
 *        The row variable is a number corresponding to               *
 *        a row location in a screen, window, or linked list of text. *
 *    column:                                                         *
 *        The column variable is a number corresponding to a          *
 *        column location in a screen, window, or linked list of text.*
 *    find_again:                                                     *
 *        The find_again variable is a TRUE/FALSE flag indicating     *
 *        whether a text search should be performed as an initial     *
 *        search (find_again = FALSE) or as a ``search again''        *
 *        (find_again = TRUE).                                        *
 *$Outputs                                                            *
 *    row:                                                            *
 *        The row variable is a number corresponding to               *
 *        a row location in a screen, window, or linked list of text. *
 *    column:                                                         *
 *        The column variable is a number corresponding to a          *
 *        column location in a screen, window, or linked list of text.*
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose              *
 *        variable which indicate success or failure,                 *
 *        generally as a flag returned by a software component.       *
 *$Detailed_Description                                               *
 *    The search_string_list_fwd  subroutine searches a linked        *
 *    list of strings (in the forward direction) for a text string.   *
 *    It will begin its search at the row and column                  *
 *    location passed to this routine (if finding for the first time) *
 *    or one character beyond the last row and column location        *
 *    returned (if finding again). The text input is ignored when     *
 *    finding again.  The row and column location output              *
 *    variables are reset to the location of the                      *
 *    text string, if it is found, or both are set to STOP_SEARCH_FLAG*
 *    if the string was not found.  Zero is returned if the string    *
 *    was not found, and non-zero if it was.                          *
 *$External_References                                                *
 *    None                                                            *
 *$Error_Handling                                                     *
 *    This routine adds an error message to the global list if it     *
 *    receives a bad argument or runs out of memory. It then stops    *
 *    its search.                                                     *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore / J.P.L.                                        *
 *$Version_and_Date                                                   *
 *    1.4   October 21, 1991                                          *
 *$Change_History                                                     *
 *    MDD   07-27-90   Original Code                                  *
 *    MDD   08-06-90   Fixed infinite loop problem and added bounds   *
 *                     check on row and column inputs                 *
 *    MDD   08-13-90   Added error calls and changed parameter lists  *
 *                        for the SUN compiler                        *
 *    MDD   05-29-91   Deleted use of pds_out_of_memory               *
 *    MDD   10-21-91   Fixed malloc, free, and sys_exit_system calls  *
 **********************************************************************/
                                                           
LOGICAL search_string_list_fwd (text_ptr, text, row, column, find_again)

STRING_LIST *text_ptr;
char        *text;
long        *row;
long        *column;
LOGICAL     find_again;

{
   long temp_row = 0;
   LOGICAL found = FALSE;
   char *column_ptr;
   static char *savetext = NULL;
   static long saverow;
   static long savecol;

   /*-----------------------------------------------------------------*/
   /** BEGIN                                                         **/
   /**    IF finding again THEN                                      **/
   /**       restore old location                                    **/
   /**    ELSE                                                       **/
   /**       save new string for subsequent calls                    **/
   /*-----------------------------------------------------------------*/

   if (find_again)
   {
      *row = saverow;
      *column = savecol + 1;
   }
   else
   {
      Lemme_Go(savetext);
      Malloc_String(savetext, (int) String_Size (text));
      strcpy (savetext, text); 
   }
   /*-----------------------------------------------------------------*/
   /**    ENDIF finding again                                        **/
   /**    IF row or column is out of bounds THEN                     **/
   /**       set text pointer to null to stop search                 **/
   /*-----------------------------------------------------------------*/

   if (*row <= 0 || *column <= 0)
   {
      err_append_message (ERROR1, 
                          "Internal -- Invalid argument sent to search routine");
      text_ptr = NULL;
   }
   /*-----------------------------------------------------------------*/
   /**    ENDIF                                                      **/
   /**    locate current row and column pointers at starting row     **/
   /**        and column                                             **/
   /*-----------------------------------------------------------------*/

   for (temp_row = 1; text_ptr && temp_row != *row; temp_row++)
   {
      text_ptr = text_ptr -> next;
   }
   if (text_ptr)
   {
      if (*column > (long) strlen (text_ptr -> text))
      {
	 text_ptr = text_ptr -> next;
	 *column = 1;
	 temp_row++;
      }
   }
   if (text_ptr) column_ptr = &(text_ptr -> text [*column - 1]);

   /*-----------------------------------------------------------------*/
   /** WHILE there are more rows of text and string is not found DO  **/
   /*-----------------------------------------------------------------*/

   while (text_ptr && !found)
   {
      /*--------------------------------------------------------------*/
      /** CALL util_locate_substring on current row                  **/
      /*--------------------------------------------------------------*/

      column_ptr = util_locate_substring (column_ptr, savetext);
      found = !(column_ptr == NULL);

      /*--------------------------------------------------------------*/
      /** IF string was not found THEN                               **/
      /**    advance pointers to next row                            **/
      /*--------------------------------------------------------------*/

      if (!found)
      {
	 text_ptr = text_ptr -> next;
	 temp_row++;
	 if (text_ptr) column_ptr = text_ptr -> text;
      }
      /*--------------------------------------------------------------*/
      /** ENDIF string was not found                                 **/
      /*--------------------------------------------------------------*/
   }
   /*-----------------------------------------------------------------*/
   /** ENDWHILE there are more rows...                               **/
   /** set row and column fields                                     **/
   /*-----------------------------------------------------------------*/

   if (found)
   {
      *row = temp_row;
      *column = (long) (strlen (text_ptr -> text) - strlen (column_ptr)) + 1;
   }
   else
   {
      *row = STOP_SEARCH_FLAG;
      *column = STOP_SEARCH_FLAG;
   }
   /*-----------------------------------------------------------------*/
   /** save row and column fields                                    **/
   /** RETURN success or failure                                     **/
   /*-----------------------------------------------------------------*/

    saverow = *row;
    savecol = *column;
    return (found);

   /** END search_string_list_fwd                                    **/
}




