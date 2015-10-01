#include "tb_utils.h"


/**********************************************************************
 *$Component                                                          *
 *    char *util_create_file_spec (directory, file_name)              *
 *$Abstract                                                           *
 *    Create a file spec from a directory and file name               *
 *$Keywords                                                           *
 *    UTILLIB                                                         *
 *    DIRECTORY                                                       *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    directory:                                                      *
 *       The directory variable is a string that contains a           *
 *       directory name.                                              *
 *    file_name:                                                      *
 *       The file_name variable is a string that contains a file      *
 *       name.                                                        *
 *$Outputs                                                            *
 *    NONE                                                            *
 *$Returns                                                            *
 *    file_spec:                                                      *
 *       The file_spec variable is a string that contains a file      *
 *       specification, i.e., a directory name and a file name.       *
 *$Detailed_Description                                               *
 *    The util_create_file_spec routine creates a complete file       *
 *    specification from a directory name and a file name.  If the    *
 *    directory name is empty or NULL, then only the file name is     *
 *    returned. If the file name is NULL or empty then only the       *
 *    directory name is returned. If both are empty or NULL, then     *
 *    NULL is returned.                                               *
 *$Side_Effects                                                       *
 *    This routine allocates memory for its return value which must   *
 *    be deallocated elsewhere.                                       *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Herbert C. Gamble / J.P.L.                                      *
 *$Version_and_Date                                                   *
 *    2.1  October 17, 1991                                           *
 *$Change_History                                                     *
 *    HCG   10-19-90   Original Code                                  *
 *    MDD   03-14-91   Changed name from util_catenate_directory and  *
 *                     added ability to handle VMS and DOS files      *
 *    MDD   10-17-91   Fixed malloc, free, and sys_exit_system calls  *
 **********************************************************************/

char *util_create_file_spec(char *directory, char *file_name)

{
  char *temp_str = NULL;
  /*-------------------------------------------------------------------------*/
  /** BEGIN                                                                 **/
  /**   IF there is no directory but there is a filename THEN               **/
  /*-------------------------------------------------------------------------*/

  if ((directory == NULL || util_string_is_empty (directory)) 
         && file_name != NULL && !util_string_is_empty(file_name))
  { 
     Malloc_String(temp_str, String_Size (file_name));
     strcpy (temp_str, file_name);
  }
  /*-------------------------------------------------------------------------*/
  /**   ELSE IF the file name is not empty THEN                             **/
  /**     copy the directory to temp_str.                                   **/
  /**     append the file name to the directory name                        **/
  /*-------------------------------------------------------------------------*/

  else if (file_name != NULL && !util_string_is_empty(file_name))
  {
     Malloc_String(temp_str, String_Size(file_name) + String_Size(directory));
	 strcpy (temp_str, directory);
#ifdef VAX
     if (*(String_End(temp_str)) != ':' &&
            *(String_End(temp_str)) != ']')
        strcat (temp_str, ":");
     strcat (temp_str, file_name);
#endif

#ifdef SUN_UNIX 
     if (*(String_End(temp_str)) != '/')
       strcat (temp_str, "/");
     strcat (temp_str, file_name);
#endif

#ifdef MSDOS_TC

     if (*(String_End(temp_str)) != '\\')
	 strcat (temp_str, "\\");
     strcat (temp_str, file_name);
#endif

#ifdef MAC_THINK
     if (*(String_End(temp_str)) != ':')
        strcat (temp_str, ":");
     strcat (temp_str, file_name);
#endif
  }

  /*-------------------------------------------------------------------------*/
  /**   ELSE IF the directory name exists THEN                              **/
  /**     copy the directory name only to temp_str                          **/
  /*-------------------------------------------------------------------------*/

  else if (directory != NULL && !util_string_is_empty(directory))
  {
   Malloc_String(temp_str, String_Size(directory));
     strcpy (temp_str, directory);
  }
  /*-------------------------------------------------------------------------*/
  /**   ENDIF the directory name exists...                                  **/
  /**   RETURN temp_str.                                                    **/
  /*-------------------------------------------------------------------------*/
  return (temp_str);

} /** END 

/**********************************************************************
 *$Component                                                          *
 *    char *util_strip_lead_and_trail (string, remove_char)           *
 *$Abstract                                                           *
 *    Strip off the leading and trailing characters from a string.    *
 *$Keywords                                                           *
 *    UTILLIB                                                         *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    string:                                                         *
 *        The string variable is a general purpose character string   *
 *        that may contain one or more characters.                    *
 *    remove_char:                                                    *
 *        The remove_char variable contains a character to be removed *
 *        from a string.                                              *
 *$Outputs                                                            *
 *    string:                                                         *
 *        The string variable is a general purpose character string   *
 *        that may contain one or more characters.                    *
 *$Returns                                                            *
 *    string:                                                         *
 *        The string variable is a general purpose character string   *
 *        that may contain one or more characters.                    *
 *$Detailed_Description                                               *
 *    The util_strip_lead_and_trail subroutine strips all of the      *
 *    occurences of a particular character from the front and rear    *
 *    of a string.                                                    *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath /J.P.L.                                        *
 *$Version_and_Date                                                   *
 *    1.2   April 29, 1991                                            *
 *$Change_history                                                     *
 *    DPB   07-05-90   Moved to utillib and updated.  This routine    *
 *                     used to be called misc_strip_lead_and_trail.   *
 *    DPB   08-22-90   The great de-shalling.                         *
 *    DPB   04-29-91   Added code to handle NULL strings.             *
 **********************************************************************/

char *util_strip_lead_and_trail (char *string, char remove_char)

{
    char *c;
    if (string != NULL)
    {

        for (c = string; ((*c != EOS) && (*c == remove_char)); ++c) ;
		if(c != string)	strcpy (string, c);

/*        strcpy (string, c); */
        for (c=String_End(string); ((c!=string) && (*c == remove_char)); --c) ;
    
        if(String_End(string) != c) *(++c) = EOS;

    }  /*  End:  "if (string != NULL) ..."  */
    return (string);


}  /*  End: "util_strip_lead_and_trail"  */

/**********************************************************************
 *$Component                                                          *
 *    char *sys_get_path (fname)                                      *
 *$Abstract                                                           *
 *    Extracts and returns the directory path from a file spec.       *
 *$Keywords                                                           *
 *    PATH                                                            *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    fname:                                                          *
 *        The fname variable is a general purpose character           *
 *        string containing a file specification.                     *
 *$Outputs                                                            *
 *    NONE.                                                           *
 *$Returns                                                            *
 *    directory:                                                      *
 *        The directory variable is a string that contains a          *
 *        directory name.                                             *
 *$Detailed_Description                                               *
 *    The sys_get_path routine extracts a directory path from the     *
 *    file name passed in.  If no path is found, this routine         *
 *    returns NULL.                                                   *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore / J.P.L.                                        *
 *$Version_and_Date                                                   *
 *    1.2   April 16, 1992                                            *
 *$Change_History                                                     *
 *    DPB   07-09-91   Original code.                                 *
 *    MDD   10-17-91   Fixed malloc, free, and sys_exit_system calls  *
 *    MDD   01-13-92   Bug fix -was returning empty string rather than*
 *                     NULL when no path was found.                   *
 *    MDD   04-16-92   Added macintosh file handling                  *
 **********************************************************************/

char *sys_get_path (char *fname)

{
    char *c = {NULL};
    char *path_name = {NULL};
    char save_char;

    if (fname != NULL)
    {
        /*  This loop starts at the end of the file spec and
         *  works backwards until it either finds a directory
         *  path, or runs out of string to check.
         */

        for (c = String_End(fname); 
                ((c >= fname) && (*c != ']') && 
                 (*c != ':') && (*c != '/') && (*c != '\\')); --c) ;

        ++c;
        save_char = *c;
        *c = EOS;
        Malloc_String(path_name, String_Size(fname));
        strcpy (path_name, fname);
        *c = save_char;
        if (strcmp (path_name, "") == 0) Lemme_Go(path_name);
          
    }  /*  End:  "if (fname != NULL) ..."  */

    return (path_name);

}  /* "sys_get_path"  */


/**********************************************************************
 *$Component                                                          *
 *   LOGICAL lt_get_pointer (label_ptr, pointer_name,                 *
 *                           pointer_position, pointer_info)          *
 *$Abstract                                                           *
 *    Gets a pointer value from a PDS label.                          *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    POINTER                                                         *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    label_ptr:                                                      *
 *        The label_ptr variable is a pointer to the root of an       *
 *        ODL tree.  Usually it points to the actual "ROOT" object,   *
 *        but it may point to any other object in the tree.  If it    *
 *        does point to something other than the "ROOT" object, then  *
 *        this object will be treated as the root of a sub-tree, and  *
 *        processing will not be allowed to move above the level of   *
 *        this object.                                                *
 *    pointer_name:                                                   *
 *        The pointer_name variable is a character string             *
 *        which contains the name of a pointer in a PDS label         *
 *        (e.g., the line "^TABLE = "TABLE.DAT" implies that "^TABLE" *
 *        is the pointer_name.  The caret must NOT be included.       *
 *    pointer_position:                                               *
 *        The pointer_position variable is an integer which           *
 *        represents the relative position of a pointer in an         *
 *        object in a PDS label.  If this variable is used in         *
 *        conjunction with the pointer_name variable, then it         *
 *        represents a particular occurrence of that pointer          *
 *        in the object (e.g., if pointer_name is "^TABLE" and        *
 *        pointer_position   is 2, then this represents the second    *
 *        "^TABLE" pointer in the object).  On the other hand, if     *
 *        this variable is used by itself, it represents the absolute *
 *        position of the parameter within the object, starting with  *
 *        first parameter in the object (position = 1), counting all  *
 *        parameters, not just pointers.                              *
 *$Outputs                                                            *
 *    pointer_info:                                                   *
 *        The pointer_info variable is the address of a POINTER_INFO  *
 *        structure, which contains information about a label pointer,*
 *        indicating the file it points to, the location of an object *
 *        within the file, the type of the location (record or bytes) *
 *        and whether the data file is attached or detached from the  *
 *        label.                                                      *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure        *
 *        (a value of FALSE).                                         *
 *$Detailed_Description                                               *
 *    The lt_get_pointer routine searches the ROOT object in the      *
 *    given label tree for the specified pointer (according to        *
 *    the search rules used by the PDS label library).  If the ROOT   *
 *    object contains the pointer, then the pointer_info output       *
 *    structure is assigned the appropriate location, file_name, and  *
 *    flag values.  If the pointer is found, this routine returns     *
 *    TRUE. If the pointer is not found, then this routine returns    *
 *    FALSE, and the pointer_info structure is set to the default:    *
 *    empty string as file_name, location = 1, has_byte_loc = FALSE,  *
 *    and is_attached = TRUE.                                         *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/JPL                                             *
 *$Version_and_Date                                                   *
 *    1.0   October 10, 1991                                          *
 *$Change_History                                                     *
 *    MDD   10-10-91   Original Code.                                 *
 **********************************************************************/


LOGICAL lt_get_pointer (OBJDESC *lbl_ptr, char *pointer_name,
					int pointer_position, POINTER_INFO *pointer_info)

{
    char *temp = NULL;
	int status = PDS_ERROR;
	LOGICAL success = FALSE;
	KEYWORD *pointer_kwd = NULL;
	char pointer_name_2[50];
	TB_STRING_LIST *kwd_value_list = NULL;
	char dbl_qts[2];


	char *temp_string = NULL;
	unsigned long  temp_u_long = 0;
	unsigned short temp_short = 0;
	int linecount = 0;



/** BEGIN **/
	dbl_qts[0] = '\"';
	dbl_qts[1] = '\0';
	strcpy(pointer_name_2, "^");
	strcat(pointer_name_2, pointer_name);


   /*------------------------------------------------------------------*/
   /** Initialize the pointer structure to default values             **/
   /*------------------------------------------------------------------*/

   pointer_info -> has_byte_loc = FALSE;
   pointer_info -> is_attached = TRUE;
   pointer_info -> location = 1;
   strcpy (pointer_info -> file_name, "");
   strcpy (pointer_info -> name, pointer_name);

   /*------------------------------------------------------------------*/
   /** Look for the pointer in the ROOT object                        **/
   /** IF it was found THEN                                           **/
   /*------------------------------------------------------------------*/
	pointer_kwd = OdlFindKwd (lbl_ptr, pointer_name_2, 
                     "*", pointer_position, 
                     ODL_TO_END);


	if (pointer_kwd != NULL)
	{
      /*---------------------------------------------------------------*/
      /** IF the pointer has at least one value THEN                  **/
	  /**	#define ODL_RECORD_LOCATION 0							  **/	
	  /**   #define ODL_BYTE_LOCATION   1							  **/
      /*---------------------------------------------------------------*/


		temp_string = OdlGetFileName (pointer_kwd, &temp_u_long, &temp_short); 
		strcpy(pointer_info->file_name, temp_string);
		pointer_info->location = temp_u_long;
		pointer_info->has_byte_loc = (LOGICAL)temp_short;
		success = TRUE;

		/* Check the filename. We must first get the line number in case of 
		   any errors to report.
	    */
		GetLineNum(lbl_ptr, pointer_name_2, linecount);
		checkfilename(temp_string, linecount);
	}
   /*------------------------------------------------------------------*/
   /** ENDIF it was found...                                          **/
   /*------------------------------------------------------------------*/

   return (success);

/** END **/
}

/**********************************************************************  
 *$Component                                                          * 
 *    char *util_lower_case (string)                                  *
 *$Abstract                                                           * 
 *    Convert a string to lowercase (in situ).                        *
 *$Keywords                                                           * 
 *    UTILLIB                                                         *
 *    CONVERT                                                         * 
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             * 
 *    string:                                                         *
 *        The string variable is a general purpose character string   *
 *        that may contain one or more characters.                    *
 *$Outputs                                                            * 
 *    string:                                                         *
 *        The string variable is a general purpose character string   *
 *        that may contain one or more characters.                    *
 *$Returns                                                            * 
 *    string:                                                         *
 *        The string variable is a general purpose character string   *
 *        that may contain one or more characters.                    *
 *$Detailed_Description                                               * 
 *    The util_lower_case subroutine converts a string to lower case  *
 *    in its original position.                                       *
 *$External_References                                                * 
 *    NONE                                                            * 
 *$Author_and_Institution                                             * 
 *    David P. Bernath/J.P.L.                                         *
 *$Version_and_Date                                                   * 
 *    1.2   April 29, 1991                                            *
 *$Change_History                                                     * 
 *    DPB   07-05-90   Moved to utillib and updated.  This routine    *
 *                     used to be part of the code listing parser.    *
 *    DPB   08-22-90   The great de-shalling.                         *
 *    DPB   04-29-91   Added code to handle NULL strings.             *
 **********************************************************************/ 
 
char *util_lower_case (char *string)

{
    char *c;

    for (c = string; ((c != NULL) && (*c != EOS)); ++c)
    {
        if (isupper (*c))
            *c = tolower (*c);
    }

    return (string);

}  /*  End:  "util_lower_case"  */

/**********************************************************************
 *$Component                                                          *
 *   LOGICAL util_string_is_empty (string)                            *
 *$Abstract                                                           *
 *    Checks a string to see if it is empty.                          *
 *$Keywords                                                           *
 *    UTILLIB                                                         *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    string:                                                         *
 *        The string variable is a general purpose character string   *
 *        that may contain one or more characters.                    *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure        *
 *        (a value of FALSE).                                         *
 *$Detailed_Description                                               *
 *    The util_string_is_empty subroutine returns TRUE if a string    *
 *    contains either blanks or the NUL character, or FALSE if the    *
 *    string contains anything else.                                  *
 *$External_References                                                *
 *    None.                                                           *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.2   April 29, 1991                                            *
 *$Change_History                                                     *
 *    DPB   07-05-90   Moved to utillib and updated.  This routine    *
 *                     used to be called misc_line_is_empty.          *
 *    DPB   08-22-90   The great de-shalling.                         *
 *    DPB   04-29-91   Added code to handle NULL strings.             *
 **********************************************************************/

LOGICAL util_string_is_empty(char *string)

{
    char *c;
    LOGICAL empty = {TRUE};

    if (string != NULL)
    {
        for (c = string; ((*c != EOS) && (empty)); ++c)
            empty = (*c == ' ');

    }  /*  End:  "if (string != NULL) ..."  */

    return (empty);

}  /*  End: "util_string_is_empty"  */

/**********************************************************************
 *$Component                                                          *
 *   char *util_remove_char (string, remove_char)                     *
 *$Abstract                                                           *
 *   Remove all occurrences of a character from a string              *
 *$Keywords                                                           *
 *    UTILLIB                                                         *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    remove_char:                                                    *
 *        The remove_char variable contains a character to be removed *
 *        from a string.                                              *
 *    string:                                                         *
 *        The string variable is a general purpose character string   *
 *        that may contain one or more characters.                    *
 *$Outputs                                                            *
 *    string:                                                         *
 *        The string variable is a general purpose character string   *
 *        that may contain one or more characters.                    *
 *$Returns                                                            *
 *    string:                                                         *
 *        The string variable is a general purpose character string   *
 *        that may contain one or more characters.                    *
 *$Detailed_Description                                               *
 *    The util_remove_char subroutine removes all occurrences of      *
 *    a character from a string.                                      *
 *$External_References                                                *
 *    None                                                            *
 *$Limitations                                                        *
 *    None.                                                           *
 *$Error_Handling                                                     *
 *    None.                                                           *
 *$Side_Effects                                                       *
 *    None.                                                           *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.2   April 29, 1991                                            *
 *$Change_History                                                     *
 *    DPB   07-05-90   Moved to utillib and updated.  This routine    *
 *                     used to be called misc_strrem.                 *
 *    DPB   08-22-90   The great de-shalling.                         *
 *    DPB   04-29-91   Added code to handle NULL strings.             *
 **********************************************************************/

char *util_remove_char (char *string, char remove_char)

{
    char *source_char;
    char *dest_char;

    dest_char = string;

    if (string != NULL)
    {
        for (source_char = string; *source_char != EOS; ++source_char)
        {
            if (*source_char != remove_char)
            {
                *dest_char = *source_char;
                ++dest_char;
            }
                
        }  /*  End:  "for (source_char = string; ..."  */
    
        *dest_char = EOS;

    }  /*  End:  "if (string != NULL) ..."  */

    return (string);

}  /*  End:  "util_remove_char"  */

/**********************************************************************
 *$Component                                                          *
 *    char *util_compress_char (string, compress_char)                *
 *$Abstract                                                           *
 *    Compresses multiple occurances of char to a single char.        *
 *$Keywords                                                           *
 *    UTILLIB                                                         *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    string:                                                         *
 *        The string variable is a general purpose character string   *
 *        that may contain one or more characters.                    *
 *    compress_char:                                                  *
 *        The compress_char variable contains the character in the    *
 *        string to be compressed.                                    *
 *$Outputs                                                            *
 *    string:                                                         *
 *        The string variable is a general purpose character string   *
 *        that may contain one or more characters.                    *
 *$Returns                                                            *
 *    string:                                                         *
 *        The string variable is a general purpose character string   *
 *        that may contain one or more characters.                    *
 *$Detailed_Description                                               *
 *    The util_compress_char subroutine compresses multiple           *
 *    occurances of the input character into a single occurence       *
 *    of that character in a string.                                  *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Kristy L. Marski /J.P.L.                                        *
 *$Version_and_Date                                                   *
 *    1.2   April 29, 1991                                            *
 *$Change_history                                                     *
 *    KLM   07-30-90   Modified from util_compress_blanks.            *
 *    DPB   08-22-90   The great de-shalling.                         *
 *    DPB   04-29-91   Added code to handle NULL strings.             *
 **********************************************************************/

char *util_compress_char(char *string, char compress_char)         
{
    char *source_char;
    char *dest_char;         
    LOGICAL compress = {FALSE};

/** BEGIN **/

/*--------------------------------------------------------------------------*/
/**                                                                        **/
/** IF the string is not NUL THEN                                          **/
/*--------------------------------------------------------------------------*/

    if ((string != NULL) && (*string != EOS))
    {
/*--------------------------------------------------------------------------*/
/**     LOOP through the string ...                                        **/
/*--------------------------------------------------------------------------*/

        dest_char = string;

        for (source_char = string; *source_char != EOS; ++source_char)
        {
/*--------------------------------------------------------------------------*/
/**         IF the current character is not a the compression char THEN    **/
/**             Turn off the 'compress' flag and copy the character.       **/
/**         ELSE                                                           **/
/*--------------------------------------------------------------------------*/

            if (*source_char != compress_char)
            {   
                compress = FALSE;
                *dest_char = *source_char;
                ++dest_char;
            }
            else
/*--------------------------------------------------------------------------*/
/**             IF this is the first in a group of compression chars THEN  **/
/**                 Turn on the 'compress' flag, and copy a single         **/
/**                 occurance of the compression character.                **/
/*--------------------------------------------------------------------------*/

                if (compress == FALSE)
                {
                    compress = TRUE;
                    *dest_char = compress_char;
                    ++dest_char;
                }
    
/*--------------------------------------------------------------------------*/
/**             ENDIF                                                      **/
/**         ENDIF                                                          **/
/**     ENDLOOP                                                            **/
/*--------------------------------------------------------------------------*/

        }  /*  End:  "for (source_char = string; ..."  */
    
/*--------------------------------------------------------------------------*/
/**     Terminate the copied string.                                       **/
/*--------------------------------------------------------------------------*/

        *dest_char = EOS;

/*--------------------------------------------------------------------------*/
/** ENDIF                                                                  **/
/*--------------------------------------------------------------------------*/

    }  /*  End:  "if ((string != NULL) && ..."  */

/*--------------------------------------------------------------------------*/
/** RETURN a pointer to the string.                                        **/
/*--------------------------------------------------------------------------*/

    return (string);

/*--------------------------------------------------------------------------*/
/** END **/


}  /*  End: "util_compress_char"  */

/**********************************************************************
 *$Component                                                          *
 *   char *util_upper_case (string)                                   *
 *$Abstract                                                           *
 *    Translate a string to upper case (in situ).                     *
 *$Keywords                                                           *
 *    UTILLIB                                                         *
 *    CONVERT                                                         *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    string:                                                         *
 *        The string variable is a general purpose character string   *
 *        that may contain one or more characters.                    *
 *$Outputs                                                            *
 *    string:                                                         *
 *        The string variable is a general purpose character string   *
 *        that may contain one or more characters.                    *
 *$Returns                                                            *
 *    string:                                                         *
 *        The string variable is a general purpose character string   *
 *        that may contain one or more characters.                    *
 *$Detailed_Description                                               *
 *    The util_upper_case subroutine converts a string to upper       *
 *    case in its original position.                                  *
 *$External_References                                                *
 *    None.                                                           *
 *$Author_and_Institution                                             *
 *    Ann M. Farny / J.P.L.                                           *
 *$Version_and_Date                                                   *
 *    1.2   April 29, 1991                                            *
 *$Change_History                                                     *
 *    DPB   07-05-90   Moved to utillib and updated.  This routine    *
 *                     used to be called misc_strup.                  *
 *    DPB   08-22-90   The great de-shalling.                         *
 *    DPB   04-29-91   Added code to handle NULL strings.             *
 **********************************************************************/

char *util_upper_case (char *string)	

{
    char *c;

    if (string != NULL)
    {
        for (c = string; *c != EOS; ++c)
        {
            if (islower (*c))
                *c = toupper (*c);
        }

    }  /*  End:  "if (string != NULL) ..."  */

    return (string);

}  /*  End:  "util_upper_case"  */


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

long search_string_array (char *string_array[], long list_size, char *text)
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