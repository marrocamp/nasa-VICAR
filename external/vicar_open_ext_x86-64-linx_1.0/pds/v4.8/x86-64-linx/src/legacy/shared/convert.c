/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Component                                                           * 
 *    Library convertlib.c                                             *
 * Abstract                                                            *
 *    Low-level PDS conversion routines                                *
 * Detailed Description                                                *
 *    The convertlib is a library of subroutines used by PDS software  *
 *    to perform a variety of low-level string conversion tasks.       *
 * Internal References                                                 *
 *    String conversion routines:                                      *
 *        cvt_dos_to_unix_file_spec                                    *
 *        cvt_dos_to_vax_file_spec                                     *
 *        cvt_unix_to_dos_file_spec                                    *
 *        cvt_unix_to_vax_file_spec                                    *
 *        cvt_vax_to_dos_file_spec                                     *
 *        cvt_vax_to_unix_file_spec                                    *
 *                                                                     *
 * Authors and Institutions                                            *
 *    Kristy L. Marski / J.P.L.                                        *
 * Version and Date                                                    *
 *    1.2   September 20, 1990                                         *
 * Change History                                                      *
 *    KLM   07-25-90   Original Generation of:                         *
 *                            cvt_dos_to_unix_file_spec                *
 *                            cvt_unix_to_dos_file_spec                *
 *                            cvt_vax_to_dos_file_spec                 *
 *    KLM   08-28-90   Original Generation of:                         *
 *                            cvt_dos_to_vax_file_spec                 *
 *    KLM   09-20-90   Original Generation of:                         *
 *                            cvt_unix_to_vax_file_spec                *
 *                            cvt_vax_to_unix_file_spec                *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "pdsdef.h"
#include "convert.h"
#include "utildef.h"


/**********************************************************************
 *$Component                                                          *
 *    char *cvt_dos_to_unix_file_spec (file_spec)                     *
 *$Abstract                                                           *
 *    Convert a DOS file spec to a UNIX file spec.                    *
 *$Keywords                                                           *
 *    CONVERTLIB                                                      *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    file_spec:                                                      *
 *        The file_spec variable contains the full file               *
 *        specification including the path (or directory), filename,  *
 *        extension, and version number if there is one.              *
 *$Outputs                                                            *
 *    file_spec:                                                      *
 *        The file_spec variable contains the full file               *
 *        specification including the path (or directory), filename,  *
 *        extension, and version number if there is one.              *
 *$Returns                                                            *
 *    file_spec:                                                      *
 *        The file_spec variable contains the full file               *
 *        specification including the path (or directory), filename,  *
 *        extension, and version number if there is one.              *
 *$Detailed_Description                                               *
 *    The cvt_dos_to_unix_file_spec subroutine converts a full        *
 *    DOS file specification to a UNIX file specification. Any        *
 *    disk references are stripped off, and do not appear in the      *
 *    resulting string.                                               *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Kristy L. Marski /JPL                                           *
 *$Version_and_Date                                                   *
 *    1.0   July 25, 1990                                             *
 *$Change_history                                                     *
 *    KLM   07-25-90   Original generation.                           *
 **********************************************************************/

char *cvt_dos_to_unix_file_spec (file_spec)

char *file_spec;

{

    char *temp_file_spec;         
 
    temp_file_spec = file_spec;          

    if (*file_spec == NULL)
    {
      return (file_spec);
    }
/*--------------------------------------------------------------------------*/
/**  BEGIN                                                                 **/
/**                                                                        **/
/**   Strip all characters up to the ':' -> CALL UTIL_STRIP_TO_CHAR        **/
/**   IF the current character is a ':' THEN                               **/
/**       move the pointer one character to skip the ':'                   **/
/**   ENDIF                                                                **/
/**                                                                        **/
/**   Replace '\' with '/'  -> CALL UTIL_REPLACE_CHAR                      **/
/**                                                                        **/
/**   Copy the temp_file_spec to the original file_spec                    **/
/**                                                                        **/
/**   RETURN file_spec                                                     **/
/**                                                                        **/
/**  END                                                                   **/
/*--------------------------------------------------------------------------*/

                  
    util_strip_to_char (temp_file_spec,':');

    if (*temp_file_spec == ':')
    {
      temp_file_spec++;
    }

    util_replace_char (temp_file_spec,'\\','/');
    
    strcpy (file_spec,temp_file_spec);

    return (file_spec);

}  /*  End: "cvt_dos_to_unix_file_spec"  */


/**********************************************************************
 *$Component                                                          *
 *    char *cvt_dos_to_vax_file_spec (file_spec)                      *
 *$Abstract                                                           *
 *    Convert a DOS file spec to a VAX file spec.                     *
 *$Keywords                                                           *
 *    CONVERTLIB                                                      *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    file_spec:                                                      *
 *        The file_spec variable contains the full file               *
 *        specification including the path (or directory), filename,  *
 *        extension, and version number if there is one.              *
 *$Outputs                                                            *
 *    file_spec:                                                      *
 *        The file_spec variable contains the full file               *
 *        specification including the path (or directory), filename,  *
 *        extension, and version number if there is one.              *
 *$Returns                                                            *
 *    file_spec:                                                      *
 *        The file_spec variable contains the full file               *
 *        specification including the path (or directory), filename,  *
 *        extension, and version number if there is one.              *
 *$Detailed_Description                                               *
 *    The cvt_dos_to_vax_file_spec subroutine converts a full         *
 *    DOS file specification to a VAX file specification. Any         *
 *    disk references are stripped off, and do not appear in the      *
 *    resulting string. NOTE: the variable ``file_spec" must be large *
 *    enough to hold the value of ``file_spec + 2."                   *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Kristy L. Marski /JPL                                           *
 *$Version_and_Date                                                   *
 *    1.1   October 21, 1991                                          *
 *$Change_history                                                     *
 *    KLM   08-01-90   Original generation.                           *
 *    MDD   10-21-91   Fixed malloc, free, and sys_exit_system calls  *
 **********************************************************************/

char *cvt_dos_to_vax_file_spec (file_spec)

char *file_spec;

{
    char *temp_file_spec;
    char *temp_ptr;
    char *temp_str;
    LOGICAL found;
 
/** BEGIN **/

    found = FALSE;                                 
    temp_file_spec = file_spec;                         


    if (*file_spec == NULL)
    {
      return (file_spec);
    }


/*--------------------------------------------------------------------------*/
/**                                                                        **/
/**  Strip all characters up to the ':'                                    **/
/**  IF the current character is a ':' THEN                                **/
/**      move the pointer one character to skip the ':'                    **/
/**  ENDIF                                                                 **/
/**                                                                        **/
/**  Set the temporary pointer to the end of the string.                   **/
/*--------------------------------------------------------------------------*/
                                
    util_strip_to_char (temp_file_spec,':');

    if (*temp_file_spec == ':')
    {
      temp_file_spec++;
    }

    temp_ptr = String_End(temp_file_spec);

/*---------------------------------------------------------------------*/
/**                                                                   **/
/**  WHILE temp_ptr is not at the beginning of the string             **/
/**     IF the current character is a '\' THEN                        **/
/**        IF there was a previous '\' in the string                  **/
/**           change the '\' to a '.'                                 **/
/**        ELSE                                                       **/
/**           change the '\' to a ']'                                 **/
/**           SET the '\' has been found flag to TRUE                 **/
/**        ENDIF                                                      **/
/**     ENDIF                                                         **/
/**     Decrement temp_ptr                                            **/
/**  ENDWHILE                                                         **/
/**                                                                   **/
/**  IF the first character is NOT a '\' and the '\' has been found   **/
/**           flag is TRUE                                            **/
/**     Write '[.' to a temporary string                              **/
/**     Copy temp_file_spec to the temporary string                   **/
/**     Copy the temporary string back to temp_file_spec              **/
/**     Free the temporary string                                     **/
/**  ELSE IF the first character is a '\' and the '\' has been found  **/
/**           flag is TRUE                                            **/
/**     Change the '\' to a '['                                       **/
/**  ELSE IF the first character is a '\' and the '\' has been found  **/
/**           flag is FALSE                                           **/
/**     Increment temp_file_spec to skip the '\'                      **/
/**  ENDIF                                                            **/
/**                                                                   **/
/**  Return file_spec                                                 **/
/*---------------------------------------------------------------------*/

    while (temp_ptr != temp_file_spec)
    {                                                          
      if (*temp_ptr == '\\')
      {
         if (found)
         {                         
            *temp_ptr = '.';
         }
         else
         {
            *temp_ptr = ']';
            found = TRUE;
         }   
      }
      temp_ptr--;
    }
    if (*temp_file_spec != '\\' && found)
    {
       Malloc_String(temp_str, ((int) String_Size(temp_file_spec) + 2));
       strcpy (temp_str,"[.");
       strcat (temp_str,temp_file_spec);
       strcpy (temp_file_spec,temp_str);
       Lemme_Go(temp_str);
    }
    else if (*temp_file_spec == '\\' && found)
    {
       *temp_file_spec = '[';
    }  
    else if (*temp_file_spec == '\\' && !found)
    {
       temp_file_spec++;
    }
                                      
    strcpy (file_spec,temp_file_spec);

    return (file_spec);
             
/** END **/
}  /*  End: "cvt_dos_to_vax_file_spec"  */



/**********************************************************************
 *$Component                                                          *
 *    char *cvt_unix_to_dos_file_spec (file_spec)                     *
 *$Abstract                                                           *
 *    Convert a UNIX file spec to a DOS file spec.                    *
 *$Keywords                                                           *
 *    CONVERTLIB                                                      *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    file_spec:                                                      *
 *        The file_spec variable contains the full file               *
 *        specification including the path (or directory), filename,  *
 *        extension, and version number if there is one.              *
 *$Outputs                                                            *
 *    file_spec:                                                      *
 *        The file_spec variable contains the full file               *
 *        specification including the path (or directory), filename,  *
 *        extension, and version number if there is one.              *
 *$Returns                                                            *
 *    file_spec:                                                      *
 *        The file_spec variable contains the full file               *
 *        specification including the path (or directory), filename,  *
 *        extension, and version number if there is one.              *
 *$Detailed_Description                                               *
 *    The cvt_unix_to_dos_file_spec subroutine converts a full        *
 *    UNIX file specification to a DOS file specification. Any        *
 *    disk references are stripped off, and do not appear in the      *
 *    resulting string.                                               *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Kristy L. Marski /JPL                                           *
 *$Version_and_Date                                                   *
 *    1.0   July 25, 1990                                             *
 *$Change_history                                                     *
 *    KLM   07-25-90   Original generation.                           *
 **********************************************************************/

char *cvt_unix_to_dos_file_spec (file_spec)

char *file_spec;
         
{

    char *temp_file_spec;         
 
    temp_file_spec = file_spec;          
    if (*file_spec == NULL)
    {
      return (file_spec);
    }

/*--------------------------------------------------------------------------*/
/**  BEGIN                                                                 **/
/**                                                                        **/
/**   Strip all characters up to the ':' -> CALL UTIL_STRIP_TO_CHAR        **/
/**   IF the current character is a ':' THEN                               **/
/**       move the pointer one character to skip the ':'                   **/
/**   ENDIF                                                                **/
/**                                                                        **/
/**   Replace '/' with '\'  -> CALL UTIL_REPLACE_CHAR                      **/
/**                                                                        **/
/**  END                                                                   **/
/*--------------------------------------------------------------------------*/
                                
    util_strip_to_char (temp_file_spec,':');

    if (*temp_file_spec == ':')
    {
      temp_file_spec++;
    }

    util_replace_char (temp_file_spec,'/','\\');

    strcpy (file_spec,temp_file_spec);

    return (file_spec);


}  /*  End: "cvt_unix_to_dos_file_spec"  */

/**********************************************************************
 *$Component                                                          *
 *    char *cvt_unix_to_vax_file_spec (file_spec)                     *
 *$Abstract                                                           *
 *    Convert a UNIX file spec to a VAX file spec.                    *
 *$Keywords                                                           *
 *    CONVERTLIB                                                      *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    file_spec:                                                      *
 *        The file_spec variable contains the full file               *
 *        specification including the path (or directory), filename,  *
 *        extension, and version number if there is one.              *
 *$Outputs                                                            *
 *    file_spec:                                                      *
 *        The file_spec variable contains the full file               *
 *        specification including the path (or directory), filename,  *
 *        extension, and version number if there is one.              *
 *$Returns                                                            *
 *    file_spec:                                                      *
 *        The file_spec variable contains the full file               *
 *        specification including the path (or directory), filename,  *
 *        extension, and version number if there is one.              *
 *$Detailed_Description                                               *
 *    The cvt_unix_to_vax_file_spec subroutine converts a full        *
 *    UNIX file specification to a VAX file specification. Any        *
 *    disk references are stripped off, and do not appear in the      *
 *    resulting string.                                               *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Kristy L. Marski /JPL                                           *
 *$Version_and_Date                                                   *
 *    1.0   September 20, 1990                                        *
 *$Change_history                                                     *
 *    KLM   09-20-90   Original generation.                           *
 **********************************************************************/

char *cvt_unix_to_vax_file_spec (file_spec)

char *file_spec;
                            
{
    if (*file_spec == NULL)
    {
      return (file_spec);
    }

    cvt_unix_to_dos_file_spec (file_spec);

    cvt_dos_to_vax_file_spec (file_spec);

    return (file_spec);

}  /*  End: "cvt_unix_to_vax_file_spec"  */


/**********************************************************************
 *$Component                                                          *
 *    char *cvt_vax_to_dos_file_spec (file_spec)                      *
 *$Abstract                                                           *
 *    Convert a VAX file spec to a DOS file spec.                     *
 *$Keywords                                                           *
 *    CONVERTLIB                                                      *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    file_spec:                                                      *
 *        The file_spec variable contains the full file               *
 *        specification including the path (or directory), filename,  *
 *        extension, and version number if there is one.              *
 *$Outputs                                                            *
 *    file_spec:                                                      *
 *        The file_spec variable contains the full file               *
 *        specification including the path (or directory), filename,  *
 *        extension, and version number if there is one.              *
 *$Returns                                                            *
 *    file_spec:                                                      *
 *        The file_spec variable contains the full file               *
 *        specification including the path (or directory), filename,  *
 *        extension, and version number if there is one.              *
 *$Detailed_Description                                               *
 *    The cvt_vax_to_dos_file_spec subroutine converts a full         *
 *    VAX file specification to a DOS file specification. Any         *
 *    disk references are stripped off, and do not appear in the      *
 *    resulting string.                                               *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Kristy L. Marski /JPL                                           *
 *$Version_and_Date                                                   *
 *    1.0   July 27, 1990                                             *
 *$Change_history                                                     *
 *    KLM   07-27-90   Original generation.                           *
 **********************************************************************/

char *cvt_vax_to_dos_file_spec (file_spec)

char *file_spec;
         
{
    char *temp_file_spec;         
    char *new_char;
    char *temp_ptr;
    LOGICAL found;                               

    found = TRUE;
    temp_file_spec = file_spec;          

    if (*file_spec == NULL)
    {
      return (file_spec);
    }

/*--------------------------------------------------------------------------*/
/**  BEGIN                                                                 **/
/**                                                                        **/
/**   Strip all characters up to the ':' -> CALL UTIL_STRIP_TO_CHAR        **/
/**   IF the current character is a ':' THEN                               **/
/**       move the pointer one character to skip the ':'                   **/
/**       IF the current character is a ':' THEN                           **/
/**          move the pointer one character to skip the ':'                **/
/**          strip all characters up to the ':' -> CALL UTIL_STRIP_TO_CHAR **/
/**          move the pointer one character to skip the ':'                **/
/**       ENDIF                                                            **/
/**   ENDIF                                                                **/
/**                                                                        **/
/*--------------------------------------------------------------------------*/
/*   NOTE:                                                                  */
/*   This handles the case when there is a node and a device as part of a   */
/*   file_spec. Example: JPLPDS::DISK$USER1:[file_spec]                     */
/*--------------------------------------------------------------------------*/

    util_strip_to_char (temp_file_spec,':');
    if (*temp_file_spec == ':')
    {
       temp_file_spec++;
       if (*temp_file_spec == ':')
       {
          temp_file_spec++;
          util_strip_to_char (temp_file_spec, ':');
          temp_file_spec++;
       }
    } 

    new_char = temp_file_spec;                                           

/*--------------------------------------------------------------------------*/
/**   Compress multiple occurrences of '.' -> CALL UTIL_COMPRESS_CHAR      **/
/**                                                                        **/
/**   IF the first character encountered is a '[' THEN                     **/
/**      move new_char to the next character in the string                 **/
/**      IF the next character is a '.'  THEN                              **/
/**         move new_char to the next character in the string              **/ 
/**         set temp_file_spec to point to the same place as new_char      **/
/**      ELSE                                                              **/
/**         set the current value of temp_file_spec to '\'                 **/
/**      ENDIF                                                             **/
/**   ENDIF                                                                **/
/**                                                                        **/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------*/
/*  NOTE:                                                             */
/*  This part checks to see if the first two characters in the        */
/*  file_spec are a '[.', which gets discarded, or a '[', which gets  */
/*  changed to  a '\'.                                                */
/*--------------------------------------------------------------------*/

    util_compress_char (temp_file_spec,'.');

    if (*temp_file_spec == '[')
    {
       new_char++;
       found = FALSE;
                  
       if (*new_char == '.')
       {
          new_char++;
          temp_file_spec = new_char;
       }
       else
       {
          *temp_file_spec = '\\';
       }
    }                                                                   
                                                                              
/*--------------------------------------------------------------------------*/
/**   LOOP through the string ...                                          **/
/**                                                                        **/
/**   IF a ']' is found                                                    **/
/**      change it to a '\'                                                **/
/**      set the found ']' flag to true                                    **/
/**   ELSE IF the current character is a '.' and a ']' hasn't been found   **/
/**      set temp_ptr to the current position of new_char                  **/
/**      check the next character                                          **/
/**      IF the next character is a ']'                                    **/
/**         remove the ']' CALL -> UTIL_REMOVE_CHAR                        **/
/**         set the found ']' flag to true                                 **/
/**      ENDIF                                                             **/
/**      change the '.' to a '\'                                           **/
/**   ELSE IF the current character is a ';'                               **/
/**      set it equal to NULL                                              **/
/**   ENDIF                                                                **/
/**                                                                        **/
/**   copy the temp_file_spec into file_spec                               **/
/**   RETURN file_spec                                                     **/
/**                                                                        **/
/*--------------------------------------------------------------------------*/

    for (new_char = temp_file_spec; *new_char != EOS; ++new_char)
    {

      if (*new_char == ']')
      {
          *new_char = '\\';
          found = TRUE;
      }

      else if ((*new_char == '.') && (!found))
      {
          temp_ptr = new_char;
          temp_ptr++;

          if (*temp_ptr == ']')
          {
             util_remove_char (new_char, ']');
             found = TRUE;
          } 

          *new_char = '\\';                   
      }

      else if (*new_char == ';')
      {                                          
          *new_char = '\0';
      }
    }  /*  End:  "for (new_char = file_spec; ..."  */  
              
    strcpy (file_spec,temp_file_spec);

    return (file_spec);

/**  END          **/


}  /*  End: "cvt_vax_to_dos_file_spec"  */


/**********************************************************************
 *$Component                                                          *
 *    char *cvt_vax_to_unix_file_spec (file_spec)                     *
 *$Abstract                                                           *
 *    Convert a VAX file spec to a UNIX file spec.                    *
 *$Keywords                                                           *
 *    CONVERTLIB                                                      *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    file_spec:                                                      *
 *        The file_spec variable contains the full file               *
 *        specification including the path (or directory), filename,  *
 *        extension, and version number if there is one.              *
 *$Outputs                                                            *
 *    file_spec:                                                      *
 *        The file_spec variable contains the full file               *
 *        specification including the path (or directory), filename,  *
 *        extension, and version number if there is one.              *
 *$Returns                                                            *
 *    file_spec:                                                      *
 *        The file_spec variable contains the full file               *
 *        specification including the path (or directory), filename,  *
 *        extension, and version number if there is one.              *
 *$Detailed_Description                                               *
 *    The cvt_vax_to_unix_file_spec subroutine converts a full        *
 *    VAX file specification to a UNIX file specification. Any        *
 *    disk references are stripped off, and do not appear in the      *
 *    resulting string.                                               *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Kristy L. Marski /JPL                                           *
 *$Version_and_Date                                                   *
 *    1.0   September 20, 1990                                        *
 *$Change_history                                                     *
 *    KLM   09-20-90   Original generation.                           *
 **********************************************************************/

char *cvt_vax_to_unix_file_spec (file_spec)

char *file_spec;
                            
{                                          
    if (*file_spec == NULL)
    {
      return (file_spec);
    }

    file_spec = cvt_vax_to_dos_file_spec (file_spec);
     
    file_spec = cvt_dos_to_unix_file_spec (file_spec);

    return (file_spec);

}  /*  End: "cvt_vax_to_unix_file_spec"  */

