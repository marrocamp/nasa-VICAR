/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Component                                                           * 
 *    File ddaccess.c                                                  *
 * Abstract                                                            *
 *    Utilities for acccesing the data dictionary                      *
 * Detailed Description                                                *
 *    This file contains the routines used by PDS software to access   *
 *    the definitions of objects and elements in the data dictionary   *
 * Internal References                                                 *
 *    dd_cleanup_defs                                                  *
 *    dd_get_data_type      	                                       *    
 *    dd_get_definition                                                *    
 *    dd_get_all_definitions                                           *    
 *    dd_get_description                                               *
 *    dd_get_id_members                                                *
 *    dd_get_object_class                                              *    
 *    dd_get_object_definition                                         *    
 *    dd_get_length_range                                              *    
 *    dd_get_optional_members                                          *    
 *    dd_get_optional_objects                                          *    
 *    dd_get_required_members                                          *    
 *    dd_get_required_objects                                          *    
 *    dd_get_standard_value_type                                       *    
 *    dd_get_standard_values                                           *    
 *    dd_get_value_range                                               *    
 *    dd_init                                                          *    
 *    dd_name_exists                                                   *
 *    dd_search_index                                                  *    
 *    dd_unalias                                                       *    
 * Authors and Institutions                                            *
 *    Marti D. DeMore / J.P.L.                                         *
 * Version and Date                                                    *
 *    1.2   January 24, 1992                                           *
 * Change History                                                      *
 *    MDD   04-03-91   Original code.                                  *
 *    MDD   10-22-91   Included contents of ddglob.h and deleted that  *
 *                     file.                                           *
 *    MDD   01-24-92   Added dd_get_description and dd_get_standard_   *
 *                     value_type.                                     *
 *    DWS   02-16-00   Added FORMATION to standard val type choices    *
 *                     in dd_get_standard_values                       *
 *    DWS   08-23-01   Added initialization for alias_list in          *
 *                     dd_unalieas to prevent program crash.           *
 *    MDC   03-05-05   Initialize dd_file_name and dd_index_name to    *
 *                     prevent program crash.                          *
 *                     Modified dd_get_definition routine. See notes.  *
 *                     Modified dd_unalias routine. See notes.         *
 *    MDC   04-11-06   Modified dd_search_index routine. See notes.    *
 *    MDC   09-27-06   Modified dd_unalias routine. See notes.         *
 *    MDC   11-01-06   Made a bug fix in the dd_init routine. See notes*
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "pdsdef.h"
#include "ddaccess.h"
#include "errordef.h"
#include "fiodef.h"
#include "label.h"
#include "utildef.h"
#include "labutil.h"
#include "verlabel.h"
#include "sysdef.h"

extern ERROR_LIST *pds_last_message;

        /*------------------------------------------------------------*/
        /*  The definition_root variable points to the root of the    */
        /*  tree containing the DD definitions currently in program   */
        /*  memory.                                                   */
        /*------------------------------------------------------------*/

AGGREGATE definition_root = NULL;

        /*------------------------------------------------------------*/
        /*  The following variables are used to store information     */
        /*  about the starting byte, ending byte, and record length   */
        /*  of the DD index, so that it can be binary searched.       */
        /*------------------------------------------------------------*/

long begin_index = 0;
long end_index = 0;
long index_rec_len = 0;

        /*------------------------------------------------------------*/
        /*  The next two variables store the file names of the DD     */
        /*  definition file and the DD index file.                    */
        /*------------------------------------------------------------*/

/* 03-05-05 MDC - Initialize these variables */
char dd_file_name [PDS_MAXLINE + 1] = "\0";
char dd_index_name [PDS_MAXLINE + 1] = "\0";

        /*------------------------------------------------------------*/
        /*  The next variable is a flag indicating whether the last   */
        /*  object definition read was generic or specific            */
        /*------------------------------------------------------------*/

int dd_object_type;

        /*------------------------------------------------------------*/
        /*  The next variable is the offset of the alias list in the  */
        /*  DD file.                                                  */
        /*------------------------------------------------------------*/

long dd_alias_offset = 0;


/**********************************************************************
 *$Component                                                          *
 *    void dd_cleanup_defs ()                                         *
 *$Abstract                                                           *
 *    Frees memory used by DD definitions residing in memory.         *
 *$Keywords                                                           *
 *    DDACCESS                                                        *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    The dd_cleanup_defs routine frees memory used by any object or  *
 *    element definitions in memory and closes any open DD files. It  *
 *    also initializes the DD definition tree in preparation for new  *
 *    activity.                                                       *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    definition_root         ddglob.h                 update         *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.0   April 3, 1991                                             *
 *$Change_History                                                     *
 *    MDD   04-03-91   Original generation.                           *
 **********************************************************************/

void dd_cleanup_defs ()
{
   if (definition_root != NULL)
   {
      lab_remove_label_or_template (definition_root);
   }
   definition_root = lu_append_object (NULL, "ROOT");
   return;   
}

/**********************************************************************
 *$Component                                                          *
 *    char *dd_get_data_type (dd_name)                                *
 *$Abstract                                                           *
 *    Gets the data type of an element from the DD.                   *
 *$Keywords                                                           *
 *    DDACCESS                                                        *
 *$Inputs                                                             *
 *    dd_name:                                                        *
 *        The dd_name variable is a string containing the name of a   *
 *        DD object or element.                                       *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    dd_data_type:                                                   *
 *        The dd_data_type variable is a string containing the        *
 *        general data type of an element in the data dictionary.     *
 *$Detailed_Description                                               *
 *    The dd_get_data_type routine fetches the data type of an element*
 *    from the element definitions residing in memory, if a           *
 *    definition for the element is found.  If a value is not found,  *
 *    or if it is N/A, NULL, or UNK, then the string "UNKNOWN" is     *
 *    returned.                                                       *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    definition_root         ddglob.h                 read           *
 *$Side_Effects                                                       *
 *    This routine returns a pointer to memory which must be freed    *
 *    elsewhere.                                                      *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.1   October 21, 1991                                          *
 *$Change_History                                                     *
 *    MDD   04-03-91   Original generation.                           *
 *    MDD   10-21-91   Fixed malloc and sys_exit_system calls         *
 *    MDD   02-11-93   Added check for CONTEXTDEPENDENT data type to  *
 *                     convert it to NULL, since it cannot be handled.*
 **********************************************************************/

char *dd_get_data_type (dd_name)

char *dd_name;
{
   char *type = NULL;
   int label_status = PDS_ERROR;

/** BEGIN **
   /*-----------------------------------------------------------------*/
   /** get the data type of the given element                        **/
   /*-----------------------------------------------------------------*/

   type = lab_get_value (definition_root, "ELEMENT_DEFINITION", dd_name, 1,
                            "GENERAL_DATA_TYPE", 1, 1, FALSE, &label_status);

   /*-----------------------------------------------------------------*/
   /** IF the value was NULL or UNK or N/A THEN                      **/
   /**    set the data type to NULL                                  **/
   /** ENDIF                                                         **/
   /*-----------------------------------------------------------------*/
 
   if (type == NULL || (label_status == PDS_SUCCESS && (IsNull(type) ||
                        strcmp (type, "CONTEXTDEPENDENT") == 0)))
   {
      Lemme_Go(type);
      Malloc_String(type, (int) String_Size ("NULL"));
      strcpy (type, "NULL");
   }
   return (type);

/** END dd_get_data_type **/
}

/**********************************************************************
 *$Component                                                          *
 *    LOGICAL dd_get_definition (dd_name, dd_type, dd_def_action)     *
 * or for data dictionary                                             *
 *    LOGICAL dd_get_definition (rpt_file_ptr, dd_name, dd_type,      *
 *            dd_def_action)                                          *
 *                                                                    *
 *$Abstract                                                           *
 *    Gets the entire definition of an element or object from the DD. *
 *$Keywords                                                           *
 *    DDACCESS                                                        *
 *$Inputs                                                             *
 *    dd_name:                                                        *
 *        The dd_name variable is a string containing the name of a   *
 *        DD object or element.                                       *
 *    dd_type:                                                        *
 *        The dd_type variable is a string containing the type of a   *
 *        DD definition, e.g., OBJECT_DEFINITION or                   *
 *        ELEMENT_DEFINITION.                                         *
 *    dd_def_action:                                                  *
 *        The dd_def_action variable  is a flag indicating the fate   *
 *        of the DD definitions residing in memory, e.g., DD_SAVE_DEFS*
 *        or DD_REPLACE_DEFS.                                         *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure.       *
 *$Detailed_Description                                               *
 *    The dd_get_definition routine reads the definition of the data  *
 *    dictionary object dd_name from the data dictionary into memory. *
 *    If the definition cannot be found in the data dictionary,       *
 *    then this routine returns FALSE, otherwise it returns TRUE.     *
 *    If the dd_def_action flag is DD_REPLACE_DEFS, then all the      *
 *    definitions currently residing in memory are destroyed before   *
 *    the new definition is read. If the definition of dd_name already*
 *    exists in memory, no new definition is read, but TRUE is still  *
 *    returned.                                                       *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    definition_root         ddglob.h                 update         *
 *    dd_file_name            ddglob.h                 read           *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.3   October 21, 1991                                          *
 *$Change_History                                                     *
 *    MDD   04-03-91   Original generation.                           *
 *    DPB   05-17-91   Replaced the call to lab_parse_string with a   *
 *                     call to ReadLabel.                             *
 *    DPB   06-13-91   Moved generic object message to the verifier.  *
 *    MDD   10-21-91   Fixed malloc, free, and sys_exit_system calls  *
 *    RSJ   12-18-97   Added DDictionary Extractor code               *
 *    DWS   03-13-98   Added conditional compiles for DDictionary     *
 *    MDC   03-05-05   Check for NULL value in dd_file_name first     *
 *                     before calling the fopen function to prevent   *
 *                     program crash                                  *
 **********************************************************************/

#ifndef LV_DDICT
LOGICAL dd_get_definition (dd_name, dd_type, dd_def_action)

char *dd_name;
char *dd_type;
int dd_def_action;
#else
LOGICAL dd_get_definition (rpt_file_ptr, dd_name, dd_type, dd_def_action)

FILE *rpt_file_ptr;
char *dd_name;
char *dd_type;
int dd_def_action;
#endif
{
   LOGICAL found = FALSE;
   int label_status = PDS_ERROR;
   AGGREGATE new_object = NULL;
   long dd_offset;
   long dd_size;
   FILE *dd_file = NULL;
   ERROR_LIST *end_of_list = NULL;
#ifdef LV_DDICT
   char string4[100];
   int  done_looking;
   int  str_len;
   char *new_str = {NUL};
   int counter;
#endif

/** BEGIN **/

   /*--------------------------------------------------------------------*/
   /** IF a name and type were passed in THEN                           **/
   /*--------------------------------------------------------------------*/

   if (dd_name != NULL && dd_type != NULL)
   {
      /*-----------------------------------------------------------------*/
      /** IF the definitions in memory should be deleted THEN           **/
      /**    cleanup the definitions and start a new batch              **/
      /** ENDIF                                                         **/
      /*-----------------------------------------------------------------*/

      if (dd_def_action == DD_REPLACE_DEFS)
      {
         dd_cleanup_defs ();
      }

      /*--------------------------------------------------------------*/
      /** IF the DD object is already in memory THEN                 **/
      /**    set the return value to TRUE                            **/
      /*--------------------------------------------------------------*/

      lab_find_object (definition_root, dd_type, dd_name, 1, &label_status);
      if (label_status == PDS_SUCCESS)
      {
         found = TRUE;
      }

      /*--------------------------------------------------------------*/
      /** ELSE IF the name is found in the index THEN                **/
      /*--------------------------------------------------------------*/

      else if (dd_search_index (dd_name, dd_type, &dd_offset, &dd_size) 
                  == TRUE)
      {
         /*-----------------------------------------------------------*/
         /** IF the DD definition file can be opened THEN            **/
         /*-----------------------------------------------------------*/

		 /* 03-05-05 MDC - Add conditions to check for NULL dd_file_name */
#ifdef VAX
         if(strcmp(dd_file_name,"\0") != 0) dd_file = fopen (dd_file_name, "r", "rfm=stm");
#else
         if(strcmp(dd_file_name,"\0") != 0) dd_file = fopen (dd_file_name, "r");
#endif
         if (dd_file != NULL)
         {      
            /*--------------------------------------------------------*/
            /** read the definition of the object from the file      **/
            /*--------------------------------------------------------*/

            fseek (dd_file, dd_offset, 0);

            /*--------------------------------------------------------*/
            /** parse the definition and add it to the tree          **/
            /** delete error messages added to the list in parsing   **/
            /*--------------------------------------------------------*/

            new_object = definition_root -> last_child;
            end_of_list = pds_last_message;
            found = (ReadLabel (dd_file, definition_root) &&
                      (! (definition_root -> last_child == new_object)));
            if (end_of_list != pds_last_message)
                err_deallocate_list (end_of_list -> next);
            new_object = definition_root -> last_child;

            /*--------------------------------------------------------*/
            /** IF the new definition exists and is for an object    **/
            /**    THEN                                              **/
            /*--------------------------------------------------------*/

            if (found && strcmp (dd_type, "OBJECT_DEFINITION") == 0)
            { 
 
               /*-----------------------------------------------------*/
               /** the object has to be renamed from a GENERIC or    **/
               /** SPECIFIC object definition to just plain old      **/
               /** OBJECT_DEFINITION so that other DD routines       **/
               /** won't have to fret over it                        **/
               /*-----------------------------------------------------*/

               Lemme_Go(new_object -> name);
               Malloc_String(new_object -> name, (int) String_Size(dd_type));
               strcpy (new_object -> name, dd_type);

            }
            /*--------------------------------------------------------*/
            /** ELSE IF the definition was not found THEN            **/
            /**    attempt to remove junk from memory                **/
            /*--------------------------------------------------------*/

            else if (!found)
            {
               RemoveAggregate (new_object);
            }       
            /*--------------------------------------------------------*/
            /** ENDIF                                                **/
            /*--------------------------------------------------------*/
#ifdef LV_DDICT
            /*--------------------------------------------------------*/
            /** RSJ: 12-18-97 - DDict                                **/
            /** IF dd_def_action = DD_WRITE_DEFS_TO_FILE, then       **/
            /**   write the keyword and its attributes to the        **/ 
            /**   report file.                                       **/
            /** The keyword definition can reside in more than one   **/
            /**   location.  If the string in the 1st location       **/
            /**   contains "\n" as the 1st two characters, use this  **/
            /**   location, otherwise, use the alternate location.   **/
            /*--------------------------------------------------------*/
            if (dd_def_action == DD_WRITE_DEFS_TO_FILE)
            {
              /*--------------------------------------------------------*/
              /** Write the Keyword Name                               **/
              /*--------------------------------------------------------*/
              fprintf(rpt_file_ptr, "\n%s\n", new_object             
                                              ->first_parameter
                                              ->first_value
                                              ->item.value.string);
              /*--------------------------------------------------------*/
              /** Write the Keyword Definition                         **/
              /**   (use the location which starts with "\n" as the    **/
              /**    1st two characters)                               **/
              /*--------------------------------------------------------*/

/*			  size = sizeof(new_object
                         ->last_parameter
                         ->first_value
                         ->item.value.string);

              string1 = (char *) malloc ((int) size + 20);
              Check_Malloc(string1);
              string1 =  new_object
                         ->last_parameter
                         ->first_value
                         ->item.value.string;
              if (*string1 ==  '\\' && *(string1+1) == 'n') 
              {
              }
              else
              {
			      size = sizeof(new_object
                         ->last_parameter
                         ->left_sibling
                         ->first_value
                         ->item.value.string);
                  Lemme_Go(string1);
                  string1 = (char *) malloc ((int) size + 20);
                  Check_Malloc(string1);
                  string1 =  new_object
                           ->last_parameter
                           ->left_sibling
                           ->first_value
                           ->item.value.string;
              }
*/
              /*--------------------------------------------------------*/
			  /*  format the string into 72 character strings terminated*/
			  /*  by newline characters.   Unfortunately there are also */
			  /*  imbedded "\n" strings that must be converted to       */
			  /*  newline chars                                         */
			  /*--------------------------------------------------------*/
              fseek (dd_file, dd_offset, 0);
			  done_looking = 0;
			  while(done_looking == 0)
			  {
		          fgets(string4, 80, dd_file);
		          if((string4[0] == ' ') && (string4[1] == ' ') && (string4[2] == ' ')
				                  && (string4[3] == ' ') && (string4[4] == ' '))
				  {
				       done_looking = 1;
					   str_len = strlen(string4);
					   for (counter = 0; counter < str_len; counter++)
					   {
						   if(string4[counter] == '"') string4[counter] = ' ';
					   }
                       fprintf(rpt_file_ptr, "%s", string4);
				  }
			  }
			  while(done_looking ==  1)
			  {
			      fgets(string4, 80, dd_file);
		          if(((string4[0] == ' ') && (string4[1] == ' ') && (string4[2] == ' ')
				                  && (string4[3] == ' ') && (string4[4] == ' '))
								          || (strlen(string4) == 1))
				  {
					   str_len = 0;
					   str_len = strlen(string4);
					   for (counter = 0; counter < str_len; counter++)
					   {
						   if(string4[counter] == '"') string4[counter] = ' ';
					   }
                       fprintf(rpt_file_ptr, "%s", string4);
				  }
				  else
				  {
					  done_looking = 0;
				  }
			  }

            }
#endif
            fclose (dd_file);
         }      
         /*-----------------------------------------------------------*/
         /** ENDIF the DD definition file could be opened            **/
         /*-----------------------------------------------------------*/
      }
      /*--------------------------------------------------------------*/
      /** ELSE IF the name is found in the index THEN                **/
      /*--------------------------------------------------------------*/
   }
   /*--------------------------------------------------------------------*/
   /** ENDIF a name and type were passed in...                          **/
   /** RETURN TRUE if the definition was found                          **/
   /*--------------------------------------------------------------------*/

   return (found);

/** END dd_get_definition **/
}


/**********************************************************************
 *$Component                                                          *
 *    void dd_get_all_definitions (dd_name_list, dd_type,             *
 *                                 dd_def_action)                     *
 *$Abstract                                                           *
 *    Gets a group of definitions from the DD.                        *
 *$Keywords                                                           *
 *    DDACCESS                                                        *
 *$Inputs                                                             *
 *    dd_name_list:                                                   *
 *        The dd_name_list variable is pointer to a list of strings   *
 *        containing names of DD objects or elements.                 *
 *    dd_type:                                                        *
 *        The dd_type variable is a string containing the type of a   *
 *        DD definition, e.g., OBJECT_DEFINITION or                   *
 *        ELEMENT_DEFINITION.                                         *
 *    dd_def_action:                                                  *
 *        The dd_def_action variable  is a flag indicating the fate   *
 *        of the DD definitions residing in memory, e.g., DD_SAVE_DEFS*
 *        or DD_REPLACE_DEFS.                                         *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    The dd_get_all_definitions routine gets the definitions of all  *
 *    the objects on the list dd_name_list from the data dictionary.  *
 *    If the dd_def_action flag is DD_REPLACE_DEFS, then all the      *
 *    definitions currently residing in memory are destroyed before   *
 *    the new definitions are read.                                   *
 *$Error_Handling                                                     *
 *    Or "lack thereof."  Since it didn't seem reasonable to return   *
 *    failure if only one thing on the list could not be found in     *
 *    the data dictionary (this will probably happen with some        *
 *    elements) this routine returns nothing.                         *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.0   April 3, 1991                                             *
 *$Change_History                                                     *
 *    MDD   04-03-91   Original generation.                           *
 **********************************************************************/

void dd_get_all_definitions (dd_name_list, dd_type, dd_def_action)

STRING_LIST *dd_name_list;
char *dd_type;
int dd_def_action;

{
/** BEGIN **/

   /*-----------------------------------------------------------------*/
   /** IF the definitions in memory should be deleted THEN           **/
   /**    cleanup the definitions and start a new batch              **/
   /** ENDIF                                                         **/
   /*-----------------------------------------------------------------*/

   if (dd_def_action == DD_REPLACE_DEFS)
   {
      dd_cleanup_defs ();
   }

   /*-----------------------------------------------------------------*/
   /** attempt to get the definition of everything on the list       **/
   /*-----------------------------------------------------------------*/

   for (; dd_name_list != NULL; dd_name_list = dd_name_list -> next)
   {
#ifndef LV_DDICT
      dd_get_definition (dd_name_list -> text, dd_type, DD_SAVE_DEFS);
#else
      dd_get_definition (NULL, dd_name_list -> text, dd_type, DD_SAVE_DEFS);
#endif
   }
   return;

/** END dd_get_all_definitions **/
}


/**********************************************************************
 *$Component                                                          *
 *    char *dd_get_description (dd_name, dd_type)                     *
 *$Abstract                                                           *
 *    Gets the description of an object or element from the DD.       *
 *$Keywords                                                           *
 *    DDACCESS                                                        *
 *$Inputs                                                             *
 *    dd_name:                                                        *
 *        The dd_name variable is a string containing the name of a   *
 *        DD object or element.                                       *
 *    dd_type:                                                        *
 *        The dd_type variable is a string containing the type of a   *
 *        DD definition, e.g., OBJECT_DEFINITION or                   *
 *        ELEMENT_DEFINITION.                                         *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    dd_description:                                                 *
 *        The dd_description variable is a string containing the      *
 *        general description of an element or object pds_class in the    *
 *        data dictionary.                                            *
 *$Detailed_Description                                               *
 *    The dd_get_description routine fetches the description of an    *
 *    element or object pds_class from the data dictionary definitions    *
 *    residing in memory, if a matching definition is found. The      *
 *    dd_type input indicates whether to search for an element or     *
 *    object definition.  If the description is not found, or is      *
 *    N/A, UNK, or NULL, then the string UNKNOWN is returned.         *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    definition_root         ddglob.h                 read           *
 *$Side_Effects                                                       *
 *    This routine returns a pointer to memory which must be freed    *
 *    elsewhere.                                                      *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.0   January 24, 1992                                          *
 *$Change_History                                                     *
 *    MDD   01-24-92   Original code                                  *
 **********************************************************************/

char *dd_get_description (dd_name, dd_type)

char *dd_name;
char *dd_type;
{
   char *desc = NULL;
   int label_status = PDS_ERROR;

/** BEGIN **/
   /*-----------------------------------------------------------------*/
   /** get the description of the given element or object            **/
   /*-----------------------------------------------------------------*/

   desc = lab_get_value (definition_root, dd_type, dd_name, 1,
                         "DESCRIPTION", 1, 1, FALSE, &label_status);

   /*-----------------------------------------------------------------*/
   /** IF the value was NULL or UNK or N/A THEN                      **/
   /**    set the description to UNKNOWN                             **/
   /** ENDIF                                                         **/
   /*-----------------------------------------------------------------*/
 
   if (desc == NULL || (label_status == PDS_SUCCESS && IsNull(desc)))
   {
      Lemme_Go(desc);
      Malloc_String(desc, (int) String_Size ("UNKNOWN"));
      strcpy (desc, "UNKNOWN");
   }
   return (desc);

/** END **/
}




/**********************************************************************
 *$Component                                                          *
 *   STRING_LIST *dd_get_id_members (dd_name)                         *
 *$Abstract                                                           *
 *    Gets the list of identification members of an object            *
 *$Keywords                                                           *
 *    DDACCESS                                                        *
 *$Inputs                                                             *
 *    dd_name:                                                        *
 *        The dd_name variable is a string containing the name of a   *
 *        DD object or element.                                       *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    member_list:                                                    *
 *        The member_list structure is a pointer to a STRING_LIST     *
 *        which contains the names of data elements which are         *
 *        members of some object.                                     *
 *$Detailed_Description                                               *
 *    The dd_get_id_members routine gets the identification member    *
 *    set for object dd_name and returns it in a STRING_LIST.  If     *
 *    the set is "N/A" or an error occurs, this routine returns NULL. *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    definition_root         ddglob.h                 update         *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.0   April 5, 1991                                             *
 *$Change_History                                                     *
 *    MDD   04-05-91   Original generation.                           *
 **********************************************************************/

STRING_LIST *dd_get_id_members (dd_name)

char *dd_name;

{
   int label_status = PDS_ERROR;

   STRING_LIST *member_list = NULL;

   /** BEGIN **/
   /*-----------------------------------------------------------------*/
   /** get the identification elements for object dd_name into a list**/
   /*-----------------------------------------------------------------*/

   member_list = lab_get_all_values (definition_root, "OBJECT_DEFINITION", 
                                        dd_name, 1, "ID_ELEMENT_SET", 1, 
                                        FALSE, &label_status);

   /*-----------------------------------------------------------------*/
   /** IF there was an error THEN                                    **/
   /**    deallocate the list and set it to NULL                     **/
   /*-----------------------------------------------------------------*/

   if (label_status != PDS_SUCCESS)
   {
      member_list = util_deallocate_string_list (member_list);
   }
   /*-----------------------------------------------------------------*/
   /** ELSE IF the only value on the list is N/A or UNK THEN         **/
   /**    deallocate the list and set it to NULL                     **/
   /*-----------------------------------------------------------------*/

   else if (member_list != NULL && 
              IsNull(member_list -> text))
   {
      if (member_list -> next == NULL)
         member_list = util_deallocate_string_list (member_list);
   }

   /*-----------------------------------------------------------------*/
   /** ENDIF there was an error...                                   **/
   /** RETURN the list of members                                    **/
   /*-----------------------------------------------------------------*/

   return (member_list);

   /** END dd_get_id_members **/
}


/**********************************************************************
 *$Component                                                          *
 *    char *dd_get_object_class (dd_name)                             *
 *$Abstract                                                           *
 *    Finds an object pds_class match in the Data Dictionary              *
 *$Keywords                                                           *
 *    DDACCESS                                                        *
 *$Inputs                                                             *
 *    dd_name:                                                        *
 *        The dd_name variable is a string containing the name of a   *
 *        DD object or element.                                       *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    object_class:                                                   *
 *        The object_class variable is a character string             *
 *        which contains the pds_class of an object in a PDS label        *
 *        (e.g., "OBJECT = IMAGE" implies that "IMAGE" is the pds_class   *
 *        of the object).                                             *
 *$Detailed_Description                                               *
 *    The dd_get_object_class routine searches the data dictionary    *
 *    for the longest sub_string (starting from the end) of the       *
 *    object name provided as input.  If it finds no match for the    *
 *    object name, it returns NULL.  If it finds a match, it returns  *
 *    the object pds_class as a character string.                         *
 *$External_References                                                *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    This routine allocates memory for its return value which must   *
 *    be freed elsewhere.                                             *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.1   October 21, 1991                                          *
 *$Change_History                                                     *
 *    MDD   05-09-91   Original generation.                           *
 *    MDD   10-21-91   Fixed malloc, free, and sys_exit_system calls  *
 **********************************************************************/

char *dd_get_object_class (dd_name)

char *dd_name;
{
   LOGICAL found = FALSE;
   char temp_str [PDS_MAXLINE + 1];
   char *temp_ptr = NULL;
   char *object_class = NULL;

/** BEGIN **/

   /*--------------------------------------------------------------*/
   /** Remove digits and underscores from the end of the object   **/
   /**    name                                                    **/
   /*--------------------------------------------------------------*/

   strcpy (temp_str, dd_name);
   temp_ptr = String_End(temp_str);
   while (temp_ptr >= (char *) temp_str && (isdigit (*temp_ptr) || *temp_ptr == '_'))
      temp_ptr--;
   *(++temp_ptr) = EOS;

   /*--------------------------------------------------------------*/
   /** WHILE there are more words in the object name DO           **/
   /**    check to see if the name exists in the data dictionary  **/
   /**    IF it doesn't, delete the first word from the name      **/
   /*--------------------------------------------------------------*/

   temp_ptr = temp_str;
   while (temp_ptr != NULL && !found)
   {
      found = dd_name_exists (temp_ptr, "OBJECT_DEFINITION");
      if (!found)
      {
         temp_ptr = (char *) strchr (temp_ptr, '_');
         if (temp_ptr != NULL) temp_ptr++;
      }
   }
   /*--------------------------------------------------------------*/
   /** ENDWHILE there are more words...                           **/
   /** IF a match was found in the data dictionary THEN           **/
   /**    copy the matching part of the name to the return value  **/
   /*--------------------------------------------------------------*/

   if (found)
   {
      Malloc_String(object_class, (int) String_Size(temp_ptr));
      strcpy (object_class, temp_ptr);
   }
   /*--------------------------------------------------------------*/
   /** ENDIF                                                      **/
   /*--------------------------------------------------------------*/

   return (object_class);

/** END dd_get_object_class **/
}

/**********************************************************************
 *$Component                                                          *
 *    LOGICAL dd_get_object_definition (dd_name, dd_def_action)       *
 *$Abstract                                                           *
 *    Gets an object definition and all its subdefinitions            *
 *$Keywords                                                           *
 *    DDACCESS                                                        *
 *$Inputs                                                             *
 *    dd_name:                                                        *
 *        The dd_name variable is a string containing the name of a   *
 *        DD object or element.                                       *
 *    dd_def_action:                                                  *
 *        The dd_def_action variable  is a flag indicating the fate   *
 *        of the DD definitions residing in memory, e.g., DD_SAVE_DEFS*
 *        or DD_REPLACE_DEFS.                                         *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure.       *
 *$Detailed_Description                                               *
 *    The dd_get_object_definition routine gets the definition of data*
 *    dictionary object dd_name from the data dictionary into memory. *
 *    It then gets all the applicable element definitions for the     *
 *    object. If the dd_def_action flag is DD_REPLACE_DEFS, then all  *
 *    definitions currently residing in memory are destroyed before   *
 *    the new definitions are read.                                   *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    definition_root         ddglob.h                 update         *
 *    dd_object_type          ddglob.h                 update         *
 *$Error_Handling                                                     *
 *    If the definition of the top level OBJECT cannot be found,      *
 *    then this routine returns FALSE, otherwise it returns TRUE.     *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.2   April 13, 1992                                            *
 *$Change_History                                                     *
 *    MDD   04-04-91   Original generation.                           *
 *    MDD   06-13-91   Removed fetching of subobjects and added check *
 *                     to see if the object is already in memory      *
 *    MDD   04-13-92   Removed out of date code for fetching members. *
 **********************************************************************/

LOGICAL dd_get_object_definition (dd_name, dd_def_action)

char *dd_name;
int dd_def_action;
{
   LOGICAL found = FALSE;
   int status = PDS_ERROR;

/** BEGIN **/
   dd_object_type = DD_SPECIFIC_OBJECT;

   /*----------------------------------------------------------------*/
   /** check to see if the object is already in memory              **/
   /*----------------------------------------------------------------*/

   if (dd_name != NULL)
   {
      lab_find_object (definition_root, "OBJECT_DEFINITION",
                       dd_name, 1, &status);
      if (status == PDS_SUCCESS) found = TRUE;
   }
   /*----------------------------------------------------------------*/
   /** IF the old definitions need to be replaced THEN              **/
   /**    remove the definitions from memory                        **/
   /** ENDIF                                                        **/
   /*----------------------------------------------------------------*/

   if (!found && dd_def_action == DD_REPLACE_DEFS)
   {
      dd_cleanup_defs ();
   }

   /*----------------------------------------------------------------*/
   /** IF the definition of the object is not already in memory     **/
   /**    and can be fetched THEN                                   **/
   /*----------------------------------------------------------------*/
   
#ifndef LV_DDICT
   if (!found && dd_get_definition (dd_name, "OBJECT_DEFINITION", 
                                    DD_SAVE_DEFS))
#else
   if (!found && dd_get_definition (NULL, dd_name, "OBJECT_DEFINITION", 
                                    DD_SAVE_DEFS))
#endif

      found = TRUE;

   /*----------------------------------------------------------------*/
   /** ENDIF the definition of the object can be fetched...         **/
   /** RETURN TRUE if all the objects (not elements) were found     **/
   /*----------------------------------------------------------------*/


   return (found);

/** END dd_get_object_definition **/
}


/**********************************************************************
 *$Component                                                          *
 *    LOGICAL dd_get_length_range (dd_name, dd_minimum_len,           *
 *                                 dd_maximum_len)                    *
 *$Abstract                                                           *
 *    Gets the maximum and minimum lengths of an element.             *
 *$Keywords                                                           *
 *    DDACCESS                                                        *
 *$Inputs                                                             *
 *    dd_name:                                                        *
 *        The dd_name variable is a string containing the name of a   *
 *        DD object or element.                                       *
 *$Outputs                                                            *
 *    dd_minimum_len:                                                 *
 *        The dd_minimum_len is an integer containing the minimum     *
 *        length of a character type data element.                    *
 *    dd_maximum_len:                                                 *
 *        The dd_maximum_len is an integer containing the maximum     *
 *        length of a character type data element.                    *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure.       *
 *$Detailed_Description                                               *
 *    The dd_get_length_range routine gets the maximum and minimum    *
 *    lengths of a character type data element. If the minimum and    *
 *    maximum are the same, then they represent the required length   *
 *    of the element. If no mimimum is found, then the value is set   *
 *    to PDS_DD_NO_MINIMUM. If no maximum is found, then the value    *
 *    is set to PDS_DD_NO_MAXIMUM.                                    *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    definition_root         ddglob.h                 update         *
 *$Error_Handling                                                     *
 *    If the outputs are set to PDS_DD_NO_MINIMUM and                 *
 *    PDS_DD_NO_MAXIMUM respectively, the FALSE is returned.          *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.1   October 21, 1991                                          *
 *$Change_History                                                     *
 *    MDD   04-05-91   Original generation.                           *
 *    MDD   10-21-91   Fixed free calls                               *
 **********************************************************************/

LOGICAL dd_get_length_range (dd_name, dd_minimum_len, dd_maximum_len)

char *dd_name;
long *dd_minimum_len;
long *dd_maximum_len;
{
   char *minimum_string = NULL;
   char *maximum_string = NULL;
   int label_status = PDS_ERROR;

/** BEGIN **/
   /*----------------------------------------------------------------*/
   /** get the minimum length of the element passed in              **/
   /*----------------------------------------------------------------*/

   minimum_string = lab_get_value (definition_root, "ELEMENT_DEFINITION", 
                                      dd_name, 1, "MINIMUM_LENGTH", 1, 1, 
                                         FALSE, &label_status);

   /*----------------------------------------------------------------*/
   /** IF the minimum length could not be obtained or it is NULL or **/
   /**    N/A or UNK THEN                                           **/
   /**    set the minimum length to PDS_DD_NO_MINIMUM               **/
   /** ELSE                                                         **/
   /**    copy the value obtained to the minimum length             **/
   /** ENDIF                                                        **/
   /*----------------------------------------------------------------*/

   if (label_status != PDS_SUCCESS ||
          minimum_string == NULL || 
             (label_status == PDS_SUCCESS && 
                   IsNull(minimum_string)))
   {
      *dd_minimum_len = PDS_DD_NO_MINIMUM;
   }
   else
   {
      sscanf (minimum_string, " %ld ", dd_minimum_len);
   }
   Lemme_Go(minimum_string);

   /*----------------------------------------------------------------*/
   /** get the maximum length of the element passed in              **/
   /*----------------------------------------------------------------*/

   maximum_string = lab_get_value (definition_root, "ELEMENT_DEFINITION", 
                                      dd_name, 1, "MAXIMUM_LENGTH", 1, 
                                         1, FALSE, &label_status);

   /*----------------------------------------------------------------*/
   /** IF the maximum length could not be obtained or it is NULL or **/
   /**    N/A or UNK THEN                                           **/
   /**    set the maximum length to PDS_DD_NO_MAXIMUM               **/
   /** ELSE                                                         **/
   /**    copy the value obtained to the maximum length             **/
   /** ENDIF                                                        **/
   /*----------------------------------------------------------------*/

   if (label_status != PDS_SUCCESS ||
          maximum_string == NULL ||
             (label_status == PDS_SUCCESS && 
                  IsNull(maximum_string)))
   {
      *dd_maximum_len = PDS_DD_NO_MAXIMUM;
   }
   else
   {
      sscanf (maximum_string, " %ld ", dd_maximum_len);
   }
   Lemme_Go(maximum_string);

   /*----------------------------------------------------------------*/
   /** IF both values were not found THEN return FALSE              **/
   /*----------------------------------------------------------------*/

   return (!(*dd_minimum_len == PDS_DD_NO_MINIMUM &&
	       *dd_maximum_len == PDS_DD_NO_MAXIMUM));

/** END dd_get_length_range **/
}


/**********************************************************************
 *$Component                                                          *
 *   STRING_LIST *dd_get_optional_members (dd_name)                   *
 *$Abstract                                                           *
 *    Gets the list of optional members of an object                  *
 *$Keywords                                                           *
 *    DDACCESS                                                        *
 *$Inputs                                                             *
 *    dd_name:                                                        *
 *        The dd_name variable is a string containing the name of a   *
 *        DD object or element.                                       *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    member_list:                                                    *
 *        The member_list structure is a pointer to a STRING_LIST     *
 *        which contains the names of data elements which are         *
 *        members of some object.                                     *
 *$Detailed_Description                                               *
 *    The dd_get_optional_members routine gets the optional member    *
 *    set for object dd_name and returns it in a STRING_LIST.  If     *
 *    the set is "N/A" or an error occurs, this routine returns NULL. *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    definition_root         ddglob.h                 update         *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.0   April 5, 1991                                             *
 *$Change_History                                                     *
 *    MDD   04-05-91   Original generation.                           *
 **********************************************************************/

STRING_LIST *dd_get_optional_members (dd_name)

char *dd_name;

{
   int label_status = PDS_ERROR;

   STRING_LIST *member_list = NULL;

/** BEGIN **/
   /*-----------------------------------------------------------------*/
   /** get the optional elements for object dd_name into a list      **/
   /*-----------------------------------------------------------------*/

   member_list = lab_get_all_values (definition_root, "OBJECT_DEFINITION", 
                                        dd_name, 1, "OPTIONAL_ELEMENT_SET", 
                                           1, FALSE, &label_status);

   /*-----------------------------------------------------------------*/
   /** IF there was an error THEN                                    **/
   /**    deallocate the list and set it to NULL                     **/
   /*-----------------------------------------------------------------*/

   if (label_status != PDS_SUCCESS)
   {
      member_list = util_deallocate_string_list (member_list);
   }
   /*-----------------------------------------------------------------*/
   /** ELSE IF the only value on the list is N/A or UNK THEN         **/
   /**    deallocate the list and set it to NULL                     **/
   /*-----------------------------------------------------------------*/

   else if (member_list != NULL && 
              IsNull(member_list -> text))
   {
      if (member_list -> next == NULL)
         member_list = util_deallocate_string_list (member_list);
   }

   /*-----------------------------------------------------------------*/
   /** ENDIF there was an error...                                   **/
   /** RETURN the list of members                                    **/
   /*-----------------------------------------------------------------*/

   return (member_list);

/** END dd_get_optional_members **/
}

/**********************************************************************
 *$Component                                                          *
 *   STRING_LIST *dd_get_optional_objects (dd_name)                   *
 *$Abstract                                                           *
 *    Gets the list of optional subobjects of an object               *
 *$Keywords                                                           *
 *    DDACCESS                                                        *
 *$Inputs                                                             *
 *    dd_name:                                                        *
 *        The dd_name variable is a string containing the name of a   *
 *        DD object or element.                                       *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    object_list:                                                    *
 *        The object_list structure is a pointer to a STRING_LIST     *
 *        which contains the names of data dictionary objects.        *
 *$Detailed_Description                                               *
 *    The dd_get_optional_objects routine gets the optional object    *
 *    set for object dd_name and returns it in a STRING_LIST.  If     *
 *    the set is "N/A" or an error occurs, this routine returns NULL. *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    definition_root         ddglob.h                 update         *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.0   April 5, 1991                                             *
 *$Change_History                                                     *
 *    MDD   04-05-91   Original generation.                           *
 **********************************************************************/

STRING_LIST *dd_get_optional_objects (dd_name)

char *dd_name;

{
   int label_status = PDS_ERROR;

   STRING_LIST *object_list = NULL;

/** BEGIN **/
   /*-----------------------------------------------------------------*/
   /** get the optional subobjects for object dd_name into a list    **/
   /*-----------------------------------------------------------------*/

   object_list = lab_get_all_values (definition_root, "OBJECT_DEFINITION", 
                                        dd_name, 1, "OPTIONAL_OBJECT_SET", 
                                            1, FALSE, &label_status);

   /*-----------------------------------------------------------------*/
   /** IF there was an error THEN                                    **/
   /**    deallocate the list and set it to NULL                     **/
   /*-----------------------------------------------------------------*/

   if (label_status != PDS_SUCCESS)
   {
      object_list = util_deallocate_string_list (object_list);
   }
   /*-----------------------------------------------------------------*/
   /** ELSE IF the only value on the list is N/A or UNK THEN         **/
   /**    deallocate the list and set it to NULL                     **/
   /*-----------------------------------------------------------------*/

   else if (object_list != NULL && 
              IsNull(object_list -> text))
   {
      if (object_list -> next == NULL)
         object_list = util_deallocate_string_list (object_list);
   }

   /*-----------------------------------------------------------------*/
   /** ENDIF there was an error ...                                  **/
   /** RETURN the list of objects                                    **/
   /*-----------------------------------------------------------------*/

   return (object_list);

/** END dd_get_optional_objects **/
}



/**********************************************************************
 *$Component                                                          *
 *   STRING_LIST *dd_get_required_members (dd_name)                   *
 *$Abstract                                                           *
 *    Gets the list of required members of an object                  *
 *$Keywords                                                           *
 *    DDACCESS                                                        *
 *$Inputs                                                             *
 *    dd_name:                                                        *
 *        The dd_name variable is a string containing the name of a   *
 *        DD object or element.                                       *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    member_list:                                                    *
 *        The member_list structure is a pointer to a STRING_LIST     *
 *        which contains the names of data elements which are         *
 *        members of some object.                                     *
 *$Detailed_Description                                               *
 *    The dd_get_required_members routine gets the optional member    *
 *    set for object dd_name and returns it in a STRING_LIST.  If     *
 *    the set is "N/A" or an error occurs, this routine returns NULL. *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    definition_root         ddglob.h                 update         *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.0   April 5, 1991                                             *
 *$Change_History                                                     *
 *    MDD   04-05-91   Original generation.                           *
 **********************************************************************/

STRING_LIST *dd_get_required_members (dd_name)

char *dd_name;

{
   int label_status = PDS_ERROR;

   STRING_LIST *member_list = NULL;

/** BEGIN **/
   /*-----------------------------------------------------------------*/
   /** get the required members for object dd_name into a list       **/
   /*-----------------------------------------------------------------*/

   member_list = lab_get_all_values (definition_root, "OBJECT_DEFINITION", 
                                        dd_name, 1, "REQUIRED_ELEMENT_SET", 
                                           1, FALSE, &label_status);

   /*-----------------------------------------------------------------*/
   /** IF there was an error THEN                                    **/
   /**    deallocate the list and set it to NULL                     **/
   /*-----------------------------------------------------------------*/

   if (label_status != PDS_SUCCESS)
   {
      member_list = util_deallocate_string_list (member_list);
   }
   /*-----------------------------------------------------------------*/
   /** ELSE IF the only value on the list is N/A or UNK THEN         **/
   /**    deallocate the list and set it to NULL                     **/
   /*-----------------------------------------------------------------*/

   else if (member_list != NULL && 
              IsNull(member_list -> text))
   {
      if (member_list -> next == NULL)
         member_list = util_deallocate_string_list (member_list);
   }

   /*-----------------------------------------------------------------*/
   /** ENDIF there was an error...                                   **/
   /** RETURN the list of members                                    **/
   /*-----------------------------------------------------------------*/

   return (member_list);

/** END dd_get_required_members **/
}

/**********************************************************************
 *$Component                                                          *
 *   STRING_LIST *dd_get_required_objects (dd_name)                   *
 *$Abstract                                                           *
 *    Gets the list of required subobjects of an object               *
 *$Keywords                                                           *
 *    DDACCESS                                                        *
 *$Inputs                                                             *
 *    dd_name:                                                        *
 *        The dd_name variable is a string containing the name of a   *
 *        DD object or element.                                       *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    object_list:                                                    *
 *        The object_list structure is a pointer to a STRING_LIST     *
 *        which contains the names of data dictionary objects.        *
 *$Detailed_Description                                               *
 *    The dd_get_required_objects routine gets the reuired object     *
 *    set for object dd_name and returns it in a STRING_LIST.  If     *
 *    the set is "N/A" or an error occurs, this routine returns NULL. *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    definition_root         ddglob.h                 update         *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.0   April 5, 1991                                             *
 *$Change_History                                                     *
 *    MDD   04-05-91   Original generation.                           *
 **********************************************************************/

STRING_LIST *dd_get_required_objects (dd_name)

char *dd_name;

{
   int label_status = PDS_ERROR;

   STRING_LIST *object_list = NULL;

/** BEGIN **/
   /*-----------------------------------------------------------------*/
   /** get the required subobjects for object dd_name into a list    **/
   /*-----------------------------------------------------------------*/

   object_list = lab_get_all_values (definition_root, "OBJECT_DEFINITION", 
                                        dd_name, 1, "REQUIRED_OBJECT_SET", 
                                           1, FALSE, &label_status);

   /*-----------------------------------------------------------------*/
   /** IF there was an error THEN                                    **/
   /**    deallocate the list and set it to NULL                     **/
   /*-----------------------------------------------------------------*/

   if (label_status != PDS_SUCCESS)
   {
      object_list = util_deallocate_string_list (object_list);
   }
   /*-----------------------------------------------------------------*/
   /** ELSE IF the only value on the list is N/A or UNK THEN         **/
   /**    deallocate the list and set it to NULL                     **/
   /*-----------------------------------------------------------------*/

   else if (object_list != NULL && 
              IsNull(object_list -> text))
   {
      if (object_list -> next == NULL)
         object_list = util_deallocate_string_list (object_list);
   }

   /*-----------------------------------------------------------------*/
   /** ENDIF there was an error ...                                  **/
   /** RETURN the list of objects                                    **/
   /*-----------------------------------------------------------------*/

   return (object_list);

/** END dd_get_required_objects **/
}


/**********************************************************************
 *$Component                                                          *
 *    char *dd_get_standard_value_type (dd_name)                      *
 *$Abstract                                                           *
 *    Gets the standard value type of an element from the DD.         *
 *$Keywords                                                           *
 *    DDACCESS                                                        *
 *$Inputs                                                             *
 *    dd_name:                                                        *
 *        The dd_name variable is a string containing the name of a   *
 *        DD object or element.                                       *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    dd_standard_val_type:                                           *
 *        The dd_standard_val_type variable is a string containing    *
 *        the standard value type of an element in the data           *
 *        dictionary: e.g., STATIC, DYNAMIC, SUGGESTED, etc.          *
 *$Detailed_Description                                               *
 *    The dd_get_standard_value_type routine gets the standard value  *
 *    type of an element from the data dictionary definitions         *
 *    residing in memory, if a matching definition is found.          *
 *    If the standard value type is not found, or turns out to be     *
 *    N/A, UNK, or NULL, then the string UNKNOWN is returned.         *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    definition_root         ddglob.h                 read           *
 *$Side_Effects                                                       *
 *    This routine returns a pointer to memory which must be freed    *
 *    elsewhere.                                                      *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.0   January 24, 1992                                          *
 *$Change_History                                                     *
 *    MDD   01-24-92   Original code                                  *
 **********************************************************************/

char *dd_get_standard_value_type (dd_name)

char *dd_name;

{
   int label_status = PDS_ERROR;
   char *standard_val_type = NULL;

/** BEGIN **/

   /*-----------------------------------------------------------------*/
   /** get the standard value type of the given element              **/
   /*-----------------------------------------------------------------*/

   standard_val_type = lab_get_value (definition_root, "ELEMENT_DEFINITION",
                                         dd_name, 1, "STANDARD_VALUE_TYPE", 1,
                                         1, FALSE, &label_status);

   /*-----------------------------------------------------------------*/
   /** IF the value was NULL or UNK or N/A THEN                      **/
   /**    set the return value to UNKNOWN                            **/
   /** ENDIF                                                         **/
   /*-----------------------------------------------------------------*/

   if (label_status != PDS_SUCCESS || standard_val_type == NULL)
   {
      Lemme_Go(standard_val_type);
      Malloc_String(standard_val_type, (int) String_Size("UNKNOWN"));
      strcpy (standard_val_type, "UNKNOWN");
   }
   return (standard_val_type);

/** END dd_get_standard_value_type **/
}


/**********************************************************************
 *$Component                                                          *
 *   STRING_LIST *dd_get_standard_values (dd_name)                    *
 *$Abstract                                                           *
 *    Gets the list of standard values for an element                 *
 *$Keywords                                                           *
 *    DDACCESS                                                        *
 *$Inputs                                                             *
 *    dd_name:                                                        *
 *        The dd_name variable is a string containing the name of a   *
 *        DD object or element.                                       *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    value_list:                                                     *
 *        The value_list structure is a pointer to a STRING_LIST      *
 *        which contains a list of values of or applicable to a data  *
 *        element.                                                    *
 *$Detailed_Description                                               *
 *    The dd_get_standard_values routine gets the standard value      *
 *    set for element dd_name and returns it in a STRING_LIST.  If    *
 *    the set is "N/A" or an error occurs, this routine returns NULL. *
 *    If the given element does not have a STATIC, DYNAMIC, or        *
 *    suggested standard value type, then this routine returns NULL.  *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    definition_root         ddglob.h                 update         *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.2  October 22, 1992                                           *
 *$Change_History                                                     *
 *    MDD   04-05-91   Original generation.                           *
 *    DPB   05-30-91   Changed the way standard values are fetched.   *
 *    MDD   01-24-92   Modified to use dd_get_standard_value_type     *
 *    MDD   10-22-92   Moved free statement for standard value type   *
 *    DWS   02-16-00   Added FORMATION to standard val type choices   *
 *    MDC   06-19-06   Modified routine to no longer look for         *
 *                     SUGGESTED value types.                         *
 **********************************************************************/

STRING_LIST *dd_get_standard_values (dd_name)

char *dd_name;

{
   PARAMETER parameter_ptr = {NULL};
   int label_status = PDS_ERROR;
   STRING_LIST *value_list = NULL;
   char *standard_val_type;

/** BEGIN **/

   /*-----------------------------------------------------------------*/
   /** get the standard value type of the element                    **/
   /*-----------------------------------------------------------------*/

   standard_val_type = dd_get_standard_value_type (dd_name);

   /*-----------------------------------------------------------------*/
   /** IF the standard value type is STATIC, DYNAMIC, or SUGGESTED   **/
   /**    THEN                                                       **/
   /*-----------------------------------------------------------------*/

   /* 06-19-06 MDC - Comment out looking for SUGGESTED value types.   */

   /* 01-25-06 MDC - Added check for SUGGESTED value type. Comments says
      to look for it, but for some reason, it wasn't originally in the 
	  code */
   if ((strcmp (standard_val_type, "STATIC") == 0) ||
             (strcmp (standard_val_type, "DYNAMIC") == 0) ||
             (strcmp (standard_val_type, "FORMATION") == 0))
   {
      /*--------------------------------------------------------------*/
      /** get the standard values parameter for the element          **/
      /*--------------------------------------------------------------*/

      parameter_ptr = lab_find_parameter (definition_root, "ELEMENT_DEFINITION", 
                                           dd_name, 1, "STANDARD_VALUE_SET", 1,
                                           &label_status);

      /*--------------------------------------------------------------*/
      /** IF we got the parameter THEN                               **/
      /**     get the list                                           **/
      /*--------------------------------------------------------------*/

      if (label_status == PDS_SUCCESS)
      {
         value_list = lu_fetch_all_values (parameter_ptr, TRUE, FALSE);

      /*--------------------------------------------------------------*/
      /**      IF the only value on the list is N/A or UNK THEN      **/
      /**          deallocate the list and set it to NULL            **/
      /**      ENDIF                                                 **/
      /*--------------------------------------------------------------*/

         if ((value_list != NULL) && (IsNull(value_list -> text)))
         {
            if (value_list -> next == NULL)
               value_list = util_deallocate_string_list (value_list);
         }

      /*--------------------------------------------------------------*/
      /** ENDIF there was an error ...                               **/
      /*--------------------------------------------------------------*/
      }
   }
   /*-----------------------------------------------------------------*/
   /** ENDIF the standard value type is STATIC...                    **/
   /** RETURN the standard value list                                **/
   /*-----------------------------------------------------------------*/

   Lemme_Go(standard_val_type);
   return (value_list);

/** END dd_get_standard_values **/
}

/**********************************************************************
 *$Component                                                          *
 *    LOGICAL dd_get_value_range (dd_name, dd_minimum_val,            *
 *                                dd_maximum_val)                     *
 *$Abstract                                                           *
 *    Gets the maximum and minimum value of an element.               *
 *$Keywords                                                           *
 *    DDACCESS                                                        *
 *$Inputs                                                             *
 *    dd_name:                                                        *
 *        The dd_name variable is a string containing the name of a   *
 *        DD object or element.                                       *
 *$Outputs                                                            *
 *    dd_minimum_val:                                                 *
 *        The dd_minimum_val is a real number containing the minimum  *
 *        value of a numeric type data element.                       *
 *    dd_maximum_val:                                                 *
 *        The dd_maximum_val is a real number containing the maximum  *
 *        value of a numeric type data element.                       *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure.       *
 *$Detailed_Description                                               *
 *    The dd_get_value_range routine gets the maximum and minimum     *
 *    values of a numeric type data element. If the minimum and       *
 *    maximum are the same, then they represent the required value    *
 *    of the element. If no mimimum is found, then the value is set   *
 *    to to PDS_DD_NO_MINIMUM. If no maximum is found, then the value *
 *    is set to PDS_DD_NO_MAXIMUM.                                    *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    definition_root         ddglob.h                 update         *
 *$Error_Handling                                                     *
 *    If the outputs are set to PDS_DD_NO_MINIMUM and                 *
 *    PDS_DD_NO_MAXIMUM respectively, the FALSE is returned.          *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.2  September 16, 1992                                         *
 *$Change_History                                                     *
 *    MDD   04-05-91   Original generation.                           *
 *    MDD   10-21-91   Fixed free calls                               *
 *    MDD   09-16-92   Added search for VALID_MIN and VALID_MAX.      *
 **********************************************************************/

LOGICAL dd_get_value_range (dd_name, dd_minimum_val, dd_maximum_val)

char *dd_name;
double *dd_minimum_val;
double *dd_maximum_val;

{
   char *minimum_string = NULL;
   char *maximum_string = NULL;
   int label_status = PDS_ERROR;

/** BEGIN **/

   /*----------------------------------------------------------------*/
   /** get the minimum value  of the element passed in              **/
   /*----------------------------------------------------------------*/

   minimum_string = lab_get_value (definition_root, "ELEMENT_DEFINITION", 
                                      dd_name, 1, "VALID_MINIMUM", 1, 1, FALSE, 
                                         &label_status);
   if (label_status != PDS_SUCCESS)
   {
      minimum_string = lab_get_value (definition_root, "ELEMENT_DEFINITION", 
                                         dd_name, 1, "MINIMUM", 1, 1, FALSE, 
                                            &label_status);
   }

   /*----------------------------------------------------------------*/
   /** IF the minimum value  could not be obtained or it is NULL or **/
   /**    N/A or UNK THEN                                           **/
   /**    set the minimum value  to PDS_DD_NO_MINIMUM               **/
   /** ELSE                                                         **/
   /**    copy the value obtained to the minimum value              **/
   /** ENDIF                                                        **/
   /*----------------------------------------------------------------*/

   if (label_status != PDS_SUCCESS ||
          minimum_string == NULL || 
             (label_status == PDS_SUCCESS && 
                IsNull(minimum_string)))
   {
      *dd_minimum_val = (double) PDS_DD_NO_MINIMUM;
   }
   else
   {    
      sscanf (minimum_string, "%le", dd_minimum_val);
   }
   Lemme_Go(minimum_string);

   /*----------------------------------------------------------------*/
   /** get the maximum value of the element passed in               **/
   /*----------------------------------------------------------------*/

   
   maximum_string = lab_get_value (definition_root, "ELEMENT_DEFINITION", 
                                      dd_name, 1, "VALID_MAXIMUM", 1, 1, FALSE, 
                                          &label_status);

   if (label_status != PDS_SUCCESS)
   {
      maximum_string = lab_get_value (definition_root, "ELEMENT_DEFINITION", 
                                         dd_name, 1, "MAXIMUM", 1, 1, FALSE, 
                                             &label_status);
   }

   /*----------------------------------------------------------------*/
   /** IF the maximum value could not be obtained or it is NULL or  **/
   /**    N/A or UNK THEN                                           **/
   /**    set the maximum value to PDS_DD_NO_MAXIMUM                **/
   /** ELSE                                                         **/
   /**    copy the value obtained to the maximum value              **/
   /** ENDIF                                                        **/
   /*----------------------------------------------------------------*/

   if (label_status != PDS_SUCCESS ||
         maximum_string == NULL || 
             (label_status == PDS_SUCCESS && 
                   IsNull(maximum_string)))
   {
      *dd_maximum_val = (double) PDS_DD_NO_MAXIMUM;
   }
   else
   { 
      sscanf (maximum_string, "%le", dd_maximum_val);
   }
   Lemme_Go(maximum_string);

   /*----------------------------------------------------------------*/
   /** IF both values were not found THEN return FALSE              **/
   /*----------------------------------------------------------------*/

   return (!(*dd_maximum_val == (double) PDS_DD_NO_MAXIMUM &&
	       *dd_minimum_val == (double) PDS_DD_NO_MINIMUM));

/** END dd_get_value_range **/
}



/**********************************************************************
 *$Component                                                          *
 *    LOGICAL dd_init (init_index_name)                               *
 *$Abstract                                                           *
 *    Initializes the data dictionary for access.                     *
 *$Keywords                                                           *
 *    DDACCESS                                                        *
 *$Inputs                                                             *
 *    init_index_name:                                                *
 *       The init_index_name variable is a character string containing*
 *       the name of a DD index file.                                 *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure.       *
 *$Detailed_Description                                               *
 *    The dd_init routine determines if the data dictionary file and  *
 *    its index exist.  It initializes all the globals used by        *
 *    the DD routines and initializes the DD definition tree.         *
 *    This routine must be called before any other dd access routines *
 *    are called.                                                     *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    begin_index             ddglob.h                 write          *
 *    end_index               ddglob.h                 write          *
 *    index_rec_len           ddglob.h                 write          *
 *    dd_index_name           ddglob.h                 write          *
 *    dd_file_name            ddglob.h                 write          *
 *    dd_alias_offset         ddglob.h                 write          *
 *$Error_Handling                                                     *
 *    If the data dictionary files cannot be opened or appear to be   *
 *    incorrect, then this routine returns FALSE.                     *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.5   October 20, 1992                                          *
 *$Change_History                                                     *
 *    MDD   04-05-91   Original generation.                           *
 *    MDD   06-17-91   Added initialization of dd_alias_offset        *
 *    DPB   07-09-91   Added code to append the path name of the      *
 *                     dd index onto the name of the dd file.         *
 *    MDD   10-21-91   Fixed free calls                               *
 *    MDD   01-13-92   Changed so that the path name of the index file*
 *                     will only be added to the dd file name if it   *
 *                     does not already have a path.                  *
 *    MDD   10-20-92   Corrected memory leak - index root             *
 *    MDC   11-01-06   Made a fix to correctly calculate the beginning*
 *                     index so that it is pointing to the first      *
 *                     keyword listed in the dictionary file.         *
 **********************************************************************/

LOGICAL dd_init (init_index_name)

char *init_index_name;

{
   AGGREGATE root = NULL;
   int label_status = PDS_ERROR;
   char *temp_ptr = NULL;
   char *temp_path = NULL;
   char *dd_temp_path = NULL;
   long label_records;
   FILE *test_file = NULL;
   LOGICAL success = TRUE;
   int status;

/** BEGIN **/
   /*-----------------------------------------------------------------*/
   /** IF the label on the index file can be read THEN               **/
   /*-----------------------------------------------------------------*/
  
   root = lab_read_label_or_template (init_index_name);
   if (root != NULL)
   {
      /*-------------------------------------------------------------*/
      /** get the DD definition file name from the label            **/
      /** use it to initialize the global DD file name              **/
      /*-------------------------------------------------------------*/

      strcpy (dd_index_name, init_index_name);
      temp_ptr = lab_get_value (root, "ROOT", NULL, 0, "DD_FILE", 0, 
                                   1, FALSE, &label_status);
      if ((label_status == PDS_SUCCESS) && (temp_ptr != NULL))
      {
         temp_path = sys_get_path (dd_index_name);
         dd_temp_path = sys_get_path (temp_ptr);
         if (dd_temp_path != NULL || (dd_temp_path == NULL && temp_path == NULL))
         {
             sprintf (dd_file_name, "%s", temp_ptr);
             Lemme_Go(dd_temp_path);
	 }
         else
         {
             sprintf (dd_file_name, "%s%s", temp_path, temp_ptr);
             Lemme_Go(temp_path);
	 }
         Lemme_Go(temp_ptr);
      }
      else
      {
         success = FALSE;
      }

      /*-------------------------------------------------------------*/
      /** get the record length used for the index file             **/
      /** use it to initialize the global index record length       **/
      /*-------------------------------------------------------------*/

      temp_ptr = lab_get_value (root, "ROOT", NULL, 0, "RECORD_LENGTH", 0,
                                   1, FALSE, &label_status);
      if (label_status == PDS_SUCCESS)
      {
         sscanf (temp_ptr, " %ld ", &index_rec_len);
         Lemme_Go(temp_ptr);
      }
      else
      {
         success = FALSE;
      }

      /*-------------------------------------------------------------*/
      /** get the number of label records from the label            **/
      /*-------------------------------------------------------------*/

      temp_ptr = lab_get_value (root, "ROOT", NULL, 0, "LABEL_RECORDS", 0,
                                   1, FALSE, &label_status);
      if (label_status == PDS_SUCCESS)
      {
         sscanf (temp_ptr, " %ld ", &label_records);
         Lemme_Go(temp_ptr);
      }
      else
      {
         success = FALSE;
      }

      /*-------------------------------------------------------------*/
      /** get the alias offset from the label                       **/
      /*-------------------------------------------------------------*/

      temp_ptr = lab_get_value (root, "ROOT", NULL, 0, "ALIAS_LIST", 0,
                                   1, FALSE, &label_status);
      if (label_status == PDS_SUCCESS)
      {
         sscanf (temp_ptr, " %ld ", &dd_alias_offset);
         Lemme_Go(temp_ptr);
      }
      else
      {
         success = FALSE;
      }

      /*-------------------------------------------------------------*/
      /** IF any of these values weren't in the label THEN          **/
      /**   append an error message to the global list              **/
      /*-------------------------------------------------------------*/

      if (success == FALSE)
      {
         err_append_message (ERROR1, 
              "The label on the data dictionary index file is incorrect");
      }

      /*-------------------------------------------------------------*/
      /** ELSE                                                      **/
      /*-------------------------------------------------------------*/

      else
      {
         /*----------------------------------------------------------*/
         /** set the global offsets for the beginning and end of    **/
         /** the index                                              **/
         /*----------------------------------------------------------*/
         
		  /*11-01-06 MDC - Added a 1 to the label records value before calculating
		    the beginning index so that it is correctly pointing to the first 
			keyword listed in the dictionary file.
	      */
		  begin_index = (label_records + 1) * index_rec_len;
		  end_index = fio_size (dd_index_name) - index_rec_len;

         /*----------------------------------------------------------*/
         /** append an error message and set failure flag if the    **/
         /** DD definition file cannot be opened                    **/
         /*----------------------------------------------------------*/

	 if ((test_file = fopen (dd_file_name, "r")) != NULL)
	 {
	    fclose (test_file);
	 }
	 else
	 {
	    err_append_message (ERROR1, 
                 "The data dictionary definition file could not be opened");
	    success = FALSE;
         }
      }
      /*-------------------------------------------------------------*/
      /** ENDIF any of these values weren't in the label...         **/
      /*-------------------------------------------------------------*/
   }
   /*-----------------------------------------------------------------*/
   /** ELSE                                                          **/
   /**   the index file cannot be read so append an error            **/
   /*-----------------------------------------------------------------*/
  
   else
   {
      err_append_message (WARNING, 
           "The data dictionary index file is incorrect or missing");
      success = FALSE;
   }
   /*-----------------------------------------------------------------*/
   /** ENDIF the label on the index file can be read...              **/
   /** initialize the DD definitions in memory                       **/
   /*-----------------------------------------------------------------*/

   lab_remove_label (root, &status);
   dd_cleanup_defs ();
   return (success);

/** END dd_init **/
}


/**********************************************************************
 *$Component                                                          *
 *    LOGICAL dd_name_exists (dd_name, dd_type)                       *
 *$Abstract                                                           *
 *    Determines if a name exists in the data dictionary              *
 *$Keywords                                                           *
 *    DDACCESS                                                        *
 *$Inputs                                                             *
 *    dd_name:                                                        *
 *        The dd_name variable is a string containing the name of a   *
 *        DD object or element.                                       *
 *    dd_type:                                                        *
 *        The dd_type variable is a string containing the type of a   *
 *        DD definition, e.g., OBJECT_DEFINITION or                   *
 *        ELEMENT_DEFINITION.                                         *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure.       *
 *$Detailed_Description                                               *
 *    The dd_name_exists routine calls dd_search_index to find an     *
 *    element or object in the data dictionary. If it is found, then  *
 *    this routine returns TRUE. Otherwise, it returns FALSE.         *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.0   April 8, 1991                                             *
 *$Change_History                                                     *
 *    MDD   04-08-91   Original generation.                           *
 **********************************************************************/

LOGICAL dd_name_exists (dd_name, dd_type)

char *dd_name;
char *dd_type;

{
   long offset;
   long size;
   
   return (dd_search_index (dd_name, dd_type, &offset, &size));
}


/**********************************************************************
 *$Component                                                          *
 *    LOGICAL dd_search_index (dd_name, dd_type, dd_offset, dd_size)  *
 *$Abstract                                                           *
 *    Locates a data dictionary object in the DD index.               *
 *$Keywords                                                           *
 *    DDACCESS                                                        *
 *$Inputs                                                             *
 *    dd_name:                                                        *
 *        The dd_name variable is a string containing the name of a   *
 *        DD object or element.                                       *
 *    dd_type:                                                        *
 *        The dd_type variable is a string containing the type of a   *
 *        DD definition, e.g., OBJECT_DEFINITION or                   *
 *        ELEMENT_DEFINITION.                                         *
 *$Outputs                                                            *
 *    dd_offset:                                                      *
 *        The dd_offset variable is an integer representing the byte  *
 *        offset of a data dictionary definition in the data          *
 *        dictionary definition file.                                 *
 *    dd_size:                                                        *
 *        The dd_size   variable is an integer representing the       *
 *        size in bytes of a data dictionary definition in the data   *
 *        dictionary definition file.                                 *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure.       *
 *$Detailed_Description                                               *
 *    The dd_search_index routine searches the DD index for the       *
 *    data dictionary object with the given name and type and, if     *
 *    the object is found, returns the byte offset and size of the    *
 *    object in the DD definition file. If the type of the object is  *
 *    given as OBJECT_DEFINITION, then, using recursive calls, this   *
 *    routine searches first for a SPECIFIC_OBJECT_DEFINITION and     *
 *    then for a GENERIC_OBJECT_DEFINITION.  If the object is not     *
 *    found in the index, then this routine returns FALSE. This       *
 *    routine sets the global flag dd_object_type to                  *
 *    DD_SPECIFIC_OBJECT or DD_GENERIC_OBJECT, if it finds an object. *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    dd_index_name           ddglob.h                 read           *
 *    index_rec_len           ddglob.h                 read           *
 *    begin_index             ddglob.h                 read           *
 *    end_index               ddglob.h                 read           *
 *    dd_object_type          ddglob.h                 write          *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.0   April 3, 1991                                             *
 *$Change_History                                                     *
 *    MDD   04-03-91   Original generation.                           *
 *    MDC   04-11-06   Dynamically allocate memory so that we don't   *
 *                     ever have to worry about having enough space   *
 *                     to hold a keyword.                             *
 **********************************************************************/

LOGICAL dd_search_index (dd_name, dd_type, dd_offset, dd_size)

char *dd_name;
char *dd_type;
long *dd_offset;
long *dd_size;

{
   LOGICAL found = FALSE;
   FILE *dd_index = NULL;
   long lower;
   long upper;
   long mid;
   char input_line [PDS_MAXLINE]={0};
   long temp_size;
   long temp_offset;
/*   char temp_name [PDS_ELEMENT_LEN + 1]={0}; */
   char temp_type [PDS_DD_TYPE_LEN + 1]={0};
   int compare=0;
/*   char real_dd_name [PDS_ELEMENT_LEN + 1]={0}; */
   char *real_dd_name = NULL;
   char temp_name[PDS_MAXLINE]={0};  
  
/** BEGIN **/

   /*--------------------------------------------------------------------*/
   /** IF a name and type were passed in THEN                           **/
   /*--------------------------------------------------------------------*/

   if (dd_name != NULL && dd_type != NULL)
   {
      /*-----------------------------------------------------------------*/
      /** convert the DD name passed in to upper case                   **/
      /*-----------------------------------------------------------------*/

/*      strcpy (real_dd_name, dd_name); */
	  New_String(real_dd_name, dd_name);
      util_upper_case (real_dd_name);
      
      /*-----------------------------------------------------------------*/
      /** IF the DD type is OBJECT_DEFINITION THEN                      **/
      /**    invoke this routine again, first attempting to find a      **/
      /**    SPECIFIC object, and then attempting to find a GENERIC     **/
      /**    object                                                     **/
      /*-----------------------------------------------------------------*/
      
      if (strcmp (dd_type, "OBJECT_DEFINITION") == 0)
      {
          found = dd_search_index (real_dd_name, "SPECIFIC_OBJECT_DEFINITION",
				 dd_offset, dd_size);
		  if (!found)
		  {
              found = dd_search_index (real_dd_name, "GENERIC_OBJECT_DEFINITION",
		         		   dd_offset, dd_size);
			  if (found) dd_object_type = DD_GENERIC_OBJECT;
		  }
		  else
              dd_object_type = DD_SPECIFIC_OBJECT;
      }
      /*-----------------------------------------------------------------*/
      /** ELSE                                                          **/
      /*-----------------------------------------------------------------*/
	
      else
	  {
	  /*---------------------------------------------------------------*/
	  /** IF the index file can be opened THEN                        **/
	  /*---------------------------------------------------------------*/
	  
#ifdef VAX
		dd_index = fopen (dd_index_name, "r", "rfm=stmlf");
#else
		dd_index = fopen (dd_index_name, "r");
#endif
	  
		if (dd_index != NULL)
		{
			/*------------------------------------------------------------*/
			/** initialize variables for a binary search of the index    **/
			/*------------------------------------------------------------*/
	    
		  lower = begin_index;
		  upper = end_index;
	    
	    /*------------------------------------------------------------*/
	    /** WHILE we have more index and the DD name is not found DO **/
	    /*------------------------------------------------------------*/
	    
		while (lower <= upper && !found) 
	    {
	      
	      /*----------------------------------------------------------*/
	      /** get the next name for comparison from the index        **/
	      /*----------------------------------------------------------*/
	      
	      mid = (((upper/index_rec_len) + (lower/index_rec_len)) / 2) * 
	      index_rec_len;
	      fseek (dd_index, mid, 0);
	      if (!fgets (input_line, index_rec_len + 1, dd_index)) break;
	      sscanf (input_line, " %s %s %ld %ld ", 
		      temp_name, temp_type, &temp_offset, &temp_size);
	      compare = strcmp (temp_name, real_dd_name);
	      
	      /*----------------------------------------------------------*/
	      /** IF the index name is less than the desired one THEN    **/
	      /**    prepare to search the top half of the index         **/
	      /*----------------------------------------------------------*/
	      
	      if (compare < 0)
	      {
		      lower = mid + index_rec_len;
	      }
	      
	      /*----------------------------------------------------------*/
	      /** ELSE IF the index name is greater than the desired one **/
	      /**    THEN                                                **/
	      /**    prepare to search the bottom half of the index      **/
	      /*----------------------------------------------------------*/
	      
	      else if (compare > 0)
	      {
			 upper = mid - index_rec_len;
	      }
	      /*----------------------------------------------------------*/
	      /** ELSE                                                   **/
	      /*----------------------------------------------------------*/
	      
	      else
	      {
		/*--------------------------------------------------------*/
		/** We found the name, so compare the types and do the   **/
		/** same thing as above.                                 **/
		/** If the types are the same set the outputs and the    **/
		/** found flag.                                          **/
		/*--------------------------------------------------------*/
		
		compare = strcmp (temp_type, dd_type);
		if (compare < 0)
		{
		  lower = mid + index_rec_len;
		}
		else if (compare > 0)
		{
		  upper = mid - index_rec_len;
		}
		else
		{
		  found = TRUE;
		  *dd_offset = temp_offset;
		  *dd_size = temp_size;
		}
	      }
	      /*----------------------------------------------------------*/
	      /** ENDIF the index name is less...                        **/
	      /*----------------------------------------------------------*/
	    }
	    /*------------------------------------------------------------*/
	    /** ENDWHILE we have more index...                           **/
	    /*------------------------------------------------------------*/
	    
	    fclose (dd_index);
	  }
	  /*---------------------------------------------------------------*/
	  /** ENDIF the index file can be opened and...                   **/
	  /*---------------------------------------------------------------*/
	}
	/*-----------------------------------------------------------------*/
	/** ENDIF the DD type is OBJECT_DEFINITION...                     **/
	/*-----------------------------------------------------------------*/
   }
   /*--------------------------------------------------------------------*/
   /** ENDIF a name and type were passed in...                          **/
   /*--------------------------------------------------------------------*/
   Lemme_Go(real_dd_name);
   return (found);

/** END dd_search_index **/
}



/**********************************************************************
 *$Component                                                          *
 *    ERROR_LIST *dd_unalias (label_ptr)                              *
 *$Abstract                                                           *
 *    Dealiases a PDS label                                           *
 *$Keywords                                                           *
 *    DDACCESS                                                        *
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
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    err_message_ptr:                                                *
 *        The err_message_ptr variable is a pointer to a list of the  *
 *        error messages.                                             *
 *$Detailed_Description                                               *
 *    The dd_unalias routine reads all object and element aliases     *
 *    from the data dictionary definition file.  It then scans the    *
 *    ODL tree given as input and replaces all object and element     *
 *    aliases it finds with the correct names.                        *
 *                                                                    *
 *    The aliases for objects are assumed to be aliases for object    *
 *    classes only.  This routine can replace only the ENTIRE pds_class   *
 *    name, but not part of it. In other words, this routine can      *
 *    replace the object pds_class alias TABLE_STRUCTURE with the real    *
 *    pds_class TABLE, but it will not convert EDR_TABLE_STRUCTURE to     *
 *    EDR_TABLE.                                                      *
 *                                                                    *
 *    The aliases for elements are assumed to be aliases for the      *
 *    entire keyword name.  Element aliases may apply to the element  *
 *    no matter how it is used, or they may apply only when the       *
 *    element is used in an object of a certain pds_class. In this case   *
 *    the pds_class of the object which contains the keywords will be     *
 *    determined before the replacement takes place, so an element    *
 *    alias  that applies inside of TABLE objects will be changed     *
 *    inside  both TABLE and (for example) EDR_TABLE objects.         *
 *                                                                    *
 *    This routine appends messages to the global error message list  *
 *    to indicate which element alias replacements were performed.    *
 *    A pointer to the location of these messages on the list is      *
 *    then returned.                                                  *
 *$Side_Effects                                                       *
 *    This routine frees and replaces the value in the "pds_class" field  *
 *    of each object.  PREVIOUS CLASS VALUES ARE DESTROYED.           *
 *$External_References                                                *
 *    Item                     Shared-Data           Access           *
 *    ------------------------------------------------------------    *
 *    pds_last_message         pdsglob.h             read             *
 *    dd_file_name             ddglob.h              read             *
 *    dd_alias_offset          ddglob.h              read             *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/ JPL                                            *
 *$Version_and_Date                                                   *
 *    2.0   October 26, 1992                                          *
 *$Change_history                                                     *
 *    MDD   06-21-91   Original generation.                           *
 *    MDD   10-21-91   Fixed malloc, free, and sys_exit_system calls  *
 *    MDD   10-26-92   Changed algorithm to classify all objects      *
 *                     first, in order to speed up the function       *
 *    DWS   08-23-01   Added initialization for alias_list to prevent *
 *                     program crash.                                 *
 *    MDC   03-05-05   Check for NULL in dd_file_name first before    *
 *                     calling fopen function to prevent program crash*
 *    MDC   09-27-06   Modified routine to search for aliases that are*
 *                     part of an object name.                        *
 *                     i.e. if MY_PARMS exists in a label, this       *
 *                       routine will now recognize that PARMS is an  *
 *                       alias to PARAMETERS according to the PSDD.   *
 *                       The object will change from MY_PARMS to      *
 *                       MY_PARAMETERS.                               *
 **********************************************************************/


ERROR_LIST *dd_unalias (label_ptr)

AGGREGATE label_ptr;

{
   LOGICAL first_time = TRUE;
   FILE *dd_file = NULL;
   AGGREGATE alias_root;
   STRING_LIST *alias_list;
   char alias [PDS_ELEMENT_LEN + 1]={0};
   char real_name [PDS_ELEMENT_LEN + 1]={0};
   char object_name [PDS_ELEMENT_LEN + 1]={0};
   AGGREGATE alias_object = {NULL};
   AGGREGATE current_object;
   PARAMETER parameter_ptr;
   STRING_LIST *temp_list = NULL;
   LOGICAL alias_used = FALSE;
   ERROR_LIST *alias_message = NULL;
   ERROR_LIST *end_message = NULL;
   char err_msg [PDS_MAXLINE + 1];
   char *temp_ptr = NULL;
   char *copied_name = NULL;
   LOGICAL found = FALSE;
/** BEGIN **/

   /*----------------------------------------------------------------------*/
   /** append the alias message header to the global message list and     **/
   /**    record the location of the messages                             **/
   /*----------------------------------------------------------------------*/
 
   err_append_message (CONTINUE, 
         "NOTE:    The following aliases were changed in this label:");
   alias_message = pds_last_message;
   err_append_message (CONTINUE, " ");
   end_message = pds_last_message;

   /*----------------------------------------------------------------------*/
   /** IF the data dictionary definition file can be opened THEN          **/
   /*----------------------------------------------------------------------*/

   /* 03-05-05 MDC - Check for NULL value to prevent program crash */
   if(strcmp(dd_file_name,"\0") != 0) dd_file = fopen (dd_file_name, "r");
   if (dd_file != NULL)
   {
      /*------------------------------------------------------------------*/
      /** read the alias object from the file onto a new ODL tree        **/
      /*------------------------------------------------------------------*/

      alias_root = lu_append_object (NULL, "ROOT");
      fseek (dd_file, dd_alias_offset, 0);
      ReadLabel (dd_file, alias_root);
      fclose (dd_file);

      /*------------------------------------------------------------------*/
      /** locate the alias list object                                   **/
      /** fetch all values of the object alias sequence parameter        **/
      /*------------------------------------------------------------------*/

	  alias_list = NULL;  /* added by DWS on 8/23/01                    */
						  /* program occassionally crashed by making it */
						  /* into the for loop below after failing the  */
						  /* next if statement.                         */

      alias_object = FindObject (alias_root, "ALIAS_LIST", NULL);
      if (alias_object != NULL)
         alias_list = lu_fetch_all_values (FindParameter (alias_object, 
                                               "OBJECT_ALIAS_SEQUENCE"), 
                                                  TRUE, FALSE);

      /*------------------------------------------------------------------*/
      /** LOOP through all the object alias pairs                        **/
      /*------------------------------------------------------------------*/

      for (temp_list = alias_list; temp_list != NULL; temp_list = temp_list -> next)
      {
         alias_used = FALSE;

         /*---------------------------------------------------------------*/
         /** IF we can copy the alias and real name from the alias pair  **/
         /**    string THEN                                              **/
         /*---------------------------------------------------------------*/

         if (sscanf (temp_list -> text, "%s %s", alias, real_name) == 2)
         {
 
            /*------------------------------------------------------------*/
            /** LOOP through all objects in the label                    **/
            /**    IF the current object name is the same as the current **/
            /**       alias THEN                                         **/
            /**       change the object name to the real name            **/
            /**       mark this object as having been changed            **/
            /**    ENDIF                                                 **/
            /*------------------------------------------------------------*/

            if (strcmp (alias, real_name) == 0) continue;
            for (current_object = label_ptr; 
                    current_object != NULL; 
                       current_object = NextObject (current_object))
            {

               if (first_time)
               {
                  Lemme_Go(current_object->pds_class)
                  current_object -> pds_class = dd_get_object_class 
                     (current_object -> name);
			   }
               if (!IsMarked(current_object -> name) &&
                        (*alias == *(current_object -> name)) 
                           && (strcmp (current_object -> name, alias) == 0))
               {
                  alias_used = TRUE;
                  Lemme_Go(current_object -> name);
                  Malloc_String(current_object -> name, (int) String_Size(real_name));
                  strcpy (current_object -> name, real_name);
				  current_object->pds_class = dd_get_object_class(current_object->name);
                  SetMark(current_object -> name);
               }
			   /* 09-27-06 MDC - If we haven't found an alias during the first pass, lets try
			      to see if an alias exists as part of the object name.

				  i.e. This routine will now replace EDR_TABLE_STRUCTURE with EDR_TABLE
				       (TABLE_STRUCTURE is an alias to TABLE in the PSDD)
			   */
			   if( (!IsMarked(current_object -> name)) && ((strchr(current_object -> name, '_')) != NULL) )
			   {
                   found = FALSE;
				   New_String(copied_name, current_object -> name);
				   /* Find the first occurrence of an underscore. If it is found, then...*/
				   for(temp_ptr = strchr(copied_name, '_') + 1; !found && (temp_ptr != NULL);) 
				   {
					   /* Check to see if this part of the name is an alias */
                       if((*alias == *(temp_ptr)) && (strcmp(temp_ptr,alias) == 0))
					   {
						   /* If so, replace the alias with its real name */
                           alias_used = TRUE;
						   found = TRUE;
						   *(temp_ptr) = EOS;
                           Lemme_Go(current_object -> name);
						   Malloc_String(current_object -> name, (int) (String_Size(copied_name) + String_Size(real_name)));
						   strcpy(current_object -> name, copied_name);
						   strcat(current_object -> name, real_name);
						   current_object->pds_class = dd_get_object_class(current_object->name);
						   SetMark(current_object->name);
					   }
					   if((temp_ptr = strchr(temp_ptr, '_')) != NULL)
						   ++temp_ptr;   
				   }
				   Lemme_Go(copied_name);
			   }
            }
            /*------------------------------------------------------------*/
            /** ENDLOOP through all objects...                           **/
            /*------------------------------------------------------------*/
         }
         /*---------------------------------------------------------------*/
         /** ENDIF we can copy the alias...                              **/
         /** IF this alias was used at all THEN                          **/
         /**    append a message to the global list                      **/
         /*---------------------------------------------------------------*/

         if (alias_used)
         {
            sprintf (err_msg, "         Object pds_class %s to %s", alias, real_name);
            err_append_message (CONTINUE, err_msg);
         }
         first_time = FALSE;
      }
      /*------------------------------------------------------------------*/
      /** ENDLOOP through all the object alias pairs                     **/
      /** free the alias pair list                                       **/
      /** fetch all values of the element alias sequence parameter       **/
      /*------------------------------------------------------------------*/

      alias_list = util_deallocate_string_list (alias_list);
      alias_list = lu_fetch_all_values (FindParameter (alias_object, 
                      "ELEMENT_ALIAS_SEQUENCE"), TRUE, FALSE);

      /*------------------------------------------------------------------*/
      /** LOOP through the element alias triplets                        **/
      /*------------------------------------------------------------------*/

      for (temp_list = alias_list; temp_list != NULL; temp_list = temp_list -> next)
      {
         alias_used = FALSE;

         /*---------------------------------------------------------------*/
         /** IF we can copy the alias, object, and real name from the    **/
         /**    alias triplet string THEN                                **/
         /*---------------------------------------------------------------*/
         
         if (sscanf (temp_list -> text, "%s %s %s", alias, 
                        object_name, real_name) == 3)
         {
            /*------------------------------------------------------------*/
            /** LOOP through all objects in the label                    **/
            /*------------------------------------------------------------*/

            for (current_object = label_ptr; current_object != NULL; 
                     current_object = NextObject (current_object))
            {

               /*---------------------------------------------------------*/
               /** clear any marks on the current object                 **/
               /** IF the object pds_class to which this alias applies is    **/
               /**    "N/A" or matches the current object pds_class THEN     **/
               /*---------------------------------------------------------*/

               if (IsMarked (current_object -> name)) 
                  ClearMark (current_object -> name); 
               if (IsNull(object_name) || 
                       (current_object -> pds_class != NULL && 
                           strcmp (current_object -> pds_class, object_name) == 0))
               {
                  /*-----------------------------------------------------*/
                  /** LOOP through all the parameters of this object    **/
                  /** IF the current alias matches the parameter THEN   **/
                  /**    change the current parameter name to the real  **/
                  /**    name                                           **/
                  /** ENDIF                                             **/    
                  /*-----------------------------------------------------*/

                  for (parameter_ptr = FirstParameter (current_object);
                          parameter_ptr != NULL; 
                             parameter_ptr = NextParameter (parameter_ptr))
                  {
                     if ((*alias == *(parameter_ptr -> name)) && 
                             (strcmp (parameter_ptr -> name, alias) == 0))
                     {
                        alias_used = TRUE;
                        Lemme_Go(parameter_ptr -> name);
                        Malloc_String(parameter_ptr -> name, (int) String_Size(real_name));
                        strcpy (parameter_ptr -> name, real_name);
                     }
                  }
                  /*-----------------------------------------------------*/
                  /** ENDLOOP through all the parameters...             **/
                  /*-----------------------------------------------------*/
               }
               /*---------------------------------------------------------*/
               /** ENDIF the object pds_class...                             **/
               /*---------------------------------------------------------*/

            }
            /*------------------------------------------------------------*/
            /** ENDLOOP through all objects in the label...              **/
            /*------------------------------------------------------------*/
         }
         /*---------------------------------------------------------------*/
         /** ENDIF we can copy the alias...                              **/
         /** IF this alias was used at all THEN                          **/
         /**    append a message to the global list                      **/
         /*---------------------------------------------------------------*/

         if (alias_used)
         {
            if (!IsNull(object_name))
               sprintf (err_msg, "         %s to %s in objects with pds_class %s", 
                        alias, real_name, object_name);
            else
               sprintf (err_msg, "         %s to %s", alias, real_name);
            err_append_message (CONTINUE, err_msg);
         }
         /*---------------------------------------------------------------*/
         /** ENDIF this alias was used...                                **/
         /*---------------------------------------------------------------*/
      }

      /*------------------------------------------------------------------*/
      /** ENDLOOP through all the object alias triplets                  **/
      /** IF there were no element alises THEN unmark all objects on     **/
      /**    the tree                                                    **/
      /** free the alias triplet list                                    **/
      /** free the alias tree                                            **/
      /*------------------------------------------------------------------*/

     if (alias_list == NULL)
      {
         for (current_object = label_ptr; current_object != NULL;
                current_object = NextObject(current_object))
            if (IsMarked (current_object -> name)) ClearMark (current_object -> name);
      } 
      util_deallocate_string_list (alias_list);
      lab_remove_label_or_template (alias_root);
   }

   /*----------------------------------------------------------------------*/
   /** ENDIF the data dictionary definition file can be opened...         **/
   /** IF no alias messages were needed THEN remove the alias message     **/
   /**    header from the global list                                     **/
   /*----------------------------------------------------------------------*/

   if (pds_last_message == end_message) 
   {
      err_deallocate_list (alias_message);
      alias_message = NULL;
   }
   /*----------------------------------------------------------------------*/
   /** RETURN a pointer to the alias message header                       **/
   /*----------------------------------------------------------------------*/

   return (alias_message);

/** END **/   
}

