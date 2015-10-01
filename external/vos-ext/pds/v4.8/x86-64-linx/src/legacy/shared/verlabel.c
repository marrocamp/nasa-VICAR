/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Component                                                           * 
 *    verlabel.c                                                       *
 * Abstract                                                            *
 *    PDS label object and keyword verification routines               *
 * Detailed Description                                                *
 *    This file contains the routines used to verify objects and       *
 *    keywords in PDS labels.                                          *
 * Internal References                                                 *
 *    Drivers:                                                         *
 *        ver_semantics                                                *
 *        ver_sub_objects                                              *
 *        ver_keywords                                                 *
 *        ver_keyword_semantics                                        *
 *                                                                     *
 *    Internal get-info routines:                                      *
 *        ver_get_type_from_class                                      *
 *        ver_get_type_from_dd                                         *
 *        ver_get_type_from_odl                                        *
 *        ver_get_type_string                                          *
 *        ver_get_value_string                                         *
 *                                                                     *
 *    Verification routines:                                           *
 *        ver_date                                                     *
 *        ver_date_string                                              *
 *        ver_date_time                                                *
 *        ver_integer_range                                            *
 *        ver_real_range                                               *
 *        ver_spelling                                                 *
 *        ver_standard_values                                          *
 *        ver_standard_values_2                                        *
 *        ver_string_length                                            *
 *        ver_time_string                                              *
 *        ver_units                                                    *
 *        ver_using_class_word                                         *
 *                                                                     *
 *    Verification utility routines:                                   *
 *        ver_pad_date                                                 *
 *        ver_pad_time                                                 *
 *        ver_pad_date_time                                            *
 *                                                                     *
 * Authors and Institutions                                            *
 *    Marti D. Demore / J.P.L.                                         *
 *    Kristy L. Marski / J.P.L.                                        *
 *    David P. Bernath / J.P.L.                                        *
 * Version and Date                                                    *
 *    1.1   June 22, 1992                                              *
 * Change History                                                      *
 *    DPB   06-12-91   Original code.                                  *
 *    MDD   06-22-92   Removed ver_sfdus                               *
 *    DWS   03-20-01   Stopped changing input keyword values in        *
 *                     ver_standard_values	and ver_get_value_string   *
 *    DWS   08-27-02   Added ver_standard_values_2 function to handle  *
 *                     the standard values for INDENTIFIERS            *
 *	  MDC	12-20-02   Made the output message more informational in   *
 *					   the ver_standard_values routine when lvtool	   *
 *					   prints out "Standard value is: ..."			   *
 *	  MDC	01-06-03   Modified conditional statements in ver_semantics,*
 *					   ver_keyword_semantics, ver_standard_values and  *
 *					   ver_standard_values_2 routines to determine     *
 *					   whether or not WARNING msgs will be appended    *
 *					   onto the message list.						   *
 *	  MDC	02-18-03   Added LV_TOOL conditional compile statements    *
 *					   wherever it had anything to do with comparing   *
 *					   the number of error messages generated with the *
 *					   maximum number of errors allowed.			   *
 *	  MDC	02-19-03   Modified the ver_units routine to check for	   *
 *					   the warning flag before outputting the message  *
 *					   that the units of a keyword value are not       *
 *					   recognized.									   *
 *    MDC   03-05-05   Modified ver_init routine. See notes.           *
 *    MDC   03-30-05   Modified ver_standard_values_2 and              *
 *                     ver_standard_values routines. See notes         *
 *    MDC   04-13-06   Modified ver_keyword_semantics. Added capability*
 *                     to check if new keywords found in a label exceed*
 *                     the maximum length of 30 characters.            *
 *    MDC   06-08-06   Modified ver_keyword routine to handle PSDD     *
 *                     special cases for BIT_COLUMNS and COLUMNS       *
 *    MDC   09-13-06   Modified file to not assume that a non-defined  *
 *                     keyword is of type REAL or INTEGER anymore.     *
 *    MDC   02-22-07   Modified ver_units routine. See notes.          *
 *                                                                     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "pdsdef.h"
#include "verlabel.h"
#include "ddaccess.h"
#include "label.h"
#include "labutil.h"
#include "search.h"
#include "errordef.h"
#include "utildef.h"

extern LOGICAL pds_verbose;
extern int dd_object_type;
extern long pds_error_count;
extern ERROR_LIST *pds_last_message;
extern LOGICAL pds_warning; /* 12-27-02 MDC */
extern LOGICAL pds_info; /* 02-13-03 MDC */
extern long lvtool_max_errors; /* 02-18-03 MDC */
extern long lvtool_warning_count; /* 02-18-03 MDC */
extern long total_error_count;

extern ERROR_LIST *pds_message_list; /* 01-06-03 MDC */

long     pds_line_number = {0};
int      pds_data_type = {VER_UNKNOWN};

extern char     dd_index_name [PDS_MAXLINE + 1];              /*DWS 08-17-97*/
extern char     dd_file_name [PDS_MAXLINE + 1];               /*DWS 08-17-97*/
extern long     begin_index;                                  /*DWS 08-17-97*/
extern long     end_index;                                    /*DWS 08-17-97*/
extern long     index_rec_len;                                /*DWS 08-17-97*/
long            dd_unit_offset;                               /*DWS 08-17-97*/
STRING_LIST     *unit_list;                                   /*DWS 08-17-97*/
#ifndef  _NO_PROTO
extern LOGICAL search_string_list_fwd (STRING_LIST *, char *, long *, long *,
                                            LOGICAL);
#else
extern LOGICAL search_string_list_fwd ();
#endif

/**********************************************************************
 *$Component                                                          *
 *    LOGICAL ver_semantics (label_ptr)                               *
 * or                                                                 *
 *    LOGICAL ver_semantics (keyword_file_ptr, label_ptr) for         *
 * data dictionary                                                    *
 *                                                                    *
 *$Abstract                                                           *
 *    Driver for semantic verification                                *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    DRIVER                                                          *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    keyword_file_ptr:    ONLY FOR THE DATA DICTIONARY!!             *
 *        The keyword_file_ptr variable is the pointer to the file    *
 *        to which is written the list of 'Data Dictionary validated' *
 *        keywords.                                                   *
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
 *    verify_success_flag:                                            *
 *        The verify_success_flag variable is a TRUE/FALSE value      *
 *        which indicates whether or not a keyword or object has been *
 *        successfully verified.                                      *
 *$Detailed_Description                                               *
 *    The ver_semantics routine is the driver which controls the      *
 *    semantic verification of all objects and keywords in a PDS      *
 *    label.                                                          *
 *$Error_Handling                                                     *
 *    The value of the verify_status_flag returned by the lower level *
 *    routines is returned to the calling routine.                    *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    dd_object_type          ddglob.h                 read           *
 *    pds_error_count         pdsglob.h                read           *
 *    pds_error_count         pdsglob.h                write          *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    2.4     July 9, 1992                                            *
 *$Change_History                                                     *
 *    DPB   05-30-91   Original code.                                 *
 *    DPB   06-12-91   Added object verification.                     *
 *    MDD   07-02-91   Added pds_line_offset to line numbers printed  *
 *                     in the progress messages                       *
 *    MDD   03-17-92   The great int -> long conversion               *
 *    MDD   06-22-92   Removed pds_line_offset                        *
 *    MDD   07-09-92   Shortened the dreaded GENERIC warning.         *
 *    RSJ   12-18-97   Added DDictionary Extractor code               *
 *    DWS   03-13-98   Added Conditional compiles for DDictionary     *
 *                     Extractor code.                                *
 *    DWS   08-27-02   Added test for VER_IDENTIFIER                  *
 *                      in ver_keyword_semantics                      *
 *    MDC	02-18-03   Added LV_TOOL conditional compile statements   *
 *					   Added checks to the pds_warning flag as well   *
 *					   before determining whether or not to append a  *
 *					   warning message.								  *
 *					   Added checks to the pds_info flag as well	  *
 *					   before determining whether or not to append an *
 *					   info message.								  *
 **********************************************************************/

#ifndef LV_DDICT
LOGICAL ver_semantics (label_ptr)

AGGREGATE label_ptr;
#else
LOGICAL ver_semantics (keyword_file_ptr, label_ptr)

FILE *keyword_file_ptr;
AGGREGATE label_ptr;
#endif

{
    AGGREGATE object_ptr = {NULL};
    ERROR_LIST *last_ptr = {NULL};
    ERROR_LIST *msg_ptr = {NULL};
    char *object_class = {NULL};
    LOGICAL success = {TRUE};

	ERROR_LIST *temp_ptr = {NULL};         /* 01-06-03 MDC */

	long total_count = 0; /* 02-18-03 */

#ifndef LV_DDICT
    char err_msg [PDS_MAXLINE];
#endif
/** BEGIN **/
    /*-----------------------------------------------------------------------*/
    /** LOOP through the objects in the label . . .                         **/
    /*-----------------------------------------------------------------------*/
	
	/* 02-20-03 MDC */
#ifndef LV_TOOL	
	for (object_ptr = label_ptr, pds_error_count = 0; 
             ((object_ptr != NULL) && (pds_error_count <= PDS_MAX_ERRORS));
                 object_ptr = NextObject(object_ptr))
#else
	for (object_ptr = label_ptr;
		 ((object_ptr != NULL) && (total_error_count <= lvtool_max_errors));
			object_ptr = NextObject(object_ptr))
#endif
    {                     
        /*-------------------------------------------------------------------*/
        /** Append an info message onto the global list of messages.        **/
        /*-------------------------------------------------------------------*/
#ifndef LV_DDICT

        /*-------------------------------------------------------------------*/
        /* RSJ: 12-18-97 - this code not needed in DDICT                     */
        /*-------------------------------------------------------------------*/
		sprintf (err_msg,
                 "Sub-objects of %s (line %ld):",
                 object_ptr -> name, object_ptr -> appl1);
        err_append_message (CONTINUE, " ");
        msg_ptr = pds_last_message;
        err_append_message (CONTINUE, err_msg);
        /*-------------------------------------------------------------------*/
#endif
        err_append_message (CONTINUE, " ");
        last_ptr = pds_last_message;

        /*-------------------------------------------------------------------*/
        /** Get the actual pds_class of the object                              **/
        /*-------------------------------------------------------------------*/

        object_class = dd_get_object_class (object_ptr -> name);

        /*-------------------------------------------------------------------*/
        /** IF there is no definition in the data dictionary for the        **/
        /**         object THEN                                             **/
        /**     Append a message onto the global list of messages.          **/
        /** ELSE                                                            **/
        /**     Verify object semantics.  (This includes making sure that   **/
        /**         all required sub-objects are present, and that every    **/
        /**         sub-object is either required or optional)              **/
        /** ENDIF                                                           **/
        /*-------------------------------------------------------------------*/
      
#ifndef LV_DDICT
        /*-------------------------------------------------------------------*/
        /* RSJ: 12-18-97 - this code not needed in DDICT                     */
        /*-------------------------------------------------------------------*/
        if (strcmp (object_ptr -> name, "ROOT") != 0)
        {
           if (! dd_get_object_definition (object_class, DD_REPLACE_DEFS))
           {
			   if (pds_verbose && pds_warning)    /* 01-06-03 MDC */
			   {
                   err_object_message (WARNING, object_ptr -> name, 
                                    object_ptr -> appl1, 
                    "Not in the data dictionary.  (Sub-objects and keywords will not be validated against required and optional lists)");
               }
           }
           else
           {      
               if (dd_object_type == DD_GENERIC_OBJECT)
               {
				   /*************************************************/
				   /* 02-13-03 MDC									*/
				   /* If user has turned off displaying info msgs., */
				   /* then do not create this INFO message.			*/
				   /*************************************************/
			      if (pds_verbose && pds_info)
                  {
				     /*---------------------------------------------*/
					 /* 12-20-02 MDC								*/
				     /* Changed this output message to be an INFO	*/
					 /* message rather than a WARNING message.		*/
				     /*---------------------------------------------*/

 /*                    err_object_message (WARNING, object_ptr -> name, 
                                          object_ptr -> appl1, 
                                "Using PDS GENERIC object definition."); */
					  err_object_message (INFO, object_ptr -> name,
										 object_ptr -> appl1,
							    "Using PDS GENERIC object definition."); 
                  }
               }
               success = (ver_sub_objects (object_ptr, object_class) && success);
		   }
		}
#endif

        if (last_ptr -> next == NULL)
            err_deallocate_list (msg_ptr);

        /*-------------------------------------------------------------------*/
        /** Append an info message onto the global list of messages.        **/
        /*-------------------------------------------------------------------*/
#ifndef LV_DDICT
        /*-------------------------------------------------------------------*/
        /* RSJ: 12-18-97 - this code not need in DDICT                       */
        /*-------------------------------------------------------------------*/

        sprintf (err_msg, "Keywords of %s (line %ld):",
                 object_ptr -> name, object_ptr -> appl1);
        err_append_message (CONTINUE, " ");
        msg_ptr = pds_last_message;
        err_append_message (CONTINUE, err_msg);
        err_append_message (CONTINUE, " ");
        last_ptr = pds_last_message;
        /*-------------------------------------------------------------------*/
#endif

        /*-------------------------------------------------------------------*/
        /** Validate the objects keywords.                                  **/
        /*-------------------------------------------------------------------*/
#ifndef LV_DDICT
        success = (ver_keywords (object_ptr, object_class) && success);
#else
        success = (ver_keywords (keyword_file_ptr, object_ptr,
			                      object_class) && success);
#endif

        if (last_ptr -> next == NULL)
            err_deallocate_list (msg_ptr);

		/*-------------------------------------------------------------------*/
        /** Deallocate local storage.                                       **/
        /*-------------------------------------------------------------------*/
        Lemme_Go(object_class);
    /*-----------------------------------------------------------------------*/
    /** ENDLOOP                                                             **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "for (object_ptr = label_ptr; ..."  */

    /*-----------------------------------------------------------------------*/
    /** Display a final info message.                                       **/
    /*-----------------------------------------------------------------------*/
	
	/*-----------------------------------------------------------------------*/
	/** 02-18-03 MDC														**/
	/** Added an LV_TOOL conditional compile statement.						**/
	/*-----------------------------------------------------------------------*/
#ifndef LV_TOOL
    if (pds_error_count > PDS_MAX_ERRORS)
#else 
	if (total_error_count > lvtool_max_errors)
#endif
    {
        err_append_message (CONTINUE, " ");
        err_append_message (CONTINUE, 
             "**** STOP **** STOP **** STOP **** STOP **** STOP **** STOP ****");
        err_append_message (CONTINUE, 
             "****                                                        ****");
        err_append_message (CONTINUE, 
             "****            MAXIMUM NUMBER OF ERRORS EXCEEDED!          ****");
        err_append_message (CONTINUE, 
             "****                                                        ****");
        err_append_message (CONTINUE, 
             "**** STOP **** STOP **** STOP **** STOP **** STOP **** STOP ****");
        err_append_message (CONTINUE, " ");
    }
#ifndef LV_DDICT
    else
    {
		/*-------------------------------------------------------------------*/
		/* MDC: 01-06-03 - If the error or warning counts are not 0, then    */
		/*	  tally them up together and output a message.					 */
		/*-------------------------------------------------------------------*/
#ifdef LV_TOOL

		if( (pds_error_count != 0) || (lvtool_warning_count != 0) ) 
		{
			/* 02-18-03 MDC - Tally up the total number of errors and warnings */
			total_count = pds_error_count + lvtool_warning_count;
			
			if (total_count == 1)
			{
				sprintf (err_msg, 
                     "Semantic validation completed with one error or warning");
			}
			else
			{
				sprintf (err_msg, 
                     "Semantic validation completed with %ld errors or warnings",
                     total_count);
			}

			err_append_message (CONTINUE, " ");
			err_append_message (CONTINUE, err_msg);
			err_append_message (CONTINUE, " "); 
		}
#else
		/*-------------------------------------------------------------------*/
		/* RSJ: 12-18-97 - this code not need in DDICT                       */
		/*-------------------------------------------------------------------*/
		if (pds_error_count == 1)
		{
			sprintf (err_msg, 
                  "Semantic validation completed with one error or warning");
		}
		else
		{
			sprintf (err_msg, 
                  "Semantic validation completed with %ld errors or warnings",
                   pds_error_count);
		}
		err_append_message (CONTINUE, " ");
		err_append_message (CONTINUE, err_msg);
		err_append_message (CONTINUE, " "); 
#endif
	   /*-------------------------------------------------------------------*/
    }  /*  End:  "if (pds_error_count > PDS_MAX_ERRORS) ... else ..."  */
#endif

    /*-----------------------------------------------------------------------*/
    /** RETURN the verify_status_flag                                       **/
    /*-----------------------------------------------------------------------*/
	dd_cleanup_defs ();
    return (success);

/** END **/

}  /*  "ver_semantics"  */



/**********************************************************************
 *$Component                                                          *
 *    LOGICAL ver_sub_objects (object_ptr, object_class)              *
 *$Abstract                                                           *
 *    Verifies the semantics of an object.                            *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    OBJECT                                                          *
 *    DRIVER                                                          *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    object_ptr:                                                     *
 *        The object_ptr variable is a pointer to the structure       *
 *        used to represent an object in a PDS label.                 *
 *    object_class:                                                   *
 *        The object_class variable is a character string             *
 *        which contains the pds_class of an object in a PDS label        *
 *        (e.g., "OBJECT = IMAGE" implies that "IMAGE" is the pds_class   *
 *        of the object).                                             *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    verify_success_flag:                                            *
 *        The verify_success_flag variable is a TRUE/FALSE value      *
 *        which indicates whether or not a keyword or object has been *
 *        successfully verified.                                      *
 *$Detailed_Description                                               *
 *    The ver_sub_objects routine verifies the semantics of an        *
 *    object in a PDS label.  It makes sure that all required         *
 *    sub-objects are present, and that all other sub-objects are     *
 *    optional.                                                       *
 *$Error_Handling                                                     *
 *    The value of the verify_status_flag returned by the lower level *
 *    routines is returned to the calling routine.                    *
 *$External_References                                                *
 *    Item                     Shared-Data           Access           *
 *    ------------------------------------------------------------    *
 *    pds_error_count          pdsglob.h             read             *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   October 22, 1991                                          *
 *$Change_History                                                     *
 *    DPB   06-12-91   Original code.                                 *
 *    MDD   10-22-91   Fixed malloc, free, and sys_exit_system calls  *
 *    DWS   05-13-99   Added checking for CATALOG objects.            *
 **********************************************************************/

LOGICAL ver_sub_objects (object_ptr, object_class)

AGGREGATE object_ptr;
char *object_class;
                         
{
    AGGREGATE sub_object_ptr = {NULL};
    STRING_LIST *required_list = {NULL};
    STRING_LIST *optional_list = {NULL};
    STRING_LIST *ptr = {NULL};
    char *sub_object_class = {NULL};
    char *missing_objects = {NULL};
    LOGICAL on_required_list = {FALSE};
    LOGICAL on_optional_list = {FALSE};
    LOGICAL some_are_missing = {FALSE};
    LOGICAL marked = {FALSE};
    LOGICAL first_one = {FALSE};
    LOGICAL psdd = {FALSE};
    LOGICAL ok_to_verify_lists = {FALSE};
    LOGICAL success = {TRUE};
	PARAMETER cat_ptr;			/*added for CATALOG_ stuff*/
	int		  cat_flag;         /*added for CATALOG_ stuff*/
/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** IF an object pds_class was passed in THEN                               **/
    /*-----------------------------------------------------------------------*/

    if (object_class != NULL)
    {
        /*-------------------------------------------------------------------*/
        /** Get the names of all of the required and optional sub-objects.  **/
        /*-------------------------------------------------------------------*/

        required_list = dd_get_required_objects (object_class);
        optional_list = dd_get_optional_objects (object_class);

        /*-------------------------------------------------------------------*/
        /** Set a flag indicating that at least one list exists.            **/
        /*-------------------------------------------------------------------*/

        ok_to_verify_lists = ((required_list != NULL) || (optional_list != NULL));

        /*-------------------------------------------------------------------*/
        /** Determine if all objects in the data dictionary are optional.   **/
        /*-------------------------------------------------------------------*/

        for (ptr = optional_list, psdd = FALSE; 
                ((ptr != NULL) && (! psdd)); ptr = ptr -> next)
        {
            psdd = (strcmp ("PSDD", ptr -> text) == 0);
        }

    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if (object_class != NULL) ..."  */

    /*-----------------------------------------------------------------------*/
    /** LOOP through the sub-objects . . .                                  **/
    /*-----------------------------------------------------------------------*/

    for (sub_object_ptr = object_ptr -> first_child;
             ((sub_object_ptr != NULL) && (ok_to_verify_lists)); 
                 sub_object_ptr = sub_object_ptr -> right_sibling)
    {
        /*-------------------------------------------------------------------*/
        /** Get the actual pds_class of the sub-object.                         **/
        /*-------------------------------------------------------------------*/

        sub_object_class = dd_get_object_class (sub_object_ptr -> name);

        if (sub_object_class == NULL)
        {
            Malloc_String(sub_object_class, (int) String_Size(sub_object_ptr->name));
            strcpy (sub_object_class, sub_object_ptr->name);
        }

        /*-------------------------------------------------------------------*/
        /** Check each item on the list of required objects against         **/
        /**     the pds_class of the sub-object.  If a match is found, then     **/
        /**     mark both the sub-object and the item on the required list. **/
        /*-------------------------------------------------------------------*/

        for (ptr = required_list, on_required_list = FALSE; 
                 ((ptr != NULL) && (! on_required_list));
                     ptr = ptr -> next)
        {
            marked = IsMarked(ptr -> text);

            if (marked)
                ClearMark(ptr -> text);

            on_required_list = (strcmp (sub_object_class, ptr -> text) == 0);

            if (on_required_list)
                SetMark(sub_object_ptr -> name);

            if (marked || on_required_list)
                SetMark(ptr -> text);

        }  /*  End:  "for (ptr = required_list; ..."  */
    
                          
        /*-------------------------------------------------------------------*/
        /** Now, if the current sub-object was not on the required list,    **/
        /**     then it must be checked against the optional list.  If it's **/
        /**     on the optional list then mark the object (it is not        **/
        /**     necessary to mark the item on the list this time).          **/
        /*-------------------------------------------------------------------*/

        if (! on_required_list)
        {
            if (psdd)
                on_optional_list = dd_name_exists(sub_object_class, "OBJECT_DEFINITION");
            else
            {        
                for (ptr = optional_list, on_optional_list = FALSE;
                         ((ptr != NULL) && (! on_optional_list)); 
                             ptr = ptr -> next)
                {
                    on_optional_list = (strcmp(sub_object_class,ptr->text) == 0);
                }

            }  /*  End:  "if (psdd) ... else ..."  */
    
            if (on_optional_list)
                SetMark(sub_object_ptr -> name);

        }  /*  End:  "if (! on_required_list) ..."  */

        /*-------------------------------------------------------------------*/
        /** Deallocate local storage.                                       **/
        /*-------------------------------------------------------------------*/

        Lemme_Go(sub_object_class);

    /*-----------------------------------------------------------------------*/
    /** ENDLOOP                                                             **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "for (sub_object_ptr = object_ptr -> ..."  */

    /*-----------------------------------------------------------------------*/
    /** Prepare an error message string just in case any required           **/
    /**     sub-objects were missing.                                       **/
    /*-----------------------------------------------------------------------*/

    Malloc_String(missing_objects, PDS_MAXLINE);
    sprintf (missing_objects, "The following required sub-objects are missing:  ");

    /*-----------------------------------------------------------------------*/
    /** Append the names of all items on the required list that were        **/
    /**     missing from the object onto the error message string.          **/
    /*-----------------------------------------------------------------------*/
    cat_flag = 1;
    for (ptr = required_list, first_one = TRUE, some_are_missing = FALSE; 
             ptr != NULL; ptr = ptr -> next)
    {
        if (! IsMarked(ptr -> text))
        {
			if(strcmp(object_class, "CATALOG") == 0)              /*is this a catalog type*/
			{													  /*if so, then check for */
				cat_ptr = object_ptr->first_parameter;
				for (cat_ptr = object_ptr->first_parameter;       /* pointers     */
								cat_ptr != NULL && cat_flag == 1;
										cat_ptr = cat_ptr->right_sibling)
				{                       /*The pointer may have a _CATALOG appended  */
					if( strstr(cat_ptr->name, ptr->text) == NULL) /*is this pointer the same*/
					{											  /*as a required sub-object*/
						cat_flag = 1;							  /*no, make sure error msg */
					}											  /*is generated.           */
					else								
					{											  /*yes, stop the check and */
						cat_flag = 0;							  /*do not generate error msg*/
					}
				}
			}
			else												  /*wasn't a CATALOG pds_class  */
			{													  /*be sure to generate msg */
				cat_flag = 1;
			}
		
			if(cat_flag)
			{
				some_are_missing = TRUE;

				Realloc_String(missing_objects, (String_Size(missing_objects)
                                             + String_Size(ptr -> text) + 2));      
				if (! first_one)
					strcat (missing_objects, ", ");
				strcat (missing_objects, ptr -> text);
				first_one = FALSE;
			}


        }  /*  End:  "if (IsMarked( ..."  */

    }  /*  End:  "for (ptr = required_list; ..."  */

    /*-----------------------------------------------------------------------*/
    /** Append the error message string onto the list of messages.          **/
    /*-----------------------------------------------------------------------*/

    if (some_are_missing)
    {
        success = err_object_message (ERROR1, object_ptr -> name, 
                                      object_ptr->appl1, missing_objects);
    }

    /*-----------------------------------------------------------------------*/
    /** LOOP through the sub-objects once again . . .                       **/
    /**     Clear any marks that were set.                                  **/
    /**     Append an error message onto the list of messages for each      **/
    /**         sub-object that was not marked (these are the ones that     **/
    /**         are neither required nor optional).                         **/
    /** ENDLOOP                                                             **/
    /*-----------------------------------------------------------------------*/

    for (sub_object_ptr = object_ptr -> first_child;
             ((sub_object_ptr != NULL) && (ok_to_verify_lists)); 
                 sub_object_ptr = sub_object_ptr -> right_sibling)
    {
        if (IsMarked(sub_object_ptr -> name))
            ClearMark(sub_object_ptr -> name);
        else
		{
#ifdef LV_TOOL
			if (total_error_count <= lvtool_max_errors)
#else
            if (pds_error_count <= PDS_MAX_ERRORS)
#endif
            {
                success = err_object_message (ERROR1, sub_object_ptr -> name,
                                              sub_object_ptr -> appl1, 
                              "Not an allowed sub-object.  (It was not found on the optional or required sub-objects list)");
            }
		}
 
    }  /*  End:  "for (sub_object_ptr = object_ptr -> ..."  */
    
    /*-----------------------------------------------------------------------*/
    /** Deallocate local storage.                                           **/
    /*-----------------------------------------------------------------------*/

    if (required_list != NULL)
        util_deallocate_string_list (required_list);

    if (optional_list != NULL)
        util_deallocate_string_list (optional_list);

    Lemme_Go(missing_objects);

    /*-----------------------------------------------------------------------*/
    /** RETURN the verify_status_flag                                       **/
    /*-----------------------------------------------------------------------*/

    return (success);

/** END **/

}  /*  "ver_sub_objects"  */



/**********************************************************************
 *$Component                                                          *
 *    LOGICAL ver_keywords (object_ptr, object_class)                 *
 * or for Data Dictionary                                             *
 *    Lobical ver_keywords (keyword_file_ptr, object_ptr,             *
 *                            object_class)                           *
 *$Abstract                                                           *
 *    Verifies the semantics of the keywords in an object.            *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    KEYWORD                                                         *
 *    DRIVER                                                          *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    keyword_file_ptr:    FOR DATA DICTIONARY ONLY!!!                *
 *        The keyword_file_ptr variable is the pointer to the file    *
 *        to which is written the list of 'Data Dictionary validated' *
 *        keywords.                                                   *
 *    object_ptr:                                                     *
 *        The object_ptr variable is a pointer to the structure       *
 *        used to represent an object in a PDS label.                 *
 *    object_class:                                                   *
 *        The object_class variable is a character string             *
 *        which contains the pds_class of an object in a PDS label        *
 *        (e.g., "OBJECT = IMAGE" implies that "IMAGE" is the pds_class   *
 *        of the object).                                             *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    verify_success_flag:                                            *
 *        The verify_success_flag variable is a TRUE/FALSE value      *
 *        which indicates whether or not a keyword or object has been *
 *        successfully verified.                                      *
 *$Detailed_Description                                               *
 *    The ver_keywords routine verifies the semantics of the          *
 *    keywords of an object in a PDS label.  It makes sure that all   *
 *    required keywords are present, and that all other keywords      *
 *    are on the optional list.  After verifying required and         *
 *    optional keywords, it verifies the semantics of each keyword's  *
 *    value(s).                                                       *
 *$Error_Handling                                                     *
 *    The value of the verify_status_flag returned by the lower level *
 *    routines is returned to the calling routine.                    *
 *$External_References                                                *
 *    Item                     Shared-Data           Access           *
 *    ------------------------------------------------------------    *
 *    pds_error_count          pdsglob.h             read             *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.2   October 22, 1991                                          *
 *$Change_History                                                     *
 *    DPB   06-12-91   Original code.                                 *
 *    MDD   06-27-91   Changed to complain about invalid keywords in  *
 *                     objects only if PSDD is not in the list of     *
 *                     optional keywords                              *
 *    MDD   10-22-91   Fixed malloc, free, and sys_exit_system calls  *
 *    RSJ   12-18-97   Added DDictionary Extractor code               *
 *    DWS   03-13-98   Added conditional compile for DDictionary      *
 *    MDC   06-08-06   Added routine call to handle PSDD limitations  *
 **********************************************************************/
#ifndef LV_DDICT
LOGICAL ver_keywords (object_ptr, object_class)

AGGREGATE object_ptr;
#else
LOGICAL ver_keywords (keyword_file_ptr, object_ptr, object_class)

FILE *keyword_file_ptr;
AGGREGATE object_ptr;
#endif
char *object_class;
                         
{
    PARAMETER keyword_ptr = {NULL};
    STRING_LIST *required_list = {NULL};
    STRING_LIST *optional_list = {NULL};
    STRING_LIST *ptr = {NULL};
    char *missing_keywords = {NULL};
    LOGICAL on_required_list = {FALSE};
    LOGICAL on_optional_list = {FALSE};
    LOGICAL some_are_missing = {FALSE};
    LOGICAL marked = {FALSE};
    LOGICAL first_one = {FALSE};
    LOGICAL psdd = {FALSE};
    LOGICAL ok_to_verify_lists = {FALSE};
    LOGICAL success = {TRUE};

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** IF an object pds_class was passed in THEN                               **/
    /*-----------------------------------------------------------------------*/


    if (object_class != NULL)
    {
        /*-------------------------------------------------------------------*/
        /** Get the names of all of the required and optional keywords.     **/
        /*-------------------------------------------------------------------*/

        required_list = dd_get_required_members (object_class);
        optional_list = dd_get_optional_members (object_class);

        /*-------------------------------------------------------------------*/
        /** Set a flag indicating that at least one list exists.            **/
        /*-------------------------------------------------------------------*/

        ok_to_verify_lists = ((required_list != NULL) || (optional_list != NULL));

        /*-------------------------------------------------------------------*/
        /** Determine if all keywords in the data dictionary are optional.  **/
        /*-------------------------------------------------------------------*/

        for (ptr = optional_list, psdd = FALSE; 
                ((ptr != NULL) && (! psdd)); ptr = ptr -> next)
        {
           psdd = (strcmp ("PSDD", ptr -> text) == 0);
        }

    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if (object_class != NULL) ..."  */

    /*-----------------------------------------------------------------------*/
    /** LOOP through the keywords . . .                                     **/
    /*-----------------------------------------------------------------------*/

    for (keyword_ptr = FirstParameter(object_ptr);
             ((keyword_ptr != NULL) && (ok_to_verify_lists)); 
                 keyword_ptr = NextParameter(keyword_ptr))
    {
        /*-------------------------------------------------------------------*/
        /** Check each item on the list of required keywords against        **/
        /**     the name of the keyword.  If a match is found, then         **/
        /**     mark both the keyword and the item on the required list.    **/
        /*-------------------------------------------------------------------*/
        for (ptr = required_list, on_required_list = FALSE; 
                 ((ptr != NULL) && (! on_required_list) &&
                   (keyword_ptr -> node_kind != KP_POINTER)); 
                     ptr = ptr -> next)
        {
            marked = IsMarked(ptr -> text);

            if (marked)
                ClearMark(ptr -> text);
			
				on_required_list = (strcmp (keyword_ptr -> name, ptr -> text) == 0);
            if (on_required_list)
                SetMark(keyword_ptr -> name);

            if (marked || on_required_list)
                SetMark(ptr -> text);

        }  /*  End:  "for (ptr = required_list; ..."  */
    
                          
        /*-------------------------------------------------------------------*/
        /** Now, if the current keyword was not on the required list,       **/
        /**     then it must be checked against the optional list.  If      **/
        /**     it's on the optional list then mark the keyword (it is not  **/
        /**     necessary to mark the item on the list this time).          **/
        /*-------------------------------------------------------------------*/

        if ((! on_required_list) && (keyword_ptr -> node_kind != KP_POINTER))
        {
            if (psdd)
                on_optional_list = dd_name_exists(keyword_ptr->name, "ELEMENT_DEFINITION");
            else
            {                
                for (ptr = optional_list, on_optional_list = FALSE;
                         ((ptr != NULL) && (! on_optional_list)); 
                             ptr = ptr -> next)
                {
                    on_optional_list = (strcmp(keyword_ptr->name,ptr->text) == 0);
                }
    
            }  /*  End: "if (psdd) ... else ..."  */

            if (on_optional_list)
			{
				/* 06-08-06 MDC - Need to handle limitations in the data dictionary */
				do_more_kwd_semantics(keyword_ptr, object_class, required_list);
		        SetMark(keyword_ptr -> name);
			}

        }  /*  End:  "if (! on_required_list) ..."  */
    /*-----------------------------------------------------------------------*/
    /** ENDLOOP                                                             **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "for (keyword_ptr = FirstParameter( ..."  */

    /*-----------------------------------------------------------------------*/
    /** Prepare an error message string just in case any required           **/
    /**     keywords were missing.                                          **/
    /*-----------------------------------------------------------------------*/

    Malloc_String(missing_keywords, PDS_MAXLINE);
    sprintf (missing_keywords, "The following required keywords are missing:  ");

    /*-----------------------------------------------------------------------*/
    /** Append the names of all items on the required list that were        **/
    /**     missing from the object onto the error message string.          **/
    /*-----------------------------------------------------------------------*/

    for (ptr = required_list, first_one = TRUE, some_are_missing = FALSE; 
            ptr != NULL; 
                ptr = ptr -> next)
    {
        if (! IsMarked(ptr -> text))
        {
            some_are_missing = TRUE;
            Realloc_String(missing_keywords, (String_Size(missing_keywords)
                                              + String_Size(ptr -> text) + 2));
            if (! first_one)
                strcat (missing_keywords, ", ");
            strcat (missing_keywords, ptr -> text);

            first_one = FALSE;

        }  /*  End:  "if (IsMarked( ..."  */

    }  /*  End:  "for (ptr = required_list; ..."  */

    /*-----------------------------------------------------------------------*/
    /** Append the error message string onto the list of messages.          **/
    /*-----------------------------------------------------------------------*/

    if (some_are_missing)
    {
        success = err_object_message (ERROR1, object_ptr -> name, 
                                      object_ptr->appl1, missing_keywords);
    }

    /*-----------------------------------------------------------------------*/
    /** LOOP through the keywords once again . . .                          **/
    /**     Clear any marks that were set.                                  **/
    /**     Append an error message onto the list of messages for each      **/
    /**         keyword that was not marked (these are the ones that        **/
    /**         are neither required nor optional) if psdd is not on the    **/
    /**         optional list.                                              **/
    /**     Verify the semantics of the keyword's value(s).                 **/
    /** ENDLOOP                                                             **/
    /*-----------------------------------------------------------------------*/

    for (keyword_ptr = FirstParameter(object_ptr);
             keyword_ptr != NULL;
                 keyword_ptr = NextParameter(keyword_ptr))
    {
        if (keyword_ptr -> node_kind != KP_POINTER)
        {
            if (IsMarked(keyword_ptr -> name))
                ClearMark(keyword_ptr -> name);
            else
			{
#ifdef LV_TOOL 
				if ((ok_to_verify_lists) && (pds_error_count <= lvtool_max_errors))
#else
                if ((ok_to_verify_lists) && (pds_error_count <= PDS_MAX_ERRORS))
#endif
				{
                    if (!psdd)
                    {
                       success = err_object_message (ERROR1, keyword_ptr -> name,
                                                     keyword_ptr -> appl1, 
                                     "Not an allowed keyword.  (It was not found on the optional or required keywords list)");
					}
                }
			}

#ifdef LV_TOOL
			if (total_error_count <= lvtool_max_errors)
#else
            if (pds_error_count <= PDS_MAX_ERRORS)
#endif
			{
#ifndef LV_DDICT
                success = (ver_keyword_semantics (keyword_ptr) && success);
#else
                success = (ver_keyword_semantics (keyword_file_ptr, keyword_ptr)
					                                                  && success);
#endif
            }
		}  /*  End:  "if (keyword_ptr -> ..."  */
    
    }  /*  End:  "for (keyword_ptr = object_ptr -> ..."  */
    
    /*-----------------------------------------------------------------------*/
    /** Deallocate local storage.                                           **/
    /*-----------------------------------------------------------------------*/

	if (required_list != NULL)
	      util_deallocate_string_list (required_list);
  
    if (optional_list != NULL)
			util_deallocate_string_list (optional_list);
    Lemme_Go(missing_keywords);

    /*-----------------------------------------------------------------------*/
    /** RETURN the verify_status_flag                                       **/
    /*-----------------------------------------------------------------------*/
    return (success);

/** END **/

}  /*  "ver_keywords"  */

/******************************************************************************
 ROUTINE
     void do_more_kwd_semantics

 DESCRIPTION
     This routine is needed to do some extra checking on the required 
	 keywords of an object in the PSDD. As defined in the standards, there
	 are certain objects that are defined that go away from the norm of how
	 the PSDD model is structured. Currently this applies to 2 objects:
	 BIT_COLUMN and COLUMN.

	 For BIT_COLUMN, BITS is not required if ITEMS is defined within the object.
	 For COLUMN, BYTES is not required if ITEMS is defined within the object.

	 This routine will go through the required list again and mark the BITS or
	 BYTES keywords so that LVTool does not flag it as required, but missing
	 in the label.

 CHANGE HISTORY
     06-08-06   MDC   Original code
     
 ******************************************************************************/

void do_more_kwd_semantics( PARAMETER keyword_ptr, char *object_class, STRING_LIST *required_list )
{
	LOGICAL have_bit_column_obj = FALSE;
	LOGICAL have_column_obj = FALSE;
    STRING_LIST *ptr = {NULL};
    LOGICAL on_required_list = {FALSE};
    LOGICAL marked = {FALSE};

	if(strcmp(object_class, "BIT_COLUMN") == 0)
		have_bit_column_obj = TRUE;
	else if(strcmp(object_class, "COLUMN") == 0)
		have_column_obj = TRUE;

	/* If we have ITEMS within a BIT_COLUMN or COLUMN object, then do the following...*/
    if( (strcmp(keyword_ptr -> name, "ITEMS") == 0) && (have_bit_column_obj || have_column_obj) )
	{
        for (ptr = required_list, on_required_list = FALSE; 
                 ((ptr != NULL) && (! on_required_list) &&
                 (keyword_ptr -> node_kind != KP_POINTER)); 
                 ptr = ptr -> next)
        {
            marked = IsMarked(ptr -> text);

            if (marked)
                ClearMark(ptr -> text);
			
			/* Mark the BITS or BYTES keyword so we don't flag it as a required missing
			   keyword in the report. Standards says so as long as ITEMS is defined within
			   the object in the label.
		    */
			if(have_bit_column_obj)
				on_required_list = (strcmp ("BITS", ptr->text) == 0);
			else if(have_column_obj)
				on_required_list = (strcmp ("BYTES", ptr->text) == 0);

            if (marked || on_required_list)
                SetMark(ptr -> text);

        }  /*  End:  "for (ptr = required_list; ..."  */
	}
}

/**********************************************************************
 *$Component                                                          *
 *    LOGICAL ver_keyword_semantics (keyword_ptr)                     *
 * or for the data dictionary                                         *
 *    LOGICAL ver_keyword_semantics (keyword_file_ptr, keyword_ptr)   *
 *$Abstract                                                           *
 *    Driver for keyword semantic verification                        *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    KEYWORD                                                         *
 *    DRIVER                                                          *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    keyword_file_ptr:   FOR DATA DICTIONARY ONLY!!!!                *
 *        The keyword_file_ptr variable is the pointer to the file    *
 *        to which is written the list of 'Data Dictionary validated' *
 *        keywords.                                                   *
 *    keyword_ptr:                                                    *
 *        The keyword_ptr variable is a pointer to the structure      *
 *        used to represent a keyword in a PDS label.                 *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    verify_success_flag:                                            *
 *        The verify_success_flag variable is a TRUE/FALSE value      *
 *        which indicates whether or not a keyword or object has been *
 *        successfully verified.                                      *
 *$Detailed_Description                                               *
 *    The ver_keyword_semantics routine is the driver which controls  *
 *    the semantic verification of a keyword in a PDS label.  It      *
 *    takes a pointer to the structure used to represent the keyword, *
 *    fetches the data dictionary info on it, determines its data     *
 *    type, and calls the appropriate routine to verify it.           *
 *$Error_Handling                                                     *
 *    1)  If there is no data dictionary info on the keyword, then a  *
 *        warning is appended onto the message list and pds_class word    *
 *        verification is attempted.                                  *
 *    2)  If data dictionary info is available, but the data type     *
 *        of the keyword is unknown or missing, then a warning is     *
 *        appended onto the message list and pds_class word verification  *
 *        is attempted.                                               *
 *$External_References                                                *
 *    Item                     Shared-Data           Access           *
 *    ------------------------------------------------------------    *
 *    pds_line_number          verlabel.c            read             *
 *    pds_data_type            verlabel.c            read             *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.0   May 30, 1991                                              *
 *$Change_History                                                     *
 *    DPB   05-30-91   Original code.                                 *
 *    MDD   07-09-92   Removed warning for unknown data types, because*
 *                     such elements are now considered valid.        *
 *    RSJ   12-18-97   Added DDictionary Extractor code               *
 *    DWS   03-13-98   Added conditional compiles for data dictionary *
 *    DWS   08-27-02   Added test for VER_IDENTIFIER                  *
 *    MDC	01-06-03   Added an additional check to see if the warning*
 *					   flag is ON or OFF. If it is ON, then append    *
 *					   the warning msg. to the list.				  *
 *    MDC   04-13-06   Added a call to a new routine that will check  *
 *                     that the length of the keyword does not exceed *
 *                     the maximum length as specified in the PDS     *
 *                     data dictionary.                               *
 **********************************************************************/
#ifndef LV_DDICT
LOGICAL ver_keyword_semantics (keyword_ptr)

PARAMETER keyword_ptr;
#else
LOGICAL ver_keyword_semantics (keyword_file_ptr, keyword_ptr)

FILE *keyword_file_ptr;
PARAMETER keyword_ptr;
#endif


{   
#ifdef LV_KWVTOOL
/*test code for kwvtool                                           dws 05-15-98*/
    STRING_LIST *value_list = {NULL};
    STRING_LIST *next_value = {NULL};
	char        move_temp_stor[150];
	int          move_len;
/*end test code                                                   dws 05-15-98*/
#endif
    LOGICAL success = {TRUE};

/** BEGIN **/
    /*-----------------------------------------------------------------------*/
    /** Initialize the global data type and line number variables.          **/
    /*-----------------------------------------------------------------------*/

    pds_data_type = VER_UNKNOWN;
    pds_line_number = keyword_ptr -> appl1;

    /*-----------------------------------------------------------------------*/
    /** Attempt to fetch the data dictionary info on the keyword passed in. **/
    /** IF the attempt failed THEN                                          **/
    /**     Append a warning message onto the global message list.          **/
    /** ELSE                                                                **/
    /*-----------------------------------------------------------------------*/
#ifdef LV_DDICT
	if (! dd_get_definition (keyword_file_ptr, keyword_ptr -> name,
		                         "ELEMENT_DEFINITION", DD_SAVE_DEFS))
	{
/* code for new part kwvtool                              dws 05-15-98*/
#ifdef LV_KWVTOOL
		value_list = lu_fetch_all_values (keyword_ptr, TRUE, FALSE);

	        if (value_list != NULL)
            {
                for (next_value = value_list; next_value != NULL;
	                                     next_value = next_value -> next)
	            {
 	                if(next_value -> text != NULL) 
	                {
						move_len = strlen(next_value -> text);
						if (move_len > 95) move_len = 95;
                        strncpy(move_temp_stor, next_value -> text, move_len);
						move_temp_stor[move_len] = '\0';
		                fprintf(keyword_file_ptr, "AAAAZ_%s %s\n", keyword_ptr -> name,
						                                   move_temp_stor);
                   }
                }
	       }

/*end test code                                                dws 05-15-98*/
#else
	     fprintf(keyword_file_ptr, "AAAAZ_%s\n", keyword_ptr -> name);
#endif
/*	     fprintf(keyword_file_ptr, "AAAAZ_%s\n", keyword_ptr -> name); */
#else
    if (! dd_get_definition (keyword_ptr -> name, 
                                 "ELEMENT_DEFINITION", DD_SAVE_DEFS))
    {
		if (pds_verbose && pds_warning) /* 01-06-03 MDC */
		{
            err_keyword_message (WARNING, keyword_ptr -> name, 
                                 pds_line_number, 0, 
                                 "Not in data dictionary");
        }
		/* 04-12-06 MDC - Check the keyword length and issue an error message
		      if it exceeds the 30 character limit.
        */
		check_keyword_length(keyword_ptr->name);
#endif
    }
    else
    { 
#ifdef LV_DDICT
/* test code for new part kwvtool                              dws 05-15-98*/
#ifdef LV_KWVTOOL
			value_list = lu_fetch_all_values (keyword_ptr, TRUE, FALSE);

	        if (value_list != NULL)
            {
                for (next_value = value_list; next_value != NULL;
	                                     next_value = next_value -> next)
	            {
 	                if(next_value -> text != NULL) 
	                {
						move_len = strlen(next_value -> text);
						if (move_len > 95) move_len = 95;
                        strncpy(move_temp_stor, next_value -> text, move_len);
						move_temp_stor[move_len] = '\0';
		                fprintf(keyword_file_ptr, "%s %s\n", keyword_ptr -> name,
						                                   move_temp_stor);
                   }
                }
	       }

/*end test code                                                dws 05-15-98*/
#else
/*		fprintf(keyword_file_ptr, "%s\n", keyword_ptr -> name);*/
		fprintf(keyword_file_ptr, "%s\n", keyword_ptr -> name );
#endif
#else
        /*-------------------------------------------------------------------*/
        /** Get the data type of the keyword from the data dictionary.      **/
        /*-------------------------------------------------------------------*/

        pds_data_type = ver_get_type_from_dd (keyword_ptr);

        /*-------------------------------------------------------------------*/
        /** Based on this data type, choose the appropriate routines to     **/
        /**    verify the keyword.                                          **/
        /*-------------------------------------------------------------------*/

        switch (pds_data_type)
        {
            case VER_INTEGER     :  success = (ver_integer_range(keyword_ptr)
                                              && ver_units (keyword_ptr));
                                   break;

            case VER_REAL        :  success = (ver_real_range (keyword_ptr)
                                              && ver_units (keyword_ptr));
                                   break;

			case VER_IDENTIFIER  :
            case VER_CHARACTER   :  success = (ver_string_length (keyword_ptr)
                                                && ver_spelling(keyword_ptr));
                                   break;

            case VER_DATE        : success = ver_date (keyword_ptr);
                                   break;

            case VER_DATE_TIME   : success = ver_date_time (keyword_ptr);
                                   break;
            
            default              : break;

        }  /*  End:  "switch (pds_data_type) ..."  */

        /*-------------------------------------------------------------------*/ 
        /** Verify the standard values for CHARACTER and INTEGER keywords.  **/
        /*-------------------------------------------------------------------*/
        if ((pds_data_type == VER_CHARACTER) || (pds_data_type == VER_INTEGER))
            success = ver_standard_values (keyword_ptr);
		else if (pds_data_type == VER_IDENTIFIER)  /* added 8-27-02 DWS*/
			success = ver_standard_values_2 (keyword_ptr);
#endif

    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if (! dd_get_definition ( ... else ..."  */

    /*-----------------------------------------------------------------------*/
    /** IF the data type is unknown THEN                                    **/
    /**    Attempt to verify the keyword using its pds_class word.              **/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

	/* 01-09-07 MDC - Removed this from lvtool as well so we don't get those annoying
	   inconsistent data type (value must be of type INTEGER or REAL) for
	   non-defined keywords.
    */
#ifndef LV_DDICT
  #ifndef LV_TOOL
    if (pds_data_type == VER_UNKNOWN)
        success = (ver_using_class_word (keyword_ptr) && success);
  #endif
#endif

    /*-----------------------------------------------------------------------*/
    /** RETURN the verify_success_flag                                      **/
    /*-----------------------------------------------------------------------*/
    return (success);

/** END **/
            
}  /*  "ver_keyword_semantics"  */

/**********************************************************************

 Routine: check_keyword_length

 Inputs:
        kwd   - Pointer to the keyword to be checked
 
 Returns:
        none

 Description:
        Routine to check that the keyword does not exceed the maximum 
		length as specified by the data dictionary (currently set at
		30 characters). If checking a local keyword, we check
		the lengths of the namespace ID and the data element name. This
		routine will issue an error message to the report if the keyword
		exceeds the character limit.
 
 Change History:
        04-13-06    MDC    Original code

 **********************************************************************/

void check_keyword_length(char *kwd)
{
	int length = 0;
	long size = 0;
	char *delimiter = NULL;
	char *id_ptr = NULL;
	char *element_ptr = NULL;
	char *local_kwd_delimiter = ":";
	char *keyword = NULL;
	char *buffer = NULL;
	char kwd_types[3][18] = {"Namespace ID",
		                     "Data Element Name",
					    	 "Keyword"};

	char msg[] = "exceeds the maximum length of";
	
	New_String(keyword, kwd);

	if( (delimiter = strstr(keyword, local_kwd_delimiter)) != NULL )
	{
		/* We have a local keyword. Check the namespace ID and
		   its data element. */

		/* First, we need to set-up the pointers and correctly parse the names */
		
		id_ptr = keyword;
		element_ptr = delimiter + 1;
		*delimiter = '\0';

		if(element_ptr == NULL || id_ptr == NULL)
		{
			Malloc_String(buffer, (String_Size(kwd) + 56));
			sprintf(buffer, "Namespace ID and/or Element name of %s could not be parsed", kwd);
			err_append_message(ERROR1, buffer);
			Lemme_Go(buffer);
			return;
		}
		/* Check the namespace ID */
		length = strlen(id_ptr);
		if(length > PDS_MAX_KWD_LENGTH)
		{
			size = String_Size(kwd_types[0]) + String_Size(id_ptr) + String_Size(msg) + String_Size("characters") + 12;
			Malloc_String(buffer, size);

			sprintf(buffer, "%s (%s) %s %d characters", kwd_types[0], id_ptr, msg, PDS_MAX_KWD_LENGTH);
            err_keyword_message (ERROR1, kwd, pds_line_number, 0, buffer);
			Lemme_Go(buffer);
		}
		length = 0;

		/* Check the data element name */
		length = strlen(element_ptr);
		if(length > PDS_MAX_KWD_LENGTH)
		{
			size = String_Size(kwd_types[1]) + String_Size(element_ptr) + String_Size(msg) + String_Size("characters") + 12;
			Malloc_String(buffer, size);

			sprintf(buffer, "%s (%s) %s %d characters", kwd_types[1], element_ptr, msg, PDS_MAX_KWD_LENGTH);
			err_keyword_message (ERROR1, kwd, pds_line_number, 0, buffer);
			Lemme_Go(buffer); 
		}
		length = 0;
	}
	else
	{
		/* We don't have a local keyword. */
		length = strlen(keyword);
		if(length > PDS_MAX_KWD_LENGTH)
		{
            size = String_Size(kwd_types[2]) + String_Size(msg) + String_Size("characters") + 12;
			Malloc_String(buffer, size);

			sprintf(buffer, "%s %s %d characters", kwd_types[2], msg, PDS_MAX_KWD_LENGTH);
			err_keyword_message (ERROR1, kwd, pds_line_number, 0, buffer);
			Lemme_Go(buffer); 
		}
	}
	Lemme_Go(keyword);
}

/**********************************************************************
 *$Component                                                          *
 *    int ver_get_type_from_class (keyword_ptr)                       *
 *$Abstract                                                           *
 *    Determine data type from keyword's pds_class word.                  *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    KEYWORD                                                         *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    keyword_ptr:                                                    *
 *        The keyword_ptr variable is a pointer to the structure      *
 *        used to represent a keyword in a PDS label.                 *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    data_type:                                                      *
 *        The data_type variable is an integer which represents the   *
 *        data type of the value of a keyword in a PDS label.  Valid  *
 *        values are: VER_UNKNOWN, VER_NULL, VER_INTEGER, VER_REAL,   *
 *        VER_CHARACTER, VER_DATE, and VER_DATE_TIME, or the logical  *
 *        OR of any combination of these values.                      *
 *$Detailed_Description                                               *
 *    The ver_get_type_from_class routine uses a keyword's pds_class      *
 *    word to determine its data type.  The pds_class word is the last    *
 *    part of the keyword's name.  For example, if the keyword is     *
 *    START_DATE, then the pds_class word is DATE.  These pds_class words     *
 *    are stored in an array which is binary searched using the pds_class *
 *    word extracted from the name of the keyword.  Once the index    *
 *    into the array is known, it is used as the index into a second  *
 *    array which contains the data type of each pds_class word.  Since   *
 *    a pds_class word may have more than one data type (VALUE, for       *
 *    instance, may be either INTEGER or REAL) they are represented   *
 *    as the logical OR of each data type.                            *
 *$Error_Handling                                                     *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.3   March 17, 1992                                            *
 *$Change_History                                                     *
 *    DPB   05-30-91   Original code.                                 *
 *    DPB   09-05-91   Changed the data types of IDENTIFICATION and   *
 *                     ID to be CHARACTER or INTEGER.                 *
 *    MDD   01-13-91   Added INTEGER as a valid data type for DATE    *
 *    MDD   03-17-92   The great int -> long conversion               *
 *    MDC   09-13-06   Changed to allow a non-defined keyword to be   *
 *                     either a REAL, INTEGER, or CHARACTER type      *
 *                     instead of just a REAL or INTEGER.             *
 **********************************************************************/

int ver_get_type_from_class (keyword_ptr)

PARAMETER keyword_ptr;

{
    char *pds_class = {NULL};
    char temp [PDS_MAXLINE];
    int data_type = {VER_UNKNOWN};
    int index = {0};

        /* NOTE:  The following variables MUST be declared as static 
         *        in order for the software to function correctly.
         */

    static long list_end = {0};
    static LOGICAL first_time_called = {TRUE};

        /* This is the pds_class word array.  It contains every pds_class word,
         * and abbreviation.  IT MUST BE KEPT IN ALPHABETICAL ORDER!
         * Also, every element in this array MUST have a corresponding
         * entry in the data_type_list array.
         */

    static char *class_list [] = 
    {
        "CNT",
        "COUNT",
        "D",
        "DATE",
        "DEFINITION",
        "DEFN",
        "DESC",
        "DESCRIPTION",
        "DIR",
        "DIRECTION",
        "DIST",
        "DISTANCE",
        "DT",
        "FLAG",
        "FLG",
        "FMT",
        "FORMAT",
        "GROUP",
        "GRP",
        "ID",
		"IDENTIFIER",                              /*added 8-27-02 DWS*/
        "IDENTIFICATION",
        "INDEX",
        "LINE",
        "MASK",
        "NAME",
        "NM",
        "NOTE",
        "NT",
        "NUM",
        "NUMBER",
        "RANGE",
        "RATIO",
        "RNG",
        "RTO",
        "SEQ",
        "SEQUENCE",
        "SET",
        "SMY",
        "SUMMARY",
        "TEXT",
        "TIME",
        "TM",
        "TYP",
        "TYPE",
        "UNIT",
        "VAL",
        "VALUE",
         NULL
    };

        /* This is the data_type_list array.  It contains the data type
         * of every pds_class word and abbreviation.  IT MUST BE KEPT IN 
         * THE SAME ORDER AS THE DATA_TYPE ARRAY!
         */

    static int data_type_list [] = 
    {
        /* CNT            */  VER_INTEGER,            
        /* COUNT          */  VER_INTEGER,            
        /* D              */  VER_CHARACTER,               
        /* DATE           */  VER_DATE | VER_INTEGER,               
        /* DEFINITION     */  VER_CHARACTER,          
        /* DEFN           */  VER_CHARACTER,          
        /* DESC           */  VER_CHARACTER,          
        /* DESCRIPTION    */  VER_CHARACTER,          
        /* DIR            */  VER_CHARACTER,          
        /* DIRECTION      */  VER_CHARACTER,          
        /* DIST           */  VER_REAL,               
        /* DISTANCE       */  VER_REAL,               
        /* DT             */  VER_DATE | VER_INTEGER,               
        /* FLAG           */  VER_INTEGER | VER_CHARACTER, 
        /* FLG            */  VER_INTEGER | VER_CHARACTER, 
        /* FMT            */  VER_CHARACTER,          
        /* FORMAT         */  VER_CHARACTER,          
        /* GROUP          */  VER_CHARACTER,          
        /* GRP            */  VER_CHARACTER,          
        /* ID             */  VER_INTEGER | VER_CHARACTER,          
		/* IDENTIFIER     */  VER_CHARACTER,                              /*added 8-27-02 DWS*/
        /* IDENTIFICATION */  VER_INTEGER | VER_CHARACTER,          
        /* INDEX          */  VER_INTEGER,            
        /* LINE           */  VER_INTEGER,            
        /* MASK           */  VER_INTEGER,            
        /* NAME           */  VER_CHARACTER,          
        /* NM             */  VER_CHARACTER,          
        /* NOTE           */  VER_CHARACTER,          
        /* NT             */  VER_CHARACTER,          
        /* NUM            */  VER_INTEGER,            
        /* NUMBER         */  VER_INTEGER,            
        /* RANGE          */  VER_REAL | VER_INTEGER, 
        /* RATIO          */  VER_REAL | VER_INTEGER, 
        /* RNG            */  VER_REAL | VER_INTEGER, 
        /* RTO            */  VER_REAL | VER_INTEGER, 
        /* SEQ            */  VER_REAL | VER_INTEGER | VER_CHARACTER, 
        /* SEQUENCE       */  VER_REAL | VER_INTEGER | VER_CHARACTER, 
        /* SET            */  VER_REAL | VER_INTEGER | VER_CHARACTER, 
        /* SMY            */  VER_CHARACTER,          
        /* SUMMARY        */  VER_CHARACTER,          
        /* TEXT           */  VER_CHARACTER,          
        /* TIME           */  VER_DATE_TIME,          
        /* TM             */  VER_DATE_TIME,          
        /* TYP            */  VER_CHARACTER,          
        /* TYPE           */  VER_CHARACTER,          
        /* UNIT           */  VER_CHARACTER,          
        /* VAL            */  VER_REAL | VER_INTEGER, 
        /* VALUE          */  VER_REAL | VER_INTEGER, 
        /* others         */  NULL
    };

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** IF this is the first time this routine has been called THEN         **/
    /**     Find the index of the last element in the pds_class word array.     **/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    if (first_time_called)
    {
        first_time_called = FALSE;
	for (list_end = 0; class_list [list_end] != NULL; ++list_end) ;
        --list_end;

    }  /*  End:  "if (first_time_called) ..."  */

    /*-----------------------------------------------------------------------*/
    /** Extract the pds_class word from the name of the keyword passed in.      **/
    /**    This is done in the following steps:                             **/
    /**       1)  Copy the keyword's name into a temporary variable         **/
    /**       2)  Remove any trailing numbers and terminate the string      **/
    /**       3)  Find the underscore character which preceeds the last     **/
    /**           word in the name                                          **/
    /**       4)  Set a character pointer to point to the character         **/
    /**           immediatly following the underscore                       **/
    /*-----------------------------------------------------------------------*/

    strcpy (temp, keyword_ptr -> name);
    for (pds_class = String_End(temp); 
        ((pds_class >= temp) && ((*pds_class == '_') || (isdigit(*pds_class)))); --pds_class) ;
    *(pds_class + 1) = EOS;
    for ( ; ((pds_class >= temp) && (*pds_class != '_')); --pds_class) ;
    ++pds_class;

    /*-----------------------------------------------------------------------*/
    /** Search the pds_class word array and find the index of the element       **/
    /**     which corresponds to the keyword's pds_class word.                  **/
    /*-----------------------------------------------------------------------*/

    index = (int) search_string_array (class_list, list_end, pds_class);

    /*-----------------------------------------------------------------------*/
    /** IF the pds_class word was found in the array THEN                       **/
    /**     Extract the data type from the type list array.                 **/
    /** ELSE                                                                **/
    /**     IF the pds_class word ends in an 'S' THEN                           **/
    /**         The data type is INTEGER.                                   **/
    /**     ELSE                                                            **/
    /**         The data type is either INTEGER or REAL.                    **/
    /**     ENDIF                                                           **/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    if (index != PDS_SEARCH_FAIL) 
        data_type = data_type_list [index];
    else
	    /* 09-13-06 MDC - Changed data type to default to a REAL, INTEGER, or CHARACTER
		   instead of just REAL or INTEGER for non-defined keywords.
        */
		data_type = VER_REAL | VER_INTEGER | VER_CHARACTER;
/*    
        if (*(String_End(pds_class)) == 'S')
            data_type = VER_INTEGER;
        else
            data_type = VER_REAL | VER_INTEGER;
*/
    /*-----------------------------------------------------------------------*/
    /** RETURN the data type                                                **/
    /*-----------------------------------------------------------------------*/
    
    return (data_type);

/** END **/
            
}  /*  "ver_get_type_from_class"  */



/**********************************************************************
 *$Component                                                          *
 *    int ver_get_type_from_dd (keyword_ptr)                          *
 *$Abstract                                                           *
 *    Determine data type from data dictionary                        *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    KEYWORD                                                         *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    keyword_ptr:                                                    *
 *        The keyword_ptr variable is a pointer to the structure      *
 *        used to represent a keyword in a PDS label.                 *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    data_type:                                                      *
 *        The data_type variable is an integer which represents the   *
 *        data type of the value of a keyword in a PDS label.  Valid  *
 *        values are: VER_UNKNOWN, VER_NULL, VER_INTEGER, VER_REAL,   *
 *        VER_CHARACTER, VER_DATE, and VER_DATE_TIME, or the logical  *
 *        OR of any combination of these values.                      *
 *$Detailed_Description                                               *
 *    The ver_get_type_from_dd routine uses the data dictionary       *
 *    information on the keyword passed in to determine its data      *
 *    type.  If the information is not in the data dictionary, then   *
 *    VER_UNKNOWN is returned.                                        *
 *$Error_Handling                                                     *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.0   May 30, 1991                                              *
 *$Change_History                                                     *
 *    DPB   05-30-91   Original code.                                 *
 **********************************************************************/

int ver_get_type_from_dd (keyword_ptr)

PARAMETER keyword_ptr;

{
    int data_type = {VER_UNKNOWN};
    char *type_string = {NULL};

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Attempt to fetch the data type string from the data dictionary.     **/
    /*-----------------------------------------------------------------------*/

    type_string = dd_get_data_type (keyword_ptr -> name);
        
    /*-----------------------------------------------------------------------*/
    /** IF the attempt was successful THEN                                  **/
    /*-----------------------------------------------------------------------*/

    if (type_string != NULL)
    {        
        /*-------------------------------------------------------------------*/
        /** Determine the actual data type from the data type string.       **/
        /*-------------------------------------------------------------------*/

        if (strcmp (type_string, "NULL") == 0)
            data_type = VER_NULL;
        else 
          if (strcmp (type_string, "CHARACTER") == 0)
              data_type = VER_CHARACTER;
          else 
            if (strcmp (type_string, "ALPHABET") == 0)
                data_type = VER_CHARACTER;
            else 
              if (strcmp (type_string, "ALPHANUMERIC") == 0)
                  data_type = VER_CHARACTER;
              else 
                if (strcmp (type_string, "INTEGER") == 0)
                    data_type = VER_INTEGER;
                else 
                  if (strcmp (type_string, "NON_DECIMAL") == 0)
                      data_type = VER_INTEGER;
                  else 
                    if (strcmp (type_string, "REAL") == 0)
                        data_type = VER_REAL;
                    else 
                      if (strcmp (type_string, "DATE") == 0)
                          data_type = VER_DATE;
                      else 
                        if (strcmp (type_string, "TIME") == 0)
                            data_type = VER_DATE_TIME;
						else
							if(strcmp (type_string, "IDENTIFIER") == 0) /*added 8-27-02 DWS*/
								data_type = VER_IDENTIFIER;

    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if (type_string != NULL) ..."  */

    /*-----------------------------------------------------------------------*/
    /** RETURN the data type                                                **/
    /*-----------------------------------------------------------------------*/
    
    Lemme_Go(type_string);
    return (data_type);

/** END **/

}  /*  "ver_get_type_from_dd"  */



/**********************************************************************
 *$Component                                                          *
 *    int ver_get_type_from_odl (value_ptr)                           *
 *$Abstract                                                           *
 *    Determine data type from the odl structure.                     *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    KEYWORD                                                         *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    value_ptr:                                                      *
 *        The value_ptr variable is a pointer to a VALUE              *
 *        structure, which represents the value of a keyword          *
 *        in a PDS label.                                             *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    data_type:                                                      *
 *        The data_type variable is an integer which represents the   *
 *        data type of the value of a keyword in a PDS label.  Valid  *
 *        values are: VER_UNKNOWN, VER_NULL, VER_INTEGER, VER_REAL,   *
 *        VER_CHARACTER, VER_DATE, and VER_DATE_TIME, or the logical  *
 *        OR of any combination of these values.                      *
 *$Detailed_Description                                               *
 *    The ver_get_type_from_odl routine determines the data type of   *
 *    the value of a keyword in a PDS label from the ODL structure    *
 *    used to store the value.                                        *
 *$Error_Handling                                                     *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   June 11, 1991                                             *
 *$Change_History                                                     *
 *    DPB   05-30-91   Original code.                                 *
 *    MDD   06-11-91   Changed to return VER_UNKNOWN when ODL type    *
 *                     is TV_TIME.                                    *
 **********************************************************************/

int ver_get_type_from_odl (value_ptr)

VALUE value_ptr;

{
    int data_type = VER_UNKNOWN;

    switch (value_ptr -> item.type)
    {
        case TV_INTEGER   : data_type = VER_INTEGER;
                            break;

        case TV_REAL      : data_type = VER_REAL;
                            break;

        case TV_SYMBOL    : data_type = VER_CHARACTER;
                            break;

        case TV_STRING    : data_type = VER_CHARACTER;
                            break;

        case TV_DATE      : data_type = VER_DATE;
                            break;

        case TV_TIME      : data_type = VER_UNKNOWN;
                            break;

        case TV_DATE_TIME : data_type = VER_DATE_TIME;
                            break;

        case TV_NULL      : data_type = VER_NULL;
                            break;

        default           : data_type = VER_UNKNOWN;
                            break;

    }  /*  End:  "switch (value_ptr -> item.type) ..."  */

    return (data_type);

}  /*  "ver_get_type_from_odl"  */


                     
/**********************************************************************
 *$Component                                                          *
 *    char *ver_get_type_string (data_type)                           *
 *$Abstract                                                           *
 *    Returns a string corresponding to the data_type.                *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    KEYWORD                                                         *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    data_type:                                                      *
 *        The data_type variable is an integer which represents the   *
 *        data type of the value of a keyword in a PDS label.  Valid  *
 *        values are: VER_UNKNOWN, VER_NULL, VER_INTEGER, VER_REAL,   *
 *        VER_CHARACTER, VER_DATE, and VER_DATE_TIME, or the logical  *
 *        OR of any combination of these values.                      *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    data_type_string:                                               *
 *        The data_type_string variable is a character string which   *
 *        describes the data type of a keyword in a pds label.        *
 *        Valid values are: "UNKNOWN", "NULL", "INTEGER", "REAL",     *
 *        "CHARACTER", "DATE", and "DATE_TIME", or any combination of *
 *        these, such as "INTEGER or REAL or CHARACTER".              *
 *$Detailed_Description                                               *
 *    The ver_get_type_string routine takes an integer value          *
 *    representing the logical OR of all the possible data types      *
 *    that are valid for a particular keyword, and constructs a       *
 *    string describing these data types.  For example, if the data   *
 *    type passed in is VER_REAL | VER_INTEGER, then the string       *
 *    which is returned will contain "REAL or INTEGER".               *
 *$Error_Handling                                                     *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   October 22, 1991                                          *
 *$Change_History                                                     *
 *    DPB   05-30-91   Original code.                                 *
 *    MDD   10-22-91   Fixed free, malloc, sys_exit_system calls.     *
 **********************************************************************/

char *ver_get_type_string (data_type)

int data_type;

{
    char *type_string = {NULL};
    LOGICAL other_types = {FALSE};

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Initialize the data type string.                                    **/
    /*-----------------------------------------------------------------------*/

    Malloc_String(type_string, PDS_MAXLINE);

    /*-----------------------------------------------------------------------*/
    /** IF the data type is unknown THEN                                    **/
    /**     Set the data type string to be "UNKNOWN".                       **/
    /** ELSE                                                                **/
    /*-----------------------------------------------------------------------*/

    if (data_type == VER_UNKNOWN)
        strcpy (type_string, "UNKNOWN");
    else
    {
        /*-------------------------------------------------------------------*/
        /** IF the data type is NULL THEN                                   **/
        /**     Set the data type string to be "NULL".                      **/
        /** ELSE                                                            **/
        /*-------------------------------------------------------------------*/

        if (data_type == VER_NULL)
            strcpy (type_string, "NULL");
        else
        {
            /*---------------------------------------------------------------*/
            /** Append the appropriate string (plus an "or", if necessary)  **/
            /**     onto the type string.                                   **/
            /*---------------------------------------------------------------*/

            if ((data_type & VER_REAL) == VER_REAL)
            {
                if (other_types)
                    strcat (type_string, " or ");

                strcat (type_string, "REAL");
                other_types = TRUE;
        
            }  /*  End:  "if ((data_type & VER_REAL) ..."  */
            
            if ((data_type & VER_INTEGER) == VER_INTEGER)
            {
                if (other_types)
                    strcat (type_string, " or ");

                strcat (type_string, "INTEGER");
                other_types = TRUE;
        
            }  /*  End:  "if ((data_type & VER_INTEGER) ..."  */
            
            if ((data_type & VER_CHARACTER) == VER_CHARACTER)
            {
                if (other_types)
                    strcat (type_string, " or ");

                strcat (type_string, "CHARACTER");
                other_types = TRUE;
        
            }  /*  End:  "if ((data_type & VER_CHARACTER) ..."  */
            
            if ((data_type & VER_DATE) == VER_DATE)
            {
                if (other_types)
                    strcat (type_string, " or ");

                strcat (type_string, "DATE");
                other_types = TRUE;
        
            }  /*  End:  "if ((data_type & VER_DATE) ..."  */
            
            if ((data_type & VER_DATE_TIME) == VER_DATE_TIME)
            {
                if (other_types)
                    strcat (type_string, " or ");

                strcat (type_string, "DATE_TIME");
                other_types = TRUE;
        
            }  /*  End:  "if ((data_type & VER_DATE_TIME) ..."  */
    
        /*-------------------------------------------------------------------*/
        /** ENDIF                                                           **/
        /*-------------------------------------------------------------------*/

        }  /*  End:  "if (data_type == VER_NULL) ... else ..."  */

    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if (data_type == VER_UNKNOWN) ... else ..."  */

    /*-----------------------------------------------------------------------*/
    /** RETURN the data type string                                         **/
    /*-----------------------------------------------------------------------*/
    
    return (type_string);

/** END **/

}  /*  "ver_get_type_string"  */


                     
/**********************************************************************
 *$Component                                                          *
 *    char *ver_get_value_string (value_ptr, value_count)             *
 *$Abstract                                                           *
 *    Extracts the value of a keyword from its odl structure          *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    KEYWORD                                                         *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    value_ptr:                                                      *
 *        The value_ptr variable is a pointer to a VALUE              *
 *        structure, which represents the value of a keyword          *
 *        in a PDS label.                                             *
 *    value_count:                                                    *
 *        The value_count variable is the index, within a keyword,    *
 *        of a value (e.g., the third value, first value, etc.).      *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    value_string:                                                   *
 *        The value_string variable is a character string which       *
 *        contains the value of a keyword in a PDS label.             *
 *$Detailed_Description                                               *
 *    The ver_get_value_string routine attempts to fetch the value    *
 *    of a keyword from its ODL structure.  Before it does this, it   *
 *    verifies that the value is valid.  It also checks the data type *
 *    in the odl structure against the one fetched from the data      *
 *    dictionary.                                                     *
 *$Error_Handling                                                     *
 *    1)  If the value contains syntax errors, then an error message  *
 *        is appended onto the global message list and NULL is        *
 *        returned.                                                   *
 *    2)  If the value cannot be extracted from the structure, then   *
 *        an error message is appended onto the global message list   *
 *        and NULL is returned.                                       *
 *    3)  If (here's where it gets tricky) the odl data type does not *
 *        match the data dictionary data type, and the DD type is     *
 *        neither NULL nor UNKNOWN, and the odl type is not NULL then *
 *        a warning message is appended onto the global message list  *
 *        and the value is returned (if the DD type was CHARACTER),   *
 *        or an error message is appended onto the global message     *
 *        list and NULL is returned (if the DD type wasn't).          *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$External_References                                                *
 *    Item                     Shared-Data           Access           *
 *    ------------------------------------------------------------    *
 *    pds_line_number          verlabel.c            read             *
 *    pds_data_type            verlabel.c            read             *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.4   July 9, 1992                                              *
 *$Change_History                                                     *
 *    DPB   05-30-91   Original code.                                 *
 *    MDD   06-11-91   Added case to handle INTEGERS and incomplete   *
 *                     values in DATE and DATE_TIME fields            *
 *    KLM   06-18-91   Changed call of lab_fetch_value to             *
 *                     lu_fetch_value.                                *
 *    DPB   09-27-91   Added extra checks to catch null and unknown   *
 *                     dates.                                         *
 *    MDD   10-21-91   Fixed malloc, free, and sys_exit_system calls  *
 *    MDD   03-20-92   The great int -> long conversion               *
 *    MDD   07-09-92   Removed the character conversion warning       *
 *    DWS   08-28-02   added ver indentifier test                     *
 **********************************************************************/

char *ver_get_value_string (value_ptr, value_count)

VALUE value_ptr;
long value_count;

{
    char *value_string = {NULL};
    char *type_string = {NULL};
    char keyword_name [PDS_MAXLINE];
    char err_msg [PDS_MAXLINE];
    int odl_data_type;
    LOGICAL null_string = {FALSE};

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** IF the value contains syntax errors THEN                            **/
    /**     Append an error message onto the global list of messages.       **/
    /** ELSE                                                                **/
    /*-----------------------------------------------------------------------*/

    strcpy (keyword_name, value_ptr -> parameter -> name);

    if (value_ptr -> item.valid <= 0)
    {
        err_keyword_message (ERROR1, keyword_name, 
                             pds_line_number, value_count, 
                            "Invalid value.  (Contains syntax errors)");
    }
    else
    {                        
        /*-------------------------------------------------------------------*/
        /** Attempt to fetch the value from the label.                      **/
        /*-------------------------------------------------------------------*/

        value_string = lu_fetch_value (value_ptr, FALSE);
    
        /*-------------------------------------------------------------------*/
        /** IF the value is not NULL, AND the DD data type is not UNKNOWN   **/
        /**         or NULL, AND the value is not a string interpreted      **/
        /**         as NULL THEN                                            **/
        /*-------------------------------------------------------------------*/

        null_string = (IsNull(value_string) || 
           ((pds_data_type == VER_DATE) && (IsNullDate(value_string))) ||
           ((pds_data_type == VER_DATE_TIME) && (IsNullDate(value_string))));

        if ((value_string != NULL) && (! null_string) && 
            (pds_data_type != VER_NULL) && (pds_data_type != VER_UNKNOWN))
        {
            /*---------------------------------------------------------------*/
            /** Get the ODL data type                                       **/
            /*---------------------------------------------------------------*/

            odl_data_type = ver_get_type_from_odl (value_ptr);

            /*---------------------------------------------------------------*/
            /** IF the ODL data type doesn't match the data dictionary data **/
            /**         type THEN                                           **/
            /*---------------------------------------------------------------*/

            if (pds_data_type != odl_data_type)
            {
                /*-----------------------------------------------------------*/
                /** IF the data dictionary doesn't say the value is         **/
                /**    CHARACTER THEN                                       **/
                /*-----------------------------------------------------------*/
														/*added ver indentifier test 08-28-02 DWS */
                if ((pds_data_type != VER_CHARACTER) && (pds_data_type != VER_IDENTIFIER))
                {
                    /*-------------------------------------------------------*/
                    /** IF it's not REAL, OR there is a non-numeric         **/
                    /**         value in a REAL field THEN                  **/
                    /*-------------------------------------------------------*/

                    if ((pds_data_type != VER_REAL) ||
                            ((pds_data_type == VER_REAL) && 
                             (odl_data_type != VER_INTEGER)))

                    {
                       if (!((pds_data_type == VER_DATE || 
                              pds_data_type == VER_DATE_TIME) 
                                 && (odl_data_type == VER_INTEGER || 
                                     odl_data_type == VER_DATE)))
                       {
                           /*------------------------------------------------*/
                           /** It's not OK.  Get rid of the value and append**/
                           /**     an error message onto the global list of **/
                           /**     messages.                                **/
                           /*------------------------------------------------*/
    
                           Lemme_Go(value_string);
     
                           type_string = ver_get_type_string (pds_data_type);
                           sprintf (err_msg, 
                                    "Inconsistent data type.  (Value should be of type %s)", 
                                    type_string);
                           err_keyword_message (ERROR1, keyword_name, 
                                                pds_line_number, 
                                                value_count, err_msg);
                           Lemme_Go(type_string);

                       /*----------------------------------------------------*/
                       /** ENDIF                                            **/
                       /*----------------------------------------------------*/

		       }

                    /*-------------------------------------------------------*/
                    /** ENDIF                                               **/
                    /*-------------------------------------------------------*/
    
                    }  /*  End:  "if ((pds_data_type != VER_REAL) || ..."  */

                /*-----------------------------------------------------------*/
                /** ENDIF                                                   **/
                /*-----------------------------------------------------------*/

                }  /*  End:  "if (pds_data_type == ... else ..."  */

            /*---------------------------------------------------------------*/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

            }  /*  End:  "if (pds_data_type != odl_data_type) ... else ..."  */

        /*-------------------------------------------------------------------*/
        /** ENDIF                                                           **/
        /*-------------------------------------------------------------------*/

        }  /*  End:  "if (value_string == NULL) ... else ..."  */

    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if (value_ptr -> item.valid <= 0) ... else ..."  */

    /*-----------------------------------------------------------------------*/
    /** RETURN the keyword's value                                          **/
    /*-----------------------------------------------------------------------*/
    
    return (value_string);

/** END **/

}  /*  "ver_get_value_string"  */



/**********************************************************************
 *$Component                                                          *
 *    LOGICAL ver_date (keyword_ptr)                                  *
 *$Abstract                                                           *
 *    Verifys the date.                                               *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    LABEL                                                           *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    keyword_ptr:                                                    *
 *        The keyword_ptr variable is a pointer to the structure      *
 *        used to represent an object in a PDS label.                 *
 *$Outputs                                                            *
 *    NONE                                                            *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure        *
 *        (a value of FALSE).                                         *
 *$Detailed_Description                                               *
 *    The ver_date routine checks the date value to verify that       *
 *    the day of year value is valid in a year-day-of-year format,    *
 *    or checks the month and day values in a year-month-day format.  *
 *    The year field is not verified since there is no range to       *
 *    check. If invalid date fields are found, error messages are     * 
 *    appended to the global list. This routine returns TRUE if no    *
 *    errors are found.                                               * 
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Kristy L. Marski /JPL                                           *
 *$Version_and_Date                                                   *
 *    1.2   Macrh 20, 1992                                            *
 *$Change_history                                                     *
 *    KLM   04-08-91   Original code.                                 *
 *    MDD   10-21-91   Fixed free calls.                              *
 *    MDD   03-20-92   The great int -> long conversion               *
 **********************************************************************/

LOGICAL ver_date (keyword_ptr)

PARAMETER keyword_ptr;

{
    VALUE value_ptr = {NULL};
    char *value_string = {NULL};
    long value_count = {0};
    LOGICAL date_success = {TRUE};
    LOGICAL success = {TRUE};
    LOGICAL multiple_values = {FALSE};

/** BEGIN **/

    /*----------------------------------------------------------------*/
    /** LOOP thru the value structures until there aren't any more   **/
    /*----------------------------------------------------------------*/
                                       
    multiple_values = (NextValue(FirstValue(keyword_ptr)) != NULL);

    for (value_ptr = FirstValue (keyword_ptr);
            value_ptr != NULL; value_ptr = NextValue (value_ptr))
    {
        if (multiple_values)
            ++value_count;

       /*-------------------------------------------------------------*/
       /** Get the value from the value structure                    **/
       /*-------------------------------------------------------------*/
                                                                  
       value_string = ver_get_value_string (value_ptr, value_count);

       /*-------------------------------------------------------------*/
       /** IF the value is not NULL AND                              **/
       /**     is not "N/A", "UNK", or "NULL" THEN                   **/
       /*-------------------------------------------------------------*/

       if ((value_string != NULL) && (! IsNullDate(value_string)))
       {
           date_success = ver_date_string (keyword_ptr -> name, 
                                           value_string, value_count); 
           if (date_success == FALSE)
           {
              success = FALSE;
           }
                                                                       
       /*-------------------------------------------------------------*/
       /** ENDIF value_string != NULL ...                            **/
       /*-------------------------------------------------------------*/
      
       }  /*  End:  "if ((value_string != NULL) ..."  */

       /*-------------------------------------------------------------*/
       /** Free the value                                            **/
       /*-------------------------------------------------------------*/

       Lemme_Go(value_string);

    /*----------------------------------------------------------------*/
    /** ENDLOOP thru the values ...                                  **/
    /*----------------------------------------------------------------*/

    }  /*  End:  "for (value_ptr = FirstValue ( ..."  */

    /*----------------------------------------------------------------*/
    /** Return success flag                                          **/
    /*----------------------------------------------------------------*/

    return (success);

/** END **/

}  /*  End: "ver_date"  */



/**********************************************************************
 *$Component                                                          *
 *    LOGICAL ver_date_string (keyword_name, date_string, value_count)*
 *$Abstract                                                           *
 *    Verifys the date.                                               *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    LABEL                                                           *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    keyword_name:                                                   *
 *        The keyword_name variable is a character string             *
 *        which contains the name of a keyword in a PDS label         *
 *        (e.g., the line "SCID = VG1" implies that "SCID" is the     *
 *        keyword name).                                              *
 *    date_string:                                                    *
 *        The date_string variable is a character string that         *  
 *        contains a date in either YYYY-MM-DD or YYYY-DDD format.    *
 *    value_count:                                                    *
 *        The value_count variable is the index, within a keyword,    *
 *        of a value (e.g., the third value, first value, etc.).      *
 *$Outputs                                                            *
 *    NONE                                                            *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure        *
 *        (a value of FALSE).                                         *
 *$Detailed_Description                                               *
 *    The ver_date routine checks the date value to verify that       *
 *    the day of year value is valid in a year-day of year format,    *
 *    or checks the month and day values in a year-month-day format.  *
 *    The year field is not verified since there is no range to       *
 *    check. If invalid date fields are found, error messages are     * 
 *    appended to the global list. This routine returns TRUE if no    *
 *    errors are found.                                               * 
 *    in the units expression.                                        *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Kristy L. Marski /JPL                                           *
 *$Version_and_Date                                                   *
 *    1.2   March 20, 1992                                            *
 *$Change_history                                                     *
 *    KLM   05-08-91   Original code.                                 *
 *    MDD   06-11-91   Added ver_pad_date                             *
 *    MDD   10-20-92   The great int -> long conversion               *
 **********************************************************************/

LOGICAL ver_date_string (keyword_name, date_string, value_count)

char *keyword_name;
char *date_string;
long value_count;

{

    LOGICAL success = {TRUE};
    LOGICAL leap_year = {FALSE};
    int num, year, month_or_doy, day;
    char *new_date_string = {NULL};
    static int month_map [12] = {31, 28, 31, 30, 31, 30,
                                 31, 31, 30, 31, 30, 31};

/** BEGIN **/

    /*---------------------------------------------------------------------*/
    /** Pad the date to the maximum length                                **/
    /** Scan the date_string to determine if it is in a year-day of year  **/
    /**    or a year-month-day format.                                    **/
    /*---------------------------------------------------------------------*/
    
    Malloc_String(new_date_string, (int) String_Size(date_string));
    strcpy (new_date_string, date_string);
    new_date_string = ver_pad_date (new_date_string);
    num = sscanf (new_date_string, "%d-%d-%d", &year, &month_or_doy, &day);

    /*---------------------------------------------------------------------*/
    /** IF it is a leap year                                              **/
    /**    Set leap_year to TRUE                                          **/
    /** ENDIF                                                             **/ 
    /*---------------------------------------------------------------------*/

    if (year % 4 == 0)
    {
       if (year % 100 == 0 && year % 400 != 0)
       {
          leap_year = FALSE;
       }
       else 
       {
          leap_year = TRUE;
       }
    }                      

    /*---------------------------------------------------------------------*/
    /** IF the date is in a year-day of year format                       **/
    /*---------------------------------------------------------------------*/

    if (num == 2)
    {   
       /*------------------------------------------------------------------*/
       /** IF it is a leap year                                           **/
       /*------------------------------------------------------------------*/

       if (leap_year)
       {
        
          /*---------------------------------------------------------------*/
          /** IF the day of year is not between 1 and 366                 **/
          /**    Append error message to the error list                   **/
          /**    Set success to FALSE                                     **/
          /** ENDIF                                                       **/
          /*---------------------------------------------------------------*/

          if (!(1 <= month_or_doy && month_or_doy <= 366))
          {
             success = err_keyword_message (ERROR1, keyword_name, 
                                            pds_line_number, value_count, 
                                        "Invalid value for day-of-year field");
          }
       }

       /*------------------------------------------------------------------*/
       /** ELSE                                                           **/
       /**    IF the day of year is not between 1 and 365                 **/
       /**       Append error message to the error list                   **/
       /**       Set success to FALSE                                     **/
       /**    ENDIF                                                       **/
       /** ENDIF                                                          **/
       /*------------------------------------------------------------------*/
       
       else
       {
          if (!(1 <= month_or_doy && month_or_doy <= 365))
          {
             success = err_keyword_message (ERROR1, keyword_name, 
                                            pds_line_number, value_count, 
                                         "Invalid value for day-of-year field");
          }

       } /* else not a leap year */

    } /* if num == 2 */

    /*---------------------------------------------------------------------*/
    /** ELSE                                                              **/
    /**    IF the date is in a year-month-day format                      **/
    /*---------------------------------------------------------------------*/

    else if (num == 3)
    {

       /*---------------------------------------------------------------*/
       /** IF the month is between 1 and 12                            **/
       /*---------------------------------------------------------------*/

       if (1 <= month_or_doy && month_or_doy <= 12)          
       {

          /*------------------------------------------------------------*/
          /** IF it is a leap year and month is February               **/
          /*------------------------------------------------------------*/
        
          if (leap_year && month_or_doy  == 2)
          {
          
             /*------------------------------------------------------*/
             /** IF the the day is not between 1 and 29 THEN        **/
             /**    Append error message to the error list          **/
             /**    Set success to FALSE                            **/
             /** ENDIF (day is not between 1 and 29)                **/
             /*------------------------------------------------------*/
                
             if (!(1 <= day && day <= 29))
             {
                 success = err_keyword_message (ERROR1, keyword_name, 
                                                pds_line_number, value_count, 
                                               "Invalid value for day field");
             } 
          } /* IF leap year and month is 2 */

          /*------------------------------------------------------------*/
          /** ELSE                                                     **/
          /**    IF the day is not between 1 and the correct days for  **/
          /**         that month THEN                                  **/
          /**       Append error message to the error list             **/
          /**       Set success to FALSE                               **/
          /**    ENDIF (the day is not between 1 and ...)              **/
          /** ENDIF (it is a leap year and month is 2)                 **/
          /*------------------------------------------------------------*/

          else
          {
            
             if (!(1 <= day && day <= month_map[month_or_doy - 1]))
             {
                success = err_keyword_message (ERROR1, keyword_name, 
                                               pds_line_number, value_count, 
                                              "Invalid value for day field");
             } 
          } 
       } /* IF month between 1 and 12 */

       /*---------------------------------------------------------------*/
       /** ELSE                                                        **/
       /**    Append error message to the error list                   **/
       /**    Set success to FALSE                                     **/
       /** ENDIF (month is between 1 and 12)                           **/
       /*---------------------------------------------------------------*/

       else
       {
          success = err_keyword_message (ERROR1, keyword_name, 
                                         pds_line_number, value_count, 
                                        "Invalid value for month field");
       } 

    } /* else if num is 3 */                                    

    /*---------------------------------------------------------------------*/
    /** ELSE                                                              **/
    /**    Append error message to the error list                         **/
    /**    Set success to FALSE                                           **/
    /** ENDIF (date is in the year-day of year format)                    **/
    /*---------------------------------------------------------------------*/

    else 
    {
       success = err_keyword_message (ERROR1, keyword_name, 
                                      pds_line_number, value_count, 
                                     "Invalid date string");
    }
    /*---------------------------------------------------------------------*/
    /** Return success flag                                               **/
    /*---------------------------------------------------------------------*/

    Lemme_Go(new_date_string);
    return (success);

/** END **/

}  /*  End: "ver_date_string"  */



/**********************************************************************
 *$Component                                                          *
 *    LOGICAL ver_date_time (keyword_ptr)                             *
 *$Abstract                                                           *
 *    Verifys the date and time.                                      *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    LABEL                                                           *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    keyword_ptr:                                                    *
 *        The keyword_ptr variable is a pointer to the structure      *
 *        used to represent an object in a PDS label.                 *
 *$Outputs                                                            *
 *    NONE                                                            *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure        *
 *        (a value of FALSE).                                         *
 *$Detailed_Description                                               *
 *    The ver_date_time routine checks the date_time value to         *
 *    verify that all the date and time fields are within their       *
 *    specified ranges.                                               *
 *    The year field is not verified since there is no range to       *
 *    check. If invalid date or time fields are found, error messages *
 *    are appended to the global list. This routine returns TRUE if   *
 *    no errors are found.                                            * 
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Kristy L. Marski /JPL                                           *
 *$Version_and_Date                                                   *
 *    1.3   March 20, 1992                                            *
 *$Change_history                                                     *
 *    KLM   04-09-91   Original code.                                 *
 *    MDD   06-11-91   Added ver_pad_date_time                        *
 *    MDD   10-21-91   Fixed free calls.                              *
 *    MDD   03-20-92   The great int -> long conversion               *
 **********************************************************************/

LOGICAL ver_date_time (keyword_ptr)

PARAMETER keyword_ptr;

{

    VALUE value_ptr = {NULL};
    char *value_string = {NULL};
    long value_count = {0};
    LOGICAL success = {TRUE};
    LOGICAL date_success = {FALSE};
    LOGICAL time_success = {FALSE};
    LOGICAL multiple_values = {FALSE};
    char temp_char = 'T';
    char *temp_ptr;
    char *date_part;
    char *time_part;
                                                    

/** BEGIN **/

    /*----------------------------------------------------------------*/
    /** LOOP thru the value structures until there aren't any more   **/
    /*----------------------------------------------------------------*/
                                       
    multiple_values = (NextValue(FirstValue(keyword_ptr)) != NULL);

    for (value_ptr = FirstValue (keyword_ptr);
            value_ptr != NULL; value_ptr = NextValue (value_ptr))
    {
        if (multiple_values)
            ++value_count;

       /*-------------------------------------------------------------*/
       /** Get the value from the value structure                    **/
       /** Pad the value to the maximum date/time length             **/
       /*-------------------------------------------------------------*/

       value_string = ver_get_value_string (value_ptr, value_count);

       /*-------------------------------------------------------------*/
       /** IF the value is not NULL AND                              **/
       /**     is not "N/A", "UNK", or "NULL" THEN                   **/
       /*-------------------------------------------------------------*/

       if ((value_string != NULL) && (! IsNullDate(value_string)))
       {

          /*----------------------------------------------------------------*/
          /** Call util_save_to_last_occurrence to get the date part of    **/
          /**   string up to the 'T' separator                             **/
          /** Call util_remove_char to remove the 'T' from the date part   **/
          /*----------------------------------------------------------------*/
                               
          value_string = ver_pad_date_time (value_string);
          date_part = util_save_to_last_occurrence (value_string, temp_char);
          util_remove_char (date_part, temp_char);

          /*----------------------------------------------------------------*/
          /** Set the temp_ptr to the character just after the 'T' in the  **/
          /**    original date_time_string                                 **/
          /** Copy the string from temp_ptr to the EOS into the time part  **/
          /*----------------------------------------------------------------*/

          temp_ptr = value_string + String_Size(date_part);
          time_part = (char *)malloc (String_Size(value_string));
          strcpy (time_part, temp_ptr);

          /*----------------------------------------------------------------*/
          /** Call ver_date_string to verify the date part                 **/
          /** Call ver_time_string to verify the time part                 **/
          /*----------------------------------------------------------------*/
                                                   
          date_success = ver_date_string (keyword_ptr -> name, 
                                          date_part, value_count);
          time_success = ver_time_string (keyword_ptr -> name, 
                                          time_part, value_count);

          /*----------------------------------------------------------------*/
          /** Free the string allocated in util_save_to_last_occurrence    **/
          /** Free the time part.                                          **/
          /*----------------------------------------------------------------*/

          Lemme_Go(date_part);
          Lemme_Go(time_part);                                                 

          /*----------------------------------------------------------------*/
          /** IF the date was correct and the time was correct             **/
          /**    verification was successful                               **/
          /** ELSE                                                         **/
          /**    verification failed                                       **/
          /** ENDIF                                                        **/
          /*----------------------------------------------------------------*/

          if (! (date_success && time_success))
          {
             success = FALSE;
          }

       /*-------------------------------------------------------------*/
       /** ENDIF value_string != NULL ...                            **/
       /*-------------------------------------------------------------*/
      
       }  /*  End:  "if ((value_string != NULL) ..."  */

       /*-------------------------------------------------------------*/
       /** Free the value                                            **/
       /*-------------------------------------------------------------*/

       Lemme_Go(value_string);

    /*----------------------------------------------------------------*/
    /** ENDLOOP thru the values ...                                  **/
    /*----------------------------------------------------------------*/

    }  /*  End:  "for (value_ptr = FirstValue ( ..."  */
    
    /*----------------------------------------------------------------*/
    /** Return success/failure flag                                  **/
    /*----------------------------------------------------------------*/

    return (success);

/** END **/

}  /*  End: "ver_date_time"  */

                                                                    

/**********************************************************************
 *$Component                                                          *
 *    LOGICAL ver_integer_range (keyword_ptr)                         *
 *$Abstract                                                           *
 *    Verifies the range of an integer value.                         *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    KEYWORD                                                         *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    keyword_ptr:                                                    *
 *        The keyword_ptr variable is a pointer to the structure      *
 *        used to represent a keyword in a PDS label.                 *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    verify_success_flag:                                            *
 *        The verify_success_flag variable is a TRUE/FALSE value      *
 *        which indicates whether or not a keyword or object has been *
 *        successfully verified.                                      *
 *$Detailed_Description                                               *
 *    The ver_integer_range routine fetches the maximum and mimimum   *
 *    values for an integer keyword from the data dictionary and      *
 *    compares them against the value of the keyword.                 *
 *$Error_Handling                                                     *
 *    If the value is outside the range specified in the data         *
 *    dictionary, then an error message is appended onto the global   *
 *    list of messages.                                               *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$External_References                                                *
 *    Item                     Shared-Data           Access           *
 *    ------------------------------------------------------------    *
 *    pds_line_number          verlabel.c            read             *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.2   October 22, 1992                                          *
 *$Change_History                                                     *
 *    DPB   05-30-91   Original code.                                 *
 *    MDD   03-20-92   The great int -> long conversion               *
 *    MDD   10-22-92   Added Realloc_String to get rid of array bounds*
 *                     error.                                         *
 **********************************************************************/

LOGICAL ver_integer_range (keyword_ptr)

PARAMETER keyword_ptr;

{
    VALUE value_ptr = {NULL};
    char *value_string = {NULL};
    char err_msg [PDS_MAXLINE];
    long value_count = {0};
    double test_value = {0.0};
    double minimum_value = {0.0};
    double maximum_value = {0.0};
    LOGICAL success = {TRUE};
    LOGICAL multiple_values = {FALSE};

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** IF the maximum and minimum values for this keyword are in the       **/
    /**         data dictionary THEN                                        **/
    /*-----------------------------------------------------------------------*/

    if (dd_get_value_range (keyword_ptr -> name, &minimum_value, &maximum_value))
    {
        /*-------------------------------------------------------------------*/
        /** Determine if this keyword has more than one value.              **/
        /*-------------------------------------------------------------------*/

        multiple_values = (NextValue(FirstValue(keyword_ptr)) != NULL);

        /*-------------------------------------------------------------------*/
        /** Loop through the list of values . . .                           **/
        /*-------------------------------------------------------------------*/

        for (value_ptr = FirstValue (keyword_ptr);
                value_ptr != NULL; value_ptr = NextValue (value_ptr))
        {
            /*---------------------------------------------------------------*/
            /** IF there are more than one THEN                             **/
            /**     Increment the value count.                              **/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

            if (multiple_values)
                ++value_count;

            /*---------------------------------------------------------------*/
            /** Attempt to fetch the value from the label.                  **/
            /*---------------------------------------------------------------*/

            value_string = ver_get_value_string (value_ptr, value_count);

            /*---------------------------------------------------------------*/
            /** IF the value is not NULL THEN                               **/
            /*---------------------------------------------------------------*/

            if ((value_string != NULL) && (! IsNull(value_string)))
            {
                /*-----------------------------------------------------------*/
                /** Convert it to a form that can be compared to the        **/
                /**     maximum and minimum.                                **/
                /*-----------------------------------------------------------*/
                
                Realloc_String (value_string, String_Size(value_string) + 2);
                strcat (value_string, ".0");
                sscanf (value_string, "%le", &test_value);

                /*-----------------------------------------------------------*/
                /** IF the value is less than the minimum, AND the minimum  **/
                /**        is not N/A or UNKNOWN THEN                       **/
                /**     Construct the appropriate error message and append  **/
                /**         it onto the global list of messages.            **/
                /** ELSE                                                    **/
                /*-----------------------------------------------------------*/

                if ((test_value < minimum_value) &&
                        (minimum_value != (double) PDS_DD_NO_MINIMUM))
                {
                    if (maximum_value == (double) PDS_DD_NO_MAXIMUM)
                    {
                        sprintf (err_msg, 
                             "INTEGER value too small.  (Must be greater than: %ld)", 
                                 (long) minimum_value);
                    }
                    else
                    {
                        sprintf (err_msg, 
                          "INTEGER value too small.  (Must be in range: %ld to %ld)", 
                                 (long) minimum_value, (long) maximum_value);
                    }

                    success = err_keyword_message (ERROR1, keyword_ptr -> name, 
                                                   pds_line_number, 
                                                   value_count, err_msg);
                }
                else
                {
                    /*-------------------------------------------------------*/
                    /** IF the value is greater than the maximum, AND the   **/
                    /**        maximum is not N/A or UNKNOWN THEN           **/
                    /**     Construct the appropriate error message and     **/
                    /**         append it onto the global list of messages. **/
                    /** ENDIF                                               **/
                    /*-------------------------------------------------------*/
    
                    if ((test_value > maximum_value) &&
                            (maximum_value != (double) PDS_DD_NO_MAXIMUM))
                    {
                        if (minimum_value == (double) PDS_DD_NO_MINIMUM)
                        {
                            sprintf (err_msg, 
                                "INTEGER value too large.  (Must be less than: %ld)", 
                                     (long) maximum_value);
                        }
                        else
                        {
                            sprintf (err_msg, 
                                "INTEGER value too large.  (Must be in range: %ld to %ld)", 
                                     (long) minimum_value, (long) maximum_value);
                        }

                        success = err_keyword_message (ERROR1, keyword_ptr->name, 
                                                       pds_line_number, 
                                                       value_count, err_msg);

                    }  /*  End:  "if ((test_value > maximum_value) && ..."  */

                /*-----------------------------------------------------------*/
                /** ENDIF                                                   **/
                /*-----------------------------------------------------------*/

                }  /*  End:  "if ((test_value < ... else ..."  */

            /*---------------------------------------------------------------*/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

            }  /*  End:  "if ((value_string != NULL) ..."  */

            /*---------------------------------------------------------------*/
            /** Deallocate local storage.                                   **/
            /*---------------------------------------------------------------*/

            Lemme_Go(value_string);

        /*-------------------------------------------------------------------*/
        /** ENDLOOP                                                         **/
        /*-------------------------------------------------------------------*/

        }  /*  End:  "for (value_ptr = FirstValue ( ..."  */

    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if (dd_get_value_range ( ..."  */

    /*-----------------------------------------------------------------------*/
    /** RETURN the verify_success_flag                                      **/
    /*-----------------------------------------------------------------------*/
    
    return (success);

/** END **/
            
}  /*  "ver_integer_range"  */



/**********************************************************************
 *$Component                                                          *
 *    LOGICAL ver_real_range (keyword_ptr)                            *
 *$Abstract                                                           *
 *    Verifies the range of an real value.                            *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    KEYWORD                                                         *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    keyword_ptr:                                                    *
 *        The keyword_ptr variable is a pointer to the structure      *
 *        used to represent a keyword in a PDS label.                 *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    verify_success_flag:                                            *
 *        The verify_success_flag variable is a TRUE/FALSE value      *
 *        which indicates whether or not a keyword or object has been *
 *        successfully verified.                                      *
 *$Detailed_Description                                               *
 *    The ver_real_range routine fetches the maximum and mimimum      *
 *    values for an real keyword from the data dictionary and         *
 *    compares them against the value of the keyword.                 *
 *$Error_Handling                                                     *
 *    If the value is outside the range specified in the data         *
 *    dictionary, then an error message is appended onto the global   *
 *    list of messages.                                               *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$External_References                                                *
 *    Item                     Shared-Data           Access           *
 *    ------------------------------------------------------------    *
 *    pds_line_number          verlabel.c            read             *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   March 20, 1992                                            *
 *$Change_History                                                     *
 *    DPB   05-30-91   Original code.                                 *
 *    MDD   03-17-92   The great int -> long conversion               *
 **********************************************************************/

LOGICAL ver_real_range (keyword_ptr)

PARAMETER keyword_ptr;

{
    VALUE value_ptr = {NULL};
    char *value_string = {NULL};
    char err_msg [PDS_MAXLINE];
    long value_count = {0};
    double test_value = {0.0};
    double minimum_value = {0.0};
    double maximum_value = {0.0};
    LOGICAL success = {TRUE};
    LOGICAL multiple_values = {FALSE};

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** IF the maximum and minimum values for this keyword are in the       **/
    /**         data dictionary THEN                                        **/
    /*-----------------------------------------------------------------------*/
    if (dd_get_value_range (keyword_ptr -> name, &minimum_value, &maximum_value))
    {
        /*-------------------------------------------------------------------*/
        /** Determine if this keyword has more than one value               **/
        /*-------------------------------------------------------------------*/
        multiple_values = (NextValue(FirstValue(keyword_ptr)) != NULL);

        /*-------------------------------------------------------------------*/
        /** Loop through the list of values . . .                           **/
        /*-------------------------------------------------------------------*/

        for (value_ptr = FirstValue (keyword_ptr);
                value_ptr != NULL; value_ptr = NextValue (value_ptr))
        {
            /*---------------------------------------------------------------*/
            /** IF there are more than one THEN                             **/
            /**     Increment the value count.                              **/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

            if (multiple_values)
                ++value_count;

            /*---------------------------------------------------------------*/
            /** Attempt to fetch the value from the label.                  **/
            /*---------------------------------------------------------------*/

            value_string = ver_get_value_string (value_ptr, value_count);

            /*---------------------------------------------------------------*/
            /** IF the value is not NULL THEN                               **/
            /*---------------------------------------------------------------*/

            if ((value_string != NULL) && (! IsNull(value_string)))
            {
                /*-----------------------------------------------------------*/
                /** Convert it to a form that can be compared to the        **/
                /**     maximum and minimum.                                **/
                /*-----------------------------------------------------------*/

                sscanf (value_string, "%le", &test_value);

                /*-----------------------------------------------------------*/
                /** IF the value is less than the minimum, AND the minimum  **/
                /**        is not N/A or UNKNOWN THEN                       **/
                /**     Construct the appropriate error message and append  **/
                /**         it onto the global list of messages.            **/
                /** ELSE                                                    **/
                /*-----------------------------------------------------------*/

                if ((test_value < minimum_value) &&
                        (minimum_value != (double) PDS_DD_NO_MINIMUM))
                {
                    if (maximum_value == (double) PDS_DD_NO_MAXIMUM)
                    {
                        sprintf (err_msg, 
                             "REAL value too small.  (Must be greater than: %le)", 
                                 minimum_value);
                    }
                    else
                    {
                        sprintf (err_msg, 
                             "REAL value too small.  (Must be in range: %le to %le)", 
                                 minimum_value, maximum_value);
                    }

                    success = err_keyword_message (ERROR1, keyword_ptr -> name, 
                                                   pds_line_number, 
                                                   value_count, err_msg);
                }
                else
                {
                    /*-------------------------------------------------------*/
                    /** IF the value is greater than the maximum, AND the   **/
                    /**        maximum is not N/A or UNKNOWN THEN           **/
                    /**     Construct the appropriate error message and     **/
                    /**         append it onto the global list of messages. **/
                    /** ENDIF                                               **/
                    /*-------------------------------------------------------*/
    
                    if ((test_value > maximum_value) &&
                            (maximum_value != (double) PDS_DD_NO_MAXIMUM))
                    {
                        if (minimum_value == (double) PDS_DD_NO_MINIMUM)
                        {
                            sprintf (err_msg, 
                                 "REAL value too large.  (Must be less than: %le)", 
                                     maximum_value);
                        }
                        else
                        {
                            sprintf (err_msg, 
                                 "REAL value too large.  (Must be in range: %le to %le)", 
                                     minimum_value, maximum_value);
                        }

                        success = err_keyword_message (ERROR1, keyword_ptr->name, 
                                                       pds_line_number, 
                                                       value_count, err_msg);

                    }  /*  End:  "if ((test_value > maximum_value) && ..."  */

                /*-----------------------------------------------------------*/
                /** ENDIF                                                   **/
                /*-----------------------------------------------------------*/

                }  /*  End:  "if ((test_value < ... else ..."  */

            /*---------------------------------------------------------------*/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

            }  /*  End:  "if ((value_string != NULL) ..."  */

            /*---------------------------------------------------------------*/
            /** Deallocate local storage.                                   **/
            /*---------------------------------------------------------------*/
                                                                    
            Lemme_Go(value_string);

        /*-------------------------------------------------------------------*/
        /** ENDLOOP                                                         **/
        /*-------------------------------------------------------------------*/

        }  /*  End:  "for (value_ptr = FirstValue ( ..."  */

    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if (dd_get_value_range ( ..."  */

    /*-----------------------------------------------------------------------*/
    /** RETURN the verify_success_flag                                      **/
    /*-----------------------------------------------------------------------*/
    
    return (success);

/** END **/
            
}  /*  "ver_real_range"  */                         



/**********************************************************************
 *$Component                                                          *
 *    LOGICAL ver_spelling (keyword_ptr)                              *
 *$Abstract                                                           *
 *    Verify the spelling of character keywords.                      *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    KEYWORD                                                         *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    keyword_ptr:                                                    *
 *        The keyword_ptr variable is a pointer to the structure      *
 *        used to represent a keyword in a PDS label.                 *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    verify_success_flag:                                            *
 *        The verify_success_flag variable is a TRUE/FALSE value      *
 *        which indicates whether or not a keyword or object has been *
 *        successfully verified.                                      *
 *$Detailed_Description                                               *
 *    The ver_spelling routine is a TBD.                              *
 *$Error_Handling                                                     *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.0   May 30, 1991                                              *
 *$Change_History                                                     *
 *    DPB   05-30-91   Original code.                                 *
 **********************************************************************/

LOGICAL ver_spelling (keyword_ptr)

PARAMETER keyword_ptr;

{
    LOGICAL success = {TRUE};

    return (success);

}  /*  "ver_spelling"  */



/**********************************************************************
 *$Component                                                          *
 *    LOGICAL ver_standard_values (keyword_ptr)                       *
 *$Abstract                                                           *
 *    Verifies that values are on keyword's list of standard values.  *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    KEYWORD                                                         *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    keyword_ptr:                                                    *
 *        The keyword_ptr variable is a pointer to the structure      *
 *        used to represent a keyword in a PDS label.                 *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    verify_success_flag:                                            *
 *        The verify_success_flag variable is a TRUE/FALSE value      *
 *        which indicates whether or not a keyword or object has been *
 *        successfully verified.                                      *
 *$Detailed_Description                                               *
 *    The ver_standard_values routine verifies that all of the values *
 *    of a keyword are on the keyword's list of standard values.      *
 *    When the standard values are a sequence of sets, each set's     *
 *    elements are appended onto a character string and compared      *
 *    against the keyword's values, which have also been converted    *
 *    to long character strings.                                      *
 *$Error_Handling                                                     *
 *    If a value is not a standard value then a warning message is    *
 *    appended onto the global list of messages.                      *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$External_References                                                *
 *    Item                     Shared-Data           Access           *
 *    ------------------------------------------------------------    *
 *    pds_line_number          verlabel.c            read             *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.4   Feb 11, 1993                                              *
 *$Change_History                                                     *
 *    DPB   05-30-91   Original code.                                 *
 *    KLM   06-18-91   Changed name of lab_get_all_values to          *
 *                     lu_get_all_values.                             *
 *    MDD   03-17-92   The great int -> long conversion               *
 *    MDD   07-09-92   Added verbose mode check for standard value    *
 *                     warnings.                                      *
 *    MDD   02-11-93   Added actual standard values to warnings.      *
 *    DWS   03-20-01   Stopped changing input keyword values          *
 *    DWS   06-06-01   Added code to replace _ in standard value with *
 *                     a space.										  *
 *	  MDC   12-20-02   Made the output message more informational when*
 *					   lvtool prints out "Standard value is: ...."	  *
 *    MDC   03-30-05   Changed code to dynamically allocate memory to *
 *                     err_msg to prevent program crash due to very   *
 *                     long values that could be found for a keyword  *
 *                     value                                          *
 **********************************************************************/

LOGICAL ver_standard_values (keyword_ptr)

PARAMETER keyword_ptr;

{
    STRING_LIST *standard_values_list = {NULL};
    STRING_LIST *value_list = {NULL};

    STRING_LIST *ptr = {NULL};
    STRING_LIST *standard_value_ptr = {NULL};
    int pass;
    long value_count = {0};
    LOGICAL success = {TRUE};
    LOGICAL found = {TRUE};
    LOGICAL multiple_values = {FALSE};
 /*   char err_msg [PDS_MAXLINE * 2]; */
	char *err_msg = NULL;
	int str_length = 0;

	char *test_val = {NULL};  /*copy of keyword value   DWS 03-20-01*/
/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Attempt to fetch the standard values for the keyword.               **/
    /*-----------------------------------------------------------------------*/
    standard_values_list = dd_get_standard_values (keyword_ptr -> name);

    /*-----------------------------------------------------------------------*/
    /** IF the standard values are in the data dictionary THEN              **/
    /*-----------------------------------------------------------------------*/

    if (standard_values_list != NULL)
    {
        /*-------------------------------------------------------------------*/
        /** Attempt to fetch all of the values of the keyword from          **/
        /**     the label.                                                  **/
        /*-------------------------------------------------------------------*/

        value_list = lu_fetch_all_values (keyword_ptr, TRUE, FALSE);

        /*-------------------------------------------------------------------*/
        /** Determine if this keyword has more than one value               **/
        /*-------------------------------------------------------------------*/

        multiple_values = ((value_list != NULL) && (value_list->next != NULL));

        /*-------------------------------------------------------------------*/
        /** LOOP through the list of values . . .                           **/
        /*-------------------------------------------------------------------*/

        for (ptr = value_list, value_count = 0; ptr != NULL; ptr = ptr -> next)
        {
            /*---------------------------------------------------------------*/
            /** IF there are more than one THEN                             **/
            /**     Increment the value count.                              **/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

            if (multiple_values)
                ++value_count;

            /*---------------------------------------------------------------*/
			/** New code.  We will no longer modify input values string for **/
			/** for testing.  Copy the string, modify copy, then test       **/
			/**											DWS       03-20-01  **/
			/*---------------------------------------------------------------*/
			
			Malloc_String(test_val, (int) String_Size(ptr->text) + 1);

			strcpy(test_val, ptr->text);


            /*---------------------------------------------------------------*/
            /** LOOP twice through the list of values (if necessary).       **/
            /*---------------------------------------------------------------*/

            for (pass = 1, found = IsNull(test_val);
                    ((! found) && (pass <= 2)); ++pass)
            {
                /*-----------------------------------------------------------*/
                /** IF this is the second pass THEN                         **/
                /**     Remove quotes and underscores, compress blanks,     **/
                /**             and strip off leading and trailing blanks.  **/
                /** ENDIF                                                   **/
                /*-----------------------------------------------------------*/
                if (pass == 2)
                {
                    util_remove_char (test_val, '"');
/*                    util_remove_char (test_val, '_');*/ /* removed by dws 5-25-01*/
					util_replace_char (test_val, '_', ' '); /* added by dws 6-6-01*/
                    util_compress_char (test_val, ' ');
                    util_strip_lead_and_trail (test_val, ' ');
                    util_upper_case (test_val);
                }

                /*-----------------------------------------------------------*/
                /** Compare the value against the list of standard values.  **/
                /*-----------------------------------------------------------*/

                for (standard_value_ptr = standard_values_list;
                         ((standard_value_ptr != NULL) && (! found)); 
                             standard_value_ptr = standard_value_ptr->next)
                {
                    found = (strcmp (test_val, 
                                     standard_value_ptr -> text) == 0);
                }

            /*---------------------------------------------------------------*/
            /** ENDLOOP                                                     **/
            /*---------------------------------------------------------------*/

            }  /*  End:  "for (pass = 1, ..."  */

            /*---------------------------------------------------------------*/
            /** IF the value wasn't found on the list of standard values THEN **/
            /**     Construct the appropriate warning message and append it **/
            /**         onto the global list of messages.                   **/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

            --pass;

			if (! found && pds_verbose && pds_warning) /* 01-06-03 */
			{
                if (keyword_ptr -> columns <= 0)
                {

					str_length = (int) (String_Size("Value is not a standard value: ") +
								String_Size (ptr->text));

					Malloc_String(err_msg, str_length);

                    sprintf (err_msg, "Value is not a standard value: %s",
                                          ptr -> text);
                    success = err_keyword_message (WARNING, keyword_ptr->name, 
                                                   pds_line_number, value_count, 
                                                   err_msg);
                }
                else
                {

					str_length = (int) (String_Size("Sequence or set value is not a standard value: ") +
							     String_Size(ptr->text));

					Malloc_String(err_msg, str_length);

                    sprintf (err_msg, 
                       "Sequence or set value is not a standard value: %s",
                          ptr -> text);
 
                    success = err_keyword_message (WARNING, keyword_ptr->name, 
                                                   pds_line_number, value_count, 
                                                   err_msg);
                }
				Lemme_Go(err_msg);

            }  /*  End:  "if (! found) ..."  */

            /*---------------------------------------------------------------*/
            /** IF the value was found on the list of standard values,      **/
            /**         but only after it had been massaged in the second   **/
            /**         pass THEN                                           **/
            /**     Construct the appropriate warning message and append it **/
            /**         onto the global list of messages.                   **/
            /** ENDIF                                                       **/
            /**																**/
			/** 01-06-03 MDC												**/
			/** If the flag to display warning msgs. is OFF, then don't		**/
			/** append the warning msg. onto the message list.				**/
			/*---------------------------------------------------------------*/
			
			if ((found) && (pass == 2) && pds_verbose && pds_warning) 
			{
                if (keyword_ptr -> columns <= 0)
                {
					str_length = (int) (String_Size(", Standard value is: :  , (GEN_DATA_TYPE=IDENTIFIER)") +
						          String_Size(ptr->text) + String_Size(keyword_ptr->name) + String_Size(test_val));

					Malloc_String(err_msg, str_length);

					/*------------------------------------------------------*/
					/* 12-20-02 MDC											*/
					/* Made the output message more informational by adding */
					/* in the keyword name in front of the actual standard  */
					/* value.												*/
					/*------------------------------------------------------*/
                    sprintf (err_msg,                              
/* "Matched a standard value only after blanks, underscores, and quotes were removed, and it was converted to upper case: %s", ptr -> text);*/
"%s, Standard value is: %s:  %s, (GEN_DATA_TYPE=IDENTIFIER)"
 , ptr -> text, keyword_ptr->name, test_val );
 
                    success = err_keyword_message (WARNING, keyword_ptr->name, 
                                                   pds_line_number, value_count, 
                                                   err_msg);
                }
                else
                {
					str_length = (int) String_Size("Sequence or set matched a standard value only after blanks, underscores, and quotes were removed, and it was converted to upper case: ") +
						          (int) String_Size(ptr->text);

					Malloc_String(err_msg, str_length);
                    sprintf (err_msg,
                              "Sequence or set matched a standard value only after blanks, underscores, and quotes were removed, and it was converted to upper case: %s",
                                ptr -> text );

                    success = err_keyword_message (WARNING, keyword_ptr->name, 
                                                   pds_line_number, value_count, 
                                                   err_msg);
                }
				Lemme_Go(err_msg);

            }  /*  End:  "if ((found) && (pass == 2)) ..."  */

        /*-------------------------------------------------------------------*/
        /** ENDLOOP                                                         **/
        /*-------------------------------------------------------------------*/

			Lemme_Go(test_val);                               /*DWS 03-20-01 */

        }  /*  End:  "for (ptr = value_list, ..."  */

        /*-------------------------------------------------------------------*/
        /** Deallocate local storage.                                       **/
        /*-------------------------------------------------------------------*/

        if (value_list != NULL)
            util_deallocate_string_list (value_list);

        util_deallocate_string_list (standard_values_list);
    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if (standard_values_list != NULL) ..."  */
    
    /*-----------------------------------------------------------------------*/
    /** RETURN the verify_success_flag                                      **/
    /*-----------------------------------------------------------------------*/
    return (success);

/** END **/
            
}  /*  "ver_standard_values"  */


/**********************************************************************
 *$Component                                                          *
 *    LOGICAL ver_standard_values_2 (keyword_ptr)                     *
 *$Abstract                                                           *
 *    Verifies that values are on keyword's list of standard values.  *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    KEYWORD                                                         *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    keyword_ptr:                                                    *
 *        The keyword_ptr variable is a pointer to the structure      *
 *        used to represent a keyword in a PDS label.                 *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    verify_success_flag:                                            *
 *        The verify_success_flag variable is a TRUE/FALSE value      *
 *        which indicates whether or not a keyword or object has been *
 *        successfully verified.                                      *
 *$Detailed_Description                                               *
 *    The ver_standard_values_2 routine verifies that all of the      *
 *    values of a keyword are on the keyword's list of standard       *
 *    values.  When the standard values are a sequence of sets, each  *
 *    set's elements are appended onto a character string and         *
 *    compared against the keyword's values, which have also been     *
 *    converted to long character strings.                            *
 *    This routine differs from ver_standard_values routine in the    *
 *    the way it verifies against underscores in the label.           *
 *$Error_Handling                                                     *
 *    If a value is not a standard value then a warning message is    *
 *    appended onto the global list of messages.                      *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$External_References                                                *
 *    Item                     Shared-Data           Access           *
 *    ------------------------------------------------------------    *
 *    pds_line_number          verlabel.c            read             *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.4   Feb 11, 1993                                              *
 *$Change_History                                                     *
 *    DPB   05-30-91   Original code.                                 *
 *    KLM   06-18-91   Changed name of lab_get_all_values to          *
 *                     lu_get_all_values.                             *
 *    MDD   03-17-92   The great int -> long conversion               *
 *    MDD   07-09-92   Added verbose mode check for standard value    *
 *                     warnings.                                      *
 *    MDD   02-11-93   Added actual standard values to warnings.      *
 *    DWS   03-20-01   Stopped changing input keyword values          *
 *    DWS   06-06-01   Added code to replace _ in standard value with *
 *                     a space.										  *
 *    DWS   08-27-02   Added this function to handle the standard     *
 *                     values for INDENTIFIERS                        *
 *    MDC   03-30-05   Changed code to dynamically allocate memory to *
 *                     err_msg to prevent program crash due to very   *
 *                     long values that could be found for a keyword  *
 *                     value                                          *
 **********************************************************************/

LOGICAL ver_standard_values_2 (keyword_ptr)

PARAMETER keyword_ptr;

{
    STRING_LIST *standard_values_list = {NULL};
    STRING_LIST *value_list = {NULL};

    STRING_LIST *ptr = {NULL};
    STRING_LIST *standard_value_ptr = {NULL};
    int pass;
    long value_count = {0};
    LOGICAL success = {TRUE};
    LOGICAL found = {TRUE};
    LOGICAL multiple_values = {FALSE};
/*    char err_msg [PDS_MAXLINE + 1]; */
	char *err_msg = NULL;
	int str_length = 0;

	char *test_val = {NULL};  /*copy of keyword value   DWS 03-20-01*/
/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Attempt to fetch the standard values for the keyword.               **/
    /*-----------------------------------------------------------------------*/
    standard_values_list = dd_get_standard_values (keyword_ptr -> name);

    /*-----------------------------------------------------------------------*/
    /** IF the standard values are in the data dictionary THEN              **/
    /*-----------------------------------------------------------------------*/

    if (standard_values_list != NULL)
    {
        /*-------------------------------------------------------------------*/
        /** Attempt to fetch all of the values of the keyword from          **/
        /**     the label.                                                  **/
        /*-------------------------------------------------------------------*/

        value_list = lu_fetch_all_values (keyword_ptr, TRUE, FALSE);

        /*-------------------------------------------------------------------*/
        /** Determine if this keyword has more than one value               **/
        /*-------------------------------------------------------------------*/

        multiple_values = ((value_list != NULL) && (value_list->next != NULL));

        /*-------------------------------------------------------------------*/
        /** LOOP through the list of values . . .                           **/
        /*-------------------------------------------------------------------*/

        for (ptr = value_list, value_count = 0; ptr != NULL; ptr = ptr -> next)
        {
            /*---------------------------------------------------------------*/
            /** IF there are more than one THEN                             **/
            /**     Increment the value count.                              **/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

            if (multiple_values)
                ++value_count;

            /*---------------------------------------------------------------*/
			/** New code.  We will no longer modify input values string for **/
			/** for testing.  Copy the string, modify copy, then test       **/
			/**											DWS       03-20-01  **/
			/*---------------------------------------------------------------*/
			
			Malloc_String(test_val, (int) String_Size(ptr->text) + 1);

			strcpy(test_val, ptr->text);


            /*---------------------------------------------------------------*/
            /** LOOP twice through the list of values (if necessary).       **/
            /*---------------------------------------------------------------*/

            for (pass = 1, found = IsNull(test_val);
                    ((! found) && (pass < 2)); ++pass)
            {
                /*-----------------------------------------------------------*/
                /** Compare the value against the list of standard values.  **/
                /*-----------------------------------------------------------*/

                for (standard_value_ptr = standard_values_list;
                         ((standard_value_ptr != NULL) && (! found)); 
                             standard_value_ptr = standard_value_ptr->next)
                {
                    found = (strcmp (test_val, 
                                     standard_value_ptr -> text) == 0);
                }

            /*---------------------------------------------------------------*/
            /** ENDLOOP                                                     **/
            /*---------------------------------------------------------------*/

            }  /*  End:  "for (pass = 1, ..."  */

            /*---------------------------------------------------------------*/
            /** IF the value wasn't found on the list of standard values THEN **/
            /**     Construct the appropriate warning message and append it **/
            /**         onto the global list of messages.                   **/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

            --pass;

			/*---------------------------------------------------------------*/
            /* 01-06-03 MDC													 */
			/* Added an extra check to see if the flag to display warning	 */
			/* msgs. is ON or OFF. If it is OFF, then don't append the		 */
			/* warning msg. onto the message list.							 */
			/*---------------------------------------------------------------*/

			 if (! found && pds_verbose && pds_warning) 
			{
                if (keyword_ptr -> columns <= 0)
                {
					str_length = (int) (String_Size("Value is not a standard value: ") +
						         String_Size(ptr->text));
					Malloc_String(err_msg, str_length);

                    sprintf (err_msg, "Value is not a standard value: %s",
                                          ptr -> text);
                    success = err_keyword_message (WARNING, keyword_ptr->name, 
                                                   pds_line_number, value_count, 
                                                   err_msg);
                }
                else
                {
					str_length = (int) (String_Size("Sequence or set value is not a standard value: ") +
						         String_Size(ptr->text));
					Malloc_String(err_msg, str_length);

                    sprintf (err_msg, 
                       "Sequence or set value is not a standard value: %s",
                          ptr -> text);
 
                    success = err_keyword_message (WARNING, keyword_ptr->name, 
                                                   pds_line_number, value_count, 
                                                   err_msg);
                }
				Lemme_Go(err_msg);

            }  /*  End:  "if (! found) ..."  */

            /*---------------------------------------------------------------*/
            /** IF the value was found on the list of standard values,      **/
            /**         but only after it had been massaged in the second   **/
            /**         pass THEN                                           **/
            /**     Construct the appropriate warning message and append it **/
            /**         onto the global list of messages.                   **/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

			if ((found) && (pass == 2) && pds_verbose && pds_warning) /* 01-06-03 MDC */
			{
                if (keyword_ptr -> columns <= 0)
                {
					str_length = (int) (String_Size(" , Standard value is: ") + String_Size(ptr->text) +
						         String_Size(test_val));
					Malloc_String(err_msg, str_length);

                    sprintf (err_msg,                              
/* "Matched a standard value only after blanks, underscores, and quotes were removed, and it was converted to upper case: %s", ptr -> text);*/
 "%s, Standard value is: %s"
 , ptr -> text, test_val );
 
                    success = err_keyword_message (WARNING, keyword_ptr->name, 
                                                   pds_line_number, value_count, 
                                                   err_msg);
                }
                else
                {
					str_length = (int) (String_Size("Sequence or set matched a standard value only after blanks, underscores, and quotes were removed, and it was converted to upper case: ")
						         + String_Size(ptr->text));
					Malloc_String(err_msg, str_length);

                    sprintf (err_msg,
                              "Sequence or set matched a standard value only after blanks, underscores, and quotes were removed, and it was converted to upper case: %s",
                                ptr -> text );

                    success = err_keyword_message (WARNING, keyword_ptr->name, 
                                                   pds_line_number, value_count, 
                                                   err_msg);
                }
				Lemme_Go(err_msg);

            }  /*  End:  "if ((found) && (pass == 2)) ..."  */

        /*-------------------------------------------------------------------*/
        /** ENDLOOP                                                         **/
        /*-------------------------------------------------------------------*/

			Lemme_Go(test_val);                               /*DWS 03-20-01 */

        }  /*  End:  "for (ptr = value_list, ..."  */

        /*-------------------------------------------------------------------*/
        /** Deallocate local storage.                                       **/
        /*-------------------------------------------------------------------*/

        if (value_list != NULL)
            util_deallocate_string_list (value_list);

        util_deallocate_string_list (standard_values_list);
    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if (standard_values_list != NULL) ..."  */
    
    /*-----------------------------------------------------------------------*/
    /** RETURN the verify_success_flag                                      **/
    /*-----------------------------------------------------------------------*/
    return (success);

/** END **/
            
}  /*  "ver_standard_values_2"  */


/**********************************************************************
 *$Component                                                          *
 *    LOGICAL ver_string_length (keyword_ptr)                         *
 *$Abstract                                                           *
 *    Verifies the length of text values.                             *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    KEYWORD                                                         *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    keyword_ptr:                                                    *
 *        The keyword_ptr variable is a pointer to the structure      *
 *        used to represent a keyword in a PDS label.                 *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    verify_success_flag:                                            *
 *        The verify_success_flag variable is a TRUE/FALSE value      *
 *        which indicates whether or not a keyword or object has been *
 *        successfully verified.                                      *
 *$Detailed_Description                                               *
 *    The ver_string_length routine fetches the maximum and mimimum   *
 *    lengths for the values of a text keyword from the data          *
 *    dictionary and compares them against the actual length of       *
 *    the keyword's values.                                           *
 *$Error_Handling                                                     *
 *    If the value is outside the range specified in the data         *
 *    dictionary, then an error message is appended onto the global   *
 *    list of messages.                                               *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$External_References                                                *
 *    Item                     Shared-Data           Access           *
 *    ------------------------------------------------------------    *
 *    pds_line_number          verlabel.c            read             *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   March 20, 1992                                            *
 *$Change_History                                                     *
 *    DPB   05-30-91   Original code.                                 *
 *    MDD   03-17-92   The great int -> long conversion               *
 **********************************************************************/

LOGICAL ver_string_length (keyword_ptr)

PARAMETER keyword_ptr;

{
    VALUE value_ptr = {NULL};
    char *value_string = {NULL};
    char err_msg [PDS_MAXLINE];
    long value_count = {0};
    long value_length = {0};
    long minimum_length = {0};
    long maximum_length = {0};
    LOGICAL success = {TRUE};
    LOGICAL multiple_values = {FALSE};

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** IF the maximum and minimum values for this keyword are in the       **/
    /**         data dictionary THEN                                        **/
    /*-----------------------------------------------------------------------*/

    if (dd_get_length_range (keyword_ptr -> name, &minimum_length, &maximum_length))
    {
        /*-------------------------------------------------------------------*/
        /** Determine if this keyword has more than one value               **/
        /*-------------------------------------------------------------------*/

        multiple_values = (NextValue(FirstValue(keyword_ptr)) != NULL);

        /*-------------------------------------------------------------------*/
        /** Loop through the list of values . . .                           **/
        /*-------------------------------------------------------------------*/

        for (value_ptr = FirstValue (keyword_ptr);
                value_ptr != NULL; value_ptr = NextValue (value_ptr))
        {
            /*---------------------------------------------------------------*/
            /** IF there are more than one THEN                             **/
            /**     Increment the value count.                              **/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

            if (multiple_values)
                ++value_count;

            /*---------------------------------------------------------------*/
            /** Attempt to fetch the value from the label.                  **/
            /*---------------------------------------------------------------*/

            value_string = ver_get_value_string (value_ptr, value_count);

            /*---------------------------------------------------------------*/
            /** IF the value is not NULL THEN                               **/
            /*---------------------------------------------------------------*/

            if ((value_string != NULL) && (! IsNull(value_string)))
            {
                /*-----------------------------------------------------------*/
                /** Get the length of the value.                            **/
                /*-----------------------------------------------------------*/

                value_length = (long) strlen (value_string);

                /*-----------------------------------------------------------*/
                /** IF the value is shorter than the minimum, AND the       **/
                /**        minimum is not N/A or UNKNOWN THEN               **/
                /**     Construct the appropriate error message and append  **/
                /**         it onto the global list of messages.            **/
                /** ELSE                                                    **/
                /*-----------------------------------------------------------*/

                if ((value_length < minimum_length) &&
                        (minimum_length != (long) PDS_DD_NO_MINIMUM))
                {
                    if (maximum_length == (long) PDS_DD_NO_MAXIMUM)
                    {
                        sprintf (err_msg, 
                            "CHARACTER value too short.  (Must be at least %ld characters in length)", 
                                 minimum_length);
                    }
                    else
                    {
                        sprintf (err_msg, 
                            "CHARACTER value too short.  (Must be between %ld and %ld characters in length)", 
                                 minimum_length, maximum_length);
                    }

                    success = err_keyword_message (ERROR1, keyword_ptr -> name, 
                                                   pds_line_number, 
                                                   value_count, err_msg);
                }
                else
                {
                    /*-------------------------------------------------------*/
                    /** IF the value is longer than the maximum, AND the    **/
                    /**        maximum is not N/A or UNKNOWN THEN           **/
                    /**     Construct the appropriate error message and     **/
                    /**         append it onto the global list of messages. **/
                    /** ENDIF                                               **/
                    /*-------------------------------------------------------*/
    
                    if ((value_length > maximum_length) &&
                            (maximum_length != (long) PDS_DD_NO_MAXIMUM))
                    {
                        if (minimum_length == (long) PDS_DD_NO_MINIMUM)
                        {
                            sprintf (err_msg, 
                                "CHARACTER value too long.  (Must be less than %ld characters in length)", 
                                     maximum_length);
                        }
                        else
                        {
                            sprintf (err_msg, 
                                "CHARACTER value too long.  (Must be between %ld and %ld characters in length)", 
                                     minimum_length, maximum_length);
                        }
    
                        success = err_keyword_message (ERROR1, keyword_ptr->name, 
                                                       pds_line_number, 
                                                       value_count, err_msg);

                    }  /*  End:  "if ((value_length >  ..."  */

                /*-----------------------------------------------------------*/
                /** ENDIF                                                   **/
                /*-----------------------------------------------------------*/

                }  /*  End:  "if ((value_length < ... else ..."  */

            /*---------------------------------------------------------------*/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

            }  /*  End:  "if (value_string != NULL) ..."  */

            /*---------------------------------------------------------------*/
            /** Deallocate local storage.                                   **/
            /*---------------------------------------------------------------*/

            Lemme_Go(value_string);

        /*-------------------------------------------------------------------*/
        /** ENDLOOP                                                         **/
        /*-------------------------------------------------------------------*/

        }  /*  End:  "for (value_ptr = FirstValue ( ..."  */

    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if (dd_get_length_range ( ..."  */

    /*-----------------------------------------------------------------------*/
    /** RETURN the verify_success_flag                                      **/
    /*-----------------------------------------------------------------------*/
    
    return (success);

/** END **/
            
}  /*  "ver_string_length"  */



/**********************************************************************
 *$Component                                                          *
 *    LOGICAL ver_time_string (keyword_name, time_string, value_count)*
 *$Abstract                                                           *
 *    Verifys the time.                                               *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    LABEL                                                           *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    keyword_name:                                                   *
 *        The keyword_name variable is a character string             *
 *        which contains the name of a keyword in a PDS label         *
 *        (e.g., the line "SCID = VG1" implies that "SCID" is the     *
 *        keyword name).                                              *
 *    time_string:                                                    *
 *        The time_string variable is a character string that         *  
 *        contains a time value in the format HH:MM:SS.SSS.           *
 *    value_count:                                                    *
 *        The value_count variable is the index, within a keyword,    *
 *        of a value (e.g., the third value, first value, etc.).      *
 *$Outputs                                                            *
 *    NONE                                                            *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure        *
 *        (a value of FALSE).                                         *   
 *$Detailed_Description                                               *
 *    The ver_time routine checks the time value to verify that       *
 *    the hours, minutes, seconds, and nanoseconds are within their   *
 *    specified ranges. If invalid time fields are found, error       *
 *    messages are appended to the global list. This routine returns  * 
 *    TRUE if no errors are found.                                    * 
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Kristy L. Marski /JPL                                           *
 *$Version_and_Date                                                   *
 *    1.3   September 3, 1992                                         *
 *$Change_history                                                     *
 *    KLM   04-09-91   Original generation.                           *
 *    MDD   06-11-91   Added ver_pad_time call and local time checking*
 *    MDD   03-17-92   The great int -> long conversion               *
 *    MDD   09-03-92   Changed "local" warnings to "zone" warnings.   *
 **********************************************************************/

LOGICAL ver_time_string (keyword_name, time_string, value_count)

char *keyword_name;
char *time_string;
long value_count;

{

    LOGICAL success = {TRUE};
    int hour, minute, second;
    long nanosecond = 0;
    char *local_time = NULL;
    char *new_time_string = NULL;

/** BEGIN **/

    /*----------------------------------------------------------------*/
    /** Pad string to maximum time length                            **/
    /** Scan the time_string                                         **/
    /*----------------------------------------------------------------*/

    Malloc_String(new_time_string, (int) String_Size(time_string));
    strcpy (new_time_string, time_string);
    new_time_string = ver_pad_time (new_time_string);
    sscanf (new_time_string, "%d:%d:%d.%ld", &hour, &minute,
            &second, &nanosecond);
        
    /*----------------------------------------------------------------*/
    /** IF the hours are not between 0 and 23                        **/
    /**    Append error message to the error list                    **/
    /**    Set success to FALSE                                      **/
    /** ENDIF                                                        **/
    /*----------------------------------------------------------------*/

    if (!(0 <= hour && hour <= 23))
    {                                        
       success = err_keyword_message (ERROR1, keyword_name, 
                                      pds_line_number, value_count, 
                                     "Invalid value for hours field");
    }

    /*----------------------------------------------------------------*/
    /** IF the minutes are not between 0 and 59                      **/
    /**    Append error message to the error list                    **/
    /**    Set success to FALSE                                      **/
    /** ENDIF                                                        **/
    /*----------------------------------------------------------------*/

       
    if (!(0 <= minute && minute <= 59))
    {
       success = err_keyword_message (ERROR1, keyword_name, 
                                      pds_line_number, value_count, 
                                     "Invalid value for minutes field");
    }

    /*----------------------------------------------------------------*/
    /** IF the seconds are not between 0 and 59                      **/
    /**    Append error message to the error list                    **/
    /**    Set success to FALSE                                      **/
    /** ENDIF                                                        **/
    /*----------------------------------------------------------------*/

    if (!(0 <= second && second <= 59))
    {
       success = err_keyword_message (ERROR1, keyword_name, 
                                      pds_line_number, value_count, 
                                     "Invalid value for seconds field");
    }

    /*----------------------------------------------------------------*/
    /** IF the nanoseconds are not between 0 and 9999999             **/
    /**    Append error message to the error list                    **/
    /**    Set success to FALSE                                      **/
    /** ENDIF                                                        **/
    /*----------------------------------------------------------------*/

    if (!(0 <= nanosecond && nanosecond <= 9999999L))
    {
       success = err_keyword_message (ERROR1, keyword_name, 
                                      pds_line_number, value_count, 
                                     "Invalid value for seconds fraction");
    }

    /*----------------------------------------------------------------*/
    /** IF there is a local time offset on the time THEN             **/
    /*----------------------------------------------------------------*/

    if ((local_time = strpbrk (new_time_string, "-+")) != NULL)
    {
      /*----------------------------------------------------------------*/
      /** get the local hour and minute fields                         **/
      /*----------------------------------------------------------------*/

      ++local_time;
      hour = 0;
      minute = 0;
	  /* 06-29-06 - MDC - FIX TO STOP PROGRAM FROM HANGING */
      scanf (local_time, "%d:%d", &hour, &minute);

      /*----------------------------------------------------------------*/
      /** IF the hours are not between 0 and 12                        **/
      /**    Append error message to the error list                    **/
      /**    Set success to FALSE                                      **/
      /** ENDIF                                                        **/
      /*----------------------------------------------------------------*/

      if (!(0 <= hour && hour <= 12))
      {                                        
          success = err_keyword_message (ERROR1, keyword_name, 
                                         pds_line_number, value_count, 
                                        "Invalid value for time zone hours field");
      }

      /*----------------------------------------------------------------*/
      /** IF the minutes are not between 0 and 59                      **/
      /**    Append error message to the error list                    **/
      /**    Set success to FALSE                                      **/
      /** ENDIF                                                        **/
      /*----------------------------------------------------------------*/
       
      if (!(0 <= minute && minute <= 59))
      {
         success = err_keyword_message (ERROR1, keyword_name, 
                                        pds_line_number, value_count, 
                                        "Invalid value for time zone minutes field");
      }

    }

    /*----------------------------------------------------------------*/
    /** ENDIF                                                        **/
    /** Return success flag                                          **/
    /*----------------------------------------------------------------*/

    Lemme_Go(new_time_string);
    return (success);

/** END **/

}  /*  End: "ver_time_string"  */



/**********************************************************************
 *$Component                                                          *
 *    LOGICAL ver_units (keyword_ptr)                                 *
 *$Abstract                                                           *
 *    Checks the unit name or abbreviations in a units expression.    *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    LABEL                                                           *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    keyword_ptr:                                                    *
 *        The keyword_ptr variable is a pointer to the structure      *
 *        used to represent an object in a PDS label.                 *
 *$Outputs                                                            *
 *    NONE                                                            *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure        *
 *        (a value of FALSE).                                         *
 *$Detailed_Description                                               *
 *    The ver_units module checks each of the unit names or unit      *
 *    abbreviations found in the units expression and verifies that   *
 *    it is a known unit name or abbreviation. If invalid unit names  *
 *    or abbreviations are found, error messages are appended to the  *
 *    global list. This routine returns TRUE if no errors are found   * 
 *    in the units expression.                                        *
 *$External_References                                                *
 *    NONE                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore                                                 *
 *$Version_and_Date                                                   *
 *    1.3   March 20, 1992                                            *
 *$Change_History                                                     *
 *    MDD   02-20-91   Original code.                                 *
 *    KLM   06-18-91   Changed name of lab_format_units to            *
 *                     lu_format_units.                               *
 *    MDD   10-21-91   Fixed malloc, free, and sys_exit_system calls  *
 *    MDD   03-20-92   The great int -> long conversion               *
 *	  MDC	02-19-03   Added another condition to check for before    *
 *					   outputting the message that the units are not  *
 *					   recognized.									  *
 *    MDC   02-22-07   Made change to compare the entire units        *
 *                     expression against the dictionary rather than  *
 *                     attempting to compare it in pieces             *
 **********************************************************************/

LOGICAL ver_units (keyword_ptr)

PARAMETER keyword_ptr;
                                                                      
{
/*static char *unit_abbrev_list [] = {
                                   "A",
                                   "AMPERE",
                                   "B",
                                   "BAR",
                                   "BECQUEREL",
                                   "BIT",
                                   "BQ",
                                   "C",
                                   "CANDELA",
                                   "CD",
                                   "CELSIUS",
                                   "CENTIMETER",
                                   "CM",
                                   "COULOMB",
                                   "D",
                                   "DAY",
                                   "DEG",
                                   "DEGC",
                                   "DEGREE",
                                   "F",
                                   "FARAD",
                                   "G",
                                   "GRAM",
                                   "GRAY",
                                   "GY",
                                   "H",
                                   "HENRY",
                                   "HERTZ",
                                   "HOUR",
                                   "HZ",
                                   "J",
                                   "JOULE",
                                   "K",
                                   "KELVIN",
                                   "KG",
                                   "KILOGRAM",
                                   "KILOMETER",
                                   "KM",
                                   "LM",
                                   "LOCAL_DAY",
                                   "LUMEN",
                                   "LUX",
                                   "LX",
                                   "M",
                                   "METER",
                                   "MICROMETER",
                                   "MICRON",
                                   "MILLIMETER",
                                   "MILLISECOND",
                                   "MIN",
                                   "MINUTE",
                                   "MM",
                                   "MOL",
                                   "MOLE",
                                   "MS",
                                   "N",
                                   "NANOTESLA",
                                   "NEWTON",
                                   "NT",
                                   "OHM",
                                   "PA",
                                   "PASCAL",
                                   "PER",
                                   "PIX",
                                   "PIXEL",
                                   "RAD",
                                   "RADIAN",
                                   "S",
                                   "SECOND",
                                   "SIEMEN",
                                   "SIEVERT",
                                   "SR",
                                   "STERADIAN",
                                   "SV",
                                   "T",
                                   "TESLA",
                                   "V",
                                   "VOLT",
                                   "W",
                                   "WATT",
                                   "WB",
                                   "WEBER",
                                   NULL
	         		 };
  */                      
   VALUE value_ptr = NULL;
   struct ODLUnits *units_ptr = {NULL};
   char *current_char = NULL; 
   char *name_start = NULL;                                    
   long value_count = 0;
   long name_length = 0;
   LOGICAL success = TRUE;
   char *unit_name = NULL;
   static LOGICAL first_time_called = TRUE;
   static long unit_list_end = 0;
   char err_msg [PDS_MAXLINE + 1];
   LOGICAL multiple_values = {FALSE};
   int found;

   /* new code for units 
   FILE        *dd_file;
   AGGREGATE   units_object;
   AGGREGATE   units_root;*/
   STRING_LIST *temp_list;
   /*STRING_LIST *temp_list1;*/
/** BEGIN **/
    /*----------------------------------------------------------------*/    
    /** IF we are an inexperienced routine THEN                      **/    
    /**    determine how many unit names we have to choose from      **/    
    /** ENDIF                                                        **/    
    /*----------------------------------------------------------------*/    



/**************************************************************************/
/* new code developement for obtaining units from data dictionary         */
/**************************************************************************/
 
/**************************************************************************/
/* end of new test code                                                   */
/**************************************************************************/




/***************************
    if (first_time_called)     
    {    
       first_time_called = FALSE;    
       for (unit_list_end = 0;     
	       unit_abbrev_list [unit_list_end] != NULL;
                  unit_list_end++);    
       unit_list_end--;    
    }    
***************************/
    /*----------------------------------------------------------------*/
    /** LOOP thru the value structures until there aren't any more   **/
    /*----------------------------------------------------------------*/
                                       
    multiple_values = (NextValue(FirstValue(keyword_ptr)) != NULL);

    for (value_ptr = FirstValue (keyword_ptr);
            value_ptr != NULL; value_ptr = NextValue (value_ptr))
    {
        if (multiple_values)
            ++value_count;

       /*---------------------------------------------------------------*/    
       /** IF the value pointer is not NULL THEN                       **/    
       /**    determine if the type of the value is integer or real    **/    
       /** ENDIF                                                       **/    
       /*---------------------------------------------------------------*/    

       if (value_ptr != NULL)
       {
          switch (value_ptr -> item.type)
          {
             case TV_INTEGER:   units_ptr = value_ptr -> item.value.integer.units; 
                                break;
             case TV_REAL:      units_ptr = value_ptr -> item.value.real.units;
                                break;
             default:           units_ptr = NULL;
                                break;
          }
       }
       /*---------------------------------------------------------------*/    
       /** Set current_char to the string containing the units         **/
       /*---------------------------------------------------------------*/    

	   if (units_ptr == NULL)
           current_char = NULL;
       else
           current_char = lu_format_units (units_ptr);
    
       /*---------------------------------------------------------------*/    
       /** WHILE the end of the units expression is not found DO       **/    
       /*---------------------------------------------------------------*/    
       
       while (current_char != NULL && *current_char != EOS)    
       {    
          /*------------------------------------------------------------*/    
          /** find the start of the next unit name                     **/    
          /*------------------------------------------------------------*/    

    
          while (*current_char != EOS && !isalpha ((int) *current_char))     
             current_char++;    
    
          /*---------------------------------------------------------------*/    
          /** IF the start of a unit name was found THEN                  **/    
          /*---------------------------------------------------------------*/    
    
          if (*current_char != EOS)    
          {    
             /*---------------------------------------------------------------*/    
             /** determine the length of the unit name within the expression **/    
             /** copy the unit name to a warm, safe place whose size we know **/    
             /*---------------------------------------------------------------*/    
             
             name_start = current_char;

             /*
			 02-22-07 MDC - Grab the entire unit expression instead of attempting
			 to validate the units in pieces
			 */
			 while(*current_char != EOS && *current_char != '>')
				 current_char++;
 /*            while (*current_char != EOS && ((isalpha((int) *current_char))     
                      || (*current_char == '_')))
				current_char++;
*/
             name_length = (long) current_char - (long) name_start;             
	         Malloc_String(unit_name, (int) name_length + 2);
	         strncpy (unit_name, name_start, (int) name_length);
	         *(unit_name + name_length) = EOS;
    
             /*------------------------------------------------------------*/    
             /** search the unit list for the unit name                   **/
             /** IF the unit name was not found THEN                      **/
             /**   it may be that it's a legal unit with an S on the end  **/
             /**   so delete the S on the end, unless that's all there    **/
             /**   is or there are two S's (which is an error)            **/
             /**   search the list again                                  **/ 
             /**   restore the unit name to its former value              **/ 
             /** ENDIF                                                    **/
             /*------------------------------------------------------------*/    
   
		 /* new code for testing*/
             found = PDS_SEARCH_FAIL; /* -1*/
             for (temp_list = unit_list; temp_list != NULL;
		                                     temp_list = temp_list -> next)
			{ 
		          found = strcmp(temp_list ->text, unit_name);
		          if (found == 0) break;
			}
            if (found != 0)
			{
                if ((strlen (unit_name) > 1) && (*(String_End(unit_name)) == 'S' &&
                                                     *(String_End(unit_name) - 1) != 'S'))
				{
                    *(String_End(unit_name)) = EOS;
                    for (temp_list = unit_list; temp_list != NULL;
		                                    temp_list = temp_list -> next)
					{
                        found = strcmp(temp_list ->text, unit_name);
		                if (found == 0) break;
					}
			    strcat(unit_name, "S");
				}
                if (found != 0) found = PDS_SEARCH_FAIL;
			}


           

             /*------------------------------------------------------------*/    
             /** IF the unit wasn't found THEN print an error             **/
             /*------------------------------------------------------------*/    
			
			/*-------------------------------------------------------------*/
			/* 02-19-03 MDC												   */
			/* If the pds_warning flag is OFF, then ignore outputting this */
			/* message.													   */
			/*-------------------------------------------------------------*/
			if ((found == PDS_SEARCH_FAIL) && pds_warning)
			{   
				 sprintf (err_msg, "Unit \"%s\" is not recognized",  unit_name);
                success = err_keyword_message (WARNING, keyword_ptr -> name, 
                                               pds_line_number, 
                                               value_count, err_msg);

             }
             Lemme_Go(unit_name);   
          }    
          /*----------------------------------------------------------*/    
          /** ENDIF the start of a unit name was found...            **/    
          /*----------------------------------------------------------*/    
       }    
       /*-------------------------------------------------------------*/    
       /** ENDWHILE the end of the units expression is not found...  **/    
       /*-------------------------------------------------------------*/    

    }  /*  End:  "for (value_ptr = FirstValue ( ..."  */

    /*----------------------------------------------------------------*/
    /** ENDLOOP thru the values ...                                  **/
    /** RETURN true if no errors were found, false otherwise         **/    
    /*----------------------------------------------------------------*/

    return (success);

/** END  **/

}  /* ver_units */



/**********************************************************************
 *$Component                                                          *
 *    LOGICAL ver_using_class_word (keyword_ptr)                      *
 *$Abstract                                                           *
 *    Verifies a keyword based on its pds_class word.                     *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    KEYWORD                                                         *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    keyword_ptr:                                                    *
 *        The keyword_ptr variable is a pointer to the structure      *
 *        used to represent a keyword in a PDS label.                 *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    verify_success_flag:                                            *
 *        The verify_success_flag variable is a TRUE/FALSE value      *
 *        which indicates whether or not a keyword or object has been *
 *        successfully verified.                                      *
 *$Detailed_Description                                               *
 *    The ver_using_class_word routine determines the data type of    *
 *    the values of a keyword based on its pds_class word and compares    *
 *    this against the data type in the ODL structure used to store   *
 *    each value.                                                     *
 *$Error_Handling                                                     *
 *    If the data type in the ODL structure is different than the     *
 *    data type based on the pds_class word, then an error message is     *
 *    appended onto the global list of messages.                      *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$External_References                                                *
 *    Item                     Shared-Data           Access           *
 *    ------------------------------------------------------------    *
 *    pds_line_number          verlabel.c            read             *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   March 20, 1992                                            *
 *$Change_History                                                     *
 *    DPB   05-30-91   Original code.                                 *
 *    MDD   03-20-92   The great int -> long conversion               *
 **********************************************************************/

LOGICAL ver_using_class_word (keyword_ptr)
                                            
PARAMETER keyword_ptr;

{
    VALUE value_ptr = {NULL};
    char *value_string = {NULL};
    char *type_string = {NULL};
    char err_msg [PDS_MAXLINE];
    long value_count = {0};
    int data_type = {VER_UNKNOWN};
    int odl_data_type = {VER_UNKNOWN};
    LOGICAL success = {TRUE};
    LOGICAL multiple_values = {FALSE};

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Get the data type and type string based on the keyword's pds_class word **/
    /*-----------------------------------------------------------------------*/

    data_type = ver_get_type_from_class (keyword_ptr);
    type_string = ver_get_type_string (data_type);

    /*-----------------------------------------------------------------------*/
    /** Determine if this keyword has more than one value                   **/
    /*-----------------------------------------------------------------------*/

    multiple_values = (NextValue(FirstValue(keyword_ptr)) != NULL);

    /*-----------------------------------------------------------------------*/
    /** LOOP through the list of values . . .                               **/
    /*-----------------------------------------------------------------------*/

    for (value_ptr = FirstValue (keyword_ptr);
            value_ptr != NULL; value_ptr = NextValue (value_ptr))
    {
        /*-------------------------------------------------------------------*/
        /** IF there are more than one THEN                                 **/
        /**     Increment the value count.                                  **/
        /** ENDIF                                                           **/
        /*-------------------------------------------------------------------*/

        if (multiple_values)
            ++value_count;

        /*-------------------------------------------------------------------*/
        /** Attempt to fetch the value from the label.                      **/
        /*-------------------------------------------------------------------*/

        value_string = ver_get_value_string (value_ptr, value_count);

        /*-------------------------------------------------------------------*/
        /** IF the value is not NULL THEN                                   **/
        /*-------------------------------------------------------------------*/

        if ((value_string != NULL) && (! IsNull(value_string)))
        {
            /*---------------------------------------------------------------*/
            /** Get the data type from the ODL structure.                   **/
            /*---------------------------------------------------------------*/

            odl_data_type = ver_get_type_from_odl (value_ptr);

            /*---------------------------------------------------------------*/
            /** IF the ODL data type is not one of the data types allowed   **/
            /**         for this keyword THEN                               **/
            /**     Append error message onto the global list of messages   **/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

            if ((odl_data_type & data_type) != odl_data_type)
            {
                sprintf (err_msg, 
                         "Inconsistent data type.  (Value should be of type %s)",
                         type_string);
                success = err_keyword_message (ERROR1, keyword_ptr -> name, 
                                               pds_line_number, 
                                               value_count, err_msg);

            }  /*  End:  "if ((odl_data_type & ..."  */

        /*-------------------------------------------------------------------*/
        /** ENDIF                                                           **/
        /*-------------------------------------------------------------------*/

        }  /*  End:  "if ((value_string != NULL) ..."  */

        /*-------------------------------------------------------------------*/
        /** Deallocate local storage                                        **/
        /*-------------------------------------------------------------------*/

        Lemme_Go(value_string);

    /*-----------------------------------------------------------------------*/
    /** ENDLOOP                                                             **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "for (value_ptr = FirstValue ( ..."  */

    /*-----------------------------------------------------------------------*/
    /** Deallocate some more local storage                                  **/
    /*-----------------------------------------------------------------------*/

    Lemme_Go(type_string);

    /*-----------------------------------------------------------------------*/
    /** RETURN the verify_success_flag                                      **/
    /*-----------------------------------------------------------------------*/
    
    return (success);

/** END **/
            
}  /*  "ver_using_class_word"  */



/**********************************************************************
 *$Component                                                          *
 *    char *ver_pad_date (date_string)                                *
 *$Abstract                                                           *
 *    Pads a date to full date length                                 *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    date_string:                                                    *
 *        The date_string variable is a character string that         *  
 *        contains a date in either YYYY-MM-DD or YYYY-DDD format.    *
 *$Outputs                                                            *
 *    NONE                                                            *
 *$Returns                                                            *
 *    date_string:                                                    *
 *        The date_string variable is a character string that         *  
 *        contains a date in either YYYY-MM-DD or YYYY-DDD format.    *
 *$Detailed_Description                                               *
 *    The ver_pad_date routine checks the date string passed in,      *
 *    and if it consists of a year only (i.e., has length 4) then     *
 *    this routine will allocate new memory for a longer string and   *
 *    create a new date string consisting of the year from the old    *
 *    date string plus "01-01" for the month and day. The new date    * 
 *    is returned.                                                    *
 *$Limitations                                                        *
 *    This routine cannot pad dates which have only year and month    *
 *    because there is no way to tell them from year/day-of-year      *
 *    dates. Also, it is assumed that the string passed in uses space *
 *    create by malloc and that the string can therefore be freed.    *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/JPL                                             *
 *$Version_and_Date                                                   *
 *    1.1  October 22, 1991                                           *
 *$Change_history                                                     *
 *    MDD   06-12-91   Original code.                                 *
 *    MDD   10-22-91   Fixed malloc, free, and sys_exit_system calls  *
 **********************************************************************/

char *ver_pad_date (date_string)

char *date_string;

{
   char temp_str [PDS_MAXLINE + 1];

   if (date_string != NULL)
   {
      if (strlen (date_string) == 4)
      {
         sprintf (temp_str, "%s-01-01", date_string);
         Lemme_Go(date_string);
         Malloc_String(date_string, (int) String_Size(temp_str));
         strcpy (date_string, temp_str);
      }
   }
   return (date_string);
}



/**********************************************************************
 *$Component                                                          *
 *    char *ver_pad_time (time_string)                                *
 *$Abstract                                                           *
 *    Pads a date to full time length                                 *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    time_string:                                                    *
 *        The time_string variable is a character string that         *  
 *        contains a time value in the format HH:MM:SS.SSS.           *
 *$Outputs                                                            *
 *    NONE                                                            *
 *$Returns                                                            *
 *    time_string:                                                    *
 *        The date_string variable is a character string that         *  
 *        contains a time in either HH:MM:SS.SSS format.              *
 *$Detailed_Description                                               *
 *    The ver_pad_time routine checks the time string passed in,      *
 *    and if it consists of hour only (i.e., has length 1 or 2) then  *
 *    this routine will allocate new memory for a longer string and   *
 *    create a new time string consisting of the hour from the old    *
 *    time string plus "00:00:00.00Z". The new time is returned.      * 
 *$Limitations                                                        *
 *    The ODL parser pads all times which have at least hours and     *
 *    minutes included, so this routine does not bother with those.   *
 *    Also, it is assumed that the string passed in uses space        *
 *    create by malloc and that the string can therefore be freed.    *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/JPL                                             *
 *$Version_and_Date                                                   *
 *    1.1   October 22, 1991                                          *
 *$Change_history                                                     *
 *    MDD   06-12-91   Original code.                                 *
 *    MDD   10-21-91   Fixed malloc, free, and sys_exit_system calls  *
 **********************************************************************/

char *ver_pad_time (time_string)

char *time_string;

{
   char temp_str [PDS_MAXLINE + 1];
   
   if (time_string != NULL)
   {
      if (strlen (time_string) <= 2)
      {
	 sprintf (temp_str, "%s:00:00.00Z", time_string);
         Lemme_Go(time_string);
         Malloc_String(time_string, (int) String_Size(temp_str));
         strcpy (time_string, temp_str);
      }
   }
   return (time_string);
}



/**********************************************************************
 *$Component                                                          *
 *    char *ver_pad_date_time (date_time_string)                      *
 *$Abstract                                                           *
 *    Pads a date-time to full date-time length                       *
 *$Keywords                                                           *
 *    VERIFY                                                          *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    date_time_string:                                               *
 *        The date_time_string variable is a character string that    *  
 *        contains a date-time value in the format                    *
 *        YYYY-MM-DDTHH:MM:SS.SSS.                                    *
 *$Outputs                                                            *
 *    NONE                                                            *
 *$Returns                                                            *
 *    date_time_string:                                               *
 *        The date_time_string variable is a character string that    *  
 *        contains a date-time value in the format                    *
 *        YYYY-MM-DDTHH:MM:SS.SSS.                                    *
 *$Detailed_Description                                               *
 *    The ver_pad_date_time routine checks the string passed in,      *
 *    and if it consists of only a date, it pads the date to full     *
 *    length and adds a dummy time field consisting of all zeros.     *
 *    The new date time is returned.                                  * 
 *$Limitations                                                        *
 *    The same limitations apply to padding dates as apply to the     *
 *    ver_pad_date routine.                                           *
 *                                                                    *
 *    The ODL parser pads all times which have at least hours and     *
 *    minutes included, so this routine does not bother with those.   *
 *    Also, it is assumed that the string passed in uses space        *
 *    create by malloc and that the string can therefore be freed.    *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/JPL                                             *
 *$Version_and_Date                                                   *
 *    1.1 October 22, 1991                                            *
 *$Change_history                                                     *
 *    MDD   06-12-91   Original code.                                 *
 *    MDD   10-22-91   Fixed malloc, free, and sys_exit_system calls  *
 **********************************************************************/


char *ver_pad_date_time (date_time_string)

char *date_time_string;
{
   char temp_str [PDS_MAXLINE + 1];
   
   date_time_string = ver_pad_date (date_time_string);
   if (strchr (date_time_string, 'T') == NULL)
   {
       sprintf (temp_str, "%sT00:00:00.000Z", date_time_string);
       Lemme_Go(date_time_string);
       Malloc_String(date_time_string, (int) String_Size(temp_str));
       strcpy (date_time_string, temp_str);
   }
   return (date_time_string);
}


/**********************************************************************
 *$Component                                                          *
 *    LOGICAL ver_init (init_index_name)                              *
 *$Abstract                                                           *
 *    Initializes the units abbreviations variable.                   *
 *$Inputs                                                             *
 *    init_index_name:                                                *
 *       The init_index_name variable is a character string containing*
 *       the name of a DD index file.                                 *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *$Detailed_Description                                               *
 *    ver_init initializes the global used by ver_units.              *
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
 *    Dale Schultz ACRO/JPL                                           *
 *$Version_and_Date                                                   *
 *    1.0   August 25, 1998                                           *
 *$Change_History                                                     *
 *    DWS   08-25-98   Original generation.                           *
 *    MDC   03-05-05   Check for NULL value in dd_file_name first     *
 *                     before using the fopen function on it          *
 **********************************************************************/

void ver_init (init_index_name)

char *init_index_name;
{
   AGGREGATE root = NULL;
   int label_status = PDS_ERROR;
   char *temp_ptr = NULL;
   char *temp_path = NULL;
   char *dd_temp_path = NULL;
   FILE *test_file = NULL;
   LOGICAL success = TRUE;
   int status;
   AGGREGATE unit_root;
   AGGREGATE unit_object;
   PARAMETER temp_parm;

   /** BEGIN **/
   /*-----------------------------------------------------------------*/
   /** Attempt to read the label on the index file                   **/
   /*-----------------------------------------------------------------*/
  
   root = lab_read_label_or_template (init_index_name);
   if (root != NULL)
   {


      /*-------------------------------------------------------------*/
      /** get the unit offset from the label                        **/
      /*-------------------------------------------------------------*/
      temp_ptr = lab_get_value (root, "ROOT", NULL, 0, "UNIT_LIST", 0,
                                   1, FALSE, &label_status);
      if (label_status == PDS_SUCCESS)
      {
         sscanf (temp_ptr, " %ld ", &dd_unit_offset);
         Lemme_Go(temp_ptr);
      }
      else
      {
         success = FALSE;
      }

      /*-------------------------------------------------------------*/
      /** IF this value was not in the label THEN                   **/
      /**   append an error message to the global list              **/
      /*-------------------------------------------------------------*/

      if (success == FALSE)
      {
         err_append_message (ERROR1, 
              "The label on the data dictionary index file is incomplete");
         err_append_message (ERROR1, 
              "The UNIT_LIST keyword is missing");
      }
   }
   /*-----------------------------------------------------------------*/
   /** ENDIF the label on the index file can be read...              **/
   /** initialize the DD definitions in memory                       **/
   /*-----------------------------------------------------------------*/

   /* 03-05-05 MDC - Check for NULL in dd_file_name first */
   if(strcmp(dd_file_name,"\0") != 0)
		test_file = fopen (dd_file_name, "r");
   if (test_file != NULL)
   {
      /*------------------------------------------------------------------*/
      /** read the unit object from the file onto a new ODL tree         **/
      /*------------------------------------------------------------------*/

      unit_root = lu_append_object (NULL, "ROOT");
      fseek (test_file, dd_unit_offset, 0);
      ReadLabel (test_file, unit_root);
      fclose (test_file);
      /*------------------------------------------------------------------*/
      /** locate the unit list object                                    **/
      /** fetch all values of the object alias sequence parameter        **/
      /*------------------------------------------------------------------*/

      unit_object = FindObject (unit_root, "UNIT_LIST", NULL);
      if (unit_object != NULL)
	  {
		  temp_parm = FindParameter (unit_object, "UNIT_SEQUENCE");
          unit_list = lu_fetch_all_values (temp_parm, FALSE, FALSE);
	  }
/*

         unit_list = lu_fetch_all_values (FindParameter (unit_object, 
                                               "UNIT_SEQUENCE"), 
                                                  FALSE, FALSE);
*/

   }
   lab_remove_label (root, &status);
   dd_cleanup_defs ();
}



