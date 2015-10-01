/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Component                                                           * 
 *    Library labtool.c                                                *
 * Abstract                                                            *
 *    High-level PDS label utilities                                   *
 * Detailed Description                                                *
 *    The labtool.c file contains the subroutines used by              *
 *    PDS software to perform a variety of high-level label            *
 *    manipulation tasks.                                              *
 * Internal References                                                 *
 *    lt_add_pointer                                                   *
 *    lt_get_pointer                                                   *
 *    lt_global_keyword_change                                         *
 *    lt_move_keyword                                                  *
 *    lt_read_expanded_label                                           *
 *    lt_replace_keyword                                               *
 *    ltx_attach_file_name                                             *
 *    ltx_merge_objects                                                *
 * Authors and Institutions                                            *
 *    Marti D. DeMore/ JPL                                             *
 * Version and Date                                                    *
 *    1.0   September 9, 1991                                          *
 * Change History                                                      *
 *    MDD   09-09-91   Original code.                                  *
 *    MDD   06-22-92   Removed lt_read_label_only.                     *
 *    DWS   05-23-02   modified lt_read_expanded_label to look in LABEL*
 *                     directory for files pointed to by pointers      *
 *                     Also added get_label_dir to search the          *
 *                     additional directorys.                          *
 *	  MDC	03-04-03   Modified lt_read_expanded_label with conditional*
 *					   compile statements for LV_TOOL to keep track    *
 *					   of errors found while parsing the label file.   *
 *	  MDC	03-05-03   Modified the ltx_attach_file_name routine to    *
 *					   avoid segmentation faults when running in LINUX.*
 *    MDC   01-26-06   Modified lt_read_expanded_label and             *
 *                     get_label_dir routines to properly search for   *
 *                     STRUCTURE pointer files in non-windows systems. *
 *                     We could have a file whose directory path is    *
 *                     made up of lowercase and uppercase letters and  *
 *                     the routines wouldn't originally find it.       *
 *    MDC   06-28-06   Modified get_label_dir to prevent memory leak   *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "pdsdef.h"
#include "label.h"
#include "labtool.h"
#include "labutil.h"
#include "utildef.h"
#include "errordef.h"
#include "sysdef.h"

extern LOGICAL pds_watch_ends;
extern LOGICAL pds_verbose;
extern ERROR_LIST *pds_last_message;

#ifdef LV_TOOL

extern LOGICAL track_odl_errors; /* 03-04-03 */

#endif

extern LOGICAL pds_warning;

#ifndef SUN_UNIX

static void ltx_attach_file_name (AGGREGATE, char *);
static void ltx_merge_objects (AGGREGATE, AGGREGATE);

#else

static void ltx_attach_file_name ();
static void ltx_merge_objects ();

#endif
char *get_label_dir(char *, char *, char *, char *, FILE *);

/**********************************************************************
 *$Component                                                          *
 *    LOGICAL lt_global_keyword_change (label_ptr, keyword_name,      *
 *                                      change_type, new_keyword_name,*
 *                                      new_keyword_value)            *
 *$Abstract                                                           *
 *    Makes a global change to a PDS label keyword                    *
 *$Keywords                                                           *
 *    LABEL_TOOL                                                      *
 *    KEYWORD                                                         *
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
 *    keyword_name:                                                   *
 *        The keyword_name variable is a character string             *
 *        which contains the name of a keyword in a PDS label         *
 *        (e.g., the line "SCID = VG1" implies that "SCID" is the     *
 *         keyword_name.                                              *
 *    change_type:                                                    *
 *        The change_type variable is an integer representing the     *
 *        type of global change to take place on a PDS label.         *
 *    new_keyword_name:                                               *
 *        The new_keyword_name variable is a character string         *
 *        which contains the name of a keyword in a PDS label         *
 *        (e.g., the line "SCID = VG1" implies that "SCID" is the     *
 *        keyword_name.                                               *
 *    new_keyword_value:                                              *
 *        The new_keyword_value variable is a character string        *
 *        which contains the name of a keyword in a PDS label         *
 *        (e.g., the line "SCID = VG1" implies that "VG1" is the      *
 *        keyword_value.                                              *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure        *
 *        (a value of FALSE).                                         *
 *$Detailed_Description                                               *
 *    The lt_global_keyword_change routine performs a keyword change  *
 *    on an entire label subtree.  The change_type input can be:      *
 *                                                                    *
 *    PDS_LAB_ADD: A keyword called keyword_name is added to every    *
 *                 object in the label tree pointed to by label_ptr.  *
 *                 The value of the keyword will be new_keyword_value,*
 *                 and the new_keyword_name parameter is not used.    *
 *    PDS_LAB_REMOVE: Every occurrence of the keyword keyword_name is *
 *                 removed from the label pointed to by label_ptr.    *
 *                 The new_keyword_name and new_keyword_value         *
 *                 parameters are not used.                           *
 *    PDS_LAB_CHANGE_NAME: Every occurence of keyword_name in the     *
 *                 label pointed to by label_ptr has its name changed *
 *                 to new_keyword_name. The new_keyword_value input   *
 *                 is not used.                                       *
 *    PDS_LAB_CHANGE_VALUE: Every occurence of keyword_name in the    *
 *                 label pointed to by label_ptr has its value changed*
 *                 to new_keyword_value.  The new_keyword_name input  *
 *                 is not used.                                       *
 *    If the indicated operation is performed at least once, TRUE is  *
 *    returned. Otherwise, FALSE is returned.                         *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/JPL                                             *
 *$Version_and_Date                                                   *
 *    1.0   September 3, 1991                                         *
 *$Change_History                                                     *
 *    MDD   09-03-91   Original Code.                                 *
 **********************************************************************/

LOGICAL lt_global_keyword_change (label_ptr, keyword_name, change_type,
			          new_keyword_name, new_keyword_value)
AGGREGATE label_ptr;
char *keyword_name;
int change_type;
char *new_keyword_name;
char *new_keyword_value;
{
   AGGREGATE temp_object_ptr = NULL;
   int status;
   int i;
   LOGICAL success = FALSE;

/** BEGIN **/

   /*------------------------------------------------------------------*/
   /** IF a keyword name and a label were provided THEN               **/
   /*------------------------------------------------------------------*/

   status = PDS_ERROR;
   if (keyword_name != NULL && label_ptr != NULL)
   {
      /*---------------------------------------------------------------*/
      /** LOOP through all the objects in the label subtree           **/
      /*---------------------------------------------------------------*/

      temp_object_ptr = label_ptr;
      while (temp_object_ptr != NULL)
      {
         /*------------------------------------------------------------*/
         /** CASE operation of                                        **/
         /*------------------------------------------------------------*/

	     switch (change_type)
	     {

            /*---------------------------------------------------------*/
            /** ADD: add a keyword called keyword_name to the current **/
            /**      object                                           **/
            /*---------------------------------------------------------*/

	    case PDS_LAB_ADD:
	       lab_add_parameter (temp_object_ptr, NULL, NULL, 1,
				  keyword_name, new_keyword_value, &status);
               success = success || (status == PDS_SUCCESS);
	       break;

            /*---------------------------------------------------------*/
            /** REMOVE: remove all keywords called kewyord_name from  **/
            /**         the current object                            **/
            /*---------------------------------------------------------*/

	    case PDS_LAB_REMOVE:
		do
		{
		   lab_remove_parameter (temp_object_ptr, NULL, NULL, 1,
					 keyword_name, 1, &status);
                   success = success || (status == PDS_SUCCESS);
		}
		while (status == PDS_SUCCESS);
		break;

            /*---------------------------------------------------------*/
            /** CHANGE_NAME: change all keywords called keyword_name  **/
            /**              in the current object to new_keyword_name**/
            /*---------------------------------------------------------*/

	    case PDS_LAB_CHANGE_NAME:
	       do
	       {
		  lab_change_parameter_name (temp_object_ptr, NULL,
					 NULL, 1, keyword_name,
					 1, new_keyword_name, &status);
                  success = success || (status == PDS_SUCCESS);
	       } while (status == PDS_SUCCESS);
	       break;

            /*---------------------------------------------------------*/
            /** CHANGE_VALUE: change all keywords called keyword_name **/
            /**               in the current object to have new value **/
            /**               new_keyword_value.                      **/
            /*---------------------------------------------------------*/

	    case PDS_LAB_CHANGE_VALUE:
	       i = 1;
	       do
	       {
		  lab_change_value (temp_object_ptr, NULL, NULL, 1,
				    keyword_name, i, new_keyword_value,
				    &status);
		  i++;
                  success = success || (status == PDS_SUCCESS);
	       } while (status == PDS_SUCCESS);
	       break;
	 }
         /*------------------------------------------------------------*/
         /** ENDCASE                                                  **/
         /*------------------------------------------------------------*/

	 temp_object_ptr = NextSubAggregate (label_ptr, temp_object_ptr);
      }
      /*---------------------------------------------------------------*/
      /** ENDLOOP                                                     **/
      /*---------------------------------------------------------------*/
   }
   /*------------------------------------------------------------------*/
   /** ENDIF                                                          **/
   /*------------------------------------------------------------------*/

   return (success);

/** END **/
}

/**********************************************************************
 *$Component                                                          *
 *    LOGICAL lt_move_keyword (label_ptr, object_class, object_name,  *
 *                             object_position, keyword_name,         *
 *                             keyword_position, new_object_class,    *
 *                             new_object_name, new_object_position,  *
 *                             required_keyword)                      *
 *$Abstract                                                           *
 *    Moves a keyword from one object to another in a PDS Label.      *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    KEYWORD                                                         *
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
 *    object_class:                                                   *
 *        The object_class variable is a character string             *
 *        which contains the pds_class of an object in a PDS label        *
 *        (e.g., "OBJECT = IMAGE" implies that "IMAGE" is the pds_class   *
 *        of the object).                                             *
 *    object_name:                                                    *
 *        The object_name variable is a character string              *
 *        which contains the name of an object in a PDS label.        *
 *        This is assumed to be the value of the "NAME" parameter     *
 *        in the object (e.g., "NAME = VOYAGER" implies that          *
 *        "VOYAGER" is the name of the object).                       *
 *    object_position:                                                *
 *        The object_position variable is an integer which            *
 *        represents the relative position of an object in a PDS      *
 *        label.  If this variable is used in conjunction with either *
 *        the object_class or object_name variables, then it          *
 *        represents a particular occurrence of that thing in the     *
 *        label (e.g., if object_class is "TABLE" and object_position *
 *        is 3, then this represents the third "TABLE" object in the  *
 *        label).  On the other hand, if this variable is used by     *
 *        itself, it represents the absolute position of the object,  *
 *        starting from the "ROOT" object (position = 1).  The        *
 *        sequence follows the structure of the label as it looks     *
 *        in a flat file.                                             *
 *    keyword_name:                                                   *
 *        The keyword_name variable is a character string             *
 *        which contains the name of a parameter in a PDS label       *
 *        (e.g., the line "SCID = VG1" implies that "SCID" is the     *
 *        keyword name).                                              *
 *    keyword_position:                                               *
 *        The keyword_position variable is an integer which           *
 *        represents the relative position of a parameter in an       *
 *        object in a PDS label.  If this variable is used in         *
 *        conjunction with the keyword_name variable, then it         *
 *        represents a particular occurrence of that parameter        *
 *        in the object (e.g., if parameter_name is "SCID" and        *
 *        parameter_position is 2, then this represents the second    *
 *        "SCID" parameter in the object).  On the other hand, if     *
 *        this variable is used by itself, it represents the absolute *
 *        position of the parameter within the object, starting with  *
 *        first parameter in the object (position = 1).               *
 *    new_object_class:                                               *
 *        The new_object_class variable is a character string         *
 *        which contains the pds_class of an object in a PDS label        *
 *        (e.g., "OBJECT = IMAGE" implies that "IMAGE" is the pds_class   *
 *        of the object).                                             *
 *    new_object_name:                                                *
 *        The new_object_name variable is a character string          *
 *        which contains the name of an object in a PDS label.        *
 *        This is assumed to be the value of the "NAME" parameter     *
 *        in the object (e.g., "NAME = VOYAGER" implies that          *
 *        "VOYAGER" is the name of the object).                       *
 *    new_object_position:                                            *
 *        The new_object_position variable is an integer which        *
 *        represents the relative position of an object in a PDS      *
 *        label.  If this variable is used in conjunction with either *
 *        the object_class or object_name variables, then it          *
 *        represents a particular occurrence of that thing in the     *
 *        label (e.g., if object_class is "TABLE" and object_position *
 *        is 3, then this represents the third "TABLE" object in the  *
 *        label).  On the other hand, if this variable is used by     *
 *        itself, it represents the absolute position of the object,  *
 *        starting from the "ROOT" object (position = 1).  The        *
 *        sequence follows the structure of the label as it looks     *
 *        in a flat file.                                             *
 *    required_keyword:                                               *
 *        The required_keyword variable is a TRUE/FALSE flag that     *
 *        indicates whether a keyword is required in its parent object*
 *        or not.                                                     *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure        *
 *        (a value of FALSE).                                         *
 *$Detailed_Description                                               *
 *    The lt_move_keyword routine searches for the object given by    *
 *    object_class, object_name, and object_position (according to    *
 *    the search rules used by the PDS label library).  If this       *
 *    object contains keyword keyword_name, the keyword and its value *
 *    are relocated to the object specified by new_object_class,      *
 *    new_object_name, and new_object_position.  If the first object  *
 *    or the keyword cannot be found but the required_keyword input   *
 *    is TRUE, the keyword is still added to the new object (if the   *
 *    object is found) and is given the value UNKNOWN. TRUE is then   *
 *    returned if the operation succeeded, and FALSE otherwise.       *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/JPL                                             *
 *$Version_and_Date                                                   *
 *    1.0   September 3, 1991                                         *
 *$Change_History                                                     *
 *    MDD   09-03-91   Original Code.                                 *
 **********************************************************************/

LOGICAL lt_move_keyword (label_ptr, object_class, object_name,
                         object_position, keyword_name,
                         keyword_position, new_object_class,
                         new_object_name, new_object_position,
                         required_keyword)

AGGREGATE label_ptr;
char *object_class;
char *object_name;
int object_position;
char *keyword_name;
int keyword_position;
char *new_object_class;
char *new_object_name;
int new_object_position;
LOGICAL required_keyword;

{
   int status;
   LOGICAL success = FALSE;
   AGGREGATE object_ptr = NULL;
   PARAMETER parameter_ptr = NULL;

/** BEGIN **/

   /*-----------------------------------------------------------------*/
   /** Find the old object and keyword                               **/
   /** IF it was found THEN                                          **/
   /*-----------------------------------------------------------------*/

   status = PDS_ERROR;
   parameter_ptr = lab_find_parameter (label_ptr, object_class,
                   object_name, object_position, keyword_name,
                   keyword_position, &status);

   if (parameter_ptr != NULL && status == PDS_SUCCESS)
   {
      /*--------------------------------------------------------------*/
      /** Find the new object                                        **/
      /** Cut the parameter from its old object and paste it into the**/
      /**    new one                                                 **/
      /*--------------------------------------------------------------*/

      status = PDS_ERROR;
      object_ptr = lab_find_object (label_ptr, new_object_class,
                   new_object_name, new_object_position,
                   &status);
      if (object_ptr != NULL && status == PDS_SUCCESS)
      {
         CutParameter (parameter_ptr);
         PasteParameter (object_ptr, parameter_ptr);
         success = TRUE;
      }
   }

   /*-----------------------------------------------------------------*/
   /** ELSE IF the keyword was required THEN                         **/
   /**     try to add it to the new object with value UNKNOWN        **/
   /*-----------------------------------------------------------------*/

   else if (required_keyword)
   {
      lab_add_parameter (label_ptr, new_object_class, new_object_name,
                         new_object_position, keyword_name, "UNKNOWN",
                         &status);
      success = (status == PDS_SUCCESS);
   }

   /*-----------------------------------------------------------------*/
   /** ENDIF                                                         **/
   /*-----------------------------------------------------------------*/

   return (success);

/** END **/
}

/**********************************************************************
 *$Component                                                          *
 *    AGGREGATE lt_read_expanded_label (label_fname)                  *
 *$Abstract                                                           *
 *    Read STRUCTURE pointers and build an expanded tree.             *
 *$Keywords                                                           *
 *    LABEL_TOOL                                                      *
 *    STRUCTURE                                                       *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    label_fname:                                                    *
 *        The label_fname variable is a pointer to a character string *
 *        containing a file name.                                     *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    This routine takes a file containing a PDS label, builds a tree *
 *    from the ODL statements in the file, searches the tree for all  *
 *    STRUCTURE keywords, builds trees from the label files pointed   *
 *    to by the STRUCTURE keywords, merges the new trees into the     *
 *    parent tree where it found the STRUCTURE keyword, continues     *
 *    searching the tree, including the newly merged part, for more   *
 *    STRUCTURE keywords to expand.  The file names extracted from    *
 *    the STRUCTURE keywords are appended onto a STRING_LIST which    *
 *    is attached to the root node of the main tree by using the      *
 *    "appl2" field.  The "appl2" field in each child of the root,    *
 *    including the expanded and merged parts, is used to point to    *
 *    the text part of the STRING_LIST element which has the file name*
 *    where the objects were pulled in from.                          *
 *$External_References                                                *
 *    Item                       Shared-Data               Access     *
 * ------------------------------------------------------------------ *
 *  pds_verbose                  pdsglob.h                 write      *
 *  pds_watch_ends               pdsglob.h                 write      *
 *  pds_last_message             pdsglob.h                 write      *
 *$Author_and_Institution                                             *
 *    David P. Bernath / JPL                                          *
 *$Version_and_Date                                                   *
 *    2.0   March 3, 1992                                             *
 *$Change_History                                                     *
 *    DPB   12-04-91   Original Code.                                 *
 *    MDD   03-02-92   Corrected bugs resulting when structure files  *
 *                     could not be found or when more than one       *
 *                     structure pointer existed at the ROOT level.   *
 *                     Added code to turn off end statement checking  *
 *                     and to add file name indicators to the error   *
 *                     list.                                          *
 *    MDC	03-04-03   Added conditional compile statements for		  *
 *					   LV_TOOL to keep track of the number of errors  *
 *					   found while parsing the label file.			  *
 *    MDC   01-25-06   Modified routine to look for filenames and     *
 *                     directories "as-is" first before doing casing  *
 *                     manipulation. This is because non-windows      *
 *                     environments are case-sensitive and this       *
 *                     modification allows us to search for           *
 *                     directories that happen to be made up of       *
 *                     lowercase and uppercase letters.               *
 *                     i.e. /home/mdc/MIRO/DATA/LABEL.LBL             *
 *    MDC   07-05-06   Made changes to properly allocate memory after *
 *                     finding the FMT file in a LABEL directory      *
 **********************************************************************/

AGGREGATE lt_read_expanded_label (label_fname)

char *label_fname;

{
    AGGREGATE label_ptr = {NULL};
    AGGREGATE sub_label_ptr = {NULL};
    AGGREGATE object_ptr = {NULL};
    AGGREGATE parent_ptr = {NULL};
    char err_msg [PDS_MAXLINE];
    char *structure_fname = {NULL};
    int i;
    LOGICAL found = {FALSE};
    LOGICAL save_watch;
    ERROR_LIST *message_ptr = {NULL};
    LOGICAL save_verbose;
    char savefile [PDS_MAXLINE];
    char *newfile = {NULL};
    char *label_dir = {NULL};
    char *struct_dir = {NULL};
    FILE *f_ptr = NULL;
	char upper_name[PDS_MAXLINE];
	char lower_name[PDS_MAXLINE];
	char *gld_result = NULL;

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Save the previous "end watch" and verbose flags                     **/
    /** Add a message header for this file                                  **/
    /** Save the directory name of the label file                           **/
    /** Construct the main tree from the file name passed in.               **/
    /** Remove the message header if the label had no errors.               **/
    /** Add a message trailer if the label had errors.                      **/
    /** Attach the file name to all the objects in the main tree.           **/
    /*-----------------------------------------------------------------------*/

    save_watch = pds_watch_ends;
    save_verbose = pds_verbose;
    label_dir = sys_get_path (label_fname);
    sprintf (err_msg, "***** Messages from LABEL file: %s *****", label_fname);
    err_append_message (CONTINUE, err_msg);
    message_ptr = pds_last_message;

	/*------------------------------------------------------------------------*/
	/* 03-04-03 MDC															  */
	/* Added the conditional compile statements to turn the track_odl_errors  */
	/* ON and OFF. Since the label file gets parsed multiple times throughout */
	/* lvtool, we need to know when and when not to update the running count  */
	/* of errors accumulated.												  */
	/*------------------------------------------------------------------------*/

	/* 02-16-06 MDC - We shouldn't be keeping track of ODL errors at this point. 
	          We're just attempting to expand the label. */
/*
#ifdef LV_TOOL
	track_odl_errors = TRUE; 
#endif
*/
	label_ptr = lab_read_label_or_template (label_fname);
/*	
#ifdef LV_TOOL
	track_odl_errors = FALSE;
#endif 
*/
    if (message_ptr == pds_last_message)
       err_deallocate_list (message_ptr);
    else
    {
       sprintf (err_msg, 
                "***** End of messages for LABEL file: %s *****", 
                label_fname);
       err_append_message (CONTINUE, err_msg);
    }

    ltx_attach_file_name (label_ptr, label_fname);

    /*-----------------------------------------------------------------------*/
    /** LOOP through the objects in the main tree, including any that have  **/
    /**         been merged in from previous STRUCTURE keywords.            **/
    /*-----------------------------------------------------------------------*/

    for (object_ptr = label_ptr; 
             object_ptr != NULL; object_ptr = NextAggregate(object_ptr))
    {
        /*-------------------------------------------------------------------*/
        /** Turn off the sfdu warnings                                      **/
        /** Turn off the "watch end statements" flag.                       **/
        /** LOOP through all the STRUCTURE keywords in the current object.  **/
        /*-------------------------------------------------------------------*/

        pds_verbose = FALSE;
        pds_watch_ends = FALSE;
        for (i=1; (structure_fname = lu_keyword_value (object_ptr,
			                  "STRUCTURE",i,FALSE)) != NULL; ++i) 
        {    

            if ((struct_dir = sys_get_path (structure_fname)) == NULL)
            {
                strcpy (savefile, structure_fname);  /* copy the name for safe keeping      */
				strcpy(upper_name, structure_fname); /* copy it here for conversion         */
				strcpy(lower_name, structure_fname); /* copy it here for conversion         */
				
				Lemme_Go(structure_fname);			 /* Get rid of this, it isn't big enough*/
				Malloc_String(structure_fname, PDS_MAXLINE ); /* make it bigger              */
				structure_fname[0] = '\0';
				util_upper_case(upper_name);	     /* convert the name to all upper case  */
				util_lower_case(lower_name);         /* convert the name to all lower case  */
				
				/* 01-25-06 MDC - Add a check to see if you can open the file as-is. This is
				   meaningful to non-windows systems since filenames are case-sensitive in those
				   environments.
			    */
				if ((f_ptr = fopen (savefile, "r")) != NULL)
				{
					strcpy(structure_fname, savefile);
				}
				else if ((f_ptr = fopen (upper_name, "r")) != NULL)   /*try to open it in CWD as upper case, */
				{	
					strcpy(structure_fname, upper_name);/*put the file spec in structure_fname)*/
				}									    /* if it did not open then try lower case*/
				else if ((f_ptr = fopen (lower_name, "r")) != NULL)  /*try to open it in CWD, if it did then get out*/
				{
					strcpy(structure_fname, lower_name);/*put the file spec in structure_fname)*/
				}

				/* 01-25-06 MDC - Add a check to see if you can open the file as-is after forming
				   the newfile. This is meaningful to non-windows systems since filenames are 
				   case-sensitive in those environments
			    */
				if(f_ptr == NULL)
				{
					newfile = util_create_file_spec (label_dir, savefile);
					Replace_String(structure_fname, newfile);
					Lemme_Go(newfile);
					f_ptr = fopen (structure_fname, "r");
				}
				/* 01-25-06 MDC - Replace strcpy statements with Replace_String statements.
				   If filenames become too big, we won't risk a chance of having a memory leak.
			    */
				if(f_ptr == NULL)
				{
                    newfile = util_create_file_spec (label_dir, savefile);
					util_upper_case (newfile);
					Replace_String(structure_fname, newfile);
                   /* strcpy(structure_fname, newfile); */
                    Lemme_Go(newfile);
                    f_ptr = fopen (structure_fname, "r");
				}
				if(f_ptr == NULL)
				{
                    newfile = util_create_file_spec (label_dir, savefile);
					util_lower_case (newfile);
					Replace_String(structure_fname, newfile);
                   /* strcpy(structure_fname, newfile); */
                    Lemme_Go(newfile);
                    f_ptr = fopen (structure_fname, "r");
				}
			    if(f_ptr == NULL)  /*it wasn't in CWD.  Lets see if we can find it in a LABEL directory somewhere*/
				{
					gld_result = get_label_dir(label_dir, "LABEL", savefile, structure_fname, f_ptr);
					if(gld_result != NULL)
					{
						Replace_String(structure_fname, gld_result);
						Lemme_Go(gld_result);
					}
				}
			}				
			Lemme_Go(struct_dir);
/*******************************************************************************/
            if (f_ptr) fclose (f_ptr);

            /*---------------------------------------------------------------*/
            /** Determine if the file name attached to the current STRUCTURE**/
            /**     keyword is being included recursively (e.g., one of     **/
            /**     its parent objects came from the same file).            **/
            /*---------------------------------------------------------------*/

            for (parent_ptr=object_ptr->parent, found=FALSE;
                    ((parent_ptr != NULL) && (parent_ptr != label_ptr) && (! found)); 
                        parent_ptr = parent_ptr -> parent)
            {
                found = (strcmp (structure_fname, 
                                     (char *) (parent_ptr->appl2)) == 0);
            }

            /*---------------------------------------------------------------*/
            /** IF this is a recursive STRUCTURE reference THEN             **/
            /**     Append a message onto the list of messages.             **/
            /** ELSE                                                        **/
            /*---------------------------------------------------------------*/

          /* if (found) */
			if(found && pds_warning)
            {
                sprintf (err_msg, 
                  "Recursive STRUCTURE reference found, involving the file: %s",
                   structure_fname);
		        err_append_message (WARNING, err_msg);
            }
            else
            {
                /*-----------------------------------------------------------*/
                /** Create a message header for this structure file         **/
                /** Build a temp tree from the STRUCTURE file name.         **/
                /** Remove the message header if there were no errors in    **/
                /**    the label.                                           **/
                /** Add a message trailer if the label had errors.          **/
                /*-----------------------------------------------------------*/

                sprintf (err_msg, 
                         "***** ODL syntax messages for STRUCTURE file: %s *****", 
                         structure_fname);
		        err_append_message (CONTINUE, err_msg);
                message_ptr = pds_last_message;
                sub_label_ptr = lab_read_label_or_template (structure_fname);
                if (message_ptr == pds_last_message)
		        err_deallocate_list (message_ptr);
                else
                {
                    sprintf (err_msg, 
                         "***** End of messages for STRUCTURE file: %s *****", 
                         structure_fname);
		            err_append_message (CONTINUE, err_msg);
		        }

                /*-----------------------------------------------------------*/
                /** IF a structure file was found THEN                      **/
                /**    Attach the file name to all objects in the temp tree **/
                /**    Append the temp tree's file name list onto the main  **/
                /**        tree's list.                                     **/
                /**    Merge the temp tree into the main tree where the     **/
                /**        STRUCTURE keyword was found.                     **/
                /**    Remove the temp tree.                                **/
                /*-----------------------------------------------------------*/

                if (sub_label_ptr != NULL)
                {
                   ltx_attach_file_name (sub_label_ptr, structure_fname);
                   label_ptr->appl2 = (long) util_append_string_list (
                                           (STRING_LIST *) (label_ptr->appl2), 
					   (char *) (sub_label_ptr->appl2),
					    LIST_TYPE);
                   ltx_merge_objects (sub_label_ptr, object_ptr);
                   lab_remove_label_or_template (sub_label_ptr);
		        }
                /*-----------------------------------------------------------*/
                /** ELSE                                                    **/
                /**  append "NULL_FILE" onto the list of file names         **/
                /**  notify the user of the error                           **/
                /*-----------------------------------------------------------*/

                else
                {
                   label_ptr->appl2 = (long) util_append_string_list (
                                           (STRING_LIST *) (label_ptr->appl2), 
                                           "NULL_FILE", STRING_TYPE);
                   sprintf (err_msg, 
                     "Missing or bad STRUCTURE file: %s", structure_fname);
		       /*  err_append_message (ERROR, err_msg); */
				   err_append_message (ERROR1, err_msg);

                /*-----------------------------------------------------------*/
                /** ENDIF a structure file was found...                     **/
                /*-----------------------------------------------------------*/

                }

            /*---------------------------------------------------------------*/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

            }  /*  End:  "if (found) ... else ..."  */

            Lemme_Go(structure_fname);

        /*-------------------------------------------------------------------*/
        /** ENDLOOP                                                         **/
        /*-------------------------------------------------------------------*/

        }  /*  End:  "for (i=1; (structure_fname = ..."  */

    /*-----------------------------------------------------------------------*/
    /** ENDLOOP                                                             **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "for (object_ptr = label_ptr; ..."  */

    /*-----------------------------------------------------------------------*/
    /** RETURN a pointer to the root of the expanded tree.                  **/
    /*-----------------------------------------------------------------------*/
    
    Lemme_Go(label_dir);
    pds_watch_ends = save_watch;
    pds_verbose = save_verbose;
    return (label_ptr);

/** END **/

}  /*  "lt_read_expanded_label"  */



/**********************************************************************
 *$Component                                                          *
 *   LOGICAL lt_replace_keyword (label_ptr, object_class, object_name,*
 *                               object_position, keyword_name,       *
 *                               keyword_position, keyword_value)     *
 *$Abstract                                                           *
 *    Replaces or adds a keyword to a PDS label                       *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    KEYWORD                                                         *
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
 *    object_class:                                                   *
 *        The object_class variable is a character string             *
 *        which contains the pds_class of an object in a PDS label        *
 *        (e.g., "OBJECT = IMAGE" implies that "IMAGE" is the pds_class   *
 *        of the object).                                             *
 *    object_name:                                                    *
 *        The object_name variable is a character string              *
 *        which contains the name of an object in a PDS label.        *
 *        This is assumed to be the value of the "NAME" parameter     *
 *        in the object (e.g., "NAME = VOYAGER" implies that          *
 *        "VOYAGER" is the name of the object).                       *
 *    object_position:                                                *
 *        The object_position variable is an integer which            *
 *        represents the relative position of an object in a PDS      *
 *        label.  If this variable is used in conjunction with either *
 *        the object_class or object_name variables, then it          *
 *        represents a particular occurrence of that thing in the     *
 *        label (e.g., if object_class is "TABLE" and object_position *
 *        is 3, then this represents the third "TABLE" object in the  *
 *        label).  On the other hand, if this variable is used by     *
 *        itself, it represents the absolute position of the object,  *
 *        starting from the "ROOT" object (position = 1).  The        *
 *        sequence follows the structure of the label as it looks     *
 *        in a flat file.                                             *
 *    keyword_name:                                                   *
 *        The parameter_name variable is a character string           *
 *        which contains the name of a parameter in a PDS label       *
 *        (e.g., the line "SCID = VG1" implies that "SCID" is the     *
 *        keyword name).                                              *
 *    keyword_position:                                               *
 *        The keyword_position variable is an integer which           *
 *        represents the relative position of a parameter in an       *
 *        object in a PDS label.  If this variable is used in         *
 *        conjunction with the keyword_name variable, then it         *
 *        represents a particular occurrence of that parameter        *
 *        in the object (e.g., if parameter_name is "SCID" and        *
 *        parameter_position is 2, then this represents the second    *
 *        "SCID" parameter in the object).  On the other hand, if     *
 *        this variable is used by itself, it represents the absolute *
 *        position of the parameter within the object, starting with  *
 *        first parameter in the object (position = 1).               *
 *    keyword_value:                                                  *
 *        The keyword_value variable is a character string            *
 *        which contains the value of a parameter in a PDS label      *
 *        (e.g., the line "SCID = VG1" implies that "VG1" is the      *
 *        keyword value)                                              *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure        *
 *        (a value of FALSE).                                         *
 *$Detailed_Description                                               *
 *    The lt_replace_keyword routine searches for the object given by *
 *    object_class, object_name, and object_position (according to    *
 *    the search rules used by the PDS label library).  If this       *
 *    object contains keyword keyword_name, the keyword value is      *
 *    changed to keyword_value.  If the object does not contain the   *
 *    keyword called keyword_name, then the keyword is added and is   *
 *    given the value keyword_value.                                  *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/JPL                                             *
 *$Version_and_Date                                                   *
 *    1.0   September 9, 1991                                         *
 *$Change_History                                                     *
 *    MDD   09-09-91   Original Code.                                 *
 **********************************************************************/

LOGICAL lt_replace_keyword (label_ptr, object_class, object_name,
                            object_position, keyword_name,
                            keyword_position, keyword_value)
AGGREGATE label_ptr;
char *object_class;
char *object_name;
int object_position;
char *keyword_name;
int keyword_position;
char *keyword_value;

{
   int status = PDS_ERROR;

   lab_change_value (label_ptr, object_class, object_name,
                     object_position, keyword_name, keyword_position,
                     keyword_value, &status);

   if (status == PDS_ERROR)
      lab_add_parameter (label_ptr, object_class, object_name,
                         object_position, keyword_name, keyword_value,
                         &status);

   return (status == PDS_SUCCESS);
}

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


LOGICAL lt_get_pointer (label_ptr, pointer_name, pointer_position,
                        pointer_info)

AGGREGATE label_ptr;
char *pointer_name;
int pointer_position;
POINTER_INFO *pointer_info;

{
   char *temp = NULL;
   PARAMETER keyword_ptr = NULL;
   int status = PDS_ERROR;
   LOGICAL success = FALSE;
   VALUE value_ptr;

/** BEGIN **/

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

   keyword_ptr = lab_find_parameter (label_ptr, "ROOT", NULL, 1,
                                     pointer_name, pointer_position, &status);

   if (keyword_ptr != NULL && status == PDS_SUCCESS)
   {
      /*---------------------------------------------------------------*/
      /** IF the pointer has at least one value THEN                  **/
      /*---------------------------------------------------------------*/

      value_ptr = FirstValue (keyword_ptr);
      if (value_ptr != NULL)
      {
          /*-----------------------------------------------------------*/
          /** set success flag to true                                **/
          /** set the is_attached flag if the value is an integer     **/
          /** get the actual value                                    **/
          /*-----------------------------------------------------------*/

          success = TRUE;
          pointer_info -> is_attached =
                           (value_ptr -> item.type == TV_INTEGER);
          temp = lu_fetch_value (value_ptr, FALSE);

          /*-----------------------------------------------------------*/
          /** IF the pointer is to an attached data file THEN         **/
          /**    the first value is a file name, so store it, and get **/
          /**    the next value                                       **/
          /** ENDIF                                                   **/
          /*-----------------------------------------------------------*/

          if (!pointer_info -> is_attached)
          {
             strcpy (pointer_info -> file_name, temp);
             Lemme_Go(temp);
             value_ptr = NextValue (value_ptr);
             temp = lu_fetch_value (value_ptr, FALSE);
          }
          /*-----------------------------------------------------------*/
          /** Next value is the location of the object, so store it   **/
          /** Get the units for the value                             **/
          /** set the has_byte_loc flag according to the units        **/
          /*-----------------------------------------------------------*/

          if (temp != NULL)
          {
             pointer_info -> location = Make_Long(temp);
             Lemme_Go(temp);
             temp = lu_format_units (value_ptr -> item.value.integer.units);
             if (temp != NULL)
                pointer_info -> has_byte_loc =
                                    (strcmp (temp, "<BYTES>") == 0 ||
                                     strcmp (temp, "<BYTE>") == 0);
          }
          Lemme_Go (temp);
      }
      /*---------------------------------------------------------------*/
      /** ENDIF the pointer has at least one value...                 **/
      /*---------------------------------------------------------------*/
   }
   /*------------------------------------------------------------------*/
   /** ENDIF it was found...                                          **/
   /*------------------------------------------------------------------*/

   return (success);

/** END **/
}


/**********************************************************************
 *$Component                                                          *
 *   LOGICAL lt_add_pointer (label_ptr, pointer_name, pointer_info)   *
 *$Abstract                                                           *
 *    Adds a pointer value to a PDS label.                            *
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
 *    pointer_info:                                                   *
 *        The pointer_info variable is the address of a POINTER_INFO  *
 *        structure, which contains information about a label pointer,*
 *        indicating the file it points to, the location of an object *
 *        within the file, the type of the location (record or bytes) *
 *        and whether the data file is attached or detached from the  *
 *        label.                                                      *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure        *
 *        (a value of FALSE).                                         *
 *$Detailed_Description                                               *
 *    The lt_add_pointer adds the pointer stored in the given pointer *
 *    info structure to the ROOT object of the label pointed to by    *
 *    label_ptr. It handles four types of pointers:                   *
 *           Record location, attached (^TABLE = 3)                   *
 *           Byte location, attached (^TABLE = 512<BYTES>)            *
 *           Record location, detached (^TABLE = ("TABLE.DAT",3))     *
 *           Byte location, detached (^TABLE = ("TABLE.DAT",512<BYTES>))*
 *    If the pointer cannot be added, this routine returns FALSE.     *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/JPL                                             *
 *$Version_and_Date                                                   *
 *    1.0   October 10, 1991                                          *
 *$Change_History                                                     *
 *    MDD   10-10-91   Original Code.                                 *
 **********************************************************************/

LOGICAL lt_add_pointer (label_ptr, pointer_name, pointer_info)

AGGREGATE label_ptr;
char *pointer_name;
POINTER_INFO *pointer_info;
{
   char temp_str [PDS_MAXLINE + 1];
   int status = PDS_ERROR;
   char temp_parm [PDS_MAXLINE + 1];

/** BEGIN **/

   /*---------------------------------------------------------------*/
   /** create pointer name                                         **/
   /*---------------------------------------------------------------*/

   sprintf (temp_parm, "^%s", pointer_name);

   /*---------------------------------------------------------------*/
   /** create pointer string as attached with byte location        **/
   /*---------------------------------------------------------------*/

   if (pointer_info -> is_attached && pointer_info -> has_byte_loc)
      sprintf (temp_str, "%ld<bytes>", pointer_info -> location);

   /*---------------------------------------------------------------*/
   /** create pointer string as attached with record location      **/
   /*---------------------------------------------------------------*/

   else if (pointer_info -> is_attached)
      sprintf (temp_str, "%ld", pointer_info -> location);

   /*---------------------------------------------------------------*/
   /** create pointer string as detached with byte location        **/
   /*---------------------------------------------------------------*/

   else if (pointer_info -> has_byte_loc)
      sprintf (temp_str, "(\"%s\", %ld<bytes>)", pointer_info -> file_name,
               pointer_info -> location);

   /*---------------------------------------------------------------*/
   /** create pointer string as detached with record location      **/
   /*---------------------------------------------------------------*/

   else
      sprintf (temp_str, "(\"%s\", %ld)", pointer_info -> file_name,
               pointer_info -> location);

   /*---------------------------------------------------------------*/
   /** Add the pointer to the label                                **/
   /*---------------------------------------------------------------*/

   lab_add_parameter (label_ptr, "ROOT", NULL, 1, temp_parm, temp_str, 
                      &status);

   return (status == PDS_SUCCESS);

/** END **/
}







/**********************************************************************
 *$Component                                                          *
 *    static void ltx_attach_file_name (label_ptr, label_fname)       *
 *$Abstract                                                           *
 *    Attaches a file name to the tree's list of fnames.              *
 *$Keywords                                                           *
 *    LABEL_TOOL                                                      *
 *    STRUCTURE                                                       *
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
 *    label_fname:                                                    *
 *        The label_fname variable is a pointer to a character string *
 *        containing a file name.                                     *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    This routine takes a file name which has been extracted from a  *
 *    structure pointer within a label, and appends it onto the root  *
 *    object's list of file names.  It then loops through all of the  *
 *    children of the root and points them to the list element        *
 *    containing the file name.  This allows the code which processes *
 *    the label tree to know which STRUCTURE file each object came    *
 *    from.                                                           *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / JPL                                          *
 *$Version_and_Date                                                   *
 *    1.0   December 4, 1991                                          *
 *$Change_History                                                     *
 *    DPB   12-04-91   Original Code.                                 *
 *	  MDC	03-05-03   Type casted the c variable into a long before  *
 *					   assigning it to appl2 to avoid crashing when   *
 *					   running in LINUX machines.					  *
 **********************************************************************/

static void ltx_attach_file_name (label_ptr, label_fname)

AGGREGATE label_ptr;
char *label_fname;

{
    AGGREGATE o_ptr = {NULL};
    char *c = {NULL};

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** IF the label pointer and file name are not NULL THEN                **/
    /*-----------------------------------------------------------------------*/

    if ((label_ptr != NULL) && (label_fname != NULL))
    {
        /*-------------------------------------------------------------------*/
        /** Append the file name onto the root object's list of file names  **/
        /*-------------------------------------------------------------------*/

        label_ptr->appl2 = (long) util_append_string_list (
                                     (STRING_LIST *) (label_ptr->appl2), 
                                     label_fname, STRING_TYPE);

        /*-------------------------------------------------------------------*/
        /** LOOP through the objects in the label tree and point them to    **/
        /**     the root's list element containing the file name.           **/
        /*-------------------------------------------------------------------*/

        c = ((STRING_LIST *) (label_ptr->appl2)) -> text;

        for (o_ptr=label_ptr;  o_ptr != NULL; o_ptr = NextAggregate(o_ptr))
        {
            if (o_ptr != label_ptr)
			{
#ifndef SUN_UNIX
				(char *) (o_ptr->appl2) = c;
#else
	   /*-------------------------------------------------------------------*/
	   /* 03-05-03 MDC														*/
	   /* Type casted the char pointer to a long before assigining it to    */
	   /* appl2 to avoid segmentation fault in LINUX machines.				*/
	   /*-------------------------------------------------------------------*/
               /* sprintf(c, "%c", o_ptr->appl2); */
				o_ptr->appl2 = (long) c;
#endif

			}
        }

    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if (label_ptr != NULL) ..."  */

    /*-----------------------------------------------------------------------*/
    /** RETURN                                                              **/
    /*-----------------------------------------------------------------------*/

    return;

/** END **/

}  /*  "ltx_attach_file_name"  */




/**********************************************************************
 *$Component                                                          *
 *    static void ltx_merge_objects(source_object_ptr,dest_object_ptr)*
 *$Abstract                                                           *
 *    Merges two trees.                                               *
 *$Keywords                                                           *
 *    LABEL_TOOL                                                      *
 *    STRUCTURE                                                       *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    source_object_ptr:                                              *
 *        The source_object_ptr is a pointer to the label tree under  *
 *        which the other tree is to be merged.                       *
 *    dest_object_ptr:                                                *
 *        The dest_object_ptr is a pointer to the label tree that is  *
 *        to be merged.                                               *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    This routine takes pointers to two label trees, and merges      *
 *    one into the other.  The dest tree comes from a STRUCTURE       *
 *    pointer, and is merged in where the structure was found.        *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / JPL                                          *
 *$Version_and_Date                                                   *
 *    1.0   December 4, 1991                                          *
 *$Change_History                                                     *
 *    DPB   12-04-91   Original Code.                                 *
 **********************************************************************/

static void ltx_merge_objects (source_object_ptr, dest_object_ptr)

AGGREGATE source_object_ptr;
AGGREGATE dest_object_ptr;

{
    AGGREGATE o_ptr = {NULL};
    AGGREGATE save_o = {NULL};
    PARAMETER p_ptr = {NULL};
    PARAMETER save_p = {NULL};

    for (p_ptr = FirstParameter(source_object_ptr); p_ptr != NULL; p_ptr=save_p)
    {
        save_p = NextParameter(p_ptr);
        PasteParameter (dest_object_ptr, CutParameter(p_ptr));
    }                                    

    for (o_ptr = source_object_ptr->first_child; o_ptr != NULL; o_ptr=save_o)
    {
        save_o = o_ptr -> right_sibling;
        PasteAggregate (dest_object_ptr, CutAggregate(o_ptr));
    }

    return;

}  /*  "ltx_merge_objects"  */



/*****************************************************************************/
/*$Component                                                                 */
/*    int *get_label_dir(char *label_dir, char *dir_to_add, char *savefile,  */
/*                                                          char *file_spec) */
/*$Abstract                                                                  */
/*    Looks back up the directory tree for a dir_to_add directory            */
/*    containing savefile                                                    */
/*$Inputs                                                                    */
/*    label_dir                                                              */
/*        This pointer points to a string containing a path to a file        */
/*    dir_to_add:                                                            */
/*        This pointer points to a string containing a directory to          */
/*        add to label_dir                                                   */
/*    savefile:                                                              */
/*        This pointer points to a string containing a file name to add to   */
/*        label_dir + dir_to_add                                             */
/*    file_spec                                                              */
/*        If the file is found the full file spec will be stored here.       */
/*$Outputs                                                                   */
/*    None                                                                   */
/*$Returns                                                                   */
/*    0 means we found the file                                              */
/*    1 means we did not find the file                                       */
/*$Detailed_Description                                                      */
/*    This routine will look for dir_to_add directories back up a tree.      */
/*    if label_dir is c:\first_dir\second_dir\third_dir and dir_to_add is    */
/*    "LABEL", and savefile is SOMEFILE.FMT the program will look for        */
/*    SOMEFILE.FMT in c:\first_dir\second_dir\LABEL, and then in             */
/*    c:\first_dir\LABEL, and finally in c:\LABEL.  If the file is found     */
/*    the full file specification will be returned.  If it is not found then */
/*    the routine will return a NULL string. The tests will be performed on  */
/*    both upper and lower case strings.                                     */
/*$External_References                                                       */
/*    None                                                                   */
/*$Author_and_Institution                                                    */
/*    Dale Schultz / JPL                                                     */
/*$Version_and_Date                                                          */
/*    1.0   May 23, 2002                                                     */
/*$Change_History                                                            */
/*    DWS   05-23-02   Original Code.                                        */
/*    MDC   01-26-06   Modified routine to first search for the file as-is   */
/*                     since filenames in non-windows systems are case       */
/*                     sensitive. We could have a file that is located in a  */
/*                     directory path that contains a mix of lowercase and   */
/*                     uppercase letters.                                    */
/*    MDC   06-28-06   Fixed routine to properly pass back the path to the   */
/*                     FMT file in a LABEL directory. This includes changing */
/*                     the return type of the routine.                       */
/*****************************************************************************/

char *get_label_dir(char *label_dir, char *dir_to_add, char *savefile, char *file_spec, FILE *test_file_ptr)
{
    char upper_name[PDS_MAXLINE];
    char lower_name[PDS_MAXLINE];
    char temp_name[PDS_MAXLINE];
	char *temp_ptr = NULL;
	char *result = NULL;

#ifdef MSDOS_TC
	int islash = '\\';
	char cslash[2] = {'\\'};
#else
	int islash = '/';
	char cslash[2] = {'/'};
#endif

	strcpy(temp_name, label_dir);    /* save the name of the directory containing the label file*/
	temp_ptr = strrchr(temp_name, islash); /* we need to get rid of the final slash*/
	if(temp_ptr == NULL) return(NULL);  /* there was no slash, we don't know what to do about that*/
	*temp_ptr = '\0';                /* we got one, now get rid of it.  We had something like */
									 /* C:\directory_1\directory_2\directory_3\               */
									 /* we need to get rid of directory_3\                    */
									 /* leaving something like C:\directory_1\directory_2     */
	                                 /* we will get rid of the final slash here and then get  */
                                     /* rid of the directory_3 below                          */

	for(;;)
	{
		temp_ptr = strrchr(temp_name, islash); /*find the last slash    */
		if(temp_ptr == NULL) return(NULL);        /*wasn't one, get out    */
		temp_ptr++;
		*temp_ptr = '\0';                      /* get rid of everything after the final slash*/
		strcat(temp_name, dir_to_add);         /* add the search directory to the end, it is probably LABEL*/
		strcat(temp_name, cslash);             /* add a slash and then add the file name*/
		strcat(temp_name, savefile);

		/* 01-26-05 MDC - Add a check to search for the file as-is since filenames in
		   non-Windows systems are case-sensitive.
	    */
		test_file_ptr = fopen(temp_name, "r");
		if(test_file_ptr != NULL)
		{
			New_String(result, temp_name);
			return(result);
		}
		
		strcpy(upper_name, temp_name);		   /*make an upper case copy of the neme*/
		strcpy(lower_name, temp_name);         /*make a lower case copy of the name too*/
		util_upper_case(upper_name);		   
		util_lower_case(lower_name);

		test_file_ptr = fopen(upper_name, "r");/*try to open the upper case version*/
		if(test_file_ptr != NULL)              /*was it there?*/
		{									   /*got it, close it, save the name and get out*/
			New_String(result, upper_name);
			return(result);
		}
		else 
		{	/*wasn't there in upper case, try lower case*/
			test_file_ptr = fopen(lower_name, "r");
			if(test_file_ptr != NULL)  /*was it there?*/
			{	
				New_String(label_dir, lower_name);
				return(result);
			}
		}
		temp_ptr = strrchr(temp_name, islash); /*find the last slash that we added and get rid of it    */
		if(temp_ptr == NULL) return(NULL);        /*wasn't one, get out    */
		*temp_ptr = '\0';                      /* get rid of it and everything after it*/
		temp_ptr = strrchr(temp_name, islash); /*now find the first slash that we added and get rid of it    */
		if(temp_ptr == NULL) return(NULL);        /*wasn't one, get out    */
		*temp_ptr = '\0';                      /* get rid of it and everything after it*/

	}  /*wasn't there.  Go back and try the next directory up*/
	return(NULL); /*wasn't one*/
}
