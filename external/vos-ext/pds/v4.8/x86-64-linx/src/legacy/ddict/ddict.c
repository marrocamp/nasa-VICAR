/**********************************************************************/
/**********************************************************************/


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Component                                                           * 
 *    ddict.c                                                          *
 * Abstract                                                            *
 *    PDS Label Verifier-specific routines                             *
 * Detailed Description                                                *
 *    This routine uses lvtool as a base.  Parts of lvtool have been   *
 *    removed and some new routines have been added.  This routine     *
 *    generates a report containing all of the keywords and their      *
 *    definitions for all of the specified files.                      *
 * Internal References                                                 *
 *    Driver:                                                          *
 *        ddict                                                        *
 *                                                                     *
 *    Setup routine:                                                   *
 *        lv_setup                                                     *
 *        lv_special                                                   *
 *                                                                     *
 *    Report generation routines:                                      *
 *        rpt_format_message                                           *
 *        rpt_main_footer                                              *
 *        rpt_main_header                                              *
 *        rpt_semantic_errors                                          *
 *        rpt_keyword_errors                                           *
 *                                                                     *
 *    Internal routines:                                               *
 *        ddict                                                        *
 *        lv_file_header                                               *
 *                                                                     *
 * Authors and Institutions                                            *
 *    Marti D. Demore / J.P.L.                                         *
 *    Kristy L. Marski / J.P.L.                                        *
 *    David P. Bernath / J.P.L.                                        *
 * Version and Date                                                    *
 *    1.1   June 22, 1992                                              *
 * Change History                                                      *
 *    DPB   09-27-91   File assembled.                                 *
 *    MDD   06-22-92   Removed SFDU toolkit interface.  Added calls to *
 *                     new lab_ routines.                              *
 *    DWS   11-05-97   Compiled with Microsoft C++ Version 4.0         *
 *    DWS   11-10-97   Version will be 2.0                             *
 *    DWS   05-01-98   Removed unused LVTOOL code and added dd         *
 *                     extraction code to lv_special                   *
 *    DWS   07-13-00   Changed ODL files LABEL.C at lines 4293 and     *
 *                     4567 by commenting them out.  They no longer    *
 *                     replace underscores with blanks.  Also          *
 *                     commented out lines 600 thru 605 in LABUTIL.C   *
 *    DWS   08-23-01   Added code in rpt_main_header to check the first*
 *                     5 lines of the data dictionary for the Version  *
 *                     and generated information instead of the looking*
 *                     only at lines 2 and 3.                          *
 *    DWS   12-10-01   Recompile to pick up group changes in lablib    *
 *                                                     version 4.0     *
 *    MDC   08-12-03   Modified lv_special routine. See notes.         *
 *                     Modified main routine. See notes.               *
 *    DWS   11-13-03   Recompiled for use with NULL followed by unit   *
 *                     ID value.                                       *
 *    MDC   05-27-04   Modified code to implement latest command-line  *
 *                     options: -lef, -ivd, -sf, -lef                  *
 *    MDC   06-01-04   Modified lv_special routine. See notes in       *
 *                     routine.                                        *
 *    MDC   06-17-04   Modified lv_special and lv_setup routines. Added*
 *                     routine called validate_label.                  *
 *    MDC   06-15-05   Modified lv_setup routine.                      *
 *    MDC   04-20-06   Modified main routine to not create a scratch   *
 *                     file in a Windows environment. Windows doesn't  *
 *                     have a use for it.                              *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "ddict.h"


LVTOOL_V ddict;

/**********************************************************************
 *$Component                                                          *
 *    void ddict  (verifier_args)                                     *
 *$Abstract                                                           *
 *    Main program for the label verifier                             *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    LABEL_VERIFIER                                                  *
 *    DRIVER                                                          *
 *$Inputs                                                             *
 *    verifier_args:                                                  *
 *        The verifier_args variable is a list of arguments which     *
 *        control the behavior of the verifier.                       *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    The lvtool  program is the driver for the label verifier.  It   *
 *    calls routines to parse the command line and then to verify     *
 *    each of the files specified by the command line.  Results       *
 *    are written to a report file.  LVTOOL main was seperated into   *
 *    three pieces when SLVTOOL was incorporated into it.  The part of*
 *    LVTOOL main that remains was common to both.  LV SPECIAL        *
 *    contains code that was unique to the original LV TOOL function. *
 *    SLV SPECIAL contains code that was unique to the original       *
 *    SLV TOOL function.  For the sake of history SLV TOOL was created*
 *    from LV TOOL in the first place.                                *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    pds_display             pdsglob.h                write          *
 *    pds_verbose             pdsglob.h                write          *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/JPL                                             *
 *$Version_and_Date                                                   *
 *    1.4   October 23, 1992                                          *
 *$Change_history                                                     *
 *    MDD   06-04-91   Original generation.                           *
 *    DPB   06-13-91   Added pds_display and pds_verbose.  (Modified  *
 *                     external references)                           *
 *    MDD   10-22-91   Added sv_exit calls, fixed free calls, changed *
 *                     err_write call                                 *
 *    MDD   03-18-92   The great int -> long conversion.              *
 *    MDD   06-22-92   Integrated lablib changes                      *
 *    MDD   10-23-92   Added temp_list variable to fix memory leak    *
 *    DWS   11-10-97   Combined code of slv_tool with lv_tool         *
 *                     Added code for lv_or_slv and pds_progress      *
 *    DWS   05-26-99   Added invalid file list.  Allows user to create*
 *                     a file containing a list of file extensions to *
 *                     be excluded from processing                    *
 *    MDC   08-12-03   Added code to delete the JunkZZZ files at the  *
 *                     end of the execution                           *
 **********************************************************************/

void main (argc, argv)                                       /*10-8-97*/
int argc;
char *argv [];
{

   LOGICAL     command_line_ok;
   FILE        *keyword_file_ptr = {NULL};
   char        OutFile[100];
   LOGICAL     success;


/** BEGIN **/

   /*-----------------------------------------------------------------------*/
   /** Setup the files needed by the toolbox                               **/
   /** Setup the verifier/parse the command line                           **/
   /*-----------------------------------------------------------------------*/

   printf ("\nThe PDS Data Dictionary Extractor - Version %s\n\n", DDICT_VERSION);


   /* 04-20-06 MDC - Only call the lab_setup routine when running in a non-windows
       environment. Windows doesn't need to create a scratch file.
    */
#ifndef MSDOS_TC
   lab_setup ();
#endif

   command_line_ok = lv_setup (argc, argv);

/*
   pds_display = use_terminal;
   pds_verbose = use_verbose;
*/

        /*-----------------------------------------------------------------------*/
        /** RSJ: 12-18-97 - DDict                                               **/
		/**   - 1st, attempt to delete the Keyword file (JunkZZZ1.TXT).         **/
		/**   - Then, create a new file and open the file for appending data    **/ 
        /*-----------------------------------------------------------------------*/

		if (keyword_file_ptr == NULL)
        {
          strcpy(OutFile, "JunkZZZ1.TXT");
		  if (success = remove(OutFile) == TRUE)
          {
			    if ((keyword_file_ptr = fopen(OutFile, "a+")) == NULL)
				{
					printf("\n Could not open the keyword OUTPUT file: %s", OutFile);
					exit(success);
				}
		  }
		  else
          {
				if ((keyword_file_ptr = fopen(OutFile, "w+")) == NULL)
				{
					printf("\n Could not open the keyword OUTPUT file: %s", OutFile);
					exit(success);
				}
				else
				{
					fclose(keyword_file_ptr);
			
					if ((keyword_file_ptr = fopen(OutFile, "a+")) == NULL)
					{
						printf("\n Could not open the keyword OUTPUT file: %s", OutFile);
						exit(success);
					}
				}  
		  }
        }

   /*-----------------------------------------------------------------------*/
   /** print all command line errors to the screen and clear the messages  **/
   /*-----------------------------------------------------------------------*/

   err_write_to_file (NULL, FALSE);

   /*-----------------------------------------------------------------------*/
   /** IF the command line was fine THEN                                   **/
   /*-----------------------------------------------------------------------*/
   if (command_line_ok)
   {
      /*--------------------------------------------------------------------*/
      /** print the main report header                                     **/
      /*--------------------------------------------------------------------*/

      ddict.rpt_file_ptr = rpt_main_header();

	  lv_special(keyword_file_ptr);
   } 
   /*-----------------------------------------------------------------------*/
   /** ENDIF                                                               **/
   /** clean up                                                            **/
   /*-----------------------------------------------------------------------*/

  /* 01-11-05 MDC - Delete the index file upon completion of the validation */
/*   if(ddict.dd_index_name != NULL)
	   remove(ddict.dd_index_name);
*/

   Lemme_Go(ddict.dd_index_name);
   Lemme_Go(ddict.rpt_file_name);


   util_deallocate_string_list (ddict.file_list);
   lab_exit ();

   /* 08-12-03 MDC - Close keyword_file_ptr and remove the JunkZZZ files */
   fclose(keyword_file_ptr);

   remove("JunkZZZ1.TXT");
   remove("JunkZZZ2.TXT");

   return;

} /*  End: "main"  */

 
/***********************************************************************/
/*$Component                                                           */
/*  VOID lv_special(use_terminal, rpt_file_ptr, keyword_file_ptr,      */
/*                 file_list, use_aliases, odl_root, use_dd)           */
/*$Abstract                                                            */
/*  This routine handles lv tool specific tasks                        */
/*$Keywords                                                            */
/*    LV_TOOL                                                          */
/*    TOOL_MAIN                                                        */
/*    LABEL_VERIFIER                                                   */
/*$Inputs                                                              */
/*    use_terminal:                                                    */
/*        The use_terminal variable a TRUE/FALSE flag indicating       */
/*        whether all error messages are displayed to the screen by    */
/*        the label verifier.                                          */
/*    rpt_file_ptr:                                                    */
/*        The rpt_file_ptr variable is the pointer to the file to be   */
/*        written to.                                                  */
/*    keyword_file_ptr:                                                */
/*        The keyword_file_ptr variable is the pointer to the file     */
/*        to which is written the list of 'Data Dictionary validated'  */
/*        keywords.                                                    */
/*    file_list:                                                       */
/*        The file_list variable is a pointer to a STRING_LIST         */
/*        structure that contains a list of file names.                */
/*    use_aliases:                                                     */
/*        The use_aliases variable is a TRUE/FALSE flag indicating     */
/*        whether names are dealiased by the label verifier.           */
/*    odl_root:                                                        */
/*        Points to the root of an ODL tree.                           */
/*    use_dd:                                                          */
/*        The use_dd variable is a TRUE/FALSE flag indicating whether  */
/*        data dictionary validation is enabled.                       */
/*    inval_file													   */
/*        A TRUE/FALSE value indicating the presence of an invalid     */
/*        file list file.                                              */
/*    inval_file_name												   */
/*		  A pointer to a string containing the name of the invalid     */
/*        file list file.                                              */
/*$Outputs                                                             */
/*    None                                                             */
/*$Detailed_Description                                                */
/*    The lv special function is a continuation of the driver for the  */
/*    label verifier.  It calls routines to print the report           */
/*    files created by the lv_tool main.                               */
/*$External_References                                                 */
/*    Item                       Shared Data              Access       */
/*    ---------------------------------------------------------------  */
/*   err_write_to_file ()                                              */
/*   lab_clear_messages ()                                             */
/*   lab_clear_messages ()                                             */
/*   lab_read_label ()                                                 */
/*   dd_unalias ()                                                     */
/*   err_deallocate_list ()                                            */
/*   lab_clear_messages ()                                             */
/*   ver_semantics ()                                                  */
/*   rpt_keyword_errors ()                                             */
/*   rpt_semantic_errors ()                                            */
/*   lab_clear_messages ()                                             */
/*   lab_remove_label ()                                               */
/*   rpt_main_footer ()                                                */
/*$Author_and_Institution                                              */
/*    Marti D. Demore                                                  */
/*$Version_and_Date                                                    */
/*    1.2 November 12, 1997                                            */
/*$Change_history                                                      */
/*   DWS  11-12-97  Original seperation from LV TOOL main              */
/*   DWS  11-17-97  Added expand label capability                      */
/*   RSJ  12-18-97  Added DDictionary Extractor code                   */
/*   MDC  08-12-03  Added code to double check that the last char in   */
/*                  the invalid list is indeed a comma.                */
/*   MDC  05-27-04  Modified routine to implement latest options to    */
/*                  program (-ivd,-lef,-ivf,-sf)                       */
/*   MDC  06-01-04  Modified routine to replace newline characters     */
/*                  with NULLs for keywords stored in Sarray           */
/*   MDC  06-17-04  Check for the existence of a label in a file before*/
/*                  validating it.                                     */
/***********************************************************************/

void lv_special(keyword_file_ptr)

FILE *keyword_file_ptr;

{
  int           status;  
  ERROR_LIST    *alias_message_ptr;
  STRING_LIST   *temp_list;
  char          string1[101];
  char          string2[101];
  char          *p;
  
  FILE          *new_file_ptr;		/* output file pointer       */

  int		i, j;			        /* counter	                 */
  int		MatchFound = FALSE;		/* if match found            */
  int       Scount = 0;
  int       FirstPass = TRUE;

  int		x, addit_flag;
  char		string5[101];


  int		ok_to_go, ok_to_go_1;				  /*flag that allows file to be     */
									             /*processed                       */
  int       ok_to_go_2 = 1;

  char      invalid_list[201];
  char      invalid_dir[30] [100] = {0};

                       /**************************************************/
                       /* variables for report                           */
                       /**************************************************/
  int       string1_len;                                               /**/
                       /**************************************************/
                       /* end of variables for report                    */
                       /**************************************************/
/*   char *temp_ptr; */
  status = 0;

 
  /*------------
 *  Null terminate strings
 */
      string1[100] = '\0';
      string2[100] = '\0';
      if(check_exclusion_opts(invalid_list, invalid_dir) == 0) return;

      /*--------------------------------------------------------------------*/
      /** IF the report file was opened THEN                               **/
      /*--------------------------------------------------------------------*/
      if (ddict.rpt_file_ptr != NULL)
      {
         /*-----------------------------------------------------------------*/
         /** write any messages so far to the report and clear them        **/
         /** LOOP through all the files to be verified                     **/
         /*-----------------------------------------------------------------*/
         err_write_to_file (ddict.rpt_file_ptr, TRUE);
         lab_clear_messages ();
         for (temp_list = ddict.file_list; temp_list != NULL;
		                                     temp_list = temp_list -> next)
         {
			   if(ddict.inval_file  == FALSE) 
			   {
			  	   ok_to_go = 1;                         /* process them all*/
			   }
			   else
			   {       /* check file extension for ok to process it         */
			 	   ok_to_go = chk_file_type(invalid_list, temp_list);   /*07-20-98*/
				   if((ok_to_go == 0) && (ddict.use_log_file))
				   {
					   fprintf(ddict.log_file_ptr,
						   "File Excluded, Excluded File Type, File: %s\n", temp_list->text);
				   }
			   }
			   if((ddict.inval_dir == FALSE) && (ok_to_go == 1))
			   {
					ok_to_go_1 = 1;
			   }
			   else
			   {
				   /* check if this file contains an excluded directory */
					ok_to_go_1 = chk_dir_name(invalid_dir, temp_list);
					if((ok_to_go_1 == 0) && (ddict.use_log_file))
					{
						fprintf(ddict.log_file_ptr,
							"File Excluded, Excluded Directory, File: %s\n", temp_list->text);
					}
			   }
			   
			   /* 06-16-04 MDC - Add capability to check if the file contains a label. */
			   if((ddict.lbl_det) && (ok_to_go == 1) && (ok_to_go_1  == 1))
			   {
                   ok_to_go_2 = validate_label (temp_list->text);
			       if((ok_to_go_2 == 0) && (ddict.use_log_file))
			       {
                       fprintf(ddict.log_file_ptr, 
						"File Excluded, File Has No Label, File: %s\n", temp_list->text);
				   }
			   }

			   if (ok_to_go && ok_to_go_1 && ok_to_go_2)
			   {
                 printf ("Validating %s\n", temp_list -> text);
                 
				 /*-------------------------------------------------------------*/
				 /** clear error messages                                      **/
				 /** read the label                                            **/
				 /** dealias the label, if needed                              **/
				 /** print the object heirarchy to the report                  **/
				 /** delete all alias messages                                 **/
				 /** print the syntax errors to the report                     **/
				 /*-------------------------------------------------------------*/

				 lab_clear_messages ();
				 ddict.odl_root = lab_read_label (temp_list -> text, &status);
				 if (ddict.use_aliases)
				 {
				    alias_message_ptr = dd_unalias (ddict.odl_root);
					err_deallocate_list (ddict.alias_message_ptr);
				 }

				 /*-------------------------------------------------------------*/
				 /** clear error messages                                      **/
				 /** IF a label was read and the data dictionary option in ON  **/
				 /**    THEN                                                   **/
				 /**    verify the label semantics and print the errors        **/
				 /**    clear the messages                                     **/
				 /*-------------------------------------------------------------*/

				 lab_clear_messages ();
				 if (ddict.use_dd && ddict.odl_root != NULL)
				 {
					ver_semantics (keyword_file_ptr, ddict.odl_root);
					lab_clear_messages ();
				 }
				 /*-------------------------------------------------------------*/
				 /** ENDIF                                                     **/
				 /** clean up the label in memory                              **/
				 /*-------------------------------------------------------------*/
				 lab_remove_label (ddict.odl_root, &status);
			   }
			   else  printf ("Skipping %s\n", temp_list->text);
		 }
											
		/*-----------------------------------------------------------------*/
		/** ENDLOOP                                                       **/
		/*-----------------------------------------------------------------*/
		/*-----------------------------------------------------------------------*/
		/** Part II - Using the keyword file (JunkZZZ1.TXT),                    **/
		/**           (1) Remove all duplicate keywords                         **/
		/**                - remove the duplicate keywords                      **/
		/**                - write the results to 'JunkZZZ2.TXT'                **/
		/**           (2) Sort the keywords into two lists                      **/
		/**                - sort into ascending order                          **/
		/**              a) Keywords not in the data dictionary                 **/
		/**                 (these are written to the report file 1st)          **/
		/**              b) Keywords that are in the Data Dictionary            **/
		/**                 (these are further processed by fetching the        **/
		/**                  keyword definitions and then writing to the report **/
		/**                  file)                                              **/
		/*-----------------------------------------------------------------------*/
		if ((new_file_ptr = fopen("JunkZZZ2.TXT", "w+")) == NULL)
		{
			printf("\n Could not open OUTPUT file: JunkZZZ2.TXT");
			exit(status);
		}
		printf("Removing duplicate entries\n");

	    rewind(keyword_file_ptr);
		while (fgets(string1, 100, keyword_file_ptr) != NULL)
		{
			addit_flag = 1;
			/* 06-01-04 MDC - Replace newline character at the end of
			   string with an "end of string" delimeter.
		    */
			util_remove_char(string1, '\n');
			if(Scount > 0)
			{
				for (x = 0; x < Scount; x++)
				{
					strcpy(string5, Sarray[x]);
					if(strcmp(string1, string5) == 0)
					{
						addit_flag = 0;
						break;
					}
				}
			}
			if (addit_flag == 1)
			{
/*		printf("there are %d elements in the array, the new keyword is %s\n", Scount,
			                                                  string1);*/
				strcpy(Sarray[Scount], string1);
				Scount++;
			}
			
			
			if(Scount >= DDICT_KW_ARRAY_SIZE)
			{
				if(Scount >= DDICT_KW_ARRAY_SIZE)
				{
					fprintf(ddict.rpt_file_ptr, 
						"\nWARNING, Keyword array has run out of room.  Reduce the number of\n");
					fprintf(ddict.rpt_file_ptr,
					  "         files being validated.\n\n");
					printf( 
						"\n\nWARNING, Keyword array has run out of room.  Reduce the number of\n");
					printf(
						"         files being validated.\n\n");
					break;
				}
			}
		}

	    printf("Sorting Remaining Entries\n");
        QsortString(Sarray, 0, Scount-1);
	    printf("Checking Sort\n");

		do
		{
		   status = FALSE;

		   for (i = 0; i < Scount-1; ++i)
           {
              if (strcmp(Sarray[i], Sarray[i+1]) > 0) status = TRUE;
		   }

		   if (status) QsortString(Sarray, 0, Scount -1);
		} while (status);
		printf("Writing Report\n");
		
				 /***************************************/
                 /* Check list against sel_file         */
                 /* remove all keywords that are        */
                 /* in the sel_file.                    */
                 /***************************************/

                 /* Start by attempting to open sel_file*/

		if(ddict.use_sel_file)
		{
			remove_excluded_kwds(Scount);
		}

        /*-----------------------------------------------------------------*/
        /** Write the set of Keywords that are not in the Data Dictionary **/
        /** to the report file                                            **/
        /*-----------------------------------------------------------------*/
		j = 0;
        for (i = 0; i < Scount; ++i)
		{
          if (strlen(Sarray[i]) > 2)
          {
            strcpy(string1, Sarray[i]);
            if (strstr(string1, "AAAAZ_") != NULL)
            {
              if (j == 0) 
		  	  {
			    rpt_semantic_errors (ddict.rpt_file_ptr, TRUE); 
			    ++j;
              } 

              p  = strchr(string1, '_');
              fprintf(ddict.rpt_file_ptr, "    WARNING: Not in Data Dictionary -- %s\n", (p+1));              
            }
		    else
              fprintf(new_file_ptr, "%s\n", Sarray[i]);
		  }
		}
		
        if (j == 0) rpt_semantic_errors (ddict.rpt_file_ptr, FALSE); 

        /*-----------------------------------------------------------------------*/
        /** Part III - Using the new keyword file (JunkZZZ2.TXT),               **/
        /**           (1) Retrieve the keyword definitions                      **/
        /**           (2) Write the keyword and its definition to the           **/
        /**               report file.                                          **/
        /*-----------------------------------------------------------------------*/
        rewind(new_file_ptr);

        j = 0;
        while (fgets(string1, 100, new_file_ptr) != NULL)
        {
          if (j == 0) 
          {
		    rpt_keyword_errors (ddict.rpt_file_ptr, TRUE); 
		    ++j;
          } 
/*          string1[30] = '\0';*/                          /* only first 30            */
          string1_len = strlen(string1);
          string1[string1_len -1] = '\0';
		  ddict.use_dd =  dd_get_definition (ddict.rpt_file_ptr, string1, 
                                                  "ELEMENT_DEFINITION", DD_WRITE_DEFS_TO_FILE);
        }
		printf("Done Writing Definitions\n");				
        if (j == 0) rpt_keyword_errors (ddict.rpt_file_ptr, FALSE); 
        printf("Done Writing Report\n");
        fclose(new_file_ptr);
		
        rpt_main_footer (ddict.rpt_file_ptr);
      }
      /*--------------------------------------------------------------------*/
      /** ENDIF                                                            **/
      /*--------------------------------------------------------------------*/
} /*  End: "lv_special"*/

/******************************************************************************

 ROUTINE: void remove_excluded_kwds(int kwd_count)

 DESCRIPTION: Function that will remove keywords from a list provided by the
              user
 
 INPUTS: kwd_count - the number of keywords stored in memory


 CHANGE HISTORY
      05-31-04   MDC       Original code

 *******************************************************************************/


void remove_excluded_kwds(int kwd_count)
{

	FILE *sel_file_hdl = NULL;
	char *string = NULL;
	char *kwd_ptr = NULL;

	int nbr_chrs = 0;
	int i = 0;

    printf("Removing key words that are in user's exclusion list\n");
	sel_file_hdl = fopen(ddict.sel_file_name, "r");
	
	if(!sel_file_hdl) printf ("\nsel_file did not open, all keywords will be reported\n");
	else
	{
		Malloc_String(string,200 + sizeof(char));

        fgets(string, 200, sel_file_hdl);
		for (i = 0; i < kwd_count; i++)
		{          /* select the keywords that are in the list */
			if (strlen(Sarray[i]) > 2)
			{
				/* This is for the case when keywords that are not found in the
				   data dictionary are stored in the array.
			    */
				if(strstr(Sarray[i], "AAAAZ_") != NULL)
				{
					/* Point to the beginning of the actual keyword after
					   the "AAAAZ_" header  */
                    kwd_ptr = strstr(Sarray[i], "_") + 1;
					if(strstr(string, kwd_ptr) != NULL)
						strcpy(Sarray[i], "zz");
				}
				else if(strstr(string, Sarray[i]) != NULL)
                    strcpy(Sarray[i], "zz");
			}
		}
	 }
	if(sel_file_hdl != NULL) fclose (sel_file_hdl);
	Lemme_Go(string);
	
				 /***************************************/
		         /* End of check                        */
		         /***************************************/
}

/*******************************************************************************/
/*$Componant                                                                   */
/*	int chk_dir_name(invalid_dir, temp_list);                                  */
/*                                                                             */
/*$Abstract                                                                    */
/*   Determines whether a file in a particular directory should be processed   */
/*                                                                             */
/*$inputs                                                                      */
/*   invalid_list                                                              */
/*       pointer to a list of file name extensions not to check                */
/*   temp_list                                                                 */
/*       A pointer to the name of a file to be processed                       */
/*                                                                             */
/*$Outputs                                                                     */
/*   None                                                                      */
/*                                                                             */
/*$Returns                                                                     */
/*   A value of one indicates to process the file, zero indicates do not       */
/*   process the file.                                                         */
/*                                                                             */
/*$Detailed Description                                                        */
/*   This program will determine if a file name is in one of the directories   */
/*   specified in a list of directory names.  If the file is not in the list   */
/*   the program will return a value of one									   */
/*                                                                             */
/*$Error Handling                                                              */
/*   None                                                                      */
/*                                                                             */
/*$Author and Institution                                                      */
/*   Dale Schultz  /JPL/ACRO                                                   */
/*$Date                                                                        */
/*   July 20, 1998                                                             */
/*                                                                             */
/*$Change History                                                              */
/*   DWS 07-20-1998   Original code                                            */
/*   MDC 08-08-2003   Initialize the count to 0 instead of 1 in the for loop   */
/*                                                                             */
/*******************************************************************************/
int chk_dir_name(invalid_dir, temp_list)
char invalid_dir[][100];
STRING_LIST * temp_list;
{
	int chk_result;
	int count;
	char *dir_ptr;
	char *str_ptr;

	dir_ptr = temp_list->text;
	chk_result = 1;  /* no match, process file*/
	
	for(count = 0; count < 10; count++)
	{
		if(invalid_dir[count][0] == '\0')
		{
			count = 11;
		}
		else
		{
			str_ptr = invalid_dir[count];
			if(strstr(dir_ptr, str_ptr) != NULL)
			{
				chk_result = 0; /*Don't do this file!  It is in  */
				count = 11;		/*a forbidden directory. We are  */
			}					/*done with the search.          */
		}
	}
return (chk_result);
}

/*******************************************************************************/
/* check_exclusion_opts(lvtv,invalid_list, invalid_dir)                        */
/*                                                                             */
/* Description                                                                 */
/* This function will check to see if file type exclusion has been selected.   */
/* If it has the file types will be read from the specified file and returned  */
/* to the calling function.  It will then check to see if directory exclusions */
/* have been selected.  If it has, the list of invalid directories will be read*/
/* from the selected file and returned to the user.  It will then check to see */
/* if logging of excluded files has been selected.  If it has, the file will be*/
/* opened and the handle will be returned to the caller.                       */
/*                                                                             */
/* lvtv is a structure containing a number of variables used throughout lvtool.*/
/*      For this function it has the has eight members that are needed:        */
/*      inval_file: If TRUE there is a list of invalid file types to be read   */
/*           from the file name in inval_file_name.                            */
/*      inval_file_name: The file containing a list of invalid file types.     */
/*      inval_dir: If TRUE there is a list of invalid directories to be read   */
/*           from the file name in inval_dir_name.                             */
/*      inval_dir_name: The file containing a list of invalid directories.     */
/*      use_log_file: If TRUE a list of all files excluded for any reason will */
/*           be created in the file specified in log_file_ptr.                 */
/*      log_file_name: The name of the file in which the exclusion log will be */
/*           kept.                                                             */
/*      log_file_ptr: The handle for the log file.                             */
/*      rpt_file_ptr: The handle for the report file.                          */
/*                                                                             */
/* Of the lvtv members listed only log_file_ptr will return a value to the     */
/*      calling function.                                                      */
/*                                                                             */
/* invalid_list will return a list of file extensions that are not to be       */
/*     processed by lvtool.                                                    */
/* invalid_dir will return a list of directories that are not to be processed  */
/*     by lvtool.                                                              */
/*                                                                             */
/* Change History:                                                             */
/*     08-07-03   MDC        Modified the routine to only chop off the last    */
/*                           character of a string whenever a newline          */
/*                           character is contained within it.                 */
/*******************************************************************************/
int check_exclusion_opts(char invalid_list[], char invalid_dir[][100])
{
      FILE * inv_file_hdl;
	  FILE * inv_dir_hdl;
	  int  rec_count;
	  int  count;
	  int  str_len;
	  int  ret_val = 1; /* set to good */

	  if(ddict.inval_file == TRUE)
	  {
		  inv_file_hdl = fopen(ddict.inval_file_name, "r");
		  if (inv_file_hdl == NULL)
		  {
			  printf ("Invalid file type list did not open, file name: %s \n",
				                                                        ddict.inval_file_name);
			  fprintf (ddict.rpt_file_ptr, "Invalid file type list did not open, file name: %s \n",
		                                                                ddict.inval_file_name);
              ddict.inval_file = FALSE;
			  ret_val = 0;
		  }
		  else
		  {
			  rec_count = fread(invalid_list, 1, 200, inv_file_hdl);
			  if(ferror(inv_file_hdl))
			  {
				  printf ("Invalid file type list had read error, file name: %s \n",
		                                                                ddict.inval_file_name);
				  fprintf (ddict.rpt_file_ptr, "Invalid file type list had read error, file name: %s \n",
		                                                                ddict.inval_file_name);
                  ddict.inval_file = FALSE;
  	  		      ret_val = 0;
			  }
              else
			  {
				  if (rec_count > 200)  rec_count = 200;
				  invalid_list[rec_count] = '\0';
			  }
			  fclose(inv_file_hdl);
		 }
	  }
	  if(ddict.inval_dir == TRUE)
	  {
		  inv_dir_hdl = fopen(ddict.inval_dir_name, "r");
		  if (inv_dir_hdl == NULL)
		  {
			  printf ("Invalid directory list did not open, file name: %s \n",
		                                                                ddict.inval_dir_name);

			  fprintf (ddict.rpt_file_ptr, "Invalid directory list did not open, file name: %s \n",
		                                                                ddict.inval_dir_name);
              ddict.inval_dir = FALSE;
			  ret_val = 0;
		  }
		  else
		  {
			  /* 08-07-03 MDC - Changed initialization to 0 */
		/*	  for(count = 1; count <= 10; count++) */
			  for(count = 0; count < 10; count++)
			  {
				invalid_dir[count][0] = '\0';
				if(fgets(invalid_dir[count], 101, inv_dir_hdl) == '\0')
				{
					if(feof(inv_dir_hdl) != 0)
					{
						count = 11;
					}
					if(ferror(inv_dir_hdl) != 0)
					{
						printf ("Invalid directory list had read error, file name: %s \n",
		                                                                ddict.inval_dir_name);
						fprintf (ddict.rpt_file_ptr, 
							"Invalid directory list had read error, file name: %s \n",
		                                                                ddict.inval_dir_name);
						ddict.inval_dir = FALSE;
			            ret_val = 0;
					}
				}
				else
				{
					str_len = strlen(invalid_dir[count]);

					/* 08-07-03 MDC - Added this if condition to only chop 
					   off the last character of the string when a newline
					   character has been found within it.
					*/
					if( strstr(invalid_dir[count], "\n") != NULL )
						invalid_dir[count][str_len-1] = '\0';
				}
			  }
			  fclose(inv_dir_hdl);
		  }
	  }

	  if(ddict.use_log_file == TRUE)
	  {
		  ddict.log_file_ptr = fopen(ddict.log_file_name, "w+");
		  if (ddict.log_file_ptr == NULL)
		  {
			  printf ("Log file did not open, file name: %s \n",
		                                                                ddict.log_file_name);
			  printf ("Processing will continue without a log file\n");
			  fprintf (ddict.rpt_file_ptr, "Log file did not open, file name: %s \n",
		                                                                ddict.log_file_name);
			  fprintf (ddict.rpt_file_ptr, "Processing will continue without a log file\n");
              ddict.use_log_file = FALSE;
			  ret_val = 0;
		  }
	  }
return ret_val;
}


/*****************************************************************************
 *  rem_dup_ent(int Scount)
 *
 *  Scount contains the number of entries in Sarray.
 *
 *  removes the duplicate entries from the Sarray.  It also moves remaining
 *  entries to front of array.
 *****************************************************************************/
void rem_dup_ent(Scount)
int Scount;

{
	int   i, j, k, check_flag;
    char  string1[101];
    char  string2[101];
	for (i = 0; i < Scount; ++i)
    {
        if(strlen(Sarray[i]) > 2)
	    {	  
           strcpy(string1, Sarray[i]);

	       for (j = i+1; j < Scount; ++j)
           {
              strcpy(string2, Sarray[j]);
		      if(strcmp(string1, string2) == 0)
              {
                 check_flag = 0;
                 for (k = j; k <= Scount - 1; k++)
		         {
                     strcpy(Sarray[k], Sarray[k + 1]);
				     Sarray[k + 1][0] = '\0';
				     check_flag = 1;
			     }
                 if(check_flag == 1)
				 {
					 Scount--;
					 j--;
				 }
              }
		   }
	    }
    }

}
/*****************************************************************************
 *  QsortString()
 *
 *  Sorts the string array
 *****************************************************************************/

void QsortString(Sarray, left, right)
char Sarray[][100];
int left;
int right;
{
   int   i,j;
   char  *x;
   char  temp[100];

/*------------
 *  Begin sort - set boundaries
 */

   i = left;
   j = right;
   x = Sarray[(left+right)/2];

   do
   {
     while(strcmp(Sarray[i],x) < 0 && i < right) i++;
     while(strcmp(Sarray[j],x) > 0 && j > left) j--;

     if (i <= j)
     {
       strcpy(temp, Sarray[i]);
       strcpy(Sarray[i], Sarray[j]);
       strcpy(Sarray[j], temp);
       i++;
       j--;
     }
   } while (i <= j);

   if (left < j) QsortString(Sarray, left, j);
   if (i < right) QsortString(Sarray, i, right);

} /*** end QsortString ***/


/**********************************************************************
 *$Component                                                          *
 *    LOGICAL lv_setup (num_args, command_line, rpt_file_name,        *
 *                      dd_index_name, file_list, use_dd,             *
 *                      use_verbose, use_terminal, use_progress)      *
 *$Abstract                                                           *
 *    Parses the verifier command line and sets up the verifier.      *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    LABEL_VERIFIER                                                  *
 *    SETUP                                                           *
 *$Inputs                                                             *
 *    num_args:                                                       *
 *        The num_args variable is a integer containing the number of *
 *        arguments in a command line, e.g., argc.                    *
 *    command_line:                                                   *
 *        The command_line variable is an array of character strings  *
 *        containing command line arguments, e.g., argv.              *
 *$Outputs                                                            *
 *    rpt_file_name:                                                  *
 *        The rpt_file_name variable is a character string containing *
 *        the name of the file to be written to.                      *
 *    dd_index_name:                                                  *
 *        The dd_index_name variable is the name of the file that     *
 *        contains the Data Dictionary index information.             *
 *    file_list:                                                      *
 *        The file_list variable is a pointer to a STRING_LIST        *
 *        structure that contains a list of file names.               *
 *    use_dd:                                                         *
 *        The use_dd variable is a TRUE/FALSE flag indicating whether *
 *        data dictionary validation is enabled.                      *
 *    use_verbose:                                                    *
 *        The use_verbose variable a TRUE/FALSE flag indicating       *
 *        whether all error messages are displayed by the label       *
 *        verifier.                                                   *
 *    use_terminal:                                                   *
 *        The use_terminal variable a TRUE/FALSE flag indicating      *
 *        whether all error messages are displayed to the screen by   *
 *        the label verifier.                                         *
 *    use_aliases:                                                    *
 *        The use_aliases variable is a TRUE/FALSE flag indicating    *
 *        whether names are dealiased by the label verifier.          *
 *    use_progress:                                                   *
 *        The use_progress variable is a TRUE/FALSE flag indicating   *
 *        whether to display the number of files processed as a main  *
 *        loop progresses.                                            *
 *    use_dt:                                                         *
 *         Search top level directory only.                           *
 *    use_di:                                                         *
 *         Search all subdirectories recursively                      *
 *    inval_file:                                                     *
 *         A TRUE/FALSE flag indicating the presence of an invalid    *
 *         file list file                                             *
 *    inv_file_name:                                                  *
 *         The name of the file containing the invalid file list      *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure.       *
 *$Detailed_Description                                               *
 *    The lv_setup routine parses the command line passed into the    *
 *    label verifier and notifies the user of any errors. It sets     *
 *    all the output variables based on the flags it finds on the     *
 *    command line. It also initializes the data dictionary, if one   *
 *    is to be used.  This routine returns true if it succeeds, and   *
 *    false otherwise.  All error messages are placed on the global   *
 *    message list.                                                   *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/JPL                                             *
 *$Version_and_Date                                                   *
 *    1.5   February 13, 1992                                         *
 *$Change_history                                                     *
 *    MDD   05-31-91   Original generation.                           *
 *    DPB   06-14-91   Added code to check for missing file name.     *
 *    DPB   09-27-91   Moved into lv_tool.c                           *
 *    MDD   10-22-91   Fixed malloc, free, and sys_exit_system calls  *
 *    MDD   12-10-91   Fixed so that -r option is not necessary if    *
 *                     -t is enabled.                                 *
 *    MDD   02-13-92   Fixed VMS access violation and added conditions*
 *                     so that unrecognized command line options are  *
 *                     treated properly.                              *
 *    MDC   05-11-04   Gathered all the different user options and    *
 *                     placed them in a structure. i.e. Instead of    *
 *                     use_dd, it is now ddict.use_dd.                *
 *    MDC   06-17-04   Added check for "-nol3d" option on the command *
 *                     line.                                          *
 *    MDC   06-15-05   Added disclaimer message to the help screen    *
 **********************************************************************/

LOGICAL lv_setup (num_args, command_line)

int         num_args;
char        *command_line [];
{
   LOGICAL success = TRUE;
   LOGICAL report_name_found = FALSE;
   LOGICAL input_name_found = FALSE;
   LOGICAL dd_idx_name_found = FALSE;
   LOGICAL dd_full_name_found = FALSE;
   int i;  
   char temp [PDS_MAXLINE + 1];
   char err_msg [PDS_MAXLINE + 1];
   STRING_LIST *temp_list = NULL;
   LOGICAL file_flag = FALSE;
   LOGICAL ivf_name_found = FALSE;
   LOGICAL log_name_found = FALSE;
   char *dd_full_name = NULL;

   LOGICAL index_dd_test = FALSE;
   LOGICAL full_dd_test = FALSE;

/** BEGIN **/
   /*---------------------------------------------------------------------*/
   /** initialize all flags to their defaults                            **/
   /*---------------------------------------------------------------------*/
                             
   ddict.use_dd        = TRUE;
/*   ddict.use_verbose   = TRUE;
   ddict.use_terminal  = FALSE;
*/
   ddict.use_aliases   = FALSE;
/*   ddict.use_progress  = FALSE; */
   ddict.dd_index_name = NULL;
   ddict.rpt_file_name = NULL;
   ddict.file_list     = NULL;
   ddict.use_di        = FALSE;  /* initialize for recursive directory searching */
   ddict.inval_file    = FALSE;
   ddict.use_log_file = FALSE;
   ddict.log_file_name = NULL;
   ddict.sel_file_name = NULL;
   ddict.use_sel_file = NULL;
   ddict.inval_dir = FALSE;
   ddict.inval_dir_name = NULL;
   ddict.lbl_det = TRUE;

   for (i=1; i<num_args; i++)
   {
	   strcpy(temp, command_line[i]);
	   util_strip_lead_and_trail(temp, ' ');
	   if(strcmp(temp, "-d") == 0) index_dd_test = TRUE;
	   if(strcmp(temp, "-df") == 0) full_dd_test = TRUE;
   }
   
   /* 07-26-05 MDC - Cannot run lvtool with both -d and -df specified */
   if(full_dd_test && index_dd_test)
   {
	   printf("You may not specify both -d and -df\n");
	   success = FALSE;
   }
   else
   {
       /*---------------------------------------------------------------------*/
	   /** LOOP through the command line arguments                           **/
	   /*---------------------------------------------------------------------*/
	   for (i = 1; i < num_args; i++)
	   {
           file_flag = FALSE;
		   strcpy (temp, command_line [i]);
		   util_strip_lead_and_trail (temp, ' ');
		   /*-----------------------------------------------------------------*/
		   /** IF the next flag found on the command line is a simple ON/OFF **/
		   /**    flag THEN                                                  **/
		   /**    set the appropriate global flag                            **/
		   /*-----------------------------------------------------------------*/

		/*       if (strcmp (temp, "-nd") == 0)
			ddict.use_dd = FALSE;
			else if (strcmp (temp, "-v") == 0)
			*use_verbose = TRUE;
			else if (strcmp (temp, "-nv") == 0)
			*use_verbose = FALSE;
			else if (strcmp (temp, "-nt") == 0)
			*use_terminal = FALSE;
			else if (strcmp (temp, "-t") == 0)
			*use_terminal = TRUE;
		*/
			if (strcmp (temp, "-a") == 0)
			ddict.use_aliases = TRUE;
		/*      else if (strcmp (temp, "-p") == 0)
			*use_progress = TRUE;
		*/
			else if (strcmp (temp, "-na") == 0)
			ddict.use_aliases = FALSE;
			else if (strcmp (temp, "-nol3d") == 0)
			ddict.lbl_det = FALSE;
			else if (strcmp (temp, "-di") == 0)
			ddict.use_di = TRUE;           /* Turn off recursive directory searching*/
			/*-----------------------------------------------------------------*/
			/** ELSE IF this is the last flag on the line THEN                **/
			/**     We have failed.  All of the remaining flags must be       **/
			/**     followed by file names.                                   **/
			/*-----------------------------------------------------------------*/

			else if ((file_flag = ((strcmp (temp, "-r") == 0) || 
				(strcmp (temp, "-f") == 0)   || (strcmp (temp, "-d") == 0) || 
				(strcmp (temp, "-ivf") == 0) || (strcmp (temp, "-sf") == 0) ||
				(strcmp (temp, "-ivd") == 0) || (strcmp (temp, "-lef") == 0)))
					&& (i >= (num_args - 1)) )
			{
			success = FALSE;
			sprintf (err_msg, "Command line option \"%s\" must be followed by a file name",
				command_line [i]);
			err_append_message (ERROR1, err_msg);
			}
			/*-----------------------------------------------------------------*/
			/* ELSE IF this option is followed by another flag THEN            */
			/*     We have failed.  All of the remaining flags must be         */
			/*     file names.                                                 */
			/*-----------------------------------------------------------------*/
			else if (file_flag && *(command_line[i+1]) == '-')
			{
			success = FALSE;
			sprintf (err_msg, "Command line option \"%s\" must be followed by a file name",
				command_line [i]);
			err_append_message (ERROR1, err_msg);
			}

			/*-----------------------------------------------------------------*/
			/** ELSE IF the next flag is the USE DD flag THEN                 **/
			/**    copy the next argument to the dd index name variable       **/
			/*-----------------------------------------------------------------*/

			else if (strcmp (temp, "-d") == 0)
			{
				dd_idx_name_found = TRUE;
				i++;
		
				Malloc_String(ddict.dd_index_name, String_Size(command_line [i]));
				strcpy (ddict.dd_index_name, command_line [i]);
			}
			/*----------------------------------------------------------------*/
			/** 07-26-05 MDC                                                 **/
			/** ELSE IF the next flag is the USE DD FULL flag THEN           **/
			/**   copy the next argument to the dd full name variable        **/
			/*----------------------------------------------------------------*/
			else if (strcmp (temp, "-df") == 0)
			{
				dd_full_name_found = TRUE;
				i++;

				Malloc_String(dd_full_name, ((int) String_Size(command_line [i])));
				strcpy (dd_full_name, command_line[i]);
			}
			/*-----------------------------------------------------------------*/
			/** ELSE IF the next flag is the INPUT FILE flag THEN             **/
			/**     LOOP until the next flag is found on the command line     **/
			/**        assume the next argument is a file specification and   **/
			/**        expand it to a list of files                           **/
			/**        add the list to the overall list                       **/
			/**     ENDLOOP                                                   **/
			/*-----------------------------------------------------------------*/

			else if (strcmp (temp, "-f") == 0)
			{
				input_name_found = TRUE;

				do
				{
					i++;
					temp_list = slv_get_volume_list (command_line [i], ddict.use_di);
					if (temp_list != NULL)
							ddict.file_list = util_append_string_list (ddict.file_list, 
									(char *) temp_list, LIST_TYPE);

					ddict.input_file_name = command_line[i];
				} while ((i < num_args - 1) && *(command_line [i + 1]) != '-');
			}
			/*-----------------------------------------------------------------*/
			/** ELSE IF the next flag is the INVALID FILE LIST flag THEN      **/
			/**    copy the next argument to the inval_file name variable     **/
			/*-----------------------------------------------------------------*/

			else if (strcmp (temp, "-ivf") == 0)
			{
					ddict.inval_file = TRUE;
					ivf_name_found = TRUE;
					i++;
					Malloc_String(ddict.inval_file_name, String_Size(command_line [i]));
					strcpy (ddict.inval_file_name, command_line [i]);
			}
				/*-----------------------------------------------------------------*/
			/** ELSE IF the next flag is the REPORT FILE flag THEN            **/
			/**    copy the next argument to the report file name             **/
			/*-----------------------------------------------------------------*/

			else if (strcmp (temp, "-r") == 0)
			{
				report_name_found = TRUE;
				i++;
				Malloc_String(ddict.rpt_file_name, String_Size(command_line[i]));
				strcpy (ddict.rpt_file_name, command_line [i]);
			}
			/*-----------------------------------------------------------------*/
			/** ELSE IF the next flag is the INVALID DIR LIST flag THEN      **/
			/**    copy the next argument to the inval_dir name variable     **/
			/*-----------------------------------------------------------------*/

			else if (strcmp (temp, "-ivd") == 0)
			{
				ddict.inval_dir = TRUE;
				i++;
				Malloc_String(ddict.inval_dir_name, String_Size(command_line [i]));
				strcpy (ddict.inval_dir_name, command_line [i]);
			}
			/*-----------------------------------------------------------------*/
			/** ELSE IF the next flag is the LOG FILE flag THEN               **/
			/**    copy the next argument to the log file name                **/
			/*-----------------------------------------------------------------*/
			   
			else if (strcmp (temp, "-lef") == 0)
			{
				ddict.use_log_file = TRUE;
				log_name_found = TRUE;
				i++;
				Malloc_String(ddict.log_file_name, String_Size(command_line[i]));
				strcpy (ddict.log_file_name, command_line [i]);
			}
			/*-----------------------------------------------------------------*/
			/** ELSE IF the next flag is the SELECT FILE flag THEN            **/
			/**    copy the next argument to the select file name             **/
			/*-----------------------------------------------------------------*/

			else if (strcmp (temp, "-sf") == 0)
			{
				i++;
				Malloc_String(ddict.sel_file_name, String_Size(command_line[i]));
				strcpy (ddict.sel_file_name, command_line [i]);
				ddict.use_sel_file = TRUE;
			}

			/*-----------------------------------------------------------------*/
			/** ELSE                                                          **/
			/**    flag is not recognized so print an error                   **/
			/*-----------------------------------------------------------------*/

			else
			{
			success = FALSE;
			sprintf (err_msg, "Unrecognized command line option \"%s\"",
				command_line [i]);
			err_append_message (ERROR1, err_msg);
			}
			/*-----------------------------------------------------------------*/
			/** ENDIF                                                         **/
			/*-----------------------------------------------------------------*/
		}
	} /* END IF/ELSE full_dd_test && idx_dd_test....*/
   /*---------------------------------------------------------------------*/
   /** ENDLOOP                                                           **/
   /** IF we are using a data dictionary but no name was given THEN      **/
   /**    assign a default name to the data dictionary index name        **/
   /** ENDIF                                                             **/
   /*---------------------------------------------------------------------*/
/*
   if (ddict.use_dd && !dd_name_found)
   {

      Malloc_String(ddict.dd_index_name, String_Size(PDS_DEFAULT_DD));
      strcpy (ddict.dd_index_name, PDS_DEFAULT_DD);
   }
*/
   if (ddict.use_dd && full_dd_test && !dd_full_name_found)
   {
		Malloc_String(dd_full_name,((int) String_Size(PDS_DEFAULT_DD_FULL)));
		strcpy (dd_full_name, PDS_DEFAULT_DD_FULL);
   }
   else if (ddict.use_dd && !full_dd_test && !dd_idx_name_found)
   {
		Malloc_String(ddict.dd_index_name, String_Size(PDS_DEFAULT_DD));
		strcpy (ddict.dd_index_name, PDS_DEFAULT_DD);
   }
   /*---------------------------------------------------------------------*/
   /** IF no report file name was found and terminal display is not      **/
   /**    enabled THEN print an error                                    **/
   /** ENDIF                                                             **/
   /*---------------------------------------------------------------------*/

   if (!report_name_found)
   {
      success = FALSE;         
      err_append_message (ERROR1, "You must provide a report file name");
   }
  /*---------------------------------------------------------------------*/
   /** IF no input file name was found THEN                              **/
   /**    print an error                                                 **/
   /** ENDIF                                                             **/
   /*---------------------------------------------------------------------*/

   if (!input_name_found)
   {
      success = FALSE;
      err_append_message (ERROR1, 
         "You must provide the name of the label(s) to be verified");
   }

   /*---------------------------------------------------------------------*/
   /** IF no report file name was found and terminal display is not      **/
   /**    enabled THEN print an error                                    **/
   /** ENDIF                                                             **/
   /*---------------------------------------------------------------------*/

   if (!log_name_found && ddict.use_log_file)
   {
       success = FALSE;         
	   err_append_message (ERROR1, "You must provide a log file name");
   }

   /*---------------------------------------------------------------------*/
   /** IF an error occurred THEN                                         **/
   /**    display program information                                    **/
   /** ENDIF                                                             **/
   /*---------------------------------------------------------------------*/

   if (!success)
   {
      err_append_message (INFO, "\nUsage:\n");
      err_append_message (INFO, 
       "ddict -[a,na,nol3d] [-d dd-index-name] [-df dd-full-name] -r report-file -f label-file");
      err_append_message (INFO, "Where:");
      err_append_message (INFO, "   -a:          Enables aliasing");
      err_append_message (INFO, "   -na:         Disables aliasing (default)");
	  err_append_message (INFO, "-nol3d:         Do not check for level 3 labels in files");
/*    err_append_message (INFO, "   -t:        Enables terminal output");
      err_append_message (INFO, "   -nt:       Disables terminal output (default)");
      err_append_message (INFO, "   -v:        Enables verbose error reporting (default)");
      err_append_message (INFO, "   -nv:       Disables verbose error reporting");
	  err_append_message (INFO, "   -p:        Enables progress reporting");
	  err_append_message (INFO, "   -np:       Disables progress reporting (default)");
*/    err_append_message (INFO, "   -d <file>:   Specifies the Data Dictionary index file to use");
	  err_append_message (INFO, "  -df <file>:   Specifies the Data Dictionary full file to use");
 /*   err_append_message (INFO, "   -nd:         Disables Data Dictionary usage"); */
      err_append_message (INFO, "   -r <file>:   Specifies the report file for output");
      err_append_message (INFO, "   -f <file>:   Specifies the label file(s) to be verified (can be wildcarded)");
	  err_append_message (INFO, "   -di:         DOS only, specifies not to search directory and sub directories");
	  err_append_message (INFO, "                Default is to search directories recursively");
	  err_append_message (INFO, "   -ivf <file>: Specifies a file containing file extensions to skip,");
	  err_append_message (INFO, "                default is no file");       
	  err_append_message (INFO, "                One extension per line, only 50 extensions");
	  err_append_message (INFO, "   -ivd <file>: Specifies a file containing directories to skip,");
	  err_append_message (INFO, "                default is no file. Each directory is on a");       
	  err_append_message (INFO, "                separate line, names are case sensitive. There can");
	  err_append_message (INFO, "                be up to 30 entries. Each directory name must be less");
	  err_append_message (INFO, "                than 100 characters in length");
	  err_append_message (INFO, "   -sf <file>:  Specifies a file containing keywords to be skipped,");
	  err_append_message (INFO, "                Keywords are comma seperated, no blanks, only 200");
	  err_append_message (INFO, "                characters, case sensitive");  
	  err_append_message (INFO, "   -lef <file>: Specifies a file for logging the path and name ,");
	  err_append_message (INFO, "                all files skipped");       
	  err_append_message (INFO, "\nDisclaimer:");
      err_append_message (INFO, "Copyright 2006-2007, by the California Institute of Technology.");
      err_append_message (INFO, "ALL RIGHTS RESERVED. United States Government Sponsorship acknowledged.");
      err_append_message (INFO, "Any commercial use must be negotiated with the Office of Technology Transfer");
      err_append_message (INFO, "at the California Institute of Technology.\n");
      err_append_message (INFO, "This software is subject to U. S. export control laws and regulations");
      err_append_message (INFO, "(22 C.F.R. 120-130 and 15 C.F.R. 730-774). To the extent that the software");
      err_append_message (INFO, "is subject to U.S. export control laws and regulations, the recipient has");
      err_append_message (INFO, "the responsibility to obtain export licenses or other export authority as");
      err_append_message (INFO, "may be required before exporting such information to foreign countries or");
      err_append_message (INFO, "providing access to foreign nationals.");
   }
   /*---------------------------------------------------------------------*/
   /** IF a data dictionary to be used THEN                              **/
   /**    initialize the data dictionary                                 **/
   /** ENDIF                                                             **/
   /*---------------------------------------------------------------------*/

   if (success && ddict.use_dd)
   {
	   if (full_dd_test)
	   {
           printf("\n");
		   /* 01-06-05 MDC - Create the pdsdd.idx file internally now */
		   ddict.dd_index_name = make_index(dd_full_name);
		   Lemme_Go(dd_full_name);
		   printf("\n");
		  
		   if (ddict.dd_index_name == NULL)
		   {
               err_append_message (WARNING,
		 	    		"Data Dictionary index file could not be created, Verification will cease");
		       err_append_message (WARNING,
			            "Check that the Data Dictionary full file exists in the directory path you have specified");
			   success = FALSE;
		   }
	   }
       
	   /* If the index file was successfully made or if we already have an index file...*/
	   if (success)
	   {
           if (!(ddict.use_dd = dd_init (ddict.dd_index_name)))
		   {
               err_append_message (WARNING, 
					"DDICT will not proceed without a data dictionary");
				  success = FALSE;
		   }
	   }
   }
   
   if (success && !ddict.use_dd)
   {
      err_append_message (WARNING, 
			"DDICT will not proceed without a data dictionary");
   }
   
   return (success);

} /*  End: "lv_setup"  */


/*****************************************************************************
 ROUTINE
	  char *make_index(char *dd_full_name)

 INPUT
      dd_full_name:
			The name of the PDS data dictionary .full file

 OUTPUT
	  If successful, the name of the PDS data dictionary index file

 DESCRIPTION
	  make_index will call the make_index program to create the data dictionary
	  index file internally within LVTOOL. Parts of this code was taken from
	  the make_index_name routine from the make_index program code.

  IMPORTANT NOTES
      This routine allocates memory for the resulting index file. Calling 
	  function should de-allocate this memory after use.

  CHANGE HISTORY
      10-25-04   MDC         Original code
 ******************************************************************************/
char *make_index(char *dd_full_name)
{
	char *input_command = NULL;
	int string_size = 0, make_success = 0;
	char *dd_index_file = NULL, *dd_path = NULL, *temp_ptr = NULL, *ext_ptr = NULL, *temp_str = NULL;
	char *dd_index_ext = DD_IDX_EXT;
	char *directory = NULL;

	
	if( (strstr(dd_full_name,".full") == NULL) && (strstr(dd_full_name,".FULL") == NULL) )
	{
		printf("\nData Dictionary filename does not have a .full extension: %s\n", dd_full_name);
		return NULL;
	}


	string_size = (int) String_Size("make_index") + (int) String_Size(dd_full_name);
	Malloc_String(input_command, string_size+2);
	
	sprintf(input_command,"make_index %s", dd_full_name);
	printf("\n\n**************Creating Data Dictionary index file**************\n");
	make_success = system(input_command);
	Lemme_Go(input_command);

	if(!make_success)
	{
		Malloc_String(dd_index_file, ((int) ((String_Size(dd_full_name) + String_Size(dd_index_ext)))) );
		strcpy(dd_index_file, dd_full_name);

   /*---------------------------------------------------------------------*/
   /** make life easier by converting the name to Unix format            **/
   /*---------------------------------------------------------------------*/

#ifndef SUN_UNIX
		directory = util_save_to_last_occurrence (dd_index_file, ':');
#endif

#ifdef VAX
		 dd_index_file = cvt_vax_to_unix_file_spec (dd_index_file);
#endif

#ifdef MSDOS_TC
		 dd_index_file = cvt_dos_to_unix_file_spec (dd_index_file);
#endif


   /*---------------------------------------------------------------------*/
   /** locate the old file extension                                     **/
   /** convert the new extension to upper case if the old one was upper  **/
   /**    case                                                           **/
   /** replace the old extension with the new one                        **/
   /*---------------------------------------------------------------------*/
   
		ext_ptr = strrchr (dd_index_file, '.');
		if (util_is_upper (dd_index_file))
			util_upper_case (dd_index_ext);
		if (ext_ptr == NULL || ext_ptr < (char *) strrchr (dd_index_file, '/'))
			strcat (dd_index_file, dd_index_ext);
		else
			strcpy (ext_ptr, dd_index_ext);	

   /*---------------------------------------------------------------------*/
   /** convert the file spec back to the type expected by the OS         **/
   /*---------------------------------------------------------------------*/

#ifdef VAX
		dd_index_file = cvt_unix_to_vax_file_spec (dd_index_file);
#endif

#ifdef MSDOS_TC
		dd_index_file = cvt_unix_to_dos_file_spec (dd_index_file);
#endif   

#ifndef SUN_UNIX
		if (directory != NULL)
		{
			Malloc_String( temp_str, ((int)(String_Size(directory) + String_Size(dd_index_file))) );
			sprintf (temp_str, "%s%s", directory, dd_index_file);
			Replace_String(dd_index_file, temp_str);
			Lemme_Go(directory);
			Lemme_Go(temp_str);
		}
#endif
	}
	else
        return NULL;


	return dd_index_file;
}



/**********************************************************************
 *$Component                                                          *
 *    void rpt_format_message (rpt_file_pointer, text)                *
 *$Abstract                                                           *
 *    Formats error messages and writes them to a file.               *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    LABEL_VERIFIER                                                  *
 *    REPORT                                                          *
 *$Inputs                                                             *
 *    rpt_file_pointer:                                               *
 *        The rpt_file_pointer variable is a file pointer for the     *
 *        report file.                                                *
 *    text:                                                           *
 *        The text variable is a general purpose character string     *
 *        that may contain one or more characters.                    *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    The rpt_format_message routine takes the text of the message    *
 *    passed in and formats so that it stays within the width of      *
 *    a PDS report.  Lines which are too long are wrapped and         *
 *    indented.                                                       *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    2.1   March 18, 1992                                            *
 *$Change_history                                                     *
 *    DPB   06-04-91   Original code.                                 *
 *    DPB   09-27-91   Moved into lv_toolc                           *
 *    MDD   10-22-91   Fixed malloc, free, and sys_exit_system calls  *
 *    MDD   12-10-91   Added check for NULL file pointer and changed  *
 *                     to return message string if no report file     *
 *    MDD   03-18-92   The great int -> long conversion               *
 **********************************************************************/

void rpt_format_message (rpt_file_pointer, text)
                                                      
FILE *rpt_file_pointer;
char *text;

{
    char *start_char = {NULL};
    char *end_char = {NULL};
    char *dashes = {NULL};
    char *blanks = {NULL};
    char *c = {NULL};
    char save_it = {EOS};
    long report_indent = {0};
    long report_width = {PDS_REPORT_WIDTH};
    long line_size = {0};
    long len = {0};
    long i = {0};

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** IF a message was passed in THEN                                     **/
    /*-----------------------------------------------------------------------*/

    if (text != NULL)
    {
        /*-------------------------------------------------------------------*/
        /** Find the double dash delimiter thingy in the message.           **/
        /**     Messages will look something like this:                     **/
        /**         WARNING: Line 123 -- BANDS: Not in data dictionary.     **/
        /**     We are using the location of the " -- " characters to       **/
        /**     figure out how far the wrapped part of the line should      **/
        /**     be indented.                                                **/
        /*-------------------------------------------------------------------*/

        dashes = util_locate_substring (text, " -- ");

        if (dashes != NULL)
            report_indent = 4 + ((long) (dashes - text));

        if (report_indent >= (report_width - 2))
            report_indent = 0;

        /*-------------------------------------------------------------------*/
        /** Initialize the string of blanks used for indentation.           **/
        /*-------------------------------------------------------------------*/

	    Malloc_String(blanks, (int) report_indent + 1);
        for (i = 0; i < report_indent; ++i)
	    blanks[i] = ' ';
	    blanks[i] = EOS;

        /*-------------------------------------------------------------------*/
        /** Figure out the size of the wrapped parts of the line.           **/
        /*-------------------------------------------------------------------*/

        line_size = report_width - report_indent;

        /*-------------------------------------------------------------------*/
        /** Now that we have all that out of the way, we can LOOP through   **/
        /**         the string until we have wrapped and written the        **/
        /**         whole thing.                                            **/
        /*-------------------------------------------------------------------*/

        for (start_char = text; *start_char != EOS; start_char = end_char)
        {
            /*---------------------------------------------------------------*/
            /** Find the length of the remaining part of the string.        **/
            /*---------------------------------------------------------------*/

            len = strlen (start_char);

            /*---------------------------------------------------------------*/
            /** IF we are at the beginning of the string THEN               **/
            /**     Use the total width of the report to figure out where   **/
            /**         the end of the line should be.                      **/
            /** ELSE                                                        **/
            /**     Write the blanks to the report file and use the space   **/
            /**         left over after indentation to figure out where     **/
            /**         the end of the line should be.                      **/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

            if (start_char == text) 
            {
                if (len > report_width)
		    end_char = (char *) (start_char + report_width);
                else
		    end_char = (char *) (start_char + len);
            }
            else
            {
                if (rpt_file_pointer != NULL) 
                   fprintf (rpt_file_pointer, "%s", blanks);

                if (len > line_size)
		    end_char = (char *) (start_char + line_size);
                else
		    end_char = (char *) (start_char + len);

            }  /*  End:  "if (start_char == text) ... else ..."  */

            /*---------------------------------------------------------------*/
            /** IF the current part of the message is still too large to    **/
            /**        fit without wrapping THEN                            **/
            /**     Find the last blank in the line and wrap there.         **/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

            if (*end_char != EOS)
            {
                for (c = end_char; ((c >= start_char) && (*c != ' ')); --c) ;
                           
                if (c > start_char)
                    end_char = c;

            }  /*  End: "if (*end_char != EOS) ..."  */

            /*---------------------------------------------------------------*/
            /** Write the current part of the message to the report file.   **/
            /*---------------------------------------------------------------*/

            save_it = *end_char;
            *end_char = EOS;
            if (rpt_file_pointer != NULL)
               fprintf (rpt_file_pointer, "%s\n", start_char);
            *end_char = save_it;

            /*---------------------------------------------------------------*/
            /** Bypass the last blank character.                            **/
            /*---------------------------------------------------------------*/

            if (*end_char == ' ')
                ++end_char;

        /*-------------------------------------------------------------------*/
        /** ENDLOOP                                                         **/
        /*-------------------------------------------------------------------*/

        }  /*  End:  "for (start_char = text; ..."  */

        /*-------------------------------------------------------------------*/
        /** Deallocate local storage.                                       **/
        /*-------------------------------------------------------------------*/

        Lemme_Go(blanks);

    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if ((text != NULL) && ..."  */
       
    /*-----------------------------------------------------------------------*/
    /** RETURN                                                              **/
    /*-----------------------------------------------------------------------*/

    return;

/** END **/

}  /*  "rpt_format_message"  */


/**********************************************************************
 *$Component                                                          *
 *    void rpt_main_footer (rpt_file_pointer)                         *
 *$Abstract                                                           *
 *    Closes a report file.                                           *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    LABEL_VERIFIER                                                  *
 *    REPORT                                                          *
 *$Inputs                                                             *
 *    rpt_file_pointer:                                               *
 *        The rpt_file_pointer variable is a file pointer for the     *
 *        report file.                                                *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    The rpt_main_footer routine writes a message indicating the     *
 *    end of a validation report, and closes the report file.         *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Kristy L. Marski /JPL                                           *
 *$Version_and_Date                                                   *
 *    1.2   December 10, 1991                                         *
 *$Change_history                                                     *
 *    KLM   03-25-91   Original generation.                           *
 *    DPB   09-27-91   Moved into lv_tool.c                           *
 *    MDD   12-10-91   Added check for NULL file pointer              *
 *    RSJ   12-18-97   Added DDictionary Extractor code               *
 **********************************************************************/

void rpt_main_footer (rpt_file_pointer)

FILE *rpt_file_pointer;

{

 char *date_string;

 if (rpt_file_pointer != NULL)
 {
   
   date_string = sys_get_ascii_date ();


   fprintf(rpt_file_pointer,"\n");
   fprintf(rpt_file_pointer,"************************************************************************\n");
   fprintf(rpt_file_pointer,"*                                                                      *\n");
   fprintf(rpt_file_pointer,"*        This is the end of the Data Dictionary Extractor Report       *\n");
   fprintf(rpt_file_pointer,"*                                                                      *\n");
   fprintf(rpt_file_pointer,"*                      %s", date_string);
   fprintf(rpt_file_pointer,"                        *\n");
   fprintf(rpt_file_pointer,"*                                                                      *\n");
   fprintf(rpt_file_pointer,"************************************************************************\n");

   fclose (rpt_file_pointer);
   Lemme_Go(date_string);
 }
 return;

} /*  End: "rpt_main_footer"  */

/**********************************************************************
 *$Component                                                          *
 *    FILE *rpt_main_header (rpt_file_name, use_dd,                   *
 *                           use_verbose, use_terminal, use_aliases,  *
 *                           init_index_name)                         *
 *$Abstract                                                           *
 *    Generates a report header and writes it to a file.              *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    LABEL_VERIFIER                                                  *
 *    REPORT                                                          *
 *$Inputs                                                             *
 *    rpt_file_name:                                                  *
 *        The rpt_file_name variable is a character string containing *
 *        the name of the file to be written to.                      *
 *    use_dd:                                                         *
 *        The use_dd variable is a TRUE/FALSE flag indicating whether *
 *        data dictionary validation is enabled.                      *
 *    use_verbose:                                                    *
 *        The use_verbose variable a TRUE/FALSE flag indicating       *
 *        whether all error messages are displayed by the label       *
 *        verifier.                                                   *
 *    use_terminal:                                                   *
 *        The use_terminal variable a TRUE/FALSE flag indicating      *
 *        whether all error messages are displayed to the screen by   *
 *        the label verifier.                                         *
 *    use_aliases:                                                    *
 *        The use_aliases variable is a TRUE/FALSE flag indicating    *
 *        whether names are dealiased by the label verifier.          *
 *    init_index_name:                                                *
 *       The init_index_name variable is a character string containing*
 *       the name of a DD index file.                                 *
 *    inv_file:														  *
 *	     A TRUE/FALSE flag indicating the presence of the invalid     *
 *       file list file                                               *
 *    inv_file_name													  *
 *       The name of the file containing the invalid file list.       *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    file_pointer:                                                   *
 *        The file_pointer variable is a pointer to the file          *
 *        being written to.                                           *
 *$Detailed_Description                                               *
 *    The rpt_main_header routine generates the main header for       *
 *    a validation report. This header includes the report title,     *
 *    the date, the time, and the flags used for verification.        *
 *$External_References                                                *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    This routine opens a file, but does not close it. Call the      *
 *    rpt_main_footer to end the report and close the file.           *
 *$Author_and_Institution                                             *
 *    Kristy L. Marski /JPL                                           *
 *$Version_and_Date                                                   *
 *    2.3   March 18, 1992                                            *
 *$Change_history                                                     *
 *    KLM   03-08-91   Original generation.                           *
 *    MDD   05-08-91   Changed to use flags rather than the command   *
 *                     line as arguments.                             *
 *    DPB   09-27-91   Moved into lv_tool.c                           *
 *    MDD   12-10-91   Added check for NULL file name                 *
 *    MDD   03-18-92   The great int -> long conversion               *
 *    DWS   11-13-97   Added lv_or_slv to arguement list.             *
 *    RSJ   12-18-97   Added DDictionary Extractor code               *
 *    DWS   08-23-01   Added code to check the first 5 lines of the   *
 *                     data dictionary for the Version and generated  *
 *                     information instead of the looking only at     *
 *                     lines 2 and 3.                                 *
 **********************************************************************/

FILE *rpt_main_header ()
{
 FILE *file_pointer = NULL;
 FILE *dd_pointer = NULL;
 char *date_string;
 long num_blanks = 0;
 char *dd_msg;
 long ipos;
 char string[100];
 char *string1;

/*----------*/
/** BEGIN  **/
/*----------*/

 if (ddict.rpt_file_name != NULL)
 { 
   date_string = sys_get_ascii_date ();

   /*------------------------------------------------------------*/
   /** IF there is an error opening the file, THEN              **/
   /**    Append error message to the error list                **/
   /*------------------------------------------------------------*/

   if ((file_pointer = fopen(ddict.rpt_file_name, "w+")) == NULL)
   {
         err_append_message(ERROR1, "Unable to open report file");
   }

   /*------------------------------------------------------------*/
   /** ELSE                                                     **/
   /**    Write the the welcome banner to the report file       **/
   /**    Write the date and time to the report file            **/
   /**    Write the version of the Data Dictionary being used.  **/
   /*------------------------------------------------------------*/
                
   else
   {        

      fprintf(file_pointer,"************************************************************************\n");
      fprintf(file_pointer,"*                                                                      *\n");
      fprintf(file_pointer,"*               Planetary Data System Data Dictionary                  *\n");          
      fprintf(file_pointer,"*                     Extractor Version %s                            *\n",
		                                                                     DDICT_VERSION);
      fprintf(file_pointer,"*                                                                      *\n");
      fprintf(file_pointer,"*                      %s", date_string);
      fprintf(file_pointer,"                        *\n");
      fprintf(file_pointer,"*                                                                      *\n");
      if (ddict.dd_index_name != NULL && ddict.use_dd == TRUE)
      {
         dd_msg = "       Data Dictionary Name:";
		 num_blanks = (70 - (strlen (dd_msg) + 1 + strlen (ddict.dd_index_name)));
         fprintf (file_pointer, "*%s %-*s*\n", dd_msg,
			 num_blanks + strlen (ddict.dd_index_name), ddict.dd_index_name);
      }

   /*------------------------------------------------------------*/
   /** Read the 1st three lines in Data Dictionary (PDSDD.FULL) **/
   /**    Line1 - ignore this line                              **/
   /**    Line2 - // Version: pdscat1r18 //                     **/
   /**    Line3 - // Generated: March 18, 1996 //               **/
   /**                                                          **/
   /**    Remove the comments from Line2 and Line3 and then     **/
   /**    print the version and date to the output file         **/
   /*------------------------------------------------------------*/
	  strcpy(string, ddict.dd_index_name);
	  ipos = 0;
      while (string[ipos] != '.') ++ipos;
	  string[ipos] = '\0';
	  strcat(string, ".full");

      if ((dd_pointer = fopen(string, "r+")) != NULL)
      { 
		 char buf[5][100];
		 int counter = 0;
		 string1 = NULL;

		 for(counter = 0; counter < 5; counter++)
		 {
		         fgets(buf[counter], 100, dd_pointer);
		 }
		 for(counter = 0; counter < 5; counter++)
		 {
	         string1 = strstr(buf[counter], "Version");
			 if(string1 != NULL)				 
			 {
				strcpy(string, string1);
				ipos = 0;
				while (string[ipos] != '*') ++ipos;
				string[ipos] = '\0';
				dd_msg = "    Data Dictionary";
				num_blanks = (70 - (strlen(dd_msg) + 1 + strlen(string)));
				fprintf (file_pointer, "*%s %-*s*\n", dd_msg,
								num_blanks + strlen (string), string);
				break;
			 }

		 }
		 if(string1 == NULL)
		 {
			 printf("This data dictionary is invalid, the Version statement must be in first five records\n");
			 exit(1);
		 }
		 string1 = NULL;
		 for(counter = 0; counter < 5; counter++)
		 {
       		 string1 = strstr(buf[counter], "Generated");
			 if(string1 != NULL)
			 {
				strcpy(string, string1);
				ipos = 0;
				while (string[ipos] != '*') ++ipos;
				string[ipos] = '\0';
				dd_msg = "  Data Dictionary";
				num_blanks = (70 - (strlen(dd_msg) + 1 + strlen(string)));
				fprintf (file_pointer, "*%s %-*s*\n", dd_msg,
								num_blanks + strlen (string), string);
				break;
			 }
		 }
	
		 if(string1 == NULL)
		 {
			 printf("This data dictionary is invalid, the Generated statement must be in first five records\n");
			 exit(1);
		 }
		 fclose(dd_pointer);
	  } 
	
	  if (ddict.inval_file == TRUE)
	  {
		 fprintf(file_pointer,"*           A List Of Invalid Extensions Has Been Supplied In File:    *\n");
         num_blanks = (72 - (strlen (ddict.inval_file_name) + 1)) / 2; 
         fprintf(file_pointer,"* %*s%*s\n", num_blanks, ddict.inval_file_name,
			                                 num_blanks + strlen(ddict.inval_file_name) - 1, "*");
	  }
      fprintf(file_pointer,"*                                                                      *\n");
      if (ddict.use_dd == TRUE)
         fprintf(file_pointer,"*                  Data Dictionary Validation is ON                    *\n");
      else
         fprintf(file_pointer,"*                  Data Dictionary Validation is OFF                   *\n");
      fprintf(file_pointer,"*                                                                      *\n");
      fprintf(file_pointer,"************************************************************************\n");

   }     

   /*------------------------------------------------------------*/
   /** ENDIF                                                    **/
   /** free the date string                                     **/
   /** return a pointer to the file                             **/
   /*------------------------------------------------------------*/
                            
   Lemme_Go(date_string);
 }
 return(file_pointer);

}  /*  End: "rpt_main_header"  */




/**********************************************************************
 *$Component                                                          *
 *    void rpt_keyword_errors (rpt_file_pointer, LOGICAL)             *
 *$Abstract                                                           *
 *    Writes the keyword header to the report file.                   *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    LABEL_VERIFIER                                                  *
 *    REPORT                                                          *
 *$Inputs                                                             *
 *    rpt_file_pointer:                                               *
 *        The rpt_file_pointer variable is a file pointer for the     *
 *        report file.                                                *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    The rpt_keyword_errors routine checks to see if there are       *
 *    any keywords and then writes the appropriate header....         *
 *$External_References                                                *
 *    Item                     Shared-Data           Access           *
 *    ------------------------------------------------------------    *
 *    pds_message_list         pdsglob.h             read             *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.3   December 10, 1991                                         *
 *$Change_history                                                     *
 *    MDD   05-06-91   Original code.                                 *
 *    KLM   05-14-91   Added delimiters around report sections.       *
 *    DPB   09-27-91   Moved into lv_tool.c                           *
 *    MDD   12-10-91   Added check for NULL file pointer              *
 *    RSJ   12-18-97   Added DDictionary Extractor code               *
 **********************************************************************/

void rpt_keyword_errors (rpt_file_pointer, lAction)
                                                      
FILE *rpt_file_pointer;
LOGICAL lAction; 
{
 ERROR_LIST *msg_ptr = {NULL};

 if (rpt_file_pointer != NULL)
 {   

   if (lAction == TRUE)
   {

      fprintf (rpt_file_pointer, 
      "\n------------------------------------------------------------------------\n");
      fprintf (rpt_file_pointer, 
      "*****        The following set of keywords and definitions         *****\n");
      fprintf (rpt_file_pointer, 
      "*****           were extracted from the Data Dictionary            *****\n");
      fprintf (rpt_file_pointer, 
      "------------------------------------------------------------------------\n");
   }
   else
   {

      fprintf (rpt_file_pointer, 
      "\n------------------------------------------------------------------------\n");
      fprintf (rpt_file_pointer, 
      "*****  No keywords were located to extract from the Data Dictionary  *****\n");
      fprintf (rpt_file_pointer, 
    "------------------------------------------------------------------------\n\n");

   }
 }
 return;

}  /*  End: "rpt_keyword_errors"  */


/**********************************************************************
 *$Component                                                          *
 *    void rpt_semantic_errors (rpt_file_pointer, LOGICAL)            *
 *$Abstract                                                           *
 *    Writes the semantic label errors to a report file.              *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    LABEL_VERIFIER                                                  *
 *    REPORT                                                          *
 *$Inputs                                                             *
 *    rpt_file_pointer:                                               *
 *        The rpt_file_pointer variable is a file pointer for the     *
 *        report file.                                                *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    The rpt_semantic_errors routine checks to see if there are      *
 *    and semantic errors and writes them to the report file.         *
 *$External_References                                                *
 *    Item                     Shared-Data           Access           *
 *    ------------------------------------------------------------    *
 *    pds_message_list         pdsglob.h             read             *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.3   December 10, 1991                                         *
 *$Change_history                                                     *
 *    MDD   05-06-91   Original code.                                 *
 *    KLM   05-14-91   Added delimiters around report sections.       *
 *    DPB   09-27-91   Moved into lv_tool.c                           *
 *    MDD   12-10-91   Added check for NULL file pointer              *
 *    RSJ   12-18-97   Added DDictionary Extractor code               *
 **********************************************************************/

void rpt_semantic_errors (rpt_file_pointer, lAction)
                                                      
FILE *rpt_file_pointer;
LOGICAL lAction; 
{
 ERROR_LIST *msg_ptr = {NULL};

 if (rpt_file_pointer != NULL)
 {   

   if (lAction == TRUE)
   {

      fprintf (rpt_file_pointer, 
      "\n------------------------------------------------------------------------\n");
      fprintf (rpt_file_pointer, 
      "*****            The following keywords are not present            *****\n");
      fprintf (rpt_file_pointer, 
      "*****                   in the Data Dictionary                     *****\n");
      fprintf (rpt_file_pointer, 
      "------------------------------------------------------------------------\n\n");
   }
   else
   {

      fprintf (rpt_file_pointer, 
      "\n------------------------------------------------------------------------\n");
      fprintf (rpt_file_pointer, 
      "*****       All keywords were located in the Data Dictionary       *****\n");
      fprintf (rpt_file_pointer, 
    "------------------------------------------------------------------------\n\n");

   }
 }
 return;

}  /*  End: "rpt_semantic_errors"  */


/**********************************************************************
 *$Component                                                          *
 *   STRING_LIST *slv_get_volume_list (dir_mask)                      *
 *$Abstract                                                           *
 *    Make a list of file names.                                      *
 *$Inputs                                                             *
 *    dir_mask:                                                       *
 *        The dir_mask variable is a character string that contains   *
 *        a system specific file specification, including wildcards.  *
 *$Returns                                                            *
 *    file_list:                                                      *
 *        The file_list variable is a pointer to a STRING_LIST        *
 *        structure that contains a list of file names.               *
 *$Detailed_Description                                               *
 *    The slv_get_volume_list routine will return a list of files     *
 *    that match the directory mask passed in as a STRING_LIST.       *
 *    It will recurse through the entire subtree.                     *
 *                                                                    *
 *    ****          WARNING WARNING WARNING WARNING      ****         *
 *                                                                    *
 *    This code has only been modified from sys_get_file_list for     *
 *    the UNIX case.  The VMS and MSDOS cases must be changed before  *
 *    this will work for them.                                        *
 *                                                                    *
 *$Error_Handling                                                     *
 *   The routine will return NULL if it finds no files that match     *
 *   the dir_mask or if it cannot obtain a directory listing.         *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/JPL                                             *
 *$Change_History                                                     *
 *    1.0   MDD   07-15-93  Original code.                            *
 *          DWS     11-12-97   Moved back into lv_tool, slv_tool is   *
 *                             now an option in lv_tool.              *
 **********************************************************************/
STRING_LIST *slv_get_volume_list (dir_mask, use_di)

char *dir_mask;
LOGICAL use_di;
{
  char *fmask;
  char *temp_str = NULL;
  int length_1;
  STRING_LIST *file_list = {NULL};

/** BEGIN **/

#ifdef MSDOS_TC
{

   char *directory = NULL;
   int got_handle = 0;
   char * cwdret;

   length_1 = 0;
   directory = sys_get_path (dir_mask); /* dir_mask might have c:\test\a.lbl */
                                        /* directory would then have c:\test\ */
   if (directory == NULL) 
   {
	   cwdret = getcwd(directory, sizeof(directory));
	   directory = cwdret;
   }
   else
   {
       length_1 = strlen(directory);        // gets length of path
   }
   fmask = dir_mask + length_1;         //
                                         /* Do the directory walk   */
                                         /* and build the file list */
   file_list = dir_walk(directory, fmask, file_list, use_di);}

#else
{
  FILE *fp = NULL;
  char command_str [PDS_MAXLINE + 1];
  char data_str [PDS_MAXLINE + 1];
  LOGICAL success = TRUE;
  char* slash = NULL;

   /*----------------------------------------------------------------*/
   /** create the directory command based on the OS,                **/
   /**    redirecting the output to a file                          **/
   /*----------------------------------------------------------------*/

#ifdef VAX
   sprintf (command_str, 
               "dir/col=1/ver=1/nohead/notrail/exclude=*.dir/out=%s %s",
               pds_temp_data_fname, dir_mask);
#endif

#ifdef SUN_UNIX
   slash = strrchr (dir_mask, '/');
   if (slash)
   {
      *slash++ = EOS;
      sprintf (command_str, "find %s -name \"%s\" -print > %s", dir_mask, slash,
               pds_temp_data_fname);
      *(--slash) = '/';
   }
   else
   {
      sprintf (command_str, "find . -name \"%s\" -print > %s", dir_mask,
                             pds_temp_data_fname);
   }
      
#endif

   /*----------------------------------------------------------------*/
   /** issue the command to the system                              **/
   /** IF there was an error in the system command THEN             **/
   /**    append an error message to the global list                **/ 
   /*----------------------------------------------------------------*/

   success = sys_do_command (command_str);
   success = TRUE;
   if (success != TRUE)
   {
      sprintf (command_str, "Unable to get list of files specified by: %s", 
                             dir_mask);
      err_append_message (ERROR, command_str);   
   }
   /*----------------------------------------------------------------*/
   /** ELSE                                                         **/
   /*----------------------------------------------------------------*/

   else
   {
      /*-------------------------------------------------------------*/
      /** IF the redirect file can't be opened THEN                 **/
      /**    append an error to the list                            **/
      /*-------------------------------------------------------------*/

      if ((fp = fopen (pds_temp_data_fname, "r")) == NULL)
      {
         err_append_message (ERROR, "Unable to open scratch file.");
      }
      /*-------------------------------------------------------------*/
      /** ELSE                                                      **/
      /*-------------------------------------------------------------*/

      else
      {
         /*----------------------------------------------------------*/
         /** WHILE there are more lines in the redirect file DO     **/
         /*----------------------------------------------------------*/

         while ((fgets (data_str, PDS_MAXLINE, fp)) != NULL)
         {
            util_clean_up_string (data_str);
            util_compress_char (data_str, ' ');
            if (util_string_is_empty (data_str)) break;

            /*-------------------------------------------------------*/
            /** IF this is Unix and the string does not have "/"    **/
            /**     at the end THEN                                 **/
            /**   remove the file mode characters and add the name  **/
            /**   to the list of files                              **/
            /** ENDIF                                               **/
            /*-------------------------------------------------------*/

#ifdef SUN_UNIX

              file_list = util_append_string_list (file_list,
                                                       data_str,
                                                           STRING_TYPE); 
 /*           temp_str = String_End(data_str);
            if (*temp_str != '/')
            {
              if (*temp_str == '*' || *temp_str == '=' || *temp_str == '@')
                 *temp_str = EOS;
              file_list = util_append_string_list (file_list,
                                                       data_str,
                                                           STRING_TYPE); 
            } */

#endif
            /*-------------------------------------------------------*/
            /** IF this is VMS THEN                                 **/
            /**   add the file to the file list                     **/
            /** ENDIF                                               **/
            /*-------------------------------------------------------*/

#ifdef VAX
            file_list = util_append_string_list (file_list,
                                                     data_str, 
                                                         STRING_TYPE);
#endif

         }
         /*----------------------------------------------------------*/
         /** ENDWHILE there are more lines...                       **/
         /*----------------------------------------------------------*/

         fclose (fp);
      }
      /*-------------------------------------------------------------*/
      /** ENDIF the redirect file can't be opened...                **/
      /*-------------------------------------------------------------*/

    }
   /*----------------------------------------------------------------*/
   /** ENDIF there was an error in the system command...            **/
   /*----------------------------------------------------------------*/

}
#endif

   return (file_list);

} /*  End: "slv_get_volume_list"  */


/****************************************************************************/
/*  DATA DEFINITION FORMATING ROUTINE                                       */
/*  LVDDFORM1(instring, outstring)                                          */
/*  PURPOSE:  Replace ASCII "\n" with hex '\n'                              */
/****************************************************************************/
void lvddform1(instring, outstring)
char * instring;                                     /*contains input string*/
char * outstring;                                    /*empty but will       */
                                                     /*formatted string     */
{                                               /*Replace all ascii new line*/
	                                            /*sequences with hex value  */
	char * str_ptr;
    int count_in = 0;
    int count_out = 0;
	char newchar;
	char nextchar;
	str_ptr = instring;
    while (*str_ptr != EOS) 
    {
        newchar = instring[count_in];
		nextchar = instring[count_in + 1];
		if((newchar == '\\') && (nextchar == 'n'))
		{
	        outstring[count_out] = '\n';         /* Add newline char.        */
			count_in++;
			str_ptr++;
		}
		else
		{
	        outstring[count_out] = newchar;      /* Terminate temp str.      */

		}
		count_in++;
		count_out++;
		str_ptr++;
    }  /*  End:  "for (start = old_str; ..."  */
	outstring[count_out] = '\0';
}
/****************************************************************************/
/*  DATA DEFINITION FORMATING ROUTINE                                       */
/*  LVDDFORM2(instring, outstring, length)                                  */
/*  PURPOSE:  Limit line length to specified length                         */
/****************************************************************************/
void lvddform2(instring, outstring, length)
char * instring;                                     /*contains input string*/
char * outstring;                                    /*empty but will       */
                                                     /*formatted string     */
int    length;                                       /*length of new lines  */
{                                               /*Replace all ascii new line*/
    char *start = {NUL};
    char *end = {NUL};
    long size = {String_Size(instring)};
    int len;
    char *temp_str = {NUL};
    temp_str = (char *) malloc ((int) length + 2);
    Check_Malloc(temp_str);
	                                            /*sequences with hex value  */


    for (start = instring; *start != EOS; start = end)
    {
        end = util_last_word (start, length);   /* Locate end of line.      */
        len = end - start;                      /* Length of line.          */
	    strncpy (temp_str, start, (int) len);   /* Copy line to temp str.   */
	    temp_str [len] = '\n';                  /* Add newline char.        */
	    temp_str [len + 1] = EOS;               /* Terminate temp str.      */
        strcat (outstring, temp_str);           /* Concat temp onto new str.*/

    }  /*  End:  "for (start = old_str; ..."  */
}

/*******************************************************************************/
/*$Componant                                                                   */
/*	int chk_file_type(invalid_list, temp_list);                                */
/*                                                                             */
/*$Abstract                                                                    */
/*   Determines whether a file should be processed                             */
/*                                                                             */
/*$inputs                                                                      */
/*   invalid_list                                                              */
/*       pointer to a list of file name extensions not to check                */
/*   temp_list                                                                 */
/*       A pointer to the name of a file to be processed                       */
/*                                                                             */
/*$Outputs                                                                     */
/*   None                                                                      */
/*                                                                             */
/*$Returns                                                                     */
/*   A value of one indicates to process the file, zero indicates do not       */
/*   process the file.                                                         */
/*                                                                             */
/*$Detailed Description                                                        */
/*   Check file type will search a list containing file name extension types   */
/*   that are not to be processed and compare each with the file name passed   */
/*   to the program.  If the extension of the file matches one of the          */
/*   in the list the program will return a value of zero.  If a match is not   */
/*   found a value of one will be returned.                                    */
/*                                                                             */
/*$Error Handling                                                              */
/*   None                                                                      */
/*                                                                             */
/*$Author and Institution                                                      */
/*   Dale Schultz  /JPL/ACRO                                                   */
/*$Date                                                                        */
/*   July 20, 1998                                                             */
/*                                                                             */
/*$Change History                                                              */
/*   DWS 07-20-1998   Original code                                            */ 
/*******************************************************************************/
int chk_file_type(invalid_list, temp_list)
char * invalid_list;
STRING_LIST * temp_list;
{
int check_file = 1;  /*if value left at one the file will be processed*/
int chk_result;
int pos;
char *ext_ptr;
pos = strcspn(temp_list->text, ".");
ext_ptr = temp_list->text + pos + 1;
chk_result = 0;
if(strstr(invalid_list, ext_ptr) == NULL) chk_result = 1;  /* no match, process file*/
return (chk_result);
}

/*******************************************************************************/
/* validate_label (char *file_id)                                              */
/*                                                                             */
/* A file with an extension type of "LBL" or "lbl" will be assumed to contain  */
/* a label.                                                                    */
/* This function attempts to identify files which may not be labels.  When such*/
/* a file is identified, it will not be processed.  This will speed up process-*/
/* ing and reduce the size of the report by eliminating unneccessary error     */
/* messages.                                                                   */
/* The first 1000 bytes of the file specified in the arguement will be searched*/
/* for three records.  If none of the records are found the file will not be   */
/* validated by LVTOOL.  The records are:                                      */
/* PDS_VERSION_ID = PDS3                                                       */
/* OBJECT =                                                                    */
/* RECORD_TYPE =                                                               */
/* The number of spaces between parts of these strings may very.               */
/*******************************************************************************/

int validate_label (char *file_id)
{
#define LSIZE 1000

	FILE *lfile;
	char lrec [LSIZE + 1];
	int  lcnt;
	char *lrecp, *lrecl;

	char *pntr1;
	char *pntr2;
	char *pntr3;

	if ((strstr(file_id, ".lbl") != NULL) || (strstr(file_id, ".LBL") != NULL)) return(1);

	if ((lfile = fopen (file_id, "rb")) == NULL) {
		return (-1);
	}
	lcnt = fread (lrec, sizeof(char), LSIZE, lfile);
	lrec[LSIZE] = '\0';
	lrecp = lrec;
	lrecl = lrecp + lcnt;
	fclose(lfile);


	pntr1 = strstr(lrecp, "PDS_VERSION_ID");/* looking for a line that has */
	if(pntr1 != NULL)                       /*PDS_VERSION_ID = PDS3        */
	{										/*don't know how many blanks   */
		pntr2 = strstr(pntr1, "=");         /*between parts                */
		if(pntr2 != NULL)
		{
			pntr3 = strstr(pntr2, "PDS3");
			if(pntr3 != NULL)
			{								/*got all three parts, must not*/
				if ((pntr3 - pntr1) <= 80)  /*take up more than 80 bytes   */
				{                           
					return(1);              /*this probably is a label     */
				}
			}
		}
	}
	
	pntr1 = strstr(lrecp, "OBJECT");        /*how about is there an        */
	if(pntr1 != NULL)                       /*OBJECT = in the record       */
	{                                       /*dont now how many blanks     */
		pntr2 = strstr(pntr1, "=");			/*between the parts            */
		if(pntr2 != NULL)
		{
			if ((pntr2 - pntr1) <= 80)
			{
				return(1);                  /*this may be a label          */
			}
		}
	}
	
	pntr1 = strstr(lrecp, "RECORD_TYPE");   /*Last try.  Is there a        */
	if(pntr1 != NULL)                       /*RECORD_TYPE = in the record? */
	{
		pntr2 = strstr(pntr1, "=");
		if(pntr2 != NULL)
		{
			if ((pntr2 - pntr1) <= 80)
			{
				return(1);                  /*got it, this may be a label   */
			}
		}
	}
	return (0);                             /*this is probably not a record */
}

