/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Component                                                           * 
 *    kwvtool.c                                                        *
 * Abstract                                                            *
 *    PDS Label Verifier-specific routines                             *
 * Detailed Description                                                *
 *    This file contains the driver for the PDS Label Verifier and     *
 *    the PDS Summary Label Verifier, and the routines that are        *
 *    specific to this tool.                                           *
 * Internal References                                                 *
 *    Driver:                                                          *
 *        lvtool                                                       *
 *                                                                     *
 *    Setup routine:                                                   *
 *        lv_setup                                                     *
 *        lv_special                                                   *
 *        slv_special                                                  *
 *                                                                     *
 *    Report generation routines:                                      *
 *        rpt_format_message                                           *
 *        rpt_main_footer                                              *
 *        rpt_main_header                                              *
 *        rpt_semantic_errors                                          *
 *                                                                     *
 *    Internal routines:                                               *
 *        lv_tool                                                      *
 *        slv_file_header                                              *
 *        slv_main_footer                                              *
 *        slv_odl_messages                                             *
 *        slv_print_aliases                                            *
 *        slv_semantic_errors                                          *
 *        slv_alias_messages                                           *
 *        slv_semantic_errors                                          *
 *        slv_get_volume_list                                          *
 * Authors and Institutions                                            *
 *    Marti D. Demore / J.P.L.                                         *
 *    Kristy L. Marski / J.P.L.                                        *
 *    David P. Bernath / J.P.L.                                        *
 * Version and Date                                                    *
 *    1.1   June 22, 1992                                              *
 * Change History                                                      *
 *    DWS   08-09-99   Added file extension type exclusion for 1.2     *
 *    DWS   02-03-00   Recompile because of fix in lab_utils           *
 *    DWS   05-16-00   Changed format of ivd file to have one line     *
 *                     per directory. Also moved building of file list *
 *                     for file input to end of parsing of command     *
 *                     line.                       version 1.4         *
 *    DWS   04-12-01   Added code after remove dups to clean out Sarray*
 *    DWS   12-10-01   Recompiled to pick up group changes in lablib   *
 *                                                 version 1.7         *
 *    MDC   08-07-03   Modified lv_special routine. See notes.         *
 *                     Modified check_exclusion_opts routine. See notes*
 *                     Modified chk_dir_name routine. See notes.       *
 *                     Modified lv_setup routine. See notes.           *
 *                     Modified main routine. See notes.               *
 *    MDC   08-20-03   Modified chk_dir_name routine. See notes.       *
 *    DWS   11-13-03   Recompiled for use with NULL followed by unit   *
 *                     id value.                                       *
 *    MDC   05-04-04   Modified report_writer routine. See notes.      *
 *    MDC   05-05-04   Removed any code dealing with the variables,    *
 *                     use_terminal, use_progress, and use_verbose, of *
 *                     the lvtool_v structure                          *
 *                     Obsoleted the "-t", "-nt", "-p", "-np", "-v",   *
 *                     and "-nv" options from kwvtool.                 *
 *    MDC   06-04-04   Modified lv_special routine. See notes below.   *
 *    MDC   06-16-04   Modified lv_special and lv_setup routines.      *
 *    MDC   01-11-05   Modified the following routines: main, lv_setup *
 *                     Created a new routine, make_index, to create    *
 *                     the data dictionary index file internally       *
 *    MDC   06-15-05   Modified lv_setup routine. Added software       *
 *                     disclaimer message to the help screen           *
 *    MDC   10-06-05   Modified lv_special. See notes in routine.      *
 *    MDC   12-19-05   Modified report_writer routine.                 *
 *    MDC   04-20-06   Modified main routine to not create a scratch   *
 *                     file in a windows environment since it doesn't  *
 *                     use this file during execution. This elminates  *
 *                     kwvtool from printing out a msg. to the user    *
 *                     that the scratch file could not be created if a *
 *                     drive is write-protected or what not.           *
 *                                                                     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "kwvtool.h"



/**********************************************************************
 *$Component                                                          *
 *    void lvtool  (verifier_args)                                    *
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
 *      08-08-03   MDC        Added code to remove "JunkZZZ" files.   *
 *      05-10-04   MDC        Removed initializations to pds_display  *
 *                            and pds_verbose.                        *
 *      04-20-06   MDC        Only call the lab_setup routine in a    *
 *                            non-windows environment. kwvtool doesn't*
 *                            need this scratch file.                 *
 **********************************************************************/
struct lvtool_v    lvtv;

void main (argc, argv)                                       /*10-8-97*/
int argc;
char *argv [];
{
   LOGICAL command_line_ok;
   FILE        *keyword_file_ptr = {NULL};

/** BEGIN **/


   debug = 0;	/* set this to one if you do want to take advantage of */
                /* a previously built JUNKZZZ1.TXT                     */


   lvtv.sel_file_name = '\0';
   printf("KWVTOOL %s\n", VERSION);

   /*-----------------------------------------------------------------------*/
   /** Setup the files needed by the toolbox                               **/
   /** Setup the verifier/parse the command line                           **/
   /*-----------------------------------------------------------------------*/

   /* 04-20-06 MDC - Only call the lab_setup routine when running in a
         non-windows environment. Windows doesn't need to create a scratch
		 file.
   */

#ifndef MSDOS_TC
   lab_setup ();
#endif

   command_line_ok = lv_setup (argc, argv);
/*   pds_display = lvtv.use_terminal; */
/*   pds_verbose = lvtv.use_verbose; */
   if(debug != 1)keyword_file_ptr = kwvtool_setup();
   err_write_to_file (NULL, FALSE);   /* print all command line errors to */
                                      /*the screen and clear the messages */


      if (command_line_ok)               /* IF the command line was fine THEN */
      {

         lvtv.rpt_file_ptr = rpt_main_header();  /*print the main report header*/
                   
  
		 if(lvtv.use_dd)
		 {
		    lv_special(keyword_file_ptr);
		 }
		 else
		 {
             err_write_to_file (lvtv.rpt_file_ptr, TRUE);
             lab_clear_messages ();
		 }

	     rpt_main_footer(lvtv.rpt_file_ptr);   
      }

   /*-----------------------------------------------------------------------*/
   /** ENDIF                                                               **/
   /*-----------------------------------------------------------------------*/
   if(command_line_ok && (lvtv.use_log_file == TRUE)) fclose(lvtv.log_file_ptr);

   if(lvtv.inval_file) Lemme_Go(lvtv.inval_file_name);
   if(lvtv.inval_dir)  Lemme_Go(lvtv.inval_dir_name);
   util_deallocate_string_list (lvtv.file_list);
   
   Lemme_Go(lvtv.dd_index_name);
   Lemme_Go(lvtv.rpt_file_name);

   lab_exit ();
   
   /* 08-08-03 MDC - Remove JunkZZZ files after we're done */
   fclose(keyword_file_ptr);
   remove("JunkZZZ1.TXT");
   remove("JunkZZZ2.TXT");

   return;

} /*  End: "main"  */



/********************************************************************************/
/* FILE *KWVTOOL_SETUP()                                                        */
/* Deletes the old file JunkZZZ1.TXT if there is one and opens a new one.       */
/* KWVTOOL places all keywords (and their values) into this file as they are    */
/* encountered in the specified input files.  The file will be read into memory */
/* later and all duplicates will be removed as the file is read.                */
/*                                                                              */
/* The handle for this file will be returned to the calling routine.            */
/********************************************************************************/
FILE *kwvtool_setup()
{
   FILE *keyword_file_ptr = {NULL};
   char        OutFile[100];
		if (keyword_file_ptr == NULL)
        {
			strcpy(OutFile, "JunkZZZ1.TXT");
            keyword_file_ptr = fopen(OutFile, "w+");
		    if (keyword_file_ptr == NULL)
            {
			  printf("\n Could not open the keyword OUTPUT file: %s", OutFile);
              exit(1);
            }
        }
		return(keyword_file_ptr);
}                                                /* End of KWVTOOL_SETUP */

FILE *kwvtool_setup3()
{
   FILE *keyword_file_ptr = {NULL};
   char        OutFile[100];
		if (keyword_file_ptr == NULL)
        {
			strcpy(OutFile, "JunkZZZ1.TXT");
            keyword_file_ptr = fopen(OutFile, "r");
		    if (keyword_file_ptr == NULL)
            {
			  printf("\n Could not open the keyword OUTPUT file: %s", OutFile);
              exit(1);
            }
        }
		return(keyword_file_ptr);
}                                                /* End of KWVTOOL_SETUP */

/********************************************************************************/
/* FILE *KWVTOOL_SETUP2()                                                       */
/* Deletes the old file JunkZZZ2.TXT if there is one and opens a new one.       */
/* KWVTOOL places the final keyword list into this file                         */
/*                                                                              */
/* The handle for this file will be returned to the calling routine.            */
/********************************************************************************/
FILE *kwvtool_setup2()
{
   FILE *keyword_file_ptr = {NULL};
   char        OutFile[100];
		if (keyword_file_ptr == NULL)
        {
			strcpy(OutFile, "JunkZZZ2.TXT");
            keyword_file_ptr = fopen(OutFile, "w+");
		    if (keyword_file_ptr == NULL)
            {
			  printf("\n Could not open the keyword OUTPUT file: %s", OutFile);
              exit(1);
            }
        }
		return(keyword_file_ptr);
}                                                /* End of KWVTOOL_SETUP */


/***********************************************************************/
/*$Component                                                           */
/*  VOID lv_special(lvtv)                                              */
/*$Abstract                                                            */
/*  This routine handles lv tool specific tasks                        */
/*$Keywords                                                            */
/*    LV_TOOL                                                          */
/*    TOOL_MAIN                                                        */
/*    LABEL_VERIFIER                                                   */
/*$Inputs                                                              */
/***********************************************************************/
/* These members of lvtv are used as input to this function:           */
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
/*        keywords.                                                  */
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
/*   MDC  08-07-03  Changed invalid_dir to be 30 entries long instead  */
/*                  of 10                                              */
/*   MDC  06-04-04  Clean out Sarray after sorting each keyword set to */
/*                  allow kwvtool to properly sort the keywords in     */
/*                  alphabetical order                                 */
/*   MDC  10-06-05  When checking for duplicate keywords, added code   */
/*                  for the case when there is exactly 2 keywords in   */
/*                  the Sarray                                         */
/***********************************************************************/
void lv_special(keyword_file_ptr)
FILE       *keyword_file_ptr;
{
  FILE      *keyword_file_ptr_1;
  int       status;  
  STRING_LIST   *temp_list;
  char      string1[150];
  char      string2[100];
  /*  char      string3[100][100];*/
  char string3[202];
  int		i;	       			    /* counter	                 */
  int		MatchFound = FALSE;		/* if match found            */
  int       Scount = 0;
  int       FirstPass = TRUE;
                       /**************************************************/
                       /* variables for keyword selection check          */
                       /**************************************************/
  FILE *    sel_file_hdl;                                              /**/
  char      invalid_list[201];
  int       keeper;                                                    /**/
  int       rec_count;                                                 /**/
                       /**************************************************/
                       /* end of variables for keyword selection check   */
                       /**************************************************/
  int       x, addit_flag;
  int       y;
  int       success = 1;
  int       ok_to_go;
  int       ok_to_go_1;
  int       ok_to_go_2 = 1;

  /* 08-07-03 MDC - Changed first array spec. to 30 instead of 10 */
 /* char      invalid_dir[10] [100]; */
  char      invalid_dir[30] [100];
  int       count;
  int       alpha_ct;
  int       stop_on_error = 0;
  int       nbr_chrs;
  char      *spc_ptr;
  char teststring[200];
  status = 0;


/*------------
 *  Null terminate strings
 */
      string1[148] = '\n';
      string1[149] = '\0';
      string2[99] = '\0';
      if(check_exclusion_opts(invalid_list, invalid_dir) == 0) return;
      /*--------------------------------------------------------------------*/
      /** IF the report file was opened THEN                               **/
      /*--------------------------------------------------------------------*/
/*      if (lvtv.use_terminal || lvtv.rpt_file_ptr != NULL) */
	  if(lvtv.rpt_file_ptr != NULL)
      {
         /*-----------------------------------------------------------------*/
         /** write any messages so far to the report and clear them        **/
         /** LOOP through all the files to be verified                     **/
         /*-----------------------------------------------------------------*/
         err_write_to_file (lvtv.rpt_file_ptr, TRUE);
         lab_clear_messages ();
		 if(debug != 1) 
		 {

         for (temp_list = lvtv.file_list; temp_list != NULL;
		                                     temp_list = temp_list->next)
         {
			if(lvtv.inval_file  == FALSE) 
			{
		  		ok_to_go = 1;                         /* process them all*/
			}
			else
			{
				ok_to_go = chk_file_type(invalid_list, temp_list);   /*07-20-98*/
			    if((ok_to_go == 0) && (lvtv.use_log_file))
				{
					   fprintf(lvtv.log_file_ptr,        /*log exclusion if required */
					   "File Excluded, Excluded File Type, File: %s\n", temp_list->text);
				}
			}
			if((lvtv.inval_dir  == FALSE) && (ok_to_go == 1)) 
                                                          /*if there is an invalid */
			{                                             /*directory list, make   */
				                                          /*sure that this file is */
				                                          /*in an acceptable direct*/
			  	   ok_to_go_1 = 1;                         /* process them all*/
			}
			else
			{
	 	   	       ok_to_go_1 = chk_dir_name(invalid_dir, temp_list);   /*07-20-98*/
				   if((ok_to_go_1 == 0) && (lvtv.use_log_file))
				   {
					   fprintf(lvtv.log_file_ptr, 
					   "File Excluded, Excluded Directory, File: %s\n", temp_list->text);
				   }
			}

            /* 06-16-04 MDC - Add capability to check if the file contains a label. */
			if((lvtv.lbl_det) && (ok_to_go == 1) && (ok_to_go_1  == 1))
			{
                   ok_to_go_2 = validate_label (temp_list->text);
			       if((ok_to_go_2 == 0) && (lvtv.use_log_file))
			       {
                       fprintf(lvtv.log_file_ptr, 
						"File Excluded, File Has No Label, File: %s\n", temp_list->text);
				   }
			}

			if ((ok_to_go) &&(ok_to_go_1) && (ok_to_go_2))
			{
/*				if (pds_display)
				{
					printf ("\n\n----------------------------------------------------------\n");
					printf (" Processing %s:\n\n", temp_list -> text);
				} 
*/
					printf ("Processing %s \n", temp_list -> text);

					/*-------------------------------------------------------------*/
					/** clear error messages                                      **/
					/** read the label                                            **/
					/** dealias the label, if needed                              **/
					/** print the object heirarchy to the report                  **/
					/** delete all alias messages                                 **/
					/** print the syntax errors to the report                     **/
					/*-------------------------------------------------------------*/

				lab_clear_messages ();
				lvtv.odl_root = lab_read_label (temp_list -> text, &status);
				if (lvtv.use_aliases)
				{
					lvtv.alias_message_ptr = dd_unalias (lvtv.odl_root);
					err_deallocate_list (lvtv.alias_message_ptr);
				}

					/*-------------------------------------------------------------*/
					/** clear error messages                                      **/
					/** IF a label was read and the data dictionary option in ON  **/
					/**    THEN                                                   **/
					/**    verify the label semantics and print the errors        **/
					/**    clear the messages                                     **/
					/*-------------------------------------------------------------*/

				lab_clear_messages ();
				if (lvtv.use_dd && lvtv.odl_root != NULL)
				{
					ver_semantics (keyword_file_ptr, lvtv.odl_root);
					lab_clear_messages ();
				}
					/*-------------------------------------------------------------*/
					/** ENDIF                                                     **/
					/** clean up the label in memory                              **/
					/*-------------------------------------------------------------*/
				lab_remove_label (lvtv.odl_root, &status);
			}	
			else  printf ("Skipping %s\n", temp_list->text);
		}
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


		/* Read through the file picking out all of the keywords that start with */
		/* a particular letter.  Loop through starting with A and ending with Z  */
		/* Keep them in Sarray, sort them, delete the dups, add Sarray to        */
		/* junkzzz2.txt.       */
		if(debug != 1)
		{
			fflush(keyword_file_ptr);
			fclose(keyword_file_ptr);
		}
        keyword_file_ptr = kwvtool_setup3();
		if(debug == 1)
		{
              rewind(keyword_file_ptr);
              keyword_file_ptr = fopen("junkzzz1.txt","r");
		}
        keyword_file_ptr_1 = kwvtool_setup2();
		printf("Removing Duplicate Keywords and Sorting the Resulting Unique Keywords: \n\n");
		for(alpha_ct = 65; alpha_ct < 91; alpha_ct++)    /* start the loop to get the records     */
		{
			count = 0;
			rewind(keyword_file_ptr);                    /* rewind the file to start over for     */
			while (fgets(string1, 140, keyword_file_ptr) != NULL) /*each batch of records         */
			{
				count++;
				addit_flag = 1;							 /* set the flag to add record to array   */
				if(string1[0] != (char)alpha_ct) addit_flag = 0; /* does it start with right char */
				if (addit_flag == 1)					/* if it did add it to the array          */
				{
					strcpy(Sarray[Scount], string1);	/*add it to the array					  */
					Scount++;
				}
			
														/* if Scount is bigger than the array     */
				if(Scount >= DDICT_KW_ARRAY_SIZE)		/* print error message and end            */
				{
					fprintf(lvtv.rpt_file_ptr, 
					  "\nWARNING, Keyword array has run out of room.  Reduce the number of\n");
					fprintf(lvtv.rpt_file_ptr,
					  "         files being validated.\n\n");
					printf( 
					  "\n\nWARNING, Keyword array has run out of room.  Reduce the number of\n");
					printf(
					  "         files being validated.\n\n");
					stop_on_error = 1;  /* stop this now*/
					break;
				}
			}
			if((stop_on_error == 0) && (count > 0))  /*start of sort and dup removal */
			{
														/* this message is to passify the user */
				printf("%d %c type recs ext. from a total of %d for sorting and dup checking\n",
							Scount, (char)alpha_ct, count);
				if(Scount > 1)
				{
					QsortString(Sarray, 0, Scount-1);			/* sort the array                       */
					do											/* make sure sort worked.  If not do    */
					{											/* it again.                            */
						status = FALSE;
						for (i = 0; i < Scount-1; ++i)
						{
							if (strcmp(Sarray[i], Sarray[i+1]) > 0) status = TRUE;
						}
						if (status) QsortString(Sarray, 0, Scount -1);
					} while (status);
				}

				/* remove the duplicates*/
				if(Scount == 1)
				{
					fwrite(Sarray[0], sizeof(char), strlen(Sarray[0]), keyword_file_ptr_1);
					fflush(keyword_file_ptr_1);
				}
				/* 10-06-05 MDC - Added to check condition if we have exactly 2 keywords */
				if(Scount == 2)
				{
					/* If these pair of keywords/values aren't the same, then write them both to the file */
					if(strcmp(Sarray[0], Sarray[1]) != 0)
					{
						for(x = 0; x < 2; x++)
                            fwrite(Sarray[x], sizeof(char), strlen(Sarray[x]), keyword_file_ptr_1);
						fflush(keyword_file_ptr_1);
					}
				}
				if(Scount > 2)
				{
					y = 0;
					for(x = 0; x < Scount, y <= Scount; x = y)
					{
						y++;
						if(strcmp(Sarray[x], Sarray[y]) != 0)
						{
							fwrite(Sarray[x], sizeof(char), strlen(Sarray[x]), keyword_file_ptr_1);
							x = y;
						}
					}
					fflush(keyword_file_ptr_1);
				}
				/* 06-03-04 MDC - Code to clean out Sarray. This makes kwvtool sort the
				   keywords properly. 
			    */
				for (x = 0; x < Scount; x++) strcpy(Sarray[x],"");
				Scount = 0;
			}/*end of sort and dup removal */

		} 				/* go on to next letter of alphabet */
		
		for (x = 0;x < DDICT_KW_ARRAY_SIZE; x++)  /* new code to clean array.   DWS   04-12-01*/
		{
			Sarray[x][0] = '\0';
		}
/* rebuild Sarray for use by selection process and report */		
		Scount = 0;
		rewind(keyword_file_ptr_1);
		while( !feof(keyword_file_ptr_1) )
		{
			fgets( Sarray[Scount], 150, keyword_file_ptr_1 );
			Scount++;
		}
		fclose(keyword_file_ptr_1);
		fclose(keyword_file_ptr); /* 08-08-03 MDC - Close off filehandle */

	  
				 /***************************************/
                 /* Check list against sel_file         */
                 /* remove all keywords that are        */
                 /* in the sel_file.                    */
                 /***************************************/

                 /* Start by attempting to open sel_file*/
		printf("Remove key words that are in user's exclusion list\n");
		if(lvtv.use_sel_file)
		{
			sel_file_hdl = fopen(lvtv.sel_file_name, "r");
		    if(!sel_file_hdl) printf ("\nsel_file did not open, all keywords will be reported\n");
		    else
		    {
			   rec_count = 0;
				   fgets(string3, 200, sel_file_hdl);
			   for (i = 0; i < Scount; i++)
			   {          /* select the keywords that are in the list */
					if (strlen(Sarray[i]) > 2)
					{	  
						keeper = FALSE;
						strcpy(string1, Sarray[i]);
						spc_ptr = strstr(Sarray[i], " ");
						if(spc_ptr != NULL)
						{
							nbr_chrs = spc_ptr - Sarray[i]; 
							strncpy(teststring, Sarray[i], nbr_chrs);
							teststring[nbr_chrs] = '\0';
						}
						if(strstr(string3, teststring) != NULL)
						{
							strcpy(Sarray[i], "zz");
						}
					}
				}
			}
			fclose (sel_file_hdl);
		}
		         /***************************************/
		         /* End of check                        */
		         /***************************************/
		
        /*------------------------------------------------------------------*/
        /** Write the Keywords and their values out to the report file     **/
        /*------------------------------------------------------------------*/
		printf("Write Report\n");
		report_writer(lvtv.rpt_file_ptr, Sarray, Scount);
	}  /* ENDIF **/
}                                                      /*  End: "lv_special"*/


/*****************************************************************************/
/*  QsortString()                                                            */
/*                                                                           */
/*  Sorts the string array                                                   */
/*****************************************************************************/

void QsortString(Sarray,left, right)
char Sarray[][150];
int left;
int right;
{
   int   i,j;
   char  *x;
   char  temp[150];

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



/**********************************************************************/
/*  VOID REPORT_WRITER(RPT_FILE_PTR, SARRAY, SCOUNT)                  */
/*                                                                    */
/* FUNCTION:                                                          */
/*  Write the keyword and keyword value report using Sarray as input. */
/*  rpt_file_ptr points to the report file                            */
/*  Sarray is the big array holding the report data                   */
/*  Scount is variable containing the number of elements containing   */
/*  data in Sarray                                                    */
/*                                                                    */
/*                                                                    */
/*  CHANGES:                                                          */
/*     05-04-04   MDC     Added MACRO definitions called MAX_KWD_LEN  */
/*                        and MAX_VAL_LEN.                            */
/*                                                                    */
/*     10-22-04   MDC     Print statements for the keyword/value pair */
/*                        lead off with a whitespace now just like    */
/*                        in previous versions. This fixes a bug when */
/*                        running kwvtool through the perl tools.     */
/*     12-19-05   MDC     Modified report to accomodate 61 char kwd   */
/*                        lengths.                                    */
/**********************************************************************/
void report_writer(rpt_file_ptr, Sarray, Scount)
FILE *rpt_file_ptr;
char Sarray[DDICT_KW_ARRAY_SIZE][DDICT_VAL_STR_LEN];
int Scount;
{

  /* 05-04-04 MDC - Added MACRO Definitions, MAX_KWD_LEN and MAX_VAL_LEN,
     to make it easier to make code changes since keywords can now be 
	 longer than the original 30 character limit.
	 MAX_KWD_LEN is defined as 60 and MAX_VAL_LEN is defined as 95.
  */
                       /**************************************************/
                       /* variables for report                           */
                       /**************************************************/
  char      kw_str[MAX_KWD_LEN+1] = {0};                                     /**/
  char      val_str[MAX_VAL_LEN+1] = {0};                                    /**/
  char     *kw_val_break;                                              /**/
  char      kw_count;                                                  /**/
  int       start_not_in_dd;                                           /**/
  int       start_in_dd;                                               /**/
  int       i,j;                                                         /**/
  char      string1[150];                                              /**/
  int       x;
                       /**************************************************/
                       /* end of variables for report                    */
                       /**************************************************/
  start_not_in_dd = 0;
  start_in_dd = 0;
  for (i = 0; i < Scount; i++)
  {
      if (strlen(Sarray[i]) > 2)                       /* flagged as a duplicate, don't list it*/
      {
 	      if(Sarray[i][1] != '\0')                     /* nothing in this one, don't list it   */
		  {
              strcpy(string1, Sarray[i]);              
              if (strstr(string1, "AAAAZ_") != NULL)   /* flag for not in data dictionary,     */
		      {                                        /* don't print the flag.                */
				  if(start_not_in_dd == 0)
				  {
					  if(start_in_dd == 1)             /* header for keywords not in dictionary*/
					  {
					      fprintf(rpt_file_ptr,
					    	  "\n----------------------------------------------\n");
					  }
                      fprintf(rpt_file_ptr,
						  "\nWARNING!  These selected keywords are not in the Data Dictionary\n");
				      start_not_in_dd = 1;
				      start_in_dd = 0;
				  }                                                /*initialize the two strings   */
		/*		  strcpy(kw_str, "                             ");*/ /* 59 Blanks                   */
			      
				  /* 05-06-04 MDC - Replaced hard-coded "59 blanks" statement with a for loop */
				  for(j=0; j < MAX_KWD_LEN; j ++)
				  {
					  if(kw_str[j] == ' ')
						  j = MAX_KWD_LEN;   /* Break out of the loop early if there's a NULL in place */
					  else
                          kw_str[j] = ' ';
				  }
				  kw_str[MAX_KWD_LEN] = '\0';
				                                                   /* 95 blanks in this one       */
				  for(j=0; j < MAX_VAL_LEN; j++)
				  {
					  if(val_str[j] == ' ')
						  j = MAX_VAL_LEN;
					  else
						  val_str[j] = ' ';
				  }
		/*		  strcpy(val_str,
"                                                                                               "); */
			      val_str[MAX_VAL_LEN] = '\0';
                                                                   /* there is a space between the*/
                  kw_val_break = strstr(Sarray[i], " ");           /* kw and the value            */
				  if(kw_val_break <= Sarray[i])
				  {
					  kw_count = strlen(Sarray[i]) - 1;
			          val_str[1] = '\n';
			          val_str[2] = '\0';
				  }
				  else
				  {
					  for (x = 0; x <=MAX_VAL_LEN; x++) val_str[x] = '\0';
			          kw_count = kw_val_break - Sarray[i] + 1;
			          strncpy(val_str, kw_val_break + 1, MAX_VAL_LEN);
					  for (x = 0; x <= MAX_VAL_LEN-1; x++) if(val_str[x] == 10) val_str[x] = '\0';
				  }
				  if(kw_count > MAX_KWD_LEN) kw_count = MAX_KWD_LEN;
			      strncpy(kw_str, Sarray[i] + 6, kw_count - 6);
                  fprintf(rpt_file_ptr, " %-61s     %s\n", kw_str, val_str);
		      }
		      else
              {
				  if(start_in_dd == 0)
				  {
					  if(start_not_in_dd == 1)
					  {
					      fprintf(rpt_file_ptr,
					    	  "\n----------------------------------------------\n\n");
					  }
					  fprintf(rpt_file_ptr,
						  "\nThese selected keywords are in the Data Dictionary\n\n");
					  start_in_dd = 1;
					  start_not_in_dd = 0;
				  }

				  /* 05-06-04 MDC - Replaced hard-coded "59 blanks" statement with a for loop */
				  for(j=0; j < MAX_KWD_LEN; j ++)
				  {
					  if(kw_str[j] == ' ')
						  j = MAX_KWD_LEN;   /* Break out of the loop early if there's a NULL in place */
					  else
                          kw_str[j] = ' ';
				  }
	/*			  strcpy(kw_str, "");*/ /* 59 Blanks */
			      kw_str[MAX_KWD_LEN] = '\0';
				  /* 95 blanks in this one */
				  for(j=0; j < MAX_VAL_LEN; j++)
				  {
					  if(val_str[j] == ' ')
						  j = MAX_VAL_LEN;
					  else
						  val_str[j] = ' ';
				  }

/*				  strcpy(val_str,
"                                                                                               ");*/
			      val_str[MAX_VAL_LEN] = '\0';
                  kw_val_break = strstr(Sarray[i], " ");
				  if(kw_val_break <= Sarray[i])
				  {
					  kw_count = strlen(Sarray[i]) - 1;
			          val_str[1] = '\n';
			          val_str[2] = '\0';
				  }
				  else
				  {
					  for (x = 0; x <=MAX_VAL_LEN; x++) val_str[x] = '\0';
			          kw_count = kw_val_break - Sarray[i] + 1;
			          strncpy(val_str, kw_val_break + 1, 95);
					  for (x = 0; x <= MAX_VAL_LEN-1; x++) if(val_str[x] == 10) val_str[x] = '\0';
				  }
				  if(kw_count > MAX_KWD_LEN) kw_count = MAX_KWD_LEN;
			      strncpy(kw_str, Sarray[i], kw_count);
                  fprintf(rpt_file_ptr, " %-61s     %s\n", kw_str, val_str);
              }
		  }
	  }
	} /*end report writer */
}


/**********************************************************************
 *$Component                                                          *
 *    LOGICAL lv_setup (lvtv)                                         *
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
 * LVTOOL_V lvtv is a structure containing the following variables    *
 * which are used by this function                                    *  
 *    file_list:                                                      *
 *        The file_list variable is a pointer to a STRING_LIST        *
 *        structure that contains a list of file names.               *
 *    dd_index_name:                                                  *
 *        The dd_index_name variable is the name of the file that     *
 *        contains the Data Dictionary index information.             *
 *    inval_dir                                                       *
 *        A TRUE/FALSE flag which when TRUE enables the use of the    *
 *        invalid file directory option.  This option will exclude    *
 *        from validation any file that is in any directory listed in *
 *        the file specified in inval_dir_name                        *
 *    inval_dir_name                                                  *
 *        A pointer to a string containing the name of the file       *
 *        containing a list of directories to be excluded from the    *
 *        search for file to validate                                 *
 *    inval_file                                                      *
 *        A TRUE/FALSE flag which when TRUE enables the use of the    *
 *        invalid file extension option.  This option will exclude    *
 *        from validation any file having an extension that is listed *
 *        in the file specified in inval_file_name                    *
 *    inval_file_name                                                 *
 *        This is a string for the name of the file containing a list *
 *        of file extensions to be excluded from validation           *
 *    log_file_name                                                   *
 *        name of file to be used to track the names of all files     *
 *        excluded from validation                                    *
 *    log_file_ptr                                                    *
 *        pointer to the file to be used to track the names of all    *
 *        files exclueded from validation                             *
 *    rpt_file_name:                                                  *
 *        The rpt_file_name variable is a character string containing *
 *        the name of the file to be written to.                      *
 *    use_aliases:                                                    *
 *        The use_aliases variable is a TRUE/FALSE flag indicating    *
 *        whether names are dealiased by the label verifier.          *
 *    use_dd:                                                         *
 *        The use_dd variable is a TRUE/FALSE flag indicating whether *
 *        data dictionary validation is enabled.                      *
 *    use_di:                                                         *
 *         Search all subdirectories recursively                      *
 *    use_log_file                                                    *
 *        a TRUE/FALSE flag which if true will enable tracking the    *
 *        names of all files which have been excluded from validation *
 *    use_progress:                                                   *
 *        The use_progress variable is a TRUE/FALSE flag indicating   *
 *        whether to display the number of files processed as a main  *
 *        loop progresses.                                            *
 *    use_terminal:                                                   *
 *        The use_terminal variable a TRUE/FALSE flag indicating      *
 *        whether all error messages are displayed to the screen by   *
 *        the label verifier.                                         *
 *    use_verbose:                                                    *
 *        The use_verbose variable a TRUE/FALSE flag indicating       *
 *        whether all error messages are displayed by the label       *
 *        verifier.                                                   *
 *    input_file_name                                                 *
 *        Name of file containing the list of files to be processed   *
 *    use_input_file                                                  *
 *        The flag specifing that this input_file_name is to be used  *
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
 *    DWS   11-10-97   Combined slv_tool with lv_tool                 *
 *                     Added use_progress and lv_or_slv variables     *
 *    DWS   11-17-97   Added use_expand                               *
 *    DWS   05-13-99   Added no_wrap parameter                        *
 *    DWS   02-16-00   added   section for fin, lvtv->use_input_file, *
 *                     lvtv->input_file_name                          *
 *    MDC   08-08-03   Updated -ivd description to its correct usage  *
 *    MDC   06-16-04   Added new option called "-nol3d", which turns  *
 *                     off checking for level 3 labels before         *
 *                     attempting to validate a file. Default is to   *
 *                     check for the existence of a label before      *
 *                     validating.                                    *
 *    MDC   06-15-05   Added software disclaimer message              *
 *    MDC   07-26-05   Made the routine backwards compatible, where   *
 *                     -d will still accept the index file. A -df flag*
 *                     has been added where it accepts the data       *
 *                     dictionary full file as an input. So the user  *
 *                     now has the option of specifying either the    *
 *                     index or full file in the command-line.        *
 **********************************************************************/




LOGICAL lv_setup (num_args, command_line)

int         num_args;
char        *command_line [];
{
   LOGICAL success = TRUE;
   LOGICAL report_name_found = FALSE;
   LOGICAL log_name_found = FALSE;
   LOGICAL input_name_found = FALSE;
   LOGICAL dd_idx_name_found = FALSE;
   LOGICAL dd_full_name_found = TRUE;
   LOGICAL ivf_name_found = FALSE;  /*invalid file name, list of invalid file types*/
   LOGICAL ivd_name_found = FALSE;  /*invalid dir name, list of invalid directories*/
   LOGICAL use_input_file_found = FALSE;
   int i;  
   char temp [PDS_MAXLINE + 1];
   char err_msg [PDS_MAXLINE + 1];
   STRING_LIST *temp_list = NULL;
   LOGICAL file_flag = FALSE;
   LOGICAL test1     = FALSE;
   LOGICAL test2     = FALSE;
   char *dd_full_name = NULL;

   LOGICAL index_dd_test = FALSE;
   LOGICAL full_dd_test = FALSE;

/** BEGIN **/
   /*---------------------------------------------------------------------*/
   /** initialize all flags to their defaults                            **/
   /*---------------------------------------------------------------------*/
                             
   lvtv.use_dd        = TRUE;
/*   lvtv.use_verbose   = TRUE; */
 /*  lvtv.use_terminal  = FALSE; */
   lvtv.use_aliases   = FALSE;
/*   lvtv.use_progress  = FALSE; */
   lvtv.use_log_file  = FALSE;
   lvtv.dd_index_name = NULL;
   lvtv.log_file_name = NULL;
   lvtv.rpt_file_name = NULL;
   lvtv.file_list     = NULL;
   lvtv.use_di        = FALSE; /*initialize for recursive directory searchinglvtv*/ 
   lvtv.inval_file    = FALSE;
   lvtv.inval_dir     = FALSE;
   lvtv.use_sel_file  = FALSE;
   lvtv.use_input_file = FALSE;
   lvtv.lbl_det = TRUE;    /* 06-16-04 MDC - Initialize to TRUE */



   /*---------------------------------------------------------------------*/
   /** LOOP through the command line arguments                           **/
   /*---------------------------------------------------------------------*/
    for (i=1; i<num_args; i++)
   {
	   strcpy(temp, command_line[i]);
	   util_strip_lead_and_trail(temp, ' ');
	   if(strcmp(temp, "-f") == 0) test1 = TRUE;
	   if(strcmp(temp, "-fin") == 0) test2 = TRUE;
	   if(strcmp(temp, "-d") == 0) index_dd_test = TRUE;
	   if(strcmp(temp, "-df") == 0) full_dd_test = TRUE;
   }
   if(test1 && test2)
   {
	   printf("You may not specify both -f and -fin\n");
	   success = FALSE;
   }
   /* 07-26-05 MDC - Cannot run lvtool with both -d and -df specified */
   else if(full_dd_test && index_dd_test)
   {
	   printf("You may not specify both -d and -df\n");
	   success = FALSE;
   }
   else
   {
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

/*			if (strcmp (temp, "-v") == 0)
				lvtv.use_verbose = TRUE;
			else if (strcmp (temp, "-nv") == 0)
				lvtv.use_verbose = FALSE;         */
/*			else if (strcmp (temp, "-nt") == 0)
				lvtv.use_terminal = FALSE;       */
/*			else if (strcmp (temp, "-t") == 0)
				lvtv.use_terminal = TRUE;           */
			if (strcmp (temp, "-a") == 0)
				lvtv.use_aliases = TRUE;
/*			else if (strcmp (temp, "-p") == 0)
				lvtv.use_progress = TRUE;        */
			else if (strcmp (temp, "-di") == 0)
				lvtv.use_di = TRUE;         /* Turn off recursive directory searching*/
			else if (strcmp (temp, "-na") == 0)
				lvtv.use_aliases = FALSE;
			else if (strcmp (temp, "-nol3d") == 0)
				lvtv.lbl_det = FALSE;
			/*-----------------------------------------------------------------*/
			/** ELSE IF this is the last flag on the line THEN                **/
			/**     We have failed.  All of the remaining flags must be       **/
			/**     followed by file names. While -t must be followed by a    **/
			/**     a string as with -r, -f, -d this string will be a path.   **/
			/*-----------------------------------------------------------------*/

			else if ((file_flag = ((strcmp (temp, "-r") == 0) || 
		         (strcmp (temp, "-f"  ) == 0) || (strcmp (temp, "-d"  ) == 0) ||
				 (strcmp (temp, "-df" ) == 0) || (strcmp (temp, "-sf" ) == 0) ||
				 (strcmp (temp, "-ivf") == 0) || (strcmp (temp, "-ivd") == 0) ||
				 (strcmp (temp, "-lef") == 0)) && i >= (num_args - 1) )  ) 
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

				Malloc_String(lvtv.dd_index_name, String_Size(command_line [i]));
				strcpy (lvtv.dd_index_name, command_line [i]);
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
			/** ELSE IF the next flag is the INVALID FILE LIST flag THEN      **/
			/**    copy the next argument to the inval_file name variable     **/
			/*-----------------------------------------------------------------*/

			else if (strcmp (temp, "-ivf") == 0)
			{
				lvtv.inval_file = TRUE;
				ivf_name_found = TRUE;
				i++;
				Malloc_String(lvtv.inval_file_name, String_Size(command_line [i]));
				strcpy (lvtv.inval_file_name, command_line [i]);
			}
			/*-----------------------------------------------------------------*/
			/** ELSE IF the next flag is the INVALID DIR LIST flag THEN      **/
			/**    copy the next argument to the inval_dir name variable     **/
			/*-----------------------------------------------------------------*/

			else if (strcmp (temp, "-ivd") == 0)
			{
				lvtv.inval_dir = TRUE;
				ivd_name_found = TRUE;
				i++;
				Malloc_String(lvtv.inval_dir_name, String_Size(command_line [i]));
				strcpy (lvtv.inval_dir_name, command_line [i]);
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
				/* The following lines of commented code moved to end of command line parsing*/
				/* In case parsing fails user will not have to sit through building a file */
				/* list that will not be used.                                             */
				do
				{
					i++;
/*					temp_list = slv_get_volume_list (command_line [i], lvtv.use_di);
					if (temp_list != NULL)
		              lvtv.file_list = util_append_string_list (lvtv.file_list, 
					          (char *) temp_list, LIST_TYPE);*/
				} while ((i < num_args - 1) && *(command_line [i + 1]) != '-');

			}
			/*-----------------------------------------------------------------*/
			/** ELSE IF the next flag is the INPUT FILE LIST flag THEN        **/
			/**     LOOP until the next flag is found on the command line     **/
			/**        assume the next argument is a file specification and   **/
			/**        expand it to a list of files                           **/
			/**        add the list to the overall list                       **/
			/**     ENDLOOP                                                   **/
			/*-----------------------------------------------------------------*/
			else if (strcmp (temp, "-fin") == 0)
			{
				input_name_found = TRUE;
				lvtv.use_input_file = TRUE;
				use_input_file_found = TRUE;
				i++;
				Malloc_String(lvtv.input_file_name, String_Size(command_line[i]));
				strcpy (lvtv.input_file_name, command_line [i]);
				/* The following lines of commented code moved to end of command line parsing*/
				/* In case parsing fails user will not have to sit through building a file */
				/* list that will not be used.                                             */
/*				temp_list = new_volume_list(lvtv.input_file_name);
					 if (temp_list != NULL)
					 {
						  lvtv.file_list = util_append_string_list (lvtv.file_list, 
					          (char *) temp_list, LIST_TYPE);
					 }*/
			}
			/*-----------------------------------------------------------------*/
			/** ELSE IF the next flag is the REPORT FILE flag THEN            **/
			/**    copy the next argument to the report file name             **/
			/*-----------------------------------------------------------------*/

			else if (strcmp (temp, "-r") == 0)
			{
				report_name_found = TRUE;
				i++;
				Malloc_String(lvtv.rpt_file_name, String_Size(command_line[i]));
				strcpy (lvtv.rpt_file_name, command_line [i]);
			}
			/*-----------------------------------------------------------------*/
			/** ELSE IF the next flag is the LOG FILE flag THEN               **/
			/**    copy the next argument to the log file name                **/
			/*-----------------------------------------------------------------*/

			else if (strcmp (temp, "-lef") == 0)
			{
				log_name_found = TRUE;
				lvtv.use_log_file = TRUE;
				i++;
				Malloc_String(lvtv.log_file_name, String_Size(command_line[i]));
				strcpy (lvtv.log_file_name, command_line [i]);
			}
			/*-----------------------------------------------------------------*/
			/** ELSE IF the next flag is the SELECT FILE flag THEN            **/
			/**    copy the next argument to the select file name             **/
			/*-----------------------------------------------------------------*/

			else if (strcmp (temp, "-sf") == 0)
			{
				i++;
				Malloc_String(lvtv.sel_file_name, String_Size(command_line[i]));
				strcpy (lvtv.sel_file_name, command_line [i]);
				lvtv.use_sel_file = TRUE;
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
		/*---------------------------------------------------------------------*/
		/** ENDLOOP                                                           **/
		/** IF we are using a data dictionary but no name was given THEN      **/
		/**    assign a default name to the data dictionary index name        **/
		/** ENDIF                                                             **/
		/*---------------------------------------------------------------------*/
/*		if (lvtv.use_dd && !dd_idx_name_found)
		{
			Malloc_String(lvtv.dd_index_name, String_Size(PDS_DEFAULT_DD));
			strcpy (lvtv.dd_index_name, PDS_DEFAULT_DD);
		}
*/
		if (lvtv.use_dd && full_dd_test && !dd_full_name_found)
		{
			Malloc_String(dd_full_name,((int) String_Size(PDS_DEFAULT_DD_FULL)));
			strcpy (dd_full_name, PDS_DEFAULT_DD_FULL);
		}

		else if (lvtv.use_dd && !full_dd_test && !dd_idx_name_found)
		{
			Malloc_String(lvtv.dd_index_name, String_Size(PDS_DEFAULT_DD));
			strcpy (lvtv.dd_index_name, PDS_DEFAULT_DD);
		}

		/*---------------------------------------------------------------------*/
		/** IF no report file name was found and terminal display is not      **/
		/**    enabled THEN print an error                                    **/
		/** ENDIF                                                             **/
		/*---------------------------------------------------------------------*/

/*		if (!report_name_found && !lvtv.use_terminal) */
		if (!report_name_found)
		{
			success = FALSE;         
			err_append_message (ERROR1, "You must provide a report file name");
		}
		/*---------------------------------------------------------------------*/
		/** IF no report file name was found and terminal display is not      **/
		/**    enabled THEN print an error                                    **/
		/** ENDIF                                                             **/
		/*---------------------------------------------------------------------*/

		if (!log_name_found && lvtv.use_log_file)
		{
			success = FALSE;         
			err_append_message (ERROR1, "You must provide a log file name");
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
		/** IF an error occurred THEN                                         **/
		/**    display program information                                    **/
		/** ENDIF                                                             **/
		/*---------------------------------------------------------------------*/
    }

	    /*---------------------------------------------------------------------*/
		/** build file list if input file names are contained in a file       **/
		/*---------------------------------------------------------------------*/

	if(success && (lvtv.use_input_file == TRUE) && (use_input_file_found == TRUE))
	{
		temp_list = new_volume_list(lvtv.input_file_name);
		if (temp_list != NULL)
		{
			  lvtv.file_list = util_append_string_list (lvtv.file_list, 
			  (char *) temp_list, LIST_TYPE);
		}
	}

	    /*---------------------------------------------------------------------*/
		/** build file list if input names are contained on the command line  **/
		/*---------------------------------------------------------------------*/
	if(success && (lvtv.use_input_file != TRUE) && (input_name_found == TRUE))
	{
		for (i = 1; i < num_args; i++)
		{
			file_flag = FALSE;
			strcpy (temp, command_line [i]);
			util_strip_lead_and_trail (temp, ' ');
			if ((file_flag = (strcmp (temp, "-f")) == 0) && i <= (num_args - 1))
			{
				do
				{
					i++;
					temp_list = slv_get_volume_list (command_line [i], lvtv.use_di);
					if (temp_list != NULL)
						  lvtv.file_list = util_append_string_list (lvtv.file_list, 
					          (char *) temp_list, LIST_TYPE);

					lvtv.input_file_name = command_line[i];
				} while ((i < num_args - 1) && *(command_line [i + 1]) != '-');
				i = num_args + 1;
			}
		}
	}

	/* One of these conditions needs to be true if we have gotten this far and success is TRUE*/
	if(success && (((lvtv.use_input_file != TRUE) && (use_input_file_found == TRUE)) ||
		 ((lvtv.use_input_file == TRUE) && (use_input_file_found == TRUE)))) success = FALSE;


	if (!success)
	{
	  long wait_count1;
	  long wait_count2;
	  char ch;
	  printf("\nDisclaimer:\n");
      printf("Copyright 2006-2007, by the California Institute of Technology.\n");
      printf("ALL RIGHTS RESERVED. United States Government Sponsorship acknowledged.\n");
      printf("Any commercial use must be negotiated with the Office of Technology Transfer\n");
      printf("at the California Institute of Technology.\n\n");
      printf("This software is subject to U. S. export control laws and regulations\n");
      printf("(22 C.F.R. 120-130 and 15 C.F.R. 730-774). To the extent that the software\n");
      printf("is subject to U.S. export control laws and regulations, the recipient has\n");
      printf("the responsibility to obtain export licenses or other export authority as\n");
      printf("may be required before exporting such information to foreign countries or\n");
      printf("providing access to foreign nationals.\n\n");
      printf("\nUsage:\n");
      printf("kwvtool -a,na,di,-nol3d\n");
	  printf("        -d dd-index-name\n");
	  printf("        -df dd-full-name\n");
      printf("        -r file name for report file -f file name for label file\n");
      printf("        -ivf file name for excluded file extension list\n");
      printf("        -ivd file name for excluded directory list\n");
      printf("        -sf  file name for excluded keyword list\n");
      printf("        -lef file name excluded file log\n");
      printf("Where:\n");
      printf("   -a:          Enables aliasing\n");
      printf("   -na:         Disables aliasing (default)\n");
	  printf("-nol3d:         Do not check for level 3 labels in files\n");
 /*     printf("   -t:          Enables terminal output\n");
      printf("   -nt:         Disables terminal output (default)\n"); */
 /*     printf("   -v:          Enables verbose error reporting (default)\n");
      printf("   -nv:         Disables verbose error reporting\n");   */
/*	  printf("   -p:          Enables progress reporting\n");
	  printf("   -np:         Disables progress reporting (default)\n"); */
	  printf("   -di:         DOS only, specifies not to search directory and \n");
	  printf("                sub directories.  Default is to search directories\n");
	  printf("                recursively\n");

#ifdef MSDOS_TC      
	  printf("\n\n Press the ENTER KEY to continue now or the program will \n");
      printf(" continue in a moment.\n");
	  for( wait_count1 = 0; wait_count1 < 100000; wait_count1++)
	  {
		  for( wait_count2 = 0; wait_count2 < 10000; wait_count2++);
		  if(_kbhit())
		  {
			  ch = _getch();
			  wait_count1 = 100001;
			  wait_count2 = 100001;
		  }
	  }
#endif	  
	  /* Updated -ivd description to the correct usage. */
      printf("   -d <file>:   Specifies the Data Dictionary index file to use \n");
	  printf("                KWVTOOL will attempt to use pdsdd.idx if -d is  \n");
	  printf("                specified without a file name\n");
	  printf("   -df <file>:  Specifies the Data Dictionary full file to use \n");
	  printf("                KWVTOOL will attempt to use pdsdd.full if -df is \n");
	  printf("                specified without a file name\n");
	  printf("                Either -d or -df must be specified, otherwise KWVTOOL \n");
	  printf("                will not continue processing\n");
      printf("   -r <file>:   Specifies the report file for output\n");
      printf("   -f <file>:   Specifies the label file(s) to be verified\n");
	  printf("                (can be wildcarded)\n");
	  printf("   -ivf <file>: Specifies a file containing file extensions to skip,\n");
	  printf("                default is no file\n");       
	  printf("                Extensions are comma seperated, no blanks, only 200 \n");
	  printf("                characters, case sensitive\n");  
	  printf("   -sf <file>:  Specifies a file containing keywords to be skipped,\n");
	  printf("                Keywords are comma seperated, no blanks, only 200 \n");
	  printf("                characters, case sensitive\n");  
	  printf("   -ivd <file>: Specifies a file containing directories to skip,\n");
	  printf("                default is no file. Each directory is on a \n");       
	  printf("                separate line, names are case sensitive. There can\n");
	  printf("                be up to 30 entries. Each directory name must be less\n");
	  printf("                than 100 characters in length\n");
	  printf("   -lef <file>: Specifies a file for logging the path and name ,\n");
	  printf("                all files skipped\n");       
	  printf("   -fin <file>: Specifies a file containing a list of files to ,\n");
	  printf("                validate.  One file per line, path is assumed to be included\n");       
   }
   /*---------------------------------------------------------------------*/
   /** IF a data dictionary to be used THEN                              **/
   /**    initialize the data dictionary                                 **/
   /** ENDIF                                                             **/
   /*---------------------------------------------------------------------*/

   if (success && lvtv.use_dd)
   {
	   if (full_dd_test)
	   {
          printf("\n");
		  /* 01-06-05 MDC - Create the pdsdd.idx file internally now */
		  lvtv.dd_index_name = make_index(dd_full_name);
		  Lemme_Go(dd_full_name);
		  printf("\n");

		  if(lvtv.dd_index_name == NULL)
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
           if (!(lvtv.use_dd = dd_init (lvtv.dd_index_name)))
		   {
               err_append_message (WARNING, 
					"KWVTOOL will not proceed without a data dictionary");
			   success = FALSE;
		   }
	   }
   }

   if (success && !lvtv.use_dd)
   {
        err_append_message (WARNING, 
			"KWVTOOL will not proceed without a data dictionary");
   }

   return (success);

/** END **/
}


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
   fprintf(rpt_file_pointer,"*        This is the end of the Keyword / Value Tool Report            *\n");
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
 *    FILE *rpt_main_header (lvtv)                                    *
 *$Abstract                                                           *
 *    Generates a report header and writes it to a file.              *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    LABEL_VERIFIER                                                  *
 *    REPORT                                                          *
 *$Inputs                                                             *
 **********************************************************************
 * LVTOOL_V lvtv is a structure containing the following variables    *
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
 *    MDC   12-19-05   Made small change to notify user that kwds will*
 *                     be truncated at 61 chars instead of 60.        *
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

 if (lvtv.rpt_file_name != NULL)
 { 
   date_string = sys_get_ascii_date ();

   /*------------------------------------------------------------*/
   /** IF there is an error opening the file, THEN              **/
   /**    Append error message to the error list                **/
   /*------------------------------------------------------------*/

   if ((file_pointer = fopen(lvtv.rpt_file_name, "w+")) == NULL)
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
      fprintf(file_pointer,"*               Planetary Data System KWVTOOL                          *\n");          
      fprintf(file_pointer,"*               Selected Keywords and Values                           *\n");          
      fprintf(file_pointer,"*                     Extractor Version %s                            *\n",
		                                                                     VERSION);
      fprintf(file_pointer,"*                                                                      *\n");
      fprintf(file_pointer,"*                      %s", date_string);
      fprintf(file_pointer,"                        *\n");
      fprintf(file_pointer,"*                                                                      *\n");
      if (lvtv.dd_index_name != NULL && lvtv.use_dd == TRUE)
      {
         dd_msg = "       Data Dictionary Name:";
		 num_blanks = (70 - (strlen (dd_msg) + 1 + strlen (lvtv.dd_index_name)));
         fprintf (file_pointer, "*%s %-*s*\n", dd_msg,
			 num_blanks + strlen (lvtv.dd_index_name), lvtv.dd_index_name);
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
	  if(lvtv.use_dd)
	  {
	     strcpy(string, lvtv.dd_index_name);
	     ipos = 0;
         while (string[ipos] != '.') ++ipos;
	     string[ipos] = '\0';
	     strcat(string, ".full");

         if ((dd_pointer = fopen(string, "r+")) != NULL)
         { 
            fgets(string, 100, dd_pointer);
            fgets(string, 100, dd_pointer);
		    string1 = strstr(string, "Version");
		    if(string1 != NULL) strcpy(string, string1);
		    else strcpy(string, "Version not found");
		    ipos = 0;
		    while (string[ipos] != '*') ++ipos;
		    string[ipos] = '\0';
            dd_msg = "    Data Dictionary";
		    num_blanks = (70 - (strlen(dd_msg) + 1 + strlen(string)));
            fprintf (file_pointer, "*%s %-*s*\n", dd_msg,
			                        num_blanks + strlen (string), string);
       
            fgets(string, 100, dd_pointer);
		    string1 = strstr(string, "Generated");

		    if(string1 != NULL) strcpy(string, string1);
		    else strcpy(string, "Generated date not found");
		    ipos = 0;
		    while (string[ipos] != '*') ++ipos;
		    string[ipos] = '\0';
            dd_msg = "  Data Dictionary";
		    num_blanks = (70 - (strlen(dd_msg) + 1 + strlen(string)));
            fprintf (file_pointer, "*%s %-*s*\n", dd_msg,
			                         num_blanks + strlen (string), string);
 
		    fclose(dd_pointer);
	     } 
	  }
      fprintf(file_pointer,"*                                                                      *\n");
      if (lvtv.use_dd == TRUE)
         fprintf(file_pointer,"*                  Data Dictionary Validation is ON                    *\n");
      else
         fprintf(file_pointer,"*                  Data Dictionary Validation is OFF                   *\n");
      fprintf(file_pointer,"*                                                                      *\n");
	  fprintf(file_pointer,"* WARNING!                                                             *\n");
	  fprintf(file_pointer,"*----------------------------------------------------------------------*\n");
	  fprintf(file_pointer,"* Keywords will be truncated at 61 characters                          *\n");
	  fprintf(file_pointer,"* values will be truncated at 95 characters                            *\n");
	  fprintf(file_pointer,"* No indication will be given that truncation has occurred             *\n");
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
 *    void rpt_semantic_errors (rpt_file_pointer, LOGICAL)             *
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
       length_1 = strlen(directory);        /* gets length of path*/
   }
   fmask = dir_mask + length_1;         
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

/**********************************************************************
 *$Component                                                          *
 *   STRING_LIST *new_volume_list (lvtv)                              *
 *$Abstract                                                           *
 *    Make a list of file names, from a list in a file.               *
 *$Inputs                                                             *
 *    Structure lvtv                                                  *
 *$Returns                                                            *
 *    file_list:                                                      *
 *        The file_list variable is a pointer to a STRING_LIST        *
 *        structure that contains a list of file names.               *
 *Global structure lvtv used:                                         *
 *    lvtv->input_file_name                                           *
 *$Detailed_Description                                               *
 *    The slv_get_volume_list routine will return a list of files     *
 *                                                                    *
 *                                                                    *
 *$Error_Handling                                                     *
 *   The routine will return NULL if it:                              *
 *    cannot open the file                                            *
 *    the file is empty                                               *
 *$Author_and_Institution                                             *
 *    Dale Schultz/JPL                                                *
 *$Change_History                                                     *
 *          DWS   02-10-00 New code                                   *
 **********************************************************************/
STRING_LIST *new_volume_list (char * input_file_name)

{
  char file_string[256];
  STRING_LIST *file_list = {NULL};

  FILE *input_list;
  input_list = fopen(input_file_name, "r");
  if (input_list == NULL)
  {
	  printf ("Input file %s, did not open\n", input_file_name);
	  return NULL;
  }

  while(fgets(file_string, 256, input_list) != NULL)	
  {
	   util_strip_lead_and_trail(file_string, ' ');
	   util_strip_lead_and_trail(file_string, '\n');
	   util_strip_lead_and_trail(file_string, '\r');
       file_list = util_append_string_list (file_list, file_string, STRING_TYPE);
  }
    
  if (file_list == NULL)
  {
	 printf("\n\n Input file %s was empty\n\n", input_file_name);
	 return NULL;
  }

   return (file_list);
/** END new_volume_list **/
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
/*   MDC 08-20-2003   Made the code more robust in its checking of directories */
/*                    to exclude                                               */
/*   MDC 08-03-2005   Reverted back to old code to prevent program crash when  */
/*                    checking files within a directory                        */
/*******************************************************************************/
int chk_dir_name(invalid_dir, temp_list)
char invalid_dir[][100];
STRING_LIST * temp_list;
{
	int chk_result;
	int count;
	char *dir_ptr;
	char *str_ptr;
/*	char *input_dir_ptr;
	int input_dir_length;
*/
	dir_ptr = temp_list->text;
	chk_result = 1;  /* no match, process file*/
	

	/* 08-03-05 MDC - Take out for now */

	/* 08-20-03 MDC
	   Get the directory path of the input file name so that we can exclude
	   that path from the file name that we are going to check. What this
	   will do is allow kwvtool from distinguishing sub-directories that
	   have the same name as its parent directory (i.e. c:\images\images). If
	   a user wanted to exclude all files in the sub-directory "images", this
	   code would prevent kwvtool from mistakingly exclude all files because
	   "images" happens to also be the name of the parent directory.
	*/
/*	input_dir_ptr = sys_get_path(lvtv.input_file_name);
	input_dir_length = strlen(input_dir_ptr);
	
	dir_ptr = strstr(temp_list->text, input_dir_ptr);
	dir_ptr += input_dir_length;
*/

	/* 08-08-03 MDC - Changed the initialization to 0 instead of 1 to not skip
	   the first directory thats stored in the first element of the 2-D array.
	*/
/*	for(count = 1; count <= 10; count++) */
	for(count = 0; count < NBR_EXCLUD_DIRS; count++)
	{
		if(invalid_dir[count][0] == '\0')
		{
			count = NBR_EXCLUD_DIRS + 1;
		}
		else
		{
			str_ptr = invalid_dir[count];
			if(strstr(dir_ptr, str_ptr) != NULL)
			{
				chk_result = 0; /*Don't do this file!  It is in  */
				count = NBR_EXCLUD_DIRS + 1;		/*a forbidden directory. We are  */
			}					/*done with the search.          */
		}
	}

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

	  if(lvtv.inval_file == TRUE)
	  {
		  inv_file_hdl = fopen(lvtv.inval_file_name, "r");
		  if (inv_file_hdl == NULL)
		  {
			  fprintf (lvtv.rpt_file_ptr, "Invalid file type list did not open, file name: %s \n",
		                                                                lvtv.inval_file_name);
              lvtv.inval_file = FALSE;
			  ret_val = 0;
		  }
		  else
		  {
			  rec_count = fread(invalid_list, 1, 200, inv_file_hdl);
			  if(ferror(inv_file_hdl))
			  {
				  printf ("Invalid file type list had read error, file name: %s \n",
		                                                                lvtv.inval_file_name);
				  fprintf (lvtv.rpt_file_ptr, "Invalid file type list had read error, file name: %s \n",
		                                                                lvtv.inval_file_name);
                  lvtv.inval_file = FALSE;
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
	  if(lvtv.inval_dir == TRUE)
	  {
		  inv_dir_hdl = fopen(lvtv.inval_dir_name, "r");
		  if (inv_dir_hdl == NULL)
		  {
			  printf ("Invalid directory list did not open, file name: %s \n",
		                                                                lvtv.inval_dir_name);

			  fprintf (lvtv.rpt_file_ptr, "Invalid directory list did not open, file name: %s \n",
		                                                                lvtv.inval_dir_name);
              lvtv.inval_dir = FALSE;
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
		                                                                lvtv.inval_dir_name);
						fprintf (lvtv.rpt_file_ptr, 
							"Invalid directory list had read error, file name: %s \n",
		                                                                lvtv.inval_dir_name);
						lvtv.inval_dir = FALSE;
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

	  if(lvtv.use_log_file == TRUE)
	  {
		  lvtv.log_file_ptr = fopen(lvtv.log_file_name, "w+");
		  if (lvtv.log_file_ptr == NULL)
		  {
			  printf ("Log file did not open, file name: %s \n",
		                                                                lvtv.log_file_name);
			  printf ("Processing will continue without a log file\n");
			  fprintf (lvtv.rpt_file_ptr, "Log file did not open, file name: %s \n",
		                                                                lvtv.log_file_name);
			  fprintf (lvtv.rpt_file_ptr, "Processing will continue without a log file\n");
              lvtv.use_log_file = FALSE;
			  ret_val = 0;
		  }
	  }
return ret_val;
}

