/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *NOTE:  12-18-97 This program was last compiled for DOS using         *
 *       Microsoft C/C++ version 4.0. The Wizard for console projects  *
 *       built the project.  The only changes to the settings for the  *
 *       project were to add PDS_TOOLBOX and MSDOS_TC to the compiler  *
 *       macro list.                                                   *
 ***********************************************************************
 *                                                                     *
 * Component                                                           * 
 *    lvtool.c                                                         *
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
 *        rpt_file_header                                              *
 *        rpt_format_message                                           *
 *        rpt_main_footer                                              *
 *        rpt_main_header                                              *
 *        rpt_odl_errors                                               *
 *        rpt_print_tree                                               *
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
 *        expand_label                                                 * 
 *        make_out_file                                                *
 * Authors and Institutions                                            *
 *    Marti D. Demore / J.P.L.                                         *
 *    Kristy L. Marski / J.P.L.                                        *
 *    David P. Bernath / J.P.L.                                        *
 * Version and Date                                                    *
 *    1.1   June 22, 1992                                              *
 *    2.5.j Aug 12, 1999                                               *
 * Change History                                                      *
 *    DPB   09-27-91   File assembled.                                 *
 *    MDD   03-23-92   Changed name from lv_tool to lvtool.            *
 *    MDD   06-22-92   Removed SFDU toolkit interface.  Added calls to *
 *                     new lab_ routines.                              *
 *    DWS   11-05-97   Compiled with Microsoft C++ Version 4.0         *
 *    DWS   11-10-97   Combining slv_tool code with lv_tool            *
 *    DWS   11-10-97   Version will be 2.0                             *
 *    DWS   11-17-97   Added Expand Label for Version 2.1              *
 *    DWS   12-18-97   Fixed the DOS directory walk for volume searches*
 *    DWS   01-07-98   Fixed bug in slv_get_volume_list, version 2.3   *
 *    DWS   01-08-98   FIxed bug in lv_special, version 2.4            *
 *    DWS   07-17-98   Fixed bug in deletion of expanded files         *
 *    DWS   07-21-98   Added list of file extensions to be skipped     *
 *    DWS   12-11-98   Added code to line number error and warning     *
 *                     messages and file search messages               *
 *    DWS   04-12-99   fixed bug in slv_special, * was missing         *
 *    DWS   05-13-99   Added check to VERLABEL.C to handle CATALOG     *
 *                     with pointers instead of KEYWORDS               *
 *    DWS   05-13-99   added alt_rpt_format_messages to handle         *
 *                     writing messages with no wrapping.              *
 *    DWS   08-12-99   Added list of directories to be skipped         *
 *    DWS   08-19-99   made version 2.5.j                              *
 *    DWS   08-20-99   Added structure LVTOOL_V.  It is used to pass   *
 *                     arguments to lv_setup, lv_special, slv_special, *
 *                     and rpt_main_header.  Also added label_validate *
 *                     option to ignore files that do not contain a    *
 *                     label. Version 2.5.k                            *
 *    DWS   02-16-00   Added FORMATION to standard val type choices    *
 *                     in function dd_get_standard_values which is     *
 *                     in ddaccess.c                                   *
 *    DWS   02-16-00   Added feature to allow input files to specified *
 *                     in a file                                       *
 *    DWS    5-25-01   Added code in file LABUTIL.c in routine		   *
 *					   lu_fetch_all_values to remove beginning /n from *
 *					   keyword value befor it is checked to determine  *
 *					   if value is a standard value.                   *
 *    DWS    6-04-01   Added code in LVTOOL.C to prevent missing       *
 *                     FILE_RECORDS keyword message when the           *
 *                     RECORD_TYPE is STREAM                           *
 *    DWS    5-20-02   Changed printed comment in line 2075            *
 *    DWS   05-23-02   modified labtool.c routine                      *
 *                     lt_read_expanded_label to look in LABEL         *
 *                     directory for files pointed to by pointers      *
 *                     Also added get_label_dir to search the          *
 *                     additional directorys.                          *
 *	  MDC	12-16-02   Added a routine, deallocate_string_list_node, to*
 *					   remove a single node from a link list of		   *
 *					   string_list objects.							   *
 *	  MDC	02-18-03   Added the following modifications:			   *
 *						 1) Added code to handle a new flag option,	   *
 *							-max, which allows the user to specify the *
 *							maximum number of errors that lvtool will  *
 *							output to a report file.				   *
 *						 2) Added a -nwarn flag to allow a user to	   *
 *							turn off warning messages.				   *
 *						 3) Added a -ninfo flag to allow a user to turn*
 *							off informational messages.				   *
 *    MDC   08-08-03   Modified chk_dir_name and check_exclusion_opts  *
 *                     routines. See notes.                            *
 *    MDC   08-20-03   Modified chk_dir_name routine. See notes.       *
 *    DWS   11-13-03   Reompiled for use with NULL followed by unit ID *
 *                     value.                                          *
 *    DWS   01-16-04   Modified check_exclusion_opts to take input for *
 *                     file extensions and directories to skip and     *
 *                     change it so that it is saved in both upper     *
 *                     and lower case.  Also changed the file extension*
 *                     list to be 500 characters long.                 *
 *    MDC   02-25-04   Modified the lv_special routine. See notes in   *
 *                     function. Also added a routine called           *
 *                     check_filename to check a filename for          *
 *                     lowercase letters.                              *
 *    MDC   05-04-04   Modified make_out_file routine. See notes.      *
 *    MDC   11-12-04   Made a bug fix to make_out_file routine. See    *
 *                     notes.                                          *
 *    MDC   12-16-04   Modified rpt_file_header routine. See notes.    *
 *    MDC   01-06-05   Modified lv_setup routine. Created new function *
 *                     called make_index. Modified rpt_main_header.    *
 *                     Modified lv_special routine.                    *
 *    MDC   05-11-05   Modified chk_dir_name. See notes.               *
 *    MDC   05-25-05   Modified lv_special and slv_special routines.   *
 *    MDC   05-26-05   Modified lv_special routines.                   *
 *    MDC   06-07-05   Modified rpt_file_messages. See notes.          *
 *    MDC   06-13-05   Modified lv_setup routine. See notes.           *
 *    MDC   07-26-05   Modified lv_setup routine. See notes.           *
 *    MDC   01-26-06   Added new routine for non-windows systems called*
 *                     interpret_unix_dir_symbol                       *
 *                     Modified slv_get_volume_list routine. See notes *
 *    MDC   01-31-06   Minor change to slv_special routine. See notes  *
 *    MDC   02-22-06   Modified lv_setup routine. See notes.           *
 *    MDC   04-17-06   Modified main routine. See notes.               *
 *                                                                     *
 ***********************************************************************/
 

#ifdef MSDOS_TC
#include <conio.h>
#endif


#include "pdsglob.h"
#include "ddaccess.h"
#include "errordef.h"
#include "fiodef.h"
#include "label.h"
#include "labutil.h"
#include "labtool.h"
#include "sysdef.h"
#include "utildef.h"
#include "verlabel.h"
#include "lvtool.h"
#include "convert.h"
#include "pdsdef.h"

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
 *    DWS   09-21-99   Created lvtool.h                               *
 *    DWS   02-01-00   Modified Malloc in lu_format_units             *
 *    MDC   04-17-06   Do not call lab_setup routine if we're running *
 *                     in a windows environment because we do not need*
 *                     to create a scratch file.                      *
 **********************************************************************/
struct lvtool_v    lvtv;
char *input_file;

void main (argc, argv)                                       /*10-8-97*/
int argc;
char *argv [];
{
                                   /*directories recursively*/
   LOGICAL     command_line_ok;
   LOGICAL dd_idx_removed = FALSE;
   lvtv.start_path = NULL;

	
/** BEGIN **/

   /*-----------------------------------------------------------------------*/
   /** Setup the files needed by the toolbox                               **/
   /** Setup the verifier/parse the command line                           **/
   /*-----------------------------------------------------------------------*/

   printf ("\nThe PDS Label Verifier - Version %s\n\n", LVTOOL_VERSION);

	
   /* 04-17-06 MDC - Do not call the lab_setup routine if we are running on a
        Windows environment. It doesn't need the scratch file.
   */
#ifndef MSDOS_TC
   lab_setup ();
#endif

   command_line_ok = lv_setup (argc, argv);
  

   pds_display = lvtv.use_terminal;
   pds_verbose = lvtv.use_verbose;
   pds_warning = lvtv.warnings;			/* 12-26-02 MDC */
   pds_info = lvtv.info; /* 02-13-03 MDC */
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
      lvtv.rpt_file_ptr = rpt_main_header ();

     if (lvtv.lv_or_slv == 's') 
      {
   	      slv_special();
      }
      else if (lvtv.lv_or_slv == 'l')
      {
	      lv_special();
      }
   }
   /*-----------------------------------------------------------------------*/
   /** ENDIF                                                               **/
   /** clean up                                                            **/
   /*-----------------------------------------------------------------------*/
   if(command_line_ok && (lvtv.use_log_file == TRUE)) fclose(lvtv.log_file_ptr);
   
   /* 10-25-04 MDC - Delete the index file upon completion of the validation */
 /*  if(lvtv.dd_index_name != NULL)
   	   remove(lvtv.dd_index_name);
*/
   Lemme_Go(lvtv.dd_index_name);
   Lemme_Go(lvtv.rpt_file_name);
   if(lvtv.inval_file) Lemme_Go(lvtv.inval_file_name);
   if(lvtv.inval_dir)  Lemme_Go(lvtv.inval_dir_name);
   util_deallocate_string_list (lvtv.file_list);
/*   free(lvtv);*/
   lab_exit ();

   return;
/** END **/
}

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
/*    alias_message_ptr                                                */
/*        This is pointer to a list of messages                        */
/*    file_list:                                                       */
/*        The file_list variable is a pointer to a STRING_LIST         */
/*        structure that contains a list of file names.                */
/*    inval_dir                                                        */
/*        A TRUE/FALSE flag which when TRUE enables the use of the     */
/*        invalid file directory option.  This option will exclude     */
/*        from validation any file that is in any directory listed in  */
/*        the file specified in inval_dir_name                         */
/*    inval_dir_name                                                   */
/*        A pointer to a string containing the name of the file        */
/*        containing a list of directories to be excluded from the     */
/*        search for file to validate                                  */
/*    inval_file                                                       */
/*        A TRUE/FALSE flag which when TRUE enables the use of the     */
/*        invalid file extension option.  This option will exclude     */
/*        from validation any file having an extension that is listed  */
/*        in the file specified in inval_file_name                     */
/*    inval_file_name                                                  */
/*        This is a string for the name of the file containing a list  */
/*        of file extensions to be excluded from validation            */
/*    lbl_det                                                          */
/*        A TRUE/FALSE flag which when TRUE enables the exclusion of   */
/*        files that do not contain labels from validation             */
/*    no_wrap:                                                         */
/*         = TRUE do not wrap error messages in report                 */
/*    odl_root:                                                        */
/*        Points to the root of an ODL tree.                           */
/*    rpt_file_ptr:                                                    */
/*        The rpt_file_ptr variable is the pointer to the file to be   */
/*        written to.                                                  */
/*    use_aliases:                                                     */
/*        The use_aliases variable is a TRUE/FALSE flag indicating     */
/*        whether names are dealiased by the label verifier.           */
/*    use_dd:                                                          */
/*        The use_dd variable is a TRUE/FALSE flag indicating whether  */
/*        data dictionary validation is enabled.                       */
/*    use_expand:                                                      */
/*        The use_expand variable is a TRUE/FALSE flag indicating      */
/*        whether to expand structure pointers.                        */
/*    use_terminal:                                                    */
/*        The use_terminal variable a TRUE/FALSE flag indicating       */
/*        whether all error messages are displayed to the screen by    */
/*        the label verifier.                                          */
/*    x_lbl_rpt:                                                       */
/*         =0 do validation and produce external file verification     */
/*           report.                                                   */
/*         =1 do not validate but do produce external file             */
/*            verification report.                                     */
/*         =2 do validate but do not produce external file             */
/*            verification report.                                     */
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
/*   rpt_file_header ()                                                */
/*   lab_clear_messages ()                                             */
/*   lab_read_label ()                                                 */
/*   dd_unalias ()                                                     */
/*   rpt_print_tree ()                                                 */
/*   err_deallocate_list ()                                            */
/*   rpt_odl_errors ()                                                 */
/*   lab_clear_messages ()                                             */
/*   ver_semantics ()                                                  */
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
/*   DWS  01-08-98  Added second and third line to IF.  Use of non     */
/*                  existant pointer caused crash                      */
/*   MDC  02-24-04  Check the file list for excluded directories and   */
/*                  extensions before going into the for loop for      */
/*                  label validation.                                  */
/*   MDC  01-03-05  Check to see if the user set the option to save    */
/*                  an expanded file that was made before deleting it  */
/*   MDC  05-25-05  Made change to not check for an END statement in a */
/*                  format file                                        */
/*   MDC  05-26-05  Made change to correctly look for pointers to files*/
/*                  after a file has been expanded. This especially    */
/*                  fixes a bug where a file being validated from a CD */
/*                  is expanded and placed onto the hard drive         */
/*   MDC  02-22-06  Modified routine to print out the hierarchy tree   */
/*                  regardless of whether or not the -a flag was set.  */
/*                  Previous versions would only print the hierarchy   */
/*                  out only when -a was set.                          */
/***********************************************************************/
void lv_special()

{
	  int  status;
      STRING_LIST *temp_list;
/*	  STRING_LIST *next; */
	  char invalid_list[INV_EXT_LIST_LENGTH + 4]; 
/*	  char invalid_list[3 * INV_EXT_LIST_LENGTH + 4]; 
	  char invalid_dir[3 * NBR_EXCLUD_DIRS][100]; */
      char invalid_dir[NBR_EXCLUD_DIRS][100];
	  int  ok_to_go;
	  int  ok_to_go_1;
	  int  ok_to_go_2;
	  int  make_file_ret = 0;
      long num_blanks = 0;
      long i = 1;
	  
	  char *orig_dir = NULL;

	  int  local_status = 0;
      char *temp_str = {NULL};
	  
	  
	  
	  /*--------------------------------------------------------------------*/
	  /**Get the data for excluding files: with undesirable extensions,    **/
	  /**or in undesirable directores.  Also set up file exclusion logging.**/
	  /**These services will only be performed if they were selected by    **/
	  /**the user.                                                         **/
      /*--------------------------------------------------------------------*/
	  for(i = 0; i < NBR_EXCLUD_DIRS; i++) invalid_dir[i][0] = '\0';
      check_exclusion_opts(invalid_list, invalid_dir);
      /*--------------------------------------------------------------------*/
      /** IF the report file was opened THEN                               **/
      /*--------------------------------------------------------------------*/
/*	  if (success == 1)
	  {*/
         if (lvtv.use_terminal || lvtv.rpt_file_ptr != NULL)
         {
         /*-----------------------------------------------------------------*/
         /** write any messages so far to the report and clear them        **/
         /** LOOP through all the files to be verified                     **/
         /*-----------------------------------------------------------------*/

            err_write_to_file (lvtv.rpt_file_ptr, TRUE);
            lab_clear_messages ();
			
			/* 02-11-04 MDC - Check the list of files created by lvtool before
			   validating it. Note that these checks are made if the user set 
			   these options.
			*/

		/*------------------------------------------------------------------*/
		/** 02-20-03 MDC												   **/
		/** Added an extra conditional statement to the for loop. If the   **/
		/** maximum number of errors allowed has been exceeded, then stop  **/
		/** label processing. Print out the footer for the file and exit   **/
		/** the program.												   **/
		/*------------------------------------------------------------------*/
/*          for (temp_list = lvtv.file_list; temp_list != NULL;
		                                     temp_list = temp_list -> next) */
			for (temp_list = lvtv.file_list; 
					((temp_list != NULL) && (total_error_count <= lvtool_max_errors));
					temp_list = temp_list -> next)
			{
				/* 05-25-05 MDC - Set the flag to TRUE if we've just validated a format
				   file.
			    */
				if(pds_watch_ends == FALSE)
					pds_watch_ends = TRUE;
                                                        /*if there is an invalid */
			   if(lvtv.inval_file  == FALSE)            /*file type list, make   */
			   {                                         /*sure the extension of  */
				                                         /*file is acceptable     */
			  	   ok_to_go = 1;                         /* process them all      */
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
			   {                                          /*directory list, make   */
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
               ok_to_go_2 = 1;                               /*if we are to verify that*/
			                                                 /*a file contains a label */
			                                                 /*befor processing it, go */
			                                                 /*do it                   */
			   if((lvtv.lbl_det) && (ok_to_go == 1) && (ok_to_go_1  == 1))
			   {
				   ok_to_go_2 = validate_label (temp_list->text);
			       if((ok_to_go_2 == 0) && (lvtv.use_log_file))
			       {
                       fprintf(lvtv.log_file_ptr, 
						"File Excluded, File Has No Label, File: %s\n", temp_list->text);
				   }
			   }
			   if ((ok_to_go) && (ok_to_go_1) && (ok_to_go_2))
			   {
                   printf("Validating %s \n", temp_list -> text);
				   /* 05-25-05 MDC - If we are validating a format file, we need to set the flag
				      to not check for an END statement at the end of the file.
				   */
				   if( (strstr(temp_list -> text, ".FMT") != NULL) || (strstr(temp_list -> text, ".fmt") != NULL) )
					   pds_watch_ends = FALSE;

				   if(lvtv.use_expand)
				   {
					   /* 05-26-05 MDC - Save the location of where the file is originally at. If we are validating
					      files in a DVD, we need to know where it is originally at before making an expanded file.
					   */
					   orig_dir = sys_get_path(temp_list->text);

					   make_file_ret = make_out_file(temp_list, lvtv.rpt_file_ptr);
				   }
				   if (pds_display)
				   {
					   printf ("\n\n----------------------------------------------------------\n");
					   printf (" Validating %s:\n\n", temp_list -> text);
				   }

				/*-------------------------------------------------------------*/
				/** print the report header for the current file              **/
				/*-------------------------------------------------------------*/

				   rpt_file_header (lvtv.rpt_file_ptr, temp_list -> text);

				   if((lvtv.x_lbl_rpt == 0) || (lvtv.x_lbl_rpt == 2))
				   {
					   /* 05-26-05 MDC - If we've expanded the file and no start path was defined by the user in
					      the command line, then use the original directory path as the directory to start the 
						  search from since it may have changed locations because of the creation of an expanded file.
						  
						  i.e. File in a DVD was expanded and was created in a hard drive.
					   */
					   if ( (lvtv.use_expand == TRUE) && (lvtv.start_path == NULL) )
					   {
						   search_for_pointers(temp_list->text, lvtv.rpt_file_ptr, orig_dir);
					   }
					   else
					   {
							search_for_pointers(temp_list -> text, lvtv.rpt_file_ptr,
									                      lvtv.start_path);/*01-12-98*/
					   }
				   }
				   if (lvtv.x_lbl_rpt != 2) 
				   {
				/*-------------------------------------------------------------*/
				/** clear error messages                                      **/
				/** read the label                                            **/
				/** dealias the label, if needed                              **/
				/** print the object heirarchy to the report                  **/
				/** delete all alias messages                                 **/
				/** print the syntax errors to the report                     **/
				/*-------------------------------------------------------------*/		

					   lab_clear_messages ();

					   track_odl_errors = TRUE;

					   lvtv.odl_root = lab_read_label (temp_list -> text, &status);
						
					   track_odl_errors = FALSE;

                /*-------------------------------------------------------------*/
			    /*Find out if record_type is stream.  If it is we will want to */
				/*set a flag to stop error message about missing FILE_RECORDS  */
				/*keyword.                                 DWS 5-31-01         */
                /*-------------------------------------------------------------*/

					   temp_str = lab_get_value (lvtv.odl_root, NULL, NULL, 1, "RECORD_TYPE",
                                   1, 1, FALSE, &local_status);
					   if(temp_str == NULL)  /*wasn't one in first (root) object */
					   {					 /*check the second object. If none  */
											 /*here forget it.                   */
								temp_str = lab_get_value (lvtv.odl_root, NULL, NULL, 2, "RECORD_TYPE",
								       1, 1, FALSE, &local_status);
					   
						   if(temp_str != NULL)/*got something, check the value for STREAM*/
						   {                   /*and set flag                             */
							   if(strcmp(temp_str, "STREAM") == 0)  lvtv.stream = TRUE;
							   else                           lvtv.stream = FALSE;
						   }
					   }
					   else						/*got something, check the value for STREAM*/
					   {						/*and set flag                             */
						   if(strcmp(temp_str, "STREAM") == 0)  lvtv.stream = TRUE;
						   else                           lvtv.stream = FALSE;
					   }
 
               /*-------------------------------------------------------------*/
			   /* done with STREAM check									  */
               /*-------------------------------------------------------------*/
					   
					   if (lvtv.use_aliases)
					   {                                                   /*01-08-98*/
						   lvtv.alias_message_ptr = dd_unalias (lvtv.odl_root);
   /*
						   rpt_print_tree (lvtv.rpt_file_ptr, lvtv.odl_root, lvtv.alias_message_ptr);
						   err_deallocate_list (lvtv.alias_message_ptr);
   */
					   }
					   /* 02-22-06 MDC - Moved rpt_print_tree call out of the if condition so that the
					         hierarchy of the objects found in a label will be printed out to the
							 report whether or not the -a flag was set.
					   */
					   rpt_print_tree (lvtv.rpt_file_ptr, lvtv.odl_root, lvtv.alias_message_ptr);
					   err_deallocate_list (lvtv.alias_message_ptr);
					   rpt_odl_errors (lvtv.rpt_file_ptr, lvtv.no_wrap);

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
						   ver_semantics (lvtv.odl_root);  
						   rpt_semantic_errors (lvtv.rpt_file_ptr, lvtv.no_wrap); 
						   
						   lab_clear_messages ();
					   }
					/*-------------------------------------------------------------*/
					/** ENDIF                                                     **/
					/** clean up the label in memory                              **/
					/*-------------------------------------------------------------*/

					   lab_remove_label (lvtv.odl_root, &status);
				   } 
				   if(lvtv.use_expand && (make_file_ret == 1))
				   {
/*#ifndef MSDOS_TC*/
					   /* 01-03-05 MDC - Do not delete the expanded file if the 
					      option to save them are set.
					   */
					   if(!lvtv.save_expanded_files)
							remove(temp_list -> text);  /*delete expanded file*/
/*#else
					   DeleteFile(temp_list -> text);*/  /*delete expanded file*/
/*#endif*/
					   make_file_ret = 0;
				   } 
			/*-----------------------------------------------------------------*/
			/** ENDLOOP                                                       **/
			/*-----------------------------------------------------------------*/
			   }
		   }
		}
		rpt_main_footer (lvtv.rpt_file_ptr);
	
      /*--------------------------------------------------------------------*/
      /** ENDIF                                                            **/
      /*--------------------------------------------------------------------*/
}

/***********************************************************************/
/*$Component                                                           */
/*  VOID slv_special(use_terminal, rpt_file_ptr, file_list,            */
/*                       use_aliases, odl_root, use_dd, use_progress)  */
/*$Abstract                                                            */
/*  This routine handles slv tool specific tasks (error summaries)     */
/*$Keywords                                                            */
/*    LV_TOOL                                                          */
/*    SLV_TOOL                                                         */
/*    TOOL_MAIN                                                        */
/*    LABEL_VERIFIER                                                   */
/*$Inputs                                                              */
/***********************************************************************/
/* These members of lvtv are used as input to this function:           */
/*    file_list:                                                       */
/*        The file_list variable is a pointer to a STRING_LIST         */
/*        structure that contains a list of file names.                */
/*    inval_dir                                                        */
/*        A TRUE/FALSE flag which when TRUE enables the use of the     */
/*        invalid file directory option.  This option will exclude     */
/*        from validation any file that is in any directory listed in  */
/*        the file specified in inval_dir_name                         */
/*    inval_dir_name                                                   */
/*        A pointer to a string containing the name of the file        */
/*        containing a list of directories to be excluded from the     */
/*        search for file to validate                                  */
/*    inval_file                                                       */
/*        A TRUE/FALSE flag which when TRUE enables the use of the     */
/*        invalid file extension option.  This option will exclude     */
/*        from validation any file having an extension that is listed  */
/*        in the file specified in inval_file_name                     */
/*    inval_file_name                                                  */
/*        This is a string for the name of the file containing a list  */
/*        of file extensions to be excluded from validation            */
/*    lbl_det                                                          */
/*        A TRUE/FALSE flag which when TRUE enables the exclusion of   */
/*        files that do not contain labels from validation             */
/*    odl_root:                                                        */
/*        Points to the root of an ODL tree.                           */
/*    rpt_file_ptr:                                                    */
/*        The rpt_file_ptr variable is the pointer to the file to be   */
/*        written to.                                                  */
/*    use_aliases:                                                     */
/*        The use_aliases variable is a TRUE/FALSE flag indicating     */
/*        whether names are dealiased by the label verifier.           */
/*    use_dd:                                                          */
/*        The use_dd variable is a TRUE/FALSE flag indicating whether  */
/*        data dictionary validation is enabled.                       */
/*    use_expand:                                                      */
/*        The use_expand variable is a TRUE/FALSE flag indicating      */
/*        whether to expand structure pointers.                        */
/*    use_progress:                                                    */
/*        Display the number of files processed while looping.         */
/*    use_terminal:                                                    */
/*        The use_terminal variable a TRUE/FALSE flag indicating       */
/*        whether all error messages are displayed to the screen by    */
/*        the label verifier.                                          */
/*$Outputs                                                             */
/*    None                                                             */
/*$Detailed_Description                                                */
/*    The slv special function is a continuation of the driver for the */
/*    label verifier.  It calls routines to summarize all errors that  */
/*    were detected and then call the routines that print the report   */
/*    files created.                                                   */
/*$External_References                                                 */
/*    Item                       Shared Data              Access       */
/*    ---------------------------------------------------------------  */
/*    err_write_to_file ()                                             */
/*    lab_clear_messages ()                                            */
/*    lab_read_label ()                                                */
/*    slv_odl_messages ()                                              */
/*    dd_unalias ()                                                    */
/*    slv_alias_messages ()                                            */
/*    ver_semantics ()                                                 */
/*    slv_semantic_errors ()                                           */
/*    lab_remove_label ()                                              */
/*    slv_main_footer ()                                               */
/*$Author_and_Institution                                              */
/*    Marti D. Demore                                                  */
/*$Version_and_Date                                                    */
/*    1.2 November 12, 1997                                            */
/*$Change_history                                                      */
/*   DWS  11-12-97  Original seperation from SLV TOOL main             */
/*   DWS  11-17-97  Added expand label capability                      */ 
/*   MDC  01-31-06  Minor fix to de-allocate the message list first    */
/*                  prior to checking for ODL errors if the -e flag    */
/*                  was set to avoid recording duplicate messages.     */
/***********************************************************************/
void slv_special()

{

  STRING_LIST *temp_list;
/*  STRING_LIST *next;*/
  int    status;
  int    make_file_ret = 0;
  long   num_files = 0;
  long   this_file_total = 0;
  char   invalid_list[3 * INV_EXT_LIST_LENGTH + 4];
  char   invalid_dir[3 * NBR_EXCLUD_DIRS][100];
  int    ok_to_go;
  int    ok_to_go_1;
  int    ok_to_go_2;
  long   num_blanks = 0;
  long   i = 1;   

   /*--------------------------------------------------------------------*/
   /**Get the data for excluding files: with undesirable extensions,    **/
   /**or in undesirable directores.  Also set up file exclusion logging.**/
   /**These services will only be performed if they were selected by    **/
   /**the user.                                                         **/
   /*--------------------------------------------------------------------*/
      check_exclusion_opts(invalid_list, invalid_dir);
      if (lvtv.use_terminal || lvtv.rpt_file_ptr != NULL)
         {
         /*-----------------------------------------------------------------*/
         /** write any messages so far to the report and clear them        **/
         /** LOOP through all the files to be verified                     **/
         /*-----------------------------------------------------------------*/

            err_write_to_file (lvtv.rpt_file_ptr, TRUE);
            lab_clear_messages ();

			/* 02-11-04 MDC - Check the list of files created by lvtool before
			   validating it. Note that these checks are made if the user set 
			   these options.
			*/

            for (temp_list = lvtv.file_list; ((temp_list != NULL) && (total_error_count <= lvtool_max_errors)); 
								num_files++, temp_list = temp_list -> next)
            {
				/* 05-25-05 MDC - Set the flag to TRUE if we've just validated a format
				   file.
			    */
				if(pds_watch_ends == FALSE)
					pds_watch_ends = TRUE;

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
			   {
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
			   ok_to_go_2 = 1;
			   if((lvtv.lbl_det) && (ok_to_go == 1) && (ok_to_go_1  == 1))
			   {
				   ok_to_go_2 = validate_label (temp_list->text);
			       if((ok_to_go_2 == 0) && (lvtv.use_log_file))
			       {
				   fprintf(lvtv.log_file_ptr, 
					   "File Excluded, File Has No Label, File: %s\n", temp_list->text);
				   }
			   }
			   if ((ok_to_go) && (ok_to_go_1) && (ok_to_go_2))
			   {
				   /* 05-25-05 MDC - If we are validating a format file, we need to set the flag
				      to not check for an END statement at the end of the file.
				   */
				   if( (strstr(temp_list -> text, ".FMT") != NULL) || (strstr(temp_list -> text, ".fmt") != NULL) )
					   pds_watch_ends = FALSE;

                   if(lvtv.use_expand) make_file_ret = make_out_file(temp_list, lvtv.rpt_file_ptr);
               /*-------------------------------------------------------------*/
               /** reset all error counters                                  **/
               /*-------------------------------------------------------------*/

                  this_file_total = 0;
                  slv_file_odl_warns = 0;
                  slv_file_odl_errors = 0;
                  slv_file_aliases = 0;
                  slv_file_dd_warns = 0;
                  slv_file_dd_errors = 0;

             /*-------------------------------------------------------------*/
             /** read the label                                            **/
             /** count and delete all syntax errors                        **/
             /** dealias the label, if needed                              **/
             /** count and delete all alias messages                       **/
             /** print the syntax errors to the report                     **/
             /*-------------------------------------------------------------*/
				  /* 01-31-06 MDC - If the -e flag was set, then the ODL errors have already been
				        recorded earlier, so it is best to clear out the message list first so we
						don't count duplicate ODL messages.
				  */
				  if(lvtv.use_expand)
                      err_deallocate_list(pds_message_list);

                  lvtv.odl_root = lab_read_label (temp_list -> text, &status);
                  slv_odl_messages ();
             
                  if (lvtv.use_aliases && lvtv.use_dd)
                  {
                     dd_unalias (lvtv.odl_root);
                     slv_alias_messages ();
                  }

             /*-------------------------------------------------------------*/
             /** IF a label was read and the data dictionary option is ON  **/
             /**    THEN                                                   **/
             /**    verify the label semantics and count the errors        **/
             /** ENDIF                                                     **/
             /*-------------------------------------------------------------*/

                  if (lvtv.use_dd && lvtv.odl_root != NULL)
                  {
                     ver_semantics (lvtv.odl_root);
                     slv_semantic_errors ();
                  }
   
             /*-------------------------------------------------------------*/
             /** Add all error counts into the total                       **/
             /** Print progress report if it's enabled                     **/
             /*-------------------------------------------------------------*/

                  this_file_total = slv_file_odl_warns + slv_file_odl_errors +
                               slv_file_dd_warns + slv_file_dd_errors + 
                               slv_file_aliases;

                  if (lvtv.use_progress && num_files && (num_files % 20 == 0))
                       printf ("---> %ld files complete.\n", num_files);

             /*-------------------------------------------------------------*/
             /** IF the current file had errors THEN                       **/
             /*-------------------------------------------------------------*/

                  if (this_file_total != 0)
                  {
                /*-------------------------------------------------------------*/
                /** Print information to the terminal if it's enabled         **/
                /*-------------------------------------------------------------*/

                     if (lvtv.use_terminal)
                     {
                        printf ("File: %s\n", temp_list -> text);
                        printf ("      ODL Errors: %3ld  Warnings: %3ld", 
                               slv_file_odl_errors, slv_file_odl_warns);

                        if (lvtv.use_aliases && lvtv.use_dd)
                               printf ("      Aliases: %3ld", slv_file_aliases);

                        if (lvtv.use_dd)
                             printf ("   DD Errors: %3ld  Warnings: %3ld\n", 
                                  slv_file_dd_errors, slv_file_dd_warns);
                        else
                           printf ("\n");
 	                  }		

                /*-------------------------------------------------------------*/
                /** Print information to the report if it's enabled           **/
                /*-------------------------------------------------------------*/

                     if (lvtv.rpt_file_ptr != NULL)
                     {
                        fprintf (lvtv.rpt_file_ptr, "File: %s\n", temp_list -> text);

                        fprintf (lvtv.rpt_file_ptr, "      ODL Errors: %3ld  Warnings: %3ld", 
                               slv_file_odl_errors, slv_file_odl_warns);

                        if (lvtv.use_aliases && lvtv.use_dd)
                            fprintf (lvtv.rpt_file_ptr, "      Aliases: %3ld", slv_file_aliases);

                        if (lvtv.use_dd)
                            fprintf (lvtv.rpt_file_ptr, "   DD Errors: %3ld  Warnings: %3ld\n", 
                               slv_file_dd_errors, slv_file_dd_warns);
                        else
                           fprintf (lvtv.rpt_file_ptr, "\n");
 	                 }

                /*-------------------------------------------------------------*/
                /** Add this files totals into the overall total.             **/
                /*-------------------------------------------------------------*/

                     slv_dd_errors    += slv_file_dd_errors;
                     slv_dd_warnings  += slv_file_dd_warns;
                     slv_odl_errors   += slv_file_odl_errors;
                     slv_odl_warnings += slv_file_odl_warns;
                     slv_all_aliases  += slv_file_aliases;
				  }

             /*-------------------------------------------------------------*/
             /** ENDIF                                                     **/
             /** clean up the label in memory                              **/
             /*-------------------------------------------------------------*/

 		         if(lvtv.use_expand && make_file_ret)
		         {
/*#ifndef MSDOS_TC*/
					 /* 01-03-05 MDC - Do not delete the expanded file if the 
					      option to save them are set.
					 */
					   if(!lvtv.save_expanded_files)
							remove(temp_list -> text);  /*delete expanded file*/
/*#else
		            DeleteFile(temp_list -> text);*/  /*delete expanded file*/
/*#endif*/
			        make_file_ret = 0;
	             }  
               lab_remove_label (lvtv.odl_root, &status);
			  }
         /*-----------------------------------------------------------------*/
         /** ENDLOOP                                                       **/
         /** Close the report file and print final progress report         **/
         /*-----------------------------------------------------------------*/
            }
         }
         slv_main_footer (lvtv.rpt_file_ptr, lvtv.use_terminal);
         if (lvtv.use_progress) 
            printf ("---> All files complete.\n");

      /*--------------------------------------------------------------------*/
      /** ENDIF                                                            **/
      /*--------------------------------------------------------------------*/
/*   }*/
}




/*******************************************************************

FUNCTION: void check_filename();

DESCRIPTION: Routine to check if a file name is in lowercase letters.
             PDS standards require file names to be in all uppercase 
			 letters. 

*********************************************************************/

void check_filename()
{
	STRING_LIST *temp_ptr = NULL;
	STRING_LIST *next = NULL;
	char *ptr = NULL;
	int display_msg = 1;
	int lc_found = 0;
	int index = 0;
	int str_length = 0;

	for(temp_ptr = lvtv.file_list; temp_ptr != NULL;)
	{
		lc_found = 0;

		/* If we are using a PC, locate the first '\' (after the drive letter) */
#ifdef MSDOS_TC
		ptr = strstr(temp_ptr->text, "\\");
#else
		/* All other systems, look for '/' */
		ptr = strstr(temp_ptr->text, "/");
#endif
		++ptr;
		str_length = (int) strlen(ptr);
		for(index = 0; ( (lc_found == 0) && (index <= str_length) ); ptr++, index++)
		{
			/* If a lowercase letter is found... */
            if(islower(*ptr))
			{
				/* Record the error message only once */
				if(display_msg)
				{
					err_append_message(CONTINUE, "\n\n");
                    err_append_message(ERROR1, 
						"  The following files will be excluded from processing because PDS standards require file/directory names to be in all upper case");
					display_msg = 0;
				}
				/* Record the file name onto the message list */
				err_append_message(CONTINUE, temp_ptr->text);
				lc_found = 1;

				/* If user selected the logging option, record the file name and let the 
				   user know why file is being excluded */
				if(lvtv.use_log_file)
				{
                    fprintf(lvtv.log_file_ptr,
							"File Excluded, File/Directory Name Must Be All Uppercase: %s\n",
							temp_ptr->text);
				}
			}
		}	
		
		if(lc_found == 1)
		{
            next = temp_ptr->next;
			/* Remove the file from the list of files lvtool has built */
			deallocate_string_list_node(temp_ptr, &(lvtv.file_list));
			temp_ptr = next;
		}
		else
			temp_ptr = temp_ptr->next;
	}
    
	/* Right this message onto the report file */
	err_write_to_file(lvtv.rpt_file_ptr, TRUE);
	lab_clear_messages();
	return;
	
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
 *    lbl_det                                                         *
 *        A TRUE/FALSE flag which when TRUE enables the exclusion of  *
 *        files that do not contain labels from validation            *
 *    log_file_name                                                   *
 *        name of file to be used to track the names of all files     *
 *        excluded from validation                                    *
 *    log_file_ptr                                                    *
 *        pointer to the file to be used to track the names of all    *
 *        files exclueded from validation                             *
 *    lv_or_slv:                                                      *
 *        The lv_or_slv variable is a char flag where "l" indicates   *
 *        to do itemized error listings, it is the default. A value   *
 *        of "s" indicates to do summary processing.                  *
 *    no_wrap:                                                        *
 *         = TRUE do not wrap error messages in report                *
 *    rpt_file_name:                                                  *
 *        The rpt_file_name variable is a character string containing *
 *        the name of the file to be written to.                      *
 *    start_path                                                      *
 *        This is a string for the starting path for pointer searches *
 *    use_aliases:                                                    *
 *        The use_aliases variable is a TRUE/FALSE flag indicating    *
 *        whether names are dealiased by the label verifier.          *
 *    use_dd:                                                         *
 *        The use_dd variable is a TRUE/FALSE flag indicating whether *
 *        data dictionary validation is enabled.                      *
 *    use_di:                                                         *
 *         Search all subdirectories recursively                      *
 *    use_expand:                                                     *
 *        The use_expand variable is a TRUE/FALSE flag indicating     *
 *        whether to expand structure pointers.                       *
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
 *    x_lbl_rpt:                                                      *
 *         =0 do validation and produce external file verification    *
 *           report.                                                  *
 *         =1 do not validate but do produce external file            *
 *            verification report.                                    *
 *         =2 do validate but do not produce external file            *
 *            verification report.                                    *
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
 *	  MDC	12-26-02   Added initialization to a new structure member,*
 *					   warnings										  *
 *	  MDC	01-03-03   Added initialization to a new structure member,*
 *					   info											  *
 *	  MDC	02-18-03   Added code to handle a new flag option, -max,  *
 *					   that will allow the user to control the number *
 *					   of error messages that lvtool will output to   *
 *					   a report file.								  *
 *    MDC   01-06-05   Changed flag from "l3d" to "nol3d" so that it  *
 *                     looks for label files by default. Also made    *
 *                     change to internalize the making of the PDS    *
 *                     Data Dictionary index file. Added "se" option  *
 *                     that will allow lvtool to not delete the       *
 *                     files that it creates when it needs to expand  *
 *                     a label file with a STRUCTURE pointer.         *
 *    MDC   06-13-05   Modified code to add a slash mark at the end   *
 *                     of the directory path if there isn't one when  *
 *                     the user specifies the -b3 option.             *
 *    MDC   07-26-05   Made the routine backwards compatible, where   *
 *                     -d will still accept the index file. A -df flag*
 *                     has been added where it accepts the data       *
 *                     dictionary full file as an input. So the user  *
 *                     now has the option of specifying either the    *
 *                     index or full file in the command-line.        *
 *    MDC   02-22-06   Added initialization stmt to alias_message_ptr *
 *                     Updated flag descriptions for -e, -np, and -p  *
 *                     for clarity                                    *
 **********************************************************************/


LOGICAL lv_setup (num_args, command_line)

int         num_args;
char        *command_line [];
{
   LOGICAL success           = TRUE;
   LOGICAL report_name_found = FALSE;
   LOGICAL log_name_found    = FALSE;
   LOGICAL input_name_found  = FALSE;
   LOGICAL dd_idx_name_found = FALSE;
   LOGICAL dd_full_name_found = FALSE;
   LOGICAL ivf_name_found    = FALSE;  /*invalid file name, list of invalid file types*/
   LOGICAL ivd_name_found    = FALSE;  /*invalid dir name, list of invalid directories*/
   LOGICAL use_input_file_found = FALSE;
   int i;  
   char temp [PDS_MAXLINE + 1];
   char err_msg [PDS_MAXLINE + 1];
   STRING_LIST *temp_list = NULL;
   LOGICAL file_flag = FALSE;
   LOGICAL test1     = FALSE;
   LOGICAL test2     = FALSE;
   char *dd_full_name = NULL;	
   char *temp_ptr = NULL;

   LOGICAL index_dd_test = FALSE;
   LOGICAL full_dd_test = FALSE;

#ifdef MSDOS_TC
	int islash = '\\';
	char cslash[2] = {'\\'};
#else
	int islash = '/';
	char cslash[2] = {'/'};
#endif

/** BEGIN **/
   /*---------------------------------------------------------------------*/
   /** initialize all flags to their defaults                            **/
   /*---------------------------------------------------------------------*/
                             
   lvtv.use_dd        = TRUE;
   lvtv.use_verbose   = TRUE;
   lvtv.use_terminal  = FALSE;
   lvtv.use_aliases   = FALSE;
   lvtv.use_progress  = FALSE;
   lvtv.use_log_file  = FALSE;
   lvtv.dd_index_name = NULL;
   lvtv.log_file_name = NULL;
   lvtv.rpt_file_name = NULL;
   lvtv.file_list     = NULL;
   lvtv.lv_or_slv     = 'l';
   lvtv.use_expand    = FALSE;
   lvtv.use_di        = FALSE; /*initialize for recursive directory searchinglvtv*/ 
   lvtv.x_lbl_rpt     = 0;  
   lvtv.inval_file    = FALSE;
   lvtv.no_wrap       = FALSE;
   lvtv.inval_dir     = FALSE;
   lvtv.lbl_det       = TRUE;   /* 01-07-05 MDC - Default to TRUE */
   lvtv.use_input_file = FALSE;
   lvtv.warnings	  = TRUE; /* 12-26-02 MDC */
   lvtv.info		  = TRUE; /* 01-06-03 MDC */
   lvtv.save_expanded_files = FALSE; /* 01-03-05 MDC */
   lvtv.alias_message_ptr = NULL; /* 02-22-06 MDC */

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

			if (strcmp (temp, "-nd")       == 0) lvtv.use_dd = FALSE;
			else if (strcmp (temp, "-v")   == 0) lvtv.use_verbose = TRUE;
			else if (strcmp (temp, "-nv")  == 0) lvtv.use_verbose = FALSE;
			else if (strcmp (temp, "-nt")  == 0) lvtv.use_terminal = FALSE;
			else if (strcmp (temp, "-t")   == 0) lvtv.use_terminal = TRUE;
			else if (strcmp (temp, "-a")   == 0) lvtv.use_aliases = TRUE;
			else if (strcmp (temp, "-p")   == 0) lvtv.use_progress = TRUE;
			else if (strcmp (temp, "-l")   == 0) lvtv.lv_or_slv = 'l';
			else if (strcmp (temp, "-s")   == 0) lvtv.lv_or_slv = 's';
			else if (strcmp (temp, "-na")  == 0) lvtv.use_aliases = FALSE;
			else if (strcmp (temp, "-e")   == 0) lvtv.use_expand = TRUE;
			else if (strcmp (temp, "-nwarn") == 0) lvtv.warnings = FALSE; /* 12-26-02 MDC */
			else if (strcmp (temp, "-ninfo") == 0) lvtv.info = FALSE; /* 01-06-03 MDC */
												/* Turn off recursive directory searching*/
			else if (strcmp (temp, "-di")  == 0) lvtv.use_di = TRUE; 
			else if (strcmp (temp, "-se")  == 0) lvtv.save_expanded_files = TRUE; /* 01-03-05 MDC */
			else if (strcmp (temp, "-b1")  == 0) lvtv.x_lbl_rpt = 1;
			else if (strcmp (temp, "-b2")  == 0) lvtv.x_lbl_rpt = 2;
			else if (strcmp (temp, "-nw")  == 0) lvtv.no_wrap = TRUE;
			else if (strcmp (temp, "-nol3d") == 0) lvtv.lbl_det = FALSE;
            /*-----------------------------------------------------------------*/
            /** ELSE IF this is the last flag on the line THEN                **/
            /**     We have failed.  All of the remaining flags must be       **/
            /**     followed by file names. While -t must be followed by a    **/
	        /**     a string as with -r, -f, -d this string will be a path.   **/
			/**																  **/
			/** 02-18-03 MDC												  **/
			/** Added a check to look for the "-max" flag as well.			  **/
			/**                                                               **/
			/** 07-26-05 MDC                                                  **/
			/** Added check to look for the "-df" flag                        **/
            /*-----------------------------------------------------------------*/

            else if ((file_flag = ((strcmp (temp, "-r") == 0) || 
		         (strcmp (temp, "-f"  ) == 0) || (strcmp (temp, "-d"  ) == 0) ||
				 (strcmp (temp, "-df" ) == 0) || (strcmp (temp, "-b3" ) == 0) ||
				 (strcmp (temp, "-ivf") == 0) || (strcmp (temp, "-ivd") == 0) ||
				 (strcmp (temp, "-lef") == 0) || (strcmp (temp, "-fin") == 0) ||
				 (strcmp (temp, "-max") == 0)) && i >= (num_args - 1) )  ) 
			{
				 /* 03-06-03 MDC - Added an error handling msg for the -max flag */
				 if(strcmp (temp, "-max") == 0)
				 {
					 success = FALSE;
					 sprintf (err_msg, "Command line option \"%s\" must be followed by an integer value",
																		command_line [i]);
					 err_append_message (ERROR1, err_msg);
				 }
		         else if(strcmp (temp, "-b3") != 0)
				 {
	                 success = FALSE;
	                 sprintf (err_msg, "Command line option \"%s\" must be followed by a file name",
		                                                                 command_line [i]);
	                 err_append_message (ERROR1, err_msg);
				 }
		         else
				 {
	                 success = FALSE;
	                 sprintf (err_msg, "Command line option \"%s\" must be followed by a path name",
		                                                                 command_line [i]);
	                 err_append_message (ERROR1, err_msg);
				 }
			}
	        /*-----------------------------------------------------------------*/
	        /* ELSE IF this option is followed by another flag THEN            */
	        /*     We have failed.  All of the remaining flags must be         */
	        /*     file names.                                                 */
	        /*-----------------------------------------------------------------*/
            else if (file_flag && *(command_line[i+1]) == '-')
			{
		         if(strcmp (temp, "-b3") != 0)
				 {
			        success = FALSE;
	                sprintf (err_msg, "Command line option \"%s\" must be followed by a file name",
		            command_line [i]);
	                err_append_message (ERROR1, err_msg);
				 }
		         else
				 {
	                success = FALSE;
	                sprintf (err_msg, "Command line option \"%s\" must be followed by a path name",
		                                                                 command_line [i]);
	                err_append_message (ERROR1, err_msg);
				 }
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
				strcpy (dd_full_name, command_line [i]);
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
				Malloc_String(lvtv.inval_file_name, (int) String_Size(command_line [i]));
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
				Malloc_String(lvtv.inval_dir_name, (int) String_Size(command_line [i]));
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
				do
				{
					i++;
					temp_list = slv_get_volume_list (command_line [i], lvtv.use_di);
					 if (temp_list != NULL)
						  lvtv.file_list = util_append_string_list (lvtv.file_list, 
					          (char *) temp_list, LIST_TYPE);
					 lvtv.input_file_name = command_line[i];
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
				Malloc_String(lvtv.input_file_name, (int) String_Size(command_line[i]));
				strcpy (lvtv.input_file_name, command_line [i]);
				temp_list = new_volume_list(lvtv.input_file_name);
					 if (temp_list != NULL)
					 {
						  lvtv.file_list = util_append_string_list (lvtv.file_list, 
					          (char *) temp_list, LIST_TYPE);
					 }
			}
			/*-----------------------------------------------------------------*/
			/** ELSE IF the next flag is the REPORT FILE flag THEN            **/
			/**    copy the next argument to the report file name             **/
			/*-----------------------------------------------------------------*/

			else if (strcmp (temp, "-r") == 0)
			{
				report_name_found = TRUE;
				i++;
				Malloc_String(lvtv.rpt_file_name, (int) String_Size(command_line[i]));
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
				Malloc_String(lvtv.log_file_name, (int) String_Size(command_line[i]));
				strcpy (lvtv.log_file_name, command_line [i]);
			}
			/*-----------------------------------------------------------------*/
			/** ELSE IF the next flag is the path flag THEN                   **/
			/**    copy the next argument to the path variable                **/
			/*-----------------------------------------------------------------*/

			else if (strcmp (temp, "-b3") == 0)
			{
				i++;
				Malloc_String(lvtv.start_path, (int) (String_Size(command_line[i]) + 2));
				strcpy (lvtv.start_path, command_line [i]);

				/* 06-13-05 MDC - We need to make sure that there is a slash mark
				   at the end of this directory path to ensure that we correctly do
				   some of the lvtool routines that use this path.
				   i.e. C:\directory1\directory2\
			    */
				temp_ptr = lvtv.start_path + strlen(lvtv.start_path) - 1;

				if(*temp_ptr != islash)
					strcat(lvtv.start_path, cslash);
			}

			/*-----------------------------------------------------------------*/
			/** 02-18-03 MDC												  **/
			/** ELSE IF the next flag is the max flag THEN copy the next	  **/
			/** argument to the lvtool_max_error variable.					  **/
			/*-----------------------------------------------------------------*/
			else if (strcmp (temp, "-max") == 0)
			{
				i++;
				
				/* If a user specifies "0" for the max integers...*/
				if(strcmp(command_line[i], "0") == 0)
					lvtool_max_errors = 0;
				/* Convert the string to an integer...*/
				else if ( ((lvtool_max_errors = Make_Long(command_line[i])) < 0) ||
						  ((lvtool_max_errors =Make_Long(command_line[i])) == 0) )
				{
					lvtool_max_errors = PDS_MAX_ERRORS;
					
					err_append_message (CONTINUE, "");
					err_append_message (CONTINUE, "WARNING! Invalid integer entered.");
					err_append_message (CONTINUE,
						"Lvtool will now use default value of 300 maximum errors allowed.");
					err_append_message (CONTINUE, "");
				}
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
			Malloc_String(lvtv.dd_index_name, (int) String_Size(PDS_DEFAULT_DD));
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
			Malloc_String(lvtv.dd_index_name, (int) String_Size(PDS_DEFAULT_DD));
			strcpy (lvtv.dd_index_name, PDS_DEFAULT_DD);
		}

		/*---------------------------------------------------------------------*/
		/** IF no report file name was found and terminal display is not      **/
		/**    enabled THEN print an error                                    **/
		/** ENDIF                                                             **/
		/*---------------------------------------------------------------------*/

		if (!report_name_found && !lvtv.use_terminal)
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
		
		/***********************************************************************
		 01-03-05 MDC - The save expanded files option is only valid if the 
		      -e option is selected also. So, we want to check for this.      
	     ***********************************************************************/
		if (lvtv.save_expanded_files)
		{
			if(!lvtv.use_expand)
			{
				success = FALSE;
				err_append_message (ERROR1,
					"The -e option must also be specified if the -se option is set");
			}
		}
 
		/*---------------------------------------------------------------------*/
		/** IF an error occurred THEN                                         **/
		/**    display program information                                    **/
		/** ENDIF                                                             **/
		/**																	  **/
		/** 02-18-03 MDC													  **/
		/** Updated help information to include description on additional flag**/
		/** options just added, -ninfo, -nwarn, -max.						  **/
		/*---------------------------------------------------------------------*/
   }
   if (!success)
   {
	  long wait_count1;
	  long wait_count2;
	  char ch;

#ifdef MSDOS_TC      
	  printf("****************************************************************************\n");
#endif
      printf("Disclaimer:\n");
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
	  printf("Usage:\n");
      printf("lvtool -[a,na,t,nt,v,nv,nd,s,l,e,di,b1,b2,ninfo,nwarn] [-d dd-index-name]\n");
	  printf("        [-df dd-full-name] -b3 path -r report-file -f label-file\n");
	  printf("       -ivf extension_list -max integer-value\n");
      printf("Where:\n");
      printf("   -a:          Enables aliasing\n");
	  printf("   -b1:         Validation YES, File Pointer Verification NO\n");
	  printf("   -b2:         Validation NO, File Pointer Verification YES\n");
      printf("                Not b1 and Not b2, Validation YES, File pointer\n");
	  printf("                Verification YES\n");
	  printf("   -b3 <path>:  Specifies path to search for Pointer files, default\n");
	  printf("                is the current working directory\n");       
      printf("   -d <file>:   Specifies the Data Dictionary index file to use \n");
	  printf("                default \"pdsdd.idx\")\n");
	  printf("   -df <file>:  Specifies the Data Dictionary full file to use \n");
	  printf("                default \"pdsdd.full\")\n");
	  printf("                Cannot specify both -d and -df at the same time\n");
	  printf("   -di:         DOS only, specifies not to search directory and \n");
	  printf("                sub directories.  Default is to search directories\n");
	  printf("                recursively\n");
	  printf("   -e:          Specifies to expand ^STRUCTURE pointers (when present in label)\n");
	  printf("  -se:          Saves the files that lvtool creates when it expands a file\n");
	  printf("                to expand ^STRUCTURE pointers. Option is valid only when -e\n");
	  printf("                is specified also.\n");

#ifdef MSDOS_TC      
	  printf("\n\n Press the ENTER KEY to continue now or the program will \n");
      printf(" continue in a moment.\n");
	  printf("****************************************************************************\n");
	  for( wait_count1 = 0; wait_count1 < 800000; wait_count1++)
	  {
		  for( wait_count2 = 0; wait_count2 < 10000; wait_count2++);
		  if(_kbhit())
		  {
			  ch = _getch();
			  wait_count1 = 800001;
			  wait_count2 = 100001;
		  }
	  }
	  
	  printf("\n****************************************************************************\n");
#endif
      printf("   -f <file>:   Specifies the label file(s) to be verified\n");
	  printf("                (can be wildcarded)\n");
	  printf("   -fin <file>: Specifies a file containing a list of files to ,\n");
	  printf("                validate.  One file per line, path is assumed to be included\n");       
	  printf("   -ivd <file>: Specifies a file containing directories to skip,\n");
	  printf("                default is no file. Each directory is on a\n");       
	  printf("                separate line, names are NOT case sensative.  There can\n");
	  printf("                be up to 30 entries.  Each directory name must be less\n");  
	  printf("                than 100 characters in length\n");
	  printf("   -ivf <file>: Specifies a file containing file extensions to skip,\n");
	  printf("                default is no file. List is: one line, comma seperated,\n");       
	  printf("                NOT case sensative, limited to 500 characters.\n");
	  printf("   -l:          Specifies itemized error reports (default)\n");
	  printf("   -lef <file>: Directs LVTOOL to keep a log file of all files that are\n");
      printf("                skipped and specifies a file name for the log\n");
      printf("                Default is to not create a log file.\n");
	  printf("   -nol3d:      Validate files without checking for a level 3 label. Default\n");
	  printf("                is to validate only files that contain a level 3 label.\n");
      
#ifdef MSDOS_TC
	  printf("\n\n Press the ENTER KEY to continue now or the program will \n");
      printf(" continue in a moment.\n");
	  printf("****************************************************************************\n");
	  for( wait_count1 = 0; wait_count1 < 800000; wait_count1++)
	  {
		  for( wait_count2 = 0; wait_count2 < 10000; wait_count2++);
		  if(_kbhit())
		  {
			  wait_count1 = 800001;
			  wait_count2 = 100001;
		  }
	  }

	  printf("****************************************************************************\n");
#endif
	  
      printf("   -na:         Disables aliasing (default)\n");
	  printf("   -max <val>:  Specifies the maximum number of errors that LVTOOL will \n");
	  printf("                print to the output file. <val> is an integer value that the\n");
	  printf("                user must specify if this flag is used. The default value is\n");
	  printf("                300 maximum errors if this flag is not entered.\n");
      printf("   -nd:         Disables Data Dictionary usage\n");
	  printf("   -ninfo:      Disables LVTOOL from outputting INFO messages onto the output\n");
	  printf("                file\n");
	  printf("   -np:         Disables progress reporting (default) when using the -s flag\n");
      printf("   -nt:         Disables terminal output (default)\n");
      printf("   -nv:         Disables verbose error reporting\n");
	  printf("   -nwarn:      Disables LVTOOL from outputting WARNING messages onto the\n");
	  printf("                output file\n");
	  printf("   -nw:         Do not wrap error messages\n");       
	  printf("   -p:          Enables progress reporting when using the -s flag\n");
      printf("   -r <file>:   Specifies the report file for output\n");
	  printf("   -s:          Specifies summary error reports\n");
      printf("   -t:          Enables terminal output\n");
      printf("   -v:          Enables verbose error reporting (default)\n");
#ifdef MSDOS_TC
	  printf("****************************************************************************\n");
#endif	  
   }
   /*---------------------------------------------------------------------*/
   /** IF a data dictionary to be used THEN                              **/
   /**    initialize the data dictionary                                 **/
   /** ENDIF                                                             **/
   /*---------------------------------------------------------------------*/

   if (success && lvtv.use_dd)
   {
	   if(full_dd_test)
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
	   
	   if(success == TRUE)
	   {
           if (!(lvtv.use_dd = dd_init (lvtv.dd_index_name)))
		   {
               if(lvtv.lv_or_slv == 's')
			   {
                   err_append_message (WARNING,
						"No Data Dictionary, Verification will cease");
				   success = FALSE;
			   }
			   else
			   {
                   err_append_message (WARNING,
						"Verification will proceed without a Data Dictionary");
			   }
		   }
		   ver_init(lvtv.dd_index_name);
	   }
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
 *    void rpt_file_header (rpt_file_pointer, label_file_name)        *
 *$Abstract                                                           *
 *    Generates a file header and writes it to a report file          *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    LABEL_VERIFIER                                                  *
 *    REPORT                                                          *
 *$Inputs                                                             *
 *    rpt_file_pointer:                                               *
 *        The rpt_file_pointer variable is a file pointer for the     *
 *        report file.                                                *
 *    label_file_name:                                                *
 *        The label_file_name variable is a character string          *
 *        containing the name of the label file.                      *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    The rpt_file_header routine generates the file header for a     *
 *    label validation report. This header includes the name of the   *
 *    file being validated.                                           *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Kristy L. Marski /JPL                                           *
 *$Version_and_Date                                                   *
 *    1.2   December 10, 1991                                         *
 *$Change_history                                                     *
 *    KLM   03-15-91   Original generation.                           *
 *    DPB   09-27-91   Moved into lv_tool.c                           *
 *    MDD   12-10-91   Added check for NULL file pointer              *
 *    MDC   12-16-04   Minor output formatting change                 *
 **********************************************************************/

void rpt_file_header (rpt_file_pointer, label_file_name)
                                                      
FILE *rpt_file_pointer;
char *label_file_name;


{
    long i = 1;   
    long num_blanks = 0;

/*----------------------------------------------------------------*/
/** BEGIN                                                        **/
/**                                                              **/
/**    Write the the label file name to the report file          **/
/*----------------------------------------------------------------*/

   if (rpt_file_pointer != NULL)
   {
      fprintf(rpt_file_pointer,"\n\n\n");
      fprintf(rpt_file_pointer,">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n");
      fprintf(rpt_file_pointer,">                                                                      <\n");
      fprintf(rpt_file_pointer,"#     %s", label_file_name);
      num_blanks = (long) String_Size(label_file_name);
      num_blanks = (72 - num_blanks - 6);
      if (num_blanks > 1)
      {
         while (i <= num_blanks)
         {
            fprintf(rpt_file_pointer," ");
            i++;
         }
      }
	  /* 12-16-04 MDC - Add space for "cleaner" representation of filename */
      fprintf(rpt_file_pointer," <\n");
      fprintf(rpt_file_pointer,">                                                                      <\n");
      fprintf(rpt_file_pointer,"<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n\n\n");
   }

/** END **/
}  /*  End: "rpt_file_header"  */


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
	int  line_count = 1;                                       /*12-11-98*/

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
        /* If these are WARNING or ERROR lines then we will want to put      */
		/* numbers in front of them for the output analyzer      12-11-98    */
		/*-------------------------------------------------------------------*/
        line_count = 1;
		if((strstr(text, "WARNING:") == NULL) &&                    /*12-11-98*/
			(strstr(text, "ERROR:") == NULL) &&               /*12-12-98*/
			(strstr(text, "INFO:") == NULL)) line_count = 0;  /*12-11-98*/

if(((lvtv.stream == TRUE) && (strstr(text,"ERROR") != NULL) && (strstr(text, "missing") != NULL) &&
								(strstr(text, "FILE_RECORDS") != NULL)) == 0)
{

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
            len = (long) strlen (start_char);

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
			/* If the line count is zero then we are not preceding the message*/
			/* with a line number.                                            */
					if (line_count == 0 )                           /*12-11-98*/
				    {                                               /*12-11-98*/
                       fprintf (rpt_file_pointer, "%s", blanks);
				    }                                               /*12-11-98*/
				    else                                            /*12-11-98*/
				    {
                       fprintf (rpt_file_pointer, "%i%s", 
						                       line_count, blanks); /*12-11-98*/
				       line_count++;                                /*12-11-98*/
				    }                                               /*12-11-98*/
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
			/*If this is not the first line then precede it with a line      */
			/*The first line gets it's line number in previous block of      */
			/*similar code.                                                  */
					if (line_count != 1 )                          /*12-11-98*/
				    {                                              /*12-11-98*/
                       fprintf (rpt_file_pointer, "%s\n", start_char);
				    }                                              /*12-11-98*/
				    else                                           /*12-11-98*/ 
				    {                                              /*12-11-98*/
                       fprintf (rpt_file_pointer, "%i%s\n", 
						                  line_count, start_char); /*12-11-98*/
				       line_count++;                               /*12-11-98*/
				    }                                              /*12-11-98*/ 
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
}
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
 *    void alt_rpt_format_message (rpt_file_pointer, text)            *
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
 *    This version of the rpt_format_message routine takes the text   *
 *    of the message passed in and simply prints it.  It will be too  *
 *    wide for the report.  Some folks wanted it this way so that it  *
 *    it could be entered into databases more easily.                 *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    2.1   March 18, 1992                                            *
 *$Change_history                                                     *
 *    05-13-99 original version                                       *
 **********************************************************************/

void alt_rpt_format_message (rpt_file_pointer, text)
                                                      
FILE *rpt_file_pointer;
char *text;

{
    if (text != NULL)
    {
        if (rpt_file_pointer != NULL)
			{
                fprintf (rpt_file_pointer, "%s\n", text);
		    }
    } 

}  /*  "alt_rpt_format_message"  */


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
   fprintf(rpt_file_pointer,"*              This is the end of the Validation Report                *\n");
   fprintf(rpt_file_pointer,"*                                                                      *\n");
   fprintf(rpt_file_pointer,"*                      %s", date_string);
   fprintf(rpt_file_pointer,"                        *\n");
   fprintf(rpt_file_pointer,"*                                                                      *\n");
   fprintf(rpt_file_pointer,"************************************************************************\n");

   fclose (rpt_file_pointer);
   Lemme_Go(date_string);
 }
 return;

}

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
 * LVTOOL_V lvtv is a structure containing the following variables    *
 * which are used by this function                                    *  
 *    alias_message_ptr                                               *
 *        This is pointer to a list of messages                       *
 *    dd_index_name                                                   *
 *        The name of the data dictionary index                       *
 *    file_list                                                       *
 *        A pointer to a list of files to be verified                 *
 *    init_index_name:                                                *
 *       The init_index_name variable is a character string containing*
 *       the name of a DD index file.                                 *
 *    inval_dir                                                       *
 *        A flag enabling use of the invalid file directory option    *
 *    inval_dir_name                                                  *
 *        This is a string for the name of the file containing a list *
 *        of directories to be excluded from the search for file to   *
 *        validate                                                    *
 *    inval_file                                                      *
 *        A flag enabling use of the invalid file extension option    *
 *    inval_file_name                                                 *
 *        This is a string for the name of the file containing a list *
 *        of file extensions to be excluded from validation           *
 *    lbl_det                                                         *
 *        A flag allowing exclusion of files that do not contain      *
 *        labels                                                      *
 *    lv_or_slv:                                                      *
 *       A char variable containing either an "l" or an "s". "l" is   *
 *       the default and indicates to do itemized error reports. "s"  *
 *       indicates to do summary error reports.                       *
 *    no_wrap                                                         *
 *       =TRUE do not wrap error lines                                *
 *    rpt_file_name:                                                  *
 *        The rpt_file_name variable is a character string containing *
 *        the name of the file to be written to.                      *
 *    start_path                                                      *
 *        This is a string for the starting path for pointer searches *
 *    use_aliases:                                                    *
 *        The use_aliases variable is a TRUE/FALSE flag indicating    *
 *        whether names are dealiased by the label verifier.          *
 *    use_dd:                                                         *
 *        The use_dd variable is a TRUE/FALSE flag indicating whether *
 *        data dictionary validation is enabled.                      *
 *    use_di                                                          *
 *        Search all subdirectories recursively                       *
 *    use_progress                                                    *
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
 *    x_lbl_rpt:                                                      *
 *       =0 do validation and produce external file verification      *
 *          report.                                                   *
 *       =1 do not validate but do produce external file              *
 *          verification report.                                      *
 *       =2 do validate but do not produce external file              *
 *          verification report.                                      *
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
 *    DWS   05-13-97   Added no_wrap arg.                             *
 *    MDC	02-18-03   Added messages to the output file to tell the  *
 *					   user whether info and/or warning messages were *
 *					   turned on or off.							  *
 *    MDC   01-06-05   Added new print statement to tell the user     *
 *                     whether the expanded files created by lvtool   *
 *                     were set to be saved or not.                   *
 **********************************************************************/

FILE *rpt_main_header ()
{
 FILE *file_pointer = NULL;
 char *date_string;
 long num_blanks = 0;
 char *dd_msg = "Toolbox Data Dictionary Name:";
 
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

      fprintf(file_pointer,   "************************************************************************\n");
      fprintf(file_pointer,   "*                                                                      *\n");
      fprintf(file_pointer,   "*               Planetary Data System Validation Report                *\n");          
      fprintf(file_pointer,   "*                     Label Verifier Version %s                      *\n",
		                                                                     LVTOOL_VERSION);
      fprintf(file_pointer,   "*                                                                      *\n");
      fprintf(file_pointer,   "*                      %s", date_string);
      fprintf(file_pointer,   "                        *\n");
      fprintf(file_pointer,   "*                                                                      *\n");
      if (lvtv.dd_index_name != NULL && lvtv.use_dd == TRUE)
      {
         num_blanks = (70 - ((long) (strlen (dd_msg) + 1 + strlen (lvtv.dd_index_name)))) / 2;
         fprintf (file_pointer, "*%*s %-*s *\n", num_blanks + strlen (dd_msg), 
                    dd_msg, num_blanks + strlen (lvtv.dd_index_name), 
                    lvtv.dd_index_name); /* removed 1 + from begining of 5th arg */
      }
      fprintf(file_pointer,   "*                                                                      *\n");
      if (lvtv.use_dd == TRUE)
         fprintf(file_pointer,"*                  Data Dictionary Validation is ON                    *\n");
      else
         fprintf(file_pointer,"*                  Data Dictionary Validation is OFF                   *\n");
      if (lvtv.use_verbose == TRUE)
         fprintf(file_pointer,"*                    Verbose Error Reporting is ON                     *\n");
      else
         fprintf(file_pointer,"*                    Verbose Error Reporting is OFF                    *\n");
      if (lvtv.use_terminal == TRUE)
         fprintf(file_pointer,"*                        Screen Display is ON                          *\n");
      else
         fprintf(file_pointer,"*                        Screen Display is OFF                         *\n");
      if (lvtv.use_aliases == TRUE)
         fprintf(file_pointer,"*                        Name Aliasing is ON                           *\n");
      else
         fprintf(file_pointer,"*                        Name Aliasing is OFF                          *\n");
	  if (lvtv.lv_or_slv == 's')
		 fprintf(file_pointer,"*                   Summary Error Reporting is ON                      *\n");
	  else
		 fprintf(file_pointer,"*                   Itemized Error Reporting is ON                     *\n");
	  if (lvtv.use_expand == TRUE)
		 fprintf(file_pointer,"*                   Expand Structure Pointers is ON                    *\n");
	  else
		 fprintf(file_pointer,"*                   Expand Structure Pointers is OFF                   *\n");
	  if (lvtv.save_expanded_files == TRUE)
		 fprintf(file_pointer,"*                      Saving Expanded Files is ON                     *\n");
	  else
	     fprintf(file_pointer,"*                     Saving Expanded Files is OFF                     *\n");
      if (lvtv.use_di == TRUE)
         fprintf(file_pointer,"*                DOS Recursive Directory Searching is OFF              *\n");
      else
         fprintf(file_pointer,"*                DOS Recursive Directory Searching is ON               *\n");
	  
	     fprintf(file_pointer,"*            Maximum Number of Errors Allowed is set to %ld            *\n", lvtool_max_errors);

	  /* 02-13-03 MDC - Added a display to show user whether warning messages will be displayed */
	  if (lvtv.warnings == TRUE)
		 fprintf(file_pointer,"*                  Warning Messages will be displayed                  *\n");
	  else
		 fprintf(file_pointer,"*                   Warning Messages will be ignored                   *\n");
	  /* 02-18-03 MDC - Added a display to show user whether info messages will be displayed */
	  if (lvtv.info == TRUE)
		 fprintf(file_pointer,"*                   Info messages will be displayed                    *\n");
	  else
		 fprintf(file_pointer,"*                    Info messages will be ignored                     *\n");
	  if (lvtv.x_lbl_rpt == 0)
		 fprintf(file_pointer,"*             Label Validation and Pointer File Verification           *\n");
	  else if (lvtv.x_lbl_rpt == 1)
		 fprintf(file_pointer,"*           Label Validation Without Pointer File Verfication          *\n");
	  else if (lvtv.x_lbl_rpt == 2)
		 fprintf(file_pointer,"*           Pointer File Verification Without Label Validation         *\n");
      if (lvtv.lbl_det == TRUE)
         fprintf(file_pointer,"*                          Label Detection Enabled                     *\n");
      else
         fprintf(file_pointer,"*                          Label Detection Disabled                    *\n");
	  if (lvtv.start_path != NULL)
	  {
		 fprintf(file_pointer,"*           Start Path for Pointer File Search is:                     *\n");
         num_blanks = (72 - ((long) strlen (lvtv.start_path) + 1)) / 2; 
         fprintf(file_pointer,"* %*s%*s\n", num_blanks, lvtv.start_path,
			                                 num_blanks + strlen(lvtv.start_path), "*");
	  if (lvtv.inval_file == TRUE)
	  {
		 fprintf(file_pointer,"*           A List Of Invalid Extensions Has Been Supplied In File:    *\n");
         num_blanks = (72 - ((long) strlen (lvtv.inval_file_name) + 1)) / 2; 
         fprintf(file_pointer,"* %*s%*s\n", num_blanks, lvtv.inval_file_name,
			                                 num_blanks + strlen(lvtv.inval_file_name), "*");
	  }
	  if (lvtv.inval_dir == TRUE)
	  {
		 fprintf(file_pointer,"*           A List Of Invalid directories Has Been Supplied In File:    *\n");
         num_blanks = (72 - ((long) strlen (lvtv.inval_dir_name) + 1)) / 2; 
         fprintf(file_pointer,"* %*s%*s\n", num_blanks, lvtv.inval_dir_name,
			                                 num_blanks + strlen(lvtv.inval_dir_name), "*");
	  }
	  }
      if (lvtv.no_wrap == TRUE)
         fprintf(file_pointer,"*                   Error Message Wrapping is OFF                       *\n");
      else
         fprintf(file_pointer,"*                   Error Message Wrapping is ON                        *\n");
      fprintf(file_pointer,   "*                                                                       *\n");
	  fprintf(file_pointer,   "************************************************************************\n");

   }     

   /*------------------------------------------------------------*/
   /** ENDIF                                                    **/
   /** free the date string                                     **/
   /** return a pointer to the file                             **/
   /*------------------------------------------------------------*/
                            
   Lemme_Go(date_string);
 }
 return(file_pointer);

/** END **/

}  /*  End: "rpt_main_header"  */


/**********************************************************************
 *$Component                                                          *
 *    void rpt_odl_errors (rpt_file_pointer, no_wrap)                 *
 *$Abstract                                                           *
 *    Writes the ODL errors to a report file.                         *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    LABEL_VERIFIER                                                  *
 *    REPORT                                                          *
 *$Inputs                                                             *
 *    rpt_file_pointer:                                               *
 *        The rpt_file_pointer variable is a file pointer for the     *
 *        report file.                                                *
 *    no_wrap:                                                        *
 *      =TRUE do not wrap error messages                              *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    The rpt_odl_errors routine checks to see if the there are any   *
 *    ODL syntax errors and writes them to the report file.           *
 *$External_References                                                *
 *    Item                     Shared-Data           Access           *
 *    ------------------------------------------------------------    *
 *    pds_message_list         pdsglob.h             read             *
 *$Author_and_Institution                                             *
 *    Kristy L. Marski /JPL                                           *
 *$Version_and_Date                                                   *
 *    1.4   December 10, 1991                                         *
 *$Change_history                                                     *
 *    KLM   03-25-91   Original generation.                           *
 *    KLM   05-14-91   Added delimiters around report sections.       *
 *    DPB   06-04-91   Added code to format messages.                 *
 *    DPB   09-27-91   Moved into lv_tool.c                           *
 *    MDD   12-10-91   Added check for NULL file pointer              *
 *    DWS   05-13-99   Added no_wrap arg.                             *
 **********************************************************************/

void rpt_odl_errors (rpt_file_pointer, no_wrap)
                                                      
FILE *rpt_file_pointer;

{
	ERROR_LIST *msg_ptr;

 if (rpt_file_pointer != NULL)
 {

   if (pds_message_list != NULL)
   {
      fprintf (rpt_file_pointer, 
      "\n------------------------------------------------------------------------\n");
      fprintf (rpt_file_pointer, 
      "*****           The ODL syntax errors in this file are:            *****\n");
      fprintf (rpt_file_pointer, 
     "------------------------------------------------------------------------\n\n");


      for (msg_ptr = pds_message_list; msg_ptr != NULL; msg_ptr = msg_ptr->next)
          if(!no_wrap) rpt_format_message (rpt_file_pointer, msg_ptr -> message);
          else alt_rpt_format_message (rpt_file_pointer, msg_ptr -> message);


   }
  
   else
   {

      fprintf (rpt_file_pointer, 
      "\n------------------------------------------------------------------------\n");
      fprintf (rpt_file_pointer, 
      "*****        There were no ODL syntax errors for this file.        *****\n"); 
      fprintf (rpt_file_pointer, 
      "------------------------------------------------------------------------\n\n");

   }
 }
 return;

}  /*  End: "rpt_odl_errors"  */


/**********************************************************************
 *$Component                                                          *
 *    void rpt_print_tree (rpt_file_pointer, odl_tree_pointer,        *
 *                         err_message_ptr)                           *
 *$Abstract                                                           *
 *    Writes the object level diagram to a report file                *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    LABEL_VERIFIER                                                  *
 *    REPORT                                                          *
 *$Inputs                                                             *
 *    rpt_file_pointer:                                               *
 *        The rpt_file_pointer variable is a file pointer for the     *
 *        report file.                                                *
 *    odl_tree_pointer:                                               *
 *        The odl_tree_pointer variable is a pointer to the root of   *
 *        on ODL tree structure.                                      *
 *    err_message_ptr:                                                *
 *        The err_message_ptr variable is a pointer to a list of the  *
 *        error messages.                                             *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    The rpt_print_tree routine writes the object level diagram of   *
 *    a label to a report file. The err_message_ptr input points to   *
 *    any error messages which should be printed after the ODL tree   *
 *    is printed. This is intended to be used to indicate the         *
 *    starting point of messages generated by de-aliasing.            *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Kristy L. Marski /JPL                                           *
 *$Version_and_Date                                                   *
 *    2.6   March 18, 1992                                            *
 *$Change_history                                                     *
 *    KLM   03-26-91   Original generation.                           *
 *    KLM   05-14-91   Changed the code so it would not begin each    *
 *                     search for the next object from the root, but  *
 *                     would just use NextAggregate instead.          *
 *                     Added delimiters around report sections.       *
 *    MDD   06-18-91   Added err_message_ptr input and code           *	
 *    KLM   06-18-91   Changed lab_find_object_level to               *
 *                     lu_find_object_level.                          *
 *    MDD   07-02-91   Added pds_line_offset to the line number       *
 *                     printed next to each object on the tree        *
 *    DPB   09-16-91   Added initial values to character string       *
 *                     declarations.                                  *
 *    DPB   09-27-91   Moved into lv_tool.c                           *
 *    MDD   10-21-91   Fixed free calls                               *
 *    MDD   12-10-91   Added check for NULL file pointer              *
 *    MDD   03-18-92   The great int -> long conversion               *
 *    MDD   06-22-92   Removed pds_line_offset                        *
 *    DWS   08-22-01   Added code to report duplicate ojbect/group    *
 *                     names                                          *
 *	  MDC   11-07-02   Added a check to ignore "PERSONNEL" keywords   *
 *					   when checking for duplicate keywords since	  *
 *					   they are not errors.							  *
 *	  MDC   12-16-02   Added code to deallocate a STRING_LIST node if *
 *					   a duplicate has been detected in the dup_names *
 *					   link list. This prevents printing out the same *
 *					   error over and over. i.e. "Object A is used 8" *
 *					   times, "Object A is used 7 times"...."Object A *
 *					   is used 2 time."								  *
 *																	  *
 **********************************************************************/

void rpt_print_tree (rpt_file_pointer, odl_tree_pointer, err_message_ptr)
                                                       
FILE *rpt_file_pointer;
AGGREGATE odl_tree_pointer;
ERROR_LIST *err_message_ptr;
 

{
 AGGREGATE current_ptr = {NULL};
 int status = {PDS_ERROR};                                   
 long num_blanks = 0;
 char *parm_name = {NULL};
 
 
 STRING_LIST *dup_names = {NULL};  /*for a list of all group and object names*/
 STRING_LIST *dup_names_2 = {NULL};/*working copy                            */
 STRING_LIST *dup_names_3 = {NULL};/*another working copy                    */
 int dup_counter = 0;              /*count of the number of times a name has been used*/
 int first_dup = 0;                /*first time flag for a dup header in the report*/

 STRING_LIST *temp = {NULL}; /* 12-16-02 Added by MDC */

 /*----------*/
/** BEGIN  **/
/*----------*/

 if (rpt_file_pointer != NULL)
 {
   /*------------------------------------------------------------*/
   /** IF there are objects on the tree THEN                    **/
   /**    write the header to the report file                   **/
   /*------------------------------------------------------------*/

   if (odl_tree_pointer != NULL)

   {
      fprintf (rpt_file_pointer, 
      "\n------------------------------------------------------------------------\n");
      fprintf (rpt_file_pointer, 
      "*****          The hierarchy of objects in this label is:          *****\n");
      fprintf (rpt_file_pointer, 
      "------------------------------------------------------------------------\n\n");

      /*---------------------------------------------------------*/
      /** LOOP through the objects                              **/
      /**    get the object level for proper indentation        **/
      /**    get the name of the name of the object             **/
      /*---------------------------------------------------------*/

      for (current_ptr = odl_tree_pointer -> first_child; current_ptr != NULL;
           current_ptr = NextAggregate (current_ptr))
      {

         for (num_blanks = ((long) lu_find_object_level (current_ptr) * 3); 
                    num_blanks > 0; num_blanks--)
      
         {
            fprintf (rpt_file_pointer, " ");
         }

         parm_name = lab_get_value (current_ptr, NULL, NULL, 1, 
                                        "NAME", 0, 1, FALSE, &status);
         
         /*------------------------------------------------------------*/
         /** IF object name and it's parameter name are the same THEN **/
         /**    write just the object name to the report file         **/  
         /*------------------------------------------------------------*/

         if (status == PDS_ERROR || strcmp (current_ptr->name, parm_name) == 0)
         {
            fprintf (rpt_file_pointer, "%s (line %ld)\n", 
                     current_ptr->name, current_ptr->appl1);
			
			/*------------------------------------------------------*/
			/* 12-16-02 MDC											*/
			/* Added a check to see if the current node is an OBJECT*/
			/* or GROUP. If it is a GROUP, then we need to make this*/
			/* part of the link list to check for duplicate names.	*/
			/*------------------------------------------------------*/
			if(current_ptr->kind == KA_GROUP)
                dup_names = util_append_string_list (dup_names, current_ptr->name, STRING_TYPE);
         }

         /*------------------------------------------------------------*/
         /** ELSE                                                     **/
         /**   write the object name and class to file                **/
         /*------------------------------------------------------------*/
         else
         {
            fprintf (rpt_file_pointer, "%s - %s (line %ld)\n", 
                     current_ptr->name, parm_name, current_ptr->appl1);
         }
         /*------------------------------------------------------------*/
         /** ENDIF object name and its parameter...                   **/
         /*------------------------------------------------------------*/

         Lemme_Go(parm_name);
      }
      /*---------------------------------------------------------*/
      /** ENDLOOP through the objects...                        **/
      /*---------------------------------------------------------*/
   }
   /*------------------------------------------------------------*/
   /** ENDIF there are objects on the tree...                   **/
   /** check for duplicate object/group names                   **/
   /*------------------------------------------------------------*/

    dup_names_2 = dup_names;                 /*set up a working copy of the dup_names string list   */
	                                         /*loop through the list of object and group names      */
	                                         /*looking for dups                                     */
/*    for (; dup_names_2 != NULL, dup_names_2->next != NULL; dup_names_2 = dup_names_2->next) */

    for(; ( (dup_names_2 != NULL) && (dup_names_2->next != NULL) ); dup_names_2 = dup_names_2->next )
	{
		dup_names_3 = dup_names_2->next;     /*set up one more working copy of dup_names            */
		
		/*-------------------------------------------------------------*/
		/** 11-07-02 MDC											  **/
		/** Added a check to look for the keyword "PERSONNEL". If it  **/
		/** is found, we don't want to say this is an error, because  **/
		/** it is not. So just continue to the next object over.	  **/
		/*-------------------------------------------------------------*/
		if(  strstr(dup_names_2->text, "PERSONNEL") != NULL )
			continue;
		
		/*-------------------------------------------------------------*/
		/* 12-16-02 MDC												   */
		/* Changed to a while loop with the increments of dup_names_3  */
		/* being inside this loop.									   */
		/*-------------------------------------------------------------*/

/*		for (; dup_names_3 != NULL; dup_names_3 = dup_names_3->next) */
		while(dup_names_3 != NULL)
		{                                     /*If they are the same count it as a dup               */
			if(strcmp(dup_names_2->text, dup_names_3->text) == 0)
			{
				dup_counter++;
				
				/*------------------------------------------------------*/
				/* 12-16-02 MDC											*/
				/* If we have found a duplicate, save the node to the	*/
				/* right, remove the current node we are pointing at    */
				/* from the list to prevent reporting the duplicate     */
				/* again and again and copy the right node into			*/
				/* dup_names_3.											*/
				/*------------------------------------------------------*/
				temp = dup_names_3->next;
				deallocate_string_list_node(dup_names_3, &(dup_names));
				dup_names_3 = temp;
			}
			else
				dup_names_3 = dup_names_3->next;
		}		
											  /*if there was a dup and if we haven't printed a header*/
											  /*already then print one                               */
			if((dup_counter != 0) && (first_dup == 0))
			{
				/*-------------------------------------------------------*/
				/* 12-17-02 MDC											 */
				/* We only want to report duplicate GROUP names, not     */
				/* OBJECT names. Corrected the output to reflect this.	 */
				/*-------------------------------------------------------*/
				fprintf (rpt_file_pointer, 
				"\n------------------------------------------------------------------------\n");
	/*			fprintf (rpt_file_pointer, 
				"*****  The following OBJECT/GROUP names are duplicated             *****\n"); */
				fprintf (rpt_file_pointer,
				"*****  The following GROUP names are duplicated                    *****\n");
				fprintf (rpt_file_pointer, 
				"------------------------------------------------------------------------\n\n");
				first_dup = 1;
			}
			if(dup_counter > 0)                /*if there was a dup print the message                 */
			{
				fprintf (rpt_file_pointer, "1    ERROR:   %s is used %d times\n", dup_names_2->text, dup_counter + 1);
				dup_counter = 0;
			}

	}
   util_deallocate_string_list (dup_names);

   /*------------------------------------------------------------*/
   /** write out any pending messages                           **/
   /*------------------------------------------------------------*/
   if (err_message_ptr != NULL) 
   {
      fprintf (rpt_file_pointer, "\n");
      for (; err_message_ptr != NULL; err_message_ptr = err_message_ptr -> next)
         fprintf (rpt_file_pointer, "%s\n", err_message_ptr -> message);

   }
 }
 return;

/*----------*/
/**  END   **/
/*----------*/
}  /*  End: "rpt_print_tree"  */


/**********************************************************************
 *$Component                                                          *
 *    void rpt_semantic_errors (rpt_file_pointer, no_wrap)            *
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
 *    no_wrap:                                                        *
 *      =TRUE do not wrap error messages                              *
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
 *    DWS   05-13-99   Added no_wrap arg.                             *
 **********************************************************************/

void rpt_semantic_errors (rpt_file_pointer, no_wrap)
                                                      
FILE *rpt_file_pointer;
LOGICAL no_wrap;

{

 ERROR_LIST *msg_ptr = {NULL};

 if (rpt_file_pointer != NULL)
 {
  
   if (pds_message_list != NULL)
   {
      fprintf (rpt_file_pointer, 
      "\n------------------------------------------------------------------------\n");
      fprintf (rpt_file_pointer, 
      "*****            The semantic errors in this file are:             *****\n");
      fprintf (rpt_file_pointer, 
      "------------------------------------------------------------------------\n\n");

	  for (msg_ptr = pds_message_list; msg_ptr != NULL; msg_ptr = msg_ptr->next)
	  { 
		
		  if(!no_wrap) 
			  rpt_format_message (rpt_file_pointer, msg_ptr -> message);
          else
			  alt_rpt_format_message (rpt_file_pointer, msg_ptr -> message);
	  }

   }
  
   else
   {

      fprintf (rpt_file_pointer, 
      "\n------------------------------------------------------------------------\n");
      fprintf (rpt_file_pointer, 
      "*****         There were no semantic errors in this file.          *****\n");
      fprintf (rpt_file_pointer, 
    "------------------------------------------------------------------------\n\n");

   }
 }
 return;

}  /*  End: "rpt_semantic_errors"  */


/**********************************************************************
 *$Component                                                          *
 *    void slv_main_footer (rpt_file_ptr)                             *
 *$Abstract                                                           *
 *    Closes a report file.                                           *
 *$Inputs                                                             *
 *    rpt_file_ptr:                                                   *
 *        The rpt_file_ptr variable is a file pointer for the         *
 *        report file.                                                *
 *    use_terminal:                                                   *
 *        The use_terminal variable a TRUE/FALSE flag indicating      *
 *        whether all error messages are displayed to the screen by   *
 *        the label verifier.                                         *
 *$Detailed_Description                                               *
 *    The slv_main_footer routine writes a message indicating the     *
 *    end of a validation report, and closes the report file.         *
 *    Messages are echoed to the screen if the use_terminal flag is   *
 *    set.                                                            *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    slv_odl_errors          lvtool.c                read            *
 *    slv_odl_warnings        lvtool.c                read            *
 *    slv_dd_errors           lvtool.c                read            *
 *    slv_dd_warnings         lvtool.c                read            *
 *$Author_and_Institution                                             *
 *    Kristy L. Marski /JPL                                           *
 *$Change_History                                                     *
 *    1.0   KLM     03-25-91   Original generation.                   *
 *    1.1   DPB     09-27-91   Moved into lv_tool.c                   *
 *    1.2   MDD     12-10-91   Added check for NULL file pointer.     *
 *    2.0   MDD     06-30-93   Adapted for slvtool.                   *
 *          DWS     11-12-97   Moved back into lv_tool, slv_tool is   *
 *                             now an option in lv_tool.              *
 **********************************************************************/

void slv_main_footer (rpt_file_ptr, use_terminal)

FILE *rpt_file_ptr;
LOGICAL use_terminal;

{

  char *date_string;

  if (rpt_file_ptr != NULL)
  {
   
     date_string = sys_get_ascii_date ();

     fprintf(rpt_file_ptr,"\n");
     fprintf(rpt_file_ptr,"************************************************************************\n");
     fprintf(rpt_file_ptr,"*                                                                      *\n");
     fprintf(rpt_file_ptr,"*                ODL Errors: %6ld  ODL Warnings: %6ld              *\n",
                         slv_odl_errors, slv_odl_warnings);
     fprintf(rpt_file_ptr,"*                 DD Errors: %6ld   DD Warnings: %6ld              *\n",
                         slv_dd_errors, slv_dd_warnings);
     fprintf(rpt_file_ptr,"*                                                                      *\n");
     fprintf(rpt_file_ptr,"*              This is the end of the Validation Report                *\n");
     fprintf(rpt_file_ptr,"*                                                                      *\n");
     fprintf(rpt_file_ptr,"*                      %s", date_string);
     fprintf(rpt_file_ptr,"                        *\n");
     fprintf(rpt_file_ptr,"*                                                                      *\n");
     fprintf(rpt_file_ptr,"************************************************************************\n");

     fclose (rpt_file_ptr);
     Lemme_Go(date_string);
  }
  if (use_terminal)
  {
     printf ("\n      Total ODL Errors: %6ld  ODL Warnings: %6ld\n",
             slv_odl_errors, slv_odl_warnings);
     printf ("      Total DD Errors:  %6ld   DD Warnings: %6ld\n",
             slv_dd_errors, slv_dd_warnings);
  }
  return;
}



/**********************************************************************
 *$Component                                                          *
 *    void slv_odl_messages ()                                        *
 *$Abstract                                                           *
 *    Counts number of ODL messages                                   *
 *$Detailed_Description                                               *
 *    This routine counts the number of syntax errors and warning     *
 *    messages on the global list.  Then it clears the list. The      *
 *    counters are globals.                                           *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    pds_message_list        pdsglob.h                read           *
 *    slv_file_odl_errors     slvtool.c                write          *
 *    slv_file_odl_warns      slvtool.c                write          *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/JPL                                             *
 *$Change_History                                                     *
 *    1.0   MDD     07-15-93   Original code.                         *
 *          DWS     11-12-97   Moved back into lv_tool, slv_tool is   *
 *                             now an option in lv_tool.              *
 **********************************************************************/

void slv_odl_messages ()
{
   ERROR_LIST *msg_ptr = {NULL};
 
   for (msg_ptr = pds_message_list; msg_ptr != NULL; msg_ptr = msg_ptr->next)
      if (util_locate_substring (msg_ptr -> message, "ERROR"))
          slv_file_odl_errors++;
      else if (util_locate_substring (msg_ptr -> message, "WARNING"))
          slv_file_odl_warns++;

   if (slv_file_odl_warns)
      slv_file_odl_warns--;  /* Subtract off one for termination message */
   lab_clear_messages (); 
   return;

}  /*  End: "slv_odl_messages"  */



/**********************************************************************
 *$Component                                                          *
 *    void slv_alias_messages ()                                      *
 *$Abstract                                                           *
 *    Counts number of ODL messages                                   *
 *$Detailed_Description                                               *
 *    This routine counts the number of keyword and object            *
 *    messages on the global list.  Then it clears the list. The      *
 *    counters are globals.                                           *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    pds_message_list        pdsglob.h                read           *
 *    slv_file_aliases        slvtool.c                write          *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Change_History                                                     *
 *    1.0   MDD     07-15-93   Original code.                         *
 *          DWS     11-12-97   Moved back into lv_tool, slv_tool is   *
 *                             now an option in lv_tool.              *
 **********************************************************************/

void slv_alias_messages ()
{
   ERROR_LIST *msg_ptr = {NULL};
   for (msg_ptr = pds_message_list; msg_ptr != NULL; msg_ptr = msg_ptr->next)
         slv_file_aliases++;  
   lab_clear_messages ();
   return;

}  /*  End: "slv_alias_messages"  */


/**********************************************************************
 *$Component                                                          *
 *    void slv_semantic_errors ()                                     *
 *$Abstract                                                           *
 *    Counts number of ODL messages                                   *
 *$Detailed_Description                                               *
 *    This routine counts the number of semantic keyword and object   *
 *    messages on the global list.  Then it clears the list. The      *
 *    counters are globals.                                           *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    pds_message_list        pdsglob.h                read           *
 *    slv_file_dd_warns       slvtool.c                write          *
 *    slv_file_dd_errors      slvtool.c                write          *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Change_History                                                     *
 *    1.0   MDD     07-15-93   Original code.                         *
 *          DWS     11-12-97   Moved back into lv_tool, slv_tool is   *
 *                             now an option in lv_tool.              *
 **********************************************************************/

void slv_semantic_errors ()
{
  ERROR_LIST *msg_ptr = {NULL};

  for (msg_ptr = pds_message_list; msg_ptr != NULL; msg_ptr = msg_ptr->next)
       if (util_locate_substring (msg_ptr -> message, "ERROR"))
           slv_file_dd_errors++;
       else if (util_locate_substring (msg_ptr -> message, "WARNING"))
           slv_file_dd_warns++; 
  lab_clear_messages ();

  return;

}  /*  End: "slv_semantic_errors"  */


/**********************************************************************
 *$Component                                                          *
 *   STRING_LIST *slv_get_volume_list (dir_mask, use_di)              *
 *$Abstract                                                           *
 *    Make a list of file names.                                      *
 *$Inputs                                                             *
 *    dir_mask:                                                       *
 *        The dir_mask variable is a character string that contains   *
 *        a system specific file specification, including wildcards.  *
 *    use_di:                                                         *
 *        If true this variable will prevent searching subdirectories *
 *        for label files to be verified                              *
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
 *    the UNIX and MSDOS cases.  The VMS case must be changed before  *
 *    this will work for it.                                          *
 *                                                                    *
 *$Error_Handling                                                     *
 *   The routine will return NULL if it finds no files that match     *
 *   the dir_mask or if it cannot obtain a directory listing.         *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/JPL                                             *
 *$Change_History                                                     *
 *    1.0   MDD   07-15-93  Original code.                            *
 *          DWS   11-12-97  Moved back into lv_tool, slv_tool is      *
 *                          now an option in lv_tool.                 *
 *          DWS   12-12-98  Added a directory walk for DOS            *
 *          MDC   01-26-06  Modified code for unix/linux systems. When*
 *                          there is no directory found in dir_mask,  *
 *                          call getcwd function to get the current   *
 *                          working directory so that we don't compile*
 *                          a list of filenames with a "./" or "../"  *
 *                          which causes pointer searching problems   *
 *                          later in the verification.                *
 *          MDC   02-06-06  Surround dir names with quotes when using *
 *                          the find command to search for files. This*
 *                          takes care of directory names with spaces *
 *                          in them.                                  *
 *          MDC   02-15-06  Made minor change to print the message    *
 *                          that it could not find the input file(s)  *
 *                          without counting it as part of the        *
 *                          odl/semantic error messages we're keeping *
 *                          track of during validation.               *
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
   char command_str [PDS_MAXLINE + 1];

   length_1 = 0;
   directory = sys_get_path (dir_mask); /* dir_mask might have c:\test\a.lbl  */
                                        /* directory would then have c:\test\ */
   if (directory == NULL) 
   {
	   cwdret = getcwd(directory, sizeof(directory));
	   directory = cwdret;
   }
   else
   {
       length_1 = (int) strlen(directory);                       /* gets length of path     */
   }
   fmask = dir_mask + length_1;                            /* the file name           */
                                                           /* Do the directory walk   */
                                                           /* and build the file list */
   file_list = dir_walk(directory, fmask, file_list, use_di, FALSE); /*01-15-98*/
   if (file_list == NULL)
     {
	 sprintf(command_str,
		 "\n\n ERROR: No file(s) were found matching the specification %s\n\n",
		   dir_mask);
	 err_append_message(CONTINUE, command_str);
	 }
}
#else
{
  FILE *fp = NULL;
  char command_str [PDS_MAXLINE + 1]={0};
  char data_str [PDS_MAXLINE + 1]={0};
  LOGICAL success = TRUE;
  char* slash = NULL;

  /* 01-26-06 MDC - Added */
  char *directory = NULL;
  char *dir_mask_ptr = NULL;
  char *full_dir_name = NULL;
  char *dir_mask_copy = NULL;

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

   printf("command string is %s\n", command_str);
   success = sys_do_command (command_str);
/*   success = TRUE; */
   if (success != TRUE)
   {
	   sprintf (command_str, "ERROR: Unable to get list of files specified by: %s", 
                             dir_mask);
	  /* 02-14-06 MDC - Pass in a CONTINUE instead of ERROR so we don't count
	     this type of message as part of the actual ODL/semantic error messages
		 we need to keep track of.
	  */
/*      err_append_message (ERROR, command_str);   */
	  err_append_message (CONTINUE, command_str);
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

 /*             printf("file name is %s\n", &file_list -> text); */
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
/** END slv_get_volume_list **/
}

#ifdef SUN_UNIX
/**********************************************************************

 Component
    void interpret_unix_parent_dir_symbol(file_name)
 Abstract
    Interpret the meaning of the parent directory symbol, "../", in the
	file_name
 Inputs
	char *file_name
 Returns
    Manipulates the directory variable.
 Detailed Description
    This routine interprets the parent symbol(s), "../", found in 
	file_name. dir is assumed to be the current directory and the 
	file_name is assumed to be a file located x number of levels above
	dir.
 Author and Institution
    Michael Cayanan/JPL
 Change History
    01-26-06  MDC   Original Code

**********************************************************************/
char *interpret_unix_dir_symbol(char *file_name)
{
	char *slash_ptr = NULL;
	char *temp_ptr = NULL;
	int num_of_levels = 0;
	int i =0;
	int length = 0;
	char *absolute_path = NULL;
	char *file_name_copy = NULL;
	char *directory = NULL;
	

	New_String(file_name_copy, file_name);
	Malloc_String(directory, PDS_MAXLINE);

	/* If we have a "../" in our filename...*/
	if( (strstr(file_name_copy, "../")) == file_name_copy )
	{
        /* First count the number of "../" marks in the file name. */
		for(slash_ptr = file_name_copy; slash_ptr != NULL;)
		{
			slash_ptr = strstr(slash_ptr, "../");
			if(slash_ptr != NULL)
			{
                ++num_of_levels;
				/* We need this to ultimately point to everything after the
				   parent directory symbols. i.e. If we have ../../images/label.lbl,
				   we need temp_ptr to point to just /images/label.lbl so we can
				   properly form the absolute path later.
				   We're keeping the '/' in front for ease of the path creation.
			    */
				temp_ptr = slash_ptr + 2;
				slash_ptr = slash_ptr + 3;
			}
		}

		/* Get the current working directory */
		if(getcwd(directory, PDS_MAXLINE) != NULL)
		{
			/* We must start chopping off directory levels to match with
			   the number of levels we've determined */
            for(i = 0; i < num_of_levels; i++)
			{
				slash_ptr = strrchr(directory, '/');
				*slash_ptr = EOS;
			}
			/* Create the absolute path */
			length = String_Size(directory) + String_Size(temp_ptr);
			Malloc_String(absolute_path, length);
			strcpy(absolute_path, directory);
			strcat(absolute_path, temp_ptr);
			Lemme_Go(directory);
		}
		else
		{
			Lemme_Go(file_name_copy);
			Lemme_Go(directory);
			return NULL;
		}
	}
	/* If we encounter the "./" symbol, we need to get the current working directory */
	else if( strstr(file_name_copy, "./") == file_name_copy )
	{
		if(getcwd(directory, PDS_MAXLINE) != NULL)
		{
			/* Point to the file name with the slash at the beginning*/
			slash_ptr = strrchr(file_name_copy, '/');
		
			/* Create the absolute path */
			length = String_Size(slash_ptr) + String_Size(directory);
			Malloc_String(absolute_path, length);
			strcpy(absolute_path, directory);
			strcat(absolute_path, slash_ptr);
			Lemme_Go(directory);
		}
		else
		{
			Lemme_Go(file_name_copy);
			Lemme_Go(directory);
			return NULL;
		}
	}
	else
	{
		Lemme_Go(file_name_copy);
		Lemme_Go(directory);
		return NULL;
	}

	return(absolute_path);

}
#endif
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
/**********************************************************************
 *$Component                                                          *
 *    void explab (input_label, output_label)                         *
 *$Abstract                                                           *
 *    Main program for the expand label utility                       *
 *$Keywords                                                           *
 *    DRIVER                                                          *
 *    TOOL_MAIN                                                       *
 *$Inputs                                                             *
 *    input_label:                                                    *
 *       The input_label variable is a character string containing the*
 *       name of a label to be used as the input label in any label   *
 *       processing task.                                             *
 *    output_label:                                                   *
 *       The output_label variable is a character string containing a *
 *       name of a label to be used as the input label in any label   *
 *       processing task.                                             *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    1 There were one or more expansions performed                   *
 *    2 There were recursive expansions performed                     *
 *    3 Input file was not found                                      *
 *    4 Input file was not a PDS LABEL                                *
 *    5 Output file could not be written                              *
 *$Detailed_Description                                               *
 *    The expand label utility reads a PDS label, recursively         *
 *    includes all included labels by following the STRUCTURE         *
 *    references in the label, and writes out the expanded label.     *
 *    It also reports syntax errors found in any of the label files.  *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/JPL                                             *
 *$Version_and_Date                                                   *
 *    1.1   September 10, 1992                                        *
 *$Change_history                                                     *
 *    MDD   05-20-92   Original generation.                           *
 *    MDD   09-10-92   Added warning in even of syntax errors         *
 *    MDC   02-16-06   Made changes to improve the lvtool reporting   *
 **********************************************************************/
int expand_label (rpt_file_ptr, input_label, output_label)

FILE *rpt_file_ptr;
char *input_label;
char *output_label;
{
  LOGICAL success = FALSE;
  LOGICAL success2 = FALSE;
  AGGREGATE label_ptr = NULL;
  int res = 0;
  /*-----------------------------------------------------------------------*/
  /** Setup toolbox files                                                 **/
  /** IF number of arguments is correct THEN                              **/
  /*-----------------------------------------------------------------------*/
/*  lab_setup();*/
  /*-----------------------------------------------------------------------*/
  /** Read the input label and expand it                                  **/
  /** IF it could be read THEN                                            **/
  /**    remove all STRUCTURE keywords                                    **/
  /**    write out the expanded label                                     **/
  /*-----------------------------------------------------------------------*/
/*
  if (rpt_file_ptr != NULL)
  {
	  fprintf (rpt_file_ptr,
  "\n\n------------------------------------------------------------------------\n");
      fprintf (rpt_file_ptr, 
		  "Expanding ^STRUCTURE pointers in %s\n\n", input_label);
  }
  else
  {
	  printf ("Expanding ^STRUCTURE pointers in %s\n\n", input_label);
  }
*/
  label_ptr = lt_read_expanded_label (input_label);
  if (label_ptr != NULL) 
  {
     success = lt_global_keyword_change (label_ptr, "STRUCTURE", PDS_LAB_REMOVE, 
                                   NULL, NULL);

	 /* 02-16-06 MDC - Moved msg that tells user we're expanding ^STRUCTURE pointers to
	        here so that it only prints out when we've actually found a STRUCTURE pointer
			in the label. This prevents LVTOOL from printing out this msg regardless and
			confusing the user.
	 */
	 /* if not successfull then the keyword was not removed, probably it was not found*/
     if(success)                                               /*030298*/
	 {
         if (rpt_file_ptr != NULL)
		 {
             fprintf (rpt_file_ptr,
						"\n\n------------------------------------------------------------------------\n");
			 fprintf (rpt_file_ptr, 
							"Expanding ^STRUCTURE pointers in %s\n\n", input_label);
		 }
		 else
		 {
             printf ("Expanding ^STRUCTURE pointers in %s\n\n", input_label);
		 }
         success2 = lab_write_label_or_template (label_ptr, DEFAULT_REC_TYPE,
                                                0, output_label);
	 /* If not successfull then the file was not written, make an error message*/
	 /* and use the unexpanded file                                            */
	 }
  }
    
  /*-----------------------------------------------------------------------*/
  /** ELSE                                                                **/
  /**    notify user file could not be read                               **/
  /*-----------------------------------------------------------------------*/

  else
  {
	  if (rpt_file_ptr != NULL)
          {
           fprintf (rpt_file_ptr, 
			   "Input label %s could not be found or is not a PDS label\n\n", 
               input_label);
          }
	  else
	      {
           printf ("Input label %s could not be found or is not a PDS label\n\n", 
               input_label);
	      }
  }
  /*-----------------------------------------------------------------------*/
  /** ENDIF the label could be read...                                    **/
  /** write out messages                                                  **/
  /** print success message, if needed, and clean up                      **/
  /*-----------------------------------------------------------------------*/
  
  /* 02-16-06 MDC - Check for a success flag first before printing out the msgs found.
           If we haven't expanded the file, it makes no sense to print out the ODL msgs
		   when we're going to see the same thing later.
  */
  if(success)
  {
      if(rpt_file_ptr != NULL)
	  {
          err_write_to_file (rpt_file_ptr, TRUE);                        /*11-19-97*/
	  }
	  /* 02-16-06 MDC - When printing the report to the screen, you do not need to call this
	       function anymore since we've already printed it out earlier.
	  */
  /*
      else
	  {
          lab_print_messages ();
	  }
  */
  }

  if (success && success2 && !lab_has_messages ())
  {
	  if(rpt_file_ptr != NULL)
         {
         fprintf (rpt_file_ptr,
		      "SUCCESS! Converted file %s was written to file %s\n\n",
	          input_label, output_label);
         }
      else
	     {
         printf ("SUCCESS! Converted file %s was written to file %s\n\n",
	          input_label, output_label);
	     }
  }
  else if (success && success2 && lab_has_messages ())
  {
	  if(rpt_file_ptr != NULL)
	     {
         fprintf (rpt_file_ptr,
			 "\nWARNING! Converted file %s was written to file %s,\n", 
		     input_label, output_label);
         fprintf (rpt_file_ptr,
			 "         but syntax errors were found in input labels.\n\n");
	     }
	  else
	     {
         printf ("\nWARNING! Converted file %s was written to file %s,\n", 
		     input_label, output_label);
         printf ("         but syntax errors were found in input labels.\n\n");
	     }
  }
  else if (success && !success2)
  {
 	 if(rpt_file_ptr != NULL)
	    {
        fprintf (rpt_file_ptr,
			"\nERROR! Output file was not written. Unexpanded label will be used.\n\n");
	    }
	 else
	    {
		 printf ("\nERROR! Output file was not written. Unexpanded label will be used.\n\n");
	    }
  }
  /* 02-16-06 MDC - No need for this statement anymore. */
/* 
  else if (!success)
  {
	  if(rpt_file_ptr != NULL)
         {
         fprintf (rpt_file_ptr,
		      "STUCTURE keyword not found, label was not expanded\n\n");
         }
      else
	     {
         printf ("STRUCTURE keyword not found, label was not expanded\n\n");
	     }
  }
*/
 /* lab_clear_messages ();
  lab_remove_label_or_template (label_ptr);
  lab_exit();*/
  if (success) res = 1;
  if (success2) res = res + 1;
  return(res);

/** END **/
}

/*************************************************************************/
/*$Component                                                             */
/*    void make_out_file(input_file_name, rpt_file_ptr)                  */
/*$Abstract                                                              */
/*    Ouput file name creation for call to expand label code.            */
/*$Keywords                                                              */
/*    preprocessor                                                       */
/*    expand_label                                                       */
/*$Inputs                                                                */
/*    input_file_name:                                                   */
/*       The name of the label file to be verified.  This is also the    */
/*       name the label file to be expanded.                             */
/*    rpt_file_ptr:                                                      */
/*       The pointer to the report file.  Here it will only be used to   */
/*       write one error message.                                        */
/*$Outputs                                                               */
/*    None                                                               */
/*$Returns                                                               */
/*    a one indicates a file was created by make_out_file                */
/*$Detailed_Description                                                  */
/* make_out_file uses the input file name to create an output file.      */
/* The input file will have all ^structure pointers expanded.  The       */
/* expanded data will be written to the out put file.  If the new name is*/
/* already in use the program will display a message to stating that     */
/* LVTOOL will continue without expanding the file.  The file name       */
/* created will be the name of the input file with a new extension of    */
/* "xnn"  (nn is a two digit number between 0 and 99).  The error message*/
/* will also be placed in the report file.                               */
/*$External_References                                                   */
/*    None                                                               */
/*$Author_and_Institution                                                */
/*    Dale W. Schultz/JPL                                                */
/*$Change_history                                                        */
/*    DWS   11-17-97   Original generation.                              */
/*    DWS   07-14-98   moved some of original code to a new function     */
/*                     make_out_file1.  This was part of a fix to make   */
/*                     and delete files properly.                        */
/*    MDC   05-04-04   Made change to look for the last occurrence of a  */
/*                     period in an input file name instead of the first.*/
/*    MDC   11-12-04   Made a bug fix to free space to a string stored   */
/*                     in the file list before storing the expanded file */
/*                     name.                                             */
/*************************************************************************/
int make_out_file(input_file_name,  rpt_file_ptr)
FILE *rpt_file_ptr;
STRING_LIST *input_file_name;
{
char  file_name_out[MAX_PATH_LEN];
char  file_name_out1[MAX_PATH_LEN];
char *directory = {NULL};
int   prd_chr = '.';
char *prd_ptr;
char  new_ext[4];
char  new_ext_1[3];
int   period_pointer;
int   ext_ctr = 0;
int   good_switch = 0;
int   explab_ret = 0;
int   make_file_ret = 0;
int   count_1;

#ifdef SUN_UNIX
	char *absolute_file_name = NULL;
#endif

   file_name_out[0] = '\0';
   file_name_out1[0] = '\0';

   /* 05-04-04 MDC - Changed to look for the last occurrence of the period. This change
      came about because when a user put in a file name like so, "../file_name.lbl", lvtool
	  would make a name like ".x00" instead of "file_name.x00". This change fixes this bug.
   */

/*   prd_ptr = strchr(input_file_name -> text, prd_chr); *//*find out where the              */
   prd_ptr = strrchr(input_file_name -> text, prd_chr); 
                                                       /*period is in the input file name*/
   period_pointer = prd_ptr - input_file_name -> text + 1; /*get the address             */
                                                       /*of the period.                  */
   strcat(file_name_out, input_file_name -> text);     /*get everything up to            */
                                                       /*and including the period.       */
   
   
   for (count_1 = 0; count_1 <100; count_1 ++)
   {
		file_name_out[period_pointer] = '\0';               /*terminate the string            */
		new_ext[0] = 'x';
		new_ext[1] = '\0';
		sprintf(new_ext_1, "%.2d", count_1);
		strcat(new_ext, new_ext_1);
		strcat(file_name_out, new_ext);                     /*add the new extension.          */
		good_switch = make_out_file_1(file_name_out, file_name_out1);
		if(good_switch == 1) count_1 = 100;
   }
   
   
   
   
   
   if (good_switch)
   {
/*
#ifdef SUN_UNIX
	   if( ((strstr(input_file_name->text, "./")) == (input_file_name->text)) || 
		   ((strstr(input_file_name->text, "../")) == (input_file_name->text)) )
	   {
		   absolute_file_name = interpret_unix_dir_symbol(input_file_name->text);

           if(absolute_file_name != NULL)
               explab_ret = expand_label(rpt_file_ptr, absolute_file_name, file_name_out1);
           else
		   {
               printf("Problems converting relative to absolute path: %s\n", input_file_name->text);
			   printf("Continuing validation as is...\n");
			   explab_ret = expand_label(rpt_file_ptr, input_file_name->text, file_name_out1);
		   }
	   }
	   else
		   explab_ret = expand_label(rpt_file_ptr, input_file_name->text, file_name_out1);
#else
*/
                                                       /*expand the label.               */
	   explab_ret = expand_label(rpt_file_ptr, input_file_name -> text, file_name_out1);
	                                                   /*change the name of the input    */
	                                                   /*file so that lvtool will use    */
	                                                   /*the expanded file for the check.*/
	                                                   /*copy in the new name if exp lab */
                                                       /*made it                         */
/*#endif*/
	   if (explab_ret == 2)                            
	   {
		   /* 11-12-04 MDC - Free the allocated space first before initializing the old string
		      with a new one.
		   */
 /*           strcpy(input_file_name -> text, file_name_out1); */
		    Replace_String(input_file_name -> text, file_name_out1);
	        make_file_ret = 1;   
	   }
	   else
	   {
/*		   DeleteFile(file_name_out1);*/
		   remove(file_name_out1);
	   }
   }
   else
   {                                                   /*The file existed. Don't write   */
	                                                   /*over it.  Tell anyone watching  */
	                                                   /*the monitor and attempt to put  */
	                                                   /*the message in the error file.  */
       printf("Output file %s  already exists or could not be created.\n", file_name_out);
	   printf("LVTOOL will continue without expanding %s\n", input_file_name -> text);
	   if(rpt_file_ptr != NULL)                        
	   {
           fprintf(rpt_file_ptr, 
			   "Output file %s  already exists or could not be created.\n", file_name_out);
		   fprintf(rpt_file_ptr,    
			   "LVTOOL will continue without expanding %s\n", input_file_name -> text);
	   }
   }
   return(make_file_ret);
}

/*************************************************************************/
/*$Component                                                             */
/*    int make_out_file1(new_file, file_name_out)                        */
/*$Abstract                                                              */
/*    Create a specified file, check for existance.                      */
/*$Keywords                                                              */
/*    preprocessor                                                       */
/*    expand_label                                                       */
/*$Inputs                                                                */
/*    new_file:                                                          */
/*       The name of the file to be created                              */
/*    file_name_out:                                                     */
/*       The complete path of the file created.                          */
/*$Outputs                                                               */
/*    file_name out                                                      */
/*$Returns                                                               */
/*    Indicates file successfully created.                               */
/*$Detailed_Description                                                  */
/* make_out_file_1 creates a file with the name in new_file.  If the file*/
/* already exists the function returns a zero.  If the the name does not */
/* already exist then the program attempts to open the file.  If it is   */
/* successful the function returns a one.  If the file can not be opened */
/* the function attempts to open the file in the CWD.  Again if it       */
/* succeedes it returns a one, if not it returns a zero.                 */
/*$External_References                                                   */
/*    None                                                               */
/*$Author_and_Institution                                                */
/*    Dale W. Schultz/JPL                                                */
/*$Version_and_Date                                                      */
/*    1.1   July 14, 1998                                                */
/*$Change_history                                                        */
/*    DWS   07-14-98   Original generation.                              */
/*************************************************************************/

int make_out_file_1(new_file, file_name_out)
char *new_file;
char *file_name_out;

{
char *cwdret;
char *directory = {NULL};
char *fmask;
char  file_name_out1[MAX_PATH_LEN];
FILE *test_file;
int   good_switch = 0;
int   length_1;

	file_name_out1[0] = '\0';							/*Null terminate the string        */
	test_file = fopen(new_file, "r");                   /*try to open the file, hope not   */
	if (test_file == NULL)                              /*did it open?                     */
	{                                                   /*no, our name is good             */
		test_file = fopen(new_file, "w");               /*Now make sure we can open one.   */
						                                /*If not we may be using a cd if so*/
					                                    /*we will try to open the file in  */
				                                        /*the working directory.           */
		if (test_file == NULL)
		{                                               /*The file could not be created   */
			                                            /*in the directory the label is in*/
				                                        /*Try to create it in the cwd.    */
					                                    /*Get the cwd.                    */
		    cwdret = getcwd(directory, MAX_PATH_LEN);
		    length_1 = 0;
		    directory = sys_get_path (new_file);        /*get the path, for its length    */
		    length_1 = (int) strlen(directory);               /*get the length so the name can  */
	        fmask = new_file + length_1;                /*be isolated.                    */
		    strcpy(file_name_out1, cwdret);
#ifdef MSDOS_TC
			strcat(file_name_out1, "\\");
#else
			strcat(file_name_out1, "/");
#endif
			strcat(file_name_out1, fmask);              /*add the file name               */
		    test_file = fopen(file_name_out1, "r");      /*Now make sure it isn't already  */
			                                            /*here.                           */
			if (test_file == NULL)                      /*There isn't one, now we can     */
			{                                           /*continue                        */
				test_file = fopen(file_name_out1, "w");     /*see if the file can be created  */
				if(test_file != NULL)
				{                                           /*file opened, we will use it.    */
					good_switch = 1;
					fclose(test_file);                         
/*#if MSDOS_TC*/
					remove(file_name_out1);
/*#else
					DeleteFile(file_name_out1);
#endif*/
					strcpy(file_name_out, file_name_out1);  /*move the file name.             */
				}
			}

	    }
	    else
	    {
			strcpy(file_name_out, new_file);      /*move the file name.             */
			good_switch = 1;
			fclose(test_file);                         
/*#ifndef MSDOS_TC*/
		    remove(file_name_out);                      /*delete test file*/
/*#else
		    DeleteFile(file_name_out);      */            /*delete test file*/
/*#endif*/
		    good_switch = 1;                            /*file opened ok in the label dir.*/
	    }
	}
	else
	{
	   fclose(test_file);                              /*Since it opened we better close */
	}

return(good_switch);
}

/*********************************************************************/
/*$Component:                                                        */
/*  search_for_pointers(input_label, rpt_file_ptr)                   */
/*                                                                   */
/*$Abstract:                                                         */
/*  Verify that all files that are keywords of pointers exist and    */
/*  that they are in the proper directories                          */
/*                                                                   */
/*$Inputs:                                                           */
/*  input_label is the name of the file to be searched for pointers  */
/*  rpt_file_pointer is a pointer to the report file                 */
/*                                                                   */
/*$Detailed Description:                                             */
/*  Add remarks to the error report that identify this portion of    */
/*  the report.  Call the function which will search out the file    */
/*  names specified by any pointers.                                 */
/*                                                                   */
/*$Version:                                                          */
/*  1.0                                                              */
/*                                                                   */
/*$Author:                                                           */
/* D. Schultz                                                        */
/*                                                                   */
/*$Institution:                                                      */
/* JPL/ACRO                                                          */
/*                                                                   */
/*$Date:                                                             */
/* 1/16/98                                                           */
/*********************************************************************/

void search_for_pointers (input_label, rpt_file_ptr, start_path)

FILE *rpt_file_ptr;
char *input_label;
char *start_path;
{
	AGGREGATE label_ptr = NULL;
    if (rpt_file_ptr != NULL)
      {
	  fprintf (rpt_file_ptr,
        "\n------------------------------------------------------------------------\n\n");
      fprintf (rpt_file_ptr, 
		  "Searching for pointer files in %s\n", input_label);
	  fprintf (rpt_file_ptr,
        "\n           -------------------------------------------------------      \n");
      }
   else
      {
	  printf ("Searching for pointer files in %s\n\n", input_label);
      }

   /*-----------------------------------------------------------------------*/
   /** Get the pointer and file names, verify existance and location       **/
   /** files                                                               **/
   /*-----------------------------------------------------------------------*/

   search_for_files (input_label, rpt_file_ptr, start_path);

   /*-----------------------------------------------------------------------*/
   /** write out messages                                                  **/
   /*-----------------------------------------------------------------------*/
   /*if(rpt_file_ptr != NULL)
   {
	  err_write_to_file (rpt_file_ptr, TRUE);
   }
   else
   {
	  lab_print_messages ();
   }
   lab_clear_messages ();*/
   lab_remove_label_or_template (label_ptr);
   /*lab_exit();*/
}

/*********************************************************************/
/*$Components:                                                       */
/*              search_for_file(label_fname, rpt_file)               */  
/*                                                                   */
/*$Abstract:                                                         */
/*  Verifies the existance of files that are keywords of pointers,   */
/*  it also verifies that the files are where they are supposed to be*/
/*                                                                   */
/*$Inputs:                                                           */
/*   label_fname is the file to be scanned for pointers              */
/*   rpt_file is a pointer to the error file                         */
/*$Outputs:                                                          */
/*  None                                                             */
/*                                                                   */
/*$Returns                                                           */
/*  None                                                             */
/*                                                                   */
/*$Detailed Description:                                             */
/*  Build a tree, get the objects that are pointers, get the keywords*/
/*  for each object, if the keywords are file names go and verify    */
/*  that they exist and are in the correct directory.                */
/*                                                                   */
/*$Version:                                                          */
/*  1.0                                                              */
/*                                                                   */
/*$Author:                                                           */
/* D. Schultz                                                        */
/*                                                                   */
/*$Institution:                                                      */
/* JPL/ACRO                                                          */
/*                                                                   */
/*$Date:                                                             */
/* 1/16/98                                                           */
/*                                                                   */
/*********************************************************************/

void search_for_files(label_fname, rpt_file, start_path)
FILE *rpt_file;
char *label_fname;
char *start_path;

{
    AGGREGATE label_ptr = {NULL};
    AGGREGATE object_ptr = {NULL};
    int i;
	int ii;
	LOGICAL save_watch;
	LOGICAL save_verbose;
	STRING_LIST *structure_fname = {NULL};
	char *directory = {NULL};
	char *cwd;
	
/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Save the previous "end watch" and verbose flags                     **/
    /** Add a message header for this file                                  **/
    /** Save the directory name of the label file                           **/
    /** Construct the main tree from the file name passed in.               **/
    /*-----------------------------------------------------------------------*/

    save_watch = pds_watch_ends;
    save_verbose = pds_verbose;
	pds_verbose = FALSE;
    label_ptr = lab_read_label_or_template (label_fname);
    err_deallocate_list(pds_message_list);
    pds_watch_ends = save_watch;
    pds_verbose = save_verbose;

	if (start_path == NULL)
	{
       directory = sys_get_path(label_fname);
       if (directory == NULL) 
       {
	      cwd = getcwd(directory, PDS_MAXLINE);
          directory = cwd;
       }
	   start_path = directory;
	}
   /*-----------------------------------------------------------------------*/
    /** LOOP through the objects in the main tree.                          **/
    /*-----------------------------------------------------------------------*/

    for (object_ptr = label_ptr; 
             object_ptr != NULL; object_ptr = NextAggregate(object_ptr))
    {
        for(ii = 0; strcmp(pointer_list[ii], "") > 0; ii++)
		{
		   for (i=1; (structure_fname = lu_keyword_values (object_ptr,
			                  pointer_list[ii], i, FALSE,
							  rpt_file, start_path)) != NULL; ++i) ;
		}
	}
    pds_watch_ends = save_watch;
    pds_verbose = save_verbose;
	Lemme_Go(structure_fname);
    /** END **/
}
 

/**********************************************************************
 *$Component                                                          *
 *    char *lu_keyword_values (object_ptr, keyword_name,              *
 *                            keyword_position, specific)             *
 *$Abstract                                                           *
 *    Fetches the value of a keyword.                                 *
 *$Keywords                                                           *
 *    LABUTIL                                                         *
 *    KEYWORD                                                         *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    object_ptr:                                                     *
 *        The object_ptr variable is a pointer to the structure       *
 *        used to represent an object in a PDS label.                 *
 *    keyword_name:                                                   *
 *        The keyword_name variable is a character string             *
 *        which contains the name of a keyword in a PDS label         *
 *        (e.g., the line "SCID = VG1" implies that "SCID" is the     *
 *        keyword name).                                              *
 *    keyword_position:                                               *
 *        The keyword_position variable is an integer which           *
 *        represents the relative position of a keyword in an         *
 *        object in a PDS label.  If this variable is used in         *
 *        conjunction with the keyword_name variable, then it         *
 *        represents a particular occurrence of that keyword          *
 *        in the object (e.g., if keyword_name is "SCID" and          *
 *        keyword_position is 2, then this represents the second      *
 *        "SCID" keyword in the object).  On the other hand, if       *
 *        this variable is used by itself, it represents the absolute *
 *        position of the keyword within the object, starting with    *
 *        first keyword in the object (position = 1).                 *
 *    specific:                                                       *
 *        The specific variable is a flag which indicates whether     *
 *        or not to search for a specific keyword or a class of       *
 *        keywords.                                                   *
 *$Outputs                                                            *
 *    value_list contains the keyword values for the keyword          *
 *        contained in keyword_name (which is a class)                *
 *                                                                    *
 *    keyword_ptr_name contains the full name of a keyword matching   *
 *        the class of keyword_name.                                  *
 *                                                                    *
 *   rpt_file_name                                                    *
 *       The rpt_file_name variable is a character string containing  *
 *       the name of the file to be written to.                       *
 *                                                                    *
 *   keyword_ptr->appl1 contains the line number that keyword_ptr_name*
 *       occuppies in the label.                                      *
 *                                                                    *
 *$Returns                                                            *
 *    Keyword_value:                                                  *
 *       The keyword_value variable is a character string which       *
 *       contains the value of a keyword in a PDS label (e.g., the    *
 *       line "SCID = VG!" implies that "VG1" is the value of the     *
 *       "SCID" keyword).
 *                                                                    *
 *$Detailed_Description                                               *
 *    The lu_keyword_values routine searches an object for a keyword  *
 *    and returns a STRING LIST containing all of values in           *
 *    the list.   Unlike most of the "lab" routines, this one does    *
 *    not search from the beginning of the tree to find the object.   *
 *    The 'specific' input indicates whether the keyword being        *
 *    searched for should match the keyword_name input exactly, or    *
 *    only needs to have the same class word.                         *
 *    This function is similar to lu_fetch_keywords.  The differences *
 *    are:                                                            *
 *        the line of code that calls check_the_files.                *
 *        the return is now void                                      *
 *$Error_Handling                                                     *
 *    None                                                            *
 *                                                                    *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.0   December 4, 1991                                          *
 *$Change_History                                                     *
 *    DPB   12-04-91   New routine.                                   *
 *    DWS   02-02-98   Modified lu_fetch_keywords to create           *
 *                     lu_keyword_values                              *
 *    MDC   01-27-06   Modified code to only create lu_keyword_values *
 *                     only if we have a pointer. Could have a kwd in *
 *                     a label called DESCRIPTION, which is different *
 *                     from the ^DESCRIPTION because of the lack of   *
 *                     caret symbol. This latest code fixes this      *
 *                     problem.                                       *
 **********************************************************************/

STRING_LIST *lu_keyword_values (object_ptr, keyword_name, keyword_position,
								specific, rpt_file, start_path)

AGGREGATE object_ptr;
char *keyword_name;
int keyword_position;
LOGICAL specific;
FILE *rpt_file;
char *start_path;
{
    PARAMETER keyword_ptr = {NULL};
    char *value_string = {NULL};
    char *c = {NULL};
    char test_name [PDS_MAXLINE];
    int current_position = {0};
    LOGICAL found = {FALSE};
    STRING_LIST *value_list = NULL;
/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** LOOP through the keywords of the object until the particular one    **/
    /**         is found.                                                   **/
    /*-----------------------------------------------------------------------*/
    found = 0;
    for (keyword_ptr = FirstParameter (object_ptr);
            ((keyword_ptr != NULL) && (! found));
                keyword_ptr = NextParameter (keyword_ptr))
    {
        /*-------------------------------------------------------------------*/
        /** IF we are not looking for a specific keyword, but any keyword   **/
        /**         which is in the class of keywords passed in, THEN       **/
        /**     Extract the class name to be used instead of the keyword    **/
        /**         in subsequent tests.                                    **/
        /** ENDIF                                                           **/
        /*-------------------------------------------------------------------*/

        strcpy (test_name, keyword_ptr -> name);
        if (! specific)
        {
            for (c = String_End(test_name); 
                ((c >= test_name) && ((*c == '_') || (isdigit(*c)))); --c) ;
            *(c + 1) = EOS;
            for ( ; ((c >= test_name) && (*c != '_')); --c) ;
            ++c;
            strcpy (test_name, c);
        }

        /*-------------------------------------------------------------------*/
        /** IF the current keyword, or its class, matches the one passed in **/
        /**     AND the keyword's position matches the one passed in THEN   **/
        /**     Extract the keyword's value from the label.                 **/
        /** ENDIF                                                           **/
        /*-------------------------------------------------------------------*/
		
		/* 01-27-06 MDC - Added another check to see if the current keyword is
		   a pointer itself.
	    */
        if (strcmp (test_name, keyword_name) == 0)
        {
 /*           if ((++current_position) >= keyword_position)*/
			if( ((++current_position) >= keyword_position) && 
										(keyword_ptr->node_kind == KP_POINTER) )
            {
                found = TRUE;
                value_list = lu_fetch_all_values(keyword_ptr, FALSE, FALSE);
		    	check_the_files(value_list, keyword_ptr->name, rpt_file,
					          keyword_ptr->appl1, start_path);
           }
        }

    }  /*  End:  "for (keyword_ptr = ..."  */

    return(value_list);
/** END **/

}  

/*******************************************************************************/
/*$Componant                                                                   */
/*   void check_the_files(struct_ptr, pointer_name, rpt_file, line_nbr)        */
/*                                                                             */
/*$Abstract                                                                    */
/*   Checks list for valid file names and passes them onto rpt_file_messages.  */
/*                                                                             */
/*$inputs                                                                      */
/*   struct_ptr                                                                */
/*       The struct_ptr variable is a pointer to a STRING_LIST containing the  */
/*       name (or names) of files.                                             */
/*   pointer_name                                                              */
/*       The variable pointer_name contians the name of the pointer whose      */
/*       values are the file names pointed to by struct_ptr                    */
/*   rpt_file_name                                                             */
/*       The rpt_file_name variable is a character string containing           */
/*       the name of the file to be written to.                                */
/*   line_nbr                                                                  */
/*       The line_nbr variable is a long containing the line number of         */
/*       file_struct_ptr keyword in the label.                                 */
/*                                                                             */
/*$Outputs                                                                     */
/*   If the input is valid then the four input variables are passed to         */
/*   rpt_file_messages.                                                        */
/*                                                                             */
/*$Returns                                                                     */
/*   None                                                                      */
/*                                                                             */
/*$Detailed Description                                                        */
/*   If there is not an entry in struct_ptr the function returns. The first    */
/*   file name in struct_ptr is validated.  If it is a valid name it is passed */
/*   to rpt_file_messages.  The pointer is moved to the next entry in          */
/*   struct_ptr and check_the_files calls itself.                              */
/*                                                                             */
/*$Error Handling                                                              */
/*                                                                             */
/*$Author and Institution                                                      */
/*   Dale Schultz  /JPL/ACRO                                                   */
/*$Date                                                                        */
/*   Feb. 02, 1998                                                             */
/*                                                                             */
/*$Change History                                                              */
/*   DWS 02-02-1998                                                            */
/*   MDC 10-05-2005   Changed check length of filename from 12 to 32 and added */
/*                    error msgs to print out to user                          */
/*******************************************************************************/
void check_the_files(struct_pointer, pointer_name, rpt_file, line_nbr, start_path)
STRING_LIST *struct_pointer;
char *pointer_name;
FILE *rpt_file;
long line_nbr;
char *start_path;
{
   STRING_LIST *sub_struct_pointer = NULL;
   STRING_LIST *file_list = {NULL};
   int len;
   char *test;
   
   if (struct_pointer != NULL)
   {
	    /* is it less than 12 bytes long?                                              */
	    /* is char at length - 4 a .?                                                  */
        /* should look like "abcdefgh.ext", basic 8 bytes of file name, 3 of extension.*/
	    /* Also name may be less than 8 chars.                                         */
       len = (int) strlen(struct_pointer -> text);
       if (len <= FILENAME_LENGTH)
	   {
          test = struct_pointer -> text + len - 4;
		  if(*test == '.')
		  {
			 rpt_file_messages(pointer_name, struct_pointer, rpt_file,
				                                     line_nbr, start_path);
             sub_struct_pointer = struct_pointer -> next;
             check_the_files(sub_struct_pointer, pointer_name, rpt_file,
				                                     line_nbr, start_path);
		  }
	   }
   }
   Lemme_Go(sub_struct_pointer);
   Lemme_Go(file_list);
}

/*******************************************************************************/
/*$Componant                                                                   */
/*   void rpt_file_messages(ptr_name, file_struct_ptr, rpt_file, line_nbr      */
/*                                                                             */
/*$Abstract                                                                    */
/*   Searches for specified files                                              */
/*                                                                             */
/*$inputs                                                                      */
/*   ptr_name                                                                  */
/*       The ptr_name variable is a pointer to a string containing the name of */
/*       a PDS keyword.                                                        */
/*   file_struct_ptr                                                           */
/*       The file_struct_ptr variable is a pointer to the structure used to    */
/*       represent an object in a PDS label.                                   */
/*   rpt_file_name                                                             */
/*       The rpt_file_name variable is a character string containing           */
/*       the name of the file to be written to.                                */
/*   line_nbr                                                                  */
/*       The line_nbr variable is a long containing the line number of         */
/*       ptr_name keyword in the label.                                        */
/*                                                                             */
/*$Outputs                                                                     */
/*   Messages are placed in the rpt_file identifying all files pointed to by   */
/*   structures.  The messages will specify if the file was found and the      */
/*   directory in which it was found.  If found in more than one directory that*/
/*   will be indicated.                                                        */
/*                                                                             */
/*$Returns                                                                     */
/*   None                                                                      */
/*                                                                             */
/*$Detailed Description                                                        */
/*   rpt_file_messages will search for the files contained in file_struct_ptr  */
/*   The messages issued will contain the name of the keyword, the line number */
/*   of the keyword in the label, all of the directories in which the file was */
/*   found or a message indicating that the file was not found.                */
/*                                                                             */
/*$Error Handling                                                              */
/*   Checks for memory availability and checks for the existance of rpt_file   */
/*                                                                             */
/*$Author and Institution                                                      */
/*   Dale Schultz  /JPL/ACRO                                                   */
/*$Date                                                                        */
/*   Feb. 02, 1998                                                             */
/*                                                                             */
/*$Change History                                                              */
/*   DWS 02-02-1998                                                            */
/*   MDC 06-07-2005    Call a routine to check for the pointer files in        */
/*                     the upper level directories                             */
/*   MDC 01-30-2006    Modified code to fix a crash in lvtool when the -t flag */
/*   MDC 03-01-2006    Commented out calls to find_pointer_files routine. Not  */
/*                     needed because the -b3 flag takes care of this.         */
/*                                                                             */
/*******************************************************************************/
void rpt_file_messages(ptr_name, file_struct_ptr, rpt_file, line_nbr, start_path)
char * ptr_name;
FILE *rpt_file;
STRING_LIST *file_struct_ptr;
long line_nbr;
char *start_path;
{
	 int line_count = 0;                                            /*12-11-98*/
     STRING_LIST *file_list_ptr = {NULL};
     STRING_LIST *next_file;
#ifdef MSDOS_TC
	 printf("Searching for pointers\n");

	 file_list_ptr = dir_walk(start_path, file_struct_ptr -> text, file_list_ptr, 0, FALSE);
/*	 
	 if(file_list_ptr == NULL)
         file_list_ptr = find_pointer_files(start_path, ptr_name, file_list_ptr, file_struct_ptr->text);
*/
                                                       /* search for the file                */
#else
	 int str_len;
	 char *start_path_end;
     char temp_area[MAX_PATH_LEN];
     char command_str [PDS_MAXLINE + 1];
     if(*start_path != NULL)
	 {
		 str_len = strlen(start_path);
		 start_path_end = start_path + str_len - 1;
		 if (*start_path_end != '/')
		 {
			 sprintf(command_str, "%s/%s", start_path, file_struct_ptr -> text);
		 }
		 else
		 {
             sprintf(command_str, "%s%s", start_path, file_struct_ptr -> text);
		 }
	 }
	 else
	 {
         sprintf(command_str, "%s", file_struct_ptr -> text);
	 }
     file_list_ptr = slv_get_volume_list(command_str, 0);
	 if (file_list_ptr == NULL)
	 {
		 strcpy(temp_area, file_struct_ptr -> text);
	     util_lower_case (temp_area);
         if(*start_path != NULL)
	     {
		     str_len = strlen(start_path);
		     start_path_end = start_path + str_len - 1;
		     if (*start_path_end != '/')
		     {
			     sprintf(command_str, "%s/%s", start_path, temp_area);
		     }
		     else
		     {
                 sprintf(command_str, "%s%s", start_path, temp_area);
		     }
	     }
	     else
	     {
             sprintf(command_str, "%s", temp_area);
	     }
		 file_list_ptr = slv_get_volume_list(command_str, 0);
		 
		 /* 06-07-05 MDC - Last chance to find the pointer files */
/*		 if(file_list_ptr == NULL)
		 {
			 file_list_ptr = find_pointer_files(start_path, ptr_name, file_list_ptr, file_struct_ptr->text);
		 }
*/
	 }
#endif
	 /* 01-27-06 MDC - Add check to see if we have a report file. If not, then we need to make sure
	        user specified to print out to the terminal.
	 */
	 if(rpt_file != NULL)
	 {
         fprintf(rpt_file, "1%s pointer with keyword value of %s at line %d\n", 
		         ptr_name, file_struct_ptr -> text, line_nbr);
		 fprintf(rpt_file, "2   Results of file search :\n");
	                                                   /* file_list_pointer contains a list   */
	                                                   /* of all the directories that contain */
	                                                   /* the specified file.                 */
	 }
	 else if(lvtv.use_terminal)
	 {
		 printf("%s pointer with keyword value of %s at line %d\n",
			    ptr_name, file_struct_ptr -> text, line_nbr);
		 printf("    Results of file search:\n");
	 }

	 line_count = 3;                                   /* 12-11-98*/ 
	 if (file_list_ptr != NULL)
     {
         for (next_file = file_list_ptr; next_file != NULL;
	                                     next_file = next_file -> next)
	     {
             if(rpt_file != NULL)
		     {
                 if(next_file -> text != NULL) 
	             {
                     fprintf(rpt_file,
				            "%i      %s\n", line_count, next_file -> text);/*12-11-98*/
					 line_count++;                                        /*12-11-98*/
                  }
             }
			 /* 01-30-06 MDC - Add condition to print to terminal if option was selected */
			 else if(lvtv.use_terminal)
			 {
				 if(next_file->text != NULL)
				 {
					 printf("      %s\n", next_file->text);
				 }
			 }
         }
	  }
      else
	  {
          if(rpt_file != NULL)
          {
              fprintf(rpt_file,
					"%i       Error File not found: %s \n", line_count, file_struct_ptr -> text);
#ifdef SUN_UNIX
                fprintf(rpt_file,
            "%i       Checked for lower case file name too\n", line_count);
#endif
          }
		  /* 01-30-06 MDC - Add condition to print out error msg if terminal option is being used */
		  else if(lvtv.use_terminal)
		  {
			  printf("       Error File not found: %s \n", file_struct_ptr->text);
#ifdef SUN_UNIX
			  printf("       Checked for lower case file name too\n");
#endif
		  }
      }
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
	int   check_file = 1;  /*if value left at one the file will be processed*/
	int   chk_result;
	int   pos;
	char *ext_ptr;
	char *text_ptr;
	int   str_len;
	int   count;
/*char temp_type[20];*/

	str_len = (int) strlen(temp_list->text);
	text_ptr = temp_list->text;
	ext_ptr = text_ptr;
	for (count= 0; count <= str_len - 3; count++)
	{
		text_ptr = strstr(temp_list->text, "../");
		if(text_ptr == NULL)
		{
			count = str_len + 1;
		}
		else
		{
		    text_ptr = text_ptr + 3;
		    ext_ptr  = text_ptr;
		    count = count + 2;
		}
	}
	pos = (int) strcspn(ext_ptr, ".");
	ext_ptr = ext_ptr + pos + 1;
	chk_result = 0;
/*	temp_type[0] = ',';
	temp_type[1] = '\0';
	strcat(temp_type, ext_ptr);
	strcat(temp_type, ",");
	if(strstr(invalid_list, temp_type) == NULL) chk_result = 1; */ /* no match, process file*/
	if(strstr(invalid_list, ext_ptr) == NULL) chk_result = 1; 
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
/*   MDC 08-08-2003   Changed initialization of count to 0 so that we can look */
/*                    at the very first element in the 2-D array.              */
/*   MDC 08-20-2003   Made routine more robust in checking for directories to  */
/*                    exclude.                                                 */
/*   MDC 05-11-2005   Fixed routine to correctly find out if a file is in one  */
/*                    of the directories that is to be excluded                */
/*******************************************************************************/
int chk_dir_name(invalid_dir, temp_list)
char invalid_dir[][100];
STRING_LIST * temp_list;
{
	int chk_result;
	int count;
	char *dir_ptr;
	char *str_ptr;
	chk_result = 1;  /* no match, process file*/

	/* 08-20-03 MDC
	   Get the directory path of the input file name so that we can exclude
	   that path from the file name that we are going to check. What this
	   will do is allow lvtool from distinguishing sub-directories that
	   have the same name as its parent directory (i.e. c:\images\images). If
	   a user wanted to exclude all files in the sub-directory "images", this
	   code would prevent lvtool from mistakingly exclude all files because
	   "images" happens to also be the name of the parent directory.
	*/
/*	input_dir_ptr = sys_get_path(lvtv.input_file_name);
	input_dir_length = (int) strlen(input_dir_ptr);
	
	dir_ptr = strstr(temp_list->text, input_dir_ptr);	
	dir_ptr += input_dir_length;
*/
	/* 05-11-05 MDC - Just get the path of the input file. */
	dir_ptr = sys_get_path(temp_list->text);

	/* 08-08-03 MDC - Change initialization to 0 */
	for(count = 0; count < NBR_EXCLUD_DIRS; count++)
/*	for(count = 1; count <= NBR_EXCLUD_DIRS; count++) */
	{
		if(invalid_dir[count][0] == '\0')
		{
			count = NBR_EXCLUD_DIRS + 1;  /*That's all there are*/
		}
		else
		{
			str_ptr = invalid_dir[count];
			if(strstr(dir_ptr, str_ptr) != '\0')
			{
				chk_result = 0;              /*Don't do this file!  It is in  */
				count = NBR_EXCLUD_DIRS + 1; /*a forbidden directory. We are  */
			}					             /*done with the search.          */
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
	lcnt = (int) fread (lrec, sizeof(char), LSIZE, lfile);
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
/*      08-08-03    MDC      Modified the routine to chop off the last         */
/*                           character of a string that contains directory     */
/*                           names to exclude only when a newline character    */
/*                           is contained within the string element.           */
/*      03-11-04    MDC      Commented out code that interally converts        */
/*                           extensions and directories to exclude             */
/*******************************************************************************/
void check_exclusion_opts(char invalid_list[], char invalid_dir[][100])
{
      FILE *inv_file_hdl;
	  FILE *inv_dir_hdl;
	  
	  char temp_list[INV_EXT_LIST_LENGTH + 1] = {0};
	  char invalid_list_uc[INV_EXT_LIST_LENGTH + 1] = {0};
	  char invalid_list_lc[INV_EXT_LIST_LENGTH + 1] = {0};
	  char *inval_list_ptr_uc = NULL;
	  char *inval_list_ptr_lc = NULL;

	  int  rec_count;
	  int  count;
	  int  str_len;
	  int upper_case = 0;

	  char *temp_ptr = NULL;

	  char temp_dir[NBR_EXCLUD_DIRS + 1][100] = {0};
	  char invalid_dir_list_uc[NBR_EXCLUD_DIRS + 1][100] = {0};
	  char invalid_dir_list_lc[NBR_EXCLUD_DIRS + 1][100] = {0};
	  char *inval_dir_ptr_uc = NULL;
	  char *inval_dir_ptr_lc = NULL;

	  if(lvtv.inval_file == TRUE)
	  {
		  inv_file_hdl = fopen(lvtv.inval_file_name, "r");
		  if (inv_file_hdl == NULL)
		  {
			  fprintf (lvtv.rpt_file_ptr, "Invalid file type list did not open, file name: %s \n",
		                                                                lvtv.inval_file_name);
              lvtv.inval_file = FALSE;
		  }
		  else
		  {
			  rec_count = (int) fread(temp_list, 1, INV_EXT_LIST_LENGTH, inv_file_hdl);
			  if(ferror(inv_file_hdl))
			  {
				  fprintf (lvtv.rpt_file_ptr, "Invalid file type list had read error, file name: %s \n",
		                                                                lvtv.inval_file_name);
                  lvtv.inval_file = FALSE;
			  }
              else
			  {
				  if (rec_count > INV_EXT_LIST_LENGTH)  rec_count = INV_EXT_LIST_LENGTH;
				  temp_list[rec_count] = '\0';

				  /* Copy the original list first */
				  strcpy(invalid_list, temp_list);
			  }
			  fclose(inv_file_hdl);
		  }
	  }
	  if(lvtv.inval_dir == TRUE)
	  {
		  inv_dir_hdl = fopen(lvtv.inval_dir_name, "r");
		  if (inv_dir_hdl == NULL)
		  {
			  fprintf (lvtv.rpt_file_ptr, "Invalid directory list did not open, file name: %s \n",
		                                                                lvtv.inval_dir_name);
              lvtv.inval_dir = FALSE;
		  }
		  else
		  {
			  /* 08-08-03 MDC - Initialize to "0" */
		/*	  for(count = 1; count <= NBR_EXCLUD_DIRS; count++) */
			  for(count = 0; count < NBR_EXCLUD_DIRS; count++)
			  {

				  if(fgets(temp_dir[count], 101, inv_dir_hdl) == NULL)
				  {
                      if(feof(inv_dir_hdl) != 0)
					  {
                          count = NBR_EXCLUD_DIRS + 1;
					  }
					  if(ferror(inv_dir_hdl) != 0)
					  {
						fprintf (lvtv.rpt_file_ptr, 
							"Invalid directory list had read error, file name: %s \n",
		                                                                lvtv.inval_dir_name);
						lvtv.inval_dir = FALSE;
					  }
				  }
				  else
				  {
                      str_len = (int) strlen(temp_dir[count]);
					
					  /* 08-08-03 MDC - Only chop off the last character if a newline
					     character is contained within the string.
					  */

					  if(strstr(temp_dir[count], "\n") != NULL)
							temp_dir[count][str_len-1] = '\0';

					  /* the following code converts the input string to upper case*/
					
					  /*First, copy the string as is */
					  strcpy(invalid_dir[count], temp_dir[count]);
				 }
			  }
			  fclose(inv_dir_hdl);
		  }
	  }

	  if(lvtv.use_log_file == TRUE)
	  {
		  lvtv.log_file_ptr = fopen(lvtv.log_file_name, "w");
		  if (lvtv.log_file_ptr == NULL)
		  {
			  fprintf (lvtv.rpt_file_ptr, "Log file did not open, file name: %s \n",
		                                                                lvtv.log_file_name);
			  fprintf (lvtv.rpt_file_ptr, "Processing will continue without a log file\n");
              lvtv.use_log_file = FALSE;
		  }
	  }

}
/*******************************************************************************/
/*$Componant                                                                   */
/*	void deallocate_string_list_node(node, list)			                   */
/*                                                                             */
/*$Abstract                                                                    */
/*   Removes a string_list node from a list		                               */
/*                                                                             */
/*$inputs                                                                      */
/*   node		                                                               */
/*       pointer to a string_list node to be removed		                   */
/*   list									                                   */
/*      The link list of string_list nodes that contains the node to be removed*/
/*                                                                             */
/*$Outputs                                                                     */
/*   None                                                                      */
/*                                                                             */
/*$Returns                                                                     */
/*   None								                                       */
/*                                                                             */
/*$Detailed Description                                                        */
/*	This routine will remove a node from the link list of string_list objects. */
/*  It removes a node regardless of whether the node-to-be-removed is in the   */
/*  beginning, middle, or end of a list.									   */
/*                                                                             */
/*$Error Handling                                                              */
/*   None                                                                      */
/*                                                                             */
/*$Author and Institution                                                      */
/*   Michael Cayanan  /JPL	                                                   */
/*$Date                                                                        */
/*   December 16, 2002		                                                   */
/*                                                                             */
/*$Change History                                                              */
/*   MDC 12-16-2002   Original code                                            */ 
/*******************************************************************************/
void deallocate_string_list_node(STRING_LIST *node, STRING_LIST **list)
{
	STRING_LIST *next_ptr = NULL;
	STRING_LIST *prev_ptr = NULL;
	
	/* Grab the next and previous nodes of the node to be removed. */
	next_ptr = node->next;
	prev_ptr = node->prev;
	
	/* If prev_ptr and next_ptr are both NULL, then this node is the 
	   only one in the list 
    */
	if( (prev_ptr == NULL) && (next_ptr == NULL) )
	{
		Lemme_Go(node->text);
		Lemme_Go(node);
		*list = NULL;
		return;
	}

	/* If prev_ptr is NULL, we are at the head of the list */
	else if(prev_ptr == NULL)
	{
		*list = (*list)->next;
		Lemme_Go(node->text);
		Lemme_Go(node);
		(*list)->prev = NULL;        
		return;
	}
	/* If next_ptr is NULL, we are at the end of the list */
	else if(next_ptr == NULL)
	{
		prev_ptr->next = NULL;
		Lemme_Go(node->text);
		Lemme_Go(node);

		return;
	}

	/*-----------------------------------------------------------------------*/
	/* Deallocate the node to be removed and link the nodes to its left and  */
	/* to the right.														 */
	/*-----------------------------------------------------------------------*/
	
	prev_ptr->next = next_ptr;
	next_ptr->prev = prev_ptr;
	
	Lemme_Go(node->text);
	Lemme_Go(node);
}


