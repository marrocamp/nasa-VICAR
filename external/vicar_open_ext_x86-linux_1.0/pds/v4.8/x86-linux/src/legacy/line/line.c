/*************************************************************************
 *  PROGRAM NAME: LINE.c
 *
 *        Author:  R. Joyner  (JPL M/S 525-3610, phone 306-6011)
 *          Date:  19 Nov 1992
 *
 *      Function:  Reads an input file, numbers each line, and 
 *                   writes each numbered line to an output file 
 *                   (maximum of 78 chars).
 *                 Also, matches "OBJECT" and "END_OBJECT" strings.
 *
 *   Version 1.3 Nov.  30, 1999                                            *
 *   Version 1.4 July. 13, 2000  Initialized start_path[0] and added       *
 *                               temp_str_set.  Changed fread to fgets in  *
 *                               main.  Also removed a reset of row_bytes  *
 *                               in process_line.                          *
 *																		   *
 *	 Version 1.7 Dec. 18, 2002   Added a checker in slv_get_volume_list	   *
 *								 routine to make sure that for Solaris     *
 *								 systems, the user-specified search path   *
 *								 is included when forming the command	   *
 *								 string to search for the user-specified   *
 *								 input file(s).							   *
 *								 Added a call in main() to print out the   *
 *								 error messages onto the output file.	   *
 *   Version 1.9 Mar. 25, 2004   Modified line_setup routine. See routine  *
 *                               for further details.                      *
 *   Version 2.0 June 15, 2005   Added Software disclaimer msg. to help    *
 *                                                                         *
 ***************************************************************************/
#include "line.h"
#include "pdsglob.h"
#include "errordef.h"
/**************************************************************************/

/***************************************************************************
 *				MAIN					   
 ***************************************************************************/

main(argc, argv)

int   argc;
char  *argv[];

{
  char      InName[NameLen];		    /* Input file name             */
  FILE *    opf;
  int		i;                          /* counter                     */
  char      buffer[140];
  STRING_LIST *file_list = NULL;
  STRING_LIST *temp_list = NULL;
  /* variables for call to line_setup */
  LOGICAL     good_setup;              /*return value                  */
  char        start_path[PDS_MAXLINE]; /*start directory walk here     */
  LOGICAL     debug;                   /*debug flag for future use     */
  LOGICAL     no_recurs;               /*turn off recurs direct search */
  LOGICAL     no_details;              /*do not print details          */
  char       *temp_str;
  int         temp_str_set = 0;
  /*------------
 *  Null terminate strings
 */

   InName[NameLen - 1]   = '\0';
   OutName[NameLen - 1]  = '\0';
   start_path[PDS_MAXLINE - 1] = '\0';

/*------------
 *  Clear the screen 
 *  Start program
 */
   start_path[0] = '\0';
   for (i = 0; i < 25; ++i)
      printf("\n");				/* clear the screen */

   printf("            ===== PDS - Line Count Program ===== \n");
   printf("                                  Version 2.2 \n");
   printf("\n");

   good_setup = line_setup (argc, argv, &debug, &no_recurs, &no_details, InName, OutName, &file_list,
	                                                       start_path);
   if(good_setup == FALSE)
   {
	  printf("\n Setup failed\n");
 	  exit(FAILURE);
   }


   /*Open the summary file.  If there is one there we don't want to write over it*/
/*   if ((opf1 = fopen("linesmry.tmp", "r")) != NULL)   
   {
	   printf("\n linesmry.tmp already exists, delete file and rerun program\n\n");
	   exit(FAILURE);
   }*/
   /*There wasn't one.  Make one*/
   if((opf1 = fopen("linesmry.tmp", "w")) == NULL)
   {
	   printf("\n Could not open summary file %s \n", SummName);
       exit(FAILURE);
   }

   fprintf(opf1,"                                                                                                                                  \n");
   fprintf(opf1,"                                                                                                                                  \n");
   fprintf(opf1,"                                                                                                                                  \n");
   fprintf(opf1,
	   "                  Ill. Line RECORD   INTER.   FILE    REC.  ROW                   TOTAL      FILE PTR                   \n");
   fprintf(opf1,
	   "     File Name    Char Term TYPE     FORMAT   RECORDS BYTES BYTES     Min   Max   BYTES      LENGTH                     \n");
   fprintf(opf1,
	   "     ------------ ---- ---- -------- -------- ------- ----- --------- ----- ----- ---------- ----------                 \n");





	   
/*------------
 *  Open the OUTPUT file
 */
    if ((opf = fopen(OutName, "w+")) == NULL)
    {
      printf("\n Could not open OUTPUT file");
      exit(FAILURE);
    }
    /*    line_header(opf, &debug, &no_recurs, &no_details, OutName, InName, start_path); */
    
   /*-----------------------------------------------------------------*/
   /* 12-18-02 MDC                                                    */
   /* Added a call to print out the error messages onto the output    */
   /* file.                                                           */
   /*-----------------------------------------------------------------*/
   err_write_to_file(opf,TRUE);

/* -----------
 * Line processing moved to a function which is called from dir_walk_7 
 */

   /*dir_walk_prep(InName, opf, debug, no_recurs, no_details, start_path);*/
   for (temp_list = file_list; temp_list != NULL; temp_list = temp_list -> next)
   {
   /*	         printf("%s\n", temp_str);*/
         temp_str = temp_list->text;
		 line_process(temp_str, opf, debug, no_details);
		 temp_str_set = 1;
   }
   if(temp_str_set == 1) Lemme_Go(temp_str);
   
   fflush(opf);
   fclose(opf1);
   for (i = 0; i < 139; ++i) buffer[i] = '\0';
   opf1 = fopen("linesmry.tmp","rb");
   
   /*   while (fread(buffer, 131, 1, opf1) > 0) */
   while ((fgets(buffer, 131, opf1)) > 0)
   {
	   fprintf(opf, "%s", buffer);
	   printf("%s", buffer);
   }
   fprintf(opf, "\n\n End of Output \n\n");
   

   fflush(opf);
   fclose(opf);
   fclose(opf1);
   remove("linesmry.tmp");
   printf("\n\n    %s - contains the output of this program \n", OutName); 
   printf("\n\n");
   return(NORMAL);

}  /** end of main **/


/**********************************************************************
 *$Component                                                          *
 *    LOGICAL lv_setup (num_args, command_line, rpt_file_name,        *
 *                      dd_index_name, file_list, use_dd,             *
 *                      use_verbose, use_terminal, use_progress       *
 *                      lv_or_slv, use_expand)                        *
 *$Abstract                                                           *
 *    Parses the verifier command line.                               *
 *$Inputs                                                             *
 *    num_args:                                                       *
 *        The num_args variable is a integer containing the number of *
 *        arguments in a command line, e.g., argc.                    *
 *    command_line:                                                   *
 *        The command_line variable is an array of character strings  *
 *        containing command line arguments, e.g., argv.              *
 *$Outputs                                                            *
 *    out_file:                                                       *
 *        The outt_file variable is a character string containing     *
 *        the name of the file to be written to.                      *
 *    in_file:                                                        *
 *        The in_file variable is a character string containing       *
 *        the name of the file(s) to be processed.  It may contain    *
 *        wild cards.                                                 *
 *    start_path:                                                     *
 *        The start_path variable is a character string containing    *
 *        the name of the directory in which to begin searching for   *
 *        in_file.                                                    *
 *    debug:                                                          *
 *        The debug variable is a TRUE/FALSE flag indicating whether  *
 *        debug code should be executed.                              *
 *    no_details:                                                     *
 *        The no_details variable a TRUE/FALSE flag indicating        *
 *        whether detail messages will be displayed or just summary   *
 *        messages will be displayed.                                 *
 *    no_recurs:                                                      *
 *        The no_recurs variable a TRUE/FALSE flag indicating         *
 *        whether recursive directory searching will be used.         *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure.       *
 *$Detailed_Description                                               *
 *    The line_setup routine parses the command line passed into the  *
 *    line and notifies the user of any errors. It sets               *
 *    all the output variables based on the flags it finds on the     *
 *    command line.  This routine returns true if it succeeds, and    *
 *    false otherwise.                                                *
 *                                                                    *
 *CHANGE HISTORY:                                                     *
 *    03-25-04   MDC    Removed string concatenation statement to     *
 *                      in_cmd variable. This would cause prog. to    *
 *                      crash on Solaris after reading in 5th file to *
 *                      process.                                      *
 **********************************************************************/

LOGICAL line_setup (num_args, command_line, debug, no_recurs, no_details, in_cmd, out_file, 
					                                           file_list, start_path)

int          num_args;
char         *command_line [];
char         *in_cmd;

char         *out_file;
STRING_LIST  **file_list;
char         *start_path;
LOGICAL      *debug;
LOGICAL      *no_recurs;
LOGICAL      *no_details;

{
   LOGICAL     success = TRUE;
   LOGICAL     output_name_found = FALSE;
   LOGICAL     input_name_found = FALSE;
   LOGICAL     starting_dir_flag = FALSE;
   LOGICAL     file_flag = FALSE;
   STRING_LIST *temp_list = NULL;

   int i;  
   char temp [PDS_MAXLINE + 1];
   LOGICAL     use_di = FALSE;                  


/** BEGIN **/
   /*---------------------------------------------------------------------*/
   /** initialize all flags to their defaults                            **/
   /*---------------------------------------------------------------------*/
                             
   *debug        = FALSE;
   *no_recurs    = FALSE;
   *no_details   = FALSE;
   

      /*---------------------------------------------------------------------*/
   /** LOOP through the command line arguments to find the starting      **/
   /** path if one was specified.                                        **/
   /*---------------------------------------------------------------------*/
   starting_dir_flag = FALSE;
   for (i = 1; i < num_args; i++)
   {
	   strcpy (temp, command_line [i]);
       util_strip_lead_and_trail (temp, ' ');
       /*-----------------------------------------------------------------*/
	   /** -bd is the starting directory flag and must be followed by a  **/
	   /** string containing the starting directory, and there must be   **/
	   /** at least one more argument left                               **/
	   /*-----------------------------------------------------------------*/

		if ((file_flag = strcmp (temp, "-bd") == 0) &&
								(i <= (num_args - 1)) &&
									(*(command_line[i+1]) != '-'))
		{
			/*-----------------------------------------------------------------*/
			/** Get the string with the starting path                         **/
			/*-----------------------------------------------------------------*/
 	        i++;
	        strcpy (start_path, command_line [i]);
			starting_dir_flag = TRUE;
		}
   }
   

   
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

       if      (strcmp (temp, "-nd") == 0)          /*Do not print details*/
	       *no_details   = TRUE;
       /*     else if (strcmp (temp, "-db") == 0)          /*Debug               */
       /*	       *debug   = TRUE; */
       else if (strcmp (temp, "-di") == 0)
	       *no_recurs = TRUE;        /* Turn off recursive directory searching*/
       /*-----------------------------------------------------------------*/
       /** ELSE IF this is the last flag on the line THEN                **/
       /**     We have failed.  All of the remaining flags must be       **/
       /**     followed by strings. -if will be followed by the          **/
	   /**     input file mask, -of will be followed by the output file  **/
	   /**     name, -bd will be followed by the starting directory.     **/
	   /*-----------------------------------------------------------------*/

       else if ((file_flag = (strcmp (temp, "-if") == 0 || strcmp (temp, "-of") == 0
		         || strcmp (temp, "-bd") == 0)) && i >= (num_args - 1))
         {
		   if(strcmp (temp, "-bd") != 0)
		   {
	        success = FALSE;
	        printf ("Command line option \"%s\" must be followed by a file name",
		                                                                 command_line [i]);
	        }
		   else
		   {
	        success = FALSE;
	        printf ("Command line option \"%s\" must be followed by a path name",
		                                                                 command_line [i]);
		   }
	   }
	   /*-----------------------------------------------------------------*/
	   /* ELSE IF this option is followed by another flag THEN            */
	   /*     We have failed.  All of the remaining flags must be         */
	   /*     file names.                                                 */
	   /*-----------------------------------------------------------------*/
       else if (file_flag && *(command_line[i+1]) == '-')
       {
		   if(strcmp (temp, "-bd") != 0)
		   {
			   success = FALSE;
	           printf ("Command line option \"%s\" must be followed by a file name",
		                                                                 command_line [i]);
		   }
		   else
		   {
	           success = FALSE;
	           printf ("Command line option \"%s\" must be followed by a path name",
		                                                                 command_line [i]);
           }
	   }
       /*-----------------------------------------------------------------*/
       /** ELSE IF the next flag is the INPUT FILE flag THEN             **/
       /**     LOOP until the next flag is found on the command line     **/
       /**        assume the next argument is the input file mask.       **/
       /**     ENDLOOP                                                   **/
       /*-----------------------------------------------------------------*/

       else if (strcmp (temp, "-if") == 0)
       {
		  in_cmd[0] = '\0';
	      input_name_found = TRUE;
	      do
	      {
	         i++;
			 /* 03-25-04 MDC - Removed this "if" statement since it appears this doesn't do
			    anything with this routine. Program would crash on Solaris since in_cmd accumulates
				characters until its past the limit designated to it.
			 */
/*			 if((strlen(in_cmd) + strlen(command_line[i])) < PDS_MAXLINE) strcat(in_cmd, command_line[i]); */
		     temp_list = slv_get_volume_list (command_line [i], use_di, start_path);
		     if (temp_list != NULL)
		              *file_list = util_append_string_list (*file_list, 
					          (char *) temp_list, LIST_TYPE);
		  } while ((i < num_args - 1) && *(command_line [i + 1]) != '-');
       }
	   
       /*-----------------------------------------------------------------*/
       /** ELSE IF the next flag is the REPORT FILE flag THEN            **/
       /**    copy the next argument to the report file name             **/
       /*-----------------------------------------------------------------*/

       else if (strcmp (temp, "-of") == 0)
       {
	      output_name_found = TRUE;
	      do
	      {
	         i++;
		     strcpy(out_file, command_line [i]);
		  } while ((i < num_args - 1) && *(command_line [i + 1]) != '-');
       }
       /*-----------------------------------------------------------------*/
       /** ELSE IF the next flag is the path flag THEN                   **/
       /**    copy the next argument to the path variable                **/
       /*-----------------------------------------------------------------*/

       else if (strcmp (temp, "-bd") == 0)
       {
	      i++;       /* we already have the string, so just incr. past it */
       }

       /*-----------------------------------------------------------------*/
       /** ELSE                                                          **/
       /**    flag is not recognized so print an error                   **/
       /*-----------------------------------------------------------------*/

       else
       {
	  success = FALSE;
	  printf ("Unrecognized command line option \"%s\"",  command_line [i]);
       }
       /*-----------------------------------------------------------------*/
       /** ENDIF                                                         **/
       /*-----------------------------------------------------------------*/
   }

   /*---------------------------------------------------------------------*/
   /** IF no report file name was found and terminal display is not      **/
   /**    enabled THEN print an error                                    **/
   /** ENDIF                                                             **/
   /*---------------------------------------------------------------------*/

   if (!output_name_found)
   {
      success = FALSE;         
      printf("You must provide a report file name, -of option\n");
   }
   /*---------------------------------------------------------------------*/
   /** IF no input file name was found THEN                              **/
   /**    print an error                                                 **/
   /** ENDIF                                                             **/
   /*---------------------------------------------------------------------*/

   if (!input_name_found)
   {
      success = FALSE;
      printf( "You must provide an input file mask, -if option\n");
   }
   /*---------------------------------------------------------------------*/
   /** IF no starting directory specified, use root of cwd but print a   **/
   /** warning.                                                          **/
   /** ENDIF                                                             **/
   /*---------------------------------------------------------------------*/

   if (!starting_dir_flag)
   {
      printf( "No starting directory specified, root of working dir will be used\n");
	  printf("Starting directory is specified with -bd option\n");
   }
   /*---------------------------------------------------------------------*/
   /** IF an error occurred THEN                                         **/
   /**    display program information                                    **/
   /** ENDIF                                                             **/
   /*---------------------------------------------------------------------*/

   if (!success)
   {
      printf ( "\nUsage:\n");
      printf ( "line -[-db -nd -di] [-of report-file] [-if test-file-mask] [-bd start-directory]\n");
      printf ( "Where:\n");
      /*	  printf ( "   -db:        Enables debug (future use)\n"); */
      printf ( "   -nd:        Disables detail reporting\n");
      printf ( "   -of <file>: Specifies the report file for output\n");
      printf ( "   -if <file>: Specifies the label file(s) to be verified (can be wildcarded)\n");
	  printf ( "   -di:        disables recursive directory searching\n");
	  printf ( "   -bd <path>: Specifies search path for input files, default is root\n");
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
   }
  return (success);

/** END **/
}


/******************************************************************************************/
/*******************************************************************************************/
void line_process(InName, opf, debug, prt_det)
char * InName;
FILE * opf;
LOGICAL debug;
LOGICAL prt_det;
{
	FILE * ipf;
    fpos_t    file_loc;
    time_t	t;

  int	    j;			            /* counter		            */
  int		i;                      /* counter                  */
  int		convert    = FALSE;		/* if user wants to convert */
  int       status     = FALSE;		/* if illegal year/day data */
  int  		TotalLines = 0;			/* number of lines read     */
  int       TotalBytes = 0;			/* number of bytes read     */
  int       indent     = 0;			/* number of indentions     */
  int       scan;					/* Type of line (obj, end)  */
  int       illeg_chars = 0;        /* flag for illegal chars   */
  char *    illeg_char;             /* true or false string for print*/
  char *    illeg_line;             /* true or false string for bad line termination for print*/

  int       MinLineLen = 999999;    /* minimum number of bytes read */
  int       MaxLineLen = 0;         /* maximum number of bytes read */
  int       BadLines   = 0;         /* # of lines not terminated with <CR><LF> */
  int       TempLineLen;
	
  char *    prec_type;              /*used to check for RECORD TYPE in record  */
  char *    record_type;
  int       rec_type;               /*used to check for RECORD TYPE in record  */
  int       rec_type_is;            /*used to check for what kind of record    */
                                    /*if RECORD TYPE is found                  */
  int       look_for_rec_bytes;     /*used to determine if RECORD BYTES        */
                                    /*should be looked for.                    */
  char *    prec_bytes;             /*used to check for RECORD BYTES in record */
  int       frec_bytes;
  int       rec_bytes;              /*used to check for RECORD BYTES in record */
  int       rec_bytes_end; 
  int       rec_len_num;			/*used for temp save of RECORD BYTES       */
                                    /*numeric field                            */
  int       fr_num;					/*used for temp save of FILE RECORDS       */
  int       row_bytes;			    /*used for temp save of ROW BYTES          */
                                    /*numeric field                            */
  char      *temp_str;
  char      *int_form;
  char      *string_save;
/*
  char      temp_str[StringLen];
  char      int_form[StringLen];
  char      string[StringLen];
  */
/*
char * temp_str = new char[StringLen];
  char * int_form = new char[StringLen];
  char * string   = new char[StringLen];
*/
  char *    temp_ptr;
  char      temp_buff[133];
  char      int_buf[10];
  char      int_buf1[10];
  char      int_buf2[11];
  char      int_buf3[11];
  char      min_num_string[6];/*11*/
  char      max_num_string[6];/*11*/
  char      tot_num_string[11];
  int       inname_len;
  int       str_len;
  
  int       length_1;
  char *    cwdret;
  char *    filename;
  char *    filepath;
  static char      filepath_old[MAX_LINE_LEN];
  static int       firsttime = 0;
  int       filepath_sw;
  int       temp_ind;

  /*  int ptr_file_len; */



  int line_char;/***************************/
  
  temp_str   = (char *)malloc(sizeof(char) * StringLen);
  int_form   = (char *)malloc(sizeof(char) * StringLen);
  string_save     = (char *)malloc(sizeof(char) * StringLen);





  rec_bytes   = 0;
  rec_len_num = 0;
  fr_num    = 0;
  row_bytes   = 0;
  for (i = 0; i <133; i++) temp_buff[i] = ' ';
  filepath_old[MAX_LINE_LEN - 1] = '\0';

  for(rec_bytes = 0; rec_bytes < 80; rec_bytes++) temp_buff[rec_bytes] =  ' ';
  temp_ptr = &temp_str[0];

  string_save[StringLen - 1]  = '\0';
  int_form[0] = '\0';

    if ((ipf = fopen(InName, "rb")) == NULL)
    {
      printf("\n Could not open INPUT file %s", InName);
	  
	  /* 12-17-02 MDC */
	  Lemme_Go(temp_str);
	  Lemme_Go(string_save);
	  Lemme_Go(int_form);

      exit(FAILURE);
    }

/*------------
 *  (1) Read each character of each line in the INPUT file
 *      (the file must be opened in binary in order to read the <CR>)      
 *  (2) Ensure that each character is a printable ASCII character
 *  (3) Any characters detected outside of the normal range are 
 *          flagged to the user.
 *  (4) Normal range is defined to be between:
 *          octal 32 (space) and octal 127 (block), inclusive.
 */

/*------------
 * Write Header and Time info to the output file
 */
	if(!prt_det)
	{
		time(&t);					/* get the current date/time */
		fprintf(opf, "\n\n            ===== PDS - Line Counter Program ===== \n");
		fprintf(opf, "\n\t\t     ");
		fprintf(opf, ctime(&t));			/* convert to ascii */
		fprintf(opf, "Input file:  %s\n\n", InName); 
	}


	
   fgetpos(ipf, &file_loc);
   if(!prt_det)
   {
      fprintf(opf, "\n ================================================");
      fprintf(opf, "\n (1)  Checking for illegal characters, such as TABs \n");
   }
   rec_type = 0;
   rec_type_is = 0;
   look_for_rec_bytes = 0;
   rec_bytes = 0;
   while (fgets(string_save, 9999999, ipf) != NULL)
   {
     ++TotalLines;

     /*************************************/
     /*check for RECORD_TYPE              */
	 /*************************************/
     strcpy(temp_str, string_save);
	 if (rec_type == 0)
	 {
	   prec_type = strstr(temp_str, "RECORD_TYPE");
	   if(prec_type != NULL) 
	   {                                                   /*check for kind of record*/
	     prec_type = strstr(temp_str, "FIXED_LENGTH");
	     if(prec_type != NULL)
		 {
	  	     rec_type_is = 1;
			 look_for_rec_bytes = 1;
			 rec_bytes = 1;
			 rec_type = 1;
		 }
		 else
		 {
		     prec_type = strstr(temp_str, "VARIABLE_LENGTH");
		     if(prec_type != NULL) rec_type_is = 2;
		     else
			 {
		         prec_type = strstr(temp_str, "STREAM");
		         if(prec_type != NULL) rec_type_is = 3;
		         else
		         {
			         prec_type = strstr(temp_str, "UNKNOWN");
		             if(prec_type != NULL) rec_type_is = 4;
			     }
			 }
	      }
	   }
     }
     /*************************************/
     /*If record type is FIXED check for  */
     /*RECORD_BYTES specification         */
     /*************************************/
     if ((look_for_rec_bytes == 1) && (rec_bytes == 0))
     {
       prec_bytes = strstr(temp_str, "RECORD_BYTES");
  	   if(prec_bytes != NULL)
	   {
	      prec_bytes = strstr(temp_str, "=");
		  if(prec_bytes != NULL)
		  {
		     temp_ptr = prec_bytes;
             rec_bytes = strcspn(temp_ptr, "0123456789");
			 temp_ptr = temp_ptr + rec_bytes;
			 strcpy(temp_str, temp_ptr);
			 rec_bytes_end = strspn (temp_str, "0123456789");
			 temp_str[rec_bytes_end] = '\0';
			 rec_len_num = atoi(temp_str);
			 rec_bytes = -100;
		     look_for_rec_bytes = 0;
		  }
	   }
     }
	 if(rec_bytes != -100) rec_bytes = 0;

     /********************************/
	 /* check for FILE RECORDS       */
	 /********************************/
     strcpy(temp_str, string_save);
     /*     prec_type = strstr(temp_str, "ROWS"); */
     prec_type = strstr(temp_str, "FILE_RECORDS");
     if(prec_type != NULL) 
     {                                                   /*check for kind of record*/
        prec_bytes = strstr(temp_str, "=");
        if(prec_bytes != NULL)
	    {
	       temp_ptr = prec_bytes;
/*           rec_bytes = strcspn(temp_ptr, "0123456789");
	       temp_ptr = temp_ptr + rec_bytes;*/
           frec_bytes = strcspn(temp_ptr, "0123456789");
	       temp_ptr = temp_ptr + frec_bytes;


	       strcpy(temp_str, temp_ptr);
	       rec_bytes_end = strspn (temp_str, "0123456789");
	       temp_str[rec_bytes_end] = '\0';
	       fr_num = atoi(temp_str);
	    }
     }

     /*******************************/
     /* get interchange format      */
     /*******************************/
     strcpy(temp_str, string_save);
     prec_type = strstr(temp_str, "INTERCHANGE_FORMAT");
     if(prec_type != NULL) 
     {                                                   /*check for kind of record*/
        prec_bytes = strstr(temp_str, "=");
        if(prec_bytes != NULL)
	    {
	       temp_ptr = prec_bytes;
           rec_bytes = strcspn(temp_ptr,
			                        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
	       temp_ptr = temp_ptr + rec_bytes;
	       strcpy(temp_str, temp_ptr);
	       rec_bytes_end = strspn (temp_str,
			                        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
	       temp_str[rec_bytes_end] = '\0';
	       strcpy(int_form, temp_str);
	    }
     }
	 str_len = strlen(int_form);
	 if ((str_len < 8) && (str_len > 0))
	 {
		 int_form[str_len] = '\0';
	 }
     else if (str_len < 1)
	 {
	     int_form[0] = ' ';
		 int_form[1] = ' ';
	     int_form[2] = ' ';
		 int_form[3] = ' ';
	     int_form[4] = '-';
		 int_form[5] = ' ';
	     int_form[6] = ' ';
		 int_form[7] = ' ';
	 }
     int_form[8] = '\0';
	 
     /*************************************/
     /*Check for ROW_BYTES                */
     /*************************************/
     /*	   row_bytes = 0; */
       strcpy(temp_str, string_save);
       prec_bytes = strstr(temp_str, "ROW_BYTES");
  	   if(prec_bytes != NULL)
	   {
	      prec_bytes = strstr(temp_str, "=");
		  if(prec_bytes != NULL)
		  {
		     temp_ptr = prec_bytes;
             rec_bytes = strcspn(temp_ptr, "0123456789");
			 temp_ptr = temp_ptr + rec_bytes;
			 strcpy(temp_str, temp_ptr);
			 rec_bytes_end = strspn (temp_str, "0123456789");
			 temp_str[rec_bytes_end] = '\0';
			 row_bytes = atoi(temp_str);
		  }
	   }

	 TempLineLen = strlen(string_save);
	 if(TempLineLen < MinLineLen) MinLineLen = TempLineLen;
     if(TempLineLen > MaxLineLen) MaxLineLen = TempLineLen;
/*	 if (strlen(string) < MinLineLen) MinLineLen = strlen(string);*/
/*	 if (strlen(string) > MaxLineLen) MaxLineLen = strlen(string);*/

	 j=strlen(string_save);

	 for (i = 0; i < j; ++i)
	 {	  
       if (string_save[i] > 31 && string_save[i] < 128); 
       else
	   {
	       if ((i < j-2) || (i == j-2 && string_save[i] != 13) || (i == j-1 && string_save[i] != 10))
           {
		      if(!prt_det)
		      {
		         fprintf(opf, "\n Line: [% 5d] contains an HEX: [%0xx] character at position: [%3d]", TotalLines, string_save[i], i);
			  }
		      status = TRUE; 
			  illeg_chars = TRUE; 
		   }
       }
     }
	 if (string_save[i-2] == 13 && string_save[i-1] == 10);
	 else ++BadLines;

   } 
   if (!prt_det)
   {
      if (status == FALSE) 
      {
        fprintf(opf, "\n    --->>  No illegal characters were detected \n");
      }
        fprintf(opf, "\n (2)  Checking record type and line lengths \n");

   
        fprintf(opf, "\n ================================================");
        fprintf(opf, "\n (3)  Checking Max and Min line lengths \n");
        fprintf(opf, "\n    --->>  Line length (minimum): %5d (inclusive of line terminator(s)) ", MinLineLen);
        fprintf(opf, "\n    --->>  Line length (maximum): %5d (inclusive of line terminator(s)) \n", MaxLineLen);

      if (rec_type == 1)
      {
        fprintf(opf, "\n ================================================");
        fprintf(opf, "\n (4)  Checking FIXED RECORD line lengths \n");
	    if ((rec_len_num == MaxLineLen) && (rec_len_num == MinLineLen))
		   fprintf(opf, "\n    --->> Fixed Length line lengths are correct\n");
	    else
	    {
		   fprintf(opf, "\n    --->> Fixed Length line lengths are incorrect\n");
		   fprintf(opf, "    --->> Fixed Length line length is specified as %d\n",
			                                                                 rec_len_num);
	    }
    }
   
    fprintf(opf, "\n ================================================");
    fprintf(opf, "\n (5)  Checking line terminator sequences (<CR><LF>) \n");
    fprintf(opf, "\n    --->>  Lines detected: %5d (in input file) ", TotalLines);
    fprintf(opf, "\n    --->>        of which: %5d (were not terminated with <CR><LF> \n) ", BadLines);

    fprintf(opf, "\n ================================================");
    fprintf(opf, "\n (6)  Checking Object structure(s)  \n\n");

    TotalLines = 0;
   }
/*------------
 *  (1)  Read each line in the INPUT file
 *  (2)  Write the line number to the output file
 *  (3)  Write indentions to the output file
 *  (4)  Write the input line to the output file
 */
    rewind(ipf);
   line_char = fgetc(ipf);
   while (feof(ipf) == 0)
   {
     TotalBytes++;
     line_char = fgetc(ipf);
   }
  rewind(ipf);
   while (fgets(string_save, 9999999, ipf) != NULL)
   {
     ++TotalLines;
     /*     TotalBytes += strlen(string); */
     if (!prt_det)
	 {
       fprintf(opf, "[% 5d,", TotalLines);
       fprintf(opf, "% 5d, ", strlen(string_save));
       fprintf(opf, "% 7d] ", TotalBytes);
     }
     if (strlen(string_save) > 53) 
     {  
       string_save[53] = '\n';
       string_save[54] = '\0';
     }
     else
     {
       string_save[strlen(string_save)-2] = '\n';
       string_save[strlen(string_save)-1] = '\0';
     }

     scan = NOOBJ;

     if (strlen(string_save) > 3)		/* only check strings w/more than */
     {				                /*   3 chars                      */
       i = 0;
       while (i < 20 && isspace(string_save[i])) ++i;  /* only check 1st 20 chars */

       if (i < 20)
       {
         if (string_save[i] == 'O' && string_save[i+1] == 'B' && string_save[i+2] == 'J')
         {
           ++indent;
           scan = OBJECT;
         }

         if (string_save[i] == 'E' && string_save[i+1] == 'N' && 
                                 string_save[i+2] == 'D' && string_save[i+3] == '_')
         {
           --indent;
           scan = ENDOBJ;
         }
       }
     }
     if(!prt_det)
	 {
        if (scan == OBJECT)
        {
          for (i = 1; i < indent; ++i)
             fprintf(opf, "|");
          fprintf(opf, "+-");
        }

        if (scan == ENDOBJ)
        {
          for (i = 1; i <= indent; ++i)
             fprintf(opf, "|");
          fprintf(opf, "+-");
        }

        if (scan == NOOBJ)
        {
          for (i = 0; i < indent; ++i)
          {
            fprintf(opf, "|");
          }
        }
        fprintf(opf, "%s", string_save);
      }
   }

   length_1 = 0;
   filepath = sys_get_path (InName); /* dir_mask might have c:\test\a.lbl  */
                                        /* directory would then have c:\test\ */
   if (filepath == NULL) 
   {
#ifndef MSDOS_TC
	   cwdret = getcwd(filepath, MAX_PATH_LEN);
#else
	   cwdret = getcwd(filepath, sizeof(filepath));
#endif
	   strcpy(filepath, cwdret);
   }
   else
   {
       length_1 = strlen(filepath);                            /* gets length of path     */
   }
   filename = InName + length_1;                            /* the file name           */
   if(firsttime == 0)
   {
	   firsttime = 1;
       filepath_sw = 0;
       strcpy(filepath_old, filepath);
   }
   else
   {
       if(strcmp(filepath, filepath_old) == 0)
       {
	       filepath_sw = 1;
       }
       else
       {
	       filepath_sw = 0;
	       strcpy(filepath_old, filepath);
       }
   }
   record_type = "No Def  ";
   for (temp_ind = 0; temp_ind < 10; temp_ind++)
   {
   int_buf[temp_ind]  = ' ';
   int_buf1[temp_ind] = ' ';
   int_buf2[temp_ind] = ' ';
   int_buf3[temp_ind] = ' ';
   }
   int_buf[4] = '-';
   int_buf[5] = '\0';
   int_buf1[6] = '-';
   int_buf1[7] = '\0';
   int_buf2[8] = '-';
   int_buf2[9] = '\0';
   int_buf3[8] = '-';
   int_buf3[9] = '\0';

   record_type = "        ";
   if(rec_type_is == 1)                                        /**/ 
   {
	   record_type = "FIXED   ";
#ifndef MSDOS_TC
	   sprintf(int_buf, "%d", rec_len_num);
#else
	   itoa(rec_len_num, int_buf, 10);
#endif
   }
   if(fr_num != 0) 
   {
#ifndef MSDOS_TC
	   sprintf(int_buf1, "%d", fr_num);
#else
	   itoa(fr_num, int_buf1, 10);
#endif
   }
   if(row_bytes != 0) 
   {
#ifndef MSDOS_TC
	   sprintf(int_buf2, "%d", row_bytes);
#else
	   itoa(row_bytes, int_buf2, 10);
#endif
   }

   if((rec_type_is == 1) && (fr_num != 0))
   {
	  sprintf(int_buf3, "%d", fr_num * rec_len_num);
   }
	   
   if(rec_type_is == 2) record_type = "VARIABLE";  /**/
   if(rec_type_is == 3) record_type = "STREAM  ";  /**/
   if(rec_type_is == 4) record_type = "UNKNOWN ";  /**/
   if(illeg_chars == TRUE)
   {
	   illeg_char = "Bad  ";
   }
   else
   {
	   illeg_char = "     ";
   }
   if(BadLines > 0)
   {
	   illeg_line = "Bad  ";
   }
   else
   {
	   illeg_line = "     ";
   }
#ifndef MSDOS_TC
   sprintf(min_num_string, "%d", MinLineLen);
   sprintf(max_num_string, "%d", MaxLineLen);
   sprintf(tot_num_string, "%d", TotalBytes);
#else
   itoa(MinLineLen, min_num_string, 10);
   itoa(MaxLineLen, max_num_string, 10);
   itoa(TotalBytes, tot_num_string, 10);
#endif

   strcpy(temp_buff, filepath);
   inname_len = strlen(filepath);
   for (temp_ind = inname_len; temp_ind < 132; temp_ind++) temp_buff[temp_ind] = ' ';
   temp_buff[130] = '\n';
   temp_buff[131] = '\0';
   if(filepath_sw == 0)
   {
       fprintf(opf1,"                                                                                                                                  \n");
	   fprintf(opf1, "%s", temp_buff);
   }
   fprintf(opf1,
	   "     %12s %s%s%s %8s %7s %5s %9s %5s %5s %10s %10s                       \n",
	            filename, illeg_char, illeg_line, record_type, int_form, int_buf1,
						       int_buf, int_buf2, min_num_string, max_num_string, tot_num_string,
												int_buf3);

/*	   "                  Ill. Line RECORD   INTER.   FILE    REC.  ROW                   TOTAL      PTR FILE                              \n");
	   "     File Name    Char Term TYPE     FORMAT   RECORDS BYTES BYTES     Min   Max   BYTES      LENGTH                                \n");
	   "     ------------ ---- ---- -------- -------- ------- ----- --------- ----- ----- ---------- ----------                            \n");
*/
   if(!prt_det) fprintf(opf, "\n");   
   printf("  Total Lines read = %d\n", TotalLines);
   printf("  Total Bytes read = %d\n\n", TotalBytes);
   fclose(ipf);
	
   /* 12-17-02 MDC */
   Lemme_Go(temp_str);
   Lemme_Go(string_save);
   Lemme_Go(int_form);
}
/******************************************************************************************/
/*                                                                                        */
/******************************************************************************************/
void line_header( opf, debug, no_recurs, no_details, out_file, in_file, start_path)

FILE       *opf;
char       *out_file;
char       *in_file;
char       *start_path;
LOGICAL    *debug;
LOGICAL    *no_recurs;
LOGICAL    *no_details;
{
    time_t	t;


	time(&t);					/* get the current date/time */
	fprintf(opf, "\n\n            ===== PDS - Line Counter Program ===== \n");
	fprintf(opf, "\n\t\t     ");
	fprintf(opf, ctime(&t));			/* convert to ascii */
	fprintf(opf, "\n\n");

	fprintf(opf, "   files to be processed: %s\n", in_file);
    fprintf(opf, "   report or output file: %s\n", out_file);
    fprintf(opf, "   search path          : %s\n", start_path);
    if(no_recurs)
	{
        fprintf(opf, "   Do not disable recursive directory searches\n");
    }
	else
	{
        fprintf(opf, "   Allow recursive directory searches\n");
	}

    if(no_details)
	{
		fprintf(opf, "   Do not print details\n");
	}
	else
	{
        fprintf(opf, "   Print details\n");
	}
    /*	if(debug)
	{
          fprintf(opf, "   Enable debug (no debug avail in this version)\n");
	}
	else
	{
         fprintf(opf, "   Disable debug, default \n");
	 } */
	fprintf(opf, "\n\n\n");
}

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
 **********************************************************************/
STRING_LIST *slv_get_volume_list (dir_mask, use_di, start_dir)

char *dir_mask;
LOGICAL use_di;
char *start_dir;
{
  char *fmask;
  char *temp_str = NULL;
  int length_1;
  int length_2;
  STRING_LIST *file_list = {NULL};

/** BEGIN **/

#ifdef MSDOS_TC
{
   char *directory = NULL;
   int got_handle = 0;
   char * cwdret;
   int free_directory = 0;
//   char command_str [PDS_MAXLINE + 1];

   length_1 = 0;
   directory = sys_get_path (dir_mask); /* dir_mask might have c:\test\a.lbl  */
                                        /* directory would then have c:\test\ */
/******************************************/
   printf("start dir is\n");
   length_2 = strlen(start_dir);
   if(length_2 > 2)
   {
	   if((directory == "") || (directory == '\0') || (directory == NULL))
	   {
		   directory = malloc(256);
		   free_directory = 1;
	   }
	   strcpy(directory, start_dir);
	   fmask = dir_mask;
   }
/******************************************/
   else
   {
		if (directory == NULL) 
		{
    		cwdret = getcwd(directory, sizeof(directory));
			directory = cwdret;
		}
		else
		{
			length_1 = strlen(directory);             /* gets length of path     */
		}
/******************************************/
		fmask = dir_mask + length_1;                  /* the file name           */
   }                                                  /* Do the directory walk   */
                                                      /* and build the file list */
   file_list = dir_walk(directory, fmask, file_list, use_di, FALSE); /*01-15-98*/
   if (file_list == NULL)
     {
	 printf("\n\n No file(s) were found matching the specification %s\n\n",
		   dir_mask);
	 }
}
#else
{
  FILE *fp = NULL;
  char       command_str [PDS_MAXLINE + 1];
  char       data_str [PDS_MAXLINE + 1];
  LOGICAL    success = TRUE;
  char       *slash = NULL;
  char       temp_2[14];

   /*----------------------------------------------------------------*/
   /** create the directory command based on the OS,                **/
   /**    redirecting the output to a file                          **/
   /*----------------------------------------------------------------*/
strcpy(temp_2, "linesmr2.txt");
#ifdef VAX
   sprintf (command_str, 
               "dir/col=1/ver=1/nohead/notrail/exclude=*.dir/out=%s %s",
               temp_2, dir_mask);
#endif

#ifdef SUN_UNIX
   slash = strrchr (dir_mask, '/');
   if (slash)
   {
      *slash++ = EOS;
      sprintf (command_str, "find %s -name \"%s\" -print > %s", dir_mask, slash,
               temp_2);
      *(--slash) = '/';
   }
   else
   {
     /*---------------------------------------------------------------*/
     /* 12-18-02 MDC                                                  */
     /* Added a check to see if an argument for a directory path was  */
     /* entered. If it was, then form the appropriate command to pass */
     /* to the operating system.                                      */
     /*---------------------------------------------------------------*/

      if(start_dir)
      {
	    /*------------------------------------------------------------*/
        /* 12-18-02 MDC												  */
		/* This piece of code commented out was taken straight out of */
		/* syslib.c. Right now, there seems to be no use for these    */
		/* conditions, but it is left in here in case it is needed in */
	    /* the future.												  */
		/*------------------------------------------------------------*/
		/*  slash = String_End(start_dir);
	 
		    if (*slash != '/')
			{
			if (*slash == '*' || *slash == '=' || *slash == '@')
				*slash = EOS;   
			} */
       
	sprintf (command_str, "find %s -name \"%s\" -print > %s", start_dir, dir_mask,
       	   temp_2);
      }
      
      else
	sprintf (command_str, "find . -name \"%s\" -print > %s", dir_mask,
                 temp_2);
   }
      
#endif

   /*----------------------------------------------------------------*/
   /** issue the command to the system                              **/
   /** IF there was an error in the system command THEN             **/
   /**    append an error message to the global list                **/ 
   /*----------------------------------------------------------------*/

   printf("command string is %s\n", command_str);
   success = sys_do_command (command_str);
   /*   success = TRUE; */ /* Commented out by MDC 12-18-02 */
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

      if ((fp = fopen (temp_2, "r")) == NULL)
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
             remove(temp_2);
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

