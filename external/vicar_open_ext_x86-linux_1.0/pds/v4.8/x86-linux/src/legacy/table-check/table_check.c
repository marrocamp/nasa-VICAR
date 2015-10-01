/*

  PDS Table Verifier

  This routine reads the information in a PDS label describing one or more
  table-type objects and compares the given parameters to the actual data in
  the file. It checks for sufficent number of records, looks for common
  transposition errors in the label, compiles a list of attributes for each
  column and ten attempts to read all columns (apart from bit fields). If
  extrema are included in the label, the data values are checked to ensure
  they are within bounds.

  Table objects may be binary or ASCII; different INTERCHANGE_FORMATs may
  appear in the same label - although this is generally not a good idea.
  Multiple pointers with different file names are also allowed.

  08 Mar 2001, A.C.Raugh: Begin rewrite from 'btv' and 'atv'


  Format:   pdstv [-d <directory>] [-o <report>] [-f] [-b] <label>

   where:   -d <directory>   gives the directory to check for the data file
            -o <report>      directs the report to the named file
            -f               indicates blank fields should be flagged
            -b               signals batch mode processing

            If the label file is not found on first try, the data directory
            name is prepended to the input argument and another attempt is
            made to find the file.

  17 May 2001, acr: upgrade to handle YYYY-DDD dates.
  21 May 2001, acr: Upgrade to handle FORMAT strings with repetition factors,
                    and to check the repetition count against the ITEMS value.
  11 Jun 2001, acr: Corrected handling of BYTES and REPETITIONS in CONTAINERs:
                    BYTES * REPETITIONS = sizeof(CONTAINER); Added 
                    'checkfilename' routine to check syntax of file names in 
                    TABLE pointers.
  25 Jun 2001, acr: Relabel invalid numeric columns as type=SPARE; do not 
                    attempt to print out summary values for SPARE columns
  26 Jun 2001, acr: Fixed stupid problem of not setting ITEM_OFFSET for columns.

  03 Jan 2002, acr: Added batch mode capabilities; changed "flag blanks"
                    option to "-f" from "-b".

  26 Feb 2002, acr: Increased precision of max/min found in file

  07 Mar 2002, acr: Bug fixes: Added check for valid max/min before printing
                    summary - a null marker is printed if no valid values
                    were encountered; Removed a couple of checks specific
                    to RECORD_BYTES that duplicated checks on ROW_BYTES.
                    When ROW_BYTES<>RECORD_BYTES, ROW_BYTES should take
                    precedence - the conditions being flagged were not
                    truly errors.
  08 Mar 2002, acr: More bugs: the test for a table-type object was giving
                    false positives, so the macro has been replaced by a 
                    function; inappropriate checks against RECORD_BYTES
                    were replaced with checks agains parent object size;
                    cleaned up table attributes listing for containers.
  14 Mar 2002, acr: Added tolerance for checks against label max/min in 
                    binary files to avoid flagging caused by rounding and
                    conversion of the label values and the limits of 
                    precision on real number comparisons; added processing
                    time report and "-t" option to command line.
  08 Aug 2002, acr: Added global 'error_count' to send appropriate exit
                    status at completion.
  06 Jan 2003, acr: Adjusted 'breakline' routine and usage to preserve blank
                    space inside quotes for use in character constants; 
                    disabled the underscores-within-quotes check until PDS
                    DEs can figure out what they want to do about this.
  11 Feb 2004, mdc: Re-wrote entire program to enable use with OAL library.
  01 Nov 2005, mdc: Fixed get_column_keywords. See notes.

*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <time.h>

#include "values.h"
#include "table_check.h"
#include "tvutil.h"
#include "tverr.h"
#include "tb_utils.h"

extern long int read_ascii_table_data(TABLE_INFO *, int, int);
extern long int read_binary_table_data(TABLE_INFO *, int, int);


/*---------------------------------------------------------------------------
  Global Variables 
*/

FILE    *label, *data;              /* input files                      */
FILE    *report;                    /* output file                      */
char     barline[100],blanks[100];  /* report file dingbats             */
char     dblbar[100];
int      batch_mode;
int      error_count;

TABLE_INFO   *table_top;                 /* Table object list                */

struct tvstruct *pdstv;


LOGICAL pds_generic_class = {FALSE};

/*-------------------------------------------------------------------*/
/** Variables which store the valid column data type list and its   **/
/** size.                                                           **/
/*-------------------------------------------------------------------*/

long tb_type_end = {0};

char *tb_type_list[] = 
{
    "ASCII COMPLEX",
    "ASCII INTEGER",
    "ASCII REAL",
    "BOOLEAN",
    "CHARACTER",
    "COMPLEX",
    "DATE",
    "FLOAT",
    "IBM COMPLEX",
    "IBM INTEGER",
    "IBM REAL",
    "IBM UNSIGNED INTEGER",
    "IEEE COMPLEX",
    "IEEE REAL",
    "INTEGER",
    "LSB BIT STRING",
    "LSB IEEE COMPLEX",
    "LSB IEEE REAL",
    "LSB INTEGER",
    "LSB UNSIGNED INTEGER",
    "MAC COMPLEX",
    "MAC INTEGER",
    "MAC REAL",
    "MAC UNSIGNED INTEGER",
    "MSB BIT STRING",
    "MSB IEEE COMPLEX",
    "MSB IEEE REAL",
    "MSB INTEGER",
    "MSB UNSIGNED INTEGER",
    "PC COMPLEX",
    "PC INTEGER",
    "PC REAL",
    "PC UNSIGNED INTEGER",
    "REAL",
    "SUN COMPLEX",
    "SUN INTEGER",
    "SUN REAL",
    "SUN UNSIGNED INTEGER",
    "TIME",
    "UNSIGNED INTEGER",
    "VAX COMPLEX",
    "VAX DOUBLE",
    "VAX INTEGER",
    "VAX REAL",
    "VAX UNSIGNED INTEGER",
    "VAXG COMPLEX",
    "VAXG REAL",
    NULL
};



/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

main(argc,argv)
  int   argc;
  char *argv[];

{ /* label values (correspond to keywords of same name): */

  /* housekeeping */

  double total_clock;
  OBJDESC *lp = NULL;
  OBJDESC *temp_op;
  TABLE_INFO *table_info = NULL;
  FILE_KEYWORDS *file_keywords = NULL;
  long rb_linenum = 0;
  long fr_linenum = 0;
  long rt_linenum = 0;
  int command_line_ok = 0;

  /* Variables used to time stamp a report */
  char *date_string = NULL;
  time_t time_in_sec;
  struct tm *current_time;

  /*=========================================================================*/

  clock();    /* Start the clock */

  /*=========================================================================*/

  /* Initialize global printing dingbats: */

  memset((void *)blanks, ' ',100);
  memset((void *)barline,'-',100);
  memset((void *)dblbar, '=',100);

  /* Set total error count to zero: */

  error_count = 0;

  pdstv = (struct tvstruct *) malloc(sizeof(struct tvstruct));

  /* Read arguments, open files: */
  command_line_ok = setup(argc, argv);

  /* If the command line reading was unsuccessful, then exit */
  if (command_line_ok == 0) 
  {

	  Lemme_Go(pdstv->rpt_file);
	  Lemme_Go(pdstv->label_file);
	  Lemme_Go(pdstv->directory);

	  printf("\n\nQuitting Program...\n\n");
	  exit(1);
  }
  else 
  {

	  /* Open Report file if there was a report file specified on
	     the command line and batch mode processing was not enabled
	  */
	  if( (pdstv->rpt_file != NULL) && (!pdstv->batch_mode) )
	  {
          if( (report=fopen(pdstv->rpt_file,"w")) == NULL)
          { 
			  tverr(OPEN_FAIL,0,pdstv->rpt_file,"writing", strerror(errno));
              return 0;
          }
	  }
      else
          report = stdout;
  }

  /* Write header to report file: */
  time(&time_in_sec);
  current_time = localtime(&time_in_sec);
  date_string = asctime(current_time);

  if (!pdstv->batch_mode)
  {
	  fprintf (report,"\n");
      fprintf (report,"%25.25sBinary Table Verifier Report\n",blanks);
	  fprintf (report,"%52.52s", date_string);
      fprintf (report,"%25.25s----------------------------\n\n",blanks);
      fprintf (report,"%20.20s file: %s\n","Label",pdstv->label_file);

 
      /* Report file label error section banner: */

      fprintf (report,"%79.79s\n\n",dblbar);
      fprintf (report,"\nLABEL ERRORS:\n-------------\n\n");
  }

 /*--------------------------------------------------------------------*/

  /* Parse the label file */
  
  if ((lp = OaParseLabelFile (pdstv->label_file, NULL, 
							ODL_EXPAND_STRUCTURE, TRUE)) == NULL) 
  {
      tverr(DATA_FILE_NOT_FOUND,0,pdstv->label_file,strerror(errno));
	  return 0;
  }

  /* Find and check the label file keywords */
  
  file_keywords = get_file_keywords(lp);


  /* Find a table type object. If there is none, then there is no need to 
     proceed any further.
  */
  if( (temp_op = FindTableObj(lp, NULL, NULL, 1, ODL_RECURSIVE_DOWN)) == NULL )
  {
	  tverr(NO_TABLES);
      if (pdstv->batch_mode) fprintf (report,"OK\n");
      fclose(report);
      exit(error_count);
  }


  /* End of the general label section: */

  if (!(pdstv->batch_mode)) fprintf (report,"\n%79.79s\n\n",dblbar);




  /* Grab all table information and store it in table_info */

  printf("Getting table and column information...\n");
  table_info = tb_get_table_info(lp, file_keywords);

  
  /* Now check the table information. */

  check_table(table_info,file_keywords->record_bytes);

  /* Finish report: */

  if (!pdstv->batch_mode)
  {
      fprintf (report,"%79.79s\n\n",dblbar);
      fprintf (report,"\nDone.");
      if (pdstv->time_display)
      {
		  total_clock = (double)(clock())/(double)(CLOCKS_PER_SEC);
          fprintf (report,"  Processing Time: %f seconds\n",total_clock);
      }
      else
      {
		  fprintf (report,"\n");
	  }
  }
  else
  {
	 fprintf (report,"OK\n");
  }

  fclose(report);


  /* Now clean up all the memory allocated */
  cleanup(table_info, lp, file_keywords);

  exit(error_count);

}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/* SUBROUTINES                                                               */
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/**********************************************************************/
/*                                                                    */
/* Routine: void cleanup(TABLE_INFO *, OBJDESC *, FILE_KEYWORDS)      */
/*                                                                    */
/* Description: De-allocates memory dynamically allocated during      */
/*              program execution.                                    */
/*                                                                    */
/* INPUTS: table_info - Pointer to 1 or more tables                   */
/*         lp - Pointer to a tree containing the label                */
/*         file_keywords - Pointer to file keywords in the label      */
/*                                                                    */
/**********************************************************************/

void cleanup(TABLE_INFO *table_info, OBJDESC *lp, FILE_KEYWORDS *file_keywords)
{
	TABLE_INFO *tbl_ptr = {NULL};
	TABLE_INFO *next_tbl = {NULL};
    OBJDESC *label_ptr = {NULL};
    COLUMN_INFO *col_ptr = {NULL};
	COLUMN_INFO *next_col = {NULL};
	CONTAINER_INFO *cnt_ptr = {NULL};
	CONTAINER_INFO *next_cnt = {NULL};

    /*------------------------------------------------------------------*/
    /** Deallocate the storage used by TABLE_INFO and COLUMN_INFO      **/
    /** lists for the current label file.                              **/
	/*------------------------------------------------------------------*/
	tbl_ptr = table_info;
	
	while(tbl_ptr != NULL)
	{
        Lemme_Go(tbl_ptr -> name);
		Lemme_Go(tbl_ptr -> tb_class);
		Lemme_Go(tbl_ptr -> fname);
		
		col_ptr = tbl_ptr->columns;

		while(col_ptr != NULL)
		{
            Lemme_Go(col_ptr->name);
			Lemme_Go(col_ptr->col_class);
            Lemme_Go(col_ptr->data_type);
            Lemme_Go(col_ptr->format);
			Lemme_Go(col_ptr->maxstr);
			Lemme_Go(col_ptr->minstr);

			next_col = col_ptr->next;
			Lemme_Go(col_ptr);
			col_ptr = next_col;
		}  
		
		/*------------------------------------------------------------*/
		/* Grab the container object member, de-allocate it, then	  */
		/* move to the next container object and repeat this process  */
		/* until all the container objects have been de-allocatted.   */
		/*------------------------------------------------------------*/
		cnt_ptr = tbl_ptr->container;

        while(cnt_ptr != NULL)
		{
			Lemme_Go(cnt_ptr->name);
			next_cnt = cnt_ptr->next_container;
			Lemme_Go(cnt_ptr);
			cnt_ptr = next_cnt;
		} 

		next_tbl = tbl_ptr->next;
		Lemme_Go(tbl_ptr);
		tbl_ptr = next_tbl;

    }  /*  End:  "while(tbl_ptr != ..."  */


	Lemme_Go(file_keywords->record_type);
	Lemme_Go(file_keywords);
	Lemme_Go(pdstv->label_file);
	Lemme_Go(pdstv->rpt_file);
	Lemme_Go(pdstv->directory);
	Lemme_Go(pdstv);

	label_ptr = OdlFreeTree(lp);

}	


/************************************************************************/
/*                                                                      */
/* ROUTINE: void check_table(TABLE_INFO *tab, int record_bytes)         */
/*                                                                      */
/* DESCRIPTION: check_table will call another routine to either read    */
/*              an ascii or binary table.                               */
/*                                                                      */
/* INPUTS: tab - Pointer to 1 or more tables                            */
/*         record_bytes - the number of bytes in each record            */
/*                                                                      */
/************************************************************************/

void check_table(TABLE_INFO *tab, int record_bytes)
{
	TABLE_INFO *tbl_ptr;

	tbl_ptr = tab;

	for(tbl_ptr = tab; tbl_ptr != NULL; tbl_ptr = tbl_ptr->next)
	{
		printf("Reading table data for %s...\n", tbl_ptr->name);
        if (tbl_ptr->ascii)
		{ 
			read_ascii_table_data(tbl_ptr, tbl_ptr->column_count, record_bytes);
		}
		else
		{
			read_binary_table_data(tbl_ptr, tbl_ptr->column_count, record_bytes);
		}
	}

	return;
}

/************************************************************************/
/*                                                                      */
/* ROUTINE: int setup(int num_args, char *command_line [])              */
/*                                                                      */
/* DESCRIPTION: setup will parse the command line entered by the user   */
/*              and initialize the appropriate variables.               */
/*                                                                      */
/* INPUTS: num_args - the number of arguments in the command line       */
/*         command_line - the command line values entered by the user   */
/*                                                                      */
/* RETURNS: Routine will return "1" if command line checking was        */
/*          successful. Otherwise, a "0" will be returned.              */
/*                                                                      */
/************************************************************************/

int setup(int num_args, char *command_line [])
{
	int success = 1;
	int i = 0;
	char temp[MAXLINE] = {0};
	int file_flag = 0;
	char *dir;

	/* Initialize pdstv structure */

	pdstv->label_file = NULL;  
    pdstv->rpt_file = NULL;    
    pdstv->directory = NULL;      
    pdstv->flag_blanks = 0;      
    pdstv->batch_mode = 0;
    pdstv->time_display = 0;

    for (tb_type_end = 0; 
	     tb_type_list[tb_type_end] != NULL; ++tb_type_end) ;
    --tb_type_end;

    if (num_args == 1)
    { 
		tverr(USAGE,0); 
        exit(error_count);
    }

    for (i = 1; i < num_args; i++)
	{
        file_flag = 0;
		strcpy (temp, command_line [i]);
		util_strip_lead_and_trail (temp, ' ');

        /*-----------------------------------------------------------------*/
        /** IF the next flag found on the command line is a simple ON/OFF **/
        /**    flag THEN                                                  **/
        /**    set the appropriate global flag                            **/
        /*-----------------------------------------------------------------*/

		if (strcmp (temp, "-f")        == 0) pdstv->flag_blanks = 1;
		else if (strcmp (temp, "-b")   == 0) {
			pdstv->batch_mode = 1;
			batch_mode = pdstv->batch_mode;
		}
		else if (strcmp (temp, "-t")   == 0) pdstv->time_display = 1;

		/*-----------------------------------------------------------------*/
        /** ELSE IF this is the last flag on the line THEN                **/
        /**     We have failed.  All of the remaining flags must be       **/
        /**     followed by a string.                                     **/
        /*-----------------------------------------------------------------*/

        else if ((file_flag = ((strcmp (temp, "-l") == 0) || 
		         (strcmp (temp, "-o"  ) == 0))
				 && i >= (num_args - 1) )  ) 
		{
            if(strcmp (temp, "-l") == 0)
			{
				 success = 0;
				 printf ("Command line option \"%s\" must be followed by a label file name\n",
																	command_line [i]);
			}
		    else if (strcmp (temp, "-o") != 0)
			{
	             success = 0;
	             printf ("Command line option \"%s\" must be followed by a report file name\n",
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
            if(strcmp (temp, "-l") == 0)
			{
				 success = 0;
				 printf ("Command line option \"%s\" must be followed by a label file name\n",
																command_line [i]);
			}
		    else if(strcmp (temp, "-o") != 0)
			{
	             success = 0;
	             printf ("Command line option \"%s\" must be followed by a report file name\n",
		                                                        command_line [i]);
			}
		}
        /*-----------------------------------------------------------------*/
        /** ELSE IF the next flag is the label flag THEN                  **/
        /*-----------------------------------------------------------------*/

        else if (strcmp (temp, "-l") == 0)
		{
            i++;
			
			/* Save the full path to the file name to the label variable */
			Malloc_String(pdstv->label_file, String_Size(command_line[i]));
			strcpy(pdstv->label_file, command_line[i]);

			/* Extract the directory path from the file name and save it */
	        dir = sys_get_path(command_line[i]);

			if(dir != NULL)
			{
                Malloc_String(pdstv->directory, String_Size(dir));
				strcpy(pdstv->directory, dir);
			}
		}
        /*-----------------------------------------------------------------*/
        /** ELSE IF the next flag is the report flag THEN                 **/
        /**    copy the next argument to the report variable              **/
        /*-----------------------------------------------------------------*/

        else if (strcmp (temp, "-o") == 0 && !(pdstv->batch_mode))
		{
			i++;

			Malloc_String(pdstv->rpt_file, String_Size(command_line[i]));
			strcpy(pdstv->rpt_file, command_line[i]);
		}
		
		else if (strcmp (temp, "-o") == 0 && (pdstv->batch_mode))
		{
			success = 0;
			i++;
			printf("Cannot set \"-o\" and \"-b\" at the same time\n");
		}

		/*-----------------------------------------------------------------*/
		/** ELSE                                                          **/
		/**    flag is not recognized so print an error                   **/
		/*-----------------------------------------------------------------*/

		else
		{
			success = 0;
			printf ("\nUnrecognized command line option \"%s\"\n", command_line [i]);
		}
		/*-----------------------------------------------------------------*/
		/** ENDIF                                                         **/
		/*-----------------------------------------------------------------*/
	}

   if (!success)
   {
		tverr(USAGE,0);
   }

   return (success);

}

/********************************************************************/
/* Routine: FILE_KEYWORDS *get_file_keywords(OBJDESC *lp)           */
/*                                                                  */
/* Description: Reads and checks file keywords in the label.        */
/*                                                                  */
/* Inputs: lp - Pointer to the label tree                           */
/*                                                                  */
/* Returns: Pointer to the file keywords structure                  */
/*                                                                  */
/* Side Affects: Memory must be freed by the calling routine.       */
/********************************************************************/

FILE_KEYWORDS *get_file_keywords(OBJDESC *lp)
{
	long rb_linenum = 0;
	long fr_linenum = 0;
	long rt_linenum = 0;
	char *str = NULL;
	int value = 0;

	FILE_KEYWORDS *file_keywords;
	
	
	file_keywords = (FILE_KEYWORDS *) malloc(sizeof(FILE_KEYWORDS));

	file_keywords->file_records = 0;
	file_keywords->record_bytes = 0;
	file_keywords->record_type = NULL;
	file_keywords->fixed_length = TB_UNKNOWN;

	if( OaKwdValuetoLong("RECORD_BYTES", lp, &(file_keywords->record_bytes)) == 0 )
	{
        if(file_keywords->record_bytes < 0)
		{
			GetLineNum(lp,"RECORD_BYTES",rb_linenum);

			tverr(INV_RECORD_BYTES,rb_linenum,file_keywords->record_bytes);
			file_keywords->record_bytes = 0;
		}
	}


	if( OaKwdValuetoLong("FILE_RECORDS", lp, &(file_keywords->file_records)) == 0 )
	{
		if (file_keywords->file_records <= 0)
		{
			GetLineNum(lp,"FILE_RECORDS",fr_linenum);

			tverr(INV_FILE_RECORDS,fr_linenum,file_keywords->file_records);
			file_keywords->file_records = 0;
		}
	}


	OaKwdValuetoStr("RECORD_TYPE", lp, &str);
	if(str != NULL)
	{
		New_String(file_keywords->record_type, str);
		util_remove_char(file_keywords->record_type, '"');

        if (strcmp(file_keywords->record_type,"FIXED_LENGTH")==0)
		{
			file_keywords->fixed_length = TRUE; 
		}
		else if (strstr(file_keywords->record_type,"FIXED"))
		{
			GetLineNum(lp,"RECORD_TYPE",rt_linenum);

			file_keywords->fixed_length = TRUE;
			tverr(INV_RECORD_TYPE,rt_linenum);
		}
		else
		{
			file_keywords->fixed_length = FALSE;
		}
	}

	/*-----------------------------------------------------------------*/
	/* File-level validity checks: Make sure required file keywords are
		present.
	*/

	if (file_keywords->record_bytes == 0)
	{
		GetLineNum(lp,"RECORD_BYTES",rb_linenum);
		tverr(NO_RECORD_BYTES,rb_linenum);
	}

	if (file_keywords->file_records == 0)
	{
		GetLineNum(lp,"FILE_RECORDS",fr_linenum);
		tverr(NO_FILE_RECORDS,fr_linenum);
	}

	if (file_keywords->fixed_length < 0)
	{
		GetLineNum(lp,"RECORD_TYPE",rt_linenum);
		tverr(NO_RECORD_TYPE,rt_linenum);
	}

	return file_keywords;
}



/********************************************************************************
 *$Component                                                                    *
 *    TABLE_INFO *tb_get_table_info (OBJDESC *label_pointer, FILE_KEYWORDS *fk) *
 *$Abstract                                                                     *
 *    Fetches table info from the PDS label and saves it                        *
 *$Inputs                                                                       *
 *    label_pointer - Pointer to the tree containing the label information      *
 *    fk - Pointer to the required PDS file keywords                            *
 *$Outputs                                                                      *
 *    None                                                                      *
 *$Returns                                                                      *
 *    Pointer to the table information from the label                           *
 *$Detailed_Description                                                         *
 *    This routine searches for all the TABLE objects in a PDS label,           *
 *    and creates a list of structures to hold the information needed           *
 *    to display the data.  The list of table_info structures is                *
 *    returned.  This routine will also look for table-like objects,            *
 *    such as SERIES and SPECTRUM. The fname input is the name of the           *
 *    table label file.                                                         *
 *$External_References                                                          *
 *$Side_Effects                                                                 *
 *    Allocates memory for the list of table_info structures that               *
 *    store the table information.                                              *
 *$Author_and_Institution                                                       *
 *    David P. Bernath / J.P.L.                                                 *
 *$Version_and_Date                                                             *
 *    1.2   March 9, 1993                                                       *
 *$Change_history                                                               *
 ********************************************************************************/

TABLE_INFO *tb_get_table_info (OBJDESC *label_pointer, FILE_KEYWORDS *fk)
{                                     
    AGGREGATE		table_ptr = {NULL};
    TABLE_INFO		*first_ptr = {NULL};
    TABLE_INFO		*last_ptr = {NULL};
    TABLE_INFO		*new_ptr = {NULL};
	POINTER_INFO	p_info;
    FILE			*f_ptr = {NULL};
    char			*str = {NULL};
    long			j;
    LOGICAL			found = {FALSE};
    char			savefile [PDS_MAXLINE + 1];
    char			*newfile = {NULL};
    char			*dir = {NULL};
	char			obj_nm[50];
	OBJDESC			*op;
	int				level_tot = 0;
	int				counter_1 = 1;
	char			object_name[50];
	int             table_number = 0;

	/** BEGIN **/


	/*-----------------------------------------------------------------------*/
	/** LOOP through the list of TABLE type objects                         **/
	/*-----------------------------------------------------------------------*/

	op = OdlFindObjDesc (label_pointer, NULL, NULL, NULL, 2, ODL_RECURSIVE_DOWN);

	/*-----------------------------------------------------------------------*/
	/** LOOP through the objects in the label that are of the current type  **/
	/*-----------------------------------------------------------------------*/
	level_tot = 0;
 	while (op != NULL) 
	{
		if(op != NULL)
		{
			strcpy(obj_nm, op->class_name); /* count all of the children in this node*/
			

			if((OdlWildCardCompare ("*TABLE", obj_nm) != 0) || (OdlWildCardCompare ("*SERIES", obj_nm) != 0) ||
			   (OdlWildCardCompare ("*SPECTRUM", obj_nm) != 0) || (OdlWildCardCompare ("*PALLETTE", obj_nm) != 0) ||
			   (OdlWildCardCompare ("*GAZETTEER", obj_nm) != 0)) 
			{
				level_tot++;
				strcpy(object_name, op->class_name);
          /*-------------------------------------------------------------------*/
          /** Allocate storage for a new structure and initialize its fields  **/
          /*-------------------------------------------------------------------*/
    
				new_ptr = (TABLE_INFO *) malloc(sizeof(TABLE_INFO));
				if(new_ptr == NULL)	Exit_System("Table Check tb_get_table_info ran out of memory");

				new_ptr->next = NULL;
				new_ptr->prev = NULL;
				new_ptr->table = op;
				new_ptr->columns = NULL;
				new_ptr->tblnum = ++table_number;
				new_ptr->row_count = TB_UNKNOWN;
				new_ptr->row_bytes = TB_UNKNOWN;
				new_ptr->prefix_bytes = 0;
				new_ptr->suffix_bytes = 0;
				new_ptr->column_count = 0;
				new_ptr->data_location = 1;
				new_ptr->ascii = TRUE;
				new_ptr -> container_count = 0;
				new_ptr -> container = NULL;

				New_String(new_ptr->name, "Unknown");
				New_String(new_ptr->tb_class, "TABLE");
				New_String(new_ptr->fname, "Unknown");

                
          /*-------------------------------------------------------------------*/
          /** Replace the class name in the tbtool table info structure with  **/
          /**     the one in the ODL structure, in case it's different.       **/
          /*-------------------------------------------------------------------*/

				if (op->class_name != NULL) Replace_String(new_ptr->tb_class, op->class_name);


		   /*-------------------------------------------------------------------*/
           /** Extract the keyword values from the label and store them in the **/
           /**     TABLE_INFO structure: NAME, ROWS, BYTES (or ROW_BYTES)      **/
           /**     ROW_PREFIX_BYTES, ROW_SUFFIX_BYTES                          **/
           /*-------------------------------------------------------------------*/
  
				get_table_key_words(op, new_ptr, &fk->record_bytes, fk->fixed_length);
           /*-------------------------------------------------------------------*/
           /** Look for the pointer that matches the class name of this table- **/
           /**    type object. If this is the i-th object of this class, then  **/
           /**    start at the i-th such pointer and work backwards until a    **/
           /**    match is found.                                              **/
           /*-------------------------------------------------------------------*/

				for (j=level_tot, found=FALSE; ((j > 0) && ! (found = 
				lt_get_pointer(label_pointer, new_ptr->tb_class, (int)j, &p_info))); --j) ;
    
           /*-------------------------------------------------------------------*/
           /** IF a pointer was found for the current TABLE object THEN        **/
           /*-------------------------------------------------------------------*/

				if (found)
				{
               /*---------------------------------------------------------------*/
               /** Compute the starting location of the data using the pointer **/
               /**     information.                                            **/
               /** Store the file name if the pointer is for a detached file.  **/
               /** Otherwise, the data are attached so use the label file name **/
               /**     as the data file name for this TABLE object             **/
               /*---------------------------------------------------------------*/

					new_ptr->data_location = (p_info.has_byte_loc) ?
						p_info.location : 
								(fk->record_bytes*(p_info.location-1)) + 1;
    
					if (*(p_info.file_name) != EOS)
						Replace_String(new_ptr->fname, p_info.file_name)
					else
						Replace_String(new_ptr->fname, pdstv->label_file)
    
              /*---------------------------------------------------------------*/
              /** Attempt to open the data file as is.                        **/
              /** If that fails, try lowercasing the file name.               **/
              /** If that fails try adding the label file path name.          **/
              /** If that fails, lowercase the entire name and call it a day. **/
              /*---------------------------------------------------------------*/

					if ((f_ptr = fopen (new_ptr->fname, "rb")) == NULL)
					{
						strcpy (savefile, new_ptr->fname);
						util_lower_case (new_ptr->fname);
						if ((f_ptr = fopen (new_ptr->fname, "rb")) == NULL)
						{
							if ((dir = sys_get_path (new_ptr->fname)) == NULL)
							{
								newfile = util_create_file_spec(pdstv->directory, savefile);
								Replace_String(new_ptr->fname, newfile)
								Lemme_Go(newfile);
								if ((f_ptr = fopen (new_ptr->fname, "rb")) == NULL)
								util_lower_case (new_ptr->fname);
							}
						}
					}
					if (f_ptr) fclose (f_ptr);
					

				} /*  End:  "if (found) ..."  */
    

           /*----------------------------------------------------------------*/
           /** Fetch info on any columns in the table                       **/
           /*----------------------------------------------------------------*/
		   
				new_ptr->columns = tb_get_column_info(op, new_ptr, 0);
    
           /*----------------------------------------------------------------*/
           /** Append the new structure onto the end of the table list      **/
           /*----------------------------------------------------------------*/
    
				if (first_ptr == NULL)
					first_ptr = new_ptr;
				else
				{                          
					last_ptr->next = new_ptr;
					new_ptr->prev = last_ptr;
				}
    
				last_ptr = new_ptr;
			}
    	  op = OdlFindObjDesc (op, NULL, NULL, NULL, 2, ODL_SIBLINGS_ONLY);

		}
        /*-----------------------------------------------------------------------*/
        /** ENDLOOP                                                             **/
        /*-----------------------------------------------------------------------*/
    

    }  /*  End:  "for (k=0; ..."  */

    /*-----------------------------------------------------------------------*/
    /** RETURN table pointer                                                **/
    /*-----------------------------------------------------------------------*/
	return first_ptr;

/** END **/

}  /*  "tb_get_table_info"  */


/************************************************************************/
/* This function will get the table keywords.  I could have used		*/
/* OaGetTableKeywords but it does not try quite as hard to fill the     */
/* request.																*/
/*                                                                      */
/* 03-27-06   MDC     Remove the double quotes found in a FORMAT value  */
/*                    before checking to see if it is ASCII             */
/************************************************************************/

void get_table_key_words(OBJDESC *op, TABLE_INFO *table_ptr, long *record_bytes, int fixed_length)
{
	char *val_ptr = NULL, *format_ptr = NULL;
	int length = 0;
	char line[PDS_MAXLINE];
	long row_bytes = 0;
	long line_number;
    
	OaKwdValuetoStr("NAME", op, &val_ptr);

	if(val_ptr != NULL)
	{
		Replace_String(table_ptr->name, val_ptr);
		util_remove_char(table_ptr->name, '"');
	}

	if (OaKwdValuetoLong ("ROWS", op, &table_ptr->row_count) != 0)
	{
		if (OaKwdValuetoLong ("RECORDS", op, &table_ptr->row_count) != 0) 
			{
				table_ptr->row_count = 1;
			}
	}

	if (OaKwdValuetoLong ("ROW_BYTES", op, &table_ptr->row_bytes) != 0)
	{
		if (OaKwdValuetoLong ("BYTES", op, &table_ptr->row_bytes) != 0) 
			{
				table_ptr->row_bytes = *record_bytes;
			}

	}
    
	if (OaKwdValuetoLong ("ROW_PREFIX_BYTES", op, &table_ptr->prefix_bytes) != 0)
	{
		table_ptr->prefix_bytes = 0;
	}
    
	if (OaKwdValuetoLong ("ROW_SUFFIX_BYTES", op, &table_ptr->suffix_bytes) != 0)
	{
		table_ptr->suffix_bytes = 0;
	}

	if (!pdstv->batch_mode)
	{
        fprintf (report,"\n%79.79s\n\n",dblbar);
		sprintf (line, "%s (Table #%d):",table_ptr->tb_class,table_ptr->tblnum);
		length = strlen(line);
		fprintf (report,"\n%s\n%*.*s\n\n",line,length,length,barline);
	}

	/* We do some error-checking on the top-level table values before going
       on:
	*/

    row_bytes = table_ptr->prefix_bytes + table_ptr->row_bytes + table_ptr->suffix_bytes;
    if ((row_bytes != *record_bytes)  &&  fixed_length)
    {
		GetLineNum(op,"ROW_SUFFIX_BYTES",line_number);
		tverr(ROWB_NE_RECB,line_number,row_bytes,*record_bytes);
	}

	val_ptr = NULL;
	OaKwdValuetoStr("INTERCHANGE_FORMAT", op, &val_ptr);
	if (val_ptr == NULL) OaKwdValuetoStr("FORMAT", op, &val_ptr);
    if (val_ptr != NULL)
    {
		/* 03-27-06 MDC - Copy the value and remove the quotes before checking to see if it is ASCII */
		New_String(format_ptr, val_ptr);
		util_remove_char(format_ptr, '"');

/*		if (strcmp(val_ptr,"ASCII") == 0) */
		if (strcmp(format_ptr, "ASCII") == 0)
        {
			table_ptr->ascii = TRUE;
		}
        else
        {
			table_ptr->ascii = FALSE;
		}
		Lemme_Go(format_ptr);
    }             
}


/**********************************************************************
 *$Component                                                          *
 *    COLUMN_INFO *tb_get_column_info(op, table_info, start_byte)	  *
 *$Abstract                                                           *
 *    Fetch column info from label                                    *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    table_ptr:                                                      *
 *        The table_ptr variable is a pointer to the label structure  *
 *        for a table object.                                         *
 *    table_info:                                                     *
 *        The table_info variable is a pointer to the structure       *
 *        which contains keyword information on a table object.       *
 *    start_byte:													  *
 *		  For now, this is used only when a CONATINER object is found.*
 *		  The start_byte will be used to correctly calculate the	  *
 *		  starting byte of a nested container. If this routine is	  *
 *		  being used in an application, you can pass "0" for this     *
 *		  argument.													  *
 *$Outputs                                                            *
 *    table_info:                                                     *
 *        The table_info variable is a pointer to the structure       *
 *        which contains keyword information on a table object.       *
 *$Returns                                                            *
 *    column_info:                                                    *
 *        The column_info variable is a pointer to the structure      *
 *        which contains keyword information on a column object.      *
 *$Detailed_Description                                               *
 *    This routine extracts all the column label information the      *
 *    program needs to display data and stores it in a list of        *
 *    COLUMN_INFO structures. The table_ptr input is the AGGREGATE    *
 *    structure of the table containing the columns in question,      *
 *    as output by the ODL parser. The list of column_info structures *
 *    is returned.  The table_info structure (the parent of the       *
 *    columns) is modified to reflect the number of columns found.    *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Error_Handling                                                     *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    Allocates memory for the list of column_info structures that    *
 *    store the column information.                                   *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   March 9, 1993                                             *
 *$Change_history                                                     *
 **********************************************************************/

COLUMN_INFO *tb_get_column_info (OBJDESC *op, TABLE_INFO *table_info, 
								                 long start_byte)
{                                     
    AGGREGATE    column_ptr = {NULL};
    COLUMN_INFO *first_ptr = {NULL};
    COLUMN_INFO *last_ptr = {NULL};
    COLUMN_INFO *new_ptr = {NULL};
	char        *str = NULL;
    int          status = {PDS_ERROR};
	OBJDESC     *op1;
	int          ret_val = 0;
	long		i = 0;
	long jj = 0;
	long temp_rep_count = 0;
	/** BEGIN **/
	/* variables to handle containers */
	COLUMN_INFO *container_ptr = NULL;
	CONTAINER_INFO *container_info = NULL;

	AGGREGATE temp_ptr = NULL;
	long counter = 0;
	long contain_column_count = 1;
	long line_number = 0;
	long column_number = 0;
	long current_start_byte = 0;

	AGGREGATE parent_ptr = NULL;

    pds_generic_class = FALSE;   /* Find ALL column objects */

	/*---------------------------------------------------------------------*/
	/** If the first argument to this "get_column_info" routine is a	  **/ 
	/** pointer to a container object, then grab its parameters to		  **/ 
	/** process the container correctly. Also, record the number of		  **/ 
	/** repetitions into the table_info structure.						  **/
	/*---------------------------------------------------------------------*/
	if(strcmp((op->class_name),"CONTAINER") == 0 )
	{
		container_info = get_container_params(op, table_info); /* 01-07-03 MDC */

		/*----------------------------------------------------------------*/
		/* If this is a nested container, we need to calculate the		  */
		/* appropriate start byte of the data.							  */
		/*----------------------------------------------------------------*/

		if( strcmp(op->parent->class_name, "CONTAINER") == 0) 
			container_info->start_byte = start_byte + container_info->start_byte - 1;
	}
    /*-----------------------------------------------------------------------*/
    /** LOOP through the COLUMN objects in the current table                **/
    /*-----------------------------------------------------------------------*/

	do  
	{

	/*-----------------------------------------------------------------------*/
    /** LOOP through the COLUMN objects in the current table                **/
    /**																		**/
	/** Added a do-while loop around the "for" loop to process a container. **/
	/** If there is no container present, it will simply execute the for    **/
	/** loop once.															**/
	/*-----------------------------------------------------------------------*/
    
		if( (container_info != NULL) )
		{
			current_start_byte = container_info->start_byte 
								 + (counter * (container_info->bytes));

			contain_column_count = 0;
		}
		
		i=2;
		op1 = OdlFindObjDesc (op, NULL, NULL, NULL, i++, ODL_CHILDREN_ONLY); /* 03-05-03 MDC */
		
		while( op1 != NULL)
		{
		/*--------------------------------------------------------------------*/
		/**	Changed the argument to call lab_find_object with no object name.**/
		/** This is so it can recognize if there are container objects       **/
		/** within the label. If there is, we need to handle the column      **/
		/** objects in a different way. "i" is initialized to 2 since the 1st**/
		/** object found is actually the object in which the table_ptr		 **/
		/** variable is pointed to, and we don't want that.				     **/
		/*--------------------------------------------------------------------*/

			/*-------------------------------------------------------------*/
			/** If the object found is a container, then....			  **/
			/*-------------------------------------------------------------*/
			if(strcmp( (op1->class_name),"CONTAINER" ) == 0 )
			{
				/*--------------------------------------------------------*/
				/** Grab all the column objects within the container.    **/ 
				/** The value returned is the first pointer to a column  **/
				/** within the container.								 **/
				/*--------------------------------------------------------*/

				container_ptr = tb_get_column_info(op1,table_info, 
													current_start_byte);
						

				if (first_ptr == NULL)
				{
					first_ptr = container_ptr;
				}
				else
				{
					last_ptr -> next = container_ptr;
					container_ptr -> prev = last_ptr;
				}
					last_ptr = container_ptr;
				
				/*--------------------------------------------------------*/
				/** Now move the last_ptr to point to the very end of    **/
				/** the structure since container_ptr points to the		 **/ 
				/** first column object in the container.				 **/
				/*--------------------------------------------------------*/
				while(last_ptr->next != NULL)
					last_ptr = last_ptr->next;
				/*--------------------------------------------------------*/
				/** Now we need to know what objects to skip so that we  **/
				/** don't process them again.							 **/
				/*--------------------------------------------------------*/

				/* Set this flag to FALSE again to find all objects */
					pds_generic_class = FALSE; 
			}
			/* If the object is a "COLUMN", then.... */										
			else if((strstr(op1->class_name, "COLUMN") != NULL) && (strstr(op1->class_name, "BIT_COLUMN") == NULL))
			{
				++(table_info->column_count); 
				
        /*-------------------------------------------------------------------*/
        /** Allocate storage for a new structure and initialize its fields  **/
        /*-------------------------------------------------------------------*/
				new_ptr = (COLUMN_INFO *) malloc(sizeof(COLUMN_INFO));
				if (new_ptr == NULL) Exit_System("TABLE CHECK tb_get_column_info ran out of memory");
				new_ptr -> next           = NULL;
				new_ptr -> prev           = NULL;
				new_ptr -> size           = TB_UNKNOWN;
				new_ptr -> items          = 1;
				new_ptr -> item_bytes     = 0;
				new_ptr -> item_offset    = 0;
				new_ptr -> index          = 0;
				new_ptr -> start          = TB_UNKNOWN;
				new_ptr -> is_in_container = 0;
				new_ptr -> which_container = 0;
				new_ptr -> col_num        = 0;
				new_ptr -> rep_num        = 0;
				new_ptr -> msbflag        = -1;
				new_ptr -> max.str        = NULL;
				new_ptr -> min.str        = NULL;
				new_ptr -> mmflag         = 0;
				new_ptr -> vmax.str       = NULL;
				new_ptr -> vmin.str       = NULL;
				new_ptr -> vmflag         = 0;
				new_ptr -> dmax           = 0;
				new_ptr -> dmin           = 0;
				new_ptr -> dmflag         = 0;
				new_ptr -> invalid.str    = NULL;
				new_ptr -> invflag        = 0;
				new_ptr -> missing.str    = NULL;
				new_ptr -> missflag       = 0;
				new_ptr -> offset         = 0;
				new_ptr -> scaling_factor = 1;
				new_ptr -> format         = NULL;
				new_ptr -> maxstr         = NULL;
				new_ptr -> minstr         = NULL;
				new_ptr -> invalidcount   = 0;
				new_ptr -> missingcount   = 0;
				new_ptr -> badcount       = 0;
				/* 02-03-06 MDC - Added initializations */
				new_ptr -> fwidth         = 0;
				new_ptr -> fprecision     = 0;

				New_String(new_ptr-> name, "Unknown");
				New_String(new_ptr-> col_class, "COLUMN");
				New_String(new_ptr-> data_type, "Unknown");

				/*-------------------------------------------------------------------*/
				/** If any of the container_info members are set to a value, it is  **/
				/** implied that we are processing columns within a container. So,  **/
				/** set the flag to detect a container to ON, record what column   	**/
				/** you are in inside the container, and record what repetition you **/
				/** are in.															**/
				/*-------------------------------------------------------------------*/
				if( container_info != NULL )
				{
					new_ptr -> is_in_container = 1;
					new_ptr -> col_num = ++contain_column_count;
					new_ptr -> rep_num = counter + 1;

					new_ptr -> which_container = container_info->container_id;
				}

				get_column_keywords(new_ptr, op1, table_info, container_info, current_start_byte);

                /* Now that we know the column's attributes, we can initialize the 
				   statistics variables in the structure:
				*/

				if (NUMERIC_FIELD(new_ptr->type))
				{
					new_ptr->maxfound.dbl = -MAXDOUBLE;
					new_ptr->minfound.dbl =  MAXDOUBLE;
				}
				else
				{
					new_ptr->maxfound.bytes = -MAXINT;
					new_ptr->minfound.bytes =  MAXINT;
				}
				new_ptr->maxstr = NULL;
				new_ptr->minstr = NULL;
				new_ptr->invalidcount = 0;
				new_ptr->missingcount = 0;
				new_ptr->badcount     = 0;

				/* If we did find a COLUMN_NUMBER, make sure it's the right one: */

				if (new_ptr->index != table_info->column_count)
				{
					GetLineNum(op1, "COLUMN_NUMBER", line_number);
					tverr(BAD_COLNUM,line_number,table_info->column_count,new_ptr->index);
				}

	            
        /*-------------------------------------------------------------------*/
        /** Append the new structure onto the end of the column list        **/
        /*-------------------------------------------------------------------*/

				if (first_ptr == NULL)
					first_ptr = new_ptr;
				else
				{
					last_ptr -> next = new_ptr;
					new_ptr -> prev = last_ptr;
				}

				last_ptr = new_ptr;

    /*-----------------------------------------------------------------------*/
    /** ENDLOOP                                                             **/
    /*-----------------------------------------------------------------------*/
			}  /*  End:  if  */
			op1 = OdlFindObjDesc (op, NULL, NULL, NULL, i++, ODL_CHILDREN_ONLY);
		} /*end of while*/
		counter ++;

		if(counter == 1 && container_info != NULL)
			container_info->columns = contain_column_count;

	} while( (container_info != NULL) && (counter < (container_info->repetitions)) );

	pds_generic_class = TRUE; 

    /*-----------------------------------------------------------------------*/
    /** RETURN column pointer                                               **/
    /*-----------------------------------------------------------------------*/
 
	return (first_ptr);

/** END **/

}  /*  "tb_get_column_info"  */


/************************************************************************/
/*                                                                      */
/* ROUTINE: void get_column_keywords(COLUMN_INFO *col_ptr, OBJDESC *op, */
/*              TABLE_INFO *table_info, CONTAINER_INFO *container_info, */
/*              long current_start_byte)                                */
/*                                                                      */
/* DESCRIPTION: Routine to extract column attributes and keywords from  */
/*              the label                                               */
/*                                                                      */
/* INPUTS: col_ptr - Pointer to a column object in a label              */
/*         op - Pointer to the label                                    */
/*         table_info - Pointer to a table object                       */
/*         container_info - Pointer to a container object               */
/*         current_start_byte - The current value of the starting byte  */
/*                              of a COLUMN object                      */
/*                                                                      */
/* Change History:                                                      */
/*     11-01-05   MDC     When grabbing a FORMAT keyword value, strip   */
/*                        out the quotation marks before storing the    */
/*                        variable                                      */
/*     03-27-06   MDC     Whenever we grab a string value, remove the   */
/*                        doube-quotation marks if it is found.         */
/************************************************************************/

void get_column_keywords(COLUMN_INFO *col_ptr, OBJDESC *op, TABLE_INFO *table_info, 
						 CONTAINER_INFO *container_info, long current_start_byte)
{

	int ret_val = 0;
	int line_number = 0;
	int msb = 0;
	char *str = NULL;
    char str_1[80];
    long index = 0;


    /*-------------------------------------------------------------------*/
    /** Replace the class name in the tbtool column structure with      **/
    /**     the one in the ODL structure, in case it's different.       **/
    /*-------------------------------------------------------------------*/

	if (op->class_name != NULL) Replace_String(col_ptr->col_class, op->class_name);

        /*-------------------------------------------------------------------*/
        /** Extract the keyword values from the label and store them in the **/
        /**     COLUMN_INFO structure: NAME, ITEM_BYTES, BYTES,             **/
        /**     START_BYTE.                                                 **/
        /*-------------------------------------------------------------------*/

	str = NULL;
	OaKwdValuetoStr("NAME", op, &str);
	if (str != NULL)
	{
		Replace_String(col_ptr-> name, str)
		/*****************************************************/
		/* Remove any occurrences of '"'. The OAL routine    */
    	/* somehow does not do this when obtaining the		 */
    	/* keyword value.									 */
		/*****************************************************/
		util_remove_char(col_ptr->name, '"');
		util_remove_char(col_ptr->name, '\'');
                        
	}
	
	OaKwdValuetoLong ("START_BYTE", op, &col_ptr->start);

	if( (col_ptr->start) != 0 && (container_info != NULL) )
			(col_ptr->start) = (col_ptr->start) + (current_start_byte-1);

    /*-------------------------------------------------------------------*/
    /** Search the label structure for the column item type or data type**/
    /** Strip, de-underscore, blank compress, and upper-case it.        **/
    /** If it's a valid data type, store it as the type for the current **/
    /**   column                                                        **/
    /*-------------------------------------------------------------------*/
	str = NULL;
	OaKwdValuetoStr("ITEM_TYPE", op, &str);
	if (str == NULL) OaKwdValuetoStr("DATA_TYPE", op, &str);
	if (str != NULL)
	{
		strcpy(str_1, str);
		ReplaceChar (str_1, '_', ' ');
    	util_compress_char (str_1, ' ');
		StripLeading (str_1, ' ');
		StripTrailing (str_1, ' ');
		util_upper_case (str_1);
						
		/**************************************************/
    	/* Remove quotes surrounding the string if it is  */
	    /* there.										  */
		/**************************************************/
		util_remove_char(str_1, '"');

		index = search_string_array(tb_type_list, tb_type_end, str_1);
		if (index != PDS_SEARCH_FAIL)
		{
			Lemme_Go(col_ptr->data_type);
			New_String(col_ptr->data_type, str_1);
		}
	}

    /*-------------------------------------------------------------------*/
    /** If TEXT is a valid display format for columns, then             **/
    /**     set the display format for this column to TEXT              **/
    /*-------------------------------------------------------------------*/
/*
	index = search_string_array_1(tb_display_list,tb_display_end,"TEXT");
	if (index != PDS_SEARCH_FAIL) 
		Replace_String(col_ptr->display_format, tb_display_list[index])
*/
	/*-------------------------------------------------------------------*/
    /** Extract the keyword values from the label and store them in the **/
    /**     COLUMN_INFO structure: ITEMS, ITEM_OFFSET                   **/
    /** If ITEM_OFFSET not found, it defaults to BYTES or ITEM_BYTES    **/
    /*-------------------------------------------------------------------*/

	OaKwdValuetoLong ("ITEMS", op, &col_ptr->items);

	ret_val = OaKwdValuetoLong ("ITEM_BYTES", op, &col_ptr->size);
	if (ret_val == 1) OaKwdValuetoLong("BYTES", op, &col_ptr->size);

	OaKwdValuetoLong ("ITEM_OFFSET", op, &col_ptr->item_offset);

	if(col_ptr->item_offset == 0)
	{
		col_ptr->item_offset = col_ptr->size;
	}


	str = NULL;
	OaKwdValuetoStr("VALID_MAXIMUM", op, &str);
	if(str != NULL)
	{
		New_String(col_ptr->vmax.str, str);
		util_remove_char(col_ptr->vmax.str, '"');
		col_ptr -> vmflag += 1;
	}

	str = NULL;
    OaKwdValuetoStr("VALID_MINIMUM", op, &str);
	if(str != NULL)
	{
		New_String(col_ptr->vmin.str, str);
		util_remove_char(col_ptr->vmin.str, '"');
		col_ptr -> vmflag += 2;
	}

	str = NULL;
	OaKwdValuetoStr("MAXIMUM", op, &str);
	if(str != NULL)
	{
		New_String(col_ptr->max.str, str);
		util_remove_char(col_ptr->max.str, '"');
		col_ptr -> mmflag += 1;
	}

    str = NULL;
	OaKwdValuetoStr("MINIMUM", op, &str);
	if(str != NULL)
	{
		New_String(col_ptr->min.str, str);
		util_remove_char(col_ptr->min.str, '"');
		col_ptr -> mmflag += 2;
	}

	if(OaKwdValuetoDouble("DERIVED_MAXIMUM", op, &(col_ptr -> dmax)) == 0)
		col_ptr -> dmflag += 1;

	if(OaKwdValuetoDouble("DERIVED_MINIMUM", op, &(col_ptr -> dmin)) == 0)
		col_ptr -> dmflag += 2;
			
	str = NULL;
	OaKwdValuetoStr("INVALID_CONSTANT", op, &str);
	if(str != NULL)
	{
		New_String(col_ptr -> invalid.str, str);
		util_remove_char(col_ptr->invalid.str, '"');
		col_ptr -> invflag = 1;
	}

	str = NULL;
	OaKwdValuetoStr("INVALID", op, &str);
	if(str != NULL)
	{
		New_String(col_ptr -> invalid.str, str);
		util_remove_char(col_ptr->invalid.str, '"');
		col_ptr -> invflag = 1;

		GetLineNum(op, "INVALID", line_number);

		tverr(OBSOLETE_KEYWORD,line_number,(table_info->column_count)+1,
                         "INVALID","INVALID_CONSTANT");
	}

	str = NULL;
	OaKwdValuetoStr("MISSING_CONSTANT", op, &str);
	if(str != NULL)
	{
		New_String(col_ptr -> missing.str, str);
		util_remove_char(col_ptr->missing.str, '"');
		col_ptr -> missflag = 1;
	}

	str = NULL;
	OaKwdValuetoStr("MISSING", op, &str);
	if(str != NULL)
	{
		New_String(col_ptr -> missing.str, str);
		util_remove_char(col_ptr->missing.str, '"');
		col_ptr -> missflag = 1;

		GetLineNum(op, "MISSING", line_number);

		tverr(OBSOLETE_KEYWORD,line_number,(table_info->column_count)+1,
                          "MISSING","MISSING_CONSTANT");
	}				

	OaKwdValuetoDouble("OFFSET", op, &(col_ptr -> offset));

	OaKwdValuetoDouble("SCALING_FACTOR", op, &(col_ptr -> scaling_factor));

	OaKwdValuetoLong("COLUMN NUMBER", op, &(col_ptr -> index));

	if(col_ptr -> index == 0)
	{
		col_ptr -> index = table_info->column_count;
	}

	str = NULL;
	OaKwdValuetoStr("FORMAT", op, &str);
	if(str != NULL)
	{
		/* 11-01-05 MDC - Strip out quotation marks from string */
		Replace_String(col_ptr->format, str);
		util_remove_char(col_ptr->format, '"');
		if(strlen(col_ptr->format) >= 50)
		{
			GetLineNum(op, "FORMAT", line_number);
			tverr(FORMAT_TOO_BIG,line_number,(table_info->column_count)+1);
		}
	}
    /* Because of the different data types involved in ASCII and binary
         tables, we split here to handle the processing for each type in 
         a separate block (for readbility):
    */
	str = NULL;
	OaKwdValuetoStr("DATA_TYPE", op, &str);
	if(str != NULL)
	{
		New_String(col_ptr->data_type, str);
		util_remove_char(col_ptr->data_type, '"');
	}

    if(col_ptr->data_type != NULL && table_info->ascii)
    {
       if (strstr(col_ptr->data_type,"INTEGER") != NULL)
       {
		  col_ptr -> type = SIGNED_INTEGER;
          if (strstr(col_ptr->data_type,"ASCII_INTEGER") == NULL  &&
              strstr(col_ptr->data_type,"ASCII INTEGER") == NULL)
          {
			  GetLineNum(op, "DATA_TYPE", line_number);
              tverr(OBSOLETE_TYPE,line_number,(table_info->column_count)+1,col_ptr->data_type,
                     "ASCII_INTEGER");
          }
       }
       else if (strstr(col_ptr->data_type,"FLOAT") != NULL  ||
		        strstr(col_ptr->data_type,"REAL")  != NULL)
	   {
           col_ptr -> type = REAL;
           if (strstr(col_ptr->data_type,"ASCII_REAL") == NULL  &&
               strstr(col_ptr->data_type,"ASCII REAL") == NULL)
           {
			   GetLineNum(op, "DATA_TYPE", line_number);
               tverr(OBSOLETE_TYPE,line_number,(table_info->column_count)+1,col_ptr->data_type,
                        "ASCII_REAL");
           }
       }
       else if (strstr(col_ptr->data_type,"CHARACTER") != NULL)
            col_ptr->type = CHARACTER;
       else if (strstr(col_ptr->data_type,"DATE") != NULL)
            col_ptr->type = DATE;
       else if (strstr(col_ptr->data_type,"TIME") != NULL)
            col_ptr->type = TIME;
       else
           col_ptr->type = UNRECOGNIZED;
    }

    else if( (col_ptr->data_type != NULL) && !(table_info->ascii))
    {
        if (strstr(col_ptr->data_type,"UNSIGNED_INTEGER") != NULL)
        {
		  col_ptr->type = UNSIGNED_INTEGER;
          if (strstr(col_ptr->data_type,"MSB_UNSIGNED_INTEGER") == NULL  &&
              strstr(col_ptr->data_type,"MSB UNSIGNED INTEGER") == NULL  &&
              strstr(col_ptr->data_type,"LSB_UNSIGNED_INTEGER") == NULL  &&
              strstr(col_ptr->data_type,"LSB UNSIGNED INTEGER") == NULL)
          {
			  GetLineNum(op, "DATA_TYPE", line_number);
			  tverr(OBSOLETE_TYPE,line_number,(table_info->column_count)+1,col_ptr->data_type,
                    "MSB_UNSIGNED_INTEGER");
          }

          msb = (strstr(col_ptr->data_type,"LSB")==NULL)? 1 : 0;
          if (col_ptr->msbflag < 0)
           	  col_ptr->msbflag = msb;
          else if (col_ptr->msbflag != msb)
		  {
			  GetLineNum(op, "DATA_TYPE", line_number);
           	  tverr(INT_TYPE_CONFLICT,line_number,(table_info->column_count)+1);
		  }
			  

        }
        else if (strstr(col_ptr->data_type,"INTEGER") != NULL)
        {
            col_ptr->type = SIGNED_INTEGER;
            if (strstr(col_ptr->data_type,"MSB_INTEGER") == NULL  &&
                strstr(col_ptr->data_type,"MSB INTEGER") == NULL  &&
                strstr(col_ptr->data_type,"LSB_INTEGER") == NULL  &&
                strstr(col_ptr->data_type,"LSB INTEGER") == NULL)
            {
				GetLineNum(op, "DATA_TYPE", line_number);
                tverr(OBSOLETE_TYPE,line_number,(table_info->column_count)+1,col_ptr->data_type,
                        "MSB_INTEGER");
            }

            msb = (strstr(col_ptr->data_type,"LSB")==NULL)? 1 : 0;
            if (col_ptr->msbflag < 0)
                col_ptr->msbflag = msb;
            else if (col_ptr->msbflag != msb)
            {
				 GetLineNum(op, "DATA_TYPE", line_number);
				 tverr(INT_TYPE_CONFLICT,line_number,(table_info->column_count)+1);
			}
        }
        else if (strstr(col_ptr->data_type,"FLOAT") != NULL  ||
                 strstr(col_ptr->data_type,"REAL")  != NULL)
        {
            col_ptr->type = REAL;
            if (strstr(col_ptr->data_type,"IEEE_REAL") == NULL  &&
                strstr(col_ptr->data_type,"IEEE REAL") == NULL)
            {
    			GetLineNum(op, "DATA_TYPE", line_number);
                tverr(OBSOLETE_TYPE,line_number,(table_info->column_count)+1,col_ptr->data_type,
                       "IEEE_REAL");
            }
        }
        else if (strstr(col_ptr->data_type,"CHARACTER") != NULL)
             col_ptr->type = CHARACTER;
        else if (strstr(col_ptr->data_type,"DATE") != NULL)
             col_ptr->type = DATE;
        else if (strstr(col_ptr->data_type,"TIME") != NULL)
             col_ptr->type = TIME;
        else
             col_ptr->type = UNRECOGNIZED;
        }


		/* Parse out the field width and precision values from the format string,
			if any:
		*/

		if (col_ptr->format != NULL)
		{
			GetLineNum(op,"FORMAT",line_number);
			parse_FORMAT(col_ptr,line_number); 
		}

		check_column_attributes(col_ptr, op, table_info->prefix_bytes, table_info->row_bytes,
						table_info->row_bytes, table_info->ascii, !(col_ptr->is_in_container));

}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

int check_column_attributes(COLUMN_INFO *col, OBJDESC *op, int object_offset, int parent_bytes,
                            int row_bytes, int ASCII, int row_flag)

  /* This routine checks the attributes for a single column to make sure they
     are logically sound.

     Parameters:
       col             Column attributes structure
       object_offset   Offset in record to start of parent object
       parent_bytes    ROW_BYTES (or the equivalent) of the parent object
       row_bytes       ROW_BYTES from the label
       ASCII           TRUE if this is an ASCII table
       report          Output report file
       row_flag        TRUE if we're checking a column in a row, FALSE if
                         we're in a CONTAINER

  */

{
	int   status;      /* Return status. 1 is added for each error found */
	int   bytes;       /* holding place */
	char *buff;        /* Conversion buffer */
	char env[20];
	int   maxlen,minlen,vmaxlen,vminlen,invlen,misslen,fieldlen;
	long linecount = 0;

	status = 0;
	if (row_flag)
    { strcpy(env,"row"); }
	else
    { strcpy(env,"container"); }

	/* If this is a SPARE column, make sure it has a data type of CHARACTER,
		or signal an error.  We use a special type to indicate spare columns,
		to make them easier to identify later:
	*/

	if (spare_column(col))
	{
		if (col->type != CHARACTER)
		{
			GetLineNum(op, "NAME", linecount);
			tverr(BAD_SPARE_TYPE,linecount,col->index);
		}
		col->type = SPARE;
	}

	/* Collect the various string lengths for checking for field overflow: */

	if (col->mmflag%2) maxlen  = strlen(col->max.str);
	if (col->mmflag>1) minlen  = strlen(col->min.str);
	if (col->vmflag%2) vmaxlen = strlen(col->vmax.str);
	if (col->vmflag>1) vminlen = strlen(col->vmin.str);
	if (col->invflag)  invlen  = strlen(col->invalid.str);
	if (col->missflag) misslen = strlen(col->missing.str);

	if(col->items > 1) 
		fieldlen = col->item_bytes;
	fieldlen = col->size;

	/* First, if this is a numeric field, convert the various value strings to
		their floating-point equivalents as needed:
	*/

	if (NUMERIC_FIELD(col->type))

	{ /* ...MAXIMUM... */

		if (col->mmflag%2)
		{ 
			buff = copy_of(col->max.str);
			col->max.dbl = strtod(buff,(char **)NULL);
			Lemme_Go(buff);
			if (maxlen > fieldlen)
			{
				GetLineNum(op, "MAXIMUM", linecount);
				tverr(VALUE_TOO_LONG,linecount,col->index,"MAXIMUM",col->max.str,fieldlen);
			}
		}
	 
		/* ...MINIMUM... */

		if (col->mmflag > 1)
		{
			buff = copy_of(col->min.str);
			col->min.dbl = strtod(buff,(char **)NULL);
			Lemme_Go(buff);
			if (minlen > fieldlen)
			{
				GetLineNum(op, "MINIMUM", linecount);
				tverr(VALUE_TOO_LONG,linecount,col->index, "MINIMUM",col->min.str,fieldlen);
			}
		}

		/* ...VALID_MAXIMUM... */

		if (col->vmflag%2)
        {
			buff = copy_of(col->vmax.str);
	        col->vmax.dbl = strtod(buff,(char **)NULL);
			Lemme_Go(buff);
			if (vmaxlen > fieldlen)
            {
				GetLineNum(op, "VALID_MAXIMUM", linecount);
				tverr(VALUE_TOO_LONG,linecount,col->index,
                                  "VALID_MAXIMUM",col->vmax.str,fieldlen);
            }
        }

		/* ...VALID_MINIMUM... */

		if (col->vmflag > 1)
		{
			buff = copy_of(col->vmin.str);
			col->vmin.dbl = strtod(buff,(char **)NULL);
			Lemme_Go(buff);
			if (vminlen > fieldlen)
			{
				GetLineNum(op, "VALID_MINIMUM", linecount);
				tverr(VALUE_TOO_LONG,linecount,col->index,
									"VALID_MINIMUM",col->vmin.str,fieldlen);
			}
		}

		/* ...INVALID_CONSTANT... */

		if (col->invflag  &&  !ASCII)
        {
			buff = copy_of(col->invalid.str);
	        col->invalid.dbl = strtod(buff,(char **)NULL);
			Lemme_Go(buff);
			if (invlen > fieldlen)
            {
				GetLineNum(op, "INVALID_CONSTANT", linecount);
				tverr(VALUE_TOO_LONG,linecount,col->index,
                                  "INVALID_CONSTANT",col->invalid.str,fieldlen);
            }
        }

		/* ...MISSING_CONSTANT... */

		if (col->missflag  &&  !ASCII)
		{
			buff = copy_of(col->missing.str);
			col->missing.dbl = strtod(buff,(char **)NULL);
			Lemme_Go(buff);
			if (misslen > fieldlen)
			{
				GetLineNum(op, "MISSING_CONSTANT", linecount);
				tverr(VALUE_TOO_LONG,linecount,col->index,
									"MISSING_CONSTANT",col->missing.str,fieldlen);
			}
		}

    }

	/* But if it's a character string, make sure null max/min pointers are
	   null pointers (and not zero-values); and that any values which are 
	   given don't overflow the field:
	*/

	else
    { /* ...MAXIMUM... */

      if (col->mmflag % 2)   
      {
		  if (maxlen > fieldlen)
          {
			  GetLineNum(op, "MAXIMUM", linecount);
			  tverr(VALUE_TOO_LONG,linecount,col->index,
                                  "MAXIMUM",col->max.str,fieldlen);
          }
      }
      else
      {
		  col->max.str = NULL;
	  }

      /* ...MINIMUM... */

      if (col->mmflag > 1)
      {
		  if (minlen > fieldlen)
          {
			  GetLineNum(op, "MINIMUM", linecount);
			  tverr(VALUE_TOO_LONG,linecount,col->index,
                                  "MINIMUM",col->min.str,fieldlen);
          }
      }
      else
      {
		  col->min.str = NULL;
	  }

      /* Check for overflow in the constants, if any: */

      if (col->missflag  &&  misslen > fieldlen)
      {
		  GetLineNum(op, "MISSING_CONSTANT", linecount);
		  tverr(VALUE_TOO_LONG,linecount,col->index,
                              "MISSING_CONSTANT",col->missing.str,fieldlen);
      }

      if (col->invflag  &&  invlen > fieldlen)
      {
		  GetLineNum(op, "INVALID_CONSTANT", linecount);
		  tverr(VALUE_TOO_LONG,linecount,col->index,
                              "INVALID_CONSTANT",col->invalid.str,fieldlen);
      }
    } /* END: else */


  /* Field checks: */

  /*** MAXIMUM > MINIMUM */

  if ( col->mmflag == 3)
  {
	  if (NUMERIC_FIELD(col->type))
      {
		  if ( col->max.dbl <  col->min.dbl)
          {
			  GetLineNum(op, "MAXIMUM", linecount);
			  tverr(DBLMAX_LT_MIN,linecount,
                    col->index, col->max.dbl,  col->min.dbl);
              ++status;
          }
      }
      else 
      {
		  if (strcmp(col->max.str,col->min.str) < 0)
          {
			  GetLineNum(op, "MAXIMUM", linecount);
			  tverr(STRMAX_LT_MIN,linecount,
                    col->index, col->max.str,  col->min.str);
		      ++status;
          }
      }
  }


  /*** VALID_MAXIMUM > VALID_MINIMUM */

  if ( col->vmflag == 3)
  {
	  if (NUMERIC_FIELD(col->type))
      {
		  if ( col->vmax.dbl <  col->vmin.dbl)
          {
			  GetLineNum(op, "VALID_MAXIMUM", linecount);
			  tverr(DBLVALMAX_LT_VALMIN,linecount,
                     col->index, col->vmax.dbl,col->vmin.dbl);
              ++status;
          }
      }
      else 
      {
		  if (strcmp(col->vmax.str,col->vmin.str) < 0)
          {
			  GetLineNum(op, "VALID_MAXIMUM", linecount);
			  tverr(STRVALMAX_LT_VALMIN,linecount,
                     col->index, col->vmax.str,col->vmin.str);
              ++status;
          }
      }
  }



  /*** DERIVED_MAXMIMUM or DERIVED_MINIMUM only in numeric fields ***/

  if (col->dmflag > 0)
  {
	  if (!NUMERIC_FIELD(col->type))
      {
		  linecount = op->line_number;
		  tverr(BAD_DER_MAXMIN,linecount,col->index);
	  }
  }

  /*** DERIVED_MAXIMUM > DERIVED_MINIMUM */

  if (( col->dmflag == 3) && ( col->dmax <  col->dmin))
  {
	  GetLineNum(op, "DERIVED_MAXIMUM", linecount);
	  tverr(DERMAX_LT_DERMIN,linecount,col->index, col->dmax,col->dmax);
      ++status;
  }

  /*** DERIVED_MAXIMUM/MINIMUM must have OFFSET and SCALING factor as well */

  if ( ( col->dmflag != 0)            &&  
       ( col->offset == 0.0)          &&
       ( col->scaling_factor == 1.0))    
  {
	  linecount = op->line_number;
	  tverr(MISSING_SCALE,linecount,col->index);
      ++status;
  }


  /*** 1 <= START_BYTE <= parent object BYTES and ROW_BYTES */

  if ( col->start< 1)
  {
	  GetLineNum(op, "START BYTE", linecount);
	  tverr(STARTBYTE_LT_1,linecount,col->index, col->start);
      ++status;
  }
  if ( col->start > parent_bytes)
  {
	  GetLineNum(op, "START_BYTE", linecount);
	  tverr(STARTBYTE_GT_PAR_BYTES,linecount,
                col->index, col->start, parent_bytes);
      ++status;
  }

  if ( col->start > row_bytes)
  {
	  GetLineNum(op, "START_BYTE", linecount);
	  tverr(STARTBYTE_GT_ROW_BYTES,linecount,
                col->index, col->start, row_bytes);
      ++status;
  }
   

  /*** START_BYTE + BYTES + object_offset <= parent size and ROW_BYTES  */

  if ( col->start-1 + col->size > parent_bytes)
  {
	  GetLineNum(op, "BYTES", linecount);
	  tverr(FIELD_TOO_LONG,linecount,col->index,env);
      ++status;
  }
  if ( col->start-1 + col->size + object_offset > row_bytes)
  {
	  GetLineNum(op, "BYTES", linecount);
	  tverr(FIELD_TOO_LONG,linecount,col->index,"table row");
      ++status;
  }

  /*** SCALING_FACTOR <> 0 */

  if ( col->scaling_factor == 0.0)
  {
	  GetLineNum(op, "SCALING_FACTOR", linecount);
	  tverr(SCALE_EQ_0,linecount,col->index);
      ++status;
  }

  /*** NAME required */

  if (strcmp( col->name,"") == 0)
  {
	  tverr(UNNAMED_COLUMN,op->line_number,col->index);
      ++status;
  }

  /*** Total ITEMS x ITEM_BYTES must be <= BYTES in field */
 /*
   bytes = col->items * col->item_bytes;
   if (bytes > col->size)
   {
       GetLineNum(op, "ITEMS", linecount);
	   tverr(ITEM_BYTES_MISMATCH,linecount,col->index,bytes,col->size);
	   col->type = 0;
	   ++status;
   }
*/
  /******
    Now we've reached things that are dependent on whether or not the table
    is ASCII or binary:
   ******/

  GetLineNum(op, "FORMAT", linecount);

  if (ASCII)
  {  /*** FORMAT specifier must be appropriate to data type. */
	  if(col->format != NULL)
	  {
          if(
			((col->type==UNSIGNED_INTEGER) && (strchr(col->format,'I')==NULL))  ||
			((col->type==SIGNED_INTEGER)   && (strchr(col->format,'I')==NULL))  ||
			((col->type==REAL)             && (strpbrk(col->format,"EF")==NULL))||
			((col->type==CHARACTER)        && (strchr(col->format,'A')==NULL))  ||
			((col->type==DATE)             && (strchr(col->format,'A')==NULL))  ||
			((col->type==TIME)             && (strchr(col->format,'A')==NULL))    
			)

		  {
			tverr(FORMAT_TYPE_MISMATCH,linecount,col->index,col->format,
                            field_type[col->type]);
			++status;
		  }

		  /*** Width of FORMAT specifier must be = BYTES */

		  bytes = col->size;
		  if (col->fwidth >  bytes)
		  {
			  tverr(WIDTH_GT_BYTES,linecount,col->index,col->fwidth,bytes);
			  ++status;
		  }
		  if ( col->fwidth < bytes)
		  {
              tverr(WIDTH_LT_BYTES,linecount,col->index,col->fwidth,col->size);
			  ++status;
		  }

		  /*** Width of FORMAT specifier must be >= precision */

		  if ( col->fwidth <  col->fprecision)
		  {
              tverr(PREC_GT_WIDTH,linecount,col->index,col->fprecision,col->fwidth);
			  ++status;
		  }

		  /*** Width of FORMAT specifier for DATE and TIME fields must be 
           >= minimum: */

		  if (col->type == DATE  &&  col->fwidth < MINDATEWIDTH)
		  {
			 tverr(WIDTH_LT_MIN_DATE,linecount,col->index,col->fwidth);
		  }

		  else if (col->type == DATE  &&  col->fwidth > MAXDATEWIDTH)
		  {
			tverr(WIDTH_GT_MAX_DATE,linecount,col->index,col->fwidth);
		  }

		  else if (col->type == TIME  &&  col->fwidth < MINTIMEWIDTH)
		  {
		 	tverr(WIDTH_LT_MIN_TIME,linecount,col->index,col->fwidth,MINTIMEWIDTH);
			++status;
		  }

		  else if (col->type == TIME  &&  col->fwidth > MAXTIMEWIDTH)
		  {  
			tverr(WIDTH_GT_MAX_TIME,linecount,col->index,col->fwidth,MAXTIMEWIDTH);
			++status;
		  }
	  }
  }  /* ASCII-specific checks */

  else /* Binary-specific checks */
  {  /*** FORMAT specifier, if any, must be appropriate to data type. */
	  
      if (col->format != NULL)
      {
		  if (
             ((col->type==UNSIGNED_INTEGER) && (strchr(col->format,'I')==NULL))  ||
             ((col->type==SIGNED_INTEGER)   && (strchr(col->format,'I')==NULL))  ||
             ((col->type==REAL)             && (strpbrk(col->format,"EF")==NULL))||
             ((col->type==CHARACTER)        && (strchr(col->format,'A')==NULL))  ||
             ((col->type==DATE)             && (strchr(col->format,'A')==NULL))  ||
             ((col->type==TIME)             && (strchr(col->format,'A')==NULL))
			 )

          {
			  tverr(FORMAT_TYPE_MISMATCH,linecount,col->index,col->format,
                                field_type[col->type]);
              ++status;
          }


          /*** Width of FORMAT specifier must be >= precision */

          if ( col->fwidth <  col->fprecision)
          { 
			  tverr(PREC_GT_WIDTH,linecount,col->index,col->fprecision,col->fwidth);
              ++status;
          }

          /*** Width of FORMAT specifier for DATE and TIME fields
               must be >= minimum: 
          */

          if (col->type == DATE  &&  col->fwidth < MINDATEWIDTH)
          {
			  tverr(WIDTH_LT_MIN_DATE,linecount,col->index,col->fwidth);
              ++status;
          }

          else if (col->type == DATE  &&  col->fwidth > MAXDATEWIDTH)
          {
			  tverr(WIDTH_GT_MAX_DATE,linecount,col->index,col->fwidth);
              ++status;
          }

          else if (col->type == TIME  &&  col->fwidth < MINTIMEWIDTH)
          {
			  tverr(WIDTH_LT_MIN_TIME,linecount,col->index,col->fwidth,
                                 MINTIMEWIDTH);
              ++status;
          }

          else if (col->type == TIME  &&  col->fwidth > MAXTIMEWIDTH)
          { 
			  tverr(WIDTH_GT_MAX_TIME,linecount,col->index,col->fwidth,
                                 MAXTIMEWIDTH);
              ++status;
          }
	  } /* FORMAT check */


      /*** Numeric fields must have a reasonable number of bytes */

      bytes = col->size;

      if (col->type == SIGNED_INTEGER  ||  col->type == UNSIGNED_INTEGER)
      {
		  if (bytes != 1  &&  bytes != 2  &&  bytes != 4 && bytes != 8)
          {
			  GetLineNum(op, "BYTES", linecount);
			  tverr(INV_INT_SIZE,linecount,col->index,bytes);
              col->type = SPARE;
              ++status;
          }
      }

      if (col->type == REAL  &&  (bytes !=4  &&  bytes != 8))
      {
		  GetLineNum(op, "BYTES", linecount);

		  tverr(INV_REAL_SIZE,linecount,col->index,bytes);
          col->type = SPARE;
          ++status;
      }

   } /* Binary-specific checks */

  /* Now we return the cumulative error count as the status: */

  return status;

}

/**********************************************************************
 *$Component                                                          *
 *    CONTAINER_INFO *get_container_params (container_ptr, table_info)*
 *$Abstract                                                           *
 *    Gathers container information.		                          *
 *$Keywords                                                           *
 *			                                                          *
 *                                                                    *
 *$Inputs                                                             *
 *	   container_ptr:											      *
 *			The input is a pointer to an AGGREGATE. It should be a    *
 *			pointer to a CONTAINER object.						      *
 *$Outputs                                                            *
 *     container_info:					                              *
 *			The container_info variable will hold information about   *
 *			the container such as REPETITIONS, START_BYTE, and BYTES  *
 *			(the bytes within a container).							  *
 *$Returns                                                            *
 *    container_info:                                                 *
 *			See $outputs above.										  *
 *$Detailed_Description                                               *
 *    This routine will generate values of some of the keywords found *
 *	  in a CONTAINER object. The keyword values being gathered are    *
 *	  needed to process the columns within a CONTAINER correctly.     *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Author_and_Institution                                             *
 *    Michael D. Cayanan / J.P.L.                                     *
 *$Version_and_Date                                                   *
 *    1.0   October 10, 2002                                          *
 *$Change_history                                                     *
 *    MDC   10-10-02   Original code.                                 *
 **********************************************************************/

CONTAINER_INFO *get_container_params(OBJDESC *cnt, TABLE_INFO *table_info)
{
	CONTAINER_INFO *container_info = NULL;
	char *str = NULL;
	CONTAINER_INFO *temp = NULL;
	long linecount = 0;

	container_info = (CONTAINER_INFO *) malloc( sizeof(CONTAINER_INFO) );
	
	container_info->name = NULL;
	container_info->bytes = 0;
	container_info->columns = 0;
	container_info->container_id = 0;
	container_info->start_byte = 0;
	container_info->next_container = NULL;
	container_info->prev_container = NULL;

	/* Grab container parameters */

	/* 03-06-03 MDC - Grab container name */
	OaKwdValuetoStr("NAME", cnt, &str);
	if(str != NULL)
	{
		New_String(container_info->name, str);
		util_remove_char(container_info->name, '"');
	}

	OaKwdValuetoLong ("BYTES", cnt, &(container_info->bytes));

	OaKwdValuetoLong ("START_BYTE", cnt, &(container_info->start_byte));

	OaKwdValuetoLong ("REPETITIONS", cnt, &(container_info->repetitions));

	/* if no container is found, then this is the first container to be
	   stored. */
	if(table_info->container == NULL)
	{
		table_info->container = container_info;
	}
	else
	{
		temp = table_info->container;

		/* keep moving until you reach the end of the list */
		while( (temp->next_container) != NULL )
			temp = temp->next_container;
		
		/* Now link the object at the end of the list with the container 
		   information that we've just obtained. */
		temp->next_container = container_info;
		container_info->prev_container = temp;
	}
	
	container_info->container_id = ++(table_info->container_count);

  /* Now we'll do some error_checking on the top-level keywords we've got
     so far:
  */

  if (container_info->name == NULL)
  {
	  tverr(UNNAMED_CONTAINER,cnt->line_number,cnt->line_number);
      New_String(container_info->name,"UNNAMED");
  }

  if (container_info->repetitions < 1)
  {
	  GetLineNum(cnt, "REPETITIONS", linecount);
	  tverr(INV_REP,linecount,container_info->repetitions,cnt->line_number);
      container_info->repetitions = 1;
  }

  if ((container_info->start_byte-1 + (container_info->bytes * container_info->repetitions) + table_info->prefix_bytes) 
        > (table_info->row_bytes))
  {
	  tverr(CONTAINER_TOO_LONG,cnt->line_number,container_info->name,cnt->line_number);
  }


	return(container_info);
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void parse_FORMAT(COLUMN_INFO *col,int linecount)

  /* This routine parses the format string saved in the column definition
     and fills the related parameter fields. Note that the format may 
     contain a repetition count if the COLUMN has more than one item.
     Valid field formats are:

        iAw       i(Aw)
        iIw       i(Iw)
        iFw.d     i(Fw.d)
        iEw.d     i(Ew.d)

     where   i  =  item counts (ITEMS)
             w  =  field width
             d  =  precision (digits right of the decimal)

  */

{ char    buff[50];  /* Copy of format string */
  long    repcount;  /* repetition count: should be = items */
  char   *rem, *p;   /* conversion pointers */
  int     i, len;

  repcount = 0;

  /* We'll begin by making a copy of the format string and clipping trailing
     blanks:
  */

  strcpy(buff,col->format);
  len = strlen(buff);
  while (buff[len-1] == ' ') 
    { buff[len-1] = '\0'; 
      len--;
    }

  /* If this begins with a digit, we convert the numeric part and save it
     as the repetition count:
  */

  if (isdigit(buff[0]))
    { repcount = strtol(buff,&rem,10);
      if (repcount == 0)
        { tverr(INV_FORMAT,linecount,col->index,col->format); }
      else if (repcount != col->items)
        { tverr(BAD_REPCOUNT,linecount,col->index,col->format,col->items); }

      p = rem;
    }

  /* If not, we make sure no ITEMS were declared for the column: */

  else
    { if (col->items > 1)
        { tverr(NO_REPCOUNT,linecount,col->index,col->format); }
      p = buff; 
    }

  /* If the next character is a '(' the last character should be ')'. We
     remove these:
  */

  if (p[0] == '(')
    { p++;
      i = strlen(p);
      if (p[i-1] == ')')
        { p[i-1] = '\0'; }
      else
        { tverr(INV_FORMAT,linecount,col->index, col->format); }
     }

  /* The next character should be a type designator, either A, I, F or E.
     We check for it and branch depending on whether or not we expect a
     precision value:
  */

  if (p[0] == 'A'  ||  p[0] == 'I')
    { p++;

      /* Convert the integer width. There should be no remainder: */

      col->fwidth = (int) strtol(p,&rem,10);
      if (col->fwidth < 1  ||  strlen(rem) > 0)
        { tverr(INV_FORMAT,linecount,col->index, col->format); }
      col->fprecision = 0;
    }

  else if (p[0] == 'F'  ||  p[0] || 'E' )
    { p++;

      /* Convert the first integer as the width:   */

      col->fwidth = (int)strtol(p,&rem,10);
      if (col->fwidth < 1)
        { tverr(INV_FORMAT,linecount,col->index, col->format); }

      /* The remainder should begin with a decimal point, after which is
         the precision:
      */

      p = rem;
      if (p[0] != '.')
        { tverr(INV_FORMAT,linecount,col->index, col->format);
          col->fprecision = 0;
        }
      else
        { p++;
          col->fprecision = (int)strtol(p,&rem,10);

          /* Precision may be zero here, but there still should be no 
             remainder:
          */

          if (col->fprecision < 0  ||  strlen(rem) > 0)
            { tverr(INV_FORMAT,linecount,col->index, col->format); }
        }
    }

  else
    { tverr(INV_FORMAT,linecount,col->index, col->format); }

  /* And we're done: */

  return;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/




/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void checkfilename(char *file, int linecount)

  /* This routine checks the file name syntax to make sure it meets PDS
     constraints.
  */

{ int   i;           /* Loop/index  */
  int   lc,uc;           /* Case flags  */
  int   pd, bad;
  char c;

  /* This is a quick & dirty check. It won't catch subtle stuff, but it 
     does check for the easy mistakes:
  */

  lc = uc = pd = bad = 0;

  /* Loop through the letters, checking for mixed case and disallowed
     characters:
  */

  for (i=0; i<strlen(file); i++)
    { c = file[i];

      if (!isalnum(c) && c != '_'  &&  c != '.')
        { bad++; }

      if (islower(c)) lc++;
      if (isupper(c)) uc++;
      if (c == '.')   pd++;
    }

  /* All right, now we check for errors: */

  if (bad > 0)
    { tverr(INV_CHAR_IN_FN,linecount,file); }

  if (lc > 0  &&  uc > 0)
    { tverr(MIXED_CASE_IN_FN,linecount,file); }

  if (pd < 1)
    { tverr(NO_EXT_IN_FN,linecount,file); }

  if (pd > 1)
    { tverr(MULT_EXT_IN_FN,linecount,file); }

  /* Done. */

  return;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

int table_type(char *typestring)

  /* This routine determines whether the type string passed in corresponds
     to a table-type object (i.e., a TABLE, SPECTRUM or SERIES).
  */

{ char *p;

  /* The type identifier must either be the whole string, or the last part
     of the string with a leading '_' character. We'll check for the 
     easy cases first:
  */

  if (strcmp(typestring,"TABLE") == 0)
    { return TRUE; }
  if (strcmp(typestring,"SPECTRUM") == 0)
    { return TRUE; }
  if (strcmp(typestring,"SERIES") == 0)
    { return TRUE; }

  /* Failing that, we check for the type in the last part of the string: */

  p = strrchr(typestring,'_');

  /* If there is no underscore, we're done. Otherwise, we check again for 
     a match:
  */

  if (p == NULL)
    { return FALSE; }

  if (strcmp(p,"_TABLE") == 0)
    { return TRUE; }
  if (strcmp(p,"_SPECTRUM") == 0)
    { return TRUE; }
  if (strcmp(p,"_SERIES") == 0)
    { return TRUE; }

  /* No match - no table: */

  return FALSE;
}

/*---------------------------------------------------------------------------*/
/** 11-11-02 MDC															**/
/** Added this routine to find a table object in a tree since these "if"    **/
/** statements seem to be located in many places in the nasaview code. This **/
/** routine will search through the different types of table object names   **/
/** and return a pointer to that particular table object. This routine		**/
/** makes it simpler to find table objects rather than always inputting     **/
/** those "if" statements everywhere in the code.							**/
/**																			**/
/** Inputs:																	**/
/**   1)  start_object -> a pointer to an object in the tree where the		**/
/**			  routine can start looking for table objects.					**/
/**	  2)  keyword_name ->													**/
/**   3)  keyword_vale ->													**/
/**   4)  object_position -> This specifies the position of the table		**/
/**			  object you are searching for. For instance, if "2" is passed  **/
/**			  in, then this routine will search for the 2nd table-type		**/
/**			  object in the tree.											**/
/**	  5)  search_scope -> How the ODL routine will go about finding the     **/
/**			  table-type objects.											**/
/*---------------------------------------------------------------------------*/

OBJDESC *FindTableObj( OBJDESC *start_object, char *keyword_name, 
					 char *keyword_value, unsigned long object_position, 
					 unsigned short search_scope)
{
	OBJDESC *table_pointer = NULL;

	if ((table_pointer = OdlFindObjDesc (start_object, "*TABLE", keyword_name, keyword_value, 
		object_position, search_scope)) == NULL)
	{
            if ((table_pointer = OdlFindObjDesc (start_object, "*SERIES", keyword_name, 
				keyword_value, object_position, search_scope)) == NULL)
			{
				if ((table_pointer = OdlFindObjDesc (start_object, "*SPECTRUM", keyword_name, 
					keyword_value, object_position, search_scope)) == NULL)
				{
					if ((table_pointer = OdlFindObjDesc (start_object, "*PALETTE", keyword_name, 
						keyword_value, object_position, search_scope)) == NULL)
					{
						if ((table_pointer = OdlFindObjDesc (start_object, "*GAZETTEER", keyword_name, 
							keyword_value, object_position, search_scope)) == NULL)
						{
							if ((table_pointer = OdlFindObjDesc (start_object, "*HEADER", keyword_name, 
								keyword_value, object_position, search_scope)) == NULL)
							{
								return 0;
							}
						}
					}
				}
			}
	}
	return table_pointer;
}


/*-----------------------------------------------------------------------*/
/*																		 */
/*-----------------------------------------------------------------------*/

void Exit_System(char *message)
{
	printf("%s, Program is terminating\n\n", message);
	exit (2);
}





/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
