#include "pdsdef.h"
#include "pdsglob.h"
#include "odlinter.h"
#include "errordef.h"
#include "fiodef.h"
#include "label.h"

/*  Program: TAB2LAB

  Description: Program for generating PDS labels from tables.  The process
	       starts with a 'skeleton' label that serves as a model for
	       the labels to be generated.  The skeleton label is read in
	       and placed on an ODL tree.  Secondly, an ASCII table is
	       opened via a PDS label that describes its content and format.
	       Then, for each selected row of the table, the fields in the
	       row are substituted into the skeleton label and the resulting
	       label is written to a file.  Each column field of the table
	       must correspond to an attribute in the label. Any attributes
	       in the label that don't correspond to a table column are not
	       changed (i.e., they are static, retaining the same value they
	       had in the skeleton). The value of an attribute in a skeleton
	       label should have the same data type and characteristics
	       (i.e., number of decimal places and units) as is desired in
	       the output label.

  Author:  Randy Davis, University of Colorado LASP

  Creation Date:  09 March 1991
  Last Revision:  16 April 1991

  History:

    Version 1.1 - 16 April 1991 - Randy Davis, U. of Colorado
      Revised for use in generating MDIM CD-ROM image labels.

    Version 1.2 - 28 October 1991 - Marti DeMore, Jet Propulsion Lab
      Changed table file open statement from "r" mode to "rb" mode.
      Changed to use PDS CN error handling routines, so that this
	 sould be linked with the same parser as the PDS Label Library.
      Added include of pdsdef.h and errordef.h for same reason.
      Changed output file name creation so that it is not IMDIM specific.
      Changed SelectRows routine to terminate when user enters 0, since
	 EOF on standard input doesn't work for Unix.
      Changed SetAttributeValues routine to insert file names in sequences
	 and strip blanks from file names. Also added code to handle the
	 PDS UNKNOWN and N/A strings in date and time fields.
      Added the horrible routine PatchLabel to change the unknown dates
	 and times back to their string values.

   Version 1.3 - 20 March 1992 - M. DeMore, JPL
       Changed most ints to longs for PC usage
   Version 2.0 - 3 June 1992 - M. DeMore, JPL
       Corrected VMS input bugs.  Added ability to handle duplicate keywords
       by doing positional matching, or by using "object.keyword" as the
       name of the column in the table label.  Corrected bug that caused
       program to use COLUMNS keyword for some things, and the actual
       number of COLUMN objects for others.  Actual number of COLUMN objects
       is now always used. Added column numbers to the summary listing.
       Added code to prompt the user for the number of the column to use
       as the output label file base name (column FILE_NAME is the default),
       or to generate file name automatically if the user wants to.
   June 15, 2005 - M. Cayanan, JPL
       Added Software disclaimer message to the help screen
*/

#ifdef SUN_UNIX
#define SEEK_SET 0
#endif
#define TAB2LAB_VERSION  "2.6"

/* The following definition sets the maximum number of table columns that
   can be handled by the program.  To handle more, simply make this value
   larger */
#define MAXCOLUMNS 50



#ifndef SUN_UNIX
  int  GetLabel           (AGGREGATE *, char *, int, char *[]);
  int  GetTable           ();
  void GetColumnInfo      (int, char *[]);
  int  SelectRows         ();
  int  MakeOutputLabels   (int, char *[]);
  AGGREGATE ClassifyAll   (AGGREGATE, AGGREGATE);
  void SetAttributeValues (char *);
  int PatchLabel          (AGGREGATE);
  void PrintUsage         ();
#else
  int  GetLabel           ();
  int  GetTable           ();
  void GetColumnInfo      ();
  int  SelectRows         ();
  int  MakeOutputLabels   ();
  AGGREGATE ClassifyAll   ();
  void SetAttributeValues ();
  int PatchLabel          ();
  void PrintUsage         ();
#endif
/* The following are flags which control program behavior */
/*
int trim_blanks = FALSE;
int no_prompt = FALSE;
int use_label_info = FALSE;
*/
struct column_info
{
    PARAMETER   pointer;    /* Pointer to target parameter node           */
    VALUE_TYPE  type;       /* Type of value                              */
    long        start_byte; /* Offset to first byte of field              */
    long        bytes;      /* Length of field, in bytes                  */
    long	  items;      /* Number of items in column                  */
    long        offset;     /* Offset between items in columns            */
}; /* The following serves as the root node for the skeleton label's tree */

AGGREGATE  skeleton_tree;     /* Pointer to skeleton label's tree root    */

  /* Following are the root node for the table label's ODL tree and a
     pointer to the table object within the label */

AGGREGATE  table_tree;        /* Pointer to table label's tree root       */
AGGREGATE  table_node;        /* Pointer to table object within label     */

/* The following is used to manipulate the table file */

FILE *table_file;             /* File descriptor for table                */

/* The following is information on the columns of the table               */

long  ncolumns;             /* Number of columns in table                 */

/* The following indicates which column to use for label file name        */
int label_file_col;
/* The following give information on the rows of the table */
long   rows;                 /* Number of rows in table                    */
long   row_bytes;            /* Length of each table row, in bytes         */
/* The following specify the portion of the table to be processed         */

long   start_row;            /* First row to be processed                  */
long   end_row;              /* Last row to be processed                   */
int trim_blanks = FALSE;
int no_prompt = FALSE;
int use_label_info = FALSE;

extern int ODLlinenumber_flag;
struct column_info column[MAXCOLUMNS];void main(int argc, char *argv [])
{

  /* Local variables */

  int  status;            /* Function return status:
			       TRUE  - Processing is to continue
			       FALSE - Processing is to be terminated       */
   int i;
   char sinput [10];

   /* turn off SFDU warnings */

   pds_verbose = FALSE;

   /* Greet the user */

   printf ("\nTAB2LAB - Generate PDS Labels From Table - Version %s\n\n",
	  TAB2LAB_VERSION);

   /* set up the PDS label library */

   if (!lab_setup ())
   {
      lab_print_messages ();
      printf ("\n>>> Cannot set up label scratch files\n");
      return;
   }

   /* If the user only wants program usage, display it and exit */

   for (i = 0 ; i < argc; i++)
      if (strcmp (argv[i], "-u") == 0)
   {
      PrintUsage ();
      return;
   }

   /* set the no prompt mode if the user asked for it */

   for (i = 0 ; i < argc; i++)
      if (strcmp (argv[i], "-np") == 0)
      no_prompt = TRUE;

   /* warn the user about this program's need for a correct label */

   if (!no_prompt)
   {
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

      printf ("TAB2LAB requires a correct PDS label for the table of data\n");
      printf ("values to be inserted into the skeleton label. If this\n");
      printf ("program does not work correctly, try verifying your table\n");
      printf ("label with the PDS Table Browser.\n");
   }

  /* Read in the skeleton label */

  if (GetLabel (&skeleton_tree, "skeleton", argc, argv))
    {
      /* Read in the table's label and validate it */

      status = GetLabel (&table_tree, "table description", argc, argv);
      if (status)
	{
	  /* Open the table */

	  status = GetTable ();
	  if (status)
	  {
	     /* set the trim blanks flag based on the command line */

	    for (i = 0; i < argc; i++)
	    {
	       if (strcmp (argv[i], "-t") == 0)
	       {
		  trim_blanks = TRUE;
		  break;
	       }
	       else if (strcmp (argv [i], "-nt") == 0)
	       {
		  trim_blanks = FALSE;
		  break;
	       }
	    }

	    /* If it wasn't on the command line and we're not in no
	       prompt mode, then prompt for the trim blanks flag */

	    if (i == argc && !no_prompt)
	    {
	       printf ("\nTrim trailing blanks from character fields (y/n)? ");
	       gets (sinput);
	       *sinput = toupper (*sinput);
	       if (*sinput == 'Y') trim_blanks = TRUE;
	    }

	    /* get the "use label for formatting" flag from the command
	       line */

	    for (i = 0; i < argc; i++)
	    {
	       if (strcmp (argv[i], "-r") == 0)
	       {
		  use_label_info = TRUE;
		  break;
	       }
	       else if (strcmp (argv [i], "-nr") == 0)
	       {
		  use_label_info = FALSE;
		  break;
	       }
	    }

	    /* If it wasn't on the command line and we're not in no
	       prompt mode, then prompt for the format label flag */

	    if (i == argc && !no_prompt)
	    {
	       printf ("\nUse file format information in label (y/n)? ");
	       gets (sinput);
	       *sinput = toupper (*sinput);
	       if (*sinput == 'Y') use_label_info = TRUE;
	    }

	    /* if we're not using the file format info in the label then
	       set the label writing conditions to stream by default */

	    if (!use_label_info)
	    {
	       strcpy (pds_record_term, "\n");
	       pds_default_reclen = 0;
	    }
	      /* Get needed information on every table column */

	      GetColumnInfo (argc, argv);
	      do
		 {
		   /* Select the rows in the table to be processed */

		   status = SelectRows ();

		   if (status)
		     {
		       /* For every selected row, substitute the column
			  values into the appropriate attribute in the
			  skeleton label */

		       status = MakeOutputLabels (argc, argv);
		     }
		 } while (status && !no_prompt);
	    }
	}
    }
    /* cleanup after the label library */

    printf ("\n");
    lab_exit ();
    return;
}

int GetLabel (AGGREGATE *tree, char *label_type, int num_args, char *arglist[])
{
  char sinput[80];                    /* Buffer to hold input from user     */
  char option [3];
  int i;
  int label_status;

  if (strcmp (label_type, "skeleton") == 0)
     strcpy (option, "-s");
  else if (strcmp (label_type, "table description") == 0)
     strcpy (option, "-d");
  else
     strcpy (option, "");

  for (i = 0; i < num_args; i++)
     if (strcmp (arglist [i], option) == 0) break;

  if (i >= num_args - 1 && !no_prompt)
  {
     /* Loop until a file with a label has been processed */

     do
     {
	printf ("\nEnter name of file containing the %s label: ", label_type);

	if (gets (sinput) == NULL)
	{
	   /* End-of-file was entered by user to terminate the program */

	   return (0);
	}

       /* Open the file and read in the label */

	*tree = lab_read_label (sinput, &label_status);
	if (*tree == NULL)
	{
	  /* Input file doesn't exist: give the user another chance */

	   printf ("\n>>> The %s file doesn't exist or file error encountered", label_type);
	}
    }
    while (*tree == NULL);
  }
  else if (i >= num_args - 1)
  {
     printf ("\n>>> Missing argument: %s (%s label)", option, label_type);
     return (0);
  }
  else
  {
     *tree = lab_read_label (arglist [i+1], &label_status);
     if (*tree == NULL)
     {
	printf ("\n>>> The %s file doesn't exist or file error encountered", label_type);
	return (0);
     }
  }
  lab_print_messages ();

  if (label_status != PDS_SUCCESS)
  {
     lab_clear_messages ();
     if (!no_prompt)
     {
	printf ("\n>>> The %s label has errors or warnings", label_type);
	printf ("\n\nDo you want to continue anyway? [y or n]: ");

	gets (sinput);
	if (sinput[0] != 'y' && sinput[0] != 'Y')
	{
	   return (0);
	}
     }
     else
     {
	printf ("\n>>> The %s label has errors or warnings", label_type);
	return (0);
     }
  }
  return (1);
}


int GetTable ()
{
  AGGREGATE objdef;                   /* Dummy node for table definition    */
  PARAMETER parameter;                /* Parameter node pointing to table   */
  VALUE     data;                     /* Value node with table name         */
  char      sinput[80];               /* Buffer to hold input from user     */

  table_node = FindObject (table_tree, "TABLE", NULL);

  if (table_node == NULL)
  {
    printf ("\n>>> Sorry -- Couldn't find a table object");
    printf (" within the table description label\n");
    return (0);
  }

  /* Classify the objects in the label */

  objdef = NewAggregate (NULL, KA_OBJECT, "TABLE", "OBJECT_DEFINITION");
  ClassifyAll (table_node, objdef);
  RemoveAggregate (objdef);

  /* Make sure the table is in ASCII format.  This program cannot handle
     binary tables at this time */

  parameter = FindParameter (table_node, "FORMAT");
  if (parameter == NULL)
    {
      parameter = FindParameter (table_node, "INTERCHANGE_FORMAT");
    }

  if (parameter != NULL)
    {
      data = FirstValue (parameter);
      if (data != NULL && data->item.type == TV_SYMBOL)
	{
	  if (strcmp (data->item.value.string, "ASCII") != 0)
	    {
	      printf ("\n>>> Sorry - Can't process tables that aren't");
	      printf (" in ASCII format\n");
	      return (0);
	    }
	}
     }

  /* Open the table using the pointer in the label */

  parameter = FindParameter (table_tree, table_node->name);
  if (parameter != NULL && parameter->node_kind == KP_POINTER)
    {
      data = FirstValue (parameter);
      if (data != NULL &&
	  (data->item.type == TV_STRING || data->item.type == TV_SYMBOL))
	{

	  /* note that PCs need binary open statements to handle long
	     record lengths, but that VMS will strip out record terminators
	     if you open binary. */
#ifdef VAX
	  table_file = fopen (data->item.value.string, "r");
#else
	  table_file = fopen (data->item.value.string, "rb");
#endif
	  if (table_file != NULL)
	    {
	      /* File containing table was opened successfully */

	      return (1);
	    }
	}
     }

  /* Table couldn't be opened via the label: give the user a chance */

  if (!no_prompt)
  {
     printf ("\n>>> Couldn't open the table file through its label");
     printf ("\n\nDo you want to enter the table file name yourself? [y or n]: ");

     gets (sinput);
     if (sinput[0] != 'y' && sinput[0] != 'Y')
     {
	 /* Exit the program */

	 return (0);
     }

     do
     {
	printf ("\nEnter name of file containing the table: ");
	if (gets (sinput) == NULL)
	{
	   return (0);
	}

	table_file = fopen (sinput, "rb");
	if (table_file == NULL)
	{
	   printf ("\n>>> Named file doesn't exist or file error encountered");
	}
     } while (table_file == NULL);
  }
  else
  {
     printf ("\n>>> Couldn't open the table file through its label\n");
     return (0);
  }
  return (1);
}

void GetColumnInfo (int num_args, char *arglist[])
{
  AGGREGATE   node;
  AGGREGATE   target;
  PARAMETER   parameter;
  VALUE       data;
  int         i;
  int         j;
  int         new_label_file_col = -1;
  char        sinput[80];      /* Current input line                        */
  char        *dot;
  char        temp_targ [80];
  char        temp_name [80];

  /* Print out header to column information table */

  if (!no_prompt)
     printf ("\n\n\t\t\tInformation on Table Columns\n\n%5s%21s%6s%4s%6s%4s%5s%20s\n",
		  "Col #", "Column Name", "Strt", "Len", "Itms", "Off", "Type",
		  "Target Object/Group");

  node = table_node;
  label_file_col = -1;

  for (i=0; ((node=FindNextObject(table_node,node,NULL,"COLUMN"))!=NULL); i++)
    {

      /* Get the start byte, number of bytes in the column, number of
	 items in the column, and the offset between items.  The
	 start byte and length are supplied for every column; the
	 item count and item offset are optional (if omitted, the
	 count of items in the column is assumed to be 1) */

      data = FirstValue (FindParameter (node, "START_BYTE"));
      column[i].start_byte = data->item.value.integer.number;

      data = FirstValue (FindParameter (node, "BYTES"));
      column[i].bytes = data->item.value.integer.number;

      parameter = FindParameter (node, "ITEMS");
      if (parameter == NULL)
	{
	  column[i].items  = 1;
	  column[i].offset = 0;
	}
      else
	{
	  data = FirstValue (parameter);
	  column[i].items = data->item.value.integer.number;

	  data = FirstValue (FindParameter (node, "ITEM_OFFSET"));
	  column[i].offset = data->item.value.integer.number;
	}

      column[i].pointer = NULL;
      column[i].type = TV_NULL;

      target = skeleton_tree;
      while (target != NULL)
	{
	  strcpy (temp_targ, node -> name);
	  if ((dot = strrchr (temp_targ, (int) '.')) != NULL)
	  {
	     *dot = '\0';
	     strcpy (temp_name, (char *) (dot + 1));
	  }
	  else
	  {
	     strcpy (temp_name, node -> name);
	     strcpy (temp_targ, "");
	  }

	  if (strcmp (temp_targ, "") == 0 ||
	      strcmp (temp_targ, target -> name) == 0)
	  {
	     parameter = FindParameter (target, temp_name);
	     for (j = 0; j < i; j++)
		 if (column[j].pointer == parameter) break;
	  }
	  else
	     parameter = NULL;
	  if (j == i && parameter != NULL)
	    {
	      if (strcmp (temp_name, "FILE_NAME") == 0 && label_file_col == -1)
		  label_file_col = i + 1;

	      column[i].pointer = parameter;

	      data = FirstValue (parameter);
	      column[i].type = data->item.type;
	      break;
	    }

	  target = NextAggregate (target);
	}
      if (!no_prompt)
	 printf ("\n%3d %22s%5ld%5ld%5ld%5ld%4d  %-22s", i + 1, temp_name,
		 column[i].start_byte, column[i].bytes,
		 column[i].items, column[i].offset, column[i].type,
		 (column[i].pointer != NULL) ? target->name : "*NONE*");
    }

    if (!no_prompt) printf ("\n");

  ncolumns = i;

  for (i = 0; i < num_args - 1; i++)
     if (strcmp (arglist [i], "-c") == 0) break;
  if (i < num_args - 1)
  {
     if (strspn (arglist [i + 1], "0123456789") == strlen (arglist [i + 1]))
     {
	label_file_col = atoi (arglist [i + 1]);
	if (label_file_col < 0 || label_file_col > ncolumns)
	{
	   printf ("\n>>> Label file column number is invalid. Set to 0.");
	   label_file_col = 0;
	}
     }
  }
  for (i = 0; i < num_args - 1; i++)
  {
     if (strcmp (arglist [i], "-b") == 0)
		 break;
  }
  if (i >= num_args - 1 && !no_prompt)
  {
     do
     {
	if (label_file_col < 0) label_file_col = 0;
	printf ("\n\nEnter number of column containing label file base name");
	printf ("\nor 0 for automatically generated file names.  Default is %d: ",
		 label_file_col);
	gets (sinput);
	if (strcmp (sinput, "") != 0)
	   new_label_file_col = atoi (sinput);
	else
	   new_label_file_col = label_file_col;
	if (new_label_file_col < 0 || new_label_file_col > ncolumns)
	{
	   printf ("\n>>> Column number is invalid; please re-enter");
	   new_label_file_col = -1;
	}
     } while (new_label_file_col == -1);
     label_file_col = new_label_file_col;
  }
  return;
}

int SelectRows ()
{
	VALUE       data;            /* Pointer to value data for attribute       */
	char        sinput[80];      /* Current input line                        */

  /* Get number of rows in table */

  data = FirstValue (FindParameter (table_node, "ROWS"));
  rows = data->item.value.integer.number;

  /* Get number of bytes in a row */

  data = FirstValue (FindParameter (table_node, "ROW_BYTES"));
  row_bytes = data->item.value.integer.number;

  /* Get starting row number from user */


   if (!no_prompt)
   {
      do
      {
	 printf ("\nEnter number of first row to be processed [1..%ld]: ", rows);
	 if (!gets (sinput))
	 {
	     /* Program terminated by end-of-file from user */

	     return (0);
	  }

	 start_row = atol (sinput);
	 if (start_row == 0)
	 {
	     /* Program terminated by end-of-file from user */

	    return (0);
	 }

	 if (start_row < 1 || start_row > rows)
	 {
	     printf ("\n>>> Starting row number is invalid; please re-enter");
	     start_row = 0;
	 }
       } while (start_row == 0);

     /* Get ending row number from user */
    do
    {
	 printf ("\nEnter number of last row to be processed [%ld..%ld]: ",
		 start_row, rows);
	 if (!gets (sinput))
	 {
	     /* Program terminated by end-of-file from user */

	     return (0);
	 }

	 end_row = atol (sinput);
	 if (end_row == 0)
	 {
	    /* Program terminated by end-of-file from user */

	    return (0);
	 }
	 if (end_row < start_row || end_row > rows)
	 {
	     printf ("\n>>> Ending row number is invalid; please re-enter");
	     end_row = 0;
	  }
       } while (end_row == 0);
  }
  else
  {
     start_row = 1;
     end_row = rows;
  }
  return (1);
}

int MakeOutputLabels (int num_args, char *arglist [])
{
  char  *row_data = NULL;
  char  filename[256];
  long offset;
  int status;
  long irow;
  char  outfile[256];
  AGGREGATE temp_tree;
  VALUE temp_val;
  long  i;
  int label_status;
  char c;
  char *dot;

  ODLlinenumber_flag = 0;
  strcpy (outfile, "");
  if (label_file_col <= 0)
  {
     for (i = 0; i < num_args - 1; i++)
	if (strcmp (arglist [i], "-b") == 0) break;
     if (i < num_args - 1)
	strcpy (outfile, arglist [i + 1]);
  }
  if (label_file_col <= 0 && !no_prompt && strcmp (outfile, "") == 0)
  {
     printf ("\nEnter base name for output labels (without extension): ");
     gets (outfile);
  }
  else if (label_file_col <= 0 && no_prompt && strcmp (outfile, "") == 0)
  {
     printf ("\n>>> You must specify either the -c, -b, or -p option");
     return (0);
  }

  /* Position to proper starting place in table */

  row_data = (char *) malloc (row_bytes + 1);
  if (row_data == NULL)
  {
     printf("\n>>> Sorry, not enough memory for a data file of this size");
     return (0);
  }
  row_data[row_bytes] = '\0';
  offset = (start_row-1)*row_bytes;
  status = fseek (table_file, offset, SEEK_SET);
  if (status)
    {
      printf ("\n>>> Sorry, error while positioning to first row to be read");
      return (0);
    }

  printf ("\n");
  for (irow = start_row ; irow <= end_row ; irow++)
    {
      /* Note that VMS will return records ending with LF even if the format
	 originally stated CR LF as the record terminator.  Therefore, we
	 have no choice but to read until we see the record terminator,
	 even if the record length is less than row_bytes. It's either
	 this, or keep fiddling with ROW_BYTES in the label until you get
	 it right for the record format of this file. Can't use fgets in
	 case the file format does not include carriage control. Eee gads! */

#ifdef VAX
      for (c = '\0', i = 0; i < row_bytes && !feof (table_file) && c != '\n'; i++)
	  c = row_data [i] = fgetc (table_file);
      if (i == row_bytes || c == '\n')
	 status = 1;
      else
	 status = 0;
      row_data [i] = '\0';
#else
      status = fread (row_data, (int) row_bytes, 1, table_file);
      row_data [row_bytes] = '\0';
#endif

      if (status != 1)
	{
	  printf (">>> Sorry, something went wrong while reading record %ld\n",
		  irow);
	  return (0);
	}

      SetAttributeValues (row_data);

      /* Create the name of the output file. */

      if (label_file_col <= 0)
	 sprintf (filename, "%s%ld.lbl", outfile, irow);
      else
      {
	 temp_val = FirstValue (column[label_file_col - 1].pointer);
	 strcpy (filename, temp_val -> item.value.string);
	 if ((dot = strrchr (filename, (int) '.')))
             *dot = '\0';
	 strcat (filename, ".lbl");
      }

      /* Open a file for the output label and write it out */
      temp_tree = CopyAggregate (skeleton_tree);
      if (PatchLabel (temp_tree))
	 printf (">>> Warning: N/A or UNKNOWN date/time value found in label %s\n",
			   filename);

      lab_write_label (temp_tree, filename, use_label_info, 0, &label_status);
      if (label_status != PDS_SUCCESS || lab_has_messages ())
      {
	 printf (">>> Warnings or errors in label %s\n", filename);
	 lab_print_messages ();
	 lab_clear_messages ();
      }
      else
	 printf (">>> Writing row %ld to label %s\n", irow, filename);
      lab_remove_label (temp_tree, &label_status);
    }
  free (row_data);
  return (1);
}


void SetAttributeValues (char *inrow)
{
  VALUE_DATA  nitem;
  VALUE_DATA  vitem;
  VALUE       data;
  char        c;
  long        start_byte;
  long        bytes;
  long        items;
  long        offset;
  long        loc;
  int         i;
  long        j;
  PARAMETER   save_parm = NULL;
  VALUE       next;
  char *unk_date = "1700-01-01";
  char *na_date = "2200-01-01";
  char *unk_datetime = "1700-01-01T01:01:01Z";
  char *na_datetime = "2200-01-01T01:01:01Z";

  for (i=0 ; i < ncolumns ; i++)
    {
      if (column[i].pointer != NULL)
	{
	  data = FirstValue (column[i].pointer);

	  /* Get the first value node for this attribute.  This node
	     will serve as a template for the values we create here */

	  CutValue (data);
	  vitem = data->item;
	  free (data);

	  if (column[i].pointer -> node_kind == KP_POINTER)
	  {
	     /* save copy of parameter, if this is a pointer attribute */

	     save_parm = CopyParameter (column[i].pointer);
	  }

	  /* Remove any other values for this attribute */

	  data = FirstValue (column[i].pointer);
	  while (data != NULL)
	  {
	     data = RemoveValue (data);
	  }

	  /* Retrieve information on the current column */

	  start_byte = column[i].start_byte;
	  bytes  = column[i].bytes;
	  items  = column[i].items;
	  offset = column[i].offset;

	  loc = start_byte - 1;
	  for (j = 1; j <= items; j++)
	   {
	     /* Save the character after the current field and temporarily
		set it to a zero, so the text is zero terminated for use
		by the conversion routines */

	     c = inrow[loc+bytes];
	     inrow[loc+bytes] = '\0';

	     if (j > 1 && strspn (&inrow[loc], " ") == bytes)
	       {
		 /* This is the second or subsequent item in a multi-item
		    field and it's all blanks: ignore it */

		 continue;
	       }

	     /* Process the current value */

	     switch (column[i].type)
	       {
		 case TV_INTEGER:
		   nitem = ODLConvertInteger (&inrow[loc], (int) bytes);
		   vitem.value.integer.number = nitem.value.integer.number;
		   break;

		 case TV_REAL:
		   nitem = ODLConvertReal (&inrow[loc], (int) bytes);
		   vitem.value.real.number = nitem.value.real.number;
		   break;

		 case TV_DATE:
		   if (strncmp (&inrow[loc], "UNKNOWN", 7) == 0 ||
		       strncmp (&inrow[loc], "UNK:", 4) == 0 ||
		       strncmp (&inrow[loc], "UNK", 3) == 0)
		      nitem = ODLConvertDate (unk_date, strlen (unk_date));
		   else if (strncmp (&inrow[loc], "N/A", 3) == 0)
		      nitem = ODLConvertDate (na_date, strlen (na_date));
		   else
		      nitem = ODLConvertDate (&inrow[loc], (int) bytes);
		   vitem.value.date_time = nitem.value.date_time;
		   break;

		 case TV_TIME:
		   if (strncmp (&inrow[loc], "UNKNOWN", 7) == 0 ||
		       strncmp (&inrow[loc], "UNK:", 4) == 0 ||
		       strncmp (&inrow[loc], "UNK", 3) == 0)
		      nitem = ODLConvertDateTime (unk_datetime,
				     strlen (unk_datetime));
		   else if (strncmp (&inrow[loc], "N/A", 3) == 0)
		      nitem = ODLConvertDateTime (na_datetime, strlen (na_datetime));
		   else
		      nitem = ODLConvertTime (&inrow[loc], (int) bytes);
		   vitem.value.date_time = nitem.value.date_time;
		   break;

		 case TV_DATE_TIME:
		   if (strncmp (&inrow[loc], "UNKNOWN", 7) == 0 ||
		       strncmp (&inrow[loc], "UNK:", 4) == 0 ||
		       strncmp (&inrow[loc], "UNK", 3) == 0)
		      nitem = ODLConvertDateTime(unk_datetime, strlen (unk_datetime));
		   else if (strncmp (&inrow[loc], "N/A", 3) == 0)
		      nitem = ODLConvertDateTime (na_datetime, strlen (na_datetime));
		   else
		      nitem = ODLConvertDateTime (&inrow[loc], (int) bytes);
		   vitem.value.date_time = nitem.value.date_time;
		   break;

		 case TV_STRING:
		   nitem = ODLConvertString (&inrow[loc], (int) bytes);

		   /* If this is a file pointer or the trim_blanks flag
		      is set, strip trailing blanks */

		   if (trim_blanks ||
		       column[i].pointer -> node_kind == KP_POINTER)
		   {
		      while (nitem.length > 0 &&
			     nitem.value.string[nitem.length-1] == ' ')
			 nitem.length--;
		      nitem.value.string[nitem.length] = '\0';
		   }

		   if (vitem.value.string != NULL)
		      free (vitem.value.string);
		   vitem.value.string = nitem.value.string;
		   vitem.length = nitem.length;
		   break;

		 case TV_SYMBOL:
		   nitem = ODLConvertSymbol (&inrow[loc], (int) bytes, 2);

		   /* If this is a file pointer or the trim_blanks flag
		      is set, strip trailing blanks */

		   if (trim_blanks ||
		       column[i].pointer -> node_kind == KP_POINTER)
		   {
		      while (nitem.length > 0 &&
			     nitem.value.string[nitem.length-1] == ' ')
			 nitem.length--;
		      nitem.value.string[nitem.length] = '\0';
		   }
		   if (vitem.value.string != NULL)
		      free (vitem.value.string);
		   vitem.value.string = nitem.value.string;
		   vitem.length = nitem.length;
		   break;

		 default:
		    /* This should never happen */

		    printf ("\n>>> Unexpected conversion in SetAttributeValue");
		    break;
	       }

	     NewValue (column[i].pointer, &vitem);
	     inrow[loc+bytes] = c;
	     loc += offset;

	   }
	   /* If this was a pointer attribute then copy the rest of
	      the old pointer sequence to the parameter. This only
	      makes sense for single item fields */

	   if (items == 1 && save_parm != NULL)
	   {
	      data = FirstValue(save_parm);
	      while (data != NULL)
	      {
		next = NextValue (data);
		data = CutValue (data);
		PasteValue (column[i].pointer, data);
		data = next;
	      }
	     RemoveParameter (save_parm);
	     save_parm = NULL;
	   }
	}
    }

  return;
}

int PatchLabel (AGGREGATE label_ptr)
/*AGGREGATE label_ptr; */
{
   PARAMETER parm;
   VALUE val;
   int unknown_found = FALSE;
   int na_found = FALSE;

   for (;label_ptr != NULL; label_ptr = NextAggregate (label_ptr))
   {
       for (parm = FirstParameter (label_ptr); parm != NULL;
		    parm = NextParameter (parm))
       {
           for (val = FirstValue (parm); val != NULL;
				val = NextValue (val))
		   {
				switch (val -> item.type)
				{
					case TV_DATE:
					case TV_DATE_TIME:
					case TV_TIME:
					if (val -> item.value.date_time.year == 1700)
					{
						val -> item.type = TV_SYMBOL;
						val -> item.value.string =
									(char *) malloc (strlen ("UNK") + 1);
						strcpy (val -> item.value.string, "UNK");
						val -> item.length = strlen ("UNK");
						val -> item.format = 1;
						unknown_found = TRUE;
					}
					else if (val -> item.value.date_time.year == 2200)
					{
						val -> item.type = TV_STRING;
						val -> item.value.string =
								(char *) malloc (strlen ("N/A") + 1);
						strcpy (val -> item.value.string, "N/A");
						val -> item.length = strlen ("N/A");
						val -> item.format = 0;
						na_found = TRUE;
					}
					break;
				}
		   }
	   }
    }
    return (na_found || unknown_found);
}

void PrintUsage ()
{	 
      printf ("Usage:\n\n");
      printf ("tab2lab -[t,nt,p,np,u,r,nr] -s skeleton -d table-desc -b base-name -c file-column\n\n");
      printf ("Where:\n");
      printf ("   -t:        Enables blank stripping in data fields.\n");
      printf ("   -nt:       Disables blank stripping in data fields (default).\n");
      printf ("   -p:        Enables verbose/prompt mode (default).\n");
      printf ("   -np:       Disables verbose/prompt mode.\n");
      printf ("   -u:        Displays this text.\n");
      printf ("   -r:        Enables label record formatting.\n");
      printf ("   -nr:       Disables label record formatting (default).\n");
      printf ("   -s <file>: Specifies label skeleton (template).\n");
      printf ("   -d <file>: Specifies table description label.\n");
      printf ("   -b <name>: Specifies label file base name.\n");
      printf ("   -c <col>:  Specified label file base column number.\n");
      printf ("\nThe program will prompt for missing required arguments.\n");
}
