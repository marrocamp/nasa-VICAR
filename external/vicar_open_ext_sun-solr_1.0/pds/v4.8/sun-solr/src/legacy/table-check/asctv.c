/* 
   ASCTV.C

   This file contains the routines ued to read and verify the contents of
   ASCII table objects.

   09 April 2001, acr: Extract from the 'atv' routine.

   17 May 2001, acr: Added string terminator to line buffer to stop accidental
                     reading past the end of string.

   28 Jun 2001, acr: Adjusted output spacing.

   14 Jan 2002, acr: Modifications to add batch mode processing

   26 Feb 2002, acr: Increased precision of max/min found in file

   28 Mar 2006, mdc: Made corrections to correctly read and check each line
                     in an ascii table.
*/

#include <stdio.h>


#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>

#include "values.h"
#include "table_check.h"
#include "tverr.h"
#include "tvutil.h"

/*---------------------------------------------------------------------------
  Global Variables 
*/

extern FILE    *label, *data;              /* input files                  */
extern FILE    *report;                    /* output file                  */
extern char     barline[100],blanks[100];  /* report file dingbats         */
extern char     dblbar[100];
extern char    *field_type[];
extern char    *error_msg[];
extern int      batch_mode;

extern struct tvstruct *pdstv;
extern TABLE_INFO   *table_top;                 /* Table object list            */


long int read_ascii_table_data(TABLE_INFO *, int, int);
/*---------------------------------------------------------------------------
  Local Functions, invisible to outside routines. Note that some of the names
  are used, but with different specific functionalities, in both the 'asctv.c'
  and 'bintv.c' source files.
*/

void checkchar(char *line, long int *non_blank, long int *special,
               int byte_offset, int length);
void check_column(char *line,   int row_count,  COLUMN_INFO *col, 
                  int   offset, int error_count[], FILE   *report);
void checknumfld(char *hold, int length, int row_count, COLUMN_INFO *col,
                 int error_count[], FILE *report);
void writetables(int max_length, long int *non_blanks, long int *special,
                 FILE *report);


/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

long int read_ascii_table_data(TABLE_INFO *tab, int column_count, int record_bytes)

/* Main driver routine for reading the ASCII data of the table object pointed
   to by 'tab'. Conflicts between the label descriptions and the actual 
   contents of the data file are flagged as errors.
*/

{ FILE      *data;                   /* Data file pointer              */
  int        i;                  /* Loop/subscript                 */
  long int   table_records;          /* records in the current table   */
  char      *line;                   /* input buffer                   */
  int        length;                 /* string length                  */
  int        max_length   = 0;       /* maximum record length          */
  int        min_length   = MAXINT;  /* minimum record length          */
  int        offset;                 /* Total offset to start of field */
  int        numcount, chrcount;     /* Field type counts              */
  int        datecount,timecount;
  int        sparecount,unkcount;
  int        field_error;            /* Parameter flag                 */
  int        error_count[ERRORLISTCOUNT];  /* error counters */

  COLUMN_INFO    *col;

  /* These pointers are used to gather character type counts for the table
     lines:
  */
  long int  *non_blanks;
  long int  *special;


  /* Allocate an input record buffer and write a string terminator at 
     the end:
  */

  line = (char *)malloc(record_bytes + 1);


  line[record_bytes] = '\0';

  /* Allocate and initialize the character count arrays: */

  length     = tab->row_bytes * sizeof(long int);
  non_blanks = (long int *)malloc(length);
  special    = (long int *)malloc(length);
  memset((char *)non_blanks, '\0', length);
  memset((char *)special,    '\0', length);

  /* Open the data file and position the file pointer to the indicated
     offset:
  */

/* 03-31-06 MDC - In order to preserver the CR/LF characters at the end of
    each line in a data file, we need to open up the file in binary. 
	Otherwise, the C compiler will turn these into newline characters.

	This effectively fixes the program to read each line properly.
*/
  if ((data=fopen(tab->fname,"rb")) == NULL)
  {
	  for (i=0; i<(int)strlen(tab->fname); ++i) 
          tab->fname[i] = tolower(tab->fname[i]);

      if ((data=fopen(tab->fname,"rb")) == NULL)
      {
		  for (i=0; i<(int)strlen(tab->fname); ++i)
              tab->fname[i] = toupper(tab->fname[i]);

		  if ((data=fopen(tab->fname,"rb")) == NULL)
          {
			  tverr(DATA_FILE_NOT_FOUND,0,tab->fname,strerror(errno));
              if (pdstv->batch_mode) fprintf (report,"OK\n");
              fclose(report);
              exit(20);
          }
      }
  }

  /* The data file is now open.  If an offset was indicated, advance the
     file pointer to the correct location:
  */

  if (tab->data_location > 1)
  {
	  if (fseek(data,tab->data_location,SEEK_SET) != 0)
      {
		  tverr(FILE_SEEK_FAILED,0);
          if (pdstv->batch_mode) fprintf (report,"OK\n");
          fclose(report);
          exit(21);
      }
  }

  /* Report file banner: */

  if (!pdstv->batch_mode)
    { fprintf (report,"\n%79.79s\n\n",dblbar);
      fprintf (report,"DATA ERRORS\n------------\n\n");
    }

  /* Initialize the error count array: */

  memset((char *)error_count, '\0', sizeof error_count);

  /* Loop through the records: */

  table_records = 0;


  while ( (fread(line,record_bytes,1,data) == 1) &&
	      (table_records < tab->row_count) )
  {
	  ++table_records;
 
      /* line length checks: */

      length      = strlen(line);
      field_error = FALSE;        
      
	  if (length != record_bytes)
	  {
		  printerror(LENGTH_NE_RECORD_BYTES,field_error,error_count,
                     table_records,NULL,report);
      }
      max_length = (length > max_length)? length : max_length;
      min_length = (length < min_length)? length : min_length;

      /* Check for PDS-required carriage-return before the newline: */

	  if (line[length-2] != '\r')
          printerror(NO_CR,field_error,error_count,table_records,NULL,report);


      /* Check for blanks and special characters: */

      checkchar(line,non_blanks,special,tab->prefix_bytes,tab->row_bytes);

      /* Loop through the column fields, checking each individually: */

      offset = tab->prefix_bytes;
      col = tab->columns;
      while (col)
      {
		  check_column(line, table_records, col, offset, error_count, report);
          col = col->next;
      }
  }
  fclose(data);

  /* Make sure we read as many rows as we expected (this will not report
     extra rows at the end):
  */

  if (table_records != tab->row_count)
    { tverr(TOO_FEW_TABLE_ROWS,table_records,table_records,tab->row_count); }

  /* Details of the various fields read and errors discovered are only 
     display in a report file, not in batch mode:
  */

  if (!pdstv->batch_mode)
    { 
      /* Report file banner: */

      fprintf (report,"\n%79.79s\n\n",dblbar);
      fprintf (report, "SUMMARY\n-------\n\n");


      /**---------------------------**/
      /* Display label parameters    */

      print_table_attributes(tab,report);

      /* Get counts of column types: */

      column_count = count_column_types(tab->columns,&numcount,&chrcount,
                                        &datecount,&timecount,&sparecount,
                                        &unkcount);

      /* Write summary values for numeric fields: */

      if (numcount > 0)
        { fprintf (report,"\n\n");
          fprintf (report,"Data Values Summary for Numeric Fields. ");
          fprintf (report," Note that extrema below are the actual\n");
          fprintf (report,"data values from the file, ");
          fprintf (report,"without scaling or offset:\n\n");
          fprintf (report,"                                Minimum");
          fprintf (report,"     Maximum");
          fprintf (report,"  INVALID  MISSING  Bad Data\n");
          fprintf (report,"Col    Name            Items     Value");
          fprintf (report,"       Value ");
          fprintf (report,"   Fields   Fields    Found\n\n");
    
          writenumsum(tab->columns,report); 
        }


      /* Write summary value for character fields: */

      if ((numcount + sparecount) < column_count)
        { fprintf (report,"\n\n");
          fprintf (report,"Data Values Summary for Non-numeric Fields:\n\n");
          fprintf (report,"%31.31sMinimum    Maximum  "," ");
          fprintf (report,"INVALID  MISSING  Bad Data\n"," ");
          fprintf (report,"Column  Name            Items    Bytes      Bytes");
          fprintf (report,"    Fields   Fields    Found\n\n"," ");
    
          writecharsum(tab->columns,report); 
        }

      /* Show character counts: */

      writetables(tab->row_bytes,non_blanks,special,report);

 
      /* Show error count totals: */

      fprintf (report,"\n\nTotal Error Counts by Type:\n\n");
      for (i=0; i<ERRORLISTCOUNT; ++i)
          fprintf (report, "  %6d : %-68s\n",error_count[i],error_msg[i]);
    }


  /* And we're done with this table: */

  Lemme_Go(line);
  Lemme_Go(non_blanks);
  Lemme_Go(special);

  return table_records;
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void checkchar(char *line, long int *non_blank, long int *special, 
               int byte_offset, int length)

/* Routine to tally the non-blanks and special characters encountered in the
   record segment passed in.  "Special characters" are whitespace characters
   that are not the blank character.

   03-31-06   MDC - Check that the char value is greater than 0. For some reason,
                    the isspace crashes upon a negative input.
*/

{ int     i;

  for (i=0; i<length; ++i)
    { if (line[byte_offset+i] != ' ') 
        { ++non_blank[i];
/*          if (isspace(line[byte_offset+i])) ++special[i]; */
			if ( (line[byte_offset+i] >= 0) && (isspace(line[byte_offset+i])) )
				++special[i];
        }
    }
  return;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void check_column(char *line,   int row_count,     COLUMN_INFO *col, 
                  int   offset, int error_count[], FILE *report)

  /* Routine to check the contents of a single COLUMN object.
 
     Parameters;
         line          input line buffer
         row_count     data record count
         col           pointer to column structure
         offset        total byte offset to start of parent object
         error_count   error count accumulator array
         report        output device

  */

{
	int      i;
	int      rec_offset;   /* byte offset from start of record */
	char    *hold;         /* temporary holding place for conversion strings */
	int      type,bytes;   /* shorthand values */

	/* SPARE columns are ignored:  */

    if (col->type == SPARE) return;

	/* A COLUMN may have one or more items. Each COLUMN is initialized to 
	   have 1 item, both at creation and as the attributes are being read.
	   So, we can treat all COLUMNs the same and just loop through the 
	   items defined in the structure (usually just one).

	   For each item, we determine the actual byte offset from the start
       of the record, transfer the data to the appropriate holding area,
       convert numeric values as needed, and call a routine to check the
       specific type of data indicated.
	*/

	for (i=0; i<col->items; i++)
    {
		rec_offset = offset + (col->start - 1) + (col->item_offset * i);
	    bytes      = col->size;
		type       = col->type;

		/* We copy the field into a freshly allocated holding array: */

		hold = (char *)malloc(bytes+1);
		memcpy(hold,line+rec_offset,bytes);
		hold[bytes]='\0';

		/* Now calling a type-specific chek routine: */

		if (NUMERIC_FIELD(type))
        { checknumfld (hold,bytes,row_count,col,error_count,report); }
		else if (type == DATE)
        { checkdatefld(hold,bytes,row_count,col,error_count,report); }
		else if (type == TIME)
        { checktimefld(hold,bytes,row_count,col,error_count,report); }
		else
        { checkcharfld(hold,bytes,row_count,col,error_count,report); }
    }

	/* All done. */

	Lemme_Go(hold);

	return;
}


/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void checknumfld(char *hold, int length, int row_count, COLUMN_INFO *col,
                 int error_count[], FILE *report)

  /* Routine to check the value and format of an ASCII numeric string 
     against the label attributes.
  */

{ 
  int      integer;      /* integer test space */
  double   real;         /* real test space */
  char    *error;        /* error detecting flag */
  double   adjusted_value; /* value once offset and scaling are applied */
  int      blank;        /* 1 if the field is blank */
  int      bad_value;    /* error tracking flag */
  int      field_error = TRUE;  /* flag for error display */


  /* We ignore non-numeric fields entirely: */

  if (!NUMERIC_FIELD(col->type)) return;

  /* Check for a blank field and either flag it or ignore it (as indicated
     by the input parameters):
  */

  blank  = strspn(hold," ") == length;
  if ((pdstv->flag_blanks) &&  blank)
    { printerror(BLANK_FIELD, field_error, error_count, row_count,
                 col, report);
    }
  if (blank) return;

  /* We'll look for missing/invalid data flag values by doing a string 
     comparison. To avoid trouble we'll call a subroutine to trim blanks
     for purposes of comparison. If we find missing or inavlid flags, we
     can return immediately, since there's nothing more to check:
  */

  if (col->invflag  &&  trimcmp(col->invalid.str,hold) == 0)
    { col->invalidcount++;
      return;
    }

  if (col->missflag  &&  trimcmp(col->missing.str,hold) == 0)
    { col->missingcount++;
      return;
    }

  bad_value = FALSE;

  /* Now we convert to an appropriate numeric type. Strings should convert
     without non-blank remainders, so we check to make sure this is true:
  */

  if (col->type == UNSIGNED_INTEGER  ||  col->type == SIGNED_INTEGER)
  {
	  integer=strtol(hold,&error,10);
      if (error != NULL  &&  strspn(error," ") != strlen(error))
      {
		  printerror(INVALID_INTEGER, field_error, error_count, 
                     row_count, col, report);
          bad_value = TRUE;
      }
      real = (double)integer;
  }
  else if (col->type == REAL)
  {
	  real = strtod(hold,&error);
      if (error != NULL  &&  strspn(error," ") != strlen(error))
      {
		  printerror(INVALID_REAL, field_error, error_count,
                     row_count, col, report);
          bad_value = TRUE;
      }
  }


  /* ...Now we're ready to begin the value checks... */

  /* If the field contains an actual data value (as opposed to a flag),
     we check it against the extrema and save the actual max/min found:
  */

  col->maxfound.dbl = (real > col->maxfound.dbl)? real : col->maxfound.dbl;
  col->minfound.dbl = (real < col->minfound.dbl)? real : col->minfound.dbl;

  adjusted_value = real * col->scaling_factor + col->offset;


  /* ...MAXIMUM... */

  if (col->mmflag%2  &&  real>col->max.dbl)
  {
	  printerror(GREATER_THAN_MAX, field_error, error_count, 
                 row_count, col, report);
      bad_value = TRUE;
  }

  /* ...MINIMUM... */

  if (col->mmflag>1  &&  real<col->min.dbl)
  {
	  printerror(LESS_THAN_MIN, field_error, error_count,
                 row_count, col, report);
      bad_value = TRUE;
  }

  /* ...DERIVED_MAXIMUM... */

  if (col->dmflag%2  &&  adjusted_value>col->dmax)
  {
	  printerror(GREATER_THAN_DMAX, field_error, error_count, 
                 row_count, col, report);
	  bad_value = TRUE;
  }

  /* ...DERIVED_MINIMUM... */

  if (col->dmflag>1  &&  adjusted_value<col->dmin)
  {
	  printerror(LESS_THAN_DMIN, field_error, error_count,
                 row_count, col, report);
      bad_value = TRUE;
  }

  /* ...VALID_MAXMIMUM... */

  if (col->vmflag%2  &&  real>col->vmax.dbl)
  {
	  printerror(GREATER_THAN_VMAX, field_error, error_count,
                 row_count, col, report);
      bad_value = TRUE;
  }

  /* ...VALID_MINIMUM... */

  if (col->vmflag>1  &&  real<col->vmin.dbl)
  {
	  printerror(LESS_THAN_VMIN, field_error, error_count,
                 row_count, col, report);
      bad_value = TRUE;
  }


  /* Finally, we check the formatting of the field: */

  length = strlen(hold);
  if (col->format[0] == 'E'  &&  strpbrk(hold,"EeDd") == NULL)
  {
	  printerror(E_FORMAT_MISSING_EXPONENT, field_error, error_count, 
                 row_count, col, report);
      bad_value = TRUE;
  }
  else if (col->format[0] == 'F'  &&   hold[length-col->fprecision-1] != '.')
  {
	  printerror(DECIMAL_NOT_ALIGNED, field_error, error_count, 
                 row_count, col, report);
      bad_value = TRUE;
  }
  else if (col->format[0] == 'I'  &&  hold[length-1] == ' ')
  {
	  printerror(INTEGER_NOT_RIGHT_JUSTIFIED, field_error, error_count,
                 row_count, col, report);
      bad_value = TRUE;
  }

  /* Update the bad value counter and we're done: */

  if (bad_value) col->badcount++;
  
  return;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void writetables(int length, long int non_blanks[], long int special[], 
                 FILE *report)

/* Routine to output the character check tables.  The characters are numbered
   from 1..length, but the table columns are number 0-9 for ease of reading.
   This means the very first position is left blank.
*/

{
	int    row, col;

	fprintf (report, "\n\n");
	fprintf (report, "Number of Non-Blank Characters per Table Column:\n\n");
	fprintf (report, "          0      1      2      3      4      5");
	fprintf (report,     "      6      7      8      9\n");
	fprintf (report, "----------------------------------------------");
	fprintf (report,     "-------------------------------\n");
	for (row=0; row <= length; row+=10)
    {
		fprintf (report, "%5d: ", row);
	    for (col=0; col<10; ++col)
        {
			if (row == 0  &&  col == 0) 
              fprintf (report,"       ");
	        else if(row+col <= length)
              fprintf (report," %6d", non_blanks[row+col-1]);
        }
		fprintf(report,"\n");
    }
      

	fprintf (report, "\n\n");
	fprintf (report, "Number of Special Characters per Table Column:\n\n");
	fprintf (report, "          0      1      2      3      4      5");
	fprintf (report,     "      6      7      8      9\n");
	fprintf (report, "----------------------------------------------");
	fprintf (report,     "-------------------------------\n");
	for (row=0; row <= length; row+=10)
    {
		fprintf (report, "%5d: ", row);
	    for (col=0; col<10; ++col)
        {
			if (row == 0  &&  col == 0) 
              fprintf (report,"       ");
	        else if (row+col <= length)
              fprintf (report," %6d", special[row+col-1]);
		}
		fprintf(report,"\n");
    }

	return;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
