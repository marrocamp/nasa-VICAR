/*
   BINTV.C

   This file contains the routine used to read and verify the contents of
   binary table objects.

   15 March 2001, acr: Extracted from the 'btv' routine.

   12 Jun 2001, acr: Fixed stupid problem in using RECORD_BYTES rather then
                     ROW_BYTES for reading the table. Errors reported relative
                     to table row count.
   28 Jun 2001, acr: Adjusted output spacing.

   14 Jan 2002, acr: Modifications to add batch mode processing

   26 Feb 2002, acr: Increased precision of max/min found in file

*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <math.h>

#include "values.h"
#include "table_check.h"
#include "tverr.h"
#include "tvutil.h"
#include "formtype.h"


#define  SIGDIFF(A,B) (fabs(A-B)/A)>1.e-06

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


/*===========================================================================
  Static Arrays
  ===========================================================================*/


/*---------------------------------------------------------------------------
  Local Functions, invisible to outside routines.
*/

static void check_column(char *line,   int   row_count, COLUMN_INFO *col, 
                         int   offset, int error_count[], FILE *report);
static void checknumfld (double value, int   row_count, COLUMN_INFO *col, 
                         int error_count[], FILE *report);
static double convert_to_double(union typeconv *value, COLUMN_INFO *col);

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

long int read_binary_table_data(TABLE_INFO *tab, int column_count, int record_bytes)

/* Main driver routine for reading the binary data of the table object
   pointed to by 'tab'. Conflicts between the label descriptions and  
   the actual contents of the data file are flagged as errors.
*/

{
	FILE      *data;                   /* Data file pointer        */
	int        i;                  /* Loop/subscript           */
	long int   table_records;          /* records in the current table */
	void      *line;                   /* input buffer             */
	int        max_length   = 0;       /* maximum record length    */
	int        min_length   = MAXINT;  /* minimum record length    */
	int        offset;                 /* Total offset to start of field */
	int        numcount, chrcount;     /* Field type counts        */
	int        datecount,timecount;
	int        sparecount,unkcount;
	int        error_count[ERRORLISTCOUNT];  /* error counters */
	int        index = 0;

	COLUMN_INFO    *col;

	/* Allocate an input line buffer: */

	line = (void *)malloc(tab->row_bytes + 1);

	/* Open the data file and position the file pointer to the indicated
	   offset:
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
		/* 03-03-06 MDC - subtract 1 to point to the right place */
/*		if (fseek(data,tab->data_location,SEEK_SET) != 0) */
		if (fseek(data,(tab->data_location -1),SEEK_SET) != 0)
        {
			tverr(FILE_SEEK_FAILED,0);
	        if (pdstv->batch_mode) fprintf (report,"OK\n");
		    fclose(report);
			exit(21);
		}
    }

	/* Report file banner: */

	if (!pdstv->batch_mode)
    {
		fprintf (report,"\n%79.79s\n\n",dblbar);
	    fprintf (report,"DATA ERRORS\n------------\n\n");
    }

	/* Initialize the error count array: */

	memset((char *)error_count, '\0', sizeof error_count);

	/* Loop through the records: */

	table_records = 0;

	while ( (fread(line,tab->row_bytes,1,data) == 1)  &&
          (table_records < tab->row_count))
    {
		++table_records;
		/*printf("Checking Record #%d\n",table_records); */
		/* Loop through the fields, checking each individually: */

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
  {
	  tverr(TOO_FEW_TABLE_ROWS,table_records,table_records,tab->row_count);
  }


  /* Detailed reports are only produced in non-batch mode: */

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
      {
		  fprintf (report,"\n\n");
          fprintf (report,"Data Values Summary for Numeric Fields. ");
          fprintf (report," Note that extrema below are the actual\n");
          fprintf (report,"data values from the file, ");
          fprintf (report,"without scaling or offset:\n\n");
          fprintf (report,"                                Minimum");
          fprintf (report,"     Maximum");
          fprintf (report,"  INVALID  MISSING  Bad Data\n");
          fprintf (report," Col   Name            Items     Value");
          fprintf (report,"       Value ");
          fprintf (report,"   Fields   Fields    Found\n\n");

          writenumsum(tab->columns,report); 
      }


      /* Write summary value for character fields: */

      if ((numcount + sparecount) < column_count)
      {
		  fprintf (report,"\n\n");
          fprintf (report,"Data Values Summary for Non-numeric Fields:\n\n");
          fprintf (report,"%31.31sMinimum    Maximum  "," ");
          fprintf (report,"INVALID  MISSING  Bad Data\n"," ");
          fprintf (report,"Column  Name            Items    Bytes      Bytes");
          fprintf (report,"    Fields   Fields    Found\n\n"," ");

          writecharsum(tab->columns,report); 
      }
 
      /* Show error count totals: */

      fprintf (report,"\n\nTotal Error Counts by Type:\n\n");
      for (i=0; i<ERRORLISTCOUNT; ++i)
          fprintf (report, "  %6d : %-68s\n",error_count[i],error_msg[i]);
    }


  /* And we're done with this table: */

  Lemme_Go(line);

  return table_records;
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void check_column(char *line,   int row_count,     COLUMN_INFO *col, 
                  int   offset, int error_count[], FILE *report)

  /* Routine to check the contents of a single COLUMN object.
 
     Parameters;
         line          input line buffer
         row_count     table row counter
         col           pointer to column structure
         offset        total byte offset to start of parent object
         error_count   error count accumulator array
         report        output device

  */

{
	union typeconv value;     /* Type conversion structure */
	int      i;
	int      rec_offset;   /* byte offset from start of record */
	double   real;         /* real test space */
	char    *hold;         /* temporary holding place for conversion strings */
	int      type,bytes;   /* shorthand values */

	/* SPARE columns are ignored: */

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
	    
		bytes = col->size;

		type       = col->type;

		/* Character types get copied into a holding array. Numeric types are
		   copied to the 'value' structure and converted to double precision.
		*/

		if (NUMERIC_FIELD(type))
        {
			memcpy(&value,line+rec_offset,bytes);
	        real = convert_to_double(&value,col);
        }

		else
        {
			hold = (char *)malloc(bytes+1);
	        memcpy(hold,line+rec_offset,bytes);
		    hold[bytes] = '\0';
        }

		/* Now we call a data check routine based on type: */

		if (NUMERIC_FIELD(type))
        { checknumfld(real,row_count,col,error_count,report); }
		else if (type == DATE)
        { checkdatefld(hold,bytes,row_count,col,error_count,report); }
		else if (type == TIME)
        { checktimefld(hold,bytes,row_count,col,error_count,report); }
		else
        { checkcharfld(hold,bytes,row_count,col,error_count,report); }

		/* Free any temporary space allocated: */

		if (!NUMERIC_FIELD(type)) Lemme_Go(hold);
    }

  /* All done. */

  return;
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

double convert_to_double(union typeconv *value, COLUMN_INFO *col)

  /* Routine to input the numeric value passed in to double precision real.
     The value parameters are contained in the 'col' structure.
  */

{
    double real;        /* holding place for return value */
	int    type,bytes;  /* convenience variables */
	unsigned char *result;
	int sign_flag = 0;

	type  = col->type;
    bytes = col->size;

	if(type == UNSIGNED_INTEGER || type == SIGNED_INTEGER)
	{
		/* Determine first if the value is a signed or unsigned value */
		if(type == UNSIGNED_INTEGER) sign_flag = FALSE;
		else if(type == SIGNED_INTEGER) sign_flag = TRUE;
		
		if(col->msbflag == LSB)
			result = ft_lsb_integer( value->byte, col->size, sign_flag);
		else if(col->msbflag == MSB)
			result = ft_msb_integer( value->byte, col->size, sign_flag);

		/* If there was no result, then write an error message */
		if(result == NULL)
		{
			if(type == UNSIGNED_INTEGER)
			{
				tverr(OOPS_NUMBER_SIZE,1,"An unsigned integer",col->index,bytes);
				col->type = CHARACTER;
				real = 0.0;
			}
			if(type == SIGNED_INTEGER)
			{
				tverr(OOPS_NUMBER_SIZE,1,"A signed integer",col->index,bytes);
				col->type = CHARACTER;
				real = 0.0;
			}
		}
		else
			real = atof(result);
		
		Lemme_Go(result);
	}

	else if (type == REAL)
    {
		/*
		if (bytes == 4)
        { real = (double)value->r4; }
	    else if (bytes == 8)
        { real = value->r8; }
		*/
		result = ft_msb_ieee_real (value->byte, bytes);
		
		if(result != NULL)
		{
			real = atof(result);
		}
		else
        {
			tverr(OOPS_NUMBER_SIZE,1,"A real number",col->index,bytes);
			col->type = CHARACTER;
			real = 0.0;
        }
		Lemme_Go(result);
    }

	/* Return the converted value: */

	return real;
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void checknumfld(double value, int row_count, COLUMN_INFO *col, 
                 int error_count[], FILE *report)

  /* Routine to read and check a numeric field against the its label
     attributes.

     Parameters:
        value         double-precision version of the data value
        row_count     table row count
        col           column structure
        error_count   error count accumulator array
        report        output unit

  */

{ 
  double    adjusted;    /* value with offset and scaling applied */
  double    dabs;        /* double-precision absolute value holder */
  int       bad_value;   /* error tracking flag */
  int       FIELD_ERROR = TRUE;


  /* First check for potential special numeric values. If we find them, we
     can count them and return, since no further checking or comparison is
     needed:
  */

  /* ...INVALID_CONSTANT... */

  if (col->invflag)
    { dabs = value - col->invalid.dbl;
      dabs = (dabs > 0.0) ? dabs : -dabs;
      if (dabs < 1.e-6)
        { col->invalidcount++;
          return;
        }
    }

  /* ...MISSING_CONSTANT... */

  if (col->missflag)
    { dabs = value - col->missing.dbl;
      dabs = (dabs > 0.0) ? dabs : -dabs;
      if (dabs < 1.e-6)
        { col->missingcount++;
          return;
        }
    }


  /* If this contains an actual value, check it against the extrema in the
     label and save the max/min found in the data: 
  */

  col->maxfound.dbl = (value > col->maxfound.dbl)? value : col->maxfound.dbl;
  col->minfound.dbl = (value < col->minfound.dbl)? value : col->minfound.dbl;

  adjusted = value * col->scaling_factor + col->offset;
  bad_value = FALSE;

  /* ...MAXIMUM... */

  if (col->mmflag%2  &&  value>col->max.dbl  &&  SIGDIFF(value,col->max.dbl))
    { printerror(GREATER_THAN_MAX,FIELD_ERROR,error_count,row_count,
                 col,report);
      bad_value = TRUE;
    }

  /* ...MINIMUM... */

  if (col->mmflag>1  &&  value<col->min.dbl  &&  SIGDIFF(value,col->min.dbl))
    { printerror(LESS_THAN_MIN,FIELD_ERROR,error_count,row_count,
                 col,report);
      bad_value = TRUE;
    }

  /* ...DERIVED_MAXIMUM... */

  if (col->dmflag%2  &&  adjusted>col->dmax  &&  SIGDIFF(adjusted,col->dmax))
    { printerror(GREATER_THAN_DMAX,FIELD_ERROR,error_count,row_count,
                 col,report);
      bad_value = TRUE;
    }

  /* ...DERIVED_MINIMUM... */

  if (col->dmflag>1  &&  adjusted<col->dmin  &&  SIGDIFF(adjusted,col->dmin))
    { printerror(LESS_THAN_DMIN,FIELD_ERROR,error_count,row_count,
                 col,report);
      bad_value = TRUE;
    }

  /* ...VALID_MAXIMUM... */

  if (col->vmflag%2  &&  value>col->vmax.dbl  &&  SIGDIFF(value,col->vmax.dbl))
    { printerror(GREATER_THAN_VMAX,FIELD_ERROR,error_count,row_count,
                 col,report);
      bad_value = TRUE;
    }

  /* ...VALID_MAXIMUM... */

  if (col->vmflag>1  &&  value<col->vmin.dbl  &&  SIGDIFF(value,col->vmin.dbl))
    { printerror(LESS_THAN_VMIN,FIELD_ERROR,error_count,row_count,
                 col,report);
      bad_value = TRUE;
    }


  /* Update the bad data count and return: */

  if (bad_value) col->badcount++;
  

  return;
}


/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


