/*

  TVUTIL.C

  This file contains general utilities for the PDS Table Verifier tool.

  16 March 2001, A.C.Raugh.

  17 May 2001, acr: Modifications to checkdatefld and checktimefld for
                    YYYY-DDD format dates and truncated times.
  11 Jun 2001, acr: Modified 'breakline' to not signal underscore-within-quotes
                    warning for pointers or FILE_NAME keywords.
  28 Jun 2001, acr: Output cleanup: removed item list for max/min, since only
                    overall max/min is recorded anyway; adjusted spacing.

  14 Jan 2002, acr: Modifications to add batch mode processing.

  26 Feb 2002, acr: Increased precision of max/min found in file

  07 Mar 2002, acr: Added check for valid max/min before displaying. A null
                    marker is now displayed if no valid values were encountered
                    in the file.
  08 Mar 2002, acr: Cleaned up table attributes listing for containers

*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>

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
extern int      batch_mode;
extern struct tvstruct *pdstv;

extern TABLE_INFO   *table_top;                 /* Table object list            */


/*---------------------------------------------------------------------------
  Local Functions
*/

void set_string_maxmin(char *, COLUMN_INFO *);
COLUMN_INFO *print_container_attributes(CONTAINER_INFO *, COLUMN_INFO *, FILE *, TABLE_INFO *);

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

int getline(char *line, FILE *ifp, int *linecount)

/* Routine to read the next ODL line in the label.  It ignores comment lines
   and clips leading blanks from the input line.  It returns zero if end
   of file is encountered.

   Parameters:

     char *line;       input line buffer  
     FILE *ifp;        input file pointer 
     int  *linecount;  input line number  


   03 Nov 1994, acr:  It also checks for pairs of double-quotes and will
                      continue reading (and dumping) lines until a matching
                      end-quote is found.  It does NOT, however, perform
                      any sanity-checking on the position of the quotes.
*/

{ char *ptr;
  int   i;
  int   done;
  int   length;          /* length of input string */
  int   lblanks;         /* number of leading blanks */
  char  inptline[MAXRECORDLENGTH];
  static char whitespace[] = { ' ', '\r', '\n', '\0'};


  done = FALSE;

  while (!done)
    { if ((ptr=fgets(line,MAXRECORDLENGTH,ifp)) != NULL)
        { (*linecount)++;
          lblanks = strspn(ptr,whitespace);
          length = strlen(ptr);
          if (length!=0  &&  lblanks!=length) /* blank/null line check */
            { ptr = ptr + lblanks;
              if (strstr(ptr,"/*") != ptr)    /* comment check */
                { strcpy(line,ptr); 

                  /* If this line ends with an '=' append the next line (and
                     hope for the best):
                  */

                  i = strlen(line)-1;
                  while (isspace(line[i])) i--;
                  if (line[i] == '=')
                    { line[i+1] = ' ';
                      line[i+2] = '\0';
                      ptr=fgets(inptline,MAXRECORDLENGTH,ifp);
                      (*linecount)++;
                      line = strcat(line,inptline);
                    }

                  done = TRUE;
                }
            }
        }
      else
          return 0;
    }

  /* Before returning, check to see if there is one or two double quotes 
     in this line.  If there are two, return normally; but if there is
     only one, read in lines until a matching double-quote is found.
     These lines are discarded.
  */

  if ((ptr=strpbrk(line,"\"")) != NULL)

    { /* Found one quote.  Look for a second: */

      ptr++;

      while (strpbrk(ptr,"\"") == NULL)
        { if ((ptr=fgets(inptline,MAXRECORDLENGTH,ifp)) == NULL) 
            { return 0; }
          else
            { (*linecount)++; }
        }
    }
     
  return 1;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

int breakline(char *line, char *keyword, char *value, int linenum)

/* Routine to break a label line up into keyword and value.  If no "=" is
   found a status of 0 is returned; successful status is 1. The keyword is 
   forced to upper case; the value is checked for underscores within double
   quotes.  Blanks are trimmed from the end of the value and keyword.

   09 Nov 1994, acr: discard comments on end of line
   08 Mar 2000, acr: Move check for underscores inside quotes to here; fix
                     null-value part handling.
   06 Jan 2003, acr: Removed blank-trimming from value part, to preserve
                     significant blanks in character strings. 

*/

{ int    i,j;
  int    length;
  int    status;   /* return status value */
  int    inquotes; /* TRUE if value is in quotes */

  status   = 1;
  inquotes = FALSE;

  /* transfer letters until a blank or "=" is found: */

  length = strlen(line);
  i = 0;
  while (i < length  &&  line[i] != ' '  &&  line[i] != '=')
    { keyword[i] = toupper(line[i]);
      ++i;
    }

  /* delete blanks on the end, if any: */

  while (isspace(keyword[--i]));
  keyword[i+1] = '\0';   /* add the string terminator */

  /* If there is no value field, we're done. Pass back an empty string
     and the "no value" flag:
  */

  if (!(strchr(line,'=')))
    { value[0] = '\0';
      return 0;
    }


  /* Now, find the '=' and pass it and following blanks: */

  while (line[i] != '=') ++i;
  ++i;
  while (line[i] == ' ') ++i;

/*===========================================================================
   This check is being disabled until PDS DEs can sort out what the heck to
   do about this.
*/
  /* Check the value field for both underscores and double quotes - this
     is an error according to the way the PDS verifiers work.
  */
/*
  if (strpbrk(line+i,"_") && strpbrk(line+i,"\"")) 
    { if (strstr(keyword,"DESC") == NULL  &&  strstr(keyword,"NOTE") == NULL
          &&  strstr(keyword,"FILE_NAME") == NULL  &&  keyword[0] != '^')
        { tverr(QUOTED_UNDERSCORES,linenum); }
    }
*/
/*=========================================================================*/

  /* Copy characters over for the value string.  There are two possibilities:
     1. a value is emclosed in double quotes; or 2. the value is blank-
     delimited.
  */

  j=0;
  if (line[i] == '"')
    { ++i;
      while (i<length  &&  line[i] != '"')
        { value[j++] = line[i];
          i++;
        }
    }
  else
    { while (!isspace(line[i]))
        { value[j++] = line[i]; 
          i++;
        }
    }

  /* Blank space is left intact here, so we delete any line delimiters and
     add the string terminator:
  */

  while (value[j-1] == '\r'  ||  value[j-1] == '\n')
    { j--; }
  value[j]='\0';


  /* Done */

  return status;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

char *copy_of (char *value)

  /* Routine to return a pointer to a new copy of the input string           */

{ char *p;    /* New space */

  p = (char *)malloc(strlen(value)+1);
  strcpy(p,value);

  return p;
}



/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/*---------------------------------------------------------------------------*/


void printerror(int type, int field_error, int error_count[], int record,
                COLUMN_INFO *col, FILE *report)

/* Routine to track and display error messages.

   Parameters:
     type          error type    
     field_error   TRUE if the error applies only to the field
     record        record number 
     column        column structure 
     report        output file   

   
*/

{ /* Increment the error count: */

  error_count[type]++;

  /* If this does not exceed the maximum count, then display the message: */

  if (error_count[type] <= MAXERRORS)
    { if (field_error)
          tverr(type,record,col->index,col->name);
        else
          tverr(type,record);
    }

  if (error_count[type] == MAXERRORS)
    { tverr(MAX_ERRORS,record,type,error_count[type]); }

  return;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
 
int count_column_types(COLUMN_INFO *col, int *numeric, int *character,
                       int *date, int *time, int *spare, int *unknown)

  /* Checks each field and subfield and returns total count of defined fields
     of each type for the field list beginning at "fld".  Returns total number
     of fields (and subfields).
  */

{
    int        typecount[8];    /* This is a shortcut. There are currently 8
                                 types defined (in 'pdstv.h') by numbers
                                 which are convenient to use as subscripts.
                              */
	int        i;
	int        total;          /* Return value */

	for (i=0; i<7; i++) typecount[i] = 0; 
	total = 0;

	while (col)
    {
        typecount[col->type]++;
        col = col->next;
    }

	/* Now add together the numeric field types and generate the overall total: */

	*numeric   = typecount[UNSIGNED_INTEGER] + typecount[SIGNED_INTEGER] +
	             typecount[REAL];
	*character = typecount[CHARACTER];
	*date      = typecount[DATE];
	*time      = typecount[TIME];
	*spare     = typecount[SPARE];
	*unknown   = typecount[UNRECOGNIZED];
	total      = *numeric + *character + *date + *time + *spare + *unknown;

	return total;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void writenumsum(COLUMN_INFO *col_top, FILE *report)

  /* Routine to write out the summary statistics for numeric fields. */

{ 
  int     useFmax,useFmin;  /* printing format flags */
  double  max,min;          /* holding places */
  int     maxd,mind;        /* number of decimal places in output format */
  double  absmax,absmin;    /* absolute values of max and min */
  int     type;             /* field type */

  COLUMN_INFO *col;

  col = col_top;
  while (col)
  {
	  if (NUMERIC_FIELD(col->type))
      {
		  /* This mess is just to accommodate some pretty printing: */

          type = col->type;
          max  = col->maxfound.dbl;
          min  = col->minfound.dbl;
          absmax = (max > 0)? max : -max;
          absmin = (min > 0)? min : -min;
          useFmax = (1.e-4 < absmax  &&  absmax < 1.e9) || (absmax == 0.0);
          useFmin = (1.e-4 < absmin  &&  absmin < 1.e9) || (absmin == 0.0);
          if (useFmax) maxd = findprec(max);
          if (useFmin) mind = findprec(min);

          /* Max/min is collected across all items, so we have only a single
             value to report. We check to make sure than somewhere along the
             line we encountered at least one valid value:
          */

          fprintf (report," %4d  %-15.15s ", col->index,col->name);
          fprintf (report," %4d ",col->items);

          if (col->maxfound.dbl == -MAXDOUBLE)
          {
			  fprintf (report,"     -      ");  /* Null minimum */ 
              fprintf (report,"     -      ");  /* Null maximum */
          }
          else
          {
			  if (type == REAL  &&  useFmin)
                { fprintf (report,"%11.*f ",mind,min); }
              else if (type == REAL)
                { fprintf (report,"%11.3e ",min); }
              else if (type == SIGNED_INTEGER  ||  type == UNSIGNED_INTEGER)
                { fprintf (report,"%11d ",(int)min); }
              else
                { fprintf (report,"    n/a    "); }

              if (type == REAL  &&  useFmax)
                { fprintf (report,"%11.*f ",maxd,max); }
              else if (type == REAL)
                { fprintf (report,"%11.3e ",max); }
              else if (type == SIGNED_INTEGER  ||  type == UNSIGNED_INTEGER)
                { fprintf (report,"%11d ",(int)max); }
              else
                { fprintf (report,"    n/a    "); }
          }

          if (col->invflag)
              fprintf (report,"%5d    ",col->invalidcount);
          else
              fprintf (report,"    -    ");

          if (col->missflag)
              fprintf (report,"%5d    ",col->missingcount);
          else
              fprintf (report,"    -    ");

          fprintf (report,"%5d\n", col->badcount);

        } /* else (numeric column) */

      /* Non-numeric and illegal objects are ignored: */

      col = col->next;
    }

  /* Done. */

  return;
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void writecharsum(COLUMN_INFO *col_top, FILE *report)

  /* Routine to write out the summary statistics for non-numeric fields. */

{ 
  COLUMN_INFO  *col;
  int        type;     /* field type */

  /* Loop through the fields in the list, looking for non-numeric types: */

  col = col_top;
  while (col)
  {
      if ( col->type != SPARE && !NUMERIC_FIELD(col->type))
      {
		  type = col->type;

          fprintf (report,"%-6d  %-15.15s ", col->index,col->name);
          fprintf (report," %4d ",col->items);

          fprintf (report,"  %5d      %5d    ",
                          col->minfound.bytes,col->maxfound.bytes); 

          if (col->invflag)
              fprintf (report,"%5d    ",col->invalidcount);
          else
              fprintf (report,"    -    ");

          if (col->missflag)
              fprintf (report,"%5d    ",col->missingcount);
          else
              fprintf (report,"    -    ");

          fprintf (report,"%5d\n", col->badcount);

          /* Maximum and minimum values are printed on separate lines, 
             unless no valid values were encountered:
          */

          if (col->maxstr)
          {
			  fprintf (report,"       Minimum: '%s'\n",col->minstr);
              fprintf (report,"       Maximum: '%s'\n",col->maxstr);
          }
 
      } /* else (non-numeric column) */

      /* Everything else is ignored: */

      col = col->next;
  }

  /* Done. */

  return;
}


/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

int findprec(double value)

/* Routine to return the number of places of precision after the decimal
   for use in an F output format.
*/

{ double val;    /* holding place for absolute value */

  val = (value > 0.0)? value : -value;

  if      (val >= 10000000.)
      return 0; 
  else if (val >= 1000000.0)
      return 1;
  else if (val >= 100000.00)
      return 2;
  else if (val >= 10000.000)
      return 3;
  else if (val >= 1000.0000)
      return 4;
  else if (val >= 100.00000)
      return 5;
  else if (val >= 10.000000)
      return 6;
  else if (val >= 1.0000000)
      return 7;
  else
      return 8;
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void checkcharfld(char   *val,  int bytes,         int record_count,
                  COLUMN_INFO *col,  int error_count[], FILE *report)

  /* Routine to read and check a character field against the label attributes.

     Parameters:
        val           value buffer
        bytes         length of value
        record_count  data record count
        col           column structure
        error_count   error count accumulator array
        report        output file
  */

{ 
  int      blank;        /* 1 if the field is blank */
  int      i,not_done;   /* loop/subscript */
  int      FIELD_ERROR = TRUE;
  int      bad_value;    /* error tracking flag */
  char    *trimval;      /* Blank-trimmed copy of the input value */

  /* If this is a string field we'll check for blanks, INVALID_CONSTANTs,
     MISSING_CONSTANTS, and the extrema.  Note that blank fields are treated
     as regular string fields unless there is a specific request from the 
     user (by command line option) to flag blank fields.
  */

  if (NUMERIC_FIELD(col->type)) return;  /* A number */


  blank = (strspn(val," ")==bytes);   /* check for blank field         */
  if ((pdstv->flag_blanks)  &&  blank)    /* flag blank field if requested */
    { printerror(BLANK_FIELD,FIELD_ERROR,error_count,record_count,col,
                 report);
      return;
    }

  /* From now on we'll work with a blank-trimmed copy of the input string: */

  trimval = trim_copyof(val);

  /* Check for INVALID_CONSTANT and MISSING_CONSTANT values. If found, we
     return, as there's nothing else to check:
  */

  if (col->invflag  &&  (strcmp(col->invalid.str,trimval) == 0))
    { col->invalidcount++;
      Lemme_Go(trimval);
      return;
    }

  if (col->missflag  &&  (strcmp(col->missing.str,trimval) == 0))
    { col->missingcount++;
      Lemme_Go(trimval);
      return;
    }

  /* If there were bounds in the label, make sure we haven't exceeded them: */

  bad_value = FALSE;

  if (col->mmflag%2)   /* Max present */
    { if (strcmp(col->max.str,trimval)<0)
        { printerror(GREATER_THAN_MAX,FIELD_ERROR,error_count,
                     record_count,col,report);
          bad_value = TRUE;
        }
    }
 
  if (col->mmflag > 1) /* Min present */
    { if (strcmp(trimval,col->min.str)<0)
        { printerror(LESS_THAN_MIN,FIELD_ERROR,error_count,
                     record_count,col,report);
          bad_value = TRUE;
        }
    }

  if (col->vmflag%2)
    { if (strcmp(col->vmax.str,trimval)<0)
        { printerror(GREATER_THAN_VMAX,FIELD_ERROR,error_count,
                     record_count,col,report);
          bad_value = TRUE;
        }
    }

  if (col->vmflag>1)
    { if (strcmp(col->vmax.str,trimval)<0)
        { printerror(LESS_THAN_MIN,FIELD_ERROR,error_count,
                     record_count,col,report);
          bad_value = TRUE;
        }
    }

  /* Done with the trimmed copy: */

  Lemme_Go(trimval);

  /* And finally, loop through the characters in the field to make sure they
     include only printing characters:
  */

  i = 0;
  not_done = TRUE;
  /* 03-31-06 MDC - Whenenver we've encountered a non-ascii character that is
      of negative value, table checker would crash */
  while (i<(col->size)  && not_done)
  {/*   if (!isprint(val[i]))    */
	    if ( val[i] >= 0 && (!isprint(val[i])) )
        { printerror(NONPRINT_CHAR,FIELD_ERROR,error_count,
                     record_count,col,report);
          bad_value = TRUE;
          not_done  = FALSE;
		}
      i++;
    }
          
  if (bad_value) col->badcount++;


  /* Now set extrema and we're done: */

  set_string_maxmin(val,col);
  
  return;
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void checkdatefld(char   *val, int bytes,         int   record_count,
                  COLUMN_INFO *col, int error_count[], FILE *report)

  /* Routine to read and check a DATE field.

     Parameters
        val             string vlaue to be checked
        bytes           length of value
        record_count    data record count
        col             column structure
        error_count     error count accumulator array
        report          output file
  */

{ int      blank;        /* 1 if the field is blank */
  int      bad_value;    /* flag indicating a format problem was found */
  int      FIELD_ERROR = TRUE;

  /* Make sure this is a date field:  */

  if (col->type != DATE) return;          /* Not a DATE */

  /* Check for a blank field.  This is only an error if we've not been 
     directed to ignore blank fields:
  */

  blank = (strspn(val," ")==bytes);  /* check for blank numeric field */
  if ((pdstv->flag_blanks)  &&  blank)    /* flag blank field if requested */
    { printerror(BLANK_FIELD,FIELD_ERROR,error_count,record_count,col,report);
      return;
    }
  if (blank) return;                   /* blank fields are otherwise ignored */

  /* Check to see if we've got a MISSING or INVALID flag value rather than
     a date. If so, we return immediately as no further checking is required:
  */

  if (col->invflag  &&  trimcmp(val,col->invalid.str)==0)
    { col->invalidcount++;
      return;
    }

  if (col->missflag  &&  trimcmp(val,col->missing.str)==0)
    { col->missingcount++;
      return;
    }


  /* If the field is not the size of a valid DATE format, we don't try 
     verifying it, but we will signal the error, set extrema and return:
  */

  if (col->fwidth < MINDATEWIDTH  ||  col->fwidth > MAXDATEWIDTH)
    { printerror(INVALID_DATE,FIELD_ERROR,error_count,record_count,
                 col,report);
      col->badcount++;
      set_string_maxmin(val,col);
      return;
    }

  /* DATE fields MUST contain only date information in the format YYYY-MM-DD
     or YYYY-DDD (i.e., the PDS standard DATE formats).  Anything else should
     be described either as individual integers or a string.  All digits must
     be present (zero-padded if necessary) and the century may not be negative.
     We accomplish this checking through straight brute-force testing of each 
     character:
  */

  bad_value = 0;
  bad_value += (!isdigit(val[0]));
  bad_value += (!isdigit(val[1]));
  bad_value += (!isdigit(val[2]));
  bad_value += (!isdigit(val[3]));
  bad_value += (val[4] != '-');
  bad_value += (!isdigit(val[5]));
  bad_value += (!isdigit(val[6]));
  if (col->fwidth == MINDATEWIDTH)
    { bad_value += (!isdigit(val[7])); }
  else
    { bad_value += (val[7] != '-');
      bad_value += (!isdigit(val[8]));
      bad_value += (!isdigit(val[9]));
    }

  if (bad_value)
    { printerror(INVALID_DATE,FIELD_ERROR,error_count,record_count,
                 col,report); 
    }

  /* If there were bounds in the label, make sure we haven't exceeded them: */

  if (col->mmflag%2  &&  !bad_value)   /* MAXIMUM present */
    { if (strcmp(col->max.str,val)<0)
        { printerror(GREATER_THAN_MAX,FIELD_ERROR,error_count,record_count,
                     col,report);
          bad_value = TRUE;
        }
    }
 
  if (col->mmflag > 1  &&  !bad_value) /* MINIMUM present */
    { if (strcmp(val,col->min.str)<0)
        { printerror(LESS_THAN_MIN,FIELD_ERROR,error_count,record_count,
                     col,report);
          bad_value = TRUE;
        }
    }

  if (col->vmflag%2  &&  !bad_value)   /* VALID_MAXIMUM */
    { if (strcmp(col->vmax.str,val)<0)
        { printerror(GREATER_THAN_VMAX,FIELD_ERROR,error_count,record_count,
                     col,report);
          bad_value = TRUE;
        }
    }
 
  if (col->vmflag > 1  &&  !bad_value) /* VALID_MINIMUM */
    { if (strcmp(val,col->vmin.str)<0)
        { printerror(LESS_THAN_VMIN,FIELD_ERROR,error_count,record_count,
                     col,report);
          bad_value = TRUE;
        }
    }

  /* Update the bad value counter as needed: */

  if (bad_value) col->badcount++;


  /* Check the extrema and we're done: */

  set_string_maxmin(val,col);

  return;
}


/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void checktimefld(char   *val, int bytes,         int   record_count,
                  COLUMN_INFO *col, int error_count[], FILE *report)

  /* Routine to read and check a TIME field.

     Parameters
        val             string vlaue to be checked
        bytes           length of value
        record_count    data record count
        col             column structure
        error_count     error_count accumulator array
        report          output file

     Note that TIME fields may be truncated on the right as far as 
     needed.  The minimum size is a bare year.
  */

{ int      blank;        /* 1 if the field is blank */
  int      bad_value;    /* flag indicating a format problem was found */
  int      FIELD_ERROR = TRUE;
  int      doy;          /* TRUE if date format is YYYY-DDD */
  int      ts;           /* Subscript of start of time (HH:MM:SS) field */
  char     tmp[20];      /* Holding place for time part. */
  int      len;          /* string length */
  int      i;

  /* Make sure this is a time field:  */

  if (col->type != TIME) return;          /* Not a TIME */

  /* Check for a blank field.  This is only an error if we've not been 
     directed to ignore blank fields:
  */

  blank = (strspn(val," ")==bytes);   /* check for blank numeric field */
  if ((pdstv->flag_blanks)  &&  blank)    /* flag blank field if requested */
    { printerror(BLANK_FIELD,FIELD_ERROR,error_count,record_count,col,report);
      return;
    }
  if (blank) return;                   /* blank fields are otherwise ignored */

  /* Check to see if we've got a MISSING or INVALID flag value rather than
     a date. If so, we return immediately as no further checking is required:
  */

  if (col->invflag  &&  trimcmp(val,col->invalid.str)==0)
    { col->invalidcount++;
      return;
    }

  if (col->missflag  &&  trimcmp(val,col->missing.str)==0)
    { col->missingcount++;
      return;
    }


  /* If the field size does not fall in the valid range for TIME formats, we
     won't try verifying it, we just check the extrema and exit:
  */

  if (col->fwidth < MINTIMEWIDTH  ||  col->fwidth > MAXTIMEWIDTH)
    { printerror(INVALID_TIME,FIELD_ERROR,error_count,record_count,
                 col,report);
      col->badcount++;
      set_string_maxmin(val,col);
      return;
    }

  /* TIME fields contain both date and time information.  Date information
     must be in the same format as for DATE-only fields (i.e., YYYY-MM-DD or
     YYYY-DDD), so we'll check that first using the old brute-force method:
  */

  bad_value = 0;
  bad_value += (!isdigit(val[0]));
  bad_value += (!isdigit(val[1]));
  bad_value += (!isdigit(val[2]));
  bad_value += (!isdigit(val[3]));

  if (col->fwidth >= 8)
    { bad_value += (val[4] != '-');
      bad_value += (!isdigit(val[5]));
      bad_value += (!isdigit(val[6]));
    
      /* The next value may be a digit or a '-', depending on format: */

      if (isdigit(val[7]))
        { doy = TRUE;
          ts = 8;
        }
      else
        { doy = FALSE;
          bad_value += (val[7] != '-');
          bad_value += (!isdigit(val[8]));
          bad_value += (!isdigit(val[9]));
          ts = 10;
        }
    }

  /* If we have a non-null time part we'll copy it into a separate string 
     to make the checking a little easier. If the string ends with a 'Z' 
     (always valid) we chop it off:
  */

  if (ts != strlen(val))
    { strcpy(tmp,val+ts);
      len = strlen(tmp);
      if (tmp[len-1] == 'Z')
        { tmp[len-1] = '\0';
          len--;
        }

      /* Now we'll check in segments, being careful not to look past
         then end of the string:
      */

      bad_value = (len < 3);
      if (len >= 3)
        { bad_value = (tmp[0] != 'T');
          bad_value = (!isdigit(tmp[1]));
          bad_value = (!isdigit(tmp[2]));
        }

      bad_value = (len > 3  &&  len < 6);
      if (len >= 6)
        { bad_value = (tmp[3] != ':');
          bad_value = (!isdigit(tmp[4]));
          bad_value = (!isdigit(tmp[5]));
        }

      bad_value = (len > 6  &&  len < 9);
      if (len >= 9)
        { bad_value = (tmp[6] != ':');
          bad_value = (!isdigit(tmp[7]));
          bad_value = (!isdigit(tmp[8]));
        }

      bad_value = (len > 9  &&  tmp[9] != '.');
      for (i=10; i<len; i++)
        { bad_value = (!isdigit(tmp[i])); }
    }


  /* Now check to see if any of the above tests failed, and signal as needed.
     Then check extrema:
  */

  if (bad_value)
    { printerror(INVALID_TIME,FIELD_ERROR,error_count,record_count,
                 col,report); }

  /* If there were bounds in the label, make sure we haven't exceeded them: */

  if (col->mmflag%2)   /* MAXIMUM */
    { if (strcmp(col->max.str,val)<0)
        { printerror(GREATER_THAN_MAX,FIELD_ERROR,error_count,record_count,
                     col,report);
          bad_value = TRUE;
        }
    }
 
  if (col->mmflag > 1) /* MINIMUM */
    { if (strcmp(val,col->min.str)<0)
        { printerror(LESS_THAN_MIN,FIELD_ERROR,error_count,record_count,
                     col,report);
          bad_value = TRUE;
        }
    }

  if (col->vmflag%2)   /* VALID_MAXIMUM */
    { if (strcmp(col->vmax.str,val)<0)
        { printerror(GREATER_THAN_VMAX,FIELD_ERROR,error_count,record_count,
                     col,report);
          bad_value = TRUE;
        }
    }
 
  if (col->vmflag > 1) /* VALID_MINIMUM */
    { if (strcmp(val,col->vmin.str)<0)
        { printerror(LESS_THAN_VMIN,FIELD_ERROR,error_count,record_count,
                     col,report);
          bad_value = TRUE;
        }
    }

  /* Update the bad value counter as needed: */

  if (bad_value) col->badcount++;


  /* Check the extrema and we're done: */

  set_string_maxmin(val,col);

  return;
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void set_string_maxmin(char *value, COLUMN_INFO *col)

  /* Routine to check for and set extrema of a string field. 

     Parameters
        value       string to be compared
        col         column parameters
  */

{ int len;      /* string length */

  /* First, check for Maximum and minimum string values: */

  if (col->maxstr == NULL)
    { col->maxstr = (char *)malloc(strlen(value)+1);
      strcpy(col->maxstr,value);
    }
  else if (strcmp(col->maxstr,value)<0)
    { free(col->maxstr);
      col->maxstr = (char *)malloc(strlen(value)+1);
      strcpy(col->maxstr,value);
    }

  if (col->minstr == NULL)
    { col->minstr = (char *)malloc(strlen(value)+1);
      strcpy(col->minstr,value);
    }
  else if (strcmp(value,col->minstr)<0)
    { free(col->minstr);
      col->minstr = (char *)malloc(strlen(value)+1);
      strcpy(col->minstr,value);
    }

  /* Next, check for maximum and minimum string lengths: */

  len = strlen(value);
  while (len>0 && value[len-1]==' ')
    { len--; }

  col->maxfound.bytes = (col->maxfound.bytes < len)? len : col->maxfound.bytes;
  col->minfound.bytes = (col->minfound.bytes > len)? len : col->minfound.bytes;


  /* And we're done: */

  return;
}


/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void print_table_attributes (TABLE_INFO *tab, FILE *report)

  /* Print the summary attributed for the given table and its fields. Returns
     the total number of numeric fields encountered.
  */

{
	COLUMN_INFO   *col;
	CONTAINER_INFO *cnt;
	
	/* Start by writing the general TABLE attributes: */

	fprintf (report," Table attributes\n");
	fprintf (report," ----------------\n");
	fprintf (report," Table object %d: %s\n",tab->tblnum,tab->tb_class);
	fprintf (report,"       ROWS: %6d\n",tab->row_count);
	fprintf (report,"    COLUMNS: %6d\n",tab->column_count);
	fprintf (report,"  ROW_BYTES: %6d\n",tab->row_bytes);

	/* Now the column attributes. A recursive routine is called to deal with
	   sub-columns of CONTAINERS.
	*/

	fprintf (report,
      "\n  Col    Name                            Start   Bytes   Items ");
	fprintf (report,"  Type\n");

	col = tab->columns;

	for(col = tab->columns; col != NULL; col = col->next)
	{
		if (col->which_container != 0)
		{
			/* Skip all the repeated container columns in the tree */
			if(col->rep_num == 1)
			{
                cnt = find_container(tab, col->which_container);
				col = print_container_attributes(cnt, col, report, tab);
				if(col == NULL) return;
			}
		}
		else
        {
			fprintf (report," %4d    %-30.30s  %5d   %4d    %4d    %-10s\n",
                         col->index,col->name,col->start,
                         col->size,col->items,field_type[col->type]);
        }
    }

	/* Done: */

	return;
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

COLUMN_INFO *print_container_attributes(CONTAINER_INFO *cnt, COLUMN_INFO *col, 
								     FILE *report, TABLE_INFO *tab)

  /* Routine to print each field within a container, calling itself if 
     needed.  Returns the number of numeric fields encountered. */

{
    CONTAINER_INFO *container;
	long this_container = 0;

	fprintf (report, " %4d    %-30.30s  %5d   %4d   x%4d    %-10s\n",
                   cnt->columns, cnt->name, cnt->start_byte,
                   cnt->bytes, cnt->repetitions,"CONTAINER");

	this_container = cnt->container_id;

	for(; (col && (col->rep_num == 1) && (col->which_container == cnt->container_id));
		col = col->next)
	{
        fprintf (report," %6d  %-30.30s  %5d   %4d    %4d    %-10s\n",
                       col->index,col->name,col->start,
                       col->size,col->items,field_type[col->type]);
      
		/* If we encounter another CONTAINER, call this routine recursively */
		if (col->which_container != this_container)
		{
            container = find_container(tab, col->which_container);

			if(container != NULL)
                print_container_attributes(container, col, report, tab);
			else
			{
				fprintf (report, "CONTAINER attributes could not be found\n");
			}
		}
	}

	/* Now advance through the repeated CONTAINER COLUMN objects */
	while( col && (col->which_container == this_container))
	{
		col = col->next;
	}

	/* Done */

	return col;
}

CONTAINER_INFO *find_container(TABLE_INFO *table, long this_con)
{
	CONTAINER_INFO *con;

	con = table->container;

	for(con = table->container; con != NULL; con = con->next_container)
	{
		if(con->container_id == this_con)
			return con;
	}

	return NULL;
}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

char *trim_copyof(char *s)

  /* Routine to return a copy of the input string 's' from which the leading
     and trailing blanks have been trimmed.
  */

{ char *copy;          /* Return pointer */
  int   first,last;    /* end points     */
  int   length;
  int   i,j;

  /* First, intercept blank and empty strings: */

  length = strlen(s);
  if (length < 1  ||  length == strspn(s," "))
    { copy = (char *)malloc(2);
      copy[0] = '\0';
      return copy;
    }

  /* For non-blank strings, find the end points in the original string: */

  first = 0;
  while (s[first] == ' ') first++;
  last = strlen(s) - 1;
  while (s[last] == ' ') last--;
  length = last - first + 1;

  /* Allocate space and copy the non-blank part: */

  copy = (char *)malloc(length+1);
  j = 0;
  for (i=first; i<=last; i++) copy[i-first] = s[i];
  copy[length] = '\0';

  /* Done: */

  return copy;
}


/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

int trimcmp(char *s1, char *s2)

  /* Routine to compare blank-trimmed versions of the input strings,
     returning the 'strcmp' result.
  */

{ char *c1, *c2;    /* Copies of the input strings */
  int   result;     /* strcmp result */

  c1 = trim_copyof(s1);
  c2 = trim_copyof(s2);

  result = strcmp(c1,c2);

  Lemme_Go(c1);
  Lemme_Go(c2);

  return result;

}

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

int spare_column(COLUMN_INFO *col)

  /* Routine to check if a column is designated as SPARE.  Returns TRUE or
     FALSE accordingly.
  */

{ int    i;    /* string subscript */

  /* Spare columns are identified by the NAME value. If the NAME is "SPARE"
     or ends with "_SPARE" (or " SPARE"), we return a TRUE result.
  */

  if (strcmp(col->name,"SPARE") == 0)
    { return TRUE; }

  /* To check for "_SPARE", we'll use the brute-force method: */

  i = strlen(col->name);

  if ((col->name[i-6] == '_'  ||  col->name[i-6] == ' ')  &&
       col->name[i-5] == 'S'  &&
       col->name[i-4] == 'P'  &&
       col->name[i-3] == 'A'  &&
       col->name[i-2] == 'R'  &&
       col->name[i-1] == 'E')
    { return TRUE; }

  /* If we're here, this is not a SPARE column: */

  return FALSE;
}




/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

