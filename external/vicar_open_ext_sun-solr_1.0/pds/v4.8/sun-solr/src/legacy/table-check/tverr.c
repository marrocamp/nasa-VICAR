/*
  TVERR.C

  This file contains the error message handling routine.

  03 January 2002, ACR.

*/

#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include "tverr.h"

/* Global Variables: */

extern FILE  *report;        /* output destination */
extern int    batch_mode;    /* batch processing flag */
extern int    error_count;   /* total error_count */


/*===========================================================================*/

int tverr(int ecode,...)

  /* This routine provides detailed error signalling.  It accepts an error
     code (these are defined in the 'error.h' file) and a variable-length
     argument string with additional information.

     Program errors and warnings are all written to the report device.
  */

{ va_list   ap;     /* argument pointer */
  int       i1,i2,i3;
  long int  l1,l2;
  double    d1,d2;
  char     *s1,*s2,*s3;

  long int  linecount;
  char      flag;


  /* Set up the argument list: */

  va_start(ap, ecode);
  linecount = va_arg(ap,long int);

  /* If we're here, there's been an error. Increment the global error count: */

  if (ecode != USAGE)
    { error_count++; }


  switch (ecode)
    { case USAGE:

		fprintf(stderr, "\n\n\n");
		fprintf(stderr, "Table Checker Version %s\n", VERSION_ID);
		fprintf(stderr, "Disclaimer:\n");
		fprintf(stderr, "Copyright 2006-2007, by the California Institute of Technology.\n");
		fprintf(stderr, "ALL RIGHTS RESERVED. United States Government Sponsorship acknowledged.\n");
		fprintf(stderr, "Any commercial use must be negotiated with the Office of Technology Transfer\n");
		fprintf(stderr, "at the California Institute of Technology.\n\n");
		fprintf(stderr, "This software is subject to U. S. export control laws and regulations\n");
		fprintf(stderr, "(22 C.F.R. 120-130 and 15 C.F.R. 730-774). To the extent that the software\n");
		fprintf(stderr, "is subject to U.S. export control laws and regulations, the recipient has\n");
		fprintf(stderr, "the responsibility to obtain export licenses or other export authority as\n");
		fprintf(stderr, "may be required before exporting such information to foreign countries or\n");
		fprintf(stderr, "providing access to foreign nationals.\n\n");
	    fprintf(stderr, "Usage:\n\n");
        fprintf(stderr, "table_check [-l <label>] [-o <report>] [-f] [-b] [-t]\n\n");
        fprintf(stderr, "Where:\n");
        fprintf(stderr, "   -b:          Enables batch mode processing\n");
	    fprintf(stderr, "   -f:          Indicates blank fields should be flagged\n");
	    fprintf(stderr, "   -l <label>:  Specify the full path to the label file to\n");
		fprintf(stderr, "                be processed\n");
	    fprintf(stderr, "   -o <report>: Directs the report to the named file.\n");
		fprintf(stderr, "                Default is to output screen\n");
	    fprintf(stderr, "   -t:          Include processing time in report\n");
		fprintf(stderr, "\n\n\n");
        
        break;

      /********************
       *** Label Errors ***
       ********************/

      case BAD_COLNUM:
        i1 = va_arg(ap,int);
        i2 = va_arg(ap,int);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %d - ",linecount,BAD_COLNUM,i1);
        else
            fprintf (report,"  Column %d: ",i1);
        fprintf (report,"COLUMN_NUMBER (%d) out of sequence.\n",i2);
        break;

      case BAD_DER_MAXMIN:
        s1 = va_arg(ap, char *);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,BAD_DER_MAXMIN,s1);
        else
            fprintf (report,"  Column %s: ",s1);
        fprintf (report,"DERIVED_MAXIMUM/DERIVE_MINIMUM ");
        fprintf (report,"in non-numeric field.\n");
        break;

      case BAD_END_OBJECT:
        if (batch_mode)
            fprintf (report,"L:%d:%d:",linecount,BAD_END_OBJECT);
        else
            fprintf (report," line %3d: ",linecount);
        fprintf(report,"Misplaced END_OBJECT statement.\n");
        if (!batch_mode) fprintf (report,"\nPROCESSING STOPPED.\n");
        break;

      case BAD_REPCOUNT:
        s1 = va_arg(ap,char *);
        s2 = va_arg(ap,char *);
        i1 = va_arg(ap,int);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,BAD_REPCOUNT,s1);
        else
            fprintf (report,"Column %s: ",s1);
        fprintf (report,"Repetition count in FORMAT (\"%s\") ",s2);
        fprintf (report,"does not agree with ITEMS (%d).\n",i1);
        break;

      case BAD_SPARE_TYPE:
        s1 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,BAD_SPARE_TYPE,
                        s1);
        else
            fprintf (report,"  Column %s: ",s1);
        fprintf (report,"SPARE columns must have a DATA_TYPE of CHARACTER.\n");
        break;

      case COLUMN_MISCOUNT:
        i1 = va_arg(ap,int);
        i2 = va_arg(ap,int);
        if (batch_mode)
            fprintf (report,"L:%d:%d:",linecount,COLUMN_MISCOUNT);
        fprintf (report,"COLUMNS + %d, but %d COLUMN objects were found.\n",
                        i1,i2);
        break;

      case CONTAINER_TOO_LONG:
        s1 = va_arg(ap,char *);
        i1 = va_arg(ap,int);
        if (batch_mode)
            fprintf (report,"L:%d:%d:",linecount,CONTAINER_TOO_LONG);
        else
            fprintf (report,"  line %3d: ",linecount);
        fprintf (report,"CONTAINER '%s' (line %d) extends past end of row.\n",
                        s1,i1);
        break;

      case DATA_FILE_NOT_FOUND:
        s1 = va_arg(ap,char *);
        s2 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"T:%d:%d:",linecount,DATA_FILE_NOT_FOUND);
        else
          { fprintf (report,"Unable to open data file '%s' ('%s').\n",s1,s2);
            fprintf (report,"PROCESSING STOPPED.\n");
          }
        fprintf (stderr,"Unable to open data file '%s' ('%s').\n",s1,s2);
        fprintf (stderr,"PROCESSING STOPPED.\n");
        break;

      case DBLMAX_LT_MIN:
        s1 = va_arg(ap,char *);
        d1 = va_arg(ap,double);
        d2 = va_arg(ap,double);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,DBLMAX_LT_MIN,s1);
        else
            fprintf (report,"  Column %s: ",s1);
        fprintf (report,"Maxmimum (%f) less than minimum (%f).\n",d1,d2);
        break;

      case DBLVALMAX_LT_VALMIN:
        s1 = va_arg(ap,char *);
        d1 = va_arg(ap,double);
        d2 = va_arg(ap,double);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,
                        DBLVALMAX_LT_VALMIN,s1);
        else
            fprintf (report,"  Column %s: ",s1);
        fprintf (report,"Valid maxmimum (%f) less than valid minimum (%f).\n",
                        d1,d2);
        break;

      case DERMAX_LT_DERMIN:
        s1 = va_arg(ap,char *);
        d1 = va_arg(ap,double);
        d2 = va_arg(ap,double);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,DERMAX_LT_DERMIN,
                        s1);
        else
            fprintf (report,"  Column %s: ",s1);
        fprintf (report,"Derived maxmimum (%f) less than ",d1);
        fprintf (report,"derived minimum (%f).\n",d2);
        break;

      case EMPTY_CONTAINER:
        s1 = va_arg(ap,char *);
        i1 = va_arg(ap,int);
        if (batch_mode)
            fprintf (report,"L:%d:%d:",linecount,EMPTY_CONTAINER);
        else
            fprintf (report,"  line %3d: ",linecount);
        fprintf (report,"CONTAINER '%s' (line %d) is empty.\n",s1,i1);
        break;

      case FIELD_TOO_LONG:
		  /* 02-03-06 MDC - Made bug fix to parse an integer value instead
		           of string to prevent program crash.
	      */
/*        s1 = va_arg(ap,char *); */
	    i1 = va_arg(ap,int);
        s2 = va_arg(ap,char *);
        if (batch_mode)
		{
/*            fprintf (report,"L:%d:%d:Column %s - ",linecount,FIELD_TOO_LONG,
                        s1);
*/		
			fprintf(report, "L:%d:%d:Column %ld - ",linecount,FIELD_TOO_LONG, i1);
		}
        else
            fprintf (report,"  Column %ld: ",i1);
        fprintf (report,"Field extends past end of %s.\n",s2);
        break;

      case FILE_SEEK_FAILED:
        if (batch_mode)
            fprintf (report,"T:%d:%d:",linecount,FILE_SEEK_FAILED);
        fprintf (report,"Unable to set file pointer to indicated offset ");
        fprintf (report,"in data file.\n");
        fprintf (stderr,"Unable to set file pointer to indicated offset ");
        fprintf (stderr,"in data file.\n");
        if (!batch_mode)
          { fprintf (report,"PROCESSING STOPPED.\n");
            fprintf (stderr,"PROCESSING STOPPED.\n");
          }
        break;

      case FORMAT_TOO_BIG:
        i1 = va_arg(ap,int);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %d - ",linecount,FORMAT_TOO_BIG,i1);
        else
            fprintf (report,"  Column %d: ",i1);
        fprintf (report,"FORMAT field too lon (> 50 characters).\n");
        break;

      case FORMAT_TYPE_MISMATCH:
        s1 = va_arg(ap,char *);
	s2 = va_arg(ap,char *);
        s3 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,
                        FORMAT_TYPE_MISMATCH,s1);
        else
            fprintf (report,"  Column %s: ",s1);
        fprintf (report,"FORMAT \"%s\" incompatible with DATA_TYPE '%s'.\n",
                        s2,s3);
        break;

      case INT_TYPE_CONFLICT:
        i1 = va_arg(ap,int);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %d - ",linecount,INT_TYPE_CONFLICT,
                                i1);
        else
            fprintf (report,"  Column %d: ",i1);
        fprintf (report,"Cannot have both MSB and LSB integers in a ");
        fprintf (report,"single data file.\n");
        break;

      case INV_CHAR_IN_FN:
        s1 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"L:%d:%d:",linecount,INV_CHAR_IN_FN);
        else
            fprintf (report,"  line %3d: ",linecount);
        fprintf (report,"Ivalid character in file name ('%s').\n",s1);
        break;

      case INV_FILE_RECORDS:
        s1 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"L:%d:%d:",linecount,INV_FILE_RECORDS);
        else
            fprintf (report," line %3d: ",linecount);
        fprintf(report,"Invalid FILE_RECORDS value (%s).\n",s1);
        break;

      case INV_FORMAT:
        s1 = va_arg(ap,char *);
        s2 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,INV_FORMAT,s1);
        else
            fprintf (report,"Column %s: ",s1);
	fprintf (report,"Invalid FORMAT (\"%s\")\n",s2);
        break;

      case INV_INT_SIZE:
        i1 = va_arg(ap,int);
        i2 = va_arg(ap,int);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %d - ",linecount,INV_INT_SIZE,i1);
        else
            fprintf (report,"  Column %d: ",i1);
        fprintf (report,"Invalid number of bytes (%d) for an INTEGER ",i2);
        fprintf (report,"field.  Column will be ignored.\n");
        break;

      case INV_REAL_SIZE:
        i1 = va_arg(ap,int);
        i2 = va_arg(ap,int);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %d - ",linecount,INV_REAL_SIZE,i1);
        else
            fprintf (report,"  Column %d: ",i1);
        fprintf (report,"Invalid number of bytes (%d) for a REAL ",i2);
        fprintf (report,"field.  Column will be ignored.\n");
        break;
/*	  
	  case INV_REAL_TYPE:
	    i1 = va_arg(ap,int);
		s1 = va_arg(ap,char *);
		if (batch_mode)
			fprintf (report,"L:%d:%d:Column %d - ",linecount,INV_REAL_TYPE,i1);
		else
			fprintf (report,"  Column %d: ",i1);
		fprintf (report,"Unrecognized REAL type: %s .",s1);
		fprintf (report,"Column will be ignored.\n");
		break;
*/
      case INV_RECORD_BYTES:
        s1 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"L:%d:%d:",linecount,INV_RECORD_BYTES);
        else
            fprintf (report," line %3d: ",linecount);
        fprintf (report,"Invalid RECORD_BYTES value (%s).\n",s1);
        break;

      case INV_RECORD_TYPE:
        if (batch_mode)
            fprintf (report,"L:%d:%d:",linecount,INV_RECORD_TYPE);
        else
            fprintf (report," line %3d: ",linecount);
        fprintf (report,"RECORD_TYPE should be FIXED_LENGTH.\n");
        break;

      case INV_REP:
        i1 = va_arg(ap,int);
        i2 = va_arg(ap,int);
        if (batch_mode)
            fprintf (report,"L:%d:%d:",linecount,INV_REP);
        else
            fprintf (report,"  line %3d: ",linecount);
        fprintf (report,"Invalid REPETITIONS (%d) in CONTAINER (line %d).",
                        i1,i2);
        fprintf (report,"  '1' assumed.\n");
        break;

      case INV_SUBOBJECT:
        s1 = va_arg(ap,char *);
        s2 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"L:%d:%d:",linecount,INV_SUBOBJECT);
        else
            fprintf (report," line %3d: ",linecount);
        fprintf (report,"%s object found in %s definition -- IGNORED.\n",s1,s2);
        break;

      case ITEM_BYTES_MISMATCH:
        s1 = va_arg(ap,char *);
        i1 = va_arg(ap,int);
        i2 = va_arg(ap,int);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,
                        ITEM_BYTES_MISMATCH,s1);
        else
            fprintf (report,"  Column %s: ",s1);
        fprintf (report,"Total width of ITEMS (%d) exceeds BYTES (%d).",i1,i2);
        fprintf (report,"  Column will be ignored.\n");
        break;

      case MAX_ERRORS:
        i1 = va_arg(ap,int);
        i2 = va_arg(ap,int);
        if (batch_mode)
          { fprintf (report,"T:%d:%d:",linecount,MAX_ERRORS);
            fprintf (report,"Error code %d has been reported %d times.",i1,i2);
            fprintf (report,"  No more will be reported.\n");
          }
        else
          { fprintf (report,"   %d of these errors have been flagged. ",i2);
            fprintf (report," No more will be reported.\n");
          }
        break;

      case MAX_TOO_LONG:
        s1 = va_arg(ap,char *);
        s2 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,MAX_TOO_LONG,s1);
        else
            fprintf (report,"  Column %s: ",s1);
        fprintf (report," Maximum value ('%s') overflows the field.\n",s2);
        break;

      case MIN_TOO_LONG:
        s1 = va_arg(ap,char *);
        s2 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,MIN_TOO_LONG,s1);
        else
            fprintf (report,"  Column %s: ",s1);
        fprintf (report," Minimum value ('%s') overflows the field.\n",s2);
        break;

      case MISSING_POINTER:
        s1 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"L:%d:%d:",linecount,MISSING_POINTER);
        else
            fprintf (report," line%3d: ",linecount);
        fprintf (report,"No pointer for \"%s\" object - IGNORED.\n",s1);
        break;

      case MISSING_SCALE:
        s1 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount, MISSING_SCALE,s1);
        else
            fprintf (report,"  Column %s: ",s1);
        fprintf (report,"DERIVED_MAXIMUM/MINIMUM was specified ");
        fprintf (report,"without an OFFSET or SCALING_FACTOR.\n");
        break;

      case MIXED_CASE_IN_FN:
        s1 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"L:%d:%d:",linecount,MIXED_CASE_IN_FN);
        else
            fprintf (report,"  line %3d: ",linecount);
        fprintf (report,"Mixed case not allowed in file name ('%s').\n",s1);
        break;

      case MULT_EXT_IN_FN:
        s1 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"L:%d:%d:",linecount,MULT_EXT_IN_FN);
        else
            fprintf (report,"  line %3d: ", linecount);
        fprintf (report,"File name ('%s') has multiple extensions.\n",s1);
        break;

      case NO_EXT_IN_FN:
        s1 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"L:%d:%d:",linecount,NO_EXT_IN_FN);
        else
            fprintf (report,"  line %3d: ",linecount);
        fprintf (report,"File name ('%s') has no extensions.\n",s1);
        break;

      case NO_FILE_RECORDS:
        if (batch_mode)
            fprintf (report,"L:%d:%d:",linecount,NO_FILE_RECORDS);
        fprintf (report,"FILE_RECORDS not found.\n");
        break;

      case NO_RECORD_BYTES:
        if (batch_mode)
            fprintf (report,"L:%d:%d:",linecount,NO_RECORD_BYTES);
        fprintf (report,"RECORD_BYTES not found.\n");
        break;

      case NO_RECORD_TYPE:
        if (batch_mode)
            fprintf (report,"L:%d:%d:",linecount,NO_RECORD_TYPE);
        fprintf (report,"RECORD_TYPE not found.\n");
        break;

      case NO_REPCOUNT:
        s1 = va_arg(ap,char *);
        s2 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,NO_REPCOUNT,s1);
        else
            fprintf (report,"Column %s: ",s1);
        fprintf (report,"FORMAT (\"%s\") does not include ",s2);
        fprintf (report,"repetition count for COLUMN with ITEMS\n");
        break;

      case NO_TABLES:
        if (batch_mode)
            fprintf (report,"L:%d:%d:",linecount,NO_TABLES);
        fprintf (report,"No TABLE-type objects found.\n");
        if (!batch_mode)
            fprintf (report,"\nPROCESSING STOPPED.\n");
        break;

      case OBJLBL_MISMATCH:
        s1 = va_arg(ap,char *);
        s2 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"L:%d:%d:",linecount,OBJLBL_MISMATCH);
        else
            fprintf (report," line %3d: ",linecount);
        fprintf (report,"END_OBJECT label (%s) does not match ",s1);
        fprintf (report,"OBJECT label (%s).\n",s2);
        break;

      case OBSOLETE_KEYWORD:
        i1 = va_arg(ap,int);
        s1 = va_arg(ap,char *);
        s2 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %d - ",linecount,OBSOLETE_KEYWORD,
                             i1);
        else
            fprintf (report,"  Column %d: ",i1);
        fprintf (report,"Keyword %s should be %s.\n",s1,s2);
        break;

      case OBSOLETE_TYPE:
        i1 = va_arg(ap,int);
        s1 = va_arg(ap,char *);
        s2 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %d - ",linecount,OBSOLETE_TYPE,
                             i1);
        else
            fprintf (report,"  Column %d: ",i1);
        fprintf (report,"DATA_TYPE \"%s\" should be %s.\n",s1,s2);
        break;

      case OOPS_NUMBER_SIZE:        /* THIS SHOULD NEVER HAPPEN */
        s1 = va_arg(ap,char *);
		i1 = va_arg(ap, int);
		i2 = va_arg(ap, int);
 /*       s2 = va_arg(ap,char *);
        i1 = va_arg(ap,int);
*/
        fprintf (stderr,"pdstv: Oops! %s column definition ",s1);
        fprintf (stderr,"(%ld) slipped past with %d bytes.\n",i1,i2);
        fprintf (stderr,"I'm resetting type to 'CHARACTER'...\n");
        break;

      case OPEN_FAIL:
        s1 = va_arg(ap,char *);
        s2 = va_arg(ap,char *);
        s3 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (stderr,"L:%d:%d:",linecount,OPEN_FAIL);
        else
            fprintf (stderr,"pdstv: ");
        fprintf (stderr,"Unable to open %s for %s (%s).\n",s1,s2,s3);
        break;

      case PREC_GT_WIDTH:
        i1 = va_arg(ap,int);
        i2 = va_arg(ap,int);
        i3 = va_arg(ap,int);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %d - ",linecount,PREC_GT_WIDTH,i1);
        else
            fprintf (report,"  Column %d: ",i1);
        fprintf (report,"Field precision (%d) exceeds width (%d).\n",i2,i3);
        break;

      case QUOTED_UNDERSCORES:
        if (batch_mode)
            fprintf (report,"L:%d:%d:",linecount,QUOTED_UNDERSCORES);
        else
            fprintf (report,"  line %3d: ",linecount);
        fprintf (report,"Underscores should not be used within quotes.\n");
        break;

      case ROWB_NE_RECB:
        i1 = va_arg(ap,int);
        i2 = va_arg(ap,int);
        if (batch_mode)
            fprintf (report,"L:%d:%d:",linecount,ROWB_NE_RECB);
        else
            fprintf (report,"WARNING: ");
        fprintf (report,"ROW_BYTES (%d) <> RECORD_BYTES (%d) ",i1,i2);
        fprintf (report,"in FIXED_LENGTH record file.\n");
        break;

      case SCALE_EQ_0:
        s1 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,SCALE_EQ_0,s1);
        else
            fprintf (report,"  Column %s: ",s1);
        fprintf (report,"Scaling factor = 0. ");
        fprintf (report,"You didn't really mean that, did you?\n");
        break;

      case STARTBYTE_LT_1:
        s1 = va_arg(ap,char *);
        i1 = va_arg(ap,int);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,STARTBYTE_LT_1,s1);
        else
            fprintf (report,"  Column %s: ",s1);
        fprintf (report,"START_BYTE (%d) < 1.\n",i1);
        break;

      case STARTBYTE_GT_PAR_BYTES:
        s1 = va_arg(ap,char *);
        i1 = va_arg(ap,int);
        i2 = va_arg(ap,int);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,
                        STARTBYTE_GT_PAR_BYTES,s1);
        else
            fprintf (report,"  Column %s: ",s1);
        fprintf (report,"START_BYTE (%d) > BYTES in parent object (%d).\n",
                        i1,i2);
        break;

      case STARTBYTE_GT_ROW_BYTES:
        s1 = va_arg(ap,char *);
        i1 = va_arg(ap,int);
        i2 = va_arg(ap,int);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,
                        STARTBYTE_GT_ROW_BYTES,s1);
        else
            fprintf (report,"  Column %s: ",s1);
        fprintf (report,"START_BYTE (%d) > ROW_BYTES of table (%d).\n",i1,i2);
        break;
      case STRMAX_LT_MIN:
        s1 = va_arg(ap,char *);
        s2 = va_arg(ap,char *);
        s3 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,STRMAX_LT_MIN,s1);
        else
            fprintf (report,"  Column %s: ",s1);
        fprintf (report,"Maxmimum (%s) less than minimum (%s).\n",s2,s3);
        break;

      case STRVALMAX_LT_VALMIN:
        s1 = va_arg(ap,char *);
        s2 = va_arg(ap,char *);
        s3 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,
                        STRVALMAX_LT_VALMIN,s1);
        else
            fprintf (report,"  Column %s: ",s1);
        fprintf (report,"Valid maxmimum (%s) less than valid minimum (%s).\n",
                        s2,s3);
        break;

      case TOO_FEW_TABLE_ROWS:
        l1 = va_arg(ap,long int);
        l2 = va_arg(ap,long int);
        if (batch_mode)
            fprintf (report,"T:%d:%d:",linecount,TOO_FEW_TABLE_ROWS);
        else
            fprintf (report,"\n  *** ");
        fprintf (report,"Number of table rows read (%d) ",l1);
        fprintf (report,"does not agree with ROWS (%d).\n",l2);
        break;

      case UNEXP_END:
        if (batch_mode)
          { fprintf (report,"L:%d:%d:",linecount,UNEXP_END);
            fprintf (report,"Unexpected END of label at line %d.\n",linecount);
          }
        else
          { fprintf (report,"\nUNEXPECTED END OF LABEL at line %d.\n",
                             linecount);
            fprintf (report,"PROCESSING STOPPED.\n");
          }
        break;


      case UNEXP_EOF:
        s1   = va_arg(ap,char *);
        flag = (strcmp(s1,"label")==0)? 'L' : 'F';
        if (batch_mode)
          { fprintf (report,"%c:%d:%d:",flag,linecount,UNEXP_EOF);
            fprintf (report,"Unexpected End of File in %s.\n",s1);
          }
        else
          { fprintf (report,"\nUNEXPECTED END OF FILE in %s.\n",s1);
            fprintf (report,"PROCESSING STOPPED.\n");
          }
        break;

      case UNNAMED_COLUMN:
        s1   = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,UNNAMED_COLUMN,
                        s1);
        else
            fprintf (report,"  Column %s: ",s1);
        fprintf (report,"This column has no NAME.\n");
        break;

      case UNNAMED_CONTAINER:
        i1 = va_arg(ap,int);
        if (batch_mode)
            fprintf (report,"L:%d:%d:",linecount,UNNAMED_COLUMN);
        else
            fprintf (report,"  line %3d: ",linecount);
        fprintf (report,"CONTAINER (line %d) object has no NAME.\n",i1);
        break;

      case VALUE_TOO_LONG:
        s1 = va_arg(ap,char *);    /* colnum */
        s2 = va_arg(ap,char *);    /* keyword */
        s3 = va_arg(ap,char *);    /* value */
        i1 = va_arg(ap,int);       /* bytes */
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s",linecount,
                        VALUE_TOO_LONG,s1);
        else
            fprintf (report,"  Column %s: ",s1);
        fprintf (report,"%s value ('%s') overflows the field (%d bytes).\n",
                         s2,s3,i1);
        break;

      case WIDTH_GT_BYTES:
        s1 = va_arg(ap,char *);
        i1 = va_arg(ap,int);
        i2 = va_arg(ap,int);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,
                        WIDTH_GT_BYTES,s1);
        else
            fprintf (report,"  Column %s: ",s1);
        fprintf (report,"Field width (%d) exceeds byte count (%d).\n",
                        i1,i2);
        break;

      case WIDTH_GT_MAX_DATE:
        s1 = va_arg(ap,char *);
        i1 = va_arg(ap,int);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,
                        WIDTH_GT_MAX_DATE,s1);
        else
            fprintf (report,"  Column %s: ",s1);
        fprintf (report,"Field width (%d) greater than ",i1);
        fprintf (report,"maximum DATE size (YYYY-MM-DD).\n");
        break;

      case WIDTH_GT_MAX_TIME:
        s1 = va_arg(ap,char *);
        i1 = va_arg(ap,int);
        i2 = va_arg(ap,int);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,
                        WIDTH_GT_MAX_TIME,s1);
        else
            fprintf (report,"  Column %s: ",s1);
        fprintf (report,"Field width (%d) greater than maximum TIME size ",i1);
        fprintf (report,"(%d character: yyyy-mm-ddThh:mm:ss.sssZ).\n",i2);
        break;

      case WIDTH_LT_BYTES:
        i1 = va_arg(ap,int);
        i2 = va_arg(ap,int);
        i3 = va_arg(ap,int);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %d - ",linecount,
                        WIDTH_LT_BYTES,i1);
        else
            fprintf (report,"  Column %d: ",i1);
        fprintf (report,"Field width (%d) does not span ",i2);
        fprintf (report,"the field (%d bytes).\n",i3);
        break;

      case WIDTH_LT_MIN_DATE:
        s1 = va_arg(ap,char *);
        i1 = va_arg(ap,int);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,
                        WIDTH_LT_MIN_DATE,s1);
        else
            fprintf (report,"  Column %s: ",s1);
        fprintf (report,"Field width (%d) less than ",i1);
        fprintf (report,"minimum DATE size (YYYY-DDD).\n");
        break;

      case WIDTH_LT_MIN_TIME:
        s1 = va_arg(ap,char *);
        i1 = va_arg(ap,int);
        i2 = va_arg(ap,int);
        if (batch_mode)
            fprintf (report,"L:%d:%d:Column %s - ",linecount,
                        WIDTH_LT_MIN_TIME,s1);
        else
            fprintf (report,"  Column %s: ",s1);
        fprintf (report,"Field width (%d) less than minimum TIME size ",i1);
        fprintf (report,"(%d character: yyyy-dddThh:mm:ss).\n",i2);
        break;

      /********************
       *** Field Errors ***
       ********************/

      case GREATER_THAN_MAX:
      case LESS_THAN_MIN:
      case GREATER_THAN_DMAX:
      case LESS_THAN_DMIN:
      case GREATER_THAN_VMAX:
      case LESS_THAN_VMIN:
      case INVALID_INTEGER:
      case INVALID_REAL:
      case NONPRINT_CHAR:
      case E_FORMAT_MISSING_EXPONENT:
      case DECIMAL_NOT_ALIGNED:
      case INTEGER_NOT_RIGHT_JUSTIFIED:
      case BLANK_FIELD:
      case INVALID_DATE:
      case INVALID_TIME:
        i1 = va_arg(ap,int);
	s2 = va_arg(ap,char *);
        if (batch_mode)
            fprintf (report,"T:%d:%d:Column %d (%s) - ",linecount,ecode,i1,s2);
        else
            fprintf (report,"Record %d, Column %d (%s): ",linecount,i1,s2);
        fprintf (report,"%s.\n",error_msg[ecode]);
        break;



      /*********************
       *** Record Errors ***
       *********************/

      case LENGTH_NE_RECORD_BYTES:
      case NO_CR:
        if (batch_mode)
            fprintf (report,"T:%d:%d:",linecount,ecode);
        else
            fprintf (report,"Record %d: ",linecount);
        fprintf (report,"%s\n",error_msg[ecode]);
        break;









      default:
        fprintf (report,"ERROR: Unrecognized error code: %d\n",ecode);
        break;

    }

  /* Wrap up argument processing and return: */

  va_end(ap);

  return ecode;
}
