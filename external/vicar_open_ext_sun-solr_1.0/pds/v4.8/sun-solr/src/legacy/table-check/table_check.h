/*
   pdstv.h

   This file contains structure and constant declarations for the PDS Table
   Verifier utility 'pdstv'.

   08 Mar 2001, A.C.Raugh

   26 Feb 2002, acr: Increased precision of max/min found in file
   08 Mar 2002. acr: Removed TABLE_TYPE macro (replaced with function)

*/
#include "oal.h"
#include "lablib3.h"


/*===========================================================================
  Macro Definitions
  ===========================================================================*/

#define TB_UNKNOWN       -1
#define  MAXERRORS          10     /* Maximum times to report a single error */
#define  MINDATEWIDTH        8     /* YYYY-DDD                               */
#define  MAXDATEWIDTH       10     /* YYYY-MM-DD                             */
#define  MAXTIMEWIDTH       24     /* YYYY-DDDThh:mm:ss.sssZ                 */
#define  MINTIMEWIDTH       18     /* YYYY-DDDThh:mm:ss                      */
#define  MAXRECORDLENGTH  1000     /* Max input line length for label        */

/* Flag Values: */

#define  MAXLINE           257

#define  TRUE                1
#define  FALSE               0

#define  MAX_FOUND           1
#define  MIN_FOUND           2
#define  BOTH_FOUND          3

#define  MSB                 1
#define  LSB                 0
#define  NOT_SET            -1

#define  TRIM_BLANKS         1
#define  NO_TRIM_BLANKS      0

/* Return status from "skip_object": */

#define  UNEXPECTED_KEYWORD  4
#define  ENDOFOBJECT         3
#define  ENDOFLABEL          2
#define  ENDOFFILE           1

/* Data types: */

#define  UNRECOGNIZED        0
#define  UNSIGNED_INTEGER    1
#define  SIGNED_INTEGER      2
#define  REAL                3
#define  CHARACTER           4
#define  DATE                5
#define  TIME                6
#define  SPARE               7

#ifndef FLDTYPES
#define FLDTYPES
static char *field_type[] =   /* Column data type translation                */
                     { "Unrecognized",     /* 0 */
                       "Unsigned Integer", /* 1 */
                       "Signed Integer",   /* 2 */
                       "Real",             /* 3 */
                       "Character",        /* 4 */
                       "Date",             /* 5 */
                       "Time",             /* 6 */
                       "Spare"             /* 7 */
                     };
#endif


/* Field types: */

#define  COLUMN            100
#define  CONTAINER         200

#define NUMERIC_FIELD(A) (A == SIGNED_INTEGER || A == UNSIGNED_INTEGER || \
                          A == REAL)


#define GetLineNum(ptr,kwd,result) \
	{ \
		KEYWORD *kp; \
		kp = OdlFindKwd(ptr, kwd, NULL, 1, ODL_THIS_OBJECT); \
		if(kp != NULL) {result = kp->line_number;} \
		else result = ptr->line_number; \
	}

		
#define Lemme_Go(ptr) { if (ptr != NULL) free(ptr); ptr = NULL; }			
	

/*===========================================================================
  Structure Definitions
  ===========================================================================*/

/* pdstv structure: */

struct tvstruct 
{
    char  *label_file;     /* label file name  */
    char  *rpt_file;         /* report file pointer */
    char  *directory;      /* remote directory path name */
    int   flag_blanks;      /* TRUE for blank character field flagging */
    int   batch_mode;       /* TRUE if running in batch mode */
    int   time_display;     /* TRUE if processing time should be included in report */
};

typedef struct file_keywords
{
    /*-------------------------------------------------------------------*/
	/** Values of important FILE keywords.                              **/
	/*-------------------------------------------------------------------*/
    char *record_type;
    int fixed_length;
	long record_bytes;
	long file_records;
} FILE_KEYWORDS;

/* Column structure: */
typedef struct column_info 
{
  /*  AGGREGATE column; */
    char *name;
    char *col_class;
    char *data_type;
    char *format;
	long index;           /* COLUMN NUMBER */
	int  type;
    long size;
    long start;
    long items;
    long item_offset;
	long item_bytes;
	short is_in_container;
	long col_num;
	long rep_num;
	long which_container; /* 01-07-03 MDC */
    struct column_info *next;
    struct column_info *prev;
    int     msbflag;          /* 1 = MSB data; 0 = LSB data; -1 = unset      */
    union 
      { double  dbl;
        char   *str;
      } max;                  /* Max field value in file                     */
    union
      { double  dbl;
        char   *str;
      } min;                  /* Min field value in file                     */
    int     mmflag;           /* 1 = max present, 2= min present, 3=both     */
    union 
      { double  dbl;
        char   *str;
      } vmax;                 /* Valid max field value                       */
    union
      { double  dbl;
        char   *str;
      } vmin;                 /* Valid min field value                       */
    int     vmflag;           /* 1 = vmax present, 2= vmin present, 3=both   */
    double  dmax,dmin;        /* Derived max/min present                     */
    int     dmflag;           /* same as mmflag for derived values           */
    double  offset;           /* zero offset (to apply to values in file)    */
    double  scaling_factor;   /* scaling factor (to apply to values in file) */
    int     invflag;          /* true if INVALID is present                  */
    union
      { double  dbl;
        char   *str;
      } invalid;              /* Null data value                             */
    int     missflag;         /* true if MISSING_CONSTANT is present         */
    union
      { double  dbl;
        char   *str;
      } missing;              /* missing data value                          */
    int     fwidth,fprecision;/* "format" field width and precision values   */

    /* Statistics accumulators: */

    union
      { double dbl;           /* maximum actual value encountered, double    */
        int    bytes;         /* maximum string length encountered           */
      } maxfound;
    char      *maxstr;        /* maximum value string                        */
    union
      { double dbl;           /* minimum actual value encountered, double    */
        int    bytes;         /* minimum string length encountered           */
      } minfound;
    char      *minstr;        /* minimum value string                        */
    long int   invalidcount;  /* number of 'invalid' data values founds      */
    long int   missingcount;  /* number of data values flagged as 'missing'  */
    long int   badcount;      /* number of bad values (i.e., not flagged)    */

} COLUMN_INFO;



/* CONTAINER structure: */


typedef struct container_info
{
	long start_byte;
	long repetitions;
	long bytes;

	char *name;

	/* 01-07-03 MDC */
	long columns;
	long container_id;

	struct container_info *next_container;
	struct container_info *prev_container;
} CONTAINER_INFO;


/* TABLE structure: */
/*--------------------------------------------------------------------*/
/* The TABLE_INFO structure stores the information for one table      */
/*    (series, spectrum) object. They are linked together in a list   */
/*    to represent multiple objects in one label.                     */
/*--------------------------------------------------------------------*/

typedef struct table_info 
{
    OBJDESC *table;
    COLUMN_INFO *columns;
    char *name;
    char *tb_class;
    char *fname;
    int  ascii;
	int tblnum;
    long row_count;
    long column_count;
    long row_bytes;
    long prefix_bytes;
    long suffix_bytes;
    long data_location;
    struct table_info *next;
    struct table_info *prev;
	/* 10-10-02 Added by MDC */
	long container_count;
/*	long net_column; */

	struct container_info *container;

} TABLE_INFO;


/* Type conversion structure: */

union typeconv 
    {                double  r8;
                     float   r4;
      unsigned long  int     uli;
               long  int     sli;
      unsigned short int     usi;
               short int     ssi;
      unsigned       char    uc;
                     char    sc;
                     char    byte[8];
    };




/*---------------------------------------------------------------------------
  Functions
*/

int setup(int, char *[]);	 /* Read arguments and open files             */
void check_table(TABLE_INFO *, int); /* Read Table data information */
void parse_FORMAT (COLUMN_INFO *col, int linecount);  /* Parse FORMAT string for attributes        */
void  checkfilename(char *file, int linecount);
int   table_type(char *object_type);  /* Check for TABLE objects             */
TABLE_INFO *tb_get_table_info (OBJDESC *, FILE_KEYWORDS *);
FILE_KEYWORDS *get_file_keywords(OBJDESC *);
void get_table_key_words(OBJDESC *, TABLE_INFO *, long *, int);
COLUMN_INFO *tb_get_column_info (OBJDESC *, TABLE_INFO *, long);
void get_column_keywords(COLUMN_INFO *, OBJDESC *, TABLE_INFO *, CONTAINER_INFO *, long);
int check_column_attributes(COLUMN_INFO *, OBJDESC *, int, int, int , int , int );
CONTAINER_INFO *get_container_params(OBJDESC *, TABLE_INFO *);
OBJDESC *FindTableObj(OBJDESC *, char *, char *, unsigned long, unsigned short);
void Exit_System(char *);
void cleanup(TABLE_INFO *, OBJDESC *, FILE_KEYWORDS *);


