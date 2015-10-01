/**********************************************************************
 * Component                                                          *
 *    Include file tvtool.h                                           *
 * Used By                                                            *
 *    The PDS Table Browser                                           *
 * Detailed Description                                               *
 *    Defines symbols, macros, flags, typedefs, and structures        *
 *    used by the ASCII version of the PDS Table Browser .            *
 * Author and Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 * Version and Date                                                   *
 *    1.0   June 01, 1992                                             *
 *    1.1   August 08, 2002											  *
 *    1.2   October 10, 2002										  *
 * Change History                                                     *
 *    DPB   06-01-92   Created file.                                  *
 *    MDC   10-10-02  1) Added a new structure, container_info, which *
 *						 holds values found within a container        *
 *						 description.								  *
 *					  2) Added a new member, container_count, to the  *
 *						 table_info structure to keep count of the    *
 *					     number of containers within a label.	      *
 *					  3) Added a new member, is_in_container, to the  *
 *						 column_info structure to indicate whether    *
 *						 a column is in a container or not.			  *
 *					  4) Added a new member, net_column, to the		  *
 *						 table_info structure to keep track of the    *
 *						 number of columns in a label without going   *
 *					     through a repetition if a container exists   *
 *						 in a label.								  *
 *					  5) Added 2 new members to the column_info struct*
 *						 rep_num and col_num. These are for when there*
 *						 exists a container and you wanted to keep    *
 *						 track of what column you are looking at and  *
 *						 in what repetition.						  *
 *	 MDC   11-26-02   1) Added new members to table_info and		  *
 *						 column_info for container handling in labels.*
 *					  2) Added new members to container_info to allow *
 *						 the containers to be joined together in a	  *
 *						 link list form.							  *
 *	MDC	   12-04-02   1) Added 2 members into the table_info structure*
 *	MDC	   02-25-03   1) Added a 3rd argument to tb_get_column_info   *
 *						 routine.									  *
 *					  2) Added a append_to_container_list routine to  *
 *					     add a container to a link list of container  *
 *						 objects.									  *
 **********************************************************************/ 

/*--------------------------------------------------------------------*/
/* Definitions of screen and prompt characteristics.                  */
/*--------------------------------------------------------------------*/

#define TB_PROMPT_STRING "PDS TABLE BROWSER>"

#define TB_DISPLAY_ROWS  15
#define TB_LINE_LEN      72
#define TB_UNKNOWN       -1

/*--------------------------------------------------------------------*/
/* Definitions of tbtool commands.                                    */
/*--------------------------------------------------------------------*/

#define TB_FILE        'F'
#define TB_TABLE       'T'
#define TB_COLUMN      'C'
#define TB_BIT_COL     'B'
#define TB_DOWN        'D'
#define TB_EXIT        'E'
#define TB_EDIT        'e'
#define TB_HELP1       'H'
#define TB_HELP2       '?'
#define TB_JUMP        'J'
#define TB_LABEL       'L'
#define TB_NEXT        'N'
#define TB_PREV        'P'
#define TB_QUIT        'Q'
#define TB_ROW         'R'
#define TB_SELECT      'F'
#define TB_SUMMARY     'S'
#define TB_UP          'U'

/*--------------------------------------------------------------------*/
/* 12-03-02  MDC													  */
/* Added command to jump to a container in a table					  */
/*--------------------------------------------------------------------*/
#define TB_CONTAINER   'K'

#define VERSION         "1.8"

/*--------------------------------------------------------------------*/
/* Definitions of summary data types, and max occurrence table size   */
/*--------------------------------------------------------------------*/

#define TS_MAX_OCCURRENCES  500

#define TS_UNKNOWN        0
#define TS_REAL           1
#define TS_CHARACTER      2
#define TS_INTEGER        3
#define TS_UNSIGNED       4
#define TS_DATETIME       5

/*--------------------------------------------------------------------*/
/* The COLUMN_INFO structure stores the information for one column    */
/*    object.  These are linked together in a list to represent       */
/*    consecutive columns.                                            */
/*																	  */
/* 10-10-02 MDC														  */
/*     Added a new member, is_in_container, to indicate whether a     */
/*	   column is in a container or not. '1' means it is, '0' means it */
/*	   is not.														  */
/*     Added a new member, rep_num, to indicate what rep a column is  */
/*	   on if it is inside a container.								  */
/*	   Added a new member, col_num, to indicate the column number that*/
/*     the column is located if it is inside a container.			  */
/*																	  */
/* 12-04-02 MDC														  */
/*	   Added a new member, which_container, to record the container   */
/*	   id so that we can associate a particular container column with */
/*	   the container it resides in.									  */
/*--------------------------------------------------------------------*/

typedef struct column_info 
{
    AGGREGATE column;
    char *name;
    char *class;
    char *data_type;
    char *display_format;
    long size;
    long start;
    long items;
    long item_offset;
    long bit_column_count;
    LOGICAL display;
    struct column_info *bit_columns;
    struct column_info *next;
    struct column_info *prev;

	/* 10-10-02 Added by MDC */
	short is_in_container;
	long col_num;
	long rep_num;
	
	long which_container;

} COLUMN_INFO;


/*-------------------------------------------------------------------*/
/**	10-09-02 Added by MDC											**/
/** The CONTAINER_INFO structure is used to hold information about  **/
/** a container found in a label.									**/
/**																	**/
/** 11-26-02 Added by MDC											**/
/** Added 3 new variables, next_container, prev_container and		**/
/** columns. The first 2 aforementioned variables allow building a  **/
/** link list of container objects. The variable, columns, will     **/
/** record the number of columns in a container.					**/
/**																	**/
/**	12-04-02 MDC													**/
/** Added a new variable, container_id, to give each container a    **/
/** unique identification so that we can associate container        **/
/** columns with the container it resides in.						**/
/*-------------------------------------------------------------------*/

typedef struct container_info
{
	char *name;
	long start_byte;
	long repetitions;
	long bytes;
	long columns;
	
	long container_id;

	struct container_info *next_container;
	struct container_info *prev_container;
} CONTAINER_INFO;

/*--------------------------------------------------------------------*/
/* The TABLE_INFO structure stores the information for one table      */
/*    (series, spectrum) object. They are linked together in a list   */
/*    to represent multiple objects in one label.                     */
/*																	  */
/* 10-10-02 MDC														  */
/* Added a new member, container_count, to keep track of how many     */
/* containers are in the table.										  */
/*																	  */
/* 11-26-02 MDC														  */
/* Added a new member, container, to record container information	  */
/* found in a label. If there are multiple containers or nested		  */
/* containers in a label, this variable will join these objects		  */
/* together in a link list format.									  */
/*																	  */
/* 12-05-02 MDC														  */
/* Added 2 members, container_columns and ordinary_columns, to keep   */
/* track of the types of columns found in a file.					  */
/*--------------------------------------------------------------------*/

typedef struct table_info 
{
    AGGREGATE table;
    COLUMN_INFO *columns;
    char *name;
    char *class;
    char *fname;
    char *interchange_format;
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
	long container_columns;
	
	/* 12-04-02 Added by MDC */
	long ordinary_columns;

	/* 11-26-02 Added by MDC */
	struct container_info *container;

} TABLE_INFO;

/*-------------------------------------------------------------------*/
/** The OCCURRENCE_ENTRY structure is used to create a sorted tree  **/
/** of string occurrence counts, in order to display a table of     **/
/** the occurrences of all data values in a column.                 **/
/*-------------------------------------------------------------------*/

typedef struct occurrence_entry 
{
   char *string;
   struct occurrence_entry *left_child;
   struct occurrence_entry *right_child;
   long count;
} OCCURRENCE_ENTRY;

/*-------------------------------------------------------------------*/
/** Function prototypes.                                            **/
/*-------------------------------------------------------------------*/

#ifndef SUN_UNIX

void tb_convert_data_file (char *(*));
void tb_cleanup (void);
void tb_display_column (void);
void tb_display_label (void);
void tb_edit_label_info (long);
unsigned char *tb_format_bit_column_value (COLUMN_INFO *, unsigned char *,
                                           long *, char *);
unsigned char *tb_format_column_value (COLUMN_INFO *, unsigned char *, long);
COLUMN_INFO *tb_get_bit_column_info (AGGREGATE, COLUMN_INFO *);

COLUMN_INFO *tb_get_column_info (AGGREGATE, TABLE_INFO *, long); 
short tb_get_command (char *);
void tb_get_next_table (void);
void tb_get_prev_table (void);
void tb_get_file_info (AGGREGATE);
TABLE_INFO *tb_get_table_info (AGGREGATE, char *);
void tb_display_help (void);
void tb_jump_to_row (long);
unsigned char *tb_justify_bit_column_value (COLUMN_INFO *, unsigned char *,
					    long *, LOGICAL);
void tb_move_left (COLUMN_INFO *(*), long *, long *, long, long *);
void tb_move_right (COLUMN_INFO *(*), long *, long *, long, long *);
void tb_navigate (short, char *);
unsigned char *tb_read_column (COLUMN_INFO *, FILE *, long, long *);
void tb_replace_from_list (char *(*), char *[], long, char *);
void tb_select_label_file (char *);
void tb_setup (void);

void ts_summarize_column (void);
int ts_map_type (COLUMN_INFO *);
OCCURRENCE_ENTRY *ts_insert_occurrence (OCCURRENCE_ENTRY *, char *);
OCCURRENCE_ENTRY *ts_cleanup_occurrence (OCCURRENCE_ENTRY *);
void ts_print_occurrence (OCCURRENCE_ENTRY *);

/* 10-09-02 Added by MDC */
CONTAINER_INFO *get_container_params(AGGREGATE, TABLE_INFO *);

/* 02-25-02 Added by MDC */
void append_to_container_list(CONTAINER_INFO *, TABLE_INFO *);

#else

void tb_convert_data_file ();
void tb_cleanup ();
void tb_display_column ();
void tb_display_help ();
void tb_display_label ();
void tb_edit_label_info ();
unsigned char *tb_format_bit_column_value ();
unsigned char *tb_format_column_value ();
COLUMN_INFO *tb_get_bit_column_info ();
COLUMN_INFO *tb_get_column_info ();
short tb_get_command ();
void tb_get_next_table ();
void tb_get_prev_table ();
void tb_get_file_info ();
TABLE_INFO *tb_get_table_info ();
void tb_jump_to_row ();
unsigned char *tb_justify_bit_column_value ();
void tb_move_left ();
void tb_move_right ();
void tb_navigate ();
unsigned char *tb_read_column ();
void tb_replace_from_list ();
void tb_select_label_file ();
void tb_setup ();

void ts_summarize_column ();
int ts_map_type ();
OCCURRENCE_ENTRY *ts_insert_occurrence ();
OCCURRENCE_ENTRY *ts_cleanup_occurrence ();
void ts_print_occurrence ();

/* 10-09-02 Added by MDC */
CONTAINER_INFO *get_container_params();
void append_to_container_list();

#endif                     
                        
/*-------------------------------------------------------------------*/
/** Variables to keep track of the size of the occurrence table     **/
/** and the count of occurrences already displayed on the screen    **/
/** by the summary feature.                                         **/
/*-------------------------------------------------------------------*/

long ts_occurrence_count = 0;
int ts_line_count = 0;

/*-------------------------------------------------------------------*/
/** Variables which store the valid blocking type list and its      **/
/** size.                                                           **/
/*-------------------------------------------------------------------*/

long tb_blocking_type_end = {0};

char *tb_blocking_type_list [] = 
{
    "SPANNED",
    "UNSPANNED",
     NULL
};

/*-------------------------------------------------------------------*/
/** Variables which store the valid record type list and its        **/
/** size.                                                           **/
/*-------------------------------------------------------------------*/

long tb_record_type_end = {0};

char *tb_record_type_list [] = 
{
     "FIXED LENGTH",
     "STREAM",
     "UNKNOWN",
     "VARIABLE LENGTH",
     NULL
};

/*-------------------------------------------------------------------*/
/** Definitions of tbtool data display modes.                       **/
/*-------------------------------------------------------------------*/

#define TB_DISPLAY_BINARY  0
#define TB_DISPLAY_HEX     1
#define TB_DISPLAY_OCTAL   2
#define TB_DISPLAY_TEXT    3

/*-------------------------------------------------------------------*/
/** Variables which store the valid display type list and its       **/
/** size.                                                           **/
/*-------------------------------------------------------------------*/

long tb_display_end = {0};

char *tb_display_list [] = 
{
    "BINARY",
    "HEX",
    "OCTAL",
    "TEXT",
     NULL
};

/*-------------------------------------------------------------------*/
/** Definitions of valid data formats.                              **/
/*-------------------------------------------------------------------*/

#define TB_FORMAT_ASCII    0
#define TB_FORMAT_BINARY   1

/*-------------------------------------------------------------------*/
/** Variables which store the valid data format list and its        **/
/** size.                                                           **/
/*-------------------------------------------------------------------*/

long tb_format_end = {0};

char *tb_format_list [] = 
{
    "ASCII",
    "BINARY",
     NULL
};

/*-------------------------------------------------------------------*/
/** Definitions of valid bit column data types                      **/
/*-------------------------------------------------------------------*/

#define TB_BOOLEAN_BIT                 0
#define TB_INTEGER_BITS                1
#define TB_MSB_INTEGER_BITS            2
#define TB_MSB_UNSIGNED_INTEGER_BITS   3
#define TB_UNSIGNED_INTEGER_BITS       4

/*-------------------------------------------------------------------*/
/** Variables which store the valid bit data type list and its      **/
/** size.                                                           **/
/*-------------------------------------------------------------------*/

long tb_bit_type_end = {0};

char *tb_bit_type_list [] = 
{
    "BOOLEAN",
    "INTEGER",
    "MSB INTEGER",
    "MSB UNSIGNED INTEGER",
    "UNSIGNED INTEGER",
    NULL
};

/*-------------------------------------------------------------------*/
/** Definitions of valid column data types                          **/
/*-------------------------------------------------------------------*/

#define TB_ASCII_COMPLEX          0
#define TB_ASCII_INTEGER          1
#define TB_ASCII_REAL             2
#define TB_BOOLEAN                3
#define TB_CHARACTER              4
#define TB_COMPLEX                5
#define TB_DATE                   6
#define TB_FLOAT                  7
#define TB_IBM_COMPLEX            8
#define TB_IBM_INTEGER            9
#define TB_IBM_REAL              10
#define TB_IBM_UNSIGNED_INTEGER  11
#define TB_IEEE_COMPLEX          12
#define TB_IEEE_REAL             13
#define TB_INTEGER               14
#define TB_LSB_BIT_STRING        15
#define TB_LSB_IEEE_COMPLEX      16
#define TB_LSB_IEEE_REAL         17
#define TB_LSB_INTEGER           18
#define TB_LSB_UNSIGNED_INTEGER  19
#define TB_MAC_COMPLEX           20
#define TB_MAC_INTEGER           21
#define TB_MAC_REAL              22
#define TB_MAC_UNSIGNED_INTEGER  23
#define TB_MSB_BIT_STRING        24
#define TB_MSB_IEEE_COMPLEX      25
#define TB_MSB_IEEE_REAL         26
#define TB_MSB_INTEGER           27
#define TB_MSB_UNSIGNED_INTEGER  28
#define TB_PC_COMPLEX            29
#define TB_PC_INTEGER            30
#define TB_PC_REAL               31
#define TB_PC_UNSIGNED_INTEGER   32
#define TB_REAL                  33
#define TB_SUN_COMPLEX           34
#define TB_SUN_INTEGER           35
#define TB_SUN_REAL              36
#define TB_SUN_UNSIGNED_INTEGER  37
#define TB_TIME                  38
#define TB_UNSIGNED_INTEGER      39
#define TB_VAX_COMPLEX           40
#define TB_VAX_DOUBLE            41
#define TB_VAX_INTEGER           42
#define TB_VAX_REAL              43
#define TB_VAX_UNSIGNED_INTEGER  44
#define TB_VAXG_COMPLEX          45
#define TB_VAXG_REAL             46

/*-------------------------------------------------------------------*/
/** Variables which store the valid column data type list and its   **/
/** size.                                                           **/
/*-------------------------------------------------------------------*/

long tb_type_end = {0};

char *tb_type_list [] = 
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

/*-------------------------------------------------------------------*/
/** Current label, table, column, and bit_column structure pointers.**/
/*-------------------------------------------------------------------*/

AGGREGATE tb_label_ptr = {NULL};
TABLE_INFO *tb_table_info = {NULL};
TABLE_INFO *tb_table_list = {NULL};
COLUMN_INFO *tb_bit_column_info = {NULL};
COLUMN_INFO *tb_column_info = {NULL};

/*-------------------------------------------------------------------*/
/** Values of important FILE keywords.                              **/
/*-------------------------------------------------------------------*/

char *tb_record_type = {NULL};
char *tb_blocking_type = {NULL};
long tb_block_bytes = {0};
long tb_block_records = {0};
long tb_record_bytes = {0};
long tb_file_records = {0};

/*-------------------------------------------------------------------*/
/** Variables that track the count and offset of bit_columns, bit   **/
/**    items, columns, column items, rows, and tables.              **/
/*-------------------------------------------------------------------*/

long tb_bit_column_offset = {0};
long tb_bit_item_count = {0};
long tb_column_offset = {0};
long tb_row_offset = {0};
long tb_display_rows = {TB_DISPLAY_ROWS};
long tb_current_column = {1};
long tb_current_bit_column = {1};
long tb_item_count = {0};
long tb_table_count = {0};
long tb_table_number = {0};

/*-------------------------------------------------------------------*/
/** Flags that control the display mode.                            **/
/*-------------------------------------------------------------------*/

LOGICAL tb_show_bit_column = {FALSE};
LOGICAL tb_show_column = {FALSE};
LOGICAL tb_show_items = {FALSE};
LOGICAL tb_show_table = {TRUE};
LOGICAL tb_show_label = {TRUE};
LOGICAL tb_show_file = {FALSE};
LOGICAL tb_show_summary = {FALSE};

/*-------------------------------------------------------------------*/
/* 12-03-02 MDC														 */
/* Added another flag to display a container column upon user		 */
/* request.															 */
/*-------------------------------------------------------------------*/
LOGICAL tb_show_container = {FALSE};

/*-------------------------------------------------------------------*/
/** Variables for VMS, to convert data files to stream format so    **/
/**    they can be fseek'd.                                         **/
/*-------------------------------------------------------------------*/

#ifdef VAX

LOGICAL tb_vms_var = {FALSE};

struct MACRO_INTERFACE
{
    long varflag;
    char *fname;
    long fnlen;

} macro_interface;

#endif

