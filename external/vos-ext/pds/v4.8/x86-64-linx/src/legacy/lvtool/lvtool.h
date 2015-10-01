/**********************************************************************
 * lvtool.h                                                           *
 *                                                                    *
 **********************************************************************
 * History                                                            *
 *    DWS   08-21-99   created                                        *
 *    DWS   08-21-99   Added this structure                           *
 *    DWS   02-16-00   Added the input_file_name and the              *
 *                     use input file for file list members           *
 *    DWS   07-13-00   Changed ODL files LABEL.C at lines 4293 and    *
 *                     4567 by commenting them out.  They no longer   *
 *                     replace underscores with blanks.  Also         *
 *                     commented out lines 600 thru 605 in LABUTIL.C  *
 *    DWS   10-08-00   added a "," in front of and behind the string  *
 *                     containing the list of file types to be        *
 *                     excluded.  Made the size of the array of       *
 *                     directory names to be excluded a define and    *
 *                     nulled out the array befor calling             *
 *                     check_options.  Also changed arguement         *
 *                     presentation at init time.                     *
 *    DWS    5-25-01   Added code in file LABUTIL.c in routine		  *
 *					   lu_fetch_all_values to remove beginning /n from*
 *					   keyword value befor it is checked to determine *
 *					   if value is a standard value.                  *
 *    DWS    6-04-01   Added code in LVTOOL.C to prevent missing      *
 *                     FILE_RECORDS keyword message when the          *
 *                     RECORD_TYPE is STREAM.  Added stream to        *
 *                     LVTOOL_V                                       *
 *                     Changed version to 2.7                         *
 *    DWS    8-22-01   Added code to lvtool to display duplicate      *
 *                     group/ojbect names in output.                  *
 *                     Added code to AO_NODES.C to use KA_GROUPS as   *
 *                     well as KA_OBJECTS                             *
 *    DWS    5-20-02   Changed printed comment in line 2075 of        *
 *                     lvtool.c                                       *
 *    DWS   05-23-02   modified labtool.c routine                     *
 *                     lt_read_expanded_label to look in LABEL        *
 *                     directory for files pointed to by pointers     *
 *                     Also added get_label_dir to search the         *
 *                     additional directorys.                         *
 *    MDC   12-16-02   Added routine to remove a node from a link list*
 *					   of string_list objects.						  *
 *	  MDC	12-26-02   Added a member to the LVTOOL_V structure to    *
 *					   allow user to disable WARNING messages.		  *
 *	  MDC   01-06-03   Added a member to the LVTOOL_V structure to	  *
 *					   allow the user to disable INFO messages.		  *
 *	  MDC	03-04-03   Added track_odl_errors and total_error_count   *
 *					   variables.									  *
 *	  DWS   01-13-04   Added INV_EXT_LIST_LENGTH to allow us to change*
 *                     the length of the string that will contain the *
 *                     list of invalide file extensions.              *
 *    MDC   10-18-04   Added declaration to make_index routine, and   *
 *                     created a macro for the pdsdd.full default     *
 *                     file name called PDS_DEFAULT_DD_FULL           *
 *    MDC   01-03-05   Added new member to LVTOOL_V structure         *
 *    MDC   10-05-05   Added MACRO definition for FILENAME_LENGTH     *
 *    MDC   01-26-06   Added new function prototype for non-windows   *
 *                     systems, interpret_unix_dir_symbol             *
 *    MDC   02-23-06   Added more keywords to the pointer_list        *
 **********************************************************************/



/**********************************************************************
 *LVTOOL_V                                                            *
 *    This structure contains all of the variables used by lv_setup,  *
 *    lv_special, slv_special, and rpt_main_header.  As the arguement *
 *    lists grew it bacame increasingly difficult to add new features.*
 *    This structure simplifies that process.                         *
 *                                                                    *
 **********************************************************************
 * The members of LVTOOL_V are:                                       *
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
 *    odl_root                                                        *
 *        Points to the root of an ODL tree.                          *
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
 *    stream                                                          *
 *        set to true if RECORD_TYPE = STREAM, 0 if not               *
 *																	  *
 * Added by MDC 12-26-02:											  *
 *	  warnings														  *
 *		  allows user to trun off WARNING messages					  *
 **********************************************************************/
 struct lvtool_v {
   ERROR_LIST  *alias_message_ptr;/*list of alias messages            */
   char        *dd_index_name;    /*data dictionary name              */
   STRING_LIST *file_list;        /*list of file names                */
   LOGICAL     inval_dir;         /*exclude directories option        */
   char        *inval_dir_name;   /*file with names of directories    */
   LOGICAL     inval_file;        /*exclude files                     */
   char        *inval_file_name;  /*list of file extensions           */
   LOGICAL     lbl_det;           /*exclude unlabeled files           */
   char        *log_file_name;    /*name of excluded files log file   */
   FILE        *log_file_ptr;     /*pointer to the "ef"log file       */
   char        lv_or_slv;         /*lv or slv option                  */
   LOGICAL     no_wrap;           /*do not wrap error message option  */
   AGGREGATE   odl_root;          /*points to root of odl tree        */
   char        *rpt_file_name;    /*name of report file               */
   FILE        *rpt_file_ptr;     /*pointer to the report file        */
   char        *start_path;       /*path to start pointer search      */
   LOGICAL     use_aliases;       /*allow use of aliases              */
   LOGICAL     use_dd;            /*use data dictionary               */
   LOGICAL     use_di;            /*search directories recursively    */
   LOGICAL     use_expand;        /*expand all ^STRUCTURE pointers    */
   LOGICAL     use_log_file;      /*track excluded files              */
   LOGICAL     use_progress;      /*display progress of LVTOOL proc   */
   LOGICAL     use_terminal;      /*display all messages to terminal  */
   LOGICAL     use_verbose;       /*display all optional messages     */
   LOGICAL	   warnings;		  /*12-26-02 MDC disable warning msgs */
   LOGICAL	   info;			  /*01-06-03 MDC display info msgs	  */
   LOGICAL     save_expanded_files; /*01-03-05 MDC save expanded files */
   int         x_lbl_rpt;         /*external label report             */
   char        *input_file_name;  /*name of file supplying input list */
   LOGICAL     use_input_file;    /*use input file for file list      */
   LOGICAL     stream;            /*set to true if RECORD_TYPE =      */
								  /*STREAM                            */
	};





/*--------------------------------------------------------------------*/
/*                     Definitions and Prototypes                     */
/*--------------------------------------------------------------------*/
#define NBR_EXCLUD_DIRS    30
#define PDS_REPORT_WIDTH   72
#define PDS_REPORT_INDENT  20
#define FILENAME_LENGTH    32
/*--------------------------------------------------------------------*/
/*                                                                    */
/*This is the length of the buffer used for saveing the list of       */
/*invalid file extensions.  Added by DWS  01/13/04                    */
/*--------------------------------------------------------------------*/
#define INV_EXT_LIST_LENGTH  500

#define LVTOOL_VERSION       "2.20"                         

/* 02-23-06 MDC - Added the following to the pointer_list:
    HEADER
	QUBE
	IMAGE
	SPREADSHEET
	SERIES
	SPECTRUM
	PALETTE
*/
char pointer_list[14][12] = 
    {"CATALOG",
	"DESCRIPTION",
	"DOCUMENT",
	"TABLE",
    "TEXT",
    "STRUCTURE",
	"HEADER",
	"QUBE",
	"IMAGE",
	"SPREADSHEET",
	"SERIES",
	"SPECTRUM",
	"PALETTE",
	""};
    /*----------------------------------------------------------------*/
    /*  These are the function prototypes that are used by all ANSI   */
    /*  standard C compilers.  At this time, SUN systems running UNIX */
    /*  do not allow this type of prototype declaration.              */
    /*----------------------------------------------------------------*/

/*--------------------------------------------------------------------*/
/* 02-14-03 MDC														  */
/* lvtool_max_errors will limit the number of errors that lvtool will */
/* print to an output file. The default value will be 300 error		  */
/* messages. The user can specify the max number of errors by running */
/* lvtool with the -max flag followed by an integer value.			  */
/*--------------------------------------------------------------------*/
long lvtool_max_errors = PDS_MAX_ERRORS;

/*--------------------------------------------------------------------*/
/* 02-18-03 MDC														  */
/* lvtool_warning_count variable will keep a running count of warnings*/
/* encountered during processing.									  */
/*--------------------------------------------------------------------*/
long lvtool_warning_count = 0;


/* 03-04-03 MDC - Flag to keep track of the total number of errors accumulated 
			throughout a program execution. */
extern long total_error_count;

/* 10-18-04 MDC - Added to create the data dictionary index file internally in
            the program.
*/
char *make_index(char *);

/*-------------------------------------------------------------------*/
/* 03-04-03 MDC														 */
/* This flag will allow us to properly keep track of ODL errors		 */
/* accumulated whenever the label file is being parsed. Lvtool seems */
/* to parse the same label file multiple times for various reasons,  */
/* so this flag was created to avoid counting the same error more	 */
/* than once.														 */
/*-------------------------------------------------------------------*/
LOGICAL track_odl_errors = FALSE;

/* 06-03-05 MDC */
extern STRING_LIST *find_pointer_files(char *, char *, STRING_LIST *, char *);
extern char *create_start_path(char *, char *);

#ifndef SUN_UNIX            


void check_exclusion_opts (char [], char [][100]);
void check_the_files      (STRING_LIST *, char *, FILE *, long, char *);            /*01-15-98*/
int  chk_file_type        (char * , STRING_LIST *);
int  chk_dir_name         (char [][100] , STRING_LIST *);
int  expand_label         (FILE *, char *, char *);                 
LOGICAL lv_setup          (int, char * []);
void lv_special           ();
int  make_out_file        (STRING_LIST *, FILE *);
int  make_out_file_1      (char *, char *);                                         /*07-14-98*/

                               
void rpt_file_messages    (char *, STRING_LIST *, FILE *, long, char *);            /*01-15-98*/
STRING_LIST *lu_keyword_values(AGGREGATE, char *, int,  LOGICAL, FILE *, char *);  /*02-02-98*/

void rpt_file_header     (FILE *, char *);
void rpt_format_message  (FILE *, char *);
void alt_rpt_format_message  (FILE *, char *);
void rpt_main_footer     (FILE *);
FILE *rpt_main_header    ();
void rpt_odl_errors      (FILE *, LOGICAL);

void rpt_semantic_errors (FILE *, LOGICAL);

void rpt_print_tree      (FILE *, AGGREGATE, ERROR_LIST *);
void search_for_files    (char *, FILE *, char *);                              /*01-15-98*/
void search_for_pointers (char *, FILE *, char *);                              /*01-15-98*/
void slv_special         ();

void slv_file_header     (FILE *, char *);

void slv_main_footer     (FILE *, LOGICAL);

/*FILE *slv_main_header    (char *, LOGICAL, LOGICAL, LOGICAL, char *, LOGICAL, int);
*/
void slv_odl_messages    (void);

void slv_semantic_errors (void);

void slv_alias_messages  (void);

STRING_LIST *slv_get_volume_list (char *, LOGICAL);
int validate_label (char *);


extern STRING_LIST *dir_walk(char *, char *, STRING_LIST *, LOGICAL, LOGICAL);/*01-15-98*/
extern int dir_walk_2(char *);
extern STRING_LIST *lu_fetch_all_values (PARAMETER, LOGICAL, LOGICAL);
extern void ver_init(char *);											      /*08-17-98*/
STRING_LIST *new_volume_list (char *);

/*12-16-02 Added by MDC */
void deallocate_string_list_node(STRING_LIST *, STRING_LIST **);

char *make_index(char *);
    /*----------------------------------------------------------------*/
    /*  These are the function prototypes that are used by C          */
    /*  compilers that do not follow the ANSI standard for the        */
    /*  declaration of function prototypes.                           */
    /*----------------------------------------------------------------*/

#else
void check_exclusion_opts();
int chk_dir_name();
int chk_file_type();
void check_the_files ();                                          /*01-21-98*/
int expand_label();
int make_out_file();
int make_out_file_1();                                             /*07-14-98*/
STRING_LIST *lu_keyword_values();                                  /*01-21-98*/
LOGICAL lv_setup();                            
void lv_special();
void rpt_file_header();
void rpt_format_message();
void alt_rpt_format_message();
void rpt_main_footer ();
FILE *rpt_main_header();
void rpt_file_messages();                                         /*01-21-98*/
void rpt_odl_errors ();
void rpt_semantic_errors ();
void rpt_print_tree ();
void slv_special();
void search_for_files();                                          /*01-21-98*/
void search_for_pointers();                                       /*01-21-98*/

void slv_file_header             ();
void slv_main_footer             ();
FILE *slv_main_header            ();
void slv_odl_messages            ();
void slv_semantic_errors         ();
void slv_alias_messages          ();
STRING_LIST *slv_get_volume_list ();
int validate_label ();
STRING_LIST *new_volume_list ();
void deallocate_string_list_node();							/* 12-16-02 MDC */
char *make_index();

/* 01-26-06 MDC - Added */
char *interpret_unix_dir_symbol(char *);

#endif


long slv_dd_errors             = 0;
long slv_dd_warnings           = 0;
long slv_odl_errors            = 0;
long slv_odl_warnings          = 0;
long slv_all_aliases           = 0;
long slv_file_dd_errors        = 0;
long slv_file_dd_warns         = 0;
long slv_file_odl_errors       = 0;
long slv_file_odl_warns        = 0;
long slv_file_aliases          = 0;


/*--------------------------------------------------------------------*/
/*                  End of Definitions and Prototypes                 */
/*--------------------------------------------------------------------*/


