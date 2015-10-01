/**********************************************************************
 * lvtool.h                                                           *
 *                                                                    *
 **********************************************************************
 * History                                                            *
 * 08-21-99 created  DWS                                              *
 * 05-04-04 MDC       Added 2 MACRO definitions, MAX_KWD_LEN and      *
 *                    MAX_VAL_LEN                                     *
 * 05-05-04 MDC       Removed use_terminal, use_progress, and         *
 *                    use_verbose members from lvtool_v structure     *
 * 01-11-05 MDC       Added function prototype for make_index         *
 * 12-08-05 MDC       Increased MAX_KWD_LEN by 1                      *
 **********************************************************************/

#include "pdsdef.h"
#include "pdsglob.h"
#include "ddaccess.h"
#include "errordef.h"
#include "fiodef.h"
#include "label.h"
#include "labutil.h"
#include "labtool.h"
#include "sysdef.h"
#include "utildef.h"
#include "verlabel.h"
#include "convert.h"

#ifdef MSDOS_TC
#include <conio.h>
#endif

#include <stdio.h>


/*--------------------------------------------------------------------*/
/*                     Definitions and Prototypes                     */
/*--------------------------------------------------------------------*/
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
 *$Change_history                                                     *
 *    DWS   08-21-99   Added this structure                           *
 *    DWS   02-16-00   Added the input_file_name and the              *
 *                     use input file for file list members           *
 *    DWS   05-16-00   Changed ivd file format to one line per        *
 *                     directory                                      *
 *    DWS   07-13-00   Changed ODL files LABEL.C at lines 4293 and    *
 *                     4567 by commenting them out.  They no longer   *
 *                     replace underscores with blanks.  Also         *
 *                     commented out lines 600 thru 605 in LABUTIL.C  *
 *    DWS   04-12-01   Nulled out Sarray in kwvtool.c                 *
 **********************************************************************/
typedef struct lvtool_v {
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
   int         x_lbl_rpt;         /*external label report             */
   char		   *sel_file_name;    /*list of keywords to exclude, added*/
								  /*for kwvtool  02-04-00  DWS        */
   LOGICAL     use_sel_file;      /*use the sel file list             */
   char        *input_file_name;  /*name of file supplying input list */
   LOGICAL     use_input_file;    /*use input file for file list      */
};


#define PDS_REPORT_WIDTH   72
#define PDS_REPORT_INDENT  20

#define VERSION      "2.4"                     /* 07-01-05 */

/* 12-08-05 MDC - Changed MAX_KWD_LEN from 60 to 61 */
/* 05-04-04 MDC - Added these MACRO definitions */
#define MAX_KWD_LEN    61
#define MAX_VAL_LEN    95

#define NBR_EXCLUD_DIRS 30

	int debug = 0;
    /*----------------------------------------------------------------*/
    /*  These are the function prototypes that are used by all ANSI   */
    /*  standard C compilers.  At this time, SUN systems running UNIX */
    /*  do not allow this type of prototype declaration.              */
    /*----------------------------------------------------------------*/
/*
#ifndef SUN_UNIX
*/
int  chk_dir_name        (char [][100] , STRING_LIST *);
int check_exclusion_opts (char [], char [][100]);
int chk_file_type        (char * , STRING_LIST *);
FILE   *kwvtool_setup    ();         
FILE   *kwvtool_setup2   ();         
FILE   *kwvtool_setup3   ();         
LOGICAL lv_setup         (int, char * []);

void lv_special          (FILE *);
void QsortString         (char [1000000][150], int, int);
void rpt_format_message  (FILE *, char *);
void rpt_main_footer     (FILE *);
FILE *rpt_main_header    ();

void rpt_keyword_errors  (FILE *, LOGICAL);
void rpt_semantic_errors (FILE *, LOGICAL);
void report_writer(FILE *, char [DDICT_KW_ARRAY_SIZE][DDICT_VAL_STR_LEN], int);

STRING_LIST *slv_get_volume_list (char *, LOGICAL);

extern STRING_LIST *dir_walk(char *, char *, STRING_LIST *, LOGICAL);
STRING_LIST *new_volume_list (char *);
int validate_label (char *);
char *make_index(char *);








