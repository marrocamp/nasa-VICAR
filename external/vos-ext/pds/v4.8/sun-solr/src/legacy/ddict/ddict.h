/******************************************************************************

  ddict.h


  Header file for the Keyword Data dictionary PDS Tool.


  Changes:
     
	 05-11-04    MDC         Original code
	 06-17-04    MDC         Added prototype for validate_label routine


 
 *****************************************************************************/


#include "pdsdef.h"
#include "pdsglob.h"
#include "convert.h"
#include "ddaccess.h"
#include "errordef.h"
#include "fiodef.h"
#include "label.h"
#include "labutil.h"
#include "labtool.h"
#include "sysdef.h"
#include "utildef.h"
#include "verlabel.h"

/*--------------------------------------------------------------------*/
/*                     Definitions and Prototypes                     */
/*--------------------------------------------------------------------*/

#define PDS_REPORT_WIDTH   72
#define PDS_REPORT_INDENT  20

#define DDICT_VERSION     "4.7"               


typedef struct lvtool_v {
   ERROR_LIST  *alias_message_ptr;/*list of alias messages            */
   char        *dd_index_name;    /*data dictionary name              */
   STRING_LIST *file_list;        /*list of file names                */
   LOGICAL     inval_dir;         /*exclude directories option        */
   char        *inval_dir_name;   /*file with names of directories    */
   LOGICAL     inval_file;        /*exclude files                     */
   char        *inval_file_name;  /*list of file extensions           */
   char        *log_file_name;    /*name of excluded files log file   */
   FILE        *log_file_ptr;     /*pointer to the "ef"log file       */
   AGGREGATE   odl_root;          /*points to root of odl tree        */
   char        *rpt_file_name;    /*name of report file               */
   FILE        *rpt_file_ptr;     /*pointer to the report file        */
   char        *start_path;       /*path to start pointer search      */
   LOGICAL     use_aliases;       /*allow use of aliases              */
   LOGICAL     use_dd;            /*use data dictionary               */
   LOGICAL     use_di;            /*search directories recursively    */
   LOGICAL     use_log_file;      /*track excluded files              */
   char		   *sel_file_name;    /*list of keywords to exclude, added*/
								  /*for kwvtool  02-04-00  DWS        */
   LOGICAL     use_sel_file;      /*use the sel file list             */
   char        *input_file_name;  /*name of file supplying input list */
   LOGICAL     use_input_file;    /*use input file for file list      */
   LOGICAL     lbl_det;           /*check for level 3 labels          */
} LVTOOL_V;


    /*----------------------------------------------------------------*/
    /*  These are the function prototypes that are used by all ANSI   */
    /*  standard C compilers.  At this time, SUN systems running UNIX */
    /*  do not allow this type of prototype declaration.              */
    /*----------------------------------------------------------------*/

#ifndef SUN_UNIX            
int chk_file_type        (char * , STRING_LIST *);
                               
/*LOGICAL lv_setup         (int, char * [], char **, char **, STRING_LIST **,
                         LOGICAL *, LOGICAL *, LOGICAL *, LOGICAL *,
						 LOGICAL *, LOGICAL *, LOGICAL *, char **);
*/
LOGICAL lv_setup         (int, char * []);
/*void lv_special          (LOGICAL, FILE *, FILE *, STRING_LIST *,
						            LOGICAL, AGGREGATE, LOGICAL, LOGICAL *, CHAR *);
*/
void lv_special          (FILE *);
void QsortString         (char [100][100], int, int);
void rem_dup_ent         (int );
void rpt_format_message  (FILE *, char *);
void rpt_main_footer     (FILE *);
/*FILE *rpt_main_header    (char *, LOGICAL, LOGICAL, LOGICAL, LOGICAL, char *,
						  LOGICAL, LOGICAL , char *);
*/
FILE *rpt_main_header    ();

void rpt_keyword_errors  (FILE *, LOGICAL);
void rpt_semantic_errors (FILE *, LOGICAL);


STRING_LIST *slv_get_volume_list (char *, LOGICAL);

extern STRING_LIST *dir_walk(char *, char *, STRING_LIST *, LOGICAL);

int check_exclusion_opts(char [], char [][100]);

int chk_dir_name(char [][100], STRING_LIST *);

void remove_excluded_kwds(int);

int validate_label (char *);

char *make_index(char *);

    /*----------------------------------------------------------------*/
    /*  These are the function prototypes that are used by C          */
    /*  compilers that do not follow the ANSI standard for the        */
    /*  declaration of function prototypes.                           */
    /*----------------------------------------------------------------*/

#else
int chk_file_type();
int check_exclusion_opts(char [], char [][]);
LOGICAL lv_setup ();
void lv_special();
void QsortString ();
void rem_dup_ent ();
void rpt_format_message ();
void rpt_main_footer ();
FILE *rpt_main_header();
void rpt_keyword_errors ();
void rpt_semantic_errors ();
STRING_LIST *slv_get_volume_list ();
void remove_excluded_kwds();
int validate_label();
char *make_index();

#endif


/*--------------------------------------------------------------------*/
/*                  End of Definitions and Prototypes                 */
/*--------------------------------------------------------------------*/