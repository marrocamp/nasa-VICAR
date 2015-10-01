/****************************************************************************/
/*          INCLUDE FILE FOR LINE.C                                         */
/****************************************************************************/
/*#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <direct.h>
#include <sys/types.h>
#include <windows.h> 
#include <ctype.h>

#include <dos.h>
#include <io.h>          
#include <fstream.h>
#include <signal.h>
*/
/* Re cast because it isn't right */
#include "pdsdef.h"
#define IBMCAST
#define MSCCAST (char *)


#define MAX_LINE_LEN 256
#define DO_SIGNAL_STUFF


/*#include <signal.h>*/

/* Misc...   */
/**************************************************************************************/

#define FA_DIREC        0x10        

/*--------------------------------------------------------------------*/
/*                    General Symbol Definitions                      */
/*--------------------------------------------------------------------*/

#define PDS_MAXLINE        257      /*  Length of a line read from    */
				                    /*  an input file.                */
#define MAX_PATH_LEN 256

/*--------------------------------------------------------------------*/
/*                        line    typedefs                            */
/*--------------------------------------------------------------------*/


#define  DEBUG		     0			    /* DEBUG flag      */
#define  StringLen     9999999   	    /* Length of input string */
#define  NameLen     	40			    /* Length of file names   */

#define  NOOBJ           0
#define  OBJECT          1
#define  ENDOBJ          2

#define  NORMAL          1
#define  FAILURE         0  

#define  TRUE            1
#define  FALSE           0
#define EOS                  0      /*  Your basic end-of-string.     */
/*****************************************************************/
static char  Command[MAX_LINE_LEN] = "";
static char  InitialDir[MAX_PATH_LEN];
static char  * FMask = NULL;
static char  * Start_dir = NULL;
static short IsDir;

static  char PATH_DELIM[] = "\\";

#ifdef MSDOS_TC
static void  DoGetCWD(char *, int, FILE *);
static int   dw_save_cwd(LOGICAL, FILE *);
void         line_header(FILE *, LOGICAL *, LOGICAL *, LOGICAL *, char *, char *, char *);
void         line_path_file(char , char , char );
void         line_process(char *, FILE *, LOGICAL, LOGICAL);
LOGICAL      line_setup (int, char *[], LOGICAL *, LOGICAL *, LOGICAL *, char *, char *, 
						                                             STRING_LIST **, char *);
void         lvddform1(char *, char *);
void         lvddform2(char *, char *, int);
extern char  *sys_get_path ( char *);
extern STRING_LIST *util_append_string_list (STRING_LIST *, char *, int);
char         *util_strip_lead_and_trail (char *, char);


//extern STRING_LIST  *slv_get_volume_list (char *, LOGICAL);
STRING_LIST  *slv_get_volume_list (char *, LOGICAL, char *);
//STRING_LIST  *slv_get_volume_list (char *, LOGICAL);

extern STRING_LIST *dir_walk(char *, char *, STRING_LIST *, LOGICAL, LOGICAL);/*01-15-98*/
#else
static void        DoGetCWD();
static int         dw_save_cwd();
void               line_header();
void               line_path_file();
void               line_process();
LOGICAL            line_setup ();
void               lvddform1();
void               lvddform2();
extern char        *sys_get_path ();
extern STRING_LIST *util_append_string_list ();
char               *util_strip_lead_and_trail ();
extern STRING_LIST *slv_get_volume_list ();
STRING_LIST        *slv_get_volume_list ();
extern STRING_LIST *dir_walk();/*01-15-98*/
#endif


char   * EntryCwd;
char   * CwdParm;
char   * SummName;                             /*output summary file    */
FILE   * opf1;                                 /*handle for SummName    */
char     OutName[NameLen];		               /*Output file name       */











