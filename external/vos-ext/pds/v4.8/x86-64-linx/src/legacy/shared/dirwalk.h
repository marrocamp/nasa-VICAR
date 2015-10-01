/******************************************************************************/
/* INCLUDE FILE FOR DIRWALK.C                                                 */
/******************************************************************************/
/* Re cast because it isn't right */
#define IBMCAST
#define MSCCAST (char *)


/*#define MAX_PATH_LEN 256*/
#define MAX_LINE_LEN 256
#define DO_SIGNAL_STUFF

#ifndef SUN_UNIX
#include <dos.h>
#include <io.h>            /* Needed for dup2() */
#include <fstream.h>
#endif

#include <signal.h>
#include "pdsdef.h"

/* Misc...   */
static char  Command[MAX_LINE_LEN] = "";
static char  InitialDir[MAX_PATH_LEN];
static char  * FMask = NULL;
static char  * Start_dir = NULL;
static short IsDir;

static  char PATH_DELIM[] = "\\";

STRING_LIST  * dir_walk(char *, char *, STRING_LIST *, LOGICAL, LOGICAL);/*01-15-98*/
static int   dw_save_cwd(LOGICAL);                                       /*01-15-98*/
int   dir_walk_2(char *);  /* 06-06-05 MDC - Removed static keyword since definition  does not have static kwd either */
STRING_LIST  * dir_walk_3(STRING_LIST *, LOGICAL);
static void  dir_walk_5( char *,  char *);
STRING_LIST  * dir_walk_7 (char *, STRING_LIST *);
static void  cdecl Kill(int);
static char  * GetFirstFile(short);
static char  * GetNextFile(short);

extern void         err_append_message (int, char *);
extern char         *sys_get_path ( char *);
extern char         *util_create_file_spec (char *, char *);
extern STRING_LIST  *util_append_string_list (STRING_LIST *, char *, int);

long   DirHandle = 0;
char   * EntryCwd;
struct _finddata_t FileInfo;
char  * CwdParm;
STRING_LIST * temp_f_s;


