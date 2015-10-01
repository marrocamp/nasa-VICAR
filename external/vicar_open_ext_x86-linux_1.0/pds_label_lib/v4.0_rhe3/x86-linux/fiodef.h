/**********************************************************************
 * Component                                                          *
 *    Include file fiodef.h                                           *
 * Used By                                                            *
 *    PDS FIOLIB software.                                            *
 * Detailed Description                                               *
 *    Defines symbols, macros, flags, typedefs, and structures        *
 *    used by PDS FIOLIB software.                                    *
 * Author and Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 * Version and Date                                                   *
 *    1.11  October 31, 1991                                          *
 * Change History                                                     *
 *    DPB   08-17-90   Original code.                                 *  
 *    DPB   08-23-90   Changed prototype of fio_restore_output.       *
 *    DPB   08-29-90   Added fio_flush prototype.                     *
 *    DPB   08-31-90   Added the non-ANSI standard ``#ifdef".         *
 *    MDD   09-18-90   Added new prototypes                           *
 *    DPB   02-04-91   Added prototypes for fio_has_var_records and   *
 *                     fio_convert_var_to_stream.  Also added symbols *
 *                     TAB, PDS_SIZE_OF_KEYWORDS, PDS_SIZE_OF_FILLERS *
 *                     PDS_NUMBER_OF_KEYWORDS, PDS_NUMBER_OF_FILLERS, *
 *                     and PDS_NUMBER_OF_VAR_BYTES.                   *
 *    DPB   03-13-91   Added prototypes for fio_get_setup_value and   *
 *                     fio_setup.                                     *
 *    MDD   03-20-91   Added prototype for fio_convert_stream_to      *
 *                     _fixed                                         *
 *    DPB   04-05-91   Added fio_size, fio_initialize, fio_cleanup.   *
 *    MDD   04-05-91   Added fio_convert_to_rms_stream                *
 *    DPB   09-24-91   Added fio_read_line and fio_copy_file, and     *
 *                     removed most of the fio_convert prototypes.    *
 *    DPB   10-23-91   Changed fio_read_line to a static function     *
 *                     called fiox_read_line.                         *
 *    DPB   10-31-91   Changed fiox_read_line prototype.              *
 **********************************************************************/ 

/*--------------------------------------------------------------------*/
/*                  FIOLIB Local Symbol Definitions                   */
/*--------------------------------------------------------------------*/

#define TAB                       9  /* ASCII for the TAB character */
#define PDS_CR_STRING             "\015"
#define PDS_LF_STRING             "\012"
#define PDS_CRLF_STRING           "\015\012"

#define PDS_SIZE_OF_KEYWORDS      6
#define PDS_NUMBER_OF_KEYWORDS    4
#define PDS_SIZE_OF_FILLERS       2
#define PDS_NUMBER_OF_FILLERS     9
#define PDS_NUMBER_OF_VAR_BYTES   2

/*--------------------------------------------------------------------*/
/*                    FIOLIB Function Prototypes                      */
/*--------------------------------------------------------------------*/

    /*----------------------------------------------------------------*/
    /*  These are the function prototypes that are used by all ANSI   */
    /*  standard C compilers.  At this time, SUN systems running UNIX */
    /*  do not allow this type of prototype declaration.              */
    /*----------------------------------------------------------------*/

#ifndef SUN_UNIX

LOGICAL fio_convert_file (char *file_name,
                          int old_record_type,
                          int new_record_type,
                          long old_record_length,
                          long new_record_length);
                          
LOGICAL fio_copy_file (char *old_file_name,
                       char *new_file_name);

void fio_exit (void);

LOGICAL fio_get_setup_value (char *keyword, 
                             char *value);

LOGICAL fio_has_var_records (char *file_name);

char *fio_read_file (char *file_name);

LOGICAL fio_setup (void);

long fio_size (char *file_name);

void fio_write_line (FILE *file_ptr,
                     char *text,
                     int record_type);

static char *fiox_read_line (FILE *file_ptr,
                             long record_length,
                             int record_type,
                             long *char_count,
                             LOGICAL *term_found);

    /*----------------------------------------------------------------*/
    /*  These are the function prototypes that are used by C          */
    /*  compilers that do not follow the ANSI standard for the        */
    /*  declaration of function prototypes.                           */
    /*----------------------------------------------------------------*/

#else

LOGICAL fio_convert_file ();
LOGICAL fio_copy_file ();
void fio_exit ();
LOGICAL fio_get_setup_value ();
LOGICAL fio_has_var_records ();
char *fio_read_file ();
LOGICAL fio_setup ();
long fio_size ();
void fio_write_line ();
static char *fiox_read_line ();

#endif

/*--------------------------------------------------------------------*/
/*                        End:  "fiodef.h"                            */
/*--------------------------------------------------------------------*/

