/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Component                                                           * 
 *    Include file: verlabel.h                                         *
 * Abstract                                                            *
 *    PDS label object and keyword verification include file           *
 * Detailed Description                                                *
 *    This file contains the function prototypes and symbol            *
 *    definitions used by PDS keyword and object verification routines.*
 * Authors and Institutions                                            *
 *    David P. Bernath / J.P.L.                                        *
 * Version and Date                                                    *
 *    1.1   March 23, 1992                                             *
 * Change History                                                      *
 *    DPB   06-12-91   Original code.                                  *
 *    MDD   03-23-92   Modified function prototypes.                   *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

    /*-----------------------------------------------------------------*/
    /* The following symbols are used by keyword verification routines */
    /* to determine the data type of a keyword.  PLEASE NOTE that they */
    /* are defined as powers of two.  THE CODE RELIES ON THIS, SO      */
    /* DON'T CHANGE IT!!                                               */
    /*-----------------------------------------------------------------*/

#define VER_UNKNOWN      0
#define VER_NULL         1
#define VER_INTEGER      2
#define VER_REAL         4
#define VER_CHARACTER    8
#define VER_DATE        16
#define VER_DATE_TIME   32
#define VER_IDENTIFIER  64  /*added 8-27-02  DWS*/

    /*-----------------------------------------------------------------*/
    /* These are macros used by label verification routines.           */
    /*-----------------------------------------------------------------*/

#define IsMarked(zxz)  (*zxz < 'A')
#define SetMark(zxz)   (*zxz -= 26)
#define ClearMark(zxz) (*zxz += 26)

    /*-----------------------------------------------------------------*/
    /* These are the label verification function prototypes            */
    /*-----------------------------------------------------------------*/

#ifndef SUN_UNIX
#ifndef LV_DDICT
LOGICAL ver_semantics (AGGREGATE);
LOGICAL ver_keywords (AGGREGATE, char *);
LOGICAL ver_keyword_semantics (PARAMETER);
#else
LOGICAL ver_semantics (FILE *, AGGREGATE);
LOGICAL ver_keywords (FILE *,AGGREGATE, char *);
LOGICAL ver_keyword_semantics (FILE *, PARAMETER);
#endif
LOGICAL ver_sub_objects (AGGREGATE, char *);
int ver_get_type_from_class (PARAMETER);
int ver_get_type_from_dd (PARAMETER);
int ver_get_type_from_odl (VALUE);
char *ver_get_type_string (int);
char *ver_get_value_string (VALUE, long);
LOGICAL ver_date (PARAMETER);
LOGICAL ver_date_string (char *, char *, long);
LOGICAL ver_date_time (PARAMETER);
LOGICAL ver_integer_range (PARAMETER);
LOGICAL ver_real_range (PARAMETER);
LOGICAL ver_spelling (PARAMETER);
LOGICAL ver_standard_values (PARAMETER);
LOGICAL ver_standard_values_2 (PARAMETER);  /* added 8-27-02  DWS*/
LOGICAL ver_string_length (PARAMETER);
LOGICAL ver_time_string (char *, char *, long);
LOGICAL ver_units (PARAMETER);
LOGICAL ver_using_class_word (PARAMETER);
char *ver_pad_date (char *);
char *ver_pad_time (char *);
char *ver_pad_date_time (char *);
void  ver_init(char *);                                      /*DWS 08-17-98*/
void check_keyword_length(char *);    /* MDC 04-12-06 */
void do_more_kwd_semantics( PARAMETER, char *, STRING_LIST * ); /* MDC 06-08-06 */

#else

LOGICAL ver_semantics ();
LOGICAL ver_sub_objects ();
LOGICAL ver_keywords ();
LOGICAL ver_keyword_semantics ();
int ver_get_type_from_class ();
int ver_get_type_from_dd ();
int ver_get_type_from_odl ();
char *ver_get_type_string ();
char *ver_get_value_string ();
LOGICAL ver_date ();
LOGICAL ver_date_string ();
LOGICAL ver_date_time ();
LOGICAL ver_integer_range ();
LOGICAL ver_real_range ();
LOGICAL ver_spelling ();
LOGICAL ver_standard_values ();
LOGICAL ver_string_length ();
LOGICAL ver_time_string ();
LOGICAL ver_units ();
LOGICAL ver_using_class_word ();
char *ver_pad_date ();
char *ver_pad_time ();
char *ver_pad_date_time ();

void check_keyword_length();    /* MDC 04-12-06 */
void do_more_kwd_semantics();
#endif


