/**********************************************************************
 * Component                                                          *
 *    Include file labutil.h                                          *
 * Used By                                                            *
 *    PDS label software.                                             *
 * Detailed Description                                               *
 *    Defines symbols, macros, flags, typedefs, and structures        *
 *    used by PDS label software.                                     *
 * Author and Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 * Version and Date                                                   *
 *    1.1  December 4, 1991                                           *
 * Change History                                                     *
 *    KLM   06-12-91   Low level utility routines from label.c        *
 *    DPB   12-04-91   Added lu_keyword_value prototype.              *
 **********************************************************************/ 


/*---------------------------------------------------------------------------*/
/*                         LABEL Define Statements                           */
/*---------------------------------------------------------------------------*/

#define  MAX_CLASS   3
#define  MAX_IGNORE  2

/*---------------------------------------------------------------------------*/
/*                        LABEL Function Prototypes                          */
/*---------------------------------------------------------------------------*/

#ifndef SUN_UNIX

AGGREGATE lu_append_object (AGGREGATE parent_object_ptr,
                             char *new_object_class);

PARAMETER lu_append_parameter (AGGREGATE object_ptr,
                                char *parameter_name,
                                char *parameter_value);

VALUE lu_append_value (PARAMETER parameter_ptr,
                        char *parameter_value);
                                              
char *lu_fetch_value (VALUE value_ptr,
                       LOGICAL use_quotes);

STRING_LIST *lu_fetch_all_values (PARAMETER parameter_ptr,
                                   LOGICAL group_together,
                                   LOGICAL use_quotes);

int lu_find_object_level (AGGREGATE object_ptr);

char *lu_format_date (VALUE_DATA *item_ptr); 

char *lu_format_date_time (VALUE_DATA *item_ptr);

char *lu_format_integer (VALUE_DATA *item_ptr);

char *lu_format_real (VALUE_DATA *item_ptr);

char *lu_format_string (VALUE_DATA *item_ptr,
                         LOGICAL use_quotes);

char *lu_format_symbol (VALUE_DATA *item_ptr,
                         LOGICAL use_quotes);

char *lu_format_time (VALUE_DATA *item_ptr);

char *lu_format_units (struct ODLUnits *ODL_units);

char *lu_keyword_value (AGGREGATE object_ptr,
                          char *keyword_name,
                          int keyword_position,
                          LOGICAL specific);

VALUE lu_new_value (PARAMETER parameter_ptr,
                     VALUE_DATA *item_ptr);

LOGICAL lu_parse_string (AGGREGATE label_ptr,
                          char *odl_string,
                          LOGICAL clear_messages,
                          LOGICAL save_messages);

PARAMETER lu_paste_parm_at_pos (AGGREGATE object_ptr,
                                PARAMETER parameter_ptr,
                                int new_position);

LOGICAL lu_write_zi1_label (AGGREGATE label_ptr,
                             char *top_ddid,
                             int new_record_type,
                             int record_length,
                             char *file_name);

LOGICAL lu_write_zi3_label (AGGREGATE label_ptr,
                             char *top_ddid,
                             int new_record_type,
                             int record_length,
                             char *file_name);

LOGICAL lu_write_zk3_label (AGGREGATE label_ptr,
                             char *top_ddid,
                             char *bottom_ddid,
                             int new_record_type,
                             int record_length,
                             char *file_name);

#else

AGGREGATE lu_append_object ();
PARAMETER lu_append_parameter ();
VALUE lu_append_value ();
STRING_LIST *lu_fetch_all_values ();
char *lu_fetch_value ();
int lu_find_object_level ();
char *lu_format_date ();
char *lu_format_date_time ();
char *lu_format_integer ();
char *lu_format_real ();
char *lu_format_string ();
char *lu_format_symbol ();
char *lu_format_time ();
char *lu_format_units ();
char *lu_keyword_value ();
VALUE lu_new_value ();
LOGICAL lu_parse_string ();
PARAMETER lu_paste_parm_at_pos ();
LOGICAL lu_write_zi1_label ();
LOGICAL lu_write_zi3_label ();
LOGICAL lu_write_zk3_label ();
#endif

