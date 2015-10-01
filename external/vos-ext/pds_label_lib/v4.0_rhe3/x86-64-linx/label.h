/**********************************************************************
 * Component                                                          *
 *    Include file labdef.h                                           *
 * Used By                                                            *
 *    PDS label software.                                             *
 * Detailed Description                                               *
 *    Defines symbols, macros, flags, typedefs, and structures        *
 *    used by PDS label software.                                     *
 * Author and Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 * Version and Date                                                   *
 *    1.5   January 24, 1992                                          *
 * Change History                                                     *
 *    DPB   04-10-91   Original code.                                 *
 *    MDD   04-25-91   Added get and format value routines            *
 *    DPB   05-30-91   Added lab_fetch_all_values                     *
 *    KLM   06-25-91   Added lab_skip_sfdus                           *
 *    DPB   07-25-91   Added lab_match_classes.                       *
 *    MDD   01-24-92   Added lab_exit and lab_setup macros            *
 **********************************************************************/ 



/*---------------------------------------------------------------------------*/
/*                         LABEL Define Statements                           */
/*---------------------------------------------------------------------------*/

#define  lab_setup()            fio_setup()
#define  lab_exit()             fio_exit()

/*---------------------------------------------------------------------------*/
/*                        LABEL Function Prototypes                          */
/*---------------------------------------------------------------------------*/

#ifndef SUN_UNIX
                                              
AGGREGATE lab_add_object (AGGREGATE label_ptr,
                          char *parent_object_class,
                          char *parent_object_name,
                          int parent_object_position,
                          char *new_object_class,
                          int *status);

PARAMETER lab_add_parameter (AGGREGATE label_ptr,
                             char *object_class,
                             char *object_name,
                             int object_position,
                             char *parameter_name,
                             char *parameter_value,
                             int *status);

VALUE lab_add_value (AGGREGATE label_ptr,
                     char *object_class,
                     char *object_name,
                     int object_position,
                     char *parameter_name,
                     int parameter_position,
                     char *parameter_value,
                     int *status);

LOGICAL lab_change_object_class (AGGREGATE label_ptr,
                                 char *object_class,
                                 char *object_name,
                                 int object_position,
                                 char *new_object_class,
                                 int *status);

LOGICAL lab_change_parameter_name (AGGREGATE label_ptr,
                                   char *object_class,
                                   char *object_name,
                                   int object_position,
                                   char *parameter_name,
                                   int parameter_position,
                                   char *new_parameter_name,
                                   int *status);

VALUE lab_change_value (AGGREGATE label_ptr,
                        char *object_class,
                        char *object_name,
                        int object_position,
                        char *parameter_name,
                        int parameter_position,
                        char *parameter_value,
                        int *status);

void lab_clear_messages ();

AGGREGATE lab_find_object (AGGREGATE label_ptr,
                           char *object_class,
                           char *object_name,
                           int object_position,
                           int *status);

PARAMETER lab_find_parameter (AGGREGATE label_ptr,
                              char *object_class,
                              char *object_name,
                              int object_position,
                              char *parameter_name,
                              int parameter_position,
                              int *status);

STRING_LIST *lab_get_all_values (AGGREGATE label_ptr,
                                char *object_class,
                                char *object_name,
                                int object_position,
                                char *parameter_name,
                                int parameter_position,
                                LOGICAL use_quotes,
                                int *label_status);

char *lab_get_value (AGGREGATE label_ptr,
                     char *object_class,
                     char *object_name,
                     int object_position,
                     char *parameter_name,
                     int parameter_position,
                     int value_position,
                     LOGICAL use_quotes,
                     int *label_status);

LOGICAL lab_match_classes (char *primary_object_class,
                           char *compared_object_class);

PARAMETER lab_move_parameter (AGGREGATE label_ptr,
                               char *object_class,
                               char *object_name,
                               int object_position,
                               char *parameter_name,
                               int parameter_position,
                               int new_position,
                               int *label_status);

void lab_print_messages ();

LOGICAL lab_print_pds_label (AGGREGATE label_ptr);

AGGREGATE lab_read_label_or_template (char *input_fname);

AGGREGATE lab_remove_label_or_template (AGGREGATE label_ptr);

AGGREGATE lab_remove_object (AGGREGATE label_ptr,
                             char *object_class,
                             char *object_name,
                             int object_position,
                             int *status);

AGGREGATE lab_remove_parameter (AGGREGATE label_ptr,
                                char *object_class,
                                char *object_name,
                                int object_position,
                                char *parameter_name,
                                int parameter_position,
                                int *status);

FILE *lab_skip_sfdus (char *input_ptr,
                      int *line_offset);

LOGICAL lab_write_label_or_template (AGGREGATE label_ptr,
                                     int new_record_type,
                                     int record_length,
                                     char *file_name);

LOGICAL lab_write_product_label (AGGREGATE label_ptr,
                                 char *top_ddid,
                                 char *bottom_ddid,
                                 int version_id,
                                 char *label_type,
                                 int new_record_type,
                                 int record_length,
                                 char *file_name);



#else
 
AGGREGATE lab_add_object ();
PARAMETER lab_add_parameter ();
VALUE lab_add_value ();
LOGICAL lab_change_object_class ();
LOGICAL lab_change_parameter_name ();
VALUE lab_change_value ();
void lab_clear_messages ();
AGGREGATE lab_find_object ();
PARAMETER lab_find_parameter ();
STRING_LIST *lab_get_all_values ();
char *lab_get_value ();
PARAMETER lab_move_parameter ();
LOGICAL lab_match_classes ();
void lab_print_messages ();
LOGICAL lab_print_pds_label ();
AGGREGATE lab_read_label_or_template ();
AGGREGATE lab_remove_label_or_template ();
AGGREGATE lab_remove_object ();
AGGREGATE lab_remove_parameter ();
FILE *lab_skip_sfdus ();
LOGICAL lab_write_label_or_template ();
LOGICAL lab_write_product_label ();

#endif
