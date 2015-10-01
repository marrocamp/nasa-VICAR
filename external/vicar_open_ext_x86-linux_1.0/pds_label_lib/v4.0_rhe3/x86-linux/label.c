/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Component                                                           * 
 *    Library label.c                                                  *
 * Abstract                                                            *
 *    Label utility routines.                                          *
 * Detailed_Description                                                *
 *    The lablib is a library of subroutines used by PDS software      *
 *    to perform a variety of operations on PDS labels.                *
 * Internal_References                                                 *
 *    lab_add_object                                                   *
 *    lab_add_parameter                                                *
 *    lab_add_value                                                    *
 *    lab_change_object_class                                          *
 *    lab_change_parameter_name                                        *
 *    lab_change_value                                                 *
 *    lab_clear_messages                                               *
 *    lab_find_object                                                  *
 *    lab_find_parameter                                               *
 *    lab_get_all_values                                               *
 *    lab_get_object_class                                             *
 *    lab_get_value                                                    *
 *    lab_match_classes                                                *
 *    lab_move_parameter                                               *
 *    lab_print_messages                                               *
 *    lab_print_pds_label                                              *
 *    lab_read_label_or_template                                       *
 *    lab_remove_label_or_template                                     *
 *    lab_remove_object                                                *
 *    lab_remove_parameter                                             *
 *    lab_skip_sfdus                                                   *
 *    lab_write_label_or_template                                      *
 *    lab_write_product_label                                          *
 * Authors_and_Institutions                                            *
 *    Herbert C. Gamble / J.P.L.                                       *
 *    David P. Bernath / J.P.L.                                        *
 *    Marti D. Demore / J.P.L.                                         *
 * Version and Date                                                    *
 *    5.2   September 27, 1991                                         *
 * Change History                                                      *
 *    HCG   03-08-91   Library created.                                *
 *    DPB   04-10-91   Routines restructured and a whole slew of new   *
 *                     ones added.                                     *
 *    MDD   04-29-91   Added get and format value routines             *
 *    MDD   05-31-91   Added new lab_write routines                    *
 *    DPB   05-30-91   Added lab_fetch_all_values                      *
 *    KLM   06-11-91   Broke up the label.c file into higer level label*
 *                     routines and lower level label utility routines.*
 *    KLM   06-25-91   Added lab_skip_sfdus                            *
 *    DPB   07-25-91   Added lab_match_classes and pds_generic_class.  *
 *    KLM   09-12-91   Made some minor changes to the headers of a few *
 *                     routines. The errors were found while parsing   *
 *                     the files.                                      *
 *    DPB   09-27-91   Removed lab_get_object_class.                   *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "pdsdef.h"
#include "label.h"
#include "labutil.h"
#include "errordef.h"
#include "fiodef.h"
#include "utildef.h"

extern AGGREGATE ODLroot_node;
extern ERROR_LIST *pds_message_list;
extern LOGICAL pds_verbose;
extern LOGICAL pds_generic_class;



/**********************************************************************
 *$Component                                                          *
 *    AGGREGATE lab_add_object (label_ptr, object_class, object_name, *
 *                              object_position, new_object_class,    *
 *                              label_status)                         *
 *$Abstract                                                           *
 *    Adds a new object to a PDS Label.                               *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    ADD                                                             *
 *    OBJECT                                                          *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    label_ptr:                                                      *
 *        The label_ptr variable is a pointer to the root of an       *
 *        ODL tree.  Usually it points to the actual "ROOT" object,   *
 *        but it may point to any other object in the tree.  If it    *
 *        does point to something other than the "ROOT" object, then  *
 *        this object will be treated as the root of a sub-tree, and  *
 *        processing will not be allowed to move above the level of   *
 *        this object.                                                *
 *    object_class:                                                   *
 *        The object_class variable is a character string             *
 *        which contains the class of an object in a PDS label        *
 *        (e.g., "OBJECT = IMAGE" implies that "IMAGE" is the class   *
 *        of the object).                                             *
 *    object_name:                                                    *
 *        The object_name variable is a character string              *
 *        which contains the name of an object in a PDS label.        *
 *        This is assumed to be the value of the "NAME" parameter     *
 *        in the object (e.g., "NAME = VOYAGER" implies that          *
 *        "VOYAGER" is the name of the object).                       *
 *    object_position:                                                *
 *        The object_position variable is an integer which            *
 *        represents the relative position of an object in a PDS      *
 *        label.  If this variable is used in conjunction with either *
 *        the object_class or object_name variables, then it          *
 *        represents a particular occurrence of that thing in the     *
 *        label (e.g., if object_class is "TABLE" and object_position *
 *        is 3, then this represents the third "TABLE" object in the  *
 *        label).  On the other hand, if this variable is used by     *
 *        itself, it represents the absolute position of the object,  *
 *        starting from the "ROOT" object (position = 1).  The        *
 *        sequence follows the structure of the label as it looks     *
 *        in a flat file.                                             *
 *    new_object_class:                                               *
 *        The new_object_class variable is a character string         *
 *        which contains the class of an object to be added to        *
 *        a PDS label (e.g., "OBJECT = IMAGE" implies that "IMAGE"    *
 *        is the class of the object).                                *
 *$Outputs                                                            *
 *    label_status:                                                   *
 *        The label_status variable is an integer which is used to    *
 *        indicate the success or failure of a PDS label routine,     *
 *        and whether or not the object or parameter specifications   *
 *        passed in were ambiguous.  Valid values are:  PDS_SUCCESS,  *
 *        PDS_ERROR, PDS_MULTIPLE_OBJECTS, and PDS_MULTIPLE_PARMS.    *
 *$Returns                                                            *
 *    new_object_ptr:                                                 *
 *        The new_object_ptr variable is a pointer to the structure   *
 *        used to represent a new object in a PDS label.              *
 *$Detailed_Description                                               *
 *    The lab_add_object routine adds a new object to a PDS label.    *
 *    It searches for the parent of the new object, mallocs storage   *
 *    for the new object, then appends the new object onto the list   *
 *    of children of the parent.  The parent object may by specified  *
 *    by class, name, or position, or by any combination of these.    *
 *    The search for the parent begins at the object pointed to by    *
 *    the label_ptr variable.  This is usually the "ROOT" object, but *
 *    may be any other object in the tree.  Please note that it is    *
 *    not necessary to specify the object's name, class, and          *
 *    position at the same time.  In fact, if you specify only one    *
 *    of them and pass in zero for the others, the routine will work  *
 *    just fine.                                                      *
 *$Error_Handling                                                     *
 *    1) If the parent object cannot be found, or memory cannot be    *
 *       allocated for the new object, then the label_status          *
 *       is set to PDS_ERROR and a NULL value is returned.            *
 *    2) If more than one object is found that matches the            *
 *       specifications passed in for the parent object, then the     *
 *       label_status is set to PDS_MULTIPLE_OBJECTS and a NULL       *
 *       value is returned.                                           *
 *    3) If everything went according to plan, then the label_status  *
 *       is set to PDS_SUCCESS and a pointer to the new object is     *
 *       returned.                                                    *
 *$Side_Effects                                                       *
 *    This routine causes memory to be allocated which must be freed  *
 *    by the calling routine.                                         *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Herbert C. Gamble / J.P.L.                                      *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.2   June 17, 1991                                             *
 *$Change_History                                                     *
 *    HCG   03-08-91   Original Code.                                 *
 *    DPB   04-10-91   Restructured routine.                          *
 *    DPB   04-26-91   Added code to strip blanks from inputs.        *
 *    KLM   06-17-91   Changed call of lab_append_object to           *
 *                     lu_append_object.                              *
 **********************************************************************/

AGGREGATE lab_add_object (label_ptr, object_class, object_name, 
                          object_position, new_object_class, label_status)

AGGREGATE label_ptr;
char *object_class;
char *object_name;
int object_position;
char *new_object_class;
int *label_status;

{
    AGGREGATE parent_object_ptr = {label_ptr};
    AGGREGATE new_object_ptr = {NULL};

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Initialize label_status                                             **/
    /*-----------------------------------------------------------------------*/
 
    *label_status = PDS_ERROR;

    /*-----------------------------------------------------------------------*/
    /** Remove leading and trailing blanks from character inputs.           **/
    /*-----------------------------------------------------------------------*/

    util_strip_lead_and_trail (object_class, ' ');
    util_strip_lead_and_trail (object_name, ' ');
    util_strip_lead_and_trail (new_object_class, ' ');

    /*-----------------------------------------------------------------------*/
    /** Find the parent object and retrieve a pointer to it                 **/
    /*-----------------------------------------------------------------------*/

    parent_object_ptr = lab_find_object (label_ptr, object_class, 
                                         object_name, object_position, 
                                         label_status);

    /*-----------------------------------------------------------------------*/
    /** IF the parent object was found THEN                                 **/
    /**     Append a new child object onto the parent's list of children    **/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    if ((parent_object_ptr != NULL) && (*label_status == PDS_SUCCESS))
        new_object_ptr = lu_append_object(parent_object_ptr,new_object_class);

    /*-----------------------------------------------------------------------*/
    /** RETURN a pointer to the new object structure                        **/
    /*-----------------------------------------------------------------------*/

    return (new_object_ptr);

/** END **/

}  /*  "lab_add_object"  */ 


/**********************************************************************
 *$Component                                                          *
 *    PARAMETER lab_add_parameter (label_ptr, object_class,           *
 *                                 object_name, object_position,      *
 *                                 parameter_name, parameter_value,   *
 *                                 label_status)                      *
 *$Abstract                                                           *
 *    Adds a new parameter to a PDS Label.                            *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    ADD                                                             *
 *    PARAMETER                                                       *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    label_ptr:                                                      *
 *        The label_ptr variable is a pointer to the root of an       *
 *        ODL tree.  Usually it points to the actual "ROOT" object,   *
 *        but it may point to any other object in the tree.  If it    *
 *        does point to something other than the "ROOT" object, then  *
 *        this object will be treated as the root of a sub-tree, and  *
 *        processing will not be allowed to move above the level of   *
 *        this object.                                                *
 *    object_class:                                                   *
 *        The object_class variable is a character string             *
 *        which contains the class of an object in a PDS label        *
 *        (e.g., "OBJECT = IMAGE" implies that "IMAGE" is the class   *
 *        of the object).                                             *
 *    object_name:                                                    *
 *        The object_name variable is a character string              *
 *        which contains the name of an object in a PDS label.        *
 *        This is assumed to be the value of the "NAME" parameter     *
 *        in the object (e.g., "NAME = VOYAGER" implies that          *
 *        "VOYAGER" is the name of the object).                       *
 *    object_position:                                                *
 *        The object_position variable is an integer which            *
 *        represents the relative position of an object in a PDS      *
 *        label.  If this variable is used in conjunction with either *
 *        the object_class or object_name variables, then it          *
 *        represents a particular occurrence of that thing in the     *
 *        label (e.g., if object_class is "TABLE" and object_position *
 *        is 3, then this represents the third "TABLE" object in the  *
 *        label).  On the other hand, if this variable is used by     *
 *        itself, it represents the absolute position of the object,  *
 *        starting from the "ROOT" object (position = 1).  The        *
 *        sequence follows the structure of the label as it looks     *
 *        in a flat file.                                             *
 *    parameter_name:                                                 *
 *        The parameter_name variable is a character string           *
 *        which contains the name of a parameter in a PDS label       *
 *        (e.g., the line "SCID = VG1" implies that "SCID" is the     *
 *        parameter name).                                            *
 *    parameter_value:                                                *
 *        The parameter_value variable is a character string          *
 *        which contains the value of a parameter in a PDS label      *
 *        (e.g., the line "SCID = VG1" implies that "VG1" is the      *
 *        value of the "SCID" parameter).                             *
 *$Outputs                                                            *
 *    label_status:                                                   *
 *        The label_status variable is an integer which is used to    *
 *        indicate the success or failure of a PDS label routine,     *
 *        and whether or not the object or parameter specifications   *
 *        passed in were ambiguous.  Valid values are:  PDS_SUCCESS,  *
 *        PDS_ERROR, PDS_MULTIPLE_OBJECTS, and PDS_MULTIPLE_PARMS.    *
 *$Returns                                                            *
 *    new_parameter_ptr:                                              *
 *        The new_parameter_ptr variable is a pointer to the          *
 *        structure used to represent a new parameter in a PDS label. *
 *$Detailed_Description                                               *
 *    The lab_add_parameter routine adds a new parameter to an        *
 *    object in a PDS label.  It searches for the object, mallocs     *
 *    storage for the new parameter and its value, and appends the    *
 *    new parameter onto the list of parameters attached to that      *
 *    object.  The parent object may by specified by class, name,     *
 *    or position, or by any combination of these.  The search for    *
 *    the parent begins at the object pointed to by the label_ptr     *
 *    variable.  This is usually the "ROOT" object, but may be any    *
 *    other object in the tree.  Please note that it is not necessary *
 *    to specify the object's name, class, and position at the same   *
 *    time.  In fact, if you specify only one of them and pass in     *
 *    zero for the others, the routine will work just fine.           *
 *$Error_Handling                                                     *
 *    1) If the object cannot be found, or memory cannot be           *
 *       allocated for the new parameter, then the label_status       *
 *       is set to PDS_ERROR and a NULL value is returned.            *
 *    2) If more than one object is found that matches the            *
 *       specifications passed in for the object, then the            *
 *       label_status is set to PDS_MULTIPLE_OBJECTS and a NULL       *
 *       value is returned.                                           *
 *    3) If everything went according to plan, then the label_status  *
 *       is set to PDS_SUCCESS and a pointer to the new parameter is  *
 *       returned.                                                    *
 *$Side_Effects                                                       *
 *    This routine causes memory to be allocated which must be freed  *
 *    by the calling routine.                                         *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Herbert C. Gamble / J.P.L.                                      *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.2   June 17, 1991                                             *
 *$Change_History                                                     *
 *    HCG   03-08-91   Original Code.                                 *
 *    DPB   04-10-91   Restructured routine.                          *
 *    DPB   04-26-91   Added code to strip blanks from inputs.        *
 *    KLM   06-17-91   Changed call from lab_append_parameter to      *
 *                     lu_append_parameter.                           * 
 **********************************************************************/

PARAMETER lab_add_parameter (label_ptr, object_class, object_name,        
                             object_position, parameter_name, 
                             parameter_value, label_status)

AGGREGATE label_ptr;
char *object_class;
char *object_name;
int object_position;
char *parameter_name;
char *parameter_value;
int *label_status;

{
    AGGREGATE object_ptr = {NULL};
    PARAMETER parameter_ptr = {NULL};

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Initialize the label_status variable                                **/
    /*-----------------------------------------------------------------------*/

    *label_status = PDS_ERROR;

    /*-----------------------------------------------------------------------*/
    /** IF the parameter name passed in is not NULL THEN                    **/
    /*-----------------------------------------------------------------------*/

    if (parameter_name != NULL) 
    {
        /*-------------------------------------------------------------------*/
        /** Remove leading and trailing blanks from character inputs.       **/
        /*-------------------------------------------------------------------*/

        util_strip_lead_and_trail (object_class, ' ');
        util_strip_lead_and_trail (object_name, ' ');
        util_strip_lead_and_trail (parameter_name, ' ');

        /*-------------------------------------------------------------------*/
        /** Try to find the object which contains the parameter             **/
        /*-------------------------------------------------------------------*/

        object_ptr = lab_find_object (label_ptr, object_class, 
                                      object_name, object_position, label_status
);

        /*-------------------------------------------------------------------*/
        /** IF the object was found THEN                                    **/
        /**     Append the new parameter onto the object's list of parms    **/
        /** ENDIF                                                           **/
        /*-------------------------------------------------------------------*/

        if ((object_ptr != NULL) && (*label_status == PDS_SUCCESS))
        {
            parameter_ptr = lu_append_parameter (object_ptr, parameter_name, 
                                                  parameter_value);

        }  /*  End:  "if ((object_ptr != NULL) && ..."  */

    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if (parameter_name != NULL) ..."  */

    /*-----------------------------------------------------------------------*/
    /** RETURN a pointer to the new parameter structure                     **/
    /*-----------------------------------------------------------------------*/

    return (parameter_ptr);

/** END **/

}  /*  "lab_add_parameter"  */



/**********************************************************************
 *$Component                                                          *
 *    VALUE lab_add_value (label_ptr, object_class, object_name,      *
 *                         object_position, parameter_name,           *
 *                         parameter_position, parameter_value,       *
 *                         label_status)                              *
 *$Abstract                                                           *
 *    Adds a value to a parameter in a PDS Label.                     *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    ADD                                                             *
 *    VALUE                                                           *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    label_ptr:                                                      *
 *        The label_ptr variable is a pointer to the root of an       *
 *        ODL tree.  Usually it points to the actual "ROOT" object,   *
 *        but it may point to any other object in the tree.  If it    *
 *        does point to something other than the "ROOT" object, then  *
 *        this object will be treated as the root of a sub-tree, and  *
 *        processing will not be allowed to move above the level of   *
 *        this object.                                                *
 *    object_class:                                                   *
 *        The object_class variable is a character string             *
 *        which contains the class of an object in a PDS label        *
 *        (e.g., "OBJECT = IMAGE" implies that "IMAGE" is the class   *
 *        of the object).                                             *
 *    object_name:                                                    *
 *        The object_name variable is a character string              *
 *        which contains the name of an object in a PDS label.        *
 *        This is assumed to be the value of the "NAME" parameter     *
 *        in the object (e.g., "NAME = VOYAGER" implies that          *
 *        "VOYAGER" is the name of the object).                       *
 *    object_position:                                                *
 *        The object_position variable is an integer which            *
 *        represents the relative position of an object in a PDS      *
 *        label.  If this variable is used in conjunction with either *
 *        the object_class or object_name variables, then it          *
 *        represents a particular occurrence of that thing in the     *
 *        label (e.g., if object_class is "TABLE" and object_position *
 *        is 3, then this represents the third "TABLE" object in the  *
 *        label).  On the other hand, if this variable is used by     *
 *        itself, it represents the absolute position of the object,  *
 *        starting from the "ROOT" object (position = 1).  The        *
 *        sequence follows the structure of the label as it looks     *
 *        in a flat file.                                             *
 *    parameter_name:                                                 *
 *        The parameter_name variable is a character string           *
 *        which contains the name of a parameter in a PDS label       *
 *        (e.g., the line "SCID = VG1" implies that "SCID" is the     *
 *        parameter name).                                            *
 *    parameter_position:                                             *
 *        The parameter_position variable is an integer which         *
 *        represents the relative position of a parameter in an       *
 *        object in a PDS label.  If this variable is used in         *
 *        conjunction with the parameter_name variable, then it       *
 *        represents a particular occurrence of that parameter        *
 *        in the object (e.g., if parameter_name is "SCID" and        *
 *        parameter_position is 2, then this represents the second    *
 *        "SCID" parameter in the object).  On the other hand, if     *
 *        this variable is used by itself, it represents the absolute *
 *        position of the parameter within the object, starting with  *
 *        first parameter in the object (position = 1).               *
 *    parameter_value:                                                *
 *        The parameter_value variable is a character string          *
 *        which contains the value of a parameter in a PDS label      *
 *        (e.g., the line "SCID = VG1" implies that "VG1" is the      *
 *        value of the "SCID" parameter).                             *
 *$Outputs                                                            *
 *    label_status:                                                   *
 *        The label_status variable is an integer which is used to    *
 *        indicate the success or failure of a PDS label routine,     *
 *        and whether or not the object or parameter specifications   *
 *        passed in were ambiguous.  Valid values are:  PDS_SUCCESS,  *
 *        PDS_ERROR, PDS_MULTIPLE_OBJECTS, and PDS_MULTIPLE_PARMS.    *
 *$Returns                                                            *
 *    new_value_ptr:                                                  *
 *        The new_value_ptr variable is a pointer to the structure    *
 *        used to represent a new value of a parameter in a PDS label.*
 *$Detailed_Description                                               *
 *    The lab_add_value routine adds a new value to a parameter of    *
 *    an object in a PDS label.  It searches for the object,          *
 *    searches for the parameter attached to the object, then mallocs *
 *    storage for the new value and appends it onto the list of       *
 *    values attached to the parameter.  The parent object may by     *
 *    specified by class, name, or position, or by any combination    *
 *    of these, and the parameter may be specified by name or         *
 *    position, or both.   The search for the object begins at the    *
 *    object pointed to by the label_ptr variable.  This is usually   *
 *    the "ROOT" object, but may be any other object in the tree.     *
 *    Please note that it is not necessary to specify the object's    *
 *    name, class, and position at the same time.  In fact, if you    *
 *    specify only one of them and pass in zero for the others, the   *
 *    routine will work just fine.  This also applies to the          *
 *    parameter name and position.                                    *
 *                                                                    *
 *    Please note that lab_add_value has been left in this library    *
 *    only for backwards compatibility.  It is NOT functioning        *
 *    correctly.  The resulting format of the output may be incorrect.*
 *$Error_Handling                                                     *
 *    1) If the object or parameter cannot be found, or memory cannot *
 *       be allocated for the new value, then the label_status        *
 *       is set to PDS_ERROR and a NULL value is returned.            *
 *    2) If more than one object is found that matches the            *
 *       specifications passed in for the object, then the            *
 *       label_status is set to PDS_MULTIPLE_OBJECTS and a NULL       *
 *       value is returned.                                           *
 *    3) If more than one parameter is found that matches the         *
 *       specifications passed in for the parameter, then the         *
 *       label_status is set to PDS_MULTIPLE_PARMS and a NULL         *
 *       value is returned.                                           *
 *    4) If everything went according to plan, then the label_status  *
 *       is set to PDS_SUCCESS and a pointer to the new value is      *
 *       returned.                                                    *
 *$Side_Effects                                                       *
 *    This routine causes memory to be allocated which must be freed  *
 *    by the calling routine.                                         *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Herbert C. Gamble / J.P.L.                                      *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.2   June 17, 1991                                             *
 *$Change_History                                                     *
 *    HCG   03-08-91   Original Code.                                 *
 *    DPB   04-10-91   Restructured routine.                          *
 *    DPB   04-26-91   Added code to strip blanks from inputs.        *
 *    KLM   06-17-91   Changed call from lab_append_value to          *
 *                     lu_append_value.                               *
 **********************************************************************/

VALUE lab_add_value (label_ptr, object_class, object_name, 
                     object_position, parameter_name, 
                     parameter_position, parameter_value, label_status)

AGGREGATE label_ptr;
char *object_class;
char *object_name;
int object_position;
char *parameter_name;
int parameter_position;
char *parameter_value;
int *label_status;

{
    PARAMETER parameter_ptr = {NULL};
    VALUE value_ptr = {NULL};

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Initialize the label_status variable                                **/
    /*-----------------------------------------------------------------------*/

    *label_status = PDS_ERROR;

    /*-----------------------------------------------------------------------*/
    /** Remove leading and trailing blanks from character inputs.           **/
    /*-----------------------------------------------------------------------*/

    util_strip_lead_and_trail (object_class, ' ');
    util_strip_lead_and_trail (object_name, ' ');
    util_strip_lead_and_trail (parameter_name, ' ');

    /*-----------------------------------------------------------------------*/
    /** Try to find the object and parameter in the label                   **/
    /*-----------------------------------------------------------------------*/

    parameter_ptr = lab_find_parameter (label_ptr, object_class, 
                                        object_name, object_position, 
                                        parameter_name, parameter_position, 
                                        label_status);

    /*-----------------------------------------------------------------------*/
    /** IF the object and parameter were found THEN                         **/
    /**     Append the new value onto the parameter's list of values        **/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    if ((parameter_ptr != NULL) && (*label_status == PDS_SUCCESS))
        value_ptr = lu_append_value (parameter_ptr, parameter_value);

    /*-----------------------------------------------------------------------*/
    /** RETURN a pointer to the new value structure                         **/
    /*-----------------------------------------------------------------------*/

    return (value_ptr);

/** END **/

}  /*  "lab_add_value"  */

/**********************************************************************
 *$Component                                                          *
 *    LOGICAL lab_change_object_class (label_ptr, object_class,       *
 *                       object_name, object_position,                *
 *                       new_object_class, label_status)              *
 *$Abstract                                                           *
 *    Locates an object in a PDS label.                               *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    label_ptr:                                                      *
 *        The label_ptr variable is a pointer to the root of an       *
 *        ODL tree.  Usually it points to the actual "ROOT" object,   *
 *        but it may point to any other object in the tree.  If it    *
 *        does point to something other than the "ROOT" object, then  *
 *        this object will be treated as the root of a sub-tree, and  *
 *        processing will not be allowed to move above the level of   *
 *        this object.                                                *
 *    object_class:                                                   *
 *        The object_class variable is a character string             *
 *        which contains the class of an object in a PDS label        *
 *        (e.g., "OBJECT = IMAGE" implies that "IMAGE" is the class   *
 *        of the object).                                             *
 *    object_name:                                                    *
 *        The object_name variable is a character string              *
 *        which contains the name of an object in a PDS label.        *
 *        This is assumed to be the value of the "NAME" parameter     *
 *        in the object (e.g., "NAME = VOYAGER" implies that          *
 *        "VOYAGER" is the name of the object).                       *
 *    object_position:                                                *
 *        The object_position variable is an integer which            *
 *        represents the relative position of an object in a PDS      *
 *        label.  If this variable is used in conjunction with either *
 *        the object_class or object_name variables, then it          *
 *        represents a particular occurrence of that thing in the     *
 *        label (e.g., if object_class is "TABLE" and object_position *
 *        is 3, then this represents the third "TABLE" object in the  *
 *        label).  On the other hand, if this variable is used by     *
 *        itself, it represents the absolute position of the object,  *
 *        starting from the "ROOT" object (position = 1).  The        *
 *        sequence follows the structure of the label as it looks     *
 *        in a flat file.                                             *
 *    new_object_class:                                               *
 *        The new_object_class variable is a character string         *
 *        which contains the new class of an object in a PDS label    *
 *        (e.g., "OBJECT = IMAGE" implies that "IMAGE" is the class   *
 *        of the object).                                             *
 *$Outputs                                                            *
 *    label_status:                                                   *
 *        The label_status variable is an integer which is used to    *
 *        indicate the success or failure of a PDS label routine,     *
 *        and whether or not the object or parameter specifications   *
 *        passed in were ambiguous.  Valid values are:  PDS_SUCCESS,  *
 *        PDS_ERROR, PDS_MULTIPLE_OBJECTS, and PDS_MULTIPLE_PARMS.    *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure        *
 *        (a value of FALSE).                                         *
 *$Detailed_Description                                               *
 *    The lab_change_object_class routine finds an object and changes *
 *    its class. The object may by specified by class, name, or       *
 *    position, or by any combination of these. Please note that it   *
 *    is not necessary to specify the object's name, class, and       *
 *    position at the same time.  In fact, if you specify only one of *
 *    them and pass in zero for the others, the routine will work     *
 *    just fine.                                                      *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Kristy L. Marski / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   October 21, 1991                                          *
 *$Change_History                                                     *
 *    KLM   05-16-91   Original Code.                                 *
 *    MDD   10-21-91   FIxed malloc, free, and sys_exit_system calls  *
 **********************************************************************/

LOGICAL lab_change_object_class (label_ptr, object_class, object_name, 
                                 object_position, new_object_class, 
                                 label_status)

AGGREGATE label_ptr;
char *object_class;
char *object_name;
int object_position;
char *new_object_class;
int *label_status;

{            

    AGGREGATE object_ptr = {NULL};
    LOGICAL success = {TRUE};

/** BEGIN **/           
   /*----------------------------------------------------------------------*/
   /** strip blanks from string arguments                                 **/
   /** Try to find the object                                             **/  
   /*----------------------------------------------------------------------*/

   util_strip_lead_and_trail (new_object_class, ' ');
   object_ptr = lab_find_object (label_ptr, object_class, object_name, 
                                 object_position, label_status);

   /*----------------------------------------------------------------------*/
   /** IF the object was found and the pointer isn't NULL THEN            **/  
   /**    free the current object name                                    **/
   /**    allocate memory for the new object name                         **/
   /**    copy the new object name to the old name location               **/
   /*----------------------------------------------------------------------*/

   if (*label_status == PDS_SUCCESS && object_ptr != NULL)
   {
      Lemme_Go(object_ptr -> name);
      Malloc_String(object_ptr -> name,String_Size(new_object_class));
      strcpy (object_ptr -> name, new_object_class);
   }

   /*----------------------------------------------------------------------*/
   /** ELSE IF the there was an error locating the object THEN            **/  
   /**    set the success/failure to FALSE                                **/
   /** ENDIF                                                              **/
   /*----------------------------------------------------------------------*/ 

   else 
   {
        success = FALSE;
   }

   /*----------------------------------------------------------------------*/
   /** return success/failure                                             **/
   /*----------------------------------------------------------------------*/

   return (success);

/** END **/

}  /*  "lab_change_object_class"  */                          


/**********************************************************************
 *$Component                                                          *
 *    LOGICAL lab_change_parameter_name (label_ptr, object_class,     *
 *                                object_name, object_position,       *
 *                                parameter_name, parameter_position, *
 *                                new_parameter_name, label_status)   *
 *$Abstract                                                           *
 *    Changes a parameter in a PDS label.                             *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    NON_UI_COMMON                                                   * 
 *$Inputs                                                             *
 *    label_ptr:                                                      *
 *        The label_ptr variable is a pointer to the root of an       *
 *        ODL tree.  Usually it points to the actual "ROOT" object,   *
 *        but it may point to any other object in the tree.  If it    *
 *        does point to something other than the "ROOT" object, then  *
 *        this object will be treated as the root of a sub-tree, and  *
 *        processing will not be allowed to move above the level of   *
 *        this object.                                                *
 *    object_class:                                                   *
 *        The object_class variable is a character string             *
 *        which contains the class of an object in a PDS label        *
 *        (e.g., "OBJECT = IMAGE" implies that "IMAGE" is the class   *
 *        of the object).                                             *
 *    object_name:                                                    *
 *        The object_name variable is a character string              *
 *        which contains the name of an object in a PDS label.        *
 *        This is assumed to be the value of the "NAME" parameter     *
 *        in the object (e.g., "NAME = VOYAGER" implies that          *
 *        "VOYAGER" is the name of the object).                       *
 *    object_position:                                                *
 *        The object_position variable is an integer which            *
 *        represents the relative position of an object in a PDS      *
 *        label.  If this variable is used in conjunction with either *
 *        the object_class or object_name variables, then it          *
 *        represents a particular occurrence of that thing in the     *
 *        label (e.g., if object_class is "TABLE" and object_position *
 *        is 3, then this represents the third "TABLE" object in the  *
 *        label).  On the other hand, if this variable is used by     *
 *        itself, it represents the absolute position of the object,  *
 *        starting from the "ROOT" object (position = 1).  The        *
 *        sequence follows the structure of the label as it looks     *
 *        in a flat file.                                             *
 *    parameter_name:                                                 *
 *        The parameter_name variable is a character string           *
 *        which contains the name of a parameter in a PDS label       *
 *        (e.g., the line "SCID = VG1" implies that "SCID" is the     *
 *        parameter name).                                            *
 *    parameter_position:                                             *
 *        The parameter_position variable is an integer which         *
 *        represents the relative position of a parameter in an       *
 *        object in a PDS label.  If this variable is used in         *
 *        conjunction with the parameter_name variable, then it       *
 *        represents a particular occurrence of that parameter        *
 *        in the object (e.g., if parameter_name is "SCID" and        *
 *        parameter_position is 2, then this represents the second    *
 *        "SCID" parameter in the object).  On the other hand, if     *
 *        this variable is used by itself, it represents the absolute *
 *        position of the parameter within the object, starting with  *
 *        first parameter in the object (position = 1).               *
 *    new_parameter_name:                                             *
 *        The new_parameter_name variable is a character string       *
 *        which contains the new name for a parameter in a PDS label  *
 *        (e.g., the line "SCID = VG1" implies that "SCID" is the     *
 *        parameter name).                                            *
 *$Outputs                                                            *
 *    label_status:                                                   *
 *        The label_status variable is an integer which is used to    *
 *        indicate the success or failure of a PDS label routine,     *
 *        and whether or not the object or parameter specifications   *
 *        passed in were ambiguous.  Valid values are:  PDS_SUCCESS,  *
 *        PDS_ERROR, PDS_MULTIPLE_OBJECTS, and PDS_MULTIPLE_PARMS.    *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure        *
 *        (a value of FALSE).                                         *
 *$Detailed_Description                                               *
 *    The lab_change_parameter_name routine finds a parameter and     *
 *    changes its name. The object may by specified by class, name, or*
 *    position, or by any combination of these, and the parameter may *
 *    be specified by name or position or both. Please note that it   *
 *    is not necessary to specify the object's name, class, and       *
 *    position at the same time.  In fact, if you specify only one of *
 *    them and pass in zero for the others, the routine will work     *
 *    just fine.  This also applies to the parameter name and         *
 *    position.                                                       *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Kristy L. Marski / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   October 21, 1991                                          *
 *$Change_History                                                     *
 *    KLM   05-17-91   Original Code.                                 *
 *    MDD   10-17-91   Fixed malloc, free, and sys_exit_system calls  *
 **********************************************************************/

LOGICAL lab_change_parameter_name (label_ptr, object_class, object_name, 
                                   object_position, parameter_name, 
                                   parameter_position, new_parameter_name,
                                   label_status)

AGGREGATE label_ptr;
char *object_class;
char *object_name;
int object_position;
char *parameter_name;
int parameter_position;
char *new_parameter_name;
int *label_status;

{
    PARAMETER parameter_ptr = {NULL};       
    LOGICAL success = {TRUE};

/** BEGIN **/
   /*----------------------------------------------------------------------*/
   /** strip blanks from string arguments                                 **/  
   /** Try to find the parameter                                          **/  
   /*----------------------------------------------------------------------*/

   util_strip_lead_and_trail (new_parameter_name, ' ');
   parameter_ptr = lab_find_parameter (label_ptr, object_class, 
                      object_name, object_position, parameter_name, 
                      parameter_position, label_status);

   /*----------------------------------------------------------------------*/
   /** IF the parameter was found and the pointer isn't NULL THEN         **/  
   /**    free the current parameter name                                 **/
   /**    allocate memory for the new parameter name                      **/
   /**    copy the new parameter name to the old name location            **/
   /*----------------------------------------------------------------------*/
 
   if (*label_status == PDS_SUCCESS && parameter_ptr != NULL)
   {
      Lemme_Go(parameter_ptr -> name);
      Malloc_String(parameter_ptr -> name, String_Size(new_parameter_name));
      strcpy (parameter_ptr -> name, new_parameter_name);
   }
   /*----------------------------------------------------------------------*/
   /** ELSE IF the there was an error locating the object THEN            **/  
   /**    set the success/failure to FALSE                                **/
   /** ENDIF                                                              **/
   /*----------------------------------------------------------------------*/
         
   else
   {
        success = FALSE;
   }

   /*----------------------------------------------------------------------*/
   /** return success/failure                                             **/
   /*----------------------------------------------------------------------*/

   return (success);

/** END **/

}  /*  "lab_change_parameter_name"  */                          


/**********************************************************************
 *$Component                                                          *
 *    VALUE lab_change_value (label_ptr, object_class, object_name,   *
 *                            object_position, parameter_name,        *
 *                            parameter_position, parameter_value,    *
 *                            label_status)                           *
 *$Abstract                                                           *
 *    Changes the value of a parameter in a PDS label.                *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    CHANGE                                                          *
 *    PARAMETER                                                       *
 *    VALUE                                                           *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    label_ptr:                                                      *
 *        The label_ptr variable is a pointer to the root of an       *
 *        ODL tree.  Usually it points to the actual "ROOT" object,   *
 *        but it may point to any other object in the tree.  If it    *
 *        does point to something other than the "ROOT" object, then  *
 *        this object will be treated as the root of a sub-tree, and  *
 *        processing will not be allowed to move above the level of   *
 *        this object.                                                *
 *    object_class:                                                   *
 *        The object_class variable is a character string             *
 *        which contains the class of an object in a PDS label        *
 *        (e.g., "OBJECT = IMAGE" implies that "IMAGE" is the class   *
 *        of the object).                                             *
 *    object_name:                                                    *
 *        The object_name variable is a character string              *
 *        which contains the name of an object in a PDS label.        *
 *        This is assumed to be the value of the "NAME" parameter     *
 *        in the object (e.g., "NAME = VOYAGER" implies that          *
 *        "VOYAGER" is the name of the object).                       *
 *    object_position:                                                *
 *        The object_position variable is an integer which            *
 *        represents the relative position of an object in a PDS      *
 *        label.  If this variable is used in conjunction with either *
 *        the object_class or object_name variables, then it          *
 *        represents a particular occurrence of that thing in the     *
 *        label (e.g., if object_class is "TABLE" and object_position *
 *        is 3, then this represents the third "TABLE" object in the  *
 *        label).  On the other hand, if this variable is used by     *
 *        itself, it represents the absolute position of the object,  *
 *        starting from the "ROOT" object (position = 1).  The        *
 *        sequence follows the structure of the label as it looks     *
 *        in a flat file.                                             *
 *    parameter_name:                                                 *
 *        The parameter_name variable is a character string           *
 *        which contains the name of a parameter in a PDS label       *
 *        (e.g., the line "SCID = VG1" implies that "SCID" is the     *
 *        parameter name).                                            *
 *    parameter_position:                                             *
 *        The parameter_position variable is an integer which         *
 *        represents the relative position of a parameter in an       *
 *        object in a PDS label.  If this variable is used in         *
 *        conjunction with the parameter_name variable, then it       *
 *        represents a particular occurrence of that parameter        *
 *        in the object (e.g., if parameter_name is "SCID" and        *
 *        parameter_position is 2, then this represents the second    *
 *        "SCID" parameter in the object).  On the other hand, if     *
 *        this variable is used by itself, it represents the absolute *
 *        position of the parameter within the object, starting with  *
 *        first parameter in the object (position = 1).               *
 *    parameter_value:                                                *
 *        The parameter_value variable is a character string          *
 *        which contains the value of a parameter in a PDS label      *
 *        (e.g., the line "SCID = VG1" implies that "VG1" is the      *
 *        value of the "SCID" parameter).                             *
 *$Outputs                                                            *
 *    label_status:                                                   *
 *        The label_status variable is an integer which is used to    *
 *        indicate the success or failure of a PDS label routine,     *
 *        and whether or not the object or parameter specifications   *
 *        passed in were ambiguous.  Valid values are:  PDS_SUCCESS,  *
 *        PDS_ERROR, PDS_MULTIPLE_OBJECTS, and PDS_MULTIPLE_PARMS.    *
 *$Returns                                                            *
 *    new_value_ptr:                                                  *
 *        The new_value_ptr variable is a pointer to the structure    *
 *        used to represent a new value of a parameter in a PDS label.*
 *$Detailed_Description                                               *
 *    The lab_change_value routine removes a list of values from      *
 *    a parameter structure in a PDS label and replaces it with       *
 *    the new value passed in.  It searches for the object,           *
 *    searches for the parameter attached to the object, then         *
 *    verifies whether or not the value passed in is valid.  If it    *
 *    is, then the old value is removed, storage for the new value    *
 *    is malloc'd, and the new value is added to the parameter.       *
 *    The object may by specified by class, name, or position, or     *
 *    by any combination of these, and the parameter may be           *
 *    specified by name or position, or both.   The search for the    *
 *    object begins at the object pointed to by the label_ptr         *
 *    variable.  This is usually the "ROOT" object, but may be any    *
 *    other object in the tree.  Please note that it is not necessary *
 *    to specify the object's name, class, and position at the same   *
 *    time.  In fact, if you specify only one of them and pass in     *
 *    zero for the others, the routine will work just fine.  This     *
 *    also applies to the parameter name and position.                *
 *$Error_Handling                                                     *
 *    1) If the object or parameter cannot be found, or memory cannot *
 *       be allocated for the new value, or the new value is invalid, *
 *       then the label_status is set to PDS_ERROR and a NULL value   *
 *       is returned.                                                 *
 *    2) If more than one object is found that matches the            *
 *       specifications passed in for the object, then the            *
 *       label_status is set to PDS_MULTIPLE_OBJECTS and a NULL       *
 *       value is returned.                                           *
 *    3) If more than one parameter is found that matches the         *
 *       specifications passed in for the parameter, then the         *
 *       label_status is set to PDS_MULTIPLE_PARMS and a NULL         *
 *       value is returned.                                           *
 *    4) If everything went according to plan, then the label_status  *
 *       is set to PDS_SUCCESS and a pointer to the new value is      *
 *       returned.                                                    *
 *$Side_Effects                                                       *
 *    This routine causes memory to be allocated which must be freed  *
 *    by the calling routine.                                         *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Herbert C. Gamble / J.P.L.                                      *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    2.1   January 13, 1992                                          *
 *$Change_History                                                     *
 *    HCG   03-08-91   Original Code.                                 *
 *    DPB   04-10-91   Restructured routine.                          *
 *    DPB   04-26-91   Added code to strip blanks from inputs.        *
 *    KLM   06-17-91   Changed call from lab_append_value to          *
 *                     lu_append_value.                               *
 *    MDD   08-22-91   Rewrote to use cut and paste parameter routines*
 *    MDD   01-13-92   Added transfer of node_kind from old to new    *
 *                     parameter.                                     *
 **********************************************************************/

VALUE lab_change_value (label_ptr, object_class, object_name, object_position, 
                        parameter_name, parameter_position, parameter_value,
                        label_status)

AGGREGATE label_ptr;
char *object_class;
char *object_name;
int object_position;
char *parameter_name;
int parameter_position;
char *parameter_value;
int *label_status;

{
    AGGREGATE object_ptr = {NULL};
    AGGREGATE temp_object_ptr = {NULL};
    PARAMETER temp_parameter_ptr = {NULL};
    PARAMETER parameter_ptr = {NULL};
    VALUE value_ptr = {NULL};
    PARAMETER_KIND temp_kind;

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Initialize the label_status variable.                               **/
    /*-----------------------------------------------------------------------*/

    *label_status = PDS_ERROR;

    /*-----------------------------------------------------------------------*/
    /** Remove leading and trailing blanks from character inputs.           **/
    /*-----------------------------------------------------------------------*/

    util_strip_lead_and_trail (object_class, ' ');
    util_strip_lead_and_trail (object_name, ' ');
    util_strip_lead_and_trail (parameter_name, ' ');

    /*-----------------------------------------------------------------------*/
    /** Try to find the parameter whose value is to                         **/
    /**    be changed.                                                      **/
    /*-----------------------------------------------------------------------*/

    parameter_ptr = lab_find_parameter (label_ptr, object_class, object_name,
	      			     object_position, parameter_name,
				     parameter_position, label_status);


    /*-----------------------------------------------------------------------*/
    /** IF the parameter was found THEN                                     **/
    /*-----------------------------------------------------------------------*/

    if ((parameter_ptr != NULL) && (*label_status == PDS_SUCCESS))
    {

	  object_ptr = parameter_ptr -> owner;

	  /*-----------------------------------------------------------------*/
	  /** Create a one object tree and add the new parameter and value  **/
	  /**     to it.   We need to verify that the new value is valid    **/
	  /**     (and that there is enough memory left to malloc it)       **/
	  /**     BEFORE we remove the old value.                           **/
	  /*-----------------------------------------------------------------*/

	  *label_status = PDS_ERROR;
	  temp_object_ptr = NewAggregate (NULL, KA_OBJECT, "ROOT", "");

	  /*-----------------------------------------------------------------*/
	  /** IF the temporary object was created THEN                      **/
	  /*-----------------------------------------------------------------*/

	  if (temp_object_ptr != NULL)
	  {
	     /*--------------------------------------------------------------*/
	     /** Add a temporary parameter to this temporary object whose   **/
	     /**     value is the value passed in.                          **/
	     /*--------------------------------------------------------------*/

	     temp_parameter_ptr = lu_append_parameter (temp_object_ptr,
						       parameter_name,
						       parameter_value);

	     /*--------------------------------------------------------------*/
	     /** IF the temporary parameter was added to the object THEN    **/
	     /**     Cut the parameter from the temporary object and        **/
	     /**         paste it into the real object                      **/
	     /** ENDIF                                                      **/
	     /*--------------------------------------------------------------*/

	     if (temp_parameter_ptr != NULL)
	     {
		temp_parameter_ptr = CutParameter (temp_parameter_ptr);
		if (temp_parameter_ptr != NULL)
		{

                   temp_kind = parameter_ptr -> node_kind;
		   RemoveParameter (parameter_ptr);
		   temp_parameter_ptr = PasteParameter (object_ptr,
							temp_parameter_ptr);
		   if (temp_parameter_ptr != NULL)
		   {
		      *label_status = PDS_SUCCESS;
		      value_ptr = temp_parameter_ptr -> first_value;
                      temp_parameter_ptr -> node_kind = temp_kind;
		   }
		}
	     }

	     /*--------------------------------------------------------------*/
	     /** Remove the temporary object.                               **/
	     /*--------------------------------------------------------------*/

	     temp_object_ptr = RemoveAggregate (temp_object_ptr);

	  }
	  /*-----------------------------------------------------------------*/
	  /** ENDIF the temporary object was created                        **/
	  /*-----------------------------------------------------------------*/
    }
    /*-----------------------------------------------------------------------*/
    /** ENDIF the parameter was found                                       **/
    /** RETURN a pointer to the new value structure                         **/
    /*-----------------------------------------------------------------------*/

    return (value_ptr);

/** END **/

}  /*  "lab_change_value"  */



/**********************************************************************
 *$Component                                                          *
 *    void lab_clear_messages ()                                      *
 *$Abstract                                                           *
 *    Clears the global message list.                                 *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    ERROR                                                           *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    None                                                            *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    The lab_clear_messages routine deallocates the global message   *
 *    list.                                                           *
 *$Error_Handling                                                     *
 *    None                                                            *
 *$External_References                                                *
 *    Item                     Shared-Data           Access           *
 *    ------------------------------------------------------------    *
 *    pds_message_list         pdsglob.h             read             *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   October 21, 1991                                          *
 *$Change_History                                                     *
 *    DPB   04-10-91   Original Code.                                 *
 *    MDD   10-21-91   Removed redirect stuff.                        *
 **********************************************************************/

void lab_clear_messages ()

{
    err_deallocate_list (pds_message_list);

}  /*  "lab_clear_messages"  */



/**********************************************************************
 *$Component                                                          *
 *    AGGREGATE lab_find_object (label_ptr, object_class,             *
 *                       object_name, object_position, label_status)  *
 *$Abstract                                                           *
 *    Locates an object in a PDS label.                               *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    FIND                                                            *
 *    OBJECT                                                          *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    label_ptr:                                                      *
 *        The label_ptr variable is a pointer to the root of an       *
 *        ODL tree.  Usually it points to the actual "ROOT" object,   *
 *        but it may point to any other object in the tree.  If it    *
 *        does point to something other than the "ROOT" object, then  *
 *        this object will be treated as the root of a sub-tree, and  *
 *        processing will not be allowed to move above the level of   *
 *        this object.                                                *
 *    object_class:                                                   *
 *        The object_class variable is a character string             *
 *        which contains the class of an object in a PDS label        *
 *        (e.g., "OBJECT = IMAGE" implies that "IMAGE" is the class   *
 *        of the object).                                             *
 *    object_name:                                                    *
 *        The object_name variable is a character string              *
 *        which contains the name of an object in a PDS label.        *
 *        This is assumed to be the value of the "NAME" parameter     *
 *        in the object (e.g., "NAME = VOYAGER" implies that          *
 *        "VOYAGER" is the name of the object).                       *
 *    object_position:                                                *
 *        The object_position variable is an integer which            *
 *        represents the relative position of an object in a PDS      *
 *        label.  If this variable is used in conjunction with either *
 *        the object_class or object_name variables, then it          *
 *        represents a particular occurrence of that thing in the     *
 *        label (e.g., if object_class is "TABLE" and object_position *
 *        is 3, then this represents the third "TABLE" object in the  *
 *        label).  On the other hand, if this variable is used by     *
 *        itself, it represents the absolute position of the object,  *
 *        starting from the "ROOT" object (position = 1).  The        *
 *        sequence follows the structure of the label as it looks     *
 *        in a flat file.                                             *
 *$Outputs                                                            *
 *    label_status:                                                   *
 *        The label_status variable is an integer which is used to    *
 *        indicate the success or failure of a PDS label routine,     *
 *        and whether or not the object or parameter specifications   *
 *        passed in were ambiguous.  Valid values are:  PDS_SUCCESS,  *
 *        PDS_ERROR, PDS_MULTIPLE_OBJECTS, and PDS_MULTIPLE_PARMS.    *
 *$Returns                                                            *
 *    object_ptr:                                                     *
 *        The object_ptr variable is a pointer to the structure       *
 *        used to represent an object in a PDS label.                 *
 *$Detailed_Description                                               *
 *    The lab_find_object routine traverses an ODL tree, which        *
 *    represents the structure of a PDS label, and returns a pointer  *
 *    to the object that meets the specifications passed in.          *
 *    The object may by specified by class, name, or position, or     *
 *    by any combination of these.  The search for the object         *
 *    begins at the object pointed to by the label_ptr variable.      *
 *    This is usually the "ROOT" object, but may be any other object  *
 *    in the tree.  Please note that this routine only searches the   *
 *    tree at or below the level of the label_ptr object passed in.   *
 *    Please note that it is not necessary to specify the object's    *
 *    name, class, and position at the same time.  In fact, if you    *
 *    specify only one of them and pass in zero for the others, the   *
 *    routine will work just fine.                                    *
 *$Error_Handling                                                     *
 *    1) If the object cannot be found, then label_status is set to   *
 *       PDS_ERROR and a NULL value is returned.                      *
 *    2) If more than one object is found that matches the            *
 *       specifications passed in, then the label_status is set       *
 *       to PDS_MULTIPLE_OBJECTS and a pointer to the FIRST one       *
 *       is returned.                                                 *
 *    3) If everything went according to plan, then the label_status  *
 *       is set to PDS_SUCCESS and a pointer to the object is         *
 *       returned.                                                    *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.4   October 30, 1991                                          *
 *$Change_History                                                     *
 *    DPB   04-10-91   Original Code.                                 *
 *    DPB   04-26-91   Added code to strip blanks from inputs.        *
 *    MDD   04-30-91   Changed to use lab_find_object_level rather    *
 *                     than the AGGREGATE -> level field.             *
 *    KLM   06-17-91   Changed call from lab_find_object_level to     *
 *                     lu_find_object_level.                          *
 *    MDD   10-30-91   Changed NextAggregate call to a NextSubAgg..   *
 *                     call, to avoid processing siblings of the      *
 *                     root.                                          *
 **********************************************************************/

AGGREGATE lab_find_object (label_ptr, object_class, 
                           object_name, object_position, label_status)

AGGREGATE label_ptr;
char *object_class;
char *object_name;
int object_position;
int *label_status;

{
    AGGREGATE object_ptr = {NULL};
    AGGREGATE current_object = {NULL};
    PARAMETER parameter_ptr = {NULL};
    int curr_position = {1};
    int root_level = 0;
    LOGICAL done = {FALSE};
    LOGICAL class_found;
    LOGICAL name_found;
    LOGICAL position_found;
    LOGICAL this_is_the_first_one = {TRUE};
    
/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Initialize the label_status variable and level indicator            **/
    /*-----------------------------------------------------------------------*/

    *label_status = PDS_ERROR;
    root_level = lu_find_object_level (label_ptr);

    /*-----------------------------------------------------------------------*/
    /** Remove leading and trailing blanks from character inputs.           **/
    /*-----------------------------------------------------------------------*/

    util_strip_lead_and_trail (object_class, ' ');
    util_strip_lead_and_trail (object_name, ' ');

    /*-----------------------------------------------------------------------*/
    /** Figure out if we should stop right now or continue boldly on.       **/
    /*-----------------------------------------------------------------------*/

    done = ((label_ptr == NULL) || ((object_class == NULL) && 
             (object_name == NULL) && (object_position <= 0)));

    /*-----------------------------------------------------------------------*/
    /** LOOP through the tree until the object is found or the end is nigh  **/
    /*-----------------------------------------------------------------------*/

    for (current_object = label_ptr; 
            ((! done) && (current_object != NULL) &&
                    (lu_find_object_level (current_object) >= root_level));
                              current_object = NextSubAggregate (label_ptr, current_object))
    {

        /*-------------------------------------------------------------------*/
        /** Initialize the three "found" flags.  These are set based on the **/
        /**     values passed in.  If the class or name are NULL, or the    **/
        /**     position is zero, we assume that we have already found an   **/
        /**     object which meets our criteria.  Or, to put it another     **/
        /**     way, if the calling routine did not specify a class, a name **/
        /**     or a position for the object, then any class, name, or      **/
        /**     position will do.                                           **/
        /*-------------------------------------------------------------------*/

        class_found = (object_class == NULL);
        name_found = (object_name == NULL);
        position_found = (object_position <= 0);

        /*-------------------------------------------------------------------*/
        /** IF we have not already found an object whose class matches      **/
        /**         the one passed in THEN                                  **/
        /**     Check the class of the current object.                      **/
        /** ENDIF                                                           **/
        /*-------------------------------------------------------------------*/

        if (! class_found)
        {
            class_found = lab_match_classes (object_class, 
                                             current_object -> name);
        }

        /*-------------------------------------------------------------------*/
        /** IF we've found an object whose class matches the one            **/
        /**         passed in, AND we're supposed to look for the           **/
        /**         name too THEN                                           **/
        /*-------------------------------------------------------------------*/

        if (class_found && ! name_found)
        {
            /*---------------------------------------------------------------*/
            /** Look at the current object and try to find a parameter      **/
            /**     called "NAME"                                           **/
            /*---------------------------------------------------------------*/

            parameter_ptr = FindParameter (current_object, "NAME");

            /*---------------------------------------------------------------*/
            /** IF we have found one THEN                                   **/
            /**     Compare the value of the "NAME" parameter with the      **/
            /**         name passed in.                                     **/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

            if (parameter_ptr != NULL)
            {
                name_found = (strcmp (object_name, 
                          parameter_ptr->first_value->item.value.string) == 0);
            }

        /*-------------------------------------------------------------------*/
        /** ENDIF                                                           **/
        /*-------------------------------------------------------------------*/

        }  /*  End:  "if (class_found && ..."  */

        /*-------------------------------------------------------------------*/
        /** IF we've found an object with the right class and name, AND     **/
        /**     we're supposed to check its position in the label too THEN  **/
        /*-------------------------------------------------------------------*/

        if (class_found && name_found && ! position_found)
        {
            /*---------------------------------------------------------------*/
            /** Check the position of the current object against the        **/
            /**     against the position passed in.                         **/
            /*---------------------------------------------------------------*/

            position_found = (curr_position == object_position);

            /*---------------------------------------------------------------*/
            /** IF the current object is not at the right position THEN     **/
            /**     We had better increment the position counter, since     **/
            /**         this is not the object we're looking for.           **/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

            if (! position_found)
                ++curr_position;

        /*-------------------------------------------------------------------*/
        /** ENDIF                                                           **/
        /*-------------------------------------------------------------------*/

        }  /*  End:  "if (class_found && ..."  */

        /*-------------------------------------------------------------------*/
        /** IF (eureka!) we've found an object which meets all of the       **/
        /**         specifications passed in THEN                           **/
        /*-------------------------------------------------------------------*/

        if (class_found && name_found && position_found)
        {
            /*---------------------------------------------------------------*/
            /** IF this is the first object encountered which meets all     **/
            /**         of the specifications THEN                          **/
            /**     Set the label_status to SUCCESS and save a pointer to   **/
            /**         the current object.  If a position was specified,   **/
            /**         then we're done.  There can't be any confusion      **/
            /**         about redundant objects.  On the other hand, if     **/
            /**         a position was not specified, then we've got to     **/
            /**         keep looping through the tree until we find         **/
            /**         another object which satisfies the same criteria,   **/
            /**         reach the end of the tree, or hit an object whose   **/
            /**         level is higher than the label_ptr object passed in **/
            /**         (i.e., it's a sibling of the parent of the          **/
            /**         label_ptr object).                                  **/
            /** ELSE                                                        **/
            /**     We have two objects that meet the specifications passed **/
            /**         in.  Since we can't differentiate between them,     **/
            /**         all we can do is set the label_status value to      **/
            /**         indicate multiple objects, and return a pointer to  **/
            /**         the first one.  We'll let the calling routine       **/
            /**         decide what to do with the other one.               **/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

            if (this_is_the_first_one)
            {
                *label_status = PDS_SUCCESS;
                object_ptr = current_object;

                if (object_position > 0)
                    done = TRUE;
            }
            else
            {
                *label_status = PDS_MULTIPLE_OBJECTS;
                done = TRUE;
            }

            /*---------------------------------------------------------------*/
            /** Indicate that we've already found one object which meets    **/
            /**     the specifications passed in.                           **/
            /*---------------------------------------------------------------*/

            this_is_the_first_one = FALSE;

        /*-------------------------------------------------------------------*/
        /** ENDIF                                                           **/
        /*-------------------------------------------------------------------*/

        }  /*  End:  "if (class_found && ..."  */

    /*-----------------------------------------------------------------------*/
    /** ENDLOOP                                                             **?
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "for (current_object = label_ptr, ..."  */

    /*-----------------------------------------------------------------------*/
    /** RETURN a pointer to the object found                                **/
    /*-----------------------------------------------------------------------*/

    return (object_ptr);

/** END **/

}  /*  "lab_find_object"  */



/**********************************************************************
 *$Component                                                          *
 *    PARAMETER lab_find_parameter (label_ptr, object_class,          *
 *                                object_name, object_position,       *
 *                                parameter_name, parameter_position, *
 *                                label_status)                       *
 *$Abstract                                                           *
 *    Locates a parameter in a PDS label.                             *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    ADD                                                             *
 *    PARAMETER                                                       *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    label_ptr:                                                      *
 *        The label_ptr variable is a pointer to the root of an       *
 *        ODL tree.  Usually it points to the actual "ROOT" object,   *
 *        but it may point to any other object in the tree.  If it    *
 *        does point to something other than the "ROOT" object, then  *
 *        this object will be treated as the root of a sub-tree, and  *
 *        processing will not be allowed to move above the level of   *
 *        this object.                                                *
 *    object_class:                                                   *
 *        The object_class variable is a character string             *
 *        which contains the class of an object in a PDS label        *
 *        (e.g., "OBJECT = IMAGE" implies that "IMAGE" is the class   *
 *        of the object).                                             *
 *    object_name:                                                    *
 *        The object_name variable is a character string              *
 *        which contains the name of an object in a PDS label.        *
 *        This is assumed to be the value of the "NAME" parameter     *
 *        in the object (e.g., "NAME = VOYAGER" implies that          *
 *        "VOYAGER" is the name of the object).                       *
 *    object_position:                                                *
 *        The object_position variable is an integer which            *
 *        represents the relative position of an object in a PDS      *
 *        label.  If this variable is used in conjunction with either *
 *        the object_class or object_name variables, then it          *
 *        represents a particular occurrence of that thing in the     *
 *        label (e.g., if object_class is "TABLE" and object_position *
 *        is 3, then this represents the third "TABLE" object in the  *
 *        label).  On the other hand, if this variable is used by     *
 *        itself, it represents the absolute position of the object,  *
 *        starting from the "ROOT" object (position = 1).  The        *
 *        sequence follows the structure of the label as it looks     *
 *        in a flat file.                                             *
 *    parameter_name:                                                 *
 *        The parameter_name variable is a character string           *
 *        which contains the name of a parameter in a PDS label       *
 *        (e.g., the line "SCID = VG1" implies that "SCID" is the     *
 *        parameter name).                                            *
 *    parameter_position:                                             *
 *        The parameter_position variable is an integer which         *
 *        represents the relative position of a parameter in an       *
 *        object in a PDS label.  If this variable is used in         *
 *        conjunction with the parameter_name variable, then it       *
 *        represents a particular occurrence of that parameter        *
 *        in the object (e.g., if parameter_name is "SCID" and        *
 *        parameter_position is 2, then this represents the second    *
 *        "SCID" parameter in the object).  On the other hand, if     *
 *        this variable is used by itself, it represents the absolute *
 *        position of the parameter within the object, starting with  *
 *        first parameter in the object (position = 1).               *
 *$Outputs                                                            *
 *    label_status:                                                   *
 *        The label_status variable is an integer which is used to    *
 *        indicate the success or failure of a PDS label routine,     *
 *        and whether or not the object or parameter specifications   *
 *        passed in were ambiguous.  Valid values are:  PDS_SUCCESS,  *
 *        PDS_ERROR, PDS_MULTIPLE_OBJECTS, and PDS_MULTIPLE_PARMS.    *
 *$Returns                                                            *
 *    parameter_ptr:                                                  *
 *        The parameter_ptr variable is a pointer to the structure    *
 *        used to represent a parameter in a PDS label.               *
 *$Detailed_Description                                               *
 *    The lab_find_parameter routine traverses an ODL tree, which     *
 *    represents the structure of a PDS label, finds the object       *
 *    which meets the specifications passed in, and returns a pointer *
 *    to the parameter that meets the specifications passed in.       *
 *    The object may by specified by class, name, or position, or     *
 *    by any combination of these, and the parameter may be specified *
 *    by name or position or both.  The search for the object begins  *
 *    at the object pointed to by the label_ptr variable.  This is    *
 *    usually the "ROOT" object, but may be any other object in the   *
 *    tree.  Please note that it is not necessary to specify the      *
 *    object's name, class, and position at the same time.  In fact,  *
 *    if you specify only one of them and pass in zero for the        *
 *    others, the routine will work just fine.  This also applies to  *
 *    the parameter name and position.                                *
 *$Error_Handling                                                     *
 *    1) If the object or parameter cannot be found, then the         *
 *       label_status variable is set to PDS_ERROR and a NULL value   *
 *       is returned.                                                 *
 *    2) If more than one object is found that matches the            *
 *       specifications passed in for the object, then the            *
 *       label_status is set to PDS_MULTIPLE_OBJECTS and a NULL       *
 *       value is returned.                                           *
 *    3) If more than one parameter is found that matches the         *
 *       specifications passed in for the parameter, then the         *
 *       label_status is set to PDS_MULTIPLE_PARMS and a pointer to   *
 *       the FIRST one is returned.                                   *
 *    4) If everything went according to plan, then the label_status  *
 *       is set to PDS_SUCCESS and a pointer to the parameter is      *
 *       returned.                                                    *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   April 26, 1991                                            *
 *$Change_History                                                     *
 *    DPB   04-10-91   Original Code.                                 *
 *    DPB   04-26-91   Added code to strip blanks from inputs.        *
 **********************************************************************/

PARAMETER lab_find_parameter (label_ptr, object_class, object_name, 
                              object_position, parameter_name, 
                              parameter_position, label_status)

AGGREGATE label_ptr;
char *object_class;
char *object_name;
int object_position;
char *parameter_name;
int parameter_position;
int *label_status;

{
    OBJECT object_ptr = {label_ptr};
    PARAMETER parameter_ptr = {NULL};       
    PARAMETER current_parameter = {NULL};
    int curr_position = {1};
    LOGICAL done = {FALSE};
    LOGICAL name_found;
    LOGICAL position_found;
    LOGICAL this_is_the_first_one = {TRUE};

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Initialize the label_status variable.                               **/
    /*-----------------------------------------------------------------------*/

    *label_status = PDS_ERROR;

    /*-----------------------------------------------------------------------*/
    /** Remove leading and trailing blanks from character inputs.           **/
    /*-----------------------------------------------------------------------*/

    util_strip_lead_and_trail (object_class, ' ');
    util_strip_lead_and_trail (object_name, ' ');
    util_strip_lead_and_trail (parameter_name, ' ');

    /*-----------------------------------------------------------------------*/
    /** IF any parameter info was passed in THEN                            **/
    /**     Try to find the specified object.                               **/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    if ((parameter_name != NULL) || (parameter_position > 0))
    {
        object_ptr = lab_find_object (label_ptr, object_class, 
                                      object_name, object_position, label_status
);
    }

    /*-----------------------------------------------------------------------*/
    /** IF the object was found THEN                                        **/
    /*-----------------------------------------------------------------------*/

    if ((object_ptr != NULL) && (*label_status == PDS_SUCCESS))
    {
        /*-------------------------------------------------------------------*/
        /** Re-initialize the label_status variable.                        **/
        /*-------------------------------------------------------------------*/

        *label_status = PDS_ERROR;
            
        /*-------------------------------------------------------------------*/
        /** LOOP through the parameter list until the parameter is found    **/
        /**         or the list has ended.                                  **/
        /*-------------------------------------------------------------------*/

        for (current_parameter = object_ptr -> first_parameter; 
                ((! done) && (current_parameter != NULL));
                    current_parameter = NextParameter (current_parameter))
        {
            /*---------------------------------------------------------------*/
            /** Initialize the two "found" flags.  These are set based on   **/
            /**     the values passed in.  If the name is NULL, or the      **/
            /**     position is zero, we assume that we have already        **/
            /**     found a parameter which meets our criteria.  Or, to     **/
            /**     put it another way, if the calling routine did not      **/
            /**     specify a name or a position, then any name or position **/
            /**     will do.                                                **/
            /*---------------------------------------------------------------*/

            name_found = (parameter_name == NULL);
            position_found = (parameter_position <= 0);

            /*---------------------------------------------------------------*/
            /** IF we haven't already found the parameter THEN              **/
            /**     Compare the current parameter's name with the name      **/
            /**         passed in.                                          **/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

            if (! name_found)
            {
                name_found = (strcmp (parameter_name, 
                                          current_parameter -> name) == 0);
            }

            /*---------------------------------------------------------------*/
            /** IF we've found a parameter with the right name, AND we're   **/
            /**     supposed to check its position in the object too THEN   **/
            /*---------------------------------------------------------------*/

            if (name_found && ! position_found)
            {
                /*-----------------------------------------------------------*/
                /** Check the position of the current parameter against the **/
                /**     position passed in.                                 **/
                /*-----------------------------------------------------------*/

                position_found = (curr_position == parameter_position);

                /*-----------------------------------------------------------*/
                /** IF the current parameter is not at the right            **/
                /**         position THEN                                   **/
                /**     We had better increment the position counter,       **/
                /**         since this is not the parameter we're looking   **/
                /**         for.                                            **/
                /** ENDIF                                                   **/
                /*-----------------------------------------------------------*/

                if (! position_found)
                    ++curr_position;
    
            /*---------------------------------------------------------------*/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

            }  /*  End:  "if (name_found && ..."  */

            /*---------------------------------------------------------------*/
            /** IF we've found a parameter which meets all of the           **/
            /**         specifications passed in THEN                       **/
            /*---------------------------------------------------------------*/

            if (name_found && position_found)
            {
                /*-----------------------------------------------------------*/
                /** IF this is the first parameter encountered which meets  **/
                /**         all of the specifications THEN                  **/
                /**     Set the label_status to SUCCESS and save a pointer  **/
                /**         to the current parameter.  If a position was    **/
                /**         specified, then we're done.  There can't be     **/
                /**         any confusion about redundant parameters.  On   **/
                /**         the other hand, if a position was not specified **/
                /**         then we've got to keep looping through the list **/
                /**         of parameters until we find another parameter   **/
                /**         which satisfies the same criteria, or run out   **/
                /**         of parameters to check.                         **/
                /** ELSE                                                    **/
                /**     We have two parameters that meet the specifications **/
                /**         passed in.  Since we can't differentiate        **/
                /**         between them, all we can do is set the          **/
                /**         label_status to indicate multiple paramaters,   **/
                /**         and return a pointer to the first one.  We'll   **/
                /**         let the calling routine decide what to do with  **/
                /**         the other one.                                  **/
                /** ENDIF                                                   **/
                /*-----------------------------------------------------------*/

                if (this_is_the_first_one)
                {
                    *label_status = PDS_SUCCESS;
                    parameter_ptr = current_parameter;

                    if (parameter_position > 0) 
                        done = TRUE;
                }
                else
                {
                    *label_status = PDS_MULTIPLE_PARMS;
                    done = TRUE;
                }

                /*-----------------------------------------------------------*/
                /** Indicate that we've already found one parameter which   **/
                /**     meets the specifications passed in.                 **/
                /*-----------------------------------------------------------*/

                this_is_the_first_one = FALSE;

            /*---------------------------------------------------------------*/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

            }  /*  End:  "if (name_found && ..."  */

        /*-------------------------------------------------------------------*/
        /** ENDLOOP                                                         **/
        /*-------------------------------------------------------------------*/

        }  /*  End:  "for (current_parameter = ..."  */

    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if ((parameter_name != NULL) || ..."  */

    /*-----------------------------------------------------------------------*/
    /** RETURN a pointer to the parameter structure                         **/
    /*-----------------------------------------------------------------------*/

    return (parameter_ptr);

/** END **/

}  /*  "lab_find_parameter"  */


/**********************************************************************
 *$Component                                                          *
 *   STRING_LIST *lab_get_all_values (label_ptr, object_class,        *
 *                       object_name, object_position, parameter_name,*
 *                       parameter_position, use_quotes,              *
 *                       label_status)                                *
 *$Abstract                                                           *
 *    Gets all the values of a keyword in a label.                    *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    VALUE                                                           *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    label_ptr:                                                      *
 *        The label_ptr variable is a pointer to the root of an       *
 *        ODL tree.  Usually it points to the actual "ROOT" object,   *
 *        but it may point to any other object in the tree.  If it    *
 *        does point to something other than the "ROOT" object, then  *
 *        this object will be treated as the root of a sub-tree, and  *
 *        processing will not be allowed to move above the level of   *
 *        this object.                                                *
 *    object_class:                                                   *
 *        The object_class variable is a character string             *
 *        which contains the class of an object in a PDS label        *
 *        (e.g., "OBJECT = IMAGE" implies that "IMAGE" is the class   *
 *        of the object).                                             *
 *    object_name:                                                    *
 *        The object_name variable is a character string              *
 *        which contains the name of an object in a PDS label.        *
 *        This is assumed to be the value of the "NAME" parameter     *
 *        in the object (e.g., "NAME = VOYAGER" implies that          *
 *        "VOYAGER" is the name of the object).                       *
 *    object_position:                                                *
 *        The object_position variable is an integer which            *
 *        represents the relative position of an object in a PDS      *
 *        label.  If this variable is used in conjunction with either *
 *        the object_class or object_name variables, then it          *
 *        represents a particular occurrence of that thing in the     *
 *        label (e.g., if object_class is "TABLE" and object_position *
 *        is 3, then this represents the third "TABLE" object in the  *
 *        label).  On the other hand, if this variable is used by     *
 *        itself, it represents the absolute position of the object,  *
 *        starting from the "ROOT" object (position = 1).  The        *
 *        sequence follows the structure of the label as it looks     *
 *        in a flat file.                                             *
 *    parameter_name:                                                 *
 *        The parameter_name variable is a character string           *
 *        which contains the name of a parameter in a PDS label       *
 *        (e.g., the line "SCID = VG1" implies that "SCID" is the     *
 *        parameter name).                                            *
 *    parameter_position:                                             *
 *        The parameter_position variable is an integer which         *
 *        represents the relative position of a parameter in an       *
 *        object in a PDS label.  If this variable is used in         *
 *        conjunction with the parameter_name variable, then it       *
 *        represents a particular occurrence of that parameter        *
 *        in the object (e.g., if parameter_name is "SCID" and        *
 *        parameter_position is 2, then this represents the second    *
 *        "SCID" parameter in the object).  On the other hand, if     *
 *        this variable is used by itself, it represents the absolute *
 *        position of the parameter within the object, starting with  *
 *        first parameter in the object (position = 1).               *
 *    use_quotes:                                                     *
 *        The use_quotes variable is a true/false flag which indicates*
 *        whether quotes should be included in a string or not.       *
 *$Outputs                                                            *
 *    label_status:                                                   *
 *        The label_status variable is an integer which is used to    *
 *        indicate the success or failure of a PDS label routine,     *
 *        and whether or not the object or parameter specifications   *
 *        passed in were ambiguous.  Valid values are:  PDS_SUCCESS,  *
 *        PDS_ERROR, PDS_MULTIPLE_OBJECTS, and PDS_MULTIPLE_PARMS.    *
 *$Returns                                                            *
 *    label_value_list:                                               *
 *        The label_value_list variable is a string list              *
 *        containing all the values of a keyword in a PDS label.      *
 *$Detailed_Description                                               *
 *    The lab_get_all_values routine gets the values of a keyword in  *
 *    a PDS label.  It searches for the parent object of a parameter, *
 *    then searches for the parameter.  The object may be specified   *
 *    by class, name, or position, or by any combination of these.    *
 *    The search for the parent begins at the object pointed to by    *
 *    the label_ptr variable.  This is usually the "ROOT" object, but *
 *    may be any other object in the tree.  Please note that it is    *
 *    not necessary to specify the object's name, class, and          *
 *    position at the same time.  In fact, if you specify only one    *
 *    of them and pass in zero for the others, the routine will work  *
 *    just fine.  The parameter may be specified by name, position,   *
 *    or by both.  A string list is allocated for all the values      *
 *    found.                                                          *
 *    If the use_quotes input is TRUE, then quotes will be included   *
 *    in the value returned if it represents a symbol or character    *
 *    string.                                                         *
 *$Error_Handling                                                     *
 *    1) If the object or parameter cannot be found, then the         *
 *       label_status variable is set to PDS_ERROR and a NULL value   *
 *       is returned.                                                 *
 *    2) If more than one object is found that matches the            *
 *       specifications passed in for the object, then the            *
 *       label_status is set to PDS_MULTIPLE_OBJECTS and a NULL       *
 *       value is returned.                                           *
 *    3) If more than one parameter is found that matches the         *
 *       specifications passed in for the parameter, then the         *
 *       label_status is set to PDS_MULTIPLE_PARMS and a NULL value   *
 *       is returned.                                                 *
 *    4) If everything went according to plan, then the label_status  *
 *       is set to PDS_SUCCESS and a pointer to the value is returned.*
 *$Side_Effects                                                       *
 *    This routine causes memory to be allocated which must be freed  *
 *    by the calling routine.                                         *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore / J.P.L.                                        *
 *$Version_and_Date                                                   *
 *    1.2   October 21, 1991                                          *
 *$Change_History                                                     *
 *    MDD   04-23-91   Original Code.                                 *
 *    KLM   06-17-91   Changed call from lab_fetch_value to           *
 *                     lu_fetch_value.                                *
 *    MDD   10-21-91   Fixed free statement.                          *
 **********************************************************************/


STRING_LIST *lab_get_all_values (label_ptr, object_class, object_name,
                                 object_position, parameter_name, 
                                 parameter_position, use_quotes, 
                                 label_status)
   AGGREGATE label_ptr;
   char *object_class;
   char *object_name;
   int object_position;
   char *parameter_name;
   int parameter_position;
   LOGICAL use_quotes;
   int *label_status;
{
   STRING_LIST *value_list = NULL;
   PARAMETER parameter_ptr = {NULL};
   char *data_value = NULL;
   VALUE value_ptr;


   /*----------------------------------------------------------------*/
   /** find the specified parameter                                 **/
   /*----------------------------------------------------------------*/

   *label_status = PDS_ERROR;
   parameter_ptr = lab_find_parameter (label_ptr, object_class, 
                                        object_name, object_position, 
                                        parameter_name, parameter_position, 
                                                   label_status);
   /*----------------------------------------------------------------*/
   /** IF the parameter was found THEN                              **/
   /**   LOOP through the values of the parameter                   **/
   /**      fetch the value                                         **/
   /**      add it to the string list                               **/
   /**   ENDLOOP    
   /*----------------------------------------------------------------*/

   if (parameter_ptr != NULL && *label_status == PDS_SUCCESS)
      {
      for (value_ptr = FirstValue (parameter_ptr); value_ptr != NULL;
              value_ptr = NextValue (value_ptr)) 
      {
         data_value = lu_fetch_value (value_ptr, use_quotes);
         if (data_value != NULL)
         {
            value_list = util_append_string_list (value_list, data_value, 
                                                     STRING_TYPE);
            Lemme_Go(data_value);
	 }
      }
   }
   /*----------------------------------------------------------------*/
   /** ENDIF the parameter was found...                             **/
   /*----------------------------------------------------------------*/

   return (value_list);

   /** END lab_get_all_values **/
}




/**********************************************************************
 *$Component                                                          *
 *   char *lab_get_value (label_ptr, object_class, object_name,       *
 *                        object_position, parameter_name,            *
 *                        parameter_position, value_position,         *
 *                        use_quotes, label_status)                   *
 *$Abstract                                                           *
 *    Gets a value of a keyword in a PDS label.                       *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    VALUE                                                           *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    label_ptr:                                                      *
 *        The label_ptr variable is a pointer to the root of an       *
 *        ODL tree.  Usually it points to the actual "ROOT" object,   *
 *        but it may point to any other object in the tree.  If it    *
 *        does point to something other than the "ROOT" object, then  *
 *        this object will be treated as the root of a sub-tree, and  *
 *        processing will not be allowed to move above the level of   *
 *        this object.                                                *
 *    object_class:                                                   *
 *        The object_class variable is a character string             *
 *        which contains the class of an object in a PDS label        *
 *        (e.g., "OBJECT = IMAGE" implies that "IMAGE" is the class   *
 *        of the object).                                             *
 *    object_name:                                                    *
 *        The object_name variable is a character string              *
 *        which contains the name of an object in a PDS label.        *
 *        This is assumed to be the value of the "NAME" parameter     *
 *        in the object (e.g., "NAME = VOYAGER" implies that          *
 *        "VOYAGER" is the name of the object).                       *
 *    object_position:                                                *
 *        The object_position variable is an integer which            *
 *        represents the relative position of an object in a PDS      *
 *        label.  If this variable is used in conjunction with either *
 *        the object_class or object_name variables, then it          *
 *        represents a particular occurrence of that thing in the     *
 *        label (e.g., if object_class is "TABLE" and object_position *
 *        is 3, then this represents the third "TABLE" object in the  *
 *        label).  On the other hand, if this variable is used by     *
 *        itself, it represents the absolute position of the object,  *
 *        starting from the "ROOT" object (position = 1).  The        *
 *        sequence follows the structure of the label as it looks     *
 *        in a flat file.                                             *
 *    parameter_name:                                                 *
 *        The parameter_name variable is a character string           *
 *        which contains the name of a parameter in a PDS label       *
 *        (e.g., the line "SCID = VG1" implies that "SCID" is the     *
 *        parameter name).                                            *
 *    parameter_position:                                             *
 *        The parameter_position variable is an integer which         *
 *        represents the relative position of a parameter in an       *
 *        object in a PDS label.  If this variable is used in         *
 *        conjunction with the parameter_name variable, then it       *
 *        represents a particular occurrence of that parameter        *
 *        in the object (e.g., if parameter_name is "SCID" and        *
 *        parameter_position is 2, then this represents the second    *
 *        "SCID" parameter in the object).  On the other hand, if     *
 *        this variable is used by itself, it represents the absolute *
 *        position of the parameter within the object, starting with  *
 *        first parameter in the object (position = 1).               *
 *   value_position:                                                  *
 *        The value_position variable is an integer which represents  *
 *        the position of a value in a keyword/value statement in a   *
 *        PDS label.  For example, the statement ``SPACECRAFT_ID =    *
 *        (VG1, MGN)" has two values with positions 1 and 2. If only  *
 *        a single value is present, then it has position 1.          *
 *    use_quotes:                                                     *
 *        The use_quotes variable is a true/false flag which indicates*
 *        whether quotes should be included in a string or not.       *
 *$Outputs                                                            *
 *    label_status:                                                   *
 *        The label_status variable is an integer which is used to    *
 *        indicate the success or failure of a PDS label routine,     *
 *        and whether or not the object or parameter specifications   *
 *        passed in were ambiguous.  Valid values are:  PDS_SUCCESS,  *
 *        PDS_ERROR, PDS_MULTIPLE_OBJECTS, and PDS_MULTIPLE_PARMS.    *
 *$Returns                                                            *
 *    label_value_ptr:                                                *
 *        The label_value_ptr variable is a character string list     *
 *        containing the value of a keyword in a PDS label.           *
 *$Detailed_Description                                               *
 *    The lab_get_value routine returns the value of a keyword in a   *
 *    PDS label.  It searches for the parent object of a parameter,   *
 *    then searches for the parameter, and then finally searches for  *
 *    the value with the given position. The object may be specified  *
 *    by class, name, or position, or by any combination of these.    *
 *    The search for the parent begins at the object pointed to by    *
 *    the label_ptr variable.  This is usually the "ROOT" object, but *
 *    may be any other object in the tree.  Please note that it is    *
 *    not necessary to specify the object's name, class, and          *
 *    position at the same time.  In fact, if you specify only one    *
 *    of them and pass in zero for the others, the routine will work  *
 *    just fine.  The parameter may be specified by name, position,   *
 *    or by both.                                                     *
 *    If the use_quotes input is TRUE, then quotes will be included   *
 *    in the value returned if it represents a symbol or character    *
 *    string.                                                         *
 *$Error_Handling                                                     *
 *    1) If the object or parameter cannot be found, then the         *
 *       label_status variable is set to PDS_ERROR and a NULL value   *
 *       is returned.                                                 *
 *    2) If more than one object is found that matches the            *
 *       specifications passed in for the object, then the            *
 *       label_status is set to PDS_MULTIPLE_OBJECTS and a NULL       *
 *       value is returned.                                           *
 *    3) If more than one parameter is found that matches the         *
 *       specifications passed in for the parameter, then the         *
 *       label_status is set to PDS_MULTIPLE_PARMS and a NULL value   *
 *       is returned.                                                 *
 *    4) If everything went according to plan, then the label_status  *
 *       is set to PDS_SUCCESS and a pointer to the value is returned.*
 *$Side_Effects                                                       *
 *    This routine causes memory to be allocated which must be freed  *
 *    by the calling routine.                                         *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore / J.P.L.                                        *
 *$Version_and_Date                                                   *
 *    1.1   June 17, 1991                                             *
 *$Change_History                                                     *
 *    MDD   04-23-91   Original Code.                                 *
 *    KLM   06-17-91   Changed call from lab_fetch_value to           *
 *                     lu_fetch_value.                                *
 **********************************************************************/

char *lab_get_value (label_ptr, object_class, object_name,
                     object_position, parameter_name, parameter_position,
                     value_position, use_quotes, label_status)

AGGREGATE label_ptr;
char *object_class;
char *object_name;
int object_position;
char *parameter_name;
int parameter_position;
int value_position;
LOGICAL use_quotes;
int *label_status;

{
    PARAMETER parameter_ptr = {NULL};
    char *data_value = NULL;
    VALUE value_ptr;
    int i;

/** BEGIN **/
    /*----------------------------------------------------------------*/
    /** find the specified parameter                                 **/
    /*----------------------------------------------------------------*/

    *label_status = PDS_ERROR;
    parameter_ptr = lab_find_parameter (label_ptr, object_class, 
                                         object_name, object_position, 
                                         parameter_name, parameter_position, 
                                                   label_status);

    /*----------------------------------------------------------------*/
    /** IF the parameter was found THEN                              **/
    /**   find the value with the given position                     **/
    /**   fetch that value                                           **/
    /*----------------------------------------------------------------*/

    if (parameter_ptr != NULL && *label_status == PDS_SUCCESS)
    {
       value_ptr = FirstValue (parameter_ptr);
       for (i = 1; i < value_position; i++) 
       {
          value_ptr = NextValue (value_ptr);
       }
       data_value = lu_fetch_value (value_ptr, use_quotes);
    }

    /*----------------------------------------------------------------*/
    /** ENDIF the parameter was found...                             **/
    /** reset the label_status to error if no value was found        **/
    /*----------------------------------------------------------------*/

    if (value_ptr == NULL || data_value == NULL)
    {
       *label_status = PDS_ERROR;
    }
    return (data_value);

/** END lab_get_value **/
}




/**********************************************************************
 *$Component                                                          *
 *    LOGICAL lab_match_classes (primary_object_class,                *
 *                               compared_object_class)               *
 *$Abstract                                                           *
 *    Compares two object classes                                     *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    OBJECT                                                          *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    primary_object_class:                                           *
 *        The primary_object_class variable is a character string     *
 *        which contains the primary object class, e.g. the object    *
 *        class against which all other classes are to be compared.   *
 *    compared_object_class:                                          *
 *        The compared_object_class variable is a character string    *
 *        which contains the object class to be compare against       *
 *        the primary class.  This class may be compared whole, or    *
 *        it may be compared piece by piece, depending on whether     *
 *        or not the global generic class flag is set.                *
 *$Outputs                                                            *
 *    NONE                                                            *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure        *
 *        (a value of FALSE).                                         *
 *$Detailed_Description                                               *
 *    This routine takes the primary class passed in and compares it  *
 *    against the compared class.  If they match then TRUE is         *
 *    returned.  A match is determined in the following way:          *
 *    First, all trailing numbers and underscores are stripped from   *
 *    the compared class.   Then, the remaining string is compared    *
 *    against the primary class.  If the global generic class flag is *
 *    not set, then this comparison is performed on the entire string.*
 *    Otherwise, the primary class is treated as a generic class and  *
 *    it is compared against the other string piece by piece.         *
 *$Error_Handling                                                     *
 *    None.                                                           *
 *$Side_Effects                                                       *
 *    This routine causes memory to be allocated which must be freed  *
 *    by the calling routine.                                         *
 *$External_References                                                *
 *    Item                     Shared-Data           Access           *
 *    ------------------------------------------------------------    *
 *    pds_generic_class        pdsglob.h             read             *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J. P. L.                                     *
 *$Version_and_Date                                                   *
 *    1.1   October 21, 1991                                          *
 *$Change_History                                                     *
 *    DPB   07-24-91   Original Code.                                 *
 *    MDD   10-21-91   Fixe malloc, free, and sys_exit_system calls   *
 **********************************************************************/

LOGICAL lab_match_classes (primary_object_class, compared_object_class)

char *primary_object_class;
char *compared_object_class;

{
    char *temp_class= {NULL};
    char *c = {NULL};
    LOGICAL found = {FALSE};

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** IF both pieces of information were passed in THEN                   **/
    /*-----------------------------------------------------------------------*/

    if ((primary_object_class != NULL) && (compared_object_class != NULL))
    {
        /*-------------------------------------------------------------------*/
        /** Prepare a temporary string.                                     **/
        /*-------------------------------------------------------------------*/

        Malloc_String(temp_class, String_Size(compared_object_class));
        strcpy (temp_class, compared_object_class);

	/*-------------------------------------------------------------------*/
	/** IF the global generic class flag is not set THEN                **/
	/**     The classes match if a direct comparison succeeds.          **/
	/** ELSE                                                            **/
	/*-------------------------------------------------------------------*/

        if (! pds_generic_class)
            found = (strcmp (primary_object_class, temp_class) == 0);
        else
	{
	   /*-------------------------------------------------------------------*/
	   /** Remove any trailing numbers and underscores.                    **/
	   /*-------------------------------------------------------------------*/

	   for (c = String_End(temp_class);
		((c >= temp_class) && (isdigit(*c) || (*c == '_'))); --c) ;
	   *(++c) = EOS;


	    /*---------------------------------------------------------------*/
	    /** The classes match when the primary class matches a piece    **/
	    /**     of the compared class.  These pieces start from the     **/
	    /**     left and go to the end of the string.                   **/
            /*---------------------------------------------------------------*/

            for (c = temp_class; 
                    ((*c != EOS) && (strcmp (primary_object_class, c) != 0)); )
            {
                c = strchr (c, (int) ('_'));

                if (c == NULL)
                    c = String_End(temp_class);

                ++c;
    
            }  /*  End:  "for (c = temp_class, ..."  */

            found = (*c != EOS);

        /*-------------------------------------------------------------------*/
        /** ENDIF                                                           **/
        /*-------------------------------------------------------------------*/

        }  /*  End:  "if (! pds_temp_class) ... else ..."  */

        /*-------------------------------------------------------------------*/
        /** Free local storage.                                             **/
        /*-------------------------------------------------------------------*/

        Lemme_Go(temp_class);
        
    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if (object_class == NULL) ... else ..."  */

    /*-----------------------------------------------------------------------*/
    /** RETURN the success flag.                                            **/
    /*-----------------------------------------------------------------------*/

    return (found);

/** END **/

}  /*  "lab_match_classes"  */


/**********************************************************************
 *$Component                                                          *
 *    PARAMETER lab_move_parameter (label_ptr, object_class,          *
 *                         object_name, object_position,              *
 *                         parameter_name, parameter_position,        *
 *                         new_position, label_status)                *
 *$Abstract                                                           *
 *    Moves a parameter in a PDS Label.                               *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    PARAMETER                                                       *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    label_ptr:                                                      *
 *        The label_ptr variable is a pointer to the root of an       *
 *        ODL tree.  Usually it points to the actual "ROOT" object,   *
 *        but it may point to any other object in the tree.  If it    *
 *        does point to something other than the "ROOT" object, then  *
 *        this object will be treated as the root of a sub-tree, and  *
 *        processing will not be allowed to move above the level of   *
 *        this object.                                                *
 *    object_class:                                                   *
 *        The object_class variable is a character string             *
 *        which contains the class of an object in a PDS label        *
 *        (e.g., "OBJECT = IMAGE" implies that "IMAGE" is the class   *
 *        of the object).                                             *
 *    object_name:                                                    *
 *        The object_name variable is a character string              *
 *        which contains the name of an object in a PDS label.        *
 *        This is assumed to be the value of the "NAME" parameter     *
 *        in the object (e.g., "NAME = VOYAGER" implies that          *
 *        "VOYAGER" is the name of the object).                       *
 *    object_position:                                                *
 *        The object_position variable is an integer which            *
 *        represents the relative position of an object in a PDS      *
 *        label.  If this variable is used in conjunction with either *
 *        the object_class or object_name variables, then it          *
 *        represents a particular occurrence of that thing in the     *
 *        label (e.g., if object_class is "TABLE" and object_position *
 *        is 3, then this represents the third "TABLE" object in the  *
 *        label).  On the other hand, if this variable is used by     *
 *        itself, it represents the absolute position of the object,  *
 *        starting from the "ROOT" object (position = 1).  The        *
 *        sequence follows the structure of the label as it looks     *
 *        in a flat file.                                             *
 *    parameter_name:                                                 *
 *        The parameter_name variable is a character string           *
 *        which contains the name of a parameter in a PDS label       *
 *        (e.g., the line "SCID = VG1" implies that "SCID" is the     *
 *        parameter name).                                            *
 *    parameter_position:                                             *
 *        The parameter_position variable is an integer which         *
 *        represents the relative position of a parameter in an       *
 *        object in a PDS label.  If this variable is used in         *
 *        conjunction with the parameter_name variable, then it       *
 *        represents a particular occurrence of that parameter        *
 *        in the object (e.g., if parameter_name is "SCID" and        *
 *        parameter_position is 2, then this represents the second    *
 *        "SCID" parameter in the object).  On the other hand, if     *
 *        this variable is used by itself, it represents the absolute *
 *        position of the parameter within the object, starting with  *
 *        first parameter in the object (position = 1).               *
 *    new_position:                                                   *
 *        The new_position variable is the same as either             *
 *        object_position or parameter_position, depending upon       *
 *        context, and is used when either an object or parameter is  *
 *        to be moved to a new location within the parent object      *
 *        an a PDS label.                                             *
 *$Outputs                                                            *
 *    label_status:                                                   *
 *        The label_status variable is an integer which is used to    *
 *        indicate the success or failure of a PDS label routine,     *
 *        and whether or not the object or parameter specifications   *
 *        passed in were ambiguous.  Valid values are:  PDS_SUCCESS,  *
 *        PDS_ERROR, PDS_MULTIPLE_OBJECTS, and PDS_MULTIPLE_PARMS.    *
 *$Returns                                                            *
 *    parameter_ptr:                                                  *
 *        The parameter_ptr variable is a pointer to the structure    *
 *        used to represent a parameter in a PDS label.               *
 *$Detailed_Description                                               *
 *    The lab_move_parameter routine will move the specified          *
 *    parameter to a new location in its parent object.  The parent   *
 *    object and the parameter are located using the standard search  *
 *    rules for the PDS Label Library.  The parameter is then         *
 *    relocated to the absolute position specified by new_position.   *
 *$Error_Handling                                                     *
 *    1) If the object or parameter cannot be found, or memory cannot *
 *       be allocated for the new value, then the label_status        *
 *       is set to PDS_ERROR and a NULL value is returned.            *
 *    2) If more than one object is found that matches the            *
 *       specifications passed in for the object, then the            *
 *       label_status is set to PDS_MULTIPLE_OBJECTS and a NULL       *
 *       value is returned.                                           *
 *    3) If more than one parameter is found that matches the         *
 *       specifications passed in for the parameter, then the         *
 *       label_status is set to PDS_MULTIPLE_PARMS and a NULL         *
 *       value is returned.                                           *
 *    4) If everything went according to plan, then the label_status  *
 *       is set to PDS_SUCCESS and a pointer to the new parameter is  *
 *       returned.                                                    *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/ J.P.L                                          *
 *$Version_and_Date                                                   *
 *    1.0   November 4, 1991                                          *
 *$Change_History                                                     *
 *    MDD   11-04-91   Original Code.                                 *
 **********************************************************************/

PARAMETER lab_move_parameter (label_ptr, object_class, object_name, object_position,
                              parameter_name, parameter_position, new_position,
                              label_status)

AGGREGATE label_ptr;
char *object_class;
char *object_name;
int object_position;
char *parameter_name;
int parameter_position;
int new_position;
int *label_status;
{
   AGGREGATE object_ptr;
   PARAMETER parameter_ptr;

/** BEGIN **/

   /*----------------------------------------------------------------------*/
   /** IF the parameter to be moved can be found THEN                     **/
   /**    cut it from its former position                                 **/
   /**    paste it at the new location                                    **/
   /** ENDIF                                                              **/
   /*----------------------------------------------------------------------*/

   parameter_ptr = lab_find_parameter (label_ptr, object_class, object_name,
                   object_position, parameter_name, parameter_position,
                   label_status);
   if (*label_status == PDS_SUCCESS && parameter_ptr != NULL)
   {
      object_ptr = parameter_ptr -> owner;
      parameter_ptr = CutParameter(parameter_ptr);
      lu_paste_parm_at_pos(object_ptr, parameter_ptr, new_position);
   }
   return (parameter_ptr);

/** END **/
}



/**********************************************************************
 *$Component                                                          *
 *    void lab_print_messages ()                                      *
 *$Abstract                                                           *
 *    Prints the contents of the global message list.                 *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    ERROR                                                           *
 *    PRINT                                                           *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    None                                                            *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    The lab_print_messages routine displays the contents of the     *
 *    global message list.                                            *
 *$Error_Handling                                                     *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.0   April 10, 1991                                            *
 *$Change_History                                                     *
 *    DPB   04-10-91   Original Code.                                 *
 **********************************************************************/

void lab_print_messages ()

{
    err_write_to_file (NULL, TRUE);

}  /*  lab_print_messages"  */



/**********************************************************************
 *$Component                                                          *
 *    LOGICAL lab_print_pds_label (label_ptr)                         *
 *$Abstract                                                           *
 *    Prints out a PDS Label to the screen.                           *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    PRINT                                                           *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    label_ptr:                                                      *
 *        The label_ptr variable is a pointer to the root of an       *
 *        ODL tree.  Usually it points to the actual "ROOT" object,   *
 *        but it may point to any other object in the tree.  If it    *
 *        does point to something other than the "ROOT" object, then  *
 *        this object will be treated as the root of a sub-tree, and  *
 *        processing will not be allowed to move above the level of   *
 *        this object.                                                *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure        *
 *        (a value of FALSE).                                         *
 *$Detailed_Description                                               *
 *    The lab_print_pds_label routine displays the contents of an     *
 *    ODL tree in the format of a PDS label.                          *
 *$Error_Handling                                                     *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Herbert C. Gamble / J.P.L.                                      *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.0   April 10, 1991                                            *
 *$Change_History                                                     *
 *    HCG   03-08-91   Original Code.                                 *
 *    DPB   04-10-91   Restructured routine.                          *
 **********************************************************************/

LOGICAL lab_print_pds_label (label_ptr)

AGGREGATE label_ptr;

{
    if (label_ptr != NULL)
        PrintLabel(label_ptr);

    return (label_ptr != NULL);

}  /*  "lab_print_pds_label"  */


/**********************************************************************
 *$Component                                                          *
 *    AGGREGATE lab_read_label_or_template (input_fname)              *
 *$Abstract                                                           *
 *    Builds an ODL tree from a label file.                           *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    OBJECT                                                          *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    input_fname:                                                    *
 *        The input_fname variable is the name of the file that       *
 *        contains the input to be processed by a routine.            *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    label_ptr:                                                      *
 *        The label_ptr variable is a pointer to the root of an       *
 *        ODL tree.  Usually it points to the actual "ROOT" object,   *
 *        but it may point to any other object in the tree.  If it    *
 *        does point to something other than the "ROOT" object, then  *
 *        this object will be treated as the root of a sub-tree, and  *
 *        processing will not be allowed to move above the level of   *
 *        this object.                                                *
 *$Detailed_Description                                               *
 *    The lab_read_label_or_template routine reads a file containing  *
 *    a PDS label and returns a pointer to an ODL tree which          *
 *    represents the label.                                           *
 *$Error_Handling                                                     *
 *    If the input file cannot be opened, or if the tree cannot be    *
 *    created, or if there is nothing in the input file, then a       *
 *    NULL pointer is returned.                                       *
 *$Side_Effects                                                       *
 *    This routine causes memory to be allocated which must be freed  *
 *    by the calling routine.                                         *
 *$External_References                                                *
 *    Item                     Shared-Data           Access           *
 *    ------------------------------------------------------------    *
 *    ODLroot_node             odldef.h              update           *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.0   April 10, 1991                                            *
 *$Change_History                                                     *
 *    DPB   04-10-91   Original Code.                                 *
 **********************************************************************/

AGGREGATE lab_read_label_or_template (input_fname)
  
char *input_fname;

{
    AGGREGATE label_ptr = {NULL};
    FILE *file_ptr = {NULL};

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Initialize the global ODL root pointer                              **/
    /*-----------------------------------------------------------------------*/

    ODLroot_node = NULL;                                              

    /*-----------------------------------------------------------------------*/
    /** Try to open the input file.                                         **/
    /*-----------------------------------------------------------------------*/

    file_ptr = fopen (input_fname, "r");
    
    /*-----------------------------------------------------------------------*/
    /** IF the file could be opened THEN                                    **/
    /*-----------------------------------------------------------------------*/

    if (file_ptr != NULL)
    {
        /*-------------------------------------------------------------------*/
        /** Try to create a ROOT node for the new tree.                     **/
        /*-------------------------------------------------------------------*/

        label_ptr = NewAggregate (NULL, KA_OBJECT, "ROOT", "");

        /*-------------------------------------------------------------------*/
        /** IF the root was created THEN                                    **/
        /*-------------------------------------------------------------------*/

        if (label_ptr != NULL)
        {
            /*---------------------------------------------------------------*/
            /** Try to read and parse the label and build the ODL tree.     **/
            /*---------------------------------------------------------------*/

            ReadLabel (file_ptr, label_ptr);

            /*---------------------------------------------------------------*/
            /** Check to see if a tree was created.  We can tell if this    **/
            /**     happened by seeing if the root object has any children  **/
            /**     or parameters.                                          **/
            /** IF a tree was not created THEN                              **/
            /**     Remove the root object and re-set the global pointer.   **/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

            if ((label_ptr -> first_child == NULL) &&
                    (label_ptr -> first_parameter == NULL))
            {
                label_ptr = lab_remove_label_or_template (label_ptr);
                ODLroot_node = NULL;
            }

        /*-------------------------------------------------------------------*/
        /** ENDIF                                                           **/
        /*-------------------------------------------------------------------*/

        }  /*  End:  "if (label_ptr == NULL) ... else ..."  */
    
        /*-------------------------------------------------------------------*/
        /** Close the input file                                            **/
        /*-------------------------------------------------------------------*/

        fclose (file_ptr);

    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if (file_ptr != NULL) ..."  */

    /*-----------------------------------------------------------------------*/
    /** RETURN a pointer to the root object in the ODL tree.                **/
    /*-----------------------------------------------------------------------*/

    return (label_ptr);

/** END **/

}  /*  "lab_read_label_or_template"  */

/**********************************************************************
 *$Component                                                          *
 *    AGGREGATE lab_remove_label_or_template (label_ptr)              *
 *$Abstract                                                           *
 *    Deallocates all objects in an ODL tree.                         *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    REMOVE                                                          *
 *    ODL                                                             *
 *    TREE                                                            *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    label_ptr:                                                      *
 *        The label_ptr variable is a pointer to the root of an       *
 *        ODL tree.  Usually it points to the actual "ROOT" object,   *
 *        but it may point to any other object in the tree.  If it    *
 *        does point to something other than the "ROOT" object, then  *
 *        this object will be treated as the root of a sub-tree, and  *
 *        processing will not be allowed to move above the level of   *
 *        this object.                                                *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    label_ptr:                                                      *
 *        The label_ptr variable is a pointer to the root of an       *
 *        ODL tree.  Usually it points to the actual "ROOT" object,   *
 *        but it may point to any other object in the tree.  If it    *
 *        does point to something other than the "ROOT" object, then  *
 *        this object will be treated as the root of a sub-tree, and  *
 *        processing will not be allowed to move above the level of   *
 *        this object.                                                *
 *$Detailed_Description                                               *
 *    The lab_remove_label_or_template routine deallocates all        *
 *    storage used by an ODL tree.                                    *
 *$Error_Handling                                                     *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    This routine causes memory to be deallocated.                   *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Herbert C. Gamble / J.P.L.                                      *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.0   April 10, 1991                                            *
 *$Change_History                                                     *
 *    HCG   03-08-91   Original Code.                                 *
 *    DPB   04-10-91   Restructured routine.                          *
 **********************************************************************/

AGGREGATE lab_remove_label_or_template (label_ptr)

AGGREGATE label_ptr;

{
    while (label_ptr != NULL)
        label_ptr = RemoveAggregate (label_ptr);

    return (label_ptr);

}  /*  "lab_remove_label_or_template"  */ 


/**********************************************************************
 *$Component                                                          *
 *    AGGREGATE lab_remove_object (label_ptr, object_class,           *
 *                                 object_name, object_position,      *
 *                                 label_status)                      *
 *$Abstract                                                           *
 *    Removes an object from a PDS Label.                             *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    REMOVE                                                          *
 *    OBJECT                                                          *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    label_ptr:                                                      *
 *        The label_ptr variable is a pointer to the root of an       *
 *        ODL tree.  Usually it points to the actual "ROOT" object,   *
 *        but it may point to any other object in the tree.  If it    *
 *        does point to something other than the "ROOT" object, then  *
 *        this object will be treated as the root of a sub-tree, and  *
 *        processing will not be allowed to move above the level of   *
 *        this object.                                                *
 *    object_class:                                                   *
 *        The object_class variable is a character string             *
 *        which contains the class of an object in a PDS label        *
 *        (e.g., "OBJECT = IMAGE" implies that "IMAGE" is the class   *
 *        of the object).                                             *
 *    object_name:                                                    *
 *        The object_name variable is a character string              *
 *        which contains the name of an object in a PDS label.        *
 *        This is assumed to be the value of the "NAME" parameter     *
 *        in the object (e.g., "NAME = VOYAGER" implies that          *
 *        "VOYAGER" is the name of the object).                       *
 *    object_position:                                                *
 *        The object_position variable is an integer which            *
 *        represents the relative position of an object in a PDS      *
 *        label.  If this variable is used in conjunction with either *
 *        the object_class or object_name variables, then it          *
 *        represents a particular occurrence of that thing in the     *
 *        label (e.g., if object_class is "TABLE" and object_position *
 *        is 3, then this represents the third "TABLE" object in the  *
 *        label).  On the other hand, if this variable is used by     *
 *        itself, it represents the absolute position of the object,  *
 *        starting from the "ROOT" object (position = 1).  The        *
 *        sequence follows the structure of the label as it looks     *
 *        in a flat file.                                             *
 *$Outputs                                                            *
 *    label_status:                                                   *
 *        The label_status variable is an integer which is used to    *
 *        indicate the success or failure of a PDS label routine,     *
 *        and whether or not the object or parameter specifications   *
 *        passed in were ambiguous.  Valid values are:  PDS_SUCCESS,  *
 *        PDS_ERROR, PDS_MULTIPLE_OBJECTS, and PDS_MULTIPLE_PARMS.    *
 *$Returns                                                            *
 *    parent_object_ptr:                                              *
 *        The parent_object_ptr variable is a pointer to the          *
 *        structure used to represent the parent of an object in a    *
 *        PDS label.                                                  *
 *$Detailed_Description                                               *
 *    The lab_remove_object routine removes an object from a PDS      *
 *    label.  It searches for the object, deallocates its storage,    *
 *    and returns a pointer to its parent.  The object may by         *
 *    specified by class, name, or position, or by any combination    *
 *    of these.  The search for the object begins at the object       *
 *    pointed to by the label_ptr variable.  This is usually the      *
 *    "ROOT" object, but may be any other object in the tree.         *
 *    Please note that it is not necessary to specify the object's    *
 *    name, class, and position at the same time.  In fact, if you    *
 *    specify only one of them and pass in zero for the others, the   *
 *    routine will work just fine.                                    *
 *$Error_Handling                                                     *
 *    1) If the object cannot be found then the label_status          *
 *       is set to PDS_ERROR and a NULL value is returned.            *
 *    2) If more than one object is found that matches the            *
 *       specifications passed in for the parent object, then the     *
 *       label_status is set to PDS_MULTIPLE_OBJECTS and a NULL       *
 *       value is returned.                                           *
 *    3) If everything went according to plan, then the label_status  *
 *       is set to PDS_SUCCESS and a pointer to the parent of the     *
 *       object is <returned.                                          *
 *    4) If the object is the "ROOT" object, then the entire tree     *
 *       is deallocated, the label_status is set to PDS_SUCCESS,      *
 *       and a NULL pointer is returned.                              *
 *$Side_Effects                                                       *
 *    This routine causes memory to be deallocated.                   *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Herbert C. Gamble / J.P.L.                                      *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   April 26, 1991                                            *
 *$Change_History                                                     *
 *    HCG   03-08-91   Original Code.                                 *
 *    DPB   04-10-91   Restructured routine.                          *
 *    DPB   04-26-91   Added code to strip blanks from inputs.        *
 **********************************************************************/

AGGREGATE lab_remove_object (label_ptr, object_class, 
                             object_name, object_position, label_status)

AGGREGATE label_ptr;
char *object_class;
char *object_name;
int object_position;
int *label_status;

{
    AGGREGATE object_ptr = {NULL};
    AGGREGATE parent_object_ptr = {NULL};

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Initialize the label_status variable.                               **/
    /*-----------------------------------------------------------------------*/

    *label_status = PDS_ERROR;

    /*-----------------------------------------------------------------------*/
    /** Remove leading and trailing blanks from character inputs.           **/
    /*-----------------------------------------------------------------------*/

    util_strip_lead_and_trail (object_class, ' ');
    util_strip_lead_and_trail (object_name, ' ');

    /*-----------------------------------------------------------------------*/
    /** Try to find an object which meets the specifications passed in.     **/
    /*-----------------------------------------------------------------------*/

    object_ptr = lab_find_object (label_ptr, object_class, 
                                  object_name, object_position, label_status);

    /*-----------------------------------------------------------------------*/
    /** IF an object was found THEN                                         **/
    /**     Save a pointer to the object's parent and deallocate the        **/
    /**         object and all of its children.                             **/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    if ((object_ptr != NULL) && (*label_status == PDS_SUCCESS))
    {
        parent_object_ptr = object_ptr -> parent;
        RemoveAggregate (object_ptr);
    }

    /*-----------------------------------------------------------------------*/
    /** RETURN a pointer to the object's parent.                            **/
    /*-----------------------------------------------------------------------*/

    return (parent_object_ptr);

/** END **/

}  /*  "lab_remove_object"  */


/**********************************************************************
 *$Component                                                          *
 *    AGGREGATE lab_remove_parameter (label_ptr, object_class,        *
 *                                object_name, object_position,       *
 *                                parameter_name, parameter_position, *
 *                                label_status)                       *
 *$Abstract                                                           *
 *    Removes a parameter from a PDS Label.                           *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    REMOVE                                                          *
 *    PARAMETER                                                       *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    label_ptr:                                                      *
 *        The label_ptr variable is a pointer to the root of an       *
 *        ODL tree.  Usually it points to the actual "ROOT" object,   *
 *        but it may point to any other object in the tree.  If it    *
 *        does point to something other than the "ROOT" object, then  *
 *        this object will be treated as the root of a sub-tree, and  *
 *        processing will not be allowed to move above the level of   *
 *        this object.                                                *
 *    object_class:                                                   *
 *        The object_class variable is a character string             *
 *        which contains the class of an object in a PDS label        *
 *        (e.g., "OBJECT = IMAGE" implies that "IMAGE" is the class   *
 *        of the object).                                             *
 *    object_name:                                                    *
 *        The object_name variable is a character string              *
 *        which contains the name of an object in a PDS label.        *
 *        This is assumed to be the value of the "NAME" parameter     *
 *        in the object (e.g., "NAME = VOYAGER" implies that          *
 *        "VOYAGER" is the name of the object).                       *
 *    object_position:                                                *
 *        The object_position variable is an integer which            *
 *        represents the relative position of an object in a PDS      *
 *        label.  If this variable is used in conjunction with either *
 *        the object_class or object_name variables, then it          *
 *        represents a particular occurrence of that thing in the     *
 *        label (e.g., if object_class is "TABLE" and object_position *
 *        is 3, then this represents the third "TABLE" object in the  *
 *        label).  On the other hand, if this variable is used by     *
 *        itself, it represents the absolute position of the object,  *
 *        starting from the "ROOT" object (position = 1).  The        *
 *        sequence follows the structure of the label as it looks     *
 *        in a flat file.                                             *
 *    parameter_name:                                                 *
 *        The parameter_name variable is a character string           *
 *        which contains the name of a parameter in a PDS label       *
 *        (e.g., the line "SCID = VG1" implies that "SCID" is the     *
 *        parameter name).                                            *
 *    parameter_position:                                             *
 *        The parameter_position variable is an integer which         *
 *        represents the relative position of a parameter in an       *
 *        object in a PDS label.  If this variable is used in         *
 *        conjunction with the parameter_name variable, then it       *
 *        represents a particular occurrence of that parameter        *
 *        in the object (e.g., if parameter_name is "SCID" and        *
 *        parameter_position is 2, then this represents the second    *
 *        "SCID" parameter in the object).  On the other hand, if     *
 *        this variable is used by itself, it represents the absolute *
 *        position of the parameter within the object, starting with  *
 *        first parameter in the object (position = 1).               *
 *$Outputs                                                            *
 *    label_status:                                                   *
 *        The label_status variable is an integer which is used to    *
 *        indicate the success or failure of a PDS label routine,     *
 *        and whether or not the object or parameter specifications   *
 *        passed in were ambiguous.  Valid values are:  PDS_SUCCESS,  *
 *        PDS_ERROR, PDS_MULTIPLE_OBJECTS, and PDS_MULTIPLE_PARMS.    *
 *$Returns                                                            *
 *    object_ptr:                                                     *
 *        The object_ptr variable is a pointer to the structure       *
 *        used to represent an object in a PDS label.                 *
 *$Detailed_Description                                               *
 *    The lab_remove_parameter routine removes a parameter from a PDS *
 *    label.  The routine searches the ODL tree for the object        *
 *    specified, searches for the parameter specified, and removes    *
 *    the parameter from the list.  The object may by specified by    *
 *    class, name, or position, or by any combination of these, and   *
 *    the parameter may be specified by name or position, or both.    *
 *    The search for the object begins at the object pointed to by    *
 *    the label_ptr variable.  This is usually the "ROOT" object, but *
 *    may be any other object in the tree.  Please note that it is    *
 *    not necessary to specify the object's name, class, and position *
 *    at the same time.  In fact, if you specify only one of them     *
 *    and pass in zero for the others, the routine will work just     *
 *    fine.  This also applies to the parameter name and position.    *
 *$Error_Handling                                                     *
 *    1) If the object cannot be found then the label_status          *
 *       is set to PDS_ERROR and a NULL value is returned.            *
 *    2) If more than one object is found that matches the            *
 *       specifications passed in for the object, then the            *
 *       label_status is set to PDS_MULTIPLE_OBJECTS and a NULL       *
 *       value is returned.                                           *
 *    3) If more than one parameter is found that matches the         *
 *       specifications passed in for the parameter, then the         *
 *       label_status is set to PDS_MULTIPLE_PARMS and a NULL         *
 *       value is returned.                                           *
 *    4) If everything went according to plan, then the label_status  *
 *       is set to PDS_SUCCESS, and a pointer to the object which     *
 *       owns the parameter is returned.                              *
 *$Side_Effects                                                       *
 *    This routine causes memory to be deallocated.                   *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Herbert C. Gamble / J.P.L.                                      *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   April 26, 1991                                            *
 *$Change_History                                                     *
 *    HCG   03-08-91   Original Code.                                 *
 *    DPB   04-10-91   Restructured routine.                          *
 *    DPB   04-26-91   Added code to strip blanks from inputs.        *
 **********************************************************************/

AGGREGATE lab_remove_parameter (label_ptr, object_class, object_name, 
                                object_position, parameter_name, 
                                parameter_position, label_status)

AGGREGATE label_ptr;
char *object_class;
char *object_name;
int object_position;
char *parameter_name;
int parameter_position;
int *label_status;

{
    AGGREGATE object_ptr = {NULL};
    PARAMETER parameter_ptr = {NULL};

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Initialize the label_status variable.                               **/
    /*-----------------------------------------------------------------------*/

    *label_status = PDS_ERROR;

    /*-----------------------------------------------------------------------*/
    /** Remove leading and trailing blanks from character inputs.           **/
    /*-----------------------------------------------------------------------*/

    util_strip_lead_and_trail (object_class, ' ');
    util_strip_lead_and_trail (object_name, ' ');
    util_strip_lead_and_trail (parameter_name, ' ');

    /*-----------------------------------------------------------------------*/
    /** Try to locate the parameter to be removed.                          **/
    /*-----------------------------------------------------------------------*/

    parameter_ptr = lab_find_parameter (label_ptr, object_class, 
                                        object_name, object_position, 
                                        parameter_name, parameter_position, 
                                        label_status);

    /*-----------------------------------------------------------------------*/
    /** IF the parameter was found THEN                                     **/
    /**     Save a pointer to the object which owns it and deallocate the   **/
    /**         storage used by it and all of its values.                   **/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    if ((parameter_ptr != NULL) && (*label_status == PDS_SUCCESS))
    {
        object_ptr = parameter_ptr -> owner;
        RemoveParameter (parameter_ptr);
    }

    /*-----------------------------------------------------------------------*/
    /** RETURN a pointer to the object which owned the parameter            **/
    /*-----------------------------------------------------------------------*/

    return (object_ptr);

/** END **/

}  /*  "lab_remove_parameter"  */


/**********************************************************************
 *$Component                                                          *
 *    FILE *lab_skip_sfdus (input_fname, line_offset)                 *
 *$Abstract                                                           *
 *    Skips any SFDUs in an input file                                *
 *$Keywords                                                           *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    input_fname:                                                    *
 *        The input_fname variable is the name of the file that       *
 *        contains the input to be processed by a routine.            *
 *$Outputs                                                            *
 *    line_offset:                                                    *
 *        The line_offset variable is the number of lines that have   *
 *        been skipped in the input file because an SFDU was found.   *
 *        This number is assigned to pds_line_offset and used to      *
 *        report the correct line numbers in the error messages of the*
 *        label verifier report.                                      *
 *$Returns                                                            *
 *    input_ptr:                                                      *
 *        The input_ptr variable is a pointer to the file that        *
 *        contains the input to be processed by a routine.            *
 *$Detailed_Description                                               *
 *    The lab_skip_sfdus routine reads the contents of a file one line*
 *    at a time. The routine looks for the first non-blank non-comment*
 *    line and checks for SFDUs. If an SFDU is encountered, a file    *
 *    pointer is returned to the beginning of the next line, and all  *
 *    blank lines and comments up to and including the SFDU are       *
 *    skipped.  If no SFDUs are encountered, or if the SFDU is NOT the*
 *    first non-blank, non-comment line, the file is rewound and a    *
 *    pointer to the beginning of the file is returned. The           *
 *    line_offset output is set to the number of lines skipped.       *
 *$Error_Handling                                                     *
 *    If this routine cannot open the input file then an error        *
 *    will be appended onto the global message list. If this file     *
 *    contains SFDUs, a message will be appended onto the global      *
 *    message list stating which lines were skipped, and where the    *
 *    processing will begin. If lines are truncated during processing *
 *    a warning message is written, and processing continues even     *
 *    though the results are unpredictable.                           *
 *$Author_and_Institution                                             *
 *    Kristy L. Marski /J.P.L.                                        *
 *$Version_and_Date                                                   *
 *    1.0   June 10, 1991                                             *
 *$Change_History                                                     *
 *    KLM   06-10-91   Original code.                                 *
 **********************************************************************/
FILE *lab_skip_sfdus (input_fname, line_offset)

char *input_fname;
int *line_offset;

{
   FILE *input_ptr;
   LOGICAL has_sfdus = FALSE;
   LOGICAL lines_truncated = FALSE;
   char err_msg [PDS_MAXLINE + 1];
   static char temp_str [3] = "/*";
   char input_line [PDS_MAXRECORD + 2];
   char temp_line [PDS_MAXRECORD + 2];
   int i = 0;

   /** BEGIN **/

   /*--------------------------------------------------------------------*/
   /** IF the input file cannot be opened THEN                          **/
   /**    print an error                                                **/
   /*--------------------------------------------------------------------*/
                                           
   if ((input_ptr = fopen (input_fname, "r")) == NULL)
   {
      sprintf (err_msg, "File %s cannot be opened for reading", input_fname);
      err_append_message (ERROR, err_msg);
   } 
   /*--------------------------------------------------------------------*/
   /** ELSE                                                             **/
   /*--------------------------------------------------------------------*/

   else
   {
      /*-----------------------------------------------------------------*/
      /** get the first line of the input file                          **/
      /** copy the unmodified line to a temporary location              **/
      /** IF the record is truncated THEN                               **/
      /**    set lines_truncated to TRUE                                **/
      /** ENDIF                                                         **/
      /** get rid of blanks and terminating characters                  **/
      /*-----------------------------------------------------------------*/

      fgets(input_line, PDS_MAXRECORD + 2, input_ptr);
      strcpy (temp_line, input_line);
      i++;
      if (strlen (input_line) > PDS_MAXRECORD)
      {
         lines_truncated = TRUE;
      } 
      util_remove_char (input_line, '\n');
      util_remove_char (input_line, '\r');
      util_strip_lead_and_trail (input_line, ' ');

      /*-----------------------------------------------------------------*/
      /** WHILE the input line is a blank line or a comment             **/
      /**    copy the unmodified line to a temporary location           **/
      /**    IF the line is too long THEN                               **/
      /**       set the lines_truncated flag to TRUE                    **/
      /**    ENDIF                                                      **/
      /**    get rid of blanks and terminating characters               **/
      /*-----------------------------------------------------------------*/

      while (util_string_is_empty (input_line) ||
               strncmp (input_line, temp_str, 2) == 0)
      {
         fgets(input_line, PDS_MAXRECORD + 2, input_ptr);
         strcpy (temp_line, input_line);
         if (strlen (input_line) > PDS_MAXRECORD)
         {
            lines_truncated = TRUE;
         } 
         util_strip_lead_and_trail (input_line, ' ');
         util_remove_char (input_line, '\n'); 
         util_remove_char (input_line, '\r');
         i++;

      /*-----------------------------------------------------------------*/
      /** ENDWHILE the input line is a blank line or a comment          **/
      /*-----------------------------------------------------------------*/

      }

      /*-----------------------------------------------------------------*/
      /** remove any blanks from the line                               **/
      /** IF there is an "=SFDU" in the line THEN                       **/
      /**    set the has_sfdu flag to TRUE                              **/
      /** ELSE IF there is an "=PDS_SFDU_LABEL" THEN                    **/
      /**    set the has_sfdu flag to TRUE                              **/
      /** ELSE IF there is an "CCSD" THEN                               **/
      /**    set the has_sfdu flag to TRUE                              **/
      /** ELSE IF there is an "NJPL" THEN                               **/
      /**    set the has_sfdu flag to TRUE                              **/
      /** ENDIF                                                         **/
      /*-----------------------------------------------------------------*/

      util_remove_char (input_line, ' ');
      if (util_locate_substring (input_line, "=SFDU") != NULL)
      {
         has_sfdus = TRUE;
      }
      else if (util_locate_substring (input_line, "=PDS_SFDU_LABEL") != NULL)
      {
         has_sfdus = TRUE;
      }
      else if (util_locate_substring (input_line, "CCSD") != NULL)
      {
         has_sfdus = TRUE;
      }
      else if (util_locate_substring (input_line, "NJPL") != NULL)
      {
         has_sfdus = TRUE;
      } 

      /*-----------------------------------------------------------------*/
      /** set the line offset counter                                   **/
      /** IF there there were lines truncated THEN                      **/
      /**    construct the error message                                **/
      /** ENDIF                                                         **/
      /*-----------------------------------------------------------------*/

      *line_offset = i;
      if (lines_truncated)
      {
         sprintf (err_msg, "Lines were truncated when skipping SFDU labels. Results will be unpredictable");
         err_append_message (WARNING, err_msg);
      }

      /*-----------------------------------------------------------------*/
      /** IF there was an SFDU and we're in verbose mode THEN           **/
      /**    construct the error message                                **/
      /*-----------------------------------------------------------------*/

      if (has_sfdus && pds_verbose)
      {
         err_object_message (WARNING, "SFDU label skipped", i, temp_line);
      }

      /*-----------------------------------------------------------------*/
      /** ELSE IF there were no SFDUs THEN                              **/
      /**    rewind the file                                            **/
      /**    set the line offset counter to 0                           **/
      /** ENDIF                                                         **/
      /*-----------------------------------------------------------------*/

      else if (! has_sfdus)
      {
         rewind (input_ptr);
         *line_offset = 0;
      }
   /*--------------------------------------------------------------------*/
   /** ENDIF the file couldn't be opened                                **/
   /** return the file pointer                                          **/
   /*--------------------------------------------------------------------*/
   }
   return (input_ptr);

/** END lab_skip_sfdus **/
} 



/**********************************************************************
 *$Component                                                          *
 * LOGICAL lab_write_label_or_template (label_ptr, new_record_type,   *
 *                                      record_length, file_name)     *
 *$Abstract                                                           *
 *    Writes a label or template without SFDU labels to a file        *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    WRITE                                                           *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    label_ptr:                                                      *
 *        The label_ptr variable is a pointer to the root of an       *
 *        ODL tree.  Usually it points to the actual "ROOT" object,   *
 *        but it may point to any other object in the tree.  If it    *
 *        does point to something other than the "ROOT" object, then  *
 *        this object will be treated as the root of a sub-tree, and  *
 *        processing will not be allowed to move above the level of   *
 *        this object.                                                *
 *    new_record_type:                                                *
 *        The new_record_type variable is an integer that represents  *
 *        the type of records a file contains: e.g., PDS_RF_STREAM_LF,*
 *        PDS_RF_FIXED_CRLF, etc.                                     *
 *    record_length:                                                  *
 *        The record_length variable is an integer that holds the     *
 *        record length value to be used for a fixed file.            *
 *    file_name:                                                      *
 *        The file_name variable is a general purpose character       *
 *        string containing a file specification.                     *
 *$Outputs                                                            *
 *    NONE                                                            *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure        *
 *        (a value of FALSE).                                         *
 *$Detailed_Description                                               *
 *    The lab_write_label_or_template routine writes the ODL label    *
 *    pointed to by label_ptr to the given output file and then       *
 *    converts the file to the indicated record type.  If this routine*
 *    fails to open or convert the output file, it returns FALSE.     *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/JPL                                             *
 *$Version_and_Date                                                   *
 *    1.1   September 24, 1991                                        *
 *$Change_History                                                     *
 *    MDD   05-13-91   Original Code.                                 *
 *    DPB   09-24-91   Changed call to fio_convert_file.              *
 **********************************************************************/


LOGICAL lab_write_label_or_template (label_ptr, new_record_type, record_length,
                                     file_name)

AGGREGATE label_ptr;
int new_record_type;
int record_length;
char *file_name;

{
   LOGICAL success = FALSE;
   FILE *file_ptr;

/** BEGIN **/

   /*-------------------------------------------------------------------*/
   /** IF a label and file name were passed in THEN                    **/
   /*-------------------------------------------------------------------*/

   if (label_ptr != NULL && file_name != NULL)
   {

      /*----------------------------------------------------------------*/
      /** strip blanks from all character strings                      **/
      /** IF the output file cannot be opened THEN                     **/
      /**   print an error                                             **/
      /*----------------------------------------------------------------*/

      util_strip_lead_and_trail (file_name, ' ');
      file_ptr = fopen (file_name, "w");
      if (file_ptr == NULL)
      {
         err_append_message (ERROR, 
              "Could not open output file in order to write label.");
      }
      /*----------------------------------------------------------------*/
      /** ELSE                                                         **/
      /**   write the ODL label to the file                            **/
      /**   convert the file to the necessary record type              **/
      /*----------------------------------------------------------------*/

      else
      {
         WriteLabel (file_ptr, label_ptr);
         fclose (file_ptr);
         success = fio_convert_file (file_name, DEFAULT_REC_TYPE, 
                                      new_record_type, 0, record_length);

      }
      /*----------------------------------------------------------------*/
      /** ENDIF the label file cannot be opened...                     **/
      /*----------------------------------------------------------------*/
   }
   /*-------------------------------------------------------------------*/
   /** ENDIF                                                           **/
   /*-------------------------------------------------------------------*/

   return (success);

/** END lab_write_label_or_template **/
}
      

/**********************************************************************
 *$Component                                                          *
 *    LOGICAL lab_write_product_label (label_ptr, top_ddid,           *
 *                     bottom_ddid, version_id, label_type,           *
 *                     new_record_type, record_length,                *
 *                     file_name)                                     *
 *$Abstract                                                           *
 *    Writes out a product label to an output label file.             *
 *$Keywords                                                           *
 *    LABEL                                                           *
 *    WRITE                                                           *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    label_ptr:                                                      *
 *        The label_ptr variable is a pointer to the root of an       *
 *        ODL tree.  Usually it points to the actual "ROOT" object,   *
 *        but it may point to any other object in the tree.  If it    *
 *        does point to something other than the "ROOT" object, then  *
 *        this object will be treated as the root of a sub-tree, and  *
 *        processing will not be allowed to move above the level of   *
 *        this object.                                                *
 *    top_ddid:                                                       *
 *        The top_ddid variable is a pointer to a character           *
 *        string   that holds the data description ID (DDID) of an    *
 *        SFDU label. This variable should be four bytes long.        *
 *    bottom_ddid:                                                    *
 *        The bottom_ddid variable is a pointer to a character        *
 *        string   that holds the data description ID (DDID) of an    *
 *        SFDU label. This variable should be four bytes long.        *
 *    version_id:                                                     *
 *        The version_id variable is an integer that holds the        *
 *        version ID of an SFDU label (1 or 3).                       * 
 *    label_type:                                                     *
 *        The label_type variable is a pointer to a character         *
 *        string that contains the type of an SFDU label (ZK, ZI, or  *
 *        NO_SFDUS)                                                   *
 *    new_record_type:                                                *
 *        The new_record_type variable is an integer that represents  *
 *        the type of records a file contains: e.g., PDS_RF_STREAM_LF,*
 *        PDS_RF_FIXED_CRLF, etc.                                     *
 *    record_length:                                                  *
 *        The record_length variable is an integer that holds the     *
 *        record length value to be used for a fixed file.            *
 *    file_name:                                                      *
 *        The file_name variable is a general purpose character       *
 *        string containing a file specification.                     *
 *$Outputs                                                            *
 *    NONE                                                            *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure        *
 *        (a value of FALSE).                                         *
 *$Detailed_Description                                               *
 *        This routine is a router.  It merely calls the appropriate  *
 *        label write routine  based on the input arguments.          *
 *        This routine handles version 1 and version 3 "ZI" label     *
 *        structures, version 3 "ZK" strcutures, and "NO_SFDUS".      *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/JPL                                             *
 *$Version_and_Date                                                   *
 *    1.1   June 17, 1991                                             *
 *$Change_History                                                     *
 *    MDD   05-13-91   Original Code.                                 *
      KLM   06-17-91   Changed calls from lab_write_z.._label to      *
 *                     lu_write_z.._label.                            *
 **********************************************************************/

LOGICAL lab_write_product_label (label_ptr, top_ddid, bottom_ddid, version_id,
                                 label_type, new_record_type, record_length, 
                                 file_name)
AGGREGATE label_ptr;
char *top_ddid;
char *bottom_ddid;
int version_id;
char *label_type;
int new_record_type;
int record_length;
char *file_name;
{
   LOGICAL success = FALSE;

   util_strip_lead_and_trail (label_type, ' ');
   util_upper_case (label_type);

   if (strcmp (label_type, "NO SFDUS") == 0) 
      success = lab_write_label_or_template (label_ptr, new_record_type, 
                                             record_length, file_name);
   else if (strcmp (label_type, "ZK") == 0 && version_id == 3)
      success = lu_write_zk3_label (label_ptr, top_ddid, bottom_ddid,
                                    new_record_type, record_length, file_name);
   else if (strcmp (label_type, "ZI") == 0 && version_id == 1)
      success = lu_write_zi1_label (label_ptr, top_ddid, new_record_type,
                                     record_length, file_name);
   else if (strcmp (label_type, "ZI") == 0 && version_id == 3)
      success = lu_write_zi3_label (label_ptr, top_ddid, new_record_type, 
                                     record_length, file_name);
   else
   {
      err_append_message (ERROR,
         "A label file could not be written to your specifications");
   }       
   return (success);
}
