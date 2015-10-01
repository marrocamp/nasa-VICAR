/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Component                                                           * 
 *    Library errorlib.c                                               *
 * Abstract                                                            *
 *    Low-level PDS error message utilities                            *
 * Detailed Description                                                *
 *    The errorlib is a library of subroutines used by PDS software    *
 *    to perform a variety of low-level operations on                  *
 *    error message lists.                                             *
 * Internal References                                                 *
 *    err_append_message                                               *
 *    err_deallocate_list                                              *
 *    err_deallocate_message                                           *
 *    err_keyword_message                                              *
 *    err_object_message                                               *
 *    err_write_to_file                                                *
 * Authors and Institutions                                            *
 *    David P. Bernath / J.P.L.                                        *
 * Version and Date                                                    *
 *    2.0  October 22, 1991                                            *
 * Change History                                                      *
 *    DPB   08-01-90   Library created.                                *
 *    DPB   08-21-90   Added global variable pds_out_of_memory.        *
 *    DPB   08-23-90   Eliminated individual library include files.    *
 *    DPB   08-29-90   Added err_append_stderr_messages.               *
 *    MDD   10-02-90   Added type casting for SUN compiler.            *
 *    MDD   03-14-91   Added err_write_to_stdout                       *
 *    KLM   03-25-91   Added err_write_to_file                         * 
 *    DPB   05-30-91   Added err_keyword_message and removed all       *
 *                     traces of pds_out_of_memory.                    *
 *    DPB   06-12-91   Added err_object_message.                       *
 *    MDD   10-17-91   Removed err_append_stderr_messages and all      *
 *                     calls to sys_exit_system. Delete err_write_to_  *
 *                     stdout in favor of err_write_to_file            *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "pdsdef.h"
#include "errordef.h"

extern ERROR_LIST *pds_message_list;
extern ERROR_LIST *pds_last_message;
extern LOGICAL pds_display;
extern int pds_error_count;
extern long pds_line_offset;


/**********************************************************************
 *$Component                                                          *
 *    void err_append_message (severity, message)                     *
 *$Abstract                                                           *
 *    Appends an error message to a list.                             *
 *$Keywords                                                           *
 *    ERRORLIB                                                        *
 *    ALLOCATE                                                        *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    severity:                                                       *
 *        The severity variable is an integer that indicates the      *
 *        level of severity of a message.  These severity levels      *
 *        are:  INFO, WARNING, ERROR, and FATAL_ERROR, and their      *
 *        actual values can be found in the include file: errordef.h. *
 *    message:                                                        *
 *        The message variable is a character string containing the   *
 *        text of the message that is to be appended to the list of   *
 *        messages.                                                   *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    The err_append_message subroutine allocates storage for         *
 *    a new ERROR_LIST structure, stores the message and severity     *
 *    information in this structure, and appends the structure onto   *
 *    the end of list of messages pointed to by pds_message_list.     *
 *$External_References                                                *
 *    Item                     Shared-Data           Access           *
 *    ------------------------------------------------------------    *
 *    pds_display              pdsglob.h             read             *
 *    pds_last_message         pdsglob.h             update           *
 *    pds_message_list         pdsglob.h             update           *
 *$Side_Effects                                                       *
 *    When the system runs out of memory and cannot allocate storage  *
 *    for a new error structure, the system will exit.                *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    2.3   October 17, 1991                                          *
 *$Change_History                                                     *
 *    DPB   08-01-90   Initial code.                                  *
 *    DPB   08-21-90   Added error checking when memory is allcoated. *
 *                     Added Error_Handling and Side_Effects sections *
 *                     to the header.                                 *
 *    MDD   10-02-90   Added type casting for SUN compiler            *
 *    DPB   06-13-91   Added code to display message to screen, and   *
 *                     added new global, pds_display.  (Modified      *
 *                     external references)                           *
 *    MDD   10-17-91   Replaced sys_exit_system with Check_Malloc and *
 *                     updated out of data header. Changed err_write_ *
 *                     to_stdout call to err_write_to_file            *
 **********************************************************************/

void err_append_message (severity, message)

int severity;
char *message;

{
    ERROR_LIST *new_ptr = {NULL};
    ERROR_LIST *save_ptr = {NULL};

/*--------------------------------------------------------------------------*/
/** BEGIN                                                                  **/
/**                                                                        **/
/** Attempt to allocate storage for a new ERROR_LIST structure.            **/
/*--------------------------------------------------------------------------*/

    new_ptr = (ERROR_LIST *) malloc (sizeof (ERROR_LIST));
    Check_Malloc(new_ptr);

/*--------------------------------------------------------------------------*/
/** Attempt to allocate storage for the message.                           **/
/*--------------------------------------------------------------------------*/

    Malloc_String(new_ptr -> message, String_Size(message));

/*--------------------------------------------------------------------------*/
/** Initialize the new error structure's fields.                           **/
/*--------------------------------------------------------------------------*/

    strcpy (new_ptr -> message, message);
    new_ptr -> severity = severity;
    new_ptr -> next = NUL;
    new_ptr -> prev = NUL;

/*--------------------------------------------------------------------------*/
/** IF there is nothing on the message list THEN                           **/
/**     Set the three global pointers to point to the new structure.       **/
/** ELSE                                                                   **/
/**     Append the new structure onto the end of the list.                 **/
/** ENDIF                                                                  **/
/*--------------------------------------------------------------------------*/

    if (pds_message_list == NUL)
    {
        pds_message_list = new_ptr;
        pds_last_message = new_ptr;
    }
    else
    {
        pds_last_message -> next = new_ptr;
        new_ptr -> prev = pds_last_message;
        pds_last_message = new_ptr;
    
    }  /*  End:  "if (pds_message_list == NUL) ... else ..."  */

/*--------------------------------------------------------------------------*/
/** IF the PDS display flag is set THEN write the error to the screen      **/
/*--------------------------------------------------------------------------*/

    if (pds_display)
    {
        save_ptr = pds_message_list;
        pds_message_list = pds_last_message;
        err_write_to_file (NULL, TRUE);
        pds_message_list = save_ptr;
    }

    return;

/*--------------------------------------------------------------------------*/
/** END                                                                    **/
/*--------------------------------------------------------------------------*/

}  /*  End:  "err_append_message"  */


/**********************************************************************
 *$Component                                                          *
 *    void err_deallocate_list (message_ptr)                          *
 *$Abstract                                                           *
 *    Deallocates a message list.                                     *
 *$Keywords                                                           *
 *    ERRORLIB                                                        *
 *    DEALLOCATE                                                      *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    message_ptr:                                                    *
 *        The message_ptr variable is a pointer to a structure in     *
 *        a list of ERROR_LIST structures.                            *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    The err_deallocate_list subroutine deallocates the              *
 *    storage used by a list of ERROR_LIST structures, starting       *
 *    with the structure pointed to by message_ptr.                   *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.0   Aug. 01, 1990                                             *
 *$Change_History                                                     *
 *    DPB   08-01-90   Initial code.                                  *
 **********************************************************************/

void err_deallocate_list (message_ptr)

ERROR_LIST *message_ptr;

{
    ERROR_LIST *delete_ptr = {NUL};

    while (message_ptr != NUL)
    {
        delete_ptr = message_ptr;           
        message_ptr = message_ptr -> next;
        err_deallocate_message (delete_ptr);

    }  /*  End:  "while (message_ptr != NUL) ..."  */

    return;

}  /*  End:  "err_deallocate_list"  */





/**********************************************************************
 *$Component                                                          *
 *    void err_deallocate_message (message_ptr)                       *
 *$Abstract                                                           *
 *    Removes a single message from a list.                           *
 *$Keywords                                                           *
 *    ERRORLIB                                                        *
 *    DEALLOCATE                                                      *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    message_ptr:                                                    *
 *        The message_ptr variable is a pointer to a structure in     *
 *        a list of ERROR_LIST structures.                            *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    The err_deallocate_message subroutine removes an ERROR_LIST     *
 *    structure from a list of these structures, and deallocates the  *
 *    storage it uses.                                                *
 *$External_References                                                *
 *    Item                     Shared-Data           Access           *
 *    ------------------------------------------------------------    *
 *    pds_last_message         pdsglob.h             update           *
 *    pds_message_list         pdsglob.h             update           *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.0   Aug. 01, 1990                                             *
 *$Change_History                                                     *
 *    DPB   08-01-90   Initial code.                                  *
 **********************************************************************/

void err_deallocate_message (message_ptr)

ERROR_LIST *message_ptr;

{

/*--------------------------------------------------------------------------*/
/** BEGIN                                                                  **/
/**                                                                        **/
/** IF the pointer to the structure is not NUL THEN                        **/
/*--------------------------------------------------------------------------*/

    if (message_ptr != NUL)
    {
/*--------------------------------------------------------------------------*/
/**     IF the structure to be deleted is the first one on the list THEN   **/
/*--------------------------------------------------------------------------*/

        if (message_ptr == pds_message_list)
        {
/*--------------------------------------------------------------------------*/
/**         IF the structure to be deleted is the last one on the list THEN **/
/**             Set the ``last message" global variable to NUL.            **/
/**         ENDIF                                                          **/
/*--------------------------------------------------------------------------*/
            
            if (message_ptr == pds_last_message)
                pds_last_message = NUL;

/*--------------------------------------------------------------------------*/
/**         Remove the structure from the list.                            **/
/*--------------------------------------------------------------------------*/

            pds_message_list = message_ptr -> next;
        
            if (pds_message_list != NUL)
                pds_message_list -> prev = NUL;

/*--------------------------------------------------------------------------*/
/**     ELSE                                                               **/
/*--------------------------------------------------------------------------*/

        }
        else
        {
/*--------------------------------------------------------------------------*/
/**         IF the structure to be deleted is the last one on the list THEN **/
/**             Adjust the ``last message" global variable to point to the **/
/**                 previous structure on the list.                        **/
/**         ENDIF                                                          **/
/*--------------------------------------------------------------------------*/

            if (message_ptr == pds_last_message)
                pds_last_message = message_ptr -> prev;

/*--------------------------------------------------------------------------*/
/**         Remove the structure from the list.                            **/
/*--------------------------------------------------------------------------*/

            message_ptr -> prev -> next = message_ptr -> next;
    
            if (message_ptr -> next != NUL)
                message_ptr -> next -> prev = message_ptr -> prev;
            
/*--------------------------------------------------------------------------*/
/**     ENDIF                                                              **/
/*--------------------------------------------------------------------------*/

        }  /*  End:  "if (message_ptr == ... else ..."  */


/*--------------------------------------------------------------------------*/
/**     Deallocate the storage used by the structure.                      **/
/*--------------------------------------------------------------------------*/

	Lemme_Go(message_ptr -> message);
	Lemme_Go(message_ptr);

/*--------------------------------------------------------------------------*/
/** ENDIF                                                                  **/
/*--------------------------------------------------------------------------*/

    }  /*  End:  "if (message_ptr != NUL) ..."  */

    return;

/*--------------------------------------------------------------------------*/
/** END                                                                    **/
/*--------------------------------------------------------------------------*/

}  /*  End:  "err_deallocate_message"  */


/**********************************************************************
 *$Component                                                          *
 *    LOGICAL err_keyword_message (severity, keyword_name,            *
 *                                 keyword_line_number,               *
 *                                 value_count, text)                 *
 *$Abstract                                                           *
 *    Formats label verifier keyword messages.                        *
 *$Keywords                                                           *
 *    ERRORLIB                                                        *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    severity:                                                       *
 *        The severity variable is an integer that indicates the      *
 *        level of severity of a message.  These severity levels      *
 *        are:  INFO, WARNING, ERROR, and FATAL_ERROR, and their      *
 *        actual values can be found in the include file: errordef.h. *
 *    keyword_name:                                                   *
 *        The keyword_name variable is a character string             *
 *        which contains the name of a keyword in a PDS label         *
 *        (e.g., the line "SCID = VG1" implies that "SCID" is the     *
 *        keyword name).                                              *
 *    keyword_line_number:                                            *
 *        The keyword_line_number variable is the number of the line  *
 *        in a PDS label file where the keyword currently being       *
 *        verified is located.                                        *
 *    value_count:                                                    *
 *        The value_count variable is the number of the current value *
 *        of a keyword that is being verified.  For example, if a     *
 *        keyword has five values, and the third one contains an      *
 *        error, then the value_count would be 3.                     *
 *    text:                                                           *
 *        The text variable is a character string that                *
 *        may contain zero or more characters.                        *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    verify_success_flag:                                            *
 *        The verify_success_flag variable is a TRUE/FALSE value      *
 *        which indicates whether or not a keyword or object has been *
 *        successfully verified.                                      *
 *$Detailed_Description                                               *
 *    The err_keyword_message routine constructs the keyword error or *
 *    warning message that is to be displayed to the user based on    *
 *    the text passed in.  A value of FALSE is always returned.       *
 *$External_References                                                *
 *    Item                     Shared-Data           Access           *
 *    ------------------------------------------------------------    *
 *    pds_error_count          pdsglob.h             write            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.2   October 17, 1991                                          *
 *$Change_History                                                     *
 *    DPB   05-30-91   Original code.                                 *
 *    KLM   06-25-91   Added pds_line_offset to the line number so    *
 *                     that if SFDUs are skipped the line numbers of  *
 *                     the error messages will still be correct.      *
 *    MDD   10-17-91   Replaced sys_exit_system with Check_Malloc and *
 *                     free with Lemme_Go.                            *
 **********************************************************************/

LOGICAL err_keyword_message (severity, keyword_name, 
                             keyword_line_number, value_count, text)

int severity;
char *keyword_name;
int keyword_line_number;
int value_count;
char *text;

{
    char severity_str [25];
    char *temp_str = {NULL};
    char *unknown_str = {"Unknown error"};
    char display_name [PDS_MAXLINE];

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Initialize a string containing the severity of the message based    **/
    /**     on the severity passed in.  This string will be used later as   **/
    /**     part of the keyword message.                                    **/
    /*-----------------------------------------------------------------------*/

    switch (severity)
    {
        case INFO    : strcpy (severity_str, "    INFO:   ");
                       break;

        case WARNING : strcpy (severity_str, "    WARNING:");
                       break;

        case ERROR   : strcpy (severity_str, "    ERROR:  ");
                       break;
                                               
        default      : strcpy (severity_str, "            ");
                       break;

    }  /*  End:  "switch (severity) ..."  */

    /*-----------------------------------------------------------------------*/
    /** add the line offset                                                 **/
    /*-----------------------------------------------------------------------*/

    keyword_line_number += pds_line_offset;

    /*-----------------------------------------------------------------------*/
    /** IF no text was passed in THEN                                       **/
    /*-----------------------------------------------------------------------*/

    if (keyword_name == NULL)
        strcpy (display_name, "");
    else
        strcpy (display_name, keyword_name);

    if (text == NULL)
    {
        /*-------------------------------------------------------------------*/
        /** Allocate storage for the "unknown" message                      **/
        /*-------------------------------------------------------------------*/

        temp_str = (char *) malloc 
                    (50 + String_Size(severity_str) + 
                     String_Size(unknown_str) + String_Size(display_name));

        Check_Malloc(temp_str);
    
        /*-------------------------------------------------------------------*/
        /** Construct the message.  If the keyword has more than one value, **/
        /**     then the message will include the value_count.              **/
        /*-------------------------------------------------------------------*/

        if (value_count != 0)
        {
            sprintf (temp_str, "%s Line %d -- %s, value %d:  %s", 
                     severity_str, keyword_line_number, 
                     display_name, value_count, unknown_str);
        }
        else
        {
            sprintf (temp_str, "%s Line %d -- %s:  %s", 
                     severity_str, keyword_line_number, 
                     display_name, unknown_str);
        }

    /*-----------------------------------------------------------------------*/
    /** ELSE                                                                **/
    /*-----------------------------------------------------------------------*/

    }
    else
    {
        /*-------------------------------------------------------------------*/
        /** Allocate memory for the text passed in                          **/
        /*-------------------------------------------------------------------*/

        temp_str = (char *) malloc 
                      (50 + String_Size(severity_str) + 
                       String_Size(text) + String_Size(display_name));

        Check_Malloc(temp_str);
    
        /*-------------------------------------------------------------------*/
        /** Construct the message.  If the keyword has more than one value, **/
        /**     then the message will include the value_count.              **/
        /*-------------------------------------------------------------------*/

        if (value_count != 0)
        {
            sprintf (temp_str, "%s Line %d -- %s, value %d:  %s", 
                     severity_str, keyword_line_number, 
                     display_name, value_count, text);
        }
        else
        {
            sprintf (temp_str, "%s Line %d -- %s:  %s", 
                     severity_str, keyword_line_number, 
                     display_name, text);
        }

    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if (text == NULL) ... else ..."  */

    /*-----------------------------------------------------------------------*/
    /** Append the newly constructed message onto the global message list   **/
    /*-----------------------------------------------------------------------*/

    ++pds_error_count;
    err_append_message (CONTINUE, temp_str);

    /*-----------------------------------------------------------------------*/
    /** Free any locally declared memory                                    **/
    /*-----------------------------------------------------------------------*/

    Lemme_Go(temp_str);

    /*-----------------------------------------------------------------------*/
    /** RETURN a value of FALSE                                             **/
    /*-----------------------------------------------------------------------*/

    return (FALSE);

/** END **/

}  /*  "err_keyword_message"  */



/**********************************************************************
 *$Component                                                          *
 *    LOGICAL err_object_message (severity, object_class,             *
 *                                object_line_number, text)           *
 *$Abstract                                                           *
 *    Formats label verifier object messages.                         *
 *$Keywords                                                           *
 *    ERRORLIB                                                        *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    severity:                                                       *
 *        The severity variable is an integer that indicates the      *
 *        level of severity of a message.  These severity levels      *
 *        are:  INFO, WARNING, ERROR, and FATAL_ERROR, and their      *
 *        actual values can be found in the include file: errordef.h. *
 *    object_class:                                                   *
 *        The object_class variable is a character string             *
 *        which contains the class of an object in a PDS label        *
 *        (e.g., "OBJECT = IMAGE" implies that "IMAGE" is the class   *
 *        of the object).                                             *
 *    object_line_number:                                             *
 *        The object_line_number variable is the number of the line   *
 *        in a PDS label file where the objedt currently being        *
 *        verified is located.                                        *
 *    text:                                                           *
 *        The text variable is a character string that                *
 *        may contain zero or more characters.                        *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    verify_success_flag:                                            *
 *        The verify_success_flag variable is a TRUE/FALSE value      *
 *        which indicates whether or not a keyword or object has been *
 *        successfully verified.                                      *
 *$Detailed_Description                                               *
 *    The err_object_message routine constructs the object error or   *
 *    warning message that is to be displayed to the user based on    *
 *    the text passed in.  A value of FALSE is always returned.       *
 *$External_References                                                *
 *    Item                     Shared-Data           Access           *
 *    ------------------------------------------------------------    *
 *    pds_error_count          pdsglob.h             write            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.2   October 17, 1991                                          *
 *$Change_History                                                     *
 *    DPB   06-12-91   Original code.                                 *
 *    KLM   06-25-91   Added pds_line_offset to the line number so    *
 *                     that if SFDUs are skipped the line numbers of  *
 *                     the error messages will still be correct.      *
 *    MDD   10-17-91   Replaced sys_exit_system with Check_Malloc and *
 *                     free with Lemme_Go.                            *
 **********************************************************************/

LOGICAL err_object_message (severity, object_class, object_line_number, text)

int severity;
char *object_class;
int object_line_number;
char *text;

{
    char severity_str [25];
    char *temp_str = {NULL};
    char *unknown_str = {"Unknown error"};
    char display_name [PDS_MAXLINE];

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Initialize a string containing the severity of the message based    **/
    /**     on the severity passed in.  This string will be used later as   **/
    /**     part of the keyword message.                                    **/
    /*-----------------------------------------------------------------------*/

    switch (severity)
    {
        case INFO    : strcpy (severity_str, "    INFO:   ");
                       break;

        case WARNING : strcpy (severity_str, "    WARNING:");
                       break;

        case ERROR   : strcpy (severity_str, "    ERROR:  ");
                       break;
                                               
        default      : strcpy (severity_str, "            ");
                       break;

    }  /*  End:  "switch (severity) ..."  */

    /*-----------------------------------------------------------------------*/
    /** add the line offset                                                 **/
    /*-----------------------------------------------------------------------*/

    object_line_number += pds_line_offset;

    /*-----------------------------------------------------------------------*/
    /** Prepare the display name for the object.                            **/
    /*-----------------------------------------------------------------------*/

    if (object_class == NULL)
        strcpy (display_name, "");
    else
        strcpy (display_name, object_class);

    /*-----------------------------------------------------------------------*/
    /** IF no text was passed in THEN                                       **/
    /*-----------------------------------------------------------------------*/

    if (text == NULL)
    {
        /*-------------------------------------------------------------------*/
        /** Allocate storage for the "unknown" message                      **/
        /*-------------------------------------------------------------------*/

        temp_str = (char *) malloc 
                    (50 + String_Size(severity_str) + 
                     String_Size(unknown_str) + String_Size(display_name));

        Check_Malloc(temp_str);
    
        /*-------------------------------------------------------------------*/
        /** Construct the message.                                          **/
        /*-------------------------------------------------------------------*/

        sprintf (temp_str, "%s Line %d -- %s:  %s", 
                 severity_str, object_line_number, display_name, unknown_str);

    /*-----------------------------------------------------------------------*/
    /** ELSE                                                                **/
    /*-----------------------------------------------------------------------*/

    }
    else
    {
        /*-------------------------------------------------------------------*/
        /** Allocate memory for the text passed in                          **/
        /*-------------------------------------------------------------------*/

        temp_str = (char *) malloc 
                      (50 + String_Size(severity_str) + 
                       String_Size(text) + String_Size(display_name));

        Check_Malloc(temp_str);
    
        /*-------------------------------------------------------------------*/
        /** Construct the message.                                          **/
        /*-------------------------------------------------------------------*/

        sprintf (temp_str, "%s Line %d -- %s:  %s", 
                 severity_str, object_line_number, display_name, text);

    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if (text == NULL) ... else ..."  */

    /*-----------------------------------------------------------------------*/
    /** Append the newly constructed message onto the global message list   **/
    /*-----------------------------------------------------------------------*/

    ++pds_error_count;
    err_append_message (CONTINUE, temp_str);

    /*-----------------------------------------------------------------------*/
    /** Free any locally declared memory                                    **/
    /*-----------------------------------------------------------------------*/

    Lemme_Go(temp_str);

    /*-----------------------------------------------------------------------*/
    /** RETURN a value of FALSE                                             **/
    /*-----------------------------------------------------------------------*/

    return (FALSE);

/** END **/

}  /*  "err_object_message"  */





/**********************************************************************
 *$Component                                                          *
 *    void err_write_to_file (file_ptr, print_severity)               *
 *$Abstract                                                           *
 *    Writes errors on the message list to a file.                    *
 *$Keywords                                                           *
 *    ERRORLIB                                                        *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    file_ptr:                                                       *
 *        The file_ptr variable is a pointer to a file.               *
 *    print_severity:                                                 *
 *       The print_severity variable is a TRUE/FALSE flag indicating  *
 *       whether or not the severity codes should be printed or       *
 *       displayed along with the messages in the error list.         *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    The err_write_to_file subroutine writes the messages on the     *
 *    global message list to a file. If the print_severity input is   *
 *    true, the severity codes will also be printed.                  *
 *    If the file ptr input is NULL, then the messages are written    *
 *    to standard output.                                             *
 *$External_References                                                *
 *    Item                     Shared-Data           Access           *
 *    ------------------------------------------------------------    *
 *    pds_message_list         pdsglob.h             read             *
 *$Author_and_Institution                                             *
 *    Kristy L. Marski/ J. P. L.                                      *
 *$Version_and_Date                                                   *
 *    1.1   October 22, 1991                                          *
 *$Change_History                                                     *
 *    KLM   03-25-91   Original code.                                 *
 *    MDD   10-22-91   Fixed to handle a NULL file pointer so that    *
 *                     things would be written to stdout in this case.*
 **********************************************************************/

void err_write_to_file (file_ptr, print_severity)

FILE *file_ptr;
LOGICAL print_severity;
{
   ERROR_LIST *msg_ptr;
   char temp_str [PDS_MAXLINE + 1];

   strcpy (temp_str, "");
   for (msg_ptr = pds_message_list; msg_ptr != NULL; msg_ptr = msg_ptr -> next)
   {
      if (print_severity)
	switch (msg_ptr -> severity)
        {
	  case INFO:        sprintf (temp_str, "INFO: ");
	                    break;
	  case WARNING:     sprintf (temp_str, "WARNING: ");
	                    break;
	  case ERROR:       sprintf (temp_str, "ERROR: ");
	                    break;
	  case FATAL_ERROR: sprintf (temp_str, "FATAL ERROR: ");
	                    break;
	  default:          break;
	}

      if (file_ptr != NULL)
         fprintf (file_ptr, "%s%s\n", temp_str, msg_ptr -> message);
      else
         printf ("%s%s\n", temp_str, msg_ptr -> message);
   }
   return;
}





