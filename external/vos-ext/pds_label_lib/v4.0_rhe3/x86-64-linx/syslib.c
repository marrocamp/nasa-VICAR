/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Component                                                           * 
 *    Library syslib.c                                                 *
 * Abstract                                                            *
 *    System-level PDS utilities                                       *
 * Detailed Description                                                *
 *    The syslib contains the subroutines used by PDS software to      *
 *    perform and manipulate a variety of System-level calls to the    *
 *    operating system.                                                *
 * Internal References                                                 *
 *    System manipulation routines:                                    *
 *        sys_change_directory                                         *
 *        sys_check_directory_integrity                                *
 *        sys_copy_file                                                *
 *        sys_delete_file                                              *
 *        sys_do_command                                               *
 *        sys_get_ascii_date                                           *
 *        sys_get_date                                                 *
 *        sys_get_file_list                                            *
 *        sys_get_user_id                                              *
 *        sys_get_current_directory                                    *
 *        sys_get_directory_list                                       *
 *        sys_get_path                                                 *
 *        sys_make_temp_fname                                          *
 * Authors and Institutions                                            *
 *    Herbert C. Gamble / J.P.L.                                       *
 *    David P. Bernath / J.P.L.                                        *
 *    Marti D. DeMore / J.P.L.                                         *
 *    Kristy L. Marski / J.P.L.                                        * 
 * Version and Date                                                    *
 *    2.0 October 17, 1991                                             *
 * Change History                                                      *
 *    HCG   06-30-90   Gathered together the various routines that     *
 *                     make up the library.                            *
 *    MDD   10-02-90   Added casting for SUN compiler and humanized    *
 *                     error messages                                  *
 *    DPB   10-04-90   Added sys_exit_system.                          *
 *    HCG   10-12-90   Added sys_check_directory_integrity.            *
 *    HCG   10-22-90   Added sys_browser_setup.                        *
 *    MDD   10-25-90   Removed sys_browser_setup                       *
 *    MDD   03-15-91   Added sys_get_date, sys_get_ascii_date,         *
 *                     sys_copy_file, sys_get_user_id, and             *
 *                     sys_get_temp_fname                              *
 *    KLM   03-19-91   Rewrote sys_delete_file using sys_do_command.   *
 *    DPB   07-09-91   Added sys_get_path.                             *
 *    MDD   10-17-91   Removed sys_exit_system                         *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "pdsdef.h"
#include "sysdef.h"
#include "utildef.h"
#include "errordef.h"

extern char *pds_temp_data_fname;


/**********************************************************************
 *$Component                                                          *
 *    LOGICAL sys_change_directory (directory)                        *
 *$Abstract                                                           *
 *    Changes directory location.                                     *
 *$Keywords                                                           *
 *    SYSLIB                                                          *
 *    OS                                                              *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    directory:                                                      *
 *        The directory variable is a string that contains a          *
 *        directory name.                                             *
 *$Outputs                                                            *
 *    NONE.                                                           *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure.       *
 *$Detailed_Description                                               *
 *    The sys_change_directory function will change to a new          *
 *    directory location and return a LOGICAL condition indicating    *
 *    wether or not the operation has succeeded. This function will   *
 *    work in VMS, SUN UNIX, and MSDOS (Turbo C).                     *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Herbert C. Gamble / J.P.L.                                      *
 *$Version_and_Date                                                   *
 *    2.0   May 5, 1991                                               *
 *$Change_History                                                     *
 *    HCG   09-26-90   Original Code.                                 *
 *    MDD   10-02-90   Added casting for SUN compiler and humanized   *
 *                     error messages                                 *
 *    MDD   10-25-90   Removed sys_delete_files calls                 *
 *    MDD   05-05-91   Changed to use sys_do_command                  *
 **********************************************************************/

LOGICAL sys_change_directory (directory)

char *directory;
{
   LOGICAL success = {FALSE};
   char command_str [PDS_MAXLINE + 1];

   /*----------------------------------------------------------------*/
   /** BEGIN                                                        **/
   /**    create the change directory command based on the OS       **/
   /*----------------------------------------------------------------*/

#ifdef VAX
   sprintf (command_str, "set def %s", directory);
#endif


#ifdef MSDOS_TC
   sprintf (command_str, "cd %s", directory);
#endif


#ifdef SUN_UNIX
   sprintf (command_str, "cd %s", directory);
#endif

   /*----------------------------------------------------------------*/
   /**    issue the command to the system                           **/
   /**    IF there was an error in the system command THEN          **/
   /**       append an error message to the global list             **/ 
   /*----------------------------------------------------------------*/

   success = sys_do_command (command_str);
   if (success != TRUE)
   {
      sprintf (command_str, "Unable to change to directory %s", directory);
      err_append_message (ERROR, command_str);   
   }

   /*----------------------------------------------------------------*/
   /**    ENDIF there was an error...                               **/
   /** END sys_change_dir                                           **/  
   /*----------------------------------------------------------------*/

   return (success);
} 


/**********************************************************************
 *$Component                                                          *
 *    LOGICAL sys_check_directory_integrity (directory,               *
 *                                           fname, write_flag)       *
 *$Abstract                                                           *
 *    Checks a directory for read/write access.                       *
 *$Keywords                                                           *
 *    DIR                                                             *
 *    SETUP                                                           *
 *    WRITE                                                           *
 *    READ                                                            *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    directory:                                                      *
 *        The directory variable is a string that contains a          *
 *        directory name.                                             *
 *    fname:                                                          *
 *        The fname variable is a general purpose character           *
 *        string containing a file specification.                     *
 *    write_flag:                                                     *
 *        The write_flag variable is a true/false flag indicating     *
 *        whether or not a directory or file should be writable.      *
 *$Outputs                                                            *
 *    NONE.                                                           *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure.       *
 *$Detailed_Description                                               *
 *    This routine checks a directory for read/write access depending *
 *    on the value of the write_flag passed in.  It attempts to       *
 *    open a file, write to it, read from it, close it, and delete it.*
 *    It will also attempt to read from the fname passed in.          *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Herbert C. Gamble / J.P.L.                                      *
 *$Version_and_Date                                                   *
 *    1.4   October 17, 1991                                          *
 *$Change_History                                                     *
 *    HCG   10-10-90   Original Code.                                 *
 *    MDD   11-06-90   Added write_flag input.                        *
 *    DPB   03-13-91   Gutted the routine and re-wrote it from scratch*
 *    MDD   05-26-91   Added rewind statement prior to read for the   *
 *                     benefit of the Turbo C compiler.               *
 *    MDD   10-17-91   Fixed malloc, free, and sys_exit_system calls  *
 **********************************************************************/

LOGICAL sys_check_directory_integrity (directory, fname, write_flag)

char *directory;
char *fname;
LOGICAL write_flag;

{
    FILE *fp = {NULL};
    char *test_fname = {NULL};
    char check_dir [PDS_MAXLINE];
    char temp [PDS_MAXLINE];
    LOGICAL success = {TRUE};

/*--------------------------------------------------------------------------*/
/** BEGIN                                                                  **/
/**                                                                        **/
/** IF the directory is either the UNIX default or a keyword               **/
/**         representing the user's default directory THEN                 **/
/**     Set the directory to be the user's default login dir.              **/
/** ENDIF                                                                  **/
/*--------------------------------------------------------------------------*/

    if (strcmp (directory, PDS_HOME_KEYWORD) == 0)
        strcpy (check_dir, PDS_DEFAULT_LOGIN);
    else
        strcpy (check_dir, directory);

/*--------------------------------------------------------------------------*/
/** IF the directory should be checked for write access THEN               **/
/*--------------------------------------------------------------------------*/

    if (write_flag)
    {
/*--------------------------------------------------------------------------*/
/**     Attempt to open, write to, read from, close, and delete a test     **/
/**         file in the directory, and display an error message if the     **/
/**         attempt fails.                                                 **/
/*--------------------------------------------------------------------------*/

        test_fname = sys_make_temp_fname (check_dir);

        if ((fp = fopen (test_fname, "w+")) == NULL)
        {
            success = FALSE;
            printf(" *** Unable to open files in directory %s\n",check_dir);
        }
        else
        {
           fprintf (fp, "test string");

           if (ferror (fp) != NULL)
           {
               success = FALSE;
               printf (" *** Unable to write to files in directory %s\n",check_dir);
           }
	   rewind (fp);
	   fgets (temp, 2, fp);

	   if (ferror (fp) != NULL)
           {
               success = FALSE;
               printf (" *** Unable to read from files in directory %s\n",check_dir);
           }

           if (fclose (fp) != 0)
           {
               success = FALSE;
               printf(" *** Unable to close files in directory %s\n",check_dir);
           }

#ifdef VAX
           if (delete (test_fname) != 0)
#else
           if (remove (test_fname) != 0)
#endif
           {
               success = FALSE;
               printf(" *** Unable to delete files from directory %s\n",check_dir);
           }

        }  /*  End:  "if ((fp = fopen (fname ... else ..." */

/*--------------------------------------------------------------------------*/
/** ENDIF                                                                  **/
/*--------------------------------------------------------------------------*/

    }  /*  End:  "if (write_flag) ..."  */


/*--------------------------------------------------------------------------*/
/** IF a particular file in the directory is to be checked for read        **/
/**         access THEN                                                    **/
/*--------------------------------------------------------------------------*/

    if ((success) && (fname != NULL))
    {
/*--------------------------------------------------------------------------*/
/**     Attempt to open, and read from, a test file in the directory,      **/
/**         and display an error message if the attempt fails.             **/
/*--------------------------------------------------------------------------*/

        Lemme_Go(test_fname);
        test_fname = util_create_file_spec (check_dir, fname);

        if ((fp = fopen (test_fname, "r")) == NULL)
        {
            success = FALSE;
            printf(" *** Unable to open file %s\n", test_fname);
        }
        else
        {
           fgets (temp, 2, fp);

           if (ferror (fp) != NULL)
           {
               success = FALSE;
               printf (" *** Unable to read file %s\n", test_fname);
           }

           fclose (fp);

        }  /*  End:  "if ((fp = fopen (fname ... else ..." */

/*--------------------------------------------------------------------------*/
/** ENDIF                                                                  **/
/*--------------------------------------------------------------------------*/

    }  /*  End:  "if (fname != NULL) ..."  */


    Lemme_Go(test_fname);

/*--------------------------------------------------------------------------*/
/** RETURN success flag                                                    **/
/*--------------------------------------------------------------------------*/

    return (success);

/*--------------------------------------------------------------------------*/
/** END                                                                    **/
/*--------------------------------------------------------------------------*/

}  /*  End:  "sys_check_directory_integrity"  */


/**********************************************************************
 *$Component                                                          *
 *    LOGICAL sys_copy_file (source_file, dest_file)                  *
 *$Abstract                                                           *
 *    Copies a file to a given destination.                           *
 *$Keywords                                                           *
 *    SYSLIB                                                          *
 *    OS                                                              *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    source_file:                                                    *
 *        The source_file variable is a string containing the name of *
 *        a file that is to be used as the source, or original, for   *
 *        a file operation.                                           *
 *    dest_file:                                                      *
 *        The dest_file variable is a string containing the name of   *
 *        a file that is to be used as the destination, or result,    *
 *        for a file operation.                                       *
 *$Outputs                                                            *
 *    NONE.                                                           *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure.       *
 *$Detailed_Description                                               *
 *    The sys_copy_file routine will copy the file passed in as input *
 *    source file to the location passed in as dest file.  This       *
 *    function will work in VMS, SUN UNIX, and MSDOS (Turbo C).       *
 *$External_References                                                *
 *    None                                                            *
 *$Error_Handling                                                     *
 *    If any error occurs while issuing the copy command, the return  *
 *    value is set to FALSE and a message is appended to the global   *
 *    list.                                                           *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/JPL                                             *
 *$Version_and_Date                                                   *
 *    1.0   March 15, 1991                                            *
 *$Change_History                                                     *
 *    MDD   03-15-91   Original Code.                                 *
 **********************************************************************/

LOGICAL sys_copy_file (source_file, dest_file)

char *source_file;
char *dest_file;

{
   char command_str [PDS_MAXLINE];
   LOGICAL success = TRUE;

   /*----------------------------------------------------------------*/
   /** BEGIN                                                        **/
   /**    create the copy command based on the current OS           **/
   /*----------------------------------------------------------------*/

#ifdef VAX
   sprintf (command_str, "copy %s %s", source_file, dest_file);
#endif

#ifdef MSDOS_TC
   sprintf (command_str, "copy %s %s", source_file, dest_file);
#endif

#ifdef SUN_UNIX
   sprintf (command_str, "cp %s %s", source_file, dest_file);
#endif

   /*----------------------------------------------------------------*/
   /**    issue the copy command to the system                      **/
   /**    IF there was an error in the system command THEN          **/
   /**       append an error message to the global list             **/ 
   /*----------------------------------------------------------------*/

   success = sys_do_command (command_str);
   if (success != TRUE)
   {
      sprintf (command_str, "Unable to copy file %s to %s", source_file, 
                  dest_file);
      err_append_message (ERROR, command_str);   
   }

   /*----------------------------------------------------------------*/
   /**    ENDIF there was an error...                               **/
   /** END sys_copy_file                                            **/  
   /*----------------------------------------------------------------*/

   return (success);
} 


/**********************************************************************
 *$Component                                                          *
 *    LOGICAL sys_delete_file (file_name)                             *
 *$Abstract                                                           *
 *    Deletes a file.                                                 *
 *$Keywords                                                           *
 *    SYSLIB                                                          *
 *    OS                                                              *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    file_name:                                                      *
 *        The file_name variable is a string containing the name of   *
 *        a file.                                                     *
 *$Outputs                                                            *
 *    NONE.                                                           *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure.       *
 *$Detailed_Description                                               *
 *    The sys_delete_file function will delete the file passed in as  *
 *    input. This function will work in VMS, SUN UNIX, and            * 
 *    MSDOS (Turbo C).                                                *
 *$External_References                                                *
 *    None                                                            *
 *$Error_Handling                                                     *
 *    If any error occurs while issuing the delete command, the       *
 *    return value is set to FALSE and a message is appended to the   *
 *    global list.                                                    *
 *$Author_and_Institution                                             *
 *    Herbert C. Gamble / J.P.L.                                      *
 *$Version_and_Date                                                   *
 *    2.0   October 25, 1991                                          *
 *$Change_History                                                     *
 *    HCG   08-31-90   Original Code.                                 *
 *    MDD   10-02-90   Added casting for SUN compiler and humanized   *
 *                     error messages                                 *
 *    HCG   10-23-90   Deleted syslib definitions.                    *
 *    KLM   03-19-91   Rewrote the routine using sys_do_command.      *
 *    MDD   10-25-91   Removed Flush_File call.                       *
 **********************************************************************/

LOGICAL sys_delete_file (file_name)
char *file_name;
{
  char command_str[PDS_MAXLINE];
  LOGICAL success = TRUE;

   /*----------------------------------------------------------------*/
   /** BEGIN                                                        **/
   /**   IF a file name was given THEN                              **/
   /*----------------------------------------------------------------*/

   if (file_name != NULL)
   {

      /*-------------------------------------------------------------*/
      /**   Clear the input/output files.                           **/
      /**   create the delete command based on the current OS       **/
      /*-------------------------------------------------------------*/

      Flush_File(file_name);

#ifdef VAX
      sprintf (command_str, "delete %s;*", file_name);
#endif


#ifdef MSDOS_TC
      sprintf (command_str, "del %s", file_name);
#endif


#ifdef SUN_UNIX
      sprintf (command_str, "rm -f %s", file_name);
#endif

      /*-------------------------------------------------------------*/
      /**    issue the delete command to the system                 **/
      /**    IF there was an error in the system command THEN       **/
      /**       append an error message to the global list          **/ 
      /**    ENDIF there was an error...                            **/
      /*-------------------------------------------------------------*/

      success = sys_do_command (command_str);
      if (success != TRUE)
      {
         sprintf (command_str, "Unable to delete file %s", file_name);
         err_append_message (ERROR, command_str);
      }   
   }

   /*----------------------------------------------------------------*/
   /** ENDIF a file name was given...                               **/
   /*----------------------------------------------------------------*/

   return (success);

   /*----------------------------------------------------------------*/
   /** END                                                          **/
   /*----------------------------------------------------------------*/
}


/**********************************************************************
 *$Component                                                          *
 *    LOGICAL sys_do_command (os_command)                             *
 *$Abstract                                                           *
 *    Issues the given operating system command.                      *
 *$Keywords                                                           *
 *    SYSLIB                                                          *
 *    OS                                                              *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    os_command:                                                     *
 *        The os_command variable is a string containing a valid      *
 *        operating system command line.                              *
 *$Outputs                                                            *
 *    NONE.                                                           *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure.       *
 *$Detailed_Description                                               *
 *    The sys_do_command routine issues a command to the operating    *
 *    system, interprets the return code, and returns TRUE if the     *
 *    command was a success or FALSE if it was a failure.             *
 *$External_References                                                *
 *    None                                                            *
 *$Error_Handling                                                     *
 *    If any error occurs when trying to redirect standard output or  *
 *    standard error, this routine returns FALSE and appends a        *
 *    message to the global error message list.                       *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/JPL                                             *
 *$Version_and_Date                                                   *
 *    1.0   March 15, 1991                                            *
 *$Change_History                                                     *
 *    MDD   03-15-91   Original Code.                                 *
 *    MDD   05-05-91   Removed redirection                            *
 **********************************************************************/

LOGICAL sys_do_command (os_command)

char *os_command;
{
  int status = 0;
  LOGICAL success = TRUE;
   
  status = system (os_command);

#ifdef VAX
  if Vms_Error(status) success = FALSE;
#else          
  if (status != 0) success = FALSE;
#endif

  return (success);
} 


/**********************************************************************
 *$Component                                                          *
 *    char *sys_get_ascii_date ()                                     *
 *$Abstract                                                           *
 *    Gets an ascii string containing the date and time.              *
 *$Keywords                                                           *
 *    SYSLIB                                                          *
 *    OS                                                              *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    None                                                            *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    date_string                                                     *
 *        The date_string variable contains an ascii string that      *
 *        represents the date and time in the format.                 *
 *$Detailed_Description                                               *
 *    The sys_get_ascii_date routine returns  an ascii string that    *
 *    contains the current date and time in the format:               *
 *        day mon dd hh:mm:ss yyyy, e.g., Thu Mar 14 16:51:46 1991.   *
 *$External_References                                                *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    This routine allocates memory for the returned value which must *
 *    be freed elsewhere.                                             *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/JPL                                             *
 *$Version_and_Date                                                   *
 *    1.1   October 17, 1991                                          *
 *$Change_History                                                     *
 *    MDD   03-15-91   Original Code.                                 *
 *    MDD   10-17-91   Fixed mallocs, frees, and sys_exit_system calls*
 **********************************************************************/

char *sys_get_ascii_date ()
{
   struct tm *tblock;
   char *date;
   char *temp_date;

   /*----------------------------------------------------------------*/
   /** BEGIN                                                        **/
   /**   get the system date and time and convert it to ascii       **/
   /*----------------------------------------------------------------*/
   
   tblock = sys_get_date ();
   temp_date = asctime (tblock);
   Lemme_Go(tblock);

   /*----------------------------------------------------------------*/
   /**   allocate memory for the string and copy it                 **/
   /**   remove the dumb carriage return                            **/
   /*----------------------------------------------------------------*/
   
   Malloc_String(date, String_Size(temp_date));
   strcpy (date, temp_date);
   util_remove_char (date, '\n');
   return (date);

   /*----------------------------------------------------------------*/
   /** END sys_get_ascii_date                                       **/
   /*----------------------------------------------------------------*/
}  


/**********************************************************************
 *$Component                                                          *
 *    char *sys_get_current_directory()                               *
 *$Abstract                                                           *
 *    Returns the current directory path.                             *
 *$Keywords                                                           *
 *    SYSLIB                                                          *
 *    OS                                                              *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    NONE                                                            *
 *$Outputs                                                            *
 *    NONE.                                                           *
 *$Returns                                                            *
 *    directory:                                                      *
 *        The directory variable is a string that contains a          *
 *        directory name.                                             *
 *$Detailed_Description                                               *
 *    The sys_get_current_directory routine will return the current   *
 *    directory path when called. This routine is compatible with     *
 *    VMS, SUN UNIX, and MSDOS (Turbo C).                             *
 *$External_References                                                *
 *  None                                                              *
 *$Author_and_Institution                                             *
 *    Herbert C. Gamble / J.P.L.                                      *
 *$Version_and_Date                                                   *
 *    2.1 October 17, 1991                                            *
 *$Change_History                                                     *
 *    HCG   08-31-90   Original Code.                                 *
 *    MDD   10-02-90   Added casting for SUN compiler and humanized   *
 *                     error messages                                 *
 *    HCG   10-23-90   Deleted syslib definitions.                    *
 *    MDD   10-25-90   Removed sys_delete_file calls                  *
 *    MDD   05-05-91   Rewrote                                        *
 *    MDD   10-17-91   Fixed malloc, free, and sys_exit_system calls  *
 **********************************************************************/

char *sys_get_current_directory ()
{
  char temp [PDS_MAXLINE + 1];
  char *directory = NULL;

  if (getcwd (temp, PDS_MAXLINE))
  {
     Malloc_String(directory, String_Size(temp));
     strcpy (directory, temp);
  }
  else
     err_append_message (ERROR, "Unable to get current directory name");
  return (directory);
}


/**********************************************************************
 *$Component                                                          *
 *   STRING_LIST *sys_get_directory_list (directory)                  *
 *$Abstract                                                           *
 *    Make a list of directory names.                                 *
 *$Keywords                                                           *
 *    SYSLIB                                                          *
 *    OS                                                              *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    directory:                                                      *
 *        The directory variable is a string that contains a          *
 *        directory name.                                             *
 *$Outputs                                                            *
 *    NONE.                                                           *
 *$Returns                                                            *
 *    directory_list:                                                 *
 *        The directory_list variable is a pointer to a STRING_LIST   *
 *        structure that points to a list of directory names.         *
 *$Detailed_Description                                               *
 *    The sys_get_directory_list will return a list of directory      *
 *    names for the directory specified by the directory              *
 *    variable. The routine will perform a directory system call to   *
 *    the operating system. The output of this call will be           *
 *    redirected from stdout to a file. This file will be read and    *
 *    the string entries that are directories will be appended to a   *
 *    directory list that is returned by the routine. This routine    *
 *    is compatible with VMS, SUN UNIX, and MSDOS (Turbo C).          *
 *$External_References                                                *
 *   None                                                             *
 *$Author_and_Institution                                             *
 *    Herbert C. Gamble / J.P.L.                                      *
 *$Version_and_Date                                                   *
 *    2.0   May 5, 1991                                               *
 *$Change_History                                                     *
 *    HCG   08-05-90   Original Code.                                 *
 *    MDD   10-02-90   Added casting for SUN compiler and humanized   *
 *                     error messages                                 *
 *    HCG   10-23-90   Deleted syslib definitions.                    *
 *    MDD   10-25-90   Removed sys_delete_file calls                  *
 *    MDD   05-05-91   Rewrote                                        *
 **********************************************************************/

STRING_LIST *sys_get_directory_list (directory)

char *directory;
{
  FILE *fp = NULL;
  STRING_LIST *directory_list = {NULL};
  char *temp_str = NULL;
  char command_str [PDS_MAXLINE + 1];
  char data_str [PDS_MAXLINE + 1];
  LOGICAL success = TRUE;

/** BEGIN **/
   /*----------------------------------------------------------------*/
   /** create the directory command based on the OS,                **/
   /**    redirecting the output to a file                          **/
   /*----------------------------------------------------------------*/

#ifdef VAX
   sprintf (command_str, "dir/out=%s/col=1/ver=1/nohead/notrail %s*.dir",
               pds_temp_data_fname, directory);
#endif


#ifdef MSDOS_TC
   sprintf (command_str, "dir %s > %s", directory, pds_temp_data_fname);
#endif


#ifdef SUN_UNIX
   sprintf (command_str, "ls -1F %s > %s", directory, pds_temp_data_fname);
#endif

   /*----------------------------------------------------------------*/
   /** issue the command to the system                              **/
   /** IF there was an error in the system command THEN             **/
   /**    append an error message to the global list                **/ 
   /*----------------------------------------------------------------*/

   success = sys_do_command (command_str);
   if (success != TRUE)
   {
      sprintf (command_str, "Unable to get subdirectories of %s", directory);
      err_append_message (ERROR, command_str);   
   }
   /*----------------------------------------------------------------*/
   /** ELSE                                                         **/
   /*----------------------------------------------------------------*/

   else                              
   {
      /*-------------------------------------------------------------*/
      /** IF the redirect file can't be opened THEN                 **/
      /**    append an error to the list                            **/
      /*-------------------------------------------------------------*/

      if ((fp = fopen (pds_temp_data_fname, "r")) == NULL)
      {
         err_append_message (ERROR, "Unable to open scratch file.");
      }
      /*-------------------------------------------------------------*/
      /** ELSE                                                      **/
      /*-------------------------------------------------------------*/

      else
      {
         /*----------------------------------------------------------*/
         /** WHILE there are more lines in the redirect file DO     **/
         /*----------------------------------------------------------*/

         while ((fgets (data_str, PDS_MAXLINE, fp)) != NULL)
         {
            util_clean_up_string (data_str);

            /*-------------------------------------------------------*/
            /** IF this is DOS and the string has <DIR> in it THEN  **/
            /**   remove the <DIR> and add the name to the list of  **/
            /**      directories                                    **/
            /** ENDIF                                               **/
            /*-------------------------------------------------------*/

#ifdef MSDOS_TC
            if ((util_locate_substring (data_str, "<DIR>")) != NULL)
            {
               temp_str = (char *) strchr (data_str, ' ');
               *temp_str = '\0';
               directory_list = util_append_string_list (directory_list,
                                                            data_str, 
                                                               STRING_TYPE); 
            }
#endif

            /*-------------------------------------------------------*/
            /** IF this is Unix and the string has "/"  in it THEN  **/
            /**   remove the "/" and add the name to the list of    **/
            /**      directories                                    **/
            /** ENDIF                                               **/
            /*-------------------------------------------------------*/

#ifdef SUN_UNIX

            if (*(String_End(data_str) - 1) == '/')
            {
              temp_str = (char *) strchr (data_str, '/');
              *temp_str = '\0';
              directory_list = util_append_string_list (directory_list,
                                                           data_str,
                                                              STRING_TYPE); 
            }
#endif
            /*-------------------------------------------------------*/
            /** IF this is VMS THEN                                 **/
            /**   change [mmm]xxx.dir to [.xxx]                     **/
            /** ENDIF                                               **/
            /*-------------------------------------------------------*/

#ifdef VAX
            temp_str = (char *) strchr (data_str, ']');
            temp_str++;
            *((char *) strchr (temp_str, '.')) = EOS;
            sprintf (command_str, "[.%s]", temp_str);
            directory_list = util_append_string_list (directory_list,
                                                         command_str, 
                                                            STRING_TYPE);
#endif

         }
         /*----------------------------------------------------------*/
         /** ENDWHILE there are more lines...                       **/
         /** IF this is VMS THEN                                    **/
         /**   add the directory parent, "[-]"                      **/
         /** ENDIF                                                  **/
         /*----------------------------------------------------------*/

#ifdef VAX
         directory_list = util_append_string_list (directory_list,
                                                      "[-]", STRING_TYPE);
#endif
         fclose (fp);
      }
      /*-------------------------------------------------------------*/
      /** ENDIF the redirect file can't be opened...                **/
      /*-------------------------------------------------------------*/

    }
   /*----------------------------------------------------------------*/
   /** ENDIF there was an error in the system command...            **/
   /*----------------------------------------------------------------*/

   return (directory_list);

/** END sys_get_directory_list **/
}


/**********************************************************************
 *$Component                                                          *
 *    struct tm *sys_get_date ()                                      *
 *$Abstract                                                           *
 *    Gets  a structure containing the date and time                  *
 *$Keywords                                                           *
 *    SYSLIB                                                          *
 *    OS                                                              *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    None                                                            *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    date_struct                                                     *
 *        The date_struct structure contains the components of a      *
 *        date and time.  It is defined in time.h.                    *
 *$Detailed_Description                                               *
 *    The sys_get_date routine returns a structure that contains the  *
 *    broken down date and time in a tm structure, e.g,               *
 *        tm_struct->year, tm_struct->hour, etc.                      *
 *$External_References                                                *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    This routine allocates memory for the returned value which must *
 *    be freed elsewhere.                                             *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/JPL                                             *
 *$Version_and_Date                                                   *
 *    1.2   October 17, 1991                                          *
 *$Change_History                                                     *
 *    MDD   03-15-91   Original Code.                                 *
 *    MDD   05-26-91   Added casts needed by PC compiler              *
 *    MDD   10-17-91   Fixed malloc, free, and sys_exit_system calls. *
 **********************************************************************/

struct tm *sys_get_date ()
{
   time_t timer;
   struct tm *tblock;
   struct tm *temp_tblock;

   /*-----------------------------------------------------------------*/
   /** BEGIN                                                         **/
   /**   get a system date and time                                  **/
   /**   allocate memory for the return value and copy the info to it**/
   /*-----------------------------------------------------------------*/

   timer = time (NULL);
   temp_tblock = localtime (&timer);
   tblock = (struct tm *) malloc (sizeof(struct tm));
   Check_Malloc(tblock);
   util_byte_copy ((char *) tblock, (char *) temp_tblock, sizeof(struct tm));
   return (tblock);

   /*-----------------------------------------------------------------*/
   /** END sys_get_date                                              **/
   /*-----------------------------------------------------------------*/
}  


/**********************************************************************
 *$Component                                                          *
 *   STRING_LIST *sys_get_file_list (dir_mask)                        *
 *$Abstract                                                           *
 *    Make a list of file names.                                      *
 *$Keywords                                                           *
 *    SYSLIB                                                          *
 *    OS                                                              *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    dir_mask:                                                       *
 *        The dir_mask variable is a character string that contains   *
 *        a system specific file specification, including wildcards.  *
 *$Outputs                                                            *
 *    NONE.                                                           *
 *$Returns                                                            *
 *    file_list:                                                      *
 *        The file_list variable is a pointer to a STRING_LIST        *
 *        structure that contains a list of file names.               *
 *$Detailed_Description                                               *
 *    The sys_get_file_list routine will return a list of file names  *
 *    that match the directory mask passed in as a STRING_LIST.       *
 *    This routine is compatible with VMS, SUN UNIX, and              *
 *    MSDOS (Turbo C).                                                *
 *$External_References                                                *
 *   None                                                             *
 *$Error_Handling                                                     *
 *   The routine will return NULL if it finds no files that match     *
 *   the dir_mask or if it cannot obtain a directory listing.         *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/JPL                                             *
 *$Version_and_Date                                                   *
 *    1.1   June 20, 1991                                             *
 *$Change_History                                                     *
 *    MDD   05-06-91   Original Code.                                 *
 *    DPB   06-20-91   Fixed a minor bug in the VAX version.          *
 **********************************************************************/

STRING_LIST *sys_get_file_list (dir_mask)

char *dir_mask;
{
  STRING_LIST *file_list = {NULL};
  char *temp_str = NULL;

/** BEGIN **/

#ifdef MSDOS_TC
{
   struct ffblk ffblk;
   int done;
   char *directory = NULL;

   directory = sys_get_path (dir_mask);
   done = findfirst (dir_mask, &ffblk, 0);
   while (!done)
   {
      if (ffblk.ff_attrib != FA_DIREC)
      {
         temp_str = util_create_file_spec (directory, ffblk.ff_name);
         file_list = util_append_string_list (file_list, temp_str,
                     STRING_TYPE);
         Lemme_Go(temp_str);
      }
      done = findnext (&ffblk);
   }
   Lemme_Go(directory);
}

#else
{
  FILE *fp = NULL;
  char command_str [PDS_MAXLINE + 1];
  char data_str [PDS_MAXLINE + 1];
  LOGICAL success = TRUE;

   /*----------------------------------------------------------------*/
   /** create the directory command based on the OS,                **/
   /**    redirecting the output to a file                          **/
   /*----------------------------------------------------------------*/

#ifdef VAX
   sprintf (command_str, 
               "dir/col=1/ver=1/nohead/notrail/exclude=*.dir/out=%s %s",
               pds_temp_data_fname, dir_mask);
#endif

#ifdef SUN_UNIX
   sprintf (command_str, "ls -1F %s > %s", dir_mask, pds_temp_data_fname);
#endif

   /*----------------------------------------------------------------*/
   /** issue the command to the system                              **/
   /** IF there was an error in the system command THEN             **/
   /**    append an error message to the global list                **/ 
   /*----------------------------------------------------------------*/

   success = sys_do_command (command_str);
   if (success != TRUE)
   {
      sprintf (command_str, "Unable to get list of files specified by: %s", 
                             dir_mask);
      err_append_message (ERROR, command_str);   
   }
   /*----------------------------------------------------------------*/
   /** ELSE                                                         **/
   /*----------------------------------------------------------------*/

   else
   {
      /*-------------------------------------------------------------*/
      /** IF the redirect file can't be opened THEN                 **/
      /**    append an error to the list                            **/
      /*-------------------------------------------------------------*/

      if ((fp = fopen (pds_temp_data_fname, "r")) == NULL)
      {
         err_append_message (ERROR, "Unable to open scratch file.");
      }
      /*-------------------------------------------------------------*/
      /** ELSE                                                      **/
      /*-------------------------------------------------------------*/

      else
      {
         /*----------------------------------------------------------*/
         /** WHILE there are more lines in the redirect file DO     **/
         /*----------------------------------------------------------*/

         while ((fgets (data_str, PDS_MAXLINE, fp)) != NULL)
         {
            util_clean_up_string (data_str);
            util_compress_char (data_str, ' ');
            if (util_string_is_empty (data_str)) break;

            /*-------------------------------------------------------*/
            /** IF this is Unix and the string does not have "/"    **/
            /**     at the end THEN                                 **/
            /**   remove the file mode characters and add the name  **/
            /**   to the list of files                              **/
            /** ENDIF                                               **/
            /*-------------------------------------------------------*/

#ifdef SUN_UNIX

            temp_str = String_End(data_str);
            if (*temp_str != '/')
            {
              if (*temp_str == '*' || *temp_str == '=' || *temp_str == '@')
                 *temp_str = EOS;
              file_list = util_append_string_list (file_list,
                                                       data_str,
                                                           STRING_TYPE); 
            }
#endif
            /*-------------------------------------------------------*/
            /** IF this is VMS THEN                                 **/
            /**   add the file to the file list                     **/
            /** ENDIF                                               **/
            /*-------------------------------------------------------*/

#ifdef VAX
            file_list = util_append_string_list (file_list,
                                                     data_str, 
                                                         STRING_TYPE);
#endif

         }
         /*----------------------------------------------------------*/
         /** ENDWHILE there are more lines...                       **/
         /*----------------------------------------------------------*/

         fclose (fp);
      }
      /*-------------------------------------------------------------*/
      /** ENDIF the redirect file can't be opened...                **/
      /*-------------------------------------------------------------*/

    }
   /*----------------------------------------------------------------*/
   /** ENDIF there was an error in the system command...            **/
   /*----------------------------------------------------------------*/

}
#endif

   return (file_list);
/** END sys_get_file_list **/
}

/**********************************************************************
 *$Component                                                          *
 *    char *sys_get_path (fname)                                      *
 *$Abstract                                                           *
 *    Extracts and returns the directory path from a file spec.       *
 *$Keywords                                                           *
 *    PATH                                                            *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    fname:                                                          *
 *        The fname variable is a general purpose character           *
 *        string containing a file specification.                     *
 *$Outputs                                                            *
 *    NONE.                                                           *
 *$Returns                                                            *
 *    directory:                                                      *
 *        The directory variable is a string that contains a          *
 *        directory name.                                             *
 *$Detailed_Description                                               *
 *    The sys_get_path routine extracts a directory path from the     *
 *    file name passed in.  If no path is found, this routine         *
 *    returns NULL.                                                   *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore / J.P.L.                                        *
 *$Version_and_Date                                                   *
 *    1.1   October 17, 1991                                          *
 *$Change_History                                                     *
 *    DPB   07-09-91   Original code.                                 *
 *    MDD   10-17-91   Fixed malloc, free, and sys_exit_system calls  *
 *    MDD   01-13-92   Bug fix -was returning empty string rather than*
 *                     NULL when no path was found.                   *
 **********************************************************************/

char *sys_get_path (fname)

char *fname;

{
    char *c = {NULL};
    char *path_name = {NULL};
    char save_char;

    if (fname != NULL)
    {
        /*  This loop starts at the end of the file spec and
         *  works backwards until it either finds a directory
         *  path, or runs out of string to check.
         */

        for (c = String_End(fname); 
                ((c >= fname) && (*c != ']') && 
                 (*c != ':') && (*c != '/') && (*c != '\\')); --c) ;

        ++c;
        save_char = *c;
        *c = EOS;
        Malloc_String(path_name, String_Size(fname));
        strcpy (path_name, fname);
        *c = save_char;
        if (strcmp (path_name, "") == 0) Lemme_Go(path_name);
          
    }  /*  End:  "if (fname != NULL) ..."  */

    return (path_name);

}  /* "sys_get_path"  */


/**********************************************************************
 *$Component                                                          *
 *    char *sys_get_user_id ()                                        *
 *$Abstract                                                           *
 *    Gets the current user's login name                              *
 *$Keywords                                                           *
 *    SYSLIB                                                          *
 *    OS                                                              *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    None                                                            *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    user_id:                                                        *
 *        The user_id variable is a string that contains a user's     *
 *        login name.                                                 *
 *$Detailed_Description                                               *
 *    The sys_get_user_id routine returns a string containing the     *
 *    login name of the current user, on Unix and VMS systems. On     *
 *    DOS systems (Turbo C) it returns the empty string ("",not NULL).*
 *$External_References                                                *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    This routine allocates memory for the returned value which must *
 *    be freed elsewhere.                                             *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/JPL                                             *
 *$Version_and_Date                                                   *
 *    1.1   October 17, 1991                                          *
 *$Change_History                                                     *
 *    MDD   03-15-91   Original Code.                                 *
 *    MDD   10-17-91   Fixed malloc, free, and sys_exit_system calls  *
 **********************************************************************/

char *sys_get_user_id ()
{
   char *user_id;
   char *temp_user_id;

   /*----------------------------------------------------------------*/
   /** BEGIN                                                        **/ 
   /**    IF this is DOS THEN                                       **/
   /**       set the user id to the empty string                    **/
   /*----------------------------------------------------------------*/

#ifdef MSDOS_TC
   Malloc_String(user_id, sizeof(char));
   strcpy (user_id, "");

   /*----------------------------------------------------------------*/
   /**    ELSE                                                      **/
   /**       get the user's login id and assign it to return value  **/
   /*----------------------------------------------------------------*/

#else
   temp_user_id = cuserid (NULL);
   Malloc_String(user_id, String_Size(temp_user_id));
   strcpy (user_id, temp_user_id);
#endif

   /*----------------------------------------------------------------*/
   /**    ENDIF this is DOS...                                      **/
   /*----------------------------------------------------------------*/

   return (user_id);

   /*----------------------------------------------------------------*/
   /** END                                                          **/
   /*----------------------------------------------------------------*/
}  



/**********************************************************************
 *$Component                                                          *
 *    char *sys_make_temp_fname (directory)                           *
 *$Abstract                                                           *
 *    Creates a temporary file name                                   *
 *$Keywords                                                           *
 *    SYSLIB                                                          *
 *    OS                                                              *
 *    NON_UI_COMMON                                                   *
 *$Inputs                                                             *
 *    directory:                                                      *
 *        The directory variable is a string that contains a          *
 *        directory name.                                             *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    file_name:                                                      *
 *        The variable file_name is a string containing the name of   *
 *        a file.                                                     *
 *$Detailed_Description                                               *
 *    The sys_make_temp_fname routine creates a unique file name.     *
 *    The directory name passed in will be appended to the front of   *
 *    the temporary file name returned. However, the directory may be *
 *    NULL or the empty string.                                       *
 *$External_References                                                *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    This routine allocates memory for the returned value which must *
 *    be freed elsewhere.                                             *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/JPL                                             *
 *$Version_and_Date                                                   *
 *    2.0   December 17, 1991                                         *
 *$Change_History                                                     *
 *    MDD   03-15-91   Original Code.                                 *
 *    MDD   10-17-91   Fixed malloc, free, and sys_exit_system calls  *
 *    MDD   12-12-91   Rewrote to provide better handling of multiple *
 *                     processes.                                     *
 **********************************************************************/


char *sys_make_temp_fname (directory)

char *directory;
{

char *file_name;
char base_name [PDS_MAXLINE];

/** BEGIN **/

/*------------------------------------------------------------------*/
/** IF this is SUN Unix THEN                                       **/
/**    use the tempnam function to get a unique file name          **/
/*------------------------------------------------------------------*/

#ifdef SUN_UNIX
   file_name = tempnam (directory, NULL);
#endif

/*------------------------------------------------------------------*/
/** IF this is a VAX THEN                                          **/
/**   use the tmpnam function to get a base name                   **/
/**   tack on an extension so VMS won't, then add the directory    **/
/*------------------------------------------------------------------*/

#ifdef VAX
   tmpnam (base_name);
   strcat (base_name, ".PDS");
   file_name = util_create_file_spec (directory, base_name);
#endif

/*------------------------------------------------------------------*/
/** IF this is DOS TURBO C THEN                                    **/
/**   use the time function to get a base name (the tmpnam function**/
/**      doesn't work when more than one program is running)       **/
/**   make a file name using the time for both name and extension  **/
/**   shift name to the left, because times are longer than 8 chars**/
/**      and add the directory                                     **/
/*------------------------------------------------------------------*/

#ifdef MSDOS_TC
   {
      time_t t;

      t = time (NULL);
      sprintf (base_name, "%ld.%ld", t, t);
      base_name[13] = EOS;
      file_name = util_create_file_spec (directory, &base_name [1]);
   }
#endif

/*------------------------------------------------------------------*/
/** ENDIF                                                          **/
/*------------------------------------------------------------------*/

   return (file_name);

/** END sys_make_temp_fname                                       **/
}

