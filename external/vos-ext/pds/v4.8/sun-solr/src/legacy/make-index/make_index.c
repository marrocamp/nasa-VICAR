
/**********************************************************************
 *$Component                                                          *
 *    int make_index (dd_file_name)                                   *
 *$Abstract                                                           *
 *    Main program for creating a DD indeex.                          *
 *$Keywords                                                           *
 *    DDINDEX                                                         *
 *    TOOL_MAIN                                                       *
 *$Inputs                                                             *
 *    dd_file_name:                                                   *
 *        The dd_file_name variable is the name of the file that      *
 *        contains the Data Dictionary definitions.                   *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    The  make_index program performs all steps necessary to generate*
 *    an alphabetized index into a file of data dictionary object and *
 *    element definitions.  The resulting index, which will be placed *
 *    in a new file, contains the names, types, byte offsets, and     *
 *    lengths of all the definitions in the given DD file.  The index *
 *    file name will be the same as the DD file name, except the      *
 *    extension will be replaced with ".IDX".  The index file will    *
 *    be written using fixed length records. In a VMS environment,    *
 *    both the DD file and the index file will be converted to RMS    *
 *    stream format.                                                  *
 *$External_References                                                *
 *    Item                     Shared-Data           Access           *
 *    ------------------------------------------------------------    *
 *    pds_message_list         pdsglob.h             read             *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.4   October 31, 1992                                          *
 *$Change_History                                                     *
 *    MDD   03-25-91   Original generation.                           *
 *    MDD   06-17-91   Added alias processing                         *
 *    MDD   10-22-91   Added DOS stack fix, and change err_write call *
 *    MDD   03-16-92   Moved stuff in from file ddindex.c             *
 *    MDD   10-31-92   Moved stream conversion statement for VMS      *
 *    DWS   08-11-98   Added unit object to index                     *
 *    DWS   12-05-01   Added code to deal with GROUP =                *
 *    MDC   04-15-04   Increased PDS_INDEX_REC_LEN from 80 to 120     *
 *                     bytes to allow the program to not truncate     *
 *                     lines when local data dictionary keywords whose*
 *                     length is greater than 30 characters long is   *
 *                     encountered                                    *
 *    MDC   10-25-04   Modified the main routine to return a fail     *
 *                     value of TRUE or FALSE                         *
 *    MDC   06-15-05   Added Software Disclaimer message to help      *
 *                     screen                                         *
 **********************************************************************/

#include "pdsdef.h"
#include "pdsglob.h"
#include "index.h"
#include "fiodef.h"
#include "errordef.h"
#include "sysdef.h"
#include "convert.h"
#include "utildef.h"

/*--------------------------------------------------------------------*/
/*                      DD Index defines and typedefs                 */
/*--------------------------------------------------------------------*/

#define PDS_DD_INDEX_EXT		".idx"
#define PDS_INDEX_LABEL_LEN             8
/*#define PDS_INDEX_REC_LEN               80 */
#define PDS_INDEX_REC_LEN                 120     /* 04-09-04 MDC */
#define MAKE_INDEX_VERSION               "1.10"    


#ifdef MSDOS_TC

extern unsigned _stklen = 65535U;

#endif

    /*----------------------------------------------------------------*/
    /*  These are the function prototypes that are used by all ANSI   */
    /*  standard C compilers.  At this time, SUN systems running UNIX */
    /*  do not allow this type of prototype declaration.              */
    /*----------------------------------------------------------------*/

#ifndef SUN_UNIX                             

INDEX_ENTRY *dd_make_index (char *, long *, long *);  /* DWS 08-11-98 */

char *dd_make_index_name (char *);

LOGICAL dd_write_index (INDEX_ENTRY *, char *, char *, long, long);

void dd_write_index_entry (INDEX_ENTRY *, FILE *);

    /*----------------------------------------------------------------*/
    /*  These are the function prototypes that are used by C          */
    /*  compilers that do not follow the ANSI standard for the        */
    /*  declaration of function prototypes.                           */
    /*----------------------------------------------------------------*/

#else

INDEX_ENTRY *dd_make_index ();
char *dd_make_index_name ();
LOGICAL dd_write_index ();
void dd_write_index_entry ();

#endif

int main (argc, argv)

int argc;
char *argv [];

{
   char dd_file_name [PDS_MAXLINE];
   INDEX_ENTRY *index_root = NULL;
   char *dd_index_name = NULL;
   long dd_alias_offset;
   long dd_units_offset;                                        /*dws 08-11-98*/
   LOGICAL wrote_it = {FALSE};
 /*  LOGICAL success = TRUE; */
   LOGICAL fail = FALSE;


/** BEGIN **/

    printf ("PDS Make Dictionary Index - Version %s\n\n", MAKE_INDEX_VERSION);

   /*-----------------------------------------------------------------*/
   /** setup temporary files                                         **/
   /*-----------------------------------------------------------------*/

   fio_setup ();

   /*-----------------------------------------------------------------*/
   /** IF the number of arguments is correct THEN                    **/
   /*-----------------------------------------------------------------*/

   if (argc == 2)
   {

      /*--------------------------------------------------------------*/
      /** get the DD file name from the argument list                **/
      /*--------------------------------------------------------------*/

      strcpy (dd_file_name, argv [1]);

      /*--------------------------------------------------------------*/
      /** IF this is a VAX THEN                                      **/
      /**    convert the DD file to stream format                    **/
      /*--------------------------------------------------------------*/

#ifdef VAX
	    fio_convert_file (dd_file_name, PDS_RF_UNKNOWN,
                              PDS_RF_STREAM_LF, 0, 0);
#endif

      /*--------------------------------------------------------------*/
      /** ENDIF this is a VAX...                                     **/
      /** build the index tree                                       **/
      /*--------------------------------------------------------------*/      

      index_root = dd_make_index (dd_file_name, &dd_alias_offset, &dd_units_offset);

      /*--------------------------------------------------------------*/
      /** IF we grew a tree THEN                                     **/
      /*--------------------------------------------------------------*/

      if (index_root != NULL)
      {
	 /*-----------------------------------------------------------*/
	 /** create an index file name from the DD file name         **/
	 /** write out the sorted index                              **/
     /** chop down the tree and clean up stuff                   **/
	 /*-----------------------------------------------------------*/

	     dd_index_name = dd_make_index_name (dd_file_name);
	     wrote_it = dd_write_index (index_root, dd_file_name, dd_index_name,
				    dd_alias_offset, dd_units_offset);
		 /* 10-25-04 Added by MDC */
		 if(wrote_it == FALSE)
		 {
		/*	success = FALSE; */
			 fail = TRUE;
		 }
         
		 Lemme_Go(dd_index_name);
         index_root = idx_cleanup_index (index_root);
      }
      /*--------------------------------------------------------------*/
      /** ENDIF we grew a tree...                                    **/
      /*--------------------------------------------------------------*/
	  else /* 10-25-04 Added by MDC */
	  {
/*		  success = FALSE; */
		  fail = TRUE;
	  }
   }
   /*-----------------------------------------------------------------*/
   /** ELSE                                                          **/
   /**   command line was wrong so print an error                    **/
   /*-----------------------------------------------------------------*/

   else
   {
      err_append_message (INFO, "Usage: make_index <DD-file-name>");
/*	  success = FALSE; */
	  err_append_message (INFO, "\nDisclaimer:");
      err_append_message (INFO, "Copyright 2006-2007, by the California Institute of Technology.");
      err_append_message (INFO, "ALL RIGHTS RESERVED. United States Government Sponsorship acknowledged.");
      err_append_message (INFO, "Any commercial use must be negotiated with the Office of Technology Transfer");
      err_append_message (INFO, "at the California Institute of Technology.\n");
      err_append_message (INFO, "This software is subject to U. S. export control laws and regulations");
      err_append_message (INFO, "(22 C.F.R. 120-130 and 15 C.F.R. 730-774). To the extent that the software");
      err_append_message (INFO, "is subject to U.S. export control laws and regulations, the recipient has");
      err_append_message (INFO, "the responsibility to obtain export licenses or other export authority as");
      err_append_message (INFO, "may be required before exporting such information to foreign countries or");
      err_append_message (INFO, "providing access to foreign nationals.");
	  fail = TRUE;
   }

   /*-----------------------------------------------------------------*/
   /** ENDIF the number of arguments is correct...                   **/
   /** write out and throw away the errors                           **/
   /*-----------------------------------------------------------------*/

   err_write_to_file (NULL, FALSE);
   err_deallocate_list (pds_message_list);
   fio_exit ();
/*   return success; */
   return fail;
   /** END make_index                                                **/
}


/**********************************************************************
 *$Component                                                          *
 *    INDEX_ENTRY *dd_make_index (dd_file_name)                       *
 *$Abstract                                                           *
 *    Creates the index entries for the Data Dictionary definitions.  *
 *$Keywords                                                           *
 *    DDINDEX                                                         *
 *    TOOL_MAIN                                                       *
 *$Inputs                                                             *
 *    dd_file_name:                                                   *
 *        The dd_file_name variable is the name of the file that      *
 *        contains the Data Dictionary definitions.                   *
 *$Outputs                                                            *
 *    dd_alias_offset:                                                *
 *       The dd_alias_offset variable is the offset of the ALIAS_LIST *
 *       object in the data dictionary definition file.               *
 *    dd_units_offset:                                                *
 *       The dd_units_offset variable is the offset of the UNIT_LIST  *
 *       object in the data dictionary definition file.               *
 *$Returns                                                            *
 *    index_root:                                                     *
 *        The index_root variable is a pointer to the root of a       *
 *        sorted tree of index entries.                               *
 *$Detailed_Description                                               *
 *    The dd_make_index routine creates index entries for all the     *
 *    definitions in the given data dictionary file and adds them to  *
 *    a sorted tree of index entries.                                 *
 *$External_References                                                *
 *    None                                                            *
 *$Error_Handling                                                     *
 *    If definitions are found which do not have NAME fields, or the  *
 *    data dictionary file cannot be opened, then error messages are  *
 *    appended onto the global list of messages. If no definitions are*
 *    found in the DD file, a message is appended to the list, and    *
 *    this routine returns NULL.                                      *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.4   October 19, 1992                                          *
 *$Change_History                                                     *
 *    MDD   03-21-91   Original generation.                           *
 *    MDD   06-17-91   Added output dd_alias_offset and code to       *
 *                     set its value                                  *
 *    MDD   10-16-91   Added DOS fixes: int -> long                   *
 *    MDD   10-21-91   Fixed malloc, free, and sys_exit_system calls  *
 *    MDD   11-21-91   Changed to set offsets using ftell when Turbo  *
 *                     C is being used.                               *
 *    MDD   03-16-92   Moved stuff in from file ddindex.c             *
 *    MDD   10-19-92   Initialized dd_alias_offset                    *
 **********************************************************************/


INDEX_ENTRY *dd_make_index (dd_file_name, dd_alias_offset, dd_units_offset)

char *dd_file_name;
long *dd_alias_offset;
long *dd_units_offset;
{
    long begin_offset = 0;
    long current_offset = 0;
	char input_line [PDS_MAXLINE]={0};
    INDEX_ENTRY *index_root = NULL;
	char dd_name [PDS_ELEMENT_LEN + 1]={0};
	char *unquoted_dd_name = NULL;
    FILE *dd_file = NULL;
    INDEX_ENTRY *new_entry = NULL;
	char dd_type [PDS_DD_TYPE_LEN + 1]={0};
	char end_obj_str [PDS_MAXLINE]={0};
	char end_grp_str [PDS_MAXLINE]={0};
    
/** BEGIN **/
    /*-------------------------------------------------------------------*/
    /** IF the data dictionary file can be opened THEN                  **/
    /*-------------------------------------------------------------------*/

    *dd_alias_offset = 0;
    dd_file = fopen (dd_file_name, "rb");
    if (dd_file != NULL)
    {
       /*----------------------------------------------------------------*/
       /** WHILE there are more lines in the data dictionary            **/
       /*----------------------------------------------------------------*/
      
       while (fgets (input_line, PDS_MAXLINE, dd_file))
       {
          /*------------------------------------------------------------*/
          /** update the file offset counters                          **/
          /** clean up the input string                                **/
          /** determine what type of definition has been found, if any **/
          /*------------------------------------------------------------*/
          
          begin_offset = current_offset;

          /*------------------------------------------------------------*/
          /* Here's an oddity: You can't seek on VMS variable length    */
          /* files, and so the file offset of the definition is tracked */
          /* by counting bytes read rather than ftell-ing. But for      */
          /* DOS, that doesn't work, because the number of bytes read   */
          /* does not equal the value returned by ftell, so for later   */
          /* seeks to work, we have to use ftell to obtain the offset.  */
          /*------------------------------------------------------------*/

#ifdef MSDOS_TC
          current_offset = ftell (dd_file);
#else
          current_offset += (long) strlen (input_line);
#endif

          util_clean_up_string (input_line);
          util_remove_char (input_line, ' ');
          util_remove_char (input_line, '\n');
          util_upper_case (input_line);
          if (strcmp (input_line, "OBJECT=GENERIC_OBJECT_DEFINITION") == 0 ||
              strcmp (input_line, "OBJECT=SPECIFIC_OBJECT_DEFINITION") == 0 ||
              strcmp (input_line, "OBJECT=ELEMENT_DEFINITION") == 0)
          {
             sscanf (input_line, "OBJECT=%s", dd_type);
		  }
          else
          {
		       if (strcmp (input_line, "GROUP=GENERIC_OBJECT_DEFINITION") == 0 ||
			      strcmp (input_line, "GROUP=SPECIFIC_OBJECT_DEFINITION") == 0 ||
				  strcmp (input_line, "GROUP=ELEMENT_DEFINITION") == 0)
			   {
                   sscanf (input_line, "GROUP=%s", dd_type);
			   }
               else
			   {
                   strcpy (dd_type, "");
			   }
		  }
          /*------------------------------------------------------------*/
          /** IF a definition was found THEN                           **/
          /*------------------------------------------------------------*/

          if (strcmp (dd_type, "") != 0)
          {

             /*---------------------------------------------------------*/
             /** read the next line and clean it up                    **/
             /*---------------------------------------------------------*/

             fgets (input_line, PDS_MAXLINE, dd_file);

#ifndef MSDOS_TC
             current_offset += (long) strlen (input_line);
#endif
             util_clean_up_string (input_line);
             util_strip_lead_and_trail (input_line, ' ');
             util_upper_case (input_line);

             /*---------------------------------------------------------*/
             /** IF the line contains a NAME parameter THEN            **/
             /*---------------------------------------------------------*/

             if (util_locate_string (input_line, "NAME") == input_line)
             { 
                /*------------------------------------------------------*/
                /**    create a new index entry                        **/
                /**    initialize it                                   **/
                /*------------------------------------------------------*/

                sscanf (input_line, " NAME = %s ", dd_name);
				unquoted_dd_name = util_remove_char(dd_name, '"');
				new_entry = (INDEX_ENTRY *) malloc (sizeof (INDEX_ENTRY));
				Check_Malloc(new_entry);
				new_entry -> byte_offset = begin_offset;
                new_entry -> right_child = NULL;
                new_entry -> left_child = NULL;
                Malloc_String(new_entry -> name, String_Size(unquoted_dd_name));
                strcpy (new_entry -> name, unquoted_dd_name);
                strcpy (new_entry -> type, dd_type);

                /*------------------------------------------------------*/
                /**    locate the end of the definition                **/
                /*------------------------------------------------------*/

                sprintf (end_obj_str, "END_OBJECT=%s", dd_type);
                sprintf (end_grp_str, "END_GROUP=%s", dd_type);
                do
                {
                   fgets (input_line, PDS_MAXLINE, dd_file);
                   current_offset += (long) strlen (input_line);
                   util_clean_up_string (input_line);
                   util_remove_char (input_line, ' ');
                   util_remove_char (input_line, '\n');
                   util_upper_case (input_line);
				}
                while (((strcmp (end_obj_str, input_line) != 0) &&
					   (strcmp (end_grp_str, input_line) != 0)) &&
                                  !feof (dd_file));

                /*------------------------------------------------------*/
                /**    set the size of the new entry                   **/
                /**    add the new entry to the index                  **/
                /*------------------------------------------------------*/

                new_entry -> size = current_offset - begin_offset;
                index_root = idx_insert_index_entry (index_root, new_entry);
	     }
             /*---------------------------------------------------------*/
             /** ELSE a name field was not found                       **/
             /**    so print a warning                                 **/
             /*---------------------------------------------------------*/

             else
             {
                err_append_message (WARNING, "NAME field not found in data dictionary definition");
             }
             /*---------------------------------------------------------*/
             /** ENDIF a NAME parameter is found...                    **/
             /*---------------------------------------------------------*/
          }
          /*------------------------------------------------------------*/
          /** ELSE IF this is the alias list object THEN               **/
          /**    save the current offset                               **/
          /*------------------------------------------------------------*/

          else if (strcmp (input_line, "OBJECT=ALIAS_LIST") == 0)
          {
             *dd_alias_offset = begin_offset;
          }

          /*------------------------------------------------------------*/ /*dws 08-11-98*/
          /** ELSE IF this is the units list object THEN               **/ /*            */
          /**    save the current offset                               **/ /*            */
          /*------------------------------------------------------------*/ /*            */
                                                                           /*            */
          else if (strcmp (input_line, "OBJECT=UNIT_LIST") == 0)           /*            */
          {                                                                /*            */ 
             *dd_units_offset = begin_offset;                              /*            */ 
          }                                                                /*dws 08-11-98*/
          /*------------------------------------------------------------*/
          /** ENDIF a definition is found...                           **/
          /*------------------------------------------------------------*/
       }
       /*---------------------------------------------------------------*/
       /** ENDWHILE there are more lines...                            **/
       /*---------------------------------------------------------------*/

       fclose (dd_file);
    }
    /*------------------------------------------------------------------*/
    /** ELSE                                                           **/
    /**    DD file cannot be opened so print an error                  **/
    /*------------------------------------------------------------------*/
    else
    {
       err_append_message (ERROR1, 
            "The data dictionary file you specified does not exist or cannot be read.");
    }
    /*------------------------------------------------------------------*/
    /** ENDIF the DD file can be opened...                             **/
    /** IF no definitions were found print an error                    **/
    /*------------------------------------------------------------------*/

    if (index_root == NULL)
    {
       err_append_message (ERROR1,
     "The data dictionary file you specified does not contain any object or element definitions.");
    }
    return (index_root);

/** END dd_make_index **/
}


/**********************************************************************
 *$Component                                                          *
 *    char *dd_make_index_name (dd_file_name)                         *
 *$Abstract                                                           *
 *    Creates the name of the DD index file                           *
 *$Keywords                                                           *
 *    DDINDEX                                                         *
 *    TOOL_MAIN                                                       *
 *$Inputs                                                             *
 *    dd_file_name:                                                   *
 *        The dd_file_name variable is the name of the file that      *
 *        contains the Data Dictionary information.                   *
 *$Returns                                                            *
 *    dd_index_name:                                                  *
 *        The dd_index_name variable is the name of the file that     *
 *        contains the Data Dictionary index information.             *
 *$Detailed_Description                                               *
 *    The dd_make_index_name routine allocates memory for the index   *
 *    name based on the Data Dictionary name, adds an                 *
 *    IDX extension in the same case as the Data Dictionary name, and *
 *    returns the index name.                                         * 
 *$External_References                                                *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    This routine allocates memory for its return value which must   *
 *    be freed elsewhere.                                             *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.2   March 16, 1992                                            *
 *$Change_History                                                     *
 *    MDD   03-21-91   Original generation.                           *
 *    MDD   10-21-91   Fixed malloc, free, and sys_exit_system calls  *
 *    MDD   03-16-92   Moved stuff in from file ddindex.c             *
 **********************************************************************/


char *dd_make_index_name (dd_file_name)

char *dd_file_name;
{
   char *dd_index_name = NULL;
   char *ext_ptr = NULL;
   static char *dd_index_ext = PDS_DD_INDEX_EXT;
   char *directory = NULL;
   char temp_str [PDS_MAXLINE];

/** BEGIN **/
   /*---------------------------------------------------------------------*/
   /** allocate memory for the new name and copy the DD name to it       **/
   /*---------------------------------------------------------------------*/

   Malloc_String(dd_index_name, (String_Size(dd_file_name) + 
                                        strlen (PDS_DD_INDEX_EXT)));
   strcpy (dd_index_name, dd_file_name);
     
   /*---------------------------------------------------------------------*/
   /** make life easier by converting the name to Unix format            **/
   /*---------------------------------------------------------------------*/

#ifndef SUN_UNIX
   directory = util_save_to_last_occurrence (dd_index_name, ':');
#endif

#ifdef VAX
   dd_index_name = cvt_vax_to_unix_file_spec (dd_index_name);
#endif

#ifdef MSDOS_TC
   dd_index_name = cvt_dos_to_unix_file_spec (dd_index_name);
#endif   

   /*---------------------------------------------------------------------*/
   /** locate the old file extension                                     **/
   /** convert the new extension to upper case if the old one was upper  **/
   /**    case                                                           **/
   /** replace the old extension with the new one, or append it if       **/
   /**    there wasn't one                                               **/
   /*---------------------------------------------------------------------*/
   
   ext_ptr = (char *) strrchr (dd_index_name, '.');
   if (util_is_upper (dd_index_name))
      util_upper_case (dd_index_ext);
   if (ext_ptr == NULL || ext_ptr < (char *) strrchr (dd_index_name, '/'))
      strcat (dd_index_name, dd_index_ext);
   else
      strcpy (ext_ptr, dd_index_ext);

   /*---------------------------------------------------------------------*/
   /** convert the file spec back to the type expected by the OS         **/
   /*---------------------------------------------------------------------*/

#ifdef VAX
   dd_index_name = cvt_unix_to_vax_file_spec (dd_index_name);
#endif

#ifdef MSDOS_TC
   dd_index_name = cvt_unix_to_dos_file_spec (dd_index_name);
#endif   

#ifndef SUN_UNIX
   if (directory != NULL)
   {
      sprintf (temp_str, "%s%s", directory, dd_index_name);
      strcpy (dd_index_name, temp_str);
      Lemme_Go(directory);
   }
#endif

   return (dd_index_name);

/** END dd_make_index_name **/
}

/**********************************************************************
 *$Component                                                          *
 *    LOGICAL dd_write_index (index_root, dd_file_name,               *
 *                            dd_index_name, dd_alias_offset)         *
 *$Abstract                                                           *
 *    Writes out a DD index file                                      *
 *$Keywords                                                           *
 *    DDINDEX                                                         *
 *    TOOL_MAIN                                                       *
 *$Inputs                                                             *
 *    index_root:                                                     *
 *        The index_root variable is a pointer to the root of a       *
 *        sorted tree of index entries.                               *
 *    dd_file_name:                                                   *
 *        The dd_file_name variable is the name of the file that      *
 *        contains the Data Dictionary information.                   *
 *    dd_index_name:                                                  *
 *        The dd_index_name variable is the name of the file that     *
 *        contains the Data Dictionary index information.             *
 *    dd_alias_offset:                                                *
 *       The dd_alias_offset variable is the offset of the ALIAS_LIST *
 *       object in the data dictionary definition file.               *
 *$Returns                                                            *
 *    success_flag:                                                   *
 *        The success_flag variable is a general purpose variable     *
 *        which indicates success (a value of TRUE) or failure.       *
 *$Detailed_Description                                               *
 *    The dd_write_index routine writes the DD index file header      *
 *    information to the DD index file and then invokes the routines  *
 *    to write out the sorted index.  The DD file name input is used  *
 *    as part of the index header information. The resulting index    *
 *    file is converted to fixed length format.                       *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.3   March 16, 1992                                            *
 *$Change_History                                                     *
 *    MDD   03-21-91   Original generation.                           *
 *    MDD   06-17-91   Added input dd_alias_offset                    *
 *    MDD   10-16-91   Added DOS fixes                                *
 *    MDD   10-21-91   Fixed malloc, free, and sys_exit_system calls  *
 *    MDD   03-16-92   Moved stuff in from file ddindex.c             *
 **********************************************************************/
     
LOGICAL dd_write_index (index_root, dd_file_name, dd_index_name, 
                        dd_alias_offset, dd_units_offset)

INDEX_ENTRY *index_root;
char *dd_file_name;
char *dd_index_name;
long dd_alias_offset;
long dd_units_offset;

{
   FILE *dd_index;
   char info_msg [PDS_MAXLINE];
   struct tm *date;
   LOGICAL success = TRUE;
   char *date_string;
   char the_date[11];
   char temp_place[10];
/** BEGIN **/
   /*-----------------------------------------------------------------------*/
   /** IF the index file can't be opened THEN                              **/
   /**   print an error                                                    **/
   /*-----------------------------------------------------------------------*/
   the_date[10] = '\0';
   dd_index = fopen (dd_index_name, "w");
   if (dd_index == NULL)
   {
      err_append_message (ERROR1, 
           "Could not create the data dictionary index file in this directory.");
      success = FALSE;
   }
   /*-----------------------------------------------------------------------*/
   /** ELSE                                                                **/
   /*-----------------------------------------------------------------------*/

   else
   {
      /*--------------------------------------------------------------------*/
      /** write header information to the index file                       **/
      /*--------------------------------------------------------------------*/

      date = sys_get_date ();
	  date_string = sys_get_ascii_date ();

      strcpy(the_date, date_string + 20);
	  strcat(the_date, "-");
	  sprintf(temp_place, "%d", date ->tm_mon + 1);

/*	  itoa(date ->tm_mon + 1, temp_place, 10);*/
	  strcat(the_date,  temp_place);
	  strcat(the_date, "-");
	  sprintf(temp_place, "%d", date ->tm_mday);
/*	  itoa(date ->tm_mday, temp_place, 10);*/
	  strcat(the_date, temp_place);

	  fprintf (dd_index, "CCSD3ZF0000100000001NJPL3IF0PDS200000001\n");
      fprintf (dd_index, "^DD_FILE = \"%s\"\n", dd_file_name);
/*      fprintf (dd_index, "GENERATION_DATE = %d-%d-%d\n", date -> tm_year,
                             date -> tm_mon + 1, date -> tm_mday);     */
      fprintf (dd_index, "GENERATION_DATE = %s\n", the_date);

      fprintf (dd_index, "RECORD_TYPE = FIXED_LENGTH\n");
      fprintf (dd_index, "RECORD_LENGTH = %d\n", PDS_INDEX_REC_LEN);
      fprintf (dd_index, "LABEL_RECORDS = %d\n", PDS_INDEX_LABEL_LEN);
      fprintf (dd_index, "^ALIAS_LIST = %ld <BYTES>\n", dd_alias_offset);
      fprintf (dd_index, "^UNIT_LIST = %ld <BYTES>\n", dd_units_offset);
      fprintf (dd_index, "END\n");

      /*--------------------------------------------------------------------*/
      /** write the index entries to the file                              **/
      /*--------------------------------------------------------------------*/

      dd_write_index_entry (index_root, dd_index);   
      fclose (dd_index);

      /*--------------------------------------------------------------------*/
      /** convert the file to fixed format in a temporary file             **/
      /*--------------------------------------------------------------------*/
 
#ifdef MSDOS_TC
      if (fio_convert_file (dd_index_name, DEFAULT_REC_TYPE, PDS_RF_FIXED_CRLF,
                            0, PDS_INDEX_REC_LEN))
#else
      if (fio_convert_file (dd_index_name, DEFAULT_REC_TYPE, PDS_RF_FIXED_LF,
                            0, PDS_INDEX_REC_LEN))
#endif
      {
         sprintf (info_msg, "Data dictionary index file for %s was written to %s",
                     dd_file_name, dd_index_name);
         err_append_message (INFO, info_msg);
      }
      else
      {
         success = FALSE;
      }
      Lemme_Go(date_string);
   }
   /*-----------------------------------------------------------------------*/
   /** ENDIF the DD file cannot be opened...                               **/
   /*-----------------------------------------------------------------------*/

   return (success);
   
/** END dd_write_index **/
}

/**********************************************************************
 *$Component                                                          *
 *    void dd_write_index_entry (index_root, dd_index_file)           *
 *$Abstract                                                           *
 *    Writes out a sorted DD index tree                               *
 *$Keywords                                                           *
 *    DDINDEX                                                         *
 *    TOOL_MAIN                                                       *
 *$Inputs                                                             *
 *    index_root:                                                     *
 *        The index_root variable is a pointer to the root of a       *
 *        sorted tree of index entries.                               *
 *    dd_index_file:                                                  *
 *        The dd_index_file variable is a file pointer to a pre-opened*
 *        file containing a DD index.                                 *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    The dd_write_index_entry routine will write any subtree of a    *
 *    sorted DD index tree to an index file.  It is a recursive       *
 *    procedure.                                                      *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore /JPL                                            *
 *$Version_and_Date                                                   *
 *    1.1   March 16, 1992                                            *
 *$Change_History                                                     *
 *    MDD   03-21-91   Original generation.                           *
 *    MDD   03-16-92   Moved stuff in from file ddindex.c             *
 **********************************************************************/

void dd_write_index_entry (index_root, dd_index_file)

INDEX_ENTRY *index_root;
FILE *dd_index_file;

{
   if (index_root != NULL)
   {
      dd_write_index_entry (index_root -> left_child, dd_index_file);
      fprintf (dd_index_file, "%-*s %-*s %7ld %7ld\n", PDS_ELEMENT_LEN,
                  index_root -> name, PDS_DD_TYPE_LEN,
                  index_root -> type, index_root -> byte_offset,
                  index_root -> size);
      dd_write_index_entry (index_root -> right_child, dd_index_file);
   }
   return;
}




