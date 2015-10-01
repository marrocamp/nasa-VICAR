/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Component                                                           * 
 *    tbtool.c                                                         *
 * Abstract                                                            *
 *    PDS Table Browser, ASCII version                                 *
 * Detailed Description                                                *
 *    This file contains the driver for the ASCII version of the PDS   *
 *    Table Browser and the routines that are specific to it.          *
 * Internal References                                                 *
 *    main                                                             *
 *    tb_cleanup                                                       *
 *    tb_convert_data_file:                                            *
 *    tb_display_column                                                *
 *    tb_display_help                                                  *
 *    tb_display_label                                                 *
 *    tb_edit_label_info                                               *
 *    tb_format_bit_column_value                                       *
 *    tb_format_column_value                                           *
 *    tb_get_bit_column_info                                           *
 *    tb_get_column_info                                               *
 *    tb_get_command                                                   *
 *    tb_get_next_table                                                *
 *    tb_get_prev_table                                                *
 *    tb_get_file_info                                                 *
 *    tb_get_table_info                                                *
 *    tb_jump_to_row                                                   *
 *    tb_justify_bit_column_value                                      *
 *    tb_move_backward_by_column_name                                  *
 *    tb_move_by_column_name                                           *
 *    tb_move_forward_by_column_name                                   *
 *    tb_move_left                                                     *
 *    tb_move_right                                                    *
 *    tb_navigate                                                      *
 *    tb_read_column                                                   *
 *    tb_replace_from_list                                             *
 *    tb_select_label_file                                             *
 *    tb_setup                                                         *
 * Authors and Institutions                                            *
 *    David P. Bernath / J.P.L.                                        *
 * Version and Date                                                    *
 *    1.0   June 01, 1992                                              *
 *    1.1   August 13, 2002											   *
 * Change History                                                      *
 *    DPB   06-01-92   Original code.                                  *
 *    TC    04-26-93   three function routines added to facilitate     *
 *                     locating the column by column name or           *
 *                     bit_column by bit_column name                   *
 *	  MDC   08-09-02   Added a small routine in the tb_display_column  *
 *					   function to display an error msg. if a real,    *
 *					   invalid value was obtained.					   *
 *					   The variable, command, in the tb_get_command	   *
 *					   function and the short variable, c, in the	   *
 *					   tb_select_label_file function are both		   *
 *					   initialized to 0 instead of NULL to clean up    *
 *					   warning msgs.								   *
 *																	   *
 *	  MDC	08-15-02   Added a check to see if a number was entered    *
 *					   before or after the 'up' or 'down' commands.    *
 *					   If there was a number found, this indicates that*
 *					   the user would like to jump that many rows up   *
 *					   or down the column. This was originally stated  *
 *					   in the help description of the program, but was *
 *					   never implemented.							   *
 *																	   *
 *  MDC   10-10-02  1) Added functionality in the routine,             *
 *   				   tb_get_column_info, to handle containers        *
 *					2) Added a routine, get_container_params, to get   * 
 *					   the Keyword values of a container.              *
 *					3) Added the ability to show how many containers   *
 *					   are in a table and whether a column is in a     *
 *					   container or not. Also, the user has the        *
 *					   ability to change these values just like it     *
 *					   currently can with the other types of		   *
 *					   information given by tbtool.				       *
 *					4) Added initializations to the un-initalized      *
 *					   members of new_ptr in tb_get_bit_column_info    *
 *					   routine to stop tbtool from crashing when bit   *
 *					   column info is being generated.				   *
 *																	   *
 *  MDC	  11-25-02  1) Fixed a memory leak program in tbtool by        *
 *					   de-allocating 2 pointers that were allocated,   *
 *					   but never previously de-allocted.			   *
 *																	   *
 *	MDC	  12-05-02  1) Added new user commands to browse through	   *
 *					   container columns in a table.				   *
 *					2) Updated container display information to be	   *
 *					   more informative. It now displays the container *
 *					   name when displaying one of its columns.		   *
 *																	   *
 *  MDC   12-09-02  1) Updated HELP text							   *
 *																	   *
 *	MDC   02-26-03  1) Added a 3rd argument to pass in to the		   *
 *					   tb_get_column_info routine.					   *
 *					2) Updated tb_get_column_info routine to correctly *
 *					   caclulate the actual start byte values of	   *
 *					   labels containing multiple, nested containers.  *
 *					3) Updated the TB_CONTAINER case in the			   *
 *					   tb_navigate routine to correctly display the    *
 *					   container that the user wants to see.		   *
 *					4) Separated a piece of code in the				   *
 *					   get_container_params routine that deals with    *
 *					   adding a container to a link list of container  *
 *					   objects and created a routine called			   *
 *					   append_to_container_list. This is just for the  *
 *					   sake of making the code more modular.		   *
 *  DWS   11-14-03  1) Recompiled to link new ODLC that had been       *
 *					   modified to allow NULL followed by units id     *
 *  MDC   06-15-05  Added software disclaimer message to help screen   *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
#include  <stdio.h>
#include  <stdlib.h>

#include "pdsdef.h"
#include "pdsglob.h"
#include "label.h"
#include "labutil.h"
#include "labtool.h"
#include "formtype.h"
#include "fiodef.h"
#include "sysdef.h"
#include "search.h"
#include "utildef.h"
#include "tbtool.h"

void        tb_move_by_column_name();              /* TC_MOD_26APR93 */
COLUMN_INFO *tb_move_backward_by_column_name ();   /* TC_MOD_26APR93 */
COLUMN_INFO *tb_move_forward_by_column_name ();    /* TC_MOD_26APR93 */


/**********************************************************************
 *$Component                                                          *
 *    void tbtool (label_fname)                                       *
 *$Abstract                                                           *
 *    Main routine for the ASCII version of the Table Browser         *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    DRIVER                                                          *
 *$Inputs                                                             *
 *    label_fname:                                                    *
 *        The the label_fname variable is a character string          *
 *        containing the name of the label file to be browsed.        *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    The tbtool main routine is the driver for the ASCII interface   *
 *    version of the PDS Table Browser .                              *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.0   January 10, 1992                                          *
 *	  1.1   August 1, 2002											  *
 *	  1.2   October 10, 2002										  *
 *$Change_history                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    DWS   05-27-02   made changes to ODL to allow STRUCTURE pointers*
 *                     to point to files that are in the LABEL        *
 *                     directory instead of just in the same directory*
 *                     as the LBL file.                               *
 *	  MDC   08-01-02   The variable, command, in the tb_get_command   *
 *					   function and the short variable, c, in the     *
 *					   tb_select_label_file function are both		  *
 *					   initialized to 0 instead of NULL to clean up   *
 *					   warning msgs.								  *
 **********************************************************************/

#ifdef MSDOS_TC

extern unsigned _stklen = 65535U;

#endif

main (argc, argv)

int argc;
char *argv [];

{
    char command_str[80];
 	short command = {TB_UNKNOWN};
    
	/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Perform local and toolbox setup functions                           **/
    /*-----------------------------------------------------------------------*/

    fio_setup ();
    tb_setup ();

    printf ("\n\n  Welcome to the PDS Table Browser version %s \n\n", VERSION);

    /*-----------------------------------------------------------------------*/
    /** Select the first label file for viewing                             **/
    /*-----------------------------------------------------------------------*/

    *command_str = EOS;
    if (argc > 1) strcpy(command_str, argv[1]);
    tb_navigate (TB_SELECT, command_str);

    /*-----------------------------------------------------------------------*/
    /** Keep looping and processing user commands until EXIT is entered     **/
    /*-----------------------------------------------------------------------*/

    while ((command = tb_get_command (command_str)) != TB_EXIT)
        tb_navigate (command, command_str);

    /*-----------------------------------------------------------------------*/
    /** Perform local and toolbox cleanup functions                         **/
    /*-----------------------------------------------------------------------*/

    tb_cleanup ();
    fio_exit ();

	return;
/** END **/
	
}  /*  "tb_tool"  */

                                  

    
/**********************************************************************
 *$Component                                                          *
 *    void tb_cleanup ()                                              *
 *$Abstract                                                           *
 *    Performs Table Browser cleanup                                  *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *$Inputs                                                             *
 *    None                                                            *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    This routine deallocates the storage used by the Table Browser  *
 *    prior to exiting the program, or when starting with a new       *
 *    label file as input.                                            *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   March 8, 1993                                             *
 *$Change_history                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDD   03-08-93   Updated header/comments                        *
 *	  MDC	11-25-02   Added code to de-allocate a STRING_LIST object *
 *					   and a char pointer object. This fixed a		  *
 *					   memory leak in the program.					  *
 *	  MDC   11-26-02   Added a de-allocation statement for the newest *
 *					   table_info structure member, container.		  *
 **********************************************************************/

void tb_cleanup ()

{                                     
    TABLE_INFO *table_info = {NULL};
    TABLE_INFO *next_table = {NULL};
    COLUMN_INFO *col_ptr = {NULL};
    COLUMN_INFO *bit_col_ptr = {NULL};
    COLUMN_INFO *next_col = {NULL};
    COLUMN_INFO *next_bit_col = {NULL};

	CONTAINER_INFO *next_container = {NULL};

/** BEGIN **/
	
	/*-----------------------------------------------------------------------*/
	/** 11-25-02 MDC														**/
	/** Added code to deallocate memory that was never de-allocated			**/
	/** properly causing a memory leak in the code. In the					**/
	/** util_append_string_list routine in the utillib.c source file,		**/
	/** memory was allocated for a STRING_LIST object and within this		**/
	/** object, memory was allocated again to store the filename of the		**/
	/** label tbtool was looking at. It turns out that these allocated		**/
	/** objects get stored under tb_label_ptr->appl2 (STRING_LIST object) and**/
	/** tb_label_ptr->first_child->appl2 (filename of the label). So, these **/
	/** need to be de-allocated. 											**/
	/*-----------------------------------------------------------------------*/
	
	if(tb_label_ptr != NULL)
	{
		Lemme_Go(tb_label_ptr->appl2);
		Lemme_Go(tb_label_ptr->first_child->appl2);
	}

    /*-----------------------------------------------------------------------*/
    /** Dealocate the storage used for the PDS label tree and clear the     **/
    /**     global message list.                                            **/
    /*-----------------------------------------------------------------------*/

    tb_label_ptr = lab_remove_label_or_template (tb_label_ptr);
    lab_clear_messages ();

    /*-----------------------------------------------------------------------*/
    /** Deallocate the storage used by TABLE_INFO, COLUMN_INFO, and the     **/
    /** BIT_COLUMN_INFO lists for the current label file.                   **/
	/**																		**/
    /** 11-26-02 MDC														**/
	/** Added a de-allocation statement to the newest member, container, in **/
	/** the table_info structure.											**/
	/*-----------------------------------------------------------------------*/

    for (table_info = tb_table_list; table_info != NULL; )
    {
		Lemme_Go(table_info -> name);
		Lemme_Go(table_info -> class);
		Lemme_Go(table_info -> fname);
	    Lemme_Go(table_info -> interchange_format);

        for (col_ptr = table_info -> columns; col_ptr != NULL; )
        {
			Lemme_Go(col_ptr -> name);
            Lemme_Go(col_ptr -> class);
            Lemme_Go(col_ptr -> data_type);
            Lemme_Go(col_ptr -> display_format);

            for (bit_col_ptr = col_ptr -> bit_columns; bit_col_ptr != NULL; )
            {
                Lemme_Go(bit_col_ptr -> name);
                Lemme_Go(bit_col_ptr -> data_type);
                Lemme_Go(bit_col_ptr -> display_format);

                next_bit_col = bit_col_ptr -> next;
                Lemme_Go(bit_col_ptr);
                bit_col_ptr = next_bit_col;

            }  /*  End:  "for (bit_col_ptr = ..."  */

            next_col = col_ptr -> next;
            Lemme_Go(col_ptr);
            col_ptr = next_col;

        }  /*  End:  "for (col_ptr = ..."  */
		
		/*------------------------------------------------------------*/
		/* 11-26-02 MDC												  */
		/* Grab the container object member, de-allocate it, then	  */
		/* move to the next container object and repeat this process  */
		/* until all the container objects have been de-allocatted.   */
		/*------------------------------------------------------------*/
		
		while( table_info->container != NULL )
		{
            next_container = table_info->container->next_container;
			
			Lemme_Go(table_info->container->name);
			Lemme_Go(table_info->container);
			
			table_info->container = next_container;
		} 

        next_table = table_info -> next;
        Lemme_Go(table_info);
        table_info = next_table;

    }  /*  End:  "for (table_info = ..."  */

    Lemme_Go(tb_record_type);
    Lemme_Go(tb_blocking_type);

    return;         

/** END **/

}  /*  "tb_cleanup"  */                      
                                                                   

/**********************************************************************
 *$Component                                                          *
 *    void tb_convert_data_file (fname)                               *
 *$Abstract                                                           *
 *    Checks and converts VMS VARIABLE data files to STREAM_LF        *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *$Inputs                                                             *
 *    fname:                                                          *
 *        The fname variable is a character string containing the     *
 *        name of a file.                                             *
 *$Outputs                                                            *
 *    fname:                                                          *
 *        The fname variable is a character string containing the     *
 *        name of a file.                                             *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    The tb_convert_data_file converts data files that are in VMS    *
 *    Variable-length record format to STREAM_LF format. If the file  *
 *    is converted, it is written to a temporary file and the input   *
 *    file name is reset to the new name.                             *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    macro_interface         macro.h                  update         *
 *    tbtool globals          tbtool.h                 read           *
 *$Side_Effects                                                       *
 *    A new file with a name beginning with "Z" may be written to the *
 *    current directory. The memory used to store the input file name *
 *    may be realloc'd.                                               *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   March 8, 1993                                             *
 *$Change_history                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDD   03-08-93   Updated header/comments                        *
 **********************************************************************/

void tb_convert_data_file (fname)

char *(*fname);

{
    char c;
    char *temp_fname = {NULL};

#ifdef VAX



/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Set the fields in the global variable that the MACRO routine needs  **/
    /*-----------------------------------------------------------------------*/

    macro_interface.varflag = 0;
    macro_interface.fname = *fname;
    macro_interface.fnlen = strlen(*fname);

    /*-----------------------------------------------------------------------*/
    /** Call the MACRO routine to check the format of the file              **/
    /*-----------------------------------------------------------------------*/

    IsVmsVar( );

    /*-----------------------------------------------------------------------*/
    /** IF the data file is in VMS Variable length format THEN              **/
    /*-----------------------------------------------------------------------*/

    tb_vms_var = (LOGICAL) macro_interface.varflag;

    if (tb_vms_var)
    {
        printf ("\n  Your data file is in VMS variable-length record format.\n\n");
        printf ("  It must be converted to Stream_LF format in order to view the data.\n");
        printf ("  Is it ok to proceed with the conversion? ");
        Read_Char(c)

        /*-------------------------------------------------------------------*/
        /** IF the user chooses not to convert then set the file name and   **/
        /**     return                                                      **/
        /** ELSE                                                            **/
        /**     convert the file to STREAM_LF format                        **/
        /**                                                                 **/
        /*-------------------------------------------------------------------*/

        if (toupper(c) != 'Y')
            Replace_String(*fname, "Unknown")
        else
        {
	    Malloc_String(temp_fname, String_Size(*fname) + 1)
	    sprintf (temp_fname, "Z%s", *fname);
	    printf ("  The new data file will be called %s\n\n", temp_fname);
            printf ("  Converting the file . . .\n");
            sys_copy_file (*fname, temp_fname);
            fio_convert_file (temp_fname, PDS_RF_BINARY, PDS_RF_STREAM_LF, 0, 0);
            Replace_String(*fname, temp_fname)
            Lemme_Go(temp_fname)

        }  /*  End:  "if (toupper(c) != 'Y') ... else ..."  */

    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if (vms_var) ..."  */

#endif

    return;

/** END **/

}  /*  "tb_convert_data_file"  */




/**********************************************************************
 *$Component                                                          *
 *    void tb_display_column ()                                       *
 *$Abstract                                                           *
 *    Displays a column of data                                       *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *$Inputs                                                             *
 *    None                                                            *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    This routine controls the display of column values for the      *
 *    ASCII version of the Table Browser.  It does some limited       *
 *    validation of the label info, fetches each column value from    *
 *    the data file, formats and displays the values in groups that   *
 *    will fit on the screen of a VT100 terminal.                     *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   February 28, 1993                                         *
 *	  1.2	August 13, 2002											  *
 *$Change_history                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDD   02-28-93   Fixed segmentation fault by adding check for   *
 *                     NULL table, column, bit_column pointers.       *
 *    MDC   08-09-02   Added a warning msg. in the function that      *
 *					   displays an error msg. if a real value was     *
 *					   obtained, but was invalid. Previously, the     *
 *					   program would crash if a large value was		  *
 *					   obtained from a data file.					  *
 *																	  *
 **********************************************************************/

void tb_display_column ()
    
{
    COLUMN_INFO *column_info = {NULL};
    FILE *data_file = {NULL};
    unsigned char *column_value = {NULL};
    unsigned char *unformatted_value = {NULL};
    unsigned char *c = {NULL};
    char save;
    char temp_str [PDS_MAXLINE];
    char temp_str2 [PDS_MAXLINE];
	long i;
    long len;
    long size = {tb_column_info->size}; 
	LOGICAL everything_is_ok = {FALSE};
	char *temp = NULL;
	long string_length = 0;

	/* Added by MDC */
	CONTAINER_INFO *temp_container = NULL;
	
	/* Added by MDC */
	char temp_str3 [PDS_MAXLINE];
	char temp_str4 [PDS_MAXLINE];

/** BEGIN **/

    printf ("\n");

    /*-----------------------------------------------------------------------*/
    /** Check the label information needed to display a column.             **/
    /*-----------------------------------------------------------------------*/

    if (! tb_table_info)
       printf ("   There is no current table\n");
    else
    if (tb_table_info -> fname == NULL)
        printf ("  The name of the data file is invalid or missing\n");
    else
    if ((data_file = fopen (tb_table_info -> fname, "rb")) == NULL)
        printf ("  Unable to open the data file\n");
    else
    if (tb_table_info -> row_bytes == TB_UNKNOWN)
        printf ("  The table's ROW_BYTES field is invalid or missing\n");
    else
    if (! tb_column_info)
       printf ("   There is no current column\n");
    else
    if (tb_column_info -> size == TB_UNKNOWN)
        printf ("  The column's BYTES field is invalid or missing\n");
    else    
    if (tb_column_info -> start == TB_UNKNOWN)
        printf ("  The column's START_BYTES field is invalid or missing\n");
    else
    if (! tb_show_bit_column)
        everything_is_ok = TRUE;
    else
    if (! tb_bit_column_info)
        printf ("  There is no current bit column\n");
    else
    if (tb_bit_column_info -> size == TB_UNKNOWN)
        printf ("  The bit column's BITS field is invalid or missing\n");
    else    
    if (tb_bit_column_info -> start == TB_UNKNOWN)
        printf ("  The bit column's START_BIT field is invalid or missing\n");
    else
        everything_is_ok = TRUE;

    /*-----------------------------------------------------------------------*/
    /** IF anything was missing THEN                                        **/
    /**     Print an error message                                          **/
    /** ELSE                                                                **/
    /*-----------------------------------------------------------------------*/

    if (! everything_is_ok)
    {
        printf ("\n  The ");
        if (tb_show_bit_column) printf ("bit ");
        printf ("column cannot be displayed.\n");
    }
    else
    {
        /*-------------------------------------------------------------------*/
        /** Decide whether to display a BIT_COLUMN or COLUMN                **/
        /*-------------------------------------------------------------------*/

        column_info = (tb_show_bit_column) ? tb_bit_column_info : tb_column_info;
				
        /*-------------------------------------------------------------------*/
        /** Format the display information at the top of the screen         **/
        /*-------------------------------------------------------------------*/

        printf ("------------------------------------------------------------------------------\n");
	sprintf (temp_str, "  %s %s", column_info->name, column_info->class);
	sprintf (temp_str2, "  (%s-%ld)", column_info->data_type, column_info->size);
        strcat (temp_str, temp_str2);
        printf ("%s\n", temp_str); 

        (column_info->bit_columns == NULL) ? 
            printf ("\n") :
            printf ("%79s\n", "(Has Bit Columns)");

		/*-------------------------------------------------------------------*/ 
		/** 10-10-02 Added by MDC											**/
		/** If the column being displayed is within a container, indicate	**/ 
		/** that on the display.											**/
		/**																	**/
		/** 11-26-02 Added by MDC											**/
		/** We now want to display to the user how many repetitions there   **/
		/** are in the container. i.e. "Repetition x of y", where x is the  **/
	    /** current repetition and y is the total number of repetitions.    **/
		/*-------------------------------------------------------------------*/
		if(column_info->is_in_container == 1)
		{
			temp_container = tb_table_info->container;

			/* Go to the appropriate container that the column is under. */
			while( (temp_container != NULL) &&
				   ((temp_container->container_id) != (column_info->which_container)) )
			{
				temp_container = temp_container->next_container;
			}
			
			sprintf(temp_str3, "(%s COLUMN %ld of %ld)", temp_container->name, 
									column_info->col_num, temp_container->columns);
			
			sprintf(temp_str4, "%s %ld of %ld)", "(Repetition number", 
									column_info->rep_num, temp_container->repetitions);

			printf("%79s\n", temp_str3);
			printf("%79s\n", temp_str4);
		}

        /*-------------------------------------------------------------------*/
        /** LOOP through the rows of data to be displayed                   **/
        /*-------------------------------------------------------------------*/

        for (i = tb_row_offset; 
              (((i - tb_row_offset) < tb_display_rows) &&
                 (! ferror(data_file)) && (! feof (data_file))); ++i)
        {
            /*---------------------------------------------------------------*/
            /** Read a column value from the data file                      **/
            /*---------------------------------------------------------------*/

			/*---------------------------------------------------------------*/
			/** 10-17-02 MDC												**/
			/** Changed the last argument to pass in tb_column_info->size   **/
			/** instead of just "size". The other routines that take in size**/
			/** as an argument end up manipulating the contents of this     **/
			/** variable. Thus, the column_value being grabbed will be wrong**/
			/** in the 2nd go around of this loop.							**/
			/*---------------------------------------------------------------*/
/*	    unformatted_value = tb_read_column(tb_column_info,data_file,i,&size);     */ 
		unformatted_value = tb_read_column(tb_column_info,data_file,
													i, &(tb_column_info->size) );

            /*---------------------------------------------------------------*/
            /** Turn the string of bytes that were read from the file into  **/
            /**     a value that can be displayed                           **/
            /*---------------------------------------------------------------*/

            if (! tb_show_bit_column)
            {
                column_value = tb_format_column_value(tb_column_info,
                                            unformatted_value, size);
            }
            else
            {
                column_value = tb_format_bit_column_value(tb_bit_column_info, 
                                            unformatted_value,
                                            &size, tb_column_info->data_type);
            }

            /*---------------------------------------------------------------*/
            /** Prepare and display the value                               **/
			/**																**/
			/** 08-09-02 MDC	Added a check to see if the value obtained  **/
			/**    was a real number. If it is, it will print out an error  **/
			/**	   message notifying the user that an invalid value has     **/
			/**	   been obtained. The program previously crashed if a real	**/
			/**	   value was stored into a variable in which it did not		**/
			/**	   have enough space allocated to it. Thus, this warning msg**/
			/**	   allows the user to be aware that the value in the file   **/
			/**    might not be correct.									**/
            /*---------------------------------------------------------------*/
			
			if( strcmp(column_info->data_type,"IEEE REAL" ) == 0 )
			{
				temp = column_value;
				string_length = 0;
				do
				{
					if( (*temp) == '*' )
						printf("ERROR: Value too large to display on the allocated field.\n");
					else
						temp++;

					string_length ++;
				}while( ((*temp) != '*') && (string_length <= (strlen(column_value))) );

				temp = NULL;
			}
			/* If all is well, print out the value obtained */
			for (c = column_value; (len = strlen(c)) > TB_LINE_LEN; c += TB_LINE_LEN)
			{
				save = *(c + TB_LINE_LEN);
				*(c + TB_LINE_LEN) = EOS;
			(c == column_value) ? printf ("%5ld)  %s\n", i + 1, c)
					    : printf ("        %s\n", c);
				*(c + TB_LINE_LEN) = save;
			}

			if (c == column_value)
				printf ("%5ld)  %s\n", i + 1, c);
				else
			if (len != 0) printf ("        %s\n", c);
			           
		   Lemme_Go(column_value);
             Lemme_Go(unformatted_value);

		   /* 10-17-02 Added by MDC */
		   c = NULL;

        /*-------------------------------------------------------------------*/
        /** ENDLOOP                                                         **/
        /*-------------------------------------------------------------------*/

        }  /*  End:  "for (i = tb_row_offset; ..."  */

        /*-------------------------------------------------------------------*/
        /** Format the display information at the bottom of the screen      **/
        /*-------------------------------------------------------------------*/

        printf ("\n");
        temp_str[0] = EOS;

        if (tb_show_bit_column) 
        {
	    sprintf (temp_str2, "(Bit Column %ld of %ld)  (Item %ld of %ld)  ",
                tb_current_bit_column, tb_column_info -> bit_column_count,
                tb_bit_item_count, column_info -> items);
        }
        else
        {
	    sprintf (temp_str2, "(Column %ld of %ld)  (Item %ld of %ld)  ",
                tb_current_column, tb_table_info -> column_count,
                tb_item_count, column_info -> items);
        }
        strcat (temp_str, temp_str2);

        sprintf (temp_str2, "(%ld rows left)",
                (tb_table_info->row_count - (tb_row_offset + tb_display_rows)));
        strcat (temp_str, temp_str2);

	printf ("%79s", temp_str);

    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if (! everything_is_ok) ... else ..."  */

    Close_Me(data_file);

    return;

/** END **/

}  /*  "tb_display_column"  */                      



/**********************************************************************
 *$Component                                                          *
 *    void tb_display_help ()                                         *
 *$Abstract                                                           *
 *    Displays ASCII Table Browser help.                              *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    None                                                            *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    This routine displays navigation help for the ASCII version of  *
 *    the Table Browser.                                              *
 *$External_References                                                *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.0   January 10, 1992                                          *
 *$Change_history                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDC   12-09-02   Updated help text							  *
 *    MDC   06-15-05   Added software disclaimer message to help      *
 **********************************************************************/

void tb_display_help ()

{
    short c;

/** BEGIN **/

    printf("Disclaimer:\n");
    printf("Copyright 2006-2007, by the California Institute of Technology.\n");
    printf("ALL RIGHTS RESERVED. United States Government Sponsorship acknowledged.\n");
    printf("Any commercial use must be negotiated with the Office of Technology Transfer\n");
    printf("at the California Institute of Technology.\n\n");
    printf("This software is subject to U. S. export control laws and regulations\n");
    printf("(22 C.F.R. 120-130 and 15 C.F.R. 730-774). To the extent that the software\n");
    printf("is subject to U.S. export control laws and regulations, the recipient has\n");
    printf("the responsibility to obtain export licenses or other export authority as\n");
    printf("may be required before exporting such information to foreign countries or\n");
    printf("providing access to foreign nationals.\n\n");
    printf ("\n");
    printf ("  These are the commands that can be entered at the prompt:\n");
    printf ("\n");
    printf ("      ? or H  --  HELP\n");
    printf ("\n");
    printf ("      E or Q  --  EXIT the program\n");
    printf ("\n");
    printf ("      B  --  DISPLAY a bit column\n");
    printf ("      C  --  DISPLAY a column\n");
	printf ("      K  --  DISPLAY a container\n"); /* 12-09-02  MDC */
    printf ("\n");
    printf ("      N  --  Display the NEXT table, column, or bit column of data\n");
    printf ("      P  --  Display the PREVIOUS table, column, or bit column of data\n");
    printf ("\n");
    printf ("      U  --  Move UP through the rows of data in a table\n");
    printf ("      D  --  Move DOWN through the rows of data in a table\n");
    printf ("\n");
    printf ("      F  --  Select a LABEL FILE to verify.\n");
    printf ("      L  --  Display and edit the KEYWORD VALUES used to display the data\n");
    printf ("      S  --  Display a SUMMARY of the data in a column or bit column\n");
                   
    printf ("\n\n  Press RETURN for more, or E to exit help:  ");
    Read_Char(c)
    if (toupper(c) == 'E') return;
    printf ("\n\n");

    printf ("  The Table Browser commands can be grouped into the following categories:\n");
    printf ("\n");
    printf ("\n");
    printf ("  SELECT A LABEL FILE \n");
    printf ("\n");
    printf ("      F  --  Select a label file to verify.\n");
    printf ("\n");
    printf ("      The F command parses a label file and locates any table and\n");
    printf ("      column objects.  You may include a file name after the F, or\n");
    printf ("      be prompted for one.  For example, entering F test.lbl, causes\n");
    printf ("      the file test.lbl to be parsed.\n");
    printf ("\n");
    printf ("      You must use this command first in order to view the data.\n");

    printf ("\n\n  Press RETURN for more, or E to exit help:  ");
    Read_Char(c)
    if (toupper(c) == 'E') return;
    printf ("\n\n");

    printf ("  DATA DISPLAY COMMANDS \n");
    printf ("\n");
    printf ("      B  --  Display a bit column\n");
    printf ("      C  --  Display a column\n");
    printf ("\n");
    printf ("      These commands control the display of the data.  \n");
    printf ("\n");
    printf ("      The C command displays a column, while the B command displays a bit \n");
    printf ("      column within a column.  Entering a number before or after either of \n");
    printf ("      these commands displays the data in that column or bit column.  For \n");
    printf ("      example, entering 5C displays the data in column five.  Entering 3B \n");
    printf ("      would display the data in the third bit column inside the current \n");
    printf ("      column, not the third bit column in the label.  Entering a name after \n");
    printf ("      either of these commands displays the data in that column or bit column. \n");
    printf ("      For example, entering C file_nmuber displays the data in FILE_NUMBER \n");
    printf ("      column.  Entering b sect_day would display the data in the SECT_DAY \n");
    printf ("      bit column inside the current column \n");

    printf ("\n\n  Press RETURN for more, or E to exit help:  ");
    Read_Char(c)
    if (toupper(c) == 'E') return;
    printf ("\n\n");
	
    printf ("      K  --  Display a container\n");
	printf ("\n");
	printf ("      The K command displays the first column in a container.  By default, \n");
	printf ("      entering K will bring you to the first container. Entering a number \n");
	printf ("      before or after the command displays that particular container. For \n");
	printf ("      example, entering 4K or K4 displays the first column in container four. \n");
	printf ("      The container name is displayed at the top right corner of the screen \n");
	printf ("      where it says \"A COLUMN x of y\" where A = the CONTAINER name, x = what \n");
	printf ("      column is being displayed, and y = the number of columns in that \n");
	printf ("      container. The current repetition is also shown when displaying \n");
	printf ("      container columns \n");

	printf ("\n\n  Press RETURN for more, or E to exit help:  ");
    Read_Char(c)
    if (toupper(c) == 'E') return;
    printf ("\n\n");

    printf ("  NAVIGATION COMMANDS \n");
    printf ("\n");
    printf ("      U  --  Move up through the rows of data in a table\n");
    printf ("      D  --  Move down through the rows of data in a table\n");
    printf ("      N  --  Display the next column of data\n");
    printf ("      P  --  Display the previous column of data\n");
    printf ("\n");
    printf ("      The U, D, N, and P commands allow you to move through the rows and \n");
    printf ("      columns of data in a table.  They behave like arrow keys.  Here is \n");
    printf ("      how they relate to each other:\n");
    printf ("\n");
    printf ("                            U\n");
    printf ("                            ^\n");
    printf ("                        P <   > N\n");           
    printf ("                            v\n");
    printf ("                            D \n");
    printf ("\n");
    printf ("      (Navigation continued . . .)");

    printf ("\n\n  Press RETURN for more, or E to exit help:  ");
    Read_Char(c)
    if (toupper(c) == 'E') return;
    printf ("\n\n");

    printf ("      The commands U and D move up and down through the rows.  Entering a \n"); 
    printf ("      number before or after the U or D commands causes the data to scroll\n");
    printf ("      up or down by that many rows.  If you want to jump directly to a \n");
    printf ("      particular row in the table, just enter the row number and press return.\n");
    printf ("\n");
    printf ("      The commands P and N move left and right through the tables, columns, \n");
    printf ("      or bit columns depending on what is currently displayed.  If you are \n");
    printf ("      looking at table information and type N, you will see the next table. \n");
    printf ("      If you are looking at a column and type N, you will see the next \n");
    printf ("      column.  And finally, if you are looking at a bit column and type\n");
    printf ("      N, you will see the next bit column.  The P command displays the \n");
    printf ("      previous table, column, or bit column in the same way.\n");
    printf ("\n");
    printf ("      (Navigation continued . . .)");

    printf ("\n\n  Press RETURN for more, or E to exit help:  ");
    Read_Char(c)
    if (toupper(c) == 'E') return;
    printf ("\n\n");

    printf ("      You may switch between table, column, and bit column display by \n");
    printf ("      typing NT to move to the next table, NC to move to the next column,\n");
    printf ("      or NB to move to the next bit column.  PT, PC, and PB move back through\n");
    printf ("      tables, columns, or bit columns in the same way.  \n");
    printf ("\n");
    printf ("      The T, C, and B options will skip over any items in the columns or bit \n");
    printf ("      columns.  If you want to see each item, type NI to set the item display.  \n");
    printf ("\n");
    printf ("      Entering a number before or after the P or N commands moves \n");
    printf ("      that many tables, columns, or bit columns to the left or right.\n");
    printf ("\n");
	printf ("      Typing NK moves to the next container. Similarly, PK moves back through \n");
	printf ("      containers.\n");
	printf ("\n");

    printf ("\n\n  Press RETURN for more, or E to exit help:  ");
    Read_Char(c)
    if (toupper(c) == 'E') return;
    printf ("\n\n");
	
    printf ("  SHOW AND EDIT LABEL INFORMATION \n");
    printf ("\n");
    printf ("      L  --  Shows the label information used to display the data\n");
    printf ("\n");
    printf ("      The L command can show the table, column, or bit column label\n");
    printf ("      information used to display the data, plus other information\n");
    printf ("      which is used to control the appearance of the data.\n");
    printf ("\n");
    printf ("      Entering LF shows the keywords that relate to the entire file,\n");
    printf ("      LT shows the keywords extracted from the TABLE object,\n");
    printf ("      LC shows the column object keywords, and LB shows the bit column\n");
    printf ("      keywords.  You may modify any of the fields by typing the number\n");
    printf ("      to the left of the field.  You will either be prompted for input\n");
    printf ("      or be presented with a list of standard values to select from.\n");
    printf ("\n");
    printf ("      (Label information continued . . .)");

    printf ("\n\n  Press RETURN for more, or E to exit help:  ");
    Read_Char(c)
    if (toupper(c) == 'E') return;
    printf ("\n\n");

    printf ("      In addition to the keywords extracted from the label, there are\n");
    printf ("      two values displayed with the column and bit column information\n");
    printf ("      that control the display of the data.  These are \"Displayed\" and\n");
    printf ("      \"Display Format\".  The first allows you to decide which columns\n");
    printf ("      you want to see at any particular time.  The second determines \n");
    printf ("      how the data is to be interpreted for display.  The bytes in the\n");
    printf ("      data can be interpreted and displayed in BINARY, HEX, or OCTAL \n");
    printf ("      format, or in the format of the particular data type specified in \n");
    printf ("      the label (TEXT format).\n");

    printf ("\n\n  Press RETURN for more, or E to exit help:  ");
    Read_Char(c)
    if (toupper(c) == 'E') return;
    printf ("\n\n");

    printf ("  SUMMARIZE DATA\n");
    printf ("\n");
    printf ("      S -- Summarize the data in a column, column item, or bit column\n");
    printf ("\n");
    printf ("      The S command can summarize the data values in a column, item,\n");
    printf ("      or bit column by displaying minima, maxima, averages, or value\n");
    printf ("      counts of the data values in the column or bit column.\n");
    printf ("\n");
    printf ("      If you are looking at a column (or column item) and you type S,\n");
    printf ("      you will receive a summary of the data in the column.  If you \n");
    printf ("      are looking at a bit column and you type S, you will receive a\n");
    printf ("      summary of the data in the current bit column.\n");
    printf ("\n");
    printf ("      (Summarize information continued . . .)");

    printf ("\n\n  Press RETURN for more, or E to exit help:  ");
    Read_Char(c)
    if (toupper(c) == 'E') return;
    printf ("\n\n");

    printf ("      The type of summary you will receives depends on the data type\n");
    printf ("      of the column (see the L (Label) command).  Numeric data types\n");
    printf ("      (INTEGER, UNSIGNED INTEGER, and REAL) will result in a summary\n");
    printf ("      that includes minimum, maximum, and average values.  CHARACTER\n");
    printf ("      types will result in a summary that includes only value \n");
    printf ("      counts: a table of data values and the number of times each\n");
    printf ("      occurred in the column.  DATE and TIME data types will result\n");
    printf ("      in a summary that includes minimum and maximum values, plus\n");
    printf ("      an occurrence count table for values that did not appear to\n");
    printf ("      be dates or times. BOOLEAN data types will result in occurence\n");
    printf ("      counts. \n");
    printf ("\n");
    printf ("      (Summarize information continued . . .)");

    printf ("\n\n  Press RETURN for more, or E to exit help:  ");
    Read_Char(c)
    if (toupper(c) == 'E') return;
    printf ("\n\n");

    printf ("      Summaries only make sense if the column display format is TEXT\n");
    printf ("      (see L command).  Occurrence tables which exceed 500 entries will\n");
    printf ("      be truncated.  Columns which are too wide (greater than 60 bytes\n");
    printf ("      for CHARACTER columns, greater than 4 bytes for INTEGERs, or\n");
    printf ("      greater than 8 bytes for REALs) cannot be summarized.\n");

    printf ("\n\n  Press RETURN for more, or E to exit help:  ");
    Read_Char(c)
    printf ("\n\n");

    return;

/** END **/

}  /*  "tb_display_help"  */



/**********************************************************************
 *$Component                                                          *
 *    void tb_display_label ()                                        *
 *$Abstract                                                           *
 *    Displays label info                                             *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    None                                                            *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    This routine displays the label info used to fetch the data     *
 *    for display, along with some additional information used        *
 *    locally by the program. It references the tb_show_file, table,  *
 *    column, and bit_column variables in order to determine what     *
 *    part of the label the user wants to see.                        *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Error_Handling                                                     *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   March 8, 1993                                             *
 *    1.2   October 10, 2002										  *
 *$Change_history                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDD   03-08-93   Updated header/comments                        *
 *    MDC   10-10-02  1)Updated the routine to be able to show how    *
 *					    many containers are within a table for a table*
 *					    display.									  *
 *					  2)Updated the routine to be able to indicate    *
 *						in a column display whether it is in a        *
 *						container or not.							  *
 **********************************************************************/

void tb_display_label ()

{                                     
    TABLE_INFO *table_info = {tb_table_info};
    COLUMN_INFO *column_info = {NULL};
    char slinky [20];

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Decide whether to display BIT_COLUMN or COLUMN info                 **/
    /*-----------------------------------------------------------------------*/

    column_info = (tb_show_bit_column) ? tb_bit_column_info : tb_column_info;

    /*-----------------------------------------------------------------------*/
    /** Display label file info if that option was set                      **/
    /*-----------------------------------------------------------------------*/

    if (tb_show_file)
    {
	printf ("\n\n  Keywords for the data file:\n\n");

	(tb_record_type == NULL) ?
	    printf ("       1)  Record type    = Unknown\n") :
	    printf ("       1)  Record type    = %s\n", tb_record_type);

	(tb_record_bytes == TB_UNKNOWN) ?
	    printf ("       2)  Record bytes   = Unknown\n") :
	    printf ("       2)  Record bytes   = %d\n", tb_record_bytes);

	(tb_file_records == TB_UNKNOWN) ?
	    printf ("       3)  File records   = Unknown\n") :
	    printf ("       3)  File records   = %d\n", tb_file_records);

	printf ("\n");

	(tb_blocking_type == NULL) ?
	    printf ("       4)  Blocking type  = Unknown\n") :
	    printf ("       4)  Blocking type  = %s\n", tb_blocking_type);

	(tb_block_bytes == TB_UNKNOWN) ?
	    printf ("       5)  Block bytes    = Unknown\n") :
	    printf ("       5)  Block bytes    = %d\n", tb_block_bytes);

	(tb_block_records == TB_UNKNOWN) ?
	    printf ("       6)  Block records  = Unknown\n") :
	    printf ("       6)  Block records  = %d\n", tb_block_records);

    }  /*  End:  "if (tb_show_file) ..."  */

    /*-----------------------------------------------------------------------*/
    /** Display table info if that option was set                           **/
    /*-----------------------------------------------------------------------*/

    if ((table_info != NULL) && tb_show_table)
    {
	printf ("\n\n  Keywords for table %ld of %ld:\n\n",
		tb_table_number, tb_table_count);

	(table_info -> name == NULL) ?
	    printf ("       1)  Table name         = Unknown %s\n", table_info->class) :
	    printf ("       1)  Table name         = %s %s\n", table_info->name, table_info->class);

	(table_info -> row_count == TB_UNKNOWN) ?
	    printf ("       2)  Rows               = Unknown\n") :
	    printf ("       2)  Rows               = %ld\n", table_info->row_count);

	(table_info -> column_count == TB_UNKNOWN) ?
	    printf ("       3)  Columns            = Unknown\n") :
	    printf ("       3)  Columns            = %ld\n", table_info->column_count);

	(table_info -> row_bytes == TB_UNKNOWN) ?
	    printf ("       4)  Row bytes          = Unknown\n") :
	    printf ("       4)  Row bytes          = %ld\n", table_info->row_bytes);

	(table_info -> prefix_bytes == TB_UNKNOWN) ?
	    printf ("       5)  Row prefix bytes   = Unknown\n") :
	    printf ("       5)  Row prefix bytes   = %ld\n", table_info->prefix_bytes);

	(table_info -> suffix_bytes == TB_UNKNOWN) ?
	    printf ("       6)  Row suffix bytes   = Unknown\n") :
	    printf ("       6)  Row suffix bytes   = %ld\n", table_info->suffix_bytes);

	(table_info -> interchange_format == NULL) ?
	    printf ("       7)  Interchange format = Unknown\n") :
	    printf ("       7)  Interchange format = %s\n", table_info->interchange_format);

	(table_info -> fname == NULL) ?
	    printf ("       8)  Data file          = Unknown\n") :
	    printf ("       8)  Data file          = %s\n", table_info->fname);

	(table_info -> data_location == TB_UNKNOWN) ?
	    printf ("       9)  Data byte location = Unknown\n") :
	    printf ("       9)  Data byte location = %ld\n", table_info->data_location);
	
	/* 10-10-02 Added by MDC */
	/* Display the number of containers within the table */
		printf ("      10)  Containers         = %ld\n", table_info->container_count);

    }  /*  End:  "if (table_info != NULL) ..."  */


    /*-----------------------------------------------------------------------*/
    /** Display column info if that option was set                          **/
    /*-----------------------------------------------------------------------*/

    if ((column_info != NULL) && (tb_show_column || tb_show_bit_column))
    {
	printf ("\n\n");

	 printf ("   Keywords for ");

	 (tb_show_bit_column) ?
	     printf ("Bit Column %ld of %ld:\n\n", tb_current_bit_column, tb_column_info->bit_column_count) :
	     printf ("Column %ld of %ld:\n\n", tb_current_column, table_info->column_count);

	(column_info -> name == NULL) ?
	    printf ("       1)  Name             = Unknown %s\n",column_info->class) :
	    printf ("       1)  Name             = %s %s\n", column_info->name,column_info->class);

	(column_info -> display) ?
	    printf ("       2)  Displayed        = Yes\n") :
	    printf ("       2)  Displayed        = No\n");

	(column_info -> display_format == NULL) ?
	    printf ("       3)  Display Format   = Unknown\n") :
	    printf ("       3)  Display Format   = %s\n", column_info->display_format);

	(column_info -> data_type == NULL) ?
	    printf ("       4)  Data type        = Unknown\n") :
	    printf ("       4)  Data type        = %s\n", column_info->data_type);

	(tb_show_bit_column) ? strcpy(slinky, "Bits ") :
		       strcpy(slinky, "Bytes");

	(column_info -> size == TB_UNKNOWN) ?
	    printf ("       5)  %s            = Unknown\n", slinky) :
	    printf ("       5)  %s            = %ld\n", slinky, column_info->size);

	(tb_show_bit_column) ? strcpy(slinky, "Start bit ") :
			       strcpy(slinky, "Start byte");

	(column_info -> start == TB_UNKNOWN) ?
	    printf ("       6)  %s       = Unknown\n", slinky) :
	    printf ("       6)  %s       = %ld\n", slinky, column_info->start);

	(column_info -> items == TB_UNKNOWN) ?
	    printf ("       7)  Items            = Unknown\n") :
	    printf ("       7)  Items            = %ld\n", column_info->items);

	(column_info -> item_offset == TB_UNKNOWN) ?
	    printf ("       8)  Item Offset      = Unknown\n") :
	    printf ("       8)  Item Offset      = %ld\n", column_info->item_offset);

	(column_info -> bit_columns == NULL) ?
	    printf ("       9)  Has Bit Columns  = No\n") :
	    printf ("       9)  Has Bit Columns  = Yes\n");

	/*------------------------------------------------------------*/
	/** 10-10-02  Added by MDC									 **/
	/**															 **/
	/** Indicate to the user whether the column is inside a      **/
	/** container or not.										 **/
	/*------------------------------------------------------------*/
	(column_info -> is_in_container == 0) ?
		printf ("      10)  Is In Container  = No\n") :
	    printf ("      10)  Is in Container  = Yes\n");

    }  /*  End:  "if (column_info != NULL) ..."  */

    return;

/** END **/

}  /*  "tb_display_label"  */



/**********************************************************************
 *$Component                                                          *
 *    void tb_edit_label_info (selection)                             *
 *$Abstract                                                           *
 *    Edits label info                                                *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    selection:                                                      *
 *        The selection variable is an integer representing the       *
 *        number of the particular selection the user has chosen      *
 *        from a list of selections.                                  *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    This routine allows the user to change the infomation extracted *
 *    from a label which is used to fetch and display the data in the *
 *    ASCII version of the Table Browser.  The selection number       *
 *    given as input is the item number of the label item the user    *
 *    wishes to change, as listed by tb_display_label_info.  This     *
 *    routine determines whether to edit file, table, column, or      *
 *    bit_column information by examining the tb_show... variables.   *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Error_Handling                                                     *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    In replacing values in the file, table, and column info         *
 *    structures, existing memory may be free'd and realloc'd.        *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   March 8, 1993                                             *
 *    1.2	October 10, 2002									      *
 *$Change_history                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDD   03-08-93   Updated header/comments                        *
 *	  MDC   10-10-02  1)Added the ability of the user to edit the	  *
 *						container_count info. This information was    *
 *						just added to tbtool.						  *
 **********************************************************************/

void tb_edit_label_info (selection)

long selection;

{
    TABLE_INFO *table_info = {tb_table_info};
    COLUMN_INFO *column_info = {NULL};
    char *str = {NULL};

/** BEGIN **/

    Malloc_String(str, 81)

    /*-----------------------------------------------------------------------*/
    /** Decide whether to edit BIT_COLUMN or COLUMN info                    **/
    /*-----------------------------------------------------------------------*/

    column_info = (tb_show_bit_column) ? tb_bit_column_info : tb_column_info;

    printf ("\n");

    /*-----------------------------------------------------------------------*/
    /** Edit label file info if that option was set                         **/
    /*-----------------------------------------------------------------------*/

    if (tb_show_file)
    {
        switch (selection)
        {
            /*--------------------------------------------------------------*/
            /** User is updating record type. Present the list and replace **/
            /** the value of record type                                   **/
            /*--------------------------------------------------------------*/

            case  1 : tb_replace_from_list (&tb_record_type, 
                              tb_record_type_list, tb_record_type_end,
                           "  Record Type (Must be one of the following)");
                      break;

            /*--------------------------------------------------------------*/
            /** User is updating record bytes. Read a new value.           **/
            /*--------------------------------------------------------------*/

            case  2 : printf ("  Record bytes : ");
                      gets(str);
                      if (*str != EOS) tb_record_bytes = Make_Long(str);
                      break;

            /*--------------------------------------------------------------*/
            /** User is updating file records. Read a new value.           **/
            /*--------------------------------------------------------------*/

            case  3 : printf ("  File records : ");
                      gets(str);
                      if (*str != EOS) tb_file_records = Make_Long(str);
                      break;

            /*--------------------------------------------------------------*/
            /** User is updating blocking type. Display a list and replace **/
            /** the value of blocking type.                                **/
            /*--------------------------------------------------------------*/

            case  4 : tb_replace_from_list (&tb_blocking_type,
                              tb_blocking_type_list, tb_blocking_type_end,
                           "  Blocking Type (Must be one of the following)");
                      break;

            /*--------------------------------------------------------------*/
            /** User is updating block bytes.  Read a new value.           **/
            /*--------------------------------------------------------------*/

            case  5 : printf ("  Block bytes : ");
                      gets(str);
                      if (*str != EOS) tb_block_bytes = Make_Long(str);
                      break;

            /*--------------------------------------------------------------*/
            /** User is updating block records. Read a new value.          **/
            /*--------------------------------------------------------------*/

            case  6 : printf ("  Block records : ");
                      gets(str);
                      if (*str != EOS) tb_block_records = Make_Long(str);
                      break;

        }  /*  End:  "switch (selection) ..."  */

    }  /*  End:  "if (tb_show_file) ..."  */

    /*-----------------------------------------------------------------------*/
    /** Edit table info if that option is set                               **/
    /*-----------------------------------------------------------------------*/

    if (tb_show_table)
    {
        switch (selection)
        {
            /*--------------------------------------------------------------*/
            /** User is updating table name.  Read a new value.            **/
            /*--------------------------------------------------------------*/

            case  1 : printf ("\n  Table name  : ");
                      gets(str);
                      if (*str != EOS) Replace_String(table_info->name, str)
                      break;
    
            /*--------------------------------------------------------------*/
            /** User is updating table rows.  Read a new value.            **/
            /** Reset number of rows to display on a screen if new total   **/
            /** number of rows in table is smaller than the screen size.   **/
            /*--------------------------------------------------------------*/

            case  2 : printf ("  Rows        : ");
                      gets(str);                                  
                      if (*str != EOS) table_info -> row_count = Make_Long(str);
                      tb_display_rows = 
                            (table_info->row_count <= tb_display_rows) ?
                                  table_info->row_count : TB_DISPLAY_ROWS;
                      break;
    
            /*--------------------------------------------------------------*/
            /** User is updating number of columns. Read a new value.      **/
            /*--------------------------------------------------------------*/

            case  3 : printf ("  Columns     : ");
                      gets(str);
                      if (*str != EOS) table_info -> column_count = Make_Long(str);
                      break;
                             
            /*--------------------------------------------------------------*/
            /** User is updating row bytes.  Read a new value.             **/
            /*--------------------------------------------------------------*/

            case  4 : printf ("  Row bytes   : ");
                      gets(str);
                      if (*str != EOS) table_info -> row_bytes = Make_Long(str);
                      break;
    
            /*--------------------------------------------------------------*/
            /** User is updating prefix bytes.  Read a new value.          **/
            /*--------------------------------------------------------------*/

            case  5 : printf ("  Row prefix bytes   : ");
                      gets(str);
                      if (*str != EOS) table_info -> prefix_bytes = Make_Long(str);
                      break;
    
            /*--------------------------------------------------------------*/
            /** User is updating suffix bytes.  Read a new value.          **/
            /*--------------------------------------------------------------*/

            case  6 : printf ("  Row suffix bytes   : ");
                      gets(str);
                      if (*str != EOS) table_info -> suffix_bytes = Make_Long(str);
                      break;
    
            /*--------------------------------------------------------------*/
            /** User is updating interchange format. Display a list and    **/  
            /** replace the value of interchange format for the current    **/
            /** table                                                      **/
            /*--------------------------------------------------------------*/

            case  7 : tb_replace_from_list (&(table_info->interchange_format),
                                            tb_format_list, tb_format_end,
                         "  Interchange format (Must be one of the following)");
                      break;
    
            /*--------------------------------------------------------------*/
            /** User is updating the data file name. Read a new value.     **/
            /** Check and convert the file to stream if this is VMS.       **/
            /** Replace the data file name with the new file name.         **/
            /*--------------------------------------------------------------*/

            case  8 : printf ("  Data file   : ");
                      gets(str);
                      if (*str != EOS) 
                      {
#ifdef VAX  
                          tb_convert_data_file (&str);
#endif             
                          Replace_String(table_info->fname,str)
                      }
                      break;
    
            /*--------------------------------------------------------------*/
            /** User is updating byte offset of data object. Read a new    **/
            /** value for the current table.                               **/
            /*--------------------------------------------------------------*/

            case  9 : printf ("  Data byte location : ");
                      gets(str);
                      if (*str != EOS) table_info->data_location = Make_Long(str);
                      break;

		    /*--------------------------------------------------------------*/
		    /** 10-10-02 Added by MDC									   **/
		    /** User is updating the number of containers in the label.    **/
		    /** Read the new value.										   **/
		    /*--------------------------------------------------------------*/
		    
			case 10: printf ("	Containers : ");
				     gets(str);
					 if (*str != EOS) table_info->container_count = Make_Long(str);
					 break;
        
        }  /*  End:  "switch (selection) ..."  */

    }  /*  End:  "if (tb_show_table) ..."  */

    /*-----------------------------------------------------------------------*/
    /** Edit column info if that option is set                              **/
    /*-----------------------------------------------------------------------*/

    if (tb_show_column || tb_show_bit_column)
    {
        switch (selection)
        {
            /*--------------------------------------------------------------*/
            /** User is updating column name. Read a new value.            **/
            /*--------------------------------------------------------------*/

            case  1 : printf ("  Name : ");
                      gets(str);
                      if (*str != EOS) Replace_String(column_info->name,str)
                      break;
    
            /*--------------------------------------------------------------*/
            /** User is updating column display flag. Read a new value.    **/
            /*--------------------------------------------------------------*/

            case  2 : printf ("  Displayed   : ");
                      gets(str);
                      if (*str != EOS) column_info->display = (toupper(*str) == 'Y');
                      break;
    
            /*--------------------------------------------------------------*/
            /** User is updating display format. Display a list and        **/
            /** replace the value of display format for the current column.**/
            /*--------------------------------------------------------------*/

            case  3 : tb_replace_from_list (&(column_info->display_format),
                                            tb_display_list, tb_display_end,
                           "  Display format (Must be one of the following)");
                      break;
                       
            /*--------------------------------------------------------------*/
            /** User is updating data type. Based on whether the display   **/
            /** mode is column mode or bit_column mode, display a list     **/
            /** of data types and replace the value of data type for the   **/
            /** current column.                                            **/
            /*--------------------------------------------------------------*/

            case  4 : if (tb_show_bit_column)
                      {
                          tb_replace_from_list (&(column_info->data_type),
                                              tb_bit_type_list, tb_bit_type_end,
                               "  Data type  (Must be one of the following)");
                      }
                      else
                      {
                          tb_replace_from_list (&(column_info->data_type),
                                                tb_type_list, tb_type_end,
                               "  Data type  (Must be one of the following)");
                      }
                      break;

            /*--------------------------------------------------------------*/
            /** User is updating either bits or bytes. Read a new value for**/
            /** the current column or bit column.                          **/
            /*--------------------------------------------------------------*/

            case  5 : (tb_show_bit_column) ? printf ("  Bits        : ") :
                                             printf ("  Bytes       : ");
                      gets(str);
                      if (*str != EOS) column_info -> size = Make_Long(str);
                      break;
    
            /*--------------------------------------------------------------*/
            /** User is updating start bit or byte.   Read a new value for **/
            /** the current column or bit column.                          **/
            /*--------------------------------------------------------------*/

            case  6 : (tb_show_bit_column) ? printf ("  Start bit   : ") :
                                             printf ("  Start byte  : ");
                      gets(str);
                      if (*str != EOS) column_info->start = Make_Long(str);
                      break;
    
            /*--------------------------------------------------------------*/
            /** User is updating number of items. Read a new value.        **/
            /*--------------------------------------------------------------*/

            case  7 : printf ("  Items : ");
                      gets(str);
                      if (*str != EOS) column_info->items = Make_Long(str);
                      break;
    
            /*--------------------------------------------------------------*/
            /** User is updating item offset. Read a new value.            **/
            /*--------------------------------------------------------------*/

            case  8 : printf ("  Item Offset : ");
                      gets(str);
                      if (*str != EOS) column_info->item_offset = Make_Long(str);
                      break;
    
        }  /*  End:  "switch (selection) ..."  */

    }  /*  End:  "if (tb_show_column) ..."  */
                                             
    Lemme_Go(str)

    return;                                                         

/** END **/

}  /*  "tb_edit_label_info"  */


                    
/**********************************************************************
 *$Component                                                          *
 *    unsigned char *tb_format_bit_column_value (column_info, buffer, *
 *                                     size, column_data_type)        *
 *$Abstract                                                           *
 *    Format bit column values for display                            *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    column_info:                                                    *
 *        The column_info variable is a pointer to the structure      *
 *        which contains keyword information on a column object.      *
 *    buffer:                                                         *
 *        The buffer variable is a character string containing a      *
 *        column value.                                               *
 *    size:                                                           *
 *        The size variable is the size, in bytes, of a column value. *
 *    column_data_type:                                               *
 *        The column_data_type variable is a character string that    *
 *        contains the data type of a column in a table.              *
 *$Outputs                                                            *
 *    size:                                                           *
 *        The size variable is the size, in bytes, of a column value. *
 *$Returns                                                            *
 *    formatted_buffer:                                               *
 *        The formatted_buffer variable is a character string that    *
 *        contains a column value that has been formatted for display.*
 *$Detailed_Description                                               *
 *    This routine takes a bit column value and turns it into         *
 *    something that can be displayed by extracting the needed        *
 *    bits from the data buffer, right justifying them, formatting    *
 *    the value based on display format, data type, and interchange   *
 *    format, and returning the updated data value. The size of the   *
 *    data buffer is also updated. The column_data_type input is      *
 *    used to determine whether the entire data buffer must be        *
 *    swapped or reversed before bit column data is extracted.        *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Error_Handling                                                     *
 *    If the input value is in error or cannot be formatted, the      *
 *    result will be padded with asterisks up to the needed size.     *
 *$Side_Effects                                                       *
 *    Memory is allocated for the return value and must be deallocated*
 *    elsewhere.                                                      *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   March 8, 1993                                             *
 *$Change_history                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDD   03-08-93   Updated header/comments                        *
 **********************************************************************/

unsigned char *tb_format_bit_column_value (column_info, buffer, 
                                           size, column_data_type)

COLUMN_INFO *column_info;
unsigned char *buffer;
long *size;                              
char *column_data_type;

{       
    unsigned char *new_buffer = {NULL};
    unsigned char *value = {NULL};
	/* 10-17-02 MDC	Initialized */
    unsigned char c = 0;
    long i = 0;
    long bit_data_type;
    long display_format;
    long interchange_format;
    int swap_bytes = {FALSE};

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Determine the interchange format, display format and bit data type  **/
    /** of the bit column.                                                  **/
    /*-----------------------------------------------------------------------*/

    interchange_format = search_string_array (
                                     tb_format_list, 
                                     tb_format_end, 
                                     tb_table_info->interchange_format);
        
    display_format = search_string_array (tb_display_list, 
                                          tb_display_end, 
                                          column_info->display_format);
     
    bit_data_type = search_string_array (tb_bit_type_list, tb_bit_type_end, 
                                         column_info -> data_type);

    /*-----------------------------------------------------------------------*/
    /** If the column containing this bit column has an LSB_BIT_STRING      **/
    /** data type, then reverse the bytes in the data buffer                **/
    /*-----------------------------------------------------------------------*/

    if (search_string_array(tb_type_list, 
                tb_type_end, column_data_type) == TB_LSB_BIT_STRING)
    {
        c = *buffer;
        *buffer = *(buffer + 3);
        *(buffer + 3) = c;
        c = *(buffer + 1);
        *(buffer + 1) = *(buffer + 2);
        *(buffer + 2) = c;
    }

    /*-----------------------------------------------------------------------*/
    /** Extract the needed bits from the buffer and right justify them      **/
    /*-----------------------------------------------------------------------*/

    new_buffer = tb_justify_bit_column_value (column_info, buffer, size, 
                                            ((bit_data_type == TB_INTEGER) || 
                                           (bit_data_type == TB_MSB_INTEGER)));

    /*-----------------------------------------------------------------------*/
    /** Call the appropriate routine to format the data based on the        **/
    /**     interchange format, display format, and data type.              **/
    /*-----------------------------------------------------------------------*/

    if ((interchange_format == TB_FORMAT_ASCII) || 
            (display_format != TB_DISPLAY_TEXT))
    {
        switch (display_format)
        {                                                   
            case TB_DISPLAY_HEX    : value = ft_hex (new_buffer, *size);
                                     break;

            case TB_DISPLAY_OCTAL  : value = ft_octal (new_buffer, *size);
                                     break;

            case TB_DISPLAY_BINARY : value = ft_msb_binary (new_buffer, *size);
                                     break;

            case TB_DISPLAY_TEXT   : value = ft_char (new_buffer, *size);
                                     break;

        }  /*  End:  "switch (display_format) ..."  */

    }
    else                
    {
        switch (bit_data_type)
        {
            case TB_BOOLEAN_BIT               : value = ft_msb_integer 
                                                                  (new_buffer, 
                                                                   *size, FALSE);
                                                break;

            case TB_INTEGER_BITS              :
            case TB_MSB_INTEGER_BITS          : value = ft_msb_integer 
                                                                  (new_buffer, 
                                                                   *size, TRUE);
                                                break;

            case TB_UNSIGNED_INTEGER_BITS     :
            case TB_MSB_UNSIGNED_INTEGER_BITS : value = ft_msb_integer 
                                                                  (new_buffer, 
                                                                  *size, FALSE);
                                                break;

        }  /*  End:  "switch (bit_data_type) ..."  */

    }  /*  End:  "if ((interchange_format == ASCII) || ... else ..."  */

    /*-----------------------------------------------------------------------*/
    /** Strip trailing blanks if a value was formatted, or pad the output   **/
    /**     buffer with stars otherwise.                                    **/
    /*-----------------------------------------------------------------------*/

    if (value != NULL)
        U_Strip_Trailing(value, ' ')
    else
    {
	U_Malloc_String(value, (int) *size+1);
	for (i=0; i < *size; ++i) *(value + i) = '*';
	*(value + *size) = EOS;
    }

    /*-----------------------------------------------------------------------*/
    /** Free local storage and return the formatted value                   **/
    /*-----------------------------------------------------------------------*/

    Lemme_Go(new_buffer)

    return(value);

/** END **/

}  /*  "tb_format_bit_column_value"  */



/**********************************************************************
 *$Component                                                          *
 *    unsigned char *tb_format_column_value (column_info, buffer,     *
 *                                           size)                    *
 *$Abstract                                                           *
 *    Formats a string of bytes for display                           *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    column_info:                                                    *
 *        The column_info variable is a pointer to the structure      *
 *        which contains keyword information on a column object.      *
 *    buffer:                                                         *
 *        The buffer variable is a character string containing a      *
 *        column value.                                               *
 *    size:                                                           *
 *        The size variable is the size, in bytes, of a column value. *
 *$Outputs                                                            *
 *    None.                                                           *
 *$Returns                                                            *
 *    formatted_buffer:                                               *
 *        The formatted_buffer variable is a character string that    *
 *        contains a column value that has been formatted for display.*
 *$Detailed_Description                                               *
 *    This routine converts the string of bytes in a column value     *
 *    into a character string that can be displayed by formatting     *
 *    the value based on interchange format, data type, and display   *
 *    format.                                                         *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Error_Handling                                                     *
 *    If the input value is in error or cannot be formatted, the      *
 *    result will be padded with asterisks up to the needed size.     *
 *    If the value is a VAX value greater than 8-bytes, it will be    *
 *    padded with asterisks when compiled on the PC.                  * 
 *$Side_Effects                                                       *
 *    Memory is allocated for the return value and must be deallocated*
 *    elsewhere.                                                      *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   March 8, 1993                                             *
 *$Change_history                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDD   03-08-93   Updated header/comments                        *
 **********************************************************************/

unsigned char *tb_format_column_value (column_info, buffer, size)

COLUMN_INFO *column_info;
unsigned char *buffer;
long size;

{       
    unsigned char *value = {NULL};
    long i;
    long data_type;
    long display_format;
    long interchange_format;
    LOGICAL too_big = FALSE;

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Determine the interchange format, display format and data type      **/
    /*-----------------------------------------------------------------------*/

    interchange_format = search_string_array (
                                     tb_format_list, 
                                     tb_format_end, 
                                     tb_table_info->interchange_format);

    display_format = search_string_array (tb_display_list, 
                                          tb_display_end, 
                                          column_info->display_format);

    data_type = search_string_array (tb_type_list, tb_type_end, 
                                     column_info -> data_type);

    /*-----------------------------------------------------------------------*/
    /** Call the appropriate routine to format the data based on the        **/
    /**     interchange format, display format, and data type.              **/
    /*-----------------------------------------------------------------------*/

    if ((interchange_format == TB_FORMAT_ASCII) || 
            (display_format != TB_DISPLAY_TEXT))
    {
        switch (display_format)
        {
            case TB_DISPLAY_HEX    : value = ft_hex (buffer, size);
                                     break;

            case TB_DISPLAY_OCTAL  : value = ft_octal (buffer, size);
                                     break;

            case TB_DISPLAY_BINARY : value = ft_msb_binary (buffer, size);
                                     break;

            case TB_DISPLAY_TEXT   : value = ft_char (buffer, size);
                                     break;

        }  /*  End:  "switch (display_format) ..."  */

    }
    else                
    {

#ifdef MSDOS_TC
    if ((data_type == TB_VAXG_REAL && size >= 8) ||
        (data_type == TB_VAX_DOUBLE && size >= 8) ||
        (data_type == TB_VAX_REAL && size >= 8) ||
        (data_type == TB_FLOAT && size >= 8)) 
       {
          value = NULL;
          too_big = TRUE;
       }
#endif 

     if (! too_big)
        {
        switch (data_type)
        {
            case TB_ASCII_REAL           : value = ft_char (buffer, size); 
                                           break;
            case TB_ASCII_INTEGER        : value = ft_char (buffer, size); 
                                           break;
            case TB_ASCII_COMPLEX        : value = ft_char (buffer, size); 
                                           break;
            case TB_BOOLEAN              : value = ft_msb_integer (buffer, size, FALSE);
                                           break;
            case TB_CHARACTER            : value = ft_char (buffer, size); 
                                           break;
            case TB_COMPLEX              : value = ft_msb_ieee_complex(buffer,size);
                                           break;
            case TB_DATE                 : value = ft_char (buffer, size);
                                           break;
            case TB_FLOAT                : value = ft_msb_ieee_real(buffer,size);
                                           break;
            case TB_IBM_COMPLEX          : /*  tbd  */
                                           break;
            case TB_IBM_INTEGER          : /*  tbd  */
                                           break;
            case TB_IBM_REAL             : /*  tbd  */
                                           break;
            case TB_IBM_UNSIGNED_INTEGER : /*  tbd  */
                                           break;
            case TB_IEEE_COMPLEX         : value = ft_msb_ieee_complex(buffer,size);
                                           break;
            case TB_IEEE_REAL            : value = ft_msb_ieee_real(buffer,size);
                                           break;
            case TB_INTEGER              : value = ft_msb_integer(buffer,size,TRUE);
                                           break;
            case TB_LSB_BIT_STRING       : value = ft_msb_binary(buffer,size);
                                           break;
            case TB_LSB_IEEE_COMPLEX     : value = ft_lsb_ieee_complex(buffer,size);
                                           break;
            case TB_LSB_IEEE_REAL        : value = ft_lsb_ieee_real(buffer,size);
                                           break;
            case TB_LSB_INTEGER          : value = ft_lsb_integer(buffer,size,TRUE);
                                           break;                             
            case TB_LSB_UNSIGNED_INTEGER : value = ft_lsb_integer(buffer,size,FALSE);
                                           break;
            case TB_MAC_COMPLEX          : value = ft_msb_ieee_complex(buffer,size);
                                           break;
            case TB_MAC_INTEGER          : value = ft_msb_integer(buffer,size,TRUE);
                                           break;
            case TB_MAC_REAL             : value = ft_msb_ieee_real(buffer,size);
                                           break;
            case TB_MAC_UNSIGNED_INTEGER : value = ft_msb_integer(buffer,size,FALSE);
                                           break;
            case TB_MSB_BIT_STRING       : value = ft_msb_binary(buffer,size);
                                           break;
            case TB_MSB_IEEE_COMPLEX     : value = ft_msb_ieee_complex(buffer,size);
                                           break;
            case TB_MSB_IEEE_REAL        : value = ft_msb_ieee_real(buffer,size);
                                           break;
            case TB_MSB_INTEGER          : value = ft_msb_integer(buffer,size,TRUE);
                                           break;
            case TB_MSB_UNSIGNED_INTEGER : value = ft_msb_integer(buffer,size,FALSE);
                                           break;
            case TB_PC_COMPLEX           : value = ft_lsb_ieee_complex(buffer,size);
                                           break;
            case TB_PC_INTEGER           : value = ft_lsb_integer(buffer,size,TRUE);
                                           break;                             
            case TB_PC_REAL              : value = ft_lsb_ieee_real(buffer,size);
                                           break;
            case TB_PC_UNSIGNED_INTEGER  : value = ft_lsb_integer(buffer,size,FALSE);
                                           break;
            case TB_REAL                 : value = ft_msb_ieee_real(buffer,size);
                                           break;
            case TB_SUN_COMPLEX          : value = ft_msb_ieee_complex(buffer,size);
                                           break;
            case TB_SUN_INTEGER          : value = ft_msb_integer(buffer,size,TRUE);
                                           break;
            case TB_SUN_REAL             : value = ft_msb_ieee_real(buffer,size);
                                           break;
            case TB_SUN_UNSIGNED_INTEGER : value = ft_msb_integer(buffer,size,FALSE);
                                           break;
            case TB_TIME                 : value = ft_char (buffer, size);
                                           break;
            case TB_UNSIGNED_INTEGER     : value = ft_msb_integer(buffer,size,FALSE);
                                           break;
            case TB_VAX_COMPLEX          : value = ft_vax_complex(buffer,size,FALSE);
                                           break;
            case TB_VAX_DOUBLE           : value = ft_vax_real(buffer,size,FALSE);
                                           break;
            case TB_VAX_INTEGER          : value = ft_lsb_integer(buffer,size,TRUE);
                                           break;                             
            case TB_VAX_REAL             : value = ft_vax_real(buffer,size,FALSE);
                                           break;
            case TB_VAX_UNSIGNED_INTEGER : value = ft_lsb_integer(buffer,size,FALSE);
                                           break;                             
            case TB_VAXG_COMPLEX         : value = ft_vax_complex(buffer,size,TRUE);
                                           break;
            case TB_VAXG_REAL            : value = ft_vax_real(buffer,size,TRUE);
                                           break;        

        }  /*  End:  "switch (data_type) ..."  */

    }
    }  /*  End:  "if ((interchange_format == ASCII) || ... else ..."  */

    /*-----------------------------------------------------------------------*/
    /** Strip trailing blanks if a value was formatted, or pad the output   **/
    /**     buffer with stars otherwise.                                    **/
    /*-----------------------------------------------------------------------*/

    if (value != NULL)
        U_Strip_Trailing(value, ' ')
    else
    {
	U_Malloc_String(value, (int)size+1);
	for (i=0; i < size; ++i) *(value + i) = '*';
	*(value + size) = EOS;
    }

    /*-----------------------------------------------------------------------*/
    /** Return the formatted value                                          **/
    /*-----------------------------------------------------------------------*/
    
    return(value);

/** END **/

}  /*  "tb_format_column_value"  */


/**********************************************************************
 *$Component                                                          *
 *    COLUMN_INFO *tb_get_bit_column_info (column_ptr, column_info)   *
 *$Abstract                                                           *
 *    Fetch bit column info from label                                *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    column_ptr:                                                     *
 *        The column_ptr variable is a pointer to the label structure *
 *        for a column object.                                        *
 *    column_info:                                                    *
 *        The column_info variable is a pointer to the structure      *
 *        which contains keyword information on a column object.      *
 *$Outputs                                                            *
 *    column_info:                                                    *
 *        The column_info variable is a pointer to the structure      *
 *        which contains keyword information on a column object.      *
 *$Returns                                                            *
 *    bit_column_info:                                                *
 *        The bit_column_info variable is a pointer to the structure  *
 *        which contains keyword information on a bit column object.  *
 *$Detailed_Description                                               *
 *    This routine extracts all the bit column label information the  *
 *    program needs to display data and stores it in a list of        *
 *    COLUMN_INFO structures. The column_ptr input is the AGGREGATE   *
 *    structure of the column containing the bit_columns in question, *
 *    as output by the ODL parser. The column_info input is the       *
 *    tbtool columne keyword structure for the parent column.         *
 *    A list of column_info structures is returned. The column_info   *
 *    input is modified to reflect the number of bit columns found.   *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Error_Handling                                                     *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    Allocates memory for the list of column_info structures that    *
 *    store the bit_column information.                               *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   March 8, 1993                                             *
 *    1.2   October 10, 2002										  *
 *$Change_history                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDD   03-08-93   Updated header/comments                        *
 *	  MDC   10-03-02   Added initializers to some of the new_ptr      *
 *					   members since previous version was crashing    *
 *					   because of these un-initialized variables.	  *
 **********************************************************************/

COLUMN_INFO *tb_get_bit_column_info (column_ptr, column_info)

AGGREGATE column_ptr;
COLUMN_INFO *column_info;

{                                     
    AGGREGATE bit_column_ptr = {NULL};
    COLUMN_INFO *first_ptr = {NULL};
    COLUMN_INFO *last_ptr = {NULL};
    COLUMN_INFO *new_ptr = {NULL};
    char *str = {NULL};
    long i;
    long index;
    int status = {PDS_ERROR};

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** LOOP through the BIT_COLUMN objects in the current column           **/
    /*-----------------------------------------------------------------------*/

    for (i=1; (bit_column_ptr = 
       lab_find_object(column_ptr,"BIT_COLUMN",NULL,(int)i,&status)) != NULL; ++i)
    {
        /*-------------------------------------------------------------------*/
        /** Allocate storage for a new structure and initialize its fields  **/
        /*-------------------------------------------------------------------*/

        ++(column_info -> bit_column_count);

        new_ptr = (COLUMN_INFO *) malloc(sizeof(COLUMN_INFO));
        if (new_ptr == NULL) Exit_System()
                                                                   
        new_ptr -> next = NULL;
        new_ptr -> prev = NULL;
        new_ptr -> size = TB_UNKNOWN;
        new_ptr -> items = 1;
        new_ptr -> item_offset = 0;
        new_ptr -> start = TB_UNKNOWN;
        new_ptr -> column = bit_column_ptr;
        new_ptr -> display = TRUE;
        new_ptr -> bit_columns = NULL;
        new_ptr -> bit_column_count = 0;

		/*------------------------------------------------------------------*/
        /** 10-13-02 MDC												   **/
		/** Added initializations to the recently added members of the	   **/
		/** column_info structure.										   **/
		/*------------------------------------------------------------------*/
		new_ptr -> is_in_container = 0;
		new_ptr -> col_num = 0;
		new_ptr -> rep_num = 0;
		
		/*-----------------------------------------------------------------*/
		/** 10-03-02 MDC												  **/
		/** Initialized the rest of the new_ptr members. Program crashes  **/
		/** if these variables are not first initialized to NULL.		  **/
		/*-----------------------------------------------------------------*/
		new_ptr -> name = NULL;
		new_ptr -> class = NULL;
		new_ptr -> data_type = NULL;
		new_ptr -> display_format = NULL;

	New_String(new_ptr -> name, "Unknown");
	New_String(new_ptr -> class, "BIT_COLUMN");
	New_String(new_ptr -> data_type, "Unknown");
	New_String(new_ptr -> display_format, "Unknown");

        /*-------------------------------------------------------------------*/
        /** Replace the class name in the tbtool bit_column structure with  **/
        /**     the one in the ODL structure, in case it's different.       **/
        /*-------------------------------------------------------------------*/

        if (bit_column_ptr->name != NULL) 
            Replace_String(new_ptr->class,bit_column_ptr->name);

        /*-------------------------------------------------------------------*/
        /** Extract the keyword values from the label and store them in the **/
        /**     COLUMN_INFO structure: NAME, BITS (or ITEM_BITS), START_BIT **/
        /*-------------------------------------------------------------------*/

        str = lu_keyword_value (bit_column_ptr, "NAME", 1, TRUE);
        if (str != NULL) Replace_String(new_ptr -> name, str)
        Lemme_Go(str);

        str = lu_keyword_value (bit_column_ptr, "ITEM_BITS", 1, TRUE);
        if (str == NULL) str = lu_keyword_value (bit_column_ptr, "BITS", 1, TRUE);
        if (str != NULL) new_ptr -> size = Make_Long(str);
        Lemme_Go(str);

        str = lu_keyword_value (bit_column_ptr, "START_BIT", 1, TRUE);
        if (str != NULL) new_ptr -> start = Make_Long(str);
        Lemme_Go(str);

        /*-------------------------------------------------------------------*/
        /** Our desperate search for the bit column's data type must cascade**/
        /**     through the history of PDS standards.                       **/
        /** Once the data type is found, it must be stripped, de-underscored**/
        /**     and blank compressed.                                       **/
        /** Now if it's found on the approved list, save the data type in   **/
        /**    the column_info structure for the current bit column.        **/
        /*-------------------------------------------------------------------*/

        str = lu_keyword_value (bit_column_ptr, "BIT_ITEM_TYPE", 1, TRUE);
        if (str == NULL) str = lu_keyword_value (bit_column_ptr, "BIT_DATA_TYPE", 1, 
                                                 TRUE);
        if (str == NULL) str = lu_keyword_value (bit_column_ptr, "ITEM_TYPE", 1, 
                                                 TRUE);
        if (str == NULL) str = lu_keyword_value (bit_column_ptr, "DATA_TYPE", 1, 
                                                 TRUE);
        if (str != NULL)
        {
            util_replace_char (str, '_', ' ');
            util_compress_char (str, ' ');
            util_strip_lead_and_trail (str, ' ');
            util_upper_case (str);
            index = search_string_array(tb_bit_type_list, tb_bit_type_end, str);
            if (index != PDS_SEARCH_FAIL) Replace_String(new_ptr->data_type, str)
        }
        Lemme_Go(str);

        /*-------------------------------------------------------------------*/
        /** If TEXT is a valid display format for bit columns, then         **/
        /**     set the display format for this bit column to TEXT          **/
        /*-------------------------------------------------------------------*/

        index = search_string_array(tb_display_list,tb_display_end,"TEXT");
        if (index != PDS_SEARCH_FAIL) 
	    Replace_String(new_ptr->display_format, tb_display_list[index])

        /*-------------------------------------------------------------------*/
        /** Extract the keyword values from the label and store them in the **/
        /**     COLUMN_INFO structure: ITEMS, ITEM_OFFSET                   **/
        /** If ITEM_OFFSET not found, it defaults to BITS or ITEM_BITS      **/
        /*-------------------------------------------------------------------*/

        str = lu_keyword_value (bit_column_ptr, "ITEMS", 1, TRUE);
        if (str != NULL) new_ptr -> items = Make_Long(str);
        Lemme_Go(str);

        new_ptr -> item_offset = new_ptr -> size;
        str = lu_keyword_value (bit_column_ptr, "ITEM_OFFSET", 1, TRUE);
        if (str != NULL) new_ptr -> item_offset = Make_Long(str);
        Lemme_Go(str);

        /*-------------------------------------------------------------------*/
        /** Append the new structure onto the end of the bit column list    **/
        /*-------------------------------------------------------------------*/

        if (first_ptr == NULL)
            first_ptr = new_ptr;
        else
        {
            last_ptr -> next = new_ptr;
            new_ptr -> prev = last_ptr;
        }

        last_ptr = new_ptr;

    /*-----------------------------------------------------------------------*/
    /** ENDLOOP                                                             **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "for (i=1; ..."  */

    /*-----------------------------------------------------------------------*/
    /** RETURN bit column pointer                                           **/
    /*-----------------------------------------------------------------------*/

    return (first_ptr);

/** END **/

}  /*  "tb_get_bit_column_info"  */                      


/**********************************************************************
 *$Component                                                          *
 *    COLUMN_INFO *tb_get_column_info (table_ptr, table_info,         *
 *													    start_byte)   *
 *$Abstract                                                           *
 *    Fetch column info from label                                    *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    table_ptr:                                                      *
 *        The table_ptr variable is a pointer to the label structure  *
 *        for a table object.                                         *
 *    table_info:                                                     *
 *        The table_info variable is a pointer to the structure       *
 *        which contains keyword information on a table object.       *
 *	  start_byte:													  *
 *		  This variable is used in calculating the actual starting    *
 *		  byte of a nested container. This is used because of the     *
 *		  recursive call in this routine when a container is		  *
 *		  encountered. If this routine is called anywhere else,		  *
 *		  simply pass "0" as the 3rd argument.						  *
 *$Outputs                                                            *
 *    table_info:                                                     *
 *        The table_info variable is a pointer to the structure       *
 *        which contains keyword information on a table object.       *
 *$Returns                                                            *
 *    column_info:                                                    *
 *        The column_info variable is a pointer to the structure      *
 *        which contains keyword information on a column object.      *
 *$Detailed_Description                                               *
 *    This routine extracts all the column label information the      *
 *    program needs to display data and stores it in a list of        *
 *    COLUMN_INFO structures. The table_ptr input is the AGGREGATE    *
 *    structure of the table containing the columns in question,      *
 *    as output by the ODL parser. The list of column_info structures *
 *    is returned.  The table_info structure (the parent of the       *
 *    columns) is modified to reflect the number of columns found.    *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Error_Handling                                                     *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    Allocates memory for the list of column_info structures that    *
 *    store the column information.                                   *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   March 9, 1993                                             *
 *	  1.2   October 10, 2002										  *
 *$Change_history                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDD   03-09-93   Updated headers/commments                      *
 *	  MDC   10-10-02   Modified this routine to find also CONTAINER   *
 *					   objects. If a CONTAINER object is found, then  *
 *					   this routine must get the column information   *
 *					   in a slightly different manner.				  *
 *	  MDC   11-26-02   Modified the container_info to be a pointer    *
 *					   instead.										  *
 *    MDC	12-03-02   Declared current_start_byte to be a global     *
 *					   instead of being only local to this routine.	  *
 *	  MDC	02-26-03   Changed the current_start_byte back to a locl  *
 *					   instead of a global variable.				  *
 *					   Added a 3rd argument to this routine called    *
 *					   start_byte. This is used in calculating the    *
 *					   true start byte values when nested containers  *
 *					   are encountered. See the description above for *
 *					   more detail.									  *
 **********************************************************************/

COLUMN_INFO *tb_get_column_info (table_ptr, table_info, start_byte)

AGGREGATE table_ptr;
TABLE_INFO *table_info;
long start_byte;

{                                     
    AGGREGATE column_ptr = {NULL};
    COLUMN_INFO *first_ptr = {NULL};
    COLUMN_INFO *last_ptr = {NULL};
    COLUMN_INFO *new_ptr = {NULL};
    char *str = {NULL};
    long i;
    long index;
    int status = {PDS_ERROR};
	
	/* 10-01-02 MDC Added variables to implement handling containers */
	COLUMN_INFO *container_ptr = NULL;
	CONTAINER_INFO *container_info = NULL; 
	AGGREGATE temp_ptr = NULL;
	long counter = 0;
	long contain_column_count = 0;
	
	AGGREGATE parent_ptr = NULL;

	/*---------------------------------------------------------------*/
	/* 11-26-02 MDC													 */
	/* Added this variable to assign to the start byte value		 */
	/* obtained in the container_info structure so that we don't     */
	/* change this value when re-calculating the new start byte		 */
	/* during multiple repetitions in a container.					 */
	/*															     */
	/* 12-03-02 MDC												     */
	/* Made current_start_byte a global variable instead because if  */
	/* there is a nested container, we need to retain this value     */
	/* since it contains the actual START_BYTE value when we are in  */
	/* a repetition of a container.									 */
	/*																 */
	/* 02-26-03 MDC													 */
	/* Changed this variable back to a local variable.			     */
	/*---------------------------------------------------------------*/
	long current_start_byte = 0; 

/** BEGIN **/

    pds_generic_class = FALSE;   /* Find ALL column objects */
	
	/*---------------------------------------------------------------------*/
	/** 10-09-02 MDC													  **/  
	/** If the first argument to this "get_column_info" routine is a	  **/ 
	/** pointer to a container object, then grab its parameters to		  **/ 
	/** process the container correctly. Also, record the number of		  **/ 
	/** repetitions into the table_info structure.						  **/
	/**																	  **/
	/** 11-26-02 MDC													  **/
	/** Passing another parameter to the get_container_params routine to  **/
	/** record the container information into the table_info structure.   **/
	/*---------------------------------------------------------------------*/
	if(strcmp((table_ptr->name),"CONTAINER") == 0 )
	{
		container_info = get_container_params(table_ptr, table_info);

		append_to_container_list(container_info, table_info);
		/**********************************************************************/
		/* 02-26-03 MDC														  */
		/*																	  */
		/* If the container is nested inside a container, then we need to	  */
		/* calculate the actual start byte where the data can be found for    */
		/* this container. This can be done by adding the start_byte value    */
		/* passed into this routine with the start_byte value we have just    */
		/* obtained.														  */
		/*																	  */
		/**********************************************************************/
		parent_ptr = ParentAggregate(table_ptr);

		if( strcmp(parent_ptr->name, "CONTAINER") == 0)
			container_info->start_byte = start_byte + container_info->start_byte - 1;	

	}
	
	/*-----------------------------------------------------------------------*/
    /** LOOP through the COLUMN objects in the current table                **/
    /**																		**/
	/** 10-10-02 MDC														**/
	/** Added a do-while loop around the "for" loop to process a container. **/
	/** If there is no container present, it will simply execute the for    **/
	/** loop once.															**/
	/*-----------------------------------------------------------------------*/
	do
	{
		/*-------------------------------------------------------------------*/
		/** 10-10-02 Added by MDC											**/
		/**																	**/
		/** If the counter is not '0', it is implied that we are processing **/
		/**	columns within the container. Thus, the start_byte will change  **/
		/** because we are performing N number of iterations on the columns.**/
		/** The contain_column_count must be initialized back to 0. This    **/
		/** variable indicates what column we are on in the container.      **/
		/**																	**/
		/** 11-27-02 MDC													**/
		/** Developed an equation to calculate the new start byte of a		**/
		/** column when it goes into multiple repetitions:					**/
		/**																	**/
		/** new start byte = original start byte +							**/
		/**		  ( current repetition * number of bytes in the container)  **/ 
		/*-------------------------------------------------------------------*/
		if( (container_info != NULL) )
		{
			current_start_byte = container_info->start_byte 
								 + (counter * (container_info->bytes));

			contain_column_count = 0;
		}
		
		/*--------------------------------------------------------------------*/
		/** 10-10-02 Added by MDC											 **/
		/**																	 **/
		/**	Changed the argument to call lab_find_object with no object name.**/
		/** This is so it can recognize if there are container objects       **/
		/** within the label. If there is, we need to handle the column      **/
		/** objects in a different way. "i" is initialized to 2 since the 1st**/
		/** object found is actually the object in which the table_ptr		 **/
		/** variable is pointed to, and we don't want that.				     **/
		/*--------------------------------------------------------------------*/

/*		for (i=1; (column_ptr = 
		       lab_find_object(table_ptr,"COLUMN",NULL,(int)i,&status)) != NULL; ++i)   */
		for (i=2; (column_ptr =
				lab_find_object(table_ptr,NULL,NULL,(int)i,&status)) != NULL; ++i)
		{
			/*-------------------------------------------------------------*/
			/** 10-10-02 Added by MDC									  **/
			/** If the object found is a container, then....			  **/
			/*-------------------------------------------------------------*/
			if(strcmp( (column_ptr->name),"CONTAINER" ) == 0 )
			{
				
				/*--------------------------------------------------------*/
				/** Grab all the column objects within the container.    **/ 
				/** The value returned is the first pointer to a column  **/
				/** within the container.								 **/
				/**														 **/
				/** 02-26-03 MDC										 **/
				/** Since I added a 3rd argument to the					 **/
				/** tb_get_column_info routine, the current_start_byte   **/
				/** value will be passed. This is so the correct starting**/
				/** byte will be calculated for the container we are     **/
				/** about to obtain for this recursive call.			 **/
				/*--------------------------------------------------------*/

				container_ptr = tb_get_column_info(column_ptr,table_info, current_start_byte);
				
				if (first_ptr == NULL)
				{
					first_ptr = container_ptr;
				}
				else
				{
					last_ptr -> next = container_ptr;
					container_ptr -> prev = last_ptr;
				}
				last_ptr = container_ptr;
				
				/*--------------------------------------------------------*/
				/** Now move the last_ptr to point to the very end of    **/
				/** the structure since container_ptr points to the		 **/ 
				/** first column object in the container.				 **/
				/*--------------------------------------------------------*/
				while(last_ptr->next != NULL)
					last_ptr = last_ptr->next;

				/*--------------------------------------------------------*/
				/** Now we need to know what objects to skip so that we  **/
				/** don't process them again.							 **/
				/**														 **/
				/** 11-22-02 MDC										 **/
				/** Changed the "temp_ptr = NextSubAggregate..."		 **/
				/** declaration to a conditional statement because we	 **/
				/** only want to increment "i" (the object position)	 **/
				/** if the children of the object being passed in is	 **/
				/** found.												 **/
				/*--------------------------------------------------------*/
				temp_ptr = column_ptr;
				do
				{
					if( (temp_ptr = NextSubAggregate(column_ptr,temp_ptr)) != NULL)
						i++;

				} while(temp_ptr != NULL);
				
		/*		++(table_info->container_count); */ 

				/* Set this flag to FALSE again to find all objects */
				pds_generic_class = FALSE; 
			}
			/* If the object is a "COLUMN", then.... */										
			else if(strcmp( (column_ptr->name),"COLUMN" ) == 0 )
			{
                 ++(table_info->column_count); 
				
				/*--------------------------------------------------------------------*/
				/** 12-03-02 MDC													 **/
				/** Keep track of regular COLUMN objects and CONTAINER COLUMN		 **/
				/** objects. This is so we can use these values in calculating the   **/
				/** number of columns if you were to visually count them in a label. **/
				/*--------------------------------------------------------------------*/
			    
				if( container_info != NULL )
					++(table_info->container_columns);
				else
					++(table_info->ordinary_columns);

				/*-------------------------------------------------------------------*/
				/** Allocate storage for a new structure and initialize its fields  **/
				/*-------------------------------------------------------------------*/
				new_ptr = (COLUMN_INFO *) malloc(sizeof(COLUMN_INFO));
				if (new_ptr == NULL) Exit_System()
		        
				new_ptr -> next = NULL;
				new_ptr -> prev = NULL;
				new_ptr -> name = NULL;
				new_ptr -> class = NULL;
				new_ptr -> data_type = NULL;
				new_ptr -> display_format = NULL;
				new_ptr -> size = TB_UNKNOWN;
				new_ptr -> items = 1;
				new_ptr -> item_offset = 0;
				new_ptr -> start = TB_UNKNOWN;
				new_ptr -> column = column_ptr;
				new_ptr -> display = TRUE;
				new_ptr -> bit_columns = NULL;
				new_ptr -> bit_column_count = 0;
				new_ptr -> is_in_container = 0;
				new_ptr -> which_container = 0;
				new_ptr -> col_num = 0;
				new_ptr -> rep_num = 0;
				New_String(new_ptr -> name, "Unknown");
				New_String(new_ptr -> class, "COLUMN");
				New_String(new_ptr -> data_type, "Unknown");
				New_String(new_ptr -> display_format, "Unknown");
				
				/*-------------------------------------------------------------------*/
				/** 10-10-02 Added by MDC											**/
                /** Set the flag to detect a container to ON, record what column   	**/
				/** you are in inside the container, and record what repetition you **/
				/** are in.															**/
				/**																	**/
				/** 11-26-02 Added by MDC											**/
				/** Added a statement to record the container id in which the		**/
				/** column that is being processed is found. This allows us to      **/
				/** associate the column with the container it resides in. 			**/
				/*-------------------------------------------------------------------*/
				if( container_info != NULL )
				{
					new_ptr -> is_in_container = 1;
					new_ptr -> col_num = ++contain_column_count;
					new_ptr -> rep_num = counter + 1;

					new_ptr -> which_container = container_info->container_id;
				}
				
				/*-------------------------------------------------------------------*/
				/** Replace the class name in the tbtool column structure with      **/
				/**     the one in the ODL structure, in case it's different.       **/
				/*-------------------------------------------------------------------*/

				if (column_ptr->name != NULL) Replace_String(new_ptr->class,column_ptr->name);

				/*-------------------------------------------------------------------*/
				/** Extract the keyword values from the label and store them in the **/
				/**     COLUMN_INFO structure: NAME, ITEM_BYTES (or BYTES),         **/
				/**     START_BYTE.                                                 **/
				/*-------------------------------------------------------------------*/

				str = lu_keyword_value (column_ptr, "NAME", 1, TRUE);
				if (str != NULL) Replace_String(new_ptr -> name, str)
				Lemme_Go(str);

				str = lu_keyword_value (column_ptr, "ITEM_BYTES", 1, TRUE);
				if (str == NULL) str = lu_keyword_value (column_ptr, "BYTES", 1, TRUE);
				if (str != NULL) new_ptr -> size = Make_Long(str);
				Lemme_Go(str);

				str = lu_keyword_value (column_ptr, "START_BYTE", 1, TRUE);
				if (str != NULL) new_ptr -> start = Make_Long(str);
				Lemme_Go(str);
				
				if( ((new_ptr->start) != 0) && (container_info != NULL) )
				{
					(new_ptr->start) = (new_ptr->start) + (current_start_byte)-1;

				}
				
				/*-------------------------------------------------------------------*/
				/** Search the label structure for the column item type or data type**/
				/** Strip, de-underscore, blank compress, and upper-case it.        **/
				/** If it's a valid data type, store it as the type for the current **/
				/**   column                                                        **/
				/*-------------------------------------------------------------------*/

				str = lu_keyword_value (column_ptr, "ITEM_TYPE", 1, TRUE);
				if (str == NULL) str = lu_keyword_value (column_ptr, "DATA_TYPE", 1, TRUE);
				
				if (str != NULL)
				{
					util_replace_char (str, '_', ' ');
					util_compress_char (str, ' ');
					util_strip_lead_and_trail (str, ' ');
					util_upper_case (str);
					index = search_string_array(tb_type_list, tb_type_end, str);
				
					if (index != PDS_SEARCH_FAIL)
					{
						Lemme_Go(new_ptr->data_type);
						New_String(new_ptr->data_type,str);
					}
				}
				Lemme_Go(str);

				/*-------------------------------------------------------------------*/
				/** If TEXT is a valid display format for columns, then             **/
				/**     set the display format for this column to TEXT              **/
				/*-------------------------------------------------------------------*/

				index = search_string_array(tb_display_list,tb_display_end,"TEXT");
				if (index != PDS_SEARCH_FAIL) 
				Replace_String(new_ptr->display_format, tb_display_list[index])

				/*-------------------------------------------------------------------*/
				/** Extract the keyword values from the label and store them in the **/
				/**     COLUMN_INFO structure: ITEMS, ITEM_OFFSET                   **/
				/** If ITEM_OFFSET not found, it defaults to BYTES or ITEM_BYTES    **/
				/*-------------------------------------------------------------------*/

				str = lu_keyword_value (column_ptr, "ITEMS", 1, TRUE);
				if (str != NULL) new_ptr -> items = Make_Long(str);
				Lemme_Go(str);

				new_ptr -> item_offset = new_ptr -> size;
				str = lu_keyword_value (column_ptr, "ITEM_OFFSET", 1, TRUE);
				if (str != NULL) new_ptr -> item_offset = Make_Long(str);
				Lemme_Go(str);
				
				/*-------------------------------------------------------------------*/
				/** Fetch info on any bit columns in the column                     **/
				/*-------------------------------------------------------------------*/

				new_ptr -> bit_columns = tb_get_bit_column_info (column_ptr, new_ptr);

				/*-------------------------------------------------------------------*/
				/** Append the new structure onto the end of the column list        **/
				/*-------------------------------------------------------------------*/

				if (first_ptr == NULL)
				{
					first_ptr = new_ptr;
				}
				else
				{
					last_ptr -> next = new_ptr;
					new_ptr -> prev = last_ptr;
				}
				last_ptr = new_ptr;
			}

			/*-----------------------------------------------------------------------*/
			/** ENDLOOP                                                             **/
			/*-----------------------------------------------------------------------*/
			
		}  /*  End:  "for (i=1; ..."  */

		counter ++;

		/*---------------------------------------------------------------------------*/
		/* 02-24-03 MDC																 */
		/* This "if" condition was originally at the top of this do-while loop. It   */
		/* is moved down here because when there exists only 1 repetition for a		 */
		/* container, the number of columns within that container needs to be		 */
		/* recorded. Placing this statement at the top would miss recording this info*/
		/*---------------------------------------------------------------------------*/
		if(counter ==1 && container_info != NULL)
			container_info->columns = contain_column_count;

	} while( (container_info != NULL) && (counter < (container_info->repetitions)) );

	pds_generic_class = TRUE;
	
	/*-----------------------------------------------------------------------*/
	/** RETURN column pointer                                               **/
	/*-----------------------------------------------------------------------*/

	return (first_ptr);

/** END **/

}  /*  "tb_get_column_info"  */                      


/**********************************************************************
 *$Component                                                          *
 *    CONTAINER_INFO *get_container_params (container_ptr, table_info)*
 *$Abstract                                                           *
 *    Gathers container information.		                          *
 *$Keywords                                                           *
 *			                                                          *
 *                                                                    *
 *$Inputs                                                             *
 *	   container_ptr:											      *
 *			The input is a pointer to an AGGREGATE. It should be a    *
 *			pointer to a CONTAINER object.						      *
 *																	  *
 *	   table_info:													  *
 *		    This variable is passed mainly to store the container     *
 *			information into the table_info structure.				  *
 *$Outputs                                                            *
 *     container_info:					                              *
 *			The container_info variable will hold information about   *
 *			the container such as REPETITIONS, START_BYTE, and BYTES  *
 *			(the bytes within a container).							  *
 *$Returns                                                            *
 *    container_info:                                                 *
 *			See $outputs above.										  *
 *$Detailed_Description                                               *
 *    This routine will generate values of some of the keywords found *
 *	  in a CONTAINER object. The keyword values being gathered are    *
 *	  needed to process the columns within a CONTAINER correctly.     *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Author_and_Institution                                             *
 *    Michael D. Cayanan / J.P.L.                                     *
 *$Version_and_Date                                                   *
 *    1.0   October 10, 2002                                          *
 *$Change_history                                                     *
 *    MDC   10-10-02   Original code.                                 *
 *    MDC   11-26-02   Made the routine more efficient by declaring   *
 *					   and using pointers. Also changed the return    *
 *					   type to be a pointer. Since this routine       *
 *					   allocates memory, it will need to be returned  *
 *					   properly at some point in the program.		  *
 *					   This routine will now create a link list of    *
 *					   container objects if there are more than one.  *
 *					   These container objects will be linked together*
 *					   in the order that they are found within a	  *
 *					   label.										  *
 *					   Added checks in the routine to see if an actual*
 *					   value was found and obtained for the different *
 *					   container fields. If one of the parameters is  *
 *					   found to be invalid, display that on the screen*
 *					   and exit the program.						  *
 *	  MDC	12-03-02   Added calculations for the actual start byte   *
 *					   of a container if it is nested.				  *
 *	  MDC	02-26-03   Removed the piece of code that dealt with	  *
 *					   adding a container into a link list of		  *
 *					   container objects and made it its own separate *
 *					   function. This is just for the sake of making  *
 *					   the code more modular.					      *
 **********************************************************************/

CONTAINER_INFO *get_container_params(AGGREGATE container_ptr, TABLE_INFO *table_info)
{
	CONTAINER_INFO *container_info = NULL;
	char *str = NULL;
	CONTAINER_INFO *temp = NULL;
	LOGICAL error=FALSE;

	container_info = (CONTAINER_INFO *) malloc(sizeof(CONTAINER_INFO));
	Check_Malloc(container_info);

	New_String(container_info->name, "Unknown");
	container_info->bytes = 0;
	container_info->repetitions = 0;
	container_info->start_byte = 0;
	container_info->columns = 0;
	container_info->next_container = NULL;
	container_info->prev_container = NULL;
	container_info->container_id = 0;

	/* Grab container parameters */
	str = lu_keyword_value (container_ptr, "NAME", 1, TRUE);
	if (str != NULL)
	{
		Replace_String(container_info -> name, str)
	}
	else
	{
		printf("   The container's NAME field is invalid or missing.\n");
		error = TRUE;
	}
	Lemme_Go(str);
	
	str = lu_keyword_value(container_ptr,"BYTES",1,TRUE);
	if (str != NULL) 
		container_info->bytes = Make_Long(str);
	else
	{
		printf("   The container's BYTES field is invalid or missing.\n");
		error = TRUE;
	}
	Lemme_Go(str);

	str = lu_keyword_value(container_ptr,"START_BYTE",1,TRUE);
	if (str != NULL)
		container_info->start_byte = Make_Long(str);
	else
	{
		printf("\n   The container's START_BYTE field is invalid or missing.\n");
		error = TRUE;
	}
	Lemme_Go(str);

	str = lu_keyword_value(container_ptr,"REPETITIONS",1,TRUE);
	if (str != NULL)
		container_info->repetitions = Make_Long(str);
	else
	{
		printf("\n   The container's REPETITIONS field is invalid or missing.\n");
		error = TRUE;
	}
	Lemme_Go(str);

	if(error)
	{
		printf("\n   ERROR: Program cannot process columns correctly because\n");
		printf("          of invalid or missing fields in Container Object.\n\n");
        Exit_System();	
	}
	
	return(container_info);
}

/****************************************************************************************/
/* Component																			*/
/*	void append_to_container_list(CONTAINER_INFO *container_ptr,						*/ 
/*																TABLE_INFO *table_info)	*/
/*																						*/
/* Inputs:																				*/
/*		container_ptr																	*/
/*			A pointer to a container_structure that will be appended onto the container */
/*			list.																		*/
/*																						*/
/*		table_info																		*/
/*			This variable holds the container link list information.					*/
/*																						*/
/* Description:																			*/
/*		The function will simply append a container structure onto a container link		*/
/*		list. This list is found in the TABLE_INFO structure. This list must be			*/
/*		de-allocated sometime before the program exits.									*/
/*																						*/
/* History:																				*/
/*		02-25-03 MDC	Original Code													*/
/****************************************************************************************/

void append_to_container_list(CONTAINER_INFO *container_ptr, TABLE_INFO *table_info)
{
	CONTAINER_INFO *temp = NULL;

	/* if no container is found, then this is the first container to be
	   stored. */
	if(table_info->container == NULL)
	{
		table_info->container = container_ptr;
	}
	else
	{
		temp = table_info->container;

		/* keep moving until you reach the end of the list */
		while( (temp->next_container) != NULL )
			temp = temp->next_container;
		
		/* Now link the object at the end of the list with the container 
		   information that we've just obtained. */
		temp->next_container = container_ptr;
		container_ptr->prev_container = temp;
	}

	container_ptr->container_id = ++(table_info->container_count);
}


/**********************************************************************
 *$Component                                                          *
 *    short tb_get_command (command_str)                              *
 *$Abstract                                                           *
 *    Prompts user for input                                          *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    None                                                            *
 *$Outputs                                                            *
 *    command_str:                                                    *
 *        A character string containing information the user enters   *
 *        from the command line prompt.                               *
 *$Returns                                                            *
 *    command:                                                        *
 *        An integer representing the command the user enters from    *
 *        the command line prompt: usually the first character        *
 *        of the command.                                             *
 *$Detailed_Description                                               *
 *    This routine prompts the user for input and returns the first   *
 *    letter of the command entered.                                  *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   March 9, 1993                                             *
 *$Change_history                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDD   03-09-93   Updated header/comments                        *
 *	  MDC	08-02-02   Command variable initializes to 0 instead of   *
 *					   NULL to clean up warning msg.				  *
 **********************************************************************/

short tb_get_command (command_str)
         
char *command_str;

{
    char *c = {NULL};
 /*   short command = {NULL}; */
	short command = 0;

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Prompt the user                                                     **/
    /*-----------------------------------------------------------------------*/

    printf ("\n  %s ", TB_PROMPT_STRING);
    gets(command_str);

    /*-----------------------------------------------------------------------*/
    /** IF the first character is not a number THEN                         **/
    /**     Extract it and adjust the rest of the string                    **/
    /** ELSE                                                                **/
    /*-----------------------------------------------------------------------*/

    if (! isdigit(*command_str))
    {
        command = (short) toupper(*command_str);
        strcpy (command_str, (command_str + 1));
    }
    else
    {
        /*-------------------------------------------------------------------*/
        /** IF the last character is not a number THEN                      **/
        /**     Extract it and adjust the string                            **/
        /** ELSE                                                            **/
        /**     The command is either EDIT or JUMP-TO-ROW                   **/
        /** ENDIF                                                           **/
        /*-------------------------------------------------------------------*/

        c = String_End(command_str);

        if (! isdigit(*c))
        {
            command = (short) toupper(*c);
            *c = EOS;
        }
        else
        {
            if (tb_show_label)
                command = TB_EDIT;
            else
                command = TB_JUMP;
        }

    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if (! isdigit(*command_str)) ... else ..."  */

    /*-----------------------------------------------------------------------*/
    /** Do some final fiddling                                              **/
    /*-----------------------------------------------------------------------*/

    if (command == TB_QUIT) command = TB_EXIT;
    util_strip_lead_and_trail (command_str, ' ');

    /*-----------------------------------------------------------------------*/
    /** RETURN the command entered                                          **/
    /*-----------------------------------------------------------------------*/

    return (command);

/** END **/

}  /*  "tb_get_command"  */



/**********************************************************************
 *$Component                                                          *
 *    void tb_get_next_table ()                                       *
 *$Abstract                                                           *
 *    Get info for the next table in the label                        *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    None                                                            *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    This routine updates the global pointers and indices in order   *
 *    to display the next table in the label.                         *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   March 9, 1993                                             *
 *$Change_History                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDD   03-09-93   Updated header/comments                        *
 **********************************************************************/

void tb_get_next_table ()
                                  
{
/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** IF there are any tables on the list THEN                            **/
    /*-----------------------------------------------------------------------*/

    if (tb_table_info != NULL)
    {
        /*-------------------------------------------------------------------*/
        /** IF there are more tables on the list THEN                       **/
        /**     Update the pointers to point to the next table              **/
        /** ENDIF                                                           **/
        /*-------------------------------------------------------------------*/

        if (tb_table_info -> next == NULL)
            printf ("\n\n  You've reached the end of the table list");
        else
        {
            tb_table_info = tb_table_info -> next;
            ++tb_table_number;
        }

    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if (tb_table_info != NULL) ..."  */

    return;

/** END **/

}  /*  "tb_get_next_table"  */




/**********************************************************************
 *$Component                                                          *
 *    void tb_get_prev_table ()                                       *
 *$Abstract                                                           *
 *    Get info for the next table in the label                        *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    None                                                            *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    This routine updates the global pointers and indices in order   *
 *    to display the previous table in the label.                     *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   March 9, 1992                                             *
 *$Change_history                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDD   03-09-93   Updated header/comments                        *
 **********************************************************************/

void tb_get_prev_table ()
                                  
{
/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** IF there are any tables on the list THEN                            **/
    /*-----------------------------------------------------------------------*/

    if (tb_table_info != NULL)
    {
        /*-------------------------------------------------------------------*/
        /** IF there are more tables on the list THEN                       **/
        /**     Update the pointers to point to the previous table          **/
        /** ENDIF                                                           **/
        /*-------------------------------------------------------------------*/

        if (tb_table_info -> prev == NULL)
            printf ("\n\n  You've reached the beginning of the table list");
        else
        {
            tb_table_info = tb_table_info -> prev;
            --tb_table_number;
        }

    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "if (tb_table_info != NULL) ..."  */

    return;

/** END **/

}  /*  "tb_get_prev_table"  */




/**********************************************************************
 *$Component                                                          *
 *    void tb_get_file_info (label_ptr)                               *
 *$Abstract                                                           *
 *    Fetch file information from label                               *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    label_ptr:                                                      *
 *        The label_ptr variable is a pointer to the ROOT ODL         *
 *        aggregate in an ODL tree.                                   *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    This routine extracts all the file label information the        *
 *    program needs to display data and stores it in tbtool           *
 *    global variables.                                               *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Error_Handling                                                     *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   March 9, 1993                                             *
 *$Change_History                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDD   03-09-93   Updated header/comments                        *
 **********************************************************************/

void tb_get_file_info (label_ptr)

AGGREGATE label_ptr;

{
    char *str = {NULL};
    long index;

    /*----------------------------------------------------------------*/
    /** Initialize record type to unknown.                           **/
    /** Attempt to find record type in the label.                    **/
    /** De-underscore, compress, strip, and upper case it.           **/
    /** If it's legal, store the record type.                        **/
    /*----------------------------------------------------------------*/

    Replace_String(tb_record_type, "Unknown")
    str = lu_keyword_value (label_ptr, "RECORD_TYPE", 1, TRUE);
    if (str != NULL)
    {
        util_replace_char (str, '_', ' ');
        util_compress_char (str, ' ');
        util_strip_lead_and_trail (str, ' ');
        util_upper_case (str);
        index = search_string_array(tb_record_type_list, tb_record_type_end, str);
        if (index != PDS_SEARCH_FAIL) Replace_String(tb_record_type, str)
    }             
    Lemme_Go(str);

    /*----------------------------------------------------------------*/
    /** For label values: RECORD_BYTES and FILE_RECORDS              **/
    /** Initialize the value to unknown.                             **/
    /** Attempt to find the value in the label and store it.         **/
    /*----------------------------------------------------------------*/

    tb_record_bytes = TB_UNKNOWN;
    str = lu_keyword_value (label_ptr, "RECORD_BYTES", 1, TRUE);
    if (str != NULL) tb_record_bytes = Make_Long(str);
    Lemme_Go(str);

    tb_file_records = TB_UNKNOWN;
    str = lu_keyword_value (label_ptr, "FILE_RECORDS", 1, TRUE);
    if (str != NULL) tb_file_records = Make_Long(str);
    Lemme_Go(str);

    /*----------------------------------------------------------------*/
    /** Initialize blocking type to unknown.                         **/
    /** Attempt to find blocking type in the label.                  **/
    /** De-underscore, compress, strip, and upper case it.           **/
    /** If it's legal, store the blocking type.                      **/
    /*----------------------------------------------------------------*/

    Replace_String(tb_blocking_type, "Unknown")
    str = lu_keyword_value (label_ptr, "BLOCKING_TYPE", 1, TRUE);
    if (str != NULL)
    {
        util_replace_char (str, '_', ' ');
        util_compress_char (str, ' ');
        util_strip_lead_and_trail (str, ' ');
        util_upper_case (str);
        index = search_string_array(tb_blocking_type_list, tb_blocking_type_end, str);
        if (index != PDS_SEARCH_FAIL) Replace_String(tb_blocking_type, str)
    }             
    Lemme_Go(str);

    /*----------------------------------------------------------------*/
    /** For label values: BLOCK_BYTES and BLOCK_RECORDS              **/
    /** Initialize the value to unknown.                             **/
    /** Attempt to find the value in the label and store it.         **/
    /*----------------------------------------------------------------*/

    tb_block_bytes = TB_UNKNOWN;
    str = lu_keyword_value (label_ptr, "BLOCK_BYTES", 1, TRUE);
    if (str != NULL) tb_block_bytes = Make_Long(str);
    Lemme_Go(str);

    tb_block_records = TB_UNKNOWN;
    str = lu_keyword_value (label_ptr, "BLOCK_RECORDS", 1, TRUE);
    if (str != NULL) tb_block_records = Make_Long(str);
    Lemme_Go(str);

    return;

}  /*  tb_get_label_info  */




/**********************************************************************
 *$Component                                                          *
 *    TABLE_INFO *tb_get_table_info (label_ptr, fname)                *
 *$Abstract                                                           *
 *    Fetches table info from the PDS label                           *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    label_ptr:                                                      *
 *        The label_ptr variable is a pointer to the ROOT ODL         *
 *        aggregate in an ODL tree.                                   *
 *    fname:                                                          *
 *        The fname variable is a character string containing the     *
 *        name of a file.                                             *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    table_list:                                                     *
 *        The table_list variable is a pointer to a list of           *
 *        structures containing information used to display the       *
 *        data in a table.                                            *
 *$Detailed_Description                                               *
 *    This routine searches for all the TABLE objects in a PDS label, *
 *    and creates a list of structures to hold the information needed *
 *    to display the data.  The list of table_info structures is      *
 *    returned.  This routine will also look for table-like objects,  *
 *    such as SERIES and SPECTRUM. The fname input is the name of the *
 *    table label file.                                               *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Side_Effects                                                       *
 *    Allocates memory for the list of table_info structures that     *
 *    store the table information.                                    *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.2   March 9, 1993                                             *
 *$Change_history                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDD   02-15-93   Added code machinations to try harder to find  *
 *                     the table data file.                           *
 *    MDD   03-09-93   Updated header/comments                        *
 **********************************************************************/

TABLE_INFO *tb_get_table_info (label_ptr, fname)

AGGREGATE label_ptr;
char *fname;

{                                     
    AGGREGATE table_ptr = {NULL};
    TABLE_INFO *first_ptr = {NULL};
    TABLE_INFO *last_ptr = {NULL};
    TABLE_INFO *new_ptr = {NULL};
    POINTER_INFO p_info;
    FILE *f_ptr = {NULL};
    char *str = {NULL};
    char *thing[3];
    long i;
    long j;
    long k;
    long index;
    int status = {PDS_ERROR};
    LOGICAL found = {FALSE};
    char savefile [PDS_MAXLINE + 1];
    char *newfile = {NULL};
    char *dir = {NULL};
    int x = 0;
/** BEGIN **/

	CONTAINER_INFO *temp_ptr = NULL;

    /*-----------------------------------------------------------------------*/
    /** Initialize a set of generic classes that tbtool looks for.          **/
    /*-----------------------------------------------------------------------*/
    
    for(x=0; x < 3; ++x)
	thing[x] = NULL;

    New_String(thing[0], "TABLE");
    New_String(thing[1], "SPECTRUM");
    New_String(thing[2], "SERIES");
    pds_generic_class = TRUE;
    tb_table_count = 0;
    tb_table_number = 1;

    /*-----------------------------------------------------------------------*/
    /** LOOP through the list of TABLE type objects                         **/
    /*-----------------------------------------------------------------------*/

    for (k=0; k <= 2; ++k)
    {
        /*-----------------------------------------------------------------------*/
        /** LOOP through the objects in the label that are of the current type  **/
        /*-----------------------------------------------------------------------*/

	for (i=1; (table_ptr = 
               lab_find_object(label_ptr,thing[k],NULL,(int)i,&status)) != NULL; ++i)
        {
            ++tb_table_count;           
    
            /*-------------------------------------------------------------------*/
            /** Allocate storage for a new structure and initialize its fields  **/
            /*-------------------------------------------------------------------*/
    
            new_ptr = (TABLE_INFO *) malloc(sizeof(TABLE_INFO));
            if (new_ptr == NULL) Exit_System()
    
            new_ptr -> next = NULL;
            new_ptr -> prev = NULL;
	    new_ptr -> name = NULL;
	    new_ptr -> class = NULL;
	    new_ptr -> fname = NULL;
	    new_ptr -> interchange_format = NULL;
            new_ptr -> table = table_ptr;
            new_ptr -> columns = NULL;
            new_ptr -> row_count = TB_UNKNOWN;
            new_ptr -> row_bytes = TB_UNKNOWN;
            new_ptr -> prefix_bytes = 0;
            new_ptr -> suffix_bytes = 0;
            new_ptr -> column_count = 0;
            new_ptr -> data_location = 1;
			/* 10-10-02 Added by MDC */
			new_ptr -> container_count = 0;
		/*	new_ptr -> net_column = 0; */
			new_ptr -> container_columns = 0;
			/* 12-03-02 Added by MDC */
			new_ptr -> ordinary_columns = 0;
			/*11-26-02 Added by MDC */
			new_ptr -> container = NULL;
            New_String(new_ptr -> name, "Unknown");
            New_String(new_ptr -> class, "TABLE");
            New_String(new_ptr -> fname, "Unknown");
            New_String(new_ptr -> interchange_format, "Unknown");
                
            /*-------------------------------------------------------------------*/
            /** Replace the class name in the tbtool table info structure with  **/
            /**     the one in the ODL structure, in case it's different.       **/
            /*-------------------------------------------------------------------*/

            if (table_ptr->name != NULL) Replace_String(new_ptr->class, 
                table_ptr->name);
    
            /*-------------------------------------------------------------------*/
            /** Extract the keyword values from the label and store them in the **/
            /**     TABLE_INFO structure: NAME, ROWS, BYTES (or ROW_BYTES)      **/
            /**     ROW_PREFIX_BYTES, ROW_SUFFIX_BYTES                          **/
            /*-------------------------------------------------------------------*/
    
            str = lu_keyword_value (table_ptr, "NAME", 1, TRUE);
            if (str != NULL) Replace_String(new_ptr -> name, str)
            Lemme_Go(str);
    
            str = lu_keyword_value (table_ptr, "ROWS", 1, TRUE);
			
			if (str == NULL) str = lu_keyword_value (table_ptr, "TABLE_ROWS", 1, TRUE);
            if (str != NULL) new_ptr -> row_count = Make_Long(str);
            Lemme_Go(str);
    
            str = lu_keyword_value (table_ptr, "ROW_BYTES", 1, TRUE);
            if (str == NULL) str = lu_keyword_value (table_ptr, "BYTES", 1, TRUE);
            new_ptr -> row_bytes = (str != NULL) ? Make_Long(str) : tb_record_bytes;
            Lemme_Go(str);
    
            str = lu_keyword_value (table_ptr, "ROW_PREFIX_BYTES", 1, TRUE);
            new_ptr -> prefix_bytes = (str != NULL) ? Make_Long(str) : 0;
            Lemme_Go(str);
    
            str = lu_keyword_value (table_ptr, "ROW_SUFFIX_BYTES", 1, TRUE);
            new_ptr -> suffix_bytes = (str != NULL) ? Make_Long(str) : 0;
            Lemme_Go(str);
    
            /*-------------------------------------------------------------------*/
            /** Search the label structure for interchange format or just format**/
            /** Strip, de-underscore, blank compress, and upper-case it.        **/
            /** If it's a valid format, store it as the format for the current  **/
            /**   table                                                         **/
            /*-------------------------------------------------------------------*/

            str = lu_keyword_value (table_ptr, "INTERCHANGE_FORMAT", 1, TRUE);
            if (str == NULL) str = lu_keyword_value (table_ptr, "FORMAT", 1, TRUE);
            if (str != NULL)
            {
                util_replace_char (str, '_', ' ');
                util_compress_char (str, ' ');
                util_strip_lead_and_trail (str, ' ');
                util_upper_case (str);
                index = search_string_array(tb_format_list, tb_format_end, str);
                if (index != PDS_SEARCH_FAIL) 
                    Replace_String(new_ptr->interchange_format, str)
            }             
            Lemme_Go(str);
    
            /*-------------------------------------------------------------------*/
            /** Look for the pointer that matches the class name of this table- **/
            /**    type object. If this is the i-th object of this class, then  **/
            /**    start at the i-th such pointer and work backwards until a    **/
            /**    match is found.                                              **/
            /*-------------------------------------------------------------------*/

            for (j=i, found=FALSE; ((j > 0) && ! (found = 
		    lt_get_pointer(label_ptr,table_ptr->name,(int)j,&p_info))); --j) ;
    
            /*-------------------------------------------------------------------*/
            /** IF a pointer was found for the current TABLE object THEN        **/
            /*-------------------------------------------------------------------*/

            if (found)
            {
                /*---------------------------------------------------------------*/
                /** Compute the starting location of the data using the pointer **/
                /**     information.                                            **/
                /** Store the file name if the pointer is for a detached file.  **/
                /** Otherwise, the data are attached so use the label file name **/
                /**     as the data file name for this TABLE object             **/
                /*---------------------------------------------------------------*/

                new_ptr->data_location = (p_info.has_byte_loc) ?
                         p_info.location : (tb_record_bytes*(p_info.location-1)) + 1;
    
                if (*(p_info.file_name) != EOS) 
                    Replace_String(new_ptr->fname, p_info.file_name)
                else
                    Replace_String(new_ptr->fname, fname)
    
                /*---------------------------------------------------------------*/
                /** Attempt to open the data file as is.                        **/
                /** If that fails, try lowercasing the file name.               **/
                /** If that fails try adding the label file path name.          **/
                /** If that fails, lowercase the entire name and call it a day. **/
                /*---------------------------------------------------------------*/

                if ((f_ptr = fopen (new_ptr -> fname, "rb")) == NULL)
                {
                    strcpy (savefile, new_ptr -> fname);
                    util_lower_case (new_ptr -> fname);
                    if ((f_ptr = fopen (new_ptr -> fname, "rb")) == NULL)
                    {
                       if ((dir = sys_get_path (new_ptr -> fname)) == NULL)
                       {
                          dir = sys_get_path (fname);
                          newfile = util_create_file_spec (dir, savefile);
                          Replace_String(new_ptr -> fname, newfile);
                          Lemme_Go(newfile);
                          if ((f_ptr = fopen (new_ptr -> fname, "rb")) == NULL)
                             util_lower_case (new_ptr -> fname);
		       }
                       Lemme_Go(dir);
                    }
		}
                if (f_ptr) fclose (f_ptr);
    
            }  /*  End:  "if (found) ..."  */
    
            /*----------------------------------------------------------------*/
            /** Fetch info on any columns in the table                       **/
            /*----------------------------------------------------------------*/
    
            new_ptr -> columns = tb_get_column_info (table_ptr, new_ptr, 0);
    
            /*-------------------------------------------------------------------*/
            /** Extract the keyword values from the label and store them in the **/
            /**     TABLE_INFO structure: COLUMNS (or ROW_COLUMNS)              **/
            /** Print a message if this is not the same as the number of        **/
            /**     COLUMN objects found in the label.                          **/
            /*-------------------------------------------------------------------*/
    
            str = lu_keyword_value (table_ptr, "COLUMNS", 1, TRUE);
            if (str == NULL) 
                str = lu_keyword_value (table_ptr, "ROW_COLUMNS", 1, TRUE);
          
			/*-----------------------------------------------------------------*/
			/** 10-11-02 MDC												  **/
			/** Added a check to see if net_column is 0 or not. net_column is **/
			/** the number of columns in the label if you were to physically  **/
			/** look at the label. The value of COLUMNS in the label does not **/
			/** take into account a container and the number of repetitions.  **/
			/** So, thats why we need to check this variable as well.		  **/
			/*-----------------------------------------------------------------*/
			
			if( new_ptr->container_columns != 0)
			{	
				/*-----------------------------------------------------------*/
				/* 12-03-02 MDC												 */
				/*															 */
				/* The calculation for this if condition is as follows:		 */
				/* total number of columns in the table - total number of    */
				/* container columns in the table + total number of the		 */
				/* container columns FOR THE FIRST REPETITION ONLY.			 */
				/* This value should equal the value grabbed from the label. */
				/* It is the value if you were to visually count the columns */
				/* in the label.											 */
				/*-----------------------------------------------------------*/
				if((str != NULL) && 
				   (Make_Long(str) != 
				   (new_ptr->column_count - new_ptr->container_columns + (Make_Long(str) - new_ptr->ordinary_columns))) ) 
				{
					printf ("\n\n  WARNING:  The number of COLUMN objects found in the file does not\n");
					printf ("            agree with the information in the label.\n\n");
				}
			}
			else
			{
				if ((str != NULL) && (Make_Long(str) != new_ptr->column_count))
			    {
				    printf ("\n\n  WARNING:  The number of COLUMN objects found in the file does not\n");
					printf ("            agree with the information in the label.\n\n");
				}
			}
            Lemme_Go(str);
    
            /*----------------------------------------------------------------*/
            /** Append the new structure onto the end of the table list      **/
            /*----------------------------------------------------------------*/
    
            if (first_ptr == NULL)
                first_ptr = new_ptr;
            else
            {                          
                last_ptr -> next = new_ptr;
                new_ptr -> prev = last_ptr;
            }
    
            last_ptr = new_ptr;
    
        /*-----------------------------------------------------------------------*/
        /** ENDLOOP                                                             **/
        /*-----------------------------------------------------------------------*/
    
        }  /*  End:  "for (i=1; ..."  */

    }  /*  End:  "for (k=0; ..."  */

   /*-----------------------------------------------------------------------*/
   /** ENDLOOP                                                             **/
   /*-----------------------------------------------------------------------*/

    Lemme_Go(thing[0]);
    Lemme_Go(thing[1]);
    Lemme_Go(thing[2]);

    /*-----------------------------------------------------------------------*/
    /** RETURN table pointer                                                **/
    /*-----------------------------------------------------------------------*/

    return (first_ptr);

/** END **/

}  /*  "tb_get_table_info"  */                      



/**********************************************************************
 *$Component                                                          *
 *    void tb_jump_to_row (row_movement)                              *
 *$Abstract                                                           *
 *    Updates offsets and pointers for a new row                      *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    row_movement:                                                   *
 *        The row number the software is supposed to jump to.         *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    This routine updates the global row offsets to allow the        *
 *    software to display the data at the row passed in.              *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   March 9, 1993                                             *
 *$Change_history                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDD   03-09-93   Updated header/comments                        *
 **********************************************************************/

void tb_jump_to_row (row_movement)

long row_movement;

{                                           
    if (tb_table_info != NULL)
    {
        tb_row_offset = row_movement - 1;

        if (tb_row_offset < 0)
            tb_row_offset = 0;
        else
            if ((tb_row_offset + tb_display_rows) > tb_table_info->row_count)
                tb_row_offset = tb_table_info -> row_count - tb_display_rows;
			
    }  /*  End:  "if (tb_table_info ..."  */

    return; 

}  /*  "tb_jump_to_row"  */



/**********************************************************************
 *$Component                                                          *
 *    unsigned char *tb_justify_bit_column_value (bit_column_info,    *
 *                                        buffer, size, signed_value) *
 *$Abstract                                                           *
 *    Extracts and right justifies a string of bits                   *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    bit_column_info:                                                *
 *        The bit_column_info variable is a pointer to the structure  *
 *        which contains keyword information on a bit column object.  *
 *    buffer:                                                         *
 *        The buffer variable is a character string containing a      *
 *        column value.                                               *
 *    signed_value:                                                   *
 *        The signed_value variable is a flag which indicates whether *
 *        or not the string of bits extracted is to be interpreted    *
 *        as a signed number.                                         *
 *$Outputs                                                            *
 *    size:                                                           *
 *        The size variable is the size, in bytes, of a column value. *
 *$Returns                                                            *
 *    justified_value:                                                *
 *        The justified_value variable is a four byte string          *
 *        containing the bits extracted and right justified.          *
 *$Detailed_Description                                               *
 *    This routine takes a character string buffer and extracts the   *
 *    string of bits specified in the bit column info structure.      *
 *    These bits may occur anywhere in the buffer, as long as they    *
 *    are contiguous.  The routine extracts the bits and puts them    *
 *    in another character string buffer, right justifying them, and  *
 *    padding them on the left with zeros if the value is positive    *
 *    or ones if the value is negative.                               *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   March 9, 1993                                             *
 *$Change_history                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDD   03-09-93   Updated comments/header                        *
 **********************************************************************/

unsigned char *tb_justify_bit_column_value (bit_column_info, 
                                            buffer, size, signed_value)

COLUMN_INFO *bit_column_info;
unsigned char *buffer;
long *size;
LOGICAL signed_value;

{
    static unsigned char mask [8] = {1,2,4,8,16,32,64,128};
    unsigned char *new_buffer = {NULL};
    unsigned char *source_byte = {NULL};
    unsigned char *dest_byte = {NULL};
    long bit=0;
    long num_bits=0;
    long start_bit=0;
    long stop_bit=0;
    long dest_mask_num=0;
    long source_mask_num=0;
    long dest_bit=0;
    long i=0;
    LOGICAL negative = {FALSE};

/** BEGIN **/

    /*-----------------------------------------------------------------------*/
    /** Determine the number of bits to be extracted and the start and stop **/
    /**     bits in the buffer.                                             **/
    /*-----------------------------------------------------------------------*/

    num_bits = bit_column_info->size;
    start_bit = bit_column_info->start + (num_bits*(tb_bit_item_count - 1));
    stop_bit = start_bit + num_bits - 1;

    /*-----------------------------------------------------------------------*/
    /** Determine the index of the byte containing the stop bit, and the    **/
    /**     index of the source bit mask to be used.                        **/
    /*-----------------------------------------------------------------------*/

    source_byte = buffer + ((int) ((stop_bit-1)/8));
    source_mask_num = 7 - ((stop_bit-1)%8);

    /*-----------------------------------------------------------------------*/
    /** Set the size, malloc the return buffer, and initialize it.          **/
    /*-----------------------------------------------------------------------*/

    *size = sizeof(long);  
    U_Malloc_String(new_buffer, (int) *size);
    for (i=0; i < *size; ++i)  *(new_buffer+i) = 0;

    /*-----------------------------------------------------------------------*/
    /** Determine the index of the byte in the return buffer where we can   **/
    /**     start copying bits, the index of the dest mask, and the         **/
    /**     location where the first bit will be copied.                    **/
    /*-----------------------------------------------------------------------*/

    dest_byte = new_buffer + (*size - 1);
    dest_mask_num = 0;
    dest_bit = 8*(*size) - 1;

    /*-----------------------------------------------------------------------*/
    /** LOOP backwards through the bits in the source buffer, from stop bit **/
    /**     to start bit ...                                                **/
    /*-----------------------------------------------------------------------*/

    for (bit=stop_bit; bit >= start_bit; --bit)
    {
        /*-------------------------------------------------------------------*/
        /** IF the current bit is '1' in the source buffer THEN             **/
        /**     Set the corresponding bit in the return buffer to '1'       **/
        /** ENDIF                                                           **/
        /*-------------------------------------------------------------------*/

	if ((*source_byte & mask[source_mask_num]) == mask[source_mask_num])
        {
	    *dest_byte = *dest_byte | mask[dest_mask_num];
            negative = (signed_value && (bit == start_bit));
        }

        /*-------------------------------------------------------------------*/
        /** Adjust the source and return mask indices so we can check the   **/
        /**    next bit in the source buffer, and decrement the source and  **/
        /**    return buffer indices if we have reached the first bit in    **/
        /**    the byte.                                                    **/
        /*-------------------------------------------------------------------*/

	source_mask_num = (++source_mask_num)%8;  /* Use the next source mask */
	if ((bit-1)%8 == 0) --source_byte;  /* First bit in the current byte? */
	dest_mask_num = (++dest_mask_num)%8;  /* Use the next return mask */
        if (dest_mask_num == 0) --dest_byte; /* First bit in the current byte? */

        /*-------------------------------------------------------------------*/
        /** Decrement the loop control variable                             **/
        /*-------------------------------------------------------------------*/

        --dest_bit;

    /*-----------------------------------------------------------------------*/
    /** ENDLOOP                                                             **/
    /*-----------------------------------------------------------------------*/

    }  /*  End:  "for (bit=stop_bit; ..."  */

    /*-----------------------------------------------------------------------*/
    /** IF the value is negative then LOOP through the remaining bits in    **/
    /**     the return buffer, from right to left, setting them all to '1'. **/
    /**     (We're padding on the left with ones.  Positive numbers are     **/
    /**      automatically padded with zeros)                               **/
    /** ENDIF                                                               **/
    /*-----------------------------------------------------------------------*/

    for ( ; ((dest_bit >= 0) && negative); --dest_bit)
    {
	*dest_byte = *dest_byte | mask[dest_mask_num];
	dest_mask_num = (++dest_mask_num)%8;
        if (dest_mask_num == 0) --dest_byte; 
    }

    /*-----------------------------------------------------------------------*/
    /** RETURN the return buffer                                            **/
    /*-----------------------------------------------------------------------*/

    return (new_buffer);

/** END **/

}  /*  "tb_justify_bit_column_value"  */



/**********************************************************************
 *$Component                                                          *
 *    void tb_move_left (column_info, item_count, column_offset,      *
 *                       col_movement, column_number)                 *
 *$Abstract                                                           *
 *    Moves to the left through columns or bit_columns                *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    column_info:                                                    *
 *        The column_info variable is a pointer to the structure      *
 *        which contains keyword information on a column object.      *
 *    item_count:                                                     *
 *        The item_count variable keeps track of the current item in  *
 *        a column object (i.e, the item being displayed).            *
 *    column_offset:                                                  *
 *        The column_offset variable keep track of the "real" offset  *
 *        of the current column being displayed. The "real" offset    *
 *        of a column is the sum of all the column items to the       *
 *        left of it. Column_offset also applies to bit_columns.      *
 *    col_movement:                                                   *
 *        The col_movement variable is an integer representing the    *
 *        number of columns or bit_columns to move to the right or    *
 *        left, including all items.                                  *
 *    column_number:                                                  *
 *        The "column_number" variable is an integer representing     *
 *        the "user" offset of a column. The "user" offset of a       *
 *        column is a count of all the column objects (not including  *
 *        items) to the left of a column.  Column_number also applies *
 *        to bit_columns.                                             *
 *$Outputs                                                            *
 *    column_info:                                                    *
 *        The column_info variable is a pointer to the structure      *
 *        which contains keyword information on a column object.      *
 *    item_count:                                                     *
 *        The item_count variable keeps track of the current item in  *
 *        a column object (i.e, the item being displayed).            *
 *    column_offset:                                                  *
 *        The column_offset variable keep track of the "real" offset  *
 *        of the current column being displayed. The "real" offset    *
 *        of a column is the sum of all the column items to the       *
 *        left of it. Column_offset also applies to bit_columns.      *
 *    col_movement:                                                   *
 *        The col_movement variable is an integer representing the    *
 *        number of columns or bit_columns to move to the right or    *
 *        left, including all items.                                  *
 *    column_number:                                                  *
 *        The "column_number" variable is an integer representing     *
 *        the "user" offset of a column. The "user" offset of a       *
 *        column is a count of all the column objects (not including  *
 *        items) to the left of a column.  Column_number also applies *
 *        to bit_columns.                                             *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    This routine moves left through columns or bit_columns in a     *
 *    label.  It takes into account the ITEMS keyword when counting   *
 *    backwards through a list of columns.  It will skip over columns *
 *    which have the display flag set to false.  When it determines   *
 *    the new column, it resets the column_info pointer, the item     *
 *    count, the column offset, and column_number for output.         *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Error_Handling                                                     *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   April 9, 1993                                             *
 *$Change_history                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDD   04-09-93   Updated header/comments                        *
 *    MDC	12-05-02   Added ability to search for a container column *
 **********************************************************************/

void tb_move_left (column_info, item_count, column_offset, col_movement,
                   column_number)

COLUMN_INFO *(*column_info);
long *item_count;
long *column_offset;
long col_movement;
long *column_number;

{
    COLUMN_INFO *ptr = {NULL};
    long i;
    long col_count;

	LOGICAL found_it = FALSE; /* 12-05-02 Added by MDC */

/** BEGIN **/

	/*-------------------------------------------------------------*/
	/* 12-05-02 MDC												   */
	/* If we want to find a particular container....			   */
	/*-------------------------------------------------------------*/
	if(tb_show_container)
	{
		/*--------------------------------------------------------------*/
		/* Check if its within the boundaries first.					*/
		/*--------------------------------------------------------------*/
		if(col_movement <= 0)
			return;

		if( ((*column_info) != NULL) && ((*column_info)->prev != NULL) )  
		{
			/*----------------------------------------------------------*/
            /* Keep looping backwards through the column list until we  */
			/* find the container we are looking for.					*/
			/*----------------------------------------------------------*/
			while(found_it == FALSE)
			{
				/*---------------------------------------------------------------*/
				/** LOOP backwards through the column list (i.e., move to left) **/
				/**    until either we find a column with the display flag set, **/
				/**    or the first column is reached.                          **/
				/**    For each column we pass over because it isn't displayed, **/
				/**        add the number of items to a counter (assuming we    **/
				/**        are displaying data and not labels).                 **/ 
				/** ENDLOOP                                                     **/
				/*---------------------------------------------------------------*/
				
				for (i=0, ptr = (*column_info) -> prev;
						((ptr != NULL) && (! tb_show_label) && 
							(! ptr->display)); 
							ptr = ptr -> prev)
				{
					i += ptr -> items;
				}

				/*---------------------------------------------------------------*/
				/** IF we found a column to move left to THEN                   **/
				/**    add the number of items in it to the number we passed up **/
				/**    subtract this total number of items from the column      **/
				/**       offset that was passed in, to get the new column      **/
				/**       offset.                                               **/
				/**    reset the column_info pointer                            **/
				/**    reset the item count output for the new column           **/
				/**    decrement the column number                              **/
				/** ENDIF                                                       **/
				/*---------------------------------------------------------------*/

				if (ptr != NULL)
				{
					i += ptr -> items;
					*column_offset -= i;
					*column_info = ptr;
				/*  *item_count = (*column_info) -> items; */
					if(tb_show_items == TRUE)
						*item_count = (*column_info) -> items;
					else
						*item_count = 1;

					--(*column_number);
				}
				else
					break;
				
				/*---------------------------------------------------------------*/
				/* If we have found a column that is within the container we	 */
				/* want, and this column is the first column in the container    */
				/* in the first repetition, then we have found the right one.    */
				/*---------------------------------------------------------------*/
				if( ((*column_info)->which_container == col_movement) && 
				((*column_info)->rep_num == 1) && ((*column_info)->col_num == 1))
					found_it = TRUE;	

			} /* END: while(found_it == FALSE)... */

		} /* END: if( ((*column_info) != NULL)... */
	} /* End: if(tb_show_container)... */
	else
	{
		/*----------------------------------------------------------------------*/
		/** LOOP through COL_MOVEMENT columns, starting with the current one   **/
		/*----------------------------------------------------------------------*/

	    for (col_count = 0; ((*column_info != NULL) && 
		                                 (col_count < col_movement)); ++col_count)
		{
			/*------------------------------------------------------------------*/
			/** IF the current column has items left and we are displaying     **/
			/**    data as opposed to labels THEN                              **/
			/**    decrement the current item count (move one item to the left)**/
			/*------------------------------------------------------------------*/

			if ((*item_count > 1) && tb_show_items && (! tb_show_label))
				--(*item_count);

			/*------------------------------------------------------------------*/
			/** ELSE                                                           **/
			/*------------------------------------------------------------------*/

			else
			{
			/*---------------------------------------------------------------*/
			/** LOOP backwards through the column list (i.e., move to left) **/
			/**    until either we find a column with the display flag set, **/
			/**    or the first column is reached.                          **/
			/**    For each column we pass over because it isn't displayed, **/
			/**        add the number of items to a counter (assuming we    **/
			/**        are displaying data and not labels).                 **/ 
			/** ENDLOOP                                                     **/
			/*---------------------------------------------------------------*/

				for (i=0, ptr = (*column_info) -> prev;
						((ptr != NULL) && tb_show_items && (! tb_show_label) && 
							(! ptr->display)); 
							ptr = ptr -> prev)
				{
					i += ptr -> items;
				}

				/*---------------------------------------------------------------*/
				/** IF we found a column to move left to THEN                   **/
				/**    add the number of items in it to the number we passed up **/
				/**    subtract this total number of items from the column      **/
				/**       offset that was passed in, to get the new column      **/
				/**       offset.                                               **/
				/**    reset the column_info pointer                            **/
				/**    reset the item count output for the new column           **/
				/**    decrement the column number                              **/
				/** ENDIF                                                       **/
				/*---------------------------------------------------------------*/

				if (ptr != NULL)
				{
					i += ptr -> items;
					*column_offset -= i;
					*column_info = ptr;
				/*  *item_count = (*column_info) -> items; */
					if(tb_show_items == TRUE)
						*item_count = (*column_info) -> items;
					else
						*item_count = 1;

					--(*column_number);
				}

			/*------------------------------------------------------------------*/
			/** ENDIF                                                          **/
			/*------------------------------------------------------------------*/

			}  /*  End:  "if ((*item_count > 1) && ... else ..."  */


		/*----------------------------------------------------------------------*/
		/** ENDLOOP                                                            **/
		/*----------------------------------------------------------------------*/

		}  /*  End:  "for (col_count = 0; ..."  */
	}    
	
	return; 

/** END **/

}  /*  "tb_move_left"  */



/**********************************************************************
 *$Component                                                          *
 *    void tb_move_right(column_info, item_count, column_offset,      *
 *                       col_movement, column_number)                 *
 *$Abstract                                                           *
 *    Moves to the right through columns or bit_columns               * 
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    column_info:                                                    *
 *        The column_info variable is a pointer to the structure      *
 *        which contains keyword information on a column object.      *
 *    item_count:                                                     *
 *        The item_count variable keeps track of the current item in  *
 *        a column object (i.e, the item being displayed).            *
 *    column_offset:                                                  *
 *        The column_offset variable keep track of the "real" offset  *
 *        of the current column being displayed. The "real" offset    *
 *        of a column is the sum of all the column items to the       *
 *        left of it. Column_offset also applies to bit_columns.      *
 *    col_movement:                                                   *
 *        The col_movement variable is an integer representing the    *
 *        number of columns or bit_columns to move to the right or    *
 *        left, including all items.                                  *
 *    column_number:                                                  *
 *        The "column_number" variable is an integer representing     *
 *        the "user" offset of a column. The "user" offset of a       *
 *        column is a count of all the column objects (not including  *
 *        items) to the left of a column.  Column_number also applies *
 *        to bit_columns.                                             *
 *$Outputs                                                            *
 *    column_info:                                                    *
 *        The column_info variable is a pointer to the structure      *
 *        which contains keyword information on a column object.      *
 *    item_count:                                                     *
 *        The item_count variable keeps track of the current item in  *
 *        a column object (i.e, the item being displayed).            *
 *    column_offset:                                                  *
 *        The column_offset variable keep track of the "real" offset  *
 *        of the current column being displayed. The "real" offset    *
 *        of a column is the sum of all the column items to the       *
 *        left of it. Column_offset also applies to bit_columns.      *
 *    col_movement:                                                   *
 *        The col_movement variable is an integer representing the    *
 *        number of columns or bit_columns to move to the right or    *
 *        left, including all items.                                  *
 *    column_number:                                                  *
 *        The "column_number" variable is an integer representing     *
 *        the "user" offset of a column. The "user" offset of a       *
 *        column is a count of all the column objects (not including  *
 *        items) to the left of a column.  Column_number also applies *
 *        to bit_columns.                                             *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    This routine moves right through columns or bit_columns in a    *
 *    label.  It takes into account the ITEMS keyword when counting   *
 *    forwards through a list of columns.  It will skip over columns  *
 *    which have the display flag set to false.  When it determines   *
 *    the new column, it resets the column_info pointer, the item     *
 *    count, the column offset, and column_number for output.         *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Error_Handling                                                     *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   April 9, 1993                                             *
 *$Change_history                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDD   04-09-93   Updated header/comments                        *
 *	  MDC	12-05-02   Added ability to search for a container column *
 **********************************************************************/

void tb_move_right (column_info, item_count, column_offset, col_movement,
                    column_number)

COLUMN_INFO *(*column_info);
long *item_count;
long *column_offset;
long col_movement;
long *column_number;

{
    COLUMN_INFO *ptr = {NULL};        
    long i;
    long col_count;                   

	LOGICAL found_it = FALSE; /* 12-05-02 Added by MDC */

/** BEGIN **/

	/*-------------------------------------------------------------*/
	/* 12-05-02 MDC												   */
	/* If we want to find a particular container....			   */
	/*-------------------------------------------------------------*/
	if(tb_show_container)
	{
		/*--------------------------------------------------------------*/
		/* Check if its within the boundaries first.					*/
		/*--------------------------------------------------------------*/
		if(col_movement > tb_table_info->container_count)
			return;
		
		if( ((*column_info) != NULL) && ((*column_info)->next != NULL) )  
		{
			/*----------------------------------------------------------*/
            /* Keep looping forwards through the column list until we   */
			/* find the container column we are looking for.			*/
			/*----------------------------------------------------------*/
			while( found_it == FALSE )
			{
				/*---------------------------------------------------------------*/
				/** LOOP forwards through the column list (i.e., move to right) **/
				/**    until either we find a column with the display flag set, **/
				/**    or the last column is reached.                           **/
				/**    For each column we pass over because it isn't displayed, **/
				/**        add the number of items to a counter (assuming we    **/
				/**        are displaying data and not labels).                 **/ 
				/** ENDLOOP                                                    **/
				/*---------------------------------------------------------------*/
			
				for (i = (*column_info)->items, ptr = (*column_info)->next;
						((ptr != NULL) && tb_show_items && (! tb_show_label) 
							&& (! ptr->display)); 
							i += ptr->items, ptr = ptr -> next) ;

				/*---------------------------------------------------------------*/
				/** IF we found a column to move right to THEN                  **/
				/**    add the total number of items we passed up to the column **/
				/**       offset that was passed in, to get the new column      **/
				/**       offset.                                               **/
				/**    reset the column_info pointer                            **/
				/**    reset the item count output for the new column to 1      **/
				/**    increment the column number                              **/
				/** ENDIF                                                       **/
				/*---------------------------------------------------------------*/

				if (ptr != NULL)
				{
					*column_offset += i;
					*column_info = ptr;
					*item_count = 1;
					++(*column_number);
				}
				else
					break;
				
				/*---------------------------------------------------------------*/
				/* If we have reached the first column, first repetition, in the */
				/* last container in the column list, then just return the column*/
				/* information back to the calling routine regardless of if this */
				/* is the container we want or not. We can't go any further.     */
				/*---------------------------------------------------------------*/
				if( (((*column_info)->which_container) == tb_table_info->container_count) 
					&& ((*column_info)->rep_num == 1) && ((*column_info)->col_num == 1) )
				{
					return;
				}
                /*---------------------------------------------------------------*/
				/* If we have found a column that is within the container we	 */
				/* want, and this column is the first column in the container    */
				/* in the first repetition, then we have found the right one.    */
				/*---------------------------------------------------------------*/
				if( (*column_info)->which_container == col_movement &&
					((*column_info)->rep_num == 1) && ((*column_info)->col_num == 1) )
				{
					found_it = TRUE;
				}
			
			} /* END: while( (found_it == FALSE)... */
		
		} /* END: if( (*column_info) != NULL... */
	} /* END: if(tb_show_container)... */
	else
	{
		/*----------------------------------------------------------------------*/
		/** LOOP through COL_MOVEMENT columns, starting with the current one   **/
		/*----------------------------------------------------------------------*/

		for (col_count = 0; ((*column_info != NULL) && 
										(col_count < col_movement)); ++col_count)
		{

			/*------------------------------------------------------------------*/
			/** IF the current column has items left and we are displaying     **/
			/**    data as opposed to labels THEN                              **/
			/**    decrement the current item count (move one item to the left)**/
			/*------------------------------------------------------------------*/

			if (tb_show_items && (! tb_show_label) && 
					(*item_count < (*column_info) -> items))
			{
				++*item_count;
			}

			/*------------------------------------------------------------------*/
			/** ELSE                                                           **/
			/*------------------------------------------------------------------*/

			else
			{
			/*---------------------------------------------------------------*/
			/** LOOP forwards through the column list (i.e., move to right) **/
			/**    until either we find a column with the display flag set, **/
			/**    or the last column is reached.                           **/
			/**    For each column we pass over because it isn't displayed, **/
			/**        add the number of items to a counter (assuming we    **/
			/**        are displaying data and not labels).                 **/ 
			/** ENDLOOP                                                    **/
			/*---------------------------------------------------------------*/

				for (i = (*column_info)->items, ptr = (*column_info)->next;
						((ptr != NULL) && tb_show_items && (! tb_show_label) 
							&& (! ptr->display)); 
							i += ptr->items, ptr = ptr -> next) ;

				/*---------------------------------------------------------------*/
				/** IF we found a column to move right to THEN                  **/
				/**    add the total number of items we passed up to the column **/
				/**       offset that was passed in, to get the new column      **/
				/**       offset.                                               **/
				/**    reset the column_info pointer                            **/
				/**    reset the item count output for the new column to 1      **/
				/**    increment the column number                              **/
				/** ENDIF                                                       **/
				/*---------------------------------------------------------------*/

				if (ptr != NULL)
				{
					*column_offset += i;
					*column_info = ptr;
					*item_count = 1;
					++(*column_number);
				}
	
		
			/*------------------------------------------------------------------*/
			/** ENDIF                                                          **/
			/*------------------------------------------------------------------*/

			}  /*  End:  "if ((tb_show_items) && ..."  */
		
		/*----------------------------------------------------------------------*/
		/** ENDLOOP                                                            **/
		/*----------------------------------------------------------------------*/

		}  /*  End:  "for (col_count = 0; ..."  */
	}
    return; 

/** END **/

}  /*  "tb_move_right"  */


/**********************************************************************
 *$Component                                                          *
 *    void tb_navigate (command, command_str)                         *
 *$Abstract                                                           *
 *    Call appropriate routines based on user's input command         *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    command_str:                                                    *
 *        A character string containing information the user enters   *
 *        from the command line prompt.                               *
 *    command:                                                        *
 *        An integer representing the command the user enters from    *
 *        the command line prompt: usually the first character        *
 *        of the command.                                             *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    This routine performs the bulk of the user interface navigate   *
 *    For the table browser.  It examines the command and command     *
 *    string that the user entered and directs the flow of the        *
 *    table browser by setting globals flags which control the        *
 *    actions of other table browser routines.  It also determines    *
 *    which routines to call based on the user's input command.       *
 *    It also handles all navigation up and down through rows, or     *
 *    left and right through tables, columns, bit_columns, and items. *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Error_Handling                                                     *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   April 12, 1993                                            *
 *$Change_history                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDD   04-12-93   Updated header/comments.                       *
 *	  MDC	02-25-03   Updated the TB_CONTAINER case to correctly	  *
 *					   display the container that the user wants to   *
 *					   look at.										  *
 **********************************************************************/

void tb_navigate (command, command_str)

short command;
char *command_str;

{
    static TABLE_INFO *current_table_info = {NULL};
    static COLUMN_INFO *current_column_info = {NULL};
    long number;
	long container_we_want=0;
    LOGICAL show_bit_column = {FALSE};

/** BEGIN **/

    /*---------------------------------------------------------------*/
    /** IF the command string begins with a digit convert it to a   **/
    /**   number.                                                   **/
    /*---------------------------------------------------------------*/

    number = (isdigit(*command_str)) ? Make_Long(command_str) : 1;
    
    /*---------------------------------------------------------------*/
    /** CASE command code OF                                        **/
    /*---------------------------------------------------------------*/

    switch (command)
    {

        /*-----------------------------------------------------------*/
        /** QUIT OR EXIT: Do nothing                                **/
        /*-----------------------------------------------------------*/

        case TB_QUIT   : command = TB_EXIT;
                         break;

        case TB_EXIT   : 
                         break;

        /*-----------------------------------------------------------*/
        /** SUMMARY: Turn off flags for showing a table, a label, or**/
        /**          a file.                                        **/
        /**          Turn on flag for showing a summary.            **/
        /*-----------------------------------------------------------*/

        case TB_SUMMARY: tb_show_table = FALSE;
                         tb_show_label = FALSE;
                         tb_show_file = FALSE;
                         tb_show_summary = TRUE;
                         break;

        /*-----------------------------------------------------------*/
        /** SELECT:  Turn off flags for showing a bit_column,       **/
        /**          column, file, or summary.                      **/
        /**          Turn on flags for showing items, tables, or    **/
        /**          labels.                                        **/
        /**          Call routine to select a label file.           **/
        /*-----------------------------------------------------------*/

        case TB_SELECT : show_bit_column = tb_show_bit_column = FALSE;
                         tb_show_column = FALSE;
                         tb_show_items = TRUE;  
                         tb_show_table = TRUE;
                         tb_show_label = TRUE;
                         tb_show_file = FALSE;
                         tb_show_summary = FALSE;
						 tb_show_container = FALSE; /*12-03-02 Added by MDC */

	                     tb_select_label_file (command_str);

                         break;           

        /*-----------------------------------------------------------*/
        /** HELP:  Call routine to display help.                    **/
        /*-----------------------------------------------------------*/

        case TB_HELP1  : 
        case TB_HELP2  : tb_display_help ();
                         break;

        /*-----------------------------------------------------------*/
        /** BIT_COLUMN: Turn off flags for showing a column, table, **/
        /**             label, file, or summary.                    **/
        /**             Turn on flags for showing items or bit      **/
        /**             columns.                                    **/
        /*-----------------------------------------------------------*/

        case TB_BIT_COL: show_bit_column = tb_show_bit_column = TRUE;
                         tb_show_column = FALSE;
                         tb_show_items = TRUE;
                         tb_show_table = FALSE;
                         tb_show_label = FALSE;
                         tb_show_file = FALSE;
                         tb_show_summary = FALSE;
						 tb_show_container = FALSE;  /*12-03-02 Added by MDC */

                         /*------------------------------------------*/
                         /** IF the command string begins with a    **/
                         /**    number THEN                         **/
                         /**    relocate to the first item in the   **/
                         /**       list of bit_columns in the       **/
                         /**       current column                   **/
                         /**    move forward to the bit_column the  **/
                         /**       user requested                   **/
                         /** ENDIF                                  **/          
                         /*------------------------------------------*/

                         if (isdigit(*command_str))
                         {
                             tb_move_left (&tb_bit_column_info, &tb_bit_item_count,
                                           &tb_bit_column_offset,
                                           tb_bit_column_offset + tb_bit_item_count,
                                           &tb_current_bit_column);
                             tb_move_right (&tb_bit_column_info, &tb_bit_item_count,
                                            &tb_bit_column_offset, number - 1,
                                            &tb_current_bit_column);
                         }
/* TC_MOD_26APR93 */     else if(*command_str != EOS)
/* TC_MOD_26APR93 */         tb_move_by_column_name(command_str,
/* TC_MOD_26APR93 */                        &tb_bit_column_info,
/* TC_MOD_26APR93 */                        &tb_bit_item_count,
/* TC_MOD_26APR93 */                        &tb_bit_column_offset, number,
/* TC_MOD_26APR93 */                        &tb_current_bit_column);
                         break;
		

		case TB_CONTAINER: show_bit_column = tb_show_bit_column = FALSE;
						   tb_show_column = TRUE;
                           tb_show_items = FALSE; /* Changed to FALSE by MDC */
                           tb_show_table = FALSE;
                           tb_show_label = FALSE;
                           tb_show_file = FALSE;
                           tb_show_summary = FALSE;
						   
						   /* Perform checks first to see if we can even display
						      a container. */
						   if(tb_table_info->container_count == 0)
						   {
							   printf("  There are no containers to display \n");
							   /*tb_show_column = FALSE;*/
							   break;
						   }
						   
						   if( (number <= 0) || 
							   (number > (tb_table_info->container_count)) )
						   {
							   tb_show_column = TRUE;
							   tb_show_container = FALSE;
							   break;
						   }
						   
					       if (isdigit(*command_str) || number == 1)
						   {
                               tb_show_container = FALSE;
							   tb_move_left (&tb_column_info, &tb_item_count,
                                           &tb_column_offset, 
										   tb_column_offset + tb_item_count,
                                           &tb_current_column);
						   
							    /*--------------------------------------------------*/
							    /* 02-22-03 MDC			    						*/
							    /* If the current column we are on is the container */
								/* we want, then there is no need to do anything.   */
								/*--------------------------------------------------*/
								if( (tb_column_info->which_container == number) &&
									(tb_column_info->rep_num == 1) && (tb_column_info->col_num == 1) )
								{
									/*   tb_show_items = TRUE;*/
									tb_show_container = FALSE;
									break;
								}

								tb_show_container = TRUE;
								tb_move_right (&tb_column_info, &tb_item_count,
                                            &tb_column_offset, number,
                                            &tb_current_column);
                           }
						   		 
						  /* tb_show_items = TRUE;*/ /* 12-02-03 Added by MDC */	
                           tb_show_container = FALSE; /*12-06-03 Added by MDC */
                           
						   break;
		/*-----------------------------------------------------------*/
        /** COLUMN:     Turn off flags for showing a bit column,    **/
        /**             table, label, file, or summary.             **/
        /**             Turn on flags for showing items or columns  **/
        /*-----------------------------------------------------------*/
		
		case TB_COLUMN : show_bit_column = tb_show_bit_column = FALSE;
                         tb_show_column = TRUE;
                         tb_show_items = FALSE; /* Changed to FALSE by MDC */
                         tb_show_table = FALSE;
                         tb_show_label = FALSE;
                         tb_show_file = FALSE;
                         tb_show_summary = FALSE;
						 tb_show_container = FALSE; /* 12-03-02 Added by MDC */

                         /*------------------------------------------*/
                         /** IF the command string begins with a    **/
                         /**    number THEN                         **/
                         /**    relocate to the first item in the   **/
                         /**       list of columns in the           **/
                         /**       current table                    **/
                         /**    move forward to the column the      **/
                         /**       user requested                   **/
                         /** ENDIF                                  **/          
                         /*------------------------------------------*/

                         if (isdigit(*command_str))
                         {
                             tb_move_left (&tb_column_info, &tb_item_count,
                                           &tb_column_offset,
                                           tb_column_offset + tb_item_count,
                                            &tb_current_column);
                             tb_move_right (&tb_column_info, &tb_item_count,
                                            &tb_column_offset, number - 1,
                                            &tb_current_column);
                         }
/* TC_MOD_26APR93 */     else if(*command_str != EOS)
/* TC_MOD_26APR93 */         tb_move_by_column_name(command_str,
/* TC_MOD_26APR93 */                        &tb_column_info,
/* TC_MOD_26APR93 */                        &tb_item_count,
/* TC_MOD_26APR93 */                        &tb_column_offset, number,
/* TC_MOD_26APR93 */                        &tb_current_column);
						 
						 tb_show_items = TRUE; /* 12-02-03 Added by MDC */		

                         break;

		/*-----------------------------------------------------------*/
        /** LABEL:      Turn off flag for showing a summary.        **/
        /**             Turn on flag for showing labels             **/
        /*-----------------------------------------------------------*/

        case TB_LABEL  : tb_show_label = TRUE;
                         tb_show_summary = FALSE;

                         /*------------------------------------------*/
                         /** IF this was a LABEL for FILE command   **/
                         /**    Turn off flags for showing columns, **/
                         /**       tables, and bit_columns          **/
                         /**    Turn on flag for showing files.     **/
                         /*------------------------------------------*/

                         if (toupper(*command_str) == 'F') 
                         {
                             show_bit_column = tb_show_bit_column = FALSE;
                             tb_show_column = FALSE;
                             tb_show_table = FALSE;
                             tb_show_file = TRUE;
							 tb_show_container = FALSE; /*12-03-02 Added by MDC */
                         }

                         /*------------------------------------------*/
                         /** ELSE                                   **/
                         /*------------------------------------------*/

                         else

                             /*------------------------------------------*/
                             /** IF this was a LABEL for TABLE command  **/
                             /**    Turn off flags for bit columns,     **/
                             /**       columns, and files.              **/
                             /**    Turn on flag for showing tables.    **/
                             /*------------------------------------------*/

                             if (toupper(*command_str) == 'T')
                             {
                                 show_bit_column = tb_show_bit_column = FALSE;
                                 tb_show_column = FALSE;
                                 tb_show_table = TRUE;
                                 tb_show_file = FALSE;
								 tb_show_container = FALSE; /*12-03-02 Added by MDC */
                             }

                             /*------------------------------------------*/
                             /** ELSE                                   **/
                             /*------------------------------------------*/

                             else

                                 /*------------------------------------------*/
                                 /** IF this was a LABEL for COLUMN command **/
                                 /**    Turn off flags for bit columns,     **/
                                 /**       tables, and files.               **/
                                 /**    Turn on flag for showing columns.   **/
                                 /*------------------------------------------*/

                                 if (toupper(*command_str) == 'C')
                                 {
                                     show_bit_column = tb_show_bit_column = FALSE;
                                     tb_show_column = TRUE;
                                     tb_show_table = FALSE;
                                     tb_show_file = FALSE;

									 tb_show_container = FALSE; /*12-03-02 Added by MDC */
                                 }

                                 /*------------------------------------------*/
                                 /** ELSE                                   **/
                                 /*------------------------------------------*/

                                 else
                                     /*------------------------------------------*/
                                     /** IF this was a LABEL for BIT_COLUMN     **/
                                     /**    command                             **/
                                     /**    Turn off flags for showing columns, **/
                                     /**       tables, and files.               **/
                                     /**    Turn on flag for showing bit columns**/
                                     /*------------------------------------------*/

                                     if (toupper(*command_str) == 'B')
                                     {
                                         show_bit_column = tb_show_bit_column = TRUE;
                                         tb_show_column = FALSE;
                                         tb_show_table = FALSE;
                                         tb_show_file = FALSE;
										 tb_show_container = FALSE; /*12-03-02 Added by MDC */										 
                                     }

                         /*------------------------------------------*/
                         /**            ENDIF                       **/
                         /**         ENDIF                          **/
                         /**     ENDIF                              **/
                         /** ENDIF                                  **/
                         /*------------------------------------------*/

                         break;

        /*-----------------------------------------------------------*/
        /** EDIT:       Call label edit routine.                    **/
        /*-----------------------------------------------------------*/

        case TB_EDIT   : tb_edit_label_info (number);
                         break;

        /*-----------------------------------------------------------*/
        /** UP:         Call routine to jump to a row.              **/
	    /**															**/
	    /**  08-15-02  MDC   Added the condition to check if a		**/
	    /**		number was entered before or after the 'up'			**/
	    /**		command. If a number is entered, this tells the		**/
	    /**		program that the user wants to jump that many		**/
	    /**		rows up in the column. This feature was originally  **/
	    /**		stated in the help description of the program, but  **/
		/**		was never actually implemented.						**/
        /*-----------------------------------------------------------*/

        case TB_UP     :/* If a number was entered in the command line, jump that many rows
						up the column. */
						if(isdigit(*command_str))
							 tb_jump_to_row((tb_row_offset + 1) - number);

						 /* ELSE, display the previous 15 rows in the column. */
						 else
                             tb_jump_to_row((tb_row_offset + 1) - tb_display_rows);
                         break;

        /*-----------------------------------------------------------*/
        /** DOWN:       Call routine to jump to a row.              **/
	    /**															**/
	    /**  08-15-02  MDC   Added the condition to check if a		**/
	    /**		number was entered before or after the 'down'		**/
	    /**		command. If a number is entered, this tells the		**/
	    /**		program that the user wants to jump that many		**/
	    /**		rows up in the column. This feature was originally  **/
	    /**		stated in the help description of the program, but  **/
		/**		was never actually implemented.						**/
        /*-----------------------------------------------------------*/

        case TB_DOWN   :/* If a number was entered in the command line, jump that many rows down. */ 
						if(isdigit(*command_str))
						 	tb_jump_to_row((tb_row_offset + 1) + number);

                        /* ELSE, display the next 15 rows down the column. */
						 else 
							tb_jump_to_row((tb_row_offset + 1) + tb_display_rows);
                         break;

        /*-----------------------------------------------------------*/
        /** NEXT:            IF the command is "next item" THEN     **/
        /**                      set flag for showing items         **/
        /*-----------------------------------------------------------*/

        case TB_NEXT   : if (toupper(*command_str) == 'I') 
                             tb_show_items = TRUE;

                         /*------------------------------------------*/
                         /** ELSE                                   **/ 
                         /*------------------------------------------*/

                         else
                             /*-----------------------------------------*/
                             /** IF the command was "next table" THEN  **/
                             /**    Turn off flags for showing bit     **/
                             /**       columns, columns, items, and    **/
                             /**       files                           **/
                             /**    Turn on flags for showing tables   **/
                             /**       and labels                      **/
                             /*-----------------------------------------*/

                             if (toupper(*command_str) == 'T') 
                             {
                                 show_bit_column = tb_show_bit_column = FALSE;
                                 tb_show_column = FALSE;
                                 tb_show_items = FALSE;
                                 tb_show_table = TRUE;
                                 tb_show_label = TRUE;
                                 tb_show_file = FALSE; 
								 tb_show_container = FALSE; /*12-03-02 Added by MDC */
                             }
      
                             /*-----------------------------------------*/
                             /** ELSE                                  **/
                             /*-----------------------------------------*/

                             else

                                /*-----------------------------------------*/
                                /** IF the command was "next column" THEN **/
                                /**    Turn off flags for showing bit     **/
                                /**       columns, items, tables, and     **/
                                /**       files                           **/
                                /**    Turn on flag for showing columns   **/
                                /*-----------------------------------------*/

                                 if (toupper(*command_str) == 'C') 
                                 {
                                     show_bit_column = tb_show_bit_column = FALSE;
                                     tb_show_column = TRUE;
                                     tb_show_items = FALSE;
                                     tb_show_table = FALSE;
                                     tb_show_file = FALSE;
									 tb_show_container = FALSE; /*12-03-02 Added by MDC */
                                 }
      
                                 /*-----------------------------------------*/
                                 /** ELSE                                  **/
                                 /*-----------------------------------------*/

                                 else 

                                    /*-----------------------------------------*/
                                    /** IF the command was "next bit column"  **/
                                    /**    Turn off flags for showing         **/
                                    /**       columns, items, tables, and     **/
                                    /**       files                           **/
                                    /**    Turn on flag for showing bit       **/
                                    /**       columns                         **/
                                    /*-----------------------------------------*/

                                     if (toupper(*command_str) == 'B') 
                                     {
                                         show_bit_column = tb_show_bit_column = TRUE;
                                         tb_show_column = FALSE;
                                         tb_show_items = FALSE;
                                         tb_show_table = FALSE;
                                         tb_show_file = FALSE;
										 tb_show_container = FALSE; /*12-03-02 Added by MDC */
                                     }

									 else

										 if(toupper(*command_str) == 'K')
										 {
											show_bit_column = tb_show_bit_column = FALSE;
											tb_show_column = FALSE;
											tb_show_items = FALSE;
											tb_show_table = FALSE;
											tb_show_file = FALSE;
											tb_show_container = TRUE; 
										 } 

                         /*---------------------------------------------------*/
                         /**            ENDIF                                **/
                         /**         ENDIF                                   **/
                         /**     ENDIF                                       **/
                         /** ENDIF                                           **/
                         /**                                                 **/
                         /** IF flag for showing tables is on THEN           **/
                         /**    Call routine to move to the next table       **/
                         /*---------------------------------------------------*/

                         if (tb_show_table)
                             tb_get_next_table ();

                         /*---------------------------------------------------*/
                         /** ELSE                                            **/
                         /*---------------------------------------------------*/

                         else

                             /*-----------------------------------------------*/
                             /** IF flag for showing bit columns is set THEN **/
                             /**    Call routine to move to the right in the **/
                             /**       list of bit columns. (Move 1 to the   **/
                             /**       right if no number was provided by    **/
                             /**       user, else move the number the user   **/
                             /**       specified in his "next" command.)     **/
                             /*-----------------------------------------------*/
 
                             if (tb_show_bit_column)
                             {
								 tb_move_right (&tb_bit_column_info, 
                                                &tb_bit_item_count,
                                                &tb_bit_column_offset, number,
                                                &tb_current_bit_column);
                             }

                             /*-----------------------------------------------*/
                             /** ELSE                                        **/
                             /*-----------------------------------------------*/

                             else

                                 /*-------------------------------------------*/
                                 /** IF flag for showing columns is on THEN  **/
                                 /**    Call routine to move to the right in **/
                                 /**       the list of columns.              **/
                                 /*-------------------------------------------*/
                         
                                 if (tb_show_column)
                                 {
									 tb_move_right (&tb_column_info, &tb_item_count,
                                                    &tb_column_offset, number,
                                                    &tb_current_column);
                                 }
								 
								 else

                                 /*----------------------------------------------*/
                                 /** 12-05-02 MDC								**/
								 /** IF flag for showing containers is on THEN  **/
                                 /**    Call routine to move to the right in    **/
                                 /**       the list of columns.                 **/
                                 /*----------------------------------------------*/
                         
                                 if (tb_show_container)
                                 {
									 /*----------------------------------------------*/
									 /* Check to even see if there is a container to */
									 /* display.									 */
									 /*----------------------------------------------*/
                                     if(tb_table_info->container_count == 0)
									 {
										printf("\n  There are no containers to display \n");
										tb_show_column = FALSE;
										break;
									 }
									 /* Save the value of the container we want */
									 container_we_want = tb_column_info->which_container+1;
									
									 if(container_we_want > (tb_table_info->container_count))
									 {
									    tb_show_column = TRUE;
										tb_show_container = FALSE;
										break;
									 }
									 /*----------------------------------------------*/
									 /* Because of the architecture of how the tree  */
									 /* is constructed with nested containers,		 */
									 /* we need to re-locate to the 1st container    */
									 /* before moving to the one we want.			 */
									 /*----------------------------------------------*/
									 tb_move_left (&tb_column_info, &tb_item_count,
												   &tb_column_offset, 1,
												   &tb_current_column);

									 tb_move_right (&tb_column_info, &tb_item_count,
                                                    &tb_column_offset, 
													container_we_want,
                                                    &tb_current_column);
									 
									 tb_show_column = TRUE;
									 tb_show_container = FALSE;
                                 }
                     /*-------------------------------------------------------*/
                     /**             ENDIF                                   **/
                     /**         ENDIF                                       **/
                     /**     ENDIF                                           **/
                     /**     Turn on flag for showing items                  **/
                     /*-------------------------------------------------------*/
					 
					 tb_show_items = TRUE;
                     
					 break;

        /*-----------------------------------------------------------*/
        /** PREVIOUS:        IF the command is "prev item" THEN     **/
        /**                      set flag for showing items         **/
        /*-----------------------------------------------------------*/

        case TB_PREV   : if (toupper(*command_str) == 'I') 
                             tb_show_items = TRUE;

                         /*------------------------------------------*/
                         /** ELSE                                   **/ 
                         /*------------------------------------------*/

                         else
                             /*-----------------------------------------*/
                             /** IF the command was "prev table" THEN  **/
                             /**    Turn off flags for showing bit     **/
                             /**       columns, columns, items, and    **/
                             /**       files                           **/
                             /**    Turn on flags for showing tables   **/
                             /**       and labels                      **/
                             /*-----------------------------------------*/

                             if (toupper(*command_str) == 'T') 
                             {
                                 show_bit_column = tb_show_bit_column = FALSE;
                                 tb_show_column = FALSE;
                                 tb_show_items = FALSE;
                                 tb_show_table = TRUE;
                                 tb_show_label = TRUE;
                                 tb_show_file = FALSE;
								 tb_show_container = FALSE; /*12-03-02 Added by MDC */
                             }
      
                             /*-----------------------------------------*/
                             /** ELSE                                  **/
                             /*-----------------------------------------*/

                             else

                                /*-----------------------------------------*/
                                /** IF the command was "prev column" THEN **/
                                /**    Turn off flags for showing bit     **/
                                /**       columns, items, tables, and     **/
                                /**       files                           **/
                                /**    Turn on flag for showing columns   **/
                                /*-----------------------------------------*/

                                 if (toupper(*command_str) == 'C') 
                                 {
                                     show_bit_column = tb_show_bit_column = FALSE;
                                     tb_show_column = TRUE;
                                     tb_show_items = FALSE;
                                     tb_show_table = FALSE;
                                     tb_show_file = FALSE;
									 tb_show_container = FALSE; /*12-03-02 Added by MDC */
                                 }

                                 /*-----------------------------------------*/
                                 /** ELSE                                  **/
                                 /*-----------------------------------------*/

                                 else
                                    /*-----------------------------------------*/
                                    /** IF the command was "prev bit column"  **/
                                    /**    Turn off flags for showing         **/
                                    /**       columns, items, tables, and     **/
                                    /**       files                           **/
                                    /**    Turn on flag for showing bit       **/
                                    /**       columns                         **/
                                    /*-----------------------------------------*/

                                     if (toupper(*command_str) == 'B') 
                                     {
                                         show_bit_column = tb_show_bit_column = TRUE;
                                         tb_show_column = FALSE;
                                         tb_show_items = FALSE;
                                         tb_show_table = FALSE;
                                         tb_show_file = FALSE;
										 tb_show_container = FALSE; /*12-03-02 Added by MDC */
                                     }
									 
									 else
										/*-----------------------------------------*/
										/** IF the command was "prev container"   **/
										/**    Turn off flags for showing         **/
										/**       bit columns, items, tables, and **/
										/**       files                           **/
										/**    Turn on flag for showing bit       **/
										/**       columns and containers          **/
										/*-----------------------------------------*/

										if (toupper(*command_str) == 'K') 
										{
											show_bit_column = tb_show_bit_column = FALSE;
											tb_show_column = FALSE;
											tb_show_items = FALSE;
											tb_show_table = FALSE;
											tb_show_file = FALSE;
											tb_show_container = TRUE; 
										}
                         /*---------------------------------------------------*/
                         /**            ENDIF                                **/
                         /**         ENDIF                                   **/
                         /**     ENDIF                                       **/
                         /** ENDIF                                           **/
                         /**                                                 **/
                         /** IF flag for showing tables is on THEN           **/
                         /**    Call routine to move to the previous table   **/
                         /*---------------------------------------------------*/

                         if (tb_show_table)
                             tb_get_prev_table ();

                         /*---------------------------------------------------*/
                         /** ELSE                                            **/
                         /*---------------------------------------------------*/

                         else

                             /*-----------------------------------------------*/
                             /** IF flag for showing bit columns is set THEN **/
                             /**    Call routine to move to the left in the  **/
                             /**       list of bit columns. (Move 1 to the   **/
                             /**       left if no number was provided by     **/
                             /**       user, else move the number the user   **/
                             /**       specified in his "previous" command.) **/
                             /*-----------------------------------------------*/

                             if (tb_show_bit_column)
                             {
                                 tb_move_left (&tb_bit_column_info, &tb_bit_item_count,
                                                &tb_bit_column_offset, number,
                                                &tb_current_bit_column);
                             }

                             /*-----------------------------------------------*/
                             /** ELSE                                        **/
                             /*-----------------------------------------------*/

                             else

                                 /*-------------------------------------------*/
                                 /** IF flag for showing columns is on THEN  **/
                                 /**    Call routine to move to the left in  **/
                                 /**       the list of columns.              **/
                                 /*-------------------------------------------*/

                                 if (tb_show_column)
                                 {
                                     tb_move_left (&tb_column_info, &tb_item_count, 
                                                   &tb_column_offset, number,
                                                   &tb_current_column);
                                 }
	                             else

                                 /*---------------------------------------------*/
								 /** 12-05-02								   **/
                                 /** IF flag for showing containers is on THEN **/
                                 /**    Call routine to move to the left in    **/
                                 /**       the list of columns.                **/
                                 /*---------------------------------------------*/

                                 if (tb_show_container)
                                 {
									 /*----------------------------------------------*/
									 /* Check to even see if there is a container to */
									 /* display.									 */
									 /*----------------------------------------------*/
                                     if(tb_table_info->container_count == 0)
									 {
										printf("\n  There are no containers to display \n");
										tb_show_column = FALSE;
										tb_show_container = FALSE;
										break;
									 }
                                     
									 tb_move_left (&tb_column_info, &tb_item_count, 
                                                   &tb_column_offset, 
												   tb_column_info->which_container - 1,
                                                   &tb_current_column);
									 
									 tb_show_column = TRUE;
									 tb_show_container = FALSE;
                                 }		
                     /*-------------------------------------------------------*/
                     /**             ENDIF                                   **/
                     /**         ENDIF                                       **/
                     /**     ENDIF                                           **/
                     /**     Turn on flag for showing items                  **/
                     /*-------------------------------------------------------*/

                         tb_show_items = TRUE;   
                         break;

        /*-----------------------------------------------------------*/
        /** JUMP:            Call routine to jump to a row.         **/
        /*-----------------------------------------------------------*/

        case TB_JUMP   : tb_jump_to_row (number);
                         break;
	
        default        : break;

    /*---------------------------------------------------------------*/
    /** ENDCASE                                                     **/
    /*---------------------------------------------------------------*/
		
    }  /*  End:  "switch (command) ..."  */

    /*---------------------------------------------------------------*/
    /** IF the table being viewed has changed since the previous    **/
    /**    call of this routine THEN                                **/
    /**    Save the pointer to the new table for the next call      **/
    /**    Reset globals for row offset, item count, display rows,  **/
    /**       column number and offset, and current column pointer. **/
    /** ENDIF                                                       **/ 
    /*---------------------------------------------------------------*/
	    
	if ((tb_table_info != NULL) && (tb_table_info != current_table_info))
    {
        current_table_info = tb_table_info;
        tb_row_offset = 0;
        tb_item_count = 1;

        tb_display_rows = (tb_table_info->row_count < TB_DISPLAY_ROWS) ?
                                  tb_table_info->row_count : TB_DISPLAY_ROWS;

        tb_column_offset = 0;
        tb_current_column = 1;
        tb_column_info = (tb_table_info == NULL) ? 
                                  NULL : tb_table_info->columns;

    }  /*  End:  "if ((tb_table_info != NULL) && ..."  */

    /*---------------------------------------------------------------*/
    /** IF the column being viewed has changed since the previous   **/
    /**    call of this routine THEN                                **/
    /**    Save the pointer to the new column for the next call     **/
    /**    Reset globals for item count, column number and offset,  **/
    /**       bit column number and offset, and current bit column  **/
    /**       pointer.                                              **/
    /** ENDIF                                                       **/ 
    /*---------------------------------------------------------------*/

    if ((tb_column_info != NULL) && (tb_column_info != current_column_info))
    {
        current_column_info = tb_column_info;
        tb_bit_item_count = 1;
        tb_bit_column_offset = 0;
        tb_current_bit_column = 1;
        tb_bit_column_info = (tb_column_info == NULL) ? 
                                    NULL : tb_column_info->bit_columns;

    }  /*  End:  "if ((tb_column_info != NULL) && ..."  */

    /*---------------------------------------------------------------*/
    /** IF there is no current table or the table has no columns    **/
    /**       Warn the user.                                        **/
    /** ELSE                                                        **/
    /**    IF we are displaying bit columns and there is no current **/
    /**       one THEN                                              **/
    /**       Warn the user.                                        **/
    /**    ELSE                                                     **/
    /**       IF the command was not exit THEN                      **/
    /**          Call routines to either display label info, show   **/
    /**             a column summary, or display column data.       **/
    /*---------------------------------------------------------------*/


	if ((tb_table_info == NULL) || (tb_column_info == NULL))
        printf ("\n  There aren't any columns to display\n");
    else
        if ((tb_bit_column_info == NULL) && show_bit_column)
            printf ("\n  There aren't any bit columns to display\n");
        else
            if (command != TB_EXIT)
            {                           
                if (tb_show_label) 
                    tb_display_label ();
                else 
                    if (tb_show_summary)
                        ts_summarize_column ();
                    else
                        if (tb_show_column || tb_show_bit_column)
                            tb_display_column ();
            }

    /*---------------------------------------------------------------*/
    /**       ENDIF                                                 **/
    /**    ENDIF                                                    **/
    /** ENDIF                                                       **/
    /*---------------------------------------------------------------*/

    return;

/** END **/

}  /*  "tb_navigate"  */



/**********************************************************************
 *$Component                                                          *
 *    unsigned char *tb_read_column (column_info, data_file,          *
 *                                   row_offset, size)                *
 *$Abstract                                                           *
 *    Reads a column value from a data file                           *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    column_info:                                                    *
 *        The column_info variable is a pointer to the structure      *
 *        which contains keyword information on a column object.      *
 *    data_file:                                                      *
 *        The data_file variable is a pointer to an open data file.   *
 *    row_offset:                                                     *
 *        The row_offset is the byte offset of the current row of     *
 *        a table in a data file.                                     *
 *    size:                                                           *
 *        The size variable is the size, in bytes, of a column value. *
 *$Outputs                                                            *
 *    size:                                                           *
 *        The size variable is the size, in bytes, of a column value. *
 *$Returns                                                            *
 *    buffer:                                                         *
 *        The buffer variable is a character string containing a      *
 *        column value.                                               *
 *$Detailed_Description                                               *
 *    This routine seeks to the appropriate column at the given row   *
 *    offset in the given data file, attempts to read size bytes,     *
 *    updates size to the number of bytes actually read, and then     *
 *    returns the buffer containing the bytes that were read. This    *
 *    routine also accounts for UNSPANNED blocks (i.e., cases where   *
 *    data records are not allowed to span block boundaries) by       * 
 *    accounting for the padding at the end of each block.            *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Error_Handling                                                     *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   April 12, 1993                                            *
 *$Change_history                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDD   04-12-93   Updated header/comments.                       *
 **********************************************************************/
 
unsigned char *tb_read_column (column_info, data_file, row_offset, size)

COLUMN_INFO *column_info;  
FILE *data_file;
long row_offset;
long *size;

{                                     
    unsigned char *buffer = {NULL};
	/* 10-17-02 MDC    Initialized the rest of the variables. */
    long row_bytes = 0;
    long start = 0;
    long seek_to = 0;
    long block_position = 0;
    long block_data_bytes = 0;
    long block_pad_bytes = 0;

/** BEGIN **/

    /*--------------------------------------------------------------------*/
    /** Find the start byte of the data within the row, not counting     **/
    /**    any prefix bytes                                              **/
    /*--------------------------------------------------------------------*/

    start = (tb_table_info->data_location - 1) + 
              column_info->start + 
                (column_info->item_offset * (tb_item_count - 1)) - 1;

    /*--------------------------------------------------------------------*/
    /** Calculate the total size of a row, including suffix and prefix   **/
    /*--------------------------------------------------------------------*/

    row_bytes = tb_table_info->row_bytes + 
                    tb_table_info->prefix_bytes + tb_table_info->suffix_bytes;

    /*--------------------------------------------------------------------*/
    /**  Calculate byte to seek to in the data file.                     **/
    /**  Seek to =   (total bytes in all previous rows) +                **/
    /**                       (prefix bytes in this row) +               **/
    /**                      (column start byte within row)              **/
    /*--------------------------------------------------------------------*/

    seek_to = (row_offset * row_bytes) + tb_table_info->prefix_bytes + start;

    /*--------------------------------------------------------------------*/
    /**  IF the data file has unspanned blocking THEN                    **/
    /**     Adjust seek bytes based on block records and block size      **/
    /**  ENDIF                                                           **/
    /*--------------------------------------------------------------------*/

    if (strcmp (tb_blocking_type, "UNSPANNED") == 0)
    {
        block_data_bytes = tb_block_records * tb_record_bytes;
	block_pad_bytes = tb_block_bytes - block_data_bytes;
        block_position = (long) (seek_to / block_data_bytes);
        seek_to = seek_to + (block_position * block_pad_bytes);
    }

    /*--------------------------------------------------------------------*/
    /**  Allocate memory for the data buffer.                            **/
    /**  Seek to proper location in file.                                **/
    /**  Read data, reset size to actual bytes read.                     **/
    /**  RETURN the data buffer.                                         **/
    /*--------------------------------------------------------------------*/

    U_Malloc_String(buffer, (int) row_bytes + 1)

    fseek (data_file, seek_to, 0);
    *size = fread (buffer, 1, *size, data_file);

    *(buffer + *size) = EOS;

    return (buffer);

/** END **/

}  /*  "tb_read_column"  */


/**********************************************************************
 *$Component                                                          *
 *   void tb_replace_from_list (text_field, list, list_end, prompt)   *
 *$Abstract                                                           *
 *   Prints a list of options and reads the user's choice.            *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    text_field:                                                     *
 *       The text_field variable is a character string containing the *
 *       textual equivalent of an item on an options list.            *
 *    list:                                                           *
 *       The list variable is an array of character strings which     *
 *       contains a list of menu options.                             *
 *    list_end:                                                       *
 *       The list_end variable is an integer representing the length  *
 *       of a list of character strings.                              *
 *    prompt:                                                         *
 *       The prompt variable is a character string used to prompt a   *
 *       user for input.                                              *
 *$Outputs                                                            *
 *    text_field:                                                     *
 *       The text_field variable is a character string containing the *
 *       textual equivalent of an item on an options list.            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    This routine prompts the user by displaying a numbered list     *
 *    of options. (The list itself is input to this routine.) A       *
 *    number is read from the command line, and the corresponding     *
 *    option string is output by this routine. For example, if the    *
 *    list of options displayed is:                                   *
 *                   1)  ASCII                                        *
 *                   2)  BINARY                                       *
 *    and the user enters a 2, text_field will be set to the string   *
 *    "BINARY".                                                       *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Error_Handling                                                     *
 *    If the user enters an invalid number, this routine does nothing.*
 *    Text_field will be unchanged.                                   *
 *$Side_Effects                                                       *
 *    The previous contents of text_fields are destroyed, and the     *
 *    associated memory is realloc'd.                                 *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   April 12, 1993                                            *
 *$Change_History                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDD   04-12-93   Updated header/comments                        *
 **********************************************************************/

void tb_replace_from_list (text_field, list, list_end, prompt)

char *(*text_field);
char *list[];
long list_end;
char *prompt;

{                                     
    char str [PDS_MAXLINE];
    long i;
    long index;

/** BEGIN **/

    /*----------------------------------------------------------------*/
    /** Print the user prompt.                                       **/
    /** LOOP through the input list and print all the items,         **/
    /**    numbering them in the process.                            **/
    /*----------------------------------------------------------------*/

    printf ("  %s :\n\n", prompt);

    for (i = 0; i <= list_end; i += 2)
    {
	printf (" %5ld) %-30s", i+1, list[i]);
        if ((i + 1) <= list_end)      
	    printf (" %5ld) %-30s\n", i+2,list[i+1]);
    }

    /*----------------------------------------------------------------*/
    /** Read the user's input (a number)                             **/
    /** Use the number as an index into the option list, and copy    **/
    /**    the option chosen into the output string.                 **/
    /*----------------------------------------------------------------*/

    printf ("\n  : ");
    gets(str);

    if (str[0] != EOS)
    {
        index = Make_Long(str) - 1;
        if ((index >= 0) && (index <= list_end)) 
	    Replace_String(*text_field, list[index])
    }

    return;

/** END **/

}  /*  "tb_replace_from_list"  */


/**********************************************************************
 *$Component                                                          *
 *    void tb_select_label_file (fname)                               *
 *$Abstract                                                           *
 *    Prompts for and reads a new label file.                         *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    fname:                                                          *
 *        The fname variable is a character string containing the     *
 *        name of a file.                                             *
 *$Outputs                                                            *
 *    fname:                                                          *
 *        The fname variable is a character string containing the     *
 *        name of a file.                                             *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    This routine prompts the user for a label file name (unless     *
 *    a file name is passed in.)  It then attempts to open the label  *
 *    file and parse it.  Finally, it calls routines to extract       *
 *    information from the label structure: file information and      *
 *    table information. If the label file has syntax errors, then    *
 *    the user is given the option of displaying them.                *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *   tbtool globals                                                   *
 *$Error_Handling                                                     *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   April 12, 1993                                            *
 *$Change_History                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDD   04-12-93   Updated header/comments.                       *
 *	  MDC   08-01-02   Initialized short variable, c, to 0 instead of *
 *					   NULL to clean up warning msg.				  *
 **********************************************************************/

void tb_select_label_file (fname)

char *fname;

{
	 FILE *f_ptr = {NULL};
    /* short c = {NULL}; */
	short c = 0;

/** BEGIN **/

    /*-------------------------------------------------------------------*/
    /** IF the file name wasn't already provided THEN prompt the user   **/
    /**    for it.                                                      **/
    /*-------------------------------------------------------------------*/

    printf ("\n");
    if (*fname == EOS)
    {
        printf ("  Please enter the file name: ");
        *fname = EOS;
        gets (fname);
    }
    printf ("\n");

    /*-------------------------------------------------------------------*/
    /** ENDIF                                                           **/
    /** IF the label file name still wasn't provided THEN               **/
    /**    warn the user                                                **/
    /*-------------------------------------------------------------------*/

    if (*fname == EOS)
        printf ("  You must specify a label file to process.\n");

    /*-------------------------------------------------------------------*/
    /** ELSE                                                            **/
    /*-------------------------------------------------------------------*/

    else
    {
        /*---------------------------------------------------------------*/
        /** IF the file can't be opened THEN                            **/
        /**    Warn the user                                            **/
        /*---------------------------------------------------------------*/

        if ((f_ptr = fopen(fname, "r")) == NULL)
            printf ("  Unable to open the file: %s\n", fname);

        /*---------------------------------------------------------------*/
        /** ELSE                                                        **/
        /*---------------------------------------------------------------*/

        else
        {
            /*---------------------------------------------------------------*/
            /** Close the file now that we know it exists.                  **/
            /** Cleanup after the previous file.                            **/
            /** Parse the label file, including ^STRUCTURE pointers.        **/
            /*---------------------------------------------------------------*/
			
			fclose (f_ptr);

            printf ("  Parsing the label file: %s\n", fname);
            tb_cleanup();

		
            tb_label_ptr = lt_read_expanded_label (fname);


            /*---------------------------------------------------------------*/
            /** IF there were syntax errors THEN                            **/
            /**    Ask the user if he wants to see them.                    **/
            /**    Display them if he does.                                 **/
            /**    Repeat the process as many times as the user wants.      **/
            /*---------------------------------------------------------------*/

            if (pds_message_list != NULL)
            {
                printf ("\n  There were problems parsing the label file.\n");
                printf (
                   "  Would you like to see the ODL syntax errors?  (Enter Y or N) ");
                Read_Char(c)
    
                while (toupper(c) == 'Y')
                {
                    printf ("\n\n");
                    lab_print_messages ();
                    printf ("\n  Would you like to see the messages again?  (Enter Y or N) ");
                    Read_Char(c)
                }
     
                lab_clear_messages ();
    
            }  /*  End:  "if (pds_message_list != NULL) ..."  */
    
            /*---------------------------------------------------------------*/
            /** ENDIF                                                       **/
            /** IF the label was parsed THEN                                **/
            /**    Call routines to extract file and table information from **/
            /**       the label.                                            **/
            /**    IF this is VMS THEN call a routine to change the file    **/
            /**       format.                                               **/
            /*---------------------------------------------------------------*/

			if (tb_label_ptr != NULL)
            {
                printf ("  Fetching the label information\n");
                tb_get_file_info (tb_label_ptr);
                tb_table_list = tb_get_table_info (tb_label_ptr, fname);
                tb_table_info = tb_table_list;
#ifdef VAX  
                if (tb_table_info != NULL)
                    tb_convert_data_file (&(tb_table_info->fname));
#endif
    
            /*---------------------------------------------------------------*/
            /** ENDIF                                                       **/
            /*---------------------------------------------------------------*/

            }  /*  End:  "if (tb_label_ptr != NULL) ..."  */

        /*---------------------------------------------------------------*/
        /** ENDIF                                                       **/
        /*---------------------------------------------------------------*/

        }  /*  End:  if ((f_ptr = fopen( ... else ..."  */

    /*-------------------------------------------------------------------*/
    /** ENDIF                                                           **/
    /*-------------------------------------------------------------------*/

    }  /*  End:  "if (*fname == EOS) ... else ..."  */

    return;

/** END **/

}  /*  "tb_select_label_file"  */



/**********************************************************************
 *$Component                                                          *
 *    void tbsetup ()                                                 *   
 *$Abstract                                                           *
 *    Sets up variables and environment for the Table Browser.        *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    None                                                            *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    This routine sets up the global variables that support the      *
 *    display of option lists in the Table Browser user interface     *
 *    by determining the length of all the option lists.              *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    Tbtool globals                                                  *
 *$Error_Handling                                                     *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    David P. Bernath / J.P.L.                                       *
 *$Version_and_Date                                                   *
 *    1.1   April 12, 1993                                            *
 *$Change_History                                                     *
 *    DPB   01-10-92   Original code.                                 *
 *    MDD   04-12-93   Updated header/comments.                       *
 **********************************************************************/

void tb_setup ()

{
    for (tb_format_end = 0; 
	   tb_format_list[tb_format_end] != NULL; ++tb_format_end) ;
    --tb_format_end;   

    for (tb_bit_type_end = 0; 
	     tb_bit_type_list[tb_bit_type_end] != NULL; ++tb_bit_type_end) ;
    --tb_bit_type_end;

    for (tb_type_end = 0; 
	     tb_type_list[tb_type_end] != NULL; ++tb_type_end) ;
    --tb_type_end;

    for (tb_display_end = 0; 
	     tb_display_list[tb_display_end] != NULL; ++tb_display_end) ;
    --tb_display_end;

    for (tb_record_type_end = 0; 
	     tb_record_type_list[tb_record_type_end] != NULL; ++tb_record_type_end) ;
    --tb_record_type_end;

    for (tb_blocking_type_end = 0; 
	     tb_blocking_type_list[tb_blocking_type_end] != NULL; ++tb_blocking_type_end) ;
    --tb_blocking_type_end;

    return;

}  /*  "tb_setup"  */



/**********************************************************************
 *$Component                                                          *
 *    void ts_summarize_column ()                                     *
 *$Abstract                                                           *
 *    Summarizes a column of data                                     *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *$Inputs                                                             *
 *    None                                                            *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    This routine summarizes the data in a column or bit column of   *
 *    a PDS table by scanning all the data in the column and          *
 *    collecting statistics, based on the data type of the column.    *
 *    For numeric columns, min, max, and average are computed. For    *
 *    character fields, a table of occurence counts is created. For   *
 *    dates and times, min and max are determined, and an occurence   *
 *    table is kept for ``suspicious'' dates and times.               *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/ JPL                                            *
 *$Version_and_Date                                                   *
 *    1.0   July 6, 1992                                              *
 *$Change_History                                                     *
 *    MDD   07-06-92   Original code.                                 *
 **********************************************************************/

void ts_summarize_column ()
{

    COLUMN_INFO *column_info = {NULL};
    FILE *data_file = {NULL};
    unsigned char *column_value = {NULL};
    unsigned char *unformatted_value = {NULL};
    char temp_str [PDS_MAXLINE];
    char temp_str2 [PDS_MAXLINE];
    long i;
    long size = {tb_column_info->size};
    LOGICAL everything_is_ok = {FALSE};
    LOGICAL first_val = {TRUE};
    double min_double;
    double max_double;
    double temp_double = {0.0};
    long min_long;
    long max_long;
    long temp_long = {0};
    unsigned long min_unsign;
    unsigned long max_unsign;
    unsigned long temp_unsign = {0};
    double average = {0.0};
    double prev_average = {0.0};
    int summary_type = {TS_UNKNOWN};
    LOGICAL overflow_flag = {FALSE};
    LOGICAL underflow_flag = {FALSE};
    LOGICAL error_flag = {FALSE};
    LOGICAL occurrence_flag = {FALSE};
    OCCURRENCE_ENTRY *count_root = {NULL};
    char prompt = {'Y'};
    char min_date [PDS_MAXLINE + 1];
    char max_date [PDS_MAXLINE + 1];

/** BEGIN **/

    printf ("\n");

    /*-----------------------------------------------------------------------*/
    /** Check the label information needed to summarize a column.           **/
    /*-----------------------------------------------------------------------*/

    if (! tb_table_info)
       printf ("   There is no current table\n");
    else
    if (tb_table_info -> fname == NULL)
        printf ("  The name of the data file is invalid or missing\n");
    else
    if ((data_file = fopen (tb_table_info -> fname, "rb")) == NULL)
        printf ("  Unable to open the data file\n");
    else
    if (tb_table_info -> row_bytes == TB_UNKNOWN)
        printf ("  The table's ROW_BYTES field is invalid or missing\n");
    else
    if (! tb_column_info)
       printf ("   There is no current column\n");
    else
    if (tb_column_info -> size == TB_UNKNOWN)
        printf ("  The column's BYTES field is invalid or missing\n");
    else    
    if (tb_column_info -> start == TB_UNKNOWN)
        printf ("  The column's START_BYTES field is invalid or missing\n");
    else
    if (! tb_show_bit_column)
        everything_is_ok = TRUE;
    else
    if (! tb_bit_column_info)
        printf ("  There is no current bit column\n");
    else
    if (tb_bit_column_info -> size == TB_UNKNOWN)
        printf ("  The bit column's BITS field is invalid or missing\n");
    else    
    if (tb_bit_column_info -> start == TB_UNKNOWN)
        printf ("  The bit column's START_BIT field is invalid or missing\n");
    else
        everything_is_ok = TRUE;

    /*-----------------------------------------------------------------------*/
    /** IF anything was missing THEN                                        **/
    /**     Print an error message                                          **/
    /** ELSE                                                                **/
    /*-----------------------------------------------------------------------*/

    if (! everything_is_ok)
    {
        printf ("\n  The ");
        if (tb_show_bit_column) printf ("bit ");
        printf ("column cannot be displayed.\n");
    }
    else
    {
        /*-------------------------------------------------------------------*/
        /** Decide whether to summarize a BIT_COLUMN or COLUMN              **/
        /*-------------------------------------------------------------------*/

        column_info = (tb_show_bit_column) ? tb_bit_column_info : tb_column_info;

        /*-------------------------------------------------------------------*/
        /** Format the summary information at the top of the screen         **/
        /*-------------------------------------------------------------------*/

        printf ("------------------------------------------------------------------------------\n");
	sprintf (temp_str, "  %s %s", column_info->name, column_info->class);
	sprintf (temp_str2, "  (%s-%ld)", column_info->data_type, column_info->size);
        strcat (temp_str, temp_str2);
        printf ("%s\n", temp_str); 

        (column_info->bit_columns == NULL) ? 
            printf ("\n") :
            printf ("%79s\n", "(Has Bit Columns)");

        /*-------------------------------------------------------------------*/
        /** determine what type of summary operation to perform             **/
        /** intialize summary globals for line and occurence counts         **/
        /*-------------------------------------------------------------------*/

        summary_type = ts_map_type (column_info);
        ts_line_count = 0;
        ts_occurrence_count = 0;

        /*-------------------------------------------------------------------*/
        /** LOOP through the rows of data to be summarized                  **/
        /*-------------------------------------------------------------------*/

        for (i = 0;
              ((i  < tb_table_info -> row_count) &&
                 (! ferror(data_file)) && (! feof (data_file))); ++i)
        {
            /*---------------------------------------------------------------*/
            /** Read a column value from the data file                      **/
            /*---------------------------------------------------------------*/

	    unformatted_value = tb_read_column (tb_column_info, data_file, i, &size);

            /*---------------------------------------------------------------*/
            /** Turn the string of bytes that were read from the file into  **/
            /**     a value that can be displayed                           **/
            /*---------------------------------------------------------------*/

            if (tb_show_bit_column)
                column_value = tb_format_bit_column_value (tb_bit_column_info, 
                                            unformatted_value, 
                                            &size, tb_column_info->data_type); 
            else
		column_value = tb_format_column_value (tb_column_info, 
                                            unformatted_value, size);

            /*---------------------------------------------------------------*/
            /** CASE summary type OF                                        **/
            /*---------------------------------------------------------------*/

            switch (summary_type)
            {
               /*------------------------------------------------------------*/
               /** REAL data type:                                          **/
               /**    check to see if column value contains invalid chars   **/
               /**    convert column value to a real value                  **/
               /**    initialize min and max if this is the first value     **/
               /**    recompute min, max, and average                       **/
               /**    check for underflow and overflow of average           **/
               /*------------------------------------------------------------*/

	       case TS_REAL: error_flag = error_flag || 
                                             (strspn ((char *) column_value,
                                                 " +-eE.0123456789") !=
                                                    strlen ((char *) column_value));
                             sscanf ((char *)column_value, " %lf", &temp_double);

                             if (first_val)
                             {
                                first_val = FALSE;
                                min_double = temp_double;
                                max_double = temp_double;
                             }

                             if (temp_double < min_double) min_double = temp_double;
                             if (temp_double > max_double) max_double = temp_double;
                             average += temp_double /
                                           (double) tb_table_info -> row_count;

                             if (temp_double != 0.0 && prev_average == average)
                                underflow_flag = TRUE;
                             if (average < prev_average && temp_double > 0.0 ||
                                     average > prev_average && temp_double < 0.0)
                                overflow_flag = TRUE;
                             prev_average = average;

                             break;

               /*------------------------------------------------------------*/
               /** INTEGER data type:                                       **/
               /**    check to see if column value contains invalid chars   **/
               /**    convert column value to an integer value              **/
               /**    initialize min and max if this is the first value     **/
               /**    recompute min, max, and average                       **/
               /**    check for underflow and overflow of average           **/
               /*------------------------------------------------------------*/

               case TS_INTEGER: error_flag = error_flag ||
                                                (strspn ((char *) column_value,
                                                   " -+0123456789") !=
                                                      strlen ((char *) column_value));
                                sscanf ((char *)column_value, " %ld", &temp_long);

                                if (first_val)
                                {
                                   first_val = FALSE;
                                   min_long = temp_long;
                                   max_long = temp_long;
                                }

                                if (temp_long < min_long) min_long = temp_long;
                                if (temp_long > max_long) max_long = temp_long;
                                average += (double) temp_long /
                                               (double) tb_table_info -> row_count;

                                if (temp_long != 0 && prev_average == average)
                                   underflow_flag = TRUE;
                                if (average < prev_average && temp_long > 0 ||
                                       average > prev_average && temp_long < 0)
                                   overflow_flag = TRUE;
                                prev_average = average;

                                break;

               /*------------------------------------------------------------*/
               /** UNSIGNED data type:                                      **/
               /**    check to see if column value contains invalid chars   **/
               /**    convert column value to an unsigned integer value     **/
               /**    initialize min and max if this is the first value     **/
               /**    recompute min, max, and average                       **/
               /**    check for underflow and overflow of average           **/
               /*------------------------------------------------------------*/

               case TS_UNSIGNED: error_flag = error_flag ||
                                                (strspn ((char *) column_value,
                                                   " 0123456789") !=
                                                      strlen ((char *) column_value));
                                 sscanf ((char *)column_value, " %lu", &temp_unsign);

                                 if (first_val)
                                 {
                                    first_val = FALSE;
                                    min_unsign = temp_unsign;
                                    max_unsign = temp_unsign;
                                 }

                                 if (temp_unsign < min_unsign) 
                                    min_unsign = temp_unsign;
                                 if (temp_unsign > max_unsign)
                                    max_unsign = temp_unsign;
                                 average += (double) temp_unsign /
                                     (double) tb_table_info -> row_count;

                                 if (temp_unsign != 0 && prev_average == average)
                                     underflow_flag = TRUE;
                                 if (average < prev_average && temp_unsign > 0)
                                    overflow_flag = TRUE;
                                 prev_average = average;

                                 break;
                
               /*------------------------------------------------------------*/
               /** DATE or TIME data type:                                  **/
               /**    IF this really appears to be a date THEN              **/
               /**       initialize min and max if this is the first value  **/
               /**       recompute min and max                              **/
               /**    ELSE                                                  **/
               /**       add value to occurrence table, if it's not too big **/
               /**    ENDIF                                                 **/
               /*------------------------------------------------------------*/

               case TS_DATETIME: if (isdigit ((int) *((char *) column_value)))
	                         {
                                    if (first_val)
                                    {
                                       first_val = FALSE;
                                       strcpy (min_date, (char *) column_value);
                                       strcpy (max_date, (char *) column_value);
			            }
                                    if (strcmp ((char *) column_value, min_date) < 0)
                                       strcpy (min_date, (char *) column_value);
                                    if (strcmp ((char *) column_value, max_date) > 0)
                                       strcpy (max_date, (char *) column_value);
                                 }
                                 else if (ts_occurrence_count < TS_MAX_OCCURRENCES)
                                    count_root = ts_insert_occurrence (count_root, 
                                                    (char *) column_value);
                                 else
                                    occurrence_flag = TRUE;
                                 break;

               /*------------------------------------------------------------*/
               /** CHARACTER data type:                                     **/
               /**    Add value to occurence table, if it's not too big     **/
               /*------------------------------------------------------------*/

               case TS_CHARACTER: if (ts_occurrence_count < TS_MAX_OCCURRENCES)
                                     count_root = ts_insert_occurrence (count_root,
                                                     (char *) column_value);
                                  else
                                     occurrence_flag = TRUE;
                                  break;
            }

            /*---------------------------------------------------------------*/
            /** ENDCASE                                                     **/
            /*---------------------------------------------------------------*/

            Lemme_Go(column_value)
            Lemme_Go(unformatted_value)

        }  /*  End:  "for (i = 0; ..."  */

        /*-------------------------------------------------------------------*/
        /** ENDLOOP                                                         **/
        /** check all error flags and display appropriate warnings: too many**/
        /**    occurrences in table, overflow, underflow, invalid chars     **/
        /*-------------------------------------------------------------------*/

        if (occurrence_flag)
        {
           printf ("\n     WARNING: Occurrence table exceeded maximum size.");
           printf (
              "\n              All values will not be counted in the table.\n\n");
        }
        if (overflow_flag)
           printf (
              "\n     WARNING: Overflow occurred during computation of average.\n");
	if (underflow_flag)
           printf (
              "\n     WARNING: Underflow occurred during computation of average.\n");
        if (error_flag)
           printf (
              "\n     WARNING: Numeric column contains suspicious characters.\n");

        if (error_flag || underflow_flag || overflow_flag)
           printf ("     WARNING: Computed values may be incorrect.\n\n");

        /*-------------------------------------------------------------------*/
        /** CASE summary type OF                                            **/
        /*-------------------------------------------------------------------*/

        switch (summary_type)
        {
           /*----------------------------------------------------------------*/
           /** REAL summary type:                                           **/
           /**    print min, max, and average                               **/
           /*----------------------------------------------------------------*/

           case TS_REAL: printf ("     Minimum:             %e\n", min_double);
                         printf ("     Maximum:             %e\n", max_double);
                         printf ("     Approximate Average: %e\n", average);
                         break;

           /*----------------------------------------------------------------*/
           /** UNSIGNED summary type:                                       **/
           /**    print min, max, and average                               **/
           /*----------------------------------------------------------------*/

           case TS_UNSIGNED: printf ("     Minimum:             %lu\n", min_unsign);
                             printf ("     Maximum:             %lu\n", max_unsign);
                             printf ("     Approximate Average: %.6f\n", average);
                             break;

           /*----------------------------------------------------------------*/
           /** INTEGER summary type:                                        **/
           /**    print min, max, and average                               **/
           /*----------------------------------------------------------------*/

           case TS_INTEGER: printf ("     Minimum:             %ld\n", min_long);
                            printf ("     Maximum:             %ld\n", max_long);
                            printf ("     Approximate Average: %.6f\n", average);
                            break;

           /*----------------------------------------------------------------*/
           /** DATE or TIME summary type:                                   **/
           /**    print min and max, then fall into CHARACTER case          **/
           /*----------------------------------------------------------------*/
                             
           case TS_DATETIME: printf ("     Minimum: %s\n", min_date);
                             printf ("     Maximum: %s\n", max_date);
                       
           /*----------------------------------------------------------------*/
           /** CHARACTER summary type:                                      **/
           /**    IF the occurrence table is more than a screenful THEN     **/
           /**       ask the user if he wants to see it                     **/
           /**    ENDIF                                                     **/
           /**    IF there is a table and the user wants to see it THEN     **/
           /**       display the table                                      **/
           /**    ENDIF                                                     **/
           /*----------------------------------------------------------------*/

           case TS_CHARACTER: if (ts_occurrence_count > TB_DISPLAY_ROWS)
	                      {
                                 printf ("\n  Occurrence table is large (%ld rows).",
                                         ts_occurrence_count);
                                 printf ("  Do you want to see it (Y/N)? ");
                                 Read_Char(prompt);
                                 prompt = toupper ((int)prompt);
                              }
                              if (prompt == 'Y' && ts_occurrence_count != 0)
                              {
                                 if (tb_show_bit_column)
                                    printf ("\n     Bit ");
                                 else
                                    printf ("\n     ");
                                 printf ("Column contains:\n\n");
                                 ts_print_occurrence (count_root);
                              }
                              break;

           /*----------------------------------------------------------------*/
           /** OTHER:                                                       **/
           /**    display message stating there is no summary               **/
           /*----------------------------------------------------------------*/

           default:  printf ("     Unable to display a summary for this data type and/or column size.\n");
                     break;
        }
        /*-------------------------------------------------------------------*/
        /** ENDCASE                                                         **/
        /** Format the display information at the bottom of the screen      **/
        /*-------------------------------------------------------------------*/

        printf ("\n");
        temp_str[0] = EOS;

        if (tb_show_bit_column) 
        {
	    sprintf (temp_str2, "(Bit Column %ld of %ld)  (Item %ld of %ld)  ",
                tb_current_bit_column, tb_column_info -> bit_column_count,
                tb_bit_item_count, column_info -> items);
        }
        else
        {
	    sprintf (temp_str2, "(Column %ld of %ld)  (Item %ld of %ld)  ",
                tb_current_column, tb_table_info -> column_count,
                tb_item_count, column_info -> items);
        }
        strcat (temp_str, temp_str2);
	printf ("%79s", temp_str);

    }  
    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /** close the data file and destroy the occurrence tree                 **/
    /*-----------------------------------------------------------------------*/

    Close_Me(data_file);
    ts_cleanup_occurrence (count_root);

    return;

/** END **/

}  /*  "ts_summarize_column"  */



/**********************************************************************
 *$Component                                                          *
 *    void ts_map_type (column_info)                                  *
 *$Abstract                                                           *
 *    Determines which type of summary should be used for a column    *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *$Inputs                                                             *
 *    column_info:                                                    *
 *        The column_info variable is a pointer to the structure      *
 *        which contains keyword information on a column object.      *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    summary_type:                                                   *
 *       The summary type variable indicates the type of summary that *
 *       is to be used for the data in a column. Posisble values are  *
 *       TS_INTEGER, TS_REAL, TS_UNSIGNED, TS_CHARACTER, TS_DATETIME, *
 *       and etc.                                                     *
 *$Detailed_Description                                               *
 *    This routine maps the data type of a column or bit column to    *
 *    one of the summary types used when generating column summary    *
 *    information. If the data type of the column is not a supported  *
 *    type, or if the number of the bytes in the column is too large  *
 *    to summarize, then this routine returns TS_UNKNOWN.             *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/ JPL                                            *
 *$Version_and_Date                                                   *
 *    1.0   July 6, 1992                                              *
 *$Change_History                                                     *
 *    MDD   07-06-92   Original code.                                 *
 **********************************************************************/

int ts_map_type (column_info)

COLUMN_INFO *column_info;

{
    int summary_type = TS_UNKNOWN;
    long data_type = -1;
    long bit_data_type = -1;
    long interchange_format;
    long size;

/** BEGIN **/
 
    /*-----------------------------------------------------------------------*/
    /** get the interchange format of this table                            **/
    /** record the size of this data column                                 **/
    /*-----------------------------------------------------------------------*/

    interchange_format = search_string_array (tb_format_list, tb_format_end, 
                                              tb_table_info -> interchange_format);
    size = column_info -> size;
 
    /*-----------------------------------------------------------------------*/
    /** IF this column is a normal column THEN                              **/
    /**    get its data type                                                **/
    /**    set the summary type based on its data type                      **/
    /*-----------------------------------------------------------------------*/

    if (tb_show_column)
    {
       data_type = search_string_array (tb_type_list, tb_type_end, 
                                        column_info -> data_type);
       switch (data_type)
       {
            case TB_ASCII_REAL:
            case TB_FLOAT:                
            case TB_IEEE_REAL:            
            case TB_LSB_IEEE_REAL:        
            case TB_MSB_IEEE_REAL:        
            case TB_PC_REAL:              
            case TB_MAC_REAL:             
            case TB_REAL:                 
            case TB_SUN_REAL:             
            case TB_VAX_REAL:             
            case TB_VAXG_REAL:            summary_type = TS_REAL;
                                          break;

            case TB_ASCII_INTEGER:        
            case TB_INTEGER:              
            case TB_LSB_INTEGER:          
            case TB_MSB_INTEGER:          
            case TB_PC_INTEGER:           
            case TB_SUN_INTEGER:          
            case TB_VAX_INTEGER:          
            case TB_MAC_INTEGER:          summary_type = TS_INTEGER;
                                          break;

            case TB_LSB_UNSIGNED_INTEGER: 
            case TB_MSB_UNSIGNED_INTEGER: 
            case TB_PC_UNSIGNED_INTEGER:  
            case TB_SUN_UNSIGNED_INTEGER: 
            case TB_UNSIGNED_INTEGER:     
            case TB_VAX_UNSIGNED_INTEGER: 
            case TB_MAC_UNSIGNED_INTEGER: summary_type = TS_UNSIGNED;
                                          break;

            
            case TB_CHARACTER:            summary_type = TS_CHARACTER;
                                          break;

            case TB_DATE:                 
            case TB_TIME:                 summary_type = TS_DATETIME;
                                          break;

            case TB_IBM_INTEGER:          
            case TB_IBM_UNSIGNED_INTEGER: 
            case TB_IBM_REAL:             
            case TB_ASCII_COMPLEX:
            case TB_COMPLEX:              
            case TB_IBM_COMPLEX:          
            case TB_IEEE_COMPLEX:         
            case TB_LSB_BIT_STRING:       
            case TB_LSB_IEEE_COMPLEX:     
            case TB_MAC_COMPLEX:          
            case TB_MSB_BIT_STRING:       
            case TB_MSB_IEEE_COMPLEX:     
            case TB_PC_COMPLEX:           
            case TB_SUN_COMPLEX:          
            case TB_VAX_COMPLEX:          
            case TB_VAX_DOUBLE:           
            case TB_VAXG_COMPLEX:        summary_type = TS_UNKNOWN;
                                         break;

        }  /*  End:  "switch (data_type) ..."  */
    }
 
    /*-----------------------------------------------------------------------*/
    /** ELSE IF this column is a bit column THEN                            **/
    /**    get its data type                                                **/
    /**    set the summary type based on its data type                      **/
    /*-----------------------------------------------------------------------*/

    else if (tb_show_bit_column)
    {
        bit_data_type = search_string_array (tb_bit_type_list, tb_bit_type_end, 
                                             column_info -> data_type);
        switch (bit_data_type)
        {
            case TB_INTEGER_BITS:
            case TB_MSB_INTEGER_BITS:          summary_type = TS_INTEGER;
                                               break;

            case TB_UNSIGNED_INTEGER_BITS:
            case TB_MSB_UNSIGNED_INTEGER_BITS: summary_type = TS_UNSIGNED;
                                               break;

            case TB_BOOLEAN_BIT:               summary_type = TS_CHARACTER;
                                               break;
        }
    }
 
    /*-----------------------------------------------------------------------*/
    /** ENDIF                                                               **/
    /** check to see if data field is too large to summarize and set the    **/
    /**    summary type to unknown if it is                                 **/
    /*-----------------------------------------------------------------------*/

    if (summary_type == TS_REAL && 
           interchange_format == TB_FORMAT_BINARY &&
              size > 8)
                 summary_type = TS_UNKNOWN;
    else if ((summary_type == TS_INTEGER ||
                 summary_type == TS_UNSIGNED) && 
                    interchange_format == TB_FORMAT_BINARY &&
                       size > 4)
                          summary_type = TS_UNKNOWN;
    else if ((summary_type == TS_CHARACTER || 
                summary_type == TS_DATETIME) &&
                   size > 60)
                      summary_type = TS_UNKNOWN;

    return (summary_type);

/** END **/
}


/**********************************************************************
 *$Component                                                          *
 *    OCCURRENCE_ENTRY *ts_insert_occurrence (occurrence_root,        *
 *                                            string)                 *
 *$Abstract                                                           *
 *    Inserts an occurence of a string into an occurrence tree.       *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *$Inputs                                                             *
 *    occurrence_root:                                                *
 *        The occurrence_root variable is a pointer to the root       *
 *        of a sorted binary tree of string occurrences. This tree    *
 *        is used to track instance counts of strings in a data file. *
 *    string:                                                         *
 *        The string variable is a general purpose character string   *
 *        that may contain one or more characters.                    *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    occurrence_root:                                                *
 *        The occurrence_root variable is a pointer to the root       *
 *        of a sorted binary tree of string occurrences. This tree    *
 *        is used to track instance counts of strings in a data file. *
 *$Detailed_Description                                               *
 *    This routine searches the given sorted occurrence tree for the  *
 *    input string. If the string is found, then the occurrence count *
 *    for it is incremented. If it is not found, then the string is   *
 *    inserted into the tree as a new node, with occurrence count 1.  *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/ JPL                                            *
 *$Version_and_Date                                                   *
 *    1.0   July 6, 1992                                              *
 *$Change_History                                                     *
 *    MDD   07-06-92   Original code.                                 *
 **********************************************************************/


OCCURRENCE_ENTRY *ts_insert_occurrence (occurrence_root, string)

OCCURRENCE_ENTRY *occurrence_root;
char *string;
{
   int compare;

/** BEGIN **/

   /*-------------------------------------------------------------------*/
   /** IF this subtree is empty THEN                                   **/
   /**    set the root of this subtree to the new entry                **/
   /*-------------------------------------------------------------------*/

   if (occurrence_root == NULL)
   {
      occurrence_root = (OCCURRENCE_ENTRY *) malloc (sizeof (OCCURRENCE_ENTRY));
      Check_Malloc(occurrence_root);
      occurrence_root -> left_child = NULL;
      occurrence_root -> right_child = NULL;
      occurrence_root -> count = 1;
      Malloc_String(occurrence_root -> string, String_Size(string));
      strcpy (occurrence_root -> string, string);
      ts_occurrence_count++;
   }
   /*-------------------------------------------------------------------*/
   /** ELSE                                                            **/
   /*-------------------------------------------------------------------*/

   else
   {
      /*----------------------------------------------------------------*/
      /** compare the new entry to the root                            **/
      /** IF the new entry is the same THEN                            **/
      /*----------------------------------------------------------------*/

      compare = strcmp (occurrence_root -> string, string);
      if (compare == 0)
      {

         /*-------------------------------------------------------------*/
         /** increment the occurrence count                            **/
         /*-------------------------------------------------------------*/

         occurrence_root -> count++;

         /*-------------------------------------------------------------*/
         /** ENDIF they are the same...                                **/
         /*-------------------------------------------------------------*/
      }

      /*----------------------------------------------------------------*/
      /** ENDIF the root is the same...                                **/
      /** IF the root is greater than the new entry THEN               **/
      /**    insert it in the left subtree                             **/
      /*----------------------------------------------------------------*/

      if (compare > 0)
      {
         occurrence_root -> left_child = 
                ts_insert_occurrence (occurrence_root -> left_child, string);
      }
      /*----------------------------------------------------------------*/
      /** ENDIF the root is greater...                                 **/
      /** IF the root is less than the new entry                       **/
      /**    insert the new entry in the right subtree                 **/
      /*----------------------------------------------------------------*/

      if (compare < 0)
      {
         occurrence_root -> right_child = 
                ts_insert_occurrence (occurrence_root -> right_child, string);
      }
      /*----------------------------------------------------------------*/
      /** ENDIF the root is less...                                    **/
      /*----------------------------------------------------------------*/
   }
   /*-------------------------------------------------------------------*/
   /** ENDIF this subtree is empty                                     **/
   /*-------------------------------------------------------------------*/

   return (occurrence_root);

/** END ts_insert_occurrence **/
}             


/**********************************************************************
 *$Component                                                          *
 *    OCCURRENCE_ENTRY *ts_cleanup_occurrence (occurrence_root)       *
 *$Abstract                                                           *
 *    Removes an occurrence tree.                                     *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *$Inputs                                                             *
 *    occurrence_root:                                                *
 *        The occurrence_root variable is a pointer to the root       *
 *        of a sorted binary tree of string occurrences. This tree    *
 *        is used to track instance counts of strings in a data file. *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    occurrence_root:                                                *
 *        The occurrence_root variable is a pointer to the root       *
 *        of a sorted binary tree of string occurrences. This tree    *
 *        is used to track instance counts of strings in a data file. *
 *$Detailed_Description                                               *
 *    This routine deletes the occurrence tree pointed to by the      *
 *    occurrence_root input.  This routine always returns NULL.       *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/ JPL                                            *
 *$Version_and_Date                                                   *
 *    1.0   July 6, 1992                                              *
 *$Change_History                                                     *
 *    MDD   07-06-92   Original code.                                 *
 **********************************************************************/

OCCURRENCE_ENTRY *ts_cleanup_occurrence (occurrence_root)

OCCURRENCE_ENTRY *occurrence_root;

{
   if (occurrence_root != NULL)
   {
      ts_cleanup_occurrence (occurrence_root -> left_child);
      ts_cleanup_occurrence (occurrence_root -> right_child);
      Lemme_Go(occurrence_root -> string);
      Lemme_Go(occurrence_root);
   }
   return (NULL);
}

/**********************************************************************
 *$Component                                                          *
 *    OCCURRENCE_ENTRY *ts_print_occurrence (occurrence_root)         *
 *$Abstract                                                           *
 *    Prints an occurrence table on the screen.                       *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *$Inputs                                                             *
 *    occurrence_root:                                                *
 *        The occurrence_root variable is a pointer to the root       *
 *        of a sorted binary tree of string occurrences. This tree    *
 *        is used to track instance counts of strings in a data file. *
 *$Outputs                                                            *
 *    None                                                            *
 *$Returns                                                            *
 *    occurrence_root:                                                *
 *        The occurrence_root variable is a pointer to the root       *
 *        of a sorted binary tree of string occurrences. This tree    *
 *        is used to track instance counts of strings in a data file. *
 *$Detailed_Description                                               *
 *    This routine prints the  occurrence tree pointed to by the      *
 *    occurrence_root input on the terminal screen and allows the     *
 *    user to page through it.  The tree is printed in the form of    *
 *    a table, listing each string on the tree and its occurrence     *
 *    count.                                                          *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Author_and_Institution                                             *
 *    Marti D. DeMore/ JPL                                            *
 *$Version_and_Date                                                   *
 *    1.0   July 6, 1992                                              *
 *$Change_History                                                     *
 *    MDD   07-06-92   Original code.                                 *
 **********************************************************************/


void ts_print_occurrence (occurrence_root)

OCCURRENCE_ENTRY *occurrence_root;
{
   char prompt;

/** BEGIN **/

   /*----------------------------------------------------------------*/
   /** IF a full page has already been printed THEN                 **/
   /**    prompt to see if the user wants to continue               **/
   /**    If he doesn't, set line count to -1 as a stop flag, and   **/
   /**       otherwise, reset line count to 0                       **/
   /** ENDIF                                                        **/
   /*----------------------------------------------------------------*/

   if (ts_line_count == TB_DISPLAY_ROWS)
   {
       printf ("\n   Press <RETURN> or enter E to exit: ");
       Read_Char(prompt);
       if (toupper ((int) prompt) == 'E')
       {
          ts_line_count = -1;
          return;
       }
       else
          ts_line_count = 0;
       printf ("\n");
   }
   /*----------------------------------------------------------------*/
   /** IF there are more entries to print and it's not time to stop **/
   /**    print the left subtree of the root                        **/
   /**    print the root and increment line count                   **/
   /**    print the right subtree of the root                       **/
   /** ENDIF                                                        **/
   /*----------------------------------------------------------------*/

   if (occurrence_root != NULL && ts_line_count != -1)
   {
       ts_print_occurrence (occurrence_root -> left_child);
       if (ts_line_count != -1)
       {
          if (occurrence_root -> count == 1)
             printf ("     %s -- 1 occurrence\n", occurrence_root -> string);
          else
             printf ("     %s -- %ld occurrences\n", occurrence_root -> string,
                     occurrence_root -> count);
          ts_line_count++;
       }
       ts_print_occurrence (occurrence_root -> right_child);
   }
   return;

/** END **/
}


/**********************************************************************
 *$Component                                                          *
 *    void tb_move_by_column_name (column_name,                       *
 *                                 column_info, item_count,           *
 *                       column_offset, col_movement, column_number)  *
 *$Abstract                                                           *
 *    Moves through the linked list columns or bit_columns based on   *
 *    specified name                                                  *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    column_name:                                                    *
 *        The column_name variable is a pointer to a character        *
 *        string which contains a column name in a object or          *
 *        or a bit_column name in a column to be searched upon        *
 *    column_info:                                                    *
 *        The column_info variable is a pointer to the structure      *
 *        which contains keyword information on a column object.      *
 *    item_count:                                                     *
 *        The item_count variable keeps track of the current item in  *
 *        a column object (i.e, the item being displayed).            *
 *    column_offset:                                                  *
 *        The column_offset variable keep track of the "real" offset  *
 *        of the current column being displayed. The "real" offset    *
 *        of a column is the sum of all the column items to the       *
 *        left of it. Column_offset also applies to bit_columns.      *
 *    col_movement:                                                   *
 *        The col_movement variable is an integer representing the    *
 *        number of columns or bit_columns to move to the right or    *
 *        left, including all items.                                  *
 *    column_number:                                                  *
 *        The "column_number" variable is an integer representing     *
 *        the "user" offset of a column. The "user" offset of a       *
 *        column is a count of all the column objects (not including  *
 *        items) to the left of a column.  Column_number also applies *
 *        to bit_columns.                                             *
 *$Outputs                                                            *
 *    column_info:                                                    *
 *        The column_info variable is a pointer to the structure      *
 *        which contains keyword information on a column object.      *
 *    item_count:                                                     *
 *        The item_count variable keeps track of the current item in  *
 *        a column object (i.e, the item being displayed).            *
 *    column_offset:                                                  *
 *        The column_offset variable keep track of the "real" offset  *
 *        of the current column being displayed. The "real" offset    *
 *        of a column is the sum of all the column items to the       *
 *        left of it. Column_offset also applies to bit_columns.      *
 *    col_movement:                                                   *
 *        The col_movement variable is an integer representing the    *
 *        number of columns or bit_columns to move to the right or    *
 *        left, including all items.                                  *
 *    column_number:                                                  *
 *        The "column_number" variable is an integer representing     *
 *        the "user" offset of a column. The "user" offset of a       *
 *        column is a count of all the column objects (not including  *
 *        items) to the left of a column.  Column_number also applies *
 *        to bit_columns.                                             *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    This routine moves through columns or bit_columns in a label    *
 *    based on the given name.  It takes into account the ITEMS       *
 *    keyword when counting backwards through a list of columns.      *
 *    It will skip over columns  which have the display flag          *
 *    set to false.  When it determines the new column, it resets     *
 *    the column_info pointer, the item count, the column offset, and *
 *    column_number for output.                                       *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Error_Handling                                                     *
 *    Print a messsage if failed to match on the given name           *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    T. Charng / J.P.L.                                              *
 *$Version_and_Date                                                   *
 *    1.0   April 26, 1993                                            *
 *$Change_history                                                     *
 *    TC    04-26-93   Original code.                                 *
 **********************************************************************/

COLUMN_INFO *tb_move_backward_by_column_name ();
COLUMN_INFO *tb_move_forward_by_column_name ();

void tb_move_by_column_name(column_name, column_info,
             item_count, column_offset, column_number, current_column)
    char *column_name;
    COLUMN_INFO *(*column_info);
    long *item_count;
    long *column_offset;
    long column_number;
    long *current_column;
{
    char *col_name = column_name;
    COLUMN_INFO *save_column_info   = *column_info;
    long        save_item_count     = *item_count;
    long        save_column_offset  = *column_offset;
    long        save_column_number  = column_number;
    long        save_current_column = *current_column;
    	
    int m;

    if( ! *column_info)
        return;

    util_strip_lead_and_trail (col_name, ' ');
    if(util_locate_substring(col_name,"\"") == NULL)
        util_upper_case (col_name);
    else
        util_strip_lead_and_trail (col_name, '\"');

    m = strlen(column_name);
    if(strncmp((*column_info)->name, column_name, m) == 0)
    {
#ifdef I_CANNOT_GET_THE_COLUMN_NUMBER_CORRECT
        *column_info = (*column_info)->next;
        column_number++;
#else
        return;
#endif
    }

    if(tb_move_forward_by_column_name (col_name,
                      column_info, item_count, column_offset,
                      column_number - 1, current_column))
        return;

    if(tb_move_backward_by_column_name (col_name,
                      column_info, item_count, column_offset,
                      *column_offset + *item_count, current_column))
        return;

    if(tb_show_column == TRUE)
        printf("\n  Can\'t find COLUMN NAME like \"%s\" in current object\n", column_name);
    else if(tb_show_bit_column == TRUE)
        printf("\n  Can\'t find BIT_COLUMN NAME like \"%s\" in current column\n", column_name);
    printf("  press RETURN to continue..."); getchar();
    *column_info   = save_column_info;
    *item_count    = save_item_count;
    *column_offset = save_column_offset;
    column_number  = save_column_number;
    *current_column= save_current_column;
    return;
}   /* end tb_move_by_cloumn_name() */
                                                            

/**********************************************************************
 *$Component                                                          *
 *    void tb_move_backward_by_column (column_name, column_info,      *
 *                       item_count, column_offset,                   *
 *                       col_movement, column_number)                 *
 *$Abstract                                                           *
 *    Moves to the left through columns or bit_columns                *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    column_name:                                                    *
 *        The column_name variable is a pointer to a character        *
 *        string which contains a column name in a object or          *
 *        or a bit_column name in a column to be searched upon        *
 *    column_info:                                                    *
 *        The column_info variable is a pointer to the structure      *
 *        which contains keyword information on a column object.      *
 *    item_count:                                                     *
 *        The item_count variable keeps track of the current item in  *
 *        a column object (i.e, the item being displayed).            *
 *    column_offset:                                                  *
 *        The column_offset variable keep track of the "real" offset  *
 *        of the current column being displayed. The "real" offset    *
 *        of a column is the sum of all the column items to the       *
 *        left of it. Column_offset also applies to bit_columns.      *
 *    col_movement:                                                   *
 *        The col_movement variable is an integer representing the    *
 *        number of columns or bit_columns to move to the right or    *
 *        left, including all items.                                  *
 *    column_number:                                                  *
 *        The "column_number" variable is an integer representing     *
 *        the "user" offset of a column. The "user" offset of a       *
 *        column is a count of all the column objects (not including  *
 *        items) to the left of a column.  Column_number also applies *
 *        to bit_columns.                                             *
 *$Outputs                                                            *
 *    column_info:                                                    *
 *        The column_info variable is a pointer to the structure      *
 *        which contains keyword information on a column object.      *
 *    item_count:                                                     *
 *        The item_count variable keeps track of the current item in  *
 *        a column object (i.e, the item being displayed).            *
 *    column_offset:                                                  *
 *        The column_offset variable keep track of the "real" offset  *
 *        of the current column being displayed. The "real" offset    *
 *        of a column is the sum of all the column items to the       *
 *        left of it. Column_offset also applies to bit_columns.      *
 *    col_movement:                                                   *
 *        The col_movement variable is an integer representing the    *
 *        number of columns or bit_columns to move to the right or    *
 *        left, including all items.                                  *
 *    column_number:                                                  *
 *        The "column_number" variable is an integer representing     *
 *        the "user" offset of a column. The "user" offset of a       *
 *        column is a count of all the column objects (not including  *
 *        items) to the left of a column.  Column_number also applies *
 *        to bit_columns.                                             *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    This routine moves left through columns or bit_columns in a     *
 *    label.  It takes into account the ITEMS keyword when counting   *
 *    backwards through a list of columns.  It will skip over columns *
 *    which have the display flag set to false.  When it determines   *
 *    the new column, it resets the column_info pointer, the item     *
 *    count, the column offset, and column_number for output.         *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Error_Handling                                                     *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    T. Charng / J.P.L.                                              *
 *$Version_and_Date                                                   *
 *    1.0   April 26, 1993                                            *
 *$Change_history                                                     *
 *    TC    04-26-93   Original code.                                 *
 **********************************************************************/

COLUMN_INFO *tb_move_backward_by_column_name (column_name,
        column_info, item_count, column_offset, col_movement, column_number)
    char *column_name;
    COLUMN_INFO *(*column_info);
    long *item_count;
    long *column_offset;
    long col_movement;
    long *column_number;
{
    COLUMN_INFO *ptr = {NULL};
    long i;
    long col_count;
    int m;

    m = strlen(column_name);

/*
    for (col_count = 0; ((*column_info != NULL) && 
            (strncmp((*column_info)->name, column_name, m) != 0)); ++col_count)
    {
*/
    for (col_count = 0; (*column_info != NULL); ++col_count)
    {
        if(strncmp((*column_info)->name, column_name, m) == 0)
            break;

        if ((*item_count > 1) && tb_show_items && (! tb_show_label))
            --(*item_count);
        else
        {
            for (i=0, ptr = (*column_info) -> prev;
                    ((ptr != NULL) && tb_show_items && (! tb_show_label) && (! ptr->display)); 
                         ptr = ptr -> prev)
            {
                i += ptr -> items;
            }

            if (ptr == NULL)
                return(NULL);

            i += ptr -> items;
            *column_offset -= i;
            *column_info = ptr;
            *item_count = (*column_info) -> items;
            --(*column_number);

        }  /*  End:  "if ((*item_count > 1) && ... else ..."  */

    }  /*  End:  "for (col_count = 0; ..."  */
    
    return(ptr); 

}   /* end tb_move_backward_by_column_name() */


/**********************************************************************
 *$Component                                                          *
 *    void tb_move_forward_by_column (column_name, column_info,       *
 *                       item_count, column_offset,                   *
 *                       col_movement, column_number)                 *
 *$Abstract                                                           *
 *    Moves to the right through columns or bit_columns               *
 *$Keywords                                                           *
 *    TOOL_MAIN                                                       *
 *    TABLE_BROWSER                                                   *
 *    ASCII_INTERFACE                                                 *
 *$Inputs                                                             *
 *    column_name:                                                    *
 *        The column_name variable is a pointer to a character        *
 *        string which contains a column name in a object or          *
 *        or a bit_column name in a column to be searched upon        *
 *    column_info:                                                    *
 *        The column_info variable is a pointer to the structure      *
 *        which contains keyword information on a column object.      *
 *    item_count:                                                     *
 *        The item_count variable keeps track of the current item in  *
 *        a column object (i.e, the item being displayed).            *
 *    column_offset:                                                  *
 *        The column_offset variable keep track of the "real" offset  *
 *        of the current column being displayed. The "real" offset    *
 *        of a column is the sum of all the column items to the       *
 *        left of it. Column_offset also applies to bit_columns.      *
 *    col_movement:                                                   *
 *        The col_movement variable is an integer representing the    *
 *        number of columns or bit_columns to move to the right or    *
 *        left, including all items.                                  *
 *    column_number:                                                  *
 *        The "column_number" variable is an integer representing     *
 *        the "user" offset of a column. The "user" offset of a       *
 *        column is a count of all the column objects (not including  *
 *        items) to the left of a column.  Column_number also applies *
 *        to bit_columns.                                             *
 *$Outputs                                                            *
 *    column_info:                                                    *
 *        The column_info variable is a pointer to the structure      *
 *        which contains keyword information on a column object.      *
 *    item_count:                                                     *
 *        The item_count variable keeps track of the current item in  *
 *        a column object (i.e, the item being displayed).            *
 *    column_offset:                                                  *
 *        The column_offset variable keep track of the "real" offset  *
 *        of the current column being displayed. The "real" offset    *
 *        of a column is the sum of all the column items to the       *
 *        left of it. Column_offset also applies to bit_columns.      *
 *    col_movement:                                                   *
 *        The col_movement variable is an integer representing the    *
 *        number of columns or bit_columns to move to the right or    *
 *        left, including all items.                                  *
 *    column_number:                                                  *
 *        The "column_number" variable is an integer representing     *
 *        the "user" offset of a column. The "user" offset of a       *
 *        column is a count of all the column objects (not including  *
 *        items) to the left of a column.  Column_number also applies *
 *        to bit_columns.                                             *
 *$Returns                                                            *
 *    None                                                            *
 *$Detailed_Description                                               *
 *    This routine moves right through columns or bit_columns in a    *
 *    label.  It takes into account the ITEMS keyword when counting   *
 *    backwards through a list of columns.  It will skip over columns *
 *    which have the display flag set to false.  When it determines   *
 *    the new column, it resets the column_info pointer, the item     *
 *    count, the column offset, and column_number for output.         *
 *$External_References                                                *
 *    Item                    Shared-Data              Access         *
 * -----------------------------------------------------------------  *
 *    tbtool globals                                                  *
 *$Error_Handling                                                     *
 *    None                                                            *
 *$Side_Effects                                                       *
 *    None                                                            *
 *$Author_and_Institution                                             *
 *    T. Charng / J.P.L.                                              *
 *$Version_and_Date                                                   *
 *    1.0   April 26, 1993                                            *
 *$Change_history                                                     *
 *    TC    04-26-93   Original code.                                 *
 **********************************************************************/

/*------------------------------- tb_move_forward_by_column_name() --+
|   TC_MOD_26APR93                                                   |
+-------------------------------------------------------------------*/

COLUMN_INFO *tb_move_forward_by_column_name (column_name,
        column_info, item_count, column_offset, col_movement, column_number)
    char *column_name;
    COLUMN_INFO *(*column_info);
    long *item_count;
    long *column_offset;
    long col_movement;
    long *column_number;
{
    COLUMN_INFO *ptr = {NULL};        
    long i;
    long col_count;                   
    int m;

    m = strlen(column_name);
    col_count = 0;
    while(*column_info)
    {
        if(strncmp((*column_info)->name, column_name, m) == 0)
            break;

        if (tb_show_items && (! tb_show_label) && 
                 (*item_count < (*column_info) -> items))
        {
            ++*item_count;
        }
        else
        {
            for (i = (*column_info)->items, ptr = (*column_info)->next;
                    ((ptr != NULL) && tb_show_items && (! tb_show_label) && (! ptr->display)); 
                          i += ptr->items, ptr = ptr -> next) ;

            if (ptr == NULL)
                return(NULL);

            *column_offset += i;
            *column_info = ptr;
            *item_count = 1;
            ++(*column_number);
        }  /*  End:  "if ((tb_show_items) && ..."  */

        ++col_count;

    }  /*  End:  "for (col_count = 0; ..."  */
    
    return(ptr);

}  /* end tb_move_forward_by_column_name() */
