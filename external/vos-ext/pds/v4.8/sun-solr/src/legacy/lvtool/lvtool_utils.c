
#include "lvtool_utils.h"

/******************************************************************************
 Routine: STRING_LIST *find_pointer_files(char *start_path, char *ptr_name, 
									STRING_LIST *file_list_ptr, char *file_name)

 Inputs:
       start_path     pointer to the directory path of where to begin a search
	   ptr_name       pointer to what type of keyword we have
	   file_list_ptr  pointer to a list that contains the location of the pointer
	                  file(s) we are searching for
	   file_name      pointer to a file that we are looking for in the directories

 Description:
       
	   This routine will allow lvtool to search for pointers to files found in
	   a label. It will recursively search the upper level directories until
	   it has reached the top.

 Change History:
       06-06-05 MDC Original code
       01-26-06 MDC Changed directory names to look at to uppercase to conform
	                more to PDS standards
 ******************************************************************************/
STRING_LIST *find_pointer_files(char *start_path, char *ptr_name, STRING_LIST *file_list_ptr, char *file_name)
{
	int DosErr;
	char *dir_to_add = NULL;
	char *temp_path = NULL;
	char *new_start_path = NULL;
	int keep_searching = 1;
	char CurrDir[MAX_PATH_LEN]={0};
	int chdir_success = 0;

#ifdef SUN_UNIX
	char command_str[MAX_PATH_LEN]={0};
	int success = 0;
	int str_len = 0;
	char *start_path_end = NULL;
	char temp_filename[MAX_PATH_LEN]={0};
	char temp_dir[MAX_PATH_LEN]={0};
#endif

	getcwd(CurrDir, MAX_PATH_LEN);
		
    New_String(temp_path, start_path);

	/* The following checks determine what directory to look in. The directory names to
	   look for in the upper directory levels are determined by the pointers that were found in
	   the label. PDS Standards refernce manual defines what directories to look under if
	   a particular pointer was found
	*/
	/* 01-26-06 MDC - Change all names to uppercase. */
	if( (strstr(ptr_name, "DESCRIPTION") != NULL) || (strstr(ptr_name, "TEXT") != NULL) )
	{
		New_String(dir_to_add, "DOCUMENT");
	}
	else if( (strstr(ptr_name,"CATALOG") != NULL) || (strstr(ptr_name,"DATA_SET_MAP_PROJECTION") != NULL) )
	{
		New_String(dir_to_add, "CATALOG");
	}
	else if(strstr(ptr_name,"STRUCTURE") != NULL)
	{
		New_String(dir_to_add, "LABEL");
	}
	else if(strstr(ptr_name,"INDEX_TABLE") != NULL)
	{
		New_String(dir_to_add, "INDEX");
	}
	/* If it doesn't belong to any of the above, then it is best to not search anywhere else */
	else
		keep_searching = 0;
		
	while( keep_searching )
	{
        new_start_path = create_start_path(temp_path, dir_to_add);
			
		if(new_start_path != NULL)
		{

#ifdef MSDOS_TC
			/* Test the existence of the directory first. If it fails, then we need to go higher
			   in the directory levels.
			*/
            DosErr = chdir(new_start_path);
				
			if(DosErr == 0)
			{
				/* If the directory we are checking does exist, change back to the orig. working 
				   directory and then perform a search for the pointer.
			    */
				chdir_success = dir_walk_2(CurrDir);
					
				if(chdir_success != 0)
					printf("Can't change back to directory: %s\n\n", CurrDir);

				file_list_ptr = dir_walk(new_start_path, file_name, file_list_ptr, 0, FALSE);

				keep_searching = 0;
			}
#else
            str_len = strlen(new_start_path);
			start_path_end = new_start_path + str_len - 1;
			if (*start_path_end != '/')
			{
                sprintf(command_str, "%s/%s", new_start_path, file_name);
			}
			else
			{
				sprintf(command_str, "%s%s", new_start_path, file_name);
			}
		
			file_list_ptr = slv_get_volume_list(command_str, 0);
			/* Try lowercase filename if we couldn't find anything */
			if (file_list_ptr == NULL)
			{
				strcpy(temp_filename, file_name);
				util_lower_case (temp_filename);

				if (*start_path_end != '/')
				{
					sprintf(command_str, "%s/%s", new_start_path, temp_filename);
				}
				else
				{
					sprintf(command_str, "%s%s", new_start_path, temp_filename);
				}
				
				file_list_ptr = slv_get_volume_list(command_str, 0);
			}
			else
				keep_searching = 0;
#endif
		}
		else
		{
			keep_searching = 0;
		}

		Replace_String(temp_path, new_start_path);
		Lemme_Go(new_start_path);
	}
	
	Lemme_Go(dir_to_add);
	Lemme_Go(temp_path);

	return file_list_ptr;
}

/******************************************************************************
 Routine: char *create_start_path(char *label_dir, char *dir_to_add)

 Inputs:
       label_dir     path we are starting off with
	   dir_to_add    the name of the directory to add to the path

 Description: This routine will create a new directory path one level up from
              the label_dir input. It will then add the directory name at the
			  end of this new path based on the input of dir_to_add. This is
			  a modified version of the get_label_dir routine found in labtool.c
 
 Change History:
      06-06-05   MDC    Original code

******************************************************************************/

char *create_start_path(char *label_dir, char *dir_to_add)
{
 /*   char upper_name[PDS_MAXLINE];
    char lower_name[PDS_MAXLINE];*/
    char temp_name[PDS_MAXLINE];
	char *temp_ptr = NULL;
	char *new_path = NULL;

#ifdef MSDOS_TC
	int islash = '\\';
	char cslash[2] = {'\\'};
#else
	int islash = '/';
	char cslash[2] = {'/'};
#endif

	strcpy(temp_name, label_dir);    /* save the name of the directory containing the label file*/

	temp_ptr = strrchr(temp_name, islash); /* we need to get rid of the final slash*/
	if(temp_ptr == NULL) return NULL;  /* there was no slash, we don't know what to do about that*/
	*temp_ptr = '\0';                /* we got one, now get rid of it.  We had something like */
									 /* C:\directory_1\directory_2\directory_3\               */
									 /* we need to get rid of directory_3\                    */
									 /* leaving something like C:\directory_1\directory_2     */
	                                 /* we will get rid of the final slash here and then get  */
                                     /* rid of the directory_3 below                          */

	temp_ptr = strrchr(temp_name, islash); /*find the last slash    */
	if(temp_ptr == NULL) return NULL;        /*wasn't one, get out    */
	temp_ptr++;
	*temp_ptr = '\0';                      /* get rid of everything after the final slash*/
	strcat(temp_name, dir_to_add);         /* add the search directory to the end, it is probably LABEL*/

	New_String(new_path, temp_name);

	return new_path; /*wasn't one*/
}
