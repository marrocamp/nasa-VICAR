#include "dirwalk.h"

/**********************************************************************/
/* directory walk                                                     */
/* Purpose:                                                           */
/* This is the top level function for walking a DOS directory tree.   */
/* As each directory is identified the specified files will be        */
/* searched for.                                                      */
/*                                                                    */
/* Arguements:                                                        */
/* StartDir is the top directory from which to begin the search       */
/* Fmask is the file or file mask for which to search                 */
/* file_list is the pointer to the buffer that is to contain the list */
/*           of files we find.                                        */
/* use_di is a flag indicating if we should recursively search all    */
/*        directories below the StartDir or just search StartDir for  */
/*        the Fmask.  0 is recursive search, 1 is top level only      */ 
/*                                                                    */
/* Returns:                                                           */
/* None.                                                              */
/*                                                                    */
/* History:                                                           */
/* 12/2/97   Origination date                                         */
/* 12/16/97  Created dirwalk.h, added some comments, and changed      */
/*           FileMask variable name to Fmask                          */
/**********************************************************************/
STRING_LIST * dir_walk(char * StartDir, char * Fmask, STRING_LIST * file_list,
					   LOGICAL use_di, LOGICAL kill)   /*01-15-98*/
{
   int BadInit;
                                              // When a DOS program ends, you must
	                                          // specifically CD back to the starting
                                              // dir if you have changed dir while 
	                                          // running, or the command shell will be
                                              // sitting in that new dir.  This is why
	                                          // we set "InitialDir" at outset,
                                              // reset it on exit, and trap signals, so
	                                          // that we can reset it even on a
                                              // ctrl-C exit.
   temp_f_s = NULL;                                                      /*10-15-98*/
   FMask = Fmask;
   BadInit = dw_save_cwd(kill);       /* Get the current working directory 10-15-98*/
   Start_dir = StartDir;
   if (!BadInit) 
   {
      char * Cp = StartDir;
      if (Cp == NULL) 
         {
            Cp = InitialDir;
         }
      if ((StartDir != "") && (StartDir != NULL)) dir_walk_5(StartDir, NULL);
      file_list = dir_walk_7(Cp, file_list);
      if (!use_di)        //Recurse) 
      {
         dir_walk_5(Start_dir, NULL);
         file_list = dir_walk_3(file_list, use_di);
      }
   }
   dir_walk_5(InitialDir, NULL);
   fflush(stdout);
   return(file_list);
}                                                           // dir_walk

/**********************************************************************/
/* dw_save_cwd                                                        */
/* Pupose:                                                            */
/* Save the current working directory so that we can reset it on the  */
/* way out.                                                           */
/*                                                                    */
/* History:                                                           */
/* 12/2/97 origination date                                           */
/**********************************************************************/
static int dw_save_cwd(LOGICAL kill)                      /*10-15-98*/
{
	char command_str [PDS_MAXLINE + 1];
	char *cwd;
    int BadInit = 0;
	if(kill)                                              /*01-15-98*/
	{
       if (signal(SIGINT, Kill)) 
	   {
          perror("signal SIGINT");
       }
	}

    cwd = getcwd(InitialDir, sizeof(InitialDir));  //Get the current working directory
    if (cwd == NULL)                               //It didn't work.
	{
        sprintf(command_str, "initial getcwd err %d in dir_walk", errno);
        err_append_message(ERROR, command_str);
        BadInit = 1;
    }
    if (!BadInit) 
	{
        if (FMask != NULL)                      //There should be something here.
		{
            BadInit = dir_walk_2(cwd);        //set the directory and drive back
        }                                          //to the starting point.
    }
    return(BadInit);
}                                                               // dw_save_cd()

/**********************************************************************/
/* dir_walk_2                                                         */
/* Purpose:                                                           */
/* Change the current working drive and the current working directory */
/* back to original                                                   */
/* Arguements:                                                        */
/* cwd contains the original path ie c:\lvtool                        */
/*                                                                    */
/* Return:                                                            */
/* An integer indicating success or not, 0 is good, 1 is bad          */
/* History:                                                           */
/* 12/12/97 Origination date                                          */
/**********************************************************************/
int dir_walk_2(char * cwd) 
{
	char command_str [PDS_MAXLINE + 1];
    int BadInit = 0;
    if (cwd[1] == ':') {
        char Drv = (char) toupper(cwd[0]);
        int Err = _chdrive(Drv - 'A' + 1);
        if (Err) {
            sprintf(command_str, "Can't change to drive %c\n", Drv);
            err_append_message(ERROR, command_str);
            BadInit = 1;
        }
    }
    if (!BadInit) {
         char * Cp = cwd;
        if (cwd[1] == ':') {
            Cp = (cwd + 2);
        }
        if (Cp[0] != '\0') {
            int Err = chdir(IBMCAST Cp);
            if (Err) {
               sprintf(command_str, "Can't change to %s\n", Cp);
               err_append_message(ERROR, command_str);
               BadInit = 1;
            }
        }
    }
    return(BadInit);
}                                                               // dir_walk_2

/****************************************************************************/
/* dir_walk_5                                                               */
/* Purpose:                                                                 */
/* CD's somewhere.  As supplied paths might be relative to the initial dir, */
/* which may not be the root dir, we cheat by always CD'ing there first,    */
/* then to the relative dir.  This gets us around having to parse out stuff */
/* like ".." which should work in the middle of a path like it does in Unix,*/
/* but doesn't under OS/2 or DOS.                                           */
/*                                                                          */
/* Arguements:                                                              */
/* CurrDir points to a string containing the current directory which is     */
/*         really the starting directory.                                   */
/* Cp      points to the next directory down the tree to change to.         */
/*                                                                          */
/* History:                                                                 */
/* 12/11/97 Origination date                                                */
/****************************************************************************/
static void dir_walk_5( char *CurrDir,  char *Cp)
{
int DosErr;
char command_str [PDS_MAXLINE + 1];
int BadInit = 0;
char Drv;
if (CurrDir[1] == ':') {
        Drv = (char) toupper(CurrDir[0]);
        DosErr = _chdrive(Drv - 'A' + 1);
        if (DosErr) {
            sprintf(command_str, "Can't change to drive %c\n", Drv);
            err_append_message(ERROR, command_str);
            BadInit = 1;
        }
}
 DosErr = chdir(IBMCAST CurrDir);   //Change to the new directory
   if (DosErr)                         //If there was an error then QUIT
   {
      perror(CurrDir);
      exit(1);
   }
if (Cp != NULL)                        //If there was a next directory specified
   {                                   //go there.  If not use the directory
   DosErr = chdir(IBMCAST Cp);         //specified as the next directory.
   if (DosErr) 
     {
     perror(Cp);
     exit(1);
     }
   }
}                                                                 // dir_walk_5

/**********************************************************************/
/* DoGetCwd                                                           */
/* Purpose:                                                           */
/* To get the current working directory.                              */
/*                                                                    */
/* Arguements:                                                        */
/* CurrDir a pointer to a buffer into which to place the current      */
/* directory name.                                                    */
/* SizeOfCurrDir is the length of the buffer.                         */
/*                                                                    */
/* History:                                                           */
/* 12/11/97 Origination date                                          */
/**********************************************************************/

static void DoGetCWD(char *CurrDir, int SizeOfCurrDir) {
char command_str [PDS_MAXLINE + 1];
    char * cwd = getcwd(CurrDir, SizeOfCurrDir);
    if (cwd == NULL) {
		sprintf(command_str, "getcwd err %d, %s, size %d ",
                errno, CurrDir, SizeOfCurrDir);
 		err_append_message(ERROR, command_str);
       perror("");
        exit(1);
    }
}                                                             // DoGetCWD()

/**********************************************************************/
/* dir_walk_3                                                         */
/* Purpose:                                                           */
/* This is the function which actually performs the directory walk.   */
/*                                                                    */
/* Notes:                                                             */
/* This function checks the current directory for any file which is   */
/* a subdirectory. When it finds one it changes to that directory     */
/* and continues checking for more subdirectories.  Just befor it     */
/* changes to a new subdirectory it calls dir_walk_7 to check for the */
/* files that were specified.                                         */
/*                                                                    */
/* Arguements:                                                        */
/* file_list is a STRING_LIST which contians the list of files to be  */
/* processed later on.  These are files of a type specified by an     */
/* arguement to dir_walk.                                             */
/*                                                                    */
/* Returns:                                                           */
/* a pointer to a STRING_LIST which contains the file list to be      */
/* processed.                                                         */
/*                                                                    */
/* History:                                                           */
/* 12/11/97  Origination Date                                         */
/**********************************************************************/

STRING_LIST * dir_walk_3(STRING_LIST * file_list, LOGICAL use_di) {

    char * Cp;
	char Cp_1[MAX_PATH_LEN];
	char * DirMsk;
    char CurrDir[MAX_PATH_LEN];
    int Leaf = 1, len;
    unsigned DosErr = 0;
	BOOL fnf_rtn;
    HANDLE ff = NULL;
	WIN32_FIND_DATA ffblk1;

    DoGetCWD(CurrDir, MAX_PATH_LEN);
    DirMsk = "*.*";
    ff = FindFirstFile( DirMsk, &ffblk1);
	if (ff == INVALID_HANDLE_VALUE)
	{
        DosErr = errno;
		Cp = NULL;
    }
    else 
	{
		Cp = ffblk1.cFileName;
    }
    IsDir = ((!DosErr) && (ffblk1.dwFileAttributes & _A_SUBDIR));
    
   while (Cp != NULL) {
        if (IsDir) {
            // Skip ".." dir
            if ((strcmp(Cp, "..") != 0) && (strcmp(Cp, ".") != 0)) {
                Leaf = 0;
                dir_walk_5(Cp, NULL);
                if (!use_di) {  //LeavesOnly) {
					strcpy(Cp_1, CurrDir);
					len = (int) strlen(Cp_1);
                    /* If there is not a \ following  the path then add one.*/
                    /* PATH_DELIM is a \                                    */
                    if ((len > 0) && ( * (Cp_1 + len - 1) != *PATH_DELIM))
	                {                                                   
                       strcat(Cp_1, PATH_DELIM);
                    }
                    strcat(Cp_1, Cp);

                    file_list = dir_walk_7(Cp_1, file_list);
                }
                file_list = dir_walk_3(file_list, use_di);
                // Must return to this dir after having worked lower dirs
                dir_walk_5(CurrDir, NULL);
            }
        }
    Cp = NULL;
    fnf_rtn = FindNextFile(ff, &ffblk1);
    if (fnf_rtn) {
 		Cp = ffblk1.cFileName;
    }
	else
	{
		Cp = NULL;
	}
    IsDir = ((!DosErr) && (ffblk1.dwFileAttributes & _A_SUBDIR));

    }
    if (use_di && Leaf) {    //LeavesOnly && Leaf) {
        file_list = dir_walk_7(CurrDir, file_list);
    }
    (void) chdir(InitialDir);
	FindClose(ff);
	return(file_list);
}                                                             // dir_walk_3()


/**********************************************************************/
/* Kill                                                               */
/* Purpose                                                            */
/* To put the current working directory and drive back if we got a ^c */
/* or someother type of kill.                                         */
/*                                                                    */
/* Arguements:                                                        */
/* The signal number from DOS that killed us                          */
/*                                                                    */
/* History:                                                           */
/* 12/11/97 Origination date                                          */
/**********************************************************************/
static void cdecl Kill(int SigNum) {
    char command_str[PDS_MAXLINE];
    sprintf(command_str, "signal %d recv'd\n", SigNum);
	err_append_message(ERROR, command_str);
    // We ignore errors here, just try to go there.
    chdir(InitialDir);
    if (InitialDir[1] == ':') {
        char Drv = (char) toupper(InitialDir[0]);
        _chdrive(Drv - 'A' + 1);
    }
    exit(1);
}                                                         // Kill

/**************************************************************/
/* dir_walk_7                                                 */
/* Purpose:                                                   */
/* To check all files in the current working directory for a  */
/* match to the file mask.  All matches are entered into the  */
/* file_list.                                                 */
/*                                                            */
/* Arguements:                                                */
/* directory is a pointer to the current working directory.   */
/* this will be used to build the complete file path for each */
/* file match that is found.                                  */
/* file_list is the pointer to the list of files that is being*/
/* built.                                                     */
/*                                                            */
/* Returns:                                                   */
/* The return is a STRING_LIST pointer the the file_list      */
/*                                                            */
/* History:                                                   */
/* 12/11/97 Origination date                                  */
/**************************************************************/

STRING_LIST *dir_walk_7 (char * directory, STRING_LIST * file_list)
{
  // STRING_LIST *file_list = {NULL};
 //  static STRING_LIST * temp_f_s;
   char *temp_str = NULL;

   HANDLE            hSearch;
   HANDLE            handle_save;
   WIN32_FIND_DATA   ffblk;                              

   int got_handle = 0;
   int testflag;
   BOOL got_another;
/********************************/
//printf("%s\n", directory);
/********************************/
   
   hSearch = FindFirstFile(FMask, &ffblk);     // find the first file like the mask
   if (hSearch !=  INVALID_HANDLE_VALUE)       // if we got on set the got_handle to 1
   {
	   got_handle = 1;
	   handle_save = hSearch;
//   }
       while (got_handle == 1)
       {
	     testflag = ffblk.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY;
//         if (ffblk.dwFileAttributes != FA_DIREC) // is the file not a directory?
		 if(testflag == 0)
          {                                       // if it is not then make a full
			                                      // path name for the file.
             temp_str = util_create_file_spec (directory, ffblk.cFileName);  /*DWS 07-17-98*/
		     printf("Adding %s to file list\n", temp_str);
			                                      // append this name to the list.
             file_list = util_append_string_list (file_list, temp_str, STRING_TYPE);
             temp_f_s = file_list;                 // save this for on the way out
             Lemme_Go(temp_str);
          }
          got_another = FindNextFile (hSearch, &ffblk); // is there another one?
	      if (got_another != TRUE)  // no, close the handle and stop the loop
	      {
		      got_handle = 0;
	          FindClose(handle_save);
	      }
	   }
	   
   }
   if (file_list == NULL) file_list = temp_f_s;
   return(file_list);
}
                                                                     //dir_walk_7()