The Perl Validation Toolkit - README file:
==========================================

VERSION: 4.1 
DATE: 03-15-06

DISCLAIMER

THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE CALIFORNIA
INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. GOVERNMENT CONTRACT WITH
THE NATIONAL AERONAUTICS AND SPACE ADMINISTRATION (NASA). THE SOFTWARE
IS TECHNOLOGY AND SOFTWARE PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND
IS PROVIDED "AS-IS" TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND,
INCLUDING ANY WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS
FOR A PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES
UCC 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE SOFTWARE AND
RELATED MATERIALS, HOWEVER USED. IN NO EVENT SHALL CALTECH, ITS JET
PROPULSION LABORATORY, OR NASA BE LIABLE FOR ANY DAMAGES AND/OR COSTS,
INCLUDING, BUT NOT LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF
ANY KIND, INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE REASON TO KNOW,
OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. RECIPIENT BEARS ALL RISK RELATING
TO QUALITY AND PERFORMANCE OF THE SOFTWARE AND ANY RELATED MATERIALS, AND
AGREES TO INDEMNIFY CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS
RESULTING FROM THE ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.


I. INTRODUCTION

The PDS Validation Tools (PVT) are a set of scripts and programs that
perform the following validation tests:

  * check ISO9660 compliance (checkiso.pl, using isocheck)
  * check for required files and directories (checkreq.pl)     
  * find label validation errors (listerr.pl, using lvtool)          
  * find new keywords and standard values (listnew.pl, using lvtool) 
  * check that the index file points to the data (checkidx.pl) 
  * test for errors in ASCII files (testline.pl)               
  * check references against reference database (checkref.pl)
  * list keyword/value pairs for inspection (listkwv.pl, using kwvtool)
  
A single controlling script (validate.pl) allows the user to select
which of the tests to run.  Running validate.pl -b <volume root> 
executes all the tests in batch mode, automatically. 

An even easier way to use PVT is through its graphical user interface 
(tkvalid.pl). Simply run tkvalid.pl, then use your mouse to select the
volume root and choose the tests to run.

Starting with version 1.0, PVT has the capability to allow the user to
select what types of messages they do not want to see in the report when
running the label validation test. To do this, you would need to run
tkvalid.pl. Then, select the "Change Label Validation Settings" button located
at the bottom of the GUI. (You will only be able to select this button provided
that you have clicked on the "List Label Validation errors" button first. Everytime 
this button is un-checked, the "Change Label Validation Settings" button will 
be unselectable as seen by its "grey'ed out" appearance). This will cause a GUI 
screen to appear which contains all the possible error and warning messages that 
the label validation test can generate. This screen also contains other options 
that allow a user to further control the output of the label validation test.

Choosing which messages you do not want to see is performed by clicking over
the message. This will cause that message to be highlighted, thus confirming what 
you have selected. If selecting more than one message, hold down the "Ctrl" key 
while clicking on the messages with your mouse.

Located towards the bottom right of this GUI are 4 selections to further control 
how a user wants the label validation error test to act. The selections are as 
follows:

   - Max Errors Allowed: The maximum number of error messages that the label 
     validation error test is allowed to make. Default is set at 300. Only input
     integer values (e.g. 1, 100, 200, 250) if you'd like to change this to 
     another value. 
     
   - Ignore All Warning Messages: Will prevent any and all warning messages from 
     being outputted to the final report.
     
   - Ignore Info Messages: Ignore any and all informational messages from being 
     outputted to the final report. Currently, there is only 1 possible type of
     informational message that this test can generate. 
     
   - Validate Only Files That Contain Level 3 Labels: This will allow the test to
     skip any non-labelled files (e.g. .jpeg, .bmp, .img, .txt). Default is ON. 
     This is recommended for users wanting to run a test through a directory that
     contains more than just label files. 
     
   - Retrieve/Clear Saved Messages: PVT has the ability to save the most recent 
     selections that the user has made in the types of messages they have
     selected to ignore. Basically, if a user selects certain messages, runs the
     test, exits the program, then re-runs the test again, the messages that were 
     chosen from the very latest run will have been already selected automatically. 
     So, what this button does is clear all those selections if it has been "checked". 
     "Un-checking" it will bring back all those messages that were chosen from the 
     previous run. 
      
Hitting "Done" at the bottom of the GUI will allow the test to continue with the
settings you've just selected. 

Once the validation test has been completed, a GUI window will appear that contains
all the different types of messages that were generated as a result of the test. The 
goal here is to allow a user to pick and choose those messages that they want to appear 
in the output report that will also show a list of the file names and line numbers that 
the particular message was generated for. These messages will be organized into 3 
different groups: errors, warnings, or info. Default is to show the error messages first. 
To browse through the other types of messages, click the corresponding radio button at the 
top of the GUI ("Errors", "Warnings", "Info"). Each line of message contains the number of 
occurrences and the number of files that particular message was generated for. If you do 
not highlight a message, the final report will just contain a brief summary of that message 
like so,

....
    COLUMN: Using PDS GENERIC object definition
         5 occurrences in 2 files
....

By highlighting a message, you will enable the script to print out the message including the 
exact file names and line numbers that the particular message is referring to. If you would 
like to see file names for all messages, simply click "Show filenames for all messages." 
Otherwise, click "Done" when you are finished with your selections. 


FOR LINUX AND SOLARIS USERS:

When running the GUI (tkvalid.pl) through these platforms, you will notice a slight 
difference in the interface than the Windows version. Below the "Check ISO9660 
Compliance" button, there will be a text box to enter the path to where the cdrom 
device is located. This box will only be enabled if a user wants to test a cd for 
iso compliance. Simply select "Check ISO9660 Compliance", then the text box below 
will become enabled. At that time, you can enter the path to the cdrom device.
For Linux users, this means that your cdrom device might be located along the 
lines of "/dev/cdrom". For Solaris users, your cdrom device might be located 
along the lines of "/vol/dev/dsk/..." Check with your System Administrator for the 
exact path if you are unsure.

If you are more of a command-line user and are running PVT through SOLARIS or LINUX,
the validate.pl script has changed slightly for these platforms as well. If you 
wanted to test a CD for ISO compliance, you would simply enter the following,

perl validate.pl -1 -c <cdrom device path> <volume root>


KNOWN BUGS:

 - Entering an incorrect path to the cd device may cause the program to "freeze" 
   unexpectedly. Simply halt the program if this occurs.  



ADDITIONAL NEW FEATURES TO VERSION 1.0:

 - The label validation test (listerr.pl, using lvtool) now prints out warning
   and informational messages as well.



Documentation can be found in the doc directory. The following files
are currently included:

  * pvt_users_guide.htm - commands and options for PVT in HTML format.
  * checklist.htm - the validation checklist in HTML format.
  * standards.txt - a copy of the PDS Standards Reference in text
    format (for easy searching).


II. CHANGE HISTORY

Please see the release_notes_pvt.txt file for the latest changes to
the Perl Validation Toolkit.


-----------
VERSION 3.0
-----------

* Updated PERL TOOLS User guide in the doc directory: pvt_users_guide.htm


tkvalid.pl
----------
   NEW FEATURE ADDED:
       * Specifying the "--lvt" flag on the command line will bring up a window that allows
         a user to select and save onto a file those messages that they want the label 
         validation test to ignore. This file can be used in validate.pl when doing batch
         processing.
   

validate.pl
-----------
   NEW COMMAND-LINE OPTIONS ADDED:
      
      --norw    Do not display the output report display window
      
      With -3 (label validation test) or -b (batch mode) enabled, the following options
      are also valid:
             --nos  Disable the window containing a summary of the messages found during
                    the label validation test. This would result in a more detailed output
                    report, where all associated filenames are listed under each message
                    found.
    --em <filename> Specify a text file containing messages that they you do not wish to see
                    in the label validation test report. File is generated by running
                    "tkvalid.pl --lvt", selecting messages to ignore, and choosing the save
                    option on the screen.
           --ninfo  Ignore all informational messages from the label validation test report
           --nwarn  Ignore all warning messages from the label validation test report
           --l3d    Only validate level 3 label files
      --max <value> Specify the maximum number of error messages to report before quitting
                    the label validation test, where <value> is a valid integer value 
                    greater than 0.

listerr.pl
----------

   NEW COMMAND-LINE OPTIONS ADDED:
      --em <filename> Specify a text file containing messages that they you do not wish
                      to see in the label validation test report. File is generated by 
                      running "tkvalid.pl --lvt", selecting messages to ignore, and 
                      choosing the save option on the screen.
               --nos  Disable the window containing a summary of the messages found during
                      the label validation test. This would result in a more detailed output
                      report, where all associated filenames are listed under each message
                      found.

-----------
VERSION 2.0
-----------

When running the label validation test either through validate.pl or tkvalid.pl,
a summary window appears after the test has finished that shows how often
a particular message occurred throughout the lvtool report. The purpose of this
window is to allow a user to tweak the report and be able to choose how much
detail to put in the final output report. By default, all associated filenames
pertaining to a message will be listed in the report as follows:

(#31) MAXIMUM_LONGITUDE: Not an allowed keyword. (It was not found on the optional
           or required keywords list)
           images/FF01.LBL: Line 42
           images/MDIM.LBL: Line 52
           images/i480009a.img: Line 176
           images/i497909a.img: Line 176
 
 Summary messages are printed as follows:
 
 (#31) MAXIMUM_LONGITUDE: Not an allowed keyword. (It was not found on the optional
           or required keywords list)
           4 occurrences in 4 files

validate.pl
-----------
   * Removed the "-s" flag as a command-line option
   
   NEW FEATURE ADDED:
      * A window displays at the end of the test run that shows the test results.
   
tkvalid.pl
----------
   * Code to create the label validation settings window has been moved
     to this file instead of validate.pl. This is to allow the window to
     appear right away instead of hitting "Run Tests", which didn't make
     much sense from a user point-of-view.


------------
VERSION 1.0
------------

This version allows a user to tweak the output of the final report from the
label validation test. A user will now be able to select from a list of 
messages that can appear in the report that they do not want to see. This
is executed by running tkvalid.pl.




II. INSTALLATION REQUIREMENTS

The PDS Validation Toolkit (PVT) runs on Windows, Solaris, and
Linux platforms that support perl5 and and the following PDS-maintained 
programs: isocheck, lvtool, and kwvtool.

The PVT distribution requires a perl5 installation, preferrably
perl 5.0004 or later. To run the perl/tk graphical interface, the Tk
module is also required. All other required files come with the
distribution itself. This includes the PVT perl scripts, the lvtool,
isocheck, and kwvtool executables (in the bin directory), data
dictionary files (in the data directory), the AllRefs.txt reference
database (also in the data directory), and documentation (in the doc)
directory.

Whenever a new PVT distribution is built, the latest versions of 
isocheck, lvtool, kwvtool, the data dictionary, and AllRefs.txt will
be included. However, each of these files can be updated independently
of the distribution.

The PVT distribution is currently available from the PDS Software
Download page via platform-specific installer package:

<URL: http://pds/tools/software_download.cfm>


III. INSTALLATION

Prior to installation:

1. Make sure you have perl 5.0004 or better installed on your machine.

2. If you want to run the graphical interface, make sure you have the
   appropriate perl/TK module installed.
   
3. Delete any old versions of lvtool, isocheck, and kwvtool.


Installation is simple:

1. Point your web browser to the URL referenced above.

2. Download the distribution of PVT for your platform.

3. Move the compressed distribution file to the location of your
   choice and extract all the files and directories.

4. Make sure the top level directory ("PVT") is on your executable
   PATH.


IV. KNOWN ISSUES

1. The label checker creates a list of labels it can't find. However,
   it does not parse labels for pointers and follow those pointers.
   Therefore, there are may be files on the list that are correctly
   pointed to in a lable. The work-around is to consider the list of
   unfound labels as a "suspect list," which must be confirmed by
   looking at the actual volume.
   
2. The listnew.pl script doesn't yet know how to deal with a 
   multi-line series of standard values, incorrectly listing a blank
   keyword for the second and subsequent lines.

3. The index checker handles multiple index tables, but does not 
   yet deal with multiple cumulative index tables.
   
4. There are table errors that the index checker should deal with more
   gracefully. For example, it should detect PATH_NAMES beginning
   with a slash (/), make the filename relative to the volume root,
   then report it as an error. It would also be better if the index
   checker reported filenames that aren't uppercase.
   
5. The reference checker (checkref.pl) occasionally finds something 
   in brackets, such as [K], in a *.CAT file and incorrectly lists it
   as a reference that's not cited in REF.CAT.
   
6. The reference checker (checkref.pl) doesn't yet deal with multiple
   reference files.
   
7. On Unix, if the full path to the volume root isn't given, 
   recursing through the directories gives strange results.
   
8. On NT, redirection doesn't seem to work when the tests are called
   directly. This is a perl/NT bug:
   
   checkidx.pl f:\ > report  # Gives an empty report
   perl checkidx.pl f:\ > report  # Works fine
   
   
V. AN ADDITIONAL TODO LIST

1. Modify validate.pl and tkvalid.pl so that if they don't find an
   index file or REF.CAT, the user is prompted to enter an alternate
   filename.

2. In the GUI (tkvalid.pl), capture the report as it is being
   generated in a separate window and allow it to be saved under
   any file name.
   
3. Continue to improve the existing tests and clean up the code.
   (For example, place common code blocks into a library and recurse
    only once through the disk.)

4. Write more complete documentation.

5. Complete the release distributions for Solaris and Linux.

6. As much as possible, automate the rest of the tests described in
   the Validation Checklist.
   
7. Implement the tests in a client-server paradigm, most likely using
   a Java signed-applet/servlet combination.
   
8. Redesign PDS as an on-line system that generates valid volumes on
   demand.

Questions and problems with the PVT can be addressed to Joel Wilf
at joel.wilf@jpl.nasa.gov.
