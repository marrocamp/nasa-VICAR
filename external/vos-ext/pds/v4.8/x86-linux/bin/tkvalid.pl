#!/usr/bin/perl
#
# Script: tkvalid.pl - perl/tk front end to the PDS Validation Tools
# Usage:  tkvalid.pl [-d]
#
# Author: Joel Wilf
# Date: 08-29-99
#
# Description:
#   This script is a perl/tk front end the PDS Validataion Tools.
#   It gets user input through dialog windows and runs the appropriate
#   validation tests automatically. If the -d option is set, it will
#   run in debug mode and only print out the volume root and tests
#   selected. NOTE: tkvalid.pl requires that the user's perl 
#   installation include the Tk module.
# End:
#
# Revisions:
#   2000-04-03, J. Wilf, Added ISOCHECK and KWVTOOL checks.
#   2002-03-11, J. Wilf, Changed default directory to current one
#   2003-04-29, M. Cayanan, Added another button called "Change Label
#     Validation Settings" to allow user to modify certain parameters
#     if the Label Validation test is run. Also took out any reference
#     to checklab test since lvtool already does this through the 
#     label validation test.
#   2003-09-19, M. Cayanan, Carried over code to display the Label 
#     Validation Settings GUI. This was previously stored in the 
#     validate.pl script, but would not be able to pop up unless the user
#     hit "Run Tests" on the screen. So, by transferring all code to this
#     script file, the GUI will be able to show once the user wants to change
#     the label validation test settings
#   2005-01-03, M. Cayanan, Add new option to save files that are created
#     when the validation test is run and a label contains a STRUCTURE pointer
#     that needs to be expanded
#   2006-02-16, M. Cayanan, Allow user to select/de-select aliasing feature for
#     the label validation test. It was always set to ON in prev. versions, but
#     it produces "funny" results if a label doesn't need aliasing.
#
# End:


use Tk;
use Tk::LabEntry;
use Tk::Dialog;
use Cwd;
use Getopt::Long;
Getopt::Long::Configure("bundling");
use File::Basename;
$| = 1;

### Check for debug flag
#getopts('d');  # If debug flag is set, $opt_d == 1

GetOptions("d" => \$opt_d,
           "lvt" => \$opt_lvt,
           "help" => \$opt_help
          );

$usage = "tkvalid.pl [--lvt]";

if($opt_help)
{
	print "\n\nPROGRAM USAGE: $usage\n\nwhere,\n";
	print "       --lvt\toption to just display a window in which a user can save\n";
	print "            \tmultiple messages they do not want to see when running the\n";
	print "            \tlabel validation test (lvtool program).\n\n";
	
	exit;
}
	
# Save current directory but run script from PVT home directory
$current_directory = getcwd();
$home_pvt = dirname($0);
chdir $home_pvt or die "Can\'t run from the PVT home directory\n";

#$opt_lvt = 1;

if($opt_lvt)
{
	create_only_msg_window();
}
else
{
	create_main_window();
}

MainLoop;




###########################
### Support Subroutines ###
###########################

#####################################
# Routine to create the main window #
#####################################

sub create_main_window
{
	### Create main window
	$mw = MainWindow->new();
	$mw->title("PDS Validation Tools Version 4.1");
	
# Help text for path selection
$explain_path_selection = <<"EOF1";
Enter the path to the root directory on the volume you want to
validate. Alternatively, you can press the 'BROWSE...' button
and open any file on your volume's root directory.
EOF1
	
# Help text for test selection
$explain_test_selection = <<"EOF2";
select any of the following tests to run. The 'Choose/Clear All'
button will select all the tests (unless all the tests are
already selected, in which case it will unselect all the tests).
EOF2
	
	### Frame 1 is for obtaining the volume root
	
	$frame1 = $mw->Frame(
	                     -borderwidth => 2,
	                     -relief => 'groove'
	                    )->pack(
	                    	    -anchor => 'nw',
	                            -padx => 10,
	                            -pady => 5
	                            );
	
	$frame1->Label(
	               -font => "Helvetica 18 normal",
	               -text => "Set Path to Volume Root"
	              )->pack(
	                      -anchor => 'w'
	                     );
	                     
	$frame1->Label(
	               -text => "$explain_path_selection",
	               -justify => 'left'
	              )->pack(
	                      -anchor => 'w'
	                     );
	
	$frame1->Entry(
	               -background => 'white',
	               -textvariable => \$root_dir
	              )->pack(
	                      -side => 'left',
	                      -padx => 5
	                     );
	              
	$root_dir = "e:\\"; # Default root directory
	
	$frame1->Button(
	                -text => "Browse...",
	                -command => \&get_root_dir
	               )->pack(
	                       -side => 'left',
	                       -pady => 5
	                     );
	
	### Frame 2 is for choosing the tests to run
	
	$frame2 = $mw->Frame(
	                     -borderwidth => 2,
	                     -relief => 'groove'
	                    )->pack(
	                            -anchor => 'w',
	                            -padx => 10,
	                            -pady => 5,
	                            -expand => 1,
	                            -fill => 'x'
	                           );
	
	$frame2->Label(
	               -font => "Helvetica 18 normal",
	               -text => "Choose Tests to Run",
	              )->pack(
	                      -anchor => 'w'
	                     );
	                 
	$frame2->Label(
	               -text => "$explain_test_selection",
	               -justify => 'left'
	              )->pack(
	                      -anchor => 'w'
	                     );
	
	$frame2->Button(
	                -text => "Choose/Clear All",
	                -command => \&choose_all,
	                )->pack(
	                        -anchor => 'w',
	                        -padx => 5
	                       );
	
	
	if( ($^O =~ m/linux/) || ($^O =~ m/solaris/) ) {
	
	    $frame2->Checkbutton(
	                         -text => "Check ISO9660 compliance",
	                         -variable => \$checkiso,
	                         -command => \&tbox_state,
	                         )->pack(
	                                 -anchor => 'w'
	                                );
	}
	else {
	    $frame2->Checkbutton(
	                         -text => "Check ISO9660 compliance",
	                         -variable => \$checkiso
	                        )->pack(
	                                -anchor => 'w'
	                               );
	}
	
	if( ($^O =~ m/linux/) || ($^O =~ m/solaris/) ) {
	
	     $frame2->Label(
	                    -text => "Enter the path to the cdrom device below: (i.e. /dev/cdrom)",
	                    -justify => 'left'
	                   )->pack(
	                           -anchor => 'w'
	                          );
	
	     $tbox = $frame2->Entry(
	                    -background => 'light gray',
	                    -textvariable => \$cd_dir,
	                    -state => 'disabled'
	                    )->pack(
	                            -anchor => 'w',
	                            -padx => 5
	                           );
	}
	
	$frame2->Checkbutton(
	                     -text => "Check for required files",
	                     -variable => \$checkreq
	                    )->pack(
	                            -anchor => 'w'
	                           );
	
	$cb = $frame2->Checkbutton(
			   -text => "Label Validation Test(lvtool program)",
			   -variable => \$listerr,
			   -command => \&disp_lvt_settings
			   )->pack(
			   	   -anchor => 'w'
			      );
	
	$frame2->Checkbutton(
	                     -text => "List new keywords",
	                     -variable => \$listnew
	                     )->pack(
	                             -anchor => 'w'
	                            );
	
	$frame2->Checkbutton(
	                     -text => "Check the index files",
	                     -variable => \$checkidx
	                    )->pack(
	                            -anchor => 'w'
	                           );
	
	$frame2->Checkbutton(
	                     -text => "Test formatting in ASCII files",
	                     -variable => \$testline
	                    )->pack(
	                            -anchor => 'w'
	                           );
	
	$frame2->Checkbutton(
	                     -text => "Check references",
	                     -variable => \$checkref
	                    )->pack(
	                            -anchor => 'w'
	                           );
	
	$frame2->Checkbutton(
	                     -text => "List keyword/value pairs",
	                     -variable => \$listkwv
	                    )->pack(
	                            -anchor => 'w'
	                           );
	
	### Frame 3 run the tests or exits
	
	$frame3 = $mw->Frame(
	                     -borderwidth => 2,
	                    )->pack(
	                            -anchor => 'w',
	                            -padx => 10,
	                            -expand => 1,
	                            -fill => 'x'             
	                           );
	
	$frame3->Button(
	            -text => "Run Tests",
	            -command => sub {
	            	if($listerr == 1) {
	            		set_lvt_options();
	            	}
	            	run_tests();
	              }
	               )->pack(
	                   -side => 'left'
	                  );
	
	$frame3->Button(
	                -text => "Exit",
	                -command => sub { exit }
	                )->pack(
	                        -side => 'right'
	                       );
}


####################################################
# Routine to display just a window of the messages #
# the user can choose not to see in a report.      #
####################################################
sub create_only_msg_window
{
	$tl = MainWindow->new();
	$tl->title("Select Messages To Omit");
	
	create_msg_window();	
}

# Get the root directory through a file-browsing dialog
sub get_root_dir {

    $file = $frame1->getOpenFile();
    $file =~ s#\/[^/]+$#\/#; # Get directory from filename
    $file = lc $file if ($file =~ m/\w:/); # Another evil Windows hack
    $root_dir = $file;
}

# Choose or clear all the checkboxes
sub choose_all {
	
    # Get the sum of all the tests selected
    $first5 = $checkiso + $checkreq + $checklab + $listerr + $listnew;
    $last4 = $checkidx + $testline + $checkref + $listkwv;
    $all = 0 + $first5 + $last4;
    
    # If all the tests are selected, clear them
    if ($all == 9) {
        $checkiso = 0;
        $checkreq = 0;
        $checklab = 0;
        $listerr = 0;
        $listnew = 0;
        $checkidx = 0;
        $testline = 0;
        $checkref = 0;
        $listkwv = 0;
    # Otherwise, select all the tests
    } else {
        $checkiso = 1;
        $checkreq = 1;
        $checklab = 1;
        $listerr = 1;
        $listnew = 1;
        $checkidx = 1;
        $testline = 1;
        $checkref = 1;
        $listkwv = 1;
    }
    
    disp_lvt_settings();
    
    if( ($^O =~ m/solaris/) || ($^O =~ m/linux/) ) {
    	tbox_state();
    }
}

# Run the validation tests
sub run_tests {
	
    # Get the sum of all the tests selected
    $first5 = $checkiso + $checkreq + $checklab + $listerr + $listnew;
    $last4 = $checkidx + $testline + $checkref + $listkwv;
    $all = 0 + $first5 + $last4;
    
    # If the volume root directory can't be found, give error dialog
    unless (-d $root_dir) {
        &error_dialog("Can\'t open volume root: $root_dir");
        return;
    }
    
    # If no tests are specified, give error dialog
    if ($all == 0) {
        &error_dialog("No tests chosen");
        return;
    }
    
    # If validate.pl isn't present, give error dialog
    unless (-e "validate.pl") {
        &error_dialog("Can\'t find validate.pl");
    }
    
    if( ($^O =~ m/linux/) || ($^O =~ m/solaris/) ) {
    	if( ($checkiso == 1) && ($cd_dir eq "") ) {
    	   &error_dialog("Path to the cdrom device not specified");
    	   return;
    	}
    }
    $mw->withdraw();
    
    if($tl != NULL)
    {
    	$tl->destroy();
    	$tl = "";
        cleanup_tl_vars();
    }   
    
    # Build the command string and run it
    my $command = "perl validate.pl -s --rw -";
    $command = $command . "1" if ($checkiso == 1);
    $command = $command . "2" if ($checkreq == 1);
    $command = $command . "3" if ($listerr == 1);
    $command = $command . "4" if ($listnew == 1);
    $command = $command . "5" if ($checkidx == 1);
    $command = $command . "6" if ($testline == 1);
    $command = $command . "7" if ($checkref == 1);
    $command = $command . "8" if ($listkwv == 1);
    $command = "perl validate.pl -b" if ($all == 9);
    
    if ( $listerr ==1 ) {
    	    $command = $command . "$command_opts";
    	    $cb->deselect();
    }
    
    #MDC - Need double quotes surrounding the root directory and report directory because of the possiblity of the
    # name containing spaces (i.e. C:\Documents and Settings\...) This fixes a previous bug in the PERL tools
    $command = $command . " " . "-c" . " " . $cd_dir if ( ($checkiso == 1) && (($^O =~ m/linux/) || ($^O =~ m/solaris/)) );
    $command = $command . " " . $root_dir . " " . "\"$current_directory\"";
    print "Command = $command\n" if ($opt_d);
    system("$command") unless ($opt_d);
    $mw->deiconify();
    $mw->raise();
}

# Create error dialog
sub error_dialog
{

    my $message = shift @_;
    my $mainwindow;
    
    # $tl and $mw variables is either the 
    # main window or message screen
    if($opt_lvt)
    {
    	$mainwindow = $tl;
    }
    else
    {
    	$mainwindow = $mw;
    }
    
    $mainwindow->Dialog(
                        -title => 'ERROR',
                        -text => "$message",
                        -bitmap => 'error',
                        -default_button => 'OK',
                        -buttons        => ['OK']
                        )->Show();
}


# This routine is only used for non-Windows machines because of the 
# need for an extra field to properly do iso_checking in Solaris
# and Linux.
sub tbox_state
{
    if($checkiso)
    {
         $tbox->configure(-background => 'white',
			  -state => 'normal'
			  );
    }
    else
    {
	$tbox->configure(-background => 'light gray',
			 -state => 'disabled'
			);
	$cd_dir="";
    }
}


#################################################################
# Routine to display a variety of options that the user can use #
# to tweak the output of the label validation report.           #
#################################################################
sub disp_lvt_settings
{
   if($listerr == 0)
   {
   	if($tl != NULL)
   	{
   		$tl->destroy();
   		$tl ="";
   	}
      	return;
   }
   else
   {
   	if($tl != NULL)
   	{
   		$tl->deiconify();
   		$tl->raise();
   		return;
   	}
   }
   
  # $clear_logfile = 0;
   $no_info = 0;
   $no_warnings = 0;


   ### Create main window
   $tl = $mw->Toplevel();
   $tl->title("Label Validation Settings");
    
#   $logfile = "logfile";
 
 
   # Used for the "Done" button
   
   $frame4_tl = $tl->Frame(
   		      -borderwidth => 4
   		       )->pack(
   		       	      -side => 'bottom',
   		       	      -fill => 'x'
   		       	      );
   
   $frame4_tl->Button(
   		  -text => "Run Tests",
   		  -relief => 'raised',
   		  -padx => 10,
   		  -command => sub { if($listerr == 1) { set_lvt_options();} run_tests();}
   		  )->pack(
   		  	 -pady => 5,
   		  	 -padx => 15,
   		  	 -side => 'left'
   		  	 );
   
   $frame4_tl->Button(
   		 -text => "Back To Main Menu",
   		 -relief => 'raised',
   		 -padx => 10,
   		 -command => sub{ $tl->withdraw(); }
   		 )->pack(
   		 	-pady => 5,
   		 	-padx => 15,
   		 	-side => 'right'
   		 	);
 
    ### Frame 3 to modify lvtool output options

   $frame3_tl = $tl->Frame(
                     -borderwidth => 2,
                 #    -relief => 'sunken'
                        )->pack(
                            -padx => 5,
                            -pady => 5,
                            -side => 'left',
                            -fill => 'y'
                           );
   
   # Initialize Entry widget with default value of '300' max errors
   $max_err = 300;
   $frame3_tl->LabEntry(-label => "Max Errors Allowed:", 
   		     -textvariable => \$max_err,
   		     -background => 'white',
   		     -labelPack => [ -side => 'left', -anchor => 'w' ]
   		     )->pack(
   		     	     -pady => 15,
   		     	     -side => 'top',
   		     	     -anchor => 'nw');
   
   $frame3_tl->Checkbutton(
                     -text => "Ignore All Warning Messages",
                     -variable => \$no_warnings
                    )->pack(
                            -anchor => 'w',
                            -pady => 15
                           );
                           
   $frame3_tl->Checkbutton(
   	 	     -text => "Ignore Info Messages",
   	 	     -variable => \$no_info
   	            )->pack(
   	            	    -anchor => 'w',
   	            	    -pady => 15
   	            	    );
   
   $l3d = 1;
   $frame3_tl->Checkbutton(
   		     -text => "Validate Only Files That Contain Level 3 Labels",
   		     -variable => \$l3d
   		    )->pack(
   		    	   -anchor => 'w',
   		    	   -pady => 15
   		    	   );
   
   # 02-16-06 MDC - New option added 
   $alias = 1;
   $frame3_tl->Checkbutton(
                     -text => "Enable aliasing",
                     -variable => \$alias
                    )->pack(
                           -anchor => 'w',
                           -pady => 15
                           );
   
   $frame3_tl->Checkbutton(
                     -text => "Save Expanded Files",
                     -variable => \$save_exp_files
                    )->pack(
                           -anchor => 'w',
                           -pady => 15
                           );
    
   $frame3_tl->Button(
   	              -text => "Clear All Selected Messages",
   		      -relief => 'raised',
   		      -padx => 10,
   		      -command => sub{ do_clear_all(); }
   		     )->pack(
   		 	     -fill => 'x',
   		 	     -side => 'bottom'
   		 	    );
   	            	    
#   $frame3_tl->Checkbutton(
#   		     -text => "Retrieve/Clear Saved Messages",
#   		     -variable => \$clear_logfile,
#   		     -command => sub {
#   		        if($clear_logfile) {
#   		           clear_msgs();
#   		        }
#   		        else {
#   		           retrieve_saved_msgs();
#   		        }
#   		      }
#   		     )->pack(
#   		     	    -anchor => 'w',
#   		     	    -pady => 15
#   		     	    );
   
   create_msg_window();
    
   Mainloop;
   
}

#################################################
# Routine to create a display window that shows #
# messages that a user can omit from the        #
# report.                                       #
#################################################
sub create_msg_window
{
	
$explain_window = <<"EOF3";
Select the types of messages that you would like the label validation test to
ignore. Holding down the cntl key while clicking on the messages with your mouse
will allow you to make multiple selections.
EOF3

    # Display a menubar only if the user just wanted to see the list of messages window
    if($opt_lvt)
    {
    	create_msg_win_menubar();
    }
    	
 
   # Display header
   
   $top_frame = $tl->Frame(
                     -borderwidth => 1,
                     -relief => 'flat',
                     )->pack(
                             -padx => 1,
                             -pady => 1,
                             -side => 'top',
                             -fill => 'x'
                            );
   
   $top_frame->Label(
               -text => "$explain_window",
               -justify => 'left'
              )->pack(
                      -anchor => 'w'
                     );
	
	# Display listbox with warning messages
	   
	$frame2_tl = $tl->Frame(
	                    -borderwidth => 4,
	                    -relief => 'groove'
	                   )->pack(
	                           -padx => 5,
	                           -pady => 5,
	                           -side => 'top',
	                           -fill => 'both'
	                          );
	
	
	# Frame 1 to display list box with error messages
	    
	$frame1_tl = $tl->Frame(
	                    -borderwidth => 4,
	                    -relief => 'groove',
	                   )->pack(
	                           -padx => 5,
	                           -pady => 5,
	                           -side => 'bottom',
	                           -fill => 'both'
	                          );  
	  
	  
	 $frame1_tl->Label(
	               -font => "Helvetica 14 normal",
	               -text => "Possible Error Messages"
	              )->pack(
	                      -anchor => 'w'
	                     );
	                         
	 $lb1 = $frame1_tl->Scrolled( "Listbox", 
	   			-scrollbars => "se", 
	   			-height => 15, 
	  			-selectmode => "extended",
	  			-exportselection => 0,
	  			-width => 85,
	  			-relief => 'sunken',
	   			-background => 'white',
	   			-highlightcolor => 'blue',
	   			-yscrollcommand => ['set' => $yscrollbar],
	   			-xscrollcommand => ['set' => $xscrollbar]
	   		      )->pack(
	  		              -side => 'left',
	   		              -expand => 1,
	   		              -fill => 'both'
	   		             );
	   
	 # Frame 2 to display the different types of messages that can appear in a label validation
	 # Test.   
	 $frame2_tl->Label(
	               -font => "Helvetica 14 normal",
	               -text => "Possible Warning Messages"
	              )->pack(
	                      -anchor => 'w'
	                     );
	                     
	 $lb2 = $frame2_tl->Scrolled( "Listbox", 
	   			-scrollbars => "se", 
	   			-height => 15,
	  			-selectmode => "extended",
	  			-exportselection => 0,
	  			-width => 85,
	  			-relief => 'sunken',
	   			-background => 'white',
	   			-highlightcolor => 'blue',
	   			-yscrollcommand => ['set' => $yscrollbar],
	   			-xscrollcommand => ['set' => $xscrollbar]
	   		      )->pack(
	   		               -side => 'left',
	   		               -expand => 1,
	   		               -fill => 'both'
	   		             );
	   		             
	 # Grab the messages in message_list.txt       
	 open(MSGFILE, "data/message_list.txt") || die("can't open data/message_list.txt file"); 
	   
	 #First, grab the error message list from the file
	 while( chop($line = <MSGFILE>) && ($line !~ /END OF ERROR LIST/) )
	 {
	 	if($line !~ /POSSIBLE ERROR MESSAGES/)
	   	{
	   		@errmsgs = split /=\s+/, $line;
	   		
	   		#display_error_list is the list that the user sees in the GUI
	   		
	   		#real_error_list is the list that will be used to do the filtering
	   		#of unwanted messages
	   		push @display_error_list, $errmsgs[0];
	   		push @real_error_list, $errmsgs[1];
	   	}
	 }
	   
	 #Next, grab the warning message list from the file
	 #Skip blank lines first and header for the warnings list
	 while( ($line = <MSGFILE>)  && ($line !~ /END OF WARNING LIST/) )
	 {
	 	#skip blank lines
	   	if($line eq "\n") {
	   	    next;
	   	}
	   	
	   	chop($line);
	   	
	   	if($line !~ /POSSIBLE WARNING MESSAGES/)
	   	{
	   	     @warnmsgs = split /=\s+/, $line;
	   	     
	
	   	     push @display_warning_list, $warnmsgs[0];
	   	     push @real_warning_list, $warnmsgs[1];
	   	}
	 }
	   
	 # insert error and warning lists onto the listbox
	 $lb1->insert(0, @display_error_list);  
	 $lb2->insert(0, @display_warning_list);
	   
	 close MSGFILE;
	   
}


###################################################
# Routine to create a menubar for the window that #
# will display messages that a user can choose to #
# not see in a validation report.                 #
###################################################
sub create_msg_win_menubar
{
	# Create menubar
    	$menubar = $tl->Frame(
    		              -relief => 'ridge',
                 	      -borderwidth => 2
                             )->pack(
                            	    -side => 'top',
                                    -anchor => 'n',
                                    -expand => 1,
                                    -fill => 'x'
                                     );
    
    	$menu1 = $menubar->Menubutton(
        	                      -text => "File",
                	              -relief => 'flat',
                        	      -tearoff => 0,
                               	      -menuitems => [[ 'command' => "Save Selections",
                                		        -command => \&do_autosave ],
                                	       	     [ 'command' => "Save Selections as...",
                                                        -command => \&do_saveas ],
                                               	     [ 'command' => "Load Selections",
                                                        -command => \&do_load ],
                                                     "-",
                                               	     [ 'command' => "Exit",
                                                 	-command => sub {exit;} ]]
                               	      )->pack(
                                      	      -side => 'left'
                                      	     );
   	
   	# Disables "Save Selections" item initially
   	$menu1->entryconfigure(0,-state => 'disabled');
   	
   	$menu2 = $menubar->Menubutton(
   		        	      -text => "Options",
   				      -relief => 'flat',
   				      -tearoff => 0,
   				      -menuitems => [[ 'command' => "Select All Messages",
   				      	                -command => \&do_select_all ],
   				           	     [ 'command' => "Clear All Messages",
   				                	-command => \&do_clear_all ],
   				                      "-",
   				                     [ 'command' => "Select All Error Messages",
   				                  	-command => \&do_select_all_errors],
   				                     [ 'command' => "Select All Warning Messages",
   				                  	-command => \&do_select_all_warnings ]]
   				      )->pack(
   				      	      -side => 'left'
   				       	      );
}

###############################
# Clear all messages selected #
###############################
sub do_clear_all
{
	$lb1->selectionClear(0, "end");
    	$lb2->selectionClear(0, "end");
}

#######################
# Select all messages #
#######################
sub do_select_all
{
	$lb1->selectionSet(0, 'end');
	$lb2->selectionSet(0, 'end');
}

#############################
# Select all error messages #
#############################
sub do_select_all_errors
{
	$lb1->selectionSet(0, 'end');
}

###############################
# Select all warning messages #
###############################
sub do_select_all_warnings
{
	$lb2->selectionSet(0, 'end');
}		

###########################################
# Load a previously saved file containing #
# selection of messages                   #
###########################################
sub do_load
{
	my $selected_file;
	
	$selected_file = $tl->getOpenFile();
	
	retrieve_saved_msgs($selected_file);
}

###########################################################
# Look at the logfile and retrieve any messages that were #
# saved. The messages that were saved will be highlighted #
# in the settings screen.                                 #
###########################################################

sub retrieve_saved_msgs
{
	my $file_to_read = $_[0];
	
	if( open(SAVEDMSGS, "<$file_to_read") )
	{
		while( $line = <SAVEDMSGS> )
		{
			chop ($line);
   			$counter = 0;
   			foreach $msg (@real_error_list)
   			{
   				if( $line eq $msg )
   				{
   					$lb1->selectionSet($counter);
   					last;
   		    		}
   		    		++$counter;
   			}
   		
   			$counter = 0;
   			foreach $msg (@real_warning_list)
   			{
   		    		if( $line eq $msg )
   		    		{
   		    			$lb2->selectionSet($counter);
   		    			last;
   		    		}
   		    		++$counter;
   			}
   		
   		}
   		close SAVEDMSGS;
   		# Enable "Save Selections" item 
   		$menu1->entryconfigure(0,-state => 'normal');
   		$filename = $file_to_read;
    	}
    	else
    	{
    		&error_dialog("Cannot Open File: $file_to_read");
    	}
}	

########################################################################################
# Subroutine to call right when the user has finished selecting their desired settings #
########################################################################################
sub set_lvt_options
{
    
    # Enter in more command line options to label validation test
    # if they were selected
    
    $command_opts = $command_opts . " --nol3d" if($l3d == 0);  
    $command_opts = $command_opts . " --nwarn" if($no_warnings);
    $command_opts = $command_opts . " --ninfo" if($no_info);
    $command_opts = $command_opts . " --max " . "$max_err" if($max_err =~ /\d/);
    $command_opts = $command_opts . " --se" if($save_exp_files);
    $command_opts = $command_opts . " --nalias" if($alias == 0);
 #   $command_opts = $command_opts . " --lf ";

}

###################################################################
# Subroutine to clear all the messages selected in the GUI screen #
###################################################################
sub clear_msgs
{
	
    if($clear_logfile) {
    	$lb1->selectionClear(0, "end");
    	$lb2->selectionClear(0, "end");
    }

}


###############################################
# Cleanup arrays used in the top-level window #
###############################################
sub cleanup_tl_vars
{
	
	@ignore_errs = ();
	@ignore_warns = ();
	@display_error_list = ();
	@display_warning_list = ();
	@real_warning_list = ();
	@real_error_list = ();
}

#######################################################
# Subroutine to call right when the user has finished #
# selecting their desired settings                    #
#######################################################
sub save_selections
{
	# If string exists...
	if($_[0])
	{
		$filename = $_[0];
	}
	else # If no string...
	{
		return;
	}

   	#Retrieve the error and warning messages the user selected to ignore
    	@ignore_errs = $lb1->curselection();
    	@ignore_warns = $lb2->curselection();
    
    	if( ((@ignore_errs + 0) != 0) || ((@ignore_warns + 0) != 0) )
    	{
    		open(FILEHANDLE, ">$filename") or die "Can't open file: $filename";
       
       		foreach(@ignore_errs)
       		{
    	  		print FILEHANDLE "$real_error_list[$_]\n";
       		}
    
       		foreach(@ignore_warns)
       		{
    	  		print FILEHANDLE "$real_warning_list[$_]\n"; 
       		}
    
       		close FILEHANDLE;
       		
       		# Enable "Save Selections" item 
   		$menu1->entryconfigure(0,-state => 'normal');
    	}
    	else
    	{
    		&error_dialog("No Messages Were Chosen To Be Saved");
    	}
    
}

######################################
# Save selections in a file selected #
# by the user.                       #
######################################
sub do_saveas()
{
	$sfile = $tl->getSaveFile();
	
	save_selections($sfile);
}

sub do_autosave()
{
	if(-e $filename)
	{
		overwrite_dialog($filename);
	}
}

sub overwrite_dialog
{
	my $mainwindow;
    
    	# $tl and $mw variables is either the 
    	# main window or message screen
    	if($opt_lvt)
    	{
    		$mainwindow = $tl;
    	}
    	else
    	{
    		$mainwindow = $mw;
    	}
    
    	my $ow_dialog = $mainwindow->Dialog(
            	                         -title => 'OVERWRITE FILE?',
                	                 -text => "Overwrite existing file: $filename?",
                       	                 -bitmap => 'warning',
                                         -buttons => ["YES","NO"]
                           );
        my $ow_buttons = $ow_dialog->Show();
        
        if ($ow_buttons eq "YES")
        {
        	save_selections($filename);
        	success_dialog("Selections have been saved to $filename");
        }
        else # User selected "NO"
        {
        	# Do Nothing
        	return;
        }
}

sub success_dialog()
{
    my $message = shift @_;
    my $mainwindow;
    
    # $tl and $mw variables is either the 
    # main window or message screen
    if($opt_lvt)
    {
    	$mainwindow = $tl;
    }
    else
    {
    	$mainwindow = $mw;
    }
    
    $mainwindow->Dialog(
                        -title => 'MESSAGE SAVED',
                        -text => "$message",
                        -bitmap => 'info',
                        -default_button => 'OK',
                        -buttons        => ['OK']
                        )->Show();
}	