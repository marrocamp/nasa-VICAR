#!/usr/bin/perl
#
# Script: validate.pl - Runs validation tests on a volume
# Usage:  validate.pl [-b] [-123456789] [-n] [-lf] [--lvt] <volume root> [report directory]
#         With --lvt specified: [--ninfo] [--nwarn] [--max] <int_value> [--nol3d] [--nalias]
#
# Author: Joel Wilf
# Date: 7/29/99
#
# Description:
#   This script can run any or all of the following validation tests.
#   (Note that any of these tests can be run individually, using the
#   programs shown in parentheses, below:)
#
#     1) check ISO9660 compliance (isocheck, checkiso.pl)
#     2) check for required files and directories (checkreq.pl)
#     3) check whether every file has a label (checklab.pl)
#     4) find label validation errors (lvtool, listerr.pl)
#     5) find new keywords and standard values (lvtool, listnew.pl)
#     6) check that the index file points to the data (checkidx.pl)
#     7) test for errors in ASCII files (testline.pl)
#     8) check references against reference database (checkref.pl)
#     9) list keyword/value pairs for inspection (kwvtool, listkwv.pl)
#
#   There are two ways to run validate.pl. The first is interactively,
#   where the only input is the root directory of the volume to be
#   validated. The script prompts the user, asking which tests to
#   perform.
#
#   The script can also be run in "batch mode," by setting the -b
#   option. If -b is set, all tests are run automatically, no
#   questions asked. If -<some numbers> is set (and -b is not), then
#   the tests corresponding to the numbers given are run.  For
#   example, -b -1357 runs the tests numbered 1, 3, 5, and 7, as
#   defined above.
#
#   An additional option was added to account for the "pseudo PDS"
#   attached labels used by NAIF. If processing NAIF volumes, use
#   this -n flag.
#
#   The volume root (the path to the root directory of the volume) is
#   required. The test report will have the name "<VOLUME_ID.rpt>" and
#   will be placed in the current working directory, unless the 
#   [report directory] is specified.
# End:
#
# Revisions:
#   1999-08-29, J. Wilf, Added option to select tests in batch mode
#   1999-10-02, J. Wilf, Added option to specify report directory
#   2000-03-31, J. Wilf, Added option for NAIF special case
#   2000-04-03, J. Wilf, Added ISOCHECK and KWVTOOL checks
#   2002-03-12, J. Wilf, Changed default directory to current one
#   2003-03-12, M. Cayanan, Added another option, -s, to allow user to
#		configure the label validation error test. 
#   2003-04-15, M. Cayanan, Added a GUI display that allows users to 
#               configure the label validation error test, exclude 
#               warnings, info, and error messages to the users liking.
#   2003-04-17, M. Cayanan, Removed any code dealing with checklab.pl since
#               lvtool does a better job of checking for files with labels. 
#   2005-01-03, M. Cayanan, Default mode reverts back to where no popup windows
#               display unless specified by the user
#   2006-01-27, M. Cayanan, Add quotes around lvtool command-line input for
#               non-windows systems so that it can recursively search through
#               sub-directories for files
#   2006-02-16, M. Cayanan, Allow user to deselect aliasing feature for the label
#               validation test. It is set to ON by default.
# End:

use Tk;
use Cwd;
use Getopt::Long;
Getopt::Long::Configure("bundling");
use File::Find;
use File::Basename;
$| = 1;

# Save current directory but run script from PVT home directory
$current_directory = getcwd();
$home_pvt = dirname($0);
chdir $home_pvt or die "Can\'t run from the PVT home directory\n";


# Solaris and Linux versions
if( ($^O =~ m/linux/) || ($^O =~ m/solaris/) ) {
	$usage = "validate.pl [-b] [-12345678] [--rw] [-n] [-c] <cd-rom device> <volume root> <report directory>";
	# getopts('b12345678nc'); # If -b is set, $opt_b == 1, etc.
	GetOptions("ninfo" => \$opt_ninfo,
	           "rw"  => \$opt_rw,
   		   "nwarn" => \$opt_nwarn,
   		   "s"  =>  \$opt_s,
   		   "se" =>  \$opt_se,
   		   "nol3d"  => \$opt_nol3d,
   		   "nalias" => \$opt_nalias,
   		   "em=s" => \$opt_em,
   		   "max=i" => \$opt_max,
   		   "b" => \$opt_b,
		   "1" => \$opt_1,
		   "2" => \$opt_2,
		   "3" => \$opt_3,
		   "4" => \$opt_4,
		   "5" => \$opt_5,
		   "6" => \$opt_6,
		   "7" => \$opt_7,
		   "8" => \$opt_8,
		   "n" => \$opt_n,
		   "c" => \$opt_c,
		   "help" => \$opt_help
   		  );
}
else {  # Windows Version
	$usage = "validate.pl [-b] [-12345678] [-n] [--rw] <volume root> <report directory>";
	#  getopts('b12345678n'); # If -b is set, $opt_b == 1, etc.
	GetOptions("ninfo" => \$opt_ninfo,
	           "rw"  => \$opt_rw,
		   "nwarn" => \$opt_nwarn,
		   "s"   => \$opt_s,
		   "se"  => \$opt_se,
		   "nol3d"   => \$opt_nol3d,
		   "nalias" => \$opt_nalias,
		   "em=s" => \$opt_em,
		   "max=i" => \$opt_max,
		   "b" => \$opt_b,
		   "1" => \$opt_1,
		   "2" => \$opt_2,
		   "3" => \$opt_3,
		   "4" => \$opt_4,
		   "5" => \$opt_5,
		   "6" => \$opt_6,
		   "7" => \$opt_7,
		   "8" => \$opt_8,
		   "n" => \$opt_n,
		   "help" => \$opt_help
		   );
}

$usage = $usage . "\n\nWith -3 specified, these options are also valid: \n[-s] [--se] [--em] <filename> [--ninfo] [--nwarn] [--nol3d] [--nalias] [--max] <int_value>"; 

if($opt_help)
{
	print"\n\nPROGRAM USAGE: $usage\n\nwhere,\n";
	print"\nInteger values (1 thru 8) refer to the different tests that can\n";
	print"be run:\n";
	print"      -1     check ISO9660 compliance (isocheck, checkiso.pl)\n";
        print"      -2     check for required files and directories (checkreq.pl)\n";
        print"      -3     find label validation errors (lvtool, listerr.pl)\n";
        print"      -4     find new keywords and standard values (lvtool, listnew.pl)\n";
        print"      -5     check that the index file points to the data (checkidx.pl)\n";
        print"      -6     test for errors in ASCII files (testline.pl)\n";
        print"      -7     check references against reference database (checkref.pl)\n";
        print"      -8     list keyword/value pairs for inspection (kwvtool, listkwv.pl)\n\n";
        print"      -b     Run tests (1-8), no questions asked.\n"; 
	print"    --rw     Display a window that shows the results of the tests performed.\n"; 
	print"\nAdditionally, if -3 is specified, these command line options are valid:\n";
	print"  --ninfo         Ignore all informational messages from lvtool\n"; 
	print"                  report\n";
	print"  --nwarn         Ignore all warning messsages from lvtool report\n";
	print"  --max <integer> Specify an integer value stating the maximum number\n";
	print"                  of errors to report before quitting the label\n";
	print"                  validation test.\n";
	print"  --em <filename> Specify a file containing a list of messages to ignore\n";
	print"                  in the validation test report. This can be generated\n";
	print"                  using \"tkvalid.pl --lvt\".\n";
	print"   -s             This option will enable a window containing a summary of\n"; 
	print"                  the messages found during the label validation test. With\n";
	print"                  this option turned off, all associated filenames pertaining\n";
	print"                  to each message found in the label validation test will be\n";
	print"                  displayed.\n";
	print"  --nol3d         Also validate non-level 3 label files. Default is to only\n";
	print"                  validate level 3 labels\n";
	print"  --nalias        Disable the aliasing feature (Default is to enable aliasing)\n";
	print"  --se            Save files created when a label containing STRUCTURE\n";
	print"                  pointers is expanded.\n";


	
	exit;
}

$naif_option = "";	# No NAIF option unless explicitly set

if( ($^O =~ m/linux/) || ($^O =~ m/solaris/) ) {
   if($opt_c == 1) {
      $cd_dir = shift or die "$usage\n";
   }
}
	
$root_dir = shift or die "USAGE: $usage\n";
die "Can\'t find $root_dir\n" unless (-e $root_dir);
die "$root_dir isn't a directory\n" unless (-d $root_dir);

# Get VOLDESC.CAT (ignoring case)
$voldesc = "$root_dir/voldesc.cat" if (-e "$root_dir/voldesc.cat");
$voldesc = "$root_dir/VOLDESC.CAT" if (-e "$root_dir/VOLDESC.CAT");

# Use VOLDESC.CAT to get the VOLUME ID
if ($voldesc) {
    open (VOLDESC,"<$voldesc") or die "Can\'t open $voldesc\n";
    while ($line = <VOLDESC>) {
        if ($line =~ m/VOLUME_ID\s+=\s+\"*(\w+)/) {
            $volume_id = $1;
            last;
        }
    }
    close VOLDESC;
}
unless ($volume_id) {
#   $volume_id = "NO_VOLUME_ID";
    $volume_id = "validation";
    print "ERROR: There should be a VOLDESC.CAT with a VOLUME ID\n\n";
}

# Set up output reports
$report = "$volume_id" . '.rpt';
$kwv_report = 'kwv-' . "$report";
$iso_report = 'iso-' . "$report";
$report_dir = shift or ($report_dir = $current_directory);
$report = $report_dir . "/" . $report if ($report_dir);
$kwv_report = $report_dir . "/" . $kwv_report if ($report_dir);
$iso_report = $report_dir . "/" . $iso_report if ($report_dir);

#for windows, replace '/' with '\'
if( ($^O !~ m/solaris/) && ($^O !~ m/linux/) ) {
   if($root_dir =~ m/\//) {
	$root_dir =~ s/\//\\/g;
   }
#   $check = $root_dir;
#   $last_char = chop($check);
#   if( ($last_char eq "/") || ($last_char eq "\\") ) {
#	chop($root_dir);
#   }
}


# Open report file and announce presence

open (REPORT,">$report") or die "Can\'t open report file: $report\n";
print REPORT "************************************\n";
print REPORT "* Validation Report for $volume_id *\n";
print REPORT "************************************\n\n";
close REPORT;  # So system calls can add to it
print "\nValidating $volume_id:\n\n";

# Find out which tests to run
$ask_me = 1;
if ($opt_b or $opt_1) {
    $run_iso = 1;
    $ask_me = 0;
}
if ($opt_b or $opt_2) {
    $run_req = 1;
    $ask_me = 0;
}
if ($opt_b or $opt_3) {
    $run_err = 1;
    $ask_me = 0;
}
if ($opt_b or $opt_4) {
    $run_new = 1;
    $ask_me = 0;
}
if ($opt_b or $opt_5) {
    $run_idx = 1;
    $ask_me = 0;
}
if ($opt_b or $opt_6) {
    $run_asc = 1;
    $ask_me = 0;
}
if ($opt_b or $opt_7) {
    $run_ref = 1;
    $ask_me = 0;
}
if ($opt_b or $opt_8) {
    $run_kwv = 1;
    $ask_me = 0;
}

# Added by MDC 04-01-03
# Set up command line for label validation test a.k.a. lvtool

setup_lvtool_args();

# If no options are set, find out interactively
if ($ask_me == 1) {
    $help = '(Enter "y" for yes, "n" for no, "q" to quit): ';
    $run_all = &ask ("Run all tests? $help");
    if ($run_all == 0) {
        $run_iso = &ask ("Check ISO9660 compliance? ");
        if($run_iso) {
            if(($^O =~ m/linux/) || ($^O =~ m/solaris/)) {
        	print STDOUT "Enter the cd-rom device path: ";
        	$cd_dir = <STDIN>;
            }
        }
        $run_req = &ask ("Check for required files and directories? ");
        $run_err = &ask ("Find label validation errors? ");
        $run_new = &ask ("Find new keywords and standard values? ");
        $run_idx = &ask ("Check that the index file points to the data? ");
        $run_asc = &ask ("Test for errors in ASCII files?");
        $run_ref = &ask ("Check references against reference database? ");
        $run_kwv = &ask ("List keyword/value pairs for inspection? ");
    } else {
        print "\nRunning all tests...\n\n";
        $run_iso = 1;
        $run_req = 1;
        $run_err = 1;
        $run_new = 1;
        $run_idx = 1;
        $run_asc = 1;
        $run_ref = 1;
        $run_kwv = 1;
    }
}

# Test 1 - Check ISO9660 compliance (isocheck, checkiso.pl)
if ($run_iso == 1) {
    print "Checking ISO9660 compliance...\n\n";

    if( ($^O =~ m/linux/) || ($^O =~ m/solaris/) ) {
	system("perl checkiso.pl $cd_dir \"$iso_report\" >> \"$report\"");
    }
    else {
	system ("perl checkiso.pl $root_dir \"$iso_report\" >> \"$report\"");
    }
}

# Test 2 - Check for required files and directories (checkreq.pl)
if ($run_req == 1) {
    print "Checking for required files and directories...\n\n";
    system ("perl checkreq.pl $root_dir >> \"$report\"");
}


# To find validation errors or new keywords, run lvtool on every label
# Note: this section will be revised when lvtool checks for labels
if ($opt_b or $run_all or $run_err or $run_new) {
    print "Running lvtool on every label... \n\n";

    open (LVREPORT, ">lv.rpt");

# 01-06-05 MDC - Simply call lvtool now. Lvtool executable now resides in the same directory
#                as the perl scripts.

#	if( ($^O =~ m/solaris/) || ($^O =~ m/linux/) ) {
	        # MDC - Have to switch to the bin directory. For some reason,
	        # it is only when you're in the bin directory that lvtool can recursively
	        # look inside the directories.
#		chdir "$current_directory/bin" or die "Can't cd to $current_directory/bin: $!\n";
#		system ("$command -f $root_dir\"*.*\"");
#		chdir "$current_directory" or die "Can't cd to $current_directory: $!\n";
#	}
#	else {
		system ("$command");
#	}
        open (TEMP, "<temp");
        while (<TEMP>) {
            print LVREPORT;
        }
        close TEMP;

    close LVREPORT;
}

# Test 4 - Find label validation errors (listerr.pl)
if ($run_err == 1) {
    print "\nFinding label validation errors...\n\n";
    
    my $listerr_cmdline = "perl listerr.pl lv.rpt";
    
    if($opt_em)
    {
    	$listerr_cmdline = "$listerr_cmdline" . " --em $opt_em";
    }
    
    if($opt_s)
    {
    	$listerr_cmdline = "$listerr_cmdline" . " --s";
    }
    
    system ("$listerr_cmdline >> \"$report\"");
}

# Test 5 - Find new keywords and standard values (listnew.pl)
if ($run_new == 1) {
    print "Finding new keywords and standard values...\n\n";
    system ("perl listnew.pl lv.rpt >> \"$report\"");
}

# Test 6 - Check that the index file points to the data (checkidx.pl)
if ($run_idx == 1) {
    print "Checking that the index file points to the data...\n\n";
    system ("perl checkidx.pl $root_dir >> \"$report\"");
}


# Test 7 - Test text files for standards compliance (testline.pl)
if ($run_asc == 1) {
    print "Test for errors in ASCII files...\n\n";
    ### To skip attached labels, remove the -l flag
    $naif_option = "-s" if ($opt_n == 1);  # Skip NAIF attached labels
    system ("perl testline.pl $naif_option $root_dir >> \"$report\"");
}

# Test 8 - Test references against reference database (checkref.pl)
if ($run_ref == 1) {
    print "Testing references against the reference database...\n\n";
    system ("perl checkref.pl $root_dir >> \"$report\"");
}

# Test 9 - List keyword/value pairs for inspection (kwvtool, listkwv.pl)
if ($run_kwv == 1) {
    print "Listing keyword/value pairs for inspection...\n\n";
    system ("perl listkwv.pl -u $root_dir \"$kwv_report\"");
}

# Cleanup temp files
unlink "temp";
###unlink "label.lst";
unlink "lv.rpt";

# Tell everyone we're done
open (REPORT,">>$report") || die("Can't open file: $report");
print REPORT "See kwv-" . "$volume_id" . ".rpt for keyword/value pairs.\n" if ($run_kwv == 1);
print REPORT "\n\n*******************************\n";
print REPORT "* End of report for $volume_id *\n";
print REPORT "*******************************\n\n";
close REPORT;
# Ring the bell
#print "\a\a\a\a\a";

if($opt_rw)
{
	display_output();
}

print "\nTests complete. The report is in: $report\n";


#######################
# Support Subroutines #
#######################

###############################################
# Subroutine to ask questions and get answers #
###############################################
sub ask {
	
    my $question = shift @_;
    
    print "$question ";
    $answer = <STDIN>;
    exit if ($answer =~ m/q/i);
    if ($answer =~ m/y/i) {
        return 1;
    } else {
        return 0;
    }
}

#####################################
# Routine to build the command line #
# for the label validation test     #
#####################################
sub setup_lvtool_args()
{
	
	# Basic Command line if running in a solaris or linux environment
	if( ($^O =~ m/solaris/) || ($^O =~ m/linux/) ) {
#		$command = "./lvtool -a -v -e -nw -b3 $root_dir -d ../data/pdsdd.full -r $current_directory/temp";
                #01-27-06 MDC - Add quotes around wildcard pattern so that it can search thru sub-directories as well
		$command = "./lvtool -v -e -nw -b3 $root_dir -d data/pdsdd.idx -r temp -f $root_dir/\"*.*\"";
	}
	else { #Command line for windows platform
#		$command = "bin/lvtool -a -v -e -nw -b3 $root_dir -d data/pdsdd.idx -r temp -f $root_dir\\*.*";
		$command = "lvtool -v -e -nw -b3 $root_dir -d data/pdsdd.idx -r temp -f $root_dir\\*.*";
	}

	# Add the command line options if user specified them
	
	if( ($run_err == 0) && ($opt_ninfo || $opt_nwarn || $opt_nol3d || $opt_max || $opt_em || $opt_se) ) {
		die "$usage"; 
	}
	else {
		if($opt_ninfo == 1) {
			$command = $command . " -ninfo";
		}
		if($opt_nwarn == 1) {
			$command = $command . " -nwarn";
		}
		if($opt_nol3d == 1) {
			$command = $command . " -nol3d";
		}
		if($opt_se == 1) {
			$command = $command . " -se";
		}
		if($opt_max) {
			$command = $command . " -max $opt_max";
		}
		if($opt_nalias == 0) {
		        $command = $command . " -a";
		}
	}
}

############################################
# Routine to display the results of the    #
# tests onto a GUI window.                 #
############################################
sub display_output() {
	
$output_desc = <<"EOF1";
Below are the results of the validation tests. A copy of these results
is stored under $report.
EOF1
	
	$mw = MainWindow->new;
	$mw->title("Test Results");
	$f = $mw->Frame->pack(-side => 'top', -fill => 'x');

	$f->Label(-text => "$output_desc",
	          -font => "Times 12 normal"
	          )->pack(-side => 'top',
	                  -fill => 'both',
	                  -pady => 15,
	                  -expand => 1
	                 );
	
	$f->Button(-text => "Close Window",
		   -command => sub {$mw->destroy();},
		   -font => "Times 12 normal",
		   -width => 13
		  )->pack(-side => 'left',
		          -pady => 5
		          );
	
	$yscroll = $mw->Scrollbar();
		
	$text = $mw->Text(-yscrollcommand => ['set' => $yscroll],
			   -background => 'white',
			   -wrap => "word",
			   -font => "Courier"
			   );
	$yscroll->configure(-command => ['yview' => $text]);
	$yscroll->pack(-side => 'left', -fill => 'y');
	$text->pack(-side => 'left',
		  -fill => 'both',
		  -expand => 1
		  );
	
	open(OUTPUT, "<$report") || die "Could not open $report";
	
	while (<OUTPUT>) {
		$text-> insert('end', $_);
	}
	close(OUTPUT);
	
	MainLoop;
}