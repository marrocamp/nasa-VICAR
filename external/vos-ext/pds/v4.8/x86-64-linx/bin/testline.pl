#!/usr/bin/perl
#
# Script: testline.pl - Test ASCII files for PDS standards compliance
# Usage:  testline.pl [-v] [-s] [-l] <filename or volume root>
#
# Author: Joel Wilf
# Date: 3/31/99
#
# Description:
#   This script performs the following tests on ASCII files:
#
#     1. Whether each line is terminated with a <CR><LF> pair.
#     2. Whether any line exceeds 80 characters in length, including
#        the <CR><LF> pair.
#     3. Whether any line contains an embedded TAB character.
#     4. Whether any line conains another character outside the range
#        20-7E (hexidecimal).
#
#   There is one required argument. It can be either the name of a
#   file to test or the absolute path to the volume root, from which
#   all appropriate files are tested recursively. For single file
#   testing, the -l option tells the script to interpret the file as
#   an attached label.  For recursive testing, files with TXT, ASC,
#   CAT, LBL, and FMT extensions are automatically checked. All other
#   files are checked to see if an attached label is present. If it
#   is, the attached label portion of the file is tested, unless the
#   -s flag is set. The -s flag causes testline.pl to skip attached
#   labels.
#
#   Normally, testline.pl only reports files that have errors.
#   However, if the -v flag is set, all files will be listed. 
#
# Revisions:
#   1999-07-29, J. Wilf, Modified to test only appropriate files
#   1999-07-30, J. Wilf, Added -v option for a more verbose report
#   1999-08-24, J. Wilf, Added test for extra <LF> at end of file
#   2000-03-28, J. Wilf, Added -l and -s options for attached labels
# End:
#

use Getopt::Std;
use File::Find;
use File::Basename;
$| = 1;

# Get command line arguments
getopts('vls');  # If set, $opt_v == 1, etc.
$usage = "Usage: testline.pl [-v] [-l] [-s] <filename or volume root>";
$file_in = shift or die "$usage\n";
die "Can\'t find $file_in\n" unless (-e $file_in);

# Announce intentions
print "Testing for Errors in ASCII Files:\n";
print "==================================\n\n";

# If a single file was input, go test it
$bad_file_count = 0;
if (-f $file_in) {
    $opt_v = 1;  # Always print output for single file
    &test_attached_label($file_in) if ($opt_l);
    &test_full_text($file_in) unless ($opt_l);
}

# If a directory was input, find each appropriate file and test it
if (-d $file_in) {
    $root_index = length ($file_in) + 1; # Index to file names
    undef ($File::Find::prune); 
    $File::Find::dont_use_nlink = 1; # Evil hack for windows
    find(\&wanted, $file_in);
}

# Print number of bad files
if (-d $file_in) {
    print "Number of files with ASCII errors = $bad_file_count\n\n\n";
}

#######################
# Support Subroutines #
#######################

# Subroutine to find appropriate files and test them
sub wanted {
	
    # Get and parse every non-directory file
    $file = $File::Find::name;
    return if (-d $file);
    ($name, $path, $suffix) = fileparse($file,'\..*');
    
    # Test all files with TXT, ASC, CAT, LBL, and FMT extensions
    if ($suffix =~ m/txt/i) {
        &test_full_text($file);
        return;
    }
    
    if ($suffix =~ m/asc/i) {
    	&test_full_text($file);
        return;
    }
    
    if ($suffix =~ m/cat/i) {
        &test_full_text($file);
        return;
    }
    
    if ($suffix =~ m/lbl/i) {
        &test_full_text($file);
        return;
    }
    
    if ($suffix =~ m/fmt/i) {
        &test_full_text($file);
        return;
    }

#################### Add tab test for fun #######################
#
#   if (($suffix =~ m/tab/i) and (-T $file)) {
#       &test_full_text($file);
#       return;
#   }
#
#################################################################    
    
    
    # Test other files for an attached label
    return if ($opt_s == 1);
    &test_attached_label($file);
}

# Subroutine to get full text for testing
sub test_full_text {
	
    my $file = shift @_;
    
    # Read full text as single string in binary mode
    undef $/;
    open (FILE_FULL,"<$file") or die "Can\'t open $file\n";
    binmode FILE_FULL;
    $full_text = <FILE_FULL> if defined(FILE_FULL);
    while (<FILE_FULL>) { $full_text = $full_text . $_ }
    close FILE_FULL;
    $/ = "\n";  # Restore normal line-reading
    &testthis($file, $full_text);
}

# Subroutine to get attached label for testing
sub test_attached_label {

    my $file = shift @_;
	
    # Test file for an attached label
    open (FILE_IN, "<$file");
    read FILE_IN, $buffer, 200;
    close FILE_IN;
    return unless ($buffer =~ m/PDS_VERSION/);
    
    # Get attached label, reading ASCII mode until the END statement
    $end_exists = 0;
    $record_bytes = 0;
    $label_records = 0;

    open (FILE_IN, "<$file");
    while ($line = <FILE_IN>) {

        if ($line =~ m/RECORD_BYTES/) {
            @lineparts = split '=', $line;
            $record_bytes = $lineparts[1];
        }
    
        if ($line =~ m/LABEL_RECORDS/) {
            @lineparts = split '=', $line;
            $label_records = $lineparts[1];
        }
        
        if ($line =~ m/^END[^_]/) {
            $end_exists = 1;
            last;
        }
    }
    close FILE_IN;

    # Respond to errors
    if ($record_bytes == 0) {
        print ">>> Couldn\'t check $file: No RECORD_BYTES\n\n";
        return;
    }
    
    if ($label_records == 0) {
        print ">>> Couldn\'t check $file: No LABEL_RECORDS\n\n";
        return;
    }
    
    unless ($end_exists == 1) {
        print ">>> Couldn\'t check $file: No END statement\n\n";
        return;
    } 

    # Calculate total bytes in attached label
    $total_bytes = $record_bytes * $label_records;

    # Get label with binary read for testing
    open (FILE_IN, "<$file");
    binmode FILE_IN;
    read FILE_IN, $label, $total_bytes;
    close FILE_IN;
    
    # Make label into an array to test
    $label =~ s/\s+$//; # Lose trailing whitespace
    $label = $label . "\r\n" if ((substr $label, -2) ne "\r\n");
    &testthis($file, $label);
}
    
sub testthis {

    my ($testfile, $testfile_string) = @_;
    
    # Make an array of all the lines in the file 
    @testfile_array = split "\012", $testfile_string;
        
    # Test each line in the array
    $line_count = 0;
    $error_count = 0;
    foreach $line (@testfile_array) {
        $line_count++;
        $line_length = length($line) + 1;
        $error1 = $error2 = $error3 = $error4 = "";
                           
        # Test line terminators
        if ($line !~ s/\015$/ /) {
            $error_count++;
            $error1 = "wrong terminator\; ";
        }
        
        # Test line lengths
        if ($line_length > 80) {
            $error_count++;
            $error2 = "line length = $line_length\; ";
        }
        
        # Test for embedded tabs
        if ($line =~ s/\t/ /g) {
            $error_count++;
            $error3 = "embedded TAB(s)\; ";
        }
        
        # Test for other illegal characters
        if ($line =~ m/[\x00-\x1F]/ || $line =~ m/[\x7F-\xFF]/) {
            $chr = ord ($&);
            $error_count++;
            $error4 = "embedded ASCII $chr character\; ";
       }
       $error = $error1 . $error2 . $error3 . $error4;
       push @bad_lines, $line_count, $error if ($error);
    }
    
    # Test last line for an extra <LF> at EOF
    if ((substr $testfile_string, -3) eq "\r\n\n") {
        $line_count++;
        $error_count++;
        push @bad_lines, $line_count, "an extra linefeed <LF> at end of file";
    }
    
    # Test last line for no <LF> at EOF
    if ((substr $testfile_string, -1) eq "\r") {
        $error_count++;
        push @bad_lines, $line_count, "no linefeed <LF> at end of file";
    }

    # After looking at each line, display file name and error count
    $testfile = substr $testfile, $root_index;
    if ($error_count == 0) {
    	print "==> $testfile: Okay\n" if ($opt_v == 1);
    	print "Number of lines = $line_count\n\n" if ($opt_v == 1);
    	return;
    }
    print "==> $testfile: 1 error found\n" if ($error_count == 1);
    print "==> $testfile: $error_count errors found\n" if ($error_count > 1);
    $bad_file_count++ if ($error_count > 0);
 
    # Initialize values
    $end_err = 0;
    $last_message = "";
    
    # Process the bad_line array
    while ($err = shift @bad_lines) {
    	
        # Get error message
        $message = shift @bad_lines;

        # If this is the first time through the loop
        if ($end_err == 0) {
            print "line $err has $message\n" if (scalar @bad_lines == 0);
            $start_err = $err;
            $end_err = $err;
            $last_message = $message;
            next;
        }
        
        # If error is part of a sequence
        if ($err == ($end_err + 1) && $message eq $last_message) {
            print "lines $start_err - $err have $message\n" if (scalar @bad_lines == 0);
            $end_err = $err;
            next;
        }

        # If error is out of sequence or the message has changed
        if ($err > ($end_err + 1) || $message ne $last_message) {
            print "line $end_err has $last_message\n" if ($start_err == $end_err);
            print "lines $start_err - $end_err have $last_message\n" if ($start_err < $end_err);
            print "line $err has $message\n" if (scalar @bad_lines == 0);
            $start_err = $err;
            $end_err = $err;
            $last_message = $message;
        }
    }
    print "Number of lines = $line_count\n\n";

}    
