#!/usr/bin/perl
#
# Script: checklab.pl - Check that every file on a volume has a label
# Usage:  checklab.pl [-a] [-d] <volume root>
#
# Author: Joel Wilf
# Date: 7/29/99
#
# Description:
#   This script checks that every file on a volume has a label, with
#   the exception of files in the EXTRAS directory.  The check is not
#   perfect.  It does not check actual pointers (which would require
#   parsing every label). It simply checks:
#
#        1) Whether the file has an .LBL, .CAT, or .FMT exntension.
#        2) Whether the file has an attached label.
#        3) Whether a file without an attached label has an .LBL
#           file with the same basename in the same directory.
#
#   Files that fail all three tests are listed as "suspicious," and
#   their status should be checked by inspection. Checklab.pl does
#   no further checking on files or labels, except to test that
#   all files have a nonzero size. This script takes one required
#   input parameter: the root directory of the volume.
#
#   If the -a option is set, only attached labels are listed. If the
#   -d option is set, only detatched labels are listed.
# End:
#
# Revisions:
#   10/03/00, J. Wilf, Made tests case insensitive.
#   01/07/00, J. Wilf, Added -a and -d options.
# End:


use Getopt::Std;
use File::Find;
use File::Basename;
$| = 1;

# Get the PVT home directory
$home_pvt = dirname($0);

# Get volume root and options
$usage = "Usage: checklab.pl [-a] [-d] <volume root>";
getopts('ad'); # If -a is set, $opt_a == 1, etc. 
$root_dir = shift or die "$usage\n";
die "Can\'t find $root_dir\n" unless (-e $root_dir);
die "$root_dir isn\'t a directory\n" unless (-d $root_dir);
$root_index = length ($root_dir);  # Index to actual filename

# Create list of labels that can be fed to lvtool
$label_list = "$home_pvt/label.lst";
open (LABEL,">$label_list") or die "Can't write file to list labels\n";

# Check every file for a corresponding label
print "Checking that every file has a label:\n";
print "=====================================\n\n"; 
$File::Find::dont_use_nlink = 1;  # Truly evil Windows hack
fileparse_set_fstype("MSDOS"); ##### But what about Unix filesystems?
find(\&wanted, $root_dir);

# Display the list in a reasonable way
@list = sort @bad_files;
if (scalar @list > 0) {
    print "Could not find labels for the following files:\n\n";
    print join "\n", @list;
    print "\n";
} else {
    print "All files have labels.\n";
}

# Clean up and exit
close LABEL;
print "\n\n";

#######################
# Support Subroutines #
#######################

# Subroutine that does all the work
sub wanted {
	
    # Get and parse every non-directory file
    $file = $File::Find::name;
    $file =~ s/\\/\//g;              #Switch all '\' to '/'
    return if (-d $file);
    ($name, $path, $suffix) = fileparse($file,'\..*');
    
    # Test that files have a nonzero length
    ### Push into array and print error or ok message at end:
    print "ERROR: $file is a zero-length file\n\n" if (-z $file);
    
    # We're okay if the file has a label extension
    if ($suffix =~ m|\.CAT|i) {
    	print LABEL "$file\n" unless ($opt_a == 1);
    	return;
    }
    if ($suffix =~ m|\.LBL|i) {
    	print LABEL "$file\n" unless ($opt_a == 1);
    	return;
    }
    if ($suffix =~ m|\.FMT|i) {
        print LABEL "$file\n" unless ($opt_a == 1);
        return;
    }
    
    # We're okay if the file has an attached label
    open (FILE_IN, "<$file");
    read FILE_IN, $buffer, 200;
    close FILE_IN;
    if ($buffer =~ /PDS_VERSION/) {
        print LABEL "$file\n" unless ($opt_d == 1);
        return;
    }
    
    # We're okay if a file with $path/$name.LBL exists
    $labelfile = $path . $name . ".LBL";
    $labelfile_lc = $path . $name . ".lbl";
    return if (-e $labelfile or -e $labelfile_lc);
    
    # Add case where we parse pointers for non-matching LBL file
    
    # If all these tests have failed, it could be an unlabeled file
    $file = substr $file, $root_index;
    $file =~ s|^[\/]||;
    push @bad_files, "$file";
    
}
