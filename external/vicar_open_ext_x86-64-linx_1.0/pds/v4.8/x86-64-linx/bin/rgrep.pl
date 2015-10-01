#!/usr/bin/perl
#
# Script: rgrep.pl - Recursive grep
# Usage:  rgrep.pl [-i] [-r] <string> <starting directory>
#
# Author: Joel Wilf
# Date: 3/31/00
#
# Description:
#   This script performs a simple recursive grep. That is, it
#   looks at all the files in the <starting directory> tree
#   and lists all the ones that match, along with the line that
#   matches.
#
#   There are two options, currently. The -i flag uses case
#   insensitive matching. The -r file causes a report to be written
#   to the file "grep.rpt".
# End:
#
# Revisions:
# End:

use Getopt::Std;
use File::Find;
$| = 1;

# Get command line arguments
getopts('ir');  # If set, $opt_i == 1, etc.
$usage = "Usage: rgrep [-i] [-r] <string> <starting directory>";
$string = shift or die "$usage\n";
$root_dir = shift or die "$usage\n";
die "Can\'t find $root_dir\n" unless (-e $root_dir);
die "Error: $root_dir is not a directory" unless (-d $root_dir);

# Go forth and grep
undef ($File::Find::prune); 
$File::Find::dont_use_nlink = 1; # Evil hack for windows
find(\&wanted, $root_dir);

# Print the results to the appropriate place
print @matches unless ($opt_r);
if ($opt_r) {
    open (FILE_OUT, ">grep.rpt");
    print FILE_OUT @matches;
    close FILE_OUT;
}

#######################
# Support Subroutines #
#######################

# Subroutine that finds the files and does the grepping
sub wanted {
	
    # Get every non-directory file
    $file = $File::Find::name;
    return if (-d $file);
    
    # Print every line of the file that matches the string
    open (FILE_IN, "<$file") or die "Can\'t open $file\n";
    while (<FILE_IN>) {
        if ($opt_i) {
            push @matches, "$file: $_" if (m/$string/i);
        } else {
            push @matches, "$file: $_" if (m/$string/);
        }
    }
    close FILE_IN;
}    
