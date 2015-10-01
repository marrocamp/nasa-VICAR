#!/usr/bin/perl
#
# Script: listnew.pl - List new keywords and values on a volume
# Usage:  listnew.pl -f <lvtool report> [extension list]
#
# Author: Joel Wilf
# Date: 6/16/99
#
# Description:
#   This script scans the output of lvtool and lists all the keywords
#   and standard values that are not currently in the data dictionary.
#   If the "-f" option is set, the listing includes the files in which
#   the new keywords and standard values are found.  The optional
#   extension list tells the script not to look at any file with one
#   of the given extensions.  This is useful because lvtool sometimes
#   misreads certain file types (e.g., .C, .H, .PDF) as labels.  Note:
#   prior to running listnew.pl, lvtool must be run with the "-nw" flag.
# End:
#
# Revisions:
#    12-19-05   MDC   Modified script to handle local keywords
# End:


use Getopt::Std;
$| = 1;

# Process command line
getopts('f');     # If -f is set, $opt_f == 1
$usage = "Usage: listnew.pl -f <lvtool report> [extensions]";
$report = shift or die "$usage\n"; 
die "Can't find $report\n" unless (-e $report);
die "Invalid $report\n" unless (-f $report);
@extlist = @ARGV;

# Scan for new keywords and standard values
print "New Keywords and Standard Values:\n";
print "=================================\n\n";
open (REPORT,"<$report");
while (<REPORT>) {
    $line = $_;
    # Get filename
    if (substr($line,0,1) eq '#') {
        $line =~ m/(\w\S+)\s/;      # Extract file name from line.
        $file_name = $1;
        $file_name =~ s|\\|\/|g;    # Convert back-slashes to forward-slashes.
        $file_name =~ s|\w:\/||g;   # On Windows, get rid of volume:/.
        $file_text = "";
        $file_text = "- in $file_name" if ($opt_f == 1);
    }
    
    # Filter out results from bad file types
    if (scalar @extlist != 0) {
    	$bad_file_flag = 0;
    	$bad_file_flag = 1 if ($file_name !~ m/\./);
    	foreach $extension (@extlist) {
    	    $bad_file_flag = 1 if ($file_name =~ m/\.$extension/i);
    	}
    	next if ($bad_file_flag == 1);
    }
        
    # Get new keywords
    if ($line =~ m/Not in data dictionary/) {
    	# 12-19-05 MDC - Use (\S+) instead of (\w+) to account for local keywords
   # 	$line =~ m/-- (\w+):/;    # Extract new keyword from line.
   	$line =~ m/-- (\S+): /;
    	$new_keyword = $1;
    	push @new_keywords, "$new_keyword $file_text";
    }
    
    # Get new standard values
    if ($line =~ m/Value is not/) {
    	# 12-19-05 MDC - Use (\S+) instead of (\w+) to account for local keywords
   # 	$line =~ m/-- (\w+):/;    # Extract keyword from line.
   	$line =~ m/-- (\S+): /;
    	$keyword = $1;
    	$line =~ m/value: (.+)/;  # Extract new standard value from line.
    	$new_value = $1;
    	push @new_svalues, "$keyword new value = $new_value $file_text";    	
    }
    	
}
close REPORT;

# Display list of keywords
@list = sort @new_keywords;
if (scalar @list > 0) {
    print "New Keywords:\n\n";
    $prev_entry = "";
    while ($entry = shift @list) {
        print "$entry\n" unless ($entry eq $prev_entry);
        $prev_entry = $entry;
    }
    print "\n\n";
} else {
print "No new Keywords.\n\n";
}

# Display list of standard values
@list = sort @new_svalues;
if (scalar @list > 0) {
    print "New Standard Values:\n\n";
    $prev_entry = "";
    while ($entry = shift @list) {
        print "$entry\n" unless ($entry eq $prev_entry);
        $prev_entry = $entry;
    }
} else {
print "No new Standard Values:\n";
}
print "\n\n";
