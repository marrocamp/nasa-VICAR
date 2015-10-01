#!/usr/bin/perl
#
# Script: listkwv.pl - Lists all kewyword/value pairs
# Usage:  listkwv.pl -u <volume root> [report name]
#
# Author: Joel Wilf
# Date: 2/12/00
#
# Description:
#   This script runs the program KWVTOOL on all PDS labels found
#   under the volume pointed to by <volume root>. In other words,
#   the script finds all keyword/value pairs in all the labels on
#   the volume. If the -u flag is set, only unique keyword/value
#   pairs are listed. The listing is generated under [report name]
#   or the default name "kwv.rpt".
# End:
#
# Revisions:
#   2000-11-28, J. Wilf, added the -u flag.
#   2005-01-12, M. Cayanan, change call to kwvtool program.
# End:


use Getopt::Std;
use File::Find;
use File::Basename;
$| = 1;

# Get the PVT home directory
$home_pvt = dirname($0);

# Get root directory and optional report name
$usage = "listkwv.pl <volume root> [report name]";
getopts('u');  # If -u is set, $opt_u == 1
$root_dir = shift or die "$usage\n";
die "Can\'t find $root_dir\n" unless (-e $root_dir);
die "$root_dir isn't a directory\n" unless (-d $root_dir);
$kwv_report = shift or ($kwv_report = "kwv.rpt");

# Announce intentions:
print "Listing all keyword/value pairs:\n";
print "================================\n\n";

# Create some convenient variables
$label_list = "$home_pvt/label.lst";
$temp = "$home_pvt/temp";
$temp_rpt = "$home_pvt/temp.rpt";
#$ddict = "$home_pvt/data/pdsdd.idx";
$ddict = "data/pdsdd.full";

# Create a list of labels, if there isn't one already
system ("perl checklab.pl $root_dir") unless (-e "$label_list");

# Run kwvtool on all labels
open (LABEL, "<$label_list");
open (LVREPORT, ">$temp_rpt");
@all_labels = <LABEL>;
foreach $label (@all_labels) {
    if( ($^O ne "solaris") && ($^O ne "linux") ) {
    	$label =~ s/\//\\/g;    # Switch all the '/' to '\' for Windows
    }
# 01-12-05 MDC - kwvtool no longer resides in the bin directory
#    system ("$home_pvt/bin/kwvtool -a -d $ddict -r $temp -f $label");
    system ("kwvtool -a -df $ddict -r $temp -f $label");
    open (TEMP, "<$home_pvt/temp");
    while (<TEMP>) {
        print LVREPORT;
    }
    close TEMP;
}
close LABEL;
close LVREPORT;

# Filter the kwv.rpt
open (INFILE, "<$temp_rpt");
while ($line = <INFILE>) {
     push @kwv, $line if ($line =~ m/^ \w+/);
}
close INFILE;
@kwv = sort @kwv;

# Print results
open (OUTFILE, ">$kwv_report") or die "Can\'t open $kwv_report\n";
print OUTFILE "Keyword/Value Pairs:\n";
print OUTFILE "====================\n\n";
# If -u flag is set, print only unique elements
if ($opt_u) {
    $last_item = "";
    foreach $item (@kwv) {
        print OUTFILE "$item" unless ($item eq $last_item);
        $last_item = $item;
    }    	
# Otherwise, print every element in the array
} else {
    foreach $item (@kwv) {
        print OUTFILE "$item";
    }
}
close OUTFILE;

# Clean up temp files and exit
unlink "JunkZZZ1.TXT";
unlink "$temp";
unlink "$temp_rpt";
unlink "$label_list";
