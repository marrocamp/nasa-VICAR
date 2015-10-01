#!/usr/bin/perl
#
# Script: validdvd.pl - Runs validation tests on a multi-volume DVD
# Usage:  validdvd.pl <DVD volume root> [report directory]
#
# Author: Joel Wilf
# Date: 08/24/01
#
# Description:
#   This script runs validation tests on multi-volume DVDs. That is,
#   DVDs that contain sub-volumes. The following set of tests are run:
#
#     1) ISO9660 compliance for the entire DVD
#     2) Presence of required DVD top-level files
#     3) For each subvolume, all validate.pl tests except the
#        ISO check. See validate.pl for details.
#
#   The DVD volume root (the path to the root directory of the DVD) is
#   required. The test report will have the name "<VOLUME_ID.rpt>" and
#   will be placed in the same directory as validate.pl, unless the 
#   [report directory] is specified.
# End:
#
# Revisions:
#  #   2002-03-12, J. Wilf, Changed default directory to current one
# End:

use Cwd;
use File::Basename;
$| = 1;

# Save current directory but run script from PVT home directory
$current_directory = getcwd();
$home_pvt = dirname($0);
chdir $home_pvt or die "Can\'t run from the PVT home directory\n";

# Get root directory
$usage = "validdvd.pl <DVD volume root> [report directory]";
$root_dir = shift or die "$usage\n";
die "Can\'t find $root_dir\n" unless (-e $root_dir);
die "$root_dir isn't a directory\n" unless (-d $root_dir);
$report_dir = shift or ($report_dir = $current_directory);

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
    $volume_id = "NO_VOLUME_ID";
    print "ERROR: There should be a VOLDESC.CAT with a VOLUME ID\n\n";
}

# Set up output reports
$report = "$volume_id" . '.rpt';
$iso_report = 'iso-' . "$report";
$report = $report_dir . "/" . $report if ($report_dir);
$iso_report = $report_dir . "/" . $iso_report if ($report_dir);

# Open report file and announce presence
print "\nValidating DVD Volume: $volume_id...\n\n";
open (REPORT,">$report") or die "Can\'t open report file\n";
print REPORT "************************************\n";
print REPORT "* Validation Report for $volume_id *\n";
print REPORT "************************************\n\n";

# Check ISO9660 compliance for the whole DVD (isocheck)
$iso_errors = `$home_pvt/bin/ISOCHECK -V -E /$root_dir`;
if ($iso_errors =~ m/ISO Check PASS/) {
    print REPORT "This volume is ISO9660 compliant.\n\n";
} else {	
    open (ISORPT, ">$iso_report");
    print ISORPT $iso_errors;
    print REPORT "ERROR: This volume is NOT ISO9660 compliant.\n";
    print REPORT "See ISO report: $iso_report\n\n";
}

# Print root level files and directories
opendir ROOTDIR, $root_dir or die "Can't open $root_dir:$!";
@root_contents = grep !/^\.\.?$/, readdir ROOTDIR;
closedir ROOTDIR;
foreach $item (@root_contents) {
    push @root_files, $item if (-f "$root_dir/$item");
    push @root_dirs, $item if (-d "$root_dir/$item");
}
@root_files = sort @root_files;
@root_dirs = sort @root_dirs;
print REPORT "Root level files:\n";
print REPORT "=================\n\n";
print REPORT join "\n", @root_files;
print REPORT "\n\n";
print REPORT "Root level directories:\n";
print REPORT "=======================\n\n";
print REPORT join "\n", @root_dirs;
print REPORT "\n\n";

# Wrap up DVD overview report
print REPORT "These directories will now be validated as if they were\n";
print REPORT "each stand-alone volumes. See the individual validation\n";
print REPORT "reports for the details.\n\n\n";
print REPORT "*********************************\n";
print REPORT "* End of overview of $volume_id *\n";
print REPORT "*********************************\n\n";
close REPORT;

# Go forth and validate the individual volumes:
foreach $dir (@root_dirs) {
    next if ($dir =~ m/extras/i);
    $subdir = $root_dir . "/" . $dir . "/";
    system ("validate.pl -23456789 $subdir $report_dir");
}

### End ###
