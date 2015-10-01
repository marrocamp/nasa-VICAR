#!/usr/bin/perl
#
# Script: checkiso.pl - Check ISO compliance
# Usage:  checkiso.pl <CD Reader> [report name]
#
# Author: Joel Wilf
# Date: 3/31/00
#
# Description:
#   This script runs the program ISOCHECK on a CD reader and
#   checks whether the CD it holds is ISO9660 compliant. For
#   Win32 systems, the CD reader is a volume, for example, e:\.
#   For Unix systems, the CD reader is a device file, for example,
#   \dev\cdrom. In any case, checkiso.pl ONLY checks CD media and
#   should NOT be used with anything else. If the CD is ISO9660
#   compliant, a simple confirmation message is printed. If the
#   CD fails the test, an error message and a report is generated.
#   The report name is optionally given by the user in [report name]
#   and defaults to "iso.rpt".
# End:
#
# Revisions:
# End:

use File::Basename;
$| = 1;

# Get the PVT home directory
$home_pvt = dirname($0);

# Get the location of the CD reader and the ouput report name
$usage = "checkiso.pl <CD Reader>";
$cd = shift or die "$usage\n";
die "Can\'t find $cd\n" unless (-e $cd);
$iso_report = shift or ($iso_report = "iso.rpt");


# Run ISOCHECK and capture its output
$iso_errors = `isochecker -V -E /$cd`;

# If the ISO check passed, tell us the CD is compliant
if ($iso_errors =~ m/ISO Check PASS/) {
    print "This volume is ISO9660 compliant.\n\n\n";
}

# Otherwise, print out the report and tell us the test failed
else {	
    open (ISORPT, ">$iso_report");
    print ISORPT $iso_errors;
    print "ERROR: This volume is NOT ISO9660 compliant. ";
    print "See the ISO report for details.\n\n\n";
}
