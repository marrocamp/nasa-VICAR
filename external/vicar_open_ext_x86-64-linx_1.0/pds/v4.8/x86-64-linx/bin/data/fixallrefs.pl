#!/usr/local/bin/perl
#
# Script: fixallrefs.pl - Fix AllRefs.txt by adding blank lines
# Usage:  fixallrefs.pl
#
# Author: Joel Wilf
# Date: 11/29/00
#
# Description:
#   This script fixes the AllRefs.txt file by adding blank lines
#   between the citations. It must be run from the directory
#   the AllRefs.txt file. The original AllRefs.txt is saved as
#   AllRefs.old.
# End:
#
# Revisions:
# End:


# Backup AllRefs.txt file
$usage = "Usage: fixallrefs.pl [In directory containing AllRefs.txt]";
die "Can\'t find AllRefs.txt\n" unless (-e "AllRefs.txt");
rename ("AllRefs.txt", "AllRefs.old") or die "Can\'t back up AllRefs.txt\n"; 

# Fix AllRefs.txt
open (ALLREFS, "<AllRefs.old") or die "Can\'t open AllRefs.old\n";
open (NEW_ALLREFS, ">AllRefs.txt") or die "Can\'t open AllRefs.txt\n";
while ($line = <ALLREFS>) {
	
    # Add <CR><LF> before every first line of an entry
    $line =~ s/^/\n/ if ($line =~ m/\|1\|/);

    # Print original or changed line
    print NEW_ALLREFS $line;
}

close ALLREFS;
close NEW_ALLREFS;
