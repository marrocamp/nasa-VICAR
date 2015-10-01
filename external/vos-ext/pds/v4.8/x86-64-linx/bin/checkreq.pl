#!/usr/bin/perl
#
# Script: checkreq.pl - Check for required files and directories
# Usage:  checkreq.pl <volume root>
#
# Author: Joel Wilf
# Date: 6/30/99
#
# Description:
#   This script checks that required directories and files are present
#   on a volume.  It prints a report to the standard output device.
#   The only option it takes is the path to the root directory of the
#   volume.  NOTE: Checkreq.pl does not check for ISO9660 compliance
#   and treats filenames the way perl treats filenames on ISO9660
#   CD-ROMs. 
# End:
#
# Revisions:
#   1999-07-28, J. Wilf, Added check for optional EXTRAS directory.
#   1999-08-24, J. Wilf, Made check lower case for Unix systems.
#   1999-08-25, J. Wilf, Added handling of multiple index tables.
# End:

$| = 1;

# Get root directory of volume
$usage = "checkreq.pl <volume root>";
$root_dir = shift or die "$usage\n";
die "Can\'t find $root_dir\n" unless (-e $root_dir);
die "$root_dir isn't a directory\n" unless (-d $root_dir);

$root_dir =~ s/\\/\//g;
$root_dir = $root_dir . "\/";

# Get all root level files and directories
opendir ROOTDIR, $root_dir or die "Can't open $root_dir:$!";
@raw_root_contents = grep !/^\.\.?$/, readdir ROOTDIR;
closedir ROOTDIR;
@root_contents = sort @raw_root_contents;

# Check for required and optional files and directories
print "Checking for Required Files and Directories:\n";
print "============================================\n\n";

# Check root level files
print "Root-level Files:\n\n";
&check ("AAREADME.TXT", "required");
&check ("VOLDESC.CAT", "required");
&check ("AAREADME.HTM", "optional");
&check ("AAREADME.LBL", "optional");
&check ("ERRATA.TXT", "optional");
&check ("ERRATA.HTM", "optional");
&check ("ERRATA.LBL", "optional");
&check ("VOLDESC.SFD", "optional");
print "\n\n";

# Check root level directories and their required subfiles
print "Root-level Directories and their required subfiles:\n\n";
&check ("CATALOG", "required", "CATINFO.TXT");
&check ("INDEX", "required");
&checkindex(); # Special processing for index tables;
&check ("BROWSE", "optional", "BROWINFO.TXT");
&check ("EXTRAS", "optional", "EXTRINFO.TXT");
&check ("CALIB", "optional", "CALINFO.TXT");
&check ("DOCUMENT", "optional", "DOCINFO.TXT");
&check ("GAZETTER", "optional", "GAZINFO.TXT", "GAZETTER.TXT", "GAZATTER.LBL", "GAZATTER.TAB");
&check ("GEOMETRY", "optional", "GEOMINFO.TXT");
&check ("LABEL", "optional", "LABINFO.TXT");
&check ("SOFTWARE", "optional", "SOFTINFO.TXT");
&check ("DATA","DATA is the data directory");
print "\n\n";

# Sort the remaining root contents into files and directories
foreach $leftover (@root_contents) {
    push @leftover_dirs, $leftover if (-d "$root_dir/$leftover");
    push @leftover_files, $leftover if (-f "$root_dir/$leftover");
}

# List data and extraneous directories
if (scalar @leftover_dirs > 0) {

    # Classify directories
    if (-d "$root_dir/data") {
        print "These are extraneous directories:\n\n";
    } else {
        print "These are data directories, or they don't belong here:\n\n";
    }
    
    # Print directories
    foreach $entry (@leftover_dirs) {
        $listentry = uc $entry;
        print "$listentry\n";
    }
    print "\n\n";
} else {
    print "No extraneous root-level directories.\n\n\n";
}

# List extraneous files
if (scalar @leftover_files > 0) {
    print "These are extraneous files:\n\n";
    foreach $entry (@leftover_files) {
        $listentry = uc $entry;
        print "$listentry\n";
    }
    print "\n\n";
} else {
    print "No extraneous root-level files.\n\n\n";
}

#######################
# Support Subroutines #
#######################

# Check for the specified file or directory
sub check {
    
    my $item_sought = shift @_;		# Specified file
    my $status = shift @_;		# Required or Optional
    my @required_subfiles = @_;		# Example: catinfo.txt

    # Check if the file or directory is present on the volume
    $present_flag = 0;
    $entry_num = 0;
    $item_sought_lc = lc $item_sought;  # Unix perl reads files as lower case
    foreach $entry (@root_contents) {
        if (($entry eq $item_sought) or ($entry eq $item_sought_lc)) {
            print "OK: $item_sought - $status and present\n";
            $present_flag = 1;
            splice (@root_contents, $entry_num, 1);
        }
        $entry_num++;
    }
    
    # Print error message if a required file or directory is missing
    if ($status eq "required" and $present_flag == 0) {
        print "ERROR: $item_sought - required but missing\n";
    }
    
    # Check for required subfiles, if applicable
    if (defined @required_subfiles and $present_flag == 1) {
    	foreach $subfile (@required_subfiles) {
            $subfile_lc = lc $subfile;  # Unix perl reads files as lower case
    	    $file_on_volume = "$root_dir/$item_sought/$subfile";
    	    $file_on_volume_lc = "$root_dir/$item_sought_lc/$subfile_lc";
    	    if ((-e "$file_on_volume") or (-e "$file_on_volume_lc")) {
                print "OK:   $subfile - required and present\n";
            } else {
            	print "ERROR: $subfile - required and missing\n";
            }
    	}
    }
}        

# Check INDEX directory index tables
sub checkindex {

    # Get tables from INDEX directory
    $index_dir = $root_dir . "INDEX/";
    $index_dir = $root_dir . "index/" unless (-e $index_dir);
    opendir INDEXDIR, $index_dir or die "Can't open $index_dir\n";
    @indextables = grep /\.TAB/i, readdir INDEXDIR;
    closedir INDEXDIR;
    
    # Check for labels and separate index from cumulative index tables
    while ($table = shift @indextables) {
    	$label = $index_dir . $table; # Use full path to check label
        $label =~ s/\.TAB/.LBL/ if ($table =~ m/\.TAB/);
        $label =~ s/\.tab/.lbl/ if ($table =~ m/\.tab/);
        if ($table =~ m/INDEX\.TAB/i and $table !~ m/CUMINDEX\.TAB/i) {
            $it = "index table";
            $msg = "OK:   $table - $it - has a label" if (-e $label);
            $msg = "ERROR: $table - $it - has no label" unless (-e $label);
            push @idxtables, $msg;
        } elsif ($table =~ m/CUMINDEX\.TAB/i or $table =~ m/CMIDX\.TAB/i) {
	    $it = "cumulative index table";
            $msg = "OK:   $table - $it - has a label" if (-e $label);
    	    $msg = "ERROR: $table - $it - has no label" unless (-e $label);
            push @cumtables, $msg;
        }
    }
    
    # Print results
    print join "\n", @idxtables if (scalar @idxtables > 0);
    print "ERROR: No index tables" if (scalar @idxtables == 0);
    print "\n";
    print join "\n", @cumtables if (scalar @cumtables > 0);
    print "NOTE: No cumulative index tables" if (scalar @cumtables == 0);
    print "\n";
}
