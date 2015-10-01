#!/usr/bin/perl
#
# Script: checkidx.pl - Checks that index.tab points to the data
# Usage:  checkidx.pl [-v] <volume root> [alternative directory]
#
# Author: Joel Wilf
# Date: 7/31/99
#
# Description:
#   This script performs the following tests on the index files:
#
#       1. That every data file specified in INDEX.TAB exists.
#       2. That all files in the data directories are indexed.
#       3. It prints the volumes indexed in CUMINDEX.TAB.
#
#   Checkidx.pl takes one required parameter, the path to the volume
#   root. The index files are found automatically, by searching the
#   INDEX directory. However, the user can specify an alternative
#   directory, which will be searched instead of <volume root>/INDEX.
#   This is useful if you editing the index tables. NOTE: if you use
#   this option, remember to have ALL index files in the alternative
#   directory.
#
#   Data files are also found automatically, according to the PDS
#   standards for volume organization.
#
#   If the -v option is set, the program will be more verbose and
#   print out the filenames in the index tables.
# End:
#
# Revisions:
#   1999-09-01, Added handling of multiple index tables
# End:

use Getopt::Std;
use File::Find;
use File::Basename;
$| = 1;

# Get -v option and volume root
$usage = "checkidx.pl [-v] <volume root> [alternative directory]\n";
getopts('v');  # If -v is set, $opt_v == 1.
$root_dir = shift or die "$usage\n";
die "Can\'t find root directory $root_dir\n" unless (-e $root_dir);

$root_dir =~ s/\\/\//g;
$root_dir = $root_dir . "/" unless ($root_dir =~ m|\/$|);
$root_length = length ($root_dir);

# Get directory that contains the index files
if (scalar @ARGV == 0) {
    # Use the <volume root>/INDEX directory
    $index_dir = $root_dir . "INDEX/";
    $index_dir = $root_dir . "index/" unless (-e $index_dir);
} else {
    # Use the alternative directory
    $index_dir = shift;
    $index_dir = $index_dir . "/" unless ($index_dir =~ m|\/$|);
}

### Get index tables
opendir INDEXDIR, $index_dir or die "Can't open $index_dir\n";
@indextables = grep /\.TAB/i, readdir INDEXDIR;
closedir INDEXDIR;

# Separate index tables from cumulative index tables
while ($table = shift @indextables) {
    $fullname = $index_dir . $table; # Get pathname
    if ($table =~ m/INDEX\.TAB/i and $table !~ m/CUMINDEX\.TAB/i) {
        push @temp_array, $fullname;
    } elsif ($table =~ m/CUMINDEX\.TAB/i or $table =~ m/CMIDX\.TAB/i) {
        push @cumtables, $fullname;
    }
}
# These arrays contain index and cumulative index tables
@indextables = sort @temp_array; # INDEX.TAB or axxINDEX.TAB
@cumtables = sort @cumtables;    # CUMINDEX.TAB or axxCMIDX.TAB
die "No index tables to process\n" unless (scalar @indextables > 0);

### Announce intentions
print "Checking the index files against the data:\n";
print "==========================================\n\n";
# Print out index files, if "verbose" option is set
if ($opt_v == 1) {
    print "Index tables:\n";
    print join "\n", @indextables;
    print "None" if (scalar @indextables == 0);
    print "\n\n";
    print "Cumulative index tables:\n";
    print join "\n", @cumtables;
    print "None" if (scalar @cumtables == 0);
    print "\n\n";
}

### Test 1 - Check that all filenames in index tables exist on the volume
### Process every index table
print "No index tables found\n" unless (scalar @indextables > 0);
foreach $table (@indextables) {
	
    # Check for a label
    $listtable = $table;
    $listtable =~ s/^.+[\\\/]//;
    $label = $table;
    $label =~ s/\.TAB/.LBL/ if ($table =~ m/\.TAB/);
    $label =~ s/\.tab/.lbl/ if ($table =~ m/\.tab/);
    unless (-e $label) {
        print "$listtable doesn\'t have a label\n";
        next;
    }
    
    # Get all filenames from the table
    &get_filenames ($label, $table); # Subroutine sets @filenames, $idx_count.
    push @all_filenames, @filenames; # Accumulate filenames.
    # Print filenames, if we're being verbose
    if ($opt_v == 1) {
        print "$listtable points to these files:\n\n";
        foreach $name (@filenames) {
    	    $listname = substr $name, $root_length; # How to display.
            $listname = uc $listname;
            print "$listname\n";
        }
        print "\n";
    }

    # Check filenames against the volume
    $case_err = 0; # Set flag if any case errors in table
    foreach $name (@filenames) {
        $name_uc = uc $name;
        $case_err = 1 if ($name ne $name_uc);
        $listname = substr $name, $root_length; # How to display.
        $listname = uc $listname;
        push @bad_filenames, $listname unless (-e $name or -e $name_uc);
    }
    
    # Print Test 1 results - Do all files pointed to by INDEX.TAB exist?
    
    ### This test wasn't working. Will fix later...
    # if ($case_err == 1) {
    #    print "ERROR: Index table filenames are not all uppercase.\n\n";
    #}
    
    if (scalar @bad_filenames > 0) {
        print "$listtable points to the following missing files:\n\n";
        print join "\n", @bad_filenames;
        print "\n\n";
    } else {
        print "$listtable points to $idx_count data files - all exist.\n\n";
    }
}

### Test 2 - Check that all data labels are indexed in INDEX.TAB

# Create a list of data directories
$data_dir = $root_dir . "data";
if (-e $data_dir) {
    @data_dirs = ($data_dir);
} else {
    opendir ROOTDIR, $root_dir or die "Can\'t open $root_dir\n";
    @raw_root_contents = grep !/^\.\.?$/, readdir ROOTDIR;
    @root_contents = sort @raw_root_contents;
    foreach $thing (@root_contents) {
    	$thing = $root_dir . $thing;
        next unless (-d $thing);
        next if ($thing =~ m/CATALOG/i);
        next if ($thing =~ m/INDEX/i);
        next if ($thing =~ m/CALIB/i);
        next if ($thing =~ m/BROWSE/i);
        next if ($thing =~ m/DATA/i);
        next if ($thing =~ m/DOCUMENT/i);
        next if ($thing =~ m/EXTRAS/i);
        next if ($thing =~ m/GAZETTER/i);
        next if ($thing =~ m/GEOMETRY/i);
        next if ($thing =~ m/SOFTWARE/i);
        next if ($thing =~ m/LABEL/i);
        push @data_dirs, $thing;
    }
}

# Get all the labels in all the data directories (@all_data_labels)
$File::Find::dont_use_nlink = 1;  # Truly evil Windows hack
fileparse_set_fstype("MSDOS");
foreach $dir (@data_dirs) {
    find(\&wanted, $dir); # Sets @all_data_labels
}

# Match each label against the filenames
foreach $label (@all_data_labels) {
    $label_match = 0;
    foreach $filename (@filenames) {
        if (uc $label eq uc $filename) {
            $label_match = 1;
            last;
        }
    }
    $listname = substr $label, $root_length;
    $listname = uc $listname;  # Print files in upper case
    push @bad_labels, $listname if ($label_match == 0);
}

# Print Test 2 results - Does every data file have an INDEX.TAB entry?
if (scalar @bad_labels > 0) {
    print "These data labels are missing entries in the index table(s):\n\n";
    print join "\n", @bad_labels;
    print "\n\n";
} else {
    print "All data labels have entries in the index table(s).\n\n";
}

### Test 3 - Find the volumes indexed in CUMINDEX.TAB
### Process every cumulative index table
print "No cumulative index tables found.\n\n\n" unless (scalar @cumtables > 0);
foreach $table (@cumtables) {
	
    # Check for a label
    $listtable = $table;
    $listtable =~ s/^.+[\\\/]//;
    $label = $table;
    $label =~ s/\.TAB/.LBL/ if ($table =~ m/\.TAB/);
    $label =~ s/\.tab/.lbl/ if ($table =~ m/\.tab/);
    unless (-e $label) {
        print "$listtable doesn\'t have a label\n";
        next;
    }

    # Use CUMINDEX.LBL to find the START_BYTE and BYTES of the VOLUME_ID
    open (CUMLBL, "<$label");
    while (<CUMLBL>) {
        if (/VOLUME_ID/ .. /END_OBJECT/) {
            $line = $_;
            $line =~ s/\s+$//;
            if ($line =~ m/START_BYTE/) {
                @splitline = split '=', $line;
                $start_byte_v = $splitline[1];
            }
            if ($line =~ m/BYTES/) {
                @splitline = split '=', $line;
                $bytes_v = $splitline[1];
            }
        }
    }
    close CUMLBL;

    # Get volumes from CUMINDEX.TAB
    $cum_count = 0;
    open (CUMTAB, "<$table");
    while ($line = <CUMTAB>) {
        $cum_count++;
        $volname = substr $line, ($start_byte_v - 1), $bytes_v;
        $volname =~ s/\s+$//g;  # Lose any trailing whitespace
        push @volnames, $volname; 
    }
    close CUMTAB;
    
    # Print Test 3 results - Volumes referenced by CUMINDEX.TAB
    print "$listtable has $cum_count entries for the following volumes:\n";
    @list = sort @volnames;
    $lastvol = $list[0];
    $vol_count = 0;
    foreach $vol (@list) {
        if ($vol ne $lastvol) {
            print "  $lastvol - $vol_count entries\n";
            $lastvol = $vol;
            $vol_count = 0;
        }
        $vol_count++;
    }
    print "  $list[scalar @list - 1] - $vol_count entries\n\n";
}

#######################
# Support Subroutines #
#######################

### Subroutine that gets all filenames from index tables
sub get_filenames {

    my $index_lbl = shift @_;
    my $index_tab = shift @_;

    # Get one of the following from INDEX.LBL:
    # 1. The START_BYTE and BYTES from the FILE_SPECIFICATION object
    # 2. The START_BYTE and BYTES from the PATH_NAME and the FILE_NAME
    
    open (IDXLBL, "<$index_lbl");
    $filespec_flag = 0;
    $path_flag = 0;
    $file_flag = 0;
    while (<IDXLBL>) {
    	
    	# Parse FILE_SPECIFICATION_NAME
        if (/ FILE_SPECIFICATION_NAME/ .. /END_OBJECT/) {
            $filespec_flag = 1;
            $line = $_;
            $line =~ s/\s+$//;
            if ($line =~ m/START_BYTE/) {
                @splitline = split '=', $line;
                $start_byte = $splitline[1];
            }
            if ($line =~ m/BYTES/) {
                @splitline = split '=', $line;
                $bytes = $splitline[1];
            }
        }
        
        # Parse PATH_NAME
        if (/ PATH_NAME/ .. /END_OBJECT/) {
            $path_flag = 1;
            $line = $_;
            $line =~ s/\s+$//;
            if ($line =~ m/START_BYTE/) {
                @splitline = split '=', $line;
                $start_byte_pathname = $splitline[1];
            }
            if ($line =~ m/BYTES/) {
                @splitline = split '=', $line;
                $bytes_pathname = $splitline[1];
            }
        }
        
        # Parse FILE_NAME
        if (/ FILE_NAME/ .. /END_OBJECT/) {
            $file_flag = 1;
            $line = $_;
            $line =~ s/\s+$//;
            if ($line =~ m/START_BYTE/) {
                @splitline = split '=', $line;
                $start_byte_filename = $splitline[1];
            }
            if ($line =~ m/BYTES/) {
                @splitline = split '=', $line;
                $bytes_filename = $splitline[1];
            }
        }    
    }
    close IDXLBL;
    
    # Check for errors in INDEX.LBL
    if (($filespec_flag == 0) and ($path_flag + $file_flag < 2)) {
        $index_lbl_name = basename($index_lbl);
    	$error = "no FILE_SPECIFICATION_NAME or PATH_NAME/FILE_NAME";
        die "ERROR: $index_lbl_name has $error\n\n\n";
    }

    # Get filenames from INDEX.TAB
    $idx_count = 0;
    open (IDXTAB, "<$index_tab");
    while ($line = <IDXTAB>) {
        $idx_count++;
    
        # Use FILE_SPECIFICATION_NAME information
        if ($filespec_flag == 1) {
            $filename = substr $line, ($start_byte - 1), $bytes;
            $filename =~ s/\"//g; # Lose quotes.
            
        } else {
        # Or use PATH_NAME and FILE_NAME information
            $path = substr $line, ($start_byte_pathname - 1), $bytes_pathname;
            $file = substr $line, ($start_byte_filename - 1), $bytes_filename;
            $path = $path . "/" unless ($path =~ m|\/\s*$|);
            $filename = $path . $file;

        }
        # Fix any whitespace and case problems in the filename
        $filename =~ s/\s//g;  # Get rid of any whitespace
        $filename = uc $filename if ($table =~ m/\.TAB/);
        $filename = lc $filename if ($table =~ m/\.tab/);
        push @filenames, $root_dir . $filename; 
    }
    close IDXTAB;
}

### Subroutine that finds all the data labels
sub wanted {
	
    # Get and parse every non-directory file
    $file = $File::Find::name;
    return if (-d $file);
    ($name, $path, $suffix) = fileparse($file,'\..*');
    
    # Detached labels should have an .LBL extension
    if ($suffix =~ m/\.LBL/i) {
        push @all_data_labels, $file;
        return;
    }

    # Attached labels should be found with the following test
    open (FILE_IN, "<$file");
    read FILE_IN, $buffer, 200;
    close FILE_IN;
    if ($buffer =~ /PDS_VERSION/) {
        push @all_data_labels, $file;
    }

}
