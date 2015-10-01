#!/usr/local/bin/perl
#
# Script: checkref.pl - Check REF.CAT references against AllRefs.txt
# Usage:  checkref.pl [-p] [-v] [-w] <volume root>
#
# Author: Joel Wilf
# Date: 7/30/99
#
# Description:
#   This script checks references in REF.CAT against the AllRefs.txt
#   database.  It finds and lists the following:
#
#     1. Duplicate references in REF.CAT
#     2. References in *.CAT that are missing from REF.CAT
#     3. References in REF.CAT that are not in the database
#     4. Citations in REF.CAT that don't match those in the database
#
#   When a citation in REF.CAT doesn't match the same reference in
#   the database, both are printed out for comparison. Note: The
#   AllRefs.txt database must be in the current working directory.
#
#   Checkref.pl takes one required parameter, the root directory of
#   the volume.  If the -p option is set, a private copy of REF.CAT
#   is used.  It must be named "REF.CAT" in the current working
#   directory.  This is convenient for editing REF.CAT.  If the -v
#   option is set, the list of REFKEYIDs in REF.CAT and those cited
#   are also printed out. If the -w option is set, whitespace is
#   considered significant when matching citations.
# End:
#
# Revisions:
#   1999-10-12, J. Wilf, Added option to ignore whitespace.
#   2001-03-17, J. Wilf, Made ignoring whitespace the default.
# End:


use Getopt::Std;
use File::Basename;
$| = 1;

# Get the PVT home directory
$home_pvt = dirname($0);

# Get options and volume root
getopts('pvw');     # If -p is set, $opt_p == 1, etc.
$usage = "Usage: checkref.pl [-p] [-v] [-w] <volume root>";
$root_dir = shift or die "$usage\n";
die "Can\'t find $root_dir\n" unless (-e $root_dir);

# Get CATALOG directory and REF.CAT correcting for case
$catalog_dir = $root_dir . "/CATALOG";
$catalog_dir = $root_dir . "/catalog" unless (-e $catalog_dir); 
$ref_file = $catalog_dir . "/REF.CAT";
$ref_file = $catalog_dir . "/ref.cat" unless (-e $ref_file);
$ref_file = "$home_pvt/REF.CAT" if ($opt_p == 1);
unless (-e $ref_file) {
    $error = "Can\'t find the REF.CAT file";
    die "ERROR: $error\n\n\n";
}

# Get allrefs.txt
$allrefs = "$home_pvt/data/AllRefs.txt";
die "Can\'t find $allrefs\n" unless (-e $allrefs);

# Announce intentions
print "Checking references:\n";
print "====================\n\n";

# Read AllRefs.txt into a hash (%database)
open (ALLREFS, "<$allrefs");
$key = "none_yet";
while ($line = <ALLREFS>) {

    # Get rid of <CR> for Unix systems
    $line =~ s/\r//;  

    # Get the REFKEYID and citation, if the line isn't blank
    if ($line !~ m/^$/) {
       $line =~ s/\n/ /;
       ($key, $num, $val) = split /\|/, $line;
       $cite = $val if ($num eq "1");
       $cite = ($cite . $val) if ($num ne "1");
       
    # Save the key and citation in hash, if the line is blank
    } else {       
       next if ($key eq "none_yet"); # Skip leading blanks
       $database{$key} = "$cite";
    }
}
close ALLREFS;

# Read REF.CAT into a hash (%refcat) and its keys into an array (@ref_keys)
open (REFCAT, "<$ref_file");
while ($line = <REFCAT>) {

    # Get the REFKEYID and save it
    if ($line =~ m/REFERENCE_KEY_ID/) {
        @line = split /=/, $line;
        $key_refcat = @line[1];
        $key_refcat =~ s/\s//g;
        $key_refcat =~ s/\"//g;
        push @ref_keys, "$key_refcat";
    }
    
    # Get the citation 
    if ($line =~ m/REFERENCE_DESC.+\"(.*)$/) {
        $cite_refcat = $1;
        $cite_refcat =~ s/\s+$//; # adjust trailing whitespace
        $cite_refcat = $cite_refcat . " ";
        while ($cite_refcat !~ s/\"//) {
            $line = <REFCAT>;
            chomp $line;
            $line =~ s/^\s+//;  #Lose leading whitespace
            $cite_refcat = $cite_refcat . $line;
            $cite_refcat =~ s/\s+$//; # Adjust trailing whitespace
            $cite_refcat = $cite_refcat . " ";
        }
    # Save the key and citation in hash
    $refcat{$key_refcat} = "$cite_refcat";
    }
}
close REFCAT;

# Test 1 - Find duplicate references in REF.CAT
@ref_keys = sort @ref_keys;
$prev_item = "";
foreach $item (@ref_keys) {
    push @duplicates, $item if ($item eq $prev_item);
    $prev_item = $item;
}

# Test 2 - Find references in *.CAT that are missing from REF.CAT

# Find all the *.CAT files
opendir (CATALOG,"$catalog_dir") or die "Can\'t open $catalog_dir";
@catfiles = grep /\.CAT/i, readdir CATALOG;
closedir CATALOG;

# Process each catolog file
foreach $catfile (@catfiles) {

    # Get the REFKEYIDs in every *.CAT and find match in REF.CAT
    next if ($catfile eq "REF.CAT"); # Don't match REF.CAT with itself
    $catpath = $catalog_dir . "/" . $catfile; # Use real path
    open (CATFILE, "<$catpath");
    while ($line = <CATFILE>) {
        next unless ($line =~ m/\[[\w\&\-\\]+\]/);  # Lose lines without keys
        @keys_in_line = ($line =~ m/\[[\w\&\-\\]+\]/g); # Get all keys
                
        # Check each REFKEYID for existance in REF.CAT
        foreach $key_in_line (@keys_in_line) {
            $key_in_line =~ s/\[//;
            $key_in_line =~ s/\]//;
            push @cat_citations, "$key_in_line - cited in $catfile";
            unless (exists($refcat{$key_in_line})) {
                push @missing_ref_keys, "$key_in_line - cited in $catfile";
            }
        }
    }
    close CATFILE;
}
@cat_citations = sort @cat_citations;
@missing_ref_keys = sort @missing_ref_keys;

# Test 3 - Find references in REF.CAT that are not in the database

# Check each REFKEYID in REF.CAT for existance in the database
# Save existing references for next test (matching citations)
foreach $ref_key (@ref_keys) {
    if (exists($database{$ref_key})) {
        push @old_references, "$ref_key";
    } else {
        push @new_references, "$ref_key";
    }
}

# Test 4 - Find citations in REF.CAT that don't match those in the database
foreach $reference (@old_references) {
    $text_ref = $refcat{$reference};
    $text_db = $database{$reference};
    
    # If -w flag is set, use whitespace in comparisons
    unless ($opt_w) {
        $text_ref =~ s/\s//g;
        $text_db =~ s/\s//g;
    }
    
    # Make the comparisons
    if ($text_ref ne $text_db) {
        push @diffs, "$reference";
        push @diffs, "REF: $refcat{$reference}";
        push @diffs, "DB:  $database{$reference}\n";
    }
}

# If -v option is set, print all REFKEYIDs in REF.CAT and *.CAT files
if ($opt_v == 1) {
    print "REF.CAT has the following entries:\n\n";
    print join "\n", @ref_keys;
    print "\n\n\n";
    print "The following references are cited in CATALOG/*.CAT:\n\n";
    print join "\n", @cat_citations;
    print "\n\n\n";
}

# Print the results of test 1, finding duplicates in REF.CAT
if (scalar @duplicates > 0) {
    print "REF.CAT has the following duplicate entries:\n\n";
    $prev_entry = "";
    while ($entry = shift @duplicates) {
        print "$entry\n" unless ($entry eq $prev_entry);
        $prev_entry = $entry;
    }
    print "\n\n";
} else {
    print "REF.CAT has no duplicate entries.\n\n";
}

# Print the results of test 2, finding missing references
if (scalar @missing_ref_keys > 0) {
    print "REF.CAT is missing the following references:\n\n";
    $prev_entry = "";
    while ($entry = shift @missing_ref_keys) {
        print "$entry\n" unless ($entry eq $prev_entry);
        $prev_entry = $entry;
    }
    print "\n\n";
} else {
    print "REF.CAT has every reference that is cited in a *.CAT file.\n\n";
}

# Print the results of test 3, finding references not in the database
if (scalar @new_references > 0) {
    print "REF.CAT has references that are not in the database:\n\n";
    print join "\n", @new_references;
    print "\n\n\n";
} else {
    print "All references in REF.CAT are also in the database.\n\n";
}

# Print the results of test 4, comparing REF.CAT citations with AllRefs.txt
if (scalar @diffs > 0) {
    print "Differences between REF.CAT and database citations:\n\n";
    print join "\n", @diffs;
    print "\n\n";
} else {
    print "All references in REF.CAT match those in the database.\n\n";
}
