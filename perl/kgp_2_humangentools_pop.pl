#!/usr/bin/perl

use strict;
use warnings;

my $infile = $ARGV[0];
my $outfile = $ARGV[1];

unless ($infile and $outfile) {
	die "\n\nERROR: Not enough arguments.\n";
}

unless (-f $infile) {
	die "\n\nERROR: Cannot find $infile. check file path.\n";
}


open IN, "$infile";
open OUT, ">$outfile";

<IN>;

print OUT "id\tsex\tpopulation\tregion\tdataset\n";

# FamilyID SampleID FatherID MotherID Sex Population Superpopulation

while (<IN>) {
	my $line = $_;
	chomp $line;
	my @l = split /\s/, $line;
	my $ID = $l[1];
	my $sex = $l[4];
	if ($sex == 1) {
		$sex = "M";
	} elsif ($sex == 2) {
		$sex = "F";
	} elsif ($sex == 0) {
		$sex = "NA";
	}
	my $pop = $l[5];
	my $superpop = $l[6];
	
	print OUT "$ID\t$sex\t$pop\t$superpop\tKGP\n";
}

close IN;
close OUT;

exit;