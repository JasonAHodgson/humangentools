#!/usr/bin/perl

use strict;
use warnings;

my $intable = $ARGV[0];
my $data = $ARGV[1];
my $out = $ARGV[2];


open KGP, "$data";

my %lat; #pop => lat
my %lon; #pop => lon

while (<KGP>) {
	my $line = $_;
	chomp $line;
	my @l = split "\t", $line;
	my $pop = $l[0];
	my $lat = $l[5];
	my $lon = $l[6];
	
	unless (exists $lat{$pop}){
		$lat{$pop} = $lat;
	}
	
	unless (exists $lon{$pop}){
		$lon{$pop} = $lon;
	}
	
	
}

exit;