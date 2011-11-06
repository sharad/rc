#!/usr/bin/env perl

# from http://members.optusnet.com.au/~charles57/GTD/orghip.pl


use strict;
use warnings;

# Read an org file and create lists by context (tag)
# Written by Charles Cave  charles.cave (at) gmail.com
# 28th June 2006

my $orgfile = shift;
defined($orgfile) or die "syntax is orghip.pl orgfilename\n";

open(my $org, "<", $orgfile) or die "Cannot open $orgfile\n";
my %lists = ();
my $now = localtime();

while (<$org>) {
  my $line = $_;
  chomp($line);
  if ($line =~ /^\*+\s*(.*?):([A-Za-z]+):/) {
      my $hdng = "$1";
      my $tag  = $2;
      if ( defined($lists{$tag}) ) {
          $lists{$tag} = $lists{$tag}."\n".$hdng;
      } else {
          $lists{$tag} = $hdng;
      }
  }
}

print "Date Printed: $now\n";

foreach ("PROJECT", "OFFICE", "HOME", "COMPUTER", "DVD", "READING") {
  process_context($_) if $lists{$_};
}

# print any remaining contexts
foreach my $key (sort keys %lists) {
    process_context($key);
}

sub process_context {
   my $context = shift;
   print "\n\n$context:\n";
   foreach my $item( split(/\n/, $lists{$context}) ) {
       print "[ ] $item\n";
   }
   delete $lists{$context};
}

