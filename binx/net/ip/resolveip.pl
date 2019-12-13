#!/usr/bin/env perl

use strict;
use warnings;


use Getopt::Long;
use Socket;

my $silent;

my $result = GetOptions ("h"  => sub {
                          print "Help\n";
                          },
                      "s"  => \$silent);  # flag

my $addr=$ARGV[0];

my $name;

if ($addr =~ /^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$/) {
    $name = gethostbyaddr(inet_aton( $addr ), AF_INET)
} else {
    my $ip  = gethostbyname($addr);
    if ($ip) {
      my $rhost = gethostbyaddr( $ip, AF_INET);
      my $rip = gethostbyname( $rhost ) if defined $rhost;
      
      if (defined $rhost and ($ip ne $rip)) {
        print STDERR "dns reply is not correct " . $ip . " " . $rip . "\n" unless $silent;
      }
      $name = inet_ntoa($ip);
    }
}

if (defined $name and $name) {
  print "$name\n";
  exit(0);
} else {
    print STDERR "not found $addr\n" unless $silent;
    exit(143);
}
