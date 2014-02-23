#!/usr/bin/perl
use Socket;

use strict;
use warnings;



my $addr=$ARGV[0];
my $name;



if ($addr =~ /^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$/) {
    $name = gethostbyaddr(inet_aton( $addr ), AF_INET)
} else {
    my $ip  = gethostbyname($addr);
    my $rhost = gethostbyaddr( $ip, AF_INET);
    # print $rhost . "\n";
    my $rip = gethostbyname( $rhost ) ;
    if ($ip ne $rip) {
        print STDERR "dns reply is not correct " . $ip . " " . $rip . "\n";
    }
    $name = inet_ntoa($ip);
}

print "$name\n"
