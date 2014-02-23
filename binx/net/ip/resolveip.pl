#!/usr/bin/perl
use Socket;

use strict;
use warnings;



my $addr=$ARGV[0];
my $name;



if ($addr =~ /^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$/) {
    $name = gethostbyaddr(inet_aton($ARGV[0] ),AF_INET)
} else {
    my $ip = gethostbyname($ARGV[0]);
    $name = inet_ntoa($ip);
}

print "$name\n"
