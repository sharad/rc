#!/usr/bin/env perl

# http://perl.about.com/od/perltutorials/a/genpassword.htm

my $plength = 10;

unless ($#ARGV < 0) {
    if ( $ARGV[0] =~ /^\d+$/)  {
        $plength = $ARGV[0];
    } else {
        print STDERR "argument shuld be a fixed number.\n";
        exit -1;
    }
}

print generatePassword($plength) . "\n";
exit;

sub generatePassword {
    $length = shift;
    $possible = 'abcdefghijkmnpqrstuvwxyz23456789ABCDEFGHJKLMNPQRSTUVWXYZ';
    while (length($password) < $length) {
        $password .= substr($possible, (int(rand(length($possible)))), 1);
    }
    return $password
}
