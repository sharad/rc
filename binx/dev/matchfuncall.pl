#!/usr/bin/env perl
# ASN.1

use strict;
use warnings;

use Regexp::Common;
use Getopt::Std;


# declare the perl command line flags/options we want to allow
my %options=();
getopts("C:A:B:f:", \%options);


my $debuglevel = 0;
if ( $options{f} ) {
    my $function = $options{B};
} else {
    print STDERR "-f function name required";
    exit -1;
}
my $cbefore = "";
my $cafter  = "";
my $content;
my $bp = $RE{balanced}{-parens=>'()'};
my $matchcount = 0;


local $/;


if ($debuglevel > 3) {
    print $_, "\n" for @ARGV ;
}

# $content .=  <> for @ARGV;
# while (<>) {
#     $content .= $_;
# }

$cbefore = '(?:[^\n]*\n+){0,' . $options{B} . '}' if $options{B};
$cafter  = '(?:[^\n]*\n+){0,' . $options{A} . '}' if $options{A};
$cbefore = $cafter = '(?:[^\n]*\n+){0,' . $options{C} . '}' if $options{C};

$content .= $_ while (<>);

print $content if $debuglevel > 2;

while ( $content =~ m{ (
                           $cbefore
                           [^\n]*
                           MELF
                           \s*
                           $bp
                           \s*
                           ;
                           [^\n]*
                           $cafter
                       )
                 }smgix ) {
#while ( $content =~ m/( \n [^\n]+ \n [^\n]* MELF \s* $bp \s* ; ) /smgix ) {

    # g for multiple match
    # x for readability
    # m ^ and $ do not represent start and end of line.
    # s to include newline in `.'
    $matchcount++;

    print "${matchcount}:\n\n\n", $1, "\n\n\n";
}


print "\n\n\n\nTotal $matchcount found.\n";


# ##### not working
# while ( <> =~ m{ ( MELF \s* $bp \s* ; )  }smgix ) {
#     # g for multiple match
#     # x for readability
#     # m ^ and $ do not represent start and end of line.
#     # s to include newline in `.'
#     print $1, "\n";
# }
# ##### not working

__END__



