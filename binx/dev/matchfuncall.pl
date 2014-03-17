#!/usr/bin/env perl
# ASN.1

use strict;
use warnings;

use Regexp::Common;
use Getopt::Std;

my $function;
my $cbefore;
my $cafter;
my $bp;
my %options=();
my $debuglevel;

sub processsargs() {
  # declare the perl command line flags/options we want to allow
  getopts("C:A:B:f:lp", \%options);


  $debuglevel = 0;
  if ( $options{f} ) {
    $function = $options{f};
  } else {
    print STDERR "-f function name required";
    exit -1;
  }

  # print "show lines $options{l}\n";


  $cbefore = "";
  $cafter  = "";
  $bp = $RE{balanced}{-parens=>'()'};


  # local $/;


  if ($debuglevel > 3) {
    print $_, "\n" for @ARGV ;
  }


  # $cbefore = $cafter = '(?:[^\n]*\n+){0,' . $options{C} . '}' if $options{C};
  # $cbefore = '(?:[^\n]*\n+){0,' . $options{B} . '}' if $options{B};
  # $cafter  = '(?:[^\n]*\n+){0,' . $options{A} . '}' if $options{A};


  $cbefore = $cafter = '(?:[^\n]*\n){0,' . $options{C} . '}' if $options{C};
  $cbefore = '(?:[^\n]*\n){0,' . $options{B} . '}' if $options{B};
  $cafter  = '(?:[^\n]*\n){0,' . $options{A} . '}' if $options{A};

  if ( ! $options{A}  && $options{C} ) {
    $options{A}  = $options{C};
  }

  if ( ! $options{B}  && $options{C} ) {
    $options{B}  = $options{C};
  }
}



sub match1 () {

  local $/;
# $content .=  <> for @ARGV;
# while (<>) {
#     $content .= $_;
# }

  my $matchcount = 0;
  processsargs;
  my $content .= $_ while (<>);

  print $content if $debuglevel > 2;

  #                           [^\n]*

  while ( $content =~ m{ (
                           $cbefore
                           [^\n]*
                           \s+
                           $function
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

    print "${matchcount}: at line ",  , " \n\n\n", $1, "\n\n\n";
  }

  print "\n\n\n\nTotal $matchcount found.\n";
}


sub match2 () {

  my $matchcount = 0;
  processsargs;
  my $linenobuffer = $options{A} + $options{B} + 10;

  foreach my $file ( @ARGV ) {

        my $lineno = 0;
        my $lastclearlineno = 0;
        my $lines = "";
        my $lastmatch = "";
        my $xxmatched = 0;

        open(my $fh, '<', $file) # always use a variable here containing filename
            or die "Unable to open file, $!\n";

        while ( <$fh> ) {

            $lines .= $_;
            print STDERR "$file: $_" if $options{l};
            $lineno ++;

            if ( $lines =~
                     m{
                          (
                              $cbefore
                              [^\n]*
                              \s+
                              $function
                              \s*
                              $bp
                              \s*
                              ;
                              [^\n]*
                              $cafter
                          )
                        }smix ) {
                # g for multiple match
                # x for readability
                # m ^ and $ do not represent start and end of line.
                # s to include newline in `.'

                $lastmatch = $1;
                if ( $xxmatched ++ >= $options{A} ) {
                    $xxmatched = 0;
                }
            } else {
                $xxmatched = 0;
            }


            if ( 0 == $xxmatched && $lastmatch ) {
                my $newlines = $lastmatch =~ tr/\n//;

                print "${matchcount}: ";

                if ( $newlines ) {
                    print "Match found in $file on lines ", $lineno - $newlines, " through $lineno\n";
                }
                else {
                    print "Match found in $file on line $lineno\n";
                }

                print "\n\n\n", $lastmatch, "\n\n\n";

                if ( $lastmatch =~ m{ (
                                      [^\n]*
                                      $cafter
                                      $
                                  )
                            }smix ) {
                    my $x = $_;
                    $x =~ s/^.*\n//;
                    $lines = $x;
                }

                $matchcount ++;
                $lastmatch = "";
            }

            my $len = $lines =~ tr/\n//;

            if ( $len > $linenobuffer ) {
                $lines =~ s/^.*\n//;
                print STDERR "cleared $file at line $lineno current length $len \n" if $options{p};
            }
        }

        if ( $lastmatch ) {
            my $newlines = $lastmatch =~ tr/\n//;

            print "${matchcount}: ";

            if ( $newlines ) {
                print "Match found in $file on lines ", $lineno - $newlines, " through $lineno\n";
            }
            else {
                print "Match found in $file on line $lineno\n";
            }

            print "\n\n\n", $lastmatch, "\n\n\n";

            if ( $lastmatch =~ m{ (
                                      [^\n]*
                                      $cafter
                                      $
                                  )
                            }smix ) {
                my $x = $_;
                $x =~ s/^.*\n//;
                $lines = $x;
            }

            $matchcount ++;
            $lastmatch = "";
        }
    }
    print "\n\n\n\nTotal $matchcount found.\n";
}



sub match3 () {

  my $matchcount = 0;
  processsargs;

  foreach my $file ( @ARGV ) {

    my $lineno = 0;

    open(my $fh, '<', $file) # always use a variable here containing filename
      or die "Unable to open file, $!\n";

    my $content;

    while (<$fh>) {
      $content .= $_;
    }

    # my $content .= $_ while (<$fh>);

    while ( $content =~ m{ (
                             $cbefore
                             [^\n]*
                             (?:\W|\s)*
                             $function
                             \s*
                             $bp (?:\W|\s|\))* ;? (?:\W|\s)*
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

          my $mstart = substr($content, $-[0]) =~ tr/\n//;
          my $mend   = $1 =~ tr/\n//;

          my $clen = length $content;

          print "content len: $clen \n";

          print "${matchcount}: in $file at line $mstart of length $mend \n\n\n", $1, "\n\n\n";
        }

    }
    print "\n\n\n\nTotal $matchcount found.\n";
}


match3

__END__
