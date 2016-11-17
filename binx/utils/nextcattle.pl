#!/usr/bin/env perl

use strict;
use warnings;
use autodie;
use File::Basename;
use Data::Dumper;

our $debug = 0;


our $opt = {};


sub main {

    process_arg(@ARGV);




    $debug = $opt->{"debug"} if $opt->{"debug"};



    my $highest = 0;
    my $currfile=$opt->{"file"};
    my $dir = "";
    ($dir = $opt->{"file"}) =~ s/\/[^\/]+$/\// if $opt->{"file"} =~ /\/[^\/]+$/;

    print "dir $dir \n" if $debug;


    my $match = '\d+([^\d]*(?:.[^.]{1,8})?)$';
    my $replace_num_regex = "\(\\d\+\)\$1";
    my $replace_highest = '"$highest$1"';


    (my $re = $currfile) =~ s/$match/\(\\d\+\)$1/;


    print "currfile $currfile \n" if $debug;
    print "re $re \n" if $debug;
    print "dir $dir \n" if $debug;


    opendir my $dh, ($dir or ".");
    my @matched_files = sort { ($a =~ /$re/)[0] <=> ($b =~ /$re/)[0] } grep { /$re/ } map { $dir . $_ } readdir $dh;
    closedir $dh;

    print "#matched_files $#matched_files \n" if $debug;


    my $currfileIndex = $#matched_files;

    print "1. currfileIndex $currfileIndex \n" if $debug;

    map { /$re/ and $1 > $highest and $highest = $1 } @matched_files;

    print Dumper(\@matched_files) if $debug;

    ( $currfileIndex )= grep { $matched_files[$_] eq $currfile } 0..$#matched_files;


    print "2. currfileIndex $currfileIndex \n" if $debug;

    $currfileIndex = 0 unless defined $currfileIndex;

    print '$matched_files[ $opt->{"seq"} ] = ' . "$matched_files[ $opt->{seq} ] \n" if $debug;

    my $next_file;
    if ( defined $matched_files[ $currfileIndex + $opt->{"seq"} ] ) {
        if ( $opt->{"nonexistant"} ) {
            # $highest += ( $opt->{"seq"} > 0 ? $opt->{"seq"} : 1);
            $highest += $opt->{"seq"};
            ($next_file = $currfile) =~ s/$match/$highest$1/;
        } else {
            $next_file = $matched_files[ $currfileIndex + $opt->{"seq"} ];
        }
    } else {
        print "else \$highest=$highest \n" if $debug;
        if ( @matched_files ) {
            $highest += $opt->{"seq"};
            ($next_file = $currfile) =~ s/$match/$highest$1/;
        } else {
            $next_file = $currfile;
        }
    }


    print "Next file: $next_file\n" if $debug;

    print "$next_file\n";

}

sub process_arg {

    my $numpattern = '^(:?\+|\-)?\d+$';
    $opt->{"seq"}  = 1;

    # if ( $#_ > 1) {
    #     die "Error more than 2 arguments."
    # }

    while (defined (my $arg = shift) ) {

        print "$arg \n" if $debug;


        if ( $arg =~ /$numpattern/ ) {
            $opt->{"seq"} = ($arg + 0);
        } elsif ( $arg eq "-n" ) {
            $opt->{"nonexistant"} = 1;
        } elsif ( $arg eq "-d" ) {
            $opt->{"debug"} = 1;
        } elsif ( $arg eq "-h" ) {
            $opt->{"help"} = 1;
        } elsif ( $arg !~ /$numpattern/ ) {
            if (defined $opt->{"file"}) {
                die "wrong argument";
            } else {
                $opt->{"file"} = $arg;
            }
        } else {
            die "wrong argument";
        }
    }

    # print Dumper($opt) if $debug;

    help() if $opt->{"help"};


    unless ( defined $opt->{"file"} ) {
        die "file not passed."
    }

    unless ( defined $opt->{"seq"} ) {
        die "seq not known."
    }
}

sub help {
    print <<EOF;
$0: [-h] [-d] [[-|+]?NUM] FILENAME
EOF
    exit;
}


main(@ARGV)
