#!/usr/bin/env perl

use strict;
use warnings;
# use autodie;
use File::Basename;
use Data::Dumper;

# use Cwd;
# use File::Spec;

our $debug = 0;
our $maxFileExtentionLength = 8;



our $opt = {};


sub main {

    process_arg(@ARGV);




    $debug = $opt->{"debug"} if $opt->{"debug"};



    my $highest = 0;
    my $currfile=$opt->{"file"};
    my $dir = "";
    ($dir = $opt->{"file"}) =~ s/\/[^\/]+$/\// if $opt->{"file"} =~ /\/[^\/]+$/;

    print "dir $dir \n" if $debug;


    my $match = '\d+([^\d]*(?:\.[^\.]{1,' . $maxFileExtentionLength . '})?)$';
    my $replace_num_regex = "\(\\d\+\)\$1";
    my $replace_highest = '"$highest$1"';


    (my $re = $currfile) =~ s/$match/\(\\d\+\)$1/;
    $re = '^' . $re . '$';



    print "currfile $currfile \n" if $debug;
    print "re $re \n" if $debug;
    print "dir $dir \n" if $debug;

    if ( defined $dir and $dir ) {
        unless ( -d "$dir") {
            print STDERR "No such $dir directory exists.\n";
            exit -1;
        }
    }

    opendir my $dh, ("$dir" or ".") or die "Could not open '$dir' for reading '$!'\n";
    my @matchedFiles = sort { ($a =~ /$re/)[0] <=> ($b =~ /$re/)[0] } grep { /$re/ } map { $dir . $_ } readdir $dh;
    closedir $dh;

    print "#matchedFiles $#matchedFiles \n" if $debug;


    my $currfileIndex = $#matchedFiles;

    print "1. currfileIndex $currfileIndex \n" if $debug;

    map { /$re/ and $1 > $highest and $highest = $1 } @matchedFiles;

    print Dumper(\@matchedFiles) if $debug;

    ( $currfileIndex )= grep { $matchedFiles[$_] eq $currfile } 0..$#matchedFiles;

    if (defined $currfileIndex) {
        print "2. currfileIndex = $currfileIndex \n" if $debug;
    } else {
       print "2. currfileIndex = undef \n" if $debug;
    }


    $currfileIndex = 0 unless defined $currfileIndex;

    if ( @matchedFiles ) {
        if ( defined $matchedFiles[ $opt->{seq} ] ) {
            print '$matchedFiles[ $opt->{"seq"} ] = ' . '$matchedFiles[ ' . $opt->{"seq"} . ' ] = ' . "$matchedFiles[ $opt->{seq} ] \n" if $debug;
        } else {
            print '$matchedFiles[ $opt->{"seq"} ] = ' . '$matchedFiles[ ' . $opt->{"seq"} . ' ] = ' . "undef \n" if $debug;
        }
    } else {
        print '@matchedFiles empty' if $debug;
    }

    my $next_file;
    if ( defined $matchedFiles[ $currfileIndex + $opt->{"seq"} ] ) {
        if ( $opt->{"nonexistant"} ) {
            # $highest += ( $opt->{"seq"} > 0 ? $opt->{"seq"} : 1);
            $highest += $opt->{"seq"};
            ($next_file = $currfile) =~ s/$match/$highest$1/;
        } else {
            $next_file = $matchedFiles[ $currfileIndex + $opt->{"seq"} ];
        }
    } else {
        print "else \$highest=$highest \n" if $debug;
        if ( @matchedFiles ) {
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
        } elsif ( $arg eq "-d" ) {
            $debug = $opt->{"debug"} = 1;
        } elsif ( $arg eq "-h" ) {
            $opt->{"help"} = 1;
        } elsif ( $arg eq "-n" ) {
            $opt->{"nonexistant"} = 1;
        } elsif ( $arg !~ /$numpattern/ and $arg ne "-d" and $arg ne "-h" and $arg ne "-n") {
            if (defined $opt->{"file"}) {
                dieWithHelp("wrong argument");
            } else {
                $opt->{"file"} = $arg;
            }
        } else {
            dieWithHelp("wrong argument");
        }
    }

    print Dumper($opt) if $debug;

    help() if $opt->{"help"};


    unless ( defined $opt->{"file"} ) {
        dieWithHelp("file not passed.");
    }

    unless ( defined $opt->{"seq"} ) {
        dieWithHelp("seq not known.");
    }
}

sub help {
    print <<EOF;
$0: [-h] [-d] [[-|+]?NUM] FILENAME
EOF
    exit;
}

sub dieWithHelp {
    my $desc = shift;
    help();
}



main(@ARGV)
