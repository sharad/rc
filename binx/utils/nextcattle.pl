#!/usr/bin/env perl

use strict;
use warnings;
# use autodie;
use File::Basename;
use Data::Dumper;

# use Cwd;
# use File::Spec;

our $opt = {};
our $debug = 0;
our $maxFileExtentionLength = 8;
our @compressExtentions = qw( gz bz2 xz lzma );


sub main {

    process_arg(@ARGV);




    $debug = $opt->{"debug"} if $opt->{"debug"};



    my $highest = 0;
    my $currfile=$opt->{"file"};




    my $dir = "";

    my $compressExtentions = '(:?\.(:?' . (join "|", @compressExtentions) . '))';

    debug( "compressExtentions = $compressExtentions\n" );

    debug( "currfile $currfile \n" );
    $currfile =~ s/${compressExtentions}$//;
    debug( "currfile $currfile \n" );

    ($dir = $opt->{"file"}) =~ s/\/[^\/]+$/\// if $opt->{"file"} =~ /\/[^\/]+$/;

    debug( "dir $dir \n" );


    my $match = '\d+([^\d]*(?:\.[^\.]{1,' . $maxFileExtentionLength . '})?' . $compressExtentions . '?' . ')$';
    my $replace_num_regex = "\(\\d\+\)\$1";
    my $replace_highest = '"$highest$1"';

    debug( "match = $match\n" );

    (my $re = $currfile) =~ s/$match/\(\\d\+\)$1/;
    $re = '^' . $re . $compressExtentions . '?' . '$';



    debug( "currfile = $currfile \n" );
    debug( "re       = $re \n" );
    debug( "dir      = $dir \n" );

    if ( defined $dir and $dir ) {
        unless ( -d "$dir") {
            print STDERR "No such $dir directory exists.\n";
            exit -1;
        }
    }

    opendir my $dh, ("$dir" or ".") or die "Could not open '$dir' for reading '$!'\n";
    my @matchedFiles = sort { ($a =~ /$re/)[0] <=> ($b =~ /$re/)[0] } grep { /$re/ } map { $dir . $_ } readdir $dh;
    closedir $dh;

    debug( "#matchedFiles $#matchedFiles \n" );


    my $currfileIndex = $#matchedFiles - 1;

    debug( "1. currfileIndex $currfileIndex \n" );

    map { /$re/ and $1 > $highest and $highest = $1 } @matchedFiles if (@matchedFiles);

    debug( "highest = $highest \n" );

    debug( Dumper(\@matchedFiles) );

    unless ( defined $opt->{"latest"} ) {
        debug( "latest not defined \n");
        ( $currfileIndex )= grep { $matchedFiles[$_] =~ /^${currfile}${compressExtentions}?$/ } 0..$#matchedFiles;
    }

    if (defined $currfileIndex) {
        debug( "2. currfileIndex = $currfileIndex \n" );
    } else {
        debug( "2. currfileIndex = undef \n" );
    }


    $currfileIndex = 0 unless defined $currfileIndex;

    if ( @matchedFiles ) {
        if ( defined $matchedFiles[ $opt->{seq} ] ) {
            debug( '$matchedFiles[ $currfileIndex + $opt->{"seq"} ] = ' . '$matchedFiles[ ' . $currfileIndex + $opt->{"seq"} . ' ] = ' . "$matchedFiles[ $opt->{seq} ] \n" );
        } else {
            debug( '$matchedFiles[ $currfileIndex + $opt->{"seq"} ] = ' . '$matchedFiles[ ' . $currfileIndex + $opt->{"seq"} . ' ] = ' . "undef \n" );
        }
    } else {
        debug( '@matchedFiles empty' );
    }

    my $next_file;
    if ( defined $matchedFiles[ $currfileIndex + $opt->{"seq"} ] ) {
        # if ( $opt->{"latest"} ) {
        #     # $highest += ( $opt->{"seq"} > 0 ? $opt->{"seq"} : 1);
        #     $highest += $opt->{"seq"};
        #     ($next_file = $currfile) =~ s/$match/$highest$1/;
        # } else {
        #     $next_file = $matchedFiles[ $currfileIndex + $opt->{"seq"} ];
        # }
        $next_file = $matchedFiles[ $currfileIndex + $opt->{"seq"} ];
    } else {
        debug( "else \$highest=$highest \n" );
        # if ( @matchedFiles ) {
        #     $highest += $opt->{"seq"};
        #     ($next_file = $currfile) =~ s/$match/$highest$1/;
        # } else {
        #     $next_file = $currfile;
        # }
        $highest += $opt->{"seq"};
        ($next_file = $currfile) =~ s/$match/$highest$1/;
    }


    debug( "Next file: $next_file\n" );

    print "$next_file\n";

}

sub process_arg {

    my $numpattern = '^(:?\+|\-)?\d+$';
    $opt->{"seq"}  = 1;

    # if ( $#_ > 1) {
    #     die "Error more than 2 arguments."
    # }

    while (defined (my $arg = shift) ) {

        debug( "$arg \n" );


        if ( $arg =~ /$numpattern/ ) {
            $opt->{"seq"} = ($arg + 0);
        } elsif ( $arg eq "-d" ) {
            $debug = $opt->{"debug"} = 1;
        } elsif ( $arg eq "-n" ) {
            $debug = $opt->{"nonexisting"} = 1;
        } elsif ( $arg eq "-h" ) {
            $opt->{"help"} = 1;
        } elsif ( $arg eq "-l" ) {
            $opt->{"latest"} = 1;
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

    debug( Dumper($opt) );

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
$0: [-d] [-h] [-n] [[-|+]?NUM] FILENAME
EOF
    exit;
}

sub dieWithHelp {
    my $desc = shift;
    help();
}

sub debug {
    my $line = shift;
    chomp($line);
    print $line . "\n" if $debug;
}



main( @ARGV )
