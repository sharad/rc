#!/usr/bin/perl
# timeclock_project_hours_report          doom@kzsu.stanford.edu
#                                         09 Jun 2009

use warnings;
use strict;
$|=1;
use Data::Dumper;

use File::Path     qw( mkpath );
use File::Basename qw( fileparse basename dirname );
use File::Copy     qw( copy move );
use Fatal          qw( open close mkpath copy move );
use Cwd            qw( cwd abs_path );
use Time::Local;

use Env qw(HOME);

our $VERSION = 0.01;
my $prog     = basename($0);

use Getopt::Std;
my %opt = ();
getopts('d', \%opt);
my $DEBUG   = $opt{d} || 0;   # TODO set to 0 when in production

my $log_file = "$HOME/.timeclock/default.log" || shift;

open my $log_fh, '<', $log_file or croak "$!";

my $parts_pat =
  qr{
      (\d{4}) /      # YYYY
      (\d{2}) /      # MM
      (\d{2}) \s+    # DD
      (\d{2}) :      # hours
      (\d{2}) :      # mins
      (\d{2}) \s+    # secs
      (.*?)          # project
      \s+
  }xms;

my $check_in_pat =
  qr{ ^
      (i) \s+          # code: clock in
      $parts_pat
      $
  }xms;

my $check_out_pat =
  qr{ ^
      (o) \s+          # code: clock out
      $parts_pat
      $
  }xms;

my $line_pat =
  qr{ ^
      ([io]) \s+          # code: clock in/out
      $parts_pat
      $
  }xms;

my $projects = {};
my ($project, $beg, $fin);
while( my $line = <$log_fh> ) {

  if ( ($line =~ m{ $line_pat }xms) ) {

    my ( $code,
         $year,
         $mon,
         $day,
         $hour,
         $min,
         $sec,
       ) = ($1, $2, $3, $4, $5, $6, $7);

    ($DEBUG) && print "code: >>$code<<\n";

    $project = $8 if defined( $code ) && $code eq 'i';

    if ( defined( $code ) ) {
      if ( $code eq 'i' ) {
        $beg = timelocal( $sec, $min, $hour, $day, ($mon-1), ($year-1900) );
      } elsif ( $code eq 'o' ) {
        $fin = timelocal( $sec, $min, $hour, $day, ($mon-1), ($year-1900) );

        my $delta = $fin - $beg;
        $delta /= 60 * 60;
        $projects->{ $project }->{ "$year/$mon/$day" } += $delta;

        $beg = 0;
        $fin = 0;
        $project = '';
      }

      if( ($DEBUG) && defined( $code ) ){
        print STDERR "$code $year/$mon/$day $hour:$min:$sec  $project\n";
      }

    }

  }
}

($DEBUG) && print STDERR Dumper( $projects ), "\n";

# $projects is a hash of hashes, keyed by project name and date, value is hours spent
# $VAR1 =
#  {
#        'firewall' => {
#                        '2009/06/09' => '0.786388888888889'
#                       },
#        'screwing-off' => {
#                        '2009/06/09' => '0.00361111111111111'
#                       },
#        'emacs-life' => {
#                        '2009/06/09' => '0.0911111111111111'
#                       }
#   };


foreach my $project (keys %{ $projects }) {
  my $total = 0;
  printf "%s\n", $project;
  foreach my $date (keys %{ $projects->{ $project } } ){
    my $hours = $projects->{ $project }->{ $date };
    $total += $hours;
    printf "%12s  %.1f\n", $date, $hours;
  }
  printf "  total:      %.1f\n", $total;
  print "\n";
}



__END__

=head1 NAME

timeclock_project_hours_report - summarize time on projects from timeclock.el log

=head1 SYNOPSIS

  crunch_timeclock_logs -[options]  [ timeclock.log ]

  Options:
     -d          debug

=head1 OPTIONS

=over 8

=item B<-d>

Turn on debug messages.

=back

=head1 DESCRIPTION

B<timeclock_project_hours_report> is a script which crunches
through the timeclock.el/timeclock-x.el log file and totals up
time spent on different projects.

=head1 NOTES

=head2 sample log

i 2009/06/09 16:08:05 firewall
o 2009/06/09 16:42:14

>> Figuring out an alternate way of burning a disk, waiting for

>> a second copy of the iso to come down before I burn it.
i 2009/06/09 16:42:16 emacs-life
o 2009/06/09 16:48:49


>> sent in the bug report via email.
i 2009/06/09 16:48:51 firewall
o 2009/06/09 17:36:02 need_food

>> Flailing around doing a CD iso fandango, for some reason

>> trantor refuces to boot off of the damn drive.



=head1 AUTHOR

Joseph Brenner, E<lt>doom@kzsu.stanford.eduE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009 by Joseph Brenner

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.2 or,
at your option, any later version of Perl 5 you may have available.

=head1 BUGS

None reported... yet.
p
=cut
