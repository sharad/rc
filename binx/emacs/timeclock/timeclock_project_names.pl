#!/usr/bin/perl
# timeclock_project_names                   doom@kzsu.stanford.edu
#                                           11 Jun 2009

use warnings;
use strict;
$|=1;
use Data::Dumper;

use File::Path     qw( mkpath );
use File::Basename qw( fileparse basename dirname );
use File::Copy     qw( copy move );
use Fatal          qw( open close mkpath copy move );
use Cwd            qw( cwd abs_path );

use Env qw(HOME);

our $VERSION = 0.01;
my  $prog    = basename($0);

use Getopt::Std;
my %opt = ();
getopts('d', \%opt);
my $DEBUG   = $opt{d} || 0;   # TODO set default to 0 when in production

my $log_file = "$HOME/.timeclock/default.log" || shift;
open my $log_fh, '<', $log_file or croak "$!";

# TODO cut and pasted patterns from:
#   ~/End/Cave/EmacsPerl/bin/timeclock_project_hours_report
# create a module for this stuff?
my $parts_pat =
  qr{
      (\d{4}) /      # YYYY
      (\d{2}) /      # MM
      (\d{2}) \s+    # DD
      (\d{2}) :      # hours
      (\d{2}) :      # mins
      (\d{2}) \s+    # secs
      (.*?)           # project
      \s*
  }xms;

my $check_in_pat =
  qr{ ^
      (i) \s+          # code: clock in
      $parts_pat
      $
  }xms;

my %projects;
while( my $line = <$log_fh> ) {
  if ( ($line =~ m{ $check_in_pat }xms) ) {

    my $code = $1;

    my $project;
    if ( defined( $code ) && $code eq 'i') {
      $project = $8;
    }

    $projects{ $project } = 1;

  }
}

my @names = sort ( keys %projects );
($DEBUG) && print Dumper( \@names ), "\n";

print join "\n", @names, "\n";



__END__

=head1 NAME

timeclock_project_names - get list of project names from timeclock.el log

=head1 SYNOPSIS

  timeclock_project_names -[options] [arguments]

  Options:
     -d          debug

=head1 OPTIONS

=over 8

=item B<-d>

Turn on debug messages.

=back

=head1 DESCRIPTION

B<timeclock_project_names> is a script which

(( TODO  insert explaination
   This is stub documentation created by template.el.  ))

=head1 AUTHOR

Joseph Brenner, E<lt>doom@kzsu.stanford.eduE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009 by Joseph Brenner

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.2 or,
at your option, any later version of Perl 5 you may have available.

=head1 BUGS

None reported... yet.

=cut
