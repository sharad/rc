#!/usr/bin/env perl
# -*- Mode: cperl; indent-tabs-mode: nil -*-
# bu.pl ---
# Copyright 2011 Sharad Pratap
#
# Author: sh4r4d _at_ _G-mail_
# Author: sh4r4d _at_ _G-mail_
# Version: $Id: bu.pl,v 0.0 2011/03/30 09:31:36 spratap Exp $
# Keywords:
# X-URL: not distributed yet
#
# Time-stamp: <>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

## Commentary:

use strict;
use warnings;

use Perforce;
use Getopt::Long;
use Pod::Usage;
use File::Basename qw(dirname);
use File::Spec;
use HTTP::Cookies;
use XMLRPC::Lite;
use Spreadsheet::WriteExcel;
use Date::Format;
use lib $ENV{'HOME'} . ".setup/osetup/info/common/perl"
use Setup;


use lib $Setup::info->{'BugzillaDir'}
use lib qw(.);



use Getopt::Std;
use Bugzilla::WebService;


##   (require 'bu)
# Code:
my $all_global_variables = 0;       # every thing is variable.
my $verbose;

my $help;
my $order="bug,bugdescription,changelist,comment,date,bugversion";
my $changes = "182451,188630";


GetOptions(
    'order|o'        => \$order,
    'changes'        => \$changes,
    'help|h|?'       => \$help,
) or pod2usage({'-verbose' => 0, '-exitval' => 1});




&main;
exit 0;

sub main {
    my $vars;
    my $opts = &process_cli;
    my $cookie_jar =
        new HTTP::Cookies('file' => File::Spec->catdir(dirname($0), 'cookies.txt'),
                          'autosave' => 1);
    my $proxy = XMLRPC::Lite->proxy($Setup::info->{"bugzurl"} . "/xmlrpc.cgi",
                                    'cookie_jar' => $cookie_jar);
    my $soapresult = $proxy->call('Bugzilla.version');
    _die_on_fault($soapresult);
    print 'Connecting to a Bugzilla of version ' . $soapresult->result()->{version} . ".\n";

    my $p4;

    my $workbook = Spreadsheet::WriteExcel->new("test.xls");
    my $worksheet = $workbook->add_worksheet();
    my $date_format = $workbook->add_format(num_format => 'dd/mm/yyyy');

    my @columns = split /,/, $order;

    my %colmap;

    @colmap{@columns} = 0 .. @columns;

    eval {
        $p4 = new Perforce(client => "p4bkup-test");
        $p4->SetTicketFile($ENV{'HOME'} . "/.p4tickets");
        # {{{ these two only you have use for proxy server.
        $p4->SetPort($Setup::info->{"p4port"});
        $p4->SetUser($Setup::info->{"p4user"});
        # $p4->SetPassword('');
        # }}}

        # my ($x,$y) = (0,0);
        my @out;
        push @out, \@columns;

        foreach my $f ( @{$p4->Run("changes", "-lu", "spratap")}, @{$p4->Run("describe", "-s", (split /,/, $changes))}) {
          # bug,bugdescription,changelist,comment,date
          my %outhash;
          $outhash{'changelist'} = $f->{'change'};
          # $outhash{'date'}   = ((70 * ) + $f->{'time'} / (60 * 60 * 24));
          my @datet = localtime($f->{'time'});
          $outhash{'date'} = strftime("%c", @datet);
          $outhash{'comment'}   = $f->{'desc'};
          my ($bugnumber) = $f->{'desc'} =~ m/\s*(\d+)\s+\-/;
          $outhash{'bug'} = $bugnumber;
          my $bug = xmlrpc($proxy, $bugnumber, "sdfsdgsdg");
          $outhash{'bugdescription'} = $bug->{'summary'};
          $outhash{'bugversion'} = $bug->{'internals'}->{'target_milestone'};
          # push @out, \{@outhash{"change","time"}};
          my @outarray = @outhash{@columns};
          my $outhashref = \@outarray;
          push @out, $outhashref;
          print @outhash{@columns};
          }

        $worksheet->set_column($colmap{'bugdescription'}, $colmap{'bugdescription'},  70);
        $worksheet->set_column($colmap{'comment'}, $colmap{'comment'},  40);
        $worksheet->set_column($colmap{'date'}, $colmap{'date'},  20, $date_format);
        $worksheet->write_col('A1', \@out);
        $workbook->close();
    };
    if ($@) {
        print "Error: ", $@;
    } elsif ($p4->{'error'}) {
        print "Error: ", $p4->{'error'};
    }

} # end main


sub xmlrpc {
    my ($proxy, $bugnum, $fields) = @_;

    if ($bugnum) {
        my $soapresult = $proxy->call('Bug.get', { ids => [$bugnum] });
        _die_on_fault($soapresult);
        my $result = $soapresult->result;
        my $bug = $result->{bugs}->[0];
        # return $result->{bugs}->[0];

        # foreach my $field (keys(%$bug)) {
        #     my $value = $bug->{$field};
        #     if (ref($value) eq 'HASH') {
        #         foreach (keys %$value) {
        #             print "$field - $_: " . $value->{$_} . "\n";
        #         }
        #     }
        #     else {
        #         print "$field: $value\n";
        #     }
        # }

        return $bug;

      }

} # end of xmlrpc



############################################################################
##  User Options, Variables
############################################################################

sub process_cli {

  my %opts;             # p4 user data base.

  # get global datum.
  my $synerr;                        # process cli args.
  getopts('a:b:b:def', \%opts);    # assuming all takes arguments.


   if ($synerr) {
     &main::HELP_MESSAGE ();
     die;
   }

  $verbose = $opts{"v"} if $opts{"v"};
  my %missing = ();


  return \%opts;
} # end process_cli

sub main::HELP_MESSAGE() {
  if (@_) {
    print @_, "\n";
  }
  print <<EOF;
 -a   for A
 -b   for B
 -c   for C
 -d   only for himself
 -v   verbose.
 -e   only for himself
 -f   only for himself
EOF
} # end main::HELP_MESSAGE

sub _die_on_fault {
    my $soapresult = shift;

    if ($soapresult->fault) {
        my ($package, $filename, $line) = caller;
        die $soapresult->faultcode . ' ' . $soapresult->faultstring .
            " in SOAP call near $filename line $line.\n";
    }
}

sub _syntaxhelp {
    my $msg = shift;

    print "Error: $msg\n";
    pod2usage({'-verbose' => 0, '-exitval' => 1});
}



__END__
