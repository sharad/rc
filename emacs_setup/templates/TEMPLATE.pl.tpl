#!/bin/env perl
# -*- Mode: cperl; indent-tabs-mode: nil -*-
# TEMPLATE.pl.tpl ---
# Copyright (>>>YEAR<<<) (>>>USER_NAME<<<)
#
# Author: (>>>AUTHOR<<<)
# Version: $Id: (>>>FILE<<<),v 0.0 (>>>VC_DATE<<<) (>>>LOGIN_NAME<<<) Exp $
# Keywords: (>>>1<<<)
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

use Getopt::Std;

(>>>2<<<)
##   (require '(>>>FILE_SANS<<<))
# Code:
my $all_global_variables = 0;       # every thing is variable.


&main;
exit 0;

sub main {
  my $opts = &process_cli;
  (>>>POINT<<<)
} # end main


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

;;; (>>>FILE<<<) ends here
