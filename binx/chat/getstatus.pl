#!/usr/bin/perl

##
## getsttus.pl
## Login : <sh4r4d _at_ _G-mail_>
## Started on  Thu May  5 13:21:15 2011 Sharad Pratap
## $Id$
##
## Copyright (C) 2011 Sharad Pratap
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
##


use strict;
use warnings;
use 5.010;

# run the following before launching the script or pidgin:
# export LD_PRELOAD='/opt/pidgin/lib/libpurple.so
/opt/pidgin/lib/purple-2/perl.so'

use lib '/opt/pidgin/lib/purple-2/perl';

use Purple;

our %PLUGIN_INFO = (
   perl_api_version => 2,
   name => "Perl Test Plugin",
   version => "0.1",
   summary => "Test plugin for the Perl interpreter.",
   description => "Your description here",
   author => "John H. Kelm <johnhkelm\@gmail.com",
   url => "http://pidgin.im",
   load => "plugin_load",
#    unload => "plugin_unload"
#    plugin_action_sub => 'plugin_action_cb',
);

sub plugin_init {
   my $plugin = shift;
   say "plugin_init";
   return %PLUGIN_INFO;
}

sub timeout_cb {
   my $plugin = shift;
   say "timeout $plugin " . time;
   send_message();

   return 1;
}

sub send_message {

   my $account_name = 'szabgab.other at gmail.com/Home';
   my $protocol = 'prpl-jabber';
   my $acc = Purple::Accounts::find($account_name, $protocol);

   return if not $acc->is_connected;

   my $buddy = Purple::Find::buddy($acc, "szabgab");
   my $pres = $buddy->get_presence;
   my $online = $pres->is_online;
   say $online ? "online" : "offline";

   return 1;
}

sub plugin_load {
   my $plugin = shift;
   say "plugin_load";
   Purple::timeout_add($plugin, 2, \&timeout_cb, $plugin);

   return;
}
