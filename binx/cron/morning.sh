#!/usr/bin/env zsh

#!/bin/zsh
##
## alarm.sh
## Login : <s@taj>
## Started on  Mon Nov 15 23:56:45 2010 Sharad Pratap
## $Id$
##
## Copyright (C) 2010 Sharad Pratap
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



# defaults
sleep_hours=7
snooze=10
queue_name=d
volume_high=100 # 60
volume_low=70   # 45

max=

DISPLAY=:0


time=1800

# cli arg processing
set -- $(getopt t: "$@")
while [ $# -gt 0 ]
do
    case "$1" in
        (-t) time="$2"; shift;;
        (--) shift; break;;
        (-*) echo "$0: error - unrecognized option $1" 1>&2; exit 1;;
        (*)  break;;
    esac
    shift
done



xset dpms force on
xset dpms 60 80 $time
mpc play
mpc volume 100
spd-say -C
