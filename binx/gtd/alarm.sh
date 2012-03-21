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


# cli arg processing
set -- $(getopt nh:z:q: "$@")
while [ $# -gt 0 ]
do
    case "$1" in
        (-h) sleep_hours="$2"; shift;;
        (-z) snooze="$2"; shift;;
        (-q) queue_name="$2"; shift;;
        (-n) now=1;;
        (--) shift; break;;
        (-*) echo "$0: error - unrecognized option $1" 1>&2; exit 1;;
        (*)  break;;
    esac
    shift
done


# now
hour=$(date +%H)


# check 'xset q'

if [ -t 0 ] || (( $hour > 20 || $hour < 6 )) && ! pgrep xtrlock; then

# cancel all jobs in queue d
    jobs=($(atq -q $queue_name | cut -d'	' -f1 ))

    if (($#jobs)) ; then
        atrm $jobs
    fi
    command="at -q $queue_name now + ${${:-$(( $sleep_hours * 60 ))}//./} minutes"
    at -q $queue_name now + ${${:-$(( $sleep_hours * 60 ))}//./} minutes <<EOF

pacmd <<'ZZZ'
set-card-profile 0 output:analog-stereo
set-card-profile 0 output:iec958-stereo+input:analog-stereo
set-card-profile 0 output:analog-stereo
ZZZ

      xset dpms force on
      xset dpms 60 80 0
      amixer -- sset  Master   100% unmute
      mpc clear
      mpc load myfavorit
      mpc play
      sleep 2
      mpc volume $volume_high
EOF

    ### after 6 m 25 sec low down volume
    at -q $queue_name now + ${${:-$(( $sleep_hours * 60 + $snooze ))}//./} minutes <<EOF
      xset dpms force on
      xset dpms 60 80 0
      mpc play
      sleep 2
      mpc volume $volume_low
      xset dpms 60 80 1800
EOF

fi

