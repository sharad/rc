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


echo "$@" >&2

# defaults
sleep_hours=7
snooze=10
queue_name=d
volume_high=100 # 60
volume_low=70   # 45
wmlockfile_disable=~/.var/comm/disable/alarmwm
run=radio

max=

function main() {
        process_arg $@

        $run

        screen_lock
        true
}

function mpc() {
    if whence mpc >& /dev/null ; then
        playlist="$( mpc lsplaylists | sed -n $(( $RANDOM % $(mpc lsplaylists | wc -l ) + 1 ))p )"
    fi

    if whence mpc >&/dev/null && [ -t 0 ] || (( $hour > 20 || $hour < 6 )) && ! pgrep xtrlock ; then



        # pgrep mplayer && timeout  -k 4 2 echo "quit 0"  > /tmp/mplayer.fifo; # stop radio

        # cancel all jobs in queue d
        jobs=($(atq -q $queue_name | cut -d'	' -f1 ))

        if (($#jobs)) ; then
            atrm $jobs
        fi
        command="at -q $queue_name now + ${${:-$(( $sleep_hours * 60 ))}//./} minutes"
        # check 'xset q'

        at -q $queue_name now + ${${:-$(( $sleep_hours * 60 ))}//./} minutes <<EOF

pacmd <<'ZZZ'
set-card-profile 0 output:analog-stereo
set-card-profile 0 output:iec958-stereo+input:analog-stereo
set-card-profile 0 output:analog-stereo
ZZZ
      DISPLAY=:0.0
      xset dpms force on
      # xset dpms 60 80 0
      amixer -- sset  Master   100% unmute
      mpc clear
      mpc load $playlist
      mpc play
      sleep 1s
      mpc play
      sleep 1s
      mpc play
      sleep 1s
      mpc play
      sleep 1s
      mpc play
      sleep 1s
      mpc play
      sleep 2
      mpc volume $volume_high
EOF

    ### after 6 m 25 sec low down volume
            at -q $queue_name now + ${${:-$(( $sleep_hours * 60 + $snooze ))}//./} minutes <<'XYEOF'
      xset dpms force on
      # xset dpms 60 80 0
      mpc play
      sleep 1s
      mpc play
      sleep 1s
      mpc play
      sleep 1s
      mpc play
      sleep 1s
      mpc play
      sleep 1s
      mpc play
      sleep 1s
      mpc play
      sleep 2
      mpc volume $volume_low
      # xset dpms 60 80 1800
      xset dpms 1800 1800 1800
XYEOF
    fi

}

function radio() {
    if whence mplayer >&/dev/null && [ -t 0 ] || (( $hour > 20 || $hour < 6 )) && ! pgrep xtrlock ; then

        # pgrep mpd && mpc status |grep playing >& /dev/null && mpc stop

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
      DISPLAY=:0.0
      xset dpms force on
      # xset dpms 60 80 0
      amixer -- sset  Master   100% unmute
      mradio -c 6
      echo "volume ${volume_high}" > /tmp/mplayer.fifo ;
EOF

    ### after 6 m 25 sec low down volume
            at -q $queue_name now + ${${:-$(( $sleep_hours * 60 + $snooze ))}//./} minutes <<'XYEOF'
      xset dpms force on
      # xset dpms 60 80 0
      mradio -c 6
      echo "volume ${volume_low}" > /tmp/mplayer.fifo ;
      # xset dpms 60 80 1800
      xset dpms 1800 1800 1800
XYEOF
    fi

}


function state_locked () {
    pgrep pidgin >&/dev/null && purple-remote 'setstatus?status=$lockstatus&message=Away' &!
    whence transmission-remote >& /dev/null && transmission-remote -N ~/.netrc -as &!
    if [ ! -e $wmlockfile_disable ] ; then
        whence stumpish >&/dev/null && timeout 2 stumpish fclear
    fi

}

function state_unlocked () {
    whence transmission-remote >& /dev/null && transmission-remote -N ~/.netrc -AS &!
    pgrep pidgin >&/dev/null && purple-remote 'setstatus?status=$unlockstatus&message=' &!
    if [ ! -e $wmlockfile_disable ] ; then
        whence stumpish >&/dev/null && timeout 2 stumpish pull-hidden-other &!
    fi
}

function screen_lock() {
    lockstatus=away
    if [ "$HOST" = "lispm.genera.net" ] ; then
        unlockstatus=away
    else
        unlockstatus=online
    fi

    if [ "x$lock" != "x" ] ; then
        state_locked
        xtrlock
        state_unlocked
    fi
}


function process_arg() {
    # cli arg processing
    set -- $(getopt "lnh:z:q:p:r:de" "$@")
    while [ $# -gt 0 ]
    do
        case "$1" in
            (-l) lock=1;;
            (-e) rm -f $wmlockfile_disable;;
            (-d) touch $wmlockfile_disable;;
            (-p) playlist="$2"; shift;;
            (-h) sleep_hours="$2"; shift;;
            (-z) snooze="$2"; shift;;
            (-q) queue_name="$2"; shift;;
            (-r) run="$2"; shift;;
            (-n) now=1;;
            (--) shift; break;;
            (-*) echo "$0: error - unrecognized option $1" 1>&2; exit 1;;
            (*)  break;;
        esac
        shift
    done
    # now
    hour=$(date +%H)

    if ! [ "${run}" = "mpc" -o "${run}" = "radio" ] ; then
        print "please specify -r mpc|radio"
    fi

    if [ "$run" = "mpc" ] ; then
        playlist=myfav
    elif [ "$run" = "radio" ] ; then
        playlist=6
    fi

}



pgm=$(basename $0)

main $@

