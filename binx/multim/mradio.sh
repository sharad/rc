#!/usr/bin/env zsh
##
## alarm.sh
## Login : <s@taj>
## Started on  Mon Nov 15 23:56:45 2010 Sharad Pratap
## $Id: mradio.sh,v 1.1 2013/06/03 02:10:58 s Exp s $
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

exit

# echo "$@" >&2

# defaults
sleep_hours=7
snooze=10
queue_name=d
volume_high=100 # 60
volume_low=70   # 45
channel=6
max=


function main() {
        process_arg $@

        mplayer_radio $@

        true
}


function mplayer_radio() {

    [ -t ] &&
    grep -A 2 '\[radio/fm\]' ~/.mplayer/config | grep channel | cut -d: -f3 | cut -d= -f2 | tr -- ,- "\n\t"  | cat -n

    if whence mplayer >&/dev/null ; then

#         pacmd <<'ZZZ'
# set-card-profile 0 output:analog-stereo
# set-card-profile 0 output:iec958-stereo+input:analog-stereo
# set-card-profile 0 output:analog-stereo
# ZZZ

        xset dpms force on
        ensureradioup
        # setvolume 100% unmute

        # xset dpms 60 80 0
        # amixer -- sset  Master   100% unmute


        if [ "$channelcmd" ] ; then
            setchannel
        else
            sendradiocmd $@
        fi


        xset dpms force on
        # xset dpms 60 80 0
        # echo "volume ${volume_low}" > /tmp/mplayer.fifo ;
        # xset dpms 60 80 1800
        xset dpms 1800 1800 1800
    fi
}

function setchannel() {
    print changing to channel ${channel}
    echo "radio_set_channel ${channel}" > /tmp/mplayer.fifo
}

function ensureradioup() {
    if ! pgrep mplayer >& /dev/null ; then
            # pkill -9 mplayer
            # sleep 2s
        rm -f /tmp/mplayer.fifo
        if mkfifo /tmp/mplayer.fifo ; then
            mplayer  -profile radio/fm -idle -slave -input file=/tmp/mplayer.fifo >& /dev/null &!
            sleep 2s;
            echo "load radio://6/capture" > /tmp/mplayer.fifo ;
            echo "volume ${volume_high}" > /tmp/mplayer.fifo ;
        fi
    fi
}

function sendradiocmd () {
    echo $@ > /tmp/mplayer.fifo
    echo sending cmd $@ to /tmp/mplayer.fifo
}

function process_arg() {
    # cli arg processing
    # set -- $(getopt "lnh:z:q:p:r:" "$@")
    set -- $(getopt "c:" "$@")
    while [ $# -gt 0 ]
    do
        case "$1" in
            (-c) channel="$2"; channelcmd=1 ; shift;;
            (--) shift; break;;
            (-*) echo "$0: error - unrecognized option $1" 1>&2; exit 1;;
            (*)  break;;
        esac
        shift
    done
    # now
    hour=$(date +%H)
}



pgm=$(basename $0)

main $@

