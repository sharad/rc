#!/usr/bin/env zsh
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
lockpgm=${XLOCKER:-slock} # xtrlock
max=

timestep=1s

function main() {
        process_arg $@

        idlebegin=$(xprintidle)

        while [ $idlebegin -lt $(xprintidle) ]
        do
        done

        screen_lock
        true
}

function get_screens() {
    $(xrandr --listactivemonitors | sed 1d | tr -s ' \t' | cut -d' ' -f5)

}

function get_brightness() {

}

function state_locked () {
    pgrep pidgin >&/dev/null && purple-remote 'setstatus?status=$lockstatus&message=Away' &!
    whence transmission-remote >& /dev/null && transmission-remote -N ~/.netrc -as &!
    if [ ! -e $wmlockfile_disable ] ; then
        whence stumpish >&/dev/null && timeout 2 stumpish fclear
    fi
    wscreenlockon
}

function state_unlocked () {
    whence transmission-remote >& /dev/null && transmission-remote -N ~/.netrc -AS &!
    pgrep pidgin >&/dev/null && purple-remote 'setstatus?status=$unlockstatus&message=' &!
    if [ ! -e $wmlockfile_disable ] ; then
        whence stumpish >&/dev/null && timeout 2 stumpish pull-hidden-other &!
    fi
    wscreenlockoff
    if whence -p ~/bin/xintrusion >/dev/null 2>&1
    then
        ~/bin/xintrusion
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
        ${lockpgm}
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

### action pref hooks ###
function wscreenlockon () {
    if [ -r ~/.rsetup/wscreenlockon/env ] ; then
        source ~/.rsetup/wscreenlockon/env
    fi
    if [ -x ~/.rsetup/wscreenlockon/run ] ; then
        ~/.rsetup/wscreenlockon/run
    fi
}
function wscreenlockoff () {
    if [ -r ~/.rsetup/wscreenlockoff/env ] ; then
        source ~/.rsetup/wscreenlockoff/env
    fi
    if [ -x ~/.rsetup/wscreenlockoff/run ] ; then
        ~/.rsetup/wscreenlockoff/run
    fi
}
### action pref hooks ###



pgm=$(basename $0)

main $@
