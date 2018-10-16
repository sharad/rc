#!/bin/zsh
##
## repairoffimap.sh
## Login : <sh4r4d _at_ _G-mail_>
## Login : <sh4r4d _at_ _G-mail_>
## Started on  Fri Jun  3 12:16:33 2011 Sharad Pratap
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




function main() {
    process_arg "$@"
    if [ ! -e ~/.offlineimaprc ] ; then
        verbose no ~/.offlineimaprc do not exists.
        exit -1
    else

        directory=($maildir/.${account}.lists.${box//\//.}
              ~/.offlineimap/Account-${account}/LocalStatus/${account}.lists.${box//\//.}
              ~/.offlineimap/Repository-${remoterepo}/FolderValidity/INBOX.${box//\//.} )

        foreach dir ( $directory ) {
            if eval [  ! -e $dir ] ; then
                verbose $dir do not exists can not proceed.
                exit -1
            fi
        }

        run pkill offlineimap
        run sudo service dovecot stop
        foreach dir ( $directory ) {
            verbose removing $dir
            run rm -r $dir
        }
        run sudo service dovecot start
    fi
}

function process_arg() {
    set -- $(getopt -n $(basename $0) -o inva:r:b: -- "$@")

    maildir=~/.maildir

    while [ $# -gt 0 ]
    do
        case "$1" in
            (-b) eval box=$2; shift;;
            (-a) eval account=$2; shift;;
            (-r) eval remoterepo=$2; shift;;
            (-i) interactive=1;;
            (-v) verbose=1;;
            (-n) norun=1;;
            (--) shift; break;;
            (-*) echo "$0: error - unrecognized option $1" 1>&2; exit 1;;
            (*)  break;;
        esac
        shift
    done
}

function verbose() {
    [ $verbose ] && print ${pgm}: $@
}

function run() {
    if [ $norun ] ; then
        print $@
    else
        $@
    fi
}

pgm=$(basename $0)

main "$@"
