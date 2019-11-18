#!/usr/bin/env zsh
##
## addrcf.sh
## Login : <s@taj>
## Started on  Tue May 18 21:43:23 2010 Sharad Pratap
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


function main() {
    targets=("$@")
    setup
    set -- $(getopt a: "$@")
    while [ $#targets -gt 0 ]
    do
        case "$tarets[1]" in
            # (-a) account="$targets[2]"; shift targets;;
            (--) shift; break;;
            (-*) echo "$0: error - unrecognized option $targets[1]" 1>&2; exit 1;;
            (*)  install_in_setup $targets[1] ; break;;
        esac
        shift targets
    done
}

function setup() {
    setupdir=.setup
}

function install_in_setup() {
    source="$1"
    test_source $source
    basename_source=$(basename $source)
    target=${basename_source#.}
    echo mv $source $HOME/$setupdir/$target
    mv $source $HOME/$setupdir/$target
    echo ln -s $setupdir/$target $HOME/$basename_source
    ln -s $setupdir/$target $HOME/$basename_source
    echo ln -s $setupdir/$target $HOME/$setupdir/.hell/$basename_source
    ln -s $setupdir/$target $HOME/$setupdir/.hell/$basename_source
}

function install_in_setup_hell() {
}

function test_source() {
    local source="$1"
    local rsource="$(readlink -f $source)"
    # it should be in home dir, I am making two check
    if [[ $rsource =~ '^'$HOME'/.*' ]] ; then
        :
    else
        echo $source should be in homedir.
        exit -1
    fi

    if [[ $(dirname $rsource) != $HOME ]] ; then
        echo $source should be in homedir.
        exit -1
    fi

    # it should not be a link
    if [ -L $source ] ; then
        local source_link=$(readlink $source) # do not use readlink -f
        if [[ $source_link =~ '^.setup/.*' ]] ; then
            echo $source is Already a link in .setup with value $source_link
            # check for corresponding link is present in ~/.setup/.hell
            exit -1
        else
            echo $source is a link in .setup with value $source_link
            exit -1
        fi
    fi

    local basename_source="$(basename $source)"
    if [[ $basename_source != \.* ]] ; then
        echo $source file name should start with dot.
        exit -1
    fi
}



main "$@"
