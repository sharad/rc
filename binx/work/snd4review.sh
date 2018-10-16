#!/bin/zsh
##
## snd4review.sh
## Login : <sh4r4d _at_ _G-mail_>
## Started on  Fri Apr  8 17:07:50 2011 Sharad Pratap
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


clientdir=~/bugfixes/dominus/
tardir=~/bugfixes/tardir/

set -- $(getopt b: "$@")
while [ $# -gt 0 ]
do
    case "$1" in
        (-b) bug="$2"; shift;;
        (--) shift; break;;
        (-*) echo "$0: error - unrecognized option $1" 1>&2; exit 1;;
        (*)  break;;
    esac
    shift
done

if [ ! $bug ] ; then
   print bug number
   exit -2
fi


cd $clientdir
select f in ${(f)"$(p4 opened ...)"} ; do
    filelst+=($f)
done

echo cp $filelst $tardir

cd $tardir
rm -f bug${bug}.tar.gz
tar czf bug${bug}.tar.gz $filelst
rm -f bug${bug}.rar
rar a bug${bug}.rar $filelst
