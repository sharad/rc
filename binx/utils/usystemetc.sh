#!/usr/bin/env zsh
##
## usystemetc.sh
## Login : <s@taj>
## Started on  Tue May 18 21:27:31 2010 Sharad Pratap
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

DISTRO=Ubuntu                   # calculate value of vendor
SYSDIR=~/.system/$DISTRO

cd $SYSDIR
foreach f (${(f)"$(find . \! -path '*.svn*' -type f)"}) {
    if [ -r /etc/$f ] && ! diff -q /etc/$f $f ; then
        cp /etc/$f $f
    fi
}
cd -



