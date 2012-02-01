#!/bin/env bash
### pg.sh --- descriptio about about pg.sh

## Copyright 2012 Sharad Pratap
##
## Author: sharad at sdgfdfgh
## Version: $Id: pg.sh,v 0.0 2012/02/01 11:45:34 spratap Exp $
## Keywords:
## X-URL: not distributed yet

## Time-stamp: <>

## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

### Commentary:

##

##   (require 'pg)

### Code:

function comment {

    foreach art ($(tidy -iqn -asxml  articles.html 2>/dev/null  | xml sel -N x=http://www.w3.org/1999/xhtml  -t -m  'x:html/x:body/x:table/x:tr/x:td/x:table/x:tr/x:td/x:font/x:a/@href' -v . -n | grep '\.html')) {
        { echo '<div class="article" name="'$(basename $art .html)'">';  tidy -iqn -asxml  $art  2>/dev/null  | xml sel -N x=http://www.w3.org/1999/xhtml -t -m  /x:html/x:body/x:table/x:tr/x:td/x:table/x:tr/x:td -c . ; echo '</div>' } >> pg-name.xml
    }


foreach art ($(tidy -iqn -asxml  articles.html 2>/dev/null  | xml sel -N x=http://www.w3.org/1999/xhtml  -t -m  'x:html/x:body/x:table/x:tr/x:td/x:table/x:tr/x:td/x:font/x:a/@href' -v . -n | grep '\.html')) {
{ echo '<div class="article" name="'$(basename $art .html)'">';  tidy -iqn -asxml  $art  2>/dev/null  | xml sel -N x=http://www.w3.org/1999/xhtml -t -m  /x:html/x:body/x:table/x:tr/x:td/x:table/x:tr/x:td -c . ; echo '</div>' } >> pg-name.xml
echo $art
echo
}


foreach art ($(tidy -iqn -asxml  articles.html 2>/dev/null  | xml sel -N x=http://www.w3.org/1999/xhtml  -t -m  'x:html/x:body/x:table/x:tr/x:td/x:table/x:tr/x:td/x:font/x:a/@href' -v . -n | grep '\.html')) {
tidy -iqn -asxml  $art  2>/dev/null > $(basename $art .html ).xml
echo $art
echo

}

foreach art ($(tidy -iqn -asxml  articles.html 2>/dev/null  | xml sel -N x=http://www.w3.org/1999/xhtml  -t -m  'x:html/x:body/x:table/x:tr/x:td/x:table/x:tr/x:td/x:font/x:a/@href' -v . -n | grep '\.html')) {
{ echo '<div class="article" name="'$(basename $art .html)'">';  tidy -iqn -asxml  $art  2>/dev/null  | xml sel -N x=http://www.w3.org/1999/xhtml -t -m  '/x:html/x:body/x:table/x:tr/x:td/x:table[1]/x:tr[1]/x:td[1]/*'  -c . ; echo '</div>' } >> pg-name-final.xml
echo $art
echo
}

foreach art ($(tidy -iqn -asxml  articles.html 2>/dev/null  | xml sel -N x=http://www.w3.org/1999/xhtml  -t -m  'x:html/x:body/x:table/x:tr/x:td/x:table/x:tr/x:td/x:font/x:a/@href' -v . -n | grep '\.html')) {
{ echo '<div class="article" name="'$(basename $art .html)'">';  tidy -iqn -asxml  $art  2>/dev/null  | xml sel -N x=http://www.w3.org/1999/xhtml -t -m  '/x:html/x:body/x:table/x:tr/x:td/x:table[1]/x:tr[1]/x:td[1]/*'  -c . ; echo '</div>' } >> pg-name-final.xml
echo $art
echo
}



}

  function main {
      set_check "$@"

      if wget 'http://www.paulgraham.com/articles.html' ; then
          foreach art ($(tidy -iqn -asxml  articles.html 2>/dev/null |
                         xml sel -N x=http://www.w3.org/1999/xhtml  -t -m  'x:html/x:body/x:table/x:tr/x:td/x:table/x:tr/x:td/x:font/x:a/@href' -v . -n |
                         grep '\.html')) {
              wget   'http://www.paulgraham.com/'$art
              {
                  echo '<div class="article">'
                  tidy -iqn -asxml  $art  2>/dev/null  |
                  xml sel -N x=http://www.w3.org/1999/xhtml -t -m  /x:html/x:body/x:table/x:tr/x:td/x:table/x:tr/x:td -c .
                  echo '</div>'
              } >> pg.xml
          }
      fi

      cleanup
  } # end main



##############################################################################
####  User Options, Variables
##############################################################################

  function set_check {

      myself=`basename $0`

      opt=$1
      while shift ; do
          case $opt in
              -x)     x-opt-arg=$1		; shift ;;
              -debug) debug=yes		;;
              -l)     log=$1		; shift ;;
              -b)     b-opt=$1		;;
              -h)
                  helpmssg                                               >&2
                  exit 0 ;;
              *)
                  echo unrecognized: $opt option.                        >&2
                  echo -e try ${0} -h				       >&2
                  echo See you again....                                 >&2
                  exit -1 ;;
          esac
          opt=$1
      done

      if   [ ! "$x-opt-arg"     ] ; then
          echo -e $0: specify x-opt-arg with -a x-opt-arg. "\n"exiting ...          >&2
          exit -1
      fi
      chmod u+w $log

  } # end set_check

  function cleanup {
  # make it not writable.
      chmod -w $log
  } # end cleanup

  function helpmssg {

      cat <<EOF
	$myself: -a approver_name  -l logfile -a x-opt-arg

	-x x-opt-arg,		specify x-opt-arg.
	-b,		        enable b option.
        -debug,                 option for debug.
	-h,                     this help.
	-l log_file,		optional, can specify log_file, either it will use
					~spratap/log/pg file for it.
	long long story............
	..............................
EOF
  } # end helpmssg




  main "$@"

### pg.sh ends here

