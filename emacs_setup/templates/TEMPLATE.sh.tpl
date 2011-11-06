#!/bin/env bash
### TEMPLATE.sh.tpl --- descriptio about about (>>>FILE<<<)

## Copyright (>>>YEAR<<<) (>>>USER_NAME<<<)
##
## Author: (>>>AUTHOR<<<)
## Version: $Id: (>>>FILE<<<),v 0.0 (>>>VC_DATE<<<) (>>>LOGIN_NAME<<<) Exp $
## Keywords: (>>>1<<<)
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

## (>>>2<<<)

##   (require '(>>>FILE_SANS<<<))

### Code:

  function main {
      set_check "$@"
      (>>>POINT<<<)
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
					~(>>>LOGIN_NAME<<<)/log/(>>>FILE_SANS<<<) file for it.
	long long story............
	..............................
EOF
  } # end helpmssg


(>>>3<<<)

  main "$@"

### (>>>FILE<<<) ends here

