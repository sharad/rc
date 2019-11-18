#!/usr/bin/env zsh

## checkout http://www.ldc.usb.ve/docs/svn/svn.advanced.externaldifftools.html#svn.advanced.externaldifftools.diff.ex-1

# echo "$@"

# set -- $(getopt uL "$@")
# while [ $# -gt 0 ]
# do
#     case "$1" in
#         (-u) context="3";;
#         (-L) ;;
#         (--) shift; break;;
#         (-*) echo "$0: error - unrecognized option $1" 1>&2; exit 1;;
#         (*)  break;;
#     esac
#     shift
# done

LEFT="$6"
RIGHT="$7"

dwdiff -c -C3 $LEFT $RIGHT




