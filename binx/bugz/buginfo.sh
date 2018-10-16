#!/bin/zsh
##
## getinfo.sh
## Login : <sh4r4d _at_ _G-mail_>
## Started on  Fri Feb 18 18:38:14 2011 Sharad Pratap
## $Id$

normal=0

# cli arg processing
set -- $(getopt n "$@")
while [ $# -gt 0 ]
do
    case "$1" in
        (-n) normal=1; shift;;
        (--) shift; break;;
        (-*) echo "$0: error - unrecognized option $1" 1>&2; exit 1;;
        (*)  break;;
    esac
    shift
done


    if [ $normal -eq 1 ] ; then
#         cat <<EOF
#       Bug: 44409: UI: Unable to configure vlan and Tunk mode vlan using WebUI under Wired AP Profile.
#       changelist: 182451: removed "enable mld". now we have only mld snooping. corrected the behavior of snooping and igmp
#       2011/01/25 03:52:03"

# EOF
    else
        echo bug,bugdesciption,changelist,comment,date
        # echo "44409,UI: Unable to configure vlan and Tunk mode vlan using WebUI under Wired AP Profile.,182451,removed "enable mld". now we have only mld snooping. corrected the behavior of snooping and igmp,2011/01/25 03:52:03"
    fi


    foreach cl (182451 $(p4 changes -u spratap | cut -d' ' -f2) ) {
    bug=$(p4 describe $cl | sed -n 3p | sed 's/[[:blank:]]*\([[:digit:]]\+\).*/\1/')
    bugTitle=$(bugz get $bug | sed -n 3p | sed 's/Title[[:blank:]]*: \(.\+\)/\1/' | tr -d ,)
    mycomment=$(p4 describe $cl | sed -n 6p  | sed 's/[[:blank:]]*\(.\+\)/\1/' | tr -d ,)
    date=$(p4  describe -s $cl   | sed -n 1p | cut -d' ' -f6,7)
    if [ $normal -eq 1 ] ; then
        cat <<EOF
      Bug: ${bug}: $bugTitle
      changelist: ${cl}: $mycomment
      $date

EOF
    else
        echo $bug,$bugTitle,$cl,$mycomment,$date
    fi
}
