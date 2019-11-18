#!/usr/bin/env bash




function main () {
        mkset
}


testscript=testx.sh
collectdir=/tmp
workdir=.

function mkset () {

    if [ "x$1" = "x" ] ; then
        read -p "setname: " set
    fi

    tracefile=$collectdir/$set
    if [ -r  $tracefile ] ; then
        confirmnotify one $tracefile file exits.
        cat $tracefile
        msg="Do you want to use existing $tracefile file."
        match="n"
    else
        msg="asnwer yes."
        confirm=true
    fi
    while ynask "$msg "  ; do
        mksetinternal
        confirmnotify "below are the commands you used for set $set "
        cat $tracefile
        msg="do you want to redo it"
    done

    cat <<EOF >> $testscript
        function $set () {
           $(cat $tracefile | awk '{ print "run " $R ";" }' )
        }

EOF



}

function mksetinternal () {
    confirmnotify please run all  command to prepare new set $set.

    if cd $workdir ; then
        bash -i <<EOF
trap 'echo \$(history 1) >> $tracefile'  DEBUG
exec < /dev/tty
EOF
    else
        error Not able to change to $workdir >&2
    fi

}


# i/o utils
function error () {
    notify "$@" >&2
    exit -1;
}

function ynask () {
    xmatch=$match
    unset match
    if [  "x$confirm" == "x" ] ; then
        echo value of match $xmatch.
        read -p "${1} (y/n)? "
        echo REPLY $REPLY
        echo match $xmatch
        [ "$REPLY" == ${xmatch:-y} ]
    else
        $confirm
    fi
}

function confirmnotify () {
    notify "$@"
    read -p "press enter: " x
}

function notify () {
    echo "$@"
}


main;

exit 0;

