#!/usr/bin/env zsh

test=1

function run() {
    if (( ${+test} )) ; then
        echo "$@"
    else
        echo "$@"
        $@
    fi
}

if [  $# != 1 -a ! -d ${1} ] ; then
    echo need one argument >&2
    exit 2;
fi

cl=$(p4 opened ${1} | rev | cut -d' ' -f2 | rev)

p4 diff -dwu  ${1} > ${1}.patch

run p4 revert   ${1}

if [ "a${cl}" = "adefault" ] ; then
    run p4 edit ${1}
elif [ "a${cl}" != "a" ] ; then
    run p4 edit -c $cl ${1}
fi

run patch ${1} ${1}.patch


