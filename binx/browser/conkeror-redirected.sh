#!/usr/bin/env bash
#

if which conkeror > /dev/null 2>&1
then
    BIN=conkeror
elif [ -e /usr/local/bin/conkeror ]
then
    BIN=/usr/local/bin/conkeror
fi

if [ "x" != "x$BIN" ]
then
    exec $BIN "$@" >/dev/null 2>&1
else
    echo Not able to find conkeror >/dev/null 2>&1
fi

