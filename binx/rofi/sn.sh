#!/usr/bin/env bash

# how much you output, that much it will ask

if [ "$@" ]
then
    # screen -ls
    echo Test "$@"
else
    echo Hello
    echo Hi
fi


