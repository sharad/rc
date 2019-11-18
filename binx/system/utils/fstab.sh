#!/usr/bin/env bash

# from: http://unix.stackexchange.com/a/96150
# from: http://unix.stackexchange.com/questions/96037/how-to-align-fstab-entries-easily

function push() {
    buffer="$buffer"$'\n'"$1"
}

function pop() {
    if [ "$buffer" != "" ]
    then
        echo "$buffer" | column -t
        buffer=""
    fi
}

buffer=""

while read line
do
    if [ "$line" == "" -o "${line:0:1}" == "#" ]
    then
        pop
        echo "$line"
    else
        push "$line"
    fi
done < "$1"

pop
