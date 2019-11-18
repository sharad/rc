#!/usr/bin/env zsh


template=$1
file=$2


if [ ! -e $1 ] ; then

    if [  -t 1 ] ; then
        if read -q \?" $file is missing do you want to create it? (y/n): " ; then
            cp $template $file
            general-editor $file
        fi
    else
        cat <<EOF>> ~/.var/run-later
            cp $template $file
            general-editor $file
EOF
    fi

fi

