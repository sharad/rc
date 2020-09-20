#!/usr/bin/env bash

# https://kkatsuyuki.github.io/notmuch-conf/

if false
then
    if ! pgrep -u $USER notmuch
    then
        notmuch new

        if [ -r "$HOME/.config/offlineimap/notmuch_tag" ]
        then
            notmuch tag --batch --input="$HOME/.config/offlineimap/notmuch_tag"
        fi
    fi
fi

notifymuch

