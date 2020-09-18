#!/usr/bin/env bash

# https://kkatsuyuki.github.io/notmuch-conf/

notmuch new

if [ -r "$HOME/.config/offlineimap/notmuch_tag" ]
then
    : notmuch tag --batch --input="$HOME/.config/offlineimap/notmuch_tag"
fi

notifymuch

