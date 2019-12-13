#!/usr/bin/env bash

# https://kkatsuyuki.github.io/notmuch-conf/

notmuch new

if [ -r ~/.config/offlineimap/notmuch_tag ]
then
    notmuch tag --batch --input=~/.config/offlineimap/notmuch_tag
fi

notifymuch
