#!/usr/bin/env zsh
# base theirs mine merged
LOCAL=$3
REMOTE=$2
BASE=$1
MERGED=$4

emacsclient -c -d $DISPLAY -f ~/.emacs.d/server/general \
    --eval "(ediff-merge-files-with-ancestor \"`pwd`/$LOCAL\" \"`pwd`/$REMOTE\" \"`pwd`/$BASE\" nil \"`pwd`/$MERGED\")"
