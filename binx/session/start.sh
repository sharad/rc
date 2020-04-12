#!/usr/bin/env bash


if ! pgrep gnome-key
then
    echo gnome-key not running
    gnome-keyring-daemon
fi
if [ -e /run/user/1000/keyring/ssh ]
then
   export SSH_AUTH_SOCK=/run/user/1000/keyring/ssh
fi

DISPNO=$(xrandr | grep -v disconnected | grep connected | wc -l)

if [ "$DISPNO" -gt 1 ]
then
    echo 'URxvt.font: xft:DejaVu Sans Mono:style=Book:size=7:antialias=true' |  xrdb -merge -  >&/dev/null
fi

ecryptfs-mount-private
mount.ecryptfs_private
df
update-ssh-agent force

if [ "x" != "x$DISPLAY" ]
then
    secret-tool lookup server exch-cas.fortinet.com user 'fortinet-us\spratap' protocol imap  | xclip -i
fi


