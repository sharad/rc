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
    echo 'URxvt.font: xft:DejaVu Sans Mono:style=Book:size=7:antialias=true' |  xrdb -merge - >/dev/null 2>&1
fi

ecryptfs-mount-private
mount.ecryptfs_private
df
update-ssh-agent force

if [ "x" != "x$DISPLAY" ]
then
    secret-tool lookup server exch-cas.fortinet.com user 'fortinet-us\spratap' protocol imap >/dev/null 2>&1
fi

unalias ssh
unalias scp
unalias noninteractive-ssh
unalias noninteractive-scp

if [ "${SSH_AUTH_SOCK}x" = "x" ]; then
    # if we dont have an auth sock, dont use pub key identification
    alias ssh='command ssh -qX -o PubkeyAuthentication=no'
    alias scp='command scp -3 -q  -o PubkeyAuthentication=no'
    alias noninteractive-ssh='command ssh -qX -o PubkeyAuthentication=no -o VisualHostKey=yes'
    alias noninteractive-scp='command scp -q  -o PubkeyAuthentication=no -o VisualHostKey=yes'
else
    # We do have an auth sock, use auth forwarding
    alias ssh='command ssh -qXA'
    alias scp='command scp -3 -q'
    alias noninteractive-ssh='command ssh -qXA -o VisualHostKey=yes'
    alias noninteractive-scp='command scp -q -o VisualHostKey=yes'
fi

echo 'URxvt.font: xft:DejaVu Sans Mono:style=Book:size=9:antialias=true' |  xrdb -merge - >/dev/null 2>&1
