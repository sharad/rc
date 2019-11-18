#!/usr/bin/env bash
# http://usefull-dev-stuff.blogspot.in/2012/10/basic-stumpwm-startup-file-for-gnome.html

toggle=$( (pactl list sinks | grep -q Mute:.no & echo 1) || echo 0)
pactl set-sink-mute 0 $toggle

