#!/bin/sh

if [ "$(xrandr | grep DP-2 | cut -f2 -d' ')" = "connected" ]
then
    xrandr \
        --output eDP-1 --mode 1920x1080 --pos 0x0 --rotate normal \
        --output HDMI-3 --off \
        --output HDMI-2 --off \
        --output HDMI-1 --off \
        --output DP-2 --primary --mode 1366x768 --pos 1920x0 --rotate normal \
        --output DP-1 --off
fi
