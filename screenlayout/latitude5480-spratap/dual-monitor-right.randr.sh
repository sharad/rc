#!/bin/sh

if [ "$(xrandr | grep VGA-1 | cut -f2 -d' ')" = "connected" ]
then
    xrandr \
        --output VGA-1 --primary --mode 1366x768 --pos 0x0 --rotate normal \
        --output LVDS-1 --mode 1600x900 --pos 1366x0 --rotate normal \
        --output HDMI-3 --off \
        --output HDMI-2 --off \
        --output HDMI-1 --off \
        --output DP-3 --off   \
        --output DP-2 --off   \
        --output DP-1 --off
fi
