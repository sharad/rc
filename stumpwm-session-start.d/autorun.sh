#!/bin/zsh

[ -x /usr/libexec/at-spi-registryd ] && /usr/libexec/at-spi-registryd &!

# whence -p gnome-screensaver &&
# pgrep gnome-screensaver ||
# gnome-screensaver &!

if [ -x /usr/libexec/gnome-settings-daemon ] ; then
    pgrep gnome-settings-daemon ||
    /usr/libexec/gnome-settings-daemon &!
else
    whence -p gnome-settings-daemon &&
    pgrep gnome-settings-daemon ||
    gnome-settings-daemon &!
fi

pgrep nm-applet ||
DISPLAY=:0.0 dbus-launch --autolaunch=$MY_DBUS_SESSION nm-applet --sm-disable &!

# pgrep empathy ||
# DISPLAY=:0.0 dbus-launch --autolaunch=$MY_DBUS_SESSION empathy &!

# syndaemon -i 1 -d

synclient TouchpadOff=1


foreach prog (
    ## it is automatically started by gdm, so no need to start it.
    # gnome-keyring-daemon
    gnome-power-manager
    mail-notification
    blueman-adapters
    blueman-applet
    bluetoothd
    pidgin
    synergys
    ) {
        whence -p $prog &&
        pgrep $prog[0,15] ||
        $prog &!
    }


