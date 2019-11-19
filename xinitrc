#!/usr/bin/env zsh
# .xinitrc - OpenWindows startup script.
#
# last modified:

if [ -e /usr/local/bin/x-session-stumpwm ]
then
    XINITRC_DEFAULT_WM=/usr/local/bin/x-session-stumpwm
else
    XINITRC_DEFAULT_WM=stumpwm
fi

WM=${1:-$XINITRC_DEFAULT_WM}

# ~/bin/xhook

# ## Start the window manager and give it time to initialize.
# ##
# sleep 15 &  pid=$!
# olvwm -syncpid $pid &
# #/opt/kde/bin/kde
# wait $pid			# pause until olvwm inits

# . $HOME/.openwin-init		# start up all the xterms and standard apps.
# exec xterm -name leftXTerm -C	# put the console in the foreground

# from http://blog.entek.org.uk/?p=13
# xbindkeys
# tpb -d
# xv -quit -root -rmode 5 -max ~/36275-emerge.jpg
# #xpenguins & # This and conky do _NOT_ get along as both like the root window (xpenguins destroys parts of conky until it refreshes)
# conky -d
# evilwm -snap 10 &
# xautolock

source ~/.rsetup/sh/env
source ~/.rsetup/x/env
source ~/.xprofile


if which $WM ; then
    exec $WM
    if [ $WM  -eq "gnome" ] ; then
        exec gnome-session
    fi
elif which gnome-session ; then
    exec gnome-session
fi

