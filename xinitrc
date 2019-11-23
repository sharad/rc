#!/usr/bin/env zsh
# .xinitrc - OpenWindows startup script.
#
# last modified:


XINITRC_DEFAULT_WM_SESSION="twm"
XINITRC_DEFAULT_WM_SESSION_CMD="twm"


if [ "x" != "x$DESKTOP_SESSION" ]
then
    XINITRC_WM_SESSION="$DESKTOP_SESSION"
elif [ "x" != "x$XDG_SESSION_DESKTOP" ]
then
    XINITRC_WM_SESSION="$XDG_SESSION_DESKTOP"
elif [ "x" != "x$GDMSESSION" ]
then
    XINITRC_WM_SESSION="$GDMSESSION"
else
    XINITRC_WM_SESSION="$XINITRC_DEFAULT_WM_SESSION"
fi

XINITRC_WM_SESSION=${1:-$XINITRC_DEFAULT_WM_SESSION}

# ~/bin/xhook

# ## Start the window manager and give it time to initialize.
# ##
# sleep 15 &  pid=$!
# olvXINITRC_WM_SESSION -syncpid $pid &
# #/opt/kde/bin/kde
# wait $pid			# pause until olvXINITRC_WM_SESSION inits

# . $HOME/.openwin-init		# start up all the xterms and standard apps.
# exec xterm -name leftXTerm -C	# put the console in the foreground

# from http://blog.entek.org.uk/?p=13
# xbindkeys
# tpb -d
# xv -quit -root -rmode 5 -max ~/36275-emerge.jpg
# #xpenguins & # This and conky do _NOT_ get along as both like the root window (xpenguins destroys parts of conky until it refreshes)
# conky -d
# evilXINITRC_WM_SESSION -snap 10 &
# xautolock

for src in sh x
do
    source ~/.rsetup/$src/env
done
source ~/.xprofile


# env



if which "$XINITRC_WM_SESSION" > /dev/null 2>&1
then
    XINITRC_WM_SESSION_CMD="$XINITRC_WM_SESSION"
else
    case $XINITRC_WM_SESSION in
	(gnome)       XINITRC_WM_SESSION_CMD="gnome-session";;
	(windowmaker) XINITRC_WM_SESSION_CMD="wmaker";;
	(stumwpm)     XINITRC_WM_SESSION_CMD="stumpwm";;
	(twm)         XINITRC_WM_SESSION_CMD="twm";;
	# (*)           XINITRC_WM_SESSION_CMD="$XINITRC_DEFAULT_WM_SESSION_CMD";;
    esac
fi


if [ "x" = "x$XINITRC_WM_SESSION_CMD" ]
then
  if [ -e /usr/local/bin/x-session-stumpwm ]
  then
      XINITRC_WM_SESSION_CMD="/usr/local/bin/x-session-stumpwm"
  elif which stumpwm > /dev/null 2>&1
  then
      XINITRC_WM_SESSION_CMD="stumpwm"
  elif which wmaker > /dev/null 2>&1
  then
    XINITRC_WM_SESSION_CMD="wmaker"
  elif which gnome-session > /dev/null 2>&1
  then
    XINITRC_WM_SESSION_CMD="gnome-session" > /dev/null 2>&1
  else
    XINITRC_WM_SESSION_CMD="$XINITRC_DEFAULT_WM_SESSION_CMD"
  fi
fi



# echo $XINITRC_WM_SESSION_CMD
# exec $XINITRC_WM_SESSION_CMD
exec $XINITRC_WM_SESSION_CMD

exit 
