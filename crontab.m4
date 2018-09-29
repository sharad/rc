
include(config.m4)

# m h  dom mon dow   command

#variable set
SHELL=/bin/zsh
MAILTO=$MAILTO


# Common
*/ifelse(hostname,lispm,7,3) * * * * DISPLAY=:0 ~/bin/syncimap -a $OFFLINEIMAPACCOUNT > /dev/null 2>&1
*/ifelse(hostname,lispm,2,1) * * * * DISPLAY=:0 ~/bin/monitru > /dev/null 2>&1

ifelse(hostname,lispm,

##{{ Morning
# try waking up at 7.30 to 7.45
# 00 06 * * * ~/bin/alarm -h .2 -p myfav -r mpc
00 06 * * * ~/bin/alarm -h .2 -p 6 -r mpc


# 29 06 * * * mpc load myfav
# 30 06 * * * mpc play
#30 06 * * * ~/bin/mradio -c 6
# 31 06 * * * mpc volume 100
# 45 06 * * * spd-say -C

# 00 07 * * * mpc play
# 00 07 * * * ~/bin/mradio -c 6
# 01 07 * * * mpc volume 100

# # try waking up at 7.30 to 7.45
# 30 07 * * * mpc play
# 30 07 * * * ~/bin/mradio -c 6
30 07 * * * mpc play
# 31 07 * * * mpc volume 100
# 45 07 * * * spd-say -C

# # try waking up at 8.00 to 8.15
# 00 08 * * * mpc play
# 00 08 * * * ~/bin/mradio -c 2
00 08 * * * mpc play
30 08 * * * mpc play

# 15 08 * * * spd-say -C

# # try waking up at 8.30 to 8.45
# 30 08 * * * mpc play
30 08 * * * ~/bin/alarm -h .2 -p 6 -r mpc
# 45 08 * * * spd-say -C
##}}

##{{ Evening
00 18 * * * ~/bin/mradio -c 6
30 18 * * * ~/bin/mradio -c 6
10 23 * * * ~/bin/mradio -c 2
##}}

# run each hour on week days
50 9-22/1 * * 1-5 DISPLAY=:0 dbus-launch --autolaunch=$MY_DBUS_SESSION ~/bin/pwddwnsys

# run 9.50 daily
50 09     * * *   DISPLAY=:0 dbus-launch --autolaunch=$MY_DBUS_SESSION ~/bin/pwddwnsys

# run 10.50 daily
50 22     * * *   DISPLAY=:0 dbus-launch --autolaunch=$MY_DBUS_SESSION ~/bin/pwddwnsys





0 */1  * * *  DISPLAY=:0 dbus-launch --autolaunch=$MY_DBUS_SESSION  notify-send -i ~/.setup/.config/dirs.d/rc.d/osetup/images/icons/glass_empty.png "@"


@reboot    /usr/sbin/anacron -s -S $HOME/.anacron/spool/ -t $HOME/.anacron/etc/anacrontab
15 4 * * * /usr/sbin/anacron -s -S $HOME/.anacron/spool/ -t $HOME/.anacron/etc/anacrontab

# 30 06 * * * DISPLAY=:0 dbus-launch --autolaunch=$MY_DBUS_SESSION   gnomeradio




,
00 22 * * * pkill pidgin
01 22 * * * pkill skype
)
