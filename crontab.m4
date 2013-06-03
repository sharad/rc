# m h  dom mon dow   command

SHELL=/bin/zsh
# MAILTO=$MAILTO variable set

# */7 * * * * DISPLAY=:0 dbus-launch --autolaunch=$MY_DBUS_SESSION ~/bin/syncimap -a $OFFLINEIMAPACCOUNT > /dev/null 2>&1

*/7 * * * * DISPLAY=:0 ~/bin/syncimap -a $OFFLINEIMAPACCOUNT > /dev/null 2>&1

# 0 0 * * * /aruba/utils/cm/bin/p4bkup --puthere ~/.p4bkup --copies 10




# try waking up at 7.30 to 7.45
# 00 06 * * * ~/bin/alarm -h .2 -p myfav -r mpc
00 06 * * * ~/bin/alarm -h .2 -p 6 -r radio


# 29 06 * * * mpc load myfav
# 30 06 * * * mpc play
30 06 * * * ~/bin/mradion -c 6
# 31 06 * * * mpc volume 100
# 45 06 * * * spd-say -C

# 00 07 * * * mpc play
00 07 * * * ~/bin/mradion -c 6
# 01 07 * * * mpc volume 100

# # try waking up at 7.30 to 7.45
# 30 07 * * * mpc play
30 07 * * * ~/bin/mradion -c 6
# 31 07 * * * mpc volume 100
# 45 07 * * * spd-say -C

# # try waking up at 8.00 to 8.15
# 00 08 * * * mpc play
00 08 * * * ~/bin/mradion -c 2
# 15 08 * * * spd-say -C

# # try waking up at 8.30 to 8.45
# 30 08 * * * mpc play
30 08 * * * ~/bin/mradion -c 2
# 45 08 * * * spd-say -C


00 06 * * * echo "radio_set_channel 6" > /tmp/mplayer.fifo



# run each hour on week days
50 9-22/1 * * 1-5 DISPLAY=:0 dbus-launch --autolaunch=$MY_DBUS_SESSION ~/bin/pwddwnsys

# run 9.50 daily
50 09     * * *   DISPLAY=:0 dbus-launch --autolaunch=$MY_DBUS_SESSION ~/bin/pwddwnsys

# run 10.50 daily
50 22     * * *   DISPLAY=:0 dbus-launch --autolaunch=$MY_DBUS_SESSION ~/bin/pwddwnsys





0 */1  * * *  DISPLAY=:0 dbus-launch --autolaunch=$MY_DBUS_SESSION  notify-send -i ~/.osetup/images/icons/glass_empty.png "@"


@reboot    /usr/sbin/anacron -s -S $HOME/.anacron/spool/ -t $HOME/.anacron/etc/anacrontab
15 4 * * * /usr/sbin/anacron -s -S $HOME/.anacron/spool/ -t $HOME/.anacron/etc/anacrontab

# 30 06 * * * DISPLAY=:0 dbus-launch --autolaunch=$MY_DBUS_SESSION   gnomeradio




