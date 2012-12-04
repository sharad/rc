# m h  dom mon dow   command

SHELL=/bin/zsh
# MAILTO=$MAILTO variable set

# */7 * * * * DISPLAY=:0 dbus-launch --autolaunch=$MY_DBUS_SESSION ~/bin/syncimap -a $OFFLINEIMAPACCOUNT > /dev/null 2>&1

# */7 * * * * DISPLAY=:0 ~/bin/syncimap -a $OFFLINEIMAPACCOUNT > /dev/null 2>&1

# 0 0 * * * /aruba/utils/cm/bin/p4bkup --puthere ~/.p4bkup --copies 10




# try waking up at 7.30 to 7.45
00 06 * * * ~/bin/alarm -h .2 -p myfav

29 06 * * * mpc load myfav
30 06 * * * mpc play
31 06 * * * mpc volume 100
45 06 * * * spd-say -C

00 07 * * * mpc play
01 07 * * * mpc volume 100

# try waking up at 7.30 to 7.45
30 07 * * * mpc play
31 07 * * * mpc volume 100
45 07 * * * spd-say -C

# try waking up at 8.00 to 8.15
00 08 * * * mpc play
15 08 * * * spd-say -C

# try waking up at 8.30 to 8.45
30 08 * * * mpc play
45 08 * * * spd-say -C

50 08 * * * DISPLAY=:0 dbus-launch --autolaunch=$MY_DBUS_SESSION ~/bin/pwddwnsys -c
00 09 * * * ~/bin/pwddwnsys -g

# 30 09 * * * ~/bin/pwddwnsys -g
# 30 10 * * * ~/bin/pwddwnsys -g
# 30 12 * * * ~/bin/pwddwnsys -g
# 30 14 * * * ~/bin/pwddwnsys -g
# 30 16 * * * ~/bin/pwddwnsys -g
# 30 18 * * * ~/bin/pwddwnsys -g
# 30 20 * * * ~/bin/pwddwnsys -g
# 30 21 * * * ~/bin/pwddwnsys -g
# 30 01 * * * ~/bin/pwddwnsys -g
# 30 03 * * * ~/bin/pwddwnsys -g
# 30 04 * * * ~/bin/pwddwnsys -g
# 30 05 * * * ~/bin/pwddwnsys -g
# 00 06 * * * ~/bin/pwddwnsys -g



*/34  *  * * *  DISPLAY=:0 dbus-launch --autolaunch=$MY_DBUS_SESSION  notify-send -i ~/.osetup/images/icons/glass_empty.png "@"


@reboot    /usr/sbin/anacron -s -S $HOME/.anacron/spool/ -t $HOME/.anacron/etc/anacrontab
15 4 * * * /usr/sbin/anacron -s -S $HOME/.anacron/spool/ -t $HOME/.anacron/etc/anacrontab
