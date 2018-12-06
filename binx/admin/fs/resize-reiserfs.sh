#!/bin/bash

syncimap -d

LVPART=/dev/mapper/vgres01-lvres01


pkill offlineimap
sudo systemctl stop dovecot.service
sudo systemctl stop postfix.service

sleep 3s

if pgrep offlineimap
then
    echo sleep 30s
    sleep 30s
    echo pkill -9 offlineimap
    pkill -9 offlineimap
fi

if sudo umount $LVPART
then
    echo df $LVPART
    df $LVPART
    echo sleep 10s
    sleep 10s
    sudo reiserfsck --check $LVPART
    sudo lvresize -L +1G $LVPART
    sudo resize_reiserfs $LVPART
else
    sudo lsod $LVPART
fi

if sudo mount $LVPART
then
    sudo systemctl start dovecot.service
    sudo systemctl start postfix.service
    syncimap -r
fi
