#!/bin/bash

TMPDIR=~/tmp

ls -l /etc/hosts
head -10 /etc/hosts

if cd ~/tmp
then
    if [ ! -r hosts.txt ]
    then
        if wget -c 'https://hosts-file.net/download/hosts.zip'
        then
            if unzip hosts.zip
            then
                dos2unix hosts.txt || rm -f hosts.txt
            fi
        fi
    fi

    if [ -r hosts.txt ]
    then
        cp /etc/hosts.orignal $TMPDIR/hosts
        echo "\n\n\n" >> $TMPDIR/hosts
        sed -n '/BAD HOSTS BEGIN HERE/,$p' hosts.txt >> $TMPDIR/hosts
        sudo cp $TMPDIR/hosts /etc/hosts
    fi
fi

head -20 /etc/hosts
ls -l /etc/hosts
