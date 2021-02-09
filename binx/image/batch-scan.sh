#!/usr/bin/env zsh



for count in {1..10000}
do
    scanfile=document-${count}.png

    if [ -f document-${count}.png ]
    then
        # newfile=document-x${count}-random-${RANDOM}.png
        echo ${scanfile} exists
        # echo moving to new file ${newfile}
        # cp ${scanfile} ${newfile}
        echo skipping ${scanfile} try next
        continue
    fi

    echo starting scanning in 3 seconds >&2
    sleep 3s
    echo sudo scanimage --mode color --format=png --progress --output-file=${scanfile} --resolution 300 >&2
         sudo scanimage --mode color --format=png --progress --output-file=${scanfile} --resolution 300
    echo completed ${scanfile} >&2
    echo change pictures, press enter >&2
    read x;
done


