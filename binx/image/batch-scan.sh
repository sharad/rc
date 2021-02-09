#!/usr/bin/env zsh



function main()
{
    for count in {1..10000}
    do
        scanfile=document-${count}.png

        if [ -f document-${count}.png ]
        then
            notify ${scanfile} exists
            notify skipping ${scanfile} try next
            continue
        fi

        notify change pictures, press enter >&2
        zenity --info --text "Change pictures, press enter in terminal to scan to ${scanfile} file"
        focus-ctrl-term
        notify change pictures, press enter >&2

        read x

        notify starting scanning in 3 seconds >&2
        sleep 3s


        notify sudo scanimage --mode color --format=png --progress --output-file=${scanfile} --resolution 300 >&2
        sudo scanimage --mode color --format=png --progress --output-file=${scanfile} --resolution 300
        notify completed ${scanfile} >&2
    done
}



focus-ctrl-term () {
	  if [ "$PPID" ]
	  then
		    PWIN="$(xdotool search --pid $PPID)"
		    if [ "$PWIN" ]
		    then
			      wmctrl -i -a "$PWIN"
		    else
			      echo Can not get window associated with parent pid $PPID >&2
		    fi
	  else
		    echo can not get shell parent PID >&2
	  fi
}


function error() {
    notify "$*"  >&2
    logger "$*"
}

function warn() {
    if [ $warn ] ; then
        notify "$*"
    fi
    logger "$*"
}

function verbose() {
    if [ $verbose ] ; then
        notify "$*"
    fi
    logger "$*"
}

function notify() {
    if [ -t 1 ] ; then
        # echo -e "${pgm}:" "$*" >&2
        print "${pgm}:" "$*" >&2
    else
        print "${pgm}:" "$*" >&2
        notify-send "${pgm}:" "$*"
    fi
}

function logger() {
    #creating prolem
    command logger -p local1.notice -t ${pgm} -i - $USER : "$*"
}






pgm=$(basename $0)

main $@


