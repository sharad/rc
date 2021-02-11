#!/usr/bin/env zsh



function main()
{


    process_arg "$@"

    for count in {1..10000}
    do
        scanfile=document-${count}

        if [ -f ${scanfile}.png -o -f ${scanfile}.tiff -o -f ${scanfile}.bmp -o -f ${scanfile}.jpg -o -f ${scanfile}.jpeg -o -f ${scanfile}.pdf  ]
        then
            notify ${scanfile}.* exists
            notify skipping ${scanfile}.* try next
            continue
        fi

        notify change pictures, press enter >&2
        zenity --info --text "Change pictures, press enter in terminal to scan to ${scanfile} file"
        focus-ctrl-term
        notify change pictures, press enter >&2

        read x

        notify starting scanning in 3 seconds >&2

        sleep 3s
        if [ -n "${format}" ]
        then
            notify sudo scanimage --mode "${mode}" --format=${format} --progress --output-file=${scanfile}.${format} --resolution ${resolution} >&2
            sudo scanimage --mode "${mode}" --format=${format} --progress --output-file=${scanfile}.${format} --resolution ${resolution}
            sleep 1
        fi
        notify sudo scanimage --mode "${mode}" --format=png --progress --output-file=${scanfile}.png --resolution ${resolution}
        sudo scanimage --mode "${mode}" --format=png --progress --output-file=${scanfile}.png --resolution ${resolution}
        sleep 1
        notify sudo scanimage --mode "${mode}" --format=tiff --progress --output-file=${scanfile}.tiff --resolution ${resolution}
        sudo scanimage --mode "${mode}" --format=tiff --progress --output-file=${scanfile}.tiff --resolution ${resolution}

        if [ -n "${format}" ]
        then
            du -sh ${scanfile}.${format}
        fi
        du -sh ${scanfile}.png ${scanfile}.tiff
        df -h .

        if [ -n "${format}" ]
        then
            eog ${scanfile}.${format}
        fi
        eog ${scanfile}.png
        eog ${scanfile}.tiff

        notify completed ${scanfile} >&2
    done
}


function process_arg() {
    warn=1
    error=1
    resolution=300
    mode=Color
    # format=

    if ! set -- $(getopt -n $pgm -o r:f:m:vweh: -- $@)
    then
        verbose Wrong command line.
    fi
    while [ $# -gt 0 ]
    do
        case $1 in
            (-f) eval format=$2; shift;;
            (-m) eval mode=$2; shift;;
            (-r) eval resolution=$2; shift;;
            # (-i)
            #     interactive=1
            #     # ttyui
            #     ui=blinkenlights;;
            # (-u) eval ui=$2; shift;;
            (-v) verbose=1;;
            # (-s)
            #     if [ -f $disable_file ] ; then
            #          notify $pgm is disabled;
            #          exit -1;
            #     else
            #         notify $pgm is enabled;
            #         exit 0;
            #     fi
            #     ;;
            # (-d)
            #     if [ -f $disable_file ] ; then
            #          notify $pgm is already disabled;
            #     else
            #      if mkdir -p $(dirname $disable_file); touch $disable_file ; then
            #          sync
            #          notify $pgm is disabled;
            #      fi
            #     fi
            #     exit;;
            # (-r)
            #     if [ -f $disable_file ] ; then
            #         sync
            #         rm -f $disable_file && notify $pgm is enabled;
            #     else
            #         notify $pgm is already enabled;
            #     fi
            #     exit;;
            (-w) warn="";;
            (-e) error="";;
            (-h) help;
                 exit;;
            (--) shift; break;;
            (-*) echo "$0: error - unrecognized option $1" 1>&2; help; exit 1;;
            (*)  break;;
        esac
        shift
    done
}

function sleep() {
    echo sleep $@
    command sleep $@
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


