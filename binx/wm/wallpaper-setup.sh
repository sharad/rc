#!/usr/bin/env zsh

function setvars() {
    LOCAL_WALLPAPER_DIR=~/.share/backgrounds
    typeset -A POSSIBLE_WALLPAPER_DIRS
    POSSIBLE_WALLPAPER_DIRS=(/usr/share/backgrounds/ ubuntu-wallpapers-quantal)
    debug=${debug+}
}

function main() {


    setvars
    process_arg $@

    if [ ! -d $LOCAL_WALLPAPER_DIR ] ; then
        mkdir -p $LOCAL_WALLPAPER_DIR
    fi

    foreach d (${(k)POSSIBLE_WALLPAPER_DIRS}) {
        if [ ! -d $LOCAL_WALLPAPER_DIR -o "$(readlink -m $LOCAL_WALLPAPER_DIR/${POSSIBLE_WALLPAPER_DIRS[$d]:-$(basename $d)})" = "$d" ] ; then
            rm -f $LOCAL_WALLPAPER_DIR/${POSSIBLE_WALLPAPER_DIRS[$d]:-$(basename $d)}
            ln -s $LOCAL_WALLPAPER_DIR/${POSSIBLE_WALLPAPER_DIRS[$d]:-$(basename $d)} $d
        fi
    }

}

function process_arg() {

    set -- $(getopt -n $pgm -o hivwe -- $@)
    while [ $# -gt 0 ]
    do
        case $1 in
            (-i) interactive=1;;
            (-v) verbose=1;;
            (-w) warn=1;;
            (-e) error=1;;
            (-h) help;
                 exit;;
            (--) shift; break;;
            (-*) echo "$0: error - unrecognized option $1" 1>&2; help; exit 1;;
            (*)  break;;
        esac
        shift
    done
}

function help() {
    cat <<'EOF'
            -a: eval account=$2; shift;;
            -i: interactive=1;;
            -v: verbose=1;;
            -d: touch $disable_file;;
            -r: rm -f $disable_file;;
            -w: warn=1;;
            -e: error=1;;
            -h: help;;
EOF

}

function error() {
    notify "$*"
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
        notify-send "${pgm}:" "$*"
    fi
}

function logger() {
    #creating prolem
    command logger -p local1.notice -t ${pgm} -i - $USER : "$*"
}

pgm=$(basename $0)

main $@

