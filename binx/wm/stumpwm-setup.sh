#!/usr/bin/env zsh

function setvars() {
    WM=stumpwm
    debug=1
}

function main() {


    setvars
    process_arg $@

    if pgrep $WM 2>&1 > /dev/null ; then
        STUPMWM_INSTALL_DIR=$(readlink -m /proc/$(command ps -C ${WM} -o pid --no-heading | tr -d ' ' )/exe | xargs dirname  | xargs dirname)
        [ "$debug" ] && notify $STUPMWM_INSTALL_DIR
        STUMPISH=${STUPMWM_INSTALL_DIR}/share/contrib/stumpish
        if [  -e $STUMPISH ] ; then
            if [ ! -e ~/.setup/binx/wm/stumpish -o "$STUMPISH" != $(readlink -m ~/.setup/binx/wm/stumpish) ] ; then
                [ "$debug" ] && notify $STUPMISH
                ln -sf $STUMPISH ~/.setup/binx/wm/
                if [ ! -L ~/.setup/bin/stumpish ] ; then
                    ln -s ../binx/wm/stumpish ~/bin/stumpish
                fi
            else
                notify $WM already
            fi
        else
            notify ${WM} setup contrib in $STUPMWM_INSTALL_DIR/share
        fi
    else
        # verbose already offline map running with pid $(pgrep offlineimap).
        notify $WM is not running.
        exit 0;
    fi


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
