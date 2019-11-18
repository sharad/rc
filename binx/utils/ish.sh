#!/usr/bin/env zsh

runshell=bash
count=1


function main() {



    process_arg $@

    repeat $count { shift }

    exec $runshell -i <<EOF
$@ < /dev/tty
exec <> /dev/tty
EOF
}


function process_arg() {

    set -- $(getopt -n $pgm -o ivwehs: -- $@)
    while [ $# -gt 0 ]
    do
        case $1 in
            (-s) eval runshell=$2; shift;;
            (-i) interactive=1;;
            (-v) verbose=1;;
            (-w) warn=1;;
            (-e) error=1;;
            (-h) help;
                 exit;;
            (--) shift; ((count++)); break;;
            (-*) echo "$0: error - unrecognized option $1" 1>&2; help; exit 1;;
            (*)  break;;
        esac
        shift
        ((count++))
    done

}


function help() {
    cat <<'EOF'
            (-s) eval runshell=$2; shift;;
            (-i) interactive=1;;
            (-v) verbose=1;;
            (-w) warn=1;;
            (-e) error=1;;
            (-h) help;
                 exit;;
            (--) shift; break;;
            (-*) echo "$0: error - unrecognized option $1" 1>&2; help; exit 1;;
            (*)  break;;
EOF

}

function warn() {
    if [ $warn ] ; then
        notify $@
    fi
    logger $@
}

function verbose() {
    if [ $verbose ] ; then
        notify $@
    fi
    logger $@
}

function notify() {
    echo ${pgm}: $@
    # if [ -t 1 ] ; then
    #     print ${pgm}: $@
    # else
    #     notify-send ${pgm}: $@
    # fi
}

function logger() {
    #creating prolem
    : logger -p local1.notice -t ${pgm} -i - $USER : $@
}



pgm=$(basename $0)

main $@

