#!/usr/bin/env zsh


color=1

function main() {
    process_arg $@

    blankpage=$(tempfile)

    echo -n showpage > ${blankpage}.ps # repforum
    ps2pdf ${blankpage}.ps ${blankpage}.pdf

    if [ -d $src ] ; then
        cd $src
        if [ $test ] ; then
            echo running
            echo find $rest -type f
            eval find $rest -type f
        else
            echo running
            echo find $rest -type f
            files=( ${(f)"$(eval find $rest -type f )"} )
            foreach f ($files) {
                mkdir -p $dst/$(dirname $f)
                enscript --color=$color -f Courier7  -E $f -p${dst}/${f}.ps
                ps2pdf ${dst}/${f}.ps ${dst}/${f}.pdf
                rm -f ${dst}/${f}.ps

                if [ "$blank" ] ; then
                    if (( $(pdftk ${dst}/${f}.pdf dump_data output | grep NumberOfPages  | cut -d' ' -f2) % 2 == 1 )) ; then
                        mv ${dst}/${f}.pdf ${dst}/${f}-1.pdf
                        pdftk ${dst}/${f}-1.pdf ${blankpage}.pdf cat output ${dst}/${f}.pdf
                        rm -f ${dst}/${f}-1.pdf
                    fi
                fi
            }
        fi
        cd -
    fi
}

function process_arg() {
    set -- $(getopt -n $pgm -o tvwebs:c:d: -- $@)
    while [ $# -gt 0 ]
    do
        case $1 in
            (-s) eval src=$2; shift;;
            (-d) eval dst=$2; shift;;
            (-c) eval color=$2; shift;;
            (-t) test=1;;
            (-b) blank=1;;
            (-v) verbose=1;;
            (-w) warn=1;;
            (-e) error=1;;
            (--) shift; break;;
            (-*) echo "$0: error - unrecognized option $1" 1>&2; exit 1;;
            (*)  break;;
        esac
        shift
    done

    rest=$@
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
    if [ -t 1 ] ; then
        print ${pgm}: $@
    else
        notify-send ${pgm}: $@
    fi
}

function logger() {
    logger -p local1.notice -t ${pgm} -i - $USER : $@
}

pgm=$(basename $0)

main $@
