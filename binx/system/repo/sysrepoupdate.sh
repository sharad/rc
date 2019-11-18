#!/usr/bin/env zsh

function setvar() {
    # enable_file=~/.var/comm/disable/$pgm
    repodir=~/.system/ubuntu
}

function main() {

    setvar
    process_arg $@

    update-repo

    # ask

}

function update-repo() {
    findcmd='find '${repodir}' \( -path \*/SCCS -o -path \*/RCS -o -path \*/CVS -o -path \*/MCVS -o -path \*/.svn -o -path \*/.git -o -path \*/.hg -o -path \*/.bzr -o -path \*/_MTN -o -path \*/_darcs -o -path \*/\\{arch\\} \) -prune -o \( -name .\\#\* -o -name \*.o -o -name \*\\~ -o -name \*.bin -o -name \*.lbin -o -name \*.so -o -name \*.a -o -name \*.ln -o -name \*.blg -o -name \*.bbl -o -name \*.elc -o -name \*.lof -o -name \*.glo -o -name \*.idx -o -name \*.lot -o -name \*.fmt -o -name \*.tfm -o -name \*.class -o -name \*.fas -o -name \*.lib -o -name \*.mem -o -name \*.x86f -o -name \*.sparcf -o -name \*.fasl -o -name \*.ufsl -o -name \*.fsl -o -name \*.dxl -o -name \*.pfsl -o -name \*.dfsl -o -name \*.p64fsl -o -name \*.d64fsl -o -name \*.dx64fsl -o -name \*.lo -o -name \*.la -o -name \*.gmo -o -name \*.mo -o -name \*.toc -o -name \*.aux -o -name \*.cp -o -name \*.fn -o -name \*.ky -o -name \*.pg -o -name \*.tp -o -name \*.vr -o -name \*.cps -o -name \*.fns -o -name \*.kys -o -name \*.pgs -o -name \*.tps -o -name \*.vrs -o -name \*.pyc -o -name \*.pyo \) -prune -o  -type f | cut -c'$(( $#repodir + 1))-


    foreach sysf ("${(f)$(eval ${=findcmd})}") {
        if [ -r $sysf ] ; then
            print cp $sysf ${repodir}$sysf
            cp $sysf ${repodir}$sysf
        fi
    }
    git --git-dir=~/.system/.git --work-tree=~/.system/ status
}

function process_arg() {


    set -- $(getopt -n $pgm -o dhscivwent:a:o: -- $@)
    while [ $# -gt 0 ]
    do
        case $1 in
            (-o) eval timeout=$2; shift;;
            (-t) eval interval=$2; shift;;
            (-a) eval action=$2; shift;;
            (-d) debug=1;;
            (-n) noaction=1;;
            (-i) interactive=1;;
            (-v) verbose=1;;
            (-c)
                 if mkdir -p $(dirname $enable_file); touch $enable_file ; then
                     notify "will poweroff to avoid poweroff run \n\t\t $pgm -s "
                 fi
                 exit;;
            (-s) rm -f $enable_file;
                 notify $pgm disabled;
                 exit;;
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
            -c: touch $enable_file;;
            -s: rm -f $enable_file;;
            -w: warn=1;;
            -e: error=1;;
            -h: help;;
EOF

}

function warn() {
    if [ $warn ] ; then
        notify "$*"
    fi
    logmsg "$*"
}

function verbose() {
    if [ $verbose ] ; then
        notify "$*"
    fi
    logmsg "$*"
}

function notify() {
    if [ -t 1 ] ; then
        echo -e "${pgm}" "$*"
    else
        notify-send "${pgm}" "$*"
    fi
    logmsg "$*"
}

function logmsg() {
    #creating prolem
    logger -i -p local1.notice -t ${pgm}  -- "$*"
}


pgm=$(basename $0)

main $@
