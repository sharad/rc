#!/usr/bin/env bash


function setvar {

### DATA
yumpkgs=(automake
    autoconf
#    perl
#    perl-CPAN
    bison
    sharutils
    tftp-server
    tftp
    vim-X11
#    gvim
    emacs
    wireshark
    wireshark-gnome
    rdesktop
    vnc-server
    perl-XML-Parser
    expect-devel
    expect
    gcc
    )

cpanpkgs=(XML::Checker IO::Stty Expect IO::Tty Term::ReadKey)

}




function main {

    process_arg $@
    setvar

## PROCESS
    if [ "$search" ] ; then
	echo Searching for packages.
	yum search ${yumpkgs[*]}
    fi

    if [ "$install" ] ; then
	yum install -y  ${yumpkgs[@]}
    fi

    if [ "$search" ] ; then
	echo Searching for CPAN packages.
	cpan <<EOF
	m ${cpanpkgs[@]}
EOF
    fi


    if [ "$install" ] ; then
	cpan -i ${cpanpkgs[@]}
    fi

    if [ "$install" ] ; then
	mkdir -p /usr/local/bin
	if [ ! -e /usr/local/bin/p4 ] ; then
	    if [ "$(uname -m)" = "x86_64" ] ; then
		wget http://www.perforce.com/downloads/perforce/r10.2/bin.linux26x86_64/p4 -O /usr/local/bin/p4
	    else
		wget http://www.perforce.com/downloads/perforce/r10.2/bin.linux26x86/p4    -O /usr/local/bin/p4
	    fi
	fi
    fi

    

}



function process_arg() {
    set -- $(getopt -n $pgm -o ivs -- $@)
    while [ $# -gt 0 ]
    do
        case $1 in
            (-i) install=1;;
            (-s) search=1;;
            (-v) verbose=1;;
            (--) shift; break;;
            (-*) echo "$0: error - unrecognized option $1" 1>&2; exit 1;;
            (*)  break;;
        esac
        shift
    done
}

pgm=$(basename $0)

main $@

