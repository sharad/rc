##  .profile -- login script for bash
##
##  last modified:   11-Dec-1998  Fri  09:09

export CDPATH=:..:~:~/pno/src:~/dest
export    TERMINFO=/sas/share/terminfo
export      EDITOR=vim
export        LESS='-Mice'
export       PAGER='less -s'
export       SHELL=/bin/bash
export XAPPLRESDIR=~/app-defaults
export  NNTPSERVER=news-b.sasi.com
export       TROFF=groff		# for man -t
export      LINES
export     COLUMNS=80
export     DISPLAY=$HOSTNAME:0

: ${LINES:=34}

export LS_COLORS='*core=1;31:or=1;31:di=1:bd=0:cd=0:pi=103:so=103:ex=35:ln=36'

export INFOPATH=\
/sas/share/info/xemacs-20.4:\
/sas/share/info:\
/sas/share/info/tex

##  Junk this after upgrading all bashes to 2.0.  That predefines HOSTNAME.
##
case $BASH_VERSION in
    1*) export HOSTNAME=`hostname`  ;;
esac


##  System-specific setup.
##  You need XBMLANGPATH only if you run Motif apps.
##  You need XMBINDDIR only if you run Motif apps under Open Windows.
##
##  XFILESEARCHPATH specifies the path for the auxiliary files of X11 apps.
##  % in the path is used for printf()-like substitutions:
##	%T	file type (usually `app-defaults' or `bitmaps')
##	%N	file name or application class name
##	%S	file suffix.  Usually empty, may be `.bm' for bitmaps.
##	%C	value of your `*customization' resource.  Normally you
##		set this resource to `-color' if you have a colour monitor.
##		Some X11 apps on have two versions of the app-defaults file:
##		one for monochrome monitors, and one with a -color suffix
##		for colour monitors.  As of now this is needed only on Linux.

export PATH MANPATH XFILESEARCHPATH

case $OSTYPE in
    SunOS4|sunos4*)
	export            TCAT=lpr			# for man -t
	export     OPENWINHOME=/usr/openwin
	export       MOTIFHOME=/usr/Motif1.2/IXImd12x
	export LD_LIBRARY_PATH=$MOTIFHOME/lib:/usr/openwin/lib:/sas/lib
	export       XMBINDDIR=$MOTIFHOME/etc/key_bindings
	export     XBMLANGPATH=%B:$MOTIFHOME/include/X11/bitmaps/%B

	##  Put /usr/openwin/bin ahead of /sas/bin/X11 so that xterm and
	##  olvwm are picked up from /usr/openwin, which is local, rather
	##  than from NFS-mounted /sas.
	##
	PATH=~/bin:~/bin/sunos4
	PATH=$PATH:/usr/openwin/bin:/usr/ucb
	PATH=$PATH:/sas/bin:/sas/bin/gnu:/sas/bin/X11
	PATH=$PATH:/usr/bin:/usr/ucb:/usr/5bin:/usr/etc
	PATH=$PATH:$MOTIFHOME/bin:/sas/tex/bin:/usr/games:
	PATH=$PATH:/usr/openwin/demo

	MANPATH=/sas/man:/usr/openwin/share/man:/usr/man
	MANPATH=$MANPATH:/sas/tex/man:$MOTIFHOME/man:/u/scratch/xmt/man

	XFILESEARCHPATH=/sas/lib/X11/%T/%N%S
	XFILESEARCHPATH=$XFILESEARCHPATH:/usr/openwin/lib/%T/%N%S
	XFILESEARCHPATH=$XFILESEARCHPATH:/usr/openwin/include/X11/%T/%N%S
	;;

    SunOS5|solaris*)
	export            TCAT=lp			# for man -t
	export     OPENWINHOME=/usr/openwin
	export LD_LIBRARY_PATH=/usr/dt/lib:/usr/openwin/lib:/sas/lib

	PATH=~/bin:~/bin/solaris2
	PATH=$PATH:/sas/bin:/sas/bin/gnu:/sas/bin/X11
	PATH=$PATH:/usr/openwin/bin:/usr/openwin/demo
        PATH=$PATH:/usr/bin:/sbin:/usr/sbin:/usr/ucb:/usr/proc/bin
	PATH=$PATH:/opt/SUNWspro/bin:/usr/ccs/bin

	MANPATH=/usr/openwin/man:/sas/man:/opt/SUNWspro/man:/usr/man

	XFILESEARCHPATH=/sas/lib/X11/%T/%N%S
	XFILESEARCHPATH=$XFILESEARCHPATH:/usr/openwin/lib/%T/%N%S
	XFILESEARCHPATH=$XFILESEARCHPATH:/usr/openwin/include/X11/%T/%N%S
	;;

    hpux_9|hpux9*)
	export            TCAT=lp			# for man -t
	PATH=~/bin:~/bin/hpux9
	PATH=$PATH:/sas/bin:/sas/bin/gnu:/sas/bin/X11
	PATH=$PATH:/bin:/usr/bin
	PATH=$PATH:/usr/vue/bin:/usr/bin/X11:/usr/contrib/bin/X11
	PATH=$PATH:/etc:/usr/etc:

	MANPATH=/sas/man:/usr/man:/usr/contrib/man

	XFILESEARCHPATH=/sas/lib/X11/%T/%N%S
	XFILESEARCHPATH=$XFILESEARCHPATH:/usr/lib/X11/%T/%N%S:/usr/vue/%T/%N%S
	;;

    hpux10*)
	export            TCAT=lp			# for man -t
	PATH=~/bin:~/bin/hpux10
	PATH=$PATH:/sas/bin:/sas/bin/gnu:/sas/bin/X11
	PATH=$PATH:/usr/bin:/usr/sbin:/usr/bin/X11:/usr/contrib/bin/X11

	MANPATH=/sas/man:/usr/man:/usr/contrib/man

	XFILESEARCHPATH=/sas/lib/X11/%T/%N%S
	XFILESEARCHPATH=$XFILESEARCHPATH:/usr/lib/X11/%T/%N%S
	;;

    [lL]inux*)
	export            TCAT=lpr			# for man -t
	PATH=~/bin:~/bin/linux
	PATH=$PATH:/sas/bin
	PATH=$PATH:/bin:/usr/bin:/sbin:/usr/sbin:/usr/local/bin
	PATH=$PATH:/usr/X11R6/bin:/usr/openwin/bin:

	MANPATH=/usr/man:/usr/X11R6/man:/usr/openwin/man

	XFILESEARCHPATH=/usr/lib/X11/%T/%N%S%C:/usr/lib/X11/%T/%N%S

	unset INFOPATH	TERMINFO		# the defaults are correct
	;;
esac


##  Non-Linux systems don't know about Linux terminals (the console)
##  and non-HP systems don't know about hpterm.  In both cases use the
##  nearest equivalent.

case "$TERM:$OSTYPE" in
    [lL]inux:[lL]inux*)	;;
    [lL]inux:*)		TERM=vt100 ;;
    hpterm:hpux*)	;;
    hpterm:*)		TERM=xterm ;;
esac


##  Code that generates output or assumes stdout is a terminal.
##  This .profile is sourced by `back', which is run as a cron job,
##  so we must first check if this is being run interactively.
##  If your keyboard's BackSpace key is more conveniently placed than
##  the Delete key then change `^\?' to `^h'.

if [ -n "$PS1" -a -z "$VUE" ]; then
    stty cs8 erase ^\? intr ^c kill ^u susp ^z
fi

#-------------------  IXI Motif Settings  ---------------------

##  You need these only if you develop applications with IXI Motif 1.2

case $OSTYPE in
    SunOS4|sunos4*)
	export         UIDPATH=%U:$MOTIFHOME/lib/X11/uid/%U
	export         XLIBDIR=$MOTIFHOME/lib/X11
	export   XLIBI18N_PATH=$MOTIFHOME/lib/X11
	export       VTCL_HOME=$MOTIFHOME/../IXIvt10x/lib/vtcl
	export   IXIMOSAICHOME=$MOTIFHOME/../IXImos
	export   MOTIFHELPHOME=$MOTIFHOME/../IXImfhlp

##	You need to run this just once.
##	[ -f $MOTIFHELPHOME/sh-env ]  &&  . $MOTIFHELPHOME/sh-env
	;;
esac

#----------------  End of IXI Motif Settings  ---------------------


umask 022
[ -n "$PS1" ]  &&  . $HOME/.bashrc
