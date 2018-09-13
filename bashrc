# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything:
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ]; then
    eval "`dircolors -b`"
    alias ls='ls -F --color=auto'
    #alias dir='ls --color=auto --format=vertical'
    #alias vdir='ls --color=auto --format=long'
fi

# some more ls aliases
alias ll='ls -l'
alias la='ls -A'
#alias l='ls -CF'


# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" -a -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
xterm-color)
    #PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
     #PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]\$ '
     PS1='${debian_chroot:+($debian_chroot)}\[\033[01;34m\]<\W>\[\033[00m\]\[\033[01;32m\]\u\[\033[00m\]\$ '
    ;;
*)
    #PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
    #PS1='${debian_chroot:+($debian_chroot)}\u@\h:\W\$ '
    PS1='${debian_chroot:+($debian_chroot)}<\W>\u\$ '
    ;;
esac

# Comment in the above and uncomment this below for a color prompt
#PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD}\007"'
    ;;
*)
    ;;
esac

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profiles
# sources /etc/bash.bashrc).

# if [ -f /etc/bash_completion ]; then
#     . /etc/bash_completion
# fi

# My Custom Functions	     #

# set c dir
chome () {
    my_homes=(`\ls -N ~/..`)
    read -s -t 5 -p "`\ls -Nm ~/..`
dir: " MYHINDX
    MYHOME=${my_homes[$MYHINDX]}
    unset my_homes MYHINDX
}

c () {
	if [ $# -eq 1 ]; then
		cd $@ && ls -A -F
	else
		cd $MYHOME &&  ls -A -F
	fi
      }

paradise () {
	if [ $# -eq 1 ]; then
		cd $@ && ls -A -F
	else
		cd ~/../paradise &&  ls -A -F
	fi
      }


spoil () {
	if [ $# -eq 1 ]; then
		cd $@ && ls -A -F
	else
		cd ~/../spoil &&  ls -A -F
	fi
      }


getcddb () {
		echo =================== >> ~/.cddb;
		du -a	/mnt/cdrom	>>  ~/.cddb ;
		echo ============================== >> ~/.cddb
}

ad () { for i in ~/.dates/*.lst; do dates $@ $i ; done }

# quite () {
# 	if [ $# -eq 1 ]; then
# 		cd $@ && ls -A -F
# 	else
# 		cd ~/../quite &&  ls -A -F
# 	fi
#       }

q () {
	tput clear;
	tput cup 9 0 ;
	/usr/games/fortune $*;
	tput cup 37 0
	echo [?17\;0\;60c
	if [ $TERM != xterm ]; then
	    consolechars -f /usr/share/consolefonts/t850b.psf.gz
	fi
}

enable_comletion () {
    if [ -f /etc/bash_completion ]; then
	. /etc/bash_completion
    fi
}


### My Alias #
alias still='echo [?17\;0\;60c'
alias devil='dict -d devil'
alias dictwn='dict -d wn'
#alias l=less
#alias dict='less /home/paradise/books/wb1913_Complete.txt'
#alias brw='links http://large.org'
alias brw='links /pacific/weblarge/var/www/html/index.html'
alias xx=startx
alias aumix="( unset DISPLAY && aumix )"
alias comic='consolechars -f /usr/share/consolefonts/t850b.psf.gz'
#c /qutm/home/paradise/cflowgraph/c2vcg3



#export PS1="<\W>\u\$ "

# history setting
    # don't put duplicate lines in the history. See bash(1) for more options
# export P ATH=$P ATH:~/bin
# for instance
# export MANP ATH=$MANPATH:/usr/local/share/man
export HISTCONTROL=ignoredups
export HISTFILESIZE=1000000000
export HISTSIZE=50000
export EDITOR=emacs
export VISUAL=emacs
export CVS_RSA=ssh
export LOCALCVSROOT=/atlantic/lib/cvs/
export SFC2VCGCVSROOT=:ext:sh4r4d@c2vcg.cvs.sourceforge.net/cvsroot/c2vcg
export CVSROOT=$LOCALCVSROOT
#=========================================================================#
# Warnning!: My Dear Please Note: Exporting This variable wn't allow	  #
# Bash script files to change directories.                                #
# for instnace
# CDPATH=/home/s/
#=========================================================================#


# All rest I want	 #

[  -r ~/.status/rememberMe ] && cat ~/.status/rememberMe

if which dates ; then
cat <<EOF

[31;1mScheduled Dates For[39;0m :
`ad`
EOF
fi

# For changing cursor to cyan still block.	  #
echo [?17\;0\;60c

## Stty setting
stty erase  susp 

export CVSROOT=/home/c_sharad/rpstry

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

# {{{
[  -r ~/.rsetup/eev/env  ] &&
    source ~/.rsetup/eev/env
[  -r ~/.rsetup/eev/run ] &&
    ~/.rsetup/eev/run
# }}}
