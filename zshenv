# -*- Mode: shell-script; indent-tabs-mode: nil -*-
# $Id

# Before/Current
#        /etc/zshenv run, for all instance of zsh
#        if RCS option set
#            run $ZDOTDIR/.zshenv
#        fi

# After
#        if zsh is login shall
#            and RCS/GLOBAL_RCS option set; then
#            run /etc/zprofile and  then  $ZDOTDIR/.zprofile
#        fi

# Usage
#        Put only environment setting or varibales etc. Keep it minimal

#        Commands  are  first read from /etc/zshenv; this cannot be overridden.
#        Subsequent behaviour is modified by the RCS  and  GLOBAL_RCS  options;
#        the  former  affects  all startup files, while the second only affects
#        those in the /etc directory.  If one of the options is  unset  at  any
#        point,  any  subsequent startup file(s) of the corresponding type will
#        not be read.  It is also possible for a file in $ZDOTDIR to  re-enable
#        GLOBAL_RCS. Both RCS and GLOBAL_RCS are set by default.

# PATH variable

typeset -U PATH path
typeset -UT INFOPATH infopath


## GUIX
# Source the system-wide file.
if [ -r /etc/profile ]
then
  source /etc/profile
fi

GUIX_PROFILE="/home/s/hell/.guix-profile"
if [ -r $GUIX_PROFILE/etc/profile ]
then
 . $GUIX_PROFILE/etc/profile
fi

## GUIX

path=(
    # /var/guix/profiles/system/profile/etc/profile
    ~/bin
    ~/.local/bin
    ~/.cask/bin
    # /usr/local/bin
    # /usr/local/sbin
    /bin
    /usr/bin
    # /sbin
    # /usr/sbin
    ~/.emacs.d/term-cmd
    # /home/linuxbrew/.linuxbrew/bin
    $path
    )

infopath=(
    /usr/share/info
    /usr/share/local/info
)

export DOTREMINDERS=~/Documents/CreatedContent/contents/misc/remind/Reminders/init.rem # for remind or rem
export HOST
# export WORKPLACE=

[ -r ~/.$WORKPLACE.zshenv ] && source ~/.$WORKPLACE.zshenv

[  -r ~/.rsetup/sh/env  ] &&
source ~/.rsetup/sh/env

export ALTERNATE_EDITOR='echo bye'
if [ $DISPLAY ] ; then
    export EDITOR=~/bin/general-xeditor
    export VISUAL=$EDITOR
    export BROWSER="conkeror"
else
    export EDITOR=~/bin/general-editor
    export VISUAL=$EDITOR
    export BROWSER="w3m"
fi


# {{{ *Functions*
[ -r ~/.zshall/envfuns ] && source ~/.zshall/envfuns
# }}}
