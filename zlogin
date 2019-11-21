# $Id

# Before
#       if zsh is interactive
#            and RCS/GLOBAL_RCS option set; then
#          run /etc/zshrc and then $ZDOTDIR/.zshrc
#       fi

# Current
#        if zsh is login shall
#            and RCS/GLOBAL_RCS option set; then
#            run /etc/zlogin and  then $ZDOTDIR/.zlogin
#        fi

# After
#        user keystrok
#        if zsh is login shall
#            and RCS/GLOBAL_RCS option set; then
#            run /etc/zlogout and  then $ZDOTDIR/.zlogout
#        fi


# Usage
#       put all stuff need to run in login shall after
#       zshrc (intract) run.

#       If  the  shell  is  interactive, commands are read from
#       /etc/zshrc and then $ZDOTDIR/.zshrc.  Finally, if the shell is a login
#       shell, /etc/zlogin and $ZDOTDIR/.zlogin are read.
#       When  a  login  shell  exits,  the  files  $ZDOTDIR/.zlogout  and then
#       /etc/zlogout are read.


# for startup in sh login ; do
for startup in login ; do
    [ -r ~/.rsetup/$startup/env ] && . ~/.rsetup/$startup/env
done
for startup in sh login ; do
    [ -r ~/.rsetup/$startup/run ] && ~/.rsetup/$startup/run
done
