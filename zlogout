# $Id

# Before
#        if zsh is login shall 
#            and RCS/GLOBAL_RCS option set; then 
#            run /etc/zlogin and  then $ZDOTDIR/.zlogin
#        fi

# Current/After
#        user keystrok
#        if zsh is login shall 
#            and RCS/GLOBAL_RCS option set; then 
#            run /etc/zlogout and  then $ZDOTDIR/.zlogout
#        fi
        
# Usage
#       put all stuff need to run in login shall on logging out
#       or exiting.

#       if the shell is a login shell, /etc/zlogin and $ZDOTDIR/.zlogin are read.
#       When  a  login  shell  exits,  the  files  $ZDOTDIR/.zlogout  and then
#       /etc/zlogout are read.