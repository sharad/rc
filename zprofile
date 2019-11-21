# $Id

# Before
#        /etc/zshenv run, for all instance of zsh
#        if RCS option set
#            run $ZDOTDIR/.zshenv
#        fi

# Current
#        if zsh is login shall
#            and RCS/GLOBAL_RCS option set; then
#            run /etc/zprofile and  then $ZDOTDIR/.zprofile
#        fi

# After
#       if zsh is interactive
#            and RCS/GLOBAL_RCS option set; then
#          run /etc/zshrc and then $ZDOTDIR/.zshrc
#       fi

# Usage
#       Put all login related stuff need to run before
#       zshrc (intract), after it zlogin (login) will take care

#       Commands are then read from $ZDOTDIR/.zshenv.  If the shell is a login
#       shell, commands are read from /etc/zprofile and  then  $ZDOTDIR/.zpro-
#       file.   Then,  if  the  shell  is  interactive, commands are read from
#       /etc/zshrc and then $ZDOTDIR/.zshrc.  Finally, if the shell is a login
#       shell, /etc/zlogin and $ZDOTDIR/.zlogin are read.

# required only in bash
# if [ -e ~/.zshrc ]; then
#     source ~/.zshrc
# fi

# All rest I want	 #

	if [ -r ~/.status/rememberMe ] ; then
		cat ~/.status/rememberMe
	fi


# 	cat <<EOF
# 	[31;1mScheduled Dates For[39;0m :
# 	`ad`
# EOF


###########################################################################
# Clear existing broken ssh-agent environment
#

if [ -r ~/.profile ]
then
    source ~/.profile
fi

# for startup in login ; do
for startup in sh
do
    if [ -r ~/.rsetup/$startup/env ]
    then
        . ~/.rsetup/$startup/env
    fi
done
for startup in sh
do
    if [ -r ~/.rsetup/$startup/run ]
    then
        ~/.rsetup/$startup/run
    fi
done
