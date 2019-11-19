# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

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

export PATH=$PATH:~/bin

for startup in sh login ; do
    [ -r ~/.rsetup/$startup/env ] && . ~/.rsetup/$startup/env
done
for startup in sh login ; do
    [ -r ~/.rsetup/$startup/run ] && ~/.rsetup/$startup/run
done


# if which zsh > /dev/null 2>&1
# then
#     export SHELL="$(which zsh)"
# elif [ -x /bin/zsh ] # GUIX
# then
#  export SHELL=/bin/zsh
# fi


