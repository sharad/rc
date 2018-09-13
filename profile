# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

export PATH=$PATH:~/bin

for startup in sh login ; do
    [ -r ~/.rsetup/$startup/env ] && . ~/.rsetup/$startup/env
done
for startup in sh login ; do
    [ -r ~/.rsetup/$startup/run ] && ~/.rsetup/$startup/run
done

export SHELL=/bin/zsh
