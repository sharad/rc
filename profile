# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

HOST=`uname -n`

[ $SHELL = "zsh" ] && emulate sh

if [ ! -f "${SSH_AUTH_SOCK}" ] ; then
  export SSH_AUTH_SOCK=""
fi


if [ -r ~/.ssh/agent-$HOST ] ; then
    . ~/.ssh/agent-$HOST > /dev/null
fi
# if pam_ssh have not created agent then we will rely on keychain.

# if ssh auth forwarding is enabled, use it and dont start keychain

# if df | grep `cat ~/.ecryptfs/Private.mnt ` 2>&1 > /dev/null ; then
    # ecryptfs-umount-private         # forget passphrase.

    # if ! ssh-add -l 2>/dev/null && [ "${SSH_AUTH_SOCK}x" = "x" ] && [ "$UID" != "0" ] ; then
    # if [ "${SSH_AUTH_SOCK}x" = "x" ] && [ "$UID" != "0" ] ; then
    if ! ssh-add -l >/dev/null && [ "$UID" != "0" ] ; then

        if which $HOME/bin/ecryptfs-mount-private 2>&1; then
            $HOME/bin/ecryptfs-mount-private
            if [ -x /usr/bin/keychain ] ; then

                #/usr/bin/keychain -q -Q --lockwait 1 id_rsa id_dsa

                /usr/bin/keychain -q  \
                    --ignore-missing   \
                    --clear --lockwait 1\
                    --agents gpg,ssh     \
                    --inherit local-once  \
                    $( ls -d1 ~/.ssh/login-keys.d/*) \
                    ~/.ssh/login-keys.d/id_dsa       \
                    ~/.ssh/login-keys.d/id_rsa       \
                    ~/.ssh/login-keys.d/internet 070E69E5

        # echo cat ~/.keychain/${HOST}-sh >&2
        # cat ~/.keychain/${HOST}-sh >&2

                if [ -f ~/.keychain/${HOST}-sh ] ; then
                    . ~/.keychain/${HOST}-sh
            # echo cat ~/.keychain/${HOST}-sh >&2
            # cat ~/.keychain/${HOST}-sh >&2
                fi
            fi
            if [ -x /sbin/umount.ecryptfs_private ] ; then
                /sbin/umount.ecryptfs_private
            else
                ecryptfs-umount-private
            fi
        fi
    fi
# fi

# If we have ssh-agent running, forward it to the next host,
# otherwise dont try to use key authentication at all.
if [ "${SSH_AUTH_SOCK}x" = "x" ]; then
    # if we dont have an auth sock, dont use pub key identification
    alias ssh='ssh -o PubkeyAuthentication=no'
else
    # We do have an auth sock, use auth forwarding
    alias ssh='ssh -A'
fi

[ $SHELL = "zsh" ] && emulate zsh

export SHELL=/bin/zsh

