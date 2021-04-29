#!/usr/bin/env bash

# LOTUS_GUIX_NOPULL
# LOTUS_GUIX_NOSYS

function main()
{

    trap setup_finish EXIT SIGINT SIGTERM

    running debug process_arg $@
    # https://guix.gnu.org/blog/2019/guix-profiles-in-practice/
    # https://guix.gnu.org/cookbook/en/
    # https://guix.gnu.org/cookbook/en/html_node/
    # https://guix.gnu.org/cookbook/en/html_node/Advanced-package-management.html#Advanced-package-management
    # https://guix.gnu.org/cookbook/en/html_node/Basic-setup-with-manifests.html#Basic-setup-with-manifests
    if [ -f "$HOME/.setup/guix-config/per-user/$USER/meta/current" ]
    then
        LOCAL_GUIX_EXTRA_PROFILES=( $(cat "$HOME/.setup/guix-config/per-user/$USER/meta/current" | grep -v "01-simple" ) )
    else
        LOCAL_GUIX_EXTRA_PROFILES=("01-dev" "01-console" "01-x" "01-dynamic-hash" "90-heavy" "60-lengthy")
    fi
    export LOCAL_GUIX_EXTRA_PROFILES
    LOCAL_GUIX_EXTRA_PROFILE_CONTAINER_DIR="$HOME/.setup/guix-config/per-user/$USER/profiles"


    if [ -d "/run/current-system/profile" ]
    then

        if true
        then
            sudo umount /boot/efi
            sudo umount /boot

            GUIX_TMPDIR="$(grep /srv/guix /etc/fstab | cut -f2)"
            if [ "x" != "x${GUIX_TMPDIR}" ]
            then

                sudo umount "${GUIX_TMPDIR}"

                if ! running info sudo mount "${GUIX_TMPDIR}"
                then
                    error Failed in mounting "${GUIX_TMPDIR}"
                    exit -1
                fi
            else
                error GUIX_TMPDIR not found
                exit -1
            fi

            if ! running info sudo mount /boot
            then
                error failed mount /boot
                exit -1
            else
                running info sleep 3s
            fi

            if ! running info sudo mount /boot/efi
            then
                error failed mount /boot/efi
                exit -1
            fi
        fi

        if [ "x" != "x$LOTUS_GUIX_NOPULL" ] || running info guix pull
        then
            running info guix pull --news

            if ! running info sudo mount /boot
            then
                error failed mount /boot
                exit -1
            else
                running info sleep 3s
            fi

            if ! running info sudo mount /boot/efi
            then
                error failed mount /boot/efi
                exit -1
            fi

            if [ "x" != "x$LOTUS_GUIX_NOSYS" ] || running info sudo guix system reconfigure "${HOME}/.setup/guix-config/per-domain/desktop/config.scm"
            then
                if true
                then
                    running info sudo umount /boot/efi
                    running info sleep 1s
                    running info sudo umount /boot
                fi

                # verbose guix upgrading
                running info guix upgrade # default
                # running debug guix upgrade -p "${HOME}/.setup/guix-config/per-user/s/cdesktopenv/profiles.d/"
                for profile in "${LOCAL_GUIX_EXTRA_PROFILES[@]}"
                do
                    profile_container_path="${LOCAL_GUIX_EXTRA_PROFILE_CONTAINER_DIR}/${profile}"
                    manifest_path="${profile_container_path}/manifest.scm"
                    profile_path="${profile_container_path}/profiles.d/profile"
                    packages_path="${profile_container_path}/packages"
                    if [ -f "${profile_path}/etc/profile" ]
                    then
                        if [ -f "${packages_path}" ]
                        then
                            for pkg in $(cat "${packages_path}")
                            do
                                running info guix upgrade -p "${profile_path}" "$pkg"
                            done
                        fi

                        running info guix upgrade -p "${profile_path}"

                    else
                        warn file "${profile_path}/etc/profile" not exist, for "${profile_path}"
                    fi
                    unset profile_path
                    unset profile
                done

                verbose guix installing
                running info guix package -m "${LOCAL_GUIX_EXTRA_PROFILE_CONTAINER_DIR}/01-simple/manifest.scm" # default
                for profile in "${LOCAL_GUIX_EXTRA_PROFILES[@]}"
                do
                    profile_container_path="${LOCAL_GUIX_EXTRA_PROFILE_CONTAINER_DIR}/${profile}"
                    manifest_path="${profile_container_path}/manifest.scm"
                    profile_path="${profile_container_path}/profiles.d/profile"
                    packages_path="${profile_container_path}/packages"

                    if [ ! -d "$LOCAL_GUIX_EXTRA_PROFILE_CONTAINER_DIR/$profile/profiles.d" ]
                    then
                        mkdir -p "$LOCAL_GUIX_EXTRA_PROFILE_CONTAINER_DIR/$profile/profiles.d"
                    fi

                    if [ -f "${manifest_path}" ]
                    then

                        if [ -f "${packages_path}" ]
                        then
                            for pkg in $(cat "${packages_path}")
                            do
                                running info guix install -p "${profile_path}" "$pkg"
                            done
                        fi

                        running info guix package -p "${profile_path}" -m "${manifest_path}"

                    else
                        warn file "${manifest_path}" not exist, for "${profile_path}"
                    fi
                    unset profile_path
                    unset profile
                done
            else
                warn guix system reconfigure -- Failed
            fi                  # if [ "x" != "x$LOTUS_GUIX_NOSYS" ] || running info sudo guix system reconfigure "${HOME}/.setup/guix-config/per-domain/desktop/config.scm"
        else
            warn guix pull -- Failed
        fi                      # if [ "x" != "x$LOTUS_GUIX_NOPULL" ] || running info guix pull

        if true
        then
            GUIX_TMPDIR="$(grep /srv/guix /etc/fstab | cut -f2)"
            if [ "x" != "x${GUIX_TMPDIR}" ]
            then

                sudo umount "${GUIX_TMPDIR}"

                if ! running info sudo umount "${GUIX_TMPDIR}"
                then
                    error Failed in unmounting "${GUIX_TMPDIR}"
                    exit -1
                fi
            else
                error GUIX_TMPDIR not found
                exit -1
            fi
        fi
    fi                          # if [ -d "/run/current-system/profile" ]

    running info update_fc_cache

}

function update_fc_cache()
{
    if [ "x" != "x$DISPLAY" ]
    then
	      if which xset
	      then
    	      for fdir in ~/.guix-profile/share/fonts/**/fonts.dir ${LOCAL_GUIX_EXTRA_PROFILE_CONTAINER_DIR}/*/profiles.d/profile/share/fonts/**/fonts.dir
    	      do
                fontdir=$fdir
                ls $fontdir
                if [ -e "$fontdir" ]
                then
                    xset +fp $(dirname $(readlink -f $fontdir))
                else
                    warn fontdir $fontdir file do not exists.
                fi
            done
	          if which fc-cache >/dev/null 2>&1
	          then
                fc-cache -f
	          fi
	      fi
    fi
}

function process_arg()
{
    warn=1
    error=1

    if ! set -- $(getopt -n $pgm -o "rnsehvdw" -- $@)
    then
        verbose Wrong command line.
    fi

    while [ $# -gt 0 ]
    do
        echo option $1
        case $1 in
            (-r) recursive=1;;
            (-s) stash=1;;
            (-n) noaction="";;
            (-d) debug=1;;
            (-v) verbose=1;;
            (-w) warn="";;
            (-e) error="";;
            (-h) help;
                 exit;;
            (--) shift; break;;
            (-*) error "$0: error - unrecognized option $1" 1>&2; help; exit 1;;
            (*)  break;;
        esac
        shift
    done
}

function running()
{

    # echo 1="$1"
    # echo 2="$2"

    local notifier="$1"
    local _cmd="$2"

    shift
    shift


    $notifier $_cmd "$@"
    if [ ! $noaction ]
    then
        $_cmd "$@"
    fi
}

function print()
{
    echo "$*"
}

function error()
{
    notify "Error $*"  >&2
    logger "Error $*"
}

function warn()
{
    if [ $warn ] ; then
        notify "Warning: $*" >&2
    fi
    logger "Warning: $*"
}

function debug()
{
    if [ $debug ] ; then
        notify "Debug: $*" >&2
    fi
    logger "Debug: $*"
}

function verbose()
{
    if [ $verbose ] ; then
        notify "Info: $*" >&2
    fi
    logger "Info: $*"
}

function info()
{
    notify "$*" >&2
    : logger "$*"
}

function notify()
{
    print "${pgm}:" "$*"

    if [ ! -t 1 ]
    then
        notify-send "${pgm}:" "$*"
    fi
}

function logger()
{
    #creating prolem
    command logger -p local1.notice -t ${pgm} -i - $USER : "$*"
}

function setup_finish()
{
    echo
}

#verbose=1

pgm="$(basename $0)"
main "$@"
exit
