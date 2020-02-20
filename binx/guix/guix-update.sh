#!/usr/bin/env bash

LOCAL_GUIX_EXTRA_PROFILES=("dev" "dynamic-hash" "heavy" "lengthy")
export LOCAL_GUIX_EXTRA_PROFILES
LOCAL_GUIX_EXTRA_PROFILE_CONTAINER_DIR="$HOME/.setup/guix-config/per-user/$USER"


if [ -d "/run/current-system/profile" ]
then
    if running info guix pull
    then
        running info guix pull --news
        if running info sudo guix system reconfigure "${HOME}/.setup/guix-config/per-domain/desktop/config.scm"
        then
            # verbose guix upgrading
            running info guix upgrade # default
            # running debug guix upgrade -p "${HOME}/.setup/guix-config/per-user/s/cdesktopenv/profiles.d/"
            for profile in "${LOCAL_GUIX_EXTRA_PROFILES[@]}"
            do
                profile_path="$LOCAL_GUIX_EXTRA_PROFILE_CONTAINER_DIR"/"$profile"/profiles.d/"$profile"
                if [ -f "${profile_path}"/etc/profile ]
                then
                    running info guix upgrade -p "${profile_path}"
                else
                    warn file "${profile_path}"/etc/profile not exist, for "${profile_path}"
                fi
                unset profile_path
                unset profile
            done

            verbose guix installing
            running info guix package -m "${HOME}/.setup/guix-config/per-user/s/simple/manifest.scm" # default
            for profile in "${LOCAL_GUIX_EXTRA_PROFILES[@]}"
            do
                manifest_path="$LOCAL_GUIX_EXTRA_PROFILE_CONTAINER_DIR"/"$profile"/manifest.scm
                profile_path="$LOCAL_GUIX_EXTRA_PROFILE_CONTAINER_DIR"/"$profile"/profiles.d/"$profile"
                if [ -f "${manifest_path}" ]
                then
                    running info guix package -p "${profile_path}" -m "${manifest_path}"
                else
                    warn file "${manifest_path}" not exist, for "${profile_path}"
                fi
                unset profile_path
                unset profile
            done
        else
            warn guix system reconfigure -- Failed
        fi
    fi
fi

