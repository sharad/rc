#!/usr/bin/env bash


USER_GENERATION_CLEANUP_TIME=96h
SYSTEM_GENERATION_CLEANUP_TIME=10m # 10 months
SYSTEM_ABONDONED_PKG_CLEANUP_MIN_TIME=30d
SYSTEM_ABONDONED_PKG_CLEANUP_MIN_SPACE=3G


LOCAL_GUIX_EXTRA_PROFILES=("dev" "dynamic-hash" "heavy" "lengthy")
export LOCAL_GUIX_EXTRA_PROFILES
LOCAL_GUIX_EXTRA_PROFILE_CONTAINER_DIR="$HOME/.setup/guix-config/per-user/$USER"


if [ -d "/run/current-system/profile" ]
then
    if running info guix pull
    then
        running info guix pull --news

            df -hx tmpfs -x devtmpfs

            if true
            then

                guix package  --delete-generations=${USER_GENERATION_CLEANUP_TIME}
                for profile in "${LOCAL_GUIX_EXTRA_PROFILES[@]}"
                do
                    manifest_path="$LOCAL_GUIX_EXTRA_PROFILE_CONTAINER_DIR"/"$profile"/manifest.scm
                    profile_path="$LOCAL_GUIX_EXTRA_PROFILE_CONTAINER_DIR"/"$profile"/profiles.d/"$profile"
                    if [ -f "${manifest_path}" -a -f "${profile_path}"/etc/profile ]
                    then
                        running info guix package -p "${profile_path}" --delete-generations=${USER_GENERATION_CLEANUP_TIME}
                    else
                        warn file "${profile_path}"/etc/profile not exist, for "${profile_path}"
                    fi
                    unset profile_path
                    unset profile
                done

                sudo guix system delete-generations ${SYSTEM_GENERATION_CLEANUP_TIME}
                guix gc -d ${SYSTEM_ABONDONED_PKG_CLEANUP_MIN_TIME} -C  ${SYSTEM_ABONDONED_PKG_CLEANUP_MIN_SPACE}

            fi

            df -hx tmpfs -x devtmpfs


    fi
fi

