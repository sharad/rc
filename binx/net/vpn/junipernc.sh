#!/bin/sh
#
# Simple control script for Juniper Network Connect VPN clients
#
C='Copyright 2008-2012 Paul D. Smith <paul@mad-scientist.net>'
V='1.20'
D='12 Mar 2012'
U='http://mad-scientist.net/juniper.html'
#
# This script is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3 of the License, or (at your option)
# any later version.
#
# This script is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
# more details.
#
# Requires that 'zenity' or 'kdialog' be installed for GUI support.
# Need *32BIT* sun-java6-jdk and sun-java6-plugin (or equivalent)
# to use the GUI VPN control window.
#
# Default realm: view page source and look for something like:
#    <input ... name="realm" value="XXXX">
# and use "XXXX" (no quotes) as the realm value.
#
# Incorporates ideas from:
#
# Marcin Depinski <marcin@onetime.com>
#       - Support multiple VPN servers
# Paulo Aleixo Campos <paulo.aleixo.campos@ericsson.com>
#       - Support kdialog/KDE

_vpntitle="Network Connect VPN ($V)"
_profile=default

_jpath="$HOME/.juniper_networks"
_ncpath="$_jpath/network_connect"
_jarfile="$_jpath/ncLinuxApp.jar"

_vpnup="$_jpath/ncsvc.running"

_rootok=false
_gui=true
_java=true
_zenity=false
_kdialog=false

_sudo=sudo

_cmd=${0##*/}

_errlog="${TMPDIR:-/tmp}/junipernc.$$"

# Default config values
HOST=
: ${USER:=`id -u -n`}
REALM='RSA'

help ()
{
    cat <<EOF

$_cmd -- manage Juniper Network Connect VPN

This script starts and stops the Juniper Network Connect VPN application
without using your web browser.  It also will install and configure
the VPN on systems (such as Ubuntu) where there is typically no root
password configured.  Finally, it allows you to run the VPN even if
you do not have Java installed on your system (although you cannot get
statistics about the interface, such as packet transfers, etc., in this
mode).

OPTIONS
  --help        This help text.
  --profile <p> Run with profile <p> (normally "default")l
  --gui         Enable GUI mode (default).
  --nogui       Disable GUI mode.  All output goes to stdout/stderr.
  --nojava      Allow GUI dialogs, but don't use the NC GUI control app.
  --root        Allow root to start the VPN.
  --deconfig    Delete the local configuration.  The next time you start
                $0 it will be re-done.
  --uninstall   Delete the local configuration AND uninstall Network Connect
                You will have to reinstall it from scratch.

COPYRIGHT
  $C

VERSION
  Version $V ($D)

AUTHOR
  Paul Smith <paul@mad-scientist.net>
  $U

EOF
    exit 0
}

log () { echo "$*" >> "$_errlog"; }

die ()
{
    _loginfo=''
    [ -s "$_errlog" ] && _loginfo="

Check the error log file '$_errlog' for more information."
    if $_gui; then
        if $_zenity; then
            zenity --title="$_vpntitle" --error --text="$*$_loginfo"
        else
            kdialog --error "$*$_loginfo" --title="$_vpntitle"
        fi
    else
        echo "$*$_loginfo"
    fi
    exit 1
}

msg ()
{
    if $_gui; then
        if $_zenity; then
            zenity --title="$_vpntitle" --info --text="$*"
        else
            kdialog --title="$_vpntitle" --msgbox "$*"
        fi
    else
        echo "$*"
    fi
}

# Find a program on PATH
_foundprog=
findprog ()
{
    _foundprog=`which $1 2>/dev/null`
    return $?
}

query ()
{
    if $_gui; then
        if $_zenity; then
            zenity --title="$_vpntitle" --question --text="$*"
        else
            kdialog  --title "$_vpntitle" --yesno "$*"
        fi
    else
        while true; do
            printf "$* (y/n)? "
            read answer
            case $answer in
                ([Yy]*) return 0;;
                ([Nn]*) return 1;;
            esac
            echo "Bad response: please answer yes or no."
        done
    fi
}

input ()
{
    if $_gui; then
        if $_zenity; then
            zenity --title="$_vpntitle" --entry --text="$1" --entry-text="$2"
        else
            kdialog  --title "$_vpntitle" --inputbox "$1" "$2"
        fi
    else
        printf "$1${2:+ ($2)}? " 1>&2
        read answer
        echo "${answer:-$2}"
    fi
}

_PASSWORD=
password ()
{
    if $_gui; then
        if $_zenity; then
            _PASSWORD=`zenity --title="$_vpntitle" --entry --text="$1" --hide-text`
        else
            _PASSWORD=`kdialog  --title "$_vpntitle" --password "$1"`
        fi
    else
        echo "$_vpntitle" 1>&2
        stty -echo
        printf "$1 ? " 1>&2
        read _PASSWORD
        stty echo
        echo 1>&2
    fi
}

getdir ()
{
    if $_gui; then
        if $_zenity; then
            zenity --title "$*" --file-selection --directory
        else
            kdialog  --title "$*" --getexistingdirectory
        fi
    else
        while true; do
            printf "$* ?" 1>&2
            read dir
            if [ -d "$dir" ]; then
                echo "$dir"
                return 0
            fi
            echo "Not a directory: '$dir'"
        done
    fi
}

uninstall_nc ()
{
    if $1; then
        t=uninstall
    else
        t=deconfigure
    fi
    query "Are you sure you want to $t Juniper Network Connect ($_profile profile)?" \
        || exit 0

    # Deconfigure just removes the customer's configuration + cert
    rm -f "$_vpncfg" "$CERT" || die "$t failed!"
    $1 || exit 0

    # Uninstall also removes the network connect software itself
    rm -rf "$_jpath" && exit 0
    die "$t failed!"
}


# Check the host for configuration issues.
# It has been reported that Knoppix, for example, doesn't have a tun device
setup_host ()
{
    [ -c /dev/net/tun ] && return 0
    query "Your system does not have a /dev/net/tun device configured.
Should I create one?" \
        || exit 0

    ( $_sudo mknod /dev/net/tun c 10 200 && $_sudo chmod 666 /dev/net/tun) \
        >> "$_errlog" 2>&1 \
        || die "Failed to create /dev/net/tun!"
}

check_jar ()
{
    findprog jar || die "This program requires the program 'jar'.
It is often found in the Java JDK package.
Use your package manager to install it."
}

# Set up the Juniper app, if it's not done yet.
setup ()
{
    _svc="$_ncpath/ncsvc"
    # If ncsvc is not available, unpack it
    if [ ! -f "$_svc" ]; then
	check_jar
        (cd "$_ncpath" && jar xf "$_jarfile" && [ -f "$_svc" ]) >> "$_errlog" 2>&1 \
            || die "Could not unpack Juniper Network Connect!"
    fi

    # If the ownership or permissions are not correct, fix them
    if [ `stat -c '%u:%g:%a' "$_svc"` != 0:0:6711 ]; then
        msg "Initial setup requires administrator privileges.  Please enter your password."
        ($_sudo chown 0:0 "$_svc" && $_sudo chmod 06711 "$_svc") >> "$_errlog" 2>&1 \
            || die "Failed to set permissions on '$_svc'!"

        ($_sudo ldd "$_svc" | grep -i 'not found') >> "$_errlog" 2>&1 \
            && die "'$_svc' requires extra libraries to be installed!"
    fi
}

# Allow the user to customize the system
config ()
{
    # Get a hostname--needs to be valid
    err=''
    while true; do
        URL=`input "${err}Enter the Network Connect URL or server:" "$URL"` \
            || exit 1

        # If it looks like a URL, set the hostname.
        # If not, turn off URL and just keep the hostname
        case $URL in
            (http://*)   HOST=${URL#http://} ;;
            (https://*)  HOST=${URL#https://} ;;
            (*)          HOST=$URL; URL= ;;
        esac

        # Remove any leftover stuff: we just want the hostname
        HOST=${HOST%%/*}

        # If the hostname has a port extension strip it
        case $HOST in
            (*:*) PORT=${HOST##*:}; HOST=${HOST%:*} ;;
        esac

        # If it's a hostname, make sure we can look it up.
        case $HOST in
            (*[^.0-9]*)
                host "$HOST" >/dev/null 2>&1 && break
                err="Cannot resolve hostname $HOST.  Please try again.

" ;;

            # It seems to be an IP address; let it go through
            (*) break ;;
        esac
    done

    USER=`input 'Enter the VPN account username' "$USER"` \
        || exit 1

    # See if we can find the REALM from the homepage
    if findprog wget; then
        wargs='-q --no-check-certificate -O -'
        url=${URL:-"https://$HOST"}
        REALM=$( (wget $wargs "$url" || wget $wargs "http://$HOST") \
                    | sed -n 's/.*<input\( [^>]*name="realm" [^>]*\)>.*/\1/p' \
                    | sed -n 's/.* value="\([^"]*\)".*/\1/p')
    fi

    REALM=`input 'Enter the VPN service realm' "$REALM"` \
        || exit 1

    cat > "$_vpncfg" <<EOF
HOST="$HOST"
PORT="$PORT"
USER="$USER"
CERT="$CERT"
REALM="$REALM"
URL="$URL"
EOF
}

# Get the server certificate
getcert ()
{
    _certdir="$_ncpath"
    _clean=false

    check_jar
    if [ ! -x "$_certdir/getx509certificate.sh" ]; then
        _certdir="/tmp/.juniper_temp.$$"
        mkdir -p "$_certdir"
        ( cd "$_certdir" && jar xf "$_jarfile" ) \
            || die "Could not unpack certificate retrieval tool!"
        _clean=true
    fi

    (cd "$_certdir" \
        && /bin/bash ./getx509certificate.sh "$HOST" "$CERT" >>"$_errlog" 2>&1 \
        && chmod 400 "$CERT"
    ) || die "Could not retrieve server certificate from $HOST!"

    $_clean && rm -rf "$_certdir"
}

JAVA=
test_java ()
{
    dir=$1
    [ -n "$dir" ] || return 1

    # If we have a file, change to its directory
    [ -d "$dir" ] || dir=${dir%/*}
    [ -d "$dir" ] || return 1

    # If it's not fully-qualified, make it so
    case $dir in
        (/*) : all set ;;
        (*)  dir=$(cd "$dir" && /bin/pwd -P) ;;
    esac

    # Now see if we can find a JDK_HOME from this path
    while [ -n "$dir" ] && [ ! -x "$dir/bin/java" ]; do
        dir=${dir%/*}
    done

    [ -z "$dir" ] && return 1

    # Found it
    JDK_HOME=$dir
    export JDK_HOME
    JAVA="$JDK_HOME/bin/java"
}

get_bitsize ()
{
    file -L -b "$1" | sed -n 's/.*ELF \([0-9]*\).*/\1/'p
}

setup_java ()
{
    bitsize=$1
    oldjava=

    if [ -n "$JAVA" ] && [ -x "$JAVA" ]; then
        jbit=`get_bitsize "$JAVA"`
        [ "$jbit" = "$bitsize" ] && return 0

        # Wrong size!  Ignore it.
        msg "The Network Connect GUI needs a $bitsize-bit Java
Ignoring $jbit-bit Java from \$JAVA ($JAVA)"
        oldjava=$JAVA
        JAVA=
    fi

    if test_java "$JDK_HOME"; then
        jbit=`get_bitsize "$JAVA"`
        [ "$jbit" = "$bitsize" ] && return 0

        # Wrong size!  Ignore it.
        msg "The Network Connect GUI needs a $bitsize-bit Java
Ignoring $jbit-bit Java in $JDK_HOME"
        JDK_HOME=
    fi

    # Can we find a Java on the PATH, that we haven't tested?
    if findprog java && [ "$oldjava" != "$_foundprog" ]; then
        JAVA=$_foundprog
        jbit=`get_bitsize "$JAVA"`
        [ "$jbit" = "$bitsize" ] && return 0

        # Wrong size!  Ignore it.
        msg "The Network Connect GUI needs a $bitsize-bit Java
Ignoring $jbit-bit Java from \$PATH ($JAVA)"
    fi

    # OK, we can't find anything so far.  Ask the user
    while true; do
        jdir=$( cd /usr/lib/jvm;
                getdir "Select $bitsize-bit Java home directory" ) \
            || die "Cannot locate valid $bitsize-bit Java.
Please install it with your package manager
or run junipernc with the '-nojava' flag."

        if test_java "$jdir"; then
            jbit=`get_bitsize "$JAVA"`
            [ "$jbit" = "$bitsize" ] && return 0

            # Wrong size!  Ignore it.
            msg "The Network Connect GUI needs a $bitsize-bit Java
Ignoring $jbit-bit Java in $jdir"
            JDK_HOME=
        fi
    done
}

# --------------- MAIN

main ()
{
    rm -rf "$_errlog"
    touch "$_errlog"
    log "Commandline: $0 $*"

    # Parse arguments.  We want to do this very early so we get the
    # right value of _gui before we try to print any failures.
    uninstall=
    while [ -n "$1" ]; do
        case $1 in
            (--help|-help)           help ;;
            (--profile|-profile)     _profile=$2; shift ;;
            (--gui|-gui)             _gui=true ;;
            (--nogui|-nogui)         _gui=false ;;
            (--nojava|-nojava)       _java=false ;;
            (--root|-root)           _rootok=true ;;
            (--deconfig|-deconfig)   uninstall=false ;;
            (--uninstall|-uninstall) uninstall=true ;;
            (--) shift; break ;;
            (*) break ;;
        esac
        shift
    done

    # If we want GUI but there's no DISPLAY set, disable it
    if $_gui; then
        case $DISPLAY in
            ('') _gui=false
                 msg "No DISPLAY detected.  Disabling GUI mode.";;
        esac
    fi

    if $_gui; then
        # See if we have gksudo or kdesudo
        if findprog gksudo; then
            _sudo=gksudo
        elif findprog kdesudo; then
            _sudo=kdesudo
        fi

        # Make sure we can find a display command
        if findprog zenity; then
            _zenity=true
        elif findprog kdialog; then
            _kdialog=true
        else
            _gui=false
            msg "GUI mode requires either 'zenity' (for Gnome) or 'kdialog' (for KDE).
Please use your package manager to install one.
Disabling GUI mode."
        fi
    fi

    # If no GUI, disable Java requirement as well
    $_gui || _java=false

    # This script doesn't take any more arguments: fail if there are any
    case $1 in
        ('') : ok ;;
        (*)  die "Invalid arguments: $*" ;;
    esac

    # Are we root?
    if [ `id -u` -eq 0 ]; then
        $_rootok \
            || die "This script should NOT be run as root.  Please run it directly."

        # Root already; we don't need sudo
        _sudo=eval
    fi

    # Is it installed?
    [ -f "$_jarfile" ] || die "Juniper Network Connect is not installed!"

    # Setup
    _vpncfg="$HOME/.vpn.$_profile.cfg"
    CERT="$HOME/.vpn.$_profile.crt"

    [ -n "$uninstall" ] && uninstall_nc $uninstall

    # Backward-compat
    if [ ! -f "$_vpncfg" ] && [ -f "$HOME/.vpn.cfg" ]; then
        grep -v '^CERT=' "$HOME/.vpn.cfg" > "$_vpncfg" \
            && rm -f "$HOME/.vpn.cfg"
    fi

    # Read the custom file if it exists
    if [ -f "$_vpncfg" ]; then
        log "Reading $_vpncfg..."
        . "$_vpncfg"
    fi

    if [ ! -f "$CERT" ] && [ -f "$HOME/.vpn.crt" ]; then
        mv "$HOME/.vpn.crt" "$CERT"
        echo "CERT=\"$CERT\"" >> "$_vpncfg"
    fi

    rm -f "$_vpnup"

    # Check the host configuration
    setup_host

    # Make sure the service is set up
    setup

    # If anything's missing, call the config function
    # PORT and URL are optional
    case ":$HOST:$USER:$CERT:$REALM:" in
        (*::*) config ;;
    esac

    [ -f "$CERT" ] || getcert

    # If we want to use the Java UI front-end, make sure we can find a good one
    if $_java; then
        # Determine the bit size we need for the ncui shared library.
        bitsize=`get_bitsize "$_ncpath/libncui.so"`

        # Set the JDK_HOME value as needed
        cJAVA=$JAVA
        cJDK_HOME=$JDK_HOME
        setup_java "$bitsize"

        [ -n "$JAVA" ] && [ "$cJAVA" != "$JAVA" ] \
            && echo "JAVA=$JAVA" >> "$_vpncfg"
        [ -n "$JDK_HOME" ] && [ "$cJDK_HOME" != "$JDK_HOME" ] \
            && echo "JDK_HOME=$JDK_HOME" >> "$_vpncfg"
    fi

    # If we get here there's nothing useful in the error log, so delete it.
    rm -f "$_errlog"

    # Connect to the remote server and bring up the VPN
    # This loop runs forever or until the user quits.
    while true; do
        password 'Enter your PIN + SecurID Code' || exit 0
        case $_PASSWORD in
            ('') die 'Invalid passphrase' ;;
        esac

        touch "$_vpnup"

        ok=true
        # Send the password on stdin to avoid having it show up via ps
        if $_java; then
            echo "$_PASSWORD" | "$JAVA" -jar "$_ncpath/NC.jar" -h "$HOST" -u "$USER" -r "$REALM" -f "$CERT" ${PORT:+-P "$PORT"} ${URL:+-U "$URL"} >"$_errlog" 2>&1 \
                || ok=false

        else
            # No GUI?  Just let the thing run in the background
            echo "$_PASSWORD" | "$_ncpath/ncsvc" -h "$HOST" -u "$USER" -r "$REALM" -f "$CERT" ${PORT:+-P "$PORT"} ${URL:+-U "$URL"} >"$_errlog" \
                || ok=false
        fi

        rm -f "$_vpnup"

        if $ok; then
            status='VPN has exited successfully.'
        else
            status='VPN has failed!'
        fi

        query "$status

    Would you like to restart the VPN connection?" \
            || exit 0
    done
}

# Call a function.  If we update this script while it's running
# hopefully this helps 
main "$@"; exit 1
