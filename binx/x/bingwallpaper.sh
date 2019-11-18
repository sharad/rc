#!/usr/bin/env zsh
# author: Whizzzkid (me@nishantarora.in)

# Base URL

bing="http://www.bing.com"

# API end point
api="/HPImageArchive.aspx?"

# Response Format (json|xml)
format="&format=js"

# # For day (0=current; 1=yesterday... so on)
# day="&idx=0"

# Market for image
market="&mkt=en-US"

# API Constant (fetch how many)
const="&n=1"

# Image extension
extn=".jpg"

# Size
size="1920x1200"

# Collection Path
# imgpath="$HOME/Pictures/Bing/"

imgpath="$HOME/.share/backgrounds/bing/"


########################################################################
#### DO NOT EDIT BELOW THIS LINE #######################################
########################################################################
function main() {

    dayId=0

    process_arg $@

    # For day (0=current; 1=yesterday... so on)
    day="&idx=$dayId"

    # Required Image Uri
    reqImg=$bing$api$format$day$market$const

    # Logging
    echo "Pinging Bing API..."

    # Fetching API response
    apiResp=$(curl -s $reqImg)

    # Default image URL in case the required is not available
    defImgURL=$bing$(echo $apiResp | grep -oP "url\":\"[^\"]*" | cut -d "\"" -f 3)

    # Req image url (raw)
    reqImgURL=$bing$(echo $apiResp | grep -oP "urlbase\":\"[^\"]*" | cut -d "\"" -f 3)"_"$size$extn

    # Trying to check if reqImgURL exists
    if ! wget --quiet --spider $reqImgURL
    then
        reqImgURL=$defImgURL
    fi

    # Logging
    echo "Bing Image of the day: $reqImgURL"

    # Getting Image Name
    imgName=${reqImgURL##*/}

    # Create Path Dir
    mkdir -p $imgpath

    # Saving Image to collection
    resolveDownloadImg $reqImgURL $imgpath$imgName

    #Logging
    echo "Saving image to $imgpath$imgName"

    # gnomewallpaper $imgpath$imgName

    displaywallpaper $imgpath$imgName

    exit
}

function resolveDownloadImg() {
    reqImgURL="$1"
    localPath="$2"
    curl -s -o $localPath $reqImgURL
    if file $localPath | grep -q HTML
    then
        newReqImgURL="$(xmlstarlet sel -t -v  'html/body/h2/a/@href' $localPath)"
        if [ -n "$newReqImgURL" ]
        then
            echo Now downloading "$newReqImgURL" >&2
            resolveDownloadImg "$newReqImgURL" $localPath
        else
            echo Not found proper image URL for "$reqImgURL" >&2
        fi
    else
        echo Downloading finish for "$reqImgURL" >&2
    fi
}

function gnomewallpaper() {
    image=$1
    # Set the wallpaper both unity anf gnome3
    if gsettings set org.gnome.desktop.background picture-uri "file://$image"; then
        #Logging
        echo "New wallpaper set successfully"
    fi
    # Set the view to zoom
    gsettings set org.gnome.desktop.background picture-options "zoom"
}

function displaywallpaper() {
    image=$1
    resolution=$(xwininfo -root | awk '{ if ($1 == "Width:" ) { w=$2 } else if ($1 == "Height:" ) { h=$2 } } END { print w "x" h }')
    display -resize $resolution -window root $image
}

gopt_options="hd:ivwe"

function process_arg() {

    warn=1
    error=1

    disable_file=~/.var/comm/disable/$pgm
    set -- $(getopt -n $pgm -o $gopt_options -- $@)
    while [ $# -gt 0 ]
    do
        case $1 in
            (-i) interactive=1;;
            (-v) verbose=1;;
            (-d) eval dayId=$2; shift;;
            # (-s)
            #     if [ -f $disable_file ] ; then
            #          notify $pgm is disabled;
            #          exit -1;
            #     else
            #         notify $pgm is enabled;
            #         exit 0;
            #     fi
            #     ;;
            # (-d)
            #     if [ -f $disable_file ] ; then
            #          notify $pgm is already disabled;
            #     else
            #      if mkdir -p $(dirname $disable_file); touch $disable_file ; then
            #          sync
            #          notify $pgm is disabled;
            #      fi
            #     fi
            #     exit;;
            # (-r)
            #     if [ -f $disable_file ] ; then
            #         sync
            #         rm -f $disable_file && notify $pgm is enabled;
            #     else
            #         notify $pgm is already enabled;
            #     fi
            #     exit;;
            (-w) warn="";;
            (-e) error="";;
            (-h) help;
                 exit;;
            (--) shift; break;;
            (-*) echo "$0: error - unrecognized option $1" 1>&2; help; exit 1;;
            (*)  break;;
        esac
        shift
    done
}



function error() {
    notify "$*"
    logger "$*"
}

function warn() {
    if [ $warn ] ; then
        notify "$*"
    fi
    logger "$*"
}

function verbose() {
    if [ $verbose ] ; then
        notify "$*"
    fi
    logger "$*"
}

function notify() {
    if [ -t 1 ] ; then
        # echo -e "${pgm}:" "$*" >&2
        print "${pgm}:" "$*" >&2
    else
        notify-send "${pgm}:" "$*"
    fi
}

function logger() {
    #creating prolem
    command logger -p local1.notice -t ${pgm} -i - $USER : "$*"
}

function help() {
    notify $pgm $gopt_options
}

pgm=$(basename $0)

main $@
