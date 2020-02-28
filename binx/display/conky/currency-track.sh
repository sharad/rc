#!/usr/bin/env bash

currency1=$1
currency2=$2
# meta='ei%3DOIG6WYDHEti4ugS5u4S4DQ'
proto="http"
# domain="www.google.com"
domain="finance.google.com"
path="finance/converter"

if which xmlstarlet >/dev/null 2>&1
then
    XMLCMD=xmlstarlet
elif which xml >/dev/null 2>&1
then
     XMLCMD=xml
fi

# from: https://github.com/alseambusher/conky-finance/blob/master/install
# curl -s $proto'://'$domain/$path'?a=1&from='"$currency1"'&to='"$currency2"'&meta='"$meta"
curl -s $proto'://'$domain/$path'?a=1&from='"$currency1"'&to='"$currency2" |
    tidy -q -numeric -asxhtml --show-warnings no  -f /dev/null |
    $XMLCMD sel -N h='http://www.w3.org/1999/xhtml' -t -v "/h:html/h:body//h:div[@id]"
echo
