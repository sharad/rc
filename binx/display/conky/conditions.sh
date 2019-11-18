#!/usr/bin/env bash
# https://github.com/topherh/Homestash/raw/master/.conky/scripts/conditions.sh

#Czyta dane z pliku
cnd=$(cat ~/.conky/weather)

#Ustawia czcionkę obrazkową, odpowiadającą aktualnej pogoda.  
if echo "$cnd" | grep -E -i -q 'partly cloudy'; then
	echo 'c'
elif echo "$cnd" | grep -E -i -q 'fair|sunny'; then
	echo 'A'
elif echo "$cnd" | grep -E -i -q 'cloudy'; then
	echo 'e'
elif echo "$cnd" | grep -E -i -q 'storm|thunder'; then
	echo 'i'
elif echo "$cnd" | grep -E -i -q 'snow'; then
	echo 'k'
elif echo "$cnd" | grep -E -i -q 'rain'; then
	echo 'h'
elif echo "$cnd" | grep -E -i -q 'shower'; then
	echo 'g'
fi


