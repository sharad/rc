#!/usr/bin/env bash
############################################################### # # # # # # # # # # # #
# #
# Pogodynka 0.2.2.1 #
# #
# Azhag (azhag@bsd.miki.eu.org) #
# #
############################################################### # # # # # # # # # # # #
# #
# The script retrieves weather information from weather.yahoo.com for the city, then formats it and #
# Is displayed on the screen. The script can be used eg in conky'm, xosd, * message. #
# #
############################################################### # # # # # # # # # # # #
# #
# Required Applications: #
# W3m - textual web browser #
# #
############################################################### # # # # # # # # # # # #
# #
# Before using the script, you must set "path" and "code" variables. #
# #
# To find out your city code, go to http://weather.yahoo.com/ and search for your city. Code is #
# End of line with the weather of our city. #
# #
# Example codes: #
# Warsaw - PLXX0028 #
# Krakow - PLXX0012 #
# Gdañsk - PLXX0005 #
# Szczecin - PLXX0025 #
# #
# The information on how to display the script can be changed by having the appropriate lines in the "formatting information #
# You can also easily format your own result by using the variables available. # #
# #
############################################################### # # # # # # # # # # # #



# Https://github.com/topherh/Homestash/raw/master/.conky/scripts/pogodynka.sh

# Directory where the script is located
path=~/bin/

# City code
code=USWA0269

file=~/.conky/weather
# Check if the server is available
#if [`ping -c1 216.109.126.70 | Grep from | Wc -l` -eq 0]
 # Then
#echo "Service not available"
  #else
# Download information
 ## w3m -dump http://weather.yahoo.com/forecast/"$kod".html | Grep -A21 "Current" | Sed 's/DEG/°/g'> $ file
curl -s http://weather.yahooapis.com/forecastrss?p="$code" | grep -A1 "Current" | sed 's@<\([^<>][^<>]*\)>\([^<>]*\)</\1>@\2@g' | sed 's@<br />@@ig' > $file

# Fix the variables
state=`head-n3 $file | tail -n1`
temp=`tail -n1 $file | awk '{print $ 1}' `
tempo=`head -n6 $file | tail -n1`
cisn=`head -n8 $file | tail -n1`
wind=`head -n16 $file | tail -n1`
wilg=`head -n10 $file | tail -n1`
wsch=`head -n18 $file | tail -n1`
zach=`head -n20 $file | tail -n1`

if [ `cat "$path"/pogodynka | grep -x "# $ status" | wc -l` -eq 0 ]
then
  stanpl=$status
else
  stanpl=`cat '$path"/pogodynka | grep -xA1 "# $status" | tail -n1 | awk '{print $ 2, $ 3, $ 4, $ 5, $ 6, $ 7}'
fi

# Formatting the output information
# Available variables:
# $ State status description in English
# Stanpl state description in Polish
# $ Temp air temperature
# $ Tempo temperature can be felt
# $ Cisn atmospheric pressure
# $ Wind direction, wind force
# $ Wilg air humidity
# $ Wsch the sunrise
# $ E the hour of the sun

#echo $ status
#echo $ stanpl
echo $ tempo
#echo Pressure $ cisn hPa
#echo $ wind
#echo Moisture: $ wilg
#echo Sunrise: $ ws
#echo Sunset: $ zach
#echo $ stanpl, $ temp C

#fi

############################################################### # # # # # # # # # # # # # #
#
# Interpretation of weather conditions.
# If you notice the weather, which is not yet on the list, let me know by email at the top. Thank you in advance.
#
# Sunny
# Sunny
# Clear
# Transparency
# Fair
# Sunny
# Sunny / Windy
# Sunny / Wind
# Clear / Windy
# Transit / Wind
# Fair / Windy
# Transit / Wind
# Windy
# Wind
#
# Partly Cloudy
# Partially cloudy
# Partly Cloudy and Windy
# Partially cloudy / Wind
# Partly Sunny
# Partially sunny
# Mostly Clear
# Pass. View
# Partly Sunny / Windy
# Partly sunny / Wind
# Mostly Clear / Windy
# Pass. Clear / Wind
# Mostly Sunny
# Pass. Sunny
# Mostly Sunny / Windy
# Pass. Sunshine / wind
# Scattered Clouds
# Rare clusters
#
# Cloudy
# Cloudy
# Overcast
# Ca³k. Cloudy
# Cloudy / Windy
# Overcast / Wind
# Overcast / Windy
# Ca³k. Cloudy / Wind
# Mostly Cloudy / Windy
# Pass. Cloudy / Wind
# Mostly Cloudy
# Pass. cloudy
# Am Clouds / Pm Sun
# Sunny morning / Sunny afternoon
#
# Light Drizzle
# Light drizzle
# Drizzle
# Thick
# Light Rain
# Light rain
# Rain
# Rain
# Heavy Rain
# Heavy rain
# Light Rain / Fog
# Light rain / Mist
# Rain / Fog
# Rain / Fog
# Light Drizzle / Windy
# Light drizzle / Wind
# Drizzle / Windy
# Hache / Wind
# Light Rain / Windy
# Light Rain / Wind
# Rain / Windy
# Desz




exit



#!/bin/bash
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#															#
# Pogodynka 0.2.2.1													#
#															#
# azhag (azhag@bsd.miki.eu.org)												#
#															#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#															#
# Skrypt pobiera informacje o stanie pogody ze strony weather.yahoo.com dla danego miasta, nastêpnie formatuje je i	#
# wy¶wietla na ekranie. Skrypt mo¿e byæ wykorzystany np. w conky'm, xosd, *message.					#
#															#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#															#
# Wymagane aplikacje:													#
# w3m - tekstowa przegl±darka www											#
#															#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#															#
# Przed u¿yciem skryptu nale¿y ustaliæ zmienne "sciezka" oraz "kod".							#
#															#
# Aby ustaliæ kod swojego miasta wejd¿ na stronê http://weather.yahoo.com/ i wyszukaj tam swoje miasto. Kodem jest 	#
# koñcówka linka z pogod± naszego miasta.										#
#															#
# Przyk³adowe kody:													#
# Warszawa - PLXX0028													#
# Kraków - PLXX0012													#
# Gdañsk - PLXX0005													#
# Szczecin - PLXX0025													#
#															#
# Informacjê jak± wy¶wietla skrypt mo¿na zmieniæ haszuj±c odpowiednie linijki w sekcji "formatowanie informacji		#
# wyj¶ciowej". Mo¿na równie¿ w ³atwy sposób sformatowaæ w³asny wynik u¿ywaj±c dostepnych zmiennych.			#
#															#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# https://github.com/topherh/Homestash/raw/master/.conky/scripts/pogodynka.sh

# Katalog, w którym znajduje siê skrypt
sciezka=~/.conky/scripts

# Kod miasta
kod=USWA0269

plik=~/.conky/weather
# sprawdzenie czy serwer jest dostêpny
#if [ `ping -c1 216.109.126.70 | grep from | wc -l` -eq 0 ]
 # then
	#echo "Serwis niedostêpny"
  #else
	# pobieranie informacji
 	## w3m -dump http://weather.yahoo.com/forecast/"$kod".html | grep -A21 "Current" | sed 's/DEG/°/g' > $plik
    curl -s http://weather.yahooapis.com/forecastrss?p="$kod" | grep -A1 "Current" | sed 's@<\([^<>][^<>]*\)>\([^<>]*\)</\1>@\2@g' | sed 's@<br />@@ig' > $plik

	# ustalenie warto¶ci zmiennych
	stan=`head -n3 $plik | tail -n1`
	temp=`tail -n1 $plik | awk '{print $1}'`
	tempo=`head -n6 $plik | tail -n1`
	cisn=`head -n8 $plik | tail -n1`
	wiatr=`head -n16 $plik | tail -n1`
	wilg=`head -n10 $plik | tail -n1`
	wsch=`head -n18 $plik | tail -n1`
	zach=`head -n20 $plik | tail -n1`
	if [ `cat "$sciezka"/pogodynka.sh | grep -x "# $stan" | wc -l` -eq 0 ]
	  then
		stanpl=$stan
	  else
		stanpl=`cat "$sciezka"/pogodynka.sh | grep -xA1 "# $stan" | tail -n1 | awk '{print $2,$3,$4,$5,$6,$7}'`
	fi

	# formatowanie informacji wyj¶ciowej
	# dostêpne zmienne:
	# $stan		opis stanu po angielsku
	# $stanpl	opis stanu po polsku
	# $temp		temperatura powietrza
	# $tempo	temperatura odczuwalna
	# $cisn		ci¶nienie atmosferyczne
	# $wiatr	kierunek, si³a wiatru
	# $wilg		wilgotno¶æ powietrza
	# $wsch		godzina wschodu s³oñca
	# $zach		godzina zachodu s³oñca

	#echo $stan
	#echo $stanpl
	echo $tempo
	#echo Cisnienie $cisn hPa
	#echo $wiatr
	#echo Wilgotno¶æ: $wilg
	#echo Wschód S³oñca: $wsch
	#echo Zachód S³oñca: $zach
	#echo $stanpl, $temp C

#fi

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# T³umaczenia stanów pogody.
# Je¿eli zauwa¿ysz pogodê, której nie ma jeszcze na liscie daj mi znaæ na maila podanego na górze. Z góry dziêkujê.
#
# Sunny
# S³onecznie
# Clear
# Przejrzy¶cie
# Fair
# Pogodnie
# Sunny/Windy
# S³onecznie/Wiatr
# Clear/Windy
# Przejrzy¶cie/Wiatr
# Fair/Windy
# Przejrzy¶cie/Wiatr
# Windy
# Wiatr
#
# Partly Cloudy
# Czê¶ciowo pochmurnie
# Partly Cloudy and Windy
# Czê¶ciowo pochmurnie/Wiatr
# Partly Sunny
# Czê¶ciowo s³onecznie
# Mostly Clear
# Przew. przejrzy¶cie
# Partly Sunny/Windy
# Czê¶ciowo s³onecznie/Wiatr
# Mostly Clear/Windy
# Przew. przejrzy¶cie/Wiatr
# Mostly Sunny
# Przew. p³onecznie
# Mostly Sunny/Windy
# Przew. s³onecznie/Wiatr
# Scattered Clouds
# Rzadkie ob³oki
#
# Cloudy
# Pochmurnie
# Overcast
# Ca³k. zachmurzenie
# Cloudy/Windy
# Pochmurnie/Wiatr
# Overcast/Windy
# Ca³k. zachmurzenie/Wiatr
# Mostly Cloudy/Windy
# Przew. pochmurnie/Wiatr
# Mostly Cloudy
# Przew. pochmurnie
# Am Clouds / Pm Sun
# Ranek pochmurny/S³oneczne popo³udnie
#
# Light Drizzle
# Lekka m¿awka
# Drizzle
# M¿awka
# Light Rain
# Lekki deszcz
# Rain
# Deszcz
# Heavy Rain
# Ulewa
# Light Rain/Fog
# Lekki deszcz/Mg³a
# Rain/Fog
# Deszcz/Mg³a
# Light Drizzle/Windy
# Lekka m¿awka/Wiatr
# Drizzle/Windy
# M¿awka/Wiatr
# Light Rain/Windy
# Lekki deszcz/Wiatr
# Rain/Windy
# Deszcz/Wiatr
# Rain / Wind
# Deszcz/Wiatr
# Heavy Rain/Windy
# Ulewa/Wiatr
# AM Light Rain
# Ranny lekki deszcz
# PM Light Rain
# Popo³udniowy lekki deszcz
# Pm Light Rain
# Popo³udniowy lekki deszcz
# AM Light Rain/Windy
# Ranny lekki deszcz/Wiatr
# PM Light Rain/Windy
# Popo³udniowy lekki deszcz/Wiatr
#
# Rain Shower
# Przelotny deszcz
# Shower
# Przelotna ulewa
# Showers
# Przelotna ulewa
# Heavy Rain Shower
# Mocna ulewa
# Heavy Rain Shower/Windy
# Mocna ulewa/Wiatr
# Light Rain Shower
# Lekka ulewa
# AM Shower
# Poranna ulewa
# AM Showers
# Poranna ulewa
# Am Showers
# Poranna ulewa
# AM Showers / Wind
# Poranna ulewa/Wiatr
# PM Shower
# Popo³udniowa ulewa
# PM Showers / Wind
# Popo³udniowe ulewy/Wiatr
# Few Showers / Wind
# Przelotne deszcze/Wiatr
# Showers / Wind
# Deszcze/Wiatr
# PM Showers
# Popo³udniowe ulewy
# Pm Showers
# Popo³udniowe ulewy
# Scattered Shower
# Rozleg³a ulewa
# Scattered Showers
# Rozleg³e ulewy
# Scatter Showers
# Rozleg³e ulewy
# Rain Shower/Windy
# Przelotny deszcz/Wiatr
# Shower/Windy
# Przelotna ulewa/Wiatr
# Light Rain Shower/Windy
# Lekka ulewa/Wiatr
# AM Shower/Windy
# Poranna ulewa/Wiatr
# PM Shower/Windy
# Popo³udniowa ulewa/Wiatr
# Scattered Shower/Windy
# Rozleg³a ulewa/Wiatr
# Scatter Showers / Wind
# Rozleg³e ulewy/Wiatr
# Few Showers
# Mo¿liwe ulewy
# Few Showers/Windy
# Mo¿liwe ulewy/Wiatr
# Showers in the Vicinity
# Pobliskie ulewy
#
# Light Snow
# Lekki ¶nieg
# Snow
# ¦nieg
# Snow / Wind
# ¦nieg/Wiatr
# Heavy Snow
# Mocny ¶nieg
# Light Snow Pellets
# Lekki grad ¶nie¿ny
# Snow Pellets
# Grad ¶nie¿ny
# Light Ice Pellets
# Lekki grad lodowy
# Ice Pellets
# Grad lodowy
# Wintery Weather
# Zimowa pogoda
# Light Freezing Rain
# Lekki zamarzaj±y deszcz
# Freezing Rain
# Zamarzaj±cy deszcz
# Flurries/Windy
# Zamiecie/Wiatr
# Light Flurries/Windy
# Lekkie zamiecie/Wiatr
# Light Snow/Windy
# Lekki ¶nieg/Wiatr
# Light Snow / Wind
# Lekki ¶nieg/Wiatr
# Snow/Windy
# ¦nieg/Wiatr
# Heavy Snow/Windy
# Mocny ¶nieg/Wiatr
# Light Snow Pellets/Windy
# Lekki grad ¶nie¿ny/Wiatr
# Snow Pellets/Windy
# Grad ¶nie¿ny/Wiatr
# Light Ice Pellets/Windy
# Lekki grad lodowy/Wiatr
# Ice Pellets/Windy
# Grad lodowy/Wiatr
# Light Freezing Rain/Windy
# Lekki zamarzaj±cy deszcz/Wiatr
# Freezing Rain/Windy
# Zamarzaj±cy deszcz/Wiatr
# Wintery Mix
# Miks zimowy
# Light Snow Grains
# Lekkie granulki ¶niegu
# Snow Grains
# Granulki ¶niegu
# Rain/Snow
# ¦nieg z deszczem
# Rain / Snow Showers
# Deszcz ze ¶niegiem
# Rain / Snow
# Deszcz ze ¶niegiem
# Rain / Thunder
# Deszcz / Burza
# Rain/Show/Windy
# ¦nieg z deszczem/Wiatr
# Rain / Snow / Wind
# ¦nieg z deszczem/Wiatr
# Light Rain/Freezing Rain
# Lekki deszcz/Zamarzaj±cy deszcz
# Rain/Freezing Rain
# Deszcz/Zamarzaj±cy deszcz
# Light Rain/Freezing Rain/Windy
# Lekki deszcz/Zamarzaj±cy Deszcz/Wiatr
# Rain/Freezing Rain/Windy
# Deszcz/Zamarzaj±cy deszcz/Wiatr
# AM Snow
# Poranny ¶nieg
# PM Snow
# Popo³udniowy ¶nieg
# AM Light Snow
# Poranny lekki ¶nieg
# PM Light Snow
# Popo³udniowy lekki ¶nieg
# Ice Crystals
# Kryszta³ki lodu
# Ice Crystals/Windy
# Kryszta³ki lodu/Wiatr
#
# Snow Showers
# Burze ¶nie¿ne
# Snow Shower
# Burza ¶nie¿na
# Heavy Snow Shower
# Mocna burza ¶nie¿na
# Heavy Snow Shower/Windy
# Mocna burza ¶nie¿na/Wiatr
# PM Snow Showers
# Popo³udniowe burze ¶nie¿ne
# AM Snow Showers
# Poranne burze ¶nie¿ne
# Rain/Snow Showers
# Deszcz/Burze ¶nie¿ne
# Snow Showers/Windy
# Burze ¶nie¿ne/Wiatr
# PM Snow Showers/Windy
# Popo³udniowe burze ¶nie¿ne/Wiatr
# AM Snow Showers/Windy
# Poranne burze ¶nie¿ne/Wiatr
# Rain/Snow Showers/Windy
# Deszcz/Burze ¶nie¿ne/Wiatr
# Light Snow Showers
# Lekkie burze ¶nie¿ne
# Light Snow Shower
# Lekka burza ¶nie¿na
# Light Snow Showers/Windy
# Lekkie burze ¶nie¿ne/Wiatr
# Flurries
# Zamiecie
# Light Flurries
# Lekkie zamiecie
# Scattered Flurries
# Rozleg³e zamiecie
# Few Flurries
# Mo¿liwe zamiecie
# Few Flurries/Windy
# Mo¿liwe zamiecie/Wiatr
# Scattered Snow Showers
# Rozleg³e burze ¶nie¿ne
# Scattered Snow Showers/Windy
# Rozleg³e burze ¶nie¿ne/Wiatr
# Few Snow Showers
# Mo¿liwe burze ¶nie¿ne
# Few Snow Showers/Windy
# Mo¿liwe burze ¶nie¿ne/Wiatr
# Freezing Drizzle
# Marzn±ca m¿awka
# Light Freezing Drizzle
# Lekka marzn±ca m¿awka
# Freezing Drizzle/Windy
# Marzn±ca m¿awka/Wiatr
# Light Freezing Drizzle/Windy
# Lekka marzn±ca m¿awka/Wiatr
# Drifting Snow
# Zawieja ¶nie¿na
#
# Thunderstorms
# Burze
# T-storms
# Burze
# T-Storms
# Burze
# T-Storm
# Burza
# Scattered Thunderstorms
# Rozleg³e burze
# Scattered T-Storms
# Rozleg³e burze
# Thunderstorms/Windy
# Burze/Wiatr
# Scattered Thunderstorms/Windy
# Rozleg³e burze/Wiatr
# Rain/Thunder
# Deszcz/Grzmoty
# Light Thunderstorms/Rain
# Lekkie burze/Deszcz
# Thunderstorms/Rain
# Burze/Deszcz
# Light Rain with Thunder
# Lekki deszcz z grzmotami
# Rain with Thunder
# Deszcz z grzmotami
# Thunder in the Vicinity
# Pobliskie burze
#
# Fog
# Mg³a
# Haze
# Lekka mg³a
# Mist
# Lekkie zamglenie
# Fog/Windy
# Mg³a/Wiatr
# Haze/Windy
# Lekka Mg³a/Wiatr
# Mist/Windy
# Lekkie zamglenie/Wiatr
# Partial Fog
# Czê¶ciowa mg³a
# Smoke
# Gêsta mg³a
# Foggy
# Mglisto
# AM Fog/PM Sun
# Ranna mg³a/Popo³udniowe s³oñce
# Shallow Fog
# P³ytka mg³a
#
# Blowing Dust
# Zawieja py³owa
# Blowing Sand
# Zawieja piaskowa
# Duststorm
# Burza piaskowa
# Wind
# Wiatr
# Widespread Dust/Windy
# Rozleg³e zamiecie/Wiatr
# Widespread Dust
# Rozleg³e zamiecie
# Low Drifting Sand
# Zawieja piaskowa
#
# Data Not Available
# Dane niedostêpne
# N/A
# N/D
# N/a
# N/d
