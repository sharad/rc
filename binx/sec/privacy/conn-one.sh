#!/bin/bash





nmcli con down "nlisp 5"
nmcli con up id "Wired connection";
nmcli -a dev wifi rescan
nmcli con up id Forti-corp-wpa2psk;




nmcli con down id "Wired connection";
nmcli con down id Forti-corp-wpa2psk;
nmcli -a dev wifi rescan
nmcli -a dev wifi connect nlisp
