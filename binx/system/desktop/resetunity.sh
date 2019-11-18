#!/usr/bin/env bash

cd ~/.cache/ ; rm -rf  compizconfig-1/ evolution/ indicator indicator* unity* wallpaper/
cd ~/.config ;  rm -rf compiz-1/ gtk-* ibus/ pulse/ nautilus/ dconf/ evolution/ gnome-session/ libaccounts-glib/
rm -rf ~/.dbus/ ~/.local/ ~/.gconf/ 
