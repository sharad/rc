#!/usr/bin/python

import sys
import gtk
import gnomekeyring as gkey

class Keyring(object):
    def __init__(self, name, server, protocol):
        self._name = name
        self._server = server
        self._protocol = protocol
        self._keyring = gkey.get_default_keyring_sync()

    def has_credentials(self):
        try:
            attrs = {"server": self._server, "protocol": self._protocol}
            items = gkey.find_items_sync(gkey.ITEM_NETWORK_PASSWORD, attrs)
            return len(items) > 0
        except gkey.DeniedError:
            return False

    def get_credentials(self):
        attrs = {"server": self._server, "protocol": self._protocol}
        items = gkey.find_items_sync(gkey.ITEM_NETWORK_PASSWORD, attrs)
        return (items[0].attributes["user"], items[0].secret)

    def set_credentials(self, (user, pw)):
        attrs = {
                "user": user,
                "server": self._server,
                "protocol": self._protocol,
            }
        gkey.item_create_sync(gkey.get_default_keyring_sync(),
                gkey.ITEM_NETWORK_PASSWORD, self._name, attrs, pw, True)

def get_username(server):
    keyring = Keyring("offlineimap", server, "imap")
    (username, password) = keyring.get_credentials()
    return username

def get_password(server):
    keyring = Keyring("offlineimap", server, "imap")
    (username, password) = keyring.get_credentials()
    return password





## ##### From: http://burtonini.com/blog/computers/offlineimap-2008-11-04-20-00
## if 0:
##     import gobject, gnomekeyring
##
##     # The keyring needs to know the application name
##     if gobject.get_application_name() is None:
##       gobject.set_application_name("offlineimap")
##
##     def keyring(user, host):
##       keys = gnomekeyring.find_network_password_sync(user=user, server=host, protocol="imap")
##       # First one will do nicely thanks
##       return keys[0]["password"]
##     ...
##     remotepasseval = keyring("rburton", "imapmail.intel.com")
##
##
## if 0:
##     import dbus, os
##     if not os.getenv("DISPLAY"):
##       # Get the ConsoleKit manager
##       bus = dbus.SystemBus()
##       manager_obj = bus.get_object('org.freedesktop.ConsoleKit', '/org/freedesktop/ConsoleKit/Manager')
##       manager = dbus.Interface(manager_obj, 'org.freedesktop.ConsoleKit.Manager')
##
##       # For each of my sessions..
##       for ssid in manager.GetSessionsForUnixUser(os.getuid()):
##         obj = bus.get_object('org.freedesktop.ConsoleKit', ssid)
##         session = dbus.Interface(obj, 'org.freedesktop.ConsoleKit.Session')
##         # Get the X11 display name
##         dpy = session.GetX11Display()
##         if dpy:
##           # If we have a display, set the environment variable
##           os.putenv("DISPLAY", dpy);
##           break
