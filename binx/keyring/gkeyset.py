#!/usr/bin/pyhton

import gnomekeyring as gk


if gk.is_available():
    attrs = {'keyring': 'LOCAL:/keyrings/default.keyring'}
    gk.item_create_sync('login', gk.ITEM_GENERIC_SECRET, 'Unlock password for: default', attrs, 'asfsdgfdfgh', True)
else:
    print "Gnome Keyring is not avaialable."




# gk.list_keyring_names_sync()
# ['session', 'default', 'mozilla', 'login']
