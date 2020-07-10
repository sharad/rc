

# executes gnome-keyring-query get passwd
# and returns the output

import locale
from subprocess import Popen, PIPE, DEVNULL
import subprocess

encoding = locale.getdefaultlocale()[1]

def get_password(server, protocol):
    (out, err) = Popen(["secret-tool", "lookup", "server", server,  "protocol", protocol], stdout=PIPE).communicate()
    return out.decode(encoding).strip()

def get_username(server, protocol):
    err = subprocess.run(["secret-tool", "search", "server", server,  "protocol", protocol], stdout=subprocess.DEVNULL, stderr=subprocess.PIPE).stderr
    array = err.decode(encoding).strip().replace(" = ", ",").replace("\n", ",").split(",")
    kvs = {array[i]: array[i + 1] for i in range(0, len(array), 2)}
    return kvs["attribute.user"]



