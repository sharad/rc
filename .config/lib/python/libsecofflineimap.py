

# executes gnome-keyring-query get passwd
# and returns the output
# https://stackoverflow.com/questions/39447238/get-only-stderr-output-from-subprocess-check-output
# https://stackoverflow.com/questions/20624749/add-conditions-in-code-depending-on-python-version

import locale
import subprocess
import os
import sys

encoding = locale.getdefaultlocale()[1]

def get_password(server, protocol):
    proc = subprocess.Popen(["secret-tool", "lookup", "server", server,  "protocol", protocol], stdout=subprocess.PIPE)
    (out, err) = proc.communicate()
    return out.decode(encoding).strip()

def get_username(server, protocol):
    if (sys.version_info[:3] < (3, 0)):
        with open(os.devnull, 'wb') as devnull:
            proc = subprocess.Popen(["secret-tool", "search", "server", server,  "protocol", protocol],
                                    stdout=devnull,
                                    stderr=subprocess.PIPE)
            (out, err) = proc.communicate()
            array = err.decode(encoding).strip().replace(" = ", ",").replace("\n", ",").split(",")
            kvs = {array[i]: array[i + 1] for i in range(0, len(array), 2)}
            return kvs["attribute.user"]
    else:
        err = subprocess.run(["secret-tool", "search", "server", server,  "protocol", protocol], stdout=subprocess.DEVNULL, stderr=subprocess.PIPE).stderr
        array = err.decode(encoding).strip().replace(" = ", ",").replace("\n", ",").split(",")
        kvs = {array[i]: array[i + 1] for i in range(0, len(array), 2)}
        return kvs["attribute.user"]
