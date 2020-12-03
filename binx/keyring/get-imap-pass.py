#!/usr/bin/env python3

import locale
import subprocess
import os
import sys
import getopt

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




def main(argv):
   inputfile = ''
   outputfile = ''

   try:
      opts, args = getopt.getopt(argv,"hpui:o:",["ifile=","ofile="])
   except getopt.GetoptError:
      print('get-imap-pass.py <server> <proto>')
      sys.exit(2)

   for opt, arg in opts:
      if opt == '-h':
         print('test.py -i <inputfile> -o <outputfile>')
         sys.exit()
      elif opt in ("-i", "--ifile"):
         inputfile = arg
      elif opt in ("-o", "--ofile"):
         outputfile = arg
      elif opt == '-p':
         print(get_password(args[0], "imap"))
      elif opt == '-u':
         print(get_username(args[0], "imap"))

   # print args[0]


if __name__ == "__main__":
   main(sys.argv[1:])
