#!/usr/bin/python
#
# Uses older version of way2sms
# Fill in username and passwd variables below
#
#
import cookielib
import urllib2
from getpass import getpass
import sys
from urllib import urlencode
from getopt import getopt


username = None
passwd = None
message = None
number = None

# Fill in stuff
if username is None: username = raw_input("Enter USERNAME: ")
if passwd is None: passwd = getpass()
if message is None: message = raw_input("Enter Message: ")
if number is None: number = raw_input("Enter Mobile number: ")

#Logging into the SMS Site
url = 'http://ver3.way2sms.com/auth.cl'
data = 'username='+username+'&password='+passwd+'&Submit=Sign+in'

#Remember, Cookies are to be handled
cj = cookielib.CookieJar()
opener = urllib2.build_opener(urllib2.HTTPCookieProcessor(cj))



# To fool way2sms as if a Web browser is visiting the site
opener.addheaders = [('User-Agent','Mozilla/5.0 (X11; Linux i686; rv:2.0.1) Gecko/20100101 Firefox/4.0.1')]
opener.addheaders = [('Referer','http://ver3.way2sms.com/entry.jsp')]




try:
    usock = opener.open(url, data)
except IOError:
    print "Check your internet connection"
    sys.exit(1)


#urlencode performed.. Because it was done by the site as i checked through HTTP headers

while True:
    message = urlencode({'message':message})
    message = message[message.find("=")+1:]

#SMS sending
    send_sms_url = 'http://ver3.way2sms.com/FirstServletsms?custid='
    send_sms_data = 'custid=undefined&HiddenAction=instantsms&Action=455dasv556&login=&pass=&MobNo='+number+'&textArea='+message
    opener.addheaders = [('Referer', 'http://ver3.way2sms.com/jsp/InstantSMS.jsp?val=0')]

    try:
        sms_sent_page = opener.open(send_sms_url,send_sms_data)
    except IOError:
        print "Check your internet connection( while sending sms)"
        sys.exit(1)
        print "SMS sent!!!"

    message = raw_input("Enter Message: ")


