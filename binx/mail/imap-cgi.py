#!/usr/bin/env python

import sys
import smtplib
import time
import imaplib
import email
# Importing the 'cgi' module
import cgi

# -------------------------------------------------
#
# Utility to read email from Gmail Using Python
#
# ------------------------------------------------

def main(argv):


def page():
	print("Content-type: text/html\r\n\r\n")
	print("<html><body>")
	print("<h1> Hello Program! </h1>")
	# Using the inbuilt methods

	form = cgi.FieldStorage()
	if form.getvalue("name"):
	    name = form.getvalue("name")
	    print("<h1>Hello" +name+"! Thanks for using my script!</h1><br />")
	if form.getvalue("happy"):
	    print("<p> Yayy! I'm happy too! </p>")
	if form.getvalue("sad"):
	    print("<p> Oh no! Why are you sad? </p>")

	# Using HTML input and forms method
	print("<form method='post' action='hello2.py'>")
	print("<p>Name: <input type='text' name='name' /></p>")
	print("<input type='checkbox' name='happy' /> Happy")
	print("<input type='checkbox' name='sad' /> Sad")
	print("<input type='submit' value='Submit' />")
	print("</form")
	print("</body></html>")




def read_email_from_gmail():
    try:
        mail = imaplib.IMAP4_SSL(SMTP_SERVER)
        mail.login(FROM_EMAIL,FROM_PWD)
        mail.select('inbox')

        type, data = mail.search(None, 'ALL')
        mail_ids = data[0]

        id_list = mail_ids.split()
        first_email_id = int(id_list[0])
        latest_email_id = int(id_list[-1])


        for i in range(latest_email_id,first_email_id, -1):
            typ, data = mail.fetch(i, '(RFC822)' )

            for response_part in data:
                if isinstance(response_part, tuple):
                    msg = email.message_from_string(response_part[1])
                    email_subject = msg['subject']
                    email_from = msg['from']
                    print 'From : ' + email_from + '\n'
                    print 'Subject : ' + email_subject + '\n'

    except Exception, e:
        print str(e)



if __name__ == "__main__":
   main(sys.argv[1:])
