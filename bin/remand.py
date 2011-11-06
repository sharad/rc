#!/usr/bin/python

from notmuch import Database

def get_thread():
    query = Database().create_query('thread:0000000000000c48')
    return query.search_threads().next()

def replies(msg):
    acc = []
    r = msg.get_replies()
    if r: #because we cant iterate on NoneType
        for m in r:
            acc.append(m)
            acc += replies(m)
    return acc

t=get_thread()

msgs = []
for m in t.get_toplevel_messages():
    msgs.append(m)
    msgs += replies(m)

print msgs




# On 28 May 2011 23:18, Patrick Totzke <[hidden email]> wrote:
#        if r: #because we cant iterate on NoneType
# I don't understand why, but this line sets r._msgs to None. So it crashes, because it has no message ids to look for.
# If you change it to
# if r is not None:
# ... then it works for me.
# Oh, I see, for your code, there is a implied call to __len__, and the __len__ function is completely broken for the reasons described in the documentation:
#       .. note:: As this iterates over the messages, we will not be able to=
#                iterate over them again! So this will fail::
#                  #THIS FAILS
#                  msgs = Database().create_query('').search_message()
#                  if len(msgs) > 0:              #this 'exhausts' msgs
#                      # next line raises NotmuchError(STATUS.NOT_INITIALIZED)!!!
#                      for msg in msgs: print msg
#
#                Most of the time, using the
#                :meth:`Query.count_messages` is therefore more
#                appropriate (and much faster). While not guaranteeing
#                that it will return the exact same number than len(),
#                in my tests it effectively always did so.




# See http://stackoverflow.com/questions/2182196/how-do-i-reply-to-an-email-using-the-python-imaplib-and-include-the-original-mess




# How do I reply to an email using the Python imaplib and include the original message?
# up vote 9 down vote favorite
# 4


# I'm currently using imaplib to fetch email messages from a server and process the contents and attachments.

# I'd like to reply to the messages with a status/error message and links to the resulting generated content on my site if they can be processed. This should include the original message but should drop any attachments (which will be large) and preferably replace them with just their filenames/sizes.

# Since I'm already walking the MIME message parts, I'm assuming what I need to do is build a new MIME message tree containing a copy of the original message and delete/replace the attachment nodes.

# Before I start down that path, I was hoping someone can give me some tips. Is there any kind of library function to do this? Any kind of standard behavior I should stick to?

# I currently know of/am using the imaplib, smtplib and email modules and but may have missed something obvious in there. This is running in Django too, so can use anything in django.core.email if that makes it easier.
# python django email mime imaplib
# link|edit|flag

# asked Feb 2 '10 at 6:10
# Tom
# 2,94741934

# 71% accept rate
# 	add comment

# start a bounty
# 1 Answer
# active oldest votes
# up vote 6 down vote accepted


# The original MIME tree structure of the incoming message is as follows (using email.iterators._structure(msg)):

# multipart/mixed
#     text/html                (message)
#     application/octet-stream (attachment 1)
#     application/octet-stream (attachment 2)

# Replying via GMail results in the following structure:

# multipart/alternative
#     text/plain
#     text/html

# I.e. they aren't being as smart as I thought, just discarding the attachments (good) and providing text and HTML versions that explicitly restructure the "quoted content."

# I'm beginning to think that's all I should do too, just reply with a simple message as after discarding the attachments there's not much point in keeping the original message.

# Still, might as well answer my original question since I've figured out how to now anyway.

# First, replace all the attachments in the original message with text/plain placeholders:

# import email

# original = email.message_from_string( ... )

# for part in original.walk():
#     if (part.get('Content-Disposition')
#         and part.get('Content-Disposition').startswith("attachment")):

#         part.set_type("text/plain")
#         part.set_payload("Attachment removed: %s (%s, %d bytes)"
#                          %(part.get_filename(),
#                            part.get_content_type(),
#                            len(part.get_payload(decode=True))))
#         del part["Content-Disposition"]
#         del part["Content-Transfer-Encoding"]

# Then create a reply message:

# from email.mime.multipart import MIMEMultipart
# from email.mime.text import MIMEText
# from email.mime.message import MIMEMessage

# new = MIMEMultipart("mixed")
# body = MIMEMultipart("alternative")
# body.attach( MIMEText("reply body text", "plain") )
# body.attach( MIMEText("<html>reply body text</html>", "html") )
# new.attach(body)

# new["Message-ID"] = email.utils.make_msgid()
# new["In-Reply-To"] = original["Message-ID"]
# new["References"] = original["Message-ID"]
# new["Subject"] = "Re: "+original["Subject"]
# new["To"] = original["Reply-To"] or original["From"]
# new["From"] = "me@mysite.com"

# Then attach the original MIME message object and send:

# new.attach( MIMEMessage(original) )

# s = smtplib.SMTP()
# s.sendmail("me@mysite.com", [new["To"]], new.as_string())
# s.quit()

# The resulting structure is:

# multipart/mixed
#     multipart/alternative
#         text/plain
#         text/html
#     message/rfc822
#         multipart/mixed
#             text/html
#             text/plain
#             text/plain

# Or it's a bit simpler using Django:

# from django.core.mail import EmailMultiAlternatives
# from email.mime.message import MIMEMessage

# new = EmailMultiAlternatives("Re: "+original["Subject"],
#                              "reply body text",
#                              "me@mysite.com", # from
#                              [original["Reply-To"] or original["From"]], # to
#                              headers = {'Reply-To': "me@mysite.com",
#                                         "In-Reply-To": original["Message-ID"],
#                                         "References": original["Message-ID"]})
# new.attach_alternative("<html>reply body text</html>", "text/html")
# new.attach( MIMEMessage(original) ) # attach original message
# new.send()

# The result ends (in GMail at least) showing the original message as "---- Forwarded message ----" which isn't quite what I was after, but the general idea works and I hope this answer helps someone trying to figure out how to fiddle with MIME messages.
# link|edit|flag

# answered Feb 3 '10 at 4:09
# Tom
# 2,94741934
# 	add comment
