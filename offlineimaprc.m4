# -*- mode: ini; -*-

include(defs.m4)

[[general]]
metadata = ~/.offlineimap
accounts = esyscmd(echo -n $OFFLINEIMAPACCOUNT)
maxsyncaccounts = 1
# ui = blinkenlights, ttyui, basic, quiet, machineui
ui = blinkenlights
ignore-readonly = no
pythonfile = ~/bin/lib-keyring



# [[ui.Curses.Blinkenlights]]
# statuschar = .

[[ui.blinkenlights]]
statuschar = .



##################################################
# Accounts
##################################################
[[Account Gmail]]
localrepository = GmailLocal
remoterepository = GmailRemote
postsynchook = ~/.config/offlineimap/postsync.sh
maxage = 10

[[Repository GmailLocal]]

type = IMAP
ssl = no
remotehost = localhost
remoteport = 143
remoteusereval = get_username("localhost")
remotepasseval = get_password("localhost")

[[Repository GmailRemote]]

type = Gmail
ssl = yes
remoteusereval = get_username("imap.gmail.com")
remotepasseval = get_password("imap.gmail.com")
maxconnections = 1
holdconnectionopen = no
nametrans = lambda foldername: re.sub('^', 'Gmail/', re.sub('^\[[Gmail\]]/Sent Mail$', 'sent-mail', foldername))
folderfilter = lambda foldername: foldername in  [['INBOX', 'official', '[Gmail]/Sent Mail']]
realdelete = no
## https://bugs.launchpad.net/ubuntu/+source/offlineimap/+bug/1015692
sslcacertfile = /etc/ssl/certs/ca-certificates.crt

[[Account Office-Meru-Plain]]
localrepository =  Office-Meru-Local
remoterepository = Office-Meru-RemotePlain
postsynchook = ~/.config/offlineimap/postsync.sh

[[Account Office-Meru]]
localrepository =  Office-Meru-Local
remoterepository = Office-Meru-RemoteSSL
postsynchook = ~/.config/offlineimap/postsync.sh


[[Repository Office-Meru-Local]]
type = IMAP
# sep = /
ssl = no
remotehost = localhost
remoteport = 143
remoteusereval = get_username("localhost")
remotepasseval = get_password("localhost")

[[Repository Office-Meru-RemotePlain]]
type = IMAP
remotehost = SETUP_offlinemaprc_office_imap_host
remoteport = 143
ssl = no
remoteusereval = get_username( "SETUP_offlinemaprc_office_keyring_host" )
remotepasseval = get_password( "SETUP_offlinemaprc_office_keyring_host" )

# reference = Mail
realdelete = no
# folderfilter = lambda foldername: foldername in ['List/DATA/ZZ']
# folderfilter = lambda foldername: foldername in ['INBOX', 'Sent Items', 'Deleted Items', 'Calendar', 'INBOX/DATA/junk']
# folderfilter = lambda foldername: re.search('^INBOX', foldername)

# folderfilter = lambda foldername: re.search('^INBOX$|^INBOX/info|^(Sent Items|Deleted Items|Calendar)$', foldername) and (not re.search('(techtalk)', foldername))

folderfilter = lambda foldername: re.search('^INBOX$|^INBOX/info|^(Sent Items|Deleted Items)$', foldername) and (not re.search('(techtalk)', foldername))


maxconnections = 4
# holdconnectionopen = no
nametrans = lambda foldername: \
                    re.sub('^', 'Office/Meru/', re.sub('^INBOX/', 'lists/', foldername))



[[Repository Office-Meru-RemoteSSL]]
type = IMAP
remotehost = SETUP_offlinemaprc_office_imap_host
remoteport = 443
ssl = yes
remoteusereval = get_username( "SETUP_offlinemaprc_office_keyring_host" )
remotepasseval = get_password( "SETUP_offlinemaprc_office_keyring_host" )

# reference = Mail
realdelete = no
# folderfilter = lambda foldername: foldername in ['List/DATA/ZZ']
# folderfilter = lambda foldername: foldername in ['INBOX', 'Sent Items', 'Deleted Items', 'Calendar', 'INBOX/DATA/junk']
# folderfilter = lambda foldername: re.search('^INBOX', foldername)

# folderfilter = lambda foldername: re.search('^INBOX$|^INBOX/info|^(Sent Items|Deleted Items|Calendar)$', foldername) and (not re.search('(techtalk)', foldername))

folderfilter = lambda foldername: re.search('^INBOX$|^INBOX/info|^(Sent Items|Deleted Items)$', foldername) and (not re.search('(techtalk)', foldername))


maxconnections = 4
# holdconnectionopen = no
nametrans = lambda foldername: \
                    re.sub('^', 'Office/Meru/', re.sub('^INBOX/', 'lists/', foldername))



#### Another ####


[[Account Office-Fortinet-Plain]]
localrepository =  Office-Fortinet-Local
remoterepository = Office-Fortinet-RemotePlain
postsynchook = ~/.config/offlineimap/postsync.sh

[[Account Office-Fortinet]]
localrepository =  Office-Fortinet-Local
remoterepository = Office-Fortinet-RemoteSSL
postsynchook = ~/.config/offlineimap/postsync.sh


[[Repository Office-Fortinet-Local]]
type = IMAP
# sep = /
ssl = no
remotehost = localhost
remoteport = 143
remoteusereval = get_username("localhost")
remotepasseval = get_password("localhost")

[[Repository Office-Fortinet-RemotePlain]]
type = IMAP
remotehost = SETUP_offlinemaprc_office_imap_host
remoteport = 143
ssl = no
remoteusereval = get_username( "SETUP_offlinemaprc_office_keyring_host" )
remotepasseval = get_password( "SETUP_offlinemaprc_office_keyring_host" )

# reference = Mail
realdelete = no
# folderfilter = lambda foldername: foldername in ['List/DATA/ZZ']
# folderfilter = lambda foldername: foldername in ['INBOX', 'Sent Items', 'Deleted Items', 'Calendar', 'INBOX/DATA/junk']
# folderfilter = lambda foldername: re.search('^INBOX', foldername)

# folderfilter = lambda foldername: re.search('^INBOX$|^INBOX/info|^(Sent Items|Deleted Items|Calendar)$', foldername) and (not re.search('(techtalk)', foldername))

folderfilter = lambda foldername: re.search('^INBOX$|^INBOX/info|^(Sent Items|Deleted Items)$', foldername) and (not re.search('(techtalk)', foldername))


maxconnections = 4
# holdconnectionopen = no
nametrans = lambda foldername: \
                    re.sub('^', 'Office/Fortinet/', re.sub('^INBOX/', 'lists/', foldername))



[[Repository Office-Fortinet-RemoteSSL]]
type = IMAP
remotehost = SETUP_offlinemaprc_office_imap_host
remoteport = 443
ssl = yes
remoteusereval = get_username( "SETUP_offlinemaprc_office_keyring_host" )
remotepasseval = get_password( "SETUP_offlinemaprc_office_keyring_host" )

# reference = Mail
realdelete = no
# folderfilter = lambda foldername: foldername in ['List/DATA/ZZ']
# folderfilter = lambda foldername: foldername in ['INBOX', 'Sent Items', 'Deleted Items', 'Calendar', 'INBOX/DATA/junk']
# folderfilter = lambda foldername: re.search('^INBOX', foldername)

# folderfilter = lambda foldername: re.search('^INBOX$|^INBOX/info|^(Sent Items|Deleted Items|Calendar)$', foldername) and (not re.search('(techtalk)', foldername))

folderfilter = lambda foldername: re.search('^INBOX$|^INBOX/info|^(Sent Items|Deleted Items)$', foldername) and (not re.search('(techtalk)', foldername))


maxconnections = 4
# holdconnectionopen = no
nametrans = lambda foldername: \
                    re.sub('^', 'Office/Fortinet/', re.sub('^INBOX/', 'lists/', foldername))
