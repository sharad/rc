include(defs)

[[general]]
metadata = ~/.offlineimap
accounts = esyscmd(echo -n $OFFLINEIMAPACCOUNT)
maxsyncaccounts = 1
ui = Curses.Blinkenlights, TTY.TTYUI,
     Noninteractive.Basic, Noninteractive.Quiet
ignore-readonly = no
pythonfile = ~/bin/lib-keyring



[[ui.Curses.Blinkenlights]]
statuschar = .



##################################################
# Accounts
##################################################
[[Account Gmail]]
localrepository = GmailLocal
remoterepository = GmailRemote
postsynchook = notmuch new

[[Repository GmailLocal]]

type = IMAP
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

[[Account Office]]
localrepository =  OfficeLocal
remoterepository = OfficeRemote
postsynchook = notmuch new

[[Repository OfficeLocal]]
type = IMAP
# sep = /
remotehost = localhost
remoteport = 143
remoteusereval = get_username("localhost")
remotepasseval = get_password("localhost")

[[Repository OfficeRemote]]
type = IMAP
remotehost = SETUP_offlinemaprc_office_imap_host
remoteport = 993
ssl = yes
remoteusereval = get_username("SETUP_offlinemaprc_office_imap_host")
remotepasseval = get_password("SETUP_offlinemaprc_office_imap_host")

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
                    re.sub('^', 'Office/', re.sub('^INBOX/', 'lists/', foldername))


