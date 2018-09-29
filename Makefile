### Makefile --- make

## Author: sharad
## Version: $Id: Makefile,v 0.0 2011/09/27 19:06:23 s Exp $
## Keywords:
## X-URL:


host_macrodir = osetup/info/hosts/$(HOST)/m4.d
macroconfig      = m4
macrodir      = osetup/lib/m4.d # ../.osetup/info.d/common/m4.d

M4            = m4 -I /usr/share/doc/m4/examples -I $(macroconfig) -I $(macrodir) -I $(host_macrodir)

all: ../.ldaprc ../.msmtprc ../.ssh/config ../.offlineimaprc ../.notmuch-config ../.crontab

../.ldaprc: ldaprc.m4
	$(M4) ldaprc.m4 > ../.ldaprc

../.msmtprc: msmtprc.m4
	$(M4) msmtprc.m4 > ../.msmtprc

../.ssh/config: ssh/config.m4 | ../.ssh
	$(M4) ssh/config.m4 > ../.ssh/config

../.ssh:
	mkdir -p ../.ssh

../.offlineimaprc: offlineimaprc.m4
	$(M4) offlineimaprc.m4 > ../.offlineimaprc

../.notmuch-config: notmuch-config.m4
	$(M4) notmuch-config.m4 > ../.notmuch-config

../.crontab: crontab.m4
	$(M4) -Dhostname=$(shell hostname -s) crontab.m4 > ../.crontab
	crontab ../.crontab
