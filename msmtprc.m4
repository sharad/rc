# -*- Mode: shell-script; indent-tabs-mode: nil -*-

include(defs)


# Set default values for all following accounts.

defaults
# tls on
# tls_trust_file ~/ssl/certs/sasken.cer
# tls_trust_file ~/ssl/certs/ca-certificates.crt
logfile ~/.logs/msmtp


[#] upcase(SETUP_msmtprc_account1)
account SETUP_msmtprc_account1
        # necessary for more than anything.
        timeout 2
	host SETUP_msmtprc_account1_host
	port 25
	from SETUP_msmtprc_account1_from
	auth off
        ntlmdomain SETUP_msmtprc_account1_ntlmdomain
	protocol smtp
	user SETUP_msmtprc_account1_user
        password SETUP_msmtprc_account1_password
        auto_from on

# working file checkout http://intranet/departments/IT/General%20Information/Everything%20Email/Configuring%20the%20Outlook%202003%20Client%20to%20use%20RPC%20over%20HTTPs.doc #
account SETUP_msmtprc_account1-tls :SETUP_msmtprc_account1
	auth on
        ntlmdomain SETUP_msmtprc_account1_ntlmdomain
	tls on
        tls_certcheck on
        tls_starttls on
        tls_key_file  /home/spratap/.ssl/private/sharad-key.pem
        tls_cert_file /home/spratap/.ssl/certs/sharad-cert.pem
	tls_trust_file /usr/share/ca-certificates/mozilla/Equifax_Secure_CA.crt
        tls_force_sslv3 on

account SETUP_msmtprc_account1-ssl  :SETUP_msmtprc_account1-tls
	auth gssapi

account SETUP_msmtprc_account1-ntlm :SETUP_msmtprc_account1-tls
	auth ntlm
        ntlmdomain SETUP_msmtprc_account1_ntlmdomain

account SETUP_msmtprc_account1-default :SETUP_msmtprc_account1-ntlm
        port 587
        tls_certcheck on
        # tls_trust_file ~/.ssl/certs/New_aruba-server.pem
        tls_starttls on
        tls_force_sslv3 on




[#] {{{ unquote(upcase(SETUP_msmtprc_account2))
account SETUP_msmtprc_account2
	host SETUP_msmtprc_account2_host
	port 25
	from SETUP_msmtprc_account2_from
	auth off
        ntlmdomain SETUP_msmtprc_account2_ntlmdomain
	protocol smtp
	user SETUP_msmtprc_account2_user
        password 'SETUP_msmtprc_account2_password'
	# tls off
	# tls_trust_file /etc/ssl/certs/ca-certificates.crt
        # auto_from on

account SETUP_msmtprc_account2_newip
	host SETUP_msmtprc_account2_host
	port 25
	from SETUP_msmtprc_account2_from
	auth off
        ntlmdomain SETUP_msmtprc_account2_ntlmdomain
	protocol smtp
	user SETUP_msmtprc_account2_user
	password 'SETUP_msmtprc_account2_password'

	# tls off
	# tls_trust_file /etc/ssl/certs/ca-certificates.crt
        # auto_from on


account SETUP_msmtprc_account2-ssl
	host SETUP_msmtprc_account2_host
	port 25
	from SETUP_msmtprc_account2_from
	auth ntlm
	protocol smtp
	user SETUP_msmtprc_account2_user
	password '**********'
	# tls off
	# tls_trust_file /etc/ssl/certs/ca-certificates.crt
        # auto_from on
# }}}

# Set a default account
account default : SETUP_msmtprc_account1-default

[#] account default : SETUP_msmtprc_account2-ssl

# Local Variables: **
# folded-file:f **
# mode:shell-script **
# comment-column:0 **
# comment-start: "# "  **
# comment-end:"#" **
# End: **

