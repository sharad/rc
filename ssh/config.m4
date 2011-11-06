include(defs)

# ProxyCommand /usr/bin/corkscrew ipwproxy.sasken.com 3128 %h %p

Host github.com
  User git
  # Port 22
  Hostname github.com
  PreferredAuthentications publickey
  IdentityFile ~/.ssh/login-keys.d/github
  TCPKeepAlive yes
  IdentitiesOnly yes

Host SETUP_h1_name
  User SETUP_h1_user

Host SETUP_h2_name
  IdentityFile SETUP_h2_IdentityFile
  UserKnownHostsFile ~/.ssh/known_hosts

Host SETUP_h3_name
  User SETUP_h3_user


Host SETUP_h4_name
  User SETUP_h4_user

Host SETUP_h5_name
  User SETUP_h5_user

Host SETUP_h6_name
  IdentityFile SETUP_h6_IdentityFile



