include(defs)

# ProxyCommand /usr/bin/corkscrew ipwproxy.sasken.com 3128 %h %p

Host *
  VisualHostKey=yes
  # from: http://stackoverflow.com/a/4378985 for emacs tramp timeout.
  ServerAliveInterval 5

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
  User SETUP_h2_user

Host SETUP_h3_name
  User SETUP_h3_user

Host SETUP_h4_name
  User SETUP_h4_user

Host SETUP_h5_name
  User SETUP_h5_user

Host SETUP_h6_name
  IdentityFile SETUP_h6_IdentityFile




# host that changes their host-key very offen
# http://linuxcommando.blogspot.in/2008/10/how-to-disable-ssh-host-key-checking.html
Host SETUP_controllers
   StrictHostKeyChecking no
   UserKnownHostsFile=/dev/null
   ServerAliveInterval 240

