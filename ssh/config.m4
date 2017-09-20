include(defs)


# ProxyCommand /usr/bin/corkscrew ipwproxy.sasken.com 3128 %h %p

Host *
  VisualHostKey=yes
  # from: http://stackoverflow.com/a/4378985 for emacs tramp timeout.
  ServerAliveInterval 60
  ControlMaster auto
  ControlPath   ~/.ssh/tmp/%h_%p_%r
  ControlPersist 4h

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



# ProxyCommand /usr/bin/corkscrew ipwproxy.sasken.com 3128 %h %p


##begin via1 http://askubuntu.com/questions/311447/how-do-i-ssh-to-machine-a-via-b-in-one-command
# Machine B definition (the broker)
Host qa-private-pontiac
  # Change this IP address to the address of the broker
  Hostname qa-private-pontiac
  # Change this default user accordingly # (`user@unibroker` can overwrite it)
  User root
  IdentityFile ~/.ssh/login-keys.d/work

##begin via2 http://askubuntu.com/questions/311447/how-do-i-ssh-to-machine-a-via-b-in-one-command
Host qa-private-pontiac
  ControlMaster auto
  User     root
  hostname 10.35.31.6
  port 22
  IdentityFile ~/.ssh/login-keys.d/work

# Machine A definition (the target host)

Host 172.24.0.* 172.16.0.* 172.27.0.*
  User         admin
  ControlMaster no
  ControlPath   none
  ControlPersist 0
  ProxyCommand ssh -q -W %h:%p qa-private-pontiac

Host  qa-private-pontiac-* internal-x*
  User         admin
  ControlMaster no
  ControlPath   none
  ControlPersist 0
  ProxyCommand ssh -q -W $(resolveip %h):%p qa-private-pontiac

##finish via2
