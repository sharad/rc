#!/usr/bin/env bash

FORTI_VPN_USER=spratap
FORTI_VPN_SERVER=india-vpn.myfortinet.com
FORTI_VPN_PORT=10443

secret-tool lookup server exch-cas.fortinet.com user 'fortinet-us\spratap' protocol imap | tee | xclip -i

PASSWD="$(secret-tool lookup server exch-cas.fortinet.com user 'fortinet-us\spratap' protocol imap)"

echo /opt/forticlient-sslvpn/64bit/forticlientsslvpn_cli forticlientsslvpn_cli --vpnuser $FORTI_VPN_USER --server $FORTI_VPN_SERVER:$FORTI_VPN_PORT --keepalive

sshpass -p$PASSWD /opt/forticlient-sslvpn/64bit/forticlientsslvpn_cli forticlientsslvpn_cli --vpnuser $FORTI_VPN_USER --server $FORTI_VPN_SERVER:$FORTI_VPN_PORT --keepalive


