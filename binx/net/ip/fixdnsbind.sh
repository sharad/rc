#/bin/zsh


resolvednses=( $(grep nameserver /etc/resolv.conf | tr -s  ' \t'  ' ' | cut -d' ' -f2 ) )
forwarddnses=( $(sed -ne 's/^[ \t]*\([0-9.]\+\);[ \t]*/\1/p' /etc/bind/named.conf.forwarders) )
