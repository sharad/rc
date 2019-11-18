#!/usr/bin/env zsh


resolvednses=( ${(f)"$(grep nameserver /etc/resolv.conf | grep -v 127.0 | tr -s  ' \t'  ' ' | cut -d' ' -f2 )"} )
forwarddnses=( ${(f)"$(sed -ne 's/^[ \t]*\([0-9.]\+\);[ \t]*/\1/p' /etc/bind/named.conf.forwarders)"} )

dnses=( ${resolvednses} ${forwarddnses} )

print 'forwarders {'
foreach dns ( ${dnses} ) {
    print "	"${dns}";"
}
print '};'
print
