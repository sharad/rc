#!/usr/bin/perl -w
use strict;
# use Getopt::Long;
use IO::Handle;
use IO::Select;
use IO::Socket::UNIX;

my $unix_sock_addr = "/opt/meru/var/run/xclid-pipe/pipe";

if ( $#ARGV > -1 ) {
    $unix_sock_addr = $ARGV[0];
}
# Getopt::Long::Configure("bundling");
# GetOptions ('socket|s=s' => \$unix_sock_addr);
# GetOptions ('socket|s=s' => \$unix_sock_addr);
print "Connecting $unix_sock_addr\n";
my $socket = IO::Socket::UNIX->new(
#    PeerAddr => $unix_sock_addr,
    Peer => $unix_sock_addr,
    Type => SOCK_STREAM,
    Timeout => 10,
) or die "Error connecting to unix domain socket: $@";

$socket->autoflush(1);

# split the program into two processes, identical twins
my $kidpid;
my $line;

die "can't fork: $!" unless defined($kidpid = fork());

if ($kidpid) {
    # parent copies the socket to standard output
    while (defined ($line = <$socket>)) {
        print STDOUT $line;
    }
    kill("TERM" => $kidpid);        # send SIGTERM to child
}
else {
    # child copies standard input to the socket
    while (defined ($line = <STDIN>)) {
        print $socket $line;
    }
}
exit;

#-----------------------------
my $byte;
while (sysread($socket, $byte, 1) == 1) {
    print STDOUT $byte;
}
#-----------------------------

__END__

# 'use IO::Socket::UNIX;my$unix_sock_addr="/opt/meru/var/run/xclid-pipe/pipe";my$socket=IO::Socket::UNIX->new(Peer=>$unix_sock_addr,Type=>SOCK_STREAM,Timeout=>10)or die "Error connecting to unix domain socket: $@";$socket->autoflush(1);my($kidpid,$line);die"canot fork: $!" unless defined($kidpid=fork());if($kidpid){while(defined($line=<$socket>)){print STDOUT $line;}kill("TERM"=>$kidpid);}else{while(defined($line=<STDIN>)){print$socket $line;}}'

# reset ; ssh -t  controller-mc3200-sharad perl -e \''use IO::Socket::UNIX;my$unix_sock_addr="/opt/meru/var/run/xclid-pipe/pipe";my$socket=IO::Socket::UNIX->new(Peer=>$unix_sock_addr,Type=>SOCK_STREAM,Timeout=>10)or die "Error connecting to unix domain socket: $@";$socket->autoflush(1);my($kidpid,$line);die"canot fork: $!" unless defined($kidpid=fork());if($kidpid){while(defined($line=<$socket>)){print STDOUT $line;}kill("TERM"=>$kidpid);}else{while(defined($line=<STDIN>)){print$socket $line;}}'\'
