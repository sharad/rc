#! /usr/bin/env perl

# from: http://www.tek-tips.com/viewthread.cfm?qid=1155575&page=251

# use lib qw(c:\projects\scripts);
use strict;
use warnings;
# use Net::LDAP qw(:all);
use Net::LDAP;

use lib $ENV{'HOME'} . "/.setup/osetup/info/common/perl";
use Setup;



my $debug = 1;
my $base = $Setup::info->{"adbase"};
my $ldap;
my $mesg;
my $results;
my $err;
my $entry;
my $t;

$ldap = Net::LDAP->new($Setup::info->{"adhost"}) or die("Could not connect to LDAP server\n");

# $results = $ldap->ldapbind() || die "Bind Failed:$@";

$results =  $ldap->bind($Setup::info->{"adbind"}, password=> $Setup::info->{"adpassword"});



my @attr=("thumbnailPhoto");
# my $filter="(&(objectclass=User)(cn=Doe, John J.))";

my $filter="(&(sAMAccountName=" . $ARGV[0] . ")(objectClass=user))";

my $scope='subtree';

$mesg=$ldap->search(base =>$base,
                    # scope=>$scope,
                    filter=>$filter)
      or die "Search Failed using Filter: $filter$@";


if ( $debug ) {
    print <<EOF
filter: $filter
base:   $base
EOF
}


my $count=$mesg->count;
print("Found: $count\n");


foreach $entry ($mesg->all_entries)
 {
   $t=$entry->get("thumbnailPhoto");
 }


my $filename='outphoto.jpg';

open(OUT, ">$filename") or die "File does not exist: $filename !";
binmode(OUT);

foreach(@$t)
 {
   print OUT "$_";
 }

close(OUT);
$ldap->unbind;
