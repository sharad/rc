#!/usr/bin/perl

use WWW::Mechanize;
use Compress::Zlib;

my $mech = WWW::Mechanize->new();

my $username = "8951631028"; #fill in username here
my $keyword = "ctrplmqw";  #fill in password here

my $mobile = $ARGV[0];
my $text = $ARGV[1];

$deb = 1;

print length($text)."\n" if($deb);

$text = $text."\n\n\n\n\n" if(length($text) < 135);

$mech->get("http://wwwl.way2sms.com/content/index.html");
unless($mech->success())
{
	exit;
}
$dest = $mech->response->content;

print "Fetching...\n" if($deb);

if($mech->response->header("Content-Encoding") eq "gzip")
{
	$dest = Compress::Zlib::memGunzip($dest);
	$mech->update_html($dest);
}

$dest =~ s/<form name="loginForm"/<form action='..\/auth.cl' name="loginForm"/ig;


$mech->update_html($dest);
$mech->form_with_fields(("username","password"));
$mech->field("username",$username);
$mech->field("password",$keyword);

print "Loggin...\n" if($deb);

$mech->submit_form();

$dest= $mech->response->content;

if($mech->response->header("Content-Encoding") eq "gzip")
{
        $dest = Compress::Zlib::memGunzip($dest);
        $mech->update_html($dest);
}

$mech->get("http://wwwl.way2sms.com//jsp/InstantSMS.jsp?val=0");
$dest= $mech->response->content;
if($mech->response->header("Content-Encoding") eq "gzip")
{
        $dest = Compress::Zlib::memGunzip($dest);
        $mech->update_html($dest);
}

print "Sending ... \n" if($deb);

$mech->form_with_fields(("MobNo","textArea"));
$mech->field("MobNo",$mobile);
$mech->field("textArea",$text);
$mech->submit_form();

if($mech->success())
{
print "Done \n" if($deb);
}
else
{
print "Failed \n" if($deb);
exit;
}

$dest =  $mech->response->content;
if($mech->response->header("Content-Encoding") eq "gzip")
{
        $dest = Compress::Zlib::memGunzip($dest);
		#print $dest if($deb);
}

if($dest =~ m/successfully/sig)
{
  print "Message sent successfully" if($deb);
}

exit;
#EOF
