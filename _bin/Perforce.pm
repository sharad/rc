##
## p4changes.pl
## Login : <sh4r4d _at_ _G-mail_>
## Login : <sh4r4d _at_ _G-mail_>
## Started on  Thu Apr  1 12:06:57 2010 Sharad Pratap
## $Id$
##


package Perforce;

use strict;
use warnings;
use IPC::Open3;                 # I have to use it.
our $debug = 0;

our $empty = 1;

my @cmd = qw/p4 -ztag/;         # perforce command



sub new {
  my $class = shift;
  my $self = {@_};
  @{$self->{'cmd'}} = @cmd;
  bless $self, $class;
  return $self;
} # end of new

sub matcher {
  my $self = shift;
  my $pipe = shift;
  my $ret = $empty ? [] : undef;
  local $/ = "\n\n\n\.\.\. ";
  my $prefix;
  while (my $line = <$pipe>) {
    # What is this, If I will not use my $line, then it will always
    # throw "Modification of a read-only value attempted at
    # Perforce.pm line 60." From inside program, but it is fine, on cli.

    # May be $_ at sometime is readonly.

    chomp $line;                      # finish with $/
    # my $line = $_;
    $line = $prefix . $line if $prefix;
    $prefix = "\n\.\.\. ";
    print STDERR $line, "\n####" if defined $debug and $debug;
    {
      local $/ = "\n\.\.\. ";
      open (ARR, "<", \$line);
      my $hash = undef;
      while (my $x = <ARR>) {
        chomp $x;
        $x =~ s/^\.\.\. //g unless $hash;
        if ($x =~ /^(\w+)\s(\s*\S+.+)$/s) {
          my ($key, $value) = ($1, $2);
          if ($key =~ /^(\w+)(\d+)$/) {
            my ($subkey, $index) = ($1, $2);
            $hash->{files}->[$index]->{$subkey} = $value;
          } else {
            $hash->{$key} = $value;
          }
        }
        print STDERR $x, "\n~~~~~" if defined $debug and $debug;
      }
      push @{$ret}, $hash;
      close ARR;
    }
    local $/ = "\n\n\n\.\.\. ";
  }
  return $ret;
} # end of matcher.

sub dispatchcmd {
  my $self = shift;
  delete $self->{'error'} if $self->{'error'};
  my @argn = @_;
  my $runcmd = join " ", @{ $self->{'cmd'} }, @argn;

  if ($runcmd =~ /\;/) {                    # secure it.
    die "Bad data in " . (join " ", @argn); # log this somewhere
  }

  print $runcmd, "\n" if defined $debug and $debug;
  $ENV{'P4CLIENT'} = $self->{'client'};

  # open(PIPE, "${runcmd} |") or die "Error $! \n";

  my $pid = open3(\*INX, \*OUTX, \*ERRX, "${runcmd}"); # or die "Error $! \n";
  waitpid( $pid, 0 );
  my $retval = $? >> 8;

  my $ret = $self->matcher(\*OUTX) or die "$!";


 # NOTE: Perforce p4 client: return non-zero (error) if it found some
 # semantic error.  Where semantic error mean when aphabets provied
 # where number expected. But it do not retuen non-zero (error) when
 # non-existing changelist number supplied.  But in both of the cases
 # output in stderr.

  my @erout = <ERRX>;
  if ($retval) {
    die "@erout";
  # } elsif (@erout) {
  } elsif (@erout and !@{$ret}) { # error output is here and no array of hash is made in matcher.
    # die "@erout";
    $self->{'error'} = join @erout;
    die "@erout";
  }

  # close PIPE;
  # exit;
  return $ret;
} # end of dispatchcmd

sub filelog {
} # end of filelog

sub changes {
  my $self = shift;
  my $depot = shift;
  return $self->dispatchcmd(qw/changes -lt/, $depot);
} # end of changes

sub change {
  my $self = shift;
  my $chnum = shift;
  return 0 unless $chnum and $chnum =~ /\d+/;
  return $self->dispatchcmd(qw/change -o/, $chnum);
} # end of changes

sub describe {
  my $self = shift;
  my $chnum = shift;
  die "change number should be numeric" unless $chnum and $chnum =~ /\d+/;
  return $self->dispatchcmd(qw/describe/, $chnum);
} # end of changes

sub info {
  my $self = shift;
  my $chnum = shift;
  return 0 unless $chnum and $chnum =~ /\d+/;
  return $self->dispatchcmd(qw/info/);
} # end of changes

sub SetClient {
  my $self = shift;
  my $client = shift;
  die "Please provide the cleint" unless $client;
  $ENV{'P4CLIENT'} = $client;
  $self->{clientName} = $client;
}

sub GetClient {
  my $self = shift;
  my $info = @{ $self->Run('info') }[0];
  return $info->{'clientName'};
}

sub SetTicketFile {
  my $self = shift;
  my $ticketfile = shift;
  die "Please provide the ticketfile" unless $ticketfile;
  die "Given ticket file is not readable" unless -r $ticketfile;
  $ENV{'P4TICKETS'} = $ticketfile;
  $self->{'ticketfile'} = $ticketfile;
}

sub GetTicketFile {
  my $self = shift;
  return $self->{'ticketfile'};
}

sub SetUser {
  my $self = shift;
  my $user = shift;
  die "Please provide the cleint" unless $user;
  $ENV{'P4USER'} = $user;
  $self->{userName} = $user;
}

sub GetUser {
  my $self = shift;
  my $info = @{ $self->Run("info") }[0];
  return $info->{'userName'};
}

sub SetPort {
  my $self = shift;
  my $port = shift;
  die "Please provide the cleint" unless $port;
  $ENV{'P4PORT'} = $port;
  $self->{port} = $port;
}


sub GetPort {
  my $self = shift;
  my $info = @{ $self->Run("info") }[0];
  return $info->{'serverAddress'};
}

sub SetPassword {
  my $self = shift;
  my $password = shift;
  die "Please provide the password" unless defined $password;
  $ENV{'P4PASSWD'} = $password;
  $self->{password} = $password;
}

sub GetPassword {
  my $self = shift;
  return $self->{'password'} ? $self->{'password'} : "";
}

sub Connect {
  1
} # end of connect

sub Disconnect {
  1
} # end of connect

sub Tagged {
  1
} # end of Tagged

sub Run {
  my $self = shift;
  my @args = @_;
  return $self->dispatchcmd(@args);
} # end of Run

1;

