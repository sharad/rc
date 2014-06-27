#!/usr/bin/perl

# http://coding.derkeiler.com/Archive/Perl/comp.lang.perl.misc/2009-02/msg00234.html
## ===============================================
## C_FunctionParser_v2.pl @ 2/7/09
## -------------------------------
## C/C++ Style Function Parser
## Idea - To parse out C/C++ style functions
## that have parenthetical closures (some don't).
## (Could be a package some day, dunno, maybe ..)
## - sln
## ===============================================

my $VERSION = 2.0;
$|=1;

use strict;
use warnings;

# Prototype's
sub Find_Function(\$\@);

# File-scoped variables
my ($FxParse,$FName,$Preamble);

# Set default function name
SetFunctionName();

## --------
## Main
## --------

# Source file
my $Source = join '', <DATA>;

# Extended, possibly non-compliant, function name - pattern examples:
# SetFunctionName(qr/_T/);
# SetFunctionName(qr/\(\s*void\s*\)\s*function/);
# SetFunctionName("\\(\\s*void\\s*\\)\\s*function");

# Parse some functions
# func ...
my @Funct = ();
Find_Function($Source, @Funct);
# func2 ...
my @Funct2 = ();
SetFunctionName(qr/_T/);
Find_Function($Source, @Funct2);

# Print @Funct functions found
# Note that segments can be modified and collated.
if (!@Funct) {
    print "Function name pattern: '$FName' not found!\n";
} else {
    print "\nFound ".@Funct." matches.\nFunction pattern: '$FName' \n";
}
for my $ref (@Funct) {
    printf "\n\@: %6d - %s\n", $$ref[3], substr($Source, $$ref[0], $$ref[2] - $$ref[0]);
}

## ----------
## End
## ----------


# ---------------------------------------------------------

# Set the parser's function regex pattern
#
sub SetFunctionName {
    if (!@_) {
        $FName = "_*[a-zA-Z][\\w]*"; # Matches all compliant function names (default)
    } else {
        $FName = shift;
    }
    $Preamble = "\\s*\\(";

    # Compile function parser regular expression
    # Regex condensed:
    # $FxParse = qr!/{2}.*?\n|/\*.*?\*/|\\.|'["()]'|(")|($FName$Preamble)|(\()|(\))!s;
    # | | |1 1|2 2|3 3|4 4
    # Note - Non-Captured, matching items, are meant to consume!
    # -----------------------------------------------------------
    # Regex /xpanded (with commentary):
    $FxParse = # Regex Precedence (items MUST be in this order):
        qr! # -----------------------------------------------
              /{2}.*?\n | # comment - // + anything + end of line
              /\*.*?\*/ | # comment - /* + anything + */
              \\. | # escaped char - backslash + ANY character
              '["()]' | # single quote char - quote then one of ", (, or ), then quote
              (") | # capture $1 - double quote as a flag
              ($FName$Preamble) | # capture $2 - $FName + $Preamble
              (\() | # capture $3 - ( as a flag
              (\)) # capture $4 - ) as a flag
          !xs;
}

# Procedure that finds C/C++ style functions
# (the engine)
# Notes:
# - This is not a syntax checker !!!
# - Nested functions index and closure are cached. The search is single pass.
# - Parenthetical closures are determined via cached counter.
# - This precedence avoids all ambigous paranthetical open/close conditions:
# 1. Dual comment styles.
# 2. Escapes.
# 3. Single quoted characters.
# 4. Double quotes, fip-flopped to determine closure.
# - Improper closures are reported, with the last one reliably being the likely culprit
# (this would be a syntax error, ie: the code won't complie, but it is reported as a closure error).
#
sub Find_Function(\$\@) {
    my ($src,$Funct) = @_;
    my @Ndx = ();
    my @Closure = ();
    my ($Lines,$offset,$closure,$dquotes) = (1,0,0,0);

    while ($$src =~ /$FxParse/g)
        {
            if (defined $1) # double quote "
                {
                    $dquotes = !$dquotes;
                }
            next if ($dquotes);

            if (defined $2) # 'function name'
                {
                    # ------------------------------------
                    # Placeholder for exclusions......
                    # ------------------------------------

                    # Cache the current function index and current closure
                    push @Ndx, scalar(@$Funct);
                    push @Closure, $closure;

                    my ($funcpos, $parampos) = ( $-[0], pos($$src) );

                    # Get newlines since last function
                    $Lines += substr ($$src, $offset, $funcpos - $offset) =~ tr/\n//;
                    # print $Lines,"\n";

                    # Save positions: function( parms )
                    push @$Funct , [$funcpos, $parampos, 0, $Lines];

                    # Asign new offset
                    $offset = $funcpos;
                    # Closure is now 1 because of preamble '('
                    $closure = 1;
                }
            elsif (defined $3) # '('
                {
                    ++$closure;
                }
            elsif (defined $4) # ')'
                {
                    --$closure;
                    if ($closure <= 0)
                        {
                            $closure = 0;
                            if (@Ndx)
                                {
                                    # Pop index and closure, store position
                                    $$Funct[pop @Ndx][2] = pos($$src);
                                    $closure = pop @Closure;
                                }
                        }
                }
        }

    # To test an error, either take off the closure of a function in its source,
    # or force it this way (pseudo error, make sure you have data in @$Funct):
    # push @Ndx, 1;

    # Its an error if index stack has elements.
    # The last one reported is the likely culprit.
    if (@Ndx)
        {
            ## BAD RETURN ...
            ## All elements in stack have to be fixed up
            while (@Ndx) {
                my $func_index = shift @Ndx;
                my $ref = $$Funct[$func_index];
                $$ref[2] = $$ref[1];
                print STDERR "** Bad return, index = $func_index\n";
                print "** Error! Unclosed function [$func_index], line ".
                    $$ref[3].": '".substr ($$src, $$ref[0], $$ref[2] - $$ref[0] )."'\n";
            }
            return 0;
        }
    return 1
}

__DATA__

