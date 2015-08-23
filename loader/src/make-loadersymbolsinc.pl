#!/usr/bin/perl

sub maybe_print {
    my $name = shift @_;
    my $address = shift @_;

    if ($name !~ /fix|[A-Z]/) {
        if ((($address >= $loader_start) && ($address <= $loader_end))
         || (($address >= $loaderinstall_start) && ($address <= $loaderinstall_end))
         || (($address >= $loaderzp_start) && ($address <= $loaderzp_end))) {
            my @symbol = ($name, $address);
            push(@symbols, \@symbol);
        }
    }
}

$rw = open(FILE, shift ARGV);

while (defined($i = <FILE>)) {

    if ($i =~ /list:/) {
        $current_list = $i;
    }

    if ($current_list =~ 'Segment list:') {

        if ($i =~ /LOADER\w*\s+(\w+)\s+(\w+)/) {
            eval('$_start = 0x' . $1 . '; $_end = 0x' . $2);
        }

        if ($i =~ /LOADERZP/) {
            $loaderzp_start = $_start;
            $loaderzp_end = $_end;
        } elsif ($i =~ /LOADERINSTALL/) {
            $loaderinstall_start = $_start;
            $loaderinstall_end = $_end;
        } elsif ($i =~ /LOADER/) {
            $loader_start = $_start;
            $loader_end = $_end;
        }
    }

    if ($current_list =~ 'Exports list:') {

        if ($i =~ /(\w+)\s+(\w+)\s+\w+\s+(\w+)\s+(\w+)/) {

            eval('$num1 = 0x' . $2 . '; $num2 = 0x' . $4);
            maybe_print($1, $num1);
            maybe_print($3, $num2);

        } elsif ($i =~ /(\w+)\s+(\w+)\s+\w+/) {

            eval('$num = 0x' . $2);
            maybe_print($1, $num);
        }
    }
}

my @sorted_symbols = sort { @$a[1] <=> @$b[1] } @symbols;

my @oldsymbol;
print "; zeropage\n";

foreach my $symbol (@sorted_symbols) {
    if ((@oldsymbol[1] < $loaderinstall_start) && (@$symbol[1] >= $loaderinstall_start)) {
        print "\n; install\n";
    }
    if ((@oldsymbol[1] < $loader_start) && (@$symbol[1] >= $loader_start)) {
        print "\n; resident\n";
    }

    printf "%-15s = \$%." . (@$symbol[1] < 256 ? '2' : '4') . "x\n", @$symbol[0], @$symbol[1];

    @oldsymbol = @$symbol;
}
