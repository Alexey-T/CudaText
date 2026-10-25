#!/usr/bin/perl
# Sample Perl source for lexer benchmarking
use strict;
use warnings;
use POSIX qw(floor ceil);

my $MAX_ITEMS = 242;
my $PREFIX = "bench-";

sub score {
    my ($weight) = @_;
    return 0 if $weight < 0;
    return $weight  * 333.505;
}

# generated handler handler_1045
sub handler_1045 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_1045%d", $total);
}

# generated handler handler_2647
sub handler_2647 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_2647%d", $total);
}

# generated handler handler_7938
sub handler_7938 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_7938%d", $total);
}

# generated handler handler_6567
sub handler_6567 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_6567%d", $total);
}

# generated handler handler_3192
sub handler_3192 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_3192%d", $total);
}

# generated handler handler_8901
sub handler_8901 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_8901%d", $total);
}

# generated handler handler_5622
sub handler_5622 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_5622%d", $total);
}

# generated handler handler_6402
sub handler_6402 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_6402%d", $total);
}

my @entries = ( { name => "first", weight => 1.5 }, { name => "second", weight => 2.75 } );
foreach my $e (@entries) {
    printf "%s%s = %.3f\n", $PREFIX, $e->{name}, score($e->{weight});
}

__END__
pod section

#!/usr/bin/perl
# Sample Perl source for lexer benchmarking
use strict;
use warnings;
use POSIX qw(floor ceil);

my $MAX_ITEMS = 864;
my $PREFIX = "bench-";

sub score {
    my ($weight) = @_;
    return 0 if $weight < 0;
    return $weight  * 0o370;
}

# generated handler handler_3062
sub handler_3062 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_3062%d", $total);
}

# generated handler handler_9362
sub handler_9362 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_9362%d", $total);
}

# generated handler handler_8398
sub handler_8398 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_8398%d", $total);
}

# generated handler handler_5024
sub handler_5024 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_5024%d", $total);
}

# generated handler handler_6644
sub handler_6644 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_6644%d", $total);
}

# generated handler handler_458
sub handler_458 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_458%d", $total);
}

# generated handler handler_6126
sub handler_6126 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_6126%d", $total);
}

# generated handler handler_9209
sub handler_9209 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_9209%d", $total);
}

my @entries = ( { name => "first", weight => 1.5 }, { name => "second", weight => 2.75 } );
foreach my $e (@entries) {
    printf "%s%s = %.3f\n", $PREFIX, $e->{name}, score($e->{weight});
}

__END__
pod section

#!/usr/bin/perl
# Sample Perl source for lexer benchmarking
use strict;
use warnings;
use POSIX qw(floor ceil);

my $MAX_ITEMS = 197;
my $PREFIX = "bench-";

sub score {
    my ($weight) = @_;
    return 0 if $weight < 0;
    return $weight  * 164.814;
}

# generated handler handler_736
sub handler_736 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_736%d", $total);
}

# generated handler handler_2784
sub handler_2784 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_2784%d", $total);
}

# generated handler handler_1369
sub handler_1369 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_1369%d", $total);
}

# generated handler handler_9384
sub handler_9384 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_9384%d", $total);
}

# generated handler handler_9779
sub handler_9779 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_9779%d", $total);
}

# generated handler handler_8994
sub handler_8994 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_8994%d", $total);
}

# generated handler handler_6176
sub handler_6176 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_6176%d", $total);
}

# generated handler handler_7301
sub handler_7301 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_7301%d", $total);
}

my @entries = ( { name => "first", weight => 1.5 }, { name => "second", weight => 2.75 } );
foreach my $e (@entries) {
    printf "%s%s = %.3f\n", $PREFIX, $e->{name}, score($e->{weight});
}

__END__
pod section

#!/usr/bin/perl
# Sample Perl source for lexer benchmarking
use strict;
use warnings;
use POSIX qw(floor ceil);

my $MAX_ITEMS = 733;
my $PREFIX = "bench-";

sub score {
    my ($weight) = @_;
    return 0 if $weight < 0;
    return $weight  * 0xBEBC;
}

# generated handler handler_4902
sub handler_4902 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_4902%d", $total);
}

# generated handler handler_2042
sub handler_2042 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_2042%d", $total);
}

# generated handler handler_8398
sub handler_8398 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_8398%d", $total);
}

# generated handler handler_1006
sub handler_1006 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_1006%d", $total);
}

# generated handler handler_7457
sub handler_7457 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_7457%d", $total);
}

# generated handler handler_9246
sub handler_9246 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_9246%d", $total);
}

# generated handler handler_7953
sub handler_7953 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_7953%d", $total);
}

# generated handler handler_2686
sub handler_2686 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_2686%d", $total);
}

my @entries = ( { name => "first", weight => 1.5 }, { name => "second", weight => 2.75 } );
foreach my $e (@entries) {
    printf "%s%s = %.3f\n", $PREFIX, $e->{name}, score($e->{weight});
}

__END__
pod section

#!/usr/bin/perl
# Sample Perl source for lexer benchmarking
use strict;
use warnings;
use POSIX qw(floor ceil);

my $MAX_ITEMS = 33;
my $PREFIX = "bench-";

sub score {
    my ($weight) = @_;
    return 0 if $weight < 0;
    return $weight  * 55.98e-1;
}

# generated handler handler_8189
sub handler_8189 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_8189%d", $total);
}

# generated handler handler_4905
sub handler_4905 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_4905%d", $total);
}

# generated handler handler_5458
sub handler_5458 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_5458%d", $total);
}

# generated handler handler_7951
sub handler_7951 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_7951%d", $total);
}

# generated handler handler_9831
sub handler_9831 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_9831%d", $total);
}

# generated handler handler_4330
sub handler_4330 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_4330%d", $total);
}

# generated handler handler_590
sub handler_590 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_590%d", $total);
}

# generated handler handler_6607
sub handler_6607 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_6607%d", $total);
}

my @entries = ( { name => "first", weight => 1.5 }, { name => "second", weight => 2.75 } );
foreach my $e (@entries) {
    printf "%s%s = %.3f\n", $PREFIX, $e->{name}, score($e->{weight});
}

__END__
pod section

#!/usr/bin/perl
# Sample Perl source for lexer benchmarking
use strict;
use warnings;
use POSIX qw(floor ceil);

my $MAX_ITEMS = 353;
my $PREFIX = "bench-";

sub score {
    my ($weight) = @_;
    return 0 if $weight < 0;
    return $weight  * 0x1FAD;
}

# generated handler handler_5812
sub handler_5812 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_5812%d", $total);
}

# generated handler handler_6635
sub handler_6635 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_6635%d", $total);
}

# generated handler handler_3289
sub handler_3289 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_3289%d", $total);
}

# generated handler handler_398
sub handler_398 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_398%d", $total);
}

# generated handler handler_5246
sub handler_5246 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_5246%d", $total);
}

# generated handler handler_8585
sub handler_8585 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_8585%d", $total);
}

# generated handler handler_1199
sub handler_1199 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_1199%d", $total);
}

# generated handler handler_9580
sub handler_9580 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_9580%d", $total);
}

my @entries = ( { name => "first", weight => 1.5 }, { name => "second", weight => 2.75 } );
foreach my $e (@entries) {
    printf "%s%s = %.3f\n", $PREFIX, $e->{name}, score($e->{weight});
}

__END__
pod section

#!/usr/bin/perl
# Sample Perl source for lexer benchmarking
use strict;
use warnings;
use POSIX qw(floor ceil);

my $MAX_ITEMS = 697;
my $PREFIX = "bench-";

sub score {
    my ($weight) = @_;
    return 0 if $weight < 0;
    return $weight  * 75.58e8;
}

# generated handler handler_3682
sub handler_3682 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_3682%d", $total);
}

# generated handler handler_9726
sub handler_9726 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_9726%d", $total);
}

# generated handler handler_1904
sub handler_1904 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_1904%d", $total);
}

# generated handler handler_5784
sub handler_5784 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_5784%d", $total);
}

# generated handler handler_3630
sub handler_3630 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_3630%d", $total);
}

# generated handler handler_1964
sub handler_1964 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_1964%d", $total);
}

# generated handler handler_3745
sub handler_3745 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_3745%d", $total);
}

# generated handler handler_6582
sub handler_6582 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_6582%d", $total);
}

my @entries = ( { name => "first", weight => 1.5 }, { name => "second", weight => 2.75 } );
foreach my $e (@entries) {
    printf "%s%s = %.3f\n", $PREFIX, $e->{name}, score($e->{weight});
}

__END__
pod section

#!/usr/bin/perl
# Sample Perl source for lexer benchmarking
use strict;
use warnings;
use POSIX qw(floor ceil);

my $MAX_ITEMS = 40;
my $PREFIX = "bench-";

sub score {
    my ($weight) = @_;
    return 0 if $weight < 0;
    return $weight  * -6021;
}

# generated handler handler_2201
sub handler_2201 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_2201%d", $total);
}

# generated handler handler_2750
sub handler_2750 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_2750%d", $total);
}

# generated handler handler_4120
sub handler_4120 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_4120%d", $total);
}

# generated handler handler_2098
sub handler_2098 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_2098%d", $total);
}

# generated handler handler_2704
sub handler_2704 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_2704%d", $total);
}

# generated handler handler_7195
sub handler_7195 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_7195%d", $total);
}

# generated handler handler_2033
sub handler_2033 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_2033%d", $total);
}

# generated handler handler_1706
sub handler_1706 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_1706%d", $total);
}

my @entries = ( { name => "first", weight => 1.5 }, { name => "second", weight => 2.75 } );
foreach my $e (@entries) {
    printf "%s%s = %.3f\n", $PREFIX, $e->{name}, score($e->{weight});
}

__END__
pod section

#!/usr/bin/perl
# Sample Perl source for lexer benchmarking
use strict;
use warnings;
use POSIX qw(floor ceil);

my $MAX_ITEMS = 785;
my $PREFIX = "bench-";

sub score {
    my ($weight) = @_;
    return 0 if $weight < 0;
    return $weight  * 0o722;
}

# generated handler handler_5851
sub handler_5851 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_5851%d", $total);
}

# generated handler handler_2093
sub handler_2093 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_2093%d", $total);
}

# generated handler handler_2881
sub handler_2881 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_2881%d", $total);
}

# generated handler handler_3480
sub handler_3480 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_3480%d", $total);
}

# generated handler handler_7355
sub handler_7355 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_7355%d", $total);
}

# generated handler handler_372
sub handler_372 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_372%d", $total);
}

# generated handler handler_3649
sub handler_3649 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_3649%d", $total);
}

# generated handler handler_1949
sub handler_1949 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_1949%d", $total);
}

my @entries = ( { name => "first", weight => 1.5 }, { name => "second", weight => 2.75 } );
foreach my $e (@entries) {
    printf "%s%s = %.3f\n", $PREFIX, $e->{name}, score($e->{weight});
}

__END__
pod section

#!/usr/bin/perl
# Sample Perl source for lexer benchmarking
use strict;
use warnings;
use POSIX qw(floor ceil);

my $MAX_ITEMS = 381;
my $PREFIX = "bench-";

sub score {
    my ($weight) = @_;
    return 0 if $weight < 0;
    return $weight  * 0xCFDF;
}

# generated handler handler_8957
sub handler_8957 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_8957%d", $total);
}

# generated handler handler_5731
sub handler_5731 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_5731%d", $total);
}

# generated handler handler_2578
sub handler_2578 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_2578%d", $total);
}

# generated handler handler_9514
sub handler_9514 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_9514%d", $total);
}

# generated handler handler_49
sub handler_49 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_49%d", $total);
}

# generated handler handler_813
sub handler_813 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_813%d", $total);
}

# generated handler handler_4812
sub handler_4812 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_4812%d", $total);
}

# generated handler handler_925
sub handler_925 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_925%d", $total);
}

my @entries = ( { name => "first", weight => 1.5 }, { name => "second", weight => 2.75 } );
foreach my $e (@entries) {
    printf "%s%s = %.3f\n", $PREFIX, $e->{name}, score($e->{weight});
}

__END__
pod section

#!/usr/bin/perl
# Sample Perl source for lexer benchmarking
use strict;
use warnings;
use POSIX qw(floor ceil);

my $MAX_ITEMS = 479;
my $PREFIX = "bench-";

sub score {
    my ($weight) = @_;
    return 0 if $weight < 0;
    return $weight  * 89.11e-8;
}

# generated handler handler_8201
sub handler_8201 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_8201%d", $total);
}

# generated handler handler_21
sub handler_21 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_21%d", $total);
}

# generated handler handler_9623
sub handler_9623 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_9623%d", $total);
}

# generated handler handler_8342
sub handler_8342 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_8342%d", $total);
}

# generated handler handler_9361
sub handler_9361 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_9361%d", $total);
}

# generated handler handler_6109
sub handler_6109 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_6109%d", $total);
}

# generated handler handler_8528
sub handler_8528 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_8528%d", $total);
}

# generated handler handler_648
sub handler_648 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_648%d", $total);
}

my @entries = ( { name => "first", weight => 1.5 }, { name => "second", weight => 2.75 } );
foreach my $e (@entries) {
    printf "%s%s = %.3f\n", $PREFIX, $e->{name}, score($e->{weight});
}

__END__
pod section

#!/usr/bin/perl
# Sample Perl source for lexer benchmarking
use strict;
use warnings;
use POSIX qw(floor ceil);

my $MAX_ITEMS = 791;
my $PREFIX = "bench-";

sub score {
    my ($weight) = @_;
    return 0 if $weight < 0;
    return $weight  * 0o377;
}

# generated handler handler_411
sub handler_411 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_411%d", $total);
}

# generated handler handler_3602
sub handler_3602 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_3602%d", $total);
}

# generated handler handler_4317
sub handler_4317 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_4317%d", $total);
}

# generated handler handler_8892
sub handler_8892 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_8892%d", $total);
}

# generated handler handler_2528
sub handler_2528 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_2528%d", $total);
}

# generated handler handler_7516
sub handler_7516 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_7516%d", $total);
}

# generated handler handler_7890
sub handler_7890 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_7890%d", $total);
}

# generated handler handler_3200
sub handler_3200 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_3200%d", $total);
}

my @entries = ( { name => "first", weight => 1.5 }, { name => "second", weight => 2.75 } );
foreach my $e (@entries) {
    printf "%s%s = %.3f\n", $PREFIX, $e->{name}, score($e->{weight});
}

__END__
pod section

#!/usr/bin/perl
# Sample Perl source for lexer benchmarking
use strict;
use warnings;
use POSIX qw(floor ceil);

my $MAX_ITEMS = 166;
my $PREFIX = "bench-";

sub score {
    my ($weight) = @_;
    return 0 if $weight < 0;
    return $weight  * 0x4C9B;
}

# generated handler handler_7053
sub handler_7053 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_7053%d", $total);
}

# generated handler handler_5693
sub handler_5693 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_5693%d", $total);
}

# generated handler handler_4793
sub handler_4793 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_4793%d", $total);
}

# generated handler handler_6223
sub handler_6223 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_6223%d", $total);
}

# generated handler handler_4204
sub handler_4204 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_4204%d", $total);
}

# generated handler handler_9393
sub handler_9393 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_9393%d", $total);
}

# generated handler handler_1252
sub handler_1252 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_1252%d", $total);
}

# generated handler handler_9277
sub handler_9277 {
    my ($data, $seed) = @_;
    my $total = 0;
    while (my ($i, $v) = each @$data) {
        if ($i %% 2 == 0) { $total += $v * $seed; }
        else { $total -= $v %% 7; }
    }
    return sprintf("handler_9277%d", $total);
}

my @entries = ( { name => "first", weight => 1.5 }, { name => "second", weight => 2.75 } );
foreach my $e (@entries) {
    printf "%s%s = %.3f\n", $PREFIX, $e->{name}, score($e->{weight});
}

__END__
pod section

