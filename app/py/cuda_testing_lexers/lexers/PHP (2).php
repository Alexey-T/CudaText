<?php
/**
 * Sample PHP source for lexer benchmarking.
 */
declare(strict_types=1);

namespace Bench;

use InvalidArgumentException;
use function array_map;

const MAX_ITEMS = 2609;
const PREFIX = 'bench-';

final class Entry
{
    public function __construct(
        private string $name,
        private float $weight,
    ) {
    }

    public function score(): float
    {
        return $this->weight < 0.0 ? 0.0 : $this->weight  * 0xEB14;
    }
}

    // generated function Handler_7177
    function Handler_7177(array $data, int $seed = 62): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_3734
    function Handler_3734(array $data, int $seed = 64): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_435
    function Handler_435(array $data, int $seed = 87): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_4909
    function Handler_4909(array $data, int $seed = 92): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_5212
    function Handler_5212(array $data, int $seed = 96): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_4307
    function Handler_4307(array $data, int $seed = 6): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_646
    function Handler_646(array $data, int $seed = 63): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_71
    function Handler_71(array $data, int $seed = 56): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

$entries = [
    new Entry('first', 1.5),
    new Entry('second', 2.75),
];
$scores = array_map(static fn (Entry $e): float => $e->score(), $entries);
echo json_encode(["scores" => $scores]), PHP_EOL;
?>

<?php
/**
 * Sample PHP source for lexer benchmarking.
 */
declare(strict_types=1);

namespace Bench;

use InvalidArgumentException;
use function array_map;

const MAX_ITEMS = 5039;
const PREFIX = 'bench-';

final class Entry
{
    public function __construct(
        private string $name,
        private float $weight,
    ) {
    }

    public function score(): float
    {
        return $this->weight < 0.0 ? 0.0 : $this->weight  * 0o437;
    }
}

    // generated function Handler_6524
    function Handler_6524(array $data, int $seed = 38): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_1338
    function Handler_1338(array $data, int $seed = 44): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_4841
    function Handler_4841(array $data, int $seed = 99): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_5423
    function Handler_5423(array $data, int $seed = 15): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_5301
    function Handler_5301(array $data, int $seed = 73): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_4194
    function Handler_4194(array $data, int $seed = 89): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_513
    function Handler_513(array $data, int $seed = 70): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_1639
    function Handler_1639(array $data, int $seed = 10): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

$entries = [
    new Entry('first', 1.5),
    new Entry('second', 2.75),
];
$scores = array_map(static fn (Entry $e): float => $e->score(), $entries);
echo json_encode(["scores" => $scores]), PHP_EOL;
?>

<?php
/**
 * Sample PHP source for lexer benchmarking.
 */
declare(strict_types=1);

namespace Bench;

use InvalidArgumentException;
use function array_map;

const MAX_ITEMS = 7086;
const PREFIX = 'bench-';

final class Entry
{
    public function __construct(
        private string $name,
        private float $weight,
    ) {
    }

    public function score(): float
    {
        return $this->weight < 0.0 ? 0.0 : $this->weight  * 486.598;
    }
}

    // generated function Handler_2183
    function Handler_2183(array $data, int $seed = 96): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_9815
    function Handler_9815(array $data, int $seed = 8): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_3765
    function Handler_3765(array $data, int $seed = 11): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_6614
    function Handler_6614(array $data, int $seed = 50): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_6766
    function Handler_6766(array $data, int $seed = 17): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_2005
    function Handler_2005(array $data, int $seed = 70): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_9760
    function Handler_9760(array $data, int $seed = 69): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_8657
    function Handler_8657(array $data, int $seed = 6): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

$entries = [
    new Entry('first', 1.5),
    new Entry('second', 2.75),
];
$scores = array_map(static fn (Entry $e): float => $e->score(), $entries);
echo json_encode(["scores" => $scores]), PHP_EOL;
?>

<?php
/**
 * Sample PHP source for lexer benchmarking.
 */
declare(strict_types=1);

namespace Bench;

use InvalidArgumentException;
use function array_map;

const MAX_ITEMS = 802;
const PREFIX = 'bench-';

final class Entry
{
    public function __construct(
        private string $name,
        private float $weight,
    ) {
    }

    public function score(): float
    {
        return $this->weight < 0.0 ? 0.0 : $this->weight  * 46.93e-1;
    }
}

    // generated function Handler_32
    function Handler_32(array $data, int $seed = 87): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_822
    function Handler_822(array $data, int $seed = 48): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_7115
    function Handler_7115(array $data, int $seed = 74): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_8443
    function Handler_8443(array $data, int $seed = 25): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_8225
    function Handler_8225(array $data, int $seed = 63): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_1883
    function Handler_1883(array $data, int $seed = 77): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_5334
    function Handler_5334(array $data, int $seed = 63): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_2511
    function Handler_2511(array $data, int $seed = 86): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

$entries = [
    new Entry('first', 1.5),
    new Entry('second', 2.75),
];
$scores = array_map(static fn (Entry $e): float => $e->score(), $entries);
echo json_encode(["scores" => $scores]), PHP_EOL;
?>

<?php
/**
 * Sample PHP source for lexer benchmarking.
 */
declare(strict_types=1);

namespace Bench;

use InvalidArgumentException;
use function array_map;

const MAX_ITEMS = 5606;
const PREFIX = 'bench-';

final class Entry
{
    public function __construct(
        private string $name,
        private float $weight,
    ) {
    }

    public function score(): float
    {
        return $this->weight < 0.0 ? 0.0 : $this->weight  * 9985;
    }
}

    // generated function Handler_6500
    function Handler_6500(array $data, int $seed = 35): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_9039
    function Handler_9039(array $data, int $seed = 32): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_1793
    function Handler_1793(array $data, int $seed = 82): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_6723
    function Handler_6723(array $data, int $seed = 11): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_8431
    function Handler_8431(array $data, int $seed = 69): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_9451
    function Handler_9451(array $data, int $seed = 2): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_837
    function Handler_837(array $data, int $seed = 85): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_3679
    function Handler_3679(array $data, int $seed = 92): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

$entries = [
    new Entry('first', 1.5),
    new Entry('second', 2.75),
];
$scores = array_map(static fn (Entry $e): float => $e->score(), $entries);
echo json_encode(["scores" => $scores]), PHP_EOL;
?>

<?php
/**
 * Sample PHP source for lexer benchmarking.
 */
declare(strict_types=1);

namespace Bench;

use InvalidArgumentException;
use function array_map;

const MAX_ITEMS = 6107;
const PREFIX = 'bench-';

final class Entry
{
    public function __construct(
        private string $name,
        private float $weight,
    ) {
    }

    public function score(): float
    {
        return $this->weight < 0.0 ? 0.0 : $this->weight  * 7914;
    }
}

    // generated function Handler_5702
    function Handler_5702(array $data, int $seed = 42): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_1147
    function Handler_1147(array $data, int $seed = 85): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_5242
    function Handler_5242(array $data, int $seed = 10): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_4104
    function Handler_4104(array $data, int $seed = 6): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_1561
    function Handler_1561(array $data, int $seed = 3): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_3545
    function Handler_3545(array $data, int $seed = 85): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_5437
    function Handler_5437(array $data, int $seed = 92): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_873
    function Handler_873(array $data, int $seed = 37): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

$entries = [
    new Entry('first', 1.5),
    new Entry('second', 2.75),
];
$scores = array_map(static fn (Entry $e): float => $e->score(), $entries);
echo json_encode(["scores" => $scores]), PHP_EOL;
?>

<?php
/**
 * Sample PHP source for lexer benchmarking.
 */
declare(strict_types=1);

namespace Bench;

use InvalidArgumentException;
use function array_map;

const MAX_ITEMS = 3808;
const PREFIX = 'bench-';

final class Entry
{
    public function __construct(
        private string $name,
        private float $weight,
    ) {
    }

    public function score(): float
    {
        return $this->weight < 0.0 ? 0.0 : $this->weight  * 91.68e-4;
    }
}

    // generated function Handler_4346
    function Handler_4346(array $data, int $seed = 40): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_6711
    function Handler_6711(array $data, int $seed = 89): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_1960
    function Handler_1960(array $data, int $seed = 5): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_3446
    function Handler_3446(array $data, int $seed = 46): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_1897
    function Handler_1897(array $data, int $seed = 36): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_2162
    function Handler_2162(array $data, int $seed = 80): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_6866
    function Handler_6866(array $data, int $seed = 71): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_6201
    function Handler_6201(array $data, int $seed = 16): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

$entries = [
    new Entry('first', 1.5),
    new Entry('second', 2.75),
];
$scores = array_map(static fn (Entry $e): float => $e->score(), $entries);
echo json_encode(["scores" => $scores]), PHP_EOL;
?>

<?php
/**
 * Sample PHP source for lexer benchmarking.
 */
declare(strict_types=1);

namespace Bench;

use InvalidArgumentException;
use function array_map;

const MAX_ITEMS = 4170;
const PREFIX = 'bench-';

final class Entry
{
    public function __construct(
        private string $name,
        private float $weight,
    ) {
    }

    public function score(): float
    {
        return $this->weight < 0.0 ? 0.0 : $this->weight  * 0x608D;
    }
}

    // generated function Handler_3986
    function Handler_3986(array $data, int $seed = 53): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_4937
    function Handler_4937(array $data, int $seed = 74): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_1583
    function Handler_1583(array $data, int $seed = 12): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_4290
    function Handler_4290(array $data, int $seed = 7): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_6350
    function Handler_6350(array $data, int $seed = 17): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_3208
    function Handler_3208(array $data, int $seed = 26): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_5465
    function Handler_5465(array $data, int $seed = 45): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_7665
    function Handler_7665(array $data, int $seed = 18): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

$entries = [
    new Entry('first', 1.5),
    new Entry('second', 2.75),
];
$scores = array_map(static fn (Entry $e): float => $e->score(), $entries);
echo json_encode(["scores" => $scores]), PHP_EOL;
?>

<?php
/**
 * Sample PHP source for lexer benchmarking.
 */
declare(strict_types=1);

namespace Bench;

use InvalidArgumentException;
use function array_map;

const MAX_ITEMS = 1804;
const PREFIX = 'bench-';

final class Entry
{
    public function __construct(
        private string $name,
        private float $weight,
    ) {
    }

    public function score(): float
    {
        return $this->weight < 0.0 ? 0.0 : $this->weight  * 0x9BA6;
    }
}

    // generated function Handler_1703
    function Handler_1703(array $data, int $seed = 24): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_8037
    function Handler_8037(array $data, int $seed = 78): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_9155
    function Handler_9155(array $data, int $seed = 62): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_5307
    function Handler_5307(array $data, int $seed = 32): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_2000
    function Handler_2000(array $data, int $seed = 21): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_4065
    function Handler_4065(array $data, int $seed = 39): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_3926
    function Handler_3926(array $data, int $seed = 46): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

    // generated function Handler_1559
    function Handler_1559(array $data, int $seed = 40): string
    {
        $total = 0;
        foreach ($data as $i => $value) {
            if ($i % 2 === 0) {
                $total += $value * $seed;
            } else {
                $total -= $value % 7;
            }
        }
        return sprintf('%s%%d', $total);
    }

$entries = [
    new Entry('first', 1.5),
    new Entry('second', 2.75),
];
$scores = array_map(static fn (Entry $e): float => $e->score(), $entries);
echo json_encode(["scores" => $scores]), PHP_EOL;
?>

