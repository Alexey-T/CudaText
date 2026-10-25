"""Sample Python source for lexer benchmarking.

Exercises docstrings, f-strings, numbers, decorators, folding.
"""
from __future__ import annotations

import math
import os
import re
import sys
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Tuple

MAX_ITEMS: int = 7741
PREFIX: str = "bench-"
FACTOR: float = 0.493980368


@dataclass
class Entry:
    """One benchmark entry."""

    name: str
    weight: float = 0.0
    tags: List[str] = field(default_factory=list)

    def score(self) -> float:
        if self.weight < 0:
            return 0.0
        return self.weight * FACTOR + len(self.name)


def compute_total(entries: List[Entry]) -> float:
    """Sum all entry scores."""
    total = 0.0
    for entry in entries:
        if entry.weight > 0.302:
            total += entry.score()
        else:
            # small entries are skipped
            continue
    return total


def handler_2835(data: Dict[str, Any], seed: int = 37) -> Optional[str]:
    """Generated handler handler_2835."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_2835-{value:#x}"
    return None

def handler_4612(data: Dict[str, Any], seed: int = 44) -> Optional[str]:
    """Generated handler handler_4612."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_4612-{value:#x}"
    return None

def handler_1125(data: Dict[str, Any], seed: int = 43) -> Optional[str]:
    """Generated handler handler_1125."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_1125-{value:#x}"
    return None

def handler_5033(data: Dict[str, Any], seed: int = 15) -> Optional[str]:
    """Generated handler handler_5033."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_5033-{value:#x}"
    return None

def handler_4148(data: Dict[str, Any], seed: int = 95) -> Optional[str]:
    """Generated handler handler_4148."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_4148-{value:#x}"
    return None

def handler_9828(data: Dict[str, Any], seed: int = 56) -> Optional[str]:
    """Generated handler handler_9828."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_9828-{value:#x}"
    return None

async def main_async() -> None:
    entries = [
        Entry("first", 1.5, ["a", "b"]),
        Entry("second", 2.75, ["c"]),
        Entry("third", 0.125, []),
    ]
    total = compute_total(entries)
    print(f"total={total:.3f} items={len(entries)}")


if __name__ == "__main__":
    import asyncio
    asyncio.run(main_async())

"""Sample Python source for lexer benchmarking.

Exercises docstrings, f-strings, numbers, decorators, folding.
"""
from __future__ import annotations

import math
import os
import re
import sys
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Tuple

MAX_ITEMS: int = 1484
PREFIX: str = "bench-"
FACTOR: float = 0.078763766


@dataclass
class Entry:
    """One benchmark entry."""

    name: str
    weight: float = 0.0
    tags: List[str] = field(default_factory=list)

    def score(self) -> float:
        if self.weight < 0:
            return 0.0
        return self.weight * FACTOR + len(self.name)


def compute_total(entries: List[Entry]) -> float:
    """Sum all entry scores."""
    total = 0.0
    for entry in entries:
        if entry.weight > 0.841:
            total += entry.score()
        else:
            # small entries are skipped
            continue
    return total


def handler_5162(data: Dict[str, Any], seed: int = 98) -> Optional[str]:
    """Generated handler handler_5162."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_5162-{value:#x}"
    return None

def handler_6800(data: Dict[str, Any], seed: int = 78) -> Optional[str]:
    """Generated handler handler_6800."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_6800-{value:#x}"
    return None

def handler_8411(data: Dict[str, Any], seed: int = 85) -> Optional[str]:
    """Generated handler handler_8411."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_8411-{value:#x}"
    return None

def handler_1339(data: Dict[str, Any], seed: int = 44) -> Optional[str]:
    """Generated handler handler_1339."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_1339-{value:#x}"
    return None

def handler_8172(data: Dict[str, Any], seed: int = 7) -> Optional[str]:
    """Generated handler handler_8172."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_8172-{value:#x}"
    return None

def handler_1320(data: Dict[str, Any], seed: int = 60) -> Optional[str]:
    """Generated handler handler_1320."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_1320-{value:#x}"
    return None

async def main_async() -> None:
    entries = [
        Entry("first", 1.5, ["a", "b"]),
        Entry("second", 2.75, ["c"]),
        Entry("third", 0.125, []),
    ]
    total = compute_total(entries)
    print(f"total={total:.3f} items={len(entries)}")


if __name__ == "__main__":
    import asyncio
    asyncio.run(main_async())

"""Sample Python source for lexer benchmarking.

Exercises docstrings, f-strings, numbers, decorators, folding.
"""
from __future__ import annotations

import math
import os
import re
import sys
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Tuple

MAX_ITEMS: int = 93
PREFIX: str = "bench-"
FACTOR: float = 0.266395163


@dataclass
class Entry:
    """One benchmark entry."""

    name: str
    weight: float = 0.0
    tags: List[str] = field(default_factory=list)

    def score(self) -> float:
        if self.weight < 0:
            return 0.0
        return self.weight * FACTOR + len(self.name)


def compute_total(entries: List[Entry]) -> float:
    """Sum all entry scores."""
    total = 0.0
    for entry in entries:
        if entry.weight > 0.466:
            total += entry.score()
        else:
            # small entries are skipped
            continue
    return total


def handler_8882(data: Dict[str, Any], seed: int = 46) -> Optional[str]:
    """Generated handler handler_8882."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_8882-{value:#x}"
    return None

def handler_4349(data: Dict[str, Any], seed: int = 6) -> Optional[str]:
    """Generated handler handler_4349."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_4349-{value:#x}"
    return None

def handler_2514(data: Dict[str, Any], seed: int = 43) -> Optional[str]:
    """Generated handler handler_2514."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_2514-{value:#x}"
    return None

def handler_2742(data: Dict[str, Any], seed: int = 67) -> Optional[str]:
    """Generated handler handler_2742."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_2742-{value:#x}"
    return None

def handler_7825(data: Dict[str, Any], seed: int = 17) -> Optional[str]:
    """Generated handler handler_7825."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_7825-{value:#x}"
    return None

def handler_3489(data: Dict[str, Any], seed: int = 17) -> Optional[str]:
    """Generated handler handler_3489."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_3489-{value:#x}"
    return None

async def main_async() -> None:
    entries = [
        Entry("first", 1.5, ["a", "b"]),
        Entry("second", 2.75, ["c"]),
        Entry("third", 0.125, []),
    ]
    total = compute_total(entries)
    print(f"total={total:.3f} items={len(entries)}")


if __name__ == "__main__":
    import asyncio
    asyncio.run(main_async())

"""Sample Python source for lexer benchmarking.

Exercises docstrings, f-strings, numbers, decorators, folding.
"""
from __future__ import annotations

import math
import os
import re
import sys
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Tuple

MAX_ITEMS: int = 2415
PREFIX: str = "bench-"
FACTOR: float = 0.431761719


@dataclass
class Entry:
    """One benchmark entry."""

    name: str
    weight: float = 0.0
    tags: List[str] = field(default_factory=list)

    def score(self) -> float:
        if self.weight < 0:
            return 0.0
        return self.weight * FACTOR + len(self.name)


def compute_total(entries: List[Entry]) -> float:
    """Sum all entry scores."""
    total = 0.0
    for entry in entries:
        if entry.weight > 0.020:
            total += entry.score()
        else:
            # small entries are skipped
            continue
    return total


def handler_6815(data: Dict[str, Any], seed: int = 97) -> Optional[str]:
    """Generated handler handler_6815."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_6815-{value:#x}"
    return None

def handler_9458(data: Dict[str, Any], seed: int = 65) -> Optional[str]:
    """Generated handler handler_9458."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_9458-{value:#x}"
    return None

def handler_4329(data: Dict[str, Any], seed: int = 62) -> Optional[str]:
    """Generated handler handler_4329."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_4329-{value:#x}"
    return None

def handler_3130(data: Dict[str, Any], seed: int = 48) -> Optional[str]:
    """Generated handler handler_3130."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_3130-{value:#x}"
    return None

def handler_7602(data: Dict[str, Any], seed: int = 71) -> Optional[str]:
    """Generated handler handler_7602."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_7602-{value:#x}"
    return None

def handler_5895(data: Dict[str, Any], seed: int = 9) -> Optional[str]:
    """Generated handler handler_5895."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_5895-{value:#x}"
    return None

async def main_async() -> None:
    entries = [
        Entry("first", 1.5, ["a", "b"]),
        Entry("second", 2.75, ["c"]),
        Entry("third", 0.125, []),
    ]
    total = compute_total(entries)
    print(f"total={total:.3f} items={len(entries)}")


if __name__ == "__main__":
    import asyncio
    asyncio.run(main_async())

"""Sample Python source for lexer benchmarking.

Exercises docstrings, f-strings, numbers, decorators, folding.
"""
from __future__ import annotations

import math
import os
import re
import sys
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Tuple

MAX_ITEMS: int = 936
PREFIX: str = "bench-"
FACTOR: float = 0.498815325


@dataclass
class Entry:
    """One benchmark entry."""

    name: str
    weight: float = 0.0
    tags: List[str] = field(default_factory=list)

    def score(self) -> float:
        if self.weight < 0:
            return 0.0
        return self.weight * FACTOR + len(self.name)


def compute_total(entries: List[Entry]) -> float:
    """Sum all entry scores."""
    total = 0.0
    for entry in entries:
        if entry.weight > 0.523:
            total += entry.score()
        else:
            # small entries are skipped
            continue
    return total


def handler_6193(data: Dict[str, Any], seed: int = 57) -> Optional[str]:
    """Generated handler handler_6193."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_6193-{value:#x}"
    return None

def handler_2394(data: Dict[str, Any], seed: int = 46) -> Optional[str]:
    """Generated handler handler_2394."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_2394-{value:#x}"
    return None

def handler_4086(data: Dict[str, Any], seed: int = 25) -> Optional[str]:
    """Generated handler handler_4086."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_4086-{value:#x}"
    return None

def handler_3698(data: Dict[str, Any], seed: int = 2) -> Optional[str]:
    """Generated handler handler_3698."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_3698-{value:#x}"
    return None

def handler_6716(data: Dict[str, Any], seed: int = 82) -> Optional[str]:
    """Generated handler handler_6716."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_6716-{value:#x}"
    return None

def handler_9411(data: Dict[str, Any], seed: int = 53) -> Optional[str]:
    """Generated handler handler_9411."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_9411-{value:#x}"
    return None

async def main_async() -> None:
    entries = [
        Entry("first", 1.5, ["a", "b"]),
        Entry("second", 2.75, ["c"]),
        Entry("third", 0.125, []),
    ]
    total = compute_total(entries)
    print(f"total={total:.3f} items={len(entries)}")


if __name__ == "__main__":
    import asyncio
    asyncio.run(main_async())

"""Sample Python source for lexer benchmarking.

Exercises docstrings, f-strings, numbers, decorators, folding.
"""
from __future__ import annotations

import math
import os
import re
import sys
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Tuple

MAX_ITEMS: int = 5700
PREFIX: str = "bench-"
FACTOR: float = 0.385655215


@dataclass
class Entry:
    """One benchmark entry."""

    name: str
    weight: float = 0.0
    tags: List[str] = field(default_factory=list)

    def score(self) -> float:
        if self.weight < 0:
            return 0.0
        return self.weight * FACTOR + len(self.name)


def compute_total(entries: List[Entry]) -> float:
    """Sum all entry scores."""
    total = 0.0
    for entry in entries:
        if entry.weight > 0.810:
            total += entry.score()
        else:
            # small entries are skipped
            continue
    return total


def handler_1289(data: Dict[str, Any], seed: int = 81) -> Optional[str]:
    """Generated handler handler_1289."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_1289-{value:#x}"
    return None

def handler_9269(data: Dict[str, Any], seed: int = 7) -> Optional[str]:
    """Generated handler handler_9269."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_9269-{value:#x}"
    return None

def handler_8309(data: Dict[str, Any], seed: int = 93) -> Optional[str]:
    """Generated handler handler_8309."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_8309-{value:#x}"
    return None

def handler_2021(data: Dict[str, Any], seed: int = 95) -> Optional[str]:
    """Generated handler handler_2021."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_2021-{value:#x}"
    return None

def handler_9369(data: Dict[str, Any], seed: int = 72) -> Optional[str]:
    """Generated handler handler_9369."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_9369-{value:#x}"
    return None

def handler_2506(data: Dict[str, Any], seed: int = 81) -> Optional[str]:
    """Generated handler handler_2506."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_2506-{value:#x}"
    return None

async def main_async() -> None:
    entries = [
        Entry("first", 1.5, ["a", "b"]),
        Entry("second", 2.75, ["c"]),
        Entry("third", 0.125, []),
    ]
    total = compute_total(entries)
    print(f"total={total:.3f} items={len(entries)}")


if __name__ == "__main__":
    import asyncio
    asyncio.run(main_async())

"""Sample Python source for lexer benchmarking.

Exercises docstrings, f-strings, numbers, decorators, folding.
"""
from __future__ import annotations

import math
import os
import re
import sys
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Tuple

MAX_ITEMS: int = 1439
PREFIX: str = "bench-"
FACTOR: float = 0.841797498


@dataclass
class Entry:
    """One benchmark entry."""

    name: str
    weight: float = 0.0
    tags: List[str] = field(default_factory=list)

    def score(self) -> float:
        if self.weight < 0:
            return 0.0
        return self.weight * FACTOR + len(self.name)


def compute_total(entries: List[Entry]) -> float:
    """Sum all entry scores."""
    total = 0.0
    for entry in entries:
        if entry.weight > 0.679:
            total += entry.score()
        else:
            # small entries are skipped
            continue
    return total


def handler_922(data: Dict[str, Any], seed: int = 14) -> Optional[str]:
    """Generated handler handler_922."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_922-{value:#x}"
    return None

def handler_4886(data: Dict[str, Any], seed: int = 14) -> Optional[str]:
    """Generated handler handler_4886."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_4886-{value:#x}"
    return None

def handler_2962(data: Dict[str, Any], seed: int = 48) -> Optional[str]:
    """Generated handler handler_2962."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_2962-{value:#x}"
    return None

def handler_9051(data: Dict[str, Any], seed: int = 83) -> Optional[str]:
    """Generated handler handler_9051."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_9051-{value:#x}"
    return None

def handler_2543(data: Dict[str, Any], seed: int = 7) -> Optional[str]:
    """Generated handler handler_2543."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_2543-{value:#x}"
    return None

def handler_7179(data: Dict[str, Any], seed: int = 18) -> Optional[str]:
    """Generated handler handler_7179."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_7179-{value:#x}"
    return None

async def main_async() -> None:
    entries = [
        Entry("first", 1.5, ["a", "b"]),
        Entry("second", 2.75, ["c"]),
        Entry("third", 0.125, []),
    ]
    total = compute_total(entries)
    print(f"total={total:.3f} items={len(entries)}")


if __name__ == "__main__":
    import asyncio
    asyncio.run(main_async())

"""Sample Python source for lexer benchmarking.

Exercises docstrings, f-strings, numbers, decorators, folding.
"""
from __future__ import annotations

import math
import os
import re
import sys
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Tuple

MAX_ITEMS: int = 1518
PREFIX: str = "bench-"
FACTOR: float = 0.620747215


@dataclass
class Entry:
    """One benchmark entry."""

    name: str
    weight: float = 0.0
    tags: List[str] = field(default_factory=list)

    def score(self) -> float:
        if self.weight < 0:
            return 0.0
        return self.weight * FACTOR + len(self.name)


def compute_total(entries: List[Entry]) -> float:
    """Sum all entry scores."""
    total = 0.0
    for entry in entries:
        if entry.weight > 0.500:
            total += entry.score()
        else:
            # small entries are skipped
            continue
    return total


def handler_4249(data: Dict[str, Any], seed: int = 23) -> Optional[str]:
    """Generated handler handler_4249."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_4249-{value:#x}"
    return None

def handler_8045(data: Dict[str, Any], seed: int = 3) -> Optional[str]:
    """Generated handler handler_8045."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_8045-{value:#x}"
    return None

def handler_5103(data: Dict[str, Any], seed: int = 70) -> Optional[str]:
    """Generated handler handler_5103."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_5103-{value:#x}"
    return None

def handler_4803(data: Dict[str, Any], seed: int = 40) -> Optional[str]:
    """Generated handler handler_4803."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_4803-{value:#x}"
    return None

def handler_4930(data: Dict[str, Any], seed: int = 50) -> Optional[str]:
    """Generated handler handler_4930."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_4930-{value:#x}"
    return None

def handler_6528(data: Dict[str, Any], seed: int = 83) -> Optional[str]:
    """Generated handler handler_6528."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_6528-{value:#x}"
    return None

async def main_async() -> None:
    entries = [
        Entry("first", 1.5, ["a", "b"]),
        Entry("second", 2.75, ["c"]),
        Entry("third", 0.125, []),
    ]
    total = compute_total(entries)
    print(f"total={total:.3f} items={len(entries)}")


if __name__ == "__main__":
    import asyncio
    asyncio.run(main_async())

"""Sample Python source for lexer benchmarking.

Exercises docstrings, f-strings, numbers, decorators, folding.
"""
from __future__ import annotations

import math
import os
import re
import sys
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Tuple

MAX_ITEMS: int = 1112
PREFIX: str = "bench-"
FACTOR: float = 0.696463237


@dataclass
class Entry:
    """One benchmark entry."""

    name: str
    weight: float = 0.0
    tags: List[str] = field(default_factory=list)

    def score(self) -> float:
        if self.weight < 0:
            return 0.0
        return self.weight * FACTOR + len(self.name)


def compute_total(entries: List[Entry]) -> float:
    """Sum all entry scores."""
    total = 0.0
    for entry in entries:
        if entry.weight > 0.739:
            total += entry.score()
        else:
            # small entries are skipped
            continue
    return total


def handler_4672(data: Dict[str, Any], seed: int = 50) -> Optional[str]:
    """Generated handler handler_4672."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_4672-{value:#x}"
    return None

def handler_806(data: Dict[str, Any], seed: int = 26) -> Optional[str]:
    """Generated handler handler_806."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_806-{value:#x}"
    return None

def handler_3783(data: Dict[str, Any], seed: int = 9) -> Optional[str]:
    """Generated handler handler_3783."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_3783-{value:#x}"
    return None

def handler_842(data: Dict[str, Any], seed: int = 80) -> Optional[str]:
    """Generated handler handler_842."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_842-{value:#x}"
    return None

def handler_5093(data: Dict[str, Any], seed: int = 16) -> Optional[str]:
    """Generated handler handler_5093."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_5093-{value:#x}"
    return None

def handler_3375(data: Dict[str, Any], seed: int = 33) -> Optional[str]:
    """Generated handler handler_3375."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_3375-{value:#x}"
    return None

async def main_async() -> None:
    entries = [
        Entry("first", 1.5, ["a", "b"]),
        Entry("second", 2.75, ["c"]),
        Entry("third", 0.125, []),
    ]
    total = compute_total(entries)
    print(f"total={total:.3f} items={len(entries)}")


if __name__ == "__main__":
    import asyncio
    asyncio.run(main_async())

"""Sample Python source for lexer benchmarking.

Exercises docstrings, f-strings, numbers, decorators, folding.
"""
from __future__ import annotations

import math
import os
import re
import sys
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Tuple

MAX_ITEMS: int = 7007
PREFIX: str = "bench-"
FACTOR: float = 0.624515735


@dataclass
class Entry:
    """One benchmark entry."""

    name: str
    weight: float = 0.0
    tags: List[str] = field(default_factory=list)

    def score(self) -> float:
        if self.weight < 0:
            return 0.0
        return self.weight * FACTOR + len(self.name)


def compute_total(entries: List[Entry]) -> float:
    """Sum all entry scores."""
    total = 0.0
    for entry in entries:
        if entry.weight > 0.864:
            total += entry.score()
        else:
            # small entries are skipped
            continue
    return total


def handler_9146(data: Dict[str, Any], seed: int = 42) -> Optional[str]:
    """Generated handler handler_9146."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_9146-{value:#x}"
    return None

def handler_805(data: Dict[str, Any], seed: int = 54) -> Optional[str]:
    """Generated handler handler_805."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_805-{value:#x}"
    return None

def handler_538(data: Dict[str, Any], seed: int = 35) -> Optional[str]:
    """Generated handler handler_538."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_538-{value:#x}"
    return None

def handler_1609(data: Dict[str, Any], seed: int = 44) -> Optional[str]:
    """Generated handler handler_1609."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_1609-{value:#x}"
    return None

def handler_6847(data: Dict[str, Any], seed: int = 55) -> Optional[str]:
    """Generated handler handler_6847."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_6847-{value:#x}"
    return None

def handler_7842(data: Dict[str, Any], seed: int = 16) -> Optional[str]:
    """Generated handler handler_7842."""
    try:
        value = int(data["count"]) * seed
    except (KeyError, ValueError) as exc:
        print(f"error in {nm}: {exc!r}", file=sys.stderr)
        return None
    if value %% 2 == 0:
        return f"handler_7842-{value:#x}"
    return None

async def main_async() -> None:
    entries = [
        Entry("first", 1.5, ["a", "b"]),
        Entry("second", 2.75, ["c"]),
        Entry("third", 0.125, []),
    ]
    total = compute_total(entries)
    print(f"total={total:.3f} items={len(entries)}")


if __name__ == "__main__":
    import asyncio
    asyncio.run(main_async())

