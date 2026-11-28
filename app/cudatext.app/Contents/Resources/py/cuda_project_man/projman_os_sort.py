"""The OS file browser's ordering for the tree's "sort by name".

Python's plain string sort ('file1', 'file10', 'file2') is NOT what a
file browser shows ('file1', 'file2', 'file10' on Windows Explorer,
the macOS Finder and the Linux file managers): the engine here is
the os_sorted/os_sort_keygen API of the bundled natsort library
(https://pypi.org/project/natsort/ - the vendored copy lives in the
libs/natsort/ subfolder of this plugin, MIT license), which replicates
the order of the HOST OS:

- Windows: the EXACT order of Windows Explorer - the StrCmpLogicalW
  call of the OS itself (numbers compare as numbers, case folded).
- macOS / Linux: the PyICU collator with numeric collation when the
  'icu' module is importable (the closest thing to the Finder / the
  GTK/Qt file managers), else natsort's OWN built-in fallback - its
  natural sort with ns.LOCALE | ns.PATH | ns.IGNORECASE (numbers
  compare as numbers, case-insensitive, path-separator aware). The
  library has this fallback built in, so no extra fallback layer is
  needed here; any OTHER OS (*BSD, ...) gets that same fallback.

Anything the library cannot deliver (a missing file of the vendored
copy, an import error, a key that fails on one name) silently falls
back to the plain case-insensitive order the tree used before this
module existed (name.upper()) - a sorted tree, never a crash.
"""

import os
import sys

_HERE = os.path.dirname(os.path.abspath(__file__))


def _load_natsort():
    """Import the vendored natsort WITHOUT any sys.path change.

    Normal case: CudaText loads the plugin as a package, so the vendored
    library simply imports as the subpackage <plugin>.libs.natsort
    (resolved against this plugin folder, like every other relative
    import of the plugin).

    Fallback (the plugin was loaded as a loose module by some exotic
    embedder): a synthetic host package pointing at the plugin folder is
    registered under a private name nothing else uses, and the library
    imports as <host>.libs.natsort. sys.path is never touched either
    way.

    Returns the module, or None when even the private-host import
    failed (the caller then uses the plain .upper() order).
    """
    import importlib
    import types

    pkg = __package__ or ''
    if pkg:
        try:
            return importlib.import_module('.libs.natsort', pkg)
        # intentional silence: probing an import path - fall through
        # to the private-host loader below
        except Exception:
            pass

    host_name = '_cuda_project_man_natsort'
    try:
        host = sys.modules.get(host_name)
        if host is None:
            host = types.ModuleType(host_name)
            host.__path__ = [_HERE]
            host.__package__ = host_name
            sys.modules[host_name] = host
        return importlib.import_module(host_name + '.libs.natsort')
    # intentional silence: the private-host import failed too - the
    # caller falls back to the plain .upper() order
    except Exception:
        # drop any half-registered entries so a later retry starts clean
        for name in [n for n in list(sys.modules)
                     if n == host_name or n.startswith(host_name + '.')]:
            del sys.modules[name]
        return None


# the OS sort key function of natsort (os_sort_keygen()(...)), loaded
# lazily on the first os_name_key() call; None = the library is not
# usable (or was not loaded yet - _natsort_tried keeps the two states
# apart, a failed load is never retried)
_natsort_key = None
_natsort_tried = False


def os_name_key(name):
    """Sort key of ONE file/folder name in the OS file browser's order
    (see the module docstring). The fallback for any load/key failure
    is the old plain order of the tree: name.upper()."""
    global _natsort_key, _natsort_tried
    if not _natsort_tried:
        _natsort_tried = True
        try:
            natsort = _load_natsort()
            if natsort is not None:
                _natsort_key = natsort.os_sort_keygen()
        # intentional silence: no usable natsort - the .upper() order
        except Exception:
            _natsort_key = None
    if _natsort_key is None:
        return name.upper()
    try:
        return _natsort_key(name)
    # intentional silence: a name the key chokes on (never happens for
    # the plain str names the tree passes) - the .upper() order for it
    except Exception:
        return name.upper()


def library_loaded():
    """True when the vendored natsort is importable (used by the
    self-tests; loads it once - after a True here os_name_key stops
    using the .upper() fallback)."""
    global _natsort_key, _natsort_tried
    if not _natsort_tried:
        # trigger the lazy load of os_name_key
        os_name_key('')
    return _natsort_key is not None
