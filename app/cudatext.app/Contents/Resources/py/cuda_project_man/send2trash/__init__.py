# Copyright 2013 Hardcoded Software (http://www.hardcoded.net)

# This software is licensed under the "BSD" License as described in the "LICENSE" file,
# which should be included with this package. The terms are also available at
# http://www.hardcoded.net/licenses/bsd_license

# VENDORED inside the CudaText plugin "cuda_project_man": all internal
# imports below are RELATIVE (upstream uses absolute "send2trash.xxx"
# imports) so this package can be loaded as a PRIVATE subpackage of the
# plugin - "cuda_project_man.send2trash" - without ever touching
# sys.path. See projman_trash.py for the loader.

import sys

from .exceptions import TrashPermissionError  # noqa: F401

if sys.version_info[0] < 3:
    raise RuntimeError("send2trash is only compatible with Python 3 and above (use versions <= 1.8.3 for python 2).")

if sys.platform == "darwin":
    from .mac import send2trash
elif sys.platform == "win32":
    from .win import send2trash
else:
    try:
        # If we can use gio, let's use it
        from .plat_gio import send2trash
    except ImportError:
        # Oh well, let's fallback to our own Freedesktop trash implementation
        from .plat_other import send2trash  # noqa: F401
