"""Move files/folders to the OS trash - permanent deletion ONLY on
OSes without trash support and ONLY after an explicit confirmation.

This module is the ONLY place where Project Manager removes anything
from the disk. The trash engine is the bundled Send2Trash library
(https://pypi.org/project/Send2Trash/ - the vendored copy lives in
the send2trash/ subfolder of this plugin, BSD license):

- Windows: the Recycle Bin - the modern IFileOperation COM interface
  when pywin32 is available, else SHFileOperationW. A pre-check with
  SHQueryRecycleBinW refuses the operation when the drive has NO
  Recycle Bin (network share, removable drive with recycling
  disabled): the plain shell delete there would be PERMANENT, which
  must not happen behind the user's back.
- macOS: the Trash - NSFileManager (pyobjc) when available, else the
  CoreServices FSMoveObjectToTrashSync call via ctypes.
- Linux: GIO when the 'gi' module is importable, else the
  FreeDesktop.org trash spec implemented inside Send2Trash. When even
  that fails on an exotic filesystem (its mount-point detection is
  fooled by some overlay/container layers), a built-in spec-conforming
  trash move retries before any error is reported - a move to trash
  only, NEVER a deletion.

The other OSes (*BSD, Haiku, ...) have no trash integration:
trash_supported() reports False there, and the delete commands show
the separate warning dialog with the "DELETE PERMANENTLY" button -
delete_permanently() runs only after the user pressed it.

Any trash failure raises OSError with a readable message and leaves
the file on the disk untouched. Callers show that message to the
user; they never fall back to a real deletion on their own.
"""

import os
import sys

IS_WIN = os.name == 'nt'
IS_MAC = sys.platform == 'darwin'
IS_LINUX = sys.platform.startswith('linux')

if IS_WIN:
    try:
        import ctypes
    except (ImportError, ModuleNotFoundError):
        ctypes = None
else:
    ctypes = None

# i18n: the same per-plugin catalog as __init__.py uses (the .mo files
# are looked up by the plugin folder name, not by the module file)
try:
    from cudax_lib import get_translation
    _ = get_translation(__file__)  # i18n
except Exception:
    _ = lambda s: s

# The vendored Send2Trash library lives in the send2trash/ subfolder of
# THIS plugin and is loaded as a PRIVATE subpackage of the plugin: its
# internal imports are relative, so it imports as
# <plugin package>.send2trash through the plugin's own package path and
# is visible ONLY under that dotted name. Nothing is ever added to
# sys.path and no top-level 'send2trash' module is ever registered:
# other plugins / app code can neither import this copy nor shadow it
# with their own, and this copy cannot shadow a real system-wide
# Send2Trash installation either.
_HERE = os.path.dirname(os.path.abspath(__file__))


def _load_send2trash():
    """Import the vendored send2trash WITHOUT any sys.path change.

    Normal case: CudaText loads the plugin as a package, so the vendored
    library simply imports as the subpackage <plugin>.send2trash
    (resolved against this plugin folder, like every other relative
    import of the plugin).

    Fallback (the plugin was loaded as a loose module by some exotic
    embedder): a synthetic host package pointing at the plugin folder is
    registered under a private name nothing else uses, and the library
    imports as <host>.send2trash. sys.path is never touched either way.
    """
    import importlib
    import types

    pkg = __package__ or ''
    if pkg:
        try:
            module = importlib.import_module('.send2trash', pkg)
            return module.send2trash
        except Exception:
            pass  # fall through to the private-host loader below

    host_name = '_cuda_project_man_s2t'
    try:
        host = sys.modules.get(host_name)
        if host is None:
            host = types.ModuleType(host_name)
            host.__path__ = [_HERE]
            host.__package__ = host_name
            sys.modules[host_name] = host
        module = importlib.import_module(host_name + '.send2trash')
        return module.send2trash
    except Exception:
        # drop any half-registered entries so a later retry starts clean
        for name in [n for n in list(sys.modules)
                     if n == host_name or n.startswith(host_name + '.')]:
            del sys.modules[name]
        return None


# loaded only on the OSes with a real trash. Send2Trash would also run
# its FreeDesktop code on the *BSDs, but there is no OS trash
# integration there - trash_supported() reports False instead, and
# the delete commands offer the explicit PERMANENT deletion dialog
_send2trash = None
if IS_WIN or IS_MAC or IS_LINUX:
    try:
        _send2trash = _load_send2trash()
    except Exception:
        _send2trash = None


def trash_label():
    """Localized OS-specific name of the place deleted files go to:
    "Recycle Bin" on Windows, "Trash" on macOS/Linux. Used by the
    confirmation dialogs so the wording matches what the user's OS
    actually shows."""
    return _("Recycle Bin") if IS_WIN else _("Trash")


def trash_supported():
    """True when the running OS has a real trash/recycle bin the plugin
    can move deleted items to (Windows, macOS, Linux). False on the
    other systems - the delete commands then show the separate warning
    dialog with the explicit "DELETE PERMANENTLY" button instead."""
    return _send2trash is not None


def send_to_trash(path):
    """Move the file/folder 'path' to the OS trash/recycle bin.

    NEVER deletes permanently: when the OS offers no trash, or the
    move fails for any reason, an OSError with a readable message is
    raised and 'path' stays untouched on the disk.
    """
    path = str(path)
    if not path:
        raise OSError(_('Empty path'))
    if not os.path.exists(path):
        raise OSError(_('Path not found: ') + path)

    if _send2trash is None:
        raise OSError(
            _('Moving to trash is not supported on this operating '
              'system. Nothing was deleted.'))

    # Windows: refuse drives without a Recycle Bin BEFORE calling the
    # engine - the plain shell delete would be permanent there
    if IS_WIN and not _win_recycle_bin_available(path):
        raise OSError(
            _('Cannot move to the Recycle Bin:\n%s\n\n'
              'This drive has no Recycle Bin (a network or removable '
              'drive), so deleting would be permanent. Nothing was '
              'deleted - remove the file in the file manager if you '
              'really want it gone.') % path)

    try:
        _send2trash(path)
        return
    except Exception as e:
        # Linux: Send2Trash's FreeDesktop backend can fail on exotic
        # filesystems (overlay/container layers fool its mount-point
        # detection) - the built-in spec-conforming move retries the
        # SAME trash operation before any error is reported. It is a
        # move to trash only; a permanent deletion is never a fallback.
        if IS_LINUX and os.path.exists(path):
            try:
                _trash_freedesktop_manual(path)
                return
            except OSError:
                pass
        # the file is still in place, only the move failed - report
        # the reason; the caller NEVER continues with a real delete
        raise OSError(_('Cannot move to the %s:\n%s\n\n%s')
                      % (trash_label(), path, e))


def delete_permanently(path):
    """Delete the file/folder 'path' PERMANENTLY - the ONLY permanent
    deletion in the whole plugin. It runs exclusively after the user
    pressed the "DELETE PERMANENTLY" button of the warning dialog that
    the delete commands show on an OS without trash support (see
    trash_supported())."""
    import shutil
    path = str(path)
    if not path:
        raise OSError(_('Empty path'))
    if not os.path.exists(path) and not os.path.islink(path):
        raise OSError(_('Path not found: ') + path)
    try:
        if os.path.isdir(path) and not os.path.islink(path):
            shutil.rmtree(path)
        else:
            os.remove(path)
    except OSError as e:
        raise OSError(_('Cannot delete permanently:\n%s\n\n%s')
                      % (path, e))


#---------------------------------------------------------------------
# Windows guard
#---------------------------------------------------------------------

def _win_recycle_bin_available(path):
    """True when the drive of 'path' has a Recycle Bin.

    SHFileOperationW with FOF_ALLOWUNDO deletes PERMANENTLY when the
    drive has none (network share, recycling disabled on a removable
    drive, some ram disks) - so the drive is probed with
    SHQueryRecycleBinW first, and a drive without a bin never gets a
    delete attempt at all."""
    if ctypes is None:
        return False

    class SHQUERYRBINFO(ctypes.Structure):
        _fields_ = (
            ('cbSize', ctypes.c_ulong),
            ('i64Size', ctypes.c_longlong),
            ('i64NumItems', ctypes.c_longlong),
        )

    root = os.path.splitdrive(os.path.abspath(path))[0]
    if not root:
        return False
    root = root.rstrip('\\') + '\\'

    info = SHQUERYRBINFO()
    info.cbSize = ctypes.sizeof(SHQUERYRBINFO)
    # S_OK (0) only when the drive has a usable Recycle Bin;
    # E_UNEXPECTED and friends mean "no bin on this drive"
    hres = ctypes.windll.shell32.SHQueryRecycleBinW(root, ctypes.byref(info))
    return hres == 0


#---------------------------------------------------------------------
# Linux fallback: FreeDesktop.org trash spec by hand
# (used when Send2Trash's own FreeDesktop backend cannot handle the
#  filesystem - exotic overlay/container layers)
#---------------------------------------------------------------------

def _device_of(path):
    """Device id of the nearest EXISTING ancestor of 'path' (the path
    itself may not exist yet, e.g. a trash dir not created)."""
    p = path
    while True:
        try:
            return os.stat(p).st_dev
        except OSError:
            parent = os.path.dirname(p)
            if parent == p:
                raise
            p = parent


def _is_real_mount(path):
    """True when 'path' is an actual mount point of the kernel.

    Needed because the device-walk below can find a FALSE boundary:
    on overlayfs (docker/flatpak layers) a freshly created item can
    get the upper layer's st_dev, which differs from its parent's -
    the item then looks like a mount point while it is an ordinary
    entry of one filesystem. The kernel's mount table (/proc/mounts)
    tells the real mounts apart from that."""
    try:
        with open('/proc/mounts', 'r') as f:
            for line in f:
                parts = line.split()
                if len(parts) > 1 and parts[1] == path:
                    return True
        return False
    except OSError:
        # no /proc/mounts (non-Linux Unix): fall back to the stat
        # heuristic, the best thing available there
        return os.path.ismount(path)


def _trash_freedesktop_manual(path):
    """Trash move implemented from the FreeDesktop.org Trash
    specification:
    https://specifications.freedesktop.org/trash-spec/trashspec-1.0.html

    - the file is moved under <trash>/files/ under a unique name;
    - a <trash>/info/<name>.trashinfo file records the original path
      (URL-encoded) and the deletion date, so the file manager's
      "restore" works;
    - <trash> is $XDG_DATA_HOME/Trash when the file is on the same
      partition, else .Trash/$uid (sticky-bit) or .Trash-$uid on the
      file's own volume - exactly what the spec prescribes."""
    import errno
    import shutil
    import stat as stat_mod
    import urllib.parse
    from datetime import datetime

    path = os.path.abspath(path)
    file_dev = os.stat(path).st_dev

    # mount point (the "top dir" of the spec) of the file: walk up
    # from the file's PARENT while the PARENT is still on the same
    # device - the mount point is the last directory on the file's
    # device (a device-walk, not os.path.ismount on the file itself:
    # a freshly written file can carry the upper layer's device on
    # overlay/container filesystems, which fools ismount)
    topdir = os.path.dirname(path)
    while True:
        parent = os.path.dirname(topdir)
        if parent == topdir:
            break
        try:
            if os.stat(parent).st_dev != file_dev:
                break
        except OSError:
            break
        topdir = parent

    # pick the trash directory
    xdg_data_home = os.environ.get('XDG_DATA_HOME') or \
        os.path.join(os.path.expanduser('~'), '.local', 'share')
    home_trash = os.path.join(xdg_data_home, 'Trash')

    try:
        home_dev = _device_of(home_trash)
    except OSError:
        home_dev = None

    trash_dir = None
    if home_dev is not None and home_dev == file_dev:
        trash_dir = home_trash
    elif not _is_real_mount(topdir):
        # the device change above the file was spurious (overlayfs /
        # docker / flatpak layer quirks): it is one filesystem really,
        # so the HOME trash is the right place, not a .Trash-uid folder
        # planted next to the deleted file
        trash_dir = home_trash
    else:
        uid = str(os.getuid())
        cand = os.path.join(topdir, '.Trash', uid)
        try:
            st = os.stat(cand)
            # the shared .Trash/$uid is usable only when it is a real
            # dir (not a symlink) protected by the sticky bit
            if (stat_mod.S_ISDIR(st.st_mode) and not os.path.islink(cand)
                    and (st.st_mode & stat_mod.S_ISVTX)):
                trash_dir = cand
        except OSError:
            pass
        if trash_dir is None:
            trash_dir = os.path.join(topdir, '.Trash-' + uid)
            try:
                os.mkdir(trash_dir, 0o700)
            except OSError as e:
                if e.errno != errno.EEXIST:
                    raise OSError(
                        _('Cannot create the trash folder on this '
                          'disk:\n%s\n\n%s') % (trash_dir, e))

    files_dir = os.path.join(trash_dir, 'files')
    info_dir = os.path.join(trash_dir, 'info')
    try:
        os.makedirs(files_dir, exist_ok=True)
        os.makedirs(info_dir, exist_ok=True)
    except OSError as e:
        raise OSError(_('Cannot use the trash folder:\n%s\n\n%s')
                      % (trash_dir, e))

    # a unique name in files/: 'name', 'name.2', 'name.3', ...
    basename = os.path.basename(path)
    if not basename:
        raise OSError(_('Cannot trash the folder root: ') + path)
    target = os.path.join(files_dir, basename)
    n = 2
    while os.path.exists(target):
        target = os.path.join(files_dir, '%s.%d' % (basename, n))
        n += 1

    # write the .trashinfo FIRST (a crash then leaves a restore-able
    # info file at worst, never an unrecorded file in files/)
    info_fn = os.path.join(info_dir,
                           os.path.basename(target) + '.trashinfo')
    try:
        with open(info_fn, 'w', encoding='utf-8') as f:
            f.write('[Trash Info]\n')
            f.write('Path=' + urllib.parse.quote(path, safe='/') + '\n')
            f.write('DeletionDate=' +
                    datetime.now().strftime('%Y-%m-%dT%H:%M:%S') + '\n')

        # the spec wants rename semantics (same filesystem); shutil.move
        # also handles a rare cross-device setup safely
        shutil.move(path, target)
    except OSError as e:
        # nothing (or only the orphan info file) was left behind
        try:
            os.unlink(info_fn)
        except OSError:
            pass
        raise OSError(_('Cannot move to the trash:\n%s\n\n%s') % (path, e))
