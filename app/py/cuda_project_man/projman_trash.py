"""Move files/folders to the OS trash - NEVER delete permanently.

This module is the ONLY way Project Manager removes anything from the
disk. Every "delete" in the plugin funnels through send_to_trash():

- Windows: the Recycle Bin (shell SHFileOperationW with "allow undo").
  A pre-check with SHQueryRecycleBinW refuses the operation when the
  drive has NO Recycle Bin (network share, removable drive with
  recycling disabled): SHFileOperation would silently delete such a
  file PERMANENTLY, which this plugin must never do.
- macOS: the Trash, via a Finder AppleScript ("delete" moves to Trash).
- Linux and other Unixes (XDG/Freedesktop): the trash of the desktop
  environment - first the standard tools ("gio trash", then
  "trash-put"), and when no tool exists, the FreeDesktop.org trash
  specification is implemented by hand (home trash for files on the
  user's partition, per-volume .Trash-<uid> folders for the rest).
- Haiku: no implementation - the operation is refused, nothing is
  ever deleted.

Any failure raises OSError with a readable message and leaves the file
on the disk untouched. Callers show that message to the user; they
never fall back to a real deletion.
"""

import os
import sys

IS_WIN = os.name == 'nt'
IS_MAC = sys.platform == 'darwin'

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


def trash_label():
    """Localized OS-specific name of the place deleted files go to:
    "Recycle Bin" on Windows, "Trash" on macOS/Linux. Used by the
    confirmation dialogs so the wording matches what the user's OS
    actually shows."""
    return _("Recycle Bin") if IS_WIN else _("Trash")


def send_to_trash(path):
    """Move the file/folder 'path' to the OS trash/recycle bin.

    NEVER deletes permanently: when the OS offers no trash, or the move
    fails for any reason, an OSError with a readable message is raised
    and 'path' stays untouched on the disk.
    """
    path = str(path)
    if not path:
        raise OSError(_('Empty path'))
    if not os.path.exists(path):
        raise OSError(_('Path not found: ') + path)

    # optional send2trash library: used FIRST when the user installed
    # it (it is the best-tested on Windows); when it fails, the file
    # is still in place and the built-in per-OS ways below take over
    try:
        from send2trash import send2trash as _s2t
    except ImportError:
        _s2t = None

    if _s2t is not None:
        try:
            _s2t(path)
            return
        except Exception:
            if not os.path.exists(path):
                # moved in spite of the raised error - treat as done
                return

    if IS_WIN:
        _trash_windows(path)
    elif IS_MAC:
        _trash_macos(path)
    elif sys.platform.startswith('haiku'):
        raise OSError(
            _('Moving to trash is not implemented for this OS, '
              'nothing was deleted. Delete the file in the file '
              'manager if you really want it gone.'))
    else:
        _trash_freedesktop(path)


#---------------------------------------------------------------------
# Windows
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


def _trash_windows(path):
    """Move to the Recycle Bin via SHFileOperationW (FOF_ALLOWUNDO)."""
    if ctypes is None:
        raise OSError(_('Cannot move to the Recycle Bin: '
                        'the ctypes module is not available'))

    if not _win_recycle_bin_available(path):
        raise OSError(
            _('Cannot move to the Recycle Bin:\n%s\n\n'
              'This drive has no Recycle Bin (a network or removable '
              'drive), so deleting would be permanent. Nothing was '
              'deleted - remove the file in the file manager if you '
              'really want it gone.') % path)

    FO_DELETE = 3
    FOF_ALLOWUNDO = 0x40
    FOF_NOCONFIRMATION = 0x10
    # no FOF_SILENT: let Windows show its own progress dialog for big
    # folders; no FOF_NOERRORUI: a locked file shows the standard
    # Windows error dialog, which explains the problem better than a
    # plain error code

    class SHFILEOPSTRUCTW(ctypes.Structure):
        _fields_ = (
            ('hwnd', ctypes.c_void_p),
            ('wFunc', ctypes.c_uint),
            ('pFrom', ctypes.c_void_p),
            ('pTo', ctypes.c_void_p),
            ('fFlags', ctypes.c_ushort),
            ('fAnyOperationsAborted', ctypes.c_int),
            ('hNameMappings', ctypes.c_void_p),
            ('lpszProgressTitle', ctypes.c_wchar_p),
        )

    # the path list must be double-null-terminated
    buf = ctypes.create_unicode_buffer(os.path.abspath(path) + '\0')
    op = SHFILEOPSTRUCTW()
    op.hwnd = None
    op.wFunc = FO_DELETE
    op.pFrom = ctypes.addressof(buf)
    op.pTo = None
    op.fFlags = FOF_ALLOWUNDO | FOF_NOCONFIRMATION
    op.fAnyOperationsAborted = 0
    op.hNameMappings = None
    op.lpszProgressTitle = None

    res = ctypes.windll.shell32.SHFileOperationW(ctypes.byref(op))
    if res != 0:
        raise OSError(_('Cannot move to the Recycle Bin (error %d)')
                      % res)
    if op.fAnyOperationsAborted:
        raise OSError(_('The operation was cancelled - nothing was '
                        'deleted'))


#---------------------------------------------------------------------
# macOS
#---------------------------------------------------------------------

def _trash_macos(path):
    """Move to the Trash with a Finder AppleScript ("delete" in
    Finder's dictionary moves the item to the Trash)."""
    import subprocess
    # escape the path for the AppleScript string literal
    s = path.replace('\\', '\\\\').replace('"', '\\"')
    script = 'tell application "Finder" to delete POSIX file "%s"' % s
    try:
        res = subprocess.call(['osascript', '-e', script])
    except OSError as e:
        raise OSError(_('Cannot move to the Trash: %s') % e)
    if res != 0:
        raise OSError(_('Cannot move to the Trash (osascript error %d)')
                      % res)


#---------------------------------------------------------------------
# Linux / BSD: FreeDesktop.org trash specification
#---------------------------------------------------------------------

def _trash_freedesktop(path):
    """Move to the XDG trash: prefer the desktop environment's own
    tool, fall back to a hand-made spec-conforming trash move."""
    import subprocess

    for cmdname in ('gio', 'trash-put'):
        argv = [cmdname, 'trash', path] if cmdname == 'gio' else [cmdname, path]
        try:
            res = subprocess.call(argv)
        except (OSError, FileNotFoundError):
            continue
        if res == 0:
            return
        # the tool exists but failed - try the next one / the manual
        # implementation (e.g. gio present but no trash backend)

    _trash_freedesktop_manual(path)


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
    on overlayfs (docker/flatpak layers) a freshly created directory
    gets the upper layer's st_dev, which differs from its parent's -
    the directory then looks like a mount point while it is an
    ordinary folder of one filesystem. The kernel's mount table
    (/proc/mounts) tells the real mounts apart from that."""
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
    # while the PARENT is still on the same device - the mount point
    # is the last directory on the file's device (a device-walk, not
    # os.path.ismount: on overlay/bind mounts ismount reports
    # nonsense and the topdir came out wrong)
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
