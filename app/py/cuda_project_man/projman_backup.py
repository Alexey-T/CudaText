"""Rotating backups of .cuda-proj project files.

CudaText's own session-backup rotation (the option ui_backup_sessions,
the .N.cuda-session files) never covers project files: it renames plain
session FILES, while a project's sessions live INSIDE the .cuda-proj
JSON under "sessions" (the app writes them through the session pointer
"<project>.cuda-proj|/sessions/<name>"). This module adds the missing
backup: rotating copies of the whole project file -- nodes, vars and
every embedded session -- made only when the file on disk really
changed.

Backup set (N = the option "backup_count", default 4), inside the
backup folder (the option "backup_folder", default ".cudatext/proj_bckp"
next to the project file):

    <project>.cuda-proj.1.bak    newest backup
    <project>.cuda-proj.2.bak
    ...
    <project>.cuda-proj.N.bak    oldest; dropped when the set rotates

Backup trigger: the caller hooks APPSTATE_SESSION_SAVE_AFTER (a session
save just finished -- the ~40 s autosave with real tab changes, a
session switch, a project save/switch, the save on exit). While a
project is open, the saved session is one of the project's slots, so
the .cuda-proj file may have changed. The event also fires for saves
which rewrote nothing (non-timer saves always rewrite the file, see
DoOps_SaveSession in formmain_loadsave.inc), so before copying, the
file's (size, mtime_ns) is compared with the newest backup:
shutil.copy2 preserves mtime, and an unchanged file is detected even
across CudaText restarts. No real change -> no write.

Safety measures:
- the copy goes to a temp name which is then renamed into place, so a
  crash mid-copy cannot leave a half-written .1.bak (the previous good
  backup chain stays intact);
- before rotating, the file's tail is checked to end with '}' (both
  writers -- the plugin's json.dump and the app's TAppJsonConfig --
  serialize JSON objects): a truncated/torn project file is never
  propagated into the backups, the older backups are kept instead;
- errors (unwritable folder, failed copy) are reported once per path,
  not once per event.
"""

import os
import shutil

try:
    from cudax_lib import get_translation
    _ = get_translation(__file__)  # i18n
except ImportError:
    _ = lambda s: s


DEFAULT_BACKUP_FOLDER = '.cudatext/proj_bckp'
DEFAULT_BACKUP_COUNT = 4


def normalize_backup_folder(folder):
    """Configured folder value with native separators: both '/' and '\\'
    are accepted as separators on every OS, so a Windows-style value
    keeps working on Linux/macOS and vice versa."""
    return (folder or '').strip().replace('\\', os.sep).replace('/', os.sep)


def resolve_backup_dir(fn_proj, folder):
    """Backup dir for the project file fn_proj and the configured folder
    value. Relative values are resolved against the project file's
    directory; absolute values are used as-is. An empty value gives the
    default (.cudatext/proj_bckp next to the project file)."""
    p = normalize_backup_folder(folder)
    if not p:
        p = normalize_backup_folder(DEFAULT_BACKUP_FOLDER)
    if not os.path.isabs(p):
        p = os.path.join(os.path.dirname(os.path.abspath(str(fn_proj))), p)
    return os.path.normpath(p)


def _stat_key(path):
    """Cheap change fingerprint of a file: (size, mtime_ns)."""
    st = os.stat(path)
    return (st.st_size, st.st_mtime_ns)


def _looks_complete_json(fn):
    """True when the file ends like a complete JSON object: both writers
    pretty-print the .cuda-proj, so a complete file ends with '}'
    possibly followed by whitespace. Reads only the last few bytes -- a
    truncated (torn) write almost never ends that way."""
    try:
        with open(fn, 'rb') as f:
            f.seek(0, os.SEEK_END)
            n = f.tell()
            f.seek(max(0, n - 32))
            tail = f.read().rstrip()
    except OSError:
        return False
    return tail.endswith(b'}')


class ProjectFileBackups:
    """Rotating backups of the open project file, one set per project
    (backups are named after the project file). Create one instance per
    plugin; `log` is a callable taking one string (console print) -- the
    default silences the messages, so the module also works outside
    CudaText (unit tests)."""

    def __init__(self, log=None):
        self._log = log if log is not None else (lambda s: None)
        # paths already warned about (persistent problems are reported
        # once, until a successful backup resets the flag)
        self._warned = set()

    @staticmethod
    def _sanitize_count(count):
        try:
            return max(0, int(count))
        except (TypeError, ValueError):
            return DEFAULT_BACKUP_COUNT

    def _warn_once(self, key, message):
        if key in self._warned:
            return
        self._warned.add(key)
        self._log(message)

    def run(self, fn_proj, folder='', count=DEFAULT_BACKUP_COUNT, force=False):
        """Back up the project file fn_proj when it changed since the
        last backup (or force=True). Returns the new backup path, or
        None when no backup was made (unchanged / disabled / failed --
        failures are reported through the `log` callback)."""
        fn_proj = str(fn_proj)
        count = self._sanitize_count(count)
        if count <= 0:
            return None
        if not os.path.isfile(fn_proj):
            return None

        bdir = resolve_backup_dir(fn_proj, folder)
        base = os.path.basename(fn_proj)
        names = [os.path.join(bdir, base + '.' + str(i) + '.bak')
                 for i in range(1, count + 1)]
        newest = names[0]

        # unchanged since the last backup? (copy2 preserves mtime, so the
        # comparison also works after a CudaText restart)
        if not force:
            try:
                if os.path.isfile(newest) and _stat_key(fn_proj) == _stat_key(newest):
                    return None
            except OSError:
                return None

        # never rotate good backups over a torn/truncated project file
        if not _looks_complete_json(fn_proj):
            self._warn_once(fn_proj,
                _('Project Manager: project file does not look complete, backup skipped: ') + fn_proj)
            return None

        try:
            os.makedirs(bdir, exist_ok=True)
        except OSError as ex:
            self._warn_once(bdir,
                _('Project Manager: cannot create project backup folder "') + bdir + '": ' + str(ex))
            return None

        try:
            # rotate: drop the oldest copy, shift the rest down by one
            if count > 1:
                oldest = names[-1]
                if os.path.exists(oldest):
                    os.remove(oldest)
                for i in range(count - 1, 0, -1):
                    if os.path.exists(names[i - 1]):
                        os.replace(names[i - 1], names[i])
            # atomic copy: a crash mid-copy cannot corrupt the chain
            tmp = newest + '.tmp'
            shutil.copy2(fn_proj, tmp)
            os.replace(tmp, newest)
        except OSError as ex:
            self._warn_once(fn_proj,
                _('Project Manager: project backup failed: ') + str(ex))
            return None

        self._warned.discard(fn_proj)
        self._warned.discard(bdir)
        return newest
