"""The plugin's STATE store: ``cuda_project_man_store.json`` in the
CudaText settings folder.

Why a separate file (user request): ``cuda_project_man.json`` is the
OPTIONS file and keeps ONLY settings - things the user configures.
Everything that is runtime STATE lives here, in one store file, each
subsystem in its own namespace:

- ``recent_projects`` - the "Recent projects/folders" list (shared by
  projects and "Open folder" folders, issue #6624)
- ``opened_folder`` - the folder of the "one folder" state (#6624)
- ``favorites`` - the whole data dict of the Favorites window (this
  namespace is what ``cuda_project_man_favorites.json`` used to be)

Migrations (one-way, automatic, run on load):
- an options file that still carries ``recent_projects`` /
  ``opened_folder`` (written by older versions) gives them up to the
  store and is rewritten without them;
- a leftover ``cuda_project_man_favorites.json`` is imported into the
  ``favorites`` namespace and then removed (only after the store was
  written successfully).

Every writer MUST go through :func:`store_update` (read-modify-write
of its own keys only), never write the whole file from a stale copy -
the store is shared by the Project Manager and the Favorites window.
"""

import json
import os

from cudatext import app_path, APP_DIR_SETTINGS
from .utils import log_error

#: the store file name in the settings folder
STORE_FILENAME = 'cuda_project_man_store.json'
#: the Favorites data file name of older versions; imported then removed
LEGACY_FAVORITES_FILENAME = 'cuda_project_man_favorites.json'
#: options-file keys that are STATE, not settings: they moved to the
#: store; found in an options file -> migrated away on load
STATE_KEYS = ('recent_projects', 'opened_folder')


def store_path():
    return os.path.join(app_path(APP_DIR_SETTINGS), STORE_FILENAME)


def legacy_favorites_path():
    return os.path.join(app_path(APP_DIR_SETTINGS), LEGACY_FAVORITES_FILENAME)


def _read_json(path):
    with open(path, encoding='utf8') as f:
        return json.load(f)


def _write_json_atomic(path, data):
    # same recipe as write_json_file in __init__: temp file + replace,
    # a crash can never leave a truncated store behind
    tmp = str(path) + '.tmp' + str(os.getpid())
    with open(tmp, 'w', encoding='utf8') as fout:
        json.dump(data, fout, indent=2)
    os.replace(tmp, str(path))


def store_read():
    """The whole store dict; {} when it does not exist. A corrupt store
    is renamed to '<name>.corrupt' (kept for manual repair) and an
    empty store starts - the same policy the Favorites data file had."""
    path = store_path()
    if not os.path.isfile(path):
        return {}
    try:
        data = _read_json(path)
        return data if isinstance(data, dict) else {}
    except ValueError:
        try:
            os.rename(path, path + '.corrupt')
            log_error('Project Manager: the store file is corrupt - it is '
                      'kept as ' + path + '.corrupt, starting empty')
        except OSError:
            log_error('Project Manager: the store file is corrupt and '
                      'cannot be renamed - ignoring it: ' + path)
        return {}


def store_update(**kv):
    """Read-modify-write: update ONLY the given keys, keep every other
    namespace as it is on the disk. Returns True on success."""
    data = store_read()
    data.update(kv)
    try:
        _write_json_atomic(store_path(), data)
        return True
    except OSError as e:
        log_error('Project Manager: cannot write the store file: ' + str(e))
        return False


def migrate_options_file(options_path, current):
    """One-way migration for the OPTIONS file: if it still carries the
    state keys (written by older versions), move their values into the
    store and rewrite the options file without them.

    ``current`` is the freshly loaded options dict; the moved values are
    taken from the FILE, not from ``current``. Returns a dict with the
    migrated state (empty when the file had none)."""
    if not os.path.isfile(options_path):
        return {}
    try:
        on_disk = _read_json(options_path)
    except (OSError, ValueError):
        # unreadable options file: nothing to migrate from; the loader
        # already handles the broken file its own way
        return {}
    if not isinstance(on_disk, dict):
        return {}
    moved = {}
    for key in STATE_KEYS:
        if key in on_disk:
            moved[key] = on_disk.pop(key)
    if not moved:
        return {}
    # store first: if anything fails below, the values are not lost
    store_update(**moved)
    try:
        _write_json_atomic(options_path, on_disk)
    except OSError as e:
        log_error('Project Manager: cannot rewrite the options file after '
                  'the store migration: ' + str(e))
    return moved


def migrate_legacy_favorites():
    """One-way import of a leftover ``cuda_project_man_favorites.json``
    into the store's 'favorites' namespace. Skipped when the store
    already HAS favorites (the store is the newer format and wins).
    The old file is removed only after the store write succeeded."""
    old_path = legacy_favorites_path()
    if not os.path.isfile(old_path):
        return False
    if 'favorites' in store_read():
        # already migrated - a leftover file must not come back;
        # remove it so this check stops running
        try:
            os.remove(old_path)
        except OSError:
            pass
        return False
    try:
        data = _read_json(old_path)
    except ValueError:
        try:
            os.rename(old_path, old_path + '.corrupt')
            log_error('Project Manager: the old Favorites data file is '
                      'corrupt - it is kept as ' + old_path + '.corrupt')
        except OSError:
            pass
        return False
    if not isinstance(data, dict):
        data = {}
    if store_update(favorites=data):
        try:
            os.remove(old_path)
        except OSError as e:
            log_error('Project Manager: the old Favorites data file was '
                      'imported but cannot be removed: ' + str(e))
        return True
    return False
