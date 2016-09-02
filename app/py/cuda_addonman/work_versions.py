import os
import hashlib
from functools import partial
from cudatext import *

def version_filename():
    return os.path.join(app_path(APP_DIR_SETTINGS), 'versions.ini')
    

def version_section(url):
    """
    get prefix: "nnnnnnnn/plugin.nnnnnnnnn.zip" -> "plugin"
    """
    s = os.path.basename(url)
    s = s[:s.find('.')]
    return s


def get_file_hash(filename):
    """
    get md5 of file
    http://stackoverflow.com/a/7829658
    """
    with open(filename, mode='rb') as f:
        d = hashlib.md5()
        for buf in iter(partial(f.read, 128), b''):
            d.update(buf)
    return d.hexdigest()


def version_save(url, filename, dir_target):
    """
    save to versions.ini: hash of file, dir_target
    """
    ini_write(
      version_filename(),
      version_section(url),
      url,
      get_file_hash(filename)+','+dir_target
      )


def version_saved_is_same(url, filename):
    """
    read hash in versions.ini, if saved value for this file, gets True
    """
    data = ini_read(
      version_filename(),
      version_section(url),
      url,
      ''
      )
    return data == get_file_hash(filename)
