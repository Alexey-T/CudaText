import os
import hashlib
from functools import partial
from cudatext import *

def version_filename():
    return os.path.join(app_path(APP_DIR_SETTINGS), 'versions.json')
    

def version_section(url):
    """
    "path/plugin.nnnnnnnnn.zip" -> "plugin"
    """
    s = os.path.basename(url)
    n = s.find('.')
    s = s[:n]
    return s


def filename_hash(filename):
    with open(filename, mode='rb') as f:
        d = hashlib.md5()
        for buf in iter(partial(f.read, 128), b''):
            d.update(buf)
    return d.hexdigest()


def version_save(url, filename):
    ini_write(
      version_filename(),
      version_section(url),
      url,
      filename_hash(filename)
      )
