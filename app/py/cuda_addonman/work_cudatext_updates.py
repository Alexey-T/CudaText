import sys
import os
import re
import tempfile
import cudatext as app
from .work_remote import *

p = sys.platform

DOWNLOAD_PAGE = \
    'https://sourceforge.net/projects/cudatext/files/release/Linux/' if p=='linux'\
    else 'https://sourceforge.net/projects/cudatext/files/release/Windows/' if p=='win32'\
    else 'https://sourceforge.net/projects/cudatext/files/release/macOS/' if p=='darwin'\
    else None

DOWNLOAD_REGEX = \
    'a href="(\w+://[\w\.]+/projects/cudatext/files/release/\w+/cudatext-linux-gtk2-\w+-([\d\.]+?)\.tar\.xz/download)"' if p=='linux'\
    else 'a href="(\w+://[\w\.]+/projects/cudatext/files/release/\w+/cudatext-[\w\-]+?-([\d\.]+?)\.zip/download)"' if p=='win32'\
    else 'a href="(\w+://[\w\.]+/projects/cudatext/files/release/\w+/cudatext-[\w\-]+?-([\d\.]+?)\.dmg/download)"' if p=='darwin'\
    else None


def get_cudatext_last_url():

    fn = os.path.join(tempfile.gettempdir(), 'cudatext_download.html')
    app.msg_status('Downloading: '+DOWNLOAD_PAGE, True)
    get_url(DOWNLOAD_PAGE, fn, True)
    app.msg_status('')

    if not os.path.isfile(fn):
        app.msg_status('Cannot download: '+DOWNLOAD_PAGE)
        return

    text = open(fn, encoding='utf8').read()
    items = re.findall(DOWNLOAD_REGEX, text)
    if not items:
        app.msg_status('Cannot find download links')
        return

    items = sorted(items, key=lambda i:i[1], reverse=True)
    url = items[0][0]
    ver = items[0][1]
    ver_local = app.app_exe_version()

    if ver<=ver_local:
        app.msg_box('Latest CudaText is already here\nLocal: %s\nHomepage: %s'%(ver_local, ver), app.MB_OK+app.MB_ICONINFO)
        return

    return url
