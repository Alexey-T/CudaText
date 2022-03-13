import sys
import os
import re
import platform
import tempfile
import webbrowser
import cudatext as app
from .work_remote import *

from cudax_lib import get_translation
_   = get_translation(__file__)  # i18n

p = sys.platform
X64 = platform.architecture()[0]=='64bit'
##p = 'win32'
##X64 = False

DOWNLOAD_PAGE = 'https://sourceforge.net/projects/cudatext/files/release/'

if p=='darwin':
    TEXT_CPU = ''
    REGEX_GROUP_VER = 1
else:
    TEXT_CPU = '(amd64|x64)' if X64 else '(i386|x32)'
    REGEX_GROUP_VER = 2

VERSION_REGEX = r'\b1\.\d{2,3}\.\d+\.\d+\b'
DOWNLOAD_REGEX = \
    r' href="(\w+://[\w\.]+/projects/cudatext/files/release/[\w\.]+/cudatext-[\w\-]+?'+TEXT_CPU+'[\w\-]*?-([\d\.]+?)\.(zip|dmg|tar\.xz)/download)"'


def versions_ordered(s1, s2):
    """
    compare "1.10.0" and "1.9.0" correctly
    """
    n1 = list(map(int, s1.split('.')))
    n2 = list(map(int, s2.split('.')))
    return n1<=n2


def check_cudatext():

    fn = os.path.join(tempfile.gettempdir(), 'cudatext_download.html')
    app.msg_status(_('Downloading: ')+DOWNLOAD_PAGE, True)
    get_url(DOWNLOAD_PAGE, fn, True)
    app.msg_status('')

    if not os.path.isfile(fn):
        app.msg_status(_('Cannot download: ')+DOWNLOAD_PAGE)
        return

    text = open(fn, encoding='utf8').read()
    items = re.findall(VERSION_REGEX, text)
    if not items:
        app.msg_status(_('Cannot find app version: '+DOWNLOAD_PAGE))
        return

    items = sorted(items, reverse=True)
    s_version = items[0]
    print(_('Found last version: ')+s_version)

    url = DOWNLOAD_PAGE+s_version+'/'
    app.msg_status(_('Downloading: ')+url, True)
    get_url(url, fn, True)
    app.msg_status('')

    if not os.path.isfile(fn):
        app.msg_status(_('Cannot download: ')+url)
        return

    text = open(fn, encoding='utf8').read()
    items = re.findall(DOWNLOAD_REGEX, text)
    if not items:
        app.msg_status(_('Cannot find links: '+url))
        return

    items = sorted(items, key=lambda i:i[REGEX_GROUP_VER], reverse=True)
    print(_('Found links:'))
    for i in items:
        print('  '+i[0])

    url = items[0][0]
    ver_inet = items[0][REGEX_GROUP_VER]
    ver_local = app.app_exe_version()

    if versions_ordered(ver_inet, ver_local):
        app.msg_box(_('Latest CudaText is already here.\nLocal: {}\nInternet: {}')
                   .format(ver_local, ver_inet), app.MB_OK+app.MB_ICONINFO)
        return

    if app.msg_box(_('CudaText update is available.\nLocal: {}\nInternet: {}\n\nOpen download URL in browser?')
                  .format(ver_local, ver_inet), app.MB_YESNO+app.MB_ICONINFO) == app.ID_YES:
        webbrowser.open_new_tab(url)
        print(_('Opened download URL'))
