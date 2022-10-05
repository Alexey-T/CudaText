import os
import re
import tempfile
import webbrowser
import cudatext as app
from .work_remote import *

from cudax_lib import get_translation
_   = get_translation(__file__)  # i18n

info = app.app_proc(app.PROC_GET_COMPILER_INFO, '')
TEXT_OS = info['os'].replace('win32', 'windows').replace('win64', 'windows')
TEXT_CPU = info['cpu'].replace('x86_64', 'amd64')
TEXT_WS = info['widgetset']

DEBIAN_UBUNTU = False
if TEXT_OS == 'linux':
    import subprocess
    try:
        LSB_RELEASE = subprocess.check_output('cat /etc/lsb-release', shell=True).decode('utf-8')
        if ("Ubuntu" in LSB_RELEASE or "Debian" in LSB_RELEASE) and (TEXT_CPU == 'amd64'):
            DEBIAN_UBUNTU = True
    except:
        pass

DOWNLOAD_PAGE = 'https://sourceforge.net/projects/cudatext/files/release/'
VERSION_REGEX = r'\b1\.\d{2,3}\.\d+\.\d+\b'
if DEBIAN_UBUNTU:
    DOWNLOAD_REGEX = \
        r' href="(\w+://[\w\.]+/projects/cudatext/files/release/([\d\.]+)/cudatext_([\d\.\-]+)_'+ \
        TEXT_WS + '_' + TEXT_CPU + '.deb/download)"'
else:
    DOWNLOAD_REGEX = \
        r' href="(\w+://[\w\.]+/projects/cudatext/files/release/([\d\.]+)/cudatext-'+ \
        TEXT_OS + '-' + \
        ((TEXT_WS + '-') if TEXT_OS!='windows' else '') + \
        TEXT_CPU + r'-[\d\.]+'+ \
        r'\.(zip|dmg|tar\.xz|tar)/download)"'
REGEX_GROUP_VER = 1

CHANGELOG_PAGE = 'https://cudatext.github.io/history.txt'

def versions_ordered(s1, s2):
    """
    compare "1.10.0" and "1.9.0" correctly
    """
    n1 = list(map(int, s1.split('.')))
    n2 = list(map(int, s2.split('.')))
    return n1<=n2


def check_cudatext():

    fn = os.path.join(tempfile.gettempdir(), 'cudatext_download.html')
    url = DOWNLOAD_PAGE
    app.msg_status(_('Downloading: ')+url, True)
    get_url(url, fn, True)
    app.msg_status('')

    if not os.path.isfile(fn):
        app.msg_status(_('Cannot download: ')+url)
        return

    text = open(fn, encoding='utf8').read()
    items = re.findall(VERSION_REGEX, text)
    if not items:
        app.msg_status(_('Cannot find app version: '+url))
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
        msg_ = app.msg_box_ex(
            _('Check for updates'),
            _('Latest CudaText is already here.\n\nLocal: {}\nInternet: {}').format(ver_local, ver_inet),
            [_('OK'), _('View changelog...')],
            app.MB_ICONQUESTION
        )
        if msg_ == 1:
            webbrowser.open_new_tab(CHANGELOG_PAGE)
            print(_('Opened changelog link'))
        return

    msg_ = app.msg_box_ex(
        _('Check for updates'),
        _('CudaText update is available.\n\nLocal: {}\nInternet: {}').format(ver_local, ver_inet),
        [_('Cancel'), _('Open download link...'), _('View changelog...')],
        app.MB_ICONQUESTION
    )
    if msg_ == 1:
        webbrowser.open_new_tab(url)
        print(_('Opened download link'))
    if msg_ == 2:
        webbrowser.open_new_tab(CHANGELOG_PAGE)
        print(_('Opened changelog link'))