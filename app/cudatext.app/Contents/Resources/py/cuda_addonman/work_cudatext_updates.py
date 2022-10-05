import sys
import os
import re
import platform
import tempfile
import webbrowser
import cudatext as app
from .work_remote import *

p = sys.platform
X64 = platform.architecture()[0]=='64bit'
##p = 'win32'
##X64 = False

DOWNLOAD_PAGE = \
    'https://sourceforge.net/projects/cudatext/files/release/Linux/' if p.startswith('linux')\
    else 'https://sourceforge.net/projects/cudatext/files/release/Windows/' if p.startswith('win')\
    else 'https://sourceforge.net/projects/cudatext/files/release/macOS/' if p=='darwin'\
    else 'https://sourceforge.net/projects/cudatext/files/release/FreeBSD/' if p.startswith('freebsd')\
    else '?'

if p=='darwin':
    TEXT_CPU = ''
    REGEX_GROUP_VER = 1
else:
    TEXT_CPU = '(amd64|x64)' if X64 else '(i386|x32)'
    REGEX_GROUP_VER = 2

DOWNLOAD_REGEX = \
    ' href="(\w+://[\w\.]+/projects/cudatext/files/release/\w+/cudatext-[\w\-]+?'+TEXT_CPU+'[\w\-]*?-([\d\.]+?)\.(zip|dmg|tar\.xz)/download)"'

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

    items = sorted(items, key=lambda i:i[REGEX_GROUP_VER], reverse=True)
    print('Found links:')
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