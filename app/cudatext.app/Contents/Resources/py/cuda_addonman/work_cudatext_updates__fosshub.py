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

OS = platform.system()
X64 = platform.architecture()[0]=='64bit'
WIN_CPU = 'x64' if X64 else 'x32'
UNIX_CPU = 'amd64' if X64 else 'i386'

DOWNLOAD_PAGE = 'https://www.fosshub.com/CudaText.html'
REGEX_GROUP_VER = 1

LINUX_ENDING = '-linux-gtk2-'+UNIX_CPU+'-([^\-]+)\.tar\.xz'
if OS == 'Linux':
    import subprocess
    try:
        LSB_RELEASE = subprocess.check_output('cat /etc/lsb-release', shell=True).decode('utf-8')
        if ("Ubuntu" in LSB_RELEASE or "Debian" in LSB_RELEASE) and (UNIX_CPU == 'amd64'):
            LINUX_ENDING = '_([\d\.]+)-\d+_gtk2_'+UNIX_CPU+'\.deb'
    except:
        pass

FILE_RES = {
    'Windows':      ' href="(https://.+?=cudatext-win-'              +WIN_CPU+ '-([^\-]+)\.zip)"',
    'Linux':        ' href="(https://.+?=cudatext'                   +LINUX_ENDING+ ')"',
    'FreeBSD':      ' href="(https://.+?=cudatext-freebsd-gtk2-'     +UNIX_CPU+'-([^\-]+)\.tar\.xz)"',
    'OpenBSD':      ' href="(https://.+?=cudatext-openbsd-gtk2-'     +UNIX_CPU+'-([^\-]+)\.tar\.xz)"',
    'NetBSD':       ' href="(https://.+?=cudatext-netbsd-gtk2-'      +UNIX_CPU+'-([^\-]+)\.tar\.xz)"',
    'DragonFlyBSD': ' href="(https://.+?=cudatext-dragonflybsd-gtk2-'+UNIX_CPU+'-([^\-]+)\.tar\.xz)"',
    'Solaris':      ' href="(https://.+?=cudatext-solaris-gtk2-'     +UNIX_CPU+'-([^\-]+)\.tar\.xz)"',
    'Darwin':       ' href="(https://.+?=cudatext-macos-([^\-]+)\.dmg)"',
    }
FILE_RE = FILE_RES.get(OS)

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
    app.msg_status(_('Downloading: ')+DOWNLOAD_PAGE, True)
    get_url(DOWNLOAD_PAGE, fn, True)
    app.msg_status('')

    if not os.path.isfile(fn):
        app.msg_status(_('Cannot download: ')+DOWNLOAD_PAGE)
        return

    text = open(fn, encoding='utf8', errors='replace').read()
    items = re.findall(FILE_RE, text)
    if not items:
        app.msg_status(_('Cannot find download links'))
        return

    items = sorted(items, reverse=True)
    print(_('Found links:'))
    for i in items:
        print('  '+i[0])

    url = items[0][0]
    ver_inet = items[0][REGEX_GROUP_VER]
    ver_local = app.app_exe_version()

    if versions_ordered(ver_inet, ver_local):
        msg_ = app.msg_box_ex(
            _('Check for updates'),
            _('Up to Date\n\nCurrent version: {}\nLatest version: {}').format(ver_local, ver_inet),
            [_('OK'), _('Open changelog...')],
            app.MB_ICONQUESTION
        )
        if msg_ == 1:
            webbrowser.open_new_tab(CHANGELOG_PAGE)
            print(_('Opened changelog URL'))
        return

    msg_ = app.msg_box_ex(
        _('Check for updates'),
        _('Update Available\n\nCurrent version: {}\nLatest version: {}').format(ver_local, ver_inet),
        [_('Cancel'), _('Open download URL...'), _('Open changelog...')],
        app.MB_ICONQUESTION
    )
    if msg_ == 1:
        webbrowser.open_new_tab(url)
        print(_('Opened download URL'))
    if msg_ == 2:
        webbrowser.open_new_tab(CHANGELOG_PAGE)
        print(_('Opened changelog URL'))