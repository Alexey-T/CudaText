import os
import tempfile
from .workremote import *
from urllib.parse import unquote

URL_PLUG = 'http://sourceforge.net/projects/cudatext/files/addons/plugins/'
URL_SNIP = 'http://sourceforge.net/projects/cudatext/files/addons/snippets/'
URL_LEX = 'http://sourceforge.net/projects/synwrite-addons/files/Lexers/'
REGEX_PLUG = r'href="(http://sourceforge\.net/projects/cudatext/files/addons/plugins/plugin\.(.+?).zip/download)"'
REGEX_SNIP = r'href="(http://sourceforge\.net/projects/cudatext/files/addons/snippets/snippets\.(.+?).zip/download)"'
REGEX_URLS = r'href="(http://sourceforge\.net/projects/.+?zip/download)"'

temp_fn = os.path.join(tempfile.gettempdir(), 'cuda_addons_dir.html')


def get_remote_addons_list():
    list1 = _get_remote_list(URL_PLUG, REGEX_PLUG)
    list2 = _get_remote_list(URL_SNIP, REGEX_SNIP)
    res = []
    if list1: res+= [(l[0], 'Plugin: '+unquote(l[1])) for l in list1]
    if list2: res+= [(l[0], 'Snippets: '+unquote(l[1])) for l in list2]
    return sorted(res)
    

def _get_remote_list(url, regex):
    get_url(url, temp_fn)
    if not os.path.isfile(temp_fn): return

    text = open(temp_fn, encoding='utf8').read()
    res = re.findall(regex, text)
    #print(res)
    return res


def get_remote_download_all_list():
    res = []
    l = _get_remote_list(URL_SNIP, REGEX_URLS)
    if l: res+=l
    l = _get_remote_list(URL_PLUG, REGEX_URLS)
    if l: res+=l
    l = _get_remote_list(URL_LEX, REGEX_URLS)
    if l: res+=l
    return sorted(res)
