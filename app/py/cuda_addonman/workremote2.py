import os
import tempfile
from .workremote import *
from urllib.parse import unquote

URL_PLUGINS = 'http://sourceforge.net/projects/cudatext/files/addons/plugins/'
REGEX_PLUGINS = r'href="(http://sourceforge\.net/projects/cudatext/files/addons/plugins/plugin\.(.+?).zip/download)"'

URL_SNIP = 'http://sourceforge.net/projects/cudatext/files/addons/snippets/'
REGEX_SNIP = r'href="(http://sourceforge\.net/projects/cudatext/files/addons/snippets/snippets\.(.+?).zip/download)"'

temp_fn = os.path.join(tempfile.gettempdir(), 'cuda_addons_dir.html')


def get_remote_addons_list():
    list1 = get_remote_plug_list()
    list2 = get_remote_snip_list()
    res = []
    if list1: res+= [(l[0], 'Plugin: '+unquote(l[1])) for l in list1]
    if list2: res+= [(l[0], 'Snippets: '+unquote(l[1])) for l in list2]
    return sorted(res)
    

def get_remote_plug_list():
    get_url(URL_PLUGINS, temp_fn)
    if not os.path.isfile(temp_fn): return

    text = open(temp_fn).read()
    res = re.findall(REGEX_PLUGINS, text)
    #print(res)
    return res


def get_remote_snip_list():
    get_url(URL_SNIP, temp_fn)
    if not os.path.isfile(temp_fn): return

    text = open(temp_fn).read()
    res = re.findall(REGEX_SNIP, text)
    #print(res)
    return res
