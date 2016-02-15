import os
import re
import tempfile
import urllib.request
from urllib.parse import unquote


def get_url(url, fn):
    if os.path.isfile(fn):
        os.remove(fn)
    try:
        urllib.request.urlretrieve(url, fn)
    except Exception as e:
        print(e)
        

def get_plugin_zip(url):
    if not url: return
    fn = os.path.join(tempfile.gettempdir(), 'cudatext_addon.zip')
    get_url(url, fn)
    return fn


def get_channel_list(url):
    temp_fn = os.path.join(tempfile.gettempdir(), 'cuda_addons_dir.html')
    get_url(url, temp_fn)
    if not os.path.isfile(temp_fn): return

    text = open(temp_fn, encoding='utf8').read()
    
    #regex has 3 groups: (..(type)..(name)..)
    regex = r'a href="((\w+)\.(.+?)\.zip)"'
    
    res = re.findall(regex, text)
    #print(res)
    
    res = [(url+r[0], r[1]+': '+unquote(r[2])) for r in res]
    return res


def get_remote_addons_list(channels):
    res = []
    print('Read channels:')
    for s in channels: print('  '+s)
    for ch in channels:
        items = get_channel_list(ch)
        if items:
            res+=items
    return sorted(res)
