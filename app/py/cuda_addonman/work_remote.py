import os
import re
import tempfile
import urllib.request
import time
from urllib.parse import unquote

option_proxy = ''


def get_url(url, fn):
    if os.path.isfile(fn):
        os.remove(fn)

    if option_proxy:
        proxy = urllib.request.ProxyHandler({'http': option_proxy})
        opener = urllib.request.build_opener(proxy)
        urllib.request.install_opener(opener)
       
    while True: 
        try:
            urllib.request.urlretrieve(url, filename=fn)
            break
        except Exception as e:
            print(e)
            print('Pause, retrying...')
            time.sleep(5)
        

def get_plugin_zip(url):
    if not url: return
    fn = os.path.join(tempfile.gettempdir(), 'cudatext_addon.zip')
    get_url(url, fn)
    return fn


def get_channel_list(url):
    temp_fn = os.path.join(tempfile.gettempdir(), 'cuda_addons_dir.txt')
    get_url(url, temp_fn)
    if not os.path.isfile(temp_fn): return

    text = open(temp_fn, encoding='utf8').read()
    
    #regex has 3 groups: (..(type)..(name)..)
    RE = r'(http.+/(\w+)\.(.+?)\.zip)\b(.*)'
    
    res = re.findall(RE, text)
    res = [(r[0], r[1]+': '+unquote(r[2]), r[3]) for r in res]
    
    #print('debug:')
    #print(res)
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
