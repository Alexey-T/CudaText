import os
import re
import json
import time
import tempfile
import requests
from urllib.parse import unquote
import cudatext as app
from . import opt

from cudax_lib import get_translation
_   = get_translation(__file__)  # i18n

def get_url(url, fn, del_first=False):
    fn_temp = fn+'.download'
    if os.path.isfile(fn_temp):
        os.remove(fn_temp)
    if del_first and os.path.isfile(fn):
        os.remove(fn)

    if opt.proxy:
        proxies = { 'http': opt.proxy, 'https': opt.proxy, }
    else:
        proxies = None
    #print('proxy', proxies)

    while True:
        try:
            r = requests.get(url, proxies=proxies, stream=True)
            with open(fn_temp, 'wb') as f:
                for chunk in r.iter_content(chunk_size=1024):
                    if chunk: # filter out keep-alive new chunks
                        f.write(chunk)
                        #f.flush() commented by recommendation

            if os.path.isfile(fn_temp):
                if os.path.isfile(fn):
                    os.remove(fn)
                os.rename(fn_temp, fn)
            return

        except Exception as e:
            res = app.msg_box(_('Cannot download:\n{}\n{}\n\nRetry?').format(url, str(e)),
                app.MB_ABORTRETRYIGNORE + app.MB_ICONWARNING)
            if res==app.ID_IGNORE: return
            if res==app.ID_ABORT: return False


def is_file_html(fn):
    if os.path.exists(fn):
        with open(fn, 'r', encoding='cp437') as f:
            s = f.readline(10).lower()
            return s.startswith('<html>')
    return False


def get_plugin_zip(url):
    if not url: return
    fn = os.path.join(tempfile.gettempdir(), 'cudatext_addon.zip')
    get_url(url, fn, True)
    
    if is_file_html(fn):
        os.remove(fn)
    
    return fn


def file_aged(fn):
    if os.path.isfile(fn):
        age = int(time.time() - os.stat(fn).st_mtime)
        return age > opt.cache_minutes * 60
    else:
        return True


def get_channel(url):
    cap = url.split('/')[-1]

    #separate temp fn for each channel
    temp_dir = os.path.join(tempfile.gettempdir(), 'cudatext_addon_man')
    if not os.path.isdir(temp_dir):
        os.mkdir(temp_dir)
    temp_fn = os.path.join(temp_dir, cap)

    #download if not cached or cache is aged
    if file_aged(temp_fn):
        print(_('  getting:'), cap)
        get_url(url, temp_fn, True)
    else:
        print(_('  cached:'), cap)
    if not os.path.isfile(temp_fn): return

    text = open(temp_fn, encoding='utf8').read()
    d = json.loads(text)

    RE = r'http.+/(\w+)\.(.+?)\.zip'
    for item in d:
        parse = re.findall(RE, item['url'])
        item['kind'] = parse[0][0]
        item['name'] = unquote(parse[0][1])
    return d


def get_remote_addons_list(channels):
    res = []
    print(_('Read channels:'))
    for ch in channels:
        items = get_channel(ch)
        if items:
            res += items
        else:
            return
    return res
