import os
import re
import tempfile
import requests
from urllib.parse import unquote
import cudatext as app
from . import opt


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
            res = app.msg_box('Cannot download:\n%s\n%s\n\nRetry?' % (url, str(e)),
                app.MB_ABORTRETRYIGNORE + app.MB_ICONWARNING)
            if res==app.ID_IGNORE: return
            if res==app.ID_ABORT: return False


def get_plugin_zip(url):
    if not url: return
    fn = os.path.join(tempfile.gettempdir(), 'cudatext_addon.zip')
    get_url(url, fn)
    return fn


def get_channel_list(url):
    temp_fn = os.path.join(tempfile.gettempdir(), 'cuda_addons_dir.txt')
    get_url(url, temp_fn, True)
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
