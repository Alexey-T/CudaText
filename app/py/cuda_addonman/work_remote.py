import os
import re
import json
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


def get_channel(url):
    RE = r'http.+/(\w+)\.(.+?)\.zip'
    temp_fn = os.path.join(tempfile.gettempdir(), 'cuda_addons_dir.json')
    get_url(url, temp_fn, True)
    if not os.path.isfile(temp_fn): return

    text = open(temp_fn, encoding='utf8').read()
    d = json.loads(text)

    for item in d:
        parse = re.findall(RE, item['url'])
        item['kind'] = parse[0][0]
        item['name'] = unquote(parse[0][1])
    return d


def get_remote_addons_list(channels):
    res = []
    print('Read channels:')
    for ch in channels:
        print('  '+ch)
        items = get_channel(ch)
        if items:
            res += items
    return res
