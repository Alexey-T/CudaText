#!/usr/bin/python3
import sys
import os
import tempfile
import re
import json
import requests
import zipfile
import time
from urllib.parse import unquote

ch_def = [
  'https://raw.githubusercontent.com/Alexey-T/CudaText-registry/master/json/plugins.json',
  'https://raw.githubusercontent.com/Alexey-T/CudaText-registry/master/json/linters.json',
  'https://raw.githubusercontent.com/Alexey-T/CudaText-registry/master/json/data.json',
  'https://raw.githubusercontent.com/Alexey-T/CudaText-registry/master/json/snippets.json',
  'https://raw.githubusercontent.com/Alexey-T/CudaText-registry/master/json/lexers.json',
  'https://raw.githubusercontent.com/kvichans/CudaText-registry/master/kv-addons.json',
  ]

def get_remote_addons_list(channels):
    res = []
    print('Read channels:')
    for ch in channels:
        items = get_channel(ch)
        if items:
            res += items
        else:
            return
    return res

def get_channel(url):
    cap = url.split('/')[-1]

    #separate temp fn for each channel
    temp_dir = os.path.join(tempfile.gettempdir(), 'cudatext_addon_man')
    if not os.path.isdir(temp_dir):
        os.mkdir(temp_dir)
    temp_fn = os.path.join(temp_dir, cap)

    print('  getting:', cap)
    get_url(url, temp_fn, True)
    if not os.path.isfile(temp_fn): return

    text = open(temp_fn, encoding='utf8').read()
    d = json.loads(text)

    RE = r'http.+/(\w+)\.(.+?)\.zip'
    for item in d:
        parse = re.findall(RE, item['url'])
        item['kind'] = parse[0][0]
        item['name'] = unquote(parse[0][1])
    return d


def get_url(url, fn, del_first=False):
    fn_temp = fn+'.download'
    if os.path.isfile(fn_temp):
        os.remove(fn_temp)
    if del_first and os.path.isfile(fn):
        os.remove(fn)

    while True:
        try:
            r = requests.get(url, proxies=None)
            with open(fn_temp, 'wb') as f:
                f.write(r.content)

            if os.path.isfile(fn_temp):
                if os.path.isfile(fn):
                    os.remove(fn)
                os.rename(fn_temp, fn)
            return

        except Exception as e:
            print('error:', str(e))
            time.sleep(2)


print('Downloading list...')
data = get_remote_addons_list(ch_def)
if not data:
    print('Cannot download')
    sys.exit(0)
print('Downloaded list')

data = sorted([item['url'] for item in data])
'''
with open('addons_links.txt', 'w') as f:
    for s in data:
        f.write(s+'\n')
'''

dir1 = os.path.join(os.path.dirname(__file__), 'CudaText_addons')
if not os.path.isdir(dir1):
    os.mkdir(dir1)

print('Downloading files to:', dir1)

for (i, url) in enumerate(data):

    fname = unquote(url.split('/')[-1])
    kind = fname.split('.')[0]
    print('get %d/%d: %s' % (i+1, len(data), fname))

    dir2 = os.path.join(dir1, kind)
    if not os.path.isdir(dir2):
        os.mkdir(dir2)
    fn_zip = os.path.join(dir2, fname)

    get_url(url, fn_zip, True)

print('Downloaded files to:', dir1)
print('How to install addons from .zip files:')
print('open each .zip file  in CudaText (using "File / Open file..." or passing parameter in the command line) and application will suggest to install it.')
