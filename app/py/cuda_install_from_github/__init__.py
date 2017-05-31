import os
import tempfile
import urllib.request
from cudatext import *
import cudatext_cmd

def get_url(url, fn):
    if os.path.isfile(fn):
        os.remove(fn)
    try:
        urllib.request.urlretrieve(url, fn)
    except Exception as e:
        print(e)

def dialog(list_hist):
    c1 = chr(1)
    id_edit = 1
    id_ok = 2
    id_cancel = 3
    res = dlg_custom('Install from Github', 456, 90, '\n'.join([]
      + [c1.join(['type=label', 'cap=&Github repo URL', 'pos=6,6,400,0'])]
      + [c1.join(['type=combo', 'items='+'\t'.join(list_hist), 'pos=6,26,450,0', 'cap='+list_hist[0]])]
      + [c1.join(['type=button', 'cap=OK', 'pos=246,60,346,0', 'props=1'])]
      + [c1.join(['type=button', 'cap=Cancel', 'pos=350,60,450,0'])]
      ))
    if not res: return
    btn, text = res
    if btn!=id_ok: return
    text = text.splitlines()
    url = text[id_edit]
    return url


class Command:
    def run(self):
        fn_hist = os.path.join(app_path(APP_DIR_SETTINGS), 'cuda_install_from_github.ini')
        list_hist = ['https://github.com/kvichans/cuda_find_in_files']
        if os.path.isfile(fn_hist):
            list_hist = open(fn_hist).read().splitlines()

        url = dialog(list_hist)
        if not url: return
        
        fn = os.path.join(tempfile.gettempdir(), 'cudatext_addon.zip')
        msg_status('Downloading zip...')
        get_url(url+'/zipball/master', fn)
        msg_status('')
        if not os.path.isfile(fn):
            msg_status('Cannot download URL')
            return

        file_open(fn)

        #move new url to 1st item
        if url in list_hist:
            list_hist.remove(url)
        list_hist = [url]+list_hist
        #save history
        with open(fn_hist, 'w') as f:
            f.write('\n'.join(list_hist))
