import os
import string
from cudatext import *
from .events import *
from cudax_lib import get_translation

_   = get_translation(__file__)  # I18N

def is_correct_id(name):
    if not name:
        return False
    if not name[0] in string.ascii_letters+'_':
        return False
    chars = string.ascii_letters+string.digits+'_'
    for s in name:
        if not s in chars:
            return False
    return True

def get_module_dir():
    dir = app_path(APP_DIR_PY)
    for i in range(2000):
        name = 'sample'+str(i+1)
        if not os.path.isdir(os.path.join(dir, 'cuda_'+name)):
            return name
    return 'sample'


def dlg_make_plugin():
    '''
    Gets tuple:
    (s_caption, s_module, cmd_list, event_list, with_config)
    cmd_list[i] is 3-tuple: (caption, method, no_menu)
    '''
    dlg_w = 456
    dlg_h = 490
    btn_w = 80

    id_name = 1
    id_module = 3
    id_items = 5
    id_config = 6
    id_events = 8
    id_ok = 9
    c1 = chr(1)

    s_caption = 'My Sample'
    s_module = get_module_dir()
    s_plugin_items = 'My command name>run'
    s_events_checks = ''

    while True:
        res = dlg_custom(_('Make Plugin'), dlg_w, dlg_h, '\n'.join([]
          + [c1.join(['type=label', 'cap='+_('Plugin &name:'), 'pos=6,6,400,0'])]
          + [c1.join(['type=edit', 'val='+s_caption, 'pos=6,24,400,46'])]
          + [c1.join(['type=label', 'cap='+_('&Python module name without "cuda_":'), 'pos=6,54,450,0'])]
          + [c1.join(['type=edit', 'val='+s_module, 'pos=6,72,400,46'])]
          + [c1.join(['type=label', 'cap='+_('&Commands "Caption>method" ("-" at end: without menu item):'), 'pos=6,102,450,0'])]
          + [c1.join(['type=memo', 'val='+s_plugin_items, 'pos=6,120,450,240'])]
          + [c1.join(['type=check', 'cap='+_('&Add "Config" menu item to "Options / Settings-plugins"'), 'pos=6,246,450,0'])]
          + [c1.join(['type=label', 'cap='+_('&Events to handle:'), 'pos=6,276,400,0'])]
          + [c1.join(['type=checklistbox', 'items='+'\t'.join(EVENTS), 'val='+s_events_checks, 'pos=6,294,450,450'])]
          + [c1.join(['type=button', 'cap='+_('&OK'), 'ex0=1', 'pos=%d,%d,%d,%d'%(dlg_w-btn_w*2-12, dlg_h-30, dlg_w-btn_w-12, 0)])]
          + [c1.join(['type=button', 'cap='+_('Cancel'), 'pos=%d,%d,%d,%d'%(dlg_w-btn_w-6, dlg_h-30, dlg_w-6, 0)])]
          ) )
        if res is None: return
        (btn, text) = res
        if btn!=id_ok: return
        text = text.splitlines()

        s_caption = text[id_name]
        s_module = text[id_module]
        s_plugin_items = text[id_items]
        s_events_checks = text[id_events]
        with_config = text[id_config]=='1'

        items = text[id_items].split('\t')
        cmd_list = [s.split('>') for s in items if s]
        items = text[id_events].split(';')[1].split(',')
        event_list = [s for (n, s) in enumerate(EVENTS) if items[n]=='1']

        if not s_module or not s_caption:
            msg_box(_('Empty field'), MB_OK+MB_ICONERROR)
            continue

        if s_module.startswith('cuda_'):
            s_module = s_module[5:]
        s_module_ok = 'cuda_'+s_module

        if not s_module_ok.isidentifier():
            msg_box(_('Incorrect module name: "%s"') % s_module_ok, MB_OK+MB_ICONERROR)
            continue

        bad = False
        for i in cmd_list:
            if len(i)!=2:
                msg_box(_('Incorrect item: ')+repr(i), MB_OK+MB_ICONERROR)
                bad = True
                break

            if not i[0]:
                msg_box(_('Incorrect item name: ')+i[0], MB_OK+MB_ICONERROR)
                bad = True
                break

            s = i[1]
            if s.endswith('-'): s=s[:-1]
            if not is_correct_id(s):
                msg_box(_('Incorrect item method: ')+s, MB_OK+MB_ICONERROR)
                bad = True
                break

        if bad:
            continue

        #handle "-" at end of cmd:
        for i in cmd_list:
            nomenu = i[1].endswith('-')
            i += [nomenu]
            if nomenu:
                i[1] = i[1][:-1]

        return (s_caption, s_module_ok, cmd_list, event_list, with_config)
