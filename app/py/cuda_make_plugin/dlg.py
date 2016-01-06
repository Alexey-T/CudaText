import string
from cudatext import *
from .events import *

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
     

def dlg_make_plugin():
    '''
    (s_caption, s_module, cmd_list, event_list)
    '''
    dlg_w = 406
    dlg_h = 460
    btn_w = 100
    id_name = 1
    id_module = 3
    id_items = 5
    id_events = 7
    id_ok = 8
    c1 = chr(1)
    while True:
        res = dlg_custom('Make Plugin', dlg_w, dlg_h, '\n'.join([]
          + [c1.join(['type=label', 'cap=Plugin &name:', 'pos=6,6,400,0'])]
          + [c1.join(['type=edit', 'val=MySample', 'pos=6,24,400,46'])]
          + [c1.join(['type=label', 'cap=Lowercase &module name:', 'pos=6,54,400,0'])]
          + [c1.join(['type=edit', 'val=sample', 'pos=6,72,400,46'])]
          + [c1.join(['type=label', 'cap=Menu-&items in form "Caption>method":', 'pos=6,102,400,0'])]
          + [c1.join(['type=memo', 'val=MySample>run', 'pos=6,120,400,240'])]
          + [c1.join(['type=label', 'cap=&Events to handle:', 'pos=6,246,400,0'])]
          + [c1.join(['type=checklistbox', 'items='+'\t'.join(EVENTS), 'pos=6,264,400,420'])]
          + [c1.join(['type=button', 'cap=&OK', 'pos=%d,%d,%d,%d'%(dlg_w-btn_w*2-12, dlg_h-30, dlg_w-btn_w-12, 0)])]
          + [c1.join(['type=button', 'cap=Cancel', 'pos=%d,%d,%d,%d'%(dlg_w-btn_w-6, dlg_h-30, dlg_w-6, 0)])]
          ) )
        if res is None: return
        (btn, text) = res
        if btn!=id_ok: return
        text = text.splitlines()
    
        s_caption = text[id_name]
        s_module = text[id_module]
        s_items = text[id_items].split('\t')
        cmd_list = [s.split('>') for s in s_items if s]
        s_items = text[id_events].split(';')[1].split(',')
        event_list = [s for (n, s) in enumerate(EVENTS) if s_items[n]=='1']
        
        if not s_module or not s_caption:
            msg_box('Empty field', MB_OK+MB_ICONERROR)
            continue
        if not is_correct_id(s_module):
            msg_box('Incorrect module name: "%s"' % s_module, MB_OK+MB_ICONERROR)
            continue

        bad = False
        for i in cmd_list:
            if len(i)!=2:
                msg_box('Incorrect menu-item: '+repr(i), MB_OK+MB_ICONERROR)
                bad = True
                break
            if not i[0]:
                msg_box('Incorrect menu-item name: '+i[0], MB_OK+MB_ICONERROR)
                bad = True
                break
            if not is_correct_id(i[1]):
                msg_box('Incorrect menu-item method: '+i[1], MB_OK+MB_ICONERROR)
                bad = True
                break

        if bad:
            continue
                
        return (s_caption, 'cuda_'+s_module, cmd_list, event_list)
