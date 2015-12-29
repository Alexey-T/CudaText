from cudatext import *

def dlg_config(ch_def, ch_user):
    c1 = chr(1)
    id_memo = 3
    id_ok = 4
    all_size_x = 520
    all_size_y = 330
    btn_size = 80
    res = dlg_custom('Addons Manager config', all_size_x, all_size_y, '\n'.join([]+
      [c1.join(['type=label', 'pos=6,6,300,0', 'cap=&Default channels:'])]+
      [c1.join(['type=memo', 'pos=6,24,%d,150'%(all_size_x-6), 'val='+'\t'.join(ch_def), 'props=1,0,1'])]+
      [c1.join(['type=label', 'pos=6,156,300,0', 'cap=&User channels:'])]+
      [c1.join(['type=memo', 'pos=6,174,%d,%d'%(all_size_x-6, all_size_y-36), 'val='+'\t'.join(ch_user)])]+
      [c1.join(['type=button', 'pos=%d,%d,%d,0'%(all_size_x-2*btn_size-12, all_size_y-30, all_size_x-btn_size-12), 'cap=&OK'])]+ 
      [c1.join(['type=button', 'pos=%d,%d,%d,0'%(all_size_x-btn_size-6, all_size_y-30, all_size_x-6), 'cap=Cancel'])] 
      ), id_memo)
    if res is None: return
    id, text = res
    if id!=id_ok: return
          
    ch = text.splitlines()[id_memo].split('\t')
    ch = [s for s in ch if s]
    return ch
