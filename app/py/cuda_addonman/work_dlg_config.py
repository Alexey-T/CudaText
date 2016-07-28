from cudatext import *

def dlg_config(ch_def, ch_user, suggest_readme, proxy):
    c1 = chr(1)
    id_channel = 3
    id_readme = 4
    id_proxy = 6
    id_ok = 7
    all_size_x = 520
    all_size_y = 360
    btn_size = 100
    
    text = '\n'.join([]+
      [c1.join(['type=label', 'pos=6,6,250,0', 'cap=&Default channels:'])]+
      [c1.join(['type=memo', 'pos=6,24,%d,120'%(all_size_x-6), 'val='+'\t'.join(ch_def), 'props=1,0,1'])]+
      [c1.join(['type=label', 'pos=6,126,256,0', 'cap=&User channels:'])]+
      [c1.join(['type=memo', 'pos=%d,%d,%d,%d'%(6, 144, all_size_x-6, all_size_y-120), 'val='+'\t'.join(ch_user)])]+
      [c1.join(['type=check', 'pos=%d,%d,%d,0'%(6, all_size_y-115, all_size_x-btn_size-12), 'cap=Suggest to show &readme after install', 'val='+('1' if suggest_readme else '0') ])]+ 
      [c1.join(['type=label', 'pos=%d,%d,%d,0'%(6, all_size_y-85, all_size_x-btn_size-12), 'cap=&Proxy server, e.g. myserver.com:2000' ])]+ 
      [c1.join(['type=edit', 'pos=%d,%d,%d,0'%(6, all_size_y-65, all_size_x-6), 'val='+proxy ])]+ 
      [c1.join(['type=button', 'pos=%d,%d,%d,0'%(all_size_x-2*btn_size-12, all_size_y-30, all_size_x-btn_size-12), 'cap=&OK'])]+ 
      [c1.join(['type=button', 'pos=%d,%d,%d,0'%(all_size_x-btn_size-6, all_size_y-30, all_size_x-6), 'cap=Cancel'])] 
      )

    res = dlg_custom('Addons Manager options', all_size_x, all_size_y, text, id_channel)
    if res is None: return
    
    id, text = res
    if id!=id_ok: return
    text = text.splitlines()
          
    ch = text[id_channel].split('\t')
    ch = [s for s in ch if s]
    
    readme = text[id_readme]=='1'
    proxy = text[id_proxy] 
    
    return (ch, readme, proxy)
