import re
from cudatext import *

RES_OPT_TEXT = 1
RES_OPT_CASE = 2
RES_FIND = 6
RES_COPY_CLIP = 8
RES_COPY_TAB = 9

SIZE_X = 600
SIZE_Y = 400
SIZE_BTN = 150


def do_find(text, case_sens):
    l = re.findall(text, 
          ed.get_text_all(), 
          0 if case_sens else re.IGNORECASE)
    l = sorted(list(set(l)))
    return l


def do_dialog(text, case_sens, items):
    c1 = chr(1)
    s_en = '1' if items else '0'
    s_case = '1' if case_sens else '0'
    res = dlg_custom('Extract Strings', SIZE_X, SIZE_Y, 
      '\n'.join([]
         +[c1.join(['type=label', 'pos=6,5,300,0', 'cap=&Regular expression:'])]
         +[c1.join(['type=edit', 'pos=6,23,%d,0'%(SIZE_X-SIZE_BTN-12), 'val='+text])]
         +[c1.join(['type=check', 'pos=6,51,%d,0'%(SIZE_X-SIZE_BTN-12), 'cap=Case &sensitive', 'val='+s_case])]
            
         +[c1.join(['type=label', 'pos=6,78,400,0', 'cap=F&ound strings:'])]
         +[c1.join(['type=listbox', 'pos=6,96,%d,%d'%(SIZE_X-SIZE_BTN-12, SIZE_Y-22), 'items='+'\t'.join(items)])]
         +[c1.join(['type=label', 'pos=6,%d,300,0'%(SIZE_Y-20), 'cap=Found: %d'%len(items)])]
             
         +[c1.join(['type=button', 'pos=%d,25,%d,0'%(SIZE_X-SIZE_BTN-6, SIZE_X-6), 'cap=&Find', 'props=1'])]
         +[c1.join(['type=button', 'pos=%d,55,%d,0'%(SIZE_X-SIZE_BTN-6, SIZE_X-6), 'cap=Cancel'])]
         +[c1.join(['type=button', 'pos=%d,96,%d,0'%(SIZE_X-SIZE_BTN-6, SIZE_X-6), 'cap=Copy to &clipboard', 'en='+s_en])]
         +[c1.join(['type=button', 'pos=%d,126,%d,0'%(SIZE_X-SIZE_BTN-6, SIZE_X-6), 'cap=Copy to &new tab', 'en='+s_en])]
      ) )
    if res is None: return
        
    res, state = res
    states = state.split('\n')
    text = states[RES_OPT_TEXT]
    case_sens = states[RES_OPT_CASE]=='1'
    return (res, text, case_sens)


class Command:
    def run(self):
        text = r'\w+'
        case_sens = False
        items = []
        while True:
            res = do_dialog(text, case_sens, items)
            if res is None: return
            res, text, case_sens = res
                
            if res==RES_FIND:
                #print('find:', text)
                items = do_find(text, case_sens)
                continue
                
            elif res==RES_COPY_CLIP:
                app_proc(PROC_SET_CLIP, '\n'.join(items))
                msg_status('Copied to clipboard')
                ed.focus()
                return
                
            elif res==RES_COPY_TAB:
                file_open('')
                text = '\n'.join(items)+'\n'
                ed.set_text_all(text)
                msg_status('Copied to tab')
                ed.focus()
                return
                
            else:
                msg_status('Unknown code of dlg')
                return
    
    
