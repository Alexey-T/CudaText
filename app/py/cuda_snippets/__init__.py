from cudatext import *
from .proc_snip import *
from .proc_snip_insert import *


def is_name_listed(name, namelist):
    if not namelist: return True
    return bool(name) and (','+name+',' in ','+namelist+',')

class Command:
    snips=[]

    def __init__(self):
        self.do_load_snippets()
        
    def do_load_snippets(self):
        dir = os.path.join(app_path(APP_DIR_DATA), 'snippets')
        self.snips = get_snip_list_of_dicts(dir)
        
    def get_snip_list_current(self):
        lex = ed.get_prop(PROP_LEXER_CARET)
        return [item for item in self.snips
                if is_name_listed(lex, item[SNIP_LEX]) ]
    
    
    def on_key(self, ed_self, code, state):
        if code!=9: return #tab-key=9
        if state!='': return
        if ed_self.get_prop(PROP_TAB_COLLECT_MARKERS): return
        
        carets = ed_self.get_carets()
        if len(carets)!=1: return
        
        name = get_snip_name_from_editor(ed_self)
        if not name: return
        
        items = self.get_snip_list_current() #leave snips for lexer
        items = [i for i in items if i[SNIP_ID]==name] #leave snips for name
        
        if not items: return

        #delete name in text        
        x0, y0, x1, y1 = carets[0]
        ed_self.delete(x0-len(name), y0, x0, y0)
        ed_self.set_caret(x0-len(name), y0)
        
        if len(items)>1:
            self.do_menu_for_items(items)
            return False #block tab-key

        insert_snip_into_editor(ed_self, items[0][SNIP_TEXT])
        return False #block tab-key
                

    def do_menu_for_items(self, items):
        names = [item[SNIP_NAME]+'\t'+item[SNIP_ID]+'  ['+item[SNIP_LEX]+']' 
                for item in items]
        res = dlg_menu(MENU_LIST, '\n'.join(names))
        if res is None: return
        insert_snip_into_editor(ed, items[res][SNIP_TEXT])
        
    def do_menu(self):
        items = self.get_snip_list_current()
        self.do_menu_for_items(items)
        
