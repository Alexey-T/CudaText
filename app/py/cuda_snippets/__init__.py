from cudatext import *
from .proc_snippets import *

def is_lexer_listed(name, namelist):
    if not name and not namelist: return True
    return bool(name) and (','+name+',' in ','+namelist+',')

class Command:
    snips=[]

    def __init__(self):
        self.do_load_snippets()
        
    def do_load_snippets(self):
        dir = os.path.join(app_path(APP_DIR_DATA), 'snippets')
        self.snips = get_snip_list_of_dicts(dir)
    
    def on_key(self, ed_self, code, state):
        if code!=9: return #tab-key=9
        if state!='': return
        name = get_snip_name_from_editor(ed_self)
        if not name: return
        print('snip:', name)
    
    def do_menu(self):
        lex = ed.get_prop(PROP_LEXER_CARET)
        
        snips_here = [item for item in self.snips
                      if is_lexer_listed(lex, item[SNIP_LEX]) ]
        
        names = [item[SNIP_NAME]+'\t'+item[SNIP_ID]+'  ['+item[SNIP_LEX]+']' 
                for item in snips_here]
        res = dlg_menu(MENU_LIST, '\n'.join(names))
        if res is None: return

        print('selected snippet:', snips_here[res][SNIP_NAME])
        insert_snip_into_editor(ed, snips_here[res][SNIP_TEXT])
