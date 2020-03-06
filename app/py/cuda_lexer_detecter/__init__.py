import os
import tempfile
from cudatext import *
from .lexertypes import *
from cuda_addonman.work_remote import get_url

class Command:

    def on_open_none(self, ed_self):

        fn = ed_self.get_filename()
        if not fn:
            return

        name = os.path.basename(fn)
        ext1 = ''
        ext2 = ''

        if '.' in name:
            ext1 = name[name.rfind('.')+1:]
            s = name[:name.rfind('.'):]
            if '.' in s:
                ext2 = s[s.rfind('.')+1:] + '.' + ext1

        #print('name, ext1, ext2:', name, ext1, ext2)
        lexers = []

        lex = TYPES.get('/'+name, '')
        if lex:
            lexers += lex

        if ext2:
            lex = TYPES.get(ext2, '')
            if lex:
                lexers += lex

        if ext1:
            lex = TYPES.get(ext1, '')
            if lex:
                lexers += lex

        if not lexers:
            return

        items = ['Download lexer: '+s for s in lexers]
        res = dlg_menu(MENU_LIST, items, caption='Lexer(s) for "%s"' % name)
        if res is None:
            return
        lex = lexers[res]
        #print('Detected lexer:', lex)

        url = 'https://sourceforge.net/projects/cudatext/files/addons/lexers/lexer.' + lex.replace(' ', '_') + '.zip'

        tempname = tempfile.gettempdir()+os.sep+'cudatext_lexer.zip'
        get_url(url, tempname, True)
        if not os.path.isfile(tempname):
            msg_box('Could not download/install lexer "%s" from add-ons'%lex, MB_OK+MB_ICONERROR)
            return

        file_open(tempname, options='/silent')
        lexer_proc(LEXER_REREAD_LIB, '')

        ed_self.set_prop(PROP_LEXER_FILE, lex)
