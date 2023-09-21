import os
import cudatext as app
from cudatext import *
from cudax_lib import get_translation
from .lexertypes import *

_   = get_translation(__file__)  # I18N

config_file = os.path.join(app_path(APP_DIR_SETTINGS), 'plugins.ini')
config_section = 'lexer_detecter_ignore'

def log(*args):
    pass
    #print(*args)

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

        if ext1:
            if ini_read(config_file, config_section, ext1, '')=='1':
                return

        log('Lexer detecter: name, ext1, ext2:', name, ext1, ext2)
        lexers = []

        log('Lexer detecter: search by name:', '/'+name)
        lex = TYPES.get('/'+name, '')
        if lex:
            lexers += lex

        if os.name=='nt':
            ext1b = ext1.lower()
            ext2b = ext2.lower()
        else:
            ext1b = ext1
            ext2b = ext2

        if ext2:
            log('Lexer detecter: search by ext2:', ext2)
            lex = TYPES.get(ext2, '')
            if not lex and ext2b != ext2:
                lex = TYPES.get(ext2b, '')
            if lex:
                lexers += lex

        if ext1:
            log('Lexer detecter: search by ext1:', ext1)
            lex = TYPES.get(ext1, '')
            if not lex and ext1b != ext1:
                lex = TYPES.get(ext1b, '')
            if lex:
                lexers += lex

        if not lexers:
            return

        items = [_('Download lexer: ')+s for s in lexers]
        items += [_('Cancel')]
        add_ignore = bool(ext1)
        if add_ignore:
            items += [_('Cancel, don\'t suggest anymore for *.%s') % ext1]

        res = dlg_menu(DMENU_LIST, items, caption=_('Lexer(s) for "%s"') % name)
        if res is None:
            return

        if add_ignore:
            if res == len(items)-2:
                return
            if res == len(items)-1:
                ini_write(config_file, config_section, ext1, '1')
                return
        else:
            if res == len(items)-1:
                return

        lex = lexers[res]
        log('Detected lexer:', lex)

        # imports here to speedup plugin load when no lexers found
        import tempfile
        from urllib.parse import quote
        from cuda_addonman.work_remote import get_url

        url = 'https://sourceforge.net/projects/cudatext/files/addons/lexers/lexer.' + quote(lex.replace(' ', '_')) + '.zip'

        tempname = tempfile.gettempdir()+os.sep+'cudatext_lexer.zip'
        get_url(url, tempname, True)
        if not os.path.isfile(tempname):
            msg_box(_('Could not download/install lexer "%s" from add-ons') % lex, MB_OK+MB_ICONERROR)
            return

        file_open(tempname, options='/silent')
        lexer_proc(LEXER_REREAD_LIB, '')

        ed_self.set_prop(PROP_LEXER_FILE, lex)

        # save data to packages.ini
        def getZipFilesNames(f_):
            from zipfile import ZipFile
            n_ = []
            with ZipFile(f_, 'r') as z_:
                for i_ in z_.namelist():
                    if i_ != 'install.inf':
                        n_.append(i_)

            return n_

        def getVersion(l_):
            from cuda_addonman import opt
            urls_ = opt.ch_def
            lexers_url_ = ''
            for i_ in urls_:
                if i_.find('lexer') != -1:
                    lexers_url_ = i_
            vers_ = ''
            if lexers_url_:
                from cuda_addonman.work_remote import get_url
                lexers_json_ = tempfile.gettempdir() + os.sep + 'cudatext_lexers.json'
                get_url(lexers_url_, lexers_json_, True)
                import json
                lexers_json_data_ = json.loads(open(lexers_json_).read())
                for i_ in lexers_json_data_:
                    for k_, v_ in i_.items():
                        if k_ == 'url' and v_.find(l_) != -1:
                            vers_ = i_['v']

            return vers_

        fn_ = os.path.join(app_path(APP_DIR_SETTINGS), 'packages.ini')
        sec = 'lexer.' + quote(lex.replace(' ', '_')) + '.zip'
        ini_write(fn_, sec, 'd', 'data/lexlib')
        ini_write(fn_, sec, 'f', ';'.join(getZipFilesNames(tempname)))
        ini_write(fn_, sec, 'v', getVersion(sec))
