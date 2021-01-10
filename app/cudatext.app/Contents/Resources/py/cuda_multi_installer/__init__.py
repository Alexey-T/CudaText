import os
from cudatext import *
import cudax_lib as apx
from   cudax_lib import get_translation
import cuda_addonman
import urllib.request
import tempfile

_   = get_translation(__file__)  # I18N

URL_DB = 'https://raw.githubusercontent.com/Alexey-T/CudaText-registry/master/multi_inst/db.py'
COLUMN_LEN = 20
COLUMN_W = 240

def str_to_bool(s):
    return s == '1'
def bool_to_str(v):
    return '1' if v else '0'

def is_file_html(fn):
    if os.path.exists(fn):
        with open(fn, 'r', encoding='cp437') as f:
            s = f.readline(10).lower()
            return s.startswith('<html>')
    return False


def make_plugin_group(regex, name):

    apx.set_opt('plugin_groups/'+regex, name)


class Command:

    def show_wait(self):

        self.h_wait = dlg_proc(0, DLG_CREATE)
        dlg_proc(self.h_wait, DLG_PROP_SET, prop={
            'cap':_('CudaText Multi Installer'),
            'w': 360,
            'h': 410,
            })

        n = dlg_proc(self.h_wait, DLG_CTL_ADD, prop='label')
        dlg_proc(self.h_wait, DLG_CTL_PROP_SET, index=n, prop={
            'x': 100,
            'y': 190,
            'w': 300,
            'cap': _('Downloading data...'),
        })

        dlg_proc(self.h_wait, DLG_SHOW_NONMODAL)
        app_idle(True)

    def hide_wait(self):

        dlg_proc(self.h_wait, DLG_HIDE)
        dlg_proc(self.h_wait, DLG_FREE)
        self.h_wait = None

    def load_repo(self):

        self.show_wait()
        fn = os.path.join(tempfile.gettempdir(), 'cuda_multi_installer_db.py')
        cuda_addonman.work_remote.get_url(URL_DB, fn, True)
        self.hide_wait()

        if not os.path.exists(fn):
            self.packets = []
            self.installed_list = []
            return

        exec(open(fn).read())
        os.remove(fn)

        self.packets = cuda_addonman.work_remote.get_remote_addons_list(cuda_addonman.opt.ch_def+cuda_addonman.opt.ch_user)
        self.installed_list = cuda_addonman.work_local.get_installed_modules()

    def get_module(self,kind,name):

        k = TYPE_TO_KIND.get(kind)
        if not k:
            return ''
        for i in self.packets:
            if i['kind']==k and i['name']==name:
                return i.get('module','')
        return ''

    def get_url(self,kind,name):

        k = TYPE_TO_KIND.get(kind)
        for i in self.packets:
            if i['kind']==k and i['name']==name:
                return (i['url'], i['v'])
        return ('', '')

    def is_installed(self,kind,name):

        if kind==T_LEXER:
            d = os.path.join(app_path(APP_DIR_DATA), 'lexlib')
            lst = os.listdir(d)
            lst = [i.lower() for i in lst]
            name_file = name.lower().replace('_', ' ')+'.lcf'
            res = name_file in lst
            #if res:
            #    print('lexer installed:', name)
            return res

        m = self.get_module(kind,name)
        res = m and m in self.installed_list
        #if res:
        #    print('installed:', kind,name)
        return res

    def install(self,kind,name):

        url, version = self.get_url(kind, name)
        if not url:
            print(_('Not found: {} {}').format(kind, name))
            self.error_count+=1
            return

        state=_('Installing: {} {}').format(kind, name)
        print(state)
        self.show_progress()

        fn = cuda_addonman.work_remote.get_plugin_zip(url)

        # handle SF.net HTML error file
        if is_file_html(fn):
            os.remove(fn)

        if not os.path.isfile(fn):
            self.error_count += 1
            return

        ok = file_open(fn, options='/silent')
        if ok:
            self.ok_count += 1
            cuda_addonman.work_local.do_save_version(url, fn, version)


    def open_menu(self):

        self.load_repo()
        if not self.packets:
            msg_status(_('Multi Installer: cannot download list'))
            return

        langs = list(PLUGINS.keys())
        langs.sort()
        h=app_proc(PROC_GET_GUI_HEIGHT,'check')

        to_install = {}
        for i in CLASSES:
            to_install[i] = []

        RES_LIST = 2
        RES_NEXT = 4

        res = dlg_custom(_('CudaText Multi Installer'), 360, 410, '\n'.join([
            '\1'.join(['type=label','pos=5,5,350,0','cap='+_('Select groups of add-ons needed for you.')]),
            '\1'.join(['type=label','pos=5,25,350,0','cap='+_('Next steps will suggest group items.')]),
            '\1'.join(['type=checklistbox','pos=5,48,355,370','items='+
                '\t'.join(langs)
                ]),
            '\1'.join(['type=button','pos=5,375,85,0','cap='+_('Cancel')]),
            '\1'.join(['type=button','pos=275,375,355,0','cap='+_('Next')]),
            ]),
            get_dict=True
            )
        if res is None:
            return
        if res['clicked']!=RES_NEXT:
            return

        #if res[RES_GR_WEB]=='1':
        #    make_plugin_group('HTML.+', 'Web')
        #    make_plugin_group('CSS.+', 'Web')
        #    make_plugin_group('JS.+', 'Web')
        #    make_plugin_group('PHP.+', 'Web')
        #
        #if res[RES_GR_PY]=='1':
        #    make_plugin_group('Python.+', 'Python')
        #
        #if res[RES_GR_XML]=='1':
        #    make_plugin_group('XML.+', 'XML')

        res_list = res[RES_LIST].split(';')[1].split(',')
        res_list = list(map(str_to_bool,res_list))
        step_count = sum(map(int,res_list))+1
        step_index = 2

        for i,f in enumerate(res_list):
            if f:
                cl = 0
                line = 0
                UI = []
                UI_reg = [()]
                for curr_class in CLASSES:
                        pls = PLUGINS[langs[i]].setdefault(curr_class)
                        if pls:
                            if line in (COLUMN_LEN,COLUMN_LEN-1):
                                cl+=1
                                line = 0
                            UI.append('\1'.join([
                                            'type=label',
                                            'pos=%d,%d,%d,%d'%(5+COLUMN_W*cl, line*h+5, COLUMN_W*(cl+1), line*20+25),
                                            'cap='+CLASSES_MSGS[curr_class]
                                            ]))
                            UI_reg.append(())
                            line+=1
                            for pl in pls:
                                if line==COLUMN_LEN:
                                    cl+=1
                                    line = 0
                                flag_en = not self.is_installed(curr_class,pl)
                                UI.append('\1'.join([
                                                'type=check',
                                                'pos=%d,%d,%d,%d'%(5+COLUMN_W*cl, line*h, COLUMN_W*(cl+1), line*20+25),
                                                'cap='+pl.replace('_',' '),
                                                'en='+bool_to_str(flag_en)
                                                ]))
                                UI_reg.append((curr_class,pl))
                                line+=1
                if cl!=0:
                    line=COLUMN_LEN
                UI = ['\1'.join([
                            'type=button',
                            'pos=%d,%d,%d,%d'%(COLUMN_W*(cl+1)-86, line*h+5, COLUMN_W*(cl+1)-6, line*20+25),
                            'cap='+_('Next')
                            ])] +\
                    UI +\
                    ['\1'.join([
                            'type=label',
                            'pos=%d,%d,%d,0'%(COLUMN_W*(cl+1)-180, line*h+8, COLUMN_W*(cl+1)-90),
                            'cap='+_('Step {} of {}').format(step_index,step_count)
                            ])]
                line+=1
                cl+=1
                step_index += 1
                res2 = dlg_custom(
                        _('Select add-ons - ')+langs[i],
                        COLUMN_W*cl,
                        line*h+15,
                        '\n'.join(UI),
                        get_dict=True
                        )
                if res2:
                    if res2['clicked']==0:
                        for ii in range(len(UI_reg)):
                            if UI_reg[ii] and res2[ii]=='1':
                                to_install[UI_reg[ii][0]].append(UI_reg[ii][1])

        fill = False
        for k,v in to_install.items():
            if v:
                fill = True
                break

        if fill:
            self.error_count = 0
            self.ok_count = 0
            self.total_count = sum([len(to_install[i]) for i in CLASSES])

            self.init_progress()
            dlg_proc(self.h_pro, DLG_SHOW_NONMODAL)

            def do_req(kind):
                req = REQS.get(kind)
                if req:
                    if not self.is_installed(T_OTHER,req):
                        self.install(T_OTHER,req)

            for i in to_install[T_LEXER]:
                self.install(T_LEXER,i)
            if to_install[T_LINTER]:
                do_req(T_LINTER)
                for i in to_install[T_LINTER]:
                    self.install(T_LINTER,i)
            if to_install[T_FMT]:
                do_req(T_FMT)
                for i in to_install[T_FMT]:
                    self.install(T_FMT,i)
            if to_install[T_TREE]:
                do_req(T_TREE)
                for i in to_install[T_TREE]:
                    self.install(T_TREE,i)
            if to_install[T_SNIP]:
                do_req(T_SNIP)
                for i in to_install[T_SNIP]:
                    self.install(T_SNIP,i)
            for i in to_install[T_INTEL]:
                self.install(T_INTEL,i)
            for i in to_install[T_OTHER]:
                self.install(T_OTHER,i)

            dlg_proc(self.h_pro, DLG_HIDE)
            dlg_proc(self.h_pro, DLG_FREE)

            msg_status(_('Multi Installer: done'), True)

            s = 'Multi Installer:'
            if self.ok_count>0:
                s += _('\n{} add-on(s) installed').format(self.ok_count)
            if self.error_count>0:
                s += _('\n{} download error(s) (SF.net has problems?)').format(self.error_count)
            msg_box(s, MB_OK+MB_ICONINFO)

        else:
            msg_status(_('Multi Installer: nothing selected'), True)


    def init_progress(self):

        self.h_pro = dlg_proc(0, DLG_CREATE)
        dlg_proc(self.h_pro, DLG_PROP_SET, prop={
            'cap': _('Multi Installer'),
            'w': 400,
            'h': 110,
            'topmost': True,
            })

        n = dlg_proc(self.h_pro, DLG_CTL_ADD, prop='label')
        dlg_proc(self.h_pro, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'inf',
            'cap': _('Installing...'),
            'x': 10,
            'y': 30,
            })

        n = dlg_proc(self.h_pro, DLG_CTL_ADD, prop='progressbar')
        dlg_proc(self.h_pro, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'pro',
            'x': 10,
            'y': 55,
            'w': 380,
            'h': 15,
            'ex1': 0, #min
            'ex2': 100, #max
            'ex3': True, #smooth
            })

    def show_progress(self):

        v = (self.error_count+self.ok_count)*100//self.total_count

        dlg_proc(self.h_pro, DLG_CTL_PROP_SET, name='pro', prop={'val': v,})
        app_idle(False)
