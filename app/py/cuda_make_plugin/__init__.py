import os
from cudatext import *
from cudax_lib import get_translation
from .dlg import *
from .events import *

_   = get_translation(__file__)  # I18N

fn_sample_begin = os.path.join(os.path.dirname(__file__), 'sample_begin.py')
fn_sample_begin_config = os.path.join(os.path.dirname(__file__), 'sample_begin_config.py')
fn_sample_body = os.path.join(os.path.dirname(__file__), 'sample_body.py')
fn_sample_body_config = os.path.join(os.path.dirname(__file__), 'sample_body_config.py')
dir_py = app_path(APP_DIR_PY)


class Command:
    def run(self):
        res = dlg_make_plugin()
        if res is None: return
        (s_caption, s_module, cmd_list, event_list, with_config) = res

        #-------------
        # create dir
        dir_plugin = os.path.join(dir_py, s_module)
        if os.path.isdir(dir_plugin):
            msg_box(_('Cannot create plugin, folder already exists:\n') + dir_plugin, MB_OK+MB_ICONERROR)
            return

        try:
            os.mkdir(dir_plugin)
        except:
            msg_box(_('Cannot create dir:\n') + dir_plugin, MB_OK+MB_ICONERROR)
            return

        #-------------
        # create __init__.py
        fn_py = os.path.join(dir_plugin, '__init__.py')
        with open(fn_py, 'w', encoding='utf8') as f:
            if with_config:
                text = open(fn_sample_begin_config, encoding='utf8').read()
                text = text.format(module=s_module)
            else:
                text = open(fn_sample_begin, encoding='utf8').read()
            f.write(text)

            #commands
            for (i, item) in enumerate(cmd_list):
                f.write('    def %s(self):\n'%(item[1]))
                if i==0:
                    if with_config:
                        f.write(open(fn_sample_body_config, encoding='utf8').read())
                    else:
                        f.write(open(fn_sample_body, encoding='utf8').read())
                else:
                    f.write('        pass\n\n')

            #events
            for item in event_list:
                par = 'self, ed_self'
                par_add = EVENTS_ADD_PARAMS.get(item, '')
                if par_add:
                    par+=', '+par_add
                f.write('    def %s(%s):\n'%(item, par))
                f.write('        pass\n\n')

        #------------
        # create install.inf
        fn_inf = os.path.join(dir_plugin, 'install.inf')
        with open(fn_inf, 'w', encoding='utf8') as f:
            f.write('[info]\n')
            f.write('title='+s_caption+'\n')
            f.write('desc='+_('(Fill the description)\n'))
            f.write('type=cudatext-plugin\n')
            f.write('subdir='+s_module+'\n')
            f.write('homepage='+_('(Fill the GitHub repo URL)\n'))
            f.write('\n')

            if event_list:
                f.write('[item1]\n')
                f.write('section=events\n')
                f.write('events='+','.join(event_list)+'\n')
                f.write('\n')

            if cmd_list:
                for (n, item) in enumerate(cmd_list):
                    f.write('[item%d]\n'%(n+2)) #start at [item2]
                    f.write('section=commands\n')
                    f.write('caption='+s_caption+'\\'+item[0]+'\n')
                    f.write('method='+item[1]+'\n')
                    if item[2]:
                        f.write('menu=0\n')
                    f.write('\n')

            if with_config:
                f.write('[item100]\n')
                f.write('section=commands\n')
                f.write('caption='+s_caption+'\\Config\n')
                f.write('method=config\n')
                f.write('menu=o\n')
                f.write('\n')

        #------------
        # done
        file_open(fn_py)
        msg_box(_('Plugin was created.\nMenu item(s) will appear after restart of program.'), MB_OK+MB_ICONINFO)
