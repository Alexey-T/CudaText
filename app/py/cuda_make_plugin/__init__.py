import os
from cudatext import *
from .dlg import *
from .events import *

fn_sample = os.path.join(os.path.dirname(__file__), 'sample.py')
dir_py = app_path(APP_DIR_PY)


class Command:
    def run(self):
        res = dlg_make_plugin()
        if res is None: return
        (s_caption, s_module, cmd_list, event_list) = res
        
        if len(cmd_list)>1:
            prefix = s_caption+'\\'
            cmd_list = [(prefix+s[0], s[1], s[2]) for s in cmd_list] 

        #-------------
        # create dir
        dir_plugin = os.path.join(dir_py, s_module)
        if os.path.isdir(dir_plugin):
            msg_box('Cannot create plugin, folder already exists:\n' + dir_plugin, MB_OK+MB_ICONERROR)
            return
            
        try:
            os.mkdir(dir_plugin)
        except:
            msg_box('Cannot create dir:\n' + dir_plugin, MB_OK+MB_ICONERROR)
            return
        
        #-------------
        # create __init__.py
        fn_py = os.path.join(dir_plugin, '__init__.py')
        with open(fn_py, 'w') as f:
            f.write('from cudatext import *\n\n')
            f.write('class Command:\n')

            #commands
            for (i, item) in enumerate(cmd_list):
                f.write('    def %s(self):\n'%(item[1]))
                if i==0:
                    f.write(open(fn_sample).read())
                else:
                    f.write('        pass\n')

            #events
            for item in event_list:
                par = 'self, ed_self'
                par_add = EVENTS_ADD_PARAMS.get(item, '')
                if par_add:
                    par+=', '+par_add
                f.write('    def %s(%s):\n'%(item, par))
                f.write('        pass\n')

        #------------
        # create install.inf
        fn_inf = os.path.join(dir_plugin, 'install.inf')
        with open(fn_inf, 'w') as f:
            f.write('[info]\n')
            f.write('title='+s_caption+'\n')
            f.write('desc=Some description\n')
            f.write('type=cudatext-plugin\n')
            f.write('subdir='+s_module+'\n')
            f.write('homepage=\n')
            f.write('\n')
            
            if cmd_list:
                for (n, item) in enumerate(cmd_list):
                    f.write('[item%d]\n'%(n+1))
                    f.write('section=commands\n')
                    f.write('caption='+item[0]+'\n')
                    f.write('method='+item[1]+'\n')
                    if item[2]:
                        f.write('menu=0\n')
                    f.write('\n')

            if event_list:            
                f.write('[item%d]\n'%(len(cmd_list)+1))
                f.write('section=events\n')
                f.write('events='+','.join(event_list)+'\n')
                f.write('\n')
                
        
        #------------
        # done
        file_open(fn_py)
        msg_box('Plugin was created.\nMenu item(s) will appear after restart of program.', MB_OK+MB_ICONINFO)
