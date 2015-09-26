import os
import string
import json
from cudatext import *

fn_sample = os.path.join(os.path.dirname(__file__), 'sample.py')
fn_plugins = os.path.join(app_path(APP_DIR_SETTINGS), 'plugins.json')    
dir_py = app_path(APP_DIR_PY)

def is_module_name(name):
    if not name: return False
    chars = string.ascii_letters + string.digits + '_'
    for s in name:
        if not s in chars:
            return False
    return True 

class Command:
    def run(self):
        s = dlg_input_ex(2, 'New plugin', 
          'Lowercase module name, e.g. "my_sample"', 'sample',
          'Menu item caption', 'My Plugin')
        if s is None: return
        s_module = s[0]
        s_caption = s[1]
        
        if not s_module or not s_caption: return
        if not is_module_name(s_module):
            msg_box('Incorrect module name: "%s"' % s_module, MB_OK+MB_ICONERROR)
            return

        s_module = 'cuda_'+s_module
        dir_plugin = os.path.join(dir_py, s_module)
        if os.path.isdir(dir_plugin):
            msg_box('Cannot create plugin; folder already exists:\n' + dir_plugin, MB_OK+MB_ICONERROR)
            return
            
        try:
            os.mkdir(dir_plugin)
        except:
            msg_box('Cannot create folder:\n' + dir_plugin, MB_OK+MB_ICONERROR)
            return
        
        fn_py = os.path.join(dir_plugin, '__init__.py')
        text = open(fn_sample).read()
        with open(fn_py, 'w') as f:
            f.write(text)
            
        with open(fn_plugins, 'r') as f:
            d = json.load(f)
        d['commands'][s_module] = {'run':{'caption':s_caption}}
        with open(fn_plugins, 'w') as f:
            f.write(json.dumps(d, indent=2))
                  
        fn_install_inf = os.path.join(dir_plugin, 'install.inf')
        fn_sample_inf = os.path.join(os.path.dirname(__file__), 'sample.inf')
        text = open(fn_sample_inf).read()
        text = text.replace('{subdir}', s_module).replace('{menuitem}', s_caption)
        with open(fn_install_inf, 'w') as f:
            f.write(text)
        
        file_open(fn_py)
        msg_box('Plugin was created. Menu item "Plugins - %s" will appear after restart of program.' 
                % s_caption, MB_OK+MB_ICONINFO)
