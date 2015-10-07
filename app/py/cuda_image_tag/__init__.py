import os
from cudatext import *
from .imgsize import get_image_size

css_names = ('CSS', 'SCSS', 'SASS', 'Sass', 'Stylus')
_filter = 'Images (jpeg, png, gif)|*.jpg;*.jpeg;*.png;*.gif|'
ini_fn = 'cuda_image_tag.ini'
ini_section = 'op'
ini_keydir = 'dir'

def get_text(fn, sizex, sizey):
    lex = ed.get_prop(PROP_LEXER_CARET)
    eol = ed.get_prop(PROP_EOL)
    indent_size = ed.get_prop(PROP_TAB_SIZE)
    indent_sp = ed.get_prop(PROP_TAB_SPACES)
    indent = ' '*indent_size if indent_sp else '\t'
    
    #consider image is in file's subfolder
    ed_fn = ed.get_filename()
    if ed_fn:
        path = os.path.dirname(ed_fn) + os.sep
        if fn.startswith(path):
            fn = fn[len(path):]
        else:
            fn = '?/' + os.path.basename(fn)
    else:
        fn = '?/' + os.path.basename(fn)
    
    fn = fn.replace('\\', '/')
    
    if lex in css_names:
        return \
          indent + 'background: url("%s");' % fn + eol + \
          indent + 'width: %dpx;' % sizex + eol + \
          indent + 'height: %dpx;' % sizey + eol
    else:
        return '<img src="%s" width="%d" height="%d" alt="untitled" />' % (fn, sizex, sizey)

def get_filename():
    folder = ini_read(ini_fn, ini_section, ini_keydir, '')
    fn = dlg_file(True, '', folder, _filter)
    if fn:
        ini_write(ini_fn, ini_section, ini_keydir, os.path.dirname(fn))
        return fn

class Command:
    def run(self):
        if app_api_version()<'1.0.104':
            msg_box('Plugin needs newer app', MB_OK)
            return
    
        fn = get_filename()
        if not fn: return
        dim = get_image_size(fn)
        if dim is None:
            msg_box('Cannot detect image file:\n%s' % fn, MB_OK+MB_ICONWARNING)
            return
         
        x, y = dim   
        #print('Image file: %s, %d x %d' % (fn, x, y))
        text = get_text(fn, x, y)
        if not text: return
        
        npos = text.find('"')+1

        x0, y0, x1, y1 = ed.get_carets()[0]        
        ed.insert(x0, y0, text)
        ed.set_caret(x0+npos, y0)
        msg_status('Image info inserted')
