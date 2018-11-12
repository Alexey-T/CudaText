import sys, os
if os.name=='nt':
    os.putenv('PYTHONIOENCODING', 'UTF-8')

_v = sys.version_info
print("Python %d.%d.%d" % (_v[0], _v[1], _v[2]) )

# it's to test API in console
from cudatext import *

fn = os.path.join(app_path(APP_DIR_PY), 'cudax_lib')
if os.path.isdir(fn):
    msg_box('CudaText has found that old folder'+
            ' "%s" still exists, but it must be removed. Please delete it manually.'%fn, 
            MB_OK+MB_ICONWARNING)
