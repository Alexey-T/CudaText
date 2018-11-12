import sys, os
from cudatext import *

if os.name=='nt':
    os.putenv('PYTHONIOENCODING', 'UTF-8')

print("Python %d.%d.%d" % sys.version_info[:3])

bads = [
    os.path.join(app_path(APP_DIR_PY), 'cudax_lib'),
    os.path.join(app_path(APP_DIR_PY), 'requests'),
    os.path.join(app_path(APP_DIR_SETTINGS), 'lexer styles backup.ini'),
    ]
for fn in bads:
    if os.path.exists(fn):
        msg_box('CudaText has found that old file/folder still exists:\n%s\n\nPlease delete it manually.'%fn, 
                MB_OK+MB_ICONWARNING)
