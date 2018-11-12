import sys, os
if os.name=='nt':
    os.putenv('PYTHONIOENCODING', 'UTF-8')

_v = sys.version_info
print("Python %d.%d.%d" % (_v[0], _v[1], _v[2]) )

# it's to test API in console
from cudatext import *

# check old crap files/dirs
bads = [
    os.path.join(app_path(APP_DIR_PY), 'cudax_lib'),
    os.path.join(app_path(APP_DIR_PY), 'requests'),
    os.path.join(app_path(APP_DIR_SETTINGS), 'lexer styles backup.ini'),
    ]
for fn in bads:
    if os.path.exists(fn):
        msg_box('CudaText has found that old file/folder still exists:\n%s\n\nPlease delete it manually.'%fn, 
                MB_OK+MB_ICONWARNING)
