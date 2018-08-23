import os
if os.name=='nt':
    os.putenv('PYTHONIOENCODING', 'UTF-8')

# "sys" already imported
_v = sys.version_info
print("Python %d.%d.%d" % (_v[0], _v[1], _v[2]) )

# can comment this, it's to test API in console
from cudatext import *

# suggest to install plugins for popular langs
if not os.path.exists(os.path.join(app_path(APP_DIR_SETTINGS), 'history.json')):
    try:
        import cuda_multi_installer
        cuda_multi_installer.Command().open_menu()
    except:
        pass
