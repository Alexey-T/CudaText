import sys, os
if os.name=='nt':
    os.putenv('PYTHONIOENCODING', 'UTF-8')

_v = sys.version_info
print("Python %d.%d.%d" % (_v[0], _v[1], _v[2]) )

# it's to test API in console
from cudatext import *
