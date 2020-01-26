import sys, os
from cudatext import *

if os.name=='nt':
    os.putenv('PYTHONIOENCODING', 'UTF-8')

print("Python %d.%d.%d" % sys.version_info[:3])
