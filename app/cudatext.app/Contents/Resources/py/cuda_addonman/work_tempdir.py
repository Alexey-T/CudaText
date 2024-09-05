import os
import tempfile

def get_temp_dir():
    d = os.path.join(tempfile.gettempdir(), 'cudatext')
    if not os.path.isdir(d):
        os.mkdir(d)
    return d
