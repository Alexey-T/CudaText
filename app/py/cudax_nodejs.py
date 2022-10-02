import os
import platform
import subprocess
from shutil import which

MSG_CANNOT_RUN_NODE = "Cannot run Node.js. Make sure it's in your PATH."

#
# Linux: it may be "node" or "nodejs"
# Mac: need to specify path
#
NODE_FILE = 'node'
pl = platform.system()
if pl == 'Darwin':
    NODE_FILE = '/usr/local/bin/node'
else:
    NODE_FILE = which('node') or which('nodejs')


def run_node(text, params_list):
    enc = 'utf-8'
    if os.name == 'nt':
        startupinfo = subprocess.STARTUPINFO()
        startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        startupinfo.wShowWindow = subprocess.SW_HIDE
        try:
            p = subprocess.Popen([NODE_FILE] + params_list,
              startupinfo=startupinfo,
              stdout=subprocess.PIPE,
              stdin=subprocess.PIPE,
              stderr=subprocess.PIPE)
        except OSError:
            raise Exception(MSG_CANNOT_RUN_NODE)
    else:
        try:
            p = subprocess.Popen([NODE_FILE] + params_list,
              stdout=subprocess.PIPE,
              stdin=subprocess.PIPE,
              stderr=subprocess.PIPE)
        except OSError:
            raise Exception(MSG_CANNOT_RUN_NODE)


    stdout, stderr = p.communicate(text.encode(enc))
    if stdout:
        return stdout.decode(enc)
    else:
        raise Exception('Error:\n' + stderr.decode(enc))
