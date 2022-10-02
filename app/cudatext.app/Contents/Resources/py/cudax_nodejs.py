import os
import platform
import subprocess

MSG_CANNOT_RUN_NODE = "Cannot run Node.js. Make sure it's in your PATH."

# https://stackoverflow.com/questions/377017/test-if-executable-exists-in-python
def which(program):
    def is_exe(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

    fpath, fname = os.path.split(program)
    if fpath:
        if is_exe(program):
            return program
    else:
        for path in os.environ["PATH"].split(os.pathsep):
            exe_file = os.path.join(path, program)
            if is_exe(exe_file):
                return exe_file

    return None


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
