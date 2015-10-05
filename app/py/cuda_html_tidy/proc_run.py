import os
import subprocess

def do_run_normal(command):
    subprocess.call(command)

def do_run_hide(command):
    if os.name=='posix':
        subprocess.call(command)
    else:
        si = subprocess.STARTUPINFO()
        si.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        si.wShowWindow = subprocess.SW_HIDE
        subprocess.call(command, startupinfo=si)
