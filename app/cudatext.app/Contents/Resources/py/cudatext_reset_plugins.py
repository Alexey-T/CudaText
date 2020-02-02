import sys
import os
def _reset_plugins(dir):
    l = os.listdir(dir)
    l = [os.path.join(dir, fn) for fn in l if not fn.startswith('__')]
    l = [os.path.basename(fn) for fn in l if os.path.isdir(fn)]
    print()
    for s in l:
        name = 'xx' + s
        if name in globals():
            print('Reset plugin: ' + s)
            exec('global %s; del %s' % (name, name))
            if s in sys.modules:
                del sys.modules[s]
            submods = [sm for sm in sys.modules.keys() if sm.startswith(s+'.')]
            for sm in submods:
                del sys.modules[sm]
