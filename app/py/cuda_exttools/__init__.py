''' Plugin for CudaText editor
Authors:
    Andrey Kvichansky    (kvichans on github.com)
Version:
    '0.8.1 2015-12-15'
'''

from .cd_exttools import Command as CommandRLS

RLS  = CommandRLS()
class Command:
    def on_start(self, ed_self):    return RLS.on_start(ed_self)
    def dlg_config(self):           return RLS.dlg_config()
    def run(self, ext_id):          return RLS.run(ext_id)
    def run_lxr_main(self):         return RLS.run_lxr_main()
   #class Command
