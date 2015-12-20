''' Plugin for CudaText editor
Authors:
    Andrey Kvichansky    (kvichans on github.com)
Version:
    '0.9.6 2015-12-18'
'''

from .cd_macros import Command as CommandRLS

RLS  = CommandRLS()
class Command:
    def on_start(self, ed_self):                return RLS.on_start(ed_self)
    def dlg_config(self):                       return RLS.dlg_config()
    def dlg_export(self):                       return RLS.dlg_export()
    def dlg_import(self):                       return RLS.dlg_import()
    def on_macro(self, ed_self, mcr_record):    return RLS.on_macro(ed_self, mcr_record)
    def run(self, mcr_id):                      return RLS.run(mcr_id)
    #class Command
