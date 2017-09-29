from cudatext import *
import cudatext_cmd as cc

def msg(s, is_ins=False):
    prefix = '[Vim command mode]' if not is_ins else '[Vim insertion mode]'
    msg_status(prefix+' '+s)


class Command:
    active = False

    def toggle_mode(self):
        self.active = not self.active
        if self.active:
            msg('turned on')
        else:
            msg('turned off')

    def on_key(self, ed_self, key, state):
        if not self.active: return

        if key==ord('H'):
            ed.cmd(cc.cCommand_KeyLeft)
            msg('left')
            return False

        if key==ord('J'):
            ed.cmd(cc.cCommand_KeyDown)
            msg('down')
            return False

        if key==ord('K'):
            ed.cmd(cc.cCommand_KeyUp)
            msg('up')
            return False

        if key==ord('L'):
            ed.cmd(cc.cCommand_KeyRight)
            msg('right')
            return False



    def on_key_up(self, ed_self, key, state):
        if not self.active: return
