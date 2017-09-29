from cudatext import *
import cudatext_cmd as cc
from .word_proc import *

def msg(s):
    msg_status('[Vim] '+s)


class Command:
    active = False
    ins = False

    def toggle_mode(self):
        self.ins = False
        self.active = not self.active
        if self.active:
            msg('plugin activated')
        else:
            msg('plugin deactivated')


    def on_key(self, ed_self, key, state):
        if not self.active: return

        if key==27: #Esc
            if self.ins:
                self.ins = False
                msg('command mode')
            return False

        if self.ins:
            msg('insertion mode')
            return

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

        if key==ord('B'):
            ed.cmd(cc.cCommand_GotoWordPrev)
            msg('go to prev word')
            return False

        if key==ord('W'):
            ed.cmd(cc.cCommand_GotoWordNext)
            msg('go to next word')
            return False

        if key==ord('E'):
            goto_word_end()
            msg('go to word end')
            return False

        if key==ord('A'):
            ed.cmd(cc.cCommand_KeyRight)
            self.ins = True
            msg('insertion mode, after current char')
            return False

        if key==ord('I'):
            self.ins = True
            msg('insertion mode, at current char')
            return False


        #block letters
        if state=='':
            if ord('A')<=key<=ord('Z'):
                msg('key not handled: '+chr(key))
                return False


    def on_key_up(self, ed_self, key, state):
        if not self.active: return
