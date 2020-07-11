import os
from cudatext import *

class Command:
    dlg=0
    fnd=0
    edh=0
    edtext=None
    
    def apply_opt(self, back, from_caret):

        op=''
        v=dlg_proc(self.dlg, DLG_CTL_PROP_GET, name='reex')
        if v['val']=='1': op+='r'
        v=dlg_proc(self.dlg, DLG_CTL_PROP_GET, name='case')
        if v['val']=='1': op+='c'
        v=dlg_proc(self.dlg, DLG_CTL_PROP_GET, name='word')
        if v['val']=='1': op+='w'
        v=dlg_proc(self.dlg, DLG_CTL_PROP_GET, name='inse')
        if v['val']=='1': op+='s'
        v=dlg_proc(self.dlg, DLG_CTL_PROP_GET, name='wrap')
        if v['val']=='1': op+='a'
        v=dlg_proc(self.dlg, DLG_CTL_PROP_GET, name='cfm')
        if v['val']=='1': op+='o'
        
        if back: op+='b'
        if from_caret: op+='f'
        
        finder_proc(self.fnd, FINDER_SET_OPT, op)
        
        finder_proc(self.fnd, FINDER_SET_ED, self.edh)

        s_find=self.edfind.get_text_all()
        finder_proc(self.fnd, FINDER_SET_FINDTEXT, s_find)

        s_rep=self.edrep.get_text_all()
        finder_proc(self.fnd, FINDER_SET_REPTEXT, s_rep)

        v=dlg_proc(self.dlg, DLG_CTL_PROP_GET, name='crts')
        s=v['val']
        if s:
            print('finder carets:', s)
        finder_proc(self.fnd, FINDER_SET_CARETS, s)
        
        finder_proc(self.fnd, FINDER_SET_ON_REPLACE, 'cuda_testing_finder_proc.finder_confirm')
        
        print('finder: opt: "'+op+'", find what: "'+s_find+'", replace: "'+s_rep+'"')
        
    def do_count(self, id_dlg, id_ctl, data='', info=''):

        self.apply_opt(False, False)
        res=finder_proc(self.fnd, FINDER_COUNT)
        print('count:', res)
    
    def do_find(self, id_dlg, id_ctl, data='', info=''):

        self.apply_opt(False, False)
        res=finder_proc(self.fnd, FINDER_FIND, setcaret=True)
        print('find-first:', res)
    
    def do_find_next(self, id_dlg, id_ctl, data='', info=''):

        self.apply_opt(False, True)
        res=finder_proc(self.fnd, FINDER_FIND, setcaret=True)
        print('find-next:', res)
    
    def do_find_prev(self, id_dlg, id_ctl, data='', info=''):

        self.apply_opt(True, True)
        res=finder_proc(self.fnd, FINDER_FIND, setcaret=True)
        print('find-prev:', res)
    
    def do_find_all(self, id_dlg, id_ctl, data='', info=''):

        self.apply_opt(False, False)
        res=finder_proc(self.fnd, FINDER_FIND_ALL)
        print('find-all:', res)
    
    def do_rep(self, id_dlg, id_ctl, data='', info=''):

        self.apply_opt(False, True)
        res=finder_proc(self.fnd, FINDER_FIND_REP, setcaret=True)
        print('replace-next:', res)
    
    def do_rep_all(self, id_dlg, id_ctl, data='', info=''):

        self.apply_opt(False, False)
        res=finder_proc(self.fnd, FINDER_REP_ALL)
        print('replace-all:', res)
    
    def do_rep_all_ex(self, id_dlg, id_ctl, data='', info=''):

        self.apply_opt(False, False)
        res=finder_proc(self.fnd, FINDER_REP_ALL_EX)
        print('replace-all-ex:', res)
    
    def run(self):
        
        idd=dlg_proc(0, DLG_CREATE)

        idc=dlg_proc(idd, DLG_CTL_ADD,"checkbutton");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'reex', 'x':5, 'y':5, 'w':50, 'h':28, 'cap':'&.*'})

        idc=dlg_proc(idd, DLG_CTL_ADD,"checkbutton");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'case', 'x':55, 'y':5, 'w':50, 'h':28, 'cap':'&cC'})

        idc=dlg_proc(idd, DLG_CTL_ADD,"checkbutton");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'word', 'x':105, 'y':5, 'w':50, 'h':28, 'cap':'"&w"'})

        idc=dlg_proc(idd, DLG_CTL_ADD,"checkbutton");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'inse', 'x':155, 'y':5, 'w':50, 'h':28, 'cap':'[&...]'})

        idc=dlg_proc(idd, DLG_CTL_ADD,"checkbutton");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'wrap', 'x':205, 'y':5, 'w':50, 'h':28, 'cap':'wrap'})

        idc=dlg_proc(idd, DLG_CTL_ADD,"checkbutton");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'cfm', 'x':255, 'y':5, 'w':100, 'h':28, 'cap':'confirm-repl'})

        idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'wha_', 'x':5, 'y':30, 'w':200, 'h':17, 'cap':'&Find what:'})

        idc=dlg_proc(idd, DLG_CTL_ADD,"editor");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'what', 'x':5, 'y':50, 'w':200, 'h':70})
        h=dlg_proc(idd, DLG_CTL_HANDLE, name='what')
        self.edfind=Editor(h)
        self.edfind.set_text_all('aa')
        self.edfind.set_prop(PROP_GUTTER_ALL, False)

        idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'rep_', 'x':230, 'y':30, 'w':200, 'h':17, 'cap':'&Replace with:'})

        idc=dlg_proc(idd, DLG_CTL_ADD,"editor");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'repl', 'x':230, 'y':50, 'w':200, 'h':70 })
        h=dlg_proc(idd, DLG_CTL_HANDLE, name='repl')
        self.edrep=Editor(h)
        self.edrep.set_text_all('[..]')
        self.edrep.set_prop(PROP_GUTTER_ALL, False)

        idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'crt_', 'x':5, 'y':120, 'w':200, 'h':17, 'cap':'&Virtual carets: "x1,y1,x2,y2;x1,y1,x2,y2;..."'})

        idc=dlg_proc(idd, DLG_CTL_ADD,"edit");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'crts', 'x':5, 'y':140, 'w':200 })

        idc=dlg_proc(idd, DLG_CTL_ADD,"button");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'frst', 'x':5, 'y':170, 'w':100, 'h':25, 'cap':'Find first', 'on_change': self.do_find })

        idc=dlg_proc(idd, DLG_CTL_ADD,"button");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'next', 'x':5, 'y':200, 'w':100, 'h':25, 'cap':'Find next', 'on_change': self.do_find_next })

        idc=dlg_proc(idd, DLG_CTL_ADD,"button");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'prev', 'x':105, 'y':170, 'w':100, 'h':25, 'cap':'Find prev', 'on_change': self.do_find_prev })

        idc=dlg_proc(idd, DLG_CTL_ADD,"button");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'fall', 'x':105, 'y':200, 'w':100, 'h':25, 'cap':'Find all', 'on_change': self.do_find_all })

        idc=dlg_proc(idd, DLG_CTL_ADD,"button");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'rone', 'x':230, 'y':170, 'w':100, 'h':25, 'cap':'Replace next', 'on_change': self.do_rep })

        idc=dlg_proc(idd, DLG_CTL_ADD,"button");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'rall', 'x':230, 'y':200, 'w':100, 'h':25, 'cap':'Replace all', 'on_change': self.do_rep_all })

        idc=dlg_proc(idd, DLG_CTL_ADD,"button");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'rone', 'x':335, 'y':170, 'w':100, 'h':25, 'cap':'Replace all ex', 'on_change': self.do_rep_all_ex })

        idc=dlg_proc(idd, DLG_CTL_ADD,"button");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'coun', 'x':335, 'y':200, 'w':100, 'h':25, 'cap':'Count', 'on_change': self.do_count })

        idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'tex_', 'x':5, 'y':240, 'w':200, 'h':17, 'cap':'User text'})

        idc=dlg_proc(idd, DLG_CTL_ADD,"editor");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'text', 'x':5, 'y':255, 'w':450, 'h':200})
        self.edh=dlg_proc(idd, DLG_CTL_HANDLE, name='text')
        self.edtext=Editor(self.edh)
        
        self.edtext.set_text_all('''a aa
aaaaa aaaaaa aaaaaaaaaaaaaaa aa
aa aaaa aaa bb a a b b
bb
b
a
aaaaa

bb
a
bbb b bbbbb b b bbb
bb
a
bbb b bbbbb b b bbb
bb
a
bbb b bbbbb b b bbb
bb
a
bbb b bbbbb b b bbb
bb
a
bbb b bbbbb b b bbb
bb
a
bbb b bbbbb b b bbb
bb
a
bbb b bbbbb b b bbb
bb
a
bbb b bbbbb b b bbb
bb
a
bbb b bbbbb b b bbb
bb
a
bbb b bbbbb b b bbb
bb
a
bbb b bbbbb b b bbb
bb
a
bbb b bbbbb b b bbb
bb
a
bbb b bbbbb b b bbb
bb
a
bbb b bbbbb b b bbb
bb
a
bbb b bbbbb b b bbb
bb
a
bbb b bbbbb b b bbb
bb
a
bbb b bbbbb b b bbb
''')

        dlg_proc(idd, DLG_PROP_SET, prop={
            'w':500, 'h':500, 'cap':'Testing finder_proc', 'tag':''})

        self.dlg=idd
        self.fnd=finder_proc(0, FINDER_CREATE)
        
        dlg_proc(idd, DLG_SHOW_MODAL)
        dlg_proc(idd, DLG_FREE)
        
        
    def finder_confirm(self, ed_self, x1, y1, x2, y2, text):
        
        if x1==0:
            return '[line begin]'
        
        r = msg_box('Replace text at Line %d Col %d?'%(y1+1, x1+1),
                    MB_YESNOCANCEL+MB_ICONQUESTION) 
        if r==ID_YES:
            return HOWREP_REPLACE
        elif r==ID_NO:
            return HOWREP_SKIP
        else:
            return HOWREP_CANCEL
