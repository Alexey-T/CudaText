import os
from cudatext import *

class Command:
    dlg=0
    fnd=0
    edh=0
    edtext=None
    
    def apply_opt(self):

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
        
        print('finder options:', op)
        finder_proc(self.fnd, FINDER_SET_OPT, op)
        
        finder_proc(self.fnd, FINDER_SET_ED, self.edh)

        v=dlg_proc(self.dlg, DLG_CTL_PROP_GET, name='wha_')['val']
        finder_proc(self.fnd, FINDER_SET_FINDTEXT, v)

        v=dlg_proc(self.dlg, DLG_CTL_PROP_GET, name='rep_')['val']
        finder_proc(self.fnd, FINDER_SET_REPTEXT, v)
        
        
    def do_find(self, id_dlg, id_ctl, data='', info=''):

        self.apply_opt()
        finder_proc(self.fnd, FINDER_FIND)
        pass
    
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

        idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'wha_', 'x':5, 'y':30, 'w':200, 'h':17, 'cap':'&Find what'})

        idc=dlg_proc(idd, DLG_CTL_ADD,"memo");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'what', 'x':5, 'y':50, 'w':200, 'h':70, 'val': 'aa'})

        idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'rep_', 'x':230, 'y':30, 'w':200, 'h':17, 'cap':'&Replace with'})

        idc=dlg_proc(idd, DLG_CTL_ADD,"memo");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'repl', 'x':230, 'y':50, 'w':200, 'h':70, 'val': 'dd'})

        idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'crt_', 'x':5, 'y':120, 'w':200, 'h':17, 'cap':'&Carets'})

        idc=dlg_proc(idd, DLG_CTL_ADD,"edit");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'crts', 'x':5, 'y':140, 'w':200, 'h':25})

        idc=dlg_proc(idd, DLG_CTL_ADD,"button");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'frst', 'x':5, 'y':170, 'w':100, 'h':25, 'cap':'Find first', 'on_change': self.do_find})

        idc=dlg_proc(idd, DLG_CTL_ADD,"button");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'next', 'x':5, 'y':200, 'w':100, 'h':25, 'cap':'Find next'})

        idc=dlg_proc(idd, DLG_CTL_ADD,"button");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'prev', 'x':105, 'y':170, 'w':100, 'h':25, 'cap':'Find prev'})

        idc=dlg_proc(idd, DLG_CTL_ADD,"button");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'fall', 'x':105, 'y':200, 'w':100, 'h':25, 'cap':'Find all'})

        idc=dlg_proc(idd, DLG_CTL_ADD,"button");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'rone', 'x':230, 'y':170, 'w':100, 'h':25, 'cap':'Replace next'})

        idc=dlg_proc(idd, DLG_CTL_ADD,"button");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'rall', 'x':230, 'y':200, 'w':100, 'h':25, 'cap':'Replace all'})

        idc=dlg_proc(idd, DLG_CTL_ADD,"button");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
        'name':'coun', 'x':335, 'y':200, 'w':100, 'h':25, 'cap':'Count'})

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
        