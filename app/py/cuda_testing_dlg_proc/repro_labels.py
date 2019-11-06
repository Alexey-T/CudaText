from cudatext import *

def test_labels_render():

    FONT_LST    = [''] \
                + [font 
                    for font in app_proc(PROC_ENUM_FONTS, '')
                    if not font.startswith('@')] 
    text    = 'Sample Text'

    def on_ok(idd,idc,data):
        fnm = dlg_proc(idd, DLG_CTL_PROP_GET, name='fnm')['val']
        if not fnm: return 
        for t in ('n', 'b', 'i'):
            for sz in ('09', '10', '11', '12', '13', '14'):
                dlg_proc(idd, DLG_CTL_PROP_SET, name=t+sz, prop={'font_name':fnm})

    idd=dlg_proc(0, DLG_CREATE)

    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'fm__', 'x':5, 'y':8, 'w':70, 'cap':'Font name:'})
    idc=dlg_proc(idd, DLG_CTL_ADD,"combo");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'fnm', 'x':100, 'y':5, 'w':400, 'h':25, 
    'items':'\t'.join(FONT_LST)})
    idc=dlg_proc(idd, DLG_CTL_ADD,"button");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'ok', 'x':500, 'y':4, 'w':30, 'h':25, 'cap':'OK', 'ex0':True, "on_change":on_ok})

    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'s09', 'x':5, 'y':40, 'w':15, 'cap':' 9'})
    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'s10', 'x':5, 'y':65, 'w':15, 'cap':'10'})
    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'s11', 'x':5, 'y':90, 'w':15, 'cap':'11'})
    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'s12', 'x':5, 'y':120, 'w':15, 'cap':'12'})
    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'s13', 'x':5, 'y':155, 'w':15, 'cap':'13'})
    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'s14', 'x':5, 'y':195, 'w':15, 'cap':'14'})

    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'n09', 'x':25, 'y':40, 'w':150, 'cap':text, 'font_size': 9})
    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'n10', 'x':25, 'y':65, 'w':150, 'cap':text, 'font_size': 10})
    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'n11', 'x':25, 'y':90, 'w':150, 'cap':text, 'font_size': 11})
    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'n12', 'x':25, 'y':120, 'w':150, 'cap':text, 'font_size': 12})
    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'n13', 'x':25, 'y':155, 'w':150, 'cap':text, 'font_size': 13})
    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'n14', 'x':25, 'y':195, 'w':150, 'cap':text, 'font_size': 14})

    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'b09', 'x':195, 'y':40, 'w':150, 'cap':text, 'font_size': 9, 'font_style': 'b'})
    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'b10', 'x':195, 'y':65, 'w':150, 'cap':text, 'font_size': 10, 'font_style': 'b'})
    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'b11', 'x':195, 'y':90, 'w':150, 'cap':text, 'font_size': 11, 'font_style': 'b'})
    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'b12', 'x':195, 'y':120, 'w':150, 'cap':text, 'font_size': 12, 'font_style': 'b'})
    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'b13', 'x':195, 'y':155, 'w':150, 'cap':text, 'font_size': 13, 'font_style': 'b'})
    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'b14', 'x':195, 'y':195, 'w':150, 'cap':text, 'font_size': 14, 'font_style': 'b'})

    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'i09', 'x':365, 'y':40, 'w':150, 'cap':text, 'font_size': 9, 'font_style': 'i'})
    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'i10', 'x':365, 'y':65, 'w':150, 'cap':text, 'font_size': 10, 'font_style': 'i'})
    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'i11', 'x':365, 'y':90, 'w':150, 'cap':text, 'font_size': 11, 'font_style': 'i'})
    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'i12', 'x':365, 'y':120, 'w':150, 'cap':text, 'font_size': 12, 'font_style': 'i'})
    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'i13', 'x':365, 'y':155, 'w':150, 'cap':text, 'font_size': 13, 'font_style': 'i'})
    idc=dlg_proc(idd, DLG_CTL_ADD,"label");dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={
    'name':'i14', 'x':365, 'y':195, 'w':150, 'cap':text, 'font_size': 14, 'font_style': 'i'})

    dlg_proc(idd, DLG_PROP_SET, prop={
    'x':200, 'y':200, 'w':550, 'h':250, 'cap':'Labels rendering', 'tag':'', 'border':3, 'topmost':True, 'vis':False, 'keypreview':True, 'clicked': -1, 'resize': False, 'p': 0})

    dlg_proc(idd, DLG_SHOW_MODAL)
    dlg_proc(idd, DLG_FREE)
