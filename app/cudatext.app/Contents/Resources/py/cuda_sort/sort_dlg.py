from cudatext import *
from cudax_lib import get_translation

_ = get_translation(__file__)  # I18N


def get_offsets():
    if ed.get_sel_mode()==SEL_COLUMN:
        r = ed.get_sel_rect()
        return r[0], r[2]
    else:
        return -1, -1


def do_dialog(CONFIG_FN, CONFIG_SECTION):
    SIZE_W = 456
    SIZE_H = 215
    RES_REVERSE = 0
    RES_NOCASE = 1
    RES_DEL_DUP = 2
    RES_DEL_SPACE = 3
    RES_NUMERIC = 4
    RES_OFFSET1 = 7
    RES_OFFSET2 = 9
    RES_SORT = 10
    RES_SAVE = 11

    op_rev = ini_read(CONFIG_FN, CONFIG_SECTION, 'reverse', '0')
    op_nocase = ini_read(CONFIG_FN, CONFIG_SECTION, 'ignore_case', '0')
    op_del_dup = ini_read(CONFIG_FN, CONFIG_SECTION, 'del_dups', '1')
    op_del_sp = ini_read(CONFIG_FN, CONFIG_SECTION, 'del_blanks', '1')
    op_numeric = ini_read(CONFIG_FN, CONFIG_SECTION, 'numeric', '0')

    op_offset1, op_offset2 = get_offsets()
    if op_offset1==-1 and op_offset2==-1:
        op_offset1 = int(ini_read(CONFIG_FN, CONFIG_SECTION, 'offset1', '-1'))
        op_offset2 = int(ini_read(CONFIG_FN, CONFIG_SECTION, 'offset2', '-1'))

    c1 = chr(1)
    text = '\n'.join([
      c1.join(['type=check', 'pos=6,6,300,0', 'cap='+_('&Sort descending (reverse)'), 'val='+op_rev]),
      c1.join(['type=check', 'pos=6,30,300,0', 'cap='+_('&Ignore case'), 'val='+op_nocase]),
      c1.join(['type=check', 'pos=6,54,300,0', 'cap='+_('Delete d&uplicate lines'), 'val='+op_del_dup]),
      c1.join(['type=check', 'pos=6,78,300,0', 'cap='+_('Delete &blank lines'), 'val='+op_del_sp]),
      c1.join(['type=check', 'pos=6,102,450,0', 'cap='+_('&Numeric (treat groups of digits as numbers)'), 'val='+op_numeric]),
      c1.join(['type=label', 'pos=6,130,400,0', 'cap='+_('Sort only by substring, offsets are 0-based:')]),
      c1.join(['type=label', 'pos=120,152,450,0', 'cap='+_('Be&gin (-1: don\'t cut begin)')]),
      c1.join(['type=spinedit', 'pos=30,150,110,0', 'ex0=-1', 'ex1=5000', 'ex2=1', 'val='+str(op_offset1)]),
      c1.join(['type=label', 'pos=120,182,450,0', 'cap='+_('&End (-1: don\'t cut end)')]),
      c1.join(['type=spinedit', 'pos=30,180,110,0', 'ex0=-1', 'ex1=5000', 'ex2=1', 'val='+str(op_offset2)]),
      c1.join(['type=button', 'pos=350,6,450,0', 'cap='+_('Sort'), 'ex0=1']),
      c1.join(['type=button', 'pos=350,36,450,0', 'cap='+_('Save only')]),
      c1.join(['type=button', 'pos=350,66,450,0', 'cap='+_('Cancel')]),
      ])

    res = dlg_custom(_('Custom sort'), SIZE_W, SIZE_H, text)
    if res is None: return
    btn, text = res
    if btn not in [RES_SORT, RES_SAVE]: return
    text = text.splitlines()

    is_rev = text[RES_REVERSE]=='1'
    is_nocase = text[RES_NOCASE]=='1'
    is_del_dup = text[RES_DEL_DUP]=='1'
    is_del_sp = text[RES_DEL_SPACE]=='1'
    is_numeric = text[RES_NUMERIC]=='1'
    offset1 = int(text[RES_OFFSET1])
    offset2 = int(text[RES_OFFSET2])

    offsets_ok = offset1<0 or offset2<0 or offset1<offset2

    ini_write(CONFIG_FN, CONFIG_SECTION, 'reverse', text[RES_REVERSE])
    ini_write(CONFIG_FN, CONFIG_SECTION, 'ignore_case', text[RES_NOCASE])
    ini_write(CONFIG_FN, CONFIG_SECTION, 'del_dups', text[RES_DEL_DUP])
    ini_write(CONFIG_FN, CONFIG_SECTION, 'del_blanks', text[RES_DEL_SPACE])
    ini_write(CONFIG_FN, CONFIG_SECTION, 'numeric', text[RES_NUMERIC])
    if offsets_ok:
        ini_write(CONFIG_FN, CONFIG_SECTION, 'offset1', text[RES_OFFSET1])
        ini_write(CONFIG_FN, CONFIG_SECTION, 'offset2', text[RES_OFFSET2])
    else:
        msg_box(_('Incorrect offsets: {}..{}').format(offset1, offset2), MB_OK+MB_ICONERROR)

    if btn == RES_SAVE:
        msg_status(_('Custom sort options saved'))
    elif btn == RES_SORT:
        return (is_rev, is_nocase, is_del_dup, is_del_sp, is_numeric, offset1, offset2)
