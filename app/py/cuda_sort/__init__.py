import os
from random import randint
from cudatext import *
from cudax_lib import get_translation
from .app_specific import *
from .sort_ini import *
from .sort_emails import *
from .sort_sep import *
from .sort_numeric import *

_   = get_translation(__file__)  # I18N

CONFIG_FN = os.path.join(app_path(APP_DIR_SETTINGS), 'plugins.ini')
CONFIG_SECTION = 'sort'

def get_offsets():
    if ed.get_sel_mode()==SEL_COLUMN:
        r = ed.get_sel_rect()
        return r[0], r[2]
    else:
        return -1, -1


def get_shuffle(lines):
    l1 = list(lines)
    l2 = []
    while l1:
        n = randint(0, len(l1)-1)
        l2.append(l1[n])
        del l1[n]
    return l2


def get_dups(lines, nocase):
    l = list(lines)
    res = []
    while l:
        s = l[0]
        del l[0]
        for i in reversed(range(len(l))):
            if nocase:
                ok = s.lower()==l[i].lower()
            else:
                ok = s==l[i]
            if ok:
                if s not in res:
                    res.append(s)
                del l[i]
    return res


def get_uniq(lines):
    return sorted(list(set(lines)))


def get_input():

    if ed.get_prop(PROP_RO): return

    op_sort_all = ini_read(CONFIG_FN, CONFIG_SECTION, 'allow_sort_all_when_none_selected', '0')=='1'

    '''    
    s = ini_read(CONFIG_FN, CONFIG_SECTION, 'max_lines', str(DEF_MAX_LINES))
    max_cnt = int(s)

    if ed.get_line_count()>max_cnt:
        msg_box(_('Document has too many lines. Plugin Sort will not work. Current value of option [sort] max_lines in "settings/plugins.ini" is %d.\n\nInstead of Sort plugin, use CudaText built-in commands: "(without undo) sort...".') %max_cnt,
        MB_OK+MB_ICONERROR)
        return
    '''

    is_all = False
    nlines = ed.get_line_count()
    line1, line2 = ed_get_sel_lines()

    if line1<0:
        if op_sort_all:
            is_all = True
        else:
            msg_status(_('Needed multiline selection'))
            return
    elif line1>=line2:
        msg_status(_('Needed multiline selection'))
        return

    if is_all:
        lines = ed_get_text_all()
    else:
        #add last empty line
        if ed.get_text_line(nlines-1) != '':
            ed.set_text_line(-1, '')
        lines = [ed.get_text_line(i) for i in range(line1, line2+1)]

    return lines, is_all, line1, line2


def set_output(lines, is_all, line1, line2):

    if is_all:
        ed_set_text_all(lines)
    else:
        ed_insert_to_lines(lines, line1, line2)


def do_line_op(op, keep_blanks=False):

    res = get_input()
    if not res: return
    lines, is_all, line1, line2 = res

    if op=='shuffle':
        lines = get_shuffle(lines)

    elif op=='reverse':
        lines = reversed(lines)

    elif op=='delete_blanks':
        lines = [s for s in lines if s.strip()]

    elif op=='delete_blanks_adjacent':
        for i in reversed(range(len(lines))):
            if i>0 and lines[i].strip()=='' and lines[i-1].strip()=='':
                del lines[i]

    elif op=='delete_dups':
        for i in range(len(lines)-1, 0, -1):
            for j in range(i-1, -1, -1):
                if lines[i]==lines[j]:
                    if lines[i] or not keep_blanks:
                        del lines[i]
                        break

    elif op=='delete_dups_origins':
        dups = get_dups(lines, False)
        for i in reversed(range(len(lines))):
            if lines[i] in dups:
                del lines[i]

    elif op=='delete_dups_adjacent':
        for i in reversed(range(len(lines))):
            if i>0 and lines[i]==lines[i-1]:
                del lines[i]

    else:
        msg_status(_('Unknown operation: ')+op)
        return

    set_output(lines, is_all, line1, line2)
    msg_status(_('Lines operation: ')+op)


def do_extract_op(op):

    res = get_input()
    if not res: return
    lines, is_all, line1, line2 = res

    if op=='dups':
        lines = get_dups(lines, False)
    elif op=='dups_nocase':
        lines = get_dups(lines, True)
    elif op=='unique':
        lines = get_uniq(lines)
    else:
        msg_status(_('Unknown operation: ')+op)
        return

    if not lines:
        msg_status(_('Cannot extract any lines'))
        return

    file_open('')
    ed_set_tab_title(op)
    ed_set_text_all(lines)
    msg_status(_('Extract lines: ')+op)


def do_sort(
        is_reverse,
        is_nocase,
        del_dups=False,
        del_blanks=True,
        is_numeric=False,
        offset1=-1,
        offset2=-1):

    res = get_input()
    if not res: return
    lines, is_all, line1, line2 = res

    if del_blanks:
        lines = [s for s in lines if s.strip()]
    if del_dups:
        lines = list(set(lines))

    def _key(item):
        s = item
        if is_nocase:
            s = s.lower()

        if (offset1>=0) or (offset2>=0):
            s = ed_convert_tabs_to_spaces(s)
            if offset2>=0: s = s[:offset2]
            if offset1>=0: s = s[offset1:]

        #numeric must be after offsets
        if is_numeric:
            return str_to_numeric_key(s)

        return s


    lines = sorted(lines, key=_key, reverse=is_reverse)
    set_output(lines, is_all, line1, line2)

    text = _('Sorted') \
        + (_(', all text') if is_all else '') \
        + (_(', reverse') if is_reverse else '') \
        + (_(', ignore case') if is_nocase else '') \
        + (_(', numeric') if is_numeric else '') \
        + (_(', offsets ') + '%d..%d' % (offset1, offset2) if (offset1>=0) or (offset2>=0) else '')
    msg_status(text)


def do_dialog():
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
      c1.join(['type=check', 'pos=6,102,400,0', 'cap='+_('Numeric (treat groups of digits as numbers)'), 'val='+op_numeric]),
      c1.join(['type=label', 'pos=6,130,400,0', 'cap='+_('Sort only by substring, offsets 0-based:')]),
      c1.join(['type=label', 'pos=30,152,130,0', 'cap='+_('&From:')]),
      c1.join(['type=spinedit', 'pos=30,170,110,0', 'ex0=-1', 'ex1=5000', 'ex2=1', 'val='+str(op_offset1)]),
      c1.join(['type=label', 'pos=120,152,230,0', 'cap='+_('&To:')]),
      c1.join(['type=spinedit', 'pos=120,170,200,0', 'ex0=-1', 'ex1=5000', 'ex2=1', 'val='+str(op_offset2)]),
      c1.join(['type=button', 'pos=350,6,450,0', 'cap='+_('Sort'), 'ex0=1']),
      c1.join(['type=button', 'pos=350,36,450,0', 'cap='+_('Save only')]),
      c1.join(['type=button', 'pos=350,66,450,0', 'cap='+_('Cancel')]),
      ])

    res = dlg_custom(_('Custom sort'), SIZE_W, SIZE_H, text)
    if res is None: return
    btn, text = res
    if btn not in [RES_SORT, RES_SAVE]: return
    text = text.splitlines()

    ini_write(CONFIG_FN, CONFIG_SECTION, 'reverse', text[RES_REVERSE])
    ini_write(CONFIG_FN, CONFIG_SECTION, 'ignore_case', text[RES_NOCASE])
    ini_write(CONFIG_FN, CONFIG_SECTION, 'del_dups', text[RES_DEL_DUP])
    ini_write(CONFIG_FN, CONFIG_SECTION, 'del_blanks', text[RES_DEL_SPACE])
    ini_write(CONFIG_FN, CONFIG_SECTION, 'numeric', text[RES_NUMERIC])
    ini_write(CONFIG_FN, CONFIG_SECTION, 'offset1', text[RES_OFFSET1])
    ini_write(CONFIG_FN, CONFIG_SECTION, 'offset2', text[RES_OFFSET2])

    is_rev = text[RES_REVERSE]=='1'
    is_nocase = text[RES_NOCASE]=='1'
    is_del_dup = text[RES_DEL_DUP]=='1'
    is_del_sp = text[RES_DEL_SPACE]=='1'
    is_numeric = text[RES_NUMERIC]=='1'
    offset1 = int(text[RES_OFFSET1])
    offset2 = int(text[RES_OFFSET2])
    msg_status(_('Custom sort options saved'))

    if (offset1>=0) and (offset2>=0) and (offset1>=offset2):
        msg_show_error(_('Incorrect offsets: {}..{}').format(offset1, offset2))
        return

    if btn == RES_SORT:
        return (is_rev, is_nocase, is_del_dup, is_del_sp, is_numeric, offset1, offset2)


class Command:
    def sort_asc(self):
        do_sort(False, False)

    def sort_desc(self):
        do_sort(True, False)

    def sort_asc_nocase(self):
        do_sort(False, True)

    def sort_desc_nocase(self):
        do_sort(True, True)

    def sort_dlg(self):
        res = do_dialog()
        if res is None: return
        do_sort(*res)

    def sort_custom(self):
        op_rev = ini_read(CONFIG_FN, CONFIG_SECTION, 'reverse', '0') == '1'
        op_nocase = ini_read(CONFIG_FN, CONFIG_SECTION, 'ignore_case', '0') == '1'
        op_del_dup = ini_read(CONFIG_FN, CONFIG_SECTION, 'del_dups', '1') == '1'
        op_del_sp = ini_read(CONFIG_FN, CONFIG_SECTION, 'del_blanks', '1') == '1'
        op_numeric = ini_read(CONFIG_FN, CONFIG_SECTION, 'numeric', '0') == '1'
        op_offset1 = int(ini_read(CONFIG_FN, CONFIG_SECTION, 'offset1', '-1'))
        op_offset2 = int(ini_read(CONFIG_FN, CONFIG_SECTION, 'offset2', '-1'))
        do_sort(
            op_rev,
            op_nocase,
            op_del_dup,
            op_del_sp,
            op_numeric,
            op_offset1,
            op_offset2
            )

    def shuffle(self):
        do_line_op('shuffle')

    def reverse(self):
        do_line_op('reverse')

    def del_dup(self):
        do_line_op('delete_dups')

    def del_dup_keep_blanks(self):
        do_line_op('delete_dups', True)

    def del_dup_orig(self):
        do_line_op('delete_dups_origins')

    def del_dup_adj(self):
        do_line_op('delete_dups_adjacent')

    def del_blank(self):
        do_line_op('delete_blanks')

    def del_blank_adj(self):
        do_line_op('delete_blanks_adjacent')

    def get_dups(self):
        do_extract_op('dups')

    def get_dups_nocase(self):
        do_extract_op('dups_nocase')

    def get_uniq(self):
        do_extract_op('unique')

    def do_sort_ini(self, and_keys):
        case_sens = ini_read(CONFIG_FN, CONFIG_SECTION, 'ini_files_case_sensitive', '0')=='1'
        ini_sort(and_keys, case_sens)

    def ini_sort_all(self):
        self.do_sort_ini(True)

    def ini_sort_not_keys(self):
        self.do_sort_ini(False)

    def sort_emails(self):
        do_sort_emails()

    def sort_sep_values(self):
        do_sort_sep_values()

    def config(self):

        op_sort_all = ini_read(CONFIG_FN, CONFIG_SECTION, 'allow_sort_all_when_none_selected', '0')
        op_ini_case_sens = ini_read(CONFIG_FN, CONFIG_SECTION, 'ini_files_case_sensitive', '0')

        ini_write(CONFIG_FN, CONFIG_SECTION, 'allow_sort_all_when_none_selected', op_sort_all)
        ini_write(CONFIG_FN, CONFIG_SECTION, 'ini_files_case_sensitive', op_ini_case_sens)

        file_open(CONFIG_FN)

        lines = [ed.get_text_line(i) for i in range(ed.get_line_count())]
        try:
            index = lines.index('['+CONFIG_SECTION+']')
            ed.set_caret(0, index)
        except:
            pass
