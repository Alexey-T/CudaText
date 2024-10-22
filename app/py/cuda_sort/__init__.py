import os
from random import randint
from cudatext import *
from cudax_lib import get_translation
from .app_specific import *
from .sort_ini import *
from .sort_emails import *
from .sort_sep import *
from .sort_numeric import *
from .sort_dlg import *

_   = get_translation(__file__)  # I18N

CONFIG_FN = os.path.join(app_path(APP_DIR_SETTINGS), 'plugins.ini')
CONFIG_SECTION = 'sort'

DEF_MAX_LINES = 100000

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

    op_sort_all = ini_read(CONFIG_FN, CONFIG_SECTION, 'allow_sort_all_when_none_selected', '1')=='1'

    max_cnt = int(ini_read(CONFIG_FN, CONFIG_SECTION, 'max_lines', str(DEF_MAX_LINES)))
    if ed.get_line_count()>max_cnt:
        msg_box(
            _('Document has too many lines. Plugin Sort will not work. Current value of option [sort] max_lines in "settings/plugins.ini" is %d.')%max_cnt+
            '\n\n'+
            _('Instead of Sort plugin, you can always use CudaText built-in commands:\n"(without undo) sort ...".'),
        MB_OK+MB_ICONWARNING)
        return

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
        res = do_dialog(CONFIG_FN, CONFIG_SECTION)
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
        if ini_sort(and_keys, case_sens):
            msg_status(_('Sorted ini lines'))

    def ini_sort_all(self):
        self.do_sort_ini(True)

    def ini_sort_not_keys(self):
        self.do_sort_ini(False)

    def sort_emails(self):
        do_sort_emails()

    def sort_sep_values(self):
        do_sort_sep_values()

    def config(self):

        op_sort_all = ini_read(CONFIG_FN, CONFIG_SECTION, 'allow_sort_all_when_none_selected', '1')
        op_ini_case_sens = ini_read(CONFIG_FN, CONFIG_SECTION, 'ini_files_case_sensitive', '0')
        op_max_lines = ini_read(CONFIG_FN, CONFIG_SECTION, 'max_lines', str(DEF_MAX_LINES))

        ini_write(CONFIG_FN, CONFIG_SECTION, 'allow_sort_all_when_none_selected', op_sort_all)
        ini_write(CONFIG_FN, CONFIG_SECTION, 'ini_files_case_sensitive', op_ini_case_sens)
        ini_write(CONFIG_FN, CONFIG_SECTION, 'max_lines', op_max_lines)

        file_open(CONFIG_FN)

        lines = [ed.get_text_line(i) for i in range(ed.get_line_count())]
        try:
            index = lines.index('['+CONFIG_SECTION+']')
            ed.set_caret(0, index)
        except:
            pass
