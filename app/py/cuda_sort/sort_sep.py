from cudatext import *
from cudax_lib import get_translation

_   = get_translation(__file__)  # I18N

def _sort(s, sep_k, sep_v):

    if sep_k:
        if not sep_k in s:
            return s
        key, val = s.split(sep_k, 1)
        vals = sorted(val.split(sep_v))
        return key+sep_k+sep_v.join(vals)
    else:
        vals = sorted(s.split(sep_v))
        return sep_v.join(vals)


def do_sort_sep_values():

    while 1:
        res = dlg_input_ex(2,
            _('Sort: separator chars'),
            _('Separator of prefix, to skip prefix (optional):'), '=',
            _('Separator of values after prefix:'), ',')
        if res is None:
            return
        sep_k, sep_v = res

        if len(sep_k)>1:
            msg_status(_('Separators must have length=1'))
            continue

        if len(sep_v)!=1:
            msg_status(_('Separators must have length=1'))
            continue

        if sep_k==sep_v:
            msg_status(_('Separators cannot be the same'))
            continue

        break

    n1, n2 = ed.get_sel_lines()
    if n1<0:
        n1 = 0
        n2 = ed.get_line_count()-1

    cnt = 0
    for i in range(n1, n2+1):
        s = ed.get_text_line(i)
        s2 = _sort(s, sep_k, sep_v)
        if s!=s2:
            ed.set_text_line(i, s2)
            cnt += 1

    if cnt>0:
        msg_status(_('Changed %d line(s)') % cnt)
    else:
        msg_status(_('No lines were changed'))
