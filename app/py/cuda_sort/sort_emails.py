from cudatext import *
from cudax_lib import get_translation

_   = get_translation(__file__)  # I18N

def _ok_email(s):
    return '@' in s and not ' ' in s and not '\t' in s

def do_sort_emails():

    def _key(s):
        if not _ok_email(s):
            return ('', s)
        n = s.find('@')
        return (s[n:], s[:n])

    lines = [ed.get_text_line(i) for i in range(ed.get_line_count())]
    lines = [s for s in lines if s]

    ok = [s for s in lines if _ok_email(s)]
    if not ok:
        msg_status(_('No e-mails in text'))
        return

    lines = sorted(lines, key=_key)
    ed.set_text_all('\n'.join(lines)+'\n')
    msg_status(_('Sorted text as email list'))
