from cudatext import *
import re

REGEX = '[a-zA-Z0-9][\w\.\-_]*@\w[\w\.\-]*\.[a-zA-Z]{2,}'

def get_list(ed):
    text = ed.get_text_all()
    return re.findall(REGEX, text)

class Command:
    def run(self):
        res = get_list(ed)
        if res:
            file_open('')
            ed.set_text_all( '\n'.join(res) )
