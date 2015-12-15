import re
from cudatext import *

def count_words(s):
    return len(re.findall(r'\w+', s))
    
def count_letters(s):    
    return len(re.findall(r'\w', s))


class Command:
    def run(self):
        s = ed.get_text_all()
        
        n_words = count_words(s)
        n_letters = count_letters(s)
        n_lines = ed.get_line_count()
        n_chars = sum([len(ed.get_text_line(i)) for i in range(ed.get_line_count())])
        
        text = 'Statistics:\n\nLines: %d\nWords: %d\nLetters: %d\nAll chars: %d' % \
               (n_lines, n_words, n_letters, n_chars)
        msg_box(text, MB_OK+MB_ICONINFO)
