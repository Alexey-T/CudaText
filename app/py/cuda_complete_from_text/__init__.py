import re
from cudatext import *

option_lexers = '-,Text files,Markdown'
option_min_len = 3
option_case_sens = False
prefix = 'text'


def is_text_with_begin(s, begin):
    if option_case_sens:
        return s.startswith(begin)
    else:
        return s.upper().startswith(begin.upper())
        

def get_words_list():
    text = ed.get_text_all()
    regex = r'\w{%d,}'%option_min_len
    l = re.findall(regex, text)
    if not l: return
    l = sorted(list(set(l)))
    return l


def get_word(x, y):
    if x==0: return
    
    x0 = x
    while (x0>0) and (ed.get_text_substr(x0-1, y, x0, y).isalnum()):
        x0-=1
    text1 = ed.get_text_substr(x0, y, x, y)

    x0 = x
    while (ed.get_text_substr(x0, y, x0+1, y).isalnum()):
        x0+=1
    text2 = ed.get_text_substr(x, y, x0, y)

    return (text1, text2)
    

class Command:
    def on_complete(self, ed_self):
        carets = ed.get_carets()
        if len(carets)!=1: return
        x0, y0, x1, y1 = carets[0]
    
        lex = ed.get_prop(PROP_LEXER_CARET, '')
        if lex is None: return
        if lex=='': lex='-'
        allow = ','+lex+',' in ','+option_lexers+','
        if not allow: return
        
        words = get_words_list()
        word = get_word(x0, y0)
        if not words: return
        if not word: return
        word1, word2 = word
        
        words = [prefix+'|'+w for w in words if is_text_with_begin(w, word1) and w!=word1]
        #print('word:', word)
        #print('list:', words)
        
        ed.complete('\n'.join(words), len(word1), len(word2))
        return True
