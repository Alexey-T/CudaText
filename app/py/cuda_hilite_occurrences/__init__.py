import os
import string
import cudatext as app
import cudax_lib as apx

#----------------------Settings---------------------#
MIN_LEN  = 1 # For word or selected text.
MAX_SIZE = 0 # In bytes. Not used yet.

CHARS = string.ascii_letters + string.digits + '_'

SEL_ALLOW             = True  # Hilite all occurrences of selected text.
SEL_ALLOW_WHITE_SPACE = False # Hilite spaces there located in begin or end of selection
SEL_CASE_SENSITIVE    = False
SEL_WORDS_ONLY        = False # Hilite character only if it containts in CHARS.
SEL_WHOLE_WORDS       = False # Whole word only. Used only if bool(SEL_WORDS_ONLY) == True.

CARET_ALLOW          = True # Hilite all occurrences of word under caret.
CARET_CASE_SENSITIVE = True
CARET_WHOLE_WORDS    = True # Whole word only.

COLOR_FONT_OTHER   = 0x000000
COLOR_BG_OTHER     = 0x80FFFF
COLOR_FONT_CURRENT = COLOR_FONT_OTHER
COLOR_BG_CURRENT   = 0xe3c1e3
#-----------------------------------------------#

MARKTAG = 101 #uniq value for all markers plugins
fn_ini = os.path.join(app.app_path(app.APP_DIR_SETTINGS), 'cuda_hilite_occurrences.ini')


def do_load_ops():
  global MIN_LEN              
  global SEL_ALLOW            
  global SEL_ALLOW_WHITE_SPACE
  global SEL_CASE_SENSITIVE   
  global SEL_WORDS_ONLY       
  global SEL_WHOLE_WORDS      
  global CARET_ALLOW          
  global CARET_CASE_SENSITIVE 
  global CARET_WHOLE_WORDS    
  global COLOR_FONT_OTHER     
  global COLOR_BG_OTHER       
  global COLOR_FONT_CURRENT   
  global COLOR_BG_CURRENT     

  MIN_LEN               = int(app.ini_read(fn_ini, 'op', 'min_len', '1'))
  
  SEL_ALLOW             = app.ini_read(fn_ini, 'op', 'sel_allow', '1')=='1'
  SEL_ALLOW_WHITE_SPACE = app.ini_read(fn_ini, 'op', 'sel_allow_white_space', '0')=='1'
  SEL_CASE_SENSITIVE    = app.ini_read(fn_ini, 'op', 'sel_case_sensitive', '0')=='1'
  SEL_WORDS_ONLY        = app.ini_read(fn_ini, 'op', 'sel_words_only', '0')=='1'
  SEL_WHOLE_WORDS       = app.ini_read(fn_ini, 'op', 'sel_whole_words', '0')=='1'
  
  CARET_ALLOW           = app.ini_read(fn_ini, 'op', 'caret_allow', '1')=='1'
  CARET_CASE_SENSITIVE  = app.ini_read(fn_ini, 'op', 'caret_case_sensitive', '1')=='1'
  CARET_WHOLE_WORDS     = app.ini_read(fn_ini, 'op', 'caret_whole_words', '1')=='1'
  
  COLOR_FONT_OTHER      = apx.html_color_to_int(app.ini_read(fn_ini, 'colors', 'font_other', int_to_html(0x000000)))
  COLOR_BG_OTHER        = apx.html_color_to_int(app.ini_read(fn_ini, 'colors', 'bg_other', int_to_html(0x80ffff)))
  COLOR_FONT_CURRENT    = apx.html_color_to_int(app.ini_read(fn_ini, 'colors', 'font_current', int_to_html(0x000000)))
  COLOR_BG_CURRENT      = apx.html_color_to_int(app.ini_read(fn_ini, 'colors', 'bg_current', int_to_html(0xe3c1e3)))


def int_to_html(n):
    s = '%06x' % n
    r, g, b = s[4:], s[2:4], s[:2]
    return '#'+r+g+b


def bool_str(b):
  return '1' if b else '0'
  

def do_save_ops():
  app.ini_write(fn_ini, 'op', 'min_len', str(MIN_LEN))
  
  app.ini_write(fn_ini, 'op', 'sel_allow', bool_str(SEL_ALLOW))
  app.ini_write(fn_ini, 'op', 'sel_allow_white_space', bool_str(SEL_ALLOW_WHITE_SPACE))
  app.ini_write(fn_ini, 'op', 'sel_case_sensitive', bool_str(SEL_CASE_SENSITIVE))
  app.ini_write(fn_ini, 'op', 'sel_words_only', bool_str(SEL_WORDS_ONLY))
  app.ini_write(fn_ini, 'op', 'sel_whole_words', bool_str(SEL_WHOLE_WORDS))
  
  app.ini_write(fn_ini, 'op', 'caret_allow', bool_str(CARET_ALLOW))
  app.ini_write(fn_ini, 'op', 'caret_case_sensitive', bool_str(CARET_CASE_SENSITIVE))
  app.ini_write(fn_ini, 'op', 'caret_whole_words', bool_str(CARET_WHOLE_WORDS))
  
  app.ini_write(fn_ini, 'colors', 'font_other', int_to_html(COLOR_FONT_OTHER))
  app.ini_write(fn_ini, 'colors', 'bg_other', int_to_html(COLOR_BG_OTHER))
  app.ini_write(fn_ini, 'colors', 'font_current', int_to_html(COLOR_FONT_CURRENT))
  app.ini_write(fn_ini, 'colors', 'bg_current', int_to_html(COLOR_BG_CURRENT))


class Command:
  def __init__(self):
    do_load_ops()
    
  def config(self):
    do_save_ops()
    if os.path.isfile(fn_ini):
      app.file_open(fn_ini)
    else:
      app.msg_status('Config file not exists')

  def on_caret(self, ed_self):
    ed_self.attr(app.MARKERS_DELETE_BY_TAG, MARKTAG)

    # TODO: ...
    if MAX_SIZE: pass
    #  if MAX_SIZE < ??? : return

    current_text = _get_current_text(ed_self) # if not (SEL_ALLOW or CARET_ALLOW): bool(current_text) == False

    if not current_text: return

    text, caret_pos, is_selection = current_text

    if caret_pos[1] != caret_pos[3]: return # no multiline
    if not SEL_ALLOW_WHITE_SPACE: text = text.strip()
    if not text: return

    if is_selection:
      case_sensitive = SEL_CASE_SENSITIVE
      words_only     = SEL_WORDS_ONLY
      whole_words    = SEL_WHOLE_WORDS if SEL_WORDS_ONLY else False
    else:
      case_sensitive = CARET_CASE_SENSITIVE
      words_only     = True
      whole_words    = CARET_WHOLE_WORDS

    if len(text) < MIN_LEN: return

    carets = ed_self.get_carets()
    if len(carets) != 1: return

    x0, y0, x1, y1 = caret_pos
    if x0 > x1: x0, x1 = x1, x0

    items = find_all_occurrences(ed_self, text, case_sensitive, whole_words, words_only)

    if not items or (len(items) == 1 and items[0] == (x0, y1)): return

    for item in items:
      if item == (x0, y0): continue

      ed_self.attr(app.MARKERS_ADD, MARKTAG, item[0], item[1], len(text), COLOR_FONT_OTHER, COLOR_BG_OTHER)
    else:
      if CARET_ALLOW and not is_selection:
        ed_self.attr(app.MARKERS_ADD, MARKTAG, x0, y0, len(text), COLOR_FONT_CURRENT, COLOR_BG_CURRENT)

    app.msg_status('Matches hilited: {}'.format(len(items)))

def is_word(s):
  for ch in s:
    if not ch in CHARS: return False
  return True

def find_all_occurrences(ed, text, case_sensitive, whole_words, words_only):
  if words_only and not is_word(text): return

  if not case_sensitive: text = text.lower()

  res = []
  for y in range(ed.get_line_count()):
    line = ed.get_text_line(y)
    if not line: continue

    if not case_sensitive: line = line.lower()

    x = 0
    text_len = len(text)
    while True:
      x = line.find(text, x)
      if x < 0: break

      if whole_words:
        if x > 0 and is_word(line[x - 1]):
          x += text_len + 1
          continue

        next_char = x + text_len
        if next_char < len(line) and is_word(line[next_char]):
          x += 2
          continue

      res.append((x, y))

      x += text_len

  return res

def get_word_under_caret(ed):
  '''Возвращает кортеж (слово_под_кареткой, (x1, y1, x2, y2)) (не учитывая, есть выделение или нет).'''

  x1, y1, x2 = ed.get_carets()[0][:3]
  y2 = y1

  l_char = r_char = ''
  current_line = ed.get_text_line(y1)

  if current_line:
    x = x1
    if x > 0:                 l_char = current_line[x - 1]
    if x < len(current_line): r_char = current_line[x]

    l_char, r_char = is_word(l_char), is_word(r_char)

    if not (l_char or r_char): return

    if l_char:
      for x1 in range(x - 1, -1, -1):
        if is_word(current_line[x1]): continue
        else: break
      else: x1 = -1
      x1 += 1

    if r_char:
      for x2 in range(x + 1, len(current_line)):
        if is_word(current_line[x2]): continue
        else: break
      else: x2 = len(current_line)
    else: x2 = x

    word_under_caret = current_line[x1 : x2]
  else: return

  return word_under_caret, (x1, y1, x2, y2)

def _get_current_text(ed):
  caret_pos = ed.get_carets()[0]
  x1, y1, x2, y2 = caret_pos
  is_selection = not ((x1, y1) == (x2, y2) or x2 == -1)
  current_text = ''

  if is_selection:
    if SEL_ALLOW: current_text = ed.get_text_sel()
    else: return
  else:
    # Иногда бывает, что каретка может находится за текстом (если так настроен редактор, или ее перемещают через API).
    if len(ed.get_text_line(y1)) < x1: return

    if CARET_ALLOW:
      temp = get_word_under_caret(ed)
      if not temp: return
      current_text, caret_pos = temp

  return current_text, caret_pos, is_selection
