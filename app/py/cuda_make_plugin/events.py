EVENTS = [
    'on_caret',
    'on_change',
    'on_change_slow',
    'on_click',
    'on_complete',
    'on_console',
    'on_console_nav',
    'on_focus',
    'on_func_hint',
    'on_goto_def',
    'on_key',
    'on_lexer',
    'on_macro',
    'on_open',
    'on_output_nav',
    'on_save',
    'on_save_pre',
    'on_start',
  ] 

EVENTS_ADD_PARAMS = {
  'on_key': 'key, state',
  'on_click': 'state',
  'on_console': 'text',
  'on_console_nav': 'text',
  'on_output_nav': 'text, tag',
  'on_macro': 'text'
  }
