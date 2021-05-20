import unittest

import os, json
from cudatext               import *
#from cuda_options_editor    import *
import cuda_options_editor as op_ed

subset ='tests.' # Key for isolated storage on plugin settings
class TestOptEdD(unittest.TestCase):

    ##############################
    def test_no_meta_file(self):
        meta    = [
        {   "opt": "my_color_with_empty",
            "cmt": ["Comment"],
            "def": '',
            "frm": "#rgb-e",
        },
        {   "opt": "my_color_not_empty",
            "cmt": ["Comment"],
            "def": '#aaccff',
            "frm": "#rgb",
        },
        ]

        title = 'Test no meta-file' # Dialog caption
        op_ed.OptEdD(path_keys_info=meta, subset=subset
                    ,how=dict(stor_json='oped_test.json',
                              hide_lex_fil=True,
                              )
                    ).show(title)
        
        self.assertTrue(True)


    ##############################
    def test_rgb(self):
        info_file = os.path.dirname(__file__)+os.sep+'test_rgb.json'

        open(info_file, 'w').write(json.dumps([
        {   "opt": "my_color_with_empty",
            "cmt": ["Comment"],
            "def": '',
            "frm": "#rgb-e",
        },
        {   "opt": "my_color_not_empty",
            "cmt": ["Comment"],
            "def": '#aaccff',
            "frm": "#rgb",
        },
        ]))

        title = 'Test to store into "oped_test.json"' # Dialog caption
        op_ed.OptEdD(path_keys_info=info_file, subset=subset
                    ,how=dict(stor_json='oped_test.json',
                              hide_fil=True,
                              )
                    ).show(title)
        
        self.assertTrue(True)


    ##############################
#   def test_not_user(self):
#       info_file = os.path.dirname(__file__)+os.sep+'test_hide_lex_fil.json'
#
#       open(info_file, 'w').write(json.dumps([
#       {   "opt": "my_opt_3",
#           "cmt": ["Comment"],
#           "def": 11,
#           "frm": "int2s",
#           "dct": [[11, "value for 11"], [22, "value for 22"]]
#       },
#       {   "opt": "my_opt_4",
#           "cmt": ["Comment"],
#           "def": "a",
#           "frm": "int2s",
#           "dct": [["a", "value for a"], ["b", "value for b"]]
#       }
#       ]))
#
#       title = 'Test to store into "oped_test.json"' # Dialog caption
#       op_ed.OptEdD(path_keys_info=info_file, subset=subset
#                   ,how=dict(hide_lex_fil=True, stor_json='oped_test.json')
#                   ).show(title)
#       
#       self.assertTrue(True)


    ##############################
#   def test_hide_lex_fil(self):
#       info_file = os.path.dirname(__file__)+os.sep+'test_hide_lex_fil.json'
#
#       open(info_file, 'w').write(json.dumps([
#       {   "opt": "my_opt_3",
#           "cmt": ["Comment"],
#           "def": 11,
#           "frm": "int2s",
#           "dct": [[11, "value for 11"], [22, "value for 22"]]
#       },
#       {   "opt": "my_opt_4",
#           "cmt": ["Comment"],
#           "def": "a",
#           "frm": "int2s",
#           "dct": [["a", "value for a"], ["b", "value for b"]]
#       }
#       ]))
#
#       title = 'Test hide lex/fil' # Dialog caption
#       op_ed.OptEdD(path_keys_info=info_file, subset=subset
#                   ,how=dict(hide_lex_fil=True)
#                   ).show(title)
#       
#       self.assertTrue(True)


    ##############################
#   def test_wiki_ex(self):
#       info_file = os.path.dirname(__file__)+os.sep+'test_wiki_ex.json'
#
#       open(info_file, 'w').write(json.dumps([
#       {   "opt": "my_opt_1",
#           "cmt": ["Comment line 1",
#                   "Comment line 2",
#                   "Comment line 3"],
#           "def": True,        # Default value
#           "frm": "bool",      # Value type from
#                               #   bool float int str - simple types
#                               #   font - font name
#                               #   font-e - font name or empty
#                               #   hotk - hotkey
#                               #   file - file path
#                               #   strs  - list of str
#                               #   int2s - dict int to str
#                               #   str2s - dict str to str
#           "chp": "MySection"  # Section (can be empty)
#       },
#       {   "opt": "my_opt_2",
#           "cmt": ["Comment"],
#           "def": "v1",
#           "frm": "strs",
#           "lst": ["v1", "v2"]
#       },
#       {   "opt": "my_opt_3",
#           "cmt": ["Comment"],
#           "def": 11,
#           "frm": "int2s",
#           "dct": [[11, "value for 11"], [22, "value for 22"]]
#       },
#       {   "opt": "my_opt_4",
#           "cmt": ["Comment"],
#           "def": "a",
#           "frm": "int2s",
#           "dct": [["a", "value for a"], ["b", "value for b"]]
#       }
#       ]))
#
#       title = 'Test wiki example' # Dialog caption
#       op_ed.OptEdD(path_keys_info=info_file, subset=subset).show(title)
#       
#       self.assertTrue(True)

if __name__ == '__main__' :     # Tests
    # To start the tests run in Console
    #   exec(open(path_to_the_file, encoding="UTF-8").read())
    app.app_log(app.LOG_CONSOLE_CLEAR, 'm')
    for smk in [smk for smk 
        in  sys.modules                             if 'cuda_options_editor.tests.test_options_editor' in smk]:
        del sys.modules[smk]        # Avoid old module 
    import                                              cuda_options_editor.tests.test_options_editor
    import unittest
    suite = unittest.TestLoader().loadTestsFromModule(  cuda_options_editor.tests.test_options_editor)
    unittest.TextTestRunner(verbosity=0).run(suite)
        
