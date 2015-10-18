from cudatext import *
import sys
import os

from . import cssbeautifier as cssbeautifier
from . import format_proc as format_proc

format_proc.INI = 'cuda_css_format.cfg'
format_proc.MSG = '[CSS Format] '

def options():
    op = cssbeautifier.default_options()
    ini = format_proc.ini_filename()
    if os.path.isfile(ini):
        d = eval(open(ini).read())
        op.indent_size                = d["indent_size"]
        op.indent_char                = d["indent_char"]
        op.selector_separator_newline = d["selector_separator_newline"]
        op.end_with_newline           = d["end_with_newline"]
    return op

def do_format(text):
    return cssbeautifier.beautify(text, options())

class Command:
    def config_global(self):
        format_proc.config_global()

    def config_local(self):
        format_proc.config_local()

    def run(self):
        format_proc.run(do_format)
