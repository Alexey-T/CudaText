
`on_state`       | `on_state(self, ed_self, id)`       | Called before changing some editor state. `id` is one of values listed at [py property id]. Method can return `False` to disable state changing, other return value is ignored.
`on_func_hint`   | `on_func_hint(self, ed_self)`       | Called by function-hint command (default hotkey: Ctrl+Shift+Space). Method must return function-hint string (comma-separated parameters), or empty string.  
`on_lexer`       | `on_lexer(self, ed_self)`           | Called after lexer is changed.
`on_num`         | `on_num(self, ed_self, number)`     | Called to get string values for line-numbers gutter column. `number` is int line number, 0-based. Method must return str for this number.

Example for on_num
------------------

Handler shows "*" char for each 10th line number.

    def on_num(self, ed_self, number):
        s = str(number+1)
        if s.endswith('0'):
            s = '*' + s
        return s

