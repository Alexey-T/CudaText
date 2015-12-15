''' Plugin for CudaText editor
Authors:
    Andrey Kvichansky    (kvichans on githab.com)
Version:
    '0.9.1 2015-12-15'
'''

from .cd_ext import Command as CommandRLS

RLS  = CommandRLS()
class Command:
    def on_console_nav(self, ed_self, text):            return RLS.on_console_nav(ed_self, text)
    def add_indented_line_above(self):                  return RLS.add_indented_line_above()
    def add_indented_line_below(self):                  return RLS.add_indented_line_below()
    def paste_to_1st_col(self):                         return RLS.paste_to_1st_col()
    def find_cb_string(self, updn, bgn_crt_fin='crt'):  return RLS.find_cb_string()
    def find_cb_string_next(self):                      return RLS.find_cb_string_next()
    def find_cb_string_prev(self):                      return RLS.find_cb_string_prev()
    def open_selected(self):                            return RLS.open_selected()
    def replace_all_sel_to_cb(self):                    return RLS.replace_all_sel_to_cb()
    def _activate_tab(self, group, tab_ind):            return RLS._activate_tab(group, tab_ind)
    def to_tab_g1_t1(self):return self._activate_tab(0, 0)
    def to_tab_g1_t2(self):return self._activate_tab(0, 1)
    def to_tab_g1_t3(self):return self._activate_tab(0, 2)
    def to_tab_g1_t4(self):return self._activate_tab(0, 3)
    def to_tab_g1_t5(self):return self._activate_tab(0, 4)
    def to_tab_g1_t6(self):return self._activate_tab(0, 5)
    def to_tab_g1_t7(self):return self._activate_tab(0, 6)
    def to_tab_g1_t8(self):return self._activate_tab(0, 7)
    def to_tab_g1_t9(self):return self._activate_tab(0, 8)
    def to_tab_g2_t1(self):return self._activate_tab(1, 0)
    def to_tab_g2_t2(self):return self._activate_tab(1, 1)
    def to_tab_g2_t3(self):return self._activate_tab(1, 2)
    def to_tab_g2_t4(self):return self._activate_tab(1, 3)
    def to_tab_g2_t5(self):return self._activate_tab(1, 4)
    def to_tab_g2_t6(self):return self._activate_tab(1, 5)
    def to_tab_g2_t7(self):return self._activate_tab(1, 6)
    def to_tab_g2_t8(self):return self._activate_tab(1, 7)
    def to_tab_g2_t9(self):return self._activate_tab(1, 8)
    def _activate_last_tab(self, group):                return RLS._activate_last_tab(group)
    def to_tab_g1_last(self):return self._activate_last_tab(0)
    def to_tab_g2_last(self):return self._activate_last_tab(1)
    def _activate_near_tab(self, gap):                  return RLS._activate_near_tab(gap)
    def to_next_tab(self):return self._activate_near_tab(1)
    def to_prev_tab(self):return self._activate_near_tab(-1)
    def jump_to_matching_bracket(self):                 return RLS.jump_to_matching_bracket()
   #class Command
