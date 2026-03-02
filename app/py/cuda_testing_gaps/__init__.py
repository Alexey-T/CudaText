from cudatext import *

class Command:
    def add_many(self):
        self.del_all()
        s = dlg_input('How many gaps per line:', '2')
        if s is None:
            return
        try:
            nrepeat = int(s)
        except:
            return
        if nrepeat<1:
            return
        ntag = 100
        for j in range(nrepeat):
            ntag += 1
            self.do_gap(-1, ntag)
            for i in range(ed.get_line_count()//2):
                ntag += 1
                self.do_gap(i*2, ntag)

    def do_gap(self, nline, ntag):
        id_bitmap, id_canvas = ed.gap(GAP_MAKE_BITMAP, 600, 50)
        canvas_proc(id_canvas, CANVAS_SET_BRUSH, color=0xa0ffa0)
        canvas_proc(id_canvas, CANVAS_SET_ANTIALIAS, style=ANTIALIAS_ON)
        canvas_proc(id_canvas, CANVAS_POLYGON, '200,0,300,30,200,49')
        canvas_proc(id_canvas, CANVAS_SET_BRUSH, color=0xffffff, style=BRUSH_CLEAR)
        canvas_proc(id_canvas, CANVAS_TEXT, x=205, y=10, text='tag%d'%ntag)
        canvas_proc(id_canvas, CANVAS_SET_BRUSH, color=0xffffff, style=BRUSH_SOLID)
        ed.gap(GAP_ADD, nline, id_bitmap, tag=ntag)

    def on_click_gap(self, ed_self, state, nline, ntag, size_x, size_y, pos_x, pos_y):
        print('on_click_gap: line %d, tag %d'%(nline+1, ntag))

    def del_all(self):
        ed.gap(GAP_DELETE_ALL, 0, 0)
