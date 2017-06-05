from cudatext import *

class Command:
    def run(self):
        for i in range(ed.get_line_count()//2):
            self.do_gap(i*2)

    def do_gap(self, num):
        id_bitmap, id_canvas = ed.gap(GAP_MAKE_BITMAP, 600, 50)
        canvas_proc(id_canvas, CANVAS_SET_BRUSH, color=0xa0ffa0)
        canvas_proc(id_canvas, CANVAS_SET_ANTIALIAS, style=ANTIALIAS_ON)
        canvas_proc(id_canvas, CANVAS_POLYGON, '200,0,300,30,200,49')
        canvas_proc(id_canvas, CANVAS_SET_BRUSH, color=0xffffff, style=BRUSH_CLEAR)
        canvas_proc(id_canvas, CANVAS_TEXT, x=230, y=10, text='gap %d'%(num+1))
        canvas_proc(id_canvas, CANVAS_SET_BRUSH, color=0xffffff, style=BRUSH_SOLID)
        ed.gap(GAP_ADD, num, id_bitmap, tag=10)
