from cudatext import *
from cudatext_cmd import *
import math
from time import sleep, perf_counter

def plot(data,sx=450,sy=130,offsetx=0):
#def plot(data,sx=100,sy=100,offsetx=0):
    n_list = [d[0] for d in data]
    min_n = min(n_list)
    max_n = max(n_list)
    diff_n = max_n - min_n
    
    time_list = [d[1] for d in data]
    min_t = min(time_list)
    max_t = max(time_list)
    diff_t = max_t - min_t

    canvas_proc(0, CANVAS_SET_BRUSH, color=0xdd552c)
    canvas_proc(0, CANVAS_RECT_FILL, x=offsetx,y=0,x2=offsetx+sx,y2=sy)
    
    canvas_proc(0, CANVAS_SET_PEN, color=0x555555, style=PEN_STYLE_DASH, size=3)
    canvas_proc(0, CANVAS_LINE, x=offsetx, y=sy, x2=offsetx+sx,y2=0)
    canvas_proc(0, CANVAS_SET_PEN, color=0xaaaaaa)
    canvas_proc(0, CANVAS_LINE, x=offsetx+1, y=sy+1, x2=offsetx+sx+1,y2=1)
    
    canvas_proc(0, CANVAS_SET_PEN, color=0xffffff, size=1)
    canvas_proc(0, CANVAS_LINE, x=offsetx, y=sy, x2=offsetx+sx,y2=sy)
    canvas_proc(0, CANVAS_LINE, x=offsetx+sx, y=sy, x2=offsetx+sx,y2=0)
    canvas_proc(0, CANVAS_LINE, x=offsetx+sx, y=0, x2=offsetx,y2=0)
    canvas_proc(0, CANVAS_LINE, x=offsetx, y=0, x2=offsetx,y2=sy)
    
    canvas_proc(0, CANVAS_SET_PEN, color=0x00ee88, size=5)
    
    px,py = None,None
    for i,(n,t) in enumerate(data):
        t = (t - min_t) * sy # scale
        n = (n - min_n) * sx # scale
        t = t // diff_t if diff_t != 0 else t
        n = n // diff_n if diff_n != 0 else n
        t, n = int(t), int(n) # to int
        
        if i == 0:
            px, py = n, sy-t  # remember first point coords
            continue # do not draw line yet, wait for second point
        else:
            x2, y2 = n, sy-t
            canvas_proc(0, CANVAS_LINE, x=px+offsetx, y=py, x2=x2+offsetx,y2=y2)
            px, py = x2, y2
            
            canvas_proc(0, CANVAS_SET_FONT, color=0xdddddd)
            h = canvas_proc(0, CANVAS_GET_TEXT_SIZE, 'text')[1]
            #canvas_proc(0, CANVAS_SET_BRUSH, style=BRUSH_CLEAR)
            t = "time: {}".format(round(data[-1][1]/1000, 6))
            n = "n: {}".format(data[-1][0])
            nn = "nn: {}".format(len(data))
            canvas_proc(0, CANVAS_TEXT, t, x=offsetx+5, y=2)
            canvas_proc(0, CANVAS_TEXT, n, x=offsetx+5, y=h)
            canvas_proc(0, CANVAS_TEXT, nn, x=offsetx+5, y=h*2)
            #app_idle()
            #sleep(0.1)
    #canvas_proc(0, CANVAS_TEXT, 'done', x=offsetx+5, y=h*2)

def generate_const(n):
    return [[i,90] for i in range(n)]
def generate_linear(n):
    return [[i,i] for i in range(n)]
def generate_logn(n):
    return [[i,math.log(i)] for i in range(1, n+1)]
def generate_nlogn(n):
    return [[i,i*math.log(i)] for i in range(1, n+1)]
def generate_quad(n):
    return [[i,math.pow(i, 2)] for i in range(n)]
def generate_exponential(n):
    return [[i,math.pow(2, i)] for i in range(n)]
def generate_factorial(n):
    return [[i,math.factorial(i)] for i in range(n)]

class Command:
    def run_const(self):
        plot(generate_const(50), offsetx=460)
    def run_linear(self):
        plot(generate_linear(3), offsetx=460)
    def run_logn(self):
        plot(generate_logn(50), offsetx=460)
    def run_nlogn(self):
        plot(generate_nlogn(50), offsetx=460)
    def run_quad(self):
        plot(generate_quad(50), offsetx=460)
    def run_exponential(self):
        plot(generate_exponential(50), offsetx=460)
    def run_factorial(self):
        plot(generate_factorial(50), offsetx=460)
    
    def run_set_text_line(self):
        def test(n):
            file_open('')
            result = []
            for i in range(n):
                _time = perf_counter()*1000
                ed.set_text_line(-1, '00 '*10)
                ed.set_text_line(-2, '00 '*10)
                #ed.insert(0, ed.get_line_count(),'\n'+'00 '*10)
                diff = perf_counter()*1000 - _time
                result.append([i+1, diff])
            ed.set_prop(PROP_MODIFIED, False)
            ed.cmd(cmd_FileClose)
            return result
            
        plot(test(1000), offsetx=300)
    
    def run_replace_lines(self):
        def test(n):
            file_open('')
            result = []
            ed.set_text_all(('00 '*10 + '\n') * n)
            app_idle()
            for i in range(n):
                _time = perf_counter()*1000
                ed.replace_lines(0, 0, [])
                diff = perf_counter()*1000 - _time
                diff = min(diff, 0.04)
                result.append([ed.get_line_count(), diff])
            ed.set_prop(PROP_MODIFIED, False)
            ed.cmd(cmd_FileClose)
            
            #with open('/home/yura/plot_test/data.txt', 'w') as file:
            #    for d in result:
            #        file.write('{} {};'.format(d[0], d[1]))
            
            return result
            
        plot(test(10000), offsetx=300)

