"""
CudaText plugin: canvas_proc() API demo
======================================

Opens a non-modal, scrollable dialog that demonstrates every action of
canvas_proc() with every documented combination of its parameters.

Drawing model (from cuda_testing_dlg_proc.do_paint_mark):
    h_image  = dlg_proc(id_dlg, DLG_CTL_HANDLE, name='img_main')
    h_bitmap = image_proc(h_image, IMAGE_GET_BITMAP)
    h_canvas = bitmap_proc(h_bitmap, BITMAP_GET_CANVAS)   # GET_CANVAS first!
    bitmap_proc(h_bitmap, BITMAP_SET_SIZE, w, h)           # SET_SIZE after!

For a standalone bitmap:
    h_bmp = bitmap_proc(0, BITMAP_CREATE, w, h)           # size at creation
    h_cnv = bitmap_proc(h_bmp, BITMAP_GET_CANVAS)         # no SET_SIZE needed

Scrolling: scrollbox handles mouse wheel + scrollbar automatically.

No try/except. No CANVAS_SET_TESTPANEL. No deprecated paintbox control.
"""

import math

from cudatext import *


def rgb(r, g, b):
    """TColor is $00BBGGRR (opposite of web #RRGGBB)."""
    return (b << 16) | (g << 8) | r


# ---------------------------------------------------------------------------
# Color palette
# ---------------------------------------------------------------------------
COLOR_BG_PAGE     = rgb(0xF4, 0xF4, 0xF4)
COLOR_BG_HEADER   = rgb(0x1F, 0x4E, 0x79)
COLOR_FG_HEADER   = rgb(0xFF, 0xFF, 0xFF)
COLOR_FG_DESC     = rgb(0x6E, 0x6E, 0x6E)
COLOR_FG_LABEL    = rgb(0x20, 0x20, 0x20)
COLOR_BG_DEMO     = rgb(0xFF, 0xFF, 0xFF)
COLOR_BORDER      = rgb(0xC0, 0xC0, 0xC0)
COLOR_BORDER_DK   = rgb(0x80, 0x80, 0x80)
COLOR_INFO_OK     = rgb(0x00, 0x80, 0x00)
COLOR_INFO_ERR    = rgb(0xC0, 0x00, 0x00)

DEMO_COLORS = [
    rgb(0xC0, 0x39, 0x2B), rgb(0xE6, 0x7E, 0x22), rgb(0xF1, 0xC4, 0x0F),
    rgb(0x27, 0xAE, 0x60), rgb(0x16, 0xA0, 0x85), rgb(0x29, 0x80, 0xB9),
    rgb(0x8E, 0x44, 0xAD), rgb(0x34, 0x49, 0x5E),
]

# ---------------------------------------------------------------------------
# Layout constants -- generous margins to prevent overlap
# ---------------------------------------------------------------------------
PAD_TOP         = 20
SECTION_GAP     = 20
SECTION_PAD_X   = 12
TITLE_BAR_H     = 28
DESC_H          = 22
DEMO_PAD_TOP    = 12
DEMO_PAD_BOT    = 12
LABEL_W         = 240

IMAGE_W         = 920

VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT, VK_HOME, VK_END = 38, 40, 33, 34, 36, 35


# ---------------------------------------------------------------------------
# Enumeration tables
# ---------------------------------------------------------------------------
FONT_STYLES = [
    (0,                                  '0  (normal)'),
    (FONT_B,                             'FONT_B'),
    (FONT_I,                             'FONT_I'),
    (FONT_U,                             'FONT_U'),
    (FONT_S,                             'FONT_S'),
    (FONT_B | FONT_I,                    'FONT_B | FONT_I'),
    (FONT_B | FONT_U,                    'FONT_B | FONT_U'),
    (FONT_B | FONT_S,                    'FONT_B | FONT_S'),
    (FONT_I | FONT_U,                    'FONT_I | FONT_U'),
    (FONT_I | FONT_S,                    'FONT_I | FONT_S'),
    (FONT_U | FONT_S,                    'FONT_U | FONT_S'),
    (FONT_B | FONT_I | FONT_U,           'FONT_B | FONT_I | FONT_U'),
    (FONT_B | FONT_I | FONT_S,           'FONT_B | FONT_I | FONT_S'),
    (FONT_B | FONT_U | FONT_S,           'FONT_B | FONT_U | FONT_S'),
    (FONT_I | FONT_U | FONT_S,           'FONT_I | FONT_U | FONT_S'),
    (FONT_B | FONT_I | FONT_U | FONT_S,  'ALL STYLES'),
]

PEN_STYLES = [
    (PEN_STYLE_SOLID,       'PEN_STYLE_SOLID'),
    (PEN_STYLE_DASH,        'PEN_STYLE_DASH'),
    (PEN_STYLE_DOT,         'PEN_STYLE_DOT'),
    (PEN_STYLE_DASHDOT,     'PEN_STYLE_DASHDOT'),
    (PEN_STYLE_DASHDOTDOT,  'PEN_STYLE_DASHDOTDOT'),
    (PEN_STYLE_INSIDEFRAME, 'PEN_STYLE_INSIDEFRAME'),
    (PEN_STYLE_PATTERN,     'PEN_STYLE_PATTERN'),
    (PEN_STYLE_CLEAR,       'PEN_STYLE_CLEAR'),
]

PEN_CAPS = [
    (PEN_CAPS_ROUND,  'PEN_CAPS_ROUND'),
    (PEN_CAPS_SQUARE, 'PEN_CAPS_SQUARE'),
    (PEN_CAPS_FLAT,   'PEN_CAPS_FLAT'),
]

PEN_JOINS = [
    (PEN_JOIN_ROUND, 'PEN_JOIN_ROUND'),
    (PEN_JOIN_BEVEL, 'PEN_JOIN_BEVEL'),
    (PEN_JOIN_MITER, 'PEN_JOIN_MITER'),
]

BRUSH_STYLES = [
    (BRUSH_SOLID,     'BRUSH_SOLID'),
    (BRUSH_CLEAR,     'BRUSH_CLEAR'),
    (BRUSH_HORZ,      'BRUSH_HORZ'),
    (BRUSH_VERT,      'BRUSH_VERT'),
    (BRUSH_FDIAGONAL, 'BRUSH_FDIAGONAL'),
    (BRUSH_BDIAGONAL, 'BRUSH_BDIAGONAL'),
    (BRUSH_CROSS,     'BRUSH_CROSS'),
    (BRUSH_DIAGCROSS, 'BRUSH_DIAGCROSS'),
]

ANTIALIAS_MODES = [
    (ANTIALIAS_NONE, 'ANTIALIAS_NONE'),
    (ANTIALIAS_ON,   'ANTIALIAS_ON'),
    (ANTIALIAS_OFF,  'ANTIALIAS_OFF'),
]


# ===========================================================================
# Command class
# ===========================================================================
class Command:

    h_dlg         = None
    n_scroll      = -1
    n_image       = -1
    h_demo_bitmap = 0
    content_h     = 0
    sections      = None

    # ------------------------------------------------------------------ #
    # Public commands
    # ------------------------------------------------------------------ #
    def show_panel(self):
        print('canvas_demo: show_panel called, h_dlg=', self.h_dlg)
        if self.h_dlg is not None:
            dlg_proc(self.h_dlg, DLG_FOCUS)
            return
        self._build_sections()
        self._compute_content_size()
        self._create_dialog()
        dlg_proc(self.h_dlg, DLG_SHOW_NONMODAL)
        self._repaint_canvas()
        print('canvas_demo: initial paint done')

    def hide_panel(self):
        if self.h_dlg is None:
            return
        self._free_dialog()

    def repaint_panel(self):
        self._repaint_canvas()

    def on_state(self, ed_self, state):
        if state == APPSTATE_WINDOW and self.h_dlg is not None:
            self._repaint_canvas()

    # ------------------------------------------------------------------ #
    # Section registry -- heights generously calculated
    # HDR = TITLE_BAR_H(28) + DESC_H(22) + DEMO_PAD_TOP(12) + DEMO_PAD_BOT(12) = 74
    # ------------------------------------------------------------------ #
    def _build_sections(self):
        HDR = TITLE_BAR_H + DESC_H + DEMO_PAD_TOP + DEMO_PAD_BOT  # 74

        self.sections = [
            # Section 0: Overview -- needs space for 3 info lines + 8 action rows
            {
                'title':  'canvas_proc(id_canvas, id_action, text, color, size, '
                          'x, y, x2, y2, style, p1, p2)',
                'desc':   'Single API to draw on canvas. Scroll: wheel / scrollbar on the right.',
                'height': HDR + 280,
                'draw':   self._paint_overview,
            },
            # Section 1: Fonts -- header(24) + 16 rows * 28 + pad(10)
            {
                'title':  'CANVAS_SET_FONT  -- all 16 FONT_* style combinations',
                'desc':   'text=font name, color=RGB, size=font size, style=FONT_B|I|U|S bitmask.',
                'height': HDR + 24 + 16 * 28 + 10,
                'draw':   self._paint_fonts,
            },
            # Section 2: Pen styles -- header(24) + 8 rows * 34 + pad(10)
            {
                'title':  'CANVAS_SET_PEN  -- all 8 PEN_STYLE_* values',
                'desc':   'color, size, style=PEN_STYLE_*, p1=PEN_CAPS_*, p2=PEN_JOIN_*.',
                'height': HDR + 24 + 8 * 34 + 10,
                'draw':   self._paint_pen_styles,
            },
            # Section 3: Pen caps -- triangle approach (old plugin style)
            {
                'title':  'CANVAS_SET_PEN  -- all 3 PEN_CAPS_* values',
                'desc':   'p1 controls end caps. Visible at the ends of thick lines.',
                'height': HDR + 24 + 160,
                'draw':   self._paint_pen_caps,
            },
            # Section 4: Pen joins -- triangle with sharp corner
            {
                'title':  'CANVAS_SET_PEN  -- all 3 PEN_JOIN_* values',
                'desc':   'p2 controls line joining. Visible at polyline corners (triangles).',
                'height': HDR + 24 + 170,
                'draw':   self._paint_pen_joins,
            },
            # Section 5: Brush styles -- header(24) + 2 rows * 80
            {
                'title':  'CANVAS_SET_BRUSH  -- all 8 BRUSH_* styles',
                'desc':   'color, style=BRUSH_*. SOLID and CLEAR are most common.',
                'height': HDR + 24 + 2 * 80 + 10,
                'draw':   self._paint_brush_styles,
            },
            # Section 6: Antialias -- ellipses + text samples
            {
                'title':  'CANVAS_SET_ANTIALIAS  -- all 3 modes (ellipse + text)',
                'desc':   'style=ANTIALIAS_NONE | ON | OFF. Affects ellipse and text edges.',
                'height': HDR + 24 + 260,
                'draw':   self._paint_antialias,
            },
            # Section 7: Text size -- each sample on its own row, no overlap
            {
                'title':  'CANVAS_GET_TEXT_SIZE  -- text measurement',
                'desc':   'Returns (size_x, size_y). Bounds drawn around the text.',
                'height': HDR + 24 + 5 * 50 + 10,
                'draw':   self._paint_text_size,
            },
            # Section 8: Text variants -- generous spacing between samples
            {
                'title':  'CANVAS_TEXT  -- various sizes and colors',
                'desc':   'text=str, x,y=position. Uses current font and brush.',
                'height': HDR + 24 + 200,
                'draw':   self._paint_text_variants,
            },
            # Section 9: Lines -- header(24) + 8 rows * 34 + pad
            {
                'title':  'CANVAS_LINE  -- one line per pen style',
                'desc':   'x,y,x2,y2=endpoints. Uses current pen.',
                'height': HDR + 24 + 8 * 34 + 10,
                'draw':   self._paint_lines,
            },
            # Section 10: Pixels -- bigger dots, clearly visible
            {
                'title':  'CANVAS_PIXEL  -- per-pixel painting',
                'desc':   'x,y,color = single pixel. Bigger dots + colorful circle.',
                'height': HDR + 24 + 200,
                'draw':   self._paint_pixels,
            },
            # Section 11: Rect (pen+brush)
            {
                'title':  'CANVAS_RECT  -- pen + brush together',
                'desc':   'x,y,x2,y2. Uses current pen AND brush.',
                'height': HDR + 24 + 140,
                'draw':   self._paint_rects,
            },
            # Section 12: Rect frame (brush only)
            {
                'title':  'CANVAS_RECT_FRAME  -- brush only',
                'desc':   'x,y,x2,y2. Uses current brush; pen is ignored.',
                'height': HDR + 24 + 140,
                'draw':   self._paint_rects_frame,
            },
            # Section 13: Rect fill (brush only)
            {
                'title':  'CANVAS_RECT_FILL  -- brush only',
                'desc':   'x,y,x2,y2. Uses current brush; pen is ignored.',
                'height': HDR + 24 + 140,
                'draw':   self._paint_rects_fill,
            },
            # Section 14: Rect round
            {
                'title':  'CANVAS_RECT_ROUND  -- rounded rectangles',
                'desc':   'x,y,x2,y2, style=corner radius. Uses pen AND brush.',
                'height': HDR + 24 + 140,
                'draw':   self._paint_rects_round,
            },
            # Section 15: Ellipse
            {
                'title':  'CANVAS_ELLIPSE  -- circles and ellipses',
                'desc':   'x,y,x2,y2. Uses pen AND brush.',
                'height': HDR + 24 + 150,
                'draw':   self._paint_ellipses,
            },
            # Section 16: Polygon
            {
                'title':  'CANVAS_POLYGON  -- triangles, squares, pentagons, stars',
                'desc':   'text=(x1,y1,x2,y2,...). Uses pen AND brush.',
                'height': HDR + 24 + 180,
                'draw':   self._paint_polygons,
            },
            # Section 17: Bitmap
            {
                'title':  'CANVAS_BITMAP  -- paint a bitmap_proc() bitmap',
                'desc':   'p1=id_bitmap, x,y. Created via bitmap_proc(BITMAP_CREATE, w, h).',
                'height': HDR + 24 + 190,
                'draw':   self._paint_bitmap,
            },
            # Section 18: Imagelist
            {
                'title':  'Bonus: IMAGELIST_PAINT (from cuda_testing_dlg_proc)',
                'desc':   'app_proc(PROC_GET_TAB_IMAGELIST) + imagelist_proc(IMAGELIST_PAINT).',
                'height': HDR + 24 + 120,
                'draw':   self._paint_imagelist,
            },
            # Section 19: Combinations
            {
                'title':  'Combinations  -- fonts + brushes + pens together',
                'desc':   'End-to-end compositions combining several canvas_proc() actions.',
                'height': HDR + 24 + 250,
                'draw':   self._paint_combos,
            },
        ]

    def _compute_content_size(self):
        h = PAD_TOP
        for s in self.sections:
            h += s['height'] + SECTION_GAP
        h += PAD_TOP
        self.content_h = h

    # ------------------------------------------------------------------ #
    # Dialog creation -- scrollbox + image
    # ------------------------------------------------------------------ #
    def _create_dialog(self):
        h = dlg_proc(0, DLG_CREATE)
        dlg_proc(h, DLG_PROP_SET, prop={
            'cap':         'canvas_proc() API demo',
            'w':           960,
            'h':           640,
            'w_min':       520,
            'h_min':       300,
            'border':      DBORDER_SIZE,
            'color':       COLOR_BG_PAGE,
            'keypreview':  True,
            'on_key_down': self._cb_on_key_down,
            'on_resize':    self._cb_on_resize,
            'on_close':     self._cb_on_close,
        })
        self.h_dlg = h

        ns = dlg_proc(h, DLG_CTL_ADD, 'scrollbox')
        dlg_proc(h, DLG_CTL_PROP_SET, index=ns, prop={
            'name':  'scroll_main',
            'align': ALIGN_CLIENT,
            'color': COLOR_BG_PAGE,
        })
        self.n_scroll = ns

        ni = dlg_proc(h, DLG_CTL_ADD, 'image')
        dlg_proc(h, DLG_CTL_PROP_SET, index=ni, prop={
            'name':  'img_main',
            'p':     'scroll_main',
            'x':     0,
            'y':     0,
            'w':     IMAGE_W,
            'h':     self.content_h,
            'color': COLOR_BG_PAGE,
        })
        self.n_image = ni

    def _free_dialog(self):
        if self.h_dlg is None:
            return
        self._free_demo_bitmap()
        h = self.h_dlg
        self.h_dlg = None
        self.n_scroll = -1
        self.n_image = -1
        dlg_proc(h, DLG_FREE)

    # ------------------------------------------------------------------ #
    # Canvas acquisition (bitmap-backed image pattern)
    # CRITICAL: GET_CANVAS before SET_SIZE (per reference)
    # ------------------------------------------------------------------ #
    def _get_canvas(self):
        if self.h_dlg is None:
            return 0, 0, 0
        h_image = dlg_proc(self.h_dlg, DLG_CTL_HANDLE, name='img_main')
        if h_image == 0 or h_image is None:
            print('canvas_demo: DLG_CTL_HANDLE returned 0')
            return 0, 0, 0
        h_bitmap = image_proc(h_image, IMAGE_GET_BITMAP)
        if h_bitmap == 0 or h_bitmap is None:
            print('canvas_demo: IMAGE_GET_BITMAP returned 0')
            return 0, 0, 0
        # GET_CANVAS first, then SET_SIZE (matches cuda_testing_dlg_proc reference)
        h_canvas = bitmap_proc(h_bitmap, BITMAP_GET_CANVAS, 0, 0)
        if h_canvas == 0 or h_canvas is None:
            print('canvas_demo: BITMAP_GET_CANVAS returned 0')
            return 0, 0, 0
        bitmap_proc(h_bitmap, BITMAP_SET_SIZE, IMAGE_W, self.content_h)
        return h_canvas, IMAGE_W, self.content_h

    def _repaint_canvas(self):
        if self.h_dlg is None:
            return
        h_canvas, w, h = self._get_canvas()
        if h_canvas == 0 or w == 0 or h == 0:
            return
        self._paint_all(h_canvas, w, h)

    # ------------------------------------------------------------------ #
    # Demo bitmap for CANVAS_BITMAP section
    # Pattern: BITMAP_CREATE with size, then GET_CANVAS (no SET_SIZE needed)
    # ------------------------------------------------------------------ #
    def _ensure_demo_bitmap(self):
        if self.h_demo_bitmap != 0:
            return self.h_demo_bitmap
        h_bmp = bitmap_proc(0, BITMAP_CREATE, 180, 120)
        cnv = bitmap_proc(h_bmp, BITMAP_GET_CANVAS, 0, 0)
        canvas_proc(cnv, CANVAS_SET_BRUSH, color=rgb(0x10,0x20,0x30), style=BRUSH_SOLID)
        canvas_proc(cnv, CANVAS_SET_PEN,   color=rgb(0x10,0x20,0x30), size=1, style=PEN_STYLE_SOLID)
        canvas_proc(cnv, CANVAS_RECT, x=0, y=0, x2=180, y2=120)
        canvas_proc(cnv, CANVAS_SET_PEN,   color=rgb(0xF1,0xC4,0x0F), size=3, style=PEN_STYLE_SOLID)
        canvas_proc(cnv, CANVAS_LINE, x=10, y=10, x2=170, y2=110)
        canvas_proc(cnv, CANVAS_SET_BRUSH, color=0, style=BRUSH_CLEAR)
        canvas_proc(cnv, CANVAS_SET_PEN,   color=rgb(0xE7,0x4C,0x3C), size=2, style=PEN_STYLE_SOLID)
        canvas_proc(cnv, CANVAS_ELLIPSE, x=30, y=20, x2=150, y2=100)
        canvas_proc(cnv, CANVAS_SET_BRUSH, color=0, style=BRUSH_CLEAR)
        canvas_proc(cnv, CANVAS_SET_FONT, text='Consolas', color=rgb(0xEC,0xF0,0xF1), size=12, style=FONT_B)
        canvas_proc(cnv, CANVAS_TEXT, text='bitmap_proc()', x=20, y=50)
        self.h_demo_bitmap = h_bmp
        return h_bmp

    def _free_demo_bitmap(self):
        if self.h_demo_bitmap != 0:
            bitmap_proc(self.h_demo_bitmap, BITMAP_FREE, 0, 0)
            self.h_demo_bitmap = 0

    # ------------------------------------------------------------------ #
    # Callbacks
    # ------------------------------------------------------------------ #
    def _cb_on_close(self, id_dlg, id_ctl, data='', info=''):
        print('canvas_demo: on_close fired')
        self.h_dlg = None
        self.n_scroll = -1
        self.n_image = -1
        self._free_demo_bitmap()

    def _cb_on_resize(self, id_dlg, id_ctl, data='', info=''):
        self._repaint_canvas()

    def _cb_on_key_down(self, id_dlg, id_ctl, data='', info=''):
        """Form-level key handler. id_ctl = VK code (int), data = state str.
        Scrollbox handles mouse wheel + scrollbar automatically."""
        return None

    # ------------------------------------------------------------------ #
    # Master draw routine
    # ------------------------------------------------------------------ #
    def _paint_all(self, c, w, h):
        canvas_proc(c, CANVAS_SET_BRUSH, color=COLOR_BG_PAGE, style=BRUSH_SOLID)
        canvas_proc(c, CANVAS_SET_PEN,   color=COLOR_BG_PAGE, size=1, style=PEN_STYLE_SOLID)
        canvas_proc(c, CANVAS_RECT_FILL, x=0, y=0, x2=w, y2=h)

        y = PAD_TOP
        for s in self.sections:
            self._paint_section(c, 0, y, w, s['height'], s)
            y += s['height'] + SECTION_GAP

    def _paint_section(self, c, x, y, w, h, section):
        # Outer frame
        canvas_proc(c, CANVAS_SET_BRUSH, color=COLOR_BG_PAGE, style=BRUSH_CLEAR)
        canvas_proc(c, CANVAS_SET_PEN,   color=COLOR_BORDER, size=1, style=PEN_STYLE_SOLID)
        canvas_proc(c, CANVAS_RECT_FRAME, x=x+1, y=y, x2=x+w-1, y2=y+h)

        # Title bar
        canvas_proc(c, CANVAS_SET_BRUSH, color=COLOR_BG_HEADER, style=BRUSH_SOLID)
        canvas_proc(c, CANVAS_SET_PEN,   color=COLOR_BG_HEADER, size=1, style=PEN_STYLE_SOLID)
        canvas_proc(c, CANVAS_RECT_FILL, x=x+1, y=y+1, x2=x+w-2, y2=y+TITLE_BAR_H)
        canvas_proc(c, CANVAS_SET_FONT, text='Consolas', color=COLOR_FG_HEADER, size=12, style=FONT_B)
        canvas_proc(c, CANVAS_SET_BRUSH, color=COLOR_BG_HEADER, style=BRUSH_CLEAR)
        canvas_proc(c, CANVAS_TEXT, text=section['title'], x=x+12, y=y+7)

        # Description
        canvas_proc(c, CANVAS_SET_FONT, text='Consolas', color=COLOR_FG_DESC, size=10)
        canvas_proc(c, CANVAS_SET_BRUSH, color=COLOR_BG_PAGE, style=BRUSH_CLEAR)
        canvas_proc(c, CANVAS_TEXT, text=section['desc'], x=x+12, y=y+TITLE_BAR_H+5)

        # Demo area
        demo_y = y + TITLE_BAR_H + DESC_H + DEMO_PAD_TOP
        demo_h = h - TITLE_BAR_H - DESC_H - DEMO_PAD_TOP - DEMO_PAD_BOT
        if demo_h > 0:
            canvas_proc(c, CANVAS_SET_BRUSH, color=COLOR_BG_DEMO, style=BRUSH_SOLID)
            canvas_proc(c, CANVAS_SET_PEN,   color=COLOR_BORDER, size=1, style=PEN_STYLE_SOLID)
            canvas_proc(c, CANVAS_RECT_FILL,
                        x=x+SECTION_PAD_X, y=demo_y,
                        x2=x+w-SECTION_PAD_X, y2=demo_y+demo_h)
            canvas_proc(c, CANVAS_SET_PEN,   color=COLOR_BORDER, size=1, style=PEN_STYLE_SOLID)
            canvas_proc(c, CANVAS_RECT_FRAME,
                        x=x+SECTION_PAD_X, y=demo_y,
                        x2=x+w-SECTION_PAD_X, y2=demo_y+demo_h)
            section['draw'](c, x+SECTION_PAD_X+8, demo_y+8, w-2*SECTION_PAD_X-16, demo_h-16)

    @staticmethod
    def _label(c, x, y, text, color=COLOR_FG_LABEL, bold=False, size=10):
        style = FONT_B if bold else 0
        canvas_proc(c, CANVAS_SET_FONT, text='Consolas', color=color, size=size, style=style)
        canvas_proc(c, CANVAS_SET_BRUSH, color=COLOR_BG_DEMO, style=BRUSH_CLEAR)
        canvas_proc(c, CANVAS_TEXT, text=text, x=x, y=y)

    # ------------------------------------------------------------------ #
    # Section 0: Overview -- fixed height, no overflow
    # ------------------------------------------------------------------ #
    def _paint_overview(self, c, x, y, w, h):
        self._label(c, x, y+4,  'canvas_proc(id_canvas, id_action, text="", color=-1, size=-1,', bold=True, size=11)
        self._label(c, x, y+24, '                    x=-1, y=-1, x2=-1, y2=-1, style=-1, p1=-1, p2=-1)', bold=True, size=11)
        self._label(c, x, y+50, 'This panel exercises every action below on the SAME canvas:')
        actions = [
            'CANVAS_SET_FONT',       'CANVAS_TEXT',
            'CANVAS_SET_PEN',        'CANVAS_LINE',
            'CANVAS_SET_BRUSH',      'CANVAS_PIXEL',
            'CANVAS_SET_ANTIALIAS',  'CANVAS_RECT',
            'CANVAS_GET_TEXT_SIZE',  'CANVAS_RECT_FRAME',
            'CANVAS_RECT_FILL',      'CANVAS_RECT_ROUND',
            'CANVAS_ELLIPSE',        'CANVAS_POLYGON',
            'CANVAS_BITMAP',
        ]
        col_w = (w - 20) // 2
        for i, name in enumerate(actions):
            col = i % 2
            row = i // 2
            self._label(c, x + col*col_w, y+72 + row*20, name, color=COLOR_INFO_OK)
        # Bonus + scroll info at the bottom (with margin)
        self._label(c, x, y+72 + 8*20 + 16, 'Bonus: IMAGELIST_PAINT, bitmap_proc round-trip.', color=COLOR_INFO_OK)
        self._label(c, x, y+72 + 8*20 + 36, 'Scroll: mouse wheel or scrollbar on the right.', color=COLOR_INFO_OK)
        self._label(c, x, y+72 + 8*20 + 56, 'Click the scrollbar to focus, then use arrow keys / PgUp-Dn / Home-End.', color=COLOR_INFO_OK)

    # ------------------------------------------------------------------ #
    # Section 1: CANVAS_SET_FONT -- 16 rows, 28px each
    # ------------------------------------------------------------------ #
    def _paint_fonts(self, c, x, y, w, h):
        self._label(c, x, y+2, 'style bitmask -> rendered sample', bold=True)
        for i, (mask, name) in enumerate(FONT_STYLES):
            row_y = y + 28 + i * 28   # 28px per row
            self._label(c, x, row_y, name, color=COLOR_FG_LABEL)
            canvas_proc(c, CANVAS_SET_FONT, text='Consolas', color=rgb(0x10,0x20,0x30), size=14, style=mask)
            canvas_proc(c, CANVAS_SET_BRUSH, color=COLOR_BG_DEMO, style=BRUSH_CLEAR)
            canvas_proc(c, CANVAS_TEXT, text='The quick brown fox jumps over the lazy dog', x=x+LABEL_W, y=row_y)

    # ------------------------------------------------------------------ #
    # Section 2: CANVAS_SET_PEN -- PEN_STYLE_* (8 rows, 34px each)
    # ------------------------------------------------------------------ #
    def _paint_pen_styles(self, c, x, y, w, h):
        self._label(c, x, y+2, 'pen style -> thick colored line', bold=True)
        for i, (sv, name) in enumerate(PEN_STYLES):
            row_y = y + 28 + i * 34   # 34px per row for thick lines
            self._label(c, x, row_y, name, color=COLOR_FG_LABEL)
            canvas_proc(c, CANVAS_SET_PEN, color=rgb(0xC0,0x39,0x2B), size=4, style=sv, p1=PEN_CAPS_ROUND, p2=PEN_JOIN_ROUND)
            canvas_proc(c, CANVAS_LINE, x=x+LABEL_W, y=row_y+8, x2=x+w-10, y2=row_y+8)

    # ------------------------------------------------------------------ #
    # Section 3: CANVAS_SET_PEN -- PEN_CAPS_*
    # FIX: Use triangle approach -- 3 corners, each showing a different cap
    # ------------------------------------------------------------------ #
    def _paint_pen_caps(self, c, x, y, w, h):
        self._label(c, x, y+2, 'end-cap style (visible at the ends of thick lines)', bold=True)
        cell_w = (w - 20) // 3
        for i, (cv, name) in enumerate(PEN_CAPS):
            cx = x + i * cell_w
            self._label(c, cx, y+28, name, color=COLOR_FG_LABEL, bold=True)
            # Draw a thick horizontal line with the cap; bounded by a thin gray rect
            # so the cap shape is visible at both ends.
            line_y = y + 70
            # Bounding rect (thin gray)
            canvas_proc(c, CANVAS_SET_PEN, color=COLOR_BORDER_DK, size=1, style=PEN_STYLE_SOLID)
            canvas_proc(c, CANVAS_SET_BRUSH, color=COLOR_BG_DEMO, style=BRUSH_CLEAR)
            canvas_proc(c, CANVAS_RECT_FRAME, x=cx+10, y=line_y-14, x2=cx+cell_w-10, y2=line_y+14)
            # The thick line, with the cap, slightly inset from the bounding rect
            canvas_proc(c, CANVAS_SET_PEN, color=rgb(0x29,0x80,0xB9), size=24,
                        style=PEN_STYLE_SOLID, p1=cv, p2=PEN_JOIN_ROUND)
            canvas_proc(c, CANVAS_LINE, x=cx+24, y=line_y, x2=cx+cell_w-24, y2=line_y)
            # Label below
            self._label(c, cx, y+110, 'cap=' + name.split('_')[-1], size=9, color=COLOR_FG_DESC)
            # Also show a vertical line with the same cap (top end visible)
            line_x = cx + cell_w // 2
            canvas_proc(c, CANVAS_SET_PEN, color=rgb(0xE6,0x7E,0x22), size=20,
                        style=PEN_STYLE_SOLID, p1=cv, p2=PEN_JOIN_ROUND)
            canvas_proc(c, CANVAS_LINE, x=line_x, y=y+130, x2=line_x, y2=y+150)

    # ------------------------------------------------------------------ #
    # Section 4: CANVAS_SET_PEN -- PEN_JOIN_*
    # FIX: Use triangle (like old plugin) so corner join is clearly visible
    # ------------------------------------------------------------------ #
    def _paint_pen_joins(self, c, x, y, w, h):
        self._label(c, x, y+2, 'join style (visible at the corner of a triangle)', bold=True)
        cell_w = (w - 20) // 3
        for i, (jv, name) in enumerate(PEN_JOINS):
            cx = x + i * cell_w
            self._label(c, cx, y+28, name, color=COLOR_FG_LABEL, bold=True)
            # Draw a triangle (3 lines meeting at corners) with the join style.
            # The top corner is the most visible join.
            tri_x = cx + cell_w // 2
            tri_top_y = y + 60
            tri_bot_y = y + 150
            tri_left_x = cx + 24
            tri_right_x = cx + cell_w - 24
            # Set pen with the join
            canvas_proc(c, CANVAS_SET_PEN, color=rgb(0x8E,0x44,0xAD), size=14,
                        style=PEN_STYLE_SOLID, p1=PEN_CAPS_ROUND, p2=jv)
            # Three sides of the triangle
            canvas_proc(c, CANVAS_POLYGON, text=\
                (tri_left_x,  tri_bot_y,
                 tri_x, tri_top_y,
                 tri_right_x, tri_bot_y))
            # Label
            self._label(c, cx, y+165, 'join=' + name.split('_')[-1], size=9, color=COLOR_FG_DESC)

    # ------------------------------------------------------------------ #
    # Section 5: CANVAS_SET_BRUSH -- BRUSH_*
    # ------------------------------------------------------------------ #
    def _paint_brush_styles(self, c, x, y, w, h):
        self._label(c, x, y+2, 'brush style -> filled square', bold=True)
        cell_w = (w - 20) // 4
        cell_h = 80
        for i, (bs, name) in enumerate(BRUSH_STYLES):
            col = i % 4
            row = i // 4
            cx = x + col * cell_w
            cy = y + 28 + row * cell_h
            self._label(c, cx, cy, name, color=COLOR_FG_LABEL)
            canvas_proc(c, CANVAS_SET_BRUSH, color=rgb(0x27,0xAE,0x60), style=bs)
            canvas_proc(c, CANVAS_SET_PEN, color=rgb(0x34,0x49,0x5E), size=1, style=PEN_STYLE_SOLID)
            canvas_proc(c, CANVAS_RECT, x=cx, y=cy+18, x2=cx+cell_w-16, y2=cy+18+34)

    # ------------------------------------------------------------------ #
    # Section 6: CANVAS_SET_ANTIALIAS -- ellipse + TEXT examples
    # ------------------------------------------------------------------ #
    def _paint_antialias(self, c, x, y, w, h):
        self._label(c, x, y+2, 'same ellipse AND text drawn with each antialias mode', bold=True)
        cell_w = (w - 20) // 3
        for i, (aa, name) in enumerate(ANTIALIAS_MODES):
            cx = x + i * cell_w
            self._label(c, cx, y+28, name, color=COLOR_FG_LABEL, bold=True)
            # Ellipse
            canvas_proc(c, CANVAS_SET_ANTIALIAS, style=aa)
            canvas_proc(c, CANVAS_SET_BRUSH, color=0, style=BRUSH_CLEAR)
            canvas_proc(c, CANVAS_SET_PEN, color=rgb(0xE6,0x7E,0x22), size=2, style=PEN_STYLE_SOLID)
            canvas_proc(c, CANVAS_ELLIPSE, x=cx+20, y=y+50, x2=cx+cell_w-20, y2=y+110)
            # Text -- antialias affects text rendering too
            self._label(c, cx, y+130, 'Text sample (size 16):', color=COLOR_FG_DESC)
            canvas_proc(c, CANVAS_SET_ANTIALIAS, style=aa)
            canvas_proc(c, CANVAS_SET_FONT, text='Consolas', color=rgb(0x20,0x20,0x20), size=16, style=FONT_B)
            canvas_proc(c, CANVAS_SET_BRUSH, color=COLOR_BG_DEMO, style=BRUSH_CLEAR)
            canvas_proc(c, CANVAS_TEXT, text='AaBbCcDd 1234', x=cx+10, y=y+150)
            # Smaller text sample
            self._label(c, cx, y+180, 'Text sample (size 10):', color=COLOR_FG_DESC)
            canvas_proc(c, CANVAS_SET_ANTIALIAS, style=aa)
            canvas_proc(c, CANVAS_SET_FONT, text='Consolas', color=rgb(0x20,0x20,0x20), size=10)
            canvas_proc(c, CANVAS_SET_BRUSH, color=COLOR_BG_DEMO, style=BRUSH_CLEAR)
            canvas_proc(c, CANVAS_TEXT, text='The quick brown fox. 1234567890', x=cx+10, y=y+200)
            # Italic text
            self._label(c, cx, y+220, 'Italic text (size 12):', color=COLOR_FG_DESC)
            canvas_proc(c, CANVAS_SET_ANTIALIAS, style=aa)
            canvas_proc(c, CANVAS_SET_FONT, text='Consolas', color=rgb(0x8E,0x44,0xAD), size=12, style=FONT_I|FONT_B)
            canvas_proc(c, CANVAS_SET_BRUSH, color=COLOR_BG_DEMO, style=BRUSH_CLEAR)
            canvas_proc(c, CANVAS_TEXT, text='Italic & bold sample', x=cx+10, y=y+240)
        canvas_proc(c, CANVAS_SET_ANTIALIAS, style=ANTIALIAS_ON)

    # ------------------------------------------------------------------ #
    # Section 7: CANVAS_GET_TEXT_SIZE -- each sample on its own row, NO overlap
    # Layout: label on first line, sample text on next line (below label)
    # ------------------------------------------------------------------ #
    def _paint_text_size(self, c, x, y, w, h):
        self._label(c, x, y+2, 'text -> measured (w,h) -> box drawn around the text', bold=True)
        samples = [
            ('Hello, World!',       10, 0),
            ('The quick brown fox', 12, FONT_B),
            ('CudaText',            16, FONT_B | FONT_I),
            ('canvas_proc()',       14, FONT_U),
            ('1234567890',          18, 0),
        ]
        row_h = 50   # generous: 20px for label + 30px for sample+box
        for i, (txt, size, mask) in enumerate(samples):
            row_y = y + 28 + i * row_h
            # Set font for measurement
            canvas_proc(c, CANVAS_SET_FONT, text='Consolas', color=rgb(0x20,0x20,0x20), size=size, style=mask)
            tw, th = canvas_proc(c, CANVAS_GET_TEXT_SIZE, text=txt)
            # Label on the first line (above the sample)
            self._label(c, x, row_y, '%-22s size=%2d mask=%d  -> %dx%d' % (repr(txt), size, mask, tw, th))
            # Sample text on the second line (below the label)
            sample_y = row_y + 22
            canvas_proc(c, CANVAS_SET_FONT, text='Consolas', color=rgb(0x20,0x20,0x20), size=size, style=mask)
            canvas_proc(c, CANVAS_SET_BRUSH, color=COLOR_BG_DEMO, style=BRUSH_CLEAR)
            canvas_proc(c, CANVAS_TEXT, text=txt, x=x, y=sample_y)
            # Bounding box around the sample text
            canvas_proc(c, CANVAS_SET_PEN, color=rgb(0xC0,0x39,0x2B), size=1, style=PEN_STYLE_DASH)
            canvas_proc(c, CANVAS_SET_BRUSH, color=rgb(0xC0,0x39,0x2B), style=BRUSH_SOLID)
            canvas_proc(c, CANVAS_RECT_FRAME, x=x, y=sample_y, x2=x+tw, y2=sample_y+th)

    # ------------------------------------------------------------------ #
    # Section 8: CANVAS_TEXT -- generous spacing between samples
    # ------------------------------------------------------------------ #
    def _paint_text_variants(self, c, x, y, w, h):
        self._label(c, x, y+2, 'CANVAS_TEXT at different sizes / colors', bold=True)
        canvas_proc(c, CANVAS_SET_BRUSH, color=COLOR_BG_DEMO, style=BRUSH_CLEAR)
        # Big title (size 24) -- at y+30
        canvas_proc(c, CANVAS_SET_FONT, text='Consolas', color=rgb(0x1F,0x4E,0x79), size=24, style=FONT_B)
        canvas_proc(c, CANVAS_TEXT, text='canvas_proc() demo', x=x, y=y+30)
        # Medium italic green (size 14) -- at y+70 (40px gap)
        canvas_proc(c, CANVAS_SET_FONT, text='Consolas', color=rgb(0x27,0xAE,0x60), size=14, style=FONT_I)
        canvas_proc(c, CANVAS_TEXT, text='medium italic green', x=x, y=y+70)
        # Small orange (size 10) -- at y+100 (30px gap)
        canvas_proc(c, CANVAS_SET_FONT, text='Consolas', color=rgb(0xE6,0x7E,0x22), size=10)
        canvas_proc(c, CANVAS_TEXT, text='small orange normal', x=x, y=y+100)
        # Bold + underlined purple (size 12) -- at y+130 (30px gap)
        canvas_proc(c, CANVAS_SET_FONT, text='Consolas', color=rgb(0x8E,0x44,0xAD), size=12, style=FONT_U|FONT_B)
        canvas_proc(c, CANVAS_TEXT, text='bold + underlined purple', x=x, y=y+130)
        # Strikeout (size 12) -- at y+165 (35px gap)
        canvas_proc(c, CANVAS_SET_FONT, text='Consolas', color=rgb(0xC0,0x39,0x2B), size=12, style=FONT_S)
        canvas_proc(c, CANVAS_TEXT, text='strikeout red', x=x, y=y+165)
        # Right side: large size 20
        rx = x + 400
        canvas_proc(c, CANVAS_SET_FONT, text='Consolas', color=rgb(0x16,0xA0,0x85), size=20, style=FONT_B)
        canvas_proc(c, CANVAS_TEXT, text='size=20 bold', x=rx, y=y+30)
        canvas_proc(c, CANVAS_SET_FONT, text='Consolas', color=rgb(0x34,0x49,0x5E), size=8)
        canvas_proc(c, CANVAS_TEXT, text='size=8 tiny', x=rx, y=y+70)

    # ------------------------------------------------------------------ #
    # Section 9: CANVAS_LINE
    # ------------------------------------------------------------------ #
    def _paint_lines(self, c, x, y, w, h):
        self._label(c, x, y+2, 'CANVAS_LINE with each pen style', bold=True)
        for i, (sv, name) in enumerate(PEN_STYLES):
            row_y = y + 28 + i * 34
            self._label(c, x, row_y, name, color=COLOR_FG_LABEL)
            canvas_proc(c, CANVAS_SET_PEN, color=rgb(0x29,0x80,0xB9), size=2, style=sv)
            canvas_proc(c, CANVAS_LINE, x=x+LABEL_W, y=row_y+8, x2=x+w-10, y2=row_y+8)

    # ------------------------------------------------------------------ #
    # Section 10: CANVAS_PIXEL -- BIGGER dots, clearly visible
    # Each "pixel" is drawn as a small cluster of 4x4 actual pixels
    # ------------------------------------------------------------------ #
    def _paint_pixels(self, c, x, y, w, h):
        self._label(c, x, y+2, 'CANVAS_PIXEL: 16x4 grid of 4x4 dots + colorful circle', bold=True)
        # Grid of colored dots -- each dot is 4x4 actual pixels
        grid_cols = 16
        grid_rows = 4
        dot_size = 4   # each dot is 4x4 actual pixels
        dot_gap = 2     # 2px gap between dots
        for row in range(grid_rows):
            for col in range(grid_cols):
                hue = (col*16 + row*32) % 192
                color = self._hsv_to_rgb(hue/192.0, 0.8, 0.95)
                # Draw a 4x4 block of pixels
                px = x + 10 + col * (dot_size + dot_gap)
                py = y + 28 + row * (dot_size + dot_gap)
                for dy in range(dot_size):
                    for dx in range(dot_size):
                        canvas_proc(c, CANVAS_PIXEL, x=px+dx, y=py+dy, color=color)
        # Colorful circle made of pixels (bigger, radius 50)
        self._label(c, x, y+80, 'Circle outline (radius 50, made of single pixels):', bold=True)
        cx0 = x + 100
        cy0 = y + 170
        radius = 50
        for deg in range(0, 360, 2):   # every 2 degrees = 180 pixels
            rad = math.radians(deg)
            px = cx0 + int(round(radius * math.cos(rad)))
            py = cy0 + int(round(radius * math.sin(rad)))
            color = self._hsv_to_rgb(deg/360.0, 0.9, 0.95)
            canvas_proc(c, CANVAS_PIXEL, x=px, y=py, color=color)
        # Labels
        self._label(c, x+220, y+100, 'CANVAS_PIXEL paints one', color=COLOR_FG_DESC)
        self._label(c, x+220, y+118, 'pixel at (x, y, color).', color=COLOR_FG_DESC)
        self._label(c, x+220, y+140, 'Pen and brush are ignored.', color=COLOR_FG_DESC)
        self._label(c, x+220, y+162, 'Above: 4x4 dots made of', color=COLOR_FG_DESC)
        self._label(c, x+220, y+180, '16 actual pixels each.', color=COLOR_FG_DESC)

    @staticmethod
    def _hsv_to_rgb(h, s, v):
        if s == 0:
            val = int(v * 255)
            return (val << 16) | (val << 8) | val
        h6 = h * 6.0
        i = int(h6)
        f = h6 - i
        p = v * (1 - s)
        q = v * (1 - s * f)
        t = v * (1 - s * (1 - f))
        i = i % 6
        if   i == 0: r, g, b = v, t, p
        elif i == 1: r, g, b = q, v, p
        elif i == 2: r, g, b = p, v, t
        elif i == 3: r, g, b = p, q, v
        elif i == 4: r, g, b = t, p, v
        else:        r, g, b = v, p, q
        return (int(b*255) << 16) | (int(g*255) << 8) | int(r*255)

    # ------------------------------------------------------------------ #
    # Section 11: CANVAS_RECT -- pen + brush
    # ------------------------------------------------------------------ #
    def _paint_rects(self, c, x, y, w, h):
        self._label(c, x, y+2, 'CANVAS_RECT: pen (frame) + brush (fill) simultaneously', bold=True)
        n = len(DEMO_COLORS)
        cell_w = (w - 20) // n
        rect_y = y + 30   # start rects 30px below header
        rect_h = 60
        for i, col in enumerate(DEMO_COLORS):
            cx = x + 10 + i * cell_w
            canvas_proc(c, CANVAS_SET_PEN, color=rgb(0x34,0x49,0x5E), size=3, style=PEN_STYLE_SOLID)
            canvas_proc(c, CANVAS_SET_BRUSH, color=col, style=BRUSH_SOLID)
            canvas_proc(c, CANVAS_RECT, x=cx, y=rect_y, x2=cx+cell_w-16, y2=rect_y+rect_h)
            self._label(c, cx, rect_y+rect_h+8, '#%06X' % self._tc2web(col))

    # ------------------------------------------------------------------ #
    # Section 12: CANVAS_RECT_FRAME -- brush only
    # ------------------------------------------------------------------ #
    def _paint_rects_frame(self, c, x, y, w, h):
        self._label(c, x, y+2, 'CANVAS_RECT_FRAME: only outline (brush), no fill', bold=True)
        n = len(DEMO_COLORS)
        cell_w = (w - 20) // n
        rect_y = y + 30
        rect_h = 60
        for i, col in enumerate(DEMO_COLORS):
            cx = x + 10 + i * cell_w
            # Thick pen (size 4) so the outline is clearly visible
            canvas_proc(c, CANVAS_SET_PEN, color=col, size=4, style=PEN_STYLE_SOLID)
            canvas_proc(c, CANVAS_SET_BRUSH, color=col, style=BRUSH_SOLID)
            canvas_proc(c, CANVAS_RECT_FRAME, x=cx, y=rect_y, x2=cx+cell_w-16, y2=rect_y+rect_h)
            self._label(c, cx, rect_y+rect_h+8, 'frame #%d' % i)

    # ------------------------------------------------------------------ #
    # Section 13: CANVAS_RECT_FILL -- brush only
    # ------------------------------------------------------------------ #
    def _paint_rects_fill(self, c, x, y, w, h):
        self._label(c, x, y+2, 'CANVAS_RECT_FILL: only fill (brush), no outline', bold=True)
        n = len(DEMO_COLORS)
        cell_w = (w - 20) // n
        rect_y = y + 30
        rect_h = 60
        for i, col in enumerate(DEMO_COLORS):
            cx = x + 10 + i * cell_w
            canvas_proc(c, CANVAS_SET_BRUSH, color=col, style=BRUSH_SOLID)
            canvas_proc(c, CANVAS_RECT_FILL, x=cx, y=rect_y, x2=cx+cell_w-16, y2=rect_y+rect_h)
            self._label(c, cx, rect_y+rect_h+8, 'fill #%d' % i)

    # ------------------------------------------------------------------ #
    # Section 14: CANVAS_RECT_ROUND
    # ------------------------------------------------------------------ #
    def _paint_rects_round(self, c, x, y, w, h):
        self._label(c, x, y+2, 'CANVAS_RECT_ROUND: style=corner radius', bold=True)
        radii = [4, 8, 16, 24, 32]
        n = len(radii)
        cell_w = (w - 20) // n
        rect_y = y + 30
        rect_h = 60
        for i, r in enumerate(radii):
            cx = x + 10 + i * cell_w
            canvas_proc(c, CANVAS_SET_PEN, color=rgb(0x34,0x49,0x5E), size=2, style=PEN_STYLE_SOLID)
            canvas_proc(c, CANVAS_SET_BRUSH, color=DEMO_COLORS[i % len(DEMO_COLORS)], style=BRUSH_SOLID)
            canvas_proc(c, CANVAS_RECT_ROUND, x=cx, y=rect_y, x2=cx+cell_w-16, y2=rect_y+rect_h, style=r)
            self._label(c, cx, rect_y+rect_h+8, 'radius=%d' % r)

    # ------------------------------------------------------------------ #
    # Section 15: CANVAS_ELLIPSE
    # ------------------------------------------------------------------ #
    def _paint_ellipses(self, c, x, y, w, h):
        self._label(c, x, y+2, 'CANVAS_ELLIPSE: circles and ellipses (pen + brush)', bold=True)
        n = len(DEMO_COLORS)
        cell_w = (w - 20) // n
        # Filled circles in a row
        for i, col in enumerate(DEMO_COLORS):
            cx = x + 10 + i * cell_w
            canvas_proc(c, CANVAS_SET_PEN, color=rgb(0x34,0x49,0x5E), size=1, style=PEN_STYLE_SOLID)
            canvas_proc(c, CANVAS_SET_BRUSH, color=col, style=BRUSH_SOLID)
            canvas_proc(c, CANVAS_ELLIPSE, x=cx, y=y+30, x2=cx+cell_w-16, y2=y+30+50)
        # Outline-only ellipses on the next row
        for i, col in enumerate(DEMO_COLORS):
            cx = x + 10 + i * cell_w
            canvas_proc(c, CANVAS_SET_PEN, color=col, size=3, style=PEN_STYLE_SOLID)
            canvas_proc(c, CANVAS_SET_BRUSH, color=0, style=BRUSH_CLEAR)
            canvas_proc(c, CANVAS_ELLIPSE, x=cx+4, y=y+90, x2=cx+cell_w-20, y2=y+90+40)

    # ------------------------------------------------------------------ #
    # Section 16: CANVAS_POLYGON
    # ------------------------------------------------------------------ #
    def _paint_polygons(self, c, x, y, w, h):
        self._label(c, x, y+2, "CANVAS_POLYGON: text=(x1,y1,x2,y2,...)", bold=True)
        cell_w = (w - 20) // 4
        cy = y + 30
        # Triangle
        cx = x
        self._label(c, cx, cy, 'triangle (3 pts)')
        canvas_proc(c, CANVAS_SET_PEN, color=rgb(0x34,0x49,0x5E), size=2, style=PEN_STYLE_SOLID)
        canvas_proc(c, CANVAS_SET_BRUSH, color=DEMO_COLORS[0], style=BRUSH_SOLID)
        canvas_proc(c, CANVAS_POLYGON, text=(cx+10, cy+90, cx+cell_w-20, cy+90, cx+cell_w//2, cy+30))
        # Quad
        cx = x + cell_w
        self._label(c, cx, cy, 'quad (4 pts)')
        canvas_proc(c, CANVAS_SET_BRUSH, color=DEMO_COLORS[1], style=BRUSH_SOLID)
        canvas_proc(c, CANVAS_POLYGON, text=(cx+10, cy+30, cx+cell_w-20, cy+30, cx+cell_w-20, cy+90, cx+10, cy+90))
        # Pentagon
        cx = x + 2*cell_w
        self._label(c, cx, cy, 'pentagon (5 pts)')
        pts = []
        cx_c, cy_c, rad = cx+cell_w//2, cy+60, 32
        for k in range(5):
            a = -math.pi/2 + k * 2*math.pi/5
            pts.append(int(cx_c + rad * math.cos(a)))
            pts.append(int(cy_c + rad * math.sin(a)))
        canvas_proc(c, CANVAS_SET_BRUSH, color=DEMO_COLORS[2], style=BRUSH_SOLID)
        canvas_proc(c, CANVAS_POLYGON, text=pts)
        # Star
        cx = x + 3*cell_w
        self._label(c, cx, cy, 'star (10 pts)')
        pts = []
        cx_c, cy_c = cx+cell_w//2, cy+60
        for k in range(10):
            r = 36 if k % 2 == 0 else 14
            a = -math.pi/2 + k * 2*math.pi/10
            pts.append(int(cx_c + r * math.cos(a)))
            pts.append(int(cy_c + r * math.sin(a)))
        canvas_proc(c, CANVAS_SET_BRUSH, color=DEMO_COLORS[3], style=BRUSH_SOLID)
        canvas_proc(c, CANVAS_POLYGON, text=pts)

    # ------------------------------------------------------------------ #
    # Section 17: CANVAS_BITMAP
    # ------------------------------------------------------------------ #
    def _paint_bitmap(self, c, x, y, w, h):
        self._label(c, x, y+2, 'CANVAS_BITMAP: p1=id_bitmap, x, y', bold=True)
        h_bmp = self._ensure_demo_bitmap()
        canvas_proc(c, CANVAS_BITMAP, p1=h_bmp, x=x, y=y+30)
        canvas_proc(c, CANVAS_BITMAP, p1=h_bmp, x=x+200, y=y+30)
        self._label(c, x, y+170, 'bitmap #1 at (0, 30)', color=COLOR_INFO_OK)
        self._label(c, x+200, y+170, 'bitmap #2 at (200, 30)', color=COLOR_INFO_OK)

    # ------------------------------------------------------------------ #
    # Section 18: IMAGELIST_PAINT (FIXED: limit to 6 icons, handle missing)
    # ------------------------------------------------------------------ #
    def _paint_imagelist(self, c, x, y, w, h):
        self._label(c, x, y+2, "IMAGELIST_PAINT: icons from app's tab imagelist", bold=True)
        il = app_proc(PROC_GET_TAB_IMAGELIST, '')
        if il == 0 or il is None:
            self._label(c, x, y+30, '(PROC_GET_TAB_IMAGELIST returned 0)', color=COLOR_INFO_ERR)
            return
        # Try to paint up to 6 icons. The standard tab imagelist typically
        # has only a few icons; indices beyond that paint nothing.
        # We paint 6 in a row with bigger spacing.
        num_icons = 6
        spacing = 50
        for i in range(num_icons):
            imagelist_proc(il, IMAGELIST_PAINT, (c, x + i*spacing, y+30, i))
            self._label(c, x + i*spacing, y+60, 'idx %d' % i, size=9)
        # Also try painting icon 0 at a bigger position to confirm it works
        self._label(c, x, y+90, 'Standard tab imagelist may have only a few icons;', color=COLOR_FG_DESC)
        self._label(c, x, y+106, 'higher indices may paint nothing (that is normal).', color=COLOR_FG_DESC)

    # ------------------------------------------------------------------ #
    # Section 19: Combinations
    # ------------------------------------------------------------------ #
    def _paint_combos(self, c, x, y, w, h):
        self._label(c, x, y+2, 'Compositions mixing several canvas_proc() actions', bold=True)
        # Traffic light
        cx, cy = x+30, y+30
        canvas_proc(c, CANVAS_SET_PEN, color=rgb(0x20,0x20,0x20), size=2, style=PEN_STYLE_SOLID)
        canvas_proc(c, CANVAS_SET_BRUSH, color=rgb(0x20,0x20,0x20), style=BRUSH_SOLID)
        canvas_proc(c, CANVAS_RECT_ROUND, x=cx, y=cy, x2=cx+50, y2=cy+170, style=10)
        for i, col in enumerate([rgb(0xE7,0x4C,0x3C), rgb(0xF1,0xC4,0x0F), rgb(0x27,0xAE,0x60)]):
            canvas_proc(c, CANVAS_SET_BRUSH, color=col, style=BRUSH_SOLID)
            canvas_proc(c, CANVAS_SET_PEN, color=rgb(0x20,0x20,0x20), size=1, style=PEN_STYLE_SOLID)
            canvas_proc(c, CANVAS_ELLIPSE, x=cx+10, y=cy+10+i*55, x2=cx+40, y2=cy+40+i*55)
        # Bar chart
        bx, by = x+150, y+30
        chart_h, chart_w = 170, 200
        canvas_proc(c, CANVAS_SET_PEN, color=rgb(0x34,0x49,0x5E), size=1, style=PEN_STYLE_SOLID)
        canvas_proc(c, CANVAS_SET_BRUSH, color=rgb(0xEC,0xF0,0xF1), style=BRUSH_SOLID)
        canvas_proc(c, CANVAS_RECT, x=bx, y=by, x2=bx+chart_w, y2=by+chart_h)
        bars = [0.3, 0.6, 0.85, 0.45, 0.95, 0.55]
        bar_w = (chart_w - 20) // len(bars)
        for i, frac in enumerate(bars):
            bh = int((chart_h - 20) * frac)
            canvas_proc(c, CANVAS_SET_BRUSH, color=DEMO_COLORS[i % len(DEMO_COLORS)], style=BRUSH_SOLID)
            canvas_proc(c, CANVAS_SET_PEN, color=rgb(0x34,0x49,0x5E), size=1, style=PEN_STYLE_SOLID)
            canvas_proc(c, CANVAS_RECT, x=bx+10+i*bar_w, y=by+chart_h-10-bh, x2=bx+10+(i+1)*bar_w-4, y2=by+chart_h-10)
        # Chip with text
        rx, ry = x+400, y+30
        canvas_proc(c, CANVAS_SET_PEN, color=rgb(0x29,0x80,0xB9), size=2, style=PEN_STYLE_SOLID)
        canvas_proc(c, CANVAS_SET_BRUSH, color=rgb(0x34,0x98,0xDB), style=BRUSH_SOLID)
        canvas_proc(c, CANVAS_RECT_ROUND, x=rx, y=ry, x2=rx+220, y2=ry+40, style=20)
        canvas_proc(c, CANVAS_SET_FONT, text='Consolas', color=rgb(0xFF,0xFF,0xFF), size=14, style=FONT_B)
        canvas_proc(c, CANVAS_SET_BRUSH, color=rgb(0x34,0x98,0xDB), style=BRUSH_CLEAR)
        canvas_proc(c, CANVAS_TEXT, text='CANVAS_TEXT on CANVAS_RECT_ROUND', x=rx+12, y=ry+12)
        #test CANVAS_COPY_RECT
        w = 220
        h = 40
        rect_from = (rx, ry, rx+w, ry+h)
        rect_to = (0, 0, w, h)
        b = bitmap_proc(0, BITMAP_CREATE, w, h)
        c2 = bitmap_proc(b, BITMAP_GET_CANVAS)
        canvas_proc(c2, CANVAS_COPY_RECT, text=rect_from, x=rect_to[0], y=rect_to[1], x2=rect_to[2], y2=rect_to[3], p1=c)
        canvas_proc(c, CANVAS_COPY_RECT, text=rect_to, x=rect_from[0]-2, y=rect_from[1]+60, x2=rect_from[2]+15+55, y2=rect_from[3]+60+70, p1=c2)
        bitmap_proc(b, BITMAP_FREE)


    @staticmethod
    def _tc2web(tc):
        r = tc & 0xFF
        g = (tc >> 8) & 0xFF
        b = (tc >> 16) & 0xFF
        return (r << 16) | (g << 8) | b
