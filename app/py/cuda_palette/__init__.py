''' Plugin for CudaText editor
Authors:
    Andrey Kvichansky    (kvichans on github.com)
Version:
    '0.9.0 2016-11-30'
ToDo: (see end of file)
'''

import  colorsys
import  cudatext        as app
#from    cudatext    import ed
#import  cudatext_cmd    as cmds
import  cudax_lib       as apx
from    .cd_plug_lib    import *
_   = get_translation(__file__) # I18N

pass;                           # Logging
pass;                           LOG = (-2==-2)  # Do or dont logging.

def int_to_rgb(clr):
    return  255&clr      ,  255&(clr>>8)      ,  255&(clr>>16)
def int_to_rgb01(clr):
    return (255&clr)/255 , (255&(clr>>8))/255 , (255&(clr>>16))/255
def rgb_to_int(r,g,b):
    return r             | (g         <<8)    | (b         <<16)
def rgb01_to_int(r,g,b):
    return int(255*r)    | (int(255*g)<<8)    | (int(255*b)<<16)
clr_h2i = apx.html_color_to_int

COLOR_NAMES={}
PLTYPES = [ '60 colors: 3*20'
        ,   '142 colors: 7-hexagon'
        ,   '216 web-colors: 9-hexagon'
        ,   '216 web-colors: dragon'
        ,   '216 web-colors: candles'
        ,   '3221 colors: 7-hexagon, dialog'
        ,   '146 named colors'
        ,   '420 named colors: 12*35'
        ,   '1431 named colors: 27*53'
        ]
def dlg_color_palette(caption, old_color=None, palette_type=None):
    """ Show dlg to choose new color or view old one.
        Params
            caption         (str) Title for dlg.
            old_color       (int) Old color as RGB-int. None if no.
            palette_type    (str) Palette name
        Return
                            (int) Selected color    
            COLOR_NONE      (int) If "No color"
            None                  If "Cancel"
    """
    pass;                      #LOG and log('caption, old_color, palette_type={}',(caption, old_color, palette_type))
    if not palette_type:
        palette_type    = apx.get_opt('last_palette_type', PLTYPES[0])
        palette_type    = palette_type if palette_type in PLTYPES else PLTYPES[0]
    grey_clr_for_plt    = apx.get_opt('last_palette_grey_level', 0)
    
#   cnHMLs  = []
    cnRGBs  = []
#   cnHSVs  = []
    
    H_NMED  = _('Point named colors with "!"')
    H_NRBY  = _('Point the nearest name for all colors'
                '\r"!"  - [almost] exactly'
                '\r"."  - surely'
                '\r".." - doubtful'
    )
    fid     = 'type'
    vals    = dict(pltp=PLTYPES.index(palette_type)
                  ,nmed=False
                  ,nrby=False
                  )
    while True:
#       if not cnHMLs:
#           cnHMLs  = [((c&0xFF0000)>>16, (c&0x00FF00)>>8, c&0x0000FF, s) for (c, s) in COLOR_NAMES.items()]
        if not cnRGBs:
            cnRGBs  = [(int_to_rgb(c)
                       , c, s) for (c, s) in COLOR_NAMES.items()]
#       if not cnHSVs:
#           cnHSVs  = [(list(map(lambda c:int(255*c), colorsys.rgb_to_hsv(*int_to_rgb01(c))))
#                      , c, s) for (c, s) in COLOR_NAMES.items()]

        clrs,       \
        w,h,        \
        sp_clrs,    \
        sp_w,sp_h   = _dlg_color_palette_clrs(PLTYPES[vals['pltp']], grey_clr_for_plt)
        
        vw_nmed = vals['nmed']                  # Point of named color
        vw_nrby = vals['nrby'] and vw_nmed      # Point color names with nearby

        max_cnt     = max(len(r) for ir,r in enumerate(   clrs))
        sp_max_cnt  = max(len(r) for ir,r in enumerate(sp_clrs)) if sp_clrs else 0
        plt_w       =    w *    max_cnt
        sp_plt_w    = sp_w * sp_max_cnt
        plt_w       = max(plt_w, sp_plt_w)
        
        cnts    = []
        # Main plt
        for     irow,crow in enumerate(clrs):
            shft    = (plt_w - w *len(crow)) // 2
            for icol,clr  in enumerate(crow):
                if clr is None: continue
                R, G, B     = int_to_rgb(clr)
                H, S, V     = list(int(255*c) for c in colorsys.rgb_to_hsv(R/255, G/255, B/255))
                nmd         = clr in COLOR_NAMES
                nm          = COLOR_NAMES.get(clr, '')
                sure,ma     = '', ''
                cn          = clr if nm else 0
                if not nm and vw_nrby:
                    d,ma,   \
                    nm,cn   = min((abs(R-cnR)+abs(G-cnG)+abs(B-cnB), max(abs(R-cnR),abs(G-cnG),abs(B-cnB)), sn, c) for ((cnR, cnG, cnB), c, sn) in cnRGBs)
                    sure    = '!'   if ma<=3 else \
                              '.'   if ma<=5 else \
                              '..'  if ma<=7 else \
                              ''
#                   d,ma,   \
#                   nm,cn   = min((abs(H-cnH)+abs(S-cnS)+abs(V-cnV), max(abs(H-cnH),abs(S-cnS),abs(V-cnV)), sn, c) for ((cnH, cnS, cnV), c, sn) in cnHSVs)
#                   sure    = '!'   if ma<= 5 else \
#                             '.'   if ma<= 9 else \
#                             '..'  if ma<=13 else \
#                             ''
                    nm      = nm if sure else ''
                cnts += [dict(tp='clr'
                        ,cid=f('c{}', clr)  
                        ,t=5+irow*h, h=h, l=shft+10+icol*w, w=w
                        ,props=f('{bord_w},{bg},{fg},{bord_c}'  , bord_w=(2 if clr in (old_color,grey_clr_for_plt) else 1)
                                                                , bg    =clr
                                                                , fg    =(0xffffff if (R+G+B)/3<125 else 0x000000)
                                                                , bord_c=(0 if clr==old_color else 0xffffff if clr==grey_clr_for_plt else clr_h2i('#b6feff')) )
                        ,cap=                                     ('!' if vw_nmed and nmd else sure) #+ str(ma)
                        ,hint=f('{h}\rRGB:{R},{G},{B}\rHSV:{H},{S},{V}{n}'
                                                                , h=apx.int_to_html_color(clr).upper()
                                                                , R=R, G=G, B=B
                                                                , H=H, S=S, V=V
                                                                , n='\r'+nm+('' if nmd else f('\r({})',apx.int_to_html_color(cn).upper())) if nm else '')
                        ,act='1'
                    )]
        plt_h   = h * len(clrs)

        # Spec plt
        for     irow,crow in enumerate(sp_clrs):
            shft    = (plt_w - sp_w *len(crow)) // 2
            for icol,clr  in enumerate(crow):
                if clr is None: continue
                R, G, B     = int_to_rgb(clr)
                H, S, V     = list(int(255*c) for c in colorsys.rgb_to_hsv(R/255, G/255, B/255))
                nmd         = clr in COLOR_NAMES
                nm          = COLOR_NAMES.get(clr, '')
                sure        = ''
                if not nm and vw_nrby:
                    d,ma,   \
                    nm,cn   = min((abs(R-cnR)+abs(G-cnG)+abs(B-cnB), max(abs(R-cnR),abs(G-cnG),abs(B-cnB)), sn, c) for ((cnR, cnG, cnB), c, sn) in cnRGBs)
                    sure    = '!'   if ma<=3 else \
                              '.'   if ma<=5 else \
                              '..'  if ma<=7 else \
                              ''
#                   d,ma,   \
#                   nm,cn   = min((abs(H-cnH)+abs(S-cnS)+abs(V-cnV), max(abs(H-cnH),abs(S-cnS),abs(V-cnV)), sn, c) for ((cnH, cnS, cnV), c, sn) in cnHSVs)
#                   sure    = '!'   if ma<= 5 else \
#                             '.'   if ma<= 9 else \
#                             '..'  if ma<=13 else \
#                             ''
                    nm      = nm if sure else ''
                cnts += [dict(tp='clr'
                        ,cid=f('s{}', clr)  
                        ,t=plt_h+5+irow*sp_h, h=sp_h, l=shft+10+icol*sp_w, w=sp_w
                        ,props=f('{bord_w},{bg},{fg},{bord_c}'  , bord_w=1 #(2 if clr in (old_color,grey_clr_for_plt) else 1)
                                                                , bg    =clr
                                                                , fg    =(0xffffff if (R+G+B)/3<125 else 0x000000)
                                                                , bord_c=(0 if clr==old_color else 0xffffff if clr==grey_clr_for_plt else clr_h2i('#b6feff')) )
                        ,cap=                                   '^' if clr==grey_clr_for_plt else '' #if vw_nmed and nmd else sure
                        ,hint=f('{h}\rRGB:{R},{G},{B}\rHSV:{H},{S},{V}{n}'
                                                                , h=apx.int_to_html_color(clr).upper()
                                                                , R=R, G=G, B=B
                                                                , H=H, S=S, V=V
                                                                , n='\r'+nm+('' if nmd else f('\r({})',apx.int_to_html_color(cn).upper())) if nm else '')
                        ,act='1'
                    )]
        sp_plt_h    = sp_h * len(sp_clrs)

        plt_h       = plt_h + sp_plt_h
        
        cnts   += [dict(cid='pltp'  ,tp='cb-ro' ,t=5+plt_h+15   ,l=10           ,w=250,items=PLTYPES                    ,act='1'                                    )]
        cnts   += [dict(cid='nmed'  ,tp='ch'    ,tid='----'     ,l=10           ,w=130,cap=_('M&ark named') ,hint=H_NMED,act='1'                                    )]
        cnts   += [dict(cid='nrby'  ,tp='ch'    ,tid='----'     ,l=140          ,w=120,cap=_('N&earby')     ,hint=H_NRBY,act='1', en=('1' if vals['nmed'] else '0') )]
        cnts   += [dict(cid='noth'  ,tp='bt'    ,t=5+plt_h+15   ,l=10+plt_w-80  ,w=80 ,cap='&No color'                                                              )]
        cnts   += [dict(cid='----'  ,tp='bt'    ,t=5+plt_h+45   ,l=10+plt_w-80  ,w=80 ,cap='Cancel'                                                                 )]
        dlg_w   = 10 + plt_w + 10
        dlg_h   = 5  + plt_h + 15 + 30 + 25 + 5

        aid,vals,*_t = dlg_wrapper(caption, dlg_w, dlg_h, cnts, vals, focus_cid=fid)
        pass;                  #LOG and log('aid,vals={}',(aid,vals))

        if not aid or aid=='----': return None
        if aid=='noth': return app.COLOR_NONE
        if aid[0]=='c': return int(aid[1:])
        
        if aid[0]=='s':    # Special color
            clr     = int(aid[1:])
            R, G, B = int_to_rgb(clr) #255&clr, 255&(clr>>8), 255&(clr>>16)
            if R==G and G==B:
                grey_clr_for_plt    = clr
                apx.set_opt('last_palette_grey_level', clr)
            continue#while
        if aid=='pltp':
            apx.set_opt('last_palette_type', PLTYPES[vals['pltp']])
       #do while
   #def dlg_color_palette

def _dlg_color_palette_clrs(palette_type, grey_clr_for_plt=0):
    R1          = 0x000033
    G1          = 0x003300
    B1          = 0x330000
    inversed    = True
    clrs        = ()
    w,h         = 21,21
    sp_clrs     = ()
    sp_w,sp_h   = 21,21
    
    def invert_HML(clrs):
        return list(list(
            (c & 0x0000ff)<<16 | (c & 0x00ff00) | (c & 0xff0000)>>16 
                if c is not None else c
            for c in row) for row in clrs)

    if False:pass
    elif palette_type=='3221 colors: 7-hexagon, dialog':
        # 6*6 + 35 * (6+7+8+9+10+11+10+9+8+7+6) = 36 + 35 *91 = 3221
        inversed= False
        clrs    = (
                               (0x00ffff,0x00d4ff,0x00aaff,0x007fff,0x0055ff,0x002aff,0x0000ff)                                     # 0
,                           (0x00ffd4,None    ,None    ,None    ,None    ,None    ,None    ,0x2a00ff)                               # 1
,                        (0x00ffaa,None    ,None    ,None    ,None    ,None    ,None    ,None    ,0x5500ff)                         # 2
,                     (0x00ff7f,None    ,None    ,None    ,None    ,None    ,None    ,None    ,None    ,0x7f00ff)                   # 3
,                  (0x00ff55,None    ,None    ,None    ,None    ,None    ,None    ,None    ,None    ,None    ,0xaa00ff)             # 4
,               (0x00ff2a,None    ,None    ,None    ,None    ,None    ,None    ,None    ,None    ,None    ,None    ,0xd400ff)       # 5
,            (0x00ff00,None    ,None    ,None    ,None    ,None    ,None    ,None    ,None    ,None    ,None    ,None    ,0xff00ff) # 6
,               (0x2aff00,None    ,None    ,None    ,None    ,None    ,None    ,None    ,None    ,None    ,None    ,0xff00d4)       # 7
,                  (0x55ff00,None    ,None    ,None    ,None    ,None    ,None    ,None    ,None    ,None    ,0xff00aa)             # 8
,                     (0x7fff00,None    ,None    ,None    ,None    ,None    ,None    ,None    ,None    ,0xff007f)                   # 9
,                        (0xaaff00,None    ,None    ,None    ,None    ,None    ,None    ,None    ,0xff0055)                         # 10
,                           (0xd4ff00,None    ,None    ,None    ,None    ,None    ,None    ,0xff002a)                               # 11
,                              (0xffff00,0xffd400,0xffaa00,0xff7f00,0xff5500,0xff2a00,0xff0000)                                     # 12
,                                                              ()
                )
        w,h         = 29,29
        sp_clrs     = (
 (0xffffff, 0xf7f7f7, 0xf0f0f0, 0xe8e8e8, 0xe1e1e1, 0xd9d9d9, 0xd2d2d2, 0xcacaca, 0xc3c3c3, 0xbbbbbb, 0xb4b4b4, 0xacacac, 
  0xa5a5a5, 0x9d9d9d, 0x969696, 0x8e8e8e, 0x878787, 0x7f7f7f, 0x787878, 0x707070, 0x696969, 0x616161, 0x5a5a5a, 0x525252, 
  0x4b4b4b, 0x434343, 0x3c3c3c, 0x343434, 0x2d2d2d, 0x252525, 0x1e1e1e, 0x161616, 0x0f0f0f, 0x070707, 0x000000),
        )
        sp_w,sp_h   = 11,23
        clrs    = invert_HML(clrs)
        sp_clrs = invert_HML(sp_clrs)
        # Center
        clrs[6][6]  = grey_clr_for_plt

        def interpolate(r1,c1, r2,c2):
            steps   = max(abs(r2-r1), abs(c2-c1)) 
            rs,cs   = int((r2-r1)/steps), int((c2-c1)/steps)
            R1,G1,B1= int_to_rgb(clrs[r1][c1])
            R2,G2,B2= int_to_rgb(clrs[r2][c2])
            Rs,Gs,Bs= (R2-R1)/steps, (G2-G1)/steps, (B2-B1)/steps 
            for i in range(1, steps):
                clrs[r1+rs*i][c1+cs*i]  = rgb_to_int(int(R1 + Rs*i), int(G1 + Gs*i), int(B1 + Bs*i))
        # Vertex-Center
        interpolate( 0, 0, 6, 6)
        interpolate( 0, 6, 6, 6)
        interpolate( 6, 0, 6, 6)
        interpolate( 6,12, 6, 6)
        interpolate(12, 0, 6, 6)
        interpolate(12, 6, 6, 6)
        interpolate(12, 6, 6, 6)
        # Filling sectors
        pass;                       interpolate( 1, 1, 1, 6)
        pass;                       interpolate( 2, 2, 2, 6)
        pass;                       interpolate( 3, 3, 3, 6)
        pass;                       interpolate( 4, 4, 4, 6)
        interpolate( 2, 0, 2, 2);                               interpolate( 2, 6, 2, 8)
        interpolate( 3, 0, 3, 3);                               interpolate( 3, 6, 3, 9)
        interpolate( 4, 0, 4, 4);                               interpolate( 4, 6, 4,10)
        interpolate( 5, 0, 5, 5);                               interpolate( 5, 6, 5,11)
                                                                                        
        interpolate( 7, 0, 7, 5);                               interpolate( 7, 6, 7,11)
        interpolate( 8, 0, 8, 4);                               interpolate( 8, 6, 8,10)
        interpolate( 9, 0, 9, 3);                               interpolate( 9, 6, 9, 9)
        interpolate(10, 0,10, 2);                               interpolate(10, 6,10, 8)
        pass;                       interpolate( 8, 4, 8, 6)
        pass;                       interpolate( 9, 3, 9, 6)
        pass;                       interpolate(10, 2,10, 6)
        pass;                       interpolate(11, 1,11, 6)

    elif palette_type=='60 colors: 3*20':
        clrs    = (
    (0xff8080,0xffa080,0xffc080,0xffe080,0xffff80,0xe0ff80,0xc0ff80,0xa0ff80,0x80ff80,0x80ffa0,0x80ffc0,0x80ffe0,0x80ffff,0x80e0ff,0x80c0ff,0x80a0ff,0x8080ff,0xa080ff,0xc080ff,0xe080ff)
,   (0xff0000,0xff4000,0xff8000,0xffc000,0xffff00,0xc0ff00,0x80ff00,0x40ff00,0x01ff00,0x01ff40,0x01ff80,0x01ffc0,0x01ffff,0x00c0ff,0x0080ff,0x0040ff,0x0000ff,0x4000ff,0x8000ff,0xc000ff)
,   (0x800000,0x802000,0x804000,0x806000,0x808000,0x608000,0x408000,0x208000,0x008000,0x008020,0x008040,0x008060,0x008080,0x006080,0x004080,0x002080,0x000080,0x200080,0x400080,0x600080)
                )
        w,h     = 31,31

    elif palette_type=='146 named colors':
        # http://colorscheme.ru/html-colors.html
        clrs    = (
 (0xCD5C5C,0xF08080,0xFA8072,0xE9967A,0xFFA07A,0xDC143C,0xFF0000,0xB22222,0x8B0000,0xFFC0CB,0xFFB6C1,0xFF69B4,0xFF1493,0xC71585,0xDB7093)
,(0xFFA07A,0xFF7F50,0xFF6347,0xFF4500,0xFF8C00,0xFFA500,0xFFD700,0xFFFF00,0xFFFFE0,0xFFFACD,0xFAFAD2,0xFFEFD5,0xFFE4B5,0xFFDAB9,0xEEE8AA,0xF0E68C,0xBDB76B)
,(0xE6E6FA,0xD8BFD8,0xDDA0DD,0xEE82EE,0xDA70D6,0xFF00FF,0xBA55D3,0x9370DB,0x8A2BE2,0x9400D3,0x9932CC,0x8B008B,0x800080,0x4B0082,0x6A5ACD,0x483D8B)
,(0xFFF8DC,0xFFEBCD,0xFFE4C4,0xFFDEAD,0xF5DEB3,0xDEB887,0xD2B48C,0xBC8F8F,0xF4A460,0xDAA520,0xB8860B,0xCD853F,0xD2691E,0x8B4513,0xA0522D,0xA52A2A,0x800000)
,(0xC0C0C0,0xFF00FF,0x800080,0xFF0000,0x800000,0x808000,0x00FF00,0x008000,0x008080)
,(0xADFF2F,0x7FFF00,0x7CFC00,0x32CD32,0x98FB98,0x90EE90,0x00FA9A,0x00FF7F,0x3CB371,0x2E8B57,0x228B22,0x008000,0x006400,0x9ACD32,0x6B8E23,0x808000,0x556B2F,0x66CDAA,0x8FBC8F,0x20B2AA,0x008B8B)
,(0x00FFFF,0xE0FFFF,0xAFEEEE,0x7FFFD4,0x40E0D0,0x48D1CC,0x00CED1,0x5F9EA0,0x4682B4,0xB0C4DE,0xB0E0E6,0xADD8E6,0x87CEEB,0x87CEFA,0x00BFFF,0x1E90FF,0x6495ED,0x7B68EE,0x4169E1,0x0000FF,0x0000CD,0x00008B,0x000080,0x191970)
,(0xFFFFFF,0xFFFAFA,0xF0FFF0,0xF5FFFA,0xF0FFFF,0xF0F8FF,0xF8F8FF,0xF5F5F5,0xFFF5EE,0xF5F5DC,0xFDF5E6,0xFFFAF0,0xFFFFF0,0xFAEBD7,0xFAF0E6,0xFFF0F5,0xFFE4E1)
,(0xDCDCDC,0xD3D3D3,0xC0C0C0,0xA9A9A9,0x808080,0x696969,0x778899,0x708090,0x2F4F4F,0x000000)
                )
        w,h     = 31,31

    elif palette_type=='420 named colors: 12*35':
        # 420 = 6*7*10 = 12*35
        clrs    = (
 (0x000000,0x1C1C1C,0x363636,0x4F4F4F,0x696969,0x800000,0x800080,0x808000,0x808080,0x8B0000,0x8B008B,0x8B0A50)
,(0x8B1A1A,0x8B1C62,0x8B2252,0x8B2323,0x8B2500,0x8B3626,0x8B3A3A,0x8B3A62,0x8B3E2F,0x8B4500,0x8B4513,0x8B4726)
,(0x8B475D,0x8B4789,0x8B4C39,0x8B5742,0x8B5A00,0x8B5A2B,0x8B5F65,0x8B636C,0x8B658B,0x8B668B,0x8B6914,0x8B6969)
,(0x8B7355,0x8B7500,0x8B7765,0x8B795E,0x8B7B8B,0x8B7D6B,0x8B7D7B,0x8B7E66,0x8B814C,0x8B8378,0x8B8386,0x8B864E)
,(0x8B8682,0x8B8878,0x8B8970,0x8B8989,0x8B8B00,0x8B8B7A,0x8B8B83,0x9C9C9C,0xA0522D,0xA52A2A,0xA9A9A9,0xB03060)
,(0xB22222,0xB5B5B5,0xB8860B,0xBC8F8F,0xBDB76B,0xBEBEBE,0xC0C0C0,0xC71585,0xCD0000,0xCD00CD,0xCD1076,0xCD2626)
,(0xCD2990,0xCD3278,0xCD3333,0xCD3700,0xCD4F39,0xCD5555,0xCD5B45,0xCD5C5C,0xCD6090,0xCD6600,0xCD661D,0xCD6839)
,(0xCD6889,0xCD69C9,0xCD7054,0xCD8162,0xCD8500,0xCD853F,0xCD8C95,0xCD919E,0xCD950C,0xCD96CD,0xCD9B1D,0xCD9B9B)
,(0xCDAA7D,0xCDAD00,0xCDAF95,0xCDB38B,0xCDB5CD,0xCDB79E,0xCDB7B5,0xCDBA96,0xCDBE70,0xCDC0B0,0xCDC1C5,0xCDC5BF)
,(0xCDC673,0xCDC8B1,0xCDC9A5,0xCDC9C9,0xCDCD00,0xCDCDB4,0xCDCDC1,0xCFCFCF,0xD02090,0xD2691E,0xD2B48C,0xD3D3D3)
,(0xD8BFD8,0xDA70D6,0xDAA520,0xDB7093,0xDC143C,0xDCDCDC,0xDDA0DD,0xDEB887,0xE8E8E8,0xE9967A,0xEE0000,0xEE00EE)
,(0xEE1289,0xEE2C2C,0xEE30A7,0xEE3A8C,0xEE3B3B,0xEE4000,0xEE5C42,0xEE6363,0xEE6A50,0xEE6AA7,0xEE7600,0xEE7621)
,(0xEE7942,0xEE799F,0xEE7AE9,0xEE8262,0xEE82EE,0xEE9572,0xEE9A00,0xEE9A49,0xEEA2AD,0xEEA9B8,0xEEAD0E,0xEEAEEE)
,(0xEEB422,0xEEB4B4,0xEEC591,0xEEC900,0xEECBAD,0xEECFA1,0xEED2EE,0xEED5B7,0xEED5D2,0xEED8AE,0xEEDC82,0xEEDD82)
,(0xEEDFCC,0xEEE0E5,0xEEE5DE,0xEEE685,0xEEE8AA,0xEEE8CD,0xEEE9BF,0xEEE9E9,0xEEEE00,0xEEEED1,0xEEEEE0,0xF08080)
,(0xF0E68C,0xF4A460,0xF5DEB3,0xF5F5DC,0xF5F5F5,0xFA8072,0xFAEBD7,0xFAF0E6,0xFAFAD2,0xFDF5E6,0xFF0000,0xFF00FF)
,(0xFF1493,0xFF3030,0xFF34B3,0xFF3E96,0xFF4040,0xFF4500,0xFF6347,0xFF69B4,0xFF6A6A,0xFF6EB4,0xFF7256,0xFF7F00)
,(0xFF7F24,0xFF7F50,0xFF8247,0xFF82AB,0xFF83FA,0xFF8C00,0xFF8C69,0xFFA07A,0xFFA500,0xFFA54F,0xFFAEB9,0xFFB5C5)
,(0xFFB6C1,0xFFB90F,0xFFBBFF,0xFFC0CB,0xFFC125,0xFFC1C1,0xFFD39B,0xFFD700,0xFFDAB9,0xFFDEAD,0xFFE1FF,0xFFE4B5)
,(0xFFE4C4,0xFFE4E1,0xFFE7BA,0xFFEBCD,0xFFEC8B,0xFFEFD5,0xFFEFDB,0xFFF0F5,0xFFF5EE,0xFFF68F,0xFFF8DC,0xFFFACD)
,(0xFFFAF0,0xFFFAFA,0xFFFF00,0xFFFFE0,0xFFFFF0,0xFFFFFF,0x2F4F4F,0x006400,0x556B2F,0x008000,0x008080,0x008B00)
,(0x008B45,0x008B8B,0x228B22,0x2E8B57,0x458B00,0x458B74,0x528B8B,0x548B54,0x668B8B,0x698B22,0x698B69,0x6E8B3D)
,(0x7A8B8B,0x838B83,0x838B8B,0x6B8E23,0x20B2AA,0x3CB371,0x8FBC8F,0x00CD00,0x00CD66,0x00CDCD,0x32CD32,0x43CD80)
,(0x66CD00,0x66CDAA,0x79CDCD,0x7CCD7C,0x96CDCD,0x9ACD32,0x9BCD9B,0xA2CD5A,0xB4CDCD,0xC1CDC1,0xC1CDCD,0x48D1CC)
,(0x40E0D0,0x00EE00,0x00EE76,0x00EEEE,0x4EEE94,0x76EE00,0x76EEC6,0x8DEEEE,0x90EE90,0xAEEEEE,0xB3EE3A,0xB4EEB4)
,(0xBCEE68,0xD1EEEE,0xE0EEE0,0xE0EEEE,0x00FA9A,0x98FB98,0x7CFC00,0x00FF00,0x00FF7F,0x00FFFF,0x54FF9F,0x7FFF00)
,(0x7FFFD4,0x97FFFF,0x9AFF9A,0xADFF2F,0xBBFFFF,0xC0FF3E,0xC1FFC1,0xCAFF70,0xE0FFFF,0xF0FFF0,0xF0FFFF,0xF5FFFA)
,(0x191970,0x000080,0x4B0082,0x00008B,0x00688B,0x00868B,0x104E8B,0x27408B,0x36648B,0x473C8B,0x483D8B,0x4A708B)
,(0x53868B,0x551A8B,0x5D478B,0x607B8B,0x68228B,0x68838B,0x6C7B8B,0x6E7B8B,0x7A378B,0x708090,0x778899,0x5F9EA0)
,(0x4682B4,0x9932CC,0x0000CD,0x009ACD,0x00C5CD,0x1874CD,0x3A5FCD,0x4F94CD,0x6959CD,0x6A5ACD,0x6CA6CD,0x7AC5CD)
,(0x7D26CD,0x8968CD,0x8DB6CD,0x9A32CD,0x9AC0CD,0x9FB6CD,0xA2B5CD,0xB452CD,0x00CED1,0x9400D3,0xBA55D3,0x9370DB)
,(0xB0C4DE,0x4169E1,0x8A2BE2,0xADD8E6,0xB0E0E6,0x87CEEB,0x6495ED,0x0000EE,0x00B2EE,0x00E5EE,0x1C86EE,0x436EEE)
,(0x5CACEE,0x7A67EE,0x7B68EE,0x7EC0EE,0x8EE5EE,0x912CEE,0x9F79EE,0xA4D3EE,0xB23AEE,0xB2DFEE,0xB9D3EE,0xBCD2EE)
,(0xD15FEE,0xA020F0,0x87CEFA,0xE6E6FA,0x0000FF,0x00BFFF,0x00F5FF,0x1E90FF,0x4876FF,0x63B8FF,0x836FFF,0x8470FF)
,(0x87CEFF,0x98F5FF,0x9B30FF,0xAB82FF,0xB0E2FF,0xBF3EFF,0xBFEFFF,0xC6E2FF,0xCAE1FF,0xE066FF,0xF0F8FF,0xF8F8FF)
                )
        clrs    = list( list(clrs[ir][ic] for ir in range(len(clrs))) for ic in range(len(clrs[0])) )   # Transposition
        w,h     = 25,25

    elif palette_type=='1431 named colors: 27*53':
        clrs    = (
 (0x100C08,0x1A1110,0x1B1B1B,0x1C1C1C,0x242124,0x2C1608,0x343434,0x363636,0x3B331C,0x3C1414,0x3C341F,0x3D0C02,0x3D2B1F,0x43302E,0x480607,0x483C32,0x4B3621,0x4E1609,0x4F3A3C,0x4F4F4F,0x50404D,0x51484F,0x534B4F,0x543D37,0x555555,0x560319,0x59260B)
,(0x592720,0x5B3256,0x5D3954,0x614051,0x635147,0x644117,0x645452,0x65000B,0x654321,0x66023C,0x663854,0x664228,0x66424D,0x664C28,0x665D1E,0x673147,0x674846,0x674C47,0x676767,0x682860,0x696969,0x6B4423,0x6C2E1F,0x6C541E,0x6F4E37,0x701C1C,0x702670)
,(0x702963,0x703642,0x704214,0x704241,0x722F37,0x737000,0x757575,0x78184A,0x79443B,0x796878,0x7B1113,0x7B3F00,0x7C0A02,0x7C1C05,0x7C4848,0x7E5E60,0x7F1734,0x800000,0x800020,0x800080,0x801818,0x80461B,0x807532,0x808000,0x808080,0x811453,0x81613C)
,(0x820000,0x826644,0x832A0D,0x836953,0x841B2D,0x843F5B,0x848482,0x850101,0x856D4D,0x85754E,0x860111,0x867E36,0x872657,0x873260,0x880085,0x882D17,0x885818,0x88654E,0x893843,0x893F45,0x8A3324,0x8A496B,0x8A795D,0x8A7F80,0x8B0000,0x8B008B,0x8B0A50)
,(0x8B1A1A,0x8B1C62,0x8B2252,0x8B2323,0x8B2500,0x8B3626,0x8B3A3A,0x8B3A62,0x8B3E2F,0x8B4500,0x8B4513,0x8B4726,0x8B475D,0x8B4789,0x8B4C39,0x8B5742,0x8B5A00,0x8B5A2B,0x8B5f4D,0x8B5F65,0x8B636C,0x8B658B,0x8B668B,0x8B6914,0x8B6969,0x8B7355,0x8B7500)
,(0x8B7765,0x8B795E,0x8B7B8B,0x8B7D6B,0x8B7D7B,0x8B7E66,0x8B814C,0x8B8378,0x8B8386,0x8B8589,0x8B864E,0x8B8682,0x8B8878,0x8B8970,0x8B8989,0x8B8B00,0x8B8B7A,0x8B8B83,0x8D4E85,0x8E3A59,0x8E4585,0x905D5D,0x914E75,0x915C83,0x915F6D,0x918151,0x92000A)
,(0x922724,0x933D41,0x954535,0x960018,0x964B00,0x965A3E,0x967117,0x980036,0x986960,0x987456,0x987654,0x98777B,0x98817B,0x989898,0x990000,0x996515,0x996600,0x996666,0x997A8D,0x9B111E,0x9B7653,0x9B870C,0x9C2542,0x9C7C38,0x9C9C9C,0x9D2933,0x9E1316)
,(0x9E5E6F,0x9F1D35,0x9F2B68,0x9F4576,0x9F8170,0xA0522D,0xA0785A,0xA17A74,0xA2006D,0xA40000,0xA45A52,0xA50B5E,0xA52A2A,0xA55353,0xA57164,0xA63A79,0xA67B5B,0xA6A6A6,0xA75502,0xA81C07,0xA83731,0xA8516E,0xA9203E,0xA95C68,0xA99A86,0xA9A9A9,0xAA381E)
,(0xAA4069,0xAA98A9,0xAB274F,0xAB4B52,0xAB4E52,0xAC1E44,0xACACAC,0xAD4379,0xAD6F69,0xAE0C00,0xAE2029,0xAE98AA,0xAF002A,0xAF4035,0xAF6E4D,0xB03060,0xB05C52,0xB06500,0xB22222,0xB31B1B,0xB3446C,0xB38B6D,0xB48395,0xB53389,0xB5651D,0xB57281,0xB5A642)
,(0xB5B5B5,0xB7410E,0xB768A2,0xB76E79,0xB784A7,0xB78727,0xB86D29,0xB87333,0xB8860B,0xB94E48,0xBA160C,0xBA8759,0xBB3385,0xBB6528,0xBBB477,0xBC8F8F,0xBC987E,0xBCB88A,0xBD33A4,0xBDB76B,0xBE0032,0xBE4F62,0xBEBEBE,0xBF4F51,0xBFAFB2,0xC0362C,0xC04000)
,(0xC08081,0xC09999,0xC0C0C0,0xC154C1,0xC19A6B,0xC21E56,0xC23B22,0xC2B280,0xC30B4E,0xC32148,0xC39953,0xC3B091,0xC40233,0xC41E3A,0xC46210,0xC4AEAD,0xC53151,0xC54B8C,0xC5B358,0xC71585,0xC72C48,0xC74375,0xC80815,0xC84186,0xC8A2C8,0xC8AD7F,0xC90016)
,(0xC95A49,0xC9C0BB,0xCA1F7B,0xCA2C92,0xCB410B,0xCB4154,0xCB6D51,0xCB99C9,0xCBA135,0xCC0000,0xCC0033,0xCC00CC,0xCC3333,0xCC3336,0xCC338B,0xCC33CC,0xCC397B,0xCC474B,0xCC4E5C,0xCC5500,0xCC6666,0xCC7722,0xCC8899,0xCC9900,0xcc9966,0xCC99CC,0xCCA01D)
,(0xCD0000,0xCD00CD,0xCD1076,0xCD2626,0xCD2990,0xCD3278,0xCD3333,0xCD3700,0xCD4F39,0xCD5555,0xCD5700,0xCD5B45,0xCD5C5C,0xCD607E,0xCD6090,0xCD6600,0xCD661D,0xCD6839,0xCD6889,0xCD69C9,0xCD7054,0xCD7F32,0xCD8162,0xCD8500,0xCD853F,0xCD8C95,0xCD919E)
,(0xCD950C,0xCD9575,0xCD96CD,0xCD9B1D,0xCD9B9B,0xCDAA7D,0xCDAD00,0xCDAF95,0xCDB38B,0xCDB5CD,0xCDB79E,0xCDB7B5,0xCDBA96,0xCDBE70,0xCDC0B0,0xCDC1C5,0xCDC5BF,0xCDC673,0xCDC8B1,0xCDC9A5,0xCDC9C9,0xCDCD00,0xCDCDB4,0xCDCDC1,0xCE2029,0xCE4676,0xCF1020)
,(0xCF3476,0xCF6BA9,0xCF71AF,0xCFB53B,0xCFCFC4,0xCFCFCF,0xD02090,0xD0417E,0xD10047,0xD10056,0xD1BEA8,0xD2691E,0xD2B48C,0xD3003F,0xD3212D,0xD39BCB,0xD3D3D3,0xD40000,0xD470A2,0xD473D4,0xD4AF37,0xD65282,0xD68A59,0xD70040,0xD70A53,0xD71868,0xD73B3E)
,(0xD74894,0xD7837F,0xD8B2D1,0xD8BFD8,0xD9004C,0xD92121,0xD9381E,0xD9603B,0xD982B5,0xD98695,0xD99058,0xD998A0,0xDA1D81,0xDA2C43,0xDA3287,0xDA614E,0xDA70D6,0xDA8A67,0xDA9100,0xDAA520,0xDB7093,0xDBD7D2,0xDC143C,0xDCDCDC,0xDDA0DD,0xDDADAF,0xDE3163)
,(0xDE5285,0xDE5D83,0xDE6FA1,0xDEA5A4,0xDEAA88,0xDEB887,0xDF6124,0xE0115F,0xE0218A,0xE03C31,0xE08D3C,0xE12C2C,0xE18E96,0xE1A95F,0xE1AD21,0xE2062C,0xE25098,0xE25822,0xE2725B,0xE30022,0xE30B5D,0xE3256B,0xE32636,0xE34234,0xE3A857,0xE3AB57,0xE3DAC9)
,(0xE40078,0xE4007C,0xE4717A,0xE48400,0xE49B0F,0xE4D00A,0xE4D96F,0xE51A4C,0xE52B50,0xE56024,0xE58E73,0xE5AA70,0xE5B73B,0xE5CCC9,0xE5E4E2,0xE60026,0xE62020,0xE63E62,0xE66771,0xE68FAC,0xE6A8D7,0xE6BE8A,0xE6E200,0xE75480,0xE79FC4,0xE7ACCF,0xE8000D)
,(0xE86100,0xE88E5A,0xE8CCD7,0xE8E8E8,0xE936A7,0xE9692C,0xE97451,0xE9967A,0xE9D66B,0xEA3C53,0xEAA221,0xEAE0C8,0xEB4C42,0xEC3B83,0xEC5800,0xECB176,0xECD540,0xECEBBD,0xED1C24,0xED2939,0xED872D,0xED9121,0xEDC9AF,0xEE0000,0xEE00EE,0xEE1289,0xEE204D)
,(0xEE2C2C,0xEE30A7,0xEE3A8C,0xEE3B3B,0xEE4000,0xEE5C42,0xEE6363,0xEE6A50,0xEE6AA7,0xEE7600,0xEE7621,0xEE7942,0xEE799F,0xEE7AE9,0xEE8262,0xEE82EE,0xEE9572,0xEE9A00,0xEE9A49,0xEEA2AD,0xEEA9B8,0xEEAD0E,0xEEAEEE,0xEEB422,0xEEB4B4,0xEEC591,0xEEC900)
,(0xEECBAD,0xEECFA1,0xEED202,0xEED2EE,0xEED5B7,0xEED5D2,0xEED8AE,0xEEDC82,0xEEDFCC,0xEEE0E5,0xEEE5DE,0xEEE600,0xEEE685,0xEEE8AA,0xEEE8CD,0xEEE9BF,0xEEE9E9,0xEEEE00,0xEEEED1,0xEEEEE0,0xEF3038,0xEF98AA,0xEFBBCC,0xEFCC00,0xEFDECD,0xEFDFBB,0xF07427)
,(0xF08080,0xF0DC82,0xF0E130,0xF0E68C,0xF0EAD6,0xF19CBB,0xF1DDCF,0xF2003C,0xF28500,0xF2BA49,0xF2BDCD,0xF2F0E6,0xF2F27A,0xF37A48,0xF38FA9,0xF3E5AB,0xF400A1,0xF49AC2,0xF4A460,0xF4C2C2,0xF4C430,0xF4CA16,0xF4F0EC,0xF56991,0xF56FA1,0xF58025,0xF5C71A)
,(0xF5DEB3,0xF5E050,0xF5F5DC,0xF5F5F5,0xF64A8A,0xF6ADC6,0xF6EABE,0xF70D1A,0xF75394,0xF77F00,0xF77FBE,0xF78FA7,0xF7BFBE,0xF7E7CE,0xF7E98E,0xF88379,0xF8B878,0xF8D568,0xF8DE7E,0xF9429E,0xF94D00,0xF984E5,0xF984EF,0xFA5B3D,0xFA6E79,0xFA8072,0xFAD6A5)
,(0xFADA5E,0xFADADD,0xFADFAD,0xFAE7B5,0xFAEBD7,0xFAF0BE,0xFAF0E6,0xFAFA37,0xFAFAD2,0xFB4D46,0xFB4F14,0xFB607F,0xFB9902,0xFBA0E3,0xFBAB60,0xFBAED2,0xFBCCE7,0xFBCEB1,0xFBEC5D,0xFC0FC0,0xFC5A8D,0xFC6C85,0xFC89AC,0xFC8EAC,0xFCC200,0xFCE883,0xFCF75E)
,(0xFD0E35,0xFD3A4A,0xFD3F92,0xFD5240,0xFD5800,0xFD5E53,0xFD6C9E,0xFD7C6E,0xFDBCB4,0xFDD5B1,0xFDD9B5,0xFDDDE6,0xFDEE00,0xFDF5E6,0xFDFD96,0xFE2712,0xFE28A2,0xFE4164,0xFE4EDA,0xFE5A1D,0xFE6F5E,0xFEDF00,0xFEFE33,0xFEFEFA,0xFF0000,0xFF0028,0xFF0038)
,(0xFF003F,0xFF004F,0xFF006C,0xFF007C,0xFF007F,0xFF0090,0xFF00FF,0xFF033E,0xFF0800,0xFF1493,0xFF1DCE,0xFF2052,0xFF2400,0xFF2800,0xFF3030,0xFF33CC,0xFF34B3,0xFF355E,0xFF3800,0xFF3855,0xFF3E96,0xFF4040,0xFF404C,0xFF43A4,0xFF4466,0xFF4500,0xFF4681)
,(0xFF496C,0xFF4F00,0xFF5349,0xFF5470,0xFF55A3,0xFF5800,0xFF5A36,0xFF5CCD,0xFF5F00,0xFF6347,0xFF66CC,0xFF6700,0xFF6961,0xFF69B4,0xFF6A6A,0xFF6D3A,0xFF6E4A,0xFF6EB4,0xFF6FFF,0xFF7256,0xFF7518,0xFF77FF,0xFF7800,0xFF7A00,0xFF7E00,0xFF7F00,0xFF7F24)
,(0xFF7F50,0xFF8243,0xFF8247,0xFF82AB,0xFF83FA,0xFF85CF,0xFF878D,0xFF8C00,0xFF8C69,0xFF91A4,0xFF91AF,0xFF9900,0xFF9933,0xFF9966,0xFF9999,0xFF99CC,0xFF9F00,0xFFA000,0xFFA07A,0xFFA089,0xFFA343,0xFFA500,0xFFA54F,0xFFA6C9,0xFFA700,0xFFA812,0xFFAA1D)
,(0xFFAE42,0xFFAEB9,0xFFB077,0xFFB300,0xFFB347,0xFFB3DE,0xFFB5C5,0xFFB6C1,0xFFB7C5,0xFFB90F,0xFFBA00,0xFFBBFF,0xFFBCD9,0xFFBD88,0xFFBF00,0xFFC0CB,0xFFC125,0xFFC1C1,0xFFC1CC,0xFFC40C,0xFFC87C,0xFFCBA4,0xFFCC00,0xFFCC33,0xFFCC99,0xFFCFF1,0xFFD300)
,(0xFFD39B,0xFFD700,0xFFD800,0xFFDAB9,0xFFDAE9,0xFFDB00,0xFFDB58,0xFFDDCA,0xFFDDF4,0xFFDEAD,0xFFDF00,0xFFDF46,0xFFDFBF,0xFFE135,0xFFE1FF,0xFFE302,0xFFE4B5,0xFFE4C4,0xFFE4CD,0xFFE4E1,0xFFE5B4,0xFFE7BA,0xFFEB00,0xFFEBCD,0xFFEC8B,0xFFEF00,0xFFEFD5)
,(0xFFEFDB,0xFFF000,0xFFF0F5,0xFFF44F,0xFFF5EE,0xFFF600,0xFFF68F,0xFFF700,0xFFF8DC,0xFFF8E7,0xFFFACD,0xFFFAF0,0xFFFAFA,0xFFFDD0,0xFFFF00,0xFFFF31,0xFFFF33,0xFFFF66,0xFFFF99,0xFFFFBF,0xFFFFE0,0xFFFFF0,0x1A2421,0x232B2B,0x013220,0x123524,0x1C352D)
,(0x253529,0x3B3C36,0x004040,0x004225,0x004242,0x354230,0x014421,0x18453B,0x004B49,0x444C38,0x1B4D3E,0x1E4D2B,0x2F4F4F,0x4B5320,0x00563F,0x195905,0x465945,0x4A5D23,0x4D5D53,0x555D50,0x355E3B,0x306030,0x006400,0x006600,0x056608,0x006A4E,0x006B3C)
,(0x556B2F,0x00703C,0x177245,0x007474,0x727472,0x00755E,0x087830,0x317873,0x01796F,0x49796B,0x4F7942,0x3B7A57,0x0E7C61,0x507D2A,0x007F5C,0x007F66,0x008000,0x008080,0x2A8000,0x00827F,0x40826D,0x568203,0x738276,0x2F847C,0x738678,0x78866B,0x138808)
,(0x56887D,0x008B00,0x008B45,0x008B8B,0x228B22,0x2E8B57,0x458B00,0x458B74,0x528B8B,0x548B54,0x668B8B,0x698B22,0x698B69,0x6E8B3D,0x7A8B8B,0x838B83,0x838B8B,0x4D8C57,0x5E8C31,0x6B8E23,0x828E84,0x009000,0x059033,0x009150,0x319177,0x4C9141,0x679267)
,(0x299617,0x8F9779,0x009966,0x669999,0x6F9940,0x8A9A5B,0x009B7D,0x009E60,0x009F6B,0x8DA399,0x5DA493,0x00A550,0x00A693,0x39A78E,0x5FA778,0x00A86B,0x00A877,0x87A96B,0x9FA91F,0x00AB66,0x29AB87,0x1CAC78,0x00AD43,0x6EAEA1,0x3AB09E,0x66B032,0x20B2AA)
,(0x34B233,0x3CB371,0x43B3AE,0x3EB489,0x7BB661,0x8DB600,0x9AB973,0x0ABAB5,0x30BA8F,0xA9BA9D,0x4CBB17,0x85BB65,0x71BC78,0x8FBC8F,0xB2BEB5,0x30BFBF,0x48BF91,0xACBF60,0xB0BF1A,0x03C03C,0xA3C1AD,0x9DC209,0x74C365,0x00C4B0,0x93C572,0xA4C639,0x50C878)
,(0x96C8A2,0x46CB18,0x00CC33,0x00CC99,0x00CCCC,0x00CD00,0x00CD66,0x00CDCD,0x32CD32,0x43CD80,0x66CD00,0x66CDAA,0x79CDCD,0x7CCD7C,0x96CDCD,0x9ACD32,0x9BCD9B,0xA2CD5A,0xB4CDCD,0xC1CDC1,0xC1CDCD,0x3CD070,0x48D1CC,0x8FD400,0xA0D6B4,0xA6D608,0x44D7A8)
,(0x88D8C0,0x8DD9CC,0x0BDA51,0xBDDA57,0xC9DC87,0x66DDAA,0x77DD77,0x84DE02,0x96DED1,0xADDFAD,0x40E0D0,0xCAE00D,0xACE1AF,0x9FE2BF,0xD1E231,0xDDE26A,0xA8E4A0,0x8EE53F,0x99E6B3,0xD9E650,0x08E8DE,0x64E986,0xB2EC5D,0x00EE00,0x00EE76,0x00EEEE,0x4EEE94)
,(0x76EE00,0x76EEC6,0x8DEEEE,0x90EE90,0xAFEEEE,0xB3EE3A,0xB4EEB4,0xBCEE68,0xD1EEEE,0xE0EEE0,0xE0EEEE,0xAAF0D1,0xD0F0C0,0xA7F432,0xE8F48C,0xE3F988,0x00FA9A,0x98FB98,0x7CFC00,0xA7FC00,0x9EFD38,0x00FF00,0x00FF7F,0x00FFEF,0x00FFFF,0x39FF14,0x3FFF00)
,(0x4AFF00,0x54FF9F,0x66FF00,0x66FF66,0x7FFF00,0x7FFFD4,0x87FF2A,0x97FFFF,0x98FF98,0x9AFF9A,0xADFF2F,0xB2FFFF,0xBBFFFF,0xBFFF00,0xC0FF3E,0xC1FFC1,0xC9FFE5,0xCAFF70,0xCCFF00,0xCEFF00,0xD0FF14,0xDFFF00,0xE0FFFF,0xE3FF00,0xE9FFDB,0xF0FFF0,0xF0FFFF)
,(0xF5FFFA,0xFDFF00,0xFDFFF5,0x010203,0x010B13,0x1F262A,0x301934,0x210837,0x2a3439,0x353839,0x001C3D,0x1C2841,0x002147,0x264348,0x3B444B,0x414A4C,0x32174D,0x36454F,0x1D2951,0x003153,0x004953,0x563C5C,0x002E63,0x002366,0x003366,0x330066,0x333366)
,(0x00416A,0x4F666A,0x602F6B,0x4A646C,0x4C516D,0x54626F,0x191970,0x536872,0x062A78,0x536878,0x23297A,0x32127A,0x58427C,0x36747D,0x00147E,0x08457E,0x000080,0x126180,0x4E5180,0x522D80,0x6E7F80,0x733380,0x4B0082,0x4C2882,0x6C3082,0x391285,0x002387)
,(0x2E2D88,0x367588,0x512888,0x856088,0x000F89,0x00008B,0x00688B,0x00868B,0x104E8B,0x27408B,0x36648B,0x473C8B,0x483D8B,0x4A708B,0x53868B,0x551A8B,0x5D478B,0x5F8A8B,0x607B8B,0x68228B,0x68838B,0x6C7B8B,0x6E7B8B,0x7A378B,0x0A7E8C,0x86608E,0x00308F)
,(0x708090,0x091F92,0x0F4D92,0x553592,0x006994,0x2E5894,0x002395,0x436B95,0x536895,0x035096,0x734F96,0x777696,0x838996,0x004F98,0x009698,0x003399,0x333399,0x663399,0x666699,0x778899,0x00009C,0x26619C,0x28589C,0x69359C,0x5F9EA0,0x0067A5,0x007AA5)
,(0x778BA5,0x1034A6,0x002FA7,0x007BA7,0x5072A7,0x545AA7,0x0014A8,0x0018A8,0x0038A8,0x5D8AA8,0x6F2DA8,0x7851A9,0x0033AA,0x979AAA,0x0047AB,0x7C98AB,0x8C92AC,0x9A4EAE,0x0093AF,0x8601AF,0x006DB0,0x91A3B0,0x324AB2,0x5946B2,0xAB92B3,0x1164B4,0x4682B4)
,(0x4E82B4,0x4F42B5,0xB39EB5,0x0095B6,0x2243B6,0x6082B6,0x9678B6,0x967BB6,0x9C51B6,0x8BA8B7,0x0070B8,0x007BB8,0x0048BA,0x0D98BA,0x0F52BA,0x5D89BA,0x0072BB,0x1C39BB,0x9955BB,0xAA00BB,0x0087BD,0x1560BD,0x0077BE,0x2A52BE,0x8B72BE,0xB284BE,0x4682BF)
,(0x746CC0,0x72A0C1,0x188BC2,0x73A9C2,0xBFC1C2,0x6D9BC3,0x8878C3,0x9F00C5,0x214FC6,0x1CA9C9,0x779ECB,0x360CCC,0x47ABCC,0x6699CC,0x9932CC,0x9966CC,0x0000CD,0x009ACD,0x00C5CD,0x1874CD,0x21ABCD,0x3A5FCD,0x4F94CD,0x6959CD,0x6A5ACD,0x6CA6CD,0x7AC5CD)
,(0x7D26CD,0x8968CD,0x8DB6CD,0x9A32CD,0x9AC0CD,0x9FB6CD,0xA2B5CD,0xB452CD,0x8806CE,0x0073CF,0x446CCF,0x4BC7CF,0x5A4FCF,0x92A1CF,0xA76BCF,0xAEC6CF,0x0892D0,0x4997D0,0xA2A2D0,0xA2ADD0,0xC4C3D0,0x00CED1,0x1974D2,0x71A6D2,0xB666D2,0x56A0D3,0x9400D3)
,(0xBA55D3,0x1DACD6,0x8CBED6,0x966FD6,0x7C9ED9,0xB19CD9,0x9370DB,0x6050DC,0x6CA0DC,0xB57EDC,0xC9A0DC,0xD6CADD,0x3E8EDE,0xB0C4DE,0xCDA4DE,0x88ACE0,0x4169E1,0x273BE2,0x8A2BE2,0x9BC4E2,0xC4D8E2,0xB80CE3,0xBF94E4,0x5B92E5,0x7ED4E6,0xACACE6,0xADD8E6)
,(0xB0E0E6,0xBCD4E6,0x318CE7,0x45B1E8,0x7CB9E8,0xD19FE8,0x93CCEA,0x00B7EB,0x80DAEB,0x87CEEB,0x9457EB,0x5DADEC,0x24A0ED,0x6495ED,0xA4DDED,0x0000EE,0x00AAEE,0x00B2EE,0x00E5EE,0x1C86EE,0x436EEE,0x5CACEE,0x7A67EE,0x7B68EE,0x7EC0EE,0x8EE5EE,0x912CEE)
,(0x9F79EE,0xA4D3EE,0xACE5EE,0xB23AEE,0xB2DFEE,0xB9D3EE,0xBCD2EE,0xD15FEE,0xABCDEF,0xCEC8EF,0xD891EF,0x1C1CF0,0x89CFF0,0xA020F0,0x8AB9F1,0xA1CAF1,0xDBE9F4,0xF2F3F4,0x4166F5,0x4F86F7,0x87D3F8,0xA4F4F9,0x5218FA,0x87CEFA,0xE6E6FA,0xE6E8FA,0x00B9FB)
,(0x73C2FB,0x74BBFB,0x0FC0FC,0x15F2FD,0xFC74FD,0x0247FE,0x1F75FE,0x77B5FE,0xF1A7FE,0x0000FF,0x0070FF,0x007FFF,0x00BFFF,0x00CCFF,0x00F5FF,0x1E90FF,0x3399FF,0x3F00FF,0x4876FF,0x63B8FF,0x6666FF,0x6F00FF,0x7DF9FF,0x7F00FF,0x836FFF,0x8470FF,0x87CEFF)
,(0x8F00FF,0x98F5FF,0x9B30FF,0x9F00FF,0xA0E6FF,0xA6E7FF,0xAB82FF,0xB0E2FF,0xB9F2FF,0xBF00FF,0xBF3EFF,0xBFEFFF,0xC6E2FF,0xCAE1FF,0xCC00FF,0xCC99FF,0xCCCCFF,0xDCD0FF,0xDF00FF,0xDF73FF,0xE066FF,0xE0B0FF,0xE7FEFF,0xF0F8FF,0xF4BBFF,0xF8F4FF,0xF8F8FF)
                )
        clrs    = list( list(clrs[ir][ic] for ir in range(len(clrs))) for ic in range(len(clrs[0])) )   # Transposition
        w,h     = 21,21

    elif palette_type=='142 colors: 7-hexagon':
        clrs    = (
    (0xeaeaea,0xdddddd,0xc0c0c0,0xb2b2b2,0x969696,0x808080,0x777777,0x5f5f5f,0x4d4d4d,0x333333,0x292929,0x1c1c1c,0x111111,0x080808,0x000000)
,                                                              ()
,                              (0x003366,0x336699,0x3366cc,0x003399,0x000099,0x0000cc,0x000066)
,                           (0x006666,0x006699,0x0099cc,0x0066cc,0x0033cc,0x0000ff,0x3333ff,0x333399)
,                        (0x008080,0x009999,0x33cccc,0x00ccff,0x0099ff,0x0066ff,0x3366ff,0x3333cc,0x666699)
,                     (0x339966,0x00cc99,0x01ffcc,0x01ffff,0x33ccff,0x3399ff,0x6699ff,0x6666ff,0x6600ff,0x6600cc)
,                  (0x339933,0x00cc66,0x01ff99,0x66ffcc,0x66ffff,0x66ccff,0x99ccff,0x9999ff,0x9966ff,0x9933ff,0x9900ff)
,               (0x006600,0x00cc00,0x01ff00,0x66ff99,0x99ffcc,0xccffff,0xccecff,0xccccff,0xcc99ff,0xcc66ff,0xcc00ff,0x9900cc)
,            (0x003300,0x008000,0x33cc33,0x66ff66,0x99ff99,0xccffcc,0xffffff,0xffccff,0xff99ff,0xff66ff,0xff00ff,0xcc00cc,0x660066)
,               (0x336600,0x009900,0x66ff33,0x99ff66,0xccff99,0xffffcc,0xffcccc,0xff99cc,0xff66cc,0xff33cc,0xcc0099,0x800080)
,                  (0x333300,0x669900,0x99ff33,0xccff66,0xffff99,0xffcc99,0xff9999,0xff6699,0xff3399,0xcc3399,0x990099)
,                     (0x666633,0x99cc00,0xccff33,0xffff66,0xffcc66,0xff9966,0xff7c80,0xff0066,0xd60093,0x993366)
,                        (0x808000,0xcccc00,0xffff00,0xffcc00,0xff9933,0xff6600,0xff5050,0xcc0066,0x660033)
,                           (0x996633,0xcc9900,0xff9900,0xcc6600,0xff3300,0xff0000,0xcc0000,0x990033)
,                              (0x663300,0x996600,0xcc3300,0x993300,0x990000,0x800000,0xa50021)
                )
        w,h     = 31,31

    elif palette_type=='216 web-colors: dragon':
        clls    = (
                                                 ((5,5,5),(4,4,4),(3,3,3),(2,2,2),(1,1,1),(0,0,0))
,                                                                          ()
,                                                         ((5,4,0),(5,3,0),(5,2,0),(5,1,0))
,   ((3,4,0),None   ,None   ,None   ,None   ,(4,3,0),(5,4,1),(5,4,2),(5,3,2),(5,2,1),(4,1,0),None   ,None   ,None   ,None   ,(4,0,1))
,   ((4,5,0),(4,5,1),(1,1,0),(2,2,0),(3,3,0),(4,4,0),(5,5,0),(4,3,1),(4,2,1),(1,0,0),(2,1,0),(3,0,0),(4,0,0),(5,0,0),(5,1,2),(5,0,1))
,   ((3,5,0),(4,5,2),(3,4,1),(2,2,1),(3,3,1),(4,4,1),(5,5,1),(3,2,0),(3,1,0),(2,1,1),(3,1,1),(4,1,1),(5,1,1),(4,1,2),(5,2,3),(5,0,2))
,   ((2,5,0),(3,5,2),(2,4,1),(2,3,0),(3,3,2),(4,4,2),(5,5,2),(3,2,1),(2,1,0),(3,2,2),(4,2,2),(5,2,2),(3,0,1),(4,1,3),(5,2,4),(5,0,3))
,   ((1,5,0),(2,5,1),(1,3,0),(2,4,0),(3,5,1),(4,4,3),(5,5,3),(4,3,2),(4,2,0),(4,3,3),(5,3,3),(5,1,3),(4,0,2),(3,0,2),(5,1,4),(5,0,4))
,   ((0,4,0),(1,4,0),(1,2,0),(2,3,1),(3,4,2),(4,5,3),(5,5,4),(5,4,3),(5,3,1),(5,4,4),(5,3,4),(4,2,3),(3,1,2),(2,0,1),(4,0,3),(1,0,1))
,   ((1,4,1),(2,4,2),(0,5,0),(1,5,1),(2,5,2),(3,5,3),(4,5,4),None   ,None   ,None   ,(4,3,4),(3,2,3),(3,1,3),(3,0,3),(2,1,2),(2,0,2))
,   ((0,2,0),(1,2,1),(4,3,0),(1,3,1),(2,3,2),(3,4,3),None   ,None   ,None   ,(5,4,5),(5,3,5),(5,2,5),(5,1,5),(5,0,5),(4,2,4),(4,1,4))
,   ((0,1,0),(0,4,1),(0,2,1),(1,3,2),(2,4,3),(3,5,4),(4,5,5),(1,3,5),(3,4,5),(4,4,5),(4,3,5),(3,2,4),(2,1,3),(1,0,2),(3,0,4),(4,0,4))
,   ((0,5,1),(1,5,2),(0,3,1),(0,4,2),(1,5,3),(3,5,5),(3,4,4),(0,2,4),(2,3,4),(3,3,5),(3,3,4),(3,1,5),(2,0,4),(2,0,3),(4,1,5),(4,0,5))
,   ((0,5,2),(2,5,3),(1,4,2),(0,3,2),(2,5,5),(2,4,4),(2,3,3),(0,1,2),(1,2,3),(2,2,5),(2,2,4),(2,2,3),(1,0,3),(3,1,4),(4,2,5),(3,0,5))
,   ((0,5,3),(2,5,4),(1,4,3),(1,5,5),(1,4,4),(1,3,3),(1,2,2),(0,2,3),(0,1,3),(1,1,5),(1,1,4),(1,1,3),(1,1,2),(2,1,4),(0,2,5),(2,0,5))
,   ((0,5,4),(1,5,4),(0,5,5),(0,4,4),(0,3,3),(0,2,2),(0,1,1),(1,3,4),(1,2,4),(0,0,5),(0,0,4),(0,0,3),(0,0,2),(0,0,1),(2,1,5),(1,0,5))
,   ((0,4,3),None   ,None   ,None   ,None   ,(0,3,4),(1,4,5),(2,4,5),(2,3,5),(1,2,5),(0,1,4),None   ,None   ,None   ,None   ,(1,0,4))
,                                                         ((0,4,5),(0,3,5),(0,2,5),(0,1,5))
                )
        clrs    = list(list(( R1*cll[0]|G1*cll[1]|B1*cll[2] if cll else None) for cll in clls_row) for clls_row in clls)
        w,h     = 27,27

    elif palette_type=='216 web-colors: 9-hexagon':
        clls    = (
                                                 ((5,5,5),(4,4,4),(3,3,3),(2,2,2),(1,1,1),(0,0,0))
,                                                                          ()
,                                            ((3,5,0),(4,5,2),(4,5,1),(3,4,0),(4,3,0),(5,4,1),(5,4,0),(5,3,0),(5,2,0))
,                                       ((2,5,0),(3,5,1),(4,5,2),(3,4,1),(2,3,0),(3,2,0),(4,3,1),(5,4,2),(5,3,1),(5,1,0))
,                                  ((1,5,0),(3,5,2),(2,4,0),(1,1,0),(2,2,0),(3,3,0),(4,4,0),(5,5,0),(4,2,0),(5,3,2),(5,2,1))
,                             ((2,5,1),(2,4,1),(0,5,0),(1,2,0),(2,2,1),(3,3,1),(4,4,1),(5,5,1),(2,1,0),(1,0,0),(4,2,1),(4,1,0))
,                        ((1,4,0),(1,3,0),(0,4,0),(1,5,1),(2,3,1),(3,3,2),(4,4,2),(5,5,2),(3,2,1),(2,1,1),(2,0,0),(3,1,0),(4,0,1))
,                   ((0,4,1),(0,3,1),(0,3,0),(1,4,1),(2,5,2),(3,4,2),(4,4,3),(5,5,3),(4,3,2),(3,2,2),(3,1,1),(3,0,0),(3,0,1),(5,1,2))
,                 ((1,5,2),(1,4,2),(0,2,0),(1,3,1),(2,4,2),(3,5,3),(4,5,3),(5,5,4),(5,4,3),(4,3,3),(4,2,2),(4,1,1),(4,0,0),(4,1,2),(5,0,1))
,            ((0,5,1),(2,5,3),(0,1,0),(1,2,1),(2,3,2),(3,4,3),(4,5,4),None   ,None   ,(5,4,4),(5,3,3),(5,2,2),(5,1,1),(5,0,0),(5,2,3),(5,0,2))
,       ((0,5,2),(1,5,3),(0,4,2),(0,2,1),(1,3,2),(2,4,3),(3,5,4),None   ,None   ,None   ,(5,3,4),(4,2,3),(3,1,2),(2,0,1),(4,0,2),(5,1,3),(5,0,3))
,            ((0,5,3),(2,5,4),(0,5,5),(1,5,5),(2,5,5),(3,5,5),(4,5,5),None   ,None   ,(5,4,5),(4,3,4),(3,2,3),(2,1,2),(1,0,1),(5,2,4),(5,0,4))
,                 ((0,5,4),(1,4,3),(0,4,4),(1,4,4),(2,4,4),(3,4,4),(3,4,5),(4,4,5),(4,3,5),(5,3,5),(4,2,4),(3,1,3),(2,0,2),(4,1,3),(5,1,4))
,                      ((1,5,4),(0,3,2),(0,3,3),(1,3,3),(2,3,3),(2,3,4),(3,3,5),(3,3,4),(3,2,4),(5,2,5),(4,1,4),(3,0,3),(3,0,2),(4,0,3))
,                           ((0,4,3),(0,2,3),(0,2,2),(1,2,2),(1,2,3),(2,2,5),(2,2,4),(0,0,3),(2,1,3),(5,1,5),(4,0,4),(2,0,3),(3,0,4))
,                                ((0,3,4),(1,3,4),(0,1,1),(0,1,2),(1,1,5),(1,1,4),(1,1,3),(1,1,2),(1,0,2),(5,0,5),(3,1,4),(4,1,5))
,                                     ((1,4,5),(2,4,5),(0,2,4),(0,0,5),(0,0,4),(0,0,3),(0,0,2),(0,0,1),(2,0,4),(4,2,5),(4,0,5))
,                                          ((0,4,5),(1,3,5),(2,3,5),(1,2,4),(0,1,3),(1,0,3),(2,1,4),(3,2,5),(3,1,5),(3,0,5))
,                                               ((0,3,5),(0,2,5),(0,1,5),(1,2,5),(0,1,4),(1,0,4),(2,1,5),(1,0,5),(2,0,5))
                )
        clrs    = list(list(( R1*cll[0]|G1*cll[1]|B1*cll[2] if cll else None) for cll in clls_row) for clls_row in clls)
        w,h     = 27,27

    elif palette_type in ('216web:4*v-candle', '216 web-colors: candles'):
        clls    = ((
),(None         ,(0+0,0+0,0+0),None         ,None,None         ,(5-0,0+0,0+0),None         ,None,None         ,(0+0,5-0,0+0),None         ,None,None         ,(0+0,0+0,5-0),None         ,
),((0+1,0+1,0+0),(0+1,0+0,0+1),(0+0,0+1,0+1),None,(5-1,0+0,0+0),(5-0,0+1,0+0),(5-0,0+0,0+1),None,(0+1,5-1,0+0),(0+1,5-0,0+1),(0+0,5-1,0+1),None,(0+1,0+1,5-0),(0+1,0+0,5-1),(0+0,0+1,5-1),
),((0+1,0+0,0+0),(0+0,0+1,0+0),(0+0,0+0,0+1),None,(5-1,0+1,0+0),(5-1,0+0,0+1),(5-0,0+1,0+1),None,(0+1,5-0,0+0),(0+0,5-1,0+0),(0+0,5-0,0+1),None,(0+1,0+0,5-0),(0+0,0+1,5-0),(0+0,0+0,5-1),
),((0+2,0+0,0+0),(0+0,0+2,0+0),(0+0,0+0,0+2),None,(5-2,0+0,0+0),(5-0,0+2,0+0),(5-0,0+0,0+2),None,(0+2,5-0,0+0),(0+0,5-2,0+0),(0+0,5-0,0+2),None,(0+2,0+0,5-0),(0+0,0+2,5-0),(0+0,0+0,5-2),
),((0+2,0+0,0+1),(0+0,0+2,0+1),(0+1,0+0,0+2),None,(5-2,0+0,0+1),(5-0,0+2,0+1),(5-1,0+0,0+2),None,(0+2,5-0,0+1),(0+0,5-2,0+1),(0+1,5-0,0+2),None,(0+2,0+0,5-1),(0+0,0+2,5-1),(0+1,0+0,5-2),
),(None         ,(0+1,0+1,0+1),None         ,None,None         ,(5-1,0+1,0+1),None         ,None,None         ,(0+1,5-1,0+1),None         ,None,None         ,(0+1,0+1,5-1),None         ,
),((0+2,0+1,0+0),(0+1,0+2,0+0),(0+0,0+1,0+2),None,(5-2,0+1,0+0),(5-1,0+2,0+0),(5-0,0+1,0+2),None,(0+2,5-1,0+0),(0+1,5-2,0+0),(0+0,5-1,0+2),None,(0+2,0+1,5-0),(0+1,0+2,5-0),(0+0,0+1,5-2),
),((0+2,0+1,0+1),(0+1,0+2,0+1),(0+1,0+1,0+2),None,(5-2,0+1,0+1),(5-1,0+2,0+1),(5-1,0+1,0+2),None,(0+2,5-1,0+1),(0+1,5-2,0+1),(0+1,5-1,0+2),None,(0+2,0+1,5-1),(0+1,0+2,5-1),(0+1,0+1,5-2),
),((0+2,0+2,0+0),(0+2,0+0,0+2),(0+0,0+2,0+2),None,(5-2,0+2,0+0),(5-2,0+0,0+2),(5-0,0+2,0+2),None,(0+2,5-2,0+0),(0+2,5-0,0+2),(0+0,5-2,0+2),None,(0+2,0+2,5-0),(0+2,0+0,5-2),(0+0,0+2,5-2),
),((0+2,0+2,0+1),(0+2,0+1,0+2),(0+1,0+2,0+2),None,(5-2,0+2,0+1),(5-2,0+1,0+2),(5-1,0+2,0+2),None,(0+2,5-2,0+1),(0+2,5-1,0+2),(0+1,5-2,0+2),None,(0+2,0+2,5-1),(0+2,0+1,5-2),(0+1,0+2,5-2),
),(None         ,(0+2,0+2,0+2),None         ,None,None         ,(5-2,0+2,0+2),None         ,None,None         ,(0+2,5-2,0+2),None         ,None,None         ,(0+2,0+2,5-2),None         ,

),(None         ,(5-2,5-2,5-2),None         ,None,None         ,(0+2,5-2,5-2),None         ,None,None         ,(5-2,0+2,5-2),None         ,None,None         ,(5-2,5-2,0+2),None         ,
),((5-2,5-2,5-1),(5-2,5-1,5-2),(5-1,5-2,5-2),None,(0+2,5-2,5-1),(0+2,5-1,5-2),(0+1,5-2,5-2),None,(5-2,0+2,5-1),(5-2,0+1,5-2),(5-1,0+2,5-2),None,(5-2,5-2,0+1),(5-2,5-1,0+2),(5-1,5-2,0+2),
),((5-2,5-2,5-0),(5-2,5-0,5-2),(5-0,5-2,5-2),None,(0+2,5-2,5-0),(0+2,5-0,5-2),(0+0,5-2,5-2),None,(5-2,0+2,5-0),(5-2,0+0,5-2),(5-0,0+2,5-2),None,(5-2,5-2,0+0),(5-2,5-0,0+2),(5-0,5-2,0+2),
),((5-2,5-1,5-1),(5-1,5-2,5-1),(5-1,5-1,5-2),None,(0+2,5-1,5-1),(0+1,5-2,5-1),(0+1,5-1,5-2),None,(5-2,0+1,5-1),(5-1,0+2,5-1),(5-1,0+1,5-2),None,(5-2,5-1,0+1),(5-1,5-2,0+1),(5-1,5-1,0+2),
),((5-2,5-1,5-0),(5-1,5-2,5-0),(5-0,5-1,5-2),None,(0+2,5-1,5-0),(0+1,5-2,5-0),(0+0,5-1,5-2),None,(5-2,0+1,5-0),(5-1,0+2,5-0),(5-0,0+1,5-2),None,(5-2,5-1,0+0),(5-1,5-2,0+0),(5-0,5-1,0+2),
),(None         ,(5-1,5-1,5-1),None         ,None,None         ,(0+1,5-1,5-1),None         ,None,None         ,(5-1,0+1,5-1),None         ,None,None         ,(5-1,5-1,0+1),None         ,
),((5-2,5-0,5-1),(5-0,5-2,5-1),(5-1,5-0,5-2),None,(0+2,5-0,5-1),(0+0,5-2,5-1),(0+1,5-0,5-2),None,(5-2,0+0,5-1),(5-0,0+2,5-1),(5-1,0+0,5-2),None,(5-2,5-0,0+1),(5-0,5-2,0+1),(5-1,5-0,0+2),
),((5-2,5-0,5-0),(5-0,5-2,5-0),(5-0,5-0,5-2),None,(0+2,5-0,5-0),(0+0,5-2,5-0),(0+0,5-0,5-2),None,(5-2,0+0,5-0),(5-0,0+2,5-0),(5-0,0+0,5-2),None,(5-2,5-0,0+0),(5-0,5-2,0+0),(5-0,5-0,0+2),
),((5-1,5-1,5-0),(5-1,5-0,5-1),(5-0,5-1,5-1),None,(0+1,5-0,5-0),(0+0,5-1,5-0),(0+0,5-0,5-1),None,(5-1,0+0,5-0),(5-0,0+1,5-0),(5-0,0+0,5-1),None,(5-1,5-1,0+0),(5-1,5-0,0+1),(5-0,5-1,0+1),
),((5-1,5-0,5-0),(5-0,5-1,5-0),(5-0,5-0,5-1),None,(0+1,5-1,5-0),(0+1,5-0,5-1),(0+0,5-1,5-1),None,(5-1,0+1,5-0),(5-1,0+0,5-1),(5-0,0+1,5-1),None,(5-1,5-0,0+0),(5-0,5-1,0+0),(5-0,5-0,0+1),
),(None         ,(5-0,5-0,5-0),None         ,None,None         ,(0+0,5-0,5-0),None         ,None,None         ,(5-0,0+0,5-0),None         ,None,None         ,(5-0,5-0,0+0),None         ,
)                )[1:]
        if palette_type=='216 web-colors: candles':
            clls    = list( list(clls[ir][ic] for ir in range(len(clls))) for ic in range(len(clls[0])) )   # Transposition
        clrs    = list(list(( R1*cll[0]|G1*cll[1]|B1*cll[2] if cll else None) for cll in clls_row) for clls_row in clls)
        w,h     = 27,27

#   elif palette_type=='???:hsv':
#       inversed= False
#       clls    = ((
#),((200,255,255),(190,255,255),(180,255,255),(170,255,255),(160,255,255),(150,255,255),(140,255,255),(130,255,255),(120,255,255),(110,255,255),(100,255,255),( 90,255,255),( 80,255,255),( 70,255,255),( 60,255,255),( 50,255,255),( 40,255,255),( 30,255,255),( 20,255,255),( 10,255,255),
#),((200,255,255),(190,255,255),(180,255,255),(170,255,255),(160,255,255),(150,255,255),(140,255,255),(130,255,255),(120,255,255),(110,255,255),(100,255,255),( 90,255,255),( 80,255,255),( 70,255,255),( 60,255,255),( 50,255,255),( 40,255,255),( 30,255,255),( 20,255,255),( 10,255,255),
#)                )[1:]
#       clrs    = list(list(( rgb01_to_int(*colorsys.hsv_to_rgb(cll[0]/255,cll[1]/255,cll[2]/255)) if cll else None) for cll in clls_row) for clls_row in clls)
##       clrs    = list(list(( rgb(*           hsv_to_rgb(cll[0]/255,cll[1]/255,cll[2]/255)) if cll else None) for cll in clls_row) for clls_row in clls)
#       w,h     = 27,27
#       pass;                   LOG and log('clrs[0][0]={}',(clrs[0][0]))
    
#   elif palette_type=='343: 7*7*7':
#       # 343 = 8*27 
#       #     + 127
#       pass

#   elif palette_type=='216web:8*27rand':
#       # 0123456789abcdef
#       # 0  3  6  9  c  f
#       bs1     = list(i*R1 for i in range(6))
#       bs2     = list(i*G1 for i in range(6))
#       bs3     = list(i*B1 for i in range(6))
##       import itertools
##       cube_3  = itertools.product(bs1, bs2, bs3) 
#       cube_cls   = (
#                                                   (0,0,0)
#                                                   
#                                          ,(1,0,0)        ,(0,0,1)
#                                                  ,(0,1,0)
#                                                  
#                                  ,(2,0,0)        ,(1,0,1)        ,(0,0,2)
#                                          ,(1,1,0)        ,(0,1,1)
#                                                  ,(0,2,0)
#                                                  
#                          ,(3,0,0)        ,(2,0,1)        ,(1,0,2)        ,(0,0,3)
#                                  ,(2,1,0)        ,(1,1,1)        ,(0,2,1)
#                                          ,(1,2,0)        ,(0,1,2)    
#                                                  ,(0,3,0)
#                                                  
#                  ,(4,0,0)        ,(3,0,1)        ,(2,0,2)        ,(1,0,3)        ,(0,0,4)
#                          ,(3,1,0)        ,(2,1,1)        ,(1,1,2)        ,(0,1,3)
#                                  ,(2,2,0)        ,(1,2,1)        ,(0,2,2)
#                                          ,(1,3,0)        ,(0,3,1)
#                                                  ,(0,4,0)
#
#          ,(5,0,0)        ,(4,0,1)        ,(3,0,2)        ,(2,0,3)        ,(1,0,4)        ,(0,0,5)
#                  ,(4,1,0)        ,(3,1,1)        ,(2,1,2)        ,(1,1,3)        ,(0,1,4)
#                          ,(3,2,0)        ,(2,2,1)        ,(1,2,2)        ,(0,2,3)
#                                  ,(2,3,0)        ,(1,3,1)        ,(0,3,2)
#                                          ,(1,4,0)        ,(0,4,1)
#                                                  ,(0,5,0)
#
#                  ,(5,0,1)        ,(4,0,2)        ,(3,0,3)        ,(2,0,4)        ,(1,0,5)
#          ,(5,1,0)        ,(4,1,1)        ,(3,1,2)        ,(2,1,3)        ,(1,1,4)        ,(0,1,5)
#                  ,(4,2,0)        ,(3,2,1)        ,(2,2,2)        ,(1,2,3)        ,(0,2,4)
#                          ,(3,3,0)        ,(2,3,1)        ,(1,3,2)        ,(0,3,3)
#                                  ,(2,4,0)        ,(1,4,1)        ,(0,4,2)
#                                          ,(1,5,0)        ,(0,5,1)
#                   
#                          ,(5,0,2)        ,(4,0,3)        ,(3,0,4)        ,(2,0,5)
#                  ,(5,1,1)        ,(4,1,2)        ,(3,1,3)        ,(2,1,4)        ,(1,1,5)
#          ,(5,2,0)        ,(4,2,1)        ,(3,2,2)        ,(2,2,3)        ,(1,2,4)        ,(0,2,5)
#                  ,(4,3,0)        ,(3,3,1)        ,(2,3,2)        ,(1,3,3)        ,(0,3,4)
#                          ,(3,4,0)        ,(2,4,1)        ,(1,4,2)        ,(0,4,3)
#                                  ,(2,5,0)        ,(1,5,1)        ,(0,5,2)
#                   
#
#                                  ,(5,0,3)        ,(4,0,4)        ,(3,0,5)
#                          ,(5,1,2)        ,(4,1,3)        ,(3,1,4)        ,(2,1,5)
#                  ,(5,2,1)        ,(4,2,2)        ,(3,2,3)        ,(2,2,4)        ,(1,2,5)
#          ,(5,3,0)        ,(4,3,1)        ,(3,3,2)        ,(2,3,3)        ,(1,3,4)        ,(0,3,5)
#                  ,(4,4,0)        ,(3,4,1)        ,(2,4,2)        ,(1,4,3)        ,(0,4,4)
#                          ,(3,5,0)        ,(2,5,1)        ,(1,5,2)        ,(0,5,3)
#
#                                          ,(5,0,4)        ,(4,0,5)
#                                 ,(5,1,3)         ,(4,1,4)        ,(3,1,5)
#                          ,(5,2,2)        ,(4,2,3)        ,(3,2,4)        ,(2,2,5)
#                  ,(5,3,1)        ,(4,3,2)        ,(3,3,3)        ,(2,3,4)        ,(1,3,5)
#          ,(5,4,0)        ,(4,4,1)        ,(3,4,2)        ,(2,4,3)        ,(1,4,4)        ,(0,4,5)
#                  ,(4,5,0)        ,(3,5,1)        ,(2,5,2)        ,(1,5,3)        ,(0,5,4)
#
#                                                  ,(5,0,5)
#                                          ,(5,1,4)        ,(4,1,5)
#                                  ,(5,2,3)        ,(4,2,4)        ,(3,2,5)
#                          ,(5,3,2)        ,(4,3,3)        ,(3,3,4)        ,(2,3,5)
#                  ,(5,4,1)        ,(4,4,2)        ,(3,4,3)        ,(2,4,4)        ,(1,4,5)
#          ,(5,5,0)        ,(4,5,1)        ,(3,5,2)        ,(2,5,3)        ,(1,5,4)        ,(0,5,5)                
#
#                                                  ,(5,1,5)
#                                          ,(5,2,4)        ,(4,2,5)
#                                  ,(5,3,3)        ,(4,3,4)        ,(3,3,5)
#                          ,(5,4,2)        ,(4,4,3)        ,(3,4,4)        ,(2,4,5)
#                  ,(5,5,1)        ,(4,5,2)        ,(3,5,3)        ,(2,5,4)        ,(1,5,5)
#                  
#                                                  ,(5,2,5)
#                                          ,(5,3,4)        ,(4,3,5)
#                                  ,(5,4,3)        ,(4,4,4)        ,(3,4,5)
#                          ,(5,5,2)        ,(4,5,3)        ,(3,5,4)        ,(2,5,5)
#                                                  
#                                                  ,(5,3,5)
#                                          ,(5,4,4)        ,(4,4,5)
#                                  ,(5,5,3)        ,(4,5,4)        ,(3,5,5)
#                                  
#                                                  ,(5,4,5)
#                                          ,(5,5,4)        ,(4,5,5)
#                                                  
#                                                  ,(5,5,5)
#                   )
#       pass;                  #cube_cls_a   = ((i,j,k) for i in range(6) for j in range(6) for k in range(6))
#       pass;                  #diff    = list(cl for cl in cube_cls_a if cl not in cube_cls)
#       pass;                  #LOG and log('diff={}',(diff))
#       pass;                  #return
#       cube_3  = ((bs1[cl[0]], bs2[cl[1]], bs3[cl[2]]) for cl in cube_cls) 
#       cube    = list(c[0]|c[1]|c[2] for c in cube_3)
#       pass;                  #LOG and log('cube={}',(cube))
#       clrs    = []
#       for r in range(12):
#           clrs+= [cube[r*18:r*18+18]]
##       for r in range(8):
##           clrs+= [cube[r*27:r*27+27]]
#       pass;                  #LOG and log('clrs={}',(clrs))
#       pass;                  #return
##       clrs    = (cb for i, c in enumerate(cube))
#       w,h     = 25,25
                          #LOG and log('clrs={}',(clrs))
    if inversed:
        clrs    = invert_HML(clrs)

    return clrs,w,h, sp_clrs,sp_w,sp_h
   #def _dlg_color_palette_clrs

COLOR_NAMES[clr_h2i('#000000')]='Black'
COLOR_NAMES[clr_h2i('#000080')]='Navy'
COLOR_NAMES[clr_h2i('#00008B')]='Dark blue'
COLOR_NAMES[clr_h2i('#00009C')]='Duke blue'
COLOR_NAMES[clr_h2i('#0000CD')]='Medium blue'
COLOR_NAMES[clr_h2i('#0000EE')]='Blue'
COLOR_NAMES[clr_h2i('#0000FF')]='Blue'
COLOR_NAMES[clr_h2i('#000F89')]='Phthalo blue'
COLOR_NAMES[clr_h2i('#00147E')]='Dark imperial blue'
COLOR_NAMES[clr_h2i('#0014A8')]='Zaffre'
COLOR_NAMES[clr_h2i('#0018A8')]='Blue'
COLOR_NAMES[clr_h2i('#001C3D')]='Maastricht Blue'
COLOR_NAMES[clr_h2i('#002147')]='Oxford Blue'
COLOR_NAMES[clr_h2i('#002366')]='Royal blue'
COLOR_NAMES[clr_h2i('#002387')]='Resolution blue'
COLOR_NAMES[clr_h2i('#002395')]='Imperial blue'
COLOR_NAMES[clr_h2i('#002E63')]='Cool Black'
COLOR_NAMES[clr_h2i('#002FA7')]='International Klein Blue'
COLOR_NAMES[clr_h2i('#00308F')]='Air Force blue'
COLOR_NAMES[clr_h2i('#003153')]='Prussian blue'
COLOR_NAMES[clr_h2i('#003366')]='Dark midnight blue'
COLOR_NAMES[clr_h2i('#003399')]='Smalt, Dark powder blue'
COLOR_NAMES[clr_h2i('#0033AA')]='UA blue'
COLOR_NAMES[clr_h2i('#0038A8')]='Royal azure'
COLOR_NAMES[clr_h2i('#004040')]='Rich black'
COLOR_NAMES[clr_h2i('#00416A')]='Dark imperial blue'
COLOR_NAMES[clr_h2i('#004225')]='British racing green'
COLOR_NAMES[clr_h2i('#004242')]='Warm black'
COLOR_NAMES[clr_h2i('#0047AB')]='Cobalt Blue'
COLOR_NAMES[clr_h2i('#0048BA')]='Absolute zero'
COLOR_NAMES[clr_h2i('#004953')]='Midnight green, Eagle green'
COLOR_NAMES[clr_h2i('#004B49')]='Deep jungle green'
COLOR_NAMES[clr_h2i('#004F98')]='USAFA blue'
COLOR_NAMES[clr_h2i('#00563F')]='Sacramento State green'
COLOR_NAMES[clr_h2i('#006400')]='Dark green'
COLOR_NAMES[clr_h2i('#006600')]='Pakistan green'
COLOR_NAMES[clr_h2i('#0067A5')]='Sapphire blue'
COLOR_NAMES[clr_h2i('#00688B')]='Deep sky blue'
COLOR_NAMES[clr_h2i('#006994')]='Sea blue'
COLOR_NAMES[clr_h2i('#006A4E')]='Bottle green'
COLOR_NAMES[clr_h2i('#006B3C')]='Cadmium green'
COLOR_NAMES[clr_h2i('#006DB0')]='Honolulu blue'
COLOR_NAMES[clr_h2i('#00703C')]='Dartmouth green'
COLOR_NAMES[clr_h2i('#0070B8')]='Spanish blue'
COLOR_NAMES[clr_h2i('#0070FF')]='Brandeis blue'
COLOR_NAMES[clr_h2i('#0072BB')]='French blue'
COLOR_NAMES[clr_h2i('#0073CF')]='True Blue'
COLOR_NAMES[clr_h2i('#007474')]='Skobeloff'
COLOR_NAMES[clr_h2i('#00755E')]='Tropical rain forest'
COLOR_NAMES[clr_h2i('#0077BE')]='Ocean Boat Blue'
COLOR_NAMES[clr_h2i('#007AA5')]='CG Blue'
COLOR_NAMES[clr_h2i('#007BA7')]='Celadon blue'
COLOR_NAMES[clr_h2i('#007BB8')]='Star command blue'
COLOR_NAMES[clr_h2i('#007F5C')]='Spanish viridian'
COLOR_NAMES[clr_h2i('#007F66')]='Generic viridian'
COLOR_NAMES[clr_h2i('#007FFF')]='Azure'
COLOR_NAMES[clr_h2i('#008000')]='Green'
COLOR_NAMES[clr_h2i('#008080')]='Teal'
COLOR_NAMES[clr_h2i('#00827F')]='Teal green'
COLOR_NAMES[clr_h2i('#00868B')]='Turquoise'
COLOR_NAMES[clr_h2i('#0087BD')]='Blue'
COLOR_NAMES[clr_h2i('#008B00')]='Green'
COLOR_NAMES[clr_h2i('#008B45')]='Spring green'
COLOR_NAMES[clr_h2i('#008B8B')]='Dark cyan'
COLOR_NAMES[clr_h2i('#009000')]='Islamic green'
COLOR_NAMES[clr_h2i('#009150')]='Spanish green'
COLOR_NAMES[clr_h2i('#0093AF')]='Blue'
COLOR_NAMES[clr_h2i('#0095B6')]='Bondi blue'
COLOR_NAMES[clr_h2i('#009698')]='Viridian green'
COLOR_NAMES[clr_h2i('#009966')]='Green-cyan'
COLOR_NAMES[clr_h2i('#009ACD')]='Deep sky blue'
COLOR_NAMES[clr_h2i('#009B7D')]='Paolo Veronese green'
COLOR_NAMES[clr_h2i('#009E60')]='Shamrock green'
COLOR_NAMES[clr_h2i('#009F6B')]='Green'
COLOR_NAMES[clr_h2i('#00A550')]='Green'
COLOR_NAMES[clr_h2i('#00A693')]='Persian green'
COLOR_NAMES[clr_h2i('#00A86B')]='Jade'
COLOR_NAMES[clr_h2i('#00A877')]='Green'
COLOR_NAMES[clr_h2i('#00AAEE')]='Vivid cerulean'
COLOR_NAMES[clr_h2i('#00AB66')]='GO green'
COLOR_NAMES[clr_h2i('#00AD43')]='Green'
COLOR_NAMES[clr_h2i('#00B2EE')]='Deep sky blue'
COLOR_NAMES[clr_h2i('#00B7EB')]='Cyan'
COLOR_NAMES[clr_h2i('#00B9FB')]='Blue Bolt'
COLOR_NAMES[clr_h2i('#00BFFF')]='Deep sky blue'
COLOR_NAMES[clr_h2i('#00C4B0')]='Amazonite'
COLOR_NAMES[clr_h2i('#00C5CD')]='Turquoise'
COLOR_NAMES[clr_h2i('#00CC33')]='Vivid malachite'
COLOR_NAMES[clr_h2i('#00CC99')]='Caribbean green'
COLOR_NAMES[clr_h2i('#00CCCC')]='Robin egg blue'
COLOR_NAMES[clr_h2i('#00CCFF')]='Vivid sky blue'
COLOR_NAMES[clr_h2i('#00CD00')]='Green'
COLOR_NAMES[clr_h2i('#00CD66')]='Spring green'
COLOR_NAMES[clr_h2i('#00CDCD')]='Cyan'
COLOR_NAMES[clr_h2i('#00CED1')]='Dark turquoise'
COLOR_NAMES[clr_h2i('#00E5EE')]='Turquoise'
COLOR_NAMES[clr_h2i('#00EE00')]='Green'
COLOR_NAMES[clr_h2i('#00EE76')]='Spring green'
COLOR_NAMES[clr_h2i('#00EEEE')]='Cyan'
COLOR_NAMES[clr_h2i('#00F5FF')]='Turquoise'
COLOR_NAMES[clr_h2i('#00FA9A')]='Medium spring green'
COLOR_NAMES[clr_h2i('#00FF00')]='Lime Green '
COLOR_NAMES[clr_h2i('#00FF7F')]='Spring green'
COLOR_NAMES[clr_h2i('#00FFEF')]='Turquoise blue'
COLOR_NAMES[clr_h2i('#00FFFF')]='Cyan, Spanish sky blue'
COLOR_NAMES[clr_h2i('#010203')]='Rich black'
COLOR_NAMES[clr_h2i('#010B13')]='Rich black'
COLOR_NAMES[clr_h2i('#013220')]='Dark green'
COLOR_NAMES[clr_h2i('#014421')]='Forest green (traditional)'
COLOR_NAMES[clr_h2i('#01796F')]='Pine green'
COLOR_NAMES[clr_h2i('#0247FE')]='Blue'
COLOR_NAMES[clr_h2i('#035096')]='Medium electric blue'
COLOR_NAMES[clr_h2i('#03C03C')]='Dark pastel green'
COLOR_NAMES[clr_h2i('#056608')]='Deep green'
COLOR_NAMES[clr_h2i('#059033')]='North Texas Green'
COLOR_NAMES[clr_h2i('#062A78')]='Catalina blue'
COLOR_NAMES[clr_h2i('#08457E')]='Dark cerulean'
COLOR_NAMES[clr_h2i('#087830')]='La Salle Green'
COLOR_NAMES[clr_h2i('#0892D0')]='Rich electric blue'
COLOR_NAMES[clr_h2i('#08E8DE')]='Bright turquoise'
COLOR_NAMES[clr_h2i('#091F92')]='Indigo dye'
COLOR_NAMES[clr_h2i('#0A7E8C')]='Metallic Seaweed'
COLOR_NAMES[clr_h2i('#0ABAB5')]='Tiffany Blue'
COLOR_NAMES[clr_h2i('#0BDA51')]='Malachite'
COLOR_NAMES[clr_h2i('#0D98BA')]='Blue-green'
COLOR_NAMES[clr_h2i('#0E7C61')]='Deep green-cyan turquoise'
COLOR_NAMES[clr_h2i('#0F4D92')]='Yale Blue'
COLOR_NAMES[clr_h2i('#0F52BA')]='Sapphire'
COLOR_NAMES[clr_h2i('#0FC0FC')]='Spiro Disco Ball'
COLOR_NAMES[clr_h2i('#100C08')]='Smoky black'
COLOR_NAMES[clr_h2i('#1034A6')]='Egyptian blue'
COLOR_NAMES[clr_h2i('#104E8B')]='Dodger blue'
COLOR_NAMES[clr_h2i('#1164B4')]='Green-blue'
COLOR_NAMES[clr_h2i('#123524')]='Phthalo green'
COLOR_NAMES[clr_h2i('#126180')]='Blue sapphire'
COLOR_NAMES[clr_h2i('#138808')]='India green'
COLOR_NAMES[clr_h2i('#1560BD')]='Denim'
COLOR_NAMES[clr_h2i('#15F2FD')]='Vomit+indogo+Lopen+Gabriel'
COLOR_NAMES[clr_h2i('#177245')]='Dark spring green'
COLOR_NAMES[clr_h2i('#18453B')]='MSU Green'
COLOR_NAMES[clr_h2i('#1874CD')]='Dodger blue'
COLOR_NAMES[clr_h2i('#188BC2')]='Cyan cornflower blue'
COLOR_NAMES[clr_h2i('#191970')]='Midnight blue'
COLOR_NAMES[clr_h2i('#195905')]='Lincoln green'
COLOR_NAMES[clr_h2i('#1974D2')]='Bright navy blue'
COLOR_NAMES[clr_h2i('#1A1110')]='Licorice'
COLOR_NAMES[clr_h2i('#1A2421')]='Dark jungle green'
COLOR_NAMES[clr_h2i('#1B1B1B')]='Eerie black'
COLOR_NAMES[clr_h2i('#1B4D3E')]='English green'
COLOR_NAMES[clr_h2i('#1C1C1C')]='Grey'
COLOR_NAMES[clr_h2i('#1C1CF0')]='Bluebonnet'
COLOR_NAMES[clr_h2i('#1C2841')]='Yankees blue'
COLOR_NAMES[clr_h2i('#1C352D')]='Medium jungle green'
COLOR_NAMES[clr_h2i('#1C39BB')]='Persian blue'
COLOR_NAMES[clr_h2i('#1C86EE')]='Dodger blue'
COLOR_NAMES[clr_h2i('#1CA9C9')]='Pacific Blue'
COLOR_NAMES[clr_h2i('#1CAC78')]='Green'
COLOR_NAMES[clr_h2i('#1D2951')]='Space cadet'
COLOR_NAMES[clr_h2i('#1DACD6')]='Battery Charged Blue'
COLOR_NAMES[clr_h2i('#1E4D2B')]='Cal Poly Pomona green'
COLOR_NAMES[clr_h2i('#1E90FF')]='Dodger blue'
COLOR_NAMES[clr_h2i('#1F262A')]='Dark gunmetal'
COLOR_NAMES[clr_h2i('#1F75FE')]='Blue'
COLOR_NAMES[clr_h2i('#20B2AA')]='Light sea green'
COLOR_NAMES[clr_h2i('#210837')]='Middle Red Purple'
COLOR_NAMES[clr_h2i('#214FC6')]='New Car'
COLOR_NAMES[clr_h2i('#21ABCD')]='Ball blue'
COLOR_NAMES[clr_h2i('#2243B6')]='Denim Blue'
COLOR_NAMES[clr_h2i('#228B22')]='Forest green'
COLOR_NAMES[clr_h2i('#23297A')]='St. Patrick\'s blue'
COLOR_NAMES[clr_h2i('#232B2B')]='Charleston green'
COLOR_NAMES[clr_h2i('#242124')]='Raisin black'
COLOR_NAMES[clr_h2i('#24A0ED')]='Button Blue'
COLOR_NAMES[clr_h2i('#253529')]='Black leather jacket'
COLOR_NAMES[clr_h2i('#264348')]='Japanese indigo'
COLOR_NAMES[clr_h2i('#26619C')]='Lapis lazuli'
COLOR_NAMES[clr_h2i('#273BE2')]='Palatinate blue'
COLOR_NAMES[clr_h2i('#27408B')]='Royal blue'
COLOR_NAMES[clr_h2i('#28589C')]='Cyan cobalt blue'
COLOR_NAMES[clr_h2i('#299617')]='Slimy Green'
COLOR_NAMES[clr_h2i('#29AB87')]='Jungle green'
COLOR_NAMES[clr_h2i('#2a3439')]='Gunmetal'
COLOR_NAMES[clr_h2i('#2A52BE')]='Cerulean blue'
COLOR_NAMES[clr_h2i('#2A8000')]='Napier green'
COLOR_NAMES[clr_h2i('#2C1608')]='Zinnwaldite brown'
COLOR_NAMES[clr_h2i('#2E2D88')]='Cosmic Cobalt'
COLOR_NAMES[clr_h2i('#2E5894')]='B\'dazzled blue'
COLOR_NAMES[clr_h2i('#2E8B57')]='Sea green'
COLOR_NAMES[clr_h2i('#2F4F4F')]='Dark slate gray'
COLOR_NAMES[clr_h2i('#2F847C')]='Celadon green'
COLOR_NAMES[clr_h2i('#301934')]='Dark purple'
COLOR_NAMES[clr_h2i('#306030')]='Mughal green'
COLOR_NAMES[clr_h2i('#30BA8F')]='Mountain Meadow'
COLOR_NAMES[clr_h2i('#30BFBF')]='Maximum Blue Green'
COLOR_NAMES[clr_h2i('#317873')]='Myrtle green'
COLOR_NAMES[clr_h2i('#318CE7')]='Bleu de France'
COLOR_NAMES[clr_h2i('#319177')]='Illuminating Emerald'
COLOR_NAMES[clr_h2i('#32127A')]='Persian indigo'
COLOR_NAMES[clr_h2i('#32174D')]='Russian violet'
COLOR_NAMES[clr_h2i('#324AB2')]='Violet-blue'
COLOR_NAMES[clr_h2i('#32CD32')]='Lime green'
COLOR_NAMES[clr_h2i('#330066')]='Deep violet'
COLOR_NAMES[clr_h2i('#333366')]='Deep koamaru'
COLOR_NAMES[clr_h2i('#333399')]='Blue'
COLOR_NAMES[clr_h2i('#3399FF')]='Brilliant azure'
COLOR_NAMES[clr_h2i('#343434')]='Jet'
COLOR_NAMES[clr_h2i('#34B233')]='Wageningen Green'
COLOR_NAMES[clr_h2i('#353839')]='Onyx'
COLOR_NAMES[clr_h2i('#354230')]='Kombu green'
COLOR_NAMES[clr_h2i('#355E3B')]='Deep moss green'
COLOR_NAMES[clr_h2i('#360CCC')]='Interdimensional Blue'
COLOR_NAMES[clr_h2i('#363636')]='Grey'
COLOR_NAMES[clr_h2i('#36454F')]='Charcoal'
COLOR_NAMES[clr_h2i('#36648B')]='Steel blue'
COLOR_NAMES[clr_h2i('#36747D')]='Ming'
COLOR_NAMES[clr_h2i('#367588')]='Teal blue'
COLOR_NAMES[clr_h2i('#391285')]='Pixie Powder'
COLOR_NAMES[clr_h2i('#39A78E')]='Zomp'
COLOR_NAMES[clr_h2i('#39FF14')]='Neon green'
COLOR_NAMES[clr_h2i('#3A5FCD')]='Royal blue'
COLOR_NAMES[clr_h2i('#3AB09E')]='Keppel'
COLOR_NAMES[clr_h2i('#3B331C')]='Pullman Green'
COLOR_NAMES[clr_h2i('#3B3C36')]='Black olive'
COLOR_NAMES[clr_h2i('#3B444B')]='Arsenic'
COLOR_NAMES[clr_h2i('#3B7A57')]='Amazon'
COLOR_NAMES[clr_h2i('#3C1414')]='Dark sienna'
COLOR_NAMES[clr_h2i('#3C341F')]='Olive Drab #7'
COLOR_NAMES[clr_h2i('#3CB371')]='Medium sea green'
COLOR_NAMES[clr_h2i('#3CD070')]='UFO Green'
COLOR_NAMES[clr_h2i('#3D0C02')]='Black bean'
COLOR_NAMES[clr_h2i('#3D2B1F')]='Bistre'
COLOR_NAMES[clr_h2i('#3E8EDE')]='Tufts Blue'
COLOR_NAMES[clr_h2i('#3EB489')]='Mint'
COLOR_NAMES[clr_h2i('#3F00FF')]='Ultramarine'
COLOR_NAMES[clr_h2i('#3FFF00')]='Harlequin'
COLOR_NAMES[clr_h2i('#40826D')]='Deep aquamarine'
COLOR_NAMES[clr_h2i('#40E0D0')]='Turquoise'
COLOR_NAMES[clr_h2i('#414A4C')]='Outer Space'
COLOR_NAMES[clr_h2i('#4166F5')]='Ultramarine blue'
COLOR_NAMES[clr_h2i('#4169E1')]='Royal blue'
COLOR_NAMES[clr_h2i('#43302E')]='Old burgundy'
COLOR_NAMES[clr_h2i('#436B95')]='Queen blue'
COLOR_NAMES[clr_h2i('#436EEE')]='Royal blue'
COLOR_NAMES[clr_h2i('#43B3AE')]='Verdigris'
COLOR_NAMES[clr_h2i('#43CD80')]='Sea green'
COLOR_NAMES[clr_h2i('#444C38')]='Rifle green'
COLOR_NAMES[clr_h2i('#446CCF')]='Han blue'
COLOR_NAMES[clr_h2i('#44D7A8')]='Eucalyptus'
COLOR_NAMES[clr_h2i('#458B00')]='Chartreuse'
COLOR_NAMES[clr_h2i('#458B74')]='Aquamarine'
COLOR_NAMES[clr_h2i('#45B1E8')]='Picton blue'
COLOR_NAMES[clr_h2i('#465945')]='Gray-asparagus'
COLOR_NAMES[clr_h2i('#4682B4')]='Steel blue'
COLOR_NAMES[clr_h2i('#4682BF')]='Cyan-blue azure'
COLOR_NAMES[clr_h2i('#46CB18')]='Harlequin green'
COLOR_NAMES[clr_h2i('#473C8B')]='Slate blue'
COLOR_NAMES[clr_h2i('#47ABCC')]='Maximum Blue'
COLOR_NAMES[clr_h2i('#480607')]='Bulgarian rose'
COLOR_NAMES[clr_h2i('#483C32')]='Taupe'
COLOR_NAMES[clr_h2i('#483D8B')]='Dark slate blue'
COLOR_NAMES[clr_h2i('#4876FF')]='Royal blue'
COLOR_NAMES[clr_h2i('#48BF91')]='Ocean Green'
COLOR_NAMES[clr_h2i('#48D1CC')]='Medium turquoise'
COLOR_NAMES[clr_h2i('#49796B')]='Hooker\'s green'
COLOR_NAMES[clr_h2i('#4997D0')]='Celestial blue'
COLOR_NAMES[clr_h2i('#4A5D23')]='Dark moss green'
COLOR_NAMES[clr_h2i('#4A646C')]='Deep Space Sparkle'
COLOR_NAMES[clr_h2i('#4A708B')]='Sky blue'
COLOR_NAMES[clr_h2i('#4AFF00')]='Chlorophyll green'
COLOR_NAMES[clr_h2i('#4B0082')]='Indigo'
COLOR_NAMES[clr_h2i('#4B3621')]='Café noir'
COLOR_NAMES[clr_h2i('#4B5320')]='Army green'
COLOR_NAMES[clr_h2i('#4BC7CF')]='Sea Serpent'
COLOR_NAMES[clr_h2i('#4C2882')]='Spanish violet'
COLOR_NAMES[clr_h2i('#4C516D')]='Independence'
COLOR_NAMES[clr_h2i('#4C9141')]='May green'
COLOR_NAMES[clr_h2i('#4CBB17')]='Kelly green'
COLOR_NAMES[clr_h2i('#4D5D53')]='Feldgrau'
COLOR_NAMES[clr_h2i('#4D8C57')]='Middle Green'
COLOR_NAMES[clr_h2i('#4E1609')]='French puce'
COLOR_NAMES[clr_h2i('#4E5180')]='Purple navy'
COLOR_NAMES[clr_h2i('#4E82B4')]='Cyan azure'
COLOR_NAMES[clr_h2i('#4EEE94')]='Sea green'
COLOR_NAMES[clr_h2i('#4F3A3C')]='Dark puce'
COLOR_NAMES[clr_h2i('#4F42B5')]='Ocean Blue'
COLOR_NAMES[clr_h2i('#4F4F4F')]='Grey'
COLOR_NAMES[clr_h2i('#4F666A')]='Stormcloud'
COLOR_NAMES[clr_h2i('#4F7942')]='Fern green'
COLOR_NAMES[clr_h2i('#4F86F7')]='Blueberry'
COLOR_NAMES[clr_h2i('#4F94CD')]='Steel blue'
COLOR_NAMES[clr_h2i('#50404D')]='Purple taupe'
COLOR_NAMES[clr_h2i('#5072A7')]='Blue yonder'
COLOR_NAMES[clr_h2i('#507D2A')]='Sap green'
COLOR_NAMES[clr_h2i('#50C878')]='Emerald'
COLOR_NAMES[clr_h2i('#512888')]='KSU Purple'
COLOR_NAMES[clr_h2i('#51484F')]='Quartz'
COLOR_NAMES[clr_h2i('#5218FA')]='Han purple'
COLOR_NAMES[clr_h2i('#522D80')]='Regalia'
COLOR_NAMES[clr_h2i('#528B8B')]='Dark slate gray'
COLOR_NAMES[clr_h2i('#534B4F')]='Dark liver'
COLOR_NAMES[clr_h2i('#536872')]='Cadet'
COLOR_NAMES[clr_h2i('#536878')]='Dark electric blue'
COLOR_NAMES[clr_h2i('#536895')]='UCLA Blue'
COLOR_NAMES[clr_h2i('#53868B')]='Cadet blue'
COLOR_NAMES[clr_h2i('#543D37')]='Dark liver (horses)'
COLOR_NAMES[clr_h2i('#545AA7')]='Liberty'
COLOR_NAMES[clr_h2i('#54626F')]='Black Coral'
COLOR_NAMES[clr_h2i('#548B54')]='Pale green'
COLOR_NAMES[clr_h2i('#54FF9F')]='Sea green'
COLOR_NAMES[clr_h2i('#551A8B')]='Purple'
COLOR_NAMES[clr_h2i('#553592')]='Blue-magenta violet'
COLOR_NAMES[clr_h2i('#555555')]='Davy\'s grey'
COLOR_NAMES[clr_h2i('#555D50')]='Ebony'
COLOR_NAMES[clr_h2i('#556B2F')]='Dark olive green'
COLOR_NAMES[clr_h2i('#560319')]='Dark scarlet'
COLOR_NAMES[clr_h2i('#563C5C')]='Pineapple'
COLOR_NAMES[clr_h2i('#568203')]='Avocado'
COLOR_NAMES[clr_h2i('#56887D')]='Wintergreen Dream'
COLOR_NAMES[clr_h2i('#56A0D3')]='Carolina blue'
COLOR_NAMES[clr_h2i('#58427C')]='Cyber grape'
COLOR_NAMES[clr_h2i('#59260B')]='Seal brown'
COLOR_NAMES[clr_h2i('#592720')]='Caput mortuum'
COLOR_NAMES[clr_h2i('#5946B2')]='Plump Purple'
COLOR_NAMES[clr_h2i('#5A4FCF')]='Iris'
COLOR_NAMES[clr_h2i('#5B3256')]='Japanese violet'
COLOR_NAMES[clr_h2i('#5B92E5')]='United Nations blue'
COLOR_NAMES[clr_h2i('#5CACEE')]='Steel blue'
COLOR_NAMES[clr_h2i('#5D3954')]='Dark byzantium'
COLOR_NAMES[clr_h2i('#5D478B')]='Medium purple'
COLOR_NAMES[clr_h2i('#5D89BA')]='Silver Lake blue'
COLOR_NAMES[clr_h2i('#5D8AA8')]='Air Force blue'
COLOR_NAMES[clr_h2i('#5DA493')]='Polished Pine'
COLOR_NAMES[clr_h2i('#5DADEC')]='Blue Jeans'
COLOR_NAMES[clr_h2i('#5E8C31')]='Maximum Green'
COLOR_NAMES[clr_h2i('#5F8A8B')]='Steel Teal'
COLOR_NAMES[clr_h2i('#5F9EA0')]='Cadet blue'
COLOR_NAMES[clr_h2i('#5FA778')]='Shiny Shamrock'
COLOR_NAMES[clr_h2i('#602F6B')]='Imperial'
COLOR_NAMES[clr_h2i('#6050DC')]='Majorelle Blue'
COLOR_NAMES[clr_h2i('#607B8B')]='Light sky blue'
COLOR_NAMES[clr_h2i('#6082B6')]='Glaucous'
COLOR_NAMES[clr_h2i('#614051')]='Eggplant'
COLOR_NAMES[clr_h2i('#635147')]='Umber'
COLOR_NAMES[clr_h2i('#63B8FF')]='Steel blue'
COLOR_NAMES[clr_h2i('#644117')]='Pullman Brown'
COLOR_NAMES[clr_h2i('#645452')]='Wenge'
COLOR_NAMES[clr_h2i('#6495ED')]='Cornflower blue'
COLOR_NAMES[clr_h2i('#64E986')]='Very light malachite green'
COLOR_NAMES[clr_h2i('#65000B')]='Rosewood'
COLOR_NAMES[clr_h2i('#654321')]='Dark brown'
COLOR_NAMES[clr_h2i('#66023C')]='Imperial purple'
COLOR_NAMES[clr_h2i('#663399')]='Rebecca Purple'
COLOR_NAMES[clr_h2i('#663854')]='Halayà úbe'
COLOR_NAMES[clr_h2i('#664228')]='Van Dyke Brown'
COLOR_NAMES[clr_h2i('#66424D')]='Deep Tuscan red'
COLOR_NAMES[clr_h2i('#664C28')]='Donkey brown'
COLOR_NAMES[clr_h2i('#665D1E')]='Antique bronze'
COLOR_NAMES[clr_h2i('#666699')]='Dark blue-gray'
COLOR_NAMES[clr_h2i('#6666FF')]='Very light blue'
COLOR_NAMES[clr_h2i('#668B8B')]='Pale turquoise'
COLOR_NAMES[clr_h2i('#669999')]='Desaturated cyan'
COLOR_NAMES[clr_h2i('#6699CC')]='Livid'
COLOR_NAMES[clr_h2i('#66B032')]='Green'
COLOR_NAMES[clr_h2i('#66CD00')]='Chartreuse'
COLOR_NAMES[clr_h2i('#66CDAA')]='Medium aquamarine'
COLOR_NAMES[clr_h2i('#66DDAA')]='Medium aquamarine'
COLOR_NAMES[clr_h2i('#66FF00')]='Bright green'
COLOR_NAMES[clr_h2i('#66FF66')]='Screamin\' Green'
COLOR_NAMES[clr_h2i('#673147')]='Wine dregs'
COLOR_NAMES[clr_h2i('#674846')]='Rose ebony'
COLOR_NAMES[clr_h2i('#674C47')]='Medium taupe'
COLOR_NAMES[clr_h2i('#676767')]='Granite Gray'
COLOR_NAMES[clr_h2i('#679267')]='Russian green'
COLOR_NAMES[clr_h2i('#68228B')]='Dark orchid'
COLOR_NAMES[clr_h2i('#682860')]='Palatinate purple'
COLOR_NAMES[clr_h2i('#68838B')]='Light blue'
COLOR_NAMES[clr_h2i('#69359C')]='Purple Heart'
COLOR_NAMES[clr_h2i('#6959CD')]='Slate blue'
COLOR_NAMES[clr_h2i('#696969')]='Dim gray'
COLOR_NAMES[clr_h2i('#698B22')]='Olive drab'
COLOR_NAMES[clr_h2i('#698B69')]='Dark sea green'
COLOR_NAMES[clr_h2i('#6A5ACD')]='Slate blue'
COLOR_NAMES[clr_h2i('#6B4423')]='Kobicha'
COLOR_NAMES[clr_h2i('#6B8E23')]='Olive Drab'
COLOR_NAMES[clr_h2i('#6C2E1F')]='Liver (organ)'
COLOR_NAMES[clr_h2i('#6C3082')]='Eminence'
COLOR_NAMES[clr_h2i('#6C541E')]='Field drab'
COLOR_NAMES[clr_h2i('#6C7B8B')]='Slate gray'
COLOR_NAMES[clr_h2i('#6CA0DC')]='Little boy blue'
COLOR_NAMES[clr_h2i('#6CA6CD')]='Sky blue'
COLOR_NAMES[clr_h2i('#6D9BC3')]='Cerulean frost'
COLOR_NAMES[clr_h2i('#6E7B8B')]='Light steel blue'
COLOR_NAMES[clr_h2i('#6E7F80')]='Auro metal saurus'
COLOR_NAMES[clr_h2i('#6E8B3D')]='Dark olive green'
COLOR_NAMES[clr_h2i('#6EAEA1')]='Green Sheen'
COLOR_NAMES[clr_h2i('#6F00FF')]='Electric indigo'
COLOR_NAMES[clr_h2i('#6F2DA8')]='Grape'
COLOR_NAMES[clr_h2i('#6F4E37')]='Tuscan brown'
COLOR_NAMES[clr_h2i('#6F9940')]='Palm Leaf'
COLOR_NAMES[clr_h2i('#701C1C')]='Persian plum'
COLOR_NAMES[clr_h2i('#702670')]='Midnight'
COLOR_NAMES[clr_h2i('#702963')]='Byzantium'
COLOR_NAMES[clr_h2i('#703642')]='Catawba'
COLOR_NAMES[clr_h2i('#704214')]='Sepia'
COLOR_NAMES[clr_h2i('#704241')]='Roast coffee'
COLOR_NAMES[clr_h2i('#708090')]='Slate gray'
COLOR_NAMES[clr_h2i('#71A6D2')]='Iceberg'
COLOR_NAMES[clr_h2i('#71BC78')]='Iguana Green'
COLOR_NAMES[clr_h2i('#722F37')]='Puce red'
COLOR_NAMES[clr_h2i('#727472')]='Nickel'
COLOR_NAMES[clr_h2i('#72A0C1')]='Air superiority blue'
COLOR_NAMES[clr_h2i('#733380')]='Maximum Purple'
COLOR_NAMES[clr_h2i('#734F96')]='Dark lavender'
COLOR_NAMES[clr_h2i('#737000')]='Bronze Yellow'
COLOR_NAMES[clr_h2i('#738276')]='Smoke'
COLOR_NAMES[clr_h2i('#738678')]='Xanadu'
COLOR_NAMES[clr_h2i('#73A9C2')]='Moonstone blue'
COLOR_NAMES[clr_h2i('#73C2FB')]='Maya blue'
COLOR_NAMES[clr_h2i('#746CC0')]='Toolbox'
COLOR_NAMES[clr_h2i('#74BBFB')]='Very light azure'
COLOR_NAMES[clr_h2i('#74C365')]='Mantis'
COLOR_NAMES[clr_h2i('#757575')]='Sonic silver'
COLOR_NAMES[clr_h2i('#76EE00')]='Chartreuse'
COLOR_NAMES[clr_h2i('#76EEC6')]='Aquamarine'
COLOR_NAMES[clr_h2i('#777696')]='Rhythm'
COLOR_NAMES[clr_h2i('#778899')]='Light slate gray'
COLOR_NAMES[clr_h2i('#778BA5')]='Shadow blue'
COLOR_NAMES[clr_h2i('#779ECB')]='Dark pastel blue'
COLOR_NAMES[clr_h2i('#77B5FE')]='French sky blue'
COLOR_NAMES[clr_h2i('#77DD77')]='Pastel green'
COLOR_NAMES[clr_h2i('#78184A')]='Pansy purple'
COLOR_NAMES[clr_h2i('#7851A9')]='Royal purple'
COLOR_NAMES[clr_h2i('#78866B')]='Camouflage green'
COLOR_NAMES[clr_h2i('#79443B')]='Medium Tuscan red'
COLOR_NAMES[clr_h2i('#796878')]='Old lavender'
COLOR_NAMES[clr_h2i('#79CDCD')]='Dark slate gray'
COLOR_NAMES[clr_h2i('#7A378B')]='Medium orchid'
COLOR_NAMES[clr_h2i('#7A67EE')]='Slate blue'
COLOR_NAMES[clr_h2i('#7A8B8B')]='Light cyan'
COLOR_NAMES[clr_h2i('#7AC5CD')]='Cadet blue'
COLOR_NAMES[clr_h2i('#7B1113')]='UP Maroon'
COLOR_NAMES[clr_h2i('#7B3F00')]='Chocolate (traditional)'
COLOR_NAMES[clr_h2i('#7B68EE')]='Medium slate blue'
COLOR_NAMES[clr_h2i('#7BB661')]='Bud green'
COLOR_NAMES[clr_h2i('#7C0A02')]='Barn red'
COLOR_NAMES[clr_h2i('#7C1C05')]='Kenyan copper'
COLOR_NAMES[clr_h2i('#7C4848')]='Tuscan red'
COLOR_NAMES[clr_h2i('#7C98AB')]='Weldon Blue'
COLOR_NAMES[clr_h2i('#7C9ED9')]='Vista blue'
COLOR_NAMES[clr_h2i('#7CB9E8')]='Aero'
COLOR_NAMES[clr_h2i('#7CCD7C')]='Pale green'
COLOR_NAMES[clr_h2i('#7CFC00')]='Lawn green'
COLOR_NAMES[clr_h2i('#7D26CD')]='Purple'
COLOR_NAMES[clr_h2i('#7DF9FF')]='Electric blue'
COLOR_NAMES[clr_h2i('#7E5E60')]='Deep Taupe'
COLOR_NAMES[clr_h2i('#7EC0EE')]='Sky blue'
COLOR_NAMES[clr_h2i('#7ED4E6')]='Middle Blue'
COLOR_NAMES[clr_h2i('#7F00FF')]='Violet'
COLOR_NAMES[clr_h2i('#7F1734')]='Claret'
COLOR_NAMES[clr_h2i('#7FFF00')]='Chartreuse'
COLOR_NAMES[clr_h2i('#7FFFD4')]='Aquamarine'
COLOR_NAMES[clr_h2i('#800000')]='Maroon'
COLOR_NAMES[clr_h2i('#800020')]='Burgundy'
COLOR_NAMES[clr_h2i('#800080')]='Patriarch, Purple'
COLOR_NAMES[clr_h2i('#801818')]='Falu red'
COLOR_NAMES[clr_h2i('#80461B')]='Russet'
COLOR_NAMES[clr_h2i('#807532')]='Spanish bistre'
COLOR_NAMES[clr_h2i('#808000')]='Olive'
COLOR_NAMES[clr_h2i('#808080')]='Trolley Grey'
COLOR_NAMES[clr_h2i('#80DAEB')]='Medium sky blue'
COLOR_NAMES[clr_h2i('#811453')]='French plum'
COLOR_NAMES[clr_h2i('#81613C')]='Coyote brown'
COLOR_NAMES[clr_h2i('#820000')]='Deep maroon'
COLOR_NAMES[clr_h2i('#826644')]='Raw umber'
COLOR_NAMES[clr_h2i('#828E84')]='Dolphin Gray'
COLOR_NAMES[clr_h2i('#832A0D')]='Smokey Topaz'
COLOR_NAMES[clr_h2i('#836953')]='Pastel brown'
COLOR_NAMES[clr_h2i('#836FFF')]='Slate blue'
COLOR_NAMES[clr_h2i('#838996')]='Roman silver'
COLOR_NAMES[clr_h2i('#838B83')]='Honeydew'
COLOR_NAMES[clr_h2i('#838B8B')]='Azure'
COLOR_NAMES[clr_h2i('#841B2D')]='Antique ruby'
COLOR_NAMES[clr_h2i('#843F5B')]='Deep ruby'
COLOR_NAMES[clr_h2i('#8470FF')]='Light slate blue'
COLOR_NAMES[clr_h2i('#848482')]='Old silver'
COLOR_NAMES[clr_h2i('#84DE02')]='Alien Armpit'
COLOR_NAMES[clr_h2i('#850101')]='Deep Red'
COLOR_NAMES[clr_h2i('#856088')]='Chinese violet'
COLOR_NAMES[clr_h2i('#856D4D')]='French bistre'
COLOR_NAMES[clr_h2i('#85754E')]='Gold Fusion'
COLOR_NAMES[clr_h2i('#85BB65')]='Dollar bill'
COLOR_NAMES[clr_h2i('#860111')]='Red devil'
COLOR_NAMES[clr_h2i('#8601AF')]='Violet'
COLOR_NAMES[clr_h2i('#86608E')]='French lilac'
COLOR_NAMES[clr_h2i('#867E36')]='Old moss green'
COLOR_NAMES[clr_h2i('#872657')]='Dark raspberry'
COLOR_NAMES[clr_h2i('#873260')]='Boysenberry'
COLOR_NAMES[clr_h2i('#87A96B')]='Asparagus'
COLOR_NAMES[clr_h2i('#87CEEB')]='Sky blue'
COLOR_NAMES[clr_h2i('#87CEFA')]='Light sky blue'
COLOR_NAMES[clr_h2i('#87CEFF')]='Sky blue'
COLOR_NAMES[clr_h2i('#87D3F8')]='Pale cyan'
COLOR_NAMES[clr_h2i('#87FF2A')]='Spring Frost'
COLOR_NAMES[clr_h2i('#880085')]='Mardi Gras'
COLOR_NAMES[clr_h2i('#8806CE')]='French violet'
COLOR_NAMES[clr_h2i('#882D17')]='Sienna'
COLOR_NAMES[clr_h2i('#885818')]='Grizzly'
COLOR_NAMES[clr_h2i('#88654E')]='Dark brown-tangelo'
COLOR_NAMES[clr_h2i('#8878C3')]='Ube'
COLOR_NAMES[clr_h2i('#88ACE0')]='Light cobalt blue'
COLOR_NAMES[clr_h2i('#88D8C0')]='Pearl Aqua'
COLOR_NAMES[clr_h2i('#893843')]='Solid pink'
COLOR_NAMES[clr_h2i('#893F45')]='Cordovan'
COLOR_NAMES[clr_h2i('#8968CD')]='Medium purple'
COLOR_NAMES[clr_h2i('#89CFF0')]='Baby blue'
COLOR_NAMES[clr_h2i('#8A2BE2')]='Blue-violet'
COLOR_NAMES[clr_h2i('#8A3324')]='Burnt umber'
COLOR_NAMES[clr_h2i('#8A496B')]='Twilight lavender'
COLOR_NAMES[clr_h2i('#8A795D')]='Shadow'
COLOR_NAMES[clr_h2i('#8A7F80')]='Rocket metallic'
COLOR_NAMES[clr_h2i('#8A9A5B')]='Turtle green'
COLOR_NAMES[clr_h2i('#8AB9F1')]='Jordy blue'
COLOR_NAMES[clr_h2i('#8B0000')]='Dark red'
COLOR_NAMES[clr_h2i('#8B008B')]='Dark magenta'
COLOR_NAMES[clr_h2i('#8B0A50')]='Deep pink'
COLOR_NAMES[clr_h2i('#8B1A1A')]='Firebrick'
COLOR_NAMES[clr_h2i('#8B1C62')]='Maroon'
COLOR_NAMES[clr_h2i('#8B2252')]='Violet red'
COLOR_NAMES[clr_h2i('#8B2323')]='Brown'
COLOR_NAMES[clr_h2i('#8B2500')]='Orange red'
COLOR_NAMES[clr_h2i('#8B3626')]='Tomato'
COLOR_NAMES[clr_h2i('#8B3A3A')]='Indian red'
COLOR_NAMES[clr_h2i('#8B3A62')]='Hot pink'
COLOR_NAMES[clr_h2i('#8B3E2F')]='Coral'
COLOR_NAMES[clr_h2i('#8B4500')]='Dark orange'
COLOR_NAMES[clr_h2i('#8B4513')]='Saddle brown'
COLOR_NAMES[clr_h2i('#8B4726')]='Sienna'
COLOR_NAMES[clr_h2i('#8B475D')]='Pale violet red'
COLOR_NAMES[clr_h2i('#8B4789')]='Orchid'
COLOR_NAMES[clr_h2i('#8B4C39')]='Salmon'
COLOR_NAMES[clr_h2i('#8B5742')]='Light salmon'
COLOR_NAMES[clr_h2i('#8B5A00')]='Orange'
COLOR_NAMES[clr_h2i('#8B5A2B')]='Tan'
COLOR_NAMES[clr_h2i('#8B5f4D')]='Spicy mix'
COLOR_NAMES[clr_h2i('#8B5F65')]='Light pink'
COLOR_NAMES[clr_h2i('#8B636C')]='Pink'
COLOR_NAMES[clr_h2i('#8B658B')]='Dark goldenrod'
COLOR_NAMES[clr_h2i('#8B668B')]='Plum'
COLOR_NAMES[clr_h2i('#8B6914')]='Goldenrod'
COLOR_NAMES[clr_h2i('#8B6969')]='Rosy brown'
COLOR_NAMES[clr_h2i('#8B72BE')]='Middle Blue Purple'
COLOR_NAMES[clr_h2i('#8B7355')]='Burlywood'
COLOR_NAMES[clr_h2i('#8B7500')]='Gold'
COLOR_NAMES[clr_h2i('#8B7765')]='Peach puff'
COLOR_NAMES[clr_h2i('#8B795E')]='Navajo white'
COLOR_NAMES[clr_h2i('#8B7B8B')]='Thistle'
COLOR_NAMES[clr_h2i('#8B7D6B')]='Bisque'
COLOR_NAMES[clr_h2i('#8B7D7B')]='Misty rose'
COLOR_NAMES[clr_h2i('#8B7E66')]='Wheat'
COLOR_NAMES[clr_h2i('#8B814C')]='Light goldenrod'
COLOR_NAMES[clr_h2i('#8B8378')]='Antique white'
COLOR_NAMES[clr_h2i('#8B8386')]='Lavender blush'
COLOR_NAMES[clr_h2i('#8B8589')]='Taupe gray'
COLOR_NAMES[clr_h2i('#8B864E')]='Khaki'
COLOR_NAMES[clr_h2i('#8B8682')]='Seashell'
COLOR_NAMES[clr_h2i('#8B8878')]='Cornsilk'
COLOR_NAMES[clr_h2i('#8B8970')]='Lemon chiffon'
COLOR_NAMES[clr_h2i('#8B8989')]='Snow'
COLOR_NAMES[clr_h2i('#8B8B00')]='Yellow'
COLOR_NAMES[clr_h2i('#8B8B7A')]='Light yellow'
COLOR_NAMES[clr_h2i('#8B8B83')]='Ivory'
COLOR_NAMES[clr_h2i('#8BA8B7')]='Pewter Blue'
COLOR_NAMES[clr_h2i('#8C92AC')]='Cool grey, Gray-blue'
COLOR_NAMES[clr_h2i('#8CBED6')]='Dark sky blue'
COLOR_NAMES[clr_h2i('#8D4E85')]='Razzmic Berry'
COLOR_NAMES[clr_h2i('#8DA399')]='Morning blue'
COLOR_NAMES[clr_h2i('#8DB600')]='Apple green'
COLOR_NAMES[clr_h2i('#8DB6CD')]='Light sky blue'
COLOR_NAMES[clr_h2i('#8DD9CC')]='Middle Blue Green'
COLOR_NAMES[clr_h2i('#8DEEEE')]='Dark slate gray'
COLOR_NAMES[clr_h2i('#8E3A59')]='Quinacridone magenta'
COLOR_NAMES[clr_h2i('#8E4585')]='Plum'
COLOR_NAMES[clr_h2i('#8EE53F')]='Kiwi'
COLOR_NAMES[clr_h2i('#8EE5EE')]='Cadet blue'
COLOR_NAMES[clr_h2i('#8F00FF')]='Violet'
COLOR_NAMES[clr_h2i('#8F9779')]='Artichoke'
COLOR_NAMES[clr_h2i('#8FBC8F')]='Dark sea green'
COLOR_NAMES[clr_h2i('#8FD400')]='Sheen Green'
COLOR_NAMES[clr_h2i('#905D5D')]='Rose taupe'
COLOR_NAMES[clr_h2i('#90EE90')]='Light green'
COLOR_NAMES[clr_h2i('#912CEE')]='Purple'
COLOR_NAMES[clr_h2i('#914E75')]='Sugar Plum'
COLOR_NAMES[clr_h2i('#915C83')]='Antique fuchsia'
COLOR_NAMES[clr_h2i('#915F6D')]='Mauve taupe'
COLOR_NAMES[clr_h2i('#918151')]='Dark tan'
COLOR_NAMES[clr_h2i('#91A3B0')]='Cadet grey'
COLOR_NAMES[clr_h2i('#92000A')]='Sangria'
COLOR_NAMES[clr_h2i('#922724')]='Vivid auburn'
COLOR_NAMES[clr_h2i('#92A1CF')]='Ceil'
COLOR_NAMES[clr_h2i('#933D41')]='Smoky Topaz'
COLOR_NAMES[clr_h2i('#9370DB')]='Medium purple'
COLOR_NAMES[clr_h2i('#93C572')]='Pistachio'
COLOR_NAMES[clr_h2i('#93CCEA')]='Light cornflower blue'
COLOR_NAMES[clr_h2i('#9400D3')]='Dark violet'
COLOR_NAMES[clr_h2i('#9457EB')]='Lavender indigo, Navy purple'
COLOR_NAMES[clr_h2i('#954535')]='Chestnut'
COLOR_NAMES[clr_h2i('#960018')]='Carmine, Heidelberg Red'
COLOR_NAMES[clr_h2i('#964B00')]='Brown (traditional)'
COLOR_NAMES[clr_h2i('#965A3E')]='Coconut'
COLOR_NAMES[clr_h2i('#966FD6')]='Dark pastel purple'
COLOR_NAMES[clr_h2i('#967117')]='Sandy taupe'
COLOR_NAMES[clr_h2i('#9678B6')]='Purple mountain majesty'
COLOR_NAMES[clr_h2i('#967BB6')]='Lavender purple'
COLOR_NAMES[clr_h2i('#96C8A2')]='Eton blue'
COLOR_NAMES[clr_h2i('#96CDCD')]='Pale turquoise'
COLOR_NAMES[clr_h2i('#96DED1')]='Pale robin egg blue'
COLOR_NAMES[clr_h2i('#979AAA')]='Manatee'
COLOR_NAMES[clr_h2i('#97FFFF')]='Dark slate gray'
COLOR_NAMES[clr_h2i('#980036')]='Pink raspberry'
COLOR_NAMES[clr_h2i('#986960')]='Dark chestnut'
COLOR_NAMES[clr_h2i('#987456')]='Liver chestnut'
COLOR_NAMES[clr_h2i('#987654')]='Pale brown'
COLOR_NAMES[clr_h2i('#98777B')]='Bazaar'
COLOR_NAMES[clr_h2i('#98817B')]='Cinereous'
COLOR_NAMES[clr_h2i('#989898')]='Spanish gray'
COLOR_NAMES[clr_h2i('#98F5FF')]='Cadet blue'
COLOR_NAMES[clr_h2i('#98FB98')]='Pale green'
COLOR_NAMES[clr_h2i('#98FF98')]='Mint green'
COLOR_NAMES[clr_h2i('#990000')]='Crimson red'
COLOR_NAMES[clr_h2i('#9932CC')]='Dark orchid'
COLOR_NAMES[clr_h2i('#9955BB')]='Deep lilac'
COLOR_NAMES[clr_h2i('#996515')]='Golden brown'
COLOR_NAMES[clr_h2i('#996600')]='Gamboge orange (brown)'
COLOR_NAMES[clr_h2i('#996666')]='Copper rose'
COLOR_NAMES[clr_h2i('#9966CC')]='Amethyst'
COLOR_NAMES[clr_h2i('#997A8D')]='Mountbatten pink'
COLOR_NAMES[clr_h2i('#99E6B3')]='Teal deer'
COLOR_NAMES[clr_h2i('#9A32CD')]='Dark orchid'
COLOR_NAMES[clr_h2i('#9A4EAE')]='Purpureus'
COLOR_NAMES[clr_h2i('#9AB973')]='Olivine'
COLOR_NAMES[clr_h2i('#9AC0CD')]='Light blue'
COLOR_NAMES[clr_h2i('#9ACD32')]='Yellow-green'
COLOR_NAMES[clr_h2i('#9AFF9A')]='Pale green'
COLOR_NAMES[clr_h2i('#9B111E')]='Ruby red'
COLOR_NAMES[clr_h2i('#9B30FF')]='Purple'
COLOR_NAMES[clr_h2i('#9B7653')]='Dirt'
COLOR_NAMES[clr_h2i('#9B870C')]='Dark yellow'
COLOR_NAMES[clr_h2i('#9BC4E2')]='Pale cerulean'
COLOR_NAMES[clr_h2i('#9BCD9B')]='Dark sea green'
COLOR_NAMES[clr_h2i('#9C2542')]='Big dip o’ruby'
COLOR_NAMES[clr_h2i('#9C51B6')]='Purple Plum'
COLOR_NAMES[clr_h2i('#9C7C38')]='Metallic Sunburst'
COLOR_NAMES[clr_h2i('#9C9C9C')]='Grey'
COLOR_NAMES[clr_h2i('#9D2933')]='Japanese carmine'
COLOR_NAMES[clr_h2i('#9DC209')]='Limerick'
COLOR_NAMES[clr_h2i('#9E1316')]='Spartan Crimson'
COLOR_NAMES[clr_h2i('#9E5E6F')]='Rose Dust'
COLOR_NAMES[clr_h2i('#9EFD38')]='French lime'
COLOR_NAMES[clr_h2i('#9F00C5')]='Purple'
COLOR_NAMES[clr_h2i('#9F00FF')]='Vivid violet'
COLOR_NAMES[clr_h2i('#9F1D35')]='Vivid burgundy'
COLOR_NAMES[clr_h2i('#9F2B68')]='Amaranth deep purple'
COLOR_NAMES[clr_h2i('#9F4576')]='Magenta haze'
COLOR_NAMES[clr_h2i('#9F79EE')]='Medium purple'
COLOR_NAMES[clr_h2i('#9F8170')]='Beaver'
COLOR_NAMES[clr_h2i('#9FA91F')]='Citron'
COLOR_NAMES[clr_h2i('#9FB6CD')]='Slate gray'
COLOR_NAMES[clr_h2i('#9FE2BF')]='Sea Foam Green'
COLOR_NAMES[clr_h2i('#A020F0')]='Purple, Veronica'
COLOR_NAMES[clr_h2i('#A0522D')]='Sienna'
COLOR_NAMES[clr_h2i('#A0785A')]='Chamoisee'
COLOR_NAMES[clr_h2i('#A0D6B4')]='Turquoise green'
COLOR_NAMES[clr_h2i('#A0E6FF')]='Winter Wizard'
COLOR_NAMES[clr_h2i('#A17A74')]='Burnished Brown'
COLOR_NAMES[clr_h2i('#A1CAF1')]='Baby blue eyes'
COLOR_NAMES[clr_h2i('#A2006D')]='Flirt'
COLOR_NAMES[clr_h2i('#A2A2D0')]='Blue Bell'
COLOR_NAMES[clr_h2i('#A2ADD0')]='Wild blue yonder'
COLOR_NAMES[clr_h2i('#A2B5CD')]='Light steel blue'
COLOR_NAMES[clr_h2i('#A2CD5A')]='Dark olive green'
COLOR_NAMES[clr_h2i('#A3C1AD')]='Cambridge Blue'
COLOR_NAMES[clr_h2i('#A40000')]='Dark candy apple red'
COLOR_NAMES[clr_h2i('#A45A52')]='Redwood'
COLOR_NAMES[clr_h2i('#A4C639')]='Android green'
COLOR_NAMES[clr_h2i('#A4D3EE')]='Light sky blue'
COLOR_NAMES[clr_h2i('#A4DDED')]='Non-photo blue'
COLOR_NAMES[clr_h2i('#A4F4F9')]='Waterspout'
COLOR_NAMES[clr_h2i('#A50B5E')]='Jazzberry jam'
COLOR_NAMES[clr_h2i('#A52A2A')]='Auburn, Brown'
COLOR_NAMES[clr_h2i('#A55353')]='Middle Red Purple'
COLOR_NAMES[clr_h2i('#A57164')]='Blast-off bronze'
COLOR_NAMES[clr_h2i('#A63A79')]='Maximum Red Purple'
COLOR_NAMES[clr_h2i('#A67B5B')]='French beige, Tuscan tan'
COLOR_NAMES[clr_h2i('#A6A6A6')]='Quick Silver'
COLOR_NAMES[clr_h2i('#A6D608')]='Vivid lime green'
COLOR_NAMES[clr_h2i('#A6E7FF')]='Fresh Air'
COLOR_NAMES[clr_h2i('#A75502')]='Windsor tan'
COLOR_NAMES[clr_h2i('#A76BCF')]='Rich lavender'
COLOR_NAMES[clr_h2i('#A7F432')]='Green Lizard'
COLOR_NAMES[clr_h2i('#A7FC00')]='Spring bud'
COLOR_NAMES[clr_h2i('#A81C07')]='Rufous'
COLOR_NAMES[clr_h2i('#A83731')]='Sweet Brown'
COLOR_NAMES[clr_h2i('#A8516E')]='China rose'
COLOR_NAMES[clr_h2i('#A8E4A0')]='Granny Smith Apple'
COLOR_NAMES[clr_h2i('#A9203E')]='Deep carmine'
COLOR_NAMES[clr_h2i('#A95C68')]='Deep puce'
COLOR_NAMES[clr_h2i('#A99A86')]='Grullo'
COLOR_NAMES[clr_h2i('#A9A9A9')]='Dark medium gray'
COLOR_NAMES[clr_h2i('#A9BA9D')]='Laurel green'
COLOR_NAMES[clr_h2i('#AA00BB')]='Heliotrope magenta'
COLOR_NAMES[clr_h2i('#AA381E')]='Chinese red'
COLOR_NAMES[clr_h2i('#AA4069')]='Medium ruby'
COLOR_NAMES[clr_h2i('#AA98A9')]='Heliotrope gray, Rose quartz'
COLOR_NAMES[clr_h2i('#AAF0D1')]='Magic mint'
COLOR_NAMES[clr_h2i('#AB274F')]='Amaranth purple'
COLOR_NAMES[clr_h2i('#AB4B52')]='English red'
COLOR_NAMES[clr_h2i('#AB4E52')]='Rose vale'
COLOR_NAMES[clr_h2i('#AB82FF')]='Medium purple'
COLOR_NAMES[clr_h2i('#AB92B3')]='Glossy Grape'
COLOR_NAMES[clr_h2i('#ABCDEF')]='Pale cornflower blue'
COLOR_NAMES[clr_h2i('#AC1E44')]='French wine'
COLOR_NAMES[clr_h2i('#ACACAC')]='Silver chalice'
COLOR_NAMES[clr_h2i('#ACACE6')]='Maximum Blue Purple'
COLOR_NAMES[clr_h2i('#ACBF60')]='Middle Green Yellow'
COLOR_NAMES[clr_h2i('#ACE1AF')]='Celadon'
COLOR_NAMES[clr_h2i('#ACE5EE')]='Blizzard Blue, Blue Lagoon'
COLOR_NAMES[clr_h2i('#AD4379')]='Mystic Maroon'
COLOR_NAMES[clr_h2i('#AD6F69')]='Copper penny'
COLOR_NAMES[clr_h2i('#ADD8E6')]='Light blue'
COLOR_NAMES[clr_h2i('#ADDFAD')]='Light moss green'
COLOR_NAMES[clr_h2i('#ADFF2F')]='Green-yellow'
COLOR_NAMES[clr_h2i('#AE0C00')]='Mordant red 19'
COLOR_NAMES[clr_h2i('#AE2029')]='Upsdell red'
COLOR_NAMES[clr_h2i('#AE98AA')]='Lilac Luster'
COLOR_NAMES[clr_h2i('#AEC6CF')]='Pastel blue'
COLOR_NAMES[clr_h2i('#AF002A')]='Alabama crimson'
COLOR_NAMES[clr_h2i('#AF4035')]='Pale carmine'
COLOR_NAMES[clr_h2i('#AF6E4D')]='Brown Sugar'
COLOR_NAMES[clr_h2i('#AFEEEE')]='Pale blue'
COLOR_NAMES[clr_h2i('#B03060')]='Rich maroon'
COLOR_NAMES[clr_h2i('#B05C52')]='Giant\'s Club'
COLOR_NAMES[clr_h2i('#B06500')]='Ginger'
COLOR_NAMES[clr_h2i('#B0BF1A')]='Acid green'
COLOR_NAMES[clr_h2i('#B0C4DE')]='Light steel blue'
COLOR_NAMES[clr_h2i('#B0E0E6')]='Powder blue'
COLOR_NAMES[clr_h2i('#B0E2FF')]='Light sky blue'
COLOR_NAMES[clr_h2i('#B19CD9')]='Light pastel purple'
COLOR_NAMES[clr_h2i('#B22222')]='Firebrick'
COLOR_NAMES[clr_h2i('#B23AEE')]='Dark orchid'
COLOR_NAMES[clr_h2i('#B284BE')]='African violet'
COLOR_NAMES[clr_h2i('#B2BEB5')]='Ash grey'
COLOR_NAMES[clr_h2i('#B2DFEE')]='Light blue'
COLOR_NAMES[clr_h2i('#B2EC5D')]='Inchworm'
COLOR_NAMES[clr_h2i('#B2FFFF')]='Celeste, Italian sky blue'
COLOR_NAMES[clr_h2i('#B31B1B')]='Carnelian, Cornell Red'
COLOR_NAMES[clr_h2i('#B3446C')]='Irresistible, Raspberry rose'
COLOR_NAMES[clr_h2i('#B38B6D')]='Light taupe'
COLOR_NAMES[clr_h2i('#B39EB5')]='Pastel purple'
COLOR_NAMES[clr_h2i('#B3EE3A')]='Olive drab'
COLOR_NAMES[clr_h2i('#B452CD')]='Medium orchid'
COLOR_NAMES[clr_h2i('#B48395')]='English lavender'
COLOR_NAMES[clr_h2i('#B4CDCD')]='Light cyan'
COLOR_NAMES[clr_h2i('#B4EEB4')]='Dark sea green'
COLOR_NAMES[clr_h2i('#B53389')]='Fandango'
COLOR_NAMES[clr_h2i('#B5651D')]='Light brown'
COLOR_NAMES[clr_h2i('#B57281')]='Turkish rose'
COLOR_NAMES[clr_h2i('#B57EDC')]='Lavender (floral)'
COLOR_NAMES[clr_h2i('#B5A642')]='Brass'
COLOR_NAMES[clr_h2i('#B5B5B5')]='Grey'
COLOR_NAMES[clr_h2i('#B666D2')]='Rich lilac'
COLOR_NAMES[clr_h2i('#B7410E')]='Rust'
COLOR_NAMES[clr_h2i('#B768A2')]='Pearly purple'
COLOR_NAMES[clr_h2i('#B76E79')]='Rose gold'
COLOR_NAMES[clr_h2i('#B784A7')]='Opera mauve'
COLOR_NAMES[clr_h2i('#B78727')]='University of California Gold'
COLOR_NAMES[clr_h2i('#B80CE3')]='Vivid mulberry'
COLOR_NAMES[clr_h2i('#B86D29')]='Liver (dogs)'
COLOR_NAMES[clr_h2i('#B87333')]='Copper'
COLOR_NAMES[clr_h2i('#B8860B')]='Dark goldenrod'
COLOR_NAMES[clr_h2i('#B94E48')]='Deep chestnut'
COLOR_NAMES[clr_h2i('#B9D3EE')]='Slate gray'
COLOR_NAMES[clr_h2i('#B9F2FF')]='Diamond'
COLOR_NAMES[clr_h2i('#BA160C')]='International orange'
COLOR_NAMES[clr_h2i('#BA55D3')]='Medium orchid'
COLOR_NAMES[clr_h2i('#BA8759')]='Deer'
COLOR_NAMES[clr_h2i('#BB3385')]='Medium red-violet'
COLOR_NAMES[clr_h2i('#BB6528')]='Ruddy brown'
COLOR_NAMES[clr_h2i('#BBB477')]='Misty Moss'
COLOR_NAMES[clr_h2i('#BBFFFF')]='Pale turquoise'
COLOR_NAMES[clr_h2i('#BC8F8F')]='Rosy brown'
COLOR_NAMES[clr_h2i('#BC987E')]='Pale taupe'
COLOR_NAMES[clr_h2i('#BCB88A')]='Sage'
COLOR_NAMES[clr_h2i('#BCD2EE')]='Light steel blue'
COLOR_NAMES[clr_h2i('#BCD4E6')]='Pale aqua'
COLOR_NAMES[clr_h2i('#BCEE68')]='Dark olive green'
COLOR_NAMES[clr_h2i('#BD33A4')]='Byzantine'
COLOR_NAMES[clr_h2i('#BDB76B')]='Dark khaki'
COLOR_NAMES[clr_h2i('#BDDA57')]='June bud'
COLOR_NAMES[clr_h2i('#BE0032')]='Crimson glory'
COLOR_NAMES[clr_h2i('#BE4F62')]='Popstar'
COLOR_NAMES[clr_h2i('#BEBEBE')]='Gray'
COLOR_NAMES[clr_h2i('#BF00FF')]='Electric purple'
COLOR_NAMES[clr_h2i('#BF3EFF')]='Dark orchid'
COLOR_NAMES[clr_h2i('#BF4F51')]='Bittersweet shimmer'
COLOR_NAMES[clr_h2i('#BF94E4')]='Bright lavender'
COLOR_NAMES[clr_h2i('#BFAFB2')]='Black Shadows'
COLOR_NAMES[clr_h2i('#BFC1C2')]='Silver sand'
COLOR_NAMES[clr_h2i('#BFEFFF')]='Light blue'
COLOR_NAMES[clr_h2i('#BFFF00')]='Bitter lime'
COLOR_NAMES[clr_h2i('#C0362C')]='International orange (Golden Gate Bridge)'
COLOR_NAMES[clr_h2i('#C04000')]='Mahogany'
COLOR_NAMES[clr_h2i('#C08081')]='Old rose'
COLOR_NAMES[clr_h2i('#C09999')]='Tuscany'
COLOR_NAMES[clr_h2i('#C0C0C0')]='Silver'
COLOR_NAMES[clr_h2i('#C0FF3E')]='Olive drab'
COLOR_NAMES[clr_h2i('#C154C1')]='Deep fuchsia'
COLOR_NAMES[clr_h2i('#C19A6B')]='Camel, Desert, Wood brown'
COLOR_NAMES[clr_h2i('#C1CDC1')]='Honeydew'
COLOR_NAMES[clr_h2i('#C1CDCD')]='Azure'
COLOR_NAMES[clr_h2i('#C1FFC1')]='Dark sea green'
COLOR_NAMES[clr_h2i('#C21E56')]='Rose red'
COLOR_NAMES[clr_h2i('#C23B22')]='Dark pastel red'
COLOR_NAMES[clr_h2i('#C2B280')]='Sand'
COLOR_NAMES[clr_h2i('#C30B4E')]='Pictorial carmine'
COLOR_NAMES[clr_h2i('#C32148')]='Bright maroon'
COLOR_NAMES[clr_h2i('#C39953')]='Aztec Gold'
COLOR_NAMES[clr_h2i('#C3B091')]='Khaki'
COLOR_NAMES[clr_h2i('#C40233')]='Red'
COLOR_NAMES[clr_h2i('#C41E3A')]='Cardinal'
COLOR_NAMES[clr_h2i('#C46210')]='Alloy orange'
COLOR_NAMES[clr_h2i('#C4AEAD')]='Silver pink'
COLOR_NAMES[clr_h2i('#C4C3D0')]='Lavender gray'
COLOR_NAMES[clr_h2i('#C4D8E2')]='Columbia Blue'
COLOR_NAMES[clr_h2i('#C53151')]='Dingy Dungeon'
COLOR_NAMES[clr_h2i('#C54B8C')]='Mulberry'
COLOR_NAMES[clr_h2i('#C5B358')]='Vegas gold'
COLOR_NAMES[clr_h2i('#C6E2FF')]='Slate gray'
COLOR_NAMES[clr_h2i('#C71585')]='Medium violet-red'
COLOR_NAMES[clr_h2i('#C72C48')]='French raspberry'
COLOR_NAMES[clr_h2i('#C74375')]='Fuchsia rose'
COLOR_NAMES[clr_h2i('#C80815')]='Venetian red'
COLOR_NAMES[clr_h2i('#C84186')]='Smitten'
COLOR_NAMES[clr_h2i('#C8A2C8')]='Lilac'
COLOR_NAMES[clr_h2i('#C8AD7F')]='Light French beige'
COLOR_NAMES[clr_h2i('#C90016')]='Harvard crimson'
COLOR_NAMES[clr_h2i('#C95A49')]='Cedar Chest'
COLOR_NAMES[clr_h2i('#C9A0DC')]='Wisteria'
COLOR_NAMES[clr_h2i('#C9C0BB')]='Pale silver'
COLOR_NAMES[clr_h2i('#C9DC87')]='Medium spring bud'
COLOR_NAMES[clr_h2i('#C9FFE5')]='Aero blue'
COLOR_NAMES[clr_h2i('#CA1F7B')]='Magenta (dye)'
COLOR_NAMES[clr_h2i('#CA2C92')]='Royal fuchsia'
COLOR_NAMES[clr_h2i('#CAE00D')]='Bitter lemon'
COLOR_NAMES[clr_h2i('#CAE1FF')]='Light steel blue'
COLOR_NAMES[clr_h2i('#CAFF70')]='Dark olive green'
COLOR_NAMES[clr_h2i('#CB410B')]='Sinopia'
COLOR_NAMES[clr_h2i('#CB4154')]='Brick red'
COLOR_NAMES[clr_h2i('#CB6D51')]='Copper red'
COLOR_NAMES[clr_h2i('#CB99C9')]='Pastel violet'
COLOR_NAMES[clr_h2i('#CBA135')]='Satin sheen gold'
COLOR_NAMES[clr_h2i('#CC0000')]='Boston University Red'
COLOR_NAMES[clr_h2i('#CC0033')]='Vivid crimson'
COLOR_NAMES[clr_h2i('#CC00CC')]='Deep magenta'
COLOR_NAMES[clr_h2i('#CC00FF')]='Vivid orchid'
COLOR_NAMES[clr_h2i('#CC3333')]='Persian red'
COLOR_NAMES[clr_h2i('#CC3336')]='Madder Lake'
COLOR_NAMES[clr_h2i('#CC338B')]='Magenta-pink'
COLOR_NAMES[clr_h2i('#CC33CC')]='Steel pink'
COLOR_NAMES[clr_h2i('#CC397B')]='Fuchsia purple'
COLOR_NAMES[clr_h2i('#CC474B')]='English vermillion'
COLOR_NAMES[clr_h2i('#CC4E5C')]='Dark terra cotta'
COLOR_NAMES[clr_h2i('#CC5500')]='Burnt orange'
COLOR_NAMES[clr_h2i('#CC6666')]='Fuzzy Wuzzy'
COLOR_NAMES[clr_h2i('#CC7722')]='Ochre'
COLOR_NAMES[clr_h2i('#CC8899')]='Puce'
COLOR_NAMES[clr_h2i('#CC9900')]='Vivid amber'
COLOR_NAMES[clr_h2i('#cc9966')]='Brown Yellow'
COLOR_NAMES[clr_h2i('#CC99CC')]='Light grayish magenta'
COLOR_NAMES[clr_h2i('#CC99FF')]='Pale violet'
COLOR_NAMES[clr_h2i('#CCA01D')]='Lemon curry'
COLOR_NAMES[clr_h2i('#CCCCFF')]='Lavender blue, Periwinkle'
COLOR_NAMES[clr_h2i('#CCFF00')]='Fluorescent yellow'
COLOR_NAMES[clr_h2i('#CD0000')]='Red'
COLOR_NAMES[clr_h2i('#CD00CD')]='Magenta'
COLOR_NAMES[clr_h2i('#CD1076')]='Deep pink'
COLOR_NAMES[clr_h2i('#CD2626')]='Firebrick'
COLOR_NAMES[clr_h2i('#CD2990')]='Maroon'
COLOR_NAMES[clr_h2i('#CD3278')]='Violet red'
COLOR_NAMES[clr_h2i('#CD3333')]='Brown'
COLOR_NAMES[clr_h2i('#CD3700')]='Orange red'
COLOR_NAMES[clr_h2i('#CD4F39')]='Tomato'
COLOR_NAMES[clr_h2i('#CD5555')]='Indian red'
COLOR_NAMES[clr_h2i('#CD5700')]='Tenné (tawny)'
COLOR_NAMES[clr_h2i('#CD5B45')]='Dark coral'
COLOR_NAMES[clr_h2i('#CD5C5C')]='Indian red'
COLOR_NAMES[clr_h2i('#CD607E')]='Cinnamon Satin'
COLOR_NAMES[clr_h2i('#CD6090')]='Hot pink'
COLOR_NAMES[clr_h2i('#CD6600')]='Dark orange'
COLOR_NAMES[clr_h2i('#CD661D')]='Chocolate'
COLOR_NAMES[clr_h2i('#CD6839')]='Sienna'
COLOR_NAMES[clr_h2i('#CD6889')]='Pale violet red'
COLOR_NAMES[clr_h2i('#CD69C9')]='Orchid'
COLOR_NAMES[clr_h2i('#CD7054')]='Salmon'
COLOR_NAMES[clr_h2i('#CD7F32')]='Bronze'
COLOR_NAMES[clr_h2i('#CD8162')]='Light salmon'
COLOR_NAMES[clr_h2i('#CD8500')]='Orange'
COLOR_NAMES[clr_h2i('#CD853F')]='Peru'
COLOR_NAMES[clr_h2i('#CD8C95')]='Light pink'
COLOR_NAMES[clr_h2i('#CD919E')]='Pink'
COLOR_NAMES[clr_h2i('#CD950C')]='Dark goldenrod'
COLOR_NAMES[clr_h2i('#CD9575')]='Antique brass'
COLOR_NAMES[clr_h2i('#CD96CD')]='Plum'
COLOR_NAMES[clr_h2i('#CD9B1D')]='Goldenrod'
COLOR_NAMES[clr_h2i('#CD9B9B')]='Rosy brown'
COLOR_NAMES[clr_h2i('#CDA4DE')]='Tropical violet'
COLOR_NAMES[clr_h2i('#CDAA7D')]='Burlywood'
COLOR_NAMES[clr_h2i('#CDAD00')]='Gold'
COLOR_NAMES[clr_h2i('#CDAF95')]='Peach puff'
COLOR_NAMES[clr_h2i('#CDB38B')]='Navajo white'
COLOR_NAMES[clr_h2i('#CDB5CD')]='Thistle'
COLOR_NAMES[clr_h2i('#CDB79E')]='Bisque'
COLOR_NAMES[clr_h2i('#CDB7B5')]='Misty rose'
COLOR_NAMES[clr_h2i('#CDBA96')]='Wheat'
COLOR_NAMES[clr_h2i('#CDBE70')]='Light goldenrod'
COLOR_NAMES[clr_h2i('#CDC0B0')]='Antique white'
COLOR_NAMES[clr_h2i('#CDC1C5')]='Lavender blush'
COLOR_NAMES[clr_h2i('#CDC5BF')]='Seashell'
COLOR_NAMES[clr_h2i('#CDC673')]='Khaki'
COLOR_NAMES[clr_h2i('#CDC8B1')]='Cornsilk'
COLOR_NAMES[clr_h2i('#CDC9A5')]='Lemon chiffon'
COLOR_NAMES[clr_h2i('#CDC9C9')]='Snow'
COLOR_NAMES[clr_h2i('#CDCD00')]='Yellow'
COLOR_NAMES[clr_h2i('#CDCDB4')]='Light yellow'
COLOR_NAMES[clr_h2i('#CDCDC1')]='Ivory'
COLOR_NAMES[clr_h2i('#CE2029')]='Fire engine red'
COLOR_NAMES[clr_h2i('#CE4676')]='Ruber'
COLOR_NAMES[clr_h2i('#CEC8EF')]='Soap'
COLOR_NAMES[clr_h2i('#CEFF00')]='Volt'
COLOR_NAMES[clr_h2i('#CF1020')]='Lava'
COLOR_NAMES[clr_h2i('#CF3476')]='Telemagenta'
COLOR_NAMES[clr_h2i('#CF6BA9')]='Super pink'
COLOR_NAMES[clr_h2i('#CF71AF')]='Sky magenta'
COLOR_NAMES[clr_h2i('#CFB53B')]='Old gold'
COLOR_NAMES[clr_h2i('#CFCFC4')]='Pastel gray'
COLOR_NAMES[clr_h2i('#CFCFCF')]='Gray'
COLOR_NAMES[clr_h2i('#D02090')]='Violet red'
COLOR_NAMES[clr_h2i('#D0417E')]='Magenta'
COLOR_NAMES[clr_h2i('#D0F0C0')]='Tea green'
COLOR_NAMES[clr_h2i('#D0FF14')]='Arctic lime'
COLOR_NAMES[clr_h2i('#D10047')]='Spanish carmine'
COLOR_NAMES[clr_h2i('#D10056')]='Rubine red'
COLOR_NAMES[clr_h2i('#D15FEE')]='Medium orchid'
COLOR_NAMES[clr_h2i('#D19FE8')]='Bright ube'
COLOR_NAMES[clr_h2i('#D1BEA8')]='Dark vanilla'
COLOR_NAMES[clr_h2i('#D1E231')]='Pear'
COLOR_NAMES[clr_h2i('#D1EEEE')]='Light cyan'
COLOR_NAMES[clr_h2i('#D2691E')]='Chocolate, Cocoa brown'
COLOR_NAMES[clr_h2i('#D2B48C')]='Tan'
COLOR_NAMES[clr_h2i('#D3003F')]='Utah Crimson'
COLOR_NAMES[clr_h2i('#D3212D')]='Amaranth red'
COLOR_NAMES[clr_h2i('#D39BCB')]='Light medium orchid'
COLOR_NAMES[clr_h2i('#D3D3D3')]='Light gray'
COLOR_NAMES[clr_h2i('#D40000')]='Rosso corsa'
COLOR_NAMES[clr_h2i('#D470A2')]='Wild orchid'
COLOR_NAMES[clr_h2i('#D473D4')]='Deep mauve'
COLOR_NAMES[clr_h2i('#D4AF37')]='Gold (metallic)'
COLOR_NAMES[clr_h2i('#D65282')]='Mystic'
COLOR_NAMES[clr_h2i('#D68A59')]='Raw Sienna'
COLOR_NAMES[clr_h2i('#D6CADD')]='Languid lavender'
COLOR_NAMES[clr_h2i('#D70040')]='Rich carmine'
COLOR_NAMES[clr_h2i('#D70A53')]='Debian red'
COLOR_NAMES[clr_h2i('#D71868')]='Dogwood rose'
COLOR_NAMES[clr_h2i('#D73B3E')]='Jasper'
COLOR_NAMES[clr_h2i('#D74894')]='Pink'
COLOR_NAMES[clr_h2i('#D7837F')]='New York pink'
COLOR_NAMES[clr_h2i('#D891EF')]='Bright lilac'
COLOR_NAMES[clr_h2i('#D8B2D1')]='Pink lavender'
COLOR_NAMES[clr_h2i('#D8BFD8')]='Thistle'
COLOR_NAMES[clr_h2i('#D9004C')]='UA red'
COLOR_NAMES[clr_h2i('#D92121')]='Maximum Red'
COLOR_NAMES[clr_h2i('#D9381E')]='Vermilion'
COLOR_NAMES[clr_h2i('#D9603B')]='Medium vermilion'
COLOR_NAMES[clr_h2i('#D982B5')]='Middle Purple'
COLOR_NAMES[clr_h2i('#D98695')]='Shimmering Blush'
COLOR_NAMES[clr_h2i('#D99058')]='Persian orange'
COLOR_NAMES[clr_h2i('#D998A0')]='Parrot Pink'
COLOR_NAMES[clr_h2i('#D9E650')]='Maximum Green Yellow'
COLOR_NAMES[clr_h2i('#DA1D81')]='Vivid cerise'
COLOR_NAMES[clr_h2i('#DA2C43')]='Rusty red'
COLOR_NAMES[clr_h2i('#DA3287')]='Deep cerise'
COLOR_NAMES[clr_h2i('#DA614E')]='Jelly Bean'
COLOR_NAMES[clr_h2i('#DA70D6')]='Orchid'
COLOR_NAMES[clr_h2i('#DA8A67')]='Pale copper'
COLOR_NAMES[clr_h2i('#DA9100')]='Harvest gold'
COLOR_NAMES[clr_h2i('#DAA520')]='Goldenrod'
COLOR_NAMES[clr_h2i('#DB7093')]='Pale red-violet'
COLOR_NAMES[clr_h2i('#DBD7D2')]='Timberwolf'
COLOR_NAMES[clr_h2i('#DBE9F4')]='Azureish white'
COLOR_NAMES[clr_h2i('#DC143C')]='Crimson'
COLOR_NAMES[clr_h2i('#DCD0FF')]='Pale lavender'
COLOR_NAMES[clr_h2i('#DCDCDC')]='Gainsboro'
COLOR_NAMES[clr_h2i('#DDA0DD')]='Medium lavender magenta, Pale plum'
COLOR_NAMES[clr_h2i('#DDADAF')]='Pale chestnut'
COLOR_NAMES[clr_h2i('#DDE26A')]='Booger Buster'
COLOR_NAMES[clr_h2i('#DE3163')]='Cherry'
COLOR_NAMES[clr_h2i('#DE5285')]='Fandango pink'
COLOR_NAMES[clr_h2i('#DE5D83')]='Blush'
COLOR_NAMES[clr_h2i('#DE6FA1')]='China pink, Liseran Purple'
COLOR_NAMES[clr_h2i('#DEA5A4')]='Pastel pink'
COLOR_NAMES[clr_h2i('#DEAA88')]='Tumbleweed'
COLOR_NAMES[clr_h2i('#DEB887')]='Burlywood'
COLOR_NAMES[clr_h2i('#DF00FF')]='Phlox, Psychedelic purple'
COLOR_NAMES[clr_h2i('#DF6124')]='Vivid red-tangelo'
COLOR_NAMES[clr_h2i('#DF73FF')]='Heliotrope'
COLOR_NAMES[clr_h2i('#DFFF00')]='Chartreuse (traditional)'
COLOR_NAMES[clr_h2i('#E0115F')]='Ruby'
COLOR_NAMES[clr_h2i('#E0218A')]='Barbie pink'
COLOR_NAMES[clr_h2i('#E03C31')]='CG Red'
COLOR_NAMES[clr_h2i('#E066FF')]='Medium orchid'
COLOR_NAMES[clr_h2i('#E08D3C')]='Tiger\'s eye'
COLOR_NAMES[clr_h2i('#E0B0FF')]='Mauve'
COLOR_NAMES[clr_h2i('#E0EEE0')]='Honeydew'
COLOR_NAMES[clr_h2i('#E0EEEE')]='Azure'
COLOR_NAMES[clr_h2i('#E0FFFF')]='Light cyan'
COLOR_NAMES[clr_h2i('#E12C2C')]='Permanent Geranium Lake'
COLOR_NAMES[clr_h2i('#E18E96')]='Ruddy pink'
COLOR_NAMES[clr_h2i('#E1A95F')]='Earth yellow'
COLOR_NAMES[clr_h2i('#E1AD21')]='Urobilin'
COLOR_NAMES[clr_h2i('#E2062C')]='Medium candy apple red'
COLOR_NAMES[clr_h2i('#E25098')]='Raspberry pink'
COLOR_NAMES[clr_h2i('#E25822')]='Flame'
COLOR_NAMES[clr_h2i('#E2725B')]='Terra cotta'
COLOR_NAMES[clr_h2i('#E30022')]='Cadmium red'
COLOR_NAMES[clr_h2i('#E30B5D')]='Raspberry'
COLOR_NAMES[clr_h2i('#E3256B')]='Razzmatazz'
COLOR_NAMES[clr_h2i('#E32636')]='Alizarin crimson, Rose madder'
COLOR_NAMES[clr_h2i('#E34234')]='Cinnabar, Vermilion'
COLOR_NAMES[clr_h2i('#E3A857')]='Indian yellow'
COLOR_NAMES[clr_h2i('#E3AB57')]='Sunray'
COLOR_NAMES[clr_h2i('#E3DAC9')]='Bone'
COLOR_NAMES[clr_h2i('#E3F988')]='Mindaro'
COLOR_NAMES[clr_h2i('#E3FF00')]='Lemon lime'
COLOR_NAMES[clr_h2i('#E40078')]='Red-purple'
COLOR_NAMES[clr_h2i('#E4007C')]='Mexican pink'
COLOR_NAMES[clr_h2i('#E4717A')]='Tango pink'
COLOR_NAMES[clr_h2i('#E48400')]='Fulvous'
COLOR_NAMES[clr_h2i('#E49B0F')]='Gamboge'
COLOR_NAMES[clr_h2i('#E4D00A')]='Citrine'
COLOR_NAMES[clr_h2i('#E4D96F')]='Straw'
COLOR_NAMES[clr_h2i('#E51A4C')]='Spanish crimson'
COLOR_NAMES[clr_h2i('#E52B50')]='Amaranth'
COLOR_NAMES[clr_h2i('#E56024')]='Vivid vermilion'
COLOR_NAMES[clr_h2i('#E58E73')]='Middle Red'
COLOR_NAMES[clr_h2i('#E5AA70')]='Fawn'
COLOR_NAMES[clr_h2i('#E5B73B')]='Meat brown'
COLOR_NAMES[clr_h2i('#E5CCC9')]='Dust storm'
COLOR_NAMES[clr_h2i('#E5E4E2')]='Platinum'
COLOR_NAMES[clr_h2i('#E60026')]='Spanish red'
COLOR_NAMES[clr_h2i('#E62020')]='Lust'
COLOR_NAMES[clr_h2i('#E63E62')]='Paradise pink'
COLOR_NAMES[clr_h2i('#E66771')]='Light carmine pink'
COLOR_NAMES[clr_h2i('#E68FAC')]='Light Thulian pink'
COLOR_NAMES[clr_h2i('#E6A8D7')]='Light orchid'
COLOR_NAMES[clr_h2i('#E6BE8A')]='Pale gold'
COLOR_NAMES[clr_h2i('#E6E200')]='Peridot'
COLOR_NAMES[clr_h2i('#E6E6FA')]='Lavender mist'
COLOR_NAMES[clr_h2i('#E6E8FA')]='Glitter'
COLOR_NAMES[clr_h2i('#E75480')]='Dark pink'
COLOR_NAMES[clr_h2i('#E79FC4')]='Kobi'
COLOR_NAMES[clr_h2i('#E7ACCF')]='Pink pearl'
COLOR_NAMES[clr_h2i('#E7FEFF')]='Bubbles'
COLOR_NAMES[clr_h2i('#E8000D')]='KU Crimson'
COLOR_NAMES[clr_h2i('#E86100')]='Spanish orange'
COLOR_NAMES[clr_h2i('#E88E5A')]='Big Foot Feet'
COLOR_NAMES[clr_h2i('#E8CCD7')]='Queen pink'
COLOR_NAMES[clr_h2i('#E8E8E8')]='Grey'
COLOR_NAMES[clr_h2i('#E8F48C')]='Key Lime'
COLOR_NAMES[clr_h2i('#E936A7')]='Frostbite'
COLOR_NAMES[clr_h2i('#E9692C')]='Deep carrot orange'
COLOR_NAMES[clr_h2i('#E97451')]='Burnt sienna, Light red ochre'
COLOR_NAMES[clr_h2i('#E9967A')]='Dark salmon'
COLOR_NAMES[clr_h2i('#E9D66B')]='Arylide yellow'
COLOR_NAMES[clr_h2i('#E9FFDB')]='Nyanza'
COLOR_NAMES[clr_h2i('#EA3C53')]='Desire'
COLOR_NAMES[clr_h2i('#EAA221')]='Marigold'
COLOR_NAMES[clr_h2i('#EAE0C8')]='Pearl'
COLOR_NAMES[clr_h2i('#EB4C42')]='Carmine pink'
COLOR_NAMES[clr_h2i('#EC3B83')]='Cerise pink'
COLOR_NAMES[clr_h2i('#EC5800')]='Persimmon'
COLOR_NAMES[clr_h2i('#ECB176')]='Middle Yellow Red'
COLOR_NAMES[clr_h2i('#ECD540')]='Sandstorm'
COLOR_NAMES[clr_h2i('#ECEBBD')]='Pale spring bud'
COLOR_NAMES[clr_h2i('#ED1C24')]='Red'
COLOR_NAMES[clr_h2i('#ED2939')]='Imperial red'
COLOR_NAMES[clr_h2i('#ED872D')]='Cadmium orange'
COLOR_NAMES[clr_h2i('#ED9121')]='Carrot orange'
COLOR_NAMES[clr_h2i('#EDC9AF')]='Desert sand'
COLOR_NAMES[clr_h2i('#EE0000')]='Red'
COLOR_NAMES[clr_h2i('#EE00EE')]='Magenta'
COLOR_NAMES[clr_h2i('#EE1289')]='Deep pink'
COLOR_NAMES[clr_h2i('#EE204D')]='Red'
COLOR_NAMES[clr_h2i('#EE2C2C')]='Firebrick'
COLOR_NAMES[clr_h2i('#EE30A7')]='Maroon'
COLOR_NAMES[clr_h2i('#EE3A8C')]='Violet red'
COLOR_NAMES[clr_h2i('#EE3B3B')]='Brown'
COLOR_NAMES[clr_h2i('#EE4000')]='Orange red'
COLOR_NAMES[clr_h2i('#EE5C42')]='Tomato'
COLOR_NAMES[clr_h2i('#EE6363')]='Indian red'
COLOR_NAMES[clr_h2i('#EE6A50')]='Coral'
COLOR_NAMES[clr_h2i('#EE6AA7')]='Hot pink'
COLOR_NAMES[clr_h2i('#EE7600')]='Dark orange'
COLOR_NAMES[clr_h2i('#EE7621')]='Chocolate'
COLOR_NAMES[clr_h2i('#EE7942')]='Sienna'
COLOR_NAMES[clr_h2i('#EE799F')]='Pale violet red'
COLOR_NAMES[clr_h2i('#EE7AE9')]='Orchid'
COLOR_NAMES[clr_h2i('#EE8262')]='Salmon'
COLOR_NAMES[clr_h2i('#EE82EE')]='Lavender magenta, Violet'
COLOR_NAMES[clr_h2i('#EE9572')]='Light salmon'
COLOR_NAMES[clr_h2i('#EE9A00')]='Orange'
COLOR_NAMES[clr_h2i('#EE9A49')]='Tan'
COLOR_NAMES[clr_h2i('#EEA2AD')]='Light pink'
COLOR_NAMES[clr_h2i('#EEA9B8')]='Pink'
COLOR_NAMES[clr_h2i('#EEAD0E')]='Dark goldenrod'
COLOR_NAMES[clr_h2i('#EEAEEE')]='Plum'
COLOR_NAMES[clr_h2i('#EEB422')]='Goldenrod'
COLOR_NAMES[clr_h2i('#EEB4B4')]='Rosy brown'
COLOR_NAMES[clr_h2i('#EEC591')]='Burlywood'
COLOR_NAMES[clr_h2i('#EEC900')]='Gold'
COLOR_NAMES[clr_h2i('#EECBAD')]='Peach puff'
COLOR_NAMES[clr_h2i('#EECFA1')]='Navajo white'
COLOR_NAMES[clr_h2i('#EED202')]='Safety yellow'
COLOR_NAMES[clr_h2i('#EED2EE')]='Thistle'
COLOR_NAMES[clr_h2i('#EED5B7')]='Bisque'
COLOR_NAMES[clr_h2i('#EED5D2')]='Misty rose'
COLOR_NAMES[clr_h2i('#EED8AE')]='Wheat'
COLOR_NAMES[clr_h2i('#EEDC82')]='Flax, Light goldenrod'
COLOR_NAMES[clr_h2i('#EEDFCC')]='Antique white'
COLOR_NAMES[clr_h2i('#EEE0E5')]='Lavender blush'
COLOR_NAMES[clr_h2i('#EEE5DE')]='Seashell'
COLOR_NAMES[clr_h2i('#EEE600')]='Titanium yellow'
COLOR_NAMES[clr_h2i('#EEE685')]='Khaki'
COLOR_NAMES[clr_h2i('#EEE8AA')]='Pale goldenrod'
COLOR_NAMES[clr_h2i('#EEE8CD')]='Cornsilk'
COLOR_NAMES[clr_h2i('#EEE9BF')]='Lemon chiffon'
COLOR_NAMES[clr_h2i('#EEE9E9')]='Snow'
COLOR_NAMES[clr_h2i('#EEEE00')]='Yellow'
COLOR_NAMES[clr_h2i('#EEEED1')]='Light yellow'
COLOR_NAMES[clr_h2i('#EEEEE0')]='vory'
COLOR_NAMES[clr_h2i('#EF3038')]='Deep carmine pink'
COLOR_NAMES[clr_h2i('#EF98AA')]='Mauvelous'
COLOR_NAMES[clr_h2i('#EFBBCC')]='Cameo pink'
COLOR_NAMES[clr_h2i('#EFCC00')]='Yellow'
COLOR_NAMES[clr_h2i('#EFDECD')]='Almond'
COLOR_NAMES[clr_h2i('#EFDFBB')]='Dutch white'
COLOR_NAMES[clr_h2i('#F07427')]='Vivid tangelo'
COLOR_NAMES[clr_h2i('#F08080')]='Light coral'
COLOR_NAMES[clr_h2i('#F0DC82')]='Buff'
COLOR_NAMES[clr_h2i('#F0E130')]='Dandelion'
COLOR_NAMES[clr_h2i('#F0E68C')]='Light khaki'
COLOR_NAMES[clr_h2i('#F0EAD6')]='Eggshell'
COLOR_NAMES[clr_h2i('#F0F8FF')]='Alice blue'
COLOR_NAMES[clr_h2i('#F0FFF0')]='Honeydew'
COLOR_NAMES[clr_h2i('#F0FFFF')]='Azure mist'
COLOR_NAMES[clr_h2i('#F19CBB')]='Amaranth pink'
COLOR_NAMES[clr_h2i('#F1A7FE')]='Rich brilliant lavender'
COLOR_NAMES[clr_h2i('#F1DDCF')]='Champagne pink'
COLOR_NAMES[clr_h2i('#F2003C')]='Red'
COLOR_NAMES[clr_h2i('#F28500')]='Tangerine'
COLOR_NAMES[clr_h2i('#F2BA49')]='Maximum Yellow Red'
COLOR_NAMES[clr_h2i('#F2BDCD')]='Orchid pink'
COLOR_NAMES[clr_h2i('#F2F0E6')]='Alabaster'
COLOR_NAMES[clr_h2i('#F2F27A')]='Sunny'
COLOR_NAMES[clr_h2i('#F2F3F4')]='Anti-flash white'
COLOR_NAMES[clr_h2i('#F37A48')]='Mandarin'
COLOR_NAMES[clr_h2i('#F38FA9')]='Vanilla ice'
COLOR_NAMES[clr_h2i('#F3E5AB')]='Medium champagne, Vanilla'
COLOR_NAMES[clr_h2i('#F400A1')]='Fashion fuchsia, Hollywood cerise'
COLOR_NAMES[clr_h2i('#F49AC2')]='Pastel magenta'
COLOR_NAMES[clr_h2i('#F4A460')]='Sandy brown'
COLOR_NAMES[clr_h2i('#F4BBFF')]='Brilliant lavender'
COLOR_NAMES[clr_h2i('#F4C2C2')]='Baby pink, Tea rose'
COLOR_NAMES[clr_h2i('#F4C430')]='Saffron'
COLOR_NAMES[clr_h2i('#F4CA16')]='Jonquil'
COLOR_NAMES[clr_h2i('#F4F0EC')]='Isabelline'
COLOR_NAMES[clr_h2i('#F56991')]='Light crimson'
COLOR_NAMES[clr_h2i('#F56FA1')]='Cyclamen'
COLOR_NAMES[clr_h2i('#F58025')]='Princeton orange'
COLOR_NAMES[clr_h2i('#F5C71A')]='Deep lemon'
COLOR_NAMES[clr_h2i('#F5DEB3')]='Wheat'
COLOR_NAMES[clr_h2i('#F5E050')]='Minion Yellow'
COLOR_NAMES[clr_h2i('#F5F5DC')]='Beige'
COLOR_NAMES[clr_h2i('#F5F5F5')]='White smoke'
COLOR_NAMES[clr_h2i('#F5FFFA')]='Mint cream'
COLOR_NAMES[clr_h2i('#F64A8A')]='French rose'
COLOR_NAMES[clr_h2i('#F6ADC6')]='Nadeshiko pink'
COLOR_NAMES[clr_h2i('#F6EABE')]='Lemon meringue'
COLOR_NAMES[clr_h2i('#F70D1A')]='Vivid red'
COLOR_NAMES[clr_h2i('#F75394')]='Violet-red'
COLOR_NAMES[clr_h2i('#F77F00')]='University of Tennessee Orange'
COLOR_NAMES[clr_h2i('#F77FBE')]='Persian pink'
COLOR_NAMES[clr_h2i('#F78FA7')]='Pink Sherbet'
COLOR_NAMES[clr_h2i('#F7BFBE')]='Spanish pink'
COLOR_NAMES[clr_h2i('#F7E7CE')]='Champagne'
COLOR_NAMES[clr_h2i('#F7E98E')]='Flavescent'
COLOR_NAMES[clr_h2i('#F88379')]='Coral pink, Tea rose'
COLOR_NAMES[clr_h2i('#F8B878')]='Mellow apricot'
COLOR_NAMES[clr_h2i('#F8D568')]='Orange-yellow'
COLOR_NAMES[clr_h2i('#F8DE7E')]='Jasmine, Mellow yellow'
COLOR_NAMES[clr_h2i('#F8F4FF')]='Magnolia'
COLOR_NAMES[clr_h2i('#F8F8FF')]='Ghost white'
COLOR_NAMES[clr_h2i('#F9429E')]='Rose bonbon'
COLOR_NAMES[clr_h2i('#F94D00')]='Tangelo'
COLOR_NAMES[clr_h2i('#F984E5')]='Pale magenta'
COLOR_NAMES[clr_h2i('#F984EF')]='Light fuchsia pink'
COLOR_NAMES[clr_h2i('#FA5B3D')]='Orange Soda'
COLOR_NAMES[clr_h2i('#FA6E79')]='Begonia'
COLOR_NAMES[clr_h2i('#FA8072')]='Salmon'
COLOR_NAMES[clr_h2i('#FAD6A5')]='Deep champagne, Sunset, Tuscan'
COLOR_NAMES[clr_h2i('#FADA5E')]='Royal yellow'
COLOR_NAMES[clr_h2i('#FADADD')]='Pale pink'
COLOR_NAMES[clr_h2i('#FADFAD')]='Peach-yellow'
COLOR_NAMES[clr_h2i('#FAE7B5')]='Banana Mania'
COLOR_NAMES[clr_h2i('#FAEBD7')]='Antique white, Moccasin'
COLOR_NAMES[clr_h2i('#FAF0BE')]='Blond'
COLOR_NAMES[clr_h2i('#FAF0E6')]='Linen'
COLOR_NAMES[clr_h2i('#FAFA37')]='Maximum Yellow'
COLOR_NAMES[clr_h2i('#FAFAD2')]='Light goldenrod yellow'
COLOR_NAMES[clr_h2i('#FB4D46')]='Tart Orange'
COLOR_NAMES[clr_h2i('#FB4F14')]='Orioles orange'
COLOR_NAMES[clr_h2i('#FB607F')]='Brink pink'
COLOR_NAMES[clr_h2i('#FB9902')]='Orange'
COLOR_NAMES[clr_h2i('#FBA0E3')]='Lavender rose'
COLOR_NAMES[clr_h2i('#FBAB60')]='Rajah'
COLOR_NAMES[clr_h2i('#FBAED2')]='Lavender pink'
COLOR_NAMES[clr_h2i('#FBCCE7')]='Classic rose'
COLOR_NAMES[clr_h2i('#FBCEB1')]='Apricot'
COLOR_NAMES[clr_h2i('#FBEC5D')]='Corn'
COLOR_NAMES[clr_h2i('#FC0FC0')]='Shocking pink'
COLOR_NAMES[clr_h2i('#FC5A8D')]='Strawberry'
COLOR_NAMES[clr_h2i('#FC6C85')]='Ultra red, Wild watermelon'
COLOR_NAMES[clr_h2i('#FC74FD')]='Pink Flamingo'
COLOR_NAMES[clr_h2i('#FC89AC')]='Tickle Me Pink'
COLOR_NAMES[clr_h2i('#FC8EAC')]='Flamingo pink'
COLOR_NAMES[clr_h2i('#FCC200')]='Golden poppy'
COLOR_NAMES[clr_h2i('#FCE883')]='Yellow'
COLOR_NAMES[clr_h2i('#FCF75E')]='Icterine'
COLOR_NAMES[clr_h2i('#FD0E35')]='Scarlet, Tractor red'
COLOR_NAMES[clr_h2i('#FD3A4A')]='Red Salsa'
COLOR_NAMES[clr_h2i('#FD3F92')]='French fuchsia'
COLOR_NAMES[clr_h2i('#FD5240')]='Ogre Odor'
COLOR_NAMES[clr_h2i('#FD5800')]='Willpower orange'
COLOR_NAMES[clr_h2i('#FD5E53')]='Sunset orange'
COLOR_NAMES[clr_h2i('#FD6C9E')]='French pink'
COLOR_NAMES[clr_h2i('#FD7C6E')]='Coral Reef'
COLOR_NAMES[clr_h2i('#FDBCB4')]='Melon'
COLOR_NAMES[clr_h2i('#FDD5B1')]='Feldspar, Light apricot'
COLOR_NAMES[clr_h2i('#FDD9B5')]='Sandy Tan'
COLOR_NAMES[clr_h2i('#FDDDE6')]='Piggy pink'
COLOR_NAMES[clr_h2i('#FDEE00')]='Aureolin'
COLOR_NAMES[clr_h2i('#FDF5E6')]='Old lace'
COLOR_NAMES[clr_h2i('#FDFD96')]='Pastel yellow'
COLOR_NAMES[clr_h2i('#FDFF00')]='Lemon glacier'
COLOR_NAMES[clr_h2i('#FDFFF5')]='Milk'
COLOR_NAMES[clr_h2i('#FE2712')]='Red'
COLOR_NAMES[clr_h2i('#FE28A2')]='Persian rose'
COLOR_NAMES[clr_h2i('#FE4164')]='Neon fuchsia'
COLOR_NAMES[clr_h2i('#FE4EDA')]='Purple pizzazz'
COLOR_NAMES[clr_h2i('#FE5A1D')]='Giants orange'
COLOR_NAMES[clr_h2i('#FE6F5E')]='Bittersweet'
COLOR_NAMES[clr_h2i('#FEDF00')]='Yellow'
COLOR_NAMES[clr_h2i('#FEFE33')]='Yellow'
COLOR_NAMES[clr_h2i('#FEFEFA')]='Baby powder'
COLOR_NAMES[clr_h2i('#FF0000')]='Red'
COLOR_NAMES[clr_h2i('#FF0028')]='Ruddy'
COLOR_NAMES[clr_h2i('#FF0038')]='Carmine red'
COLOR_NAMES[clr_h2i('#FF003F')]='Electric crimson'
COLOR_NAMES[clr_h2i('#FF004F')]='Folly'
COLOR_NAMES[clr_h2i('#FF006C')]='Vivid raspberry'
COLOR_NAMES[clr_h2i('#FF007C')]='Winter Sky'
COLOR_NAMES[clr_h2i('#FF007F')]='Bright pink, Rose'
COLOR_NAMES[clr_h2i('#FF0090')]='Magenta'
COLOR_NAMES[clr_h2i('#FF00FF')]='Fuchsia, Magenta'
COLOR_NAMES[clr_h2i('#FF033E')]='American rose'
COLOR_NAMES[clr_h2i('#FF0800')]='Candy apple red'
COLOR_NAMES[clr_h2i('#FF1493')]='Deep pink'
COLOR_NAMES[clr_h2i('#FF1DCE')]='Hot magenta'
COLOR_NAMES[clr_h2i('#FF2052')]='Awesome'
COLOR_NAMES[clr_h2i('#FF2400')]='Scarlet'
COLOR_NAMES[clr_h2i('#FF2800')]='Ferrari Red'
COLOR_NAMES[clr_h2i('#FF3030')]='Firebrick'
COLOR_NAMES[clr_h2i('#FF33CC')]='Razzle dazzle rose'
COLOR_NAMES[clr_h2i('#FF34B3')]='Maroon'
COLOR_NAMES[clr_h2i('#FF355E')]='Radical Red'
COLOR_NAMES[clr_h2i('#FF3800')]='Coquelicot'
COLOR_NAMES[clr_h2i('#FF3855')]='Sizzling Red'
COLOR_NAMES[clr_h2i('#FF3E96')]='Violet red'
COLOR_NAMES[clr_h2i('#FF4040')]='Brown, Coral red'
COLOR_NAMES[clr_h2i('#FF404C')]='Sunburnt Cyclops'
COLOR_NAMES[clr_h2i('#FF43A4')]='Wild Strawberry'
COLOR_NAMES[clr_h2i('#FF4466')]='Magic Potion'
COLOR_NAMES[clr_h2i('#FF4500')]='Orange-red'
COLOR_NAMES[clr_h2i('#FF4681')]='Sasquatch Socks'
COLOR_NAMES[clr_h2i('#FF496C')]='Infra Red'
COLOR_NAMES[clr_h2i('#FF4F00')]='International orange (aerospace)'
COLOR_NAMES[clr_h2i('#FF5349')]='Red-orange'
COLOR_NAMES[clr_h2i('#FF5470')]='Fiery Rose'
COLOR_NAMES[clr_h2i('#FF55A3')]='Brilliant rose'
COLOR_NAMES[clr_h2i('#FF5800')]='Orange'
COLOR_NAMES[clr_h2i('#FF5A36')]='Portland Orange'
COLOR_NAMES[clr_h2i('#FF5CCD')]='Light deep pink'
COLOR_NAMES[clr_h2i('#FF5F00')]='Vivid orange'
COLOR_NAMES[clr_h2i('#FF6347')]='Tomato'
COLOR_NAMES[clr_h2i('#FF66CC')]='Rose pink'
COLOR_NAMES[clr_h2i('#FF6700')]='Safety orange'
COLOR_NAMES[clr_h2i('#FF6961')]='Pastel red'
COLOR_NAMES[clr_h2i('#FF69B4')]='Hot pink'
COLOR_NAMES[clr_h2i('#FF6A6A')]='Indian red'
COLOR_NAMES[clr_h2i('#FF6D3A')]='Smashed Pumpkin'
COLOR_NAMES[clr_h2i('#FF6E4A')]='Outrageous Orange'
COLOR_NAMES[clr_h2i('#FF6EB4')]='Hot pink'
COLOR_NAMES[clr_h2i('#FF6FFF')]='Ultra pink'
COLOR_NAMES[clr_h2i('#FF7256')]='Coral'
COLOR_NAMES[clr_h2i('#FF7518')]='Pumpkin'
COLOR_NAMES[clr_h2i('#FF77FF')]='Fuchsia pink'
COLOR_NAMES[clr_h2i('#FF7800')]='Safety orange'
COLOR_NAMES[clr_h2i('#FF7A00')]='Heat Wave'
COLOR_NAMES[clr_h2i('#FF7E00')]='Amber'
COLOR_NAMES[clr_h2i('#FF7F00')]='Dark orange'
COLOR_NAMES[clr_h2i('#FF7F24')]='Chocolate'
COLOR_NAMES[clr_h2i('#FF7F50')]='Coral'
COLOR_NAMES[clr_h2i('#FF8243')]='Mango Tango'
COLOR_NAMES[clr_h2i('#FF8247')]='Sienna'
COLOR_NAMES[clr_h2i('#FF82AB')]='Pale violet red'
COLOR_NAMES[clr_h2i('#FF83FA')]='Orchid'
COLOR_NAMES[clr_h2i('#FF85CF')]='Princess Perfume'
COLOR_NAMES[clr_h2i('#FF878D')]='Tulip'
COLOR_NAMES[clr_h2i('#FF8C00')]='Dark orange'
COLOR_NAMES[clr_h2i('#FF8C69')]='Salmon'
COLOR_NAMES[clr_h2i('#FF91A4')]='Salmon pink'
COLOR_NAMES[clr_h2i('#FF91AF')]='Baker-Miller pink, Schauss pink'
COLOR_NAMES[clr_h2i('#FF9900')]='Vivid gamboge'
COLOR_NAMES[clr_h2i('#FF9933')]='Deep saffron'
COLOR_NAMES[clr_h2i('#FF9966')]='Atomic tangerine'
COLOR_NAMES[clr_h2i('#FF9999')]='Light salmon pink'
COLOR_NAMES[clr_h2i('#FF99CC')]='Pale magenta-pink'
COLOR_NAMES[clr_h2i('#FF9F00')]='Orange peel'
COLOR_NAMES[clr_h2i('#FFA000')]='Vivid orange peel'
COLOR_NAMES[clr_h2i('#FFA07A')]='Light salmon'
COLOR_NAMES[clr_h2i('#FFA089')]='Vivid tangerine'
COLOR_NAMES[clr_h2i('#FFA343')]='Neon Carrot'
COLOR_NAMES[clr_h2i('#FFA500')]='Orange'
COLOR_NAMES[clr_h2i('#FFA54F')]='Tan'
COLOR_NAMES[clr_h2i('#FFA6C9')]='Carnation pink'
COLOR_NAMES[clr_h2i('#FFA700')]='Chrome yellow'
COLOR_NAMES[clr_h2i('#FFA812')]='Dark tangerine'
COLOR_NAMES[clr_h2i('#FFAA1D')]='Bright Yellow'
COLOR_NAMES[clr_h2i('#FFAE42')]='Yellow Orange'
COLOR_NAMES[clr_h2i('#FFAEB9')]='Light pink'
COLOR_NAMES[clr_h2i('#FFB077')]='Very light tangelo'
COLOR_NAMES[clr_h2i('#FFB300')]='UCLA Gold'
COLOR_NAMES[clr_h2i('#FFB347')]='Pastel orange'
COLOR_NAMES[clr_h2i('#FFB3DE')]='Light hot pink'
COLOR_NAMES[clr_h2i('#FFB5C5')]='Pink'
COLOR_NAMES[clr_h2i('#FFB6C1')]='Light pink'
COLOR_NAMES[clr_h2i('#FFB7C5')]='Cherry blossom pink'
COLOR_NAMES[clr_h2i('#FFB90F')]='Dark goldenrod'
COLOR_NAMES[clr_h2i('#FFBA00')]='Selective yellow'
COLOR_NAMES[clr_h2i('#FFBBFF')]='Plum'
COLOR_NAMES[clr_h2i('#FFBCD9')]='Cotton candy'
COLOR_NAMES[clr_h2i('#FFBD88')]='Macaroni and Cheese'
COLOR_NAMES[clr_h2i('#FFBF00')]='Amber, Fluorescent orange'
COLOR_NAMES[clr_h2i('#FFC0CB')]='Pink'
COLOR_NAMES[clr_h2i('#FFC125')]='Goldenrod'
COLOR_NAMES[clr_h2i('#FFC1C1')]='Rosy brown'
COLOR_NAMES[clr_h2i('#FFC1CC')]='Bubble gum'
COLOR_NAMES[clr_h2i('#FFC40C')]='Mikado yellow'
COLOR_NAMES[clr_h2i('#FFC87C')]='Topaz'
COLOR_NAMES[clr_h2i('#FFCBA4')]='Deep peach'
COLOR_NAMES[clr_h2i('#FFCC00')]='Tangerine yellow'
COLOR_NAMES[clr_h2i('#FFCC33')]='Sunglow'
COLOR_NAMES[clr_h2i('#FFCC99')]='Peach-orange'
COLOR_NAMES[clr_h2i('#FFCFF1')]='Shampoo'
COLOR_NAMES[clr_h2i('#FFD300')]='Cyber yellow'
COLOR_NAMES[clr_h2i('#FFD39B')]='Burlywood'
COLOR_NAMES[clr_h2i('#FFD700')]='Gold'
COLOR_NAMES[clr_h2i('#FFD800')]='School bus yellow'
COLOR_NAMES[clr_h2i('#FFDAB9')]='Peach puff'
COLOR_NAMES[clr_h2i('#FFDAE9')]='Mimi Pink'
COLOR_NAMES[clr_h2i('#FFDB00')]='Sizzling Sunrise'
COLOR_NAMES[clr_h2i('#FFDB58')]='Mustard'
COLOR_NAMES[clr_h2i('#FFDDCA')]='Unbleached silk'
COLOR_NAMES[clr_h2i('#FFDDF4')]='Pink lace'
COLOR_NAMES[clr_h2i('#FFDEAD')]='Navajo white'
COLOR_NAMES[clr_h2i('#FFDF00')]='Golden yellow'
COLOR_NAMES[clr_h2i('#FFDF46')]='Gargoyle Gas'
COLOR_NAMES[clr_h2i('#FFDFBF')]='Very pale orange'
COLOR_NAMES[clr_h2i('#FFE135')]='Banana yellow'
COLOR_NAMES[clr_h2i('#FFE1FF')]='Thistle'
COLOR_NAMES[clr_h2i('#FFE302')]='Vivid yellow'
COLOR_NAMES[clr_h2i('#FFE4B5')]='Moccasin'
COLOR_NAMES[clr_h2i('#FFE4C4')]='Bisque'
COLOR_NAMES[clr_h2i('#FFE4CD')]='Lumber'
COLOR_NAMES[clr_h2i('#FFE4E1')]='Misty rose'
COLOR_NAMES[clr_h2i('#FFE5B4')]='Peach'
COLOR_NAMES[clr_h2i('#FFE7BA')]='Wheat'
COLOR_NAMES[clr_h2i('#FFEB00')]='Middle Yellow'
COLOR_NAMES[clr_h2i('#FFEBCD')]='Blanched almond'
COLOR_NAMES[clr_h2i('#FFEC8B')]='Light goldenrod'
COLOR_NAMES[clr_h2i('#FFEF00')]='Canary yellow'
COLOR_NAMES[clr_h2i('#FFEFD5')]='Papaya whip'
COLOR_NAMES[clr_h2i('#FFEFDB')]='Antique white'
COLOR_NAMES[clr_h2i('#FFF000')]='Yellow rose'
COLOR_NAMES[clr_h2i('#FFF0F5')]='Lavender blush'
COLOR_NAMES[clr_h2i('#FFF44F')]='Lemon yellow'
COLOR_NAMES[clr_h2i('#FFF5EE')]='Seashell'
COLOR_NAMES[clr_h2i('#FFF600')]='Cadmium yellow'
COLOR_NAMES[clr_h2i('#FFF68F')]='Khaki'
COLOR_NAMES[clr_h2i('#FFF700')]='Lemon, Yellow Sunshine'
COLOR_NAMES[clr_h2i('#FFF8DC')]='Cornsilk'
COLOR_NAMES[clr_h2i('#FFF8E7')]='Cosmic latte'
COLOR_NAMES[clr_h2i('#FFFACD')]='Lemon chiffon'
COLOR_NAMES[clr_h2i('#FFFAF0')]='Floral white'
COLOR_NAMES[clr_h2i('#FFFAFA')]='Snow'
COLOR_NAMES[clr_h2i('#FFFDD0')]='Cream'
COLOR_NAMES[clr_h2i('#FFFF00')]='Yellow'
COLOR_NAMES[clr_h2i('#FFFF31')]='Daffodil'
COLOR_NAMES[clr_h2i('#FFFF33')]='Electric yellow'
COLOR_NAMES[clr_h2i('#FFFF66')]='Unmellow yellow'
COLOR_NAMES[clr_h2i('#FFFF99')]='Canary'
COLOR_NAMES[clr_h2i('#FFFFBF')]='Very pale yellow'
COLOR_NAMES[clr_h2i('#FFFFE0')]='Light yellow'
COLOR_NAMES[clr_h2i('#FFFFF0')]='Ivory'
COLOR_NAMES[clr_h2i('#FFFFFF')]='White'

