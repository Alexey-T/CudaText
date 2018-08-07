''' Plugin for CudaText editor
Authors:
    Andrey Kvichansky    (kvichans on github.com)
Version:
    '0.9.6 2016-12-06'
ToDo: (see end of file)
'''

import  re, colorsys
import  cudatext        as app
#from    cudatext    import ed
#import  cudatext_cmd    as cmds
import  cudax_lib       as apx
from    .cd_plug_lib    import *
_   = get_translation(__file__) # I18N

pass;                           # Logging
pass;                           LOG = (-2==-2)  # Do or dont logging.

get_hist_   = lambda k,v:   get_hist(k, v, module_name='palettes')
set_hist_   = lambda k,v:   set_hist(k, v, module_name='palettes')

Rc          = lambda c:     (c&0x0000FF)
Gc          = lambda c:     (c&0x00FF00) >> 8
Bc          = lambda c:     (c&0xFF0000) >> 16
int_to_rgb  = lambda clr:   ( 255&clr      ,  255&(clr>>8)      ,  255&(clr>>16))
int_to_rgb01= lambda clr:   ((255&clr)/255 , (255&(clr>>8))/255 , (255&(clr>>16))/255)
rgb_to_int  = lambda r,g,b: r              | (g         <<8)    | (b         <<16)
rgb01_to_int= lambda r,g,b: int(255*r)     | (int(255*g)<<8)    | (int(255*b)<<16)
clr_h2i     = apx.html_color_to_int

BLUE    = 0xff0000
YELLOW  = 0x00ffff
COLOR_NAMES={}
PLTYPES = [ '60 colors: 3*20'
        ,   '142 colors: 7-hexagon'
        ,   '216 web-colors: 9-hexagon'
        ,   '216 web-colors: dragon'
        ,   '216 web-colors: candles'
#       ,   '343 colors: 18*19'
        ,   '3221 colors: 7-hexagon, dialog'
        ,   '146 named colors'
        ,   '420 named colors: 12*35'
        ,   '1431 named colors: 27*53'
        ]
def dlg_color_palette(caption, old_color=None, palette_type=None, i18n={}):
    """ Show dlg to choose new color or view old one.
        Params
            caption         (str) Title for dlg.
            old_color       (int) Old color as RGB-int. None if no.
            palette_type    (str) Palette name
            i18n            (dict) Caption for control. Wait keys
                                'cancel', 'named', 'nearby', 'nocolor'
        Return
                            (int) Selected color    
            COLOR_NONE      (int) If "No color"
            None                  If "Cancel"
    """
    pass;                      #LOG and log('caption, old_color, palette_type={}',(caption, old_color, palette_type))
    pass;                       sltr        = 0
    pass;                      #sltr        = 37    # for 7
    pass;                      #sltr        = 43    # for 6
    pass;                       rc4exch_src = None
    new_color           = None
    active_plts         = get_hist_('active_palettes', apx.get_opt('active_palettes', '|'.join(PLTYPES))).split('|')
#   active_plts         =                              apx.get_opt('active_palettes', '|'.join(PLTYPES)).split('|')
    if not active_plts:
        # All if never Config
        active_plts     = PLTYPES[:]
    else:
        active_plts     = [plt for plt in active_plts if plt in PLTYPES]
    if palette_type     in PLTYPES and \
       palette_type not in active_plts:
       # Add to list if in params
       active_plts     += [palette_type]
    if not palette_type:
        # Use last or first if not in params
        palette_type    = get_hist_('last_palette_type', apx.get_opt('last_palette_type', active_plts[0]))
#       palette_type    =                                apx.get_opt('last_palette_type', active_plts[0])
        palette_type    = palette_type if palette_type in active_plts else active_plts[0]
    grey_clr_for_plt    = get_hist_('last_palette_grey_level', apx.get_opt('last_palette_grey_level', 0))
#   grey_clr_for_plt    =                                      apx.get_opt('last_palette_grey_level', 0)
    view_more           = get_hist_('palette_more', apx.get_opt('palette_more', False))
#   view_more           =                           apx.get_opt('palette_more', False)
    
    cnRGBs  = [(int_to_rgb(c), c, s) for (c, s) in COLOR_NAMES.items()]
    
    brd_c   = clr_h2i('#b6feff')
    MIN_PLT_WIDTH = 555
    C_NMED  = i18n.get('named'      ,_('M&ark named'))
    C_NRBY  = i18n.get('nearby'     ,_('N&earby'))
    C_NOTH  = i18n.get('nocolor'    ,_('&No color'))
    C_CANC  = i18n.get('cancel'     ,_('Cancel'))
    C_CNFG  = i18n.get('config'     ,_('Config...'))

    H_TITL  = i18n.get('help_hint'  ,_(' (Shift+Click to preview. Ctrl+Click to copy data)'))
    H_MORE  = i18n.get('more_hint'  ,_('Show/Hide advanced options'))
    H_NMED  = i18n.get('named_hint' ,_('Mark named colors with "!"'))
    H_NRBY  = i18n.get('nearby_hint',_('Assign names to some colors, which are "near" named colors. Marks show distance to these near colors:'
                '\r"!" if distance is very low,'
                '\r"." if distance is small, '
                '\r".." all others'
                ))
    H_NFLT  = i18n.get('inname_hint',_('Point colors which name includes the string'))
    H_NEWC  = i18n.get('new_c_hint' ,_('New color'))
    H_OLDC  = i18n.get('old_c_hint' ,_('Old color'))
    
    def clr_data(clr, vw_nrby, nflt):
        R, G, B     = int_to_rgb(clr)
        H, S, V     = list(int(255*c) for c in colorsys.rgb_to_hsv(R/255, G/255, B/255))
        nmd         = clr in COLOR_NAMES
        nm          = COLOR_NAMES.get(clr, '')
        sure,ma     = '', 0
        cn          = clr if nm else 0
        if not nm and vw_nrby:
            d,ma,   \
            nm,cn   = min(    (abs(R-cnR)+abs(G-cnG)+abs(B-cnB)
                         , max(abs(R-cnR),abs(G-cnG),abs(B-cnB))
                         , sn, c) for ((cnR, cnG, cnB), c, sn) in cnRGBs)
            sure    = sure_by_ma(ma)
            nm      = nm if sure else ''
        fltd        = nflt and nm and nflt in nm.upper()
#       flt_c       = YELLOW
#       pass                        #;flt_s  = ''
#       if not fltd:pass
#       elif (R+G+B)>230*3:
#           flt_c   = BLUE          #;flt_s  = 'L'
#       elif V>220 and not abs(H-170) < 50:
#           flt_c   = BLUE          #;flt_s  = 'Lb'
#       elif B>=R and B>=G:
#           pass                    #;flt_s  = 'B'
#       elif R>220:
#           flt_c   = BLUE          #;flt_s  = 'R'
#       elif G>220: 
#           flt_c   = BLUE          #;flt_s  = 'G'
#       elif R+G>190*2:
#           flt_c   = BLUE          #;flt_s  = 'RG'
        hint        = f('{h}\rRGB:{R},{G},{B}\rHSV:{H},{S},{V}{n}'
                       , h=apx.int_to_html_color(clr).upper()
                       , R=R, G=G, B=B
                       , H=H, S=S, V=V
                       , n='\r'+nm+('' if nmd else f('\r({})',apx.int_to_html_color(cn).upper())) if nm else '')
        return (R,G,B, H,S,V
               ,nmd,nm,sure,ma,cn
               ,fltd    #,flt_c               #,flt_s
               ,hint)
    fid         = 'pltp'
    vals        = dict(pltp=active_plts.index(palette_type)
                  ,nmed=False
                  ,nrby=False
                  ,nflt=''
                  )
    pre_plt     = palette_type
    pre_grey    = grey_clr_for_plt
    while True:
        C_MORE      = '&<<' if view_more else '&>>'
        if pre_plt != vals.get('pltp', pre_plt) or pre_grey != grey_clr_for_plt:
            clrs,       \
            w,h,        \
            sp_clrs,    \
            sp_w,sp_h   = _dlg_color_palette_clrs(active_plts[vals['pltp']], grey_clr_for_plt)
            pre_plt     = vals['pltp']
            pre_grey    = grey_clr_for_plt
        
        if view_more:
            vw_nmed     = vals['nmed']                      # Point of named color
            vw_nrby     = vals['nrby'] and vw_nmed          # Point color names with nearby
            nflt        = vals['nflt'].upper()              # Filter value
        else:
            vw_nmed     = False
            vw_nrby     = False
            nflt        = ''
        pass;                  #LOG and log('nflt={}',(nflt))

        max_cnt     = max(len(r) for ir,r in enumerate(   clrs))
        sp_max_cnt  = max(len(r) for ir,r in enumerate(sp_clrs)) if sp_clrs else 0
        plt_w       = max(   w *    max_cnt
                         ,sp_w * sp_max_cnt
                         ,MIN_PLT_WIDTH)
        
        sure_by_ma  = lambda ma:'!'   if ma<=3 else \
                                '.'   if ma<=9 else \
                                '..'
        cnts    = []
        # Main plt
        pass;                  #LOG and log('?? main plt (==',())
        for     irow,crow in enumerate(clrs):
            shft    = (plt_w - w *len(crow)) // 2
            for icol,clr  in enumerate(crow):
                if clr is None: continue
                (R,G,B, H,S,V
                ,nmd,nm,sure,ma,cn
                ,fltd    #,flt_c                  #,flt_s
                ,hint)= clr_data(clr, vw_nrby, nflt)
                if nflt and not fltd: continue#for
                fg_c    = (0x000000 if (R+G+B)/3>128 or G>240 else 0xffffff)
                cnts += [dict(tp='clr'
                        ,cid    =f('c{:2}{:2}',irow,icol)  
                        ,t      =10+irow*h        ,h=h+1
                        ,l      =shft+10+icol*w   ,w=w+1
                        ,props  =f('1,{bg},{fg},{bord_c}', bg=clr, fg=fg_c, bord_c= brd_c )
                        ,cap    =('!'                      if vw_nmed and nmd else 
                                  sure        # +flt_s #+ str(ma)
                                                +(f('{}{}{}', R//sltr, G//sltr, B//sltr) if sltr else '')
                                 )
                        ,hint   =hint         # +(f('\rrc={},{}',irow,icol) if sltr else '')
                        ,rc     =(irow,icol)
                        ,c      = clr
                        ,act    ='1'
                    )]
        plt_h   = h * len(clrs)
        pass;                  #LOG and log('ok main plt',())

        pass;                  #LOG and log('?? spec plt',())
        # Spec plt
        for     irow,crow in enumerate(sp_clrs):
            shft    = (plt_w - sp_w *len(crow)) // 2
            for icol,clr  in enumerate(crow):
                if clr is None: continue
                (R,G,B, H,S,V
                ,nmd,nm,sure,ma,cn
                ,fltd    #,flt_c                  #,flt_s
                ,hint)= clr_data(clr, vw_nrby, nflt)
                fg_c    = (0x000000 if (R+G+B)/3>128 or G>240 else 0xffffff)
                cnts += [dict(tp='clr'
                        ,cid    =f('s{}', clr)  
                        ,t      =plt_h+10+irow*sp_h ,h=sp_h+1
                        ,l      =shft+10+icol*sp_w  ,w=sp_w+1
                        ,props  =f('1,{bg},{fg},{bord_c}', bg=clr, fg=fg_c, bord_c=brd_c)
                        ,cap=   '^' if clr==grey_clr_for_plt else ''
                        ,hint   =hint
                        ,act=   '1'
                    )]
        sp_plt_h    = sp_h * len(sp_clrs)
        pass;                  #LOG and log('ok spec plt',())

        plt_h       = plt_h + sp_plt_h
        
        if old_color is not None:
            (old_R,old_G,old_B, old_H,old_S,old_V
            ,old_nmd,old_nm,old_sure,old_ma,old_cn
            ,old_fltd    #,old_flt_c                  #,flt_s
            ,old_hint)  = clr_data(old_color, vw_nrby, nflt)
            idold       = 'c'+str(old_color)
        if new_color is not None:
            (new_R,new_G,new_B, new_H,new_S,new_V
            ,new_nmd,new_nm,new_sure,new_ma,new_cn
            ,new_fltd    #,new_flt_c                  #,flt_s
            ,new_hint)  = clr_data(new_color, vw_nrby, nflt)
            idnew       = 'c'+str(new_color)
        if view_more:
            cnts+=     [dict(cid='aflt'  ,tp='bt'   ,tid='----'     ,l=  0          ,w=0  ,cap=''                                       ,props='1'      )] #default
            cnts+=     [dict(cid='pltp'  ,tp='cb-ro',tid='noth'     ,l= 10          ,w=385,items=active_plts+[C_CNFG]           ,act='1'                )]
            cnts+=     [dict(cid='nflt'  ,tp='ed'   ,tid='----'     ,l= 10          ,w=100                      ,hint=H_NFLT                            )]
            cnts+=     [dict(cid='nmed'  ,tp='ch'   ,tid='----'     ,l=130          ,w=150,cap=C_NMED           ,hint=H_NMED    ,act='1'                )]
            cnts+=     [dict(cid='nrby'  ,tp='ch'   ,tid='----'     ,l=260          ,w=150,cap=C_NRBY           ,hint=H_NRBY    ,act='1',en=vals['nmed'])]
            if new_color is not None:
                cnts+= [dict(cid=idnew   ,tp='clr'  ,t=10+plt_h+ 7  ,l=10+plt_w-165 ,w= 50, h=30,cap=''
                            ,props=f('1,{bg},0,{bc}',bg=new_color,bc=brd_c)             ,hint=H_NEWC+'\r'+new_hint  ,c=new_color,act='1'                )]
            if old_color is not None:
                cnts+= [dict(cid=idold   ,tp='clr'  ,t=10+plt_h+36  ,l=10+plt_w-165 ,w= 50, h=30,cap=''
                            ,props=f('1,{bg},0,{bc}',bg=old_color,bc=brd_c)             ,hint=H_OLDC+'\r'+old_hint  ,c=old_color,act='1'                )]
            cnts+=     [dict(cid='more'  ,tp='bt'   ,tid='----'     ,l=10+plt_w-215 ,w= 45,cap=C_MORE           ,hint=H_MORE                            )]
            cnts+=     [dict(cid='noth'  ,tp='bt'   ,t=10+plt_h+10  ,l=10+plt_w-110 ,w=110,cap=C_NOTH                                                   )]
            cnts+=     [dict(cid='----'  ,tp='bt'   ,t=10+plt_h+40  ,l=10+plt_w-110 ,w=110,cap=C_CANC                                                   )]
        else:
            cnts+=     [dict(cid='more'  ,tp='bt'   ,tid='----'     ,l=10           ,w= 45,cap=C_MORE           ,hint=H_MORE                            )]
            if old_color is not None:
                cnts+= [dict(cid=idold   ,tp='clr'  ,t=10+plt_h+ 7  ,l=10+plt_w-275 ,w= 50, h=30,cap=''
                            ,props=f('1,{bg},0,{bc}',bg=old_color,bc=brd_c)             ,hint=H_OLDC+'\r'+old_hint  ,c=old_color,act='1'                )]
            cnts+=     [dict(cid='noth'  ,tp='bt'   ,t=10+plt_h+10  ,l=10+plt_w-220 ,w=110,cap=C_NOTH                                                   )]
            cnts+=     [dict(cid='----'  ,tp='bt'   ,t=10+plt_h+10  ,l=10+plt_w-110 ,w=110,cap=C_CANC                                                   )]
        dlg_w   = 10 + plt_w + 10
        dlg_h   = 5  + plt_h + 15 + 30 + (30 if view_more else 0) #+ 5

        pass;                  #LOG and log('?? dlg_wrapper ==)',())
        aid,vals,*_t = dlg_wrapper(caption + (H_TITL if view_more else '')
                                  ,dlg_w, dlg_h, cnts
                                  ,vals if view_more else {}
                                  ,focus_cid=fid)
        pass;                  #LOG and log('aid,vals={}',(aid,vals))
        if not aid or aid=='----': return None
        if aid=='more':
            view_more   = not view_more
            set_hist_(  'palette_more', view_more)
#           apx.set_opt('palette_more', view_more)
            if view_more:
                fid     = 'pltp'
                vals    = dict(pltp=active_plts.index(palette_type)
                              ,nmed=False
                              ,nrby=False
                              ,nflt=''
                              )
            continue#while

        scam    = app.app_proc(app.PROC_GET_KEYSTATE, '') if app.app_api_version()>='1.0.143' else ''
        if sltr and \
           aid=='noth' and sltr and scam=='sc':  # Show 0-6 main plt
            pass;              #LOG and log('clrs={}',(clrs))
            pass;               plt_s   = '\n'.join(
                                                    ' '.join(
                                                            (f('{}{}{}',Rc(cl)//sltr, Gc(cl)//sltr, Bc(cl)//sltr) if cl is not None else 'NNN') for cl in r
                                                            )+' ' for r in clrs
                                                    ) + '\n'
            pass;               plt_s   = re.sub(r'(\S\S\S \S\S\S \S\S\S )', r'\1    ', plt_s)
            pass;               app.app_proc(app.PROC_SET_CLIP, plt_s)
            pass;               dlg_wrapper('Plt', 5+700+5, 5+320+5, [dict(cid='plt' ,tp='me',t=5,h=600  ,l=5,w=700, props='1,1,1')], dict(plt=plt_s))
            pass;               continue#while
        if aid=='noth': return app.COLOR_NONE

        if aid[0]=='c':
            cnt = [cnt for cnt in cnts if aid==cnt['cid']][0]
            new_color   = cnt['c']
            if scam=='':
                return new_color
            if sltr:
                pass;           rc4exch = cnt['rc']
            if scam=='c':
                app.app_proc(app.PROC_SET_CLIP, cnt['hint'].replace('\r', '\n'))
            if scam=='a' and sltr and rc4exch_src is not None and rc4exch!=rc4exch_src:
                    pass;      #LOG and log('?? clrs={}',clrs)
                    pass;       clrs[rc4exch_src[0]][rc4exch_src[1]] ,  \
                                clrs[rc4exch    [0]][rc4exch    [1]]    = clrs[rc4exch    [0]][rc4exch    [1]] , \
                                                                          clrs[rc4exch_src[0]][rc4exch_src[1]]
                    pass;       LOG and log('exch! {} with {}',rc4exch,rc4exch_src)
                    pass;      #LOG and log('!! clrs={}',clrs)
#               continue#while
            if scam=='s':
                if sltr:
                    pass;       rc4exch_src = rc4exch
                    pass;       LOG and log('rc4exch_src={}',(rc4exch_src))
            continue#while

        fid     = 'nflt'    if aid in ('nmed', 'nrby', 'aflt') else     'pltp'
        pass;                  #LOG and log('aid, fid={}',(aid, fid))
        
        if aid[0]=='s':    # Special color
            clr     = int(aid[1:])
            R, G, B = int_to_rgb(clr) #255&clr, 255&(clr>>8), 255&(clr>>16)
            if R==G==B:
                grey_clr_for_plt    = clr
                set_hist_(  'last_palette_grey_level', clr)
#               apx.set_opt('last_palette_grey_level', clr)
            continue#while

        if aid=='pltp' and vals['pltp']==len(active_plts):  # Config
            old_plt = get_hist_('last_palette_type', apx.get_opt('last_palette_type', ''))
#           old_plt =                                apx.get_opt('last_palette_type', '')
            sels    = [to01(plt in active_plts) for plt in PLTYPES]
            ap_vals = dict(ps=(0,sels))
            while True:
                ap_aid, \
                ap_vals,\
                *_t     = dlg_wrapper(_('Select active palettes'), 5+200+5, 5+400+5+24+5, 
                        [dict(cid='ps' ,tp='ch-lbx',t=5,h=400  ,l=5          ,w=200  ,items=PLTYPES          ) #
                        ,dict(cid='!'  ,tp='bt'    ,t=5+400+5  ,l=  200-140  ,w=70   ,cap=_('OK'),props='1'  ) #  default
                        ,dict(cid='-'  ,tp='bt'    ,t=5+400+5  ,l=5+200- 70  ,w=70   ,cap=_('Cancel')        ) #  
                        ], ap_vals, focus_cid='ps')
                if ap_aid is None or ap_aid=='-': break#while ap
                sels    = ap_vals['ps'][1]
                if not any(sl=='1' for sl in sels):
                    app.msg_box(_('Select some palettes'), app.MB_OK)
                    continue#while ap
                active_plts = [pl for ip,pl in enumerate(PLTYPES) if sels[ip]=='1']
                set_hist_(  'active_palettes', '|'.join(active_plts))
#               apx.set_opt('active_palettes', '|'.join(active_plts))
                break#while ap
            vals['pltp']    = active_plts.index(old_plt) if old_plt in active_plts else 0
            continue#while

        if aid=='pltp':
            set_hist_(  'last_palette_type', active_plts[vals['pltp']])
#           apx.set_opt('last_palette_type', active_plts[vals['pltp']])
       #do while
   #def dlg_color_palette

def _dlg_color_palette_clrs(palette_type, grey_clr_for_plt=0):
    R1          = 0x000033
    G1          = 0x003300
    B1          = 0x330000
    inversedRGB = True
    clrs        = ()
    w,h         = 21,21
    sp_clrs     = ()
    sp_w,sp_h   = 21,21
    
    def inverse_RGB(clrs):
        return list(list(
            (c & 0x0000ff)<<16 | (c & 0x00ff00) | (c & 0xff0000)>>16 
                if c is not None else c
            for c in row) for row in clrs)

    if False:pass
    elif palette_type=='343 colors: 18*19':
        #  9-hexagon:  9*2 + 10*2 + 11*2 + 12*2 + 13*2 + 14*2 + 15*2 + 16*2 + 17                            = 217
        # 10-hexagon:        10*2 + 11*2 + 12*2 + 13*2 + 14*2 + 15*2 + 16*2 + 17*2 + 18*2 + 19              = 271
        # 11-hexagon:               11*2 + 12*2 + 13*2 + 14*2 + 15*2 + 16*2 + 17*2 + 18*2 + 19*2 + 20*2 + 21= 331
        # 343 = 7*7*7 = 8*27 + 127 = 331 + 12 = 7 + 7*48 = 7 + 7*6*8 = 7 + 16*21 = 16*22 - 9 = 7 + 12*28 = 7 + 6*7*8 = 12*29 - 5 = 18*19 + 1
        RPrts   = (0x000000,0x00002a,0x000055,0x00007f,0x0000aa,0x0000d4,0x0000ff)
        GPrts   = (0x000000,0x002a00,0x005500,0x007f00,0x00aa00,0x00d400,0x00ff00)
        BPrts   = (0x000000,0x2a0000,0x550000,0x7f0000,0xaa0000,0xd40000,0xff0000)
        _rest   = """
/==   B =/=     =\=  GB ==\     ==/  G  /==     \== RG  =\=     =/= R   ==/     ==\ R B \== """
        clls_t  = '((0x' + """
016 006 106     056 066 065     061 060 050     560 660 650     610 600 601     605 606 506     
105 005 015     046 166 064     062 160 040     561 661 550     510 500 620     626 616 406     
014 004 104     045 266 054     052 051 041     540 662 640     602 612 611     604 515 505     
116 003 126     156 165 164     162 150 161     641 663 651     621 511 502     614 516 504     
025 002 206     146 055 155     151 140 260     652 664 460     501 520 400     625 526 615     
101 001 010     430 440 450     530 200 210     220 665 240     250 063 300     310 320 330     
401 411 421     431 441 451     461 201 211     221 231 241     251 261 301     311 321 331     
402 412 422     432 442 452     462 202 212     536 232 242     252 262 302     312 322 332     
403 413 423     433 443 453     463 203 213     223 233 243     253 263 303     313 323 562     
404 414 424     434 565 454     464 204 214     224 234 244     254 264 304     314 324 334     
405 415 425     435 445 455     465 205 215     225 235 245     255 265 305     315 325 335     
603 416 426     436 446 456     466 630 216     226 236 246     256 036 306     316 326 336     
535 420 020     030 100 110     120 130 410     011 021 031     503 121 654     131 141 136     
026 012 022     032 042 102     112 122 132     142 152 163     340 350 360     343 353 363     
115 013 023     033 043 053     103 113 123     133 143 153     341 351 361     344 354 364     
024 034 044     114 124 134     144 154 035     125 135 145     342 352 362     345 355 365     
521 531 541     631 346 356     366 632 642     512 522 532     542 552 622     643 653 551     
513 523 533     543 553 563     613 623 633     514 524 534     544 554 564     624 634 644     
635 525 545     000 000 000     000 655 230     645 546 556     566 636 646     656 000 000     
666 555 444     333 222 111     000 NNN NNN     NNN NNN NNN     NNN NNN NNN     NNN NNN NNN     
""".strip('\n').replace('     ', ' ').replace(' \n', ')\n,(0x').replace(' ', ',0x').replace('0xNNN', 'None')[:-3] + '))'
        pass;                  #LOG and log('clls_t={}',(clls_t))
        clls16  = eval(clls_t)
        pass;                  #LOG and log('clls16={}',(clls16))
#       clls16  = list( list(clls16[ir][ic] for ir in range(len(clls16))) for ic in range(len(clls16[0])) )   # Transposition
        clrs    = list(list(( RPrts[(cll&0xF00)>>8] | GPrts[(cll&0x0F0)>>4] | BPrts[cll&0x00F] if cll is not None else None) for cll in clls_row) for clls_row in clls16)
        pass;                  #LOG and log('clrs={}',(clrs))
        w,h     = 31,31
        inversedRGB = False

    elif palette_type=='3221 colors: 7-hexagon, dialog':
        # 6*6 + 35 * (6+7+8+9+10+11+10+9+8+7+6) = 36 + 35 *91 = 3221
        inversedRGB= False
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
        clrs    = inverse_RGB(clrs)
        sp_clrs = inverse_RGB(sp_clrs)
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
,(0xEEB422,0xEEB4B4,0xEEC591,0xEEC900,0xEECBAD,0xEECFA1,0xEED2EE,0xEED5B7,0xEED5D2,0xEED8AE,0xEEDC82,0xEEDC82)
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
#       inversedRGB= False
#       clls    = ((
#),((200,255,255),(190,255,255),(180,255,255),(170,255,255),(160,255,255),(150,255,255),(140,255,255),(130,255,255),(120,255,255),(110,255,255),(100,255,255),( 90,255,255),( 80,255,255),( 70,255,255),( 60,255,255),( 50,255,255),( 40,255,255),( 30,255,255),( 20,255,255),( 10,255,255),
#),((200,255,255),(190,255,255),(180,255,255),(170,255,255),(160,255,255),(150,255,255),(140,255,255),(130,255,255),(120,255,255),(110,255,255),(100,255,255),( 90,255,255),( 80,255,255),( 70,255,255),( 60,255,255),( 50,255,255),( 40,255,255),( 30,255,255),( 20,255,255),( 10,255,255),
#)                )[1:]
#       clrs    = list(list(( rgb01_to_int(*colorsys.hsv_to_rgb(cll[0]/255,cll[1]/255,cll[2]/255)) if cll else None) for cll in clls_row) for clls_row in clls)
##       clrs    = list(list(( rgb(*           hsv_to_rgb(cll[0]/255,cll[1]/255,cll[2]/255)) if cll else None) for cll in clls_row) for clls_row in clls)
#       w,h     = 27,27
#       pass;                   LOG and log('clrs[0][0]={}',(clrs[0][0]))
    
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
    if inversedRGB:
        clrs    = inverse_RGB(clrs)

    return clrs,w,h, sp_clrs,sp_w,sp_h
   #def _dlg_color_palette_clrs

COLOR_NAMES[clr_h2i('#000000')]=_('Black')
COLOR_NAMES[clr_h2i('#000080')]=_('Navy')
COLOR_NAMES[clr_h2i('#00008B')]=_('Dark blue')
COLOR_NAMES[clr_h2i('#00009C')]=_('Duke blue')
COLOR_NAMES[clr_h2i('#0000CD')]=_('Medium blue')
COLOR_NAMES[clr_h2i('#0000EE')]=_('Blue')
COLOR_NAMES[clr_h2i('#0000FF')]=_('Blue')
COLOR_NAMES[clr_h2i('#000F89')]=_('Phthalo blue')
COLOR_NAMES[clr_h2i('#00147E')]=_('Dark imperial blue')
COLOR_NAMES[clr_h2i('#0014A8')]=_('Zaffre')
COLOR_NAMES[clr_h2i('#0018A8')]=_('Blue')
COLOR_NAMES[clr_h2i('#001C3D')]=_('Maastricht blue')
COLOR_NAMES[clr_h2i('#002147')]=_('Oxford blue')
COLOR_NAMES[clr_h2i('#002366')]=_('Royal blue')
COLOR_NAMES[clr_h2i('#002387')]=_('Resolution blue')
COLOR_NAMES[clr_h2i('#002395')]=_('Imperial blue')
COLOR_NAMES[clr_h2i('#002E63')]=_('Cool black')
COLOR_NAMES[clr_h2i('#002FA7')]=_('International Klein blue')
COLOR_NAMES[clr_h2i('#00308F')]=_('Air Force blue')
COLOR_NAMES[clr_h2i('#003153')]=_('Prussian blue')
COLOR_NAMES[clr_h2i('#003366')]=_('Dark midnight blue')
COLOR_NAMES[clr_h2i('#003399')]=_('Smalt, Dark powder blue')
COLOR_NAMES[clr_h2i('#0033AA')]=_('UA blue')
COLOR_NAMES[clr_h2i('#0038A8')]=_('Royal azure')
COLOR_NAMES[clr_h2i('#004040')]=_('Rich black')
COLOR_NAMES[clr_h2i('#00416A')]=_('Dark imperial blue')
COLOR_NAMES[clr_h2i('#004225')]=_('British racing green')
COLOR_NAMES[clr_h2i('#004242')]=_('Warm black')
COLOR_NAMES[clr_h2i('#0047AB')]=_('Cobalt blue')
COLOR_NAMES[clr_h2i('#0048BA')]=_('Absolute zero')
COLOR_NAMES[clr_h2i('#004953')]=_('Midnight green, Eagle green')
COLOR_NAMES[clr_h2i('#004B49')]=_('Deep jungle green')
COLOR_NAMES[clr_h2i('#004F98')]=_('USAFA blue')
COLOR_NAMES[clr_h2i('#00563F')]=_('Sacramento state green')
COLOR_NAMES[clr_h2i('#006400')]=_('Dark green')
COLOR_NAMES[clr_h2i('#006600')]=_('Pakistan green')
COLOR_NAMES[clr_h2i('#0067A5')]=_('Sapphire blue')
COLOR_NAMES[clr_h2i('#00688B')]=_('Deep sky blue')
COLOR_NAMES[clr_h2i('#006994')]=_('Sea blue')
COLOR_NAMES[clr_h2i('#006A4E')]=_('Bottle green')
COLOR_NAMES[clr_h2i('#006B3C')]=_('Cadmium green')
COLOR_NAMES[clr_h2i('#006DB0')]=_('Honolulu blue')
COLOR_NAMES[clr_h2i('#00703C')]=_('Dartmouth green')
COLOR_NAMES[clr_h2i('#0070B8')]=_('Spanish blue')
COLOR_NAMES[clr_h2i('#0070FF')]=_('Brandeis blue')
COLOR_NAMES[clr_h2i('#0072BB')]=_('French blue')
COLOR_NAMES[clr_h2i('#0073CF')]=_('True blue')
COLOR_NAMES[clr_h2i('#007474')]=_('Skobeloff')
COLOR_NAMES[clr_h2i('#00755E')]=_('Tropical rain forest')
COLOR_NAMES[clr_h2i('#0077BE')]=_('Ocean boat blue')
COLOR_NAMES[clr_h2i('#007AA5')]=_('CG blue')
COLOR_NAMES[clr_h2i('#007BA7')]=_('Celadon blue')
COLOR_NAMES[clr_h2i('#007BB8')]=_('Star command blue')
COLOR_NAMES[clr_h2i('#007F5C')]=_('Spanish viridian')
COLOR_NAMES[clr_h2i('#007F66')]=_('Generic viridian')
COLOR_NAMES[clr_h2i('#007FFF')]=_('Azure')
COLOR_NAMES[clr_h2i('#008000')]=_('Green')
COLOR_NAMES[clr_h2i('#008080')]=_('Teal')
COLOR_NAMES[clr_h2i('#00827F')]=_('Teal green')
COLOR_NAMES[clr_h2i('#00868B')]=_('Turquoise')
COLOR_NAMES[clr_h2i('#0087BD')]=_('Blue')
COLOR_NAMES[clr_h2i('#008B00')]=_('Green')
COLOR_NAMES[clr_h2i('#008B45')]=_('Spring green')
COLOR_NAMES[clr_h2i('#008B8B')]=_('Dark cyan')
COLOR_NAMES[clr_h2i('#009000')]=_('Islamic green')
COLOR_NAMES[clr_h2i('#009150')]=_('Spanish green')
COLOR_NAMES[clr_h2i('#0093AF')]=_('Blue')
COLOR_NAMES[clr_h2i('#0095B6')]=_('Bondi blue')
COLOR_NAMES[clr_h2i('#009698')]=_('Viridian green')
COLOR_NAMES[clr_h2i('#009966')]=_('Green-cyan')
COLOR_NAMES[clr_h2i('#009ACD')]=_('Deep sky blue')
COLOR_NAMES[clr_h2i('#009B7D')]=_('Paolo Veronese green')
COLOR_NAMES[clr_h2i('#009E60')]=_('Shamrock green')
COLOR_NAMES[clr_h2i('#009F6B')]=_('Green')
COLOR_NAMES[clr_h2i('#00A550')]=_('Green')
COLOR_NAMES[clr_h2i('#00A693')]=_('Persian green')
COLOR_NAMES[clr_h2i('#00A86B')]=_('Jade')
COLOR_NAMES[clr_h2i('#00A877')]=_('Green')
COLOR_NAMES[clr_h2i('#00AAEE')]=_('Vivid cerulean')
COLOR_NAMES[clr_h2i('#00AB66')]=_('GO green')
COLOR_NAMES[clr_h2i('#00AD43')]=_('Green')
COLOR_NAMES[clr_h2i('#00B2EE')]=_('Deep sky blue')
COLOR_NAMES[clr_h2i('#00B7EB')]=_('Cyan')
COLOR_NAMES[clr_h2i('#00B9FB')]=_('Blue bolt')
COLOR_NAMES[clr_h2i('#00BFFF')]=_('Deep sky blue')
COLOR_NAMES[clr_h2i('#00C4B0')]=_('Amazonite')
COLOR_NAMES[clr_h2i('#00C5CD')]=_('Turquoise')
COLOR_NAMES[clr_h2i('#00CC33')]=_('Vivid malachite')
COLOR_NAMES[clr_h2i('#00CC99')]=_('Caribbean green')
COLOR_NAMES[clr_h2i('#00CCCC')]=_('Robin egg blue')
COLOR_NAMES[clr_h2i('#00CCFF')]=_('Vivid sky blue')
COLOR_NAMES[clr_h2i('#00CD00')]=_('Green')
COLOR_NAMES[clr_h2i('#00CD66')]=_('Spring green')
COLOR_NAMES[clr_h2i('#00CDCD')]=_('Cyan')
COLOR_NAMES[clr_h2i('#00CED1')]=_('Dark turquoise')
COLOR_NAMES[clr_h2i('#00E5EE')]=_('Turquoise')
COLOR_NAMES[clr_h2i('#00EE00')]=_('Green')
COLOR_NAMES[clr_h2i('#00EE76')]=_('Spring green')
COLOR_NAMES[clr_h2i('#00EEEE')]=_('Cyan')
COLOR_NAMES[clr_h2i('#00F5FF')]=_('Turquoise')
COLOR_NAMES[clr_h2i('#00FA9A')]=_('Medium spring green')
COLOR_NAMES[clr_h2i('#00FF00')]=_('Lime green ')
COLOR_NAMES[clr_h2i('#00FF7F')]=_('Spring green')
COLOR_NAMES[clr_h2i('#00FFEF')]=_('Turquoise blue')
COLOR_NAMES[clr_h2i('#00FFFF')]=_('Cyan, Spanish sky blue')
COLOR_NAMES[clr_h2i('#010203')]=_('Rich black')
COLOR_NAMES[clr_h2i('#010B13')]=_('Rich black')
COLOR_NAMES[clr_h2i('#013220')]=_('Dark green')
COLOR_NAMES[clr_h2i('#014421')]=_('Forest green (traditional)')
COLOR_NAMES[clr_h2i('#01796F')]=_('Pine green')
COLOR_NAMES[clr_h2i('#0247FE')]=_('Blue')
COLOR_NAMES[clr_h2i('#035096')]=_('Medium electric blue')
COLOR_NAMES[clr_h2i('#03C03C')]=_('Dark pastel green')
COLOR_NAMES[clr_h2i('#056608')]=_('Deep green')
COLOR_NAMES[clr_h2i('#059033')]=_('North Texas green')
COLOR_NAMES[clr_h2i('#062A78')]=_('Catalina blue')
COLOR_NAMES[clr_h2i('#08457E')]=_('Dark cerulean')
COLOR_NAMES[clr_h2i('#087830')]=_('La Salle green')
COLOR_NAMES[clr_h2i('#0892D0')]=_('Rich electric blue')
COLOR_NAMES[clr_h2i('#08E8DE')]=_('Bright turquoise')
COLOR_NAMES[clr_h2i('#091F92')]=_('Indigo dye')
COLOR_NAMES[clr_h2i('#0A7E8C')]=_('Metallic seaweed')
COLOR_NAMES[clr_h2i('#0ABAB5')]=_('Tiffany blue')
COLOR_NAMES[clr_h2i('#0BDA51')]=_('Malachite')
COLOR_NAMES[clr_h2i('#0D98BA')]=_('Blue-green')
COLOR_NAMES[clr_h2i('#0E7C61')]=_('Deep green-cyan turquoise')
COLOR_NAMES[clr_h2i('#0F4D92')]=_('Yale blue')
COLOR_NAMES[clr_h2i('#0F52BA')]=_('Sapphire')
COLOR_NAMES[clr_h2i('#0FC0FC')]=_('Spiro Disco Ball')
COLOR_NAMES[clr_h2i('#100C08')]=_('Smoky black')
COLOR_NAMES[clr_h2i('#1034A6')]=_('Egyptian blue')
COLOR_NAMES[clr_h2i('#104E8B')]=_('Dodger blue')
COLOR_NAMES[clr_h2i('#1164B4')]=_('Green-blue')
COLOR_NAMES[clr_h2i('#123524')]=_('Phthalo green')
COLOR_NAMES[clr_h2i('#126180')]=_('Blue sapphire')
COLOR_NAMES[clr_h2i('#138808')]=_('India green')
COLOR_NAMES[clr_h2i('#1560BD')]=_('Denim')
COLOR_NAMES[clr_h2i('#15F2FD')]=_('Vomit+indogo+Lopen+Gabriel')
COLOR_NAMES[clr_h2i('#177245')]=_('Dark spring green')
COLOR_NAMES[clr_h2i('#18453B')]=_('MSU green')
COLOR_NAMES[clr_h2i('#1874CD')]=_('Dodger blue')
COLOR_NAMES[clr_h2i('#188BC2')]=_('Cyan cornflower blue')
COLOR_NAMES[clr_h2i('#191970')]=_('Midnight blue')
COLOR_NAMES[clr_h2i('#195905')]=_('Lincoln green')
COLOR_NAMES[clr_h2i('#1974D2')]=_('Bright navy blue')
COLOR_NAMES[clr_h2i('#1A1110')]=_('Licorice')
COLOR_NAMES[clr_h2i('#1A2421')]=_('Dark jungle green')
COLOR_NAMES[clr_h2i('#1B1B1B')]=_('Eerie black')
COLOR_NAMES[clr_h2i('#1B4D3E')]=_('English green')
COLOR_NAMES[clr_h2i('#1C1C1C')]=_('Grey')
COLOR_NAMES[clr_h2i('#1C1CF0')]=_('Bluebonnet')
COLOR_NAMES[clr_h2i('#1C2841')]=_('Yankees blue')
COLOR_NAMES[clr_h2i('#1C352D')]=_('Medium jungle green')
COLOR_NAMES[clr_h2i('#1C39BB')]=_('Persian blue')
COLOR_NAMES[clr_h2i('#1C86EE')]=_('Dodger blue')
COLOR_NAMES[clr_h2i('#1CA9C9')]=_('Pacific blue')
COLOR_NAMES[clr_h2i('#1CAC78')]=_('Green')
COLOR_NAMES[clr_h2i('#1D2951')]=_('Space cadet')
COLOR_NAMES[clr_h2i('#1DACD6')]=_('Battery charged blue')
COLOR_NAMES[clr_h2i('#1E4D2B')]=_('Cal Poly Pomona green')
COLOR_NAMES[clr_h2i('#1E90FF')]=_('Dodger blue')
COLOR_NAMES[clr_h2i('#1F262A')]=_('Dark gunmetal')
COLOR_NAMES[clr_h2i('#1F75FE')]=_('Blue')
COLOR_NAMES[clr_h2i('#20B2AA')]=_('Light sea green')
COLOR_NAMES[clr_h2i('#210837')]=_('Middle Red purple')
COLOR_NAMES[clr_h2i('#214FC6')]=_('New car')
COLOR_NAMES[clr_h2i('#21ABCD')]=_('Ball blue')
COLOR_NAMES[clr_h2i('#2243B6')]=_('Denim blue')
COLOR_NAMES[clr_h2i('#228B22')]=_('Forest green')
COLOR_NAMES[clr_h2i('#23297A')]=_('St. Patrick\'s blue')
COLOR_NAMES[clr_h2i('#232B2B')]=_('Charleston green')
COLOR_NAMES[clr_h2i('#242124')]=_('Raisin black')
COLOR_NAMES[clr_h2i('#24A0ED')]=_('Button blue')
COLOR_NAMES[clr_h2i('#253529')]=_('Black leather jacket')
COLOR_NAMES[clr_h2i('#264348')]=_('Japanese indigo')
COLOR_NAMES[clr_h2i('#26619C')]=_('Lapis lazuli')
COLOR_NAMES[clr_h2i('#273BE2')]=_('Palatinate blue')
COLOR_NAMES[clr_h2i('#27408B')]=_('Royal blue')
COLOR_NAMES[clr_h2i('#28589C')]=_('Cyan cobalt blue')
COLOR_NAMES[clr_h2i('#299617')]=_('Slimy green')
COLOR_NAMES[clr_h2i('#29AB87')]=_('Jungle green')
COLOR_NAMES[clr_h2i('#2a3439')]=_('Gunmetal')
COLOR_NAMES[clr_h2i('#2A52BE')]=_('Cerulean blue')
COLOR_NAMES[clr_h2i('#2A8000')]=_('Napier green')
COLOR_NAMES[clr_h2i('#2C1608')]=_('Zinnwaldite brown')
COLOR_NAMES[clr_h2i('#2E2D88')]=_('Cosmic cobalt')
COLOR_NAMES[clr_h2i('#2E5894')]=_('B\'dazzled blue')
COLOR_NAMES[clr_h2i('#2E8B57')]=_('Sea green')
COLOR_NAMES[clr_h2i('#2F4F4F')]=_('Dark slate gray')
COLOR_NAMES[clr_h2i('#2F847C')]=_('Celadon green')
COLOR_NAMES[clr_h2i('#301934')]=_('Dark purple')
COLOR_NAMES[clr_h2i('#306030')]=_('Mughal green')
COLOR_NAMES[clr_h2i('#30BA8F')]=_('Mountain Meadow')
COLOR_NAMES[clr_h2i('#30BFBF')]=_('Maximum blue green')
COLOR_NAMES[clr_h2i('#317873')]=_('Myrtle green')
COLOR_NAMES[clr_h2i('#318CE7')]=_('Bleu de France')
COLOR_NAMES[clr_h2i('#319177')]=_('Illuminating emerald')
COLOR_NAMES[clr_h2i('#32127A')]=_('Persian indigo')
COLOR_NAMES[clr_h2i('#32174D')]=_('Russian violet')
COLOR_NAMES[clr_h2i('#324AB2')]=_('Violet-blue')
COLOR_NAMES[clr_h2i('#32CD32')]=_('Lime green')
COLOR_NAMES[clr_h2i('#330066')]=_('Deep violet')
COLOR_NAMES[clr_h2i('#333366')]=_('Deep koamaru')
COLOR_NAMES[clr_h2i('#333399')]=_('Blue')
COLOR_NAMES[clr_h2i('#3399FF')]=_('Brilliant azure')
COLOR_NAMES[clr_h2i('#343434')]=_('Jet')
COLOR_NAMES[clr_h2i('#34B233')]=_('Wageningen green')
COLOR_NAMES[clr_h2i('#353839')]=_('Onyx')
COLOR_NAMES[clr_h2i('#354230')]=_('Kombu green')
COLOR_NAMES[clr_h2i('#355E3B')]=_('Deep moss green')
COLOR_NAMES[clr_h2i('#360CCC')]=_('Interdimensional blue')
COLOR_NAMES[clr_h2i('#363636')]=_('Grey')
COLOR_NAMES[clr_h2i('#36454F')]=_('Charcoal')
COLOR_NAMES[clr_h2i('#36648B')]=_('Steel blue')
COLOR_NAMES[clr_h2i('#36747D')]=_('Ming')
COLOR_NAMES[clr_h2i('#367588')]=_('Teal blue')
COLOR_NAMES[clr_h2i('#391285')]=_('Pixie powder')
COLOR_NAMES[clr_h2i('#39A78E')]=_('Zomp')
COLOR_NAMES[clr_h2i('#39FF14')]=_('Neon green')
COLOR_NAMES[clr_h2i('#3A5FCD')]=_('Royal blue')
COLOR_NAMES[clr_h2i('#3AB09E')]=_('Keppel')
COLOR_NAMES[clr_h2i('#3B331C')]=_('Pullman green')
COLOR_NAMES[clr_h2i('#3B3C36')]=_('Black olive')
COLOR_NAMES[clr_h2i('#3B444B')]=_('Arsenic')
COLOR_NAMES[clr_h2i('#3B7A57')]=_('Amazon')
COLOR_NAMES[clr_h2i('#3C1414')]=_('Dark sienna')
COLOR_NAMES[clr_h2i('#3C341F')]=_('Olive Drab #7')
COLOR_NAMES[clr_h2i('#3CB371')]=_('Medium sea green')
COLOR_NAMES[clr_h2i('#3CD070')]=_('UFO green')
COLOR_NAMES[clr_h2i('#3D0C02')]=_('Black bean')
COLOR_NAMES[clr_h2i('#3D2B1F')]=_('Bistre')
COLOR_NAMES[clr_h2i('#3E8EDE')]=_('Tufts blue')
COLOR_NAMES[clr_h2i('#3EB489')]=_('Mint')
COLOR_NAMES[clr_h2i('#3F00FF')]=_('Ultramarine')
COLOR_NAMES[clr_h2i('#3FFF00')]=_('Harlequin')
COLOR_NAMES[clr_h2i('#40826D')]=_('Deep aquamarine')
COLOR_NAMES[clr_h2i('#40E0D0')]=_('Turquoise')
COLOR_NAMES[clr_h2i('#414A4C')]=_('Outer space')
COLOR_NAMES[clr_h2i('#4166F5')]=_('Ultramarine blue')
COLOR_NAMES[clr_h2i('#4169E1')]=_('Royal blue')
COLOR_NAMES[clr_h2i('#43302E')]=_('Old burgundy')
COLOR_NAMES[clr_h2i('#436B95')]=_('Queen blue')
COLOR_NAMES[clr_h2i('#436EEE')]=_('Royal blue')
COLOR_NAMES[clr_h2i('#43B3AE')]=_('Verdigris')
COLOR_NAMES[clr_h2i('#43CD80')]=_('Sea green')
COLOR_NAMES[clr_h2i('#444C38')]=_('Rifle green')
COLOR_NAMES[clr_h2i('#446CCF')]=_('Han blue')
COLOR_NAMES[clr_h2i('#44D7A8')]=_('Eucalyptus')
COLOR_NAMES[clr_h2i('#458B00')]=_('Chartreuse')
COLOR_NAMES[clr_h2i('#458B74')]=_('Aquamarine')
COLOR_NAMES[clr_h2i('#45B1E8')]=_('Picton blue')
COLOR_NAMES[clr_h2i('#465945')]=_('Gray-asparagus')
COLOR_NAMES[clr_h2i('#4682B4')]=_('Steel blue')
COLOR_NAMES[clr_h2i('#4682BF')]=_('Cyan-blue azure')
COLOR_NAMES[clr_h2i('#46CB18')]=_('Harlequin green')
COLOR_NAMES[clr_h2i('#473C8B')]=_('Slate blue')
COLOR_NAMES[clr_h2i('#47ABCC')]=_('Maximum blue')
COLOR_NAMES[clr_h2i('#480607')]=_('Bulgarian rose')
COLOR_NAMES[clr_h2i('#483C32')]=_('Taupe')
COLOR_NAMES[clr_h2i('#483D8B')]=_('Dark slate blue')
COLOR_NAMES[clr_h2i('#4876FF')]=_('Royal blue')
COLOR_NAMES[clr_h2i('#48BF91')]=_('Ocean green')
COLOR_NAMES[clr_h2i('#48D1CC')]=_('Medium turquoise')
COLOR_NAMES[clr_h2i('#49796B')]=_('Hooker\'s green')
COLOR_NAMES[clr_h2i('#4997D0')]=_('Celestial blue')
COLOR_NAMES[clr_h2i('#4A5D23')]=_('Dark moss green')
COLOR_NAMES[clr_h2i('#4A646C')]=_('Deep space sparkle')
COLOR_NAMES[clr_h2i('#4A708B')]=_('Sky blue')
COLOR_NAMES[clr_h2i('#4AFF00')]=_('Chlorophyll green')
COLOR_NAMES[clr_h2i('#4B0082')]=_('Indigo')
COLOR_NAMES[clr_h2i('#4B3621')]=_('Cafe noir')
COLOR_NAMES[clr_h2i('#4B5320')]=_('Army green')
COLOR_NAMES[clr_h2i('#4BC7CF')]=_('Sea serpent')
COLOR_NAMES[clr_h2i('#4C2882')]=_('Spanish violet')
COLOR_NAMES[clr_h2i('#4C516D')]=_('Independence')
COLOR_NAMES[clr_h2i('#4C9141')]=_('May green')
COLOR_NAMES[clr_h2i('#4CBB17')]=_('Kelly green')
COLOR_NAMES[clr_h2i('#4D5D53')]=_('Feldgrau')
COLOR_NAMES[clr_h2i('#4D8C57')]=_('Middle green')
COLOR_NAMES[clr_h2i('#4E1609')]=_('French puce')
COLOR_NAMES[clr_h2i('#4E5180')]=_('Purple navy')
COLOR_NAMES[clr_h2i('#4E82B4')]=_('Cyan azure')
COLOR_NAMES[clr_h2i('#4EEE94')]=_('Sea green')
COLOR_NAMES[clr_h2i('#4F3A3C')]=_('Dark puce')
COLOR_NAMES[clr_h2i('#4F42B5')]=_('Ocean blue')
COLOR_NAMES[clr_h2i('#4F4F4F')]=_('Grey')
COLOR_NAMES[clr_h2i('#4F666A')]=_('Stormcloud')
COLOR_NAMES[clr_h2i('#4F7942')]=_('Fern green')
COLOR_NAMES[clr_h2i('#4F86F7')]=_('Blueberry')
COLOR_NAMES[clr_h2i('#4F94CD')]=_('Steel blue')
COLOR_NAMES[clr_h2i('#50404D')]=_('Purple taupe')
COLOR_NAMES[clr_h2i('#5072A7')]=_('Blue yonder')
COLOR_NAMES[clr_h2i('#507D2A')]=_('Sap green')
COLOR_NAMES[clr_h2i('#50C878')]=_('Emerald')
COLOR_NAMES[clr_h2i('#512888')]=_('KSU purple')
COLOR_NAMES[clr_h2i('#51484F')]=_('Quartz')
COLOR_NAMES[clr_h2i('#5218FA')]=_('Han purple')
COLOR_NAMES[clr_h2i('#522D80')]=_('Regalia')
COLOR_NAMES[clr_h2i('#528B8B')]=_('Dark slate gray')
COLOR_NAMES[clr_h2i('#534B4F')]=_('Dark liver')
COLOR_NAMES[clr_h2i('#536872')]=_('Cadet')
COLOR_NAMES[clr_h2i('#536878')]=_('Dark electric blue')
COLOR_NAMES[clr_h2i('#536895')]=_('UCLA blue')
COLOR_NAMES[clr_h2i('#53868B')]=_('Cadet blue')
COLOR_NAMES[clr_h2i('#543D37')]=_('Dark liver (horses)')
COLOR_NAMES[clr_h2i('#545AA7')]=_('Liberty')
COLOR_NAMES[clr_h2i('#54626F')]=_('Black Coral')
COLOR_NAMES[clr_h2i('#548B54')]=_('Pale green')
COLOR_NAMES[clr_h2i('#54FF9F')]=_('Sea green')
COLOR_NAMES[clr_h2i('#551A8B')]=_('Purple')
COLOR_NAMES[clr_h2i('#553592')]=_('Blue-magenta violet')
COLOR_NAMES[clr_h2i('#555555')]=_('Davy\'s grey')
COLOR_NAMES[clr_h2i('#555D50')]=_('Ebony')
COLOR_NAMES[clr_h2i('#556B2F')]=_('Dark olive green')
COLOR_NAMES[clr_h2i('#560319')]=_('Dark scarlet')
COLOR_NAMES[clr_h2i('#563C5C')]=_('Pineapple')
COLOR_NAMES[clr_h2i('#568203')]=_('Avocado')
COLOR_NAMES[clr_h2i('#56887D')]=_('Wintergreen Dream')
COLOR_NAMES[clr_h2i('#56A0D3')]=_('Carolina blue')
COLOR_NAMES[clr_h2i('#58427C')]=_('Cyber grape')
COLOR_NAMES[clr_h2i('#59260B')]=_('Seal brown')
COLOR_NAMES[clr_h2i('#592720')]=_('Caput mortuum')
COLOR_NAMES[clr_h2i('#5946B2')]=_('Plump purple')
COLOR_NAMES[clr_h2i('#5A4FCF')]=_('Iris')
COLOR_NAMES[clr_h2i('#5B3256')]=_('Japanese violet')
COLOR_NAMES[clr_h2i('#5B92E5')]=_('United nations blue')
COLOR_NAMES[clr_h2i('#5CACEE')]=_('Steel blue')
COLOR_NAMES[clr_h2i('#5D3954')]=_('Dark byzantium')
COLOR_NAMES[clr_h2i('#5D478B')]=_('Medium purple')
COLOR_NAMES[clr_h2i('#5D89BA')]=_('Silver lake blue')
COLOR_NAMES[clr_h2i('#5D8AA8')]=_('Air Force blue')
COLOR_NAMES[clr_h2i('#5DA493')]=_('Polished Pine')
COLOR_NAMES[clr_h2i('#5DADEC')]=_('Blue jeans')
COLOR_NAMES[clr_h2i('#5E8C31')]=_('Maximum green')
COLOR_NAMES[clr_h2i('#5F8A8B')]=_('Steel teal')
COLOR_NAMES[clr_h2i('#5F9EA0')]=_('Cadet blue')
COLOR_NAMES[clr_h2i('#5FA778')]=_('Shiny shamrock')
COLOR_NAMES[clr_h2i('#602F6B')]=_('Imperial')
COLOR_NAMES[clr_h2i('#6050DC')]=_('Majorelle blue')
COLOR_NAMES[clr_h2i('#607B8B')]=_('Light sky blue')
COLOR_NAMES[clr_h2i('#6082B6')]=_('Glaucous')
COLOR_NAMES[clr_h2i('#614051')]=_('Eggplant')
COLOR_NAMES[clr_h2i('#635147')]=_('Umber')
COLOR_NAMES[clr_h2i('#63B8FF')]=_('Steel blue')
COLOR_NAMES[clr_h2i('#644117')]=_('Pullman brown')
COLOR_NAMES[clr_h2i('#645452')]=_('Wenge')
COLOR_NAMES[clr_h2i('#6495ED')]=_('Cornflower blue')
COLOR_NAMES[clr_h2i('#64E986')]=_('Very light malachite green')
COLOR_NAMES[clr_h2i('#65000B')]=_('Rosewood')
COLOR_NAMES[clr_h2i('#654321')]=_('Dark brown')
COLOR_NAMES[clr_h2i('#66023C')]=_('Imperial purple')
COLOR_NAMES[clr_h2i('#663399')]=_('Rebecca purple')
COLOR_NAMES[clr_h2i('#663854')]=_('Halaya ube')
COLOR_NAMES[clr_h2i('#664228')]=_('Van dyke brown')
COLOR_NAMES[clr_h2i('#66424D')]=_('Deep tuscan red')
COLOR_NAMES[clr_h2i('#664C28')]=_('Donkey brown')
COLOR_NAMES[clr_h2i('#665D1E')]=_('Antique bronze')
COLOR_NAMES[clr_h2i('#666699')]=_('Dark blue-gray')
COLOR_NAMES[clr_h2i('#6666FF')]=_('Very light blue')
COLOR_NAMES[clr_h2i('#668B8B')]=_('Pale turquoise')
COLOR_NAMES[clr_h2i('#669999')]=_('Desaturated cyan')
COLOR_NAMES[clr_h2i('#6699CC')]=_('Livid')
COLOR_NAMES[clr_h2i('#66B032')]=_('Green')
COLOR_NAMES[clr_h2i('#66CD00')]=_('Chartreuse')
COLOR_NAMES[clr_h2i('#66CDAA')]=_('Medium aquamarine')
COLOR_NAMES[clr_h2i('#66DDAA')]=_('Medium aquamarine')
COLOR_NAMES[clr_h2i('#66FF00')]=_('Bright green')
COLOR_NAMES[clr_h2i('#66FF66')]=_('Screamin\' green')
COLOR_NAMES[clr_h2i('#673147')]=_('Wine dregs')
COLOR_NAMES[clr_h2i('#674846')]=_('Rose ebony')
COLOR_NAMES[clr_h2i('#674C47')]=_('Medium taupe')
COLOR_NAMES[clr_h2i('#676767')]=_('Granite Gray')
COLOR_NAMES[clr_h2i('#679267')]=_('Russian green')
COLOR_NAMES[clr_h2i('#68228B')]=_('Dark orchid')
COLOR_NAMES[clr_h2i('#682860')]=_('Palatinate purple')
COLOR_NAMES[clr_h2i('#68838B')]=_('Light blue')
COLOR_NAMES[clr_h2i('#69359C')]=_('Purple heart')
COLOR_NAMES[clr_h2i('#6959CD')]=_('Slate blue')
COLOR_NAMES[clr_h2i('#696969')]=_('Dim gray')
COLOR_NAMES[clr_h2i('#698B22')]=_('Olive drab')
COLOR_NAMES[clr_h2i('#698B69')]=_('Dark sea green')
COLOR_NAMES[clr_h2i('#6A5ACD')]=_('Slate blue')
COLOR_NAMES[clr_h2i('#6B4423')]=_('Kobicha')
COLOR_NAMES[clr_h2i('#6B8E23')]=_('Olive drab')
COLOR_NAMES[clr_h2i('#6C2E1F')]=_('Liver (organ)')
COLOR_NAMES[clr_h2i('#6C3082')]=_('Eminence')
COLOR_NAMES[clr_h2i('#6C541E')]=_('Field drab')
COLOR_NAMES[clr_h2i('#6C7B8B')]=_('Slate gray')
COLOR_NAMES[clr_h2i('#6CA0DC')]=_('Little boy blue')
COLOR_NAMES[clr_h2i('#6CA6CD')]=_('Sky blue')
COLOR_NAMES[clr_h2i('#6D9BC3')]=_('Cerulean frost')
COLOR_NAMES[clr_h2i('#6E7B8B')]=_('Light steel blue')
COLOR_NAMES[clr_h2i('#6E7F80')]=_('Auro metal saurus')
COLOR_NAMES[clr_h2i('#6E8B3D')]=_('Dark olive green')
COLOR_NAMES[clr_h2i('#6EAEA1')]=_('Green Sheen')
COLOR_NAMES[clr_h2i('#6F00FF')]=_('Electric indigo')
COLOR_NAMES[clr_h2i('#6F2DA8')]=_('Grape')
COLOR_NAMES[clr_h2i('#6F4E37')]=_('Tuscan brown')
COLOR_NAMES[clr_h2i('#6F9940')]=_('Palm Leaf')
COLOR_NAMES[clr_h2i('#701C1C')]=_('Persian plum')
COLOR_NAMES[clr_h2i('#702670')]=_('Midnight')
COLOR_NAMES[clr_h2i('#702963')]=_('Byzantium')
COLOR_NAMES[clr_h2i('#703642')]=_('Catawba')
COLOR_NAMES[clr_h2i('#704214')]=_('Sepia')
COLOR_NAMES[clr_h2i('#704241')]=_('Roast coffee')
COLOR_NAMES[clr_h2i('#708090')]=_('Slate gray')
COLOR_NAMES[clr_h2i('#71A6D2')]=_('Iceberg')
COLOR_NAMES[clr_h2i('#71BC78')]=_('Iguana green')
COLOR_NAMES[clr_h2i('#722F37')]=_('Puce red')
COLOR_NAMES[clr_h2i('#727472')]=_('Nickel')
COLOR_NAMES[clr_h2i('#72A0C1')]=_('Air superiority blue')
COLOR_NAMES[clr_h2i('#733380')]=_('Maximum purple')
COLOR_NAMES[clr_h2i('#734F96')]=_('Dark lavender')
COLOR_NAMES[clr_h2i('#737000')]=_('Bronze yellow')
COLOR_NAMES[clr_h2i('#738276')]=_('Smoke')
COLOR_NAMES[clr_h2i('#738678')]=_('Xanadu')
COLOR_NAMES[clr_h2i('#73A9C2')]=_('Moonstone blue')
COLOR_NAMES[clr_h2i('#73C2FB')]=_('Maya blue')
COLOR_NAMES[clr_h2i('#746CC0')]=_('Toolbox')
COLOR_NAMES[clr_h2i('#74BBFB')]=_('Very light azure')
COLOR_NAMES[clr_h2i('#74C365')]=_('Mantis')
COLOR_NAMES[clr_h2i('#757575')]=_('Sonic silver')
COLOR_NAMES[clr_h2i('#76EE00')]=_('Chartreuse')
COLOR_NAMES[clr_h2i('#76EEC6')]=_('Aquamarine')
COLOR_NAMES[clr_h2i('#777696')]=_('Rhythm')
COLOR_NAMES[clr_h2i('#778899')]=_('Light slate gray')
COLOR_NAMES[clr_h2i('#778BA5')]=_('Shadow blue')
COLOR_NAMES[clr_h2i('#779ECB')]=_('Dark pastel blue')
COLOR_NAMES[clr_h2i('#77B5FE')]=_('French sky blue')
COLOR_NAMES[clr_h2i('#77DD77')]=_('Pastel green')
COLOR_NAMES[clr_h2i('#78184A')]=_('Pansy purple')
COLOR_NAMES[clr_h2i('#7851A9')]=_('Royal purple')
COLOR_NAMES[clr_h2i('#78866B')]=_('Camouflage green')
COLOR_NAMES[clr_h2i('#79443B')]=_('Medium tuscan red')
COLOR_NAMES[clr_h2i('#796878')]=_('Old lavender')
COLOR_NAMES[clr_h2i('#79CDCD')]=_('Dark slate gray')
COLOR_NAMES[clr_h2i('#7A378B')]=_('Medium orchid')
COLOR_NAMES[clr_h2i('#7A67EE')]=_('Slate blue')
COLOR_NAMES[clr_h2i('#7A8B8B')]=_('Light cyan')
COLOR_NAMES[clr_h2i('#7AC5CD')]=_('Cadet blue')
COLOR_NAMES[clr_h2i('#7B1113')]=_('UP maroon')
COLOR_NAMES[clr_h2i('#7B3F00')]=_('Chocolate (traditional)')
COLOR_NAMES[clr_h2i('#7B68EE')]=_('Medium slate blue')
COLOR_NAMES[clr_h2i('#7BB661')]=_('Bud green')
COLOR_NAMES[clr_h2i('#7C0A02')]=_('Barn red')
COLOR_NAMES[clr_h2i('#7C1C05')]=_('Kenyan copper')
COLOR_NAMES[clr_h2i('#7C4848')]=_('Tuscan red')
COLOR_NAMES[clr_h2i('#7C98AB')]=_('Weldon blue')
COLOR_NAMES[clr_h2i('#7C9ED9')]=_('Vista blue')
COLOR_NAMES[clr_h2i('#7CB9E8')]=_('Aero')
COLOR_NAMES[clr_h2i('#7CCD7C')]=_('Pale green')
COLOR_NAMES[clr_h2i('#7CFC00')]=_('Lawn green')
COLOR_NAMES[clr_h2i('#7D26CD')]=_('Purple')
COLOR_NAMES[clr_h2i('#7DF9FF')]=_('Electric blue')
COLOR_NAMES[clr_h2i('#7E5E60')]=_('Deep taupe')
COLOR_NAMES[clr_h2i('#7EC0EE')]=_('Sky blue')
COLOR_NAMES[clr_h2i('#7ED4E6')]=_('Middle blue')
COLOR_NAMES[clr_h2i('#7F00FF')]=_('Violet')
COLOR_NAMES[clr_h2i('#7F1734')]=_('Claret')
COLOR_NAMES[clr_h2i('#7FFF00')]=_('Chartreuse')
COLOR_NAMES[clr_h2i('#7FFFD4')]=_('Aquamarine')
COLOR_NAMES[clr_h2i('#800000')]=_('Maroon')
COLOR_NAMES[clr_h2i('#800020')]=_('Burgundy')
COLOR_NAMES[clr_h2i('#800080')]=_('Patriarch, purple')
COLOR_NAMES[clr_h2i('#801818')]=_('Falu red')
COLOR_NAMES[clr_h2i('#80461B')]=_('Russet')
COLOR_NAMES[clr_h2i('#807532')]=_('Spanish bistre')
COLOR_NAMES[clr_h2i('#808000')]=_('Olive')
COLOR_NAMES[clr_h2i('#808080')]=_('Trolley grey')
COLOR_NAMES[clr_h2i('#80DAEB')]=_('Medium sky blue')
COLOR_NAMES[clr_h2i('#811453')]=_('French plum')
COLOR_NAMES[clr_h2i('#81613C')]=_('Coyote brown')
COLOR_NAMES[clr_h2i('#820000')]=_('Deep maroon')
COLOR_NAMES[clr_h2i('#826644')]=_('Raw umber')
COLOR_NAMES[clr_h2i('#828E84')]=_('Dolphin gray')
COLOR_NAMES[clr_h2i('#832A0D')]=_('Smokey topaz')
COLOR_NAMES[clr_h2i('#836953')]=_('Pastel brown')
COLOR_NAMES[clr_h2i('#836FFF')]=_('Slate blue')
COLOR_NAMES[clr_h2i('#838996')]=_('Roman silver')
COLOR_NAMES[clr_h2i('#838B83')]=_('Honeydew')
COLOR_NAMES[clr_h2i('#838B8B')]=_('Azure')
COLOR_NAMES[clr_h2i('#841B2D')]=_('Antique ruby')
COLOR_NAMES[clr_h2i('#843F5B')]=_('Deep ruby')
COLOR_NAMES[clr_h2i('#8470FF')]=_('Light slate blue')
COLOR_NAMES[clr_h2i('#848482')]=_('Old silver')
COLOR_NAMES[clr_h2i('#84DE02')]=_('Alien armpit')
COLOR_NAMES[clr_h2i('#850101')]=_('Deep red')
COLOR_NAMES[clr_h2i('#856088')]=_('Chinese violet')
COLOR_NAMES[clr_h2i('#856D4D')]=_('French bistre')
COLOR_NAMES[clr_h2i('#85754E')]=_('Gold Fusion')
COLOR_NAMES[clr_h2i('#85BB65')]=_('Dollar bill')
COLOR_NAMES[clr_h2i('#860111')]=_('Red devil')
COLOR_NAMES[clr_h2i('#8601AF')]=_('Violet')
COLOR_NAMES[clr_h2i('#86608E')]=_('French lilac')
COLOR_NAMES[clr_h2i('#867E36')]=_('Old moss green')
COLOR_NAMES[clr_h2i('#872657')]=_('Dark raspberry')
COLOR_NAMES[clr_h2i('#873260')]=_('Boysenberry')
COLOR_NAMES[clr_h2i('#87A96B')]=_('Asparagus')
COLOR_NAMES[clr_h2i('#87CEEB')]=_('Sky blue')
COLOR_NAMES[clr_h2i('#87CEFA')]=_('Light sky blue')
COLOR_NAMES[clr_h2i('#87CEFF')]=_('Sky blue')
COLOR_NAMES[clr_h2i('#87D3F8')]=_('Pale cyan')
COLOR_NAMES[clr_h2i('#87FF2A')]=_('Spring Frost')
COLOR_NAMES[clr_h2i('#880085')]=_('Mardi gras')
COLOR_NAMES[clr_h2i('#8806CE')]=_('French violet')
COLOR_NAMES[clr_h2i('#882D17')]=_('Sienna')
COLOR_NAMES[clr_h2i('#885818')]=_('Grizzly')
COLOR_NAMES[clr_h2i('#88654E')]=_('Dark brown-tangelo')
COLOR_NAMES[clr_h2i('#8878C3')]=_('Ube')
COLOR_NAMES[clr_h2i('#88ACE0')]=_('Light cobalt blue')
COLOR_NAMES[clr_h2i('#88D8C0')]=_('Pearl aqua')
COLOR_NAMES[clr_h2i('#893843')]=_('Solid pink')
COLOR_NAMES[clr_h2i('#893F45')]=_('Cordovan')
COLOR_NAMES[clr_h2i('#8968CD')]=_('Medium purple')
COLOR_NAMES[clr_h2i('#89CFF0')]=_('Baby blue')
COLOR_NAMES[clr_h2i('#8A2BE2')]=_('Blue-violet')
COLOR_NAMES[clr_h2i('#8A3324')]=_('Burnt umber')
COLOR_NAMES[clr_h2i('#8A496B')]=_('Twilight lavender')
COLOR_NAMES[clr_h2i('#8A795D')]=_('Shadow')
COLOR_NAMES[clr_h2i('#8A7F80')]=_('Rocket metallic')
COLOR_NAMES[clr_h2i('#8A9A5B')]=_('Turtle green')
COLOR_NAMES[clr_h2i('#8AB9F1')]=_('Jordy blue')
COLOR_NAMES[clr_h2i('#8B0000')]=_('Dark red')
COLOR_NAMES[clr_h2i('#8B008B')]=_('Dark magenta')
COLOR_NAMES[clr_h2i('#8B0A50')]=_('Deep pink')
COLOR_NAMES[clr_h2i('#8B1A1A')]=_('Firebrick')
COLOR_NAMES[clr_h2i('#8B1C62')]=_('Maroon')
COLOR_NAMES[clr_h2i('#8B2252')]=_('Violet red')
COLOR_NAMES[clr_h2i('#8B2323')]=_('Brown')
COLOR_NAMES[clr_h2i('#8B2500')]=_('Orange red')
COLOR_NAMES[clr_h2i('#8B3626')]=_('Tomato')
COLOR_NAMES[clr_h2i('#8B3A3A')]=_('Indian red')
COLOR_NAMES[clr_h2i('#8B3A62')]=_('Hot pink')
COLOR_NAMES[clr_h2i('#8B3E2F')]=_('Coral')
COLOR_NAMES[clr_h2i('#8B4500')]=_('Dark orange')
COLOR_NAMES[clr_h2i('#8B4513')]=_('Saddle brown')
COLOR_NAMES[clr_h2i('#8B4726')]=_('Sienna')
COLOR_NAMES[clr_h2i('#8B475D')]=_('Pale violet red')
COLOR_NAMES[clr_h2i('#8B4789')]=_('Orchid')
COLOR_NAMES[clr_h2i('#8B4C39')]=_('Salmon')
COLOR_NAMES[clr_h2i('#8B5742')]=_('Light salmon')
COLOR_NAMES[clr_h2i('#8B5A00')]=_('Orange')
COLOR_NAMES[clr_h2i('#8B5A2B')]=_('Tan')
COLOR_NAMES[clr_h2i('#8B5f4D')]=_('Spicy mix')
COLOR_NAMES[clr_h2i('#8B5F65')]=_('Light pink')
COLOR_NAMES[clr_h2i('#8B636C')]=_('Pink')
COLOR_NAMES[clr_h2i('#8B658B')]=_('Dark goldenrod')
COLOR_NAMES[clr_h2i('#8B668B')]=_('Plum')
COLOR_NAMES[clr_h2i('#8B6914')]=_('Goldenrod')
COLOR_NAMES[clr_h2i('#8B6969')]=_('Rosy brown')
COLOR_NAMES[clr_h2i('#8B72BE')]=_('Middle blue purple')
COLOR_NAMES[clr_h2i('#8B7355')]=_('Burlywood')
COLOR_NAMES[clr_h2i('#8B7500')]=_('Gold')
COLOR_NAMES[clr_h2i('#8B7765')]=_('Peach puff')
COLOR_NAMES[clr_h2i('#8B795E')]=_('Navajo white')
COLOR_NAMES[clr_h2i('#8B7B8B')]=_('Thistle')
COLOR_NAMES[clr_h2i('#8B7D6B')]=_('Bisque')
COLOR_NAMES[clr_h2i('#8B7D7B')]=_('Misty rose')
COLOR_NAMES[clr_h2i('#8B7E66')]=_('Wheat')
COLOR_NAMES[clr_h2i('#8B814C')]=_('Light goldenrod')
COLOR_NAMES[clr_h2i('#8B8378')]=_('Antique white')
COLOR_NAMES[clr_h2i('#8B8386')]=_('Lavender blush')
COLOR_NAMES[clr_h2i('#8B8589')]=_('Taupe gray')
COLOR_NAMES[clr_h2i('#8B864E')]=_('Khaki')
COLOR_NAMES[clr_h2i('#8B8682')]=_('Seashell')
COLOR_NAMES[clr_h2i('#8B8878')]=_('Cornsilk')
COLOR_NAMES[clr_h2i('#8B8970')]=_('Lemon chiffon')
COLOR_NAMES[clr_h2i('#8B8989')]=_('Snow')
COLOR_NAMES[clr_h2i('#8B8B00')]=_('Yellow')
COLOR_NAMES[clr_h2i('#8B8B7A')]=_('Light yellow')
COLOR_NAMES[clr_h2i('#8B8B83')]=_('Ivory')
COLOR_NAMES[clr_h2i('#8BA8B7')]=_('Pewter blue')
COLOR_NAMES[clr_h2i('#8C92AC')]=_('Cool grey, Gray-blue')
COLOR_NAMES[clr_h2i('#8CBED6')]=_('Dark sky blue')
COLOR_NAMES[clr_h2i('#8D4E85')]=_('Razzmic berry')
COLOR_NAMES[clr_h2i('#8DA399')]=_('Morning blue')
COLOR_NAMES[clr_h2i('#8DB600')]=_('Apple green')
COLOR_NAMES[clr_h2i('#8DB6CD')]=_('Light sky blue')
COLOR_NAMES[clr_h2i('#8DD9CC')]=_('Middle blue green')
COLOR_NAMES[clr_h2i('#8DEEEE')]=_('Dark slate gray')
COLOR_NAMES[clr_h2i('#8E3A59')]=_('Quinacridone magenta')
COLOR_NAMES[clr_h2i('#8E4585')]=_('Plum')
COLOR_NAMES[clr_h2i('#8EE53F')]=_('Kiwi')
COLOR_NAMES[clr_h2i('#8EE5EE')]=_('Cadet blue')
COLOR_NAMES[clr_h2i('#8F00FF')]=_('Violet')
COLOR_NAMES[clr_h2i('#8F9779')]=_('Artichoke')
COLOR_NAMES[clr_h2i('#8FBC8F')]=_('Dark sea green')
COLOR_NAMES[clr_h2i('#8FD400')]=_('Sheen green')
COLOR_NAMES[clr_h2i('#905D5D')]=_('Rose taupe')
COLOR_NAMES[clr_h2i('#90EE90')]=_('Light green')
COLOR_NAMES[clr_h2i('#912CEE')]=_('Purple')
COLOR_NAMES[clr_h2i('#914E75')]=_('Sugar plum')
COLOR_NAMES[clr_h2i('#915C83')]=_('Antique fuchsia')
COLOR_NAMES[clr_h2i('#915F6D')]=_('Mauve taupe')
COLOR_NAMES[clr_h2i('#918151')]=_('Dark tan')
COLOR_NAMES[clr_h2i('#91A3B0')]=_('Cadet grey')
COLOR_NAMES[clr_h2i('#92000A')]=_('Sangria')
COLOR_NAMES[clr_h2i('#922724')]=_('Vivid auburn')
COLOR_NAMES[clr_h2i('#92A1CF')]=_('Ceil')
COLOR_NAMES[clr_h2i('#933D41')]=_('Smoky Topaz')
COLOR_NAMES[clr_h2i('#9370DB')]=_('Medium purple')
COLOR_NAMES[clr_h2i('#93C572')]=_('Pistachio')
COLOR_NAMES[clr_h2i('#93CCEA')]=_('Light cornflower blue')
COLOR_NAMES[clr_h2i('#9400D3')]=_('Dark violet')
COLOR_NAMES[clr_h2i('#9457EB')]=_('Lavender indigo, Navy purple')
COLOR_NAMES[clr_h2i('#954535')]=_('Chestnut')
COLOR_NAMES[clr_h2i('#960018')]=_('Carmine, Heidelberg red')
COLOR_NAMES[clr_h2i('#964B00')]=_('Brown (traditional)')
COLOR_NAMES[clr_h2i('#965A3E')]=_('Coconut')
COLOR_NAMES[clr_h2i('#966FD6')]=_('Dark pastel purple')
COLOR_NAMES[clr_h2i('#967117')]=_('Sandy taupe')
COLOR_NAMES[clr_h2i('#9678B6')]=_('Purple mountain majesty')
COLOR_NAMES[clr_h2i('#967BB6')]=_('Lavender purple')
COLOR_NAMES[clr_h2i('#96C8A2')]=_('Eton blue')
COLOR_NAMES[clr_h2i('#96CDCD')]=_('Pale turquoise')
COLOR_NAMES[clr_h2i('#96DED1')]=_('Pale robin egg blue')
COLOR_NAMES[clr_h2i('#979AAA')]=_('Manatee')
COLOR_NAMES[clr_h2i('#97FFFF')]=_('Dark slate gray')
COLOR_NAMES[clr_h2i('#980036')]=_('Pink raspberry')
COLOR_NAMES[clr_h2i('#986960')]=_('Dark chestnut')
COLOR_NAMES[clr_h2i('#987456')]=_('Liver chestnut')
COLOR_NAMES[clr_h2i('#987654')]=_('Pale brown')
COLOR_NAMES[clr_h2i('#98777B')]=_('Bazaar')
COLOR_NAMES[clr_h2i('#98817B')]=_('Cinereous')
COLOR_NAMES[clr_h2i('#989898')]=_('Spanish gray')
COLOR_NAMES[clr_h2i('#98F5FF')]=_('Cadet blue')
COLOR_NAMES[clr_h2i('#98FB98')]=_('Pale green')
COLOR_NAMES[clr_h2i('#98FF98')]=_('Mint green')
COLOR_NAMES[clr_h2i('#990000')]=_('Crimson red')
COLOR_NAMES[clr_h2i('#9932CC')]=_('Dark orchid')
COLOR_NAMES[clr_h2i('#9955BB')]=_('Deep lilac')
COLOR_NAMES[clr_h2i('#996515')]=_('Golden brown')
COLOR_NAMES[clr_h2i('#996600')]=_('Gamboge orange (brown)')
COLOR_NAMES[clr_h2i('#996666')]=_('Copper rose')
COLOR_NAMES[clr_h2i('#9966CC')]=_('Amethyst')
COLOR_NAMES[clr_h2i('#997A8D')]=_('Mountbatten pink')
COLOR_NAMES[clr_h2i('#99E6B3')]=_('Teal deer')
COLOR_NAMES[clr_h2i('#9A32CD')]=_('Dark orchid')
COLOR_NAMES[clr_h2i('#9A4EAE')]=_('Purpureus')
COLOR_NAMES[clr_h2i('#9AB973')]=_('Olivine')
COLOR_NAMES[clr_h2i('#9AC0CD')]=_('Light blue')
COLOR_NAMES[clr_h2i('#9ACD32')]=_('Yellow-green')
COLOR_NAMES[clr_h2i('#9AFF9A')]=_('Pale green')
COLOR_NAMES[clr_h2i('#9B111E')]=_('Ruby red')
COLOR_NAMES[clr_h2i('#9B30FF')]=_('Purple')
COLOR_NAMES[clr_h2i('#9B7653')]=_('Dirt')
COLOR_NAMES[clr_h2i('#9B870C')]=_('Dark yellow')
COLOR_NAMES[clr_h2i('#9BC4E2')]=_('Pale cerulean')
COLOR_NAMES[clr_h2i('#9BCD9B')]=_('Dark sea green')
COLOR_NAMES[clr_h2i('#9C2542')]=_('Big dip o\'ruby')
COLOR_NAMES[clr_h2i('#9C51B6')]=_('Purple Plum')
COLOR_NAMES[clr_h2i('#9C7C38')]=_('Metallic sunburst')
COLOR_NAMES[clr_h2i('#9C9C9C')]=_('Grey')
COLOR_NAMES[clr_h2i('#9D2933')]=_('Japanese carmine')
COLOR_NAMES[clr_h2i('#9DC209')]=_('Limerick')
COLOR_NAMES[clr_h2i('#9E1316')]=_('Spartan crimson')
COLOR_NAMES[clr_h2i('#9E5E6F')]=_('Rose dust')
COLOR_NAMES[clr_h2i('#9EFD38')]=_('French lime')
COLOR_NAMES[clr_h2i('#9F00C5')]=_('Purple')
COLOR_NAMES[clr_h2i('#9F00FF')]=_('Vivid violet')
COLOR_NAMES[clr_h2i('#9F1D35')]=_('Vivid burgundy')
COLOR_NAMES[clr_h2i('#9F2B68')]=_('Amaranth deep purple')
COLOR_NAMES[clr_h2i('#9F4576')]=_('Magenta haze')
COLOR_NAMES[clr_h2i('#9F79EE')]=_('Medium purple')
COLOR_NAMES[clr_h2i('#9F8170')]=_('Beaver')
COLOR_NAMES[clr_h2i('#9FA91F')]=_('Citron')
COLOR_NAMES[clr_h2i('#9FB6CD')]=_('Slate gray')
COLOR_NAMES[clr_h2i('#9FE2BF')]=_('Sea Foam green')
COLOR_NAMES[clr_h2i('#A020F0')]=_('Purple, Veronica')
COLOR_NAMES[clr_h2i('#A0522D')]=_('Sienna')
COLOR_NAMES[clr_h2i('#A0785A')]=_('Chamoisee')
COLOR_NAMES[clr_h2i('#A0D6B4')]=_('Turquoise green')
COLOR_NAMES[clr_h2i('#A0E6FF')]=_('Winter wizard')
COLOR_NAMES[clr_h2i('#A17A74')]=_('Burnished brown')
COLOR_NAMES[clr_h2i('#A1CAF1')]=_('Baby blue eyes')
COLOR_NAMES[clr_h2i('#A2006D')]=_('Flirt')
COLOR_NAMES[clr_h2i('#A2A2D0')]=_('Blue bell')
COLOR_NAMES[clr_h2i('#A2ADD0')]=_('Wild blue yonder')
COLOR_NAMES[clr_h2i('#A2B5CD')]=_('Light steel blue')
COLOR_NAMES[clr_h2i('#A2CD5A')]=_('Dark olive green')
COLOR_NAMES[clr_h2i('#A3C1AD')]=_('Cambridge blue')
COLOR_NAMES[clr_h2i('#A40000')]=_('Dark candy apple red')
COLOR_NAMES[clr_h2i('#A45A52')]=_('Redwood')
COLOR_NAMES[clr_h2i('#A4C639')]=_('Android green')
COLOR_NAMES[clr_h2i('#A4D3EE')]=_('Light sky blue')
COLOR_NAMES[clr_h2i('#A4DDED')]=_('Non-photo blue')
COLOR_NAMES[clr_h2i('#A4F4F9')]=_('Waterspout')
COLOR_NAMES[clr_h2i('#A50B5E')]=_('Jazzberry jam')
COLOR_NAMES[clr_h2i('#A52A2A')]=_('Auburn, brown')
COLOR_NAMES[clr_h2i('#A55353')]=_('Middle red purple')
COLOR_NAMES[clr_h2i('#A57164')]=_('Blast-off bronze')
COLOR_NAMES[clr_h2i('#A63A79')]=_('Maximum red purple')
COLOR_NAMES[clr_h2i('#A67B5B')]=_('French beige, Tuscan tan')
COLOR_NAMES[clr_h2i('#A6A6A6')]=_('Quick silver')
COLOR_NAMES[clr_h2i('#A6D608')]=_('Vivid lime green')
COLOR_NAMES[clr_h2i('#A6E7FF')]=_('Fresh air')
COLOR_NAMES[clr_h2i('#A75502')]=_('Windsor tan')
COLOR_NAMES[clr_h2i('#A76BCF')]=_('Rich lavender')
COLOR_NAMES[clr_h2i('#A7F432')]=_('Green lizard')
COLOR_NAMES[clr_h2i('#A7FC00')]=_('Spring bud')
COLOR_NAMES[clr_h2i('#A81C07')]=_('Rufous')
COLOR_NAMES[clr_h2i('#A83731')]=_('Sweet brown')
COLOR_NAMES[clr_h2i('#A8516E')]=_('China rose')
COLOR_NAMES[clr_h2i('#A8E4A0')]=_('Granny Smith apple')
COLOR_NAMES[clr_h2i('#A9203E')]=_('Deep carmine')
COLOR_NAMES[clr_h2i('#A95C68')]=_('Deep puce')
COLOR_NAMES[clr_h2i('#A99A86')]=_('Grullo')
COLOR_NAMES[clr_h2i('#A9A9A9')]=_('Dark medium gray')
COLOR_NAMES[clr_h2i('#A9BA9D')]=_('Laurel green')
COLOR_NAMES[clr_h2i('#AA00BB')]=_('Heliotrope magenta')
COLOR_NAMES[clr_h2i('#AA381E')]=_('Chinese red')
COLOR_NAMES[clr_h2i('#AA4069')]=_('Medium ruby')
COLOR_NAMES[clr_h2i('#AA98A9')]=_('Heliotrope gray, Rose quartz')
COLOR_NAMES[clr_h2i('#AAF0D1')]=_('Magic mint')
COLOR_NAMES[clr_h2i('#AB274F')]=_('Amaranth purple')
COLOR_NAMES[clr_h2i('#AB4B52')]=_('English red')
COLOR_NAMES[clr_h2i('#AB4E52')]=_('Rose vale')
COLOR_NAMES[clr_h2i('#AB82FF')]=_('Medium purple')
COLOR_NAMES[clr_h2i('#AB92B3')]=_('Glossy grape')
COLOR_NAMES[clr_h2i('#ABCDEF')]=_('Pale cornflower blue')
COLOR_NAMES[clr_h2i('#AC1E44')]=_('French wine')
COLOR_NAMES[clr_h2i('#ACACAC')]=_('Silver chalice')
COLOR_NAMES[clr_h2i('#ACACE6')]=_('Maximum blue purple')
COLOR_NAMES[clr_h2i('#ACBF60')]=_('Middle green yellow')
COLOR_NAMES[clr_h2i('#ACE1AF')]=_('Celadon')
COLOR_NAMES[clr_h2i('#ACE5EE')]=_('Blizzard blue, Blue Lagoon')
COLOR_NAMES[clr_h2i('#AD4379')]=_('Mystic maroon')
COLOR_NAMES[clr_h2i('#AD6F69')]=_('Copper penny')
COLOR_NAMES[clr_h2i('#ADD8E6')]=_('Light blue')
COLOR_NAMES[clr_h2i('#ADDFAD')]=_('Light moss green')
COLOR_NAMES[clr_h2i('#ADFF2F')]=_('Green-yellow')
COLOR_NAMES[clr_h2i('#AE0C00')]=_('Mordant red 19')
COLOR_NAMES[clr_h2i('#AE2029')]=_('Upsdell red')
COLOR_NAMES[clr_h2i('#AE98AA')]=_('Lilac luster')
COLOR_NAMES[clr_h2i('#AEC6CF')]=_('Pastel blue')
COLOR_NAMES[clr_h2i('#AF002A')]=_('Alabama crimson')
COLOR_NAMES[clr_h2i('#AF4035')]=_('Pale carmine')
COLOR_NAMES[clr_h2i('#AF6E4D')]=_('Brown sugar')
COLOR_NAMES[clr_h2i('#AFEEEE')]=_('Pale blue')
COLOR_NAMES[clr_h2i('#B03060')]=_('Rich maroon')
COLOR_NAMES[clr_h2i('#B05C52')]=_('Giant\'s club')
COLOR_NAMES[clr_h2i('#B06500')]=_('Ginger')
COLOR_NAMES[clr_h2i('#B0BF1A')]=_('Acid green')
COLOR_NAMES[clr_h2i('#B0C4DE')]=_('Light steel blue')
COLOR_NAMES[clr_h2i('#B0E0E6')]=_('Powder blue')
COLOR_NAMES[clr_h2i('#B0E2FF')]=_('Light sky blue')
COLOR_NAMES[clr_h2i('#B19CD9')]=_('Light pastel purple')
COLOR_NAMES[clr_h2i('#B22222')]=_('Firebrick')
COLOR_NAMES[clr_h2i('#B23AEE')]=_('Dark orchid')
COLOR_NAMES[clr_h2i('#B284BE')]=_('African violet')
COLOR_NAMES[clr_h2i('#B2BEB5')]=_('Ash grey')
COLOR_NAMES[clr_h2i('#B2DFEE')]=_('Light blue')
COLOR_NAMES[clr_h2i('#B2EC5D')]=_('Inchworm')
COLOR_NAMES[clr_h2i('#B2FFFF')]=_('Celeste, Italian sky blue')
COLOR_NAMES[clr_h2i('#B31B1B')]=_('Carnelian, Cornell red')
COLOR_NAMES[clr_h2i('#B3446C')]=_('Irresistible, Raspberry rose')
COLOR_NAMES[clr_h2i('#B38B6D')]=_('Light taupe')
COLOR_NAMES[clr_h2i('#B39EB5')]=_('Pastel purple')
COLOR_NAMES[clr_h2i('#B3EE3A')]=_('Olive drab')
COLOR_NAMES[clr_h2i('#B452CD')]=_('Medium orchid')
COLOR_NAMES[clr_h2i('#B48395')]=_('English lavender')
COLOR_NAMES[clr_h2i('#B4CDCD')]=_('Light cyan')
COLOR_NAMES[clr_h2i('#B4EEB4')]=_('Dark sea green')
COLOR_NAMES[clr_h2i('#B53389')]=_('Fandango')
COLOR_NAMES[clr_h2i('#B5651D')]=_('Light brown')
COLOR_NAMES[clr_h2i('#B57281')]=_('Turkish rose')
COLOR_NAMES[clr_h2i('#B57EDC')]=_('Lavender (floral)')
COLOR_NAMES[clr_h2i('#B5A642')]=_('Brass')
COLOR_NAMES[clr_h2i('#B5B5B5')]=_('Grey')
COLOR_NAMES[clr_h2i('#B666D2')]=_('Rich lilac')
COLOR_NAMES[clr_h2i('#B7410E')]=_('Rust')
COLOR_NAMES[clr_h2i('#B768A2')]=_('Pearly purple')
COLOR_NAMES[clr_h2i('#B76E79')]=_('Rose gold')
COLOR_NAMES[clr_h2i('#B784A7')]=_('Opera mauve')
COLOR_NAMES[clr_h2i('#B78727')]=_('University of California Gold')
COLOR_NAMES[clr_h2i('#B80CE3')]=_('Vivid mulberry')
COLOR_NAMES[clr_h2i('#B86D29')]=_('Liver (dogs)')
COLOR_NAMES[clr_h2i('#B87333')]=_('Copper')
COLOR_NAMES[clr_h2i('#B8860B')]=_('Dark goldenrod')
COLOR_NAMES[clr_h2i('#B94E48')]=_('Deep chestnut')
COLOR_NAMES[clr_h2i('#B9D3EE')]=_('Slate gray')
COLOR_NAMES[clr_h2i('#B9F2FF')]=_('Diamond')
COLOR_NAMES[clr_h2i('#BA160C')]=_('International orange')
COLOR_NAMES[clr_h2i('#BA55D3')]=_('Medium orchid')
COLOR_NAMES[clr_h2i('#BA8759')]=_('Deer')
COLOR_NAMES[clr_h2i('#BB3385')]=_('Medium red-violet')
COLOR_NAMES[clr_h2i('#BB6528')]=_('Ruddy brown')
COLOR_NAMES[clr_h2i('#BBB477')]=_('Misty moss')
COLOR_NAMES[clr_h2i('#BBFFFF')]=_('Pale turquoise')
COLOR_NAMES[clr_h2i('#BC8F8F')]=_('Rosy brown')
COLOR_NAMES[clr_h2i('#BC987E')]=_('Pale taupe')
COLOR_NAMES[clr_h2i('#BCB88A')]=_('Sage')
COLOR_NAMES[clr_h2i('#BCD2EE')]=_('Light steel blue')
COLOR_NAMES[clr_h2i('#BCD4E6')]=_('Pale aqua')
COLOR_NAMES[clr_h2i('#BCEE68')]=_('Dark olive green')
COLOR_NAMES[clr_h2i('#BD33A4')]=_('Byzantine')
COLOR_NAMES[clr_h2i('#BDB76B')]=_('Dark khaki')
COLOR_NAMES[clr_h2i('#BDDA57')]=_('June bud')
COLOR_NAMES[clr_h2i('#BE0032')]=_('Crimson glory')
COLOR_NAMES[clr_h2i('#BE4F62')]=_('Popstar')
COLOR_NAMES[clr_h2i('#BEBEBE')]=_('Gray')
COLOR_NAMES[clr_h2i('#BF00FF')]=_('Electric purple')
COLOR_NAMES[clr_h2i('#BF3EFF')]=_('Dark orchid')
COLOR_NAMES[clr_h2i('#BF4F51')]=_('Bittersweet shimmer')
COLOR_NAMES[clr_h2i('#BF94E4')]=_('Bright lavender')
COLOR_NAMES[clr_h2i('#BFAFB2')]=_('Black shadows')
COLOR_NAMES[clr_h2i('#BFC1C2')]=_('Silver sand')
COLOR_NAMES[clr_h2i('#BFEFFF')]=_('Light blue')
COLOR_NAMES[clr_h2i('#BFFF00')]=_('Bitter lime')
COLOR_NAMES[clr_h2i('#C0362C')]=_('International orange (Golden Gate Bridge)')
COLOR_NAMES[clr_h2i('#C04000')]=_('Mahogany')
COLOR_NAMES[clr_h2i('#C08081')]=_('Old rose')
COLOR_NAMES[clr_h2i('#C09999')]=_('Tuscany')
COLOR_NAMES[clr_h2i('#C0C0C0')]=_('Silver')
COLOR_NAMES[clr_h2i('#C0FF3E')]=_('Olive drab')
COLOR_NAMES[clr_h2i('#C154C1')]=_('Deep fuchsia')
COLOR_NAMES[clr_h2i('#C19A6B')]=_('Camel, Desert, Wood brown')
COLOR_NAMES[clr_h2i('#C1CDC1')]=_('Honeydew')
COLOR_NAMES[clr_h2i('#C1CDCD')]=_('Azure')
COLOR_NAMES[clr_h2i('#C1FFC1')]=_('Dark sea green')
COLOR_NAMES[clr_h2i('#C21E56')]=_('Rose red')
COLOR_NAMES[clr_h2i('#C23B22')]=_('Dark pastel red')
COLOR_NAMES[clr_h2i('#C2B280')]=_('Sand')
COLOR_NAMES[clr_h2i('#C30B4E')]=_('Pictorial carmine')
COLOR_NAMES[clr_h2i('#C32148')]=_('Bright maroon')
COLOR_NAMES[clr_h2i('#C39953')]=_('Aztec gold')
COLOR_NAMES[clr_h2i('#C3B091')]=_('Khaki')
COLOR_NAMES[clr_h2i('#C40233')]=_('Red')
COLOR_NAMES[clr_h2i('#C41E3A')]=_('Cardinal')
COLOR_NAMES[clr_h2i('#C46210')]=_('Alloy orange')
COLOR_NAMES[clr_h2i('#C4AEAD')]=_('Silver pink')
COLOR_NAMES[clr_h2i('#C4C3D0')]=_('Lavender gray')
COLOR_NAMES[clr_h2i('#C4D8E2')]=_('Columbia blue')
COLOR_NAMES[clr_h2i('#C53151')]=_('Dingy dungeon')
COLOR_NAMES[clr_h2i('#C54B8C')]=_('Mulberry')
COLOR_NAMES[clr_h2i('#C5B358')]=_('Vegas gold')
COLOR_NAMES[clr_h2i('#C6E2FF')]=_('Slate gray')
COLOR_NAMES[clr_h2i('#C71585')]=_('Medium violet-red')
COLOR_NAMES[clr_h2i('#C72C48')]=_('French raspberry')
COLOR_NAMES[clr_h2i('#C74375')]=_('Fuchsia rose')
COLOR_NAMES[clr_h2i('#C80815')]=_('Venetian red')
COLOR_NAMES[clr_h2i('#C84186')]=_('Smitten')
COLOR_NAMES[clr_h2i('#C8A2C8')]=_('Lilac')
COLOR_NAMES[clr_h2i('#C8AD7F')]=_('Light french beige')
COLOR_NAMES[clr_h2i('#C90016')]=_('Harvard crimson')
COLOR_NAMES[clr_h2i('#C95A49')]=_('Cedar Chest')
COLOR_NAMES[clr_h2i('#C9A0DC')]=_('Wisteria')
COLOR_NAMES[clr_h2i('#C9C0BB')]=_('Pale silver')
COLOR_NAMES[clr_h2i('#C9DC87')]=_('Medium spring bud')
COLOR_NAMES[clr_h2i('#C9FFE5')]=_('Aero blue')
COLOR_NAMES[clr_h2i('#CA1F7B')]=_('Magenta (dye)')
COLOR_NAMES[clr_h2i('#CA2C92')]=_('Royal fuchsia')
COLOR_NAMES[clr_h2i('#CAE00D')]=_('Bitter lemon')
COLOR_NAMES[clr_h2i('#CAE1FF')]=_('Light steel blue')
COLOR_NAMES[clr_h2i('#CAFF70')]=_('Dark olive green')
COLOR_NAMES[clr_h2i('#CB410B')]=_('Sinopia')
COLOR_NAMES[clr_h2i('#CB4154')]=_('Brick red')
COLOR_NAMES[clr_h2i('#CB6D51')]=_('Copper red')
COLOR_NAMES[clr_h2i('#CB99C9')]=_('Pastel violet')
COLOR_NAMES[clr_h2i('#CBA135')]=_('Satin sheen gold')
COLOR_NAMES[clr_h2i('#CC0000')]=_('Boston university red')
COLOR_NAMES[clr_h2i('#CC0033')]=_('Vivid crimson')
COLOR_NAMES[clr_h2i('#CC00CC')]=_('Deep magenta')
COLOR_NAMES[clr_h2i('#CC00FF')]=_('Vivid orchid')
COLOR_NAMES[clr_h2i('#CC3333')]=_('Persian red')
COLOR_NAMES[clr_h2i('#CC3336')]=_('Madder lake')
COLOR_NAMES[clr_h2i('#CC338B')]=_('Magenta-pink')
COLOR_NAMES[clr_h2i('#CC33CC')]=_('Steel pink')
COLOR_NAMES[clr_h2i('#CC397B')]=_('Fuchsia purple')
COLOR_NAMES[clr_h2i('#CC474B')]=_('English vermillion')
COLOR_NAMES[clr_h2i('#CC4E5C')]=_('Dark terra cotta')
COLOR_NAMES[clr_h2i('#CC5500')]=_('Burnt orange')
COLOR_NAMES[clr_h2i('#CC6666')]=_('Fuzzy Wuzzy')
COLOR_NAMES[clr_h2i('#CC7722')]=_('Ochre')
COLOR_NAMES[clr_h2i('#CC8899')]=_('Puce')
COLOR_NAMES[clr_h2i('#CC9900')]=_('Vivid amber')
COLOR_NAMES[clr_h2i('#cc9966')]=_('Brown yellow')
COLOR_NAMES[clr_h2i('#CC99CC')]=_('Light grayish magenta')
COLOR_NAMES[clr_h2i('#CC99FF')]=_('Pale violet')
COLOR_NAMES[clr_h2i('#CCA01D')]=_('Lemon curry')
COLOR_NAMES[clr_h2i('#CCCCFF')]=_('Lavender blue, Periwinkle')
COLOR_NAMES[clr_h2i('#CCFF00')]=_('Fluorescent yellow')
COLOR_NAMES[clr_h2i('#CD0000')]=_('Red')
COLOR_NAMES[clr_h2i('#CD00CD')]=_('Magenta')
COLOR_NAMES[clr_h2i('#CD1076')]=_('Deep pink')
COLOR_NAMES[clr_h2i('#CD2626')]=_('Firebrick')
COLOR_NAMES[clr_h2i('#CD2990')]=_('Maroon')
COLOR_NAMES[clr_h2i('#CD3278')]=_('Violet red')
COLOR_NAMES[clr_h2i('#CD3333')]=_('Brown')
COLOR_NAMES[clr_h2i('#CD3700')]=_('Orange red')
COLOR_NAMES[clr_h2i('#CD4F39')]=_('Tomato')
COLOR_NAMES[clr_h2i('#CD5555')]=_('Indian red')
COLOR_NAMES[clr_h2i('#CD5700')]=_('Tenne (tawny)')
COLOR_NAMES[clr_h2i('#CD5B45')]=_('Dark coral')
COLOR_NAMES[clr_h2i('#CD5C5C')]=_('Indian red')
COLOR_NAMES[clr_h2i('#CD607E')]=_('Cinnamon satin')
COLOR_NAMES[clr_h2i('#CD6090')]=_('Hot pink')
COLOR_NAMES[clr_h2i('#CD6600')]=_('Dark orange')
COLOR_NAMES[clr_h2i('#CD661D')]=_('Chocolate')
COLOR_NAMES[clr_h2i('#CD6839')]=_('Sienna')
COLOR_NAMES[clr_h2i('#CD6889')]=_('Pale violet red')
COLOR_NAMES[clr_h2i('#CD69C9')]=_('Orchid')
COLOR_NAMES[clr_h2i('#CD7054')]=_('Salmon')
COLOR_NAMES[clr_h2i('#CD7F32')]=_('Bronze')
COLOR_NAMES[clr_h2i('#CD8162')]=_('Light salmon')
COLOR_NAMES[clr_h2i('#CD8500')]=_('Orange')
COLOR_NAMES[clr_h2i('#CD853F')]=_('Peru')
COLOR_NAMES[clr_h2i('#CD8C95')]=_('Light pink')
COLOR_NAMES[clr_h2i('#CD919E')]=_('Pink')
COLOR_NAMES[clr_h2i('#CD950C')]=_('Dark goldenrod')
COLOR_NAMES[clr_h2i('#CD9575')]=_('Antique brass')
COLOR_NAMES[clr_h2i('#CD96CD')]=_('Plum')
COLOR_NAMES[clr_h2i('#CD9B1D')]=_('Goldenrod')
COLOR_NAMES[clr_h2i('#CD9B9B')]=_('Rosy brown')
COLOR_NAMES[clr_h2i('#CDA4DE')]=_('Tropical violet')
COLOR_NAMES[clr_h2i('#CDAA7D')]=_('Burlywood')
COLOR_NAMES[clr_h2i('#CDAD00')]=_('Gold')
COLOR_NAMES[clr_h2i('#CDAF95')]=_('Peach puff')
COLOR_NAMES[clr_h2i('#CDB38B')]=_('Navajo white')
COLOR_NAMES[clr_h2i('#CDB5CD')]=_('Thistle')
COLOR_NAMES[clr_h2i('#CDB79E')]=_('Bisque')
COLOR_NAMES[clr_h2i('#CDB7B5')]=_('Misty rose')
COLOR_NAMES[clr_h2i('#CDBA96')]=_('Wheat')
COLOR_NAMES[clr_h2i('#CDBE70')]=_('Light goldenrod')
COLOR_NAMES[clr_h2i('#CDC0B0')]=_('Antique white')
COLOR_NAMES[clr_h2i('#CDC1C5')]=_('Lavender blush')
COLOR_NAMES[clr_h2i('#CDC5BF')]=_('Seashell')
COLOR_NAMES[clr_h2i('#CDC673')]=_('Khaki')
COLOR_NAMES[clr_h2i('#CDC8B1')]=_('Cornsilk')
COLOR_NAMES[clr_h2i('#CDC9A5')]=_('Lemon chiffon')
COLOR_NAMES[clr_h2i('#CDC9C9')]=_('Snow')
COLOR_NAMES[clr_h2i('#CDCD00')]=_('Yellow')
COLOR_NAMES[clr_h2i('#CDCDB4')]=_('Light yellow')
COLOR_NAMES[clr_h2i('#CDCDC1')]=_('Ivory')
COLOR_NAMES[clr_h2i('#CE2029')]=_('Fire engine red')
COLOR_NAMES[clr_h2i('#CE4676')]=_('Ruber')
COLOR_NAMES[clr_h2i('#CEC8EF')]=_('Soap')
COLOR_NAMES[clr_h2i('#CEFF00')]=_('Volt')
COLOR_NAMES[clr_h2i('#CF1020')]=_('Lava')
COLOR_NAMES[clr_h2i('#CF3476')]=_('Telemagenta')
COLOR_NAMES[clr_h2i('#CF6BA9')]=_('Super pink')
COLOR_NAMES[clr_h2i('#CF71AF')]=_('Sky magenta')
COLOR_NAMES[clr_h2i('#CFB53B')]=_('Old gold')
COLOR_NAMES[clr_h2i('#CFCFC4')]=_('Pastel gray')
COLOR_NAMES[clr_h2i('#CFCFCF')]=_('Gray')
COLOR_NAMES[clr_h2i('#D02090')]=_('Violet red')
COLOR_NAMES[clr_h2i('#D0417E')]=_('Magenta')
COLOR_NAMES[clr_h2i('#D0F0C0')]=_('Tea green')
COLOR_NAMES[clr_h2i('#D0FF14')]=_('Arctic lime')
COLOR_NAMES[clr_h2i('#D10047')]=_('Spanish carmine')
COLOR_NAMES[clr_h2i('#D10056')]=_('Rubine red')
COLOR_NAMES[clr_h2i('#D15FEE')]=_('Medium orchid')
COLOR_NAMES[clr_h2i('#D19FE8')]=_('Bright ube')
COLOR_NAMES[clr_h2i('#D1BEA8')]=_('Dark vanilla')
COLOR_NAMES[clr_h2i('#D1E231')]=_('Pear')
COLOR_NAMES[clr_h2i('#D1EEEE')]=_('Light cyan')
COLOR_NAMES[clr_h2i('#D2691E')]=_('Chocolate, Cocoa brown')
COLOR_NAMES[clr_h2i('#D2B48C')]=_('Tan')
COLOR_NAMES[clr_h2i('#D3003F')]=_('Utah Crimson')
COLOR_NAMES[clr_h2i('#D3212D')]=_('Amaranth red')
COLOR_NAMES[clr_h2i('#D39BCB')]=_('Light medium orchid')
COLOR_NAMES[clr_h2i('#D3D3D3')]=_('Light gray')
COLOR_NAMES[clr_h2i('#D40000')]=_('Rosso corsa')
COLOR_NAMES[clr_h2i('#D470A2')]=_('Wild orchid')
COLOR_NAMES[clr_h2i('#D473D4')]=_('Deep mauve')
COLOR_NAMES[clr_h2i('#D4AF37')]=_('Gold (metallic)')
COLOR_NAMES[clr_h2i('#D65282')]=_('Mystic')
COLOR_NAMES[clr_h2i('#D68A59')]=_('Raw sienna')
COLOR_NAMES[clr_h2i('#D6CADD')]=_('Languid lavender')
COLOR_NAMES[clr_h2i('#D70040')]=_('Rich carmine')
COLOR_NAMES[clr_h2i('#D70A53')]=_('Debian red')
COLOR_NAMES[clr_h2i('#D71868')]=_('Dogwood rose')
COLOR_NAMES[clr_h2i('#D73B3E')]=_('Jasper')
COLOR_NAMES[clr_h2i('#D74894')]=_('Pink')
COLOR_NAMES[clr_h2i('#D7837F')]=_('New York pink')
COLOR_NAMES[clr_h2i('#D891EF')]=_('Bright lilac')
COLOR_NAMES[clr_h2i('#D8B2D1')]=_('Pink lavender')
COLOR_NAMES[clr_h2i('#D8BFD8')]=_('Thistle')
COLOR_NAMES[clr_h2i('#D9004C')]=_('UA red')
COLOR_NAMES[clr_h2i('#D92121')]=_('Maximum red')
COLOR_NAMES[clr_h2i('#D9381E')]=_('Vermilion')
COLOR_NAMES[clr_h2i('#D9603B')]=_('Medium vermilion')
COLOR_NAMES[clr_h2i('#D982B5')]=_('Middle purple')
COLOR_NAMES[clr_h2i('#D98695')]=_('Shimmering blush')
COLOR_NAMES[clr_h2i('#D99058')]=_('Persian orange')
COLOR_NAMES[clr_h2i('#D998A0')]=_('Parrot pink')
COLOR_NAMES[clr_h2i('#D9E650')]=_('Maximum green yellow')
COLOR_NAMES[clr_h2i('#DA1D81')]=_('Vivid cerise')
COLOR_NAMES[clr_h2i('#DA2C43')]=_('Rusty red')
COLOR_NAMES[clr_h2i('#DA3287')]=_('Deep cerise')
COLOR_NAMES[clr_h2i('#DA614E')]=_('Jelly bean')
COLOR_NAMES[clr_h2i('#DA70D6')]=_('Orchid')
COLOR_NAMES[clr_h2i('#DA8A67')]=_('Pale copper')
COLOR_NAMES[clr_h2i('#DA9100')]=_('Harvest gold')
COLOR_NAMES[clr_h2i('#DAA520')]=_('Goldenrod')
COLOR_NAMES[clr_h2i('#DB7093')]=_('Pale red-violet')
COLOR_NAMES[clr_h2i('#DBD7D2')]=_('Timberwolf')
COLOR_NAMES[clr_h2i('#DBE9F4')]=_('Azureish white')
COLOR_NAMES[clr_h2i('#DC143C')]=_('Crimson')
COLOR_NAMES[clr_h2i('#DCD0FF')]=_('Pale lavender')
COLOR_NAMES[clr_h2i('#DCDCDC')]=_('Gainsboro')
COLOR_NAMES[clr_h2i('#DDA0DD')]=_('Medium lavender magenta, Pale plum')
COLOR_NAMES[clr_h2i('#DDADAF')]=_('Pale chestnut')
COLOR_NAMES[clr_h2i('#DDE26A')]=_('Booger buster')
COLOR_NAMES[clr_h2i('#DE3163')]=_('Cherry')
COLOR_NAMES[clr_h2i('#DE5285')]=_('Fandango pink')
COLOR_NAMES[clr_h2i('#DE5D83')]=_('Blush')
COLOR_NAMES[clr_h2i('#DE6FA1')]=_('China pink, Liseran purple')
COLOR_NAMES[clr_h2i('#DEA5A4')]=_('Pastel pink')
COLOR_NAMES[clr_h2i('#DEAA88')]=_('Tumbleweed')
COLOR_NAMES[clr_h2i('#DEB887')]=_('Burlywood')
COLOR_NAMES[clr_h2i('#DF00FF')]=_('Phlox, Psychedelic purple')
COLOR_NAMES[clr_h2i('#DF6124')]=_('Vivid red-tangelo')
COLOR_NAMES[clr_h2i('#DF73FF')]=_('Heliotrope')
COLOR_NAMES[clr_h2i('#DFFF00')]=_('Chartreuse (traditional)')
COLOR_NAMES[clr_h2i('#E0115F')]=_('Ruby')
COLOR_NAMES[clr_h2i('#E0218A')]=_('Barbie pink')
COLOR_NAMES[clr_h2i('#E03C31')]=_('CG red')
COLOR_NAMES[clr_h2i('#E066FF')]=_('Medium orchid')
COLOR_NAMES[clr_h2i('#E08D3C')]=_('Tiger\'s eye')
COLOR_NAMES[clr_h2i('#E0B0FF')]=_('Mauve')
COLOR_NAMES[clr_h2i('#E0EEE0')]=_('Honeydew')
COLOR_NAMES[clr_h2i('#E0EEEE')]=_('Azure')
COLOR_NAMES[clr_h2i('#E0FFFF')]=_('Light cyan')
COLOR_NAMES[clr_h2i('#E12C2C')]=_('Permanent geranium lake')
COLOR_NAMES[clr_h2i('#E18E96')]=_('Ruddy pink')
COLOR_NAMES[clr_h2i('#E1A95F')]=_('Earth yellow')
COLOR_NAMES[clr_h2i('#E1AD21')]=_('Urobilin')
COLOR_NAMES[clr_h2i('#E2062C')]=_('Medium candy apple red')
COLOR_NAMES[clr_h2i('#E25098')]=_('Raspberry pink')
COLOR_NAMES[clr_h2i('#E25822')]=_('Flame')
COLOR_NAMES[clr_h2i('#E2725B')]=_('Terra cotta')
COLOR_NAMES[clr_h2i('#E30022')]=_('Cadmium red')
COLOR_NAMES[clr_h2i('#E30B5D')]=_('Raspberry')
COLOR_NAMES[clr_h2i('#E3256B')]=_('Razzmatazz')
COLOR_NAMES[clr_h2i('#E32636')]=_('Alizarin crimson, Rose madder')
COLOR_NAMES[clr_h2i('#E34234')]=_('Cinnabar, Vermilion')
COLOR_NAMES[clr_h2i('#E3A857')]=_('Indian yellow')
COLOR_NAMES[clr_h2i('#E3AB57')]=_('Sunray')
COLOR_NAMES[clr_h2i('#E3DAC9')]=_('Bone')
COLOR_NAMES[clr_h2i('#E3F988')]=_('Mindaro')
COLOR_NAMES[clr_h2i('#E3FF00')]=_('Lemon lime')
COLOR_NAMES[clr_h2i('#E40078')]=_('Red-purple')
COLOR_NAMES[clr_h2i('#E4007C')]=_('Mexican pink')
COLOR_NAMES[clr_h2i('#E4717A')]=_('Tango pink')
COLOR_NAMES[clr_h2i('#E48400')]=_('Fulvous')
COLOR_NAMES[clr_h2i('#E49B0F')]=_('Gamboge')
COLOR_NAMES[clr_h2i('#E4D00A')]=_('Citrine')
COLOR_NAMES[clr_h2i('#E4D96F')]=_('Straw')
COLOR_NAMES[clr_h2i('#E51A4C')]=_('Spanish crimson')
COLOR_NAMES[clr_h2i('#E52B50')]=_('Amaranth')
COLOR_NAMES[clr_h2i('#E56024')]=_('Vivid vermilion')
COLOR_NAMES[clr_h2i('#E58E73')]=_('Middle red')
COLOR_NAMES[clr_h2i('#E5AA70')]=_('Fawn')
COLOR_NAMES[clr_h2i('#E5B73B')]=_('Meat brown')
COLOR_NAMES[clr_h2i('#E5CCC9')]=_('Dust storm')
COLOR_NAMES[clr_h2i('#E5E4E2')]=_('Platinum')
COLOR_NAMES[clr_h2i('#E60026')]=_('Spanish red')
COLOR_NAMES[clr_h2i('#E62020')]=_('Lust')
COLOR_NAMES[clr_h2i('#E63E62')]=_('Paradise pink')
COLOR_NAMES[clr_h2i('#E66771')]=_('Light carmine pink')
COLOR_NAMES[clr_h2i('#E68FAC')]=_('Light Thulian pink')
COLOR_NAMES[clr_h2i('#E6A8D7')]=_('Light orchid')
COLOR_NAMES[clr_h2i('#E6BE8A')]=_('Pale gold')
COLOR_NAMES[clr_h2i('#E6E200')]=_('Peridot')
COLOR_NAMES[clr_h2i('#E6E6FA')]=_('Lavender mist')
COLOR_NAMES[clr_h2i('#E6E8FA')]=_('Glitter')
COLOR_NAMES[clr_h2i('#E75480')]=_('Dark pink')
COLOR_NAMES[clr_h2i('#E79FC4')]=_('Kobi')
COLOR_NAMES[clr_h2i('#E7ACCF')]=_('Pink pearl')
COLOR_NAMES[clr_h2i('#E7FEFF')]=_('Bubbles')
COLOR_NAMES[clr_h2i('#E8000D')]=_('KU crimson')
COLOR_NAMES[clr_h2i('#E86100')]=_('Spanish orange')
COLOR_NAMES[clr_h2i('#E88E5A')]=_('Big foot feet')
COLOR_NAMES[clr_h2i('#E8CCD7')]=_('Queen pink')
COLOR_NAMES[clr_h2i('#E8E8E8')]=_('Grey')
COLOR_NAMES[clr_h2i('#E8F48C')]=_('Key Lime')
COLOR_NAMES[clr_h2i('#E936A7')]=_('Frostbite')
COLOR_NAMES[clr_h2i('#E9692C')]=_('Deep carrot orange')
COLOR_NAMES[clr_h2i('#E97451')]=_('Burnt sienna, Light red ochre')
COLOR_NAMES[clr_h2i('#E9967A')]=_('Dark salmon')
COLOR_NAMES[clr_h2i('#E9D66B')]=_('Arylide yellow')
COLOR_NAMES[clr_h2i('#E9FFDB')]=_('Nyanza')
COLOR_NAMES[clr_h2i('#EA3C53')]=_('Desire')
COLOR_NAMES[clr_h2i('#EAA221')]=_('Marigold')
COLOR_NAMES[clr_h2i('#EAE0C8')]=_('Pearl')
COLOR_NAMES[clr_h2i('#EB4C42')]=_('Carmine pink')
COLOR_NAMES[clr_h2i('#EC3B83')]=_('Cerise pink')
COLOR_NAMES[clr_h2i('#EC5800')]=_('Persimmon')
COLOR_NAMES[clr_h2i('#ECB176')]=_('Middle yellow red')
COLOR_NAMES[clr_h2i('#ECD540')]=_('Sandstorm')
COLOR_NAMES[clr_h2i('#ECEBBD')]=_('Pale spring bud')
COLOR_NAMES[clr_h2i('#ED1C24')]=_('Red')
COLOR_NAMES[clr_h2i('#ED2939')]=_('Imperial red')
COLOR_NAMES[clr_h2i('#ED872D')]=_('Cadmium orange')
COLOR_NAMES[clr_h2i('#ED9121')]=_('Carrot orange')
COLOR_NAMES[clr_h2i('#EDC9AF')]=_('Desert sand')
COLOR_NAMES[clr_h2i('#EE0000')]=_('Red')
COLOR_NAMES[clr_h2i('#EE00EE')]=_('Magenta')
COLOR_NAMES[clr_h2i('#EE1289')]=_('Deep pink')
COLOR_NAMES[clr_h2i('#EE204D')]=_('Red')
COLOR_NAMES[clr_h2i('#EE2C2C')]=_('Firebrick')
COLOR_NAMES[clr_h2i('#EE30A7')]=_('Maroon')
COLOR_NAMES[clr_h2i('#EE3A8C')]=_('Violet red')
COLOR_NAMES[clr_h2i('#EE3B3B')]=_('Brown')
COLOR_NAMES[clr_h2i('#EE4000')]=_('Orange red')
COLOR_NAMES[clr_h2i('#EE5C42')]=_('Tomato')
COLOR_NAMES[clr_h2i('#EE6363')]=_('Indian red')
COLOR_NAMES[clr_h2i('#EE6A50')]=_('Coral')
COLOR_NAMES[clr_h2i('#EE6AA7')]=_('Hot pink')
COLOR_NAMES[clr_h2i('#EE7600')]=_('Dark orange')
COLOR_NAMES[clr_h2i('#EE7621')]=_('Chocolate')
COLOR_NAMES[clr_h2i('#EE7942')]=_('Sienna')
COLOR_NAMES[clr_h2i('#EE799F')]=_('Pale violet red')
COLOR_NAMES[clr_h2i('#EE7AE9')]=_('Orchid')
COLOR_NAMES[clr_h2i('#EE8262')]=_('Salmon')
COLOR_NAMES[clr_h2i('#EE82EE')]=_('Lavender magenta, Violet')
COLOR_NAMES[clr_h2i('#EE9572')]=_('Light salmon')
COLOR_NAMES[clr_h2i('#EE9A00')]=_('Orange')
COLOR_NAMES[clr_h2i('#EE9A49')]=_('Tan')
COLOR_NAMES[clr_h2i('#EEA2AD')]=_('Light pink')
COLOR_NAMES[clr_h2i('#EEA9B8')]=_('Pink')
COLOR_NAMES[clr_h2i('#EEAD0E')]=_('Dark goldenrod')
COLOR_NAMES[clr_h2i('#EEAEEE')]=_('Plum')
COLOR_NAMES[clr_h2i('#EEB422')]=_('Goldenrod')
COLOR_NAMES[clr_h2i('#EEB4B4')]=_('Rosy brown')
COLOR_NAMES[clr_h2i('#EEC591')]=_('Burlywood')
COLOR_NAMES[clr_h2i('#EEC900')]=_('Gold')
COLOR_NAMES[clr_h2i('#EECBAD')]=_('Peach puff')
COLOR_NAMES[clr_h2i('#EECFA1')]=_('Navajo white')
COLOR_NAMES[clr_h2i('#EED202')]=_('Safety yellow')
COLOR_NAMES[clr_h2i('#EED2EE')]=_('Thistle')
COLOR_NAMES[clr_h2i('#EED5B7')]=_('Bisque')
COLOR_NAMES[clr_h2i('#EED5D2')]=_('Misty rose')
COLOR_NAMES[clr_h2i('#EED8AE')]=_('Wheat')
COLOR_NAMES[clr_h2i('#EEDC82')]=_('Flax, Light goldenrod')
COLOR_NAMES[clr_h2i('#EEDFCC')]=_('Antique white')
COLOR_NAMES[clr_h2i('#EEE0E5')]=_('Lavender blush')
COLOR_NAMES[clr_h2i('#EEE5DE')]=_('Seashell')
COLOR_NAMES[clr_h2i('#EEE600')]=_('Titanium yellow')
COLOR_NAMES[clr_h2i('#EEE685')]=_('Khaki')
COLOR_NAMES[clr_h2i('#EEE8AA')]=_('Pale goldenrod')
COLOR_NAMES[clr_h2i('#EEE8CD')]=_('Cornsilk')
COLOR_NAMES[clr_h2i('#EEE9BF')]=_('Lemon chiffon')
COLOR_NAMES[clr_h2i('#EEE9E9')]=_('Snow')
COLOR_NAMES[clr_h2i('#EEEE00')]=_('Yellow')
COLOR_NAMES[clr_h2i('#EEEED1')]=_('Light yellow')
COLOR_NAMES[clr_h2i('#EEEEE0')]=_('vory')
COLOR_NAMES[clr_h2i('#EF3038')]=_('Deep carmine pink')
COLOR_NAMES[clr_h2i('#EF98AA')]=_('Mauvelous')
COLOR_NAMES[clr_h2i('#EFBBCC')]=_('Cameo pink')
COLOR_NAMES[clr_h2i('#EFCC00')]=_('Yellow')
COLOR_NAMES[clr_h2i('#EFDECD')]=_('Almond')
COLOR_NAMES[clr_h2i('#EFDFBB')]=_('Dutch white')
COLOR_NAMES[clr_h2i('#F07427')]=_('Vivid tangelo')
COLOR_NAMES[clr_h2i('#F08080')]=_('Light coral')
COLOR_NAMES[clr_h2i('#F0DC82')]=_('Buff')
COLOR_NAMES[clr_h2i('#F0E130')]=_('Dandelion')
COLOR_NAMES[clr_h2i('#F0E68C')]=_('Light khaki')
COLOR_NAMES[clr_h2i('#F0EAD6')]=_('Eggshell')
COLOR_NAMES[clr_h2i('#F0F8FF')]=_('Alice blue')
COLOR_NAMES[clr_h2i('#F0FFF0')]=_('Honeydew')
COLOR_NAMES[clr_h2i('#F0FFFF')]=_('Azure mist')
COLOR_NAMES[clr_h2i('#F19CBB')]=_('Amaranth pink')
COLOR_NAMES[clr_h2i('#F1A7FE')]=_('Rich brilliant lavender')
COLOR_NAMES[clr_h2i('#F1DDCF')]=_('Champagne pink')
COLOR_NAMES[clr_h2i('#F2003C')]=_('Red')
COLOR_NAMES[clr_h2i('#F28500')]=_('Tangerine')
COLOR_NAMES[clr_h2i('#F2BA49')]=_('Maximum yellow red')
COLOR_NAMES[clr_h2i('#F2BDCD')]=_('Orchid pink')
COLOR_NAMES[clr_h2i('#F2F0E6')]=_('Alabaster')
COLOR_NAMES[clr_h2i('#F2F27A')]=_('Sunny')
COLOR_NAMES[clr_h2i('#F2F3F4')]=_('Anti-flash white')
COLOR_NAMES[clr_h2i('#F37A48')]=_('Mandarin')
COLOR_NAMES[clr_h2i('#F38FA9')]=_('Vanilla ice')
COLOR_NAMES[clr_h2i('#F3E5AB')]=_('Medium champagne, Vanilla')
COLOR_NAMES[clr_h2i('#F400A1')]=_('Fashion fuchsia, Hollywood cerise')
COLOR_NAMES[clr_h2i('#F49AC2')]=_('Pastel magenta')
COLOR_NAMES[clr_h2i('#F4A460')]=_('Sandy brown')
COLOR_NAMES[clr_h2i('#F4BBFF')]=_('Brilliant lavender')
COLOR_NAMES[clr_h2i('#F4C2C2')]=_('Baby pink, Tea rose')
COLOR_NAMES[clr_h2i('#F4C430')]=_('Saffron')
COLOR_NAMES[clr_h2i('#F4CA16')]=_('Jonquil')
COLOR_NAMES[clr_h2i('#F4F0EC')]=_('Isabelline')
COLOR_NAMES[clr_h2i('#F56991')]=_('Light crimson')
COLOR_NAMES[clr_h2i('#F56FA1')]=_('Cyclamen')
COLOR_NAMES[clr_h2i('#F58025')]=_('Princeton orange')
COLOR_NAMES[clr_h2i('#F5C71A')]=_('Deep lemon')
COLOR_NAMES[clr_h2i('#F5DEB3')]=_('Wheat')
COLOR_NAMES[clr_h2i('#F5E050')]=_('Minion yellow')
COLOR_NAMES[clr_h2i('#F5F5DC')]=_('Beige')
COLOR_NAMES[clr_h2i('#F5F5F5')]=_('White smoke')
COLOR_NAMES[clr_h2i('#F5FFFA')]=_('Mint cream')
COLOR_NAMES[clr_h2i('#F64A8A')]=_('French rose')
COLOR_NAMES[clr_h2i('#F6ADC6')]=_('Nadeshiko pink')
COLOR_NAMES[clr_h2i('#F6EABE')]=_('Lemon meringue')
COLOR_NAMES[clr_h2i('#F70D1A')]=_('Vivid red')
COLOR_NAMES[clr_h2i('#F75394')]=_('Violet-red')
COLOR_NAMES[clr_h2i('#F77F00')]=_('University of Tennessee orange')
COLOR_NAMES[clr_h2i('#F77FBE')]=_('Persian pink')
COLOR_NAMES[clr_h2i('#F78FA7')]=_('Pink sherbet')
COLOR_NAMES[clr_h2i('#F7BFBE')]=_('Spanish pink')
COLOR_NAMES[clr_h2i('#F7E7CE')]=_('Champagne')
COLOR_NAMES[clr_h2i('#F7E98E')]=_('Flavescent')
COLOR_NAMES[clr_h2i('#F88379')]=_('Coral pink, Tea rose')
COLOR_NAMES[clr_h2i('#F8B878')]=_('Mellow apricot')
COLOR_NAMES[clr_h2i('#F8D568')]=_('Orange-yellow')
COLOR_NAMES[clr_h2i('#F8DE7E')]=_('Jasmine, Mellow yellow')
COLOR_NAMES[clr_h2i('#F8F4FF')]=_('Magnolia')
COLOR_NAMES[clr_h2i('#F8F8FF')]=_('Ghost white')
COLOR_NAMES[clr_h2i('#F9429E')]=_('Rose bonbon')
COLOR_NAMES[clr_h2i('#F94D00')]=_('Tangelo')
COLOR_NAMES[clr_h2i('#F984E5')]=_('Pale magenta')
COLOR_NAMES[clr_h2i('#F984EF')]=_('Light fuchsia pink')
COLOR_NAMES[clr_h2i('#FA5B3D')]=_('Orange soda')
COLOR_NAMES[clr_h2i('#FA6E79')]=_('Begonia')
COLOR_NAMES[clr_h2i('#FA8072')]=_('Salmon')
COLOR_NAMES[clr_h2i('#FAD6A5')]=_('Deep champagne, Sunset, Tuscan')
COLOR_NAMES[clr_h2i('#FADA5E')]=_('Royal yellow')
COLOR_NAMES[clr_h2i('#FADADD')]=_('Pale pink')
COLOR_NAMES[clr_h2i('#FADFAD')]=_('Peach-yellow')
COLOR_NAMES[clr_h2i('#FAE7B5')]=_('Banana mania')
COLOR_NAMES[clr_h2i('#FAEBD7')]=_('Antique white, Moccasin')
COLOR_NAMES[clr_h2i('#FAF0BE')]=_('Blond')
COLOR_NAMES[clr_h2i('#FAF0E6')]=_('Linen')
COLOR_NAMES[clr_h2i('#FAFA37')]=_('Maximum yellow')
COLOR_NAMES[clr_h2i('#FAFAD2')]=_('Light goldenrod yellow')
COLOR_NAMES[clr_h2i('#FB4D46')]=_('Tart orange')
COLOR_NAMES[clr_h2i('#FB4F14')]=_('Orioles orange')
COLOR_NAMES[clr_h2i('#FB607F')]=_('Brink pink')
COLOR_NAMES[clr_h2i('#FB9902')]=_('Orange')
COLOR_NAMES[clr_h2i('#FBA0E3')]=_('Lavender rose')
COLOR_NAMES[clr_h2i('#FBAB60')]=_('Rajah')
COLOR_NAMES[clr_h2i('#FBAED2')]=_('Lavender pink')
COLOR_NAMES[clr_h2i('#FBCCE7')]=_('Classic rose')
COLOR_NAMES[clr_h2i('#FBCEB1')]=_('Apricot')
COLOR_NAMES[clr_h2i('#FBEC5D')]=_('Corn')
COLOR_NAMES[clr_h2i('#FC0FC0')]=_('Shocking pink')
COLOR_NAMES[clr_h2i('#FC5A8D')]=_('Strawberry')
COLOR_NAMES[clr_h2i('#FC6C85')]=_('Ultra red, Wild watermelon')
COLOR_NAMES[clr_h2i('#FC74FD')]=_('Pink Flamingo')
COLOR_NAMES[clr_h2i('#FC89AC')]=_('Tickle me pink')
COLOR_NAMES[clr_h2i('#FC8EAC')]=_('Flamingo pink')
COLOR_NAMES[clr_h2i('#FCC200')]=_('Golden poppy')
COLOR_NAMES[clr_h2i('#FCE883')]=_('Yellow')
COLOR_NAMES[clr_h2i('#FCF75E')]=_('Icterine')
COLOR_NAMES[clr_h2i('#FD0E35')]=_('Scarlet, Tractor red')
COLOR_NAMES[clr_h2i('#FD3A4A')]=_('Red Salsa')
COLOR_NAMES[clr_h2i('#FD3F92')]=_('French fuchsia')
COLOR_NAMES[clr_h2i('#FD5240')]=_('Ogre odor')
COLOR_NAMES[clr_h2i('#FD5800')]=_('Willpower orange')
COLOR_NAMES[clr_h2i('#FD5E53')]=_('Sunset orange')
COLOR_NAMES[clr_h2i('#FD6C9E')]=_('French pink')
COLOR_NAMES[clr_h2i('#FD7C6E')]=_('Coral reef')
COLOR_NAMES[clr_h2i('#FDBCB4')]=_('Melon')
COLOR_NAMES[clr_h2i('#FDD5B1')]=_('Feldspar, Light apricot')
COLOR_NAMES[clr_h2i('#FDD9B5')]=_('Sandy tan')
COLOR_NAMES[clr_h2i('#FDDDE6')]=_('Piggy pink')
COLOR_NAMES[clr_h2i('#FDEE00')]=_('Aureolin')
COLOR_NAMES[clr_h2i('#FDF5E6')]=_('Old lace')
COLOR_NAMES[clr_h2i('#FDFD96')]=_('Pastel yellow')
COLOR_NAMES[clr_h2i('#FDFF00')]=_('Lemon glacier')
COLOR_NAMES[clr_h2i('#FDFFF5')]=_('Milk')
COLOR_NAMES[clr_h2i('#FE2712')]=_('Red')
COLOR_NAMES[clr_h2i('#FE28A2')]=_('Persian rose')
COLOR_NAMES[clr_h2i('#FE4164')]=_('Neon fuchsia')
COLOR_NAMES[clr_h2i('#FE4EDA')]=_('Purple pizzazz')
COLOR_NAMES[clr_h2i('#FE5A1D')]=_('Giants orange')
COLOR_NAMES[clr_h2i('#FE6F5E')]=_('Bittersweet')
COLOR_NAMES[clr_h2i('#FEDF00')]=_('Yellow')
COLOR_NAMES[clr_h2i('#FEFE33')]=_('Yellow')
COLOR_NAMES[clr_h2i('#FEFEFA')]=_('Baby powder')
COLOR_NAMES[clr_h2i('#FF0000')]=_('Red')
COLOR_NAMES[clr_h2i('#FF0028')]=_('Ruddy')
COLOR_NAMES[clr_h2i('#FF0038')]=_('Carmine red')
COLOR_NAMES[clr_h2i('#FF003F')]=_('Electric crimson')
COLOR_NAMES[clr_h2i('#FF004F')]=_('Folly')
COLOR_NAMES[clr_h2i('#FF006C')]=_('Vivid raspberry')
COLOR_NAMES[clr_h2i('#FF007C')]=_('Winter sky')
COLOR_NAMES[clr_h2i('#FF007F')]=_('Bright pink, Rose')
COLOR_NAMES[clr_h2i('#FF0090')]=_('Magenta')
COLOR_NAMES[clr_h2i('#FF00FF')]=_('Fuchsia, Magenta')
COLOR_NAMES[clr_h2i('#FF033E')]=_('American rose')
COLOR_NAMES[clr_h2i('#FF0800')]=_('Candy apple red')
COLOR_NAMES[clr_h2i('#FF1493')]=_('Deep pink')
COLOR_NAMES[clr_h2i('#FF1DCE')]=_('Hot magenta')
COLOR_NAMES[clr_h2i('#FF2052')]=_('Awesome')
COLOR_NAMES[clr_h2i('#FF2400')]=_('Scarlet')
COLOR_NAMES[clr_h2i('#FF2800')]=_('Ferrari red')
COLOR_NAMES[clr_h2i('#FF3030')]=_('Firebrick')
COLOR_NAMES[clr_h2i('#FF33CC')]=_('Razzle dazzle rose')
COLOR_NAMES[clr_h2i('#FF34B3')]=_('Maroon')
COLOR_NAMES[clr_h2i('#FF355E')]=_('Radical red')
COLOR_NAMES[clr_h2i('#FF3800')]=_('Coquelicot')
COLOR_NAMES[clr_h2i('#FF3855')]=_('Sizzling red')
COLOR_NAMES[clr_h2i('#FF3E96')]=_('Violet red')
COLOR_NAMES[clr_h2i('#FF4040')]=_('Brown, Coral red')
COLOR_NAMES[clr_h2i('#FF404C')]=_('Sunburnt cyclops')
COLOR_NAMES[clr_h2i('#FF43A4')]=_('Wild strawberry')
COLOR_NAMES[clr_h2i('#FF4466')]=_('Magic potion')
COLOR_NAMES[clr_h2i('#FF4500')]=_('Orange-red')
COLOR_NAMES[clr_h2i('#FF4681')]=_('Sasquatch socks')
COLOR_NAMES[clr_h2i('#FF496C')]=_('Infra red')
COLOR_NAMES[clr_h2i('#FF4F00')]=_('International orange (aerospace)')
COLOR_NAMES[clr_h2i('#FF5349')]=_('Red-orange')
COLOR_NAMES[clr_h2i('#FF5470')]=_('Fiery Rose')
COLOR_NAMES[clr_h2i('#FF55A3')]=_('Brilliant rose')
COLOR_NAMES[clr_h2i('#FF5800')]=_('Orange')
COLOR_NAMES[clr_h2i('#FF5A36')]=_('Portland orange')
COLOR_NAMES[clr_h2i('#FF5CCD')]=_('Light deep pink')
COLOR_NAMES[clr_h2i('#FF5F00')]=_('Vivid orange')
COLOR_NAMES[clr_h2i('#FF6347')]=_('Tomato')
COLOR_NAMES[clr_h2i('#FF66CC')]=_('Rose pink')
COLOR_NAMES[clr_h2i('#FF6700')]=_('Safety orange')
COLOR_NAMES[clr_h2i('#FF6961')]=_('Pastel red')
COLOR_NAMES[clr_h2i('#FF69B4')]=_('Hot pink')
COLOR_NAMES[clr_h2i('#FF6A6A')]=_('Indian red')
COLOR_NAMES[clr_h2i('#FF6D3A')]=_('Smashed pumpkin')
COLOR_NAMES[clr_h2i('#FF6E4A')]=_('Outrageous orange')
COLOR_NAMES[clr_h2i('#FF6EB4')]=_('Hot pink')
COLOR_NAMES[clr_h2i('#FF6FFF')]=_('Ultra pink')
COLOR_NAMES[clr_h2i('#FF7256')]=_('Coral')
COLOR_NAMES[clr_h2i('#FF7518')]=_('Pumpkin')
COLOR_NAMES[clr_h2i('#FF77FF')]=_('Fuchsia pink')
COLOR_NAMES[clr_h2i('#FF7800')]=_('Safety orange')
COLOR_NAMES[clr_h2i('#FF7A00')]=_('Heat wave')
COLOR_NAMES[clr_h2i('#FF7E00')]=_('Amber')
COLOR_NAMES[clr_h2i('#FF7F00')]=_('Dark orange')
COLOR_NAMES[clr_h2i('#FF7F24')]=_('Chocolate')
COLOR_NAMES[clr_h2i('#FF7F50')]=_('Coral')
COLOR_NAMES[clr_h2i('#FF8243')]=_('Mango tango')
COLOR_NAMES[clr_h2i('#FF8247')]=_('Sienna')
COLOR_NAMES[clr_h2i('#FF82AB')]=_('Pale violet red')
COLOR_NAMES[clr_h2i('#FF83FA')]=_('Orchid')
COLOR_NAMES[clr_h2i('#FF85CF')]=_('Princess perfume')
COLOR_NAMES[clr_h2i('#FF878D')]=_('Tulip')
COLOR_NAMES[clr_h2i('#FF8C00')]=_('Dark orange')
COLOR_NAMES[clr_h2i('#FF8C69')]=_('Salmon')
COLOR_NAMES[clr_h2i('#FF91A4')]=_('Salmon pink')
COLOR_NAMES[clr_h2i('#FF91AF')]=_('Baker-Miller pink, Schauss pink')
COLOR_NAMES[clr_h2i('#FF9900')]=_('Vivid gamboge')
COLOR_NAMES[clr_h2i('#FF9933')]=_('Deep saffron')
COLOR_NAMES[clr_h2i('#FF9966')]=_('Atomic tangerine')
COLOR_NAMES[clr_h2i('#FF9999')]=_('Light salmon pink')
COLOR_NAMES[clr_h2i('#FF99CC')]=_('Pale magenta-pink')
COLOR_NAMES[clr_h2i('#FF9F00')]=_('Orange peel')
COLOR_NAMES[clr_h2i('#FFA000')]=_('Vivid orange peel')
COLOR_NAMES[clr_h2i('#FFA07A')]=_('Light salmon')
COLOR_NAMES[clr_h2i('#FFA089')]=_('Vivid tangerine')
COLOR_NAMES[clr_h2i('#FFA343')]=_('Neon Carrot')
COLOR_NAMES[clr_h2i('#FFA500')]=_('Orange')
COLOR_NAMES[clr_h2i('#FFA54F')]=_('Tan')
COLOR_NAMES[clr_h2i('#FFA6C9')]=_('Carnation pink')
COLOR_NAMES[clr_h2i('#FFA700')]=_('Chrome yellow')
COLOR_NAMES[clr_h2i('#FFA812')]=_('Dark tangerine')
COLOR_NAMES[clr_h2i('#FFAA1D')]=_('Bright yellow')
COLOR_NAMES[clr_h2i('#FFAE42')]=_('Yellow orange')
COLOR_NAMES[clr_h2i('#FFAEB9')]=_('Light pink')
COLOR_NAMES[clr_h2i('#FFB077')]=_('Very light tangelo')
COLOR_NAMES[clr_h2i('#FFB300')]=_('UCLA Gold')
COLOR_NAMES[clr_h2i('#FFB347')]=_('Pastel orange')
COLOR_NAMES[clr_h2i('#FFB3DE')]=_('Light hot pink')
COLOR_NAMES[clr_h2i('#FFB5C5')]=_('Pink')
COLOR_NAMES[clr_h2i('#FFB6C1')]=_('Light pink')
COLOR_NAMES[clr_h2i('#FFB7C5')]=_('Cherry blossom pink')
COLOR_NAMES[clr_h2i('#FFB90F')]=_('Dark goldenrod')
COLOR_NAMES[clr_h2i('#FFBA00')]=_('Selective yellow')
COLOR_NAMES[clr_h2i('#FFBBFF')]=_('Plum')
COLOR_NAMES[clr_h2i('#FFBCD9')]=_('Cotton candy')
COLOR_NAMES[clr_h2i('#FFBD88')]=_('Macaroni and cheese')
COLOR_NAMES[clr_h2i('#FFBF00')]=_('Amber, Fluorescent orange')
COLOR_NAMES[clr_h2i('#FFC0CB')]=_('Pink')
COLOR_NAMES[clr_h2i('#FFC125')]=_('Goldenrod')
COLOR_NAMES[clr_h2i('#FFC1C1')]=_('Rosy brown')
COLOR_NAMES[clr_h2i('#FFC1CC')]=_('Bubble gum')
COLOR_NAMES[clr_h2i('#FFC40C')]=_('Mikado yellow')
COLOR_NAMES[clr_h2i('#FFC87C')]=_('Topaz')
COLOR_NAMES[clr_h2i('#FFCBA4')]=_('Deep peach')
COLOR_NAMES[clr_h2i('#FFCC00')]=_('Tangerine yellow')
COLOR_NAMES[clr_h2i('#FFCC33')]=_('Sunglow')
COLOR_NAMES[clr_h2i('#FFCC99')]=_('Peach-orange')
COLOR_NAMES[clr_h2i('#FFCFF1')]=_('Shampoo')
COLOR_NAMES[clr_h2i('#FFD300')]=_('Cyber yellow')
COLOR_NAMES[clr_h2i('#FFD39B')]=_('Burlywood')
COLOR_NAMES[clr_h2i('#FFD700')]=_('Gold')
COLOR_NAMES[clr_h2i('#FFD800')]=_('School bus yellow')
COLOR_NAMES[clr_h2i('#FFDAB9')]=_('Peach puff')
COLOR_NAMES[clr_h2i('#FFDAE9')]=_('Mimi pink')
COLOR_NAMES[clr_h2i('#FFDB00')]=_('Sizzling sunrise')
COLOR_NAMES[clr_h2i('#FFDB58')]=_('Mustard')
COLOR_NAMES[clr_h2i('#FFDDCA')]=_('Unbleached silk')
COLOR_NAMES[clr_h2i('#FFDDF4')]=_('Pink lace')
COLOR_NAMES[clr_h2i('#FFDEAD')]=_('Navajo white')
COLOR_NAMES[clr_h2i('#FFDF00')]=_('Golden yellow')
COLOR_NAMES[clr_h2i('#FFDF46')]=_('Gargoyle gas')
COLOR_NAMES[clr_h2i('#FFDFBF')]=_('Very pale orange')
COLOR_NAMES[clr_h2i('#FFE135')]=_('Banana yellow')
COLOR_NAMES[clr_h2i('#FFE1FF')]=_('Thistle')
COLOR_NAMES[clr_h2i('#FFE302')]=_('Vivid yellow')
COLOR_NAMES[clr_h2i('#FFE4B5')]=_('Moccasin')
COLOR_NAMES[clr_h2i('#FFE4C4')]=_('Bisque')
COLOR_NAMES[clr_h2i('#FFE4CD')]=_('Lumber')
COLOR_NAMES[clr_h2i('#FFE4E1')]=_('Misty rose')
COLOR_NAMES[clr_h2i('#FFE5B4')]=_('Peach')
COLOR_NAMES[clr_h2i('#FFE7BA')]=_('Wheat')
COLOR_NAMES[clr_h2i('#FFEB00')]=_('Middle yellow')
COLOR_NAMES[clr_h2i('#FFEBCD')]=_('Blanched almond')
COLOR_NAMES[clr_h2i('#FFEC8B')]=_('Light goldenrod')
COLOR_NAMES[clr_h2i('#FFEF00')]=_('Canary yellow')
COLOR_NAMES[clr_h2i('#FFEFD5')]=_('Papaya whip')
COLOR_NAMES[clr_h2i('#FFEFDB')]=_('Antique white')
COLOR_NAMES[clr_h2i('#FFF000')]=_('Yellow rose')
COLOR_NAMES[clr_h2i('#FFF0F5')]=_('Lavender blush')
COLOR_NAMES[clr_h2i('#FFF44F')]=_('Lemon yellow')
COLOR_NAMES[clr_h2i('#FFF5EE')]=_('Seashell')
COLOR_NAMES[clr_h2i('#FFF600')]=_('Cadmium yellow')
COLOR_NAMES[clr_h2i('#FFF68F')]=_('Khaki')
COLOR_NAMES[clr_h2i('#FFF700')]=_('Lemon, Yellow sunshine')
COLOR_NAMES[clr_h2i('#FFF8DC')]=_('Cornsilk')
COLOR_NAMES[clr_h2i('#FFF8E7')]=_('Cosmic latte')
COLOR_NAMES[clr_h2i('#FFFACD')]=_('Lemon chiffon')
COLOR_NAMES[clr_h2i('#FFFAF0')]=_('Floral white')
COLOR_NAMES[clr_h2i('#FFFAFA')]=_('Snow')
COLOR_NAMES[clr_h2i('#FFFDD0')]=_('Cream')
COLOR_NAMES[clr_h2i('#FFFF00')]=_('Yellow')
COLOR_NAMES[clr_h2i('#FFFF31')]=_('Daffodil')
COLOR_NAMES[clr_h2i('#FFFF33')]=_('Electric yellow')
COLOR_NAMES[clr_h2i('#FFFF66')]=_('Unmellow yellow')
COLOR_NAMES[clr_h2i('#FFFF99')]=_('Canary')
COLOR_NAMES[clr_h2i('#FFFFBF')]=_('Very pale yellow')
COLOR_NAMES[clr_h2i('#FFFFE0')]=_('Light yellow')
COLOR_NAMES[clr_h2i('#FFFFF0')]=_('Ivory')
COLOR_NAMES[clr_h2i('#FFFFFF')]=_('White')

COLOR_NAMES[clr_h2i('#EEDD82')]=_('Light goldenrod')
COLOR_NAMES[clr_h2i('#AEEEEE')]=_('Pale turquoise')
