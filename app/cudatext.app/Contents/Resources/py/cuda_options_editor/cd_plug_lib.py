''' Lib for Plugin
Authors:
    Andrey Kvichansky    (kvichans on github.com)
Version:
    '2.1.16 2018-05-14'
Content
    log                 Logger with timing
    get_translation     i18n
    dlg_wrapper         Wrapper for dlg_custom: pack/unpack values, h-align controls
ToDo: (see end of file)
'''

import  sys, os, gettext, logging, inspect, time, collections, json, re, subprocess
from    time        import perf_counter

try:
    import  cudatext            as app
    from    cudatext        import ed
    import  cudax_lib           as apx
except:
    import  sw                  as app
    from    sw              import ed
    from . import cudax_lib     as apx

pass;                           # Logging
pass;                           from pprint import pformat
pass;                           import tempfile

odict       = collections.OrderedDict
T,F,N       = True, False, None
GAP         = 5
c13,c10,c9  = chr(13),chr(10),chr(9)
REDUCTIONS  = {'lb'     :'label'
            ,  'ln-lb'  :'linklabel'
            ,  'llb'    :'linklabel'
            ,  'ed'     :'edit'             # ro_mono_brd
            ,  'ed_pw'  :'edit_pwd'
            ,  'sp-ed'  :'spinedit'         # min_max_inc
            ,  'sed'    :'spinedit'         # min_max_inc
            ,  'me'     :'memo'             # ro_mono_brd
            ,  'bt'     :'button'           # def_bt
            ,  'rd'     :'radio'
            ,  'ch'     :'check'
            ,  'ch-bt'  :'checkbutton'
            ,  'ch-b'   :'checkbutton'
            ,  'chb'    :'checkbutton'
            ,  'ch-gp'  :'checkgroup'
            ,  'rd-gp'  :'radiogroup'
            ,  'cb'     :'combo'
            ,  'cb-ro'  :'combo_ro'
            ,  'cb-r'   :'combo_ro'
            ,  'cbr'    :'combo_ro'
            ,  'lbx'    :'listbox'
            ,  'ch-lbx' :'checklistbox'
            ,  'clx'    :'checklistbox'
            ,  'lvw'    :'listview'
            ,  'ch-lvw' :'checklistview'
            ,  'tabs'   :'tabs'
            ,  'clr'    :'colorpanel'
            ,  'im'     :'image'
            ,  'f-lb'   :'filter_listbox'
            ,  'f-lvw'  :'filter_listview'
            ,  'fr'     :'bevel'
            ,  'pn'     :'panel'
            ,  'gr'     :'group'
            ,  'sp'     :'splitter'

            ,  'tvw'    :'treeview'
            ,  'edr'    :'editor'
            ,  'sb'     :'statusbar'
            ,  'bte'    :'button_ex'
            
#           ,  'fid'    :'focused'
            ,  'cols'   :'columns'
            }

def f(s, *args, **kwargs):return s.format(*args, **kwargs)

def log(msg='', *args, **kwargs):
    if args or kwargs:
        msg = msg.format(*args, **kwargs)
    if Tr.tr is None:
        Tr.tr=Tr()
    return Tr.tr.log(msg)
    
class Tr :
    tr=None
    """ Трассировщик.
        Основной (единственный) метод: log(строка) - выводит указанную строку в лог.
        Управляется через команды в строках для вывода.
        Команды:
            >>  Увеличить сдвиг при выводе будущих строк (пока жив возвращенный объект) 
            (:) Начать замер нового вложенного периода, закончить когда умрет возвращенный объект 
            (== Начать замер нового вложенного периода 
            ==> Вывести длительность последнего периода 
            ==) Вывести длительность последнего периода и закончить его замер
            =}} Отменить все замеры
        Вызов log с командой >> (увеличить сдвиг) возвращает объект, 
            который при уничтожении уменьшит сдвиг 
        """
    sec_digs        = 2                     # Точность отображения секунд, кол-во дробных знаков
    se_fmt          = ''
    mise_fmt        = ''
    homise_fmt      = ''
    def __init__(self, log_to_file=None) :
        # Поля объекта
        self.gap    = ''                # Отступ
        self.tm     = perf_counter()    # Отметка времени о запуске
        self.stms   = []                # Отметки времени о начале замера спец.периода

        if log_to_file:
            logging.basicConfig( filename=log_to_file
                                ,filemode='w'
                                ,level=logging.DEBUG
                                ,format='%(message)s'
                                ,datefmt='%H:%M:%S'
                                ,style='%')
        else: # to stdout
            logging.basicConfig( stream=sys.stdout
                                ,level=logging.DEBUG
                                ,format='%(message)s'
                                ,datefmt='%H:%M:%S'
                                ,style='%')
        # Tr()
    def __del__(self):
        logging.shutdown()

    class TrLiver :
        cnt = 0
        """ Автоматически сокращает gap при уничножении 
            Показывает время своей жизни"""
        def __init__(self, tr, ops) :
            # Поля объекта
            self.tr = tr
            self.ops= ops
            self.tm = 0
            self.nm = Tr.TrLiver.cnt
            if '(:)' in self.ops :
                # Начать замер нового интервала
                self.tm = perf_counter()
        def log(self, msg='') :
            if '(:)' in self.ops :
                msg = '{}(:)=[{}]{}'.format( self.nm, Tr.format_tm( perf_counter() - self.tm ), msg ) 
                logging.debug( self.tr.format_msg(msg, ops='') )
        def __del__(self) :
            #pass;                  logging.debug('in del')
            if '(:)' in self.ops :
                msg = '{}(:)=[{}]'.format( self.nm, Tr.format_tm( perf_counter() - self.tm ) ) 
                logging.debug( self.tr.format_msg(msg, ops='') )
            if '>>' in self.ops :
                self.tr.gap = self.tr.gap[:-1]
                
    def log(self, msg='') :
        if '(:)' in msg :
            Tr.TrLiver.cnt += 1
            msg     = msg.replace( '(:)', '{}(:)'.format(Tr.TrLiver.cnt) )  
        logging.debug( self.format_msg(msg) )
        if '>>' in msg :
            self.gap = self.gap + c9
            # Создаем объект, который при разрушении сократит gap
        if '>>' in msg or '(:)' in msg:
            return Tr.TrLiver(self,('>>' if '>>' in msg else '')+('(:)' if '(:)' in msg else ''))
            # return Tr.TrLiver(self,iif('>>' in msg,'>>','')+iif('(:)' in msg,'(:)',''))
        else :
            return self 
        # Tr.log
            
#   def format_msg(self, msg, dpth=2, ops='+fun:ln +wait==') :
    def format_msg(self, msg, dpth=3, ops='+fun:ln +wait==') :
        if '(==' in msg :
            # Начать замер нового интервала
            self.stms   = self.stms + [perf_counter()]
            msg = msg.replace( '(==', '(==[' + Tr.format_tm(0) + ']' )

        if '###' in msg :
            # Показать стек
            st_inf  = '\n###'
            for fr in inspect.stack()[1+dpth:]:
                try:
                    cls = fr[0].f_locals['self'].__class__.__name__ + '.'
                except:
                    cls = ''
                fun     = (cls + fr[3]).replace('.__init__','()')
                ln      = fr[2]
                st_inf  += '    {}:{}'.format(fun, ln)
            msg    += st_inf

        if '+fun:ln' in ops :
            frCaller= inspect.stack()[dpth] # 0-format_msg, 1-Tr.log|Tr.TrLiver, 2-log, 3-need func
            try:
                cls = frCaller[0].f_locals['self'].__class__.__name__ + '.'
            except:
                cls = ''
            fun     = (cls + frCaller[3]).replace('.__init__','()')
            ln      = frCaller[2]
            msg     = '[{}]{}{}:{} '.format( Tr.format_tm( perf_counter() - self.tm ), self.gap, fun, ln ) + msg
        else : 
            msg     = '[{}]{}'.format( Tr.format_tm( perf_counter() - self.tm ), self.gap ) + msg

        if '+wait==' in ops :
            if ( '==)' in msg or '==>' in msg ) and len(self.stms)>0 :
                # Закончить/продолжить замер последнего интервала и вывести его длительность
                sign    = '==)' if '==)' in msg else '==>'
                # sign    = icase( '==)' in msg, '==)', '==>' )
                stm = '[{}]'.format( Tr.format_tm( perf_counter() - self.stms[-1] ) )
                msg = msg.replace( sign, sign+stm )
                if '==)' in msg :
                    del self.stms[-1] 

            if '=}}' in msg :
                # Отменить все замеры
                self.stms   = []
                
        return msg.replace('¬',c9).replace('¶',c10)
        # Tr.format

    @staticmethod
    def format_tm(secs) :
        """ Конвертация количества секунд в 12h34'56.78" """
        if 0==len(Tr.se_fmt) :
            Tr.se_fmt       = '{:'+str(3+Tr.sec_digs)+'.'+str(Tr.sec_digs)+'f}"'
            Tr.mise_fmt     = "{:2d}'"+Tr.se_fmt
            Tr.homise_fmt   = "{:2d}h"+Tr.mise_fmt
        h = int( secs / 3600 )
        secs = secs % 3600
        m = int( secs / 60 )
        s = secs % 60
        return Tr.se_fmt.format(s) \
                if 0==h+m else \
               Tr.mise_fmt.format(m,s) \
                if 0==h else \
               Tr.homise_fmt.format(h,m,s)
        # return icase( 0==h+m,   Tr.se_fmt.format(s)
        #             , 0==h,     Tr.mise_fmt.format(m,s)
        #             ,           Tr.homise_fmt.format(h,m,s) )
        # Tr.format_tm
    # Tr

def get_desktop_environment():
    #From http://stackoverflow.com/questions/2035657/what-is-my-current-desktop-environment
    # and http://ubuntuforums.org/showthread.php?t=652320
    # and http://ubuntuforums.org/showthread.php?t=652320
    # and http://ubuntuforums.org/showthread.php?t=1139057
    if sys.platform in ["win32", "cygwin"]:
        return "win"
    elif sys.platform == "darwin":
        return "mac"
    else: #Most likely either a POSIX system or something not much common
        desktop_session = os.environ.get("DESKTOP_SESSION")
        if desktop_session is not None: #easier to match if we doesn't have  to deal with caracter cases
            desktop_session = desktop_session.lower()
            if desktop_session in ["gnome","unity", "cinnamon", "mate", "xfce4", "lxde", "fluxbox", 
                                   "blackbox", "openbox", "icewm", "jwm", "afterstep","trinity", "kde"]:
                return desktop_session
            ## Special cases ##
            # Canonical sets $DESKTOP_SESSION to Lubuntu rather than LXDE if using LXDE.
            # There is no guarantee that they will not do the same with the other desktop environments.
            elif "xfce" in desktop_session or desktop_session.startswith("xubuntu"):
                return "xfce4"
            elif desktop_session.startswith("ubuntu"):
                return "unity"       
            elif desktop_session.startswith("lubuntu"):
                return "lxde" 
            elif desktop_session.startswith("kubuntu"): 
                return "kde" 
            elif desktop_session.startswith("razor"): # e.g. razorkwin
                return "razor-qt"
            elif desktop_session.startswith("wmaker"): # e.g. wmaker-common
                return "windowmaker"
        if os.environ.get('KDE_FULL_SESSION') == 'true':
            return "kde"
        elif os.environ.get('GNOME_DESKTOP_SESSION_ID'):
            if not "deprecated" in os.environ.get('GNOME_DESKTOP_SESSION_ID'):
                return "gnome2"
        #From http://ubuntuforums.org/showthread.php?t=652320
        elif is_running("xfce-mcs-manage"):
            return "xfce4"
        elif is_running("ksmserver"):
            return "kde"
    return "unknown"
def is_running(process):
    #From http://www.bloggerpolis.com/2011/05/how-to-check-if-a-process-is-running-using-python/
    # and http://richarddingwall.name/2009/06/18/windows-equivalents-of-ps-and-kill-commands/
    try: #Linux/Unix
        s = subprocess.Popen(["ps", "axw"],stdout=subprocess.PIPE)
    except: #Windows
        s = subprocess.Popen(["tasklist", "/v"],stdout=subprocess.PIPE)
    for x in s.stdout:
        if re.search(process, str(x)):
            return True
    return False

ENV2FITS= {'win':
            {'check'      :-2
            ,'radio'      :-2
            ,'edit'       :-3
            ,'button'     :-4
            ,'combo_ro'   :-4
            ,'combo'      :-3
            ,'checkbutton':-5
            ,'linklabel'  : 0
            ,'spinedit'   :-3
            }
          ,'unity':
            {'check'      :-3
            ,'radio'      :-3
            ,'edit'       :-5
            ,'button'     :-4
            ,'combo_ro'   :-5
            ,'combo'      :-6
            ,'checkbutton':-4
            ,'linklabel'  : 0
            ,'spinedit'   :-6
            }
          ,'mac':
            {'check'      :-1
            ,'radio'      :-1
            ,'edit'       :-3
            ,'button'     :-3
            ,'combo_ro'   :-2
            ,'combo'      :-3
            ,'checkbutton':-2
            ,'linklabel'  : 0
            ,'spinedit'   : 0   ##??
            }
          }
fit_top_by_env__cash    = {}
def fit_top_by_env__clear():
    global fit_top_by_env__cash
    fit_top_by_env__cash    = {}
def fit_top_by_env(what_tp, base_tp='label'):
    """ Get "fitting" to add to top of first control to vertical align inside text with text into second control.
        The fittings rely to platform: win, unix(kde,gnome,...), mac
    """
    if what_tp==base_tp:
        return 0
    if (what_tp, base_tp) in fit_top_by_env__cash:
        pass;                  #log('cashed what_tp, base_tp={}',(what_tp, base_tp))
        return fit_top_by_env__cash[(what_tp, base_tp)]
    env     = get_desktop_environment()
    pass;                      #env = 'mac'
    fit4lb  = ENV2FITS.get(env, ENV2FITS.get('win'))
    fit     = 0
    if base_tp=='label':
        fit = apx.get_opt('dlg_wrapper_fit_va_for_'+what_tp, fit4lb.get(what_tp, 0))
        pass;                  #fit_o=fit
        fit = _os_scale(app.DLG_PROP_GET, {'y':fit})['y']
        pass;                  #log('what_tp,fit_o,fit,h={}',(what_tp,fit_o,fit,get_gui_height(what_tp)))
    else:
        fit = fit_top_by_env(what_tp) - fit_top_by_env(base_tp)
    pass;                      #log('what_tp, base_tp, fit={}',(what_tp, base_tp, fit))
    return fit_top_by_env__cash.setdefault((what_tp, base_tp), fit)
   #def fit_top_by_env

def rgb_to_int(r,g,b):
    return r | (g<<8) | (b<<16)
def dlg_wrapper(title, w, h, cnts, in_vals={}, focus_cid=None):
    """ Wrapper for dlg_custom. 
        Params
            title, w, h     Title, Width, Height 
            cnts            List of static control properties
                                [{cid:'*', tp:'*', t:1,l:1,w:1,r:1,b;1,h:1,tid:'cid', cap:'*', hint:'*', en:'0', props:'*', items:[*], act='0'}]
                                cid         (opt)(str) C(ontrol)id. Need only for buttons and conrols with value (and for tid)
                                tp               (str) Control types from wiki or short names
                                t           (opt)(int) Top
                                tid         (opt)(str) Ref to other control cid for horz-align text in both controls
                                l                (int) Left
                                r,b,w,h     (opt)(int) Position. w>>>r=l+w, h>>>b=t+h, b can be omitted
                                cap              (str) Caption for labels and buttons
                                hint        (opt)(str) Tooltip
                                en          (opt)('0'|'1'|True|False) Enabled-state
                                props       (opt)(str) See wiki
                                act         (opt)('0'|'1'|True|False) Will close dlg when changed
                                items            (str|list) String as in wiki. List structure by types:
                                                            [v1,v2,]     For combo, combo_ro, listbox, checkgroup, radiogroup, checklistbox
                                                            (head, body) For listview, checklistview 
                                                                head    [(cap,width),(cap,width),]
                                                                body    [[r0c0,r0c1,],[r1c0,r1c1,],[r2c0,r2c1,],]
            in_vals         Dict of start values for some controls 
                                {'cid':val}
            focus_cid       (opt) Control cid for start focus
        Return
            btn_cid         Clicked/changed control cid
            {'cid':val}     Dict of new values for the same (as in_vals) controls
                                Format of values is same too.
            focus_cid       Focused control cid
            [cid]           List of controls with changed values
        Short names for types
            lb      label
            ln-lb   linklabel
            ed      edit
            sp-ed   spinedit
            me      memo
            bt      button
            rd      radio
            ch      check
            ch-bt   checkbutton
            ch-gp   checkgroup
            rd-gp   radiogroup
            cb      combo
            cb-ro   combo_ro
            lbx     listbox
            ch-lbx  checklistbox
            lvw     listview
            ch-lvw  checklistview
        Example.
            def ask_number(ask, def_val):
                cnts=[dict(        tp='lb',tid='v',l=3 ,w=70,cap=ask)
                     ,dict(cid='v',tp='ed',t=3    ,l=73,w=70)
                     ,dict(cid='!',tp='bt',t=45   ,l=3 ,w=70,cap='OK',props='1')
                     ,dict(cid='-',tp='bt',t=45   ,l=73,w=70,cap='Cancel')]
                vals={'v':def_val}
                while True:
                    aid,vals,fid,chds=dlg_wrapper('Example',146,75,cnts,vals,'v')
                    if aid is None or btn=='-': return def_val
                    if not re.match(r'\d+$', vals['v']): continue
                    return vals['v']
    """
    pass;                      #log('in_vals={}',pformat(in_vals, width=120))
    cnts        = [cnt for cnt in cnts if cnt.get('vis', True) in (True, '1')]
    cid2i       = {cnt['cid']:i for i,cnt in enumerate(cnts) if 'cid' in cnt}
    if True:
        # Checks
        no_tids = {cnt['tid']   for   cnt in    cnts    if 'tid' in cnt and  cnt['tid'] not in cid2i}
        if no_tids:
            raise Exception(f('No cid(s) for tid(s): {}', no_tids))
        no_vids = {cid          for   cid in    in_vals if                          cid not in cid2i}
        if no_vids:
            raise Exception(f('No cid(s) for vals: {}', no_vids))
    
    simpp   = ['cap','hint'
              ,'props'
              ,'color'
              ,'font_name', 'font_size', 'font_color', 'font'
              ,'act'
              ,'en','vis'
             #,'tag'
              ]
    ctrls_l = []
    for cnt in cnts:
        tp      = cnt['tp']
        tp      = REDUCTIONS.get(tp, tp)
        if tp=='--':
            # Horz-line
            t   = cnt.get('t')
            l   = cnt.get('l', 0)                   # def: from DlgLeft
            r   = cnt.get('r', l+cnt.get('w', w))   # def: to   DlgRight
            lst = ['type=label']
            lst+= ['cap='+'—'*1000]
            lst+= ['font_color='+str(rgb_to_int(185,185,185))]
            lst+= ['pos={l},{t},{r},0'.format(l=l,t=t,r=r)]
            ctrls_l+= [chr(1).join(lst)]
            continue#for cnt
            
        lst     = ['type='+tp]

        # Preprocessor
        if 'props' in cnt:
            pass
        elif tp=='label' and cnt['cap'][0]=='>':
            #   cap='>smth' --> cap='smth', props='1' (r-align)
            cnt['cap']  = cnt['cap'][1:]
            cnt['props']= '1'
        elif tp=='label' and cnt.get('ralign'):
            cnt['props']=    cnt.get('ralign')
        elif tp=='button' and cnt.get('def_bt') in ('1', True):
            cnt['props']= '1'
        elif tp=='spinedit' and cnt.get('min_max_inc'):
            cnt['props']=       cnt.get('min_max_inc')
        elif tp=='linklabel' and cnt.get('url'):
            cnt['props']=        cnt.get('url')
        elif tp=='listview' and cnt.get('grid'):
            cnt['props']=       cnt.get('grid')
        elif tp=='tabs' and cnt.get('at_botttom'):
            cnt['props']=   cnt.get('at_botttom')
        elif tp=='colorpanel' and cnt.get('brdW_fillC_fontC_brdC'):
            cnt['props']=         cnt.get('brdW_fillC_fontC_brdC')
        elif tp in ('edit', 'memo') and cnt.get('ro_mono_brd'):
            cnt['props']=               cnt.get('ro_mono_brd')

#       # Simple props
#       for k in ['cap', 'hint', 'props', 'font_name', 'font_size', 'font_color', 'font', 'name']:
#               lst += [k+'='+str(cnt[k])]

        # Position:
        #   t[op] or tid, l[eft] required
        #   w[idth]  >>> r[ight ]=l+w
        #   h[eight] >>> b[ottom]=t+h
        #   b dont need for buttons, edit, labels
        l       = cnt['l']
        t       = cnt.get('t', 0)
        if 'tid' in cnt:
            # cid for horz-align text
            bs_cnt  = cnts[cid2i[cnt['tid']]]
            bs_tp   = bs_cnt['tp']
            t       = bs_cnt['t'] + fit_top_by_env(tp, REDUCTIONS.get(bs_tp, bs_tp))
        r       = cnt.get('r', l+cnt.get('w', 0)) 
        b       = cnt.get('b', t+cnt.get('h', 0)) 
        lst    += ['pos={l},{t},{r},{b}'.format(l=l,t=t,r=r,b=b)]
#       if 'en' in cnt:
#           val     = cnt['en']
#           lst    += ['en='+('1' if val in [True, '1'] else '0')]

        if 'items' in cnt:
            items   = cnt['items']
            if isinstance(items, str):
                pass
            elif tp in ['listview', 'checklistview']:
                # For listview, checklistview: "\t"-separated items.
                #   first item is column headers: title1+"="+size1 + "\r" + title2+"="+size2 + "\r" +...
                #   other items are data: cell1+"\r"+cell2+"\r"+...
                # ([(hd,wd)], [[cells],[cells],])
                items   = '\t'.join(['\r'.join(['='.join((hd,sz)) for hd,sz in items[0]])]
                                   +['\r'.join(row) for row in items[1]]
                                   )
            else:
                # For combo, combo_ro, listbox, checkgroup, radiogroup, checklistbox: "\t"-separated lines
                items   = '\t'.join(items)
            lst+= ['items='+items]
        
        # Prepare val
        if cnt.get('cid') in in_vals:
            in_val = in_vals[cnt['cid']]
            if False:pass
            elif tp in ['check', 'radio', 'checkbutton'] and isinstance(in_val, bool):
                # For check, radio, checkbutton: value "0"/"1" 
                in_val  = '1' if in_val else '0'
            elif tp=='memo':
                # For memo: "\t"-separated lines (in lines "\t" must be replaced to chr(2)) 
                if isinstance(in_val, list):
                    in_val = '\t'.join([v.replace('\t', chr(2)) for v in in_val])
                else:
                    in_val = in_val.replace('\t', chr(2)).replace('\r\n','\n').replace('\r','\n').replace('\n','\t')
            elif tp=='checkgroup' and isinstance(in_val, list):
                # For checkgroup: ","-separated checks (values "0"/"1") 
                in_val = ','.join(in_val)
            elif tp in ['checklistbox', 'checklistview'] and isinstance(in_val, tuple):
                # For checklistbox, checklistview: index+";"+checks 
                in_val = ';'.join( (str(in_val[0]), ','.join( in_val[1]) ) )
            lst+= ['val='+str(in_val)]

#       if 'act' in cnt:    # must be last in lst
#           val     = cnt['act']
#           lst    += ['act='+('1' if val in [True, '1'] else '0')]
 
        # Simple props
        for k in simpp:
            if k in cnt:
                v   = cnt[k]
                v   = ('1' if v else '0') if isinstance(v, bool) else str(v)
                lst += [k+'='+v]
        pass;                  #log('lst={}',lst)
        ctrls_l+= [chr(1).join(lst)]
       #for cnt
    pass;                      #log('ok ctrls_l={}',pformat(ctrls_l, width=120))

    pass;                      #ctrls_fn=tempfile.gettempdir()+os.sep+'dlg_custom_ctrls.txt'
    pass;                      #open(ctrls_fn, 'w', encoding='UTF-8').write('\n'.join(ctrls_l).replace('\r',''))
    pass;                      #log(f(r'app.dlg_custom("{t}",{w},{h},open(r"{fn}",encoding="UTF-8").read(), {f})',t=title, w=w, h=h, fn=ctrls_fn, f=cid2i.get(focus_cid, -1)))
    ans     = app.dlg_custom(title, w, h, '\n'.join(ctrls_l), cid2i.get(focus_cid, -1))
    if ans is None: return None, None, None, None   # btn_cid, {cid:v}, focus_cid, [cid]
    pass;                      #log('ans={})',ans)

    btn_i,  \
    vals_ls = ans[0], ans[1].splitlines()
    pass;                      #log('btn_i,vals_ls={})',(btn_i,vals_ls))

    focus_cid   = ''
    if vals_ls[-1].startswith('focused='):
        # From API 1.0.156 dlg_custom also returns index of active control
        focus_n_s   = vals_ls.pop()
        focus_i     = int(focus_n_s.split('=')[1])
        focus_cid   = cnts[focus_i].get('cid', '')
        pass;                  #log('btn_i,vals_ls,focus_cid={})',(btn_i,vals_ls,focus_cid))

    act_cid     = cnts[btn_i]['cid']
    # Parse output values
    an_vals = {cid:vals_ls[cid2i[cid]] for cid in in_vals}
    for cid in an_vals:
        cnt     = cnts[cid2i[cid]]
        tp      = cnt['tp']
        tp      = REDUCTIONS.get(tp, tp)
        in_val  = in_vals[cid]
        an_val  = an_vals[cid]
        pass;                  #log('tp,in_val,an_val={})',(tp,in_val,an_val))
        if False:pass
        elif tp=='memo':
            # For memo: "\t"-separated lines (in lines "\t" must be replaced to chr(2)) 
            if isinstance(in_val, list):
                an_val = [v.replace(chr(2), '\t') for v in an_val.split('\t')]
               #in_val = '\t'.join([v.replace('\t', chr(2)) for v in in_val])
            else:
                an_val = an_val.replace('\t','\n').replace(chr(2), '\t')
               #in_val = in_val.replace('\t', chr(2)).replace('\r\n','\n').replace('\r','\n').replace('\n','\t')
        elif tp=='checkgroup' and isinstance(in_val, list):
            # For checkgroup: ","-separated checks (values "0"/"1") 
            an_val = an_val.split(',')
           #in_val = ','.join(in_val)
        elif tp in ['checklistbox', 'checklistview'] and isinstance(in_val, tuple):
            an_val = an_val.split(';')
            an_val = (an_val[0], an_val[1].split(','))
           #in_val = ';'.join(in_val[0], ','.join(in_val[1]))
        elif isinstance(in_val, bool): 
            an_val = an_val=='1'
        elif tp=='listview':
            an_val = -1 if an_val=='' else int(an_val)
        else: 
            an_val = type(in_val)(an_val)
            pass;              #log('type(in_val),an_val={})',(type(in_val),an_val))
        an_vals[cid]    = an_val
       #for cid
    chds    = [cid for cid in in_vals if in_vals[cid]!=an_vals[cid]]
    if focus_cid:
        # If out focus points to button then will point to a unique changed control
        focus_tp= cnts[cid2i[focus_cid]]['tp']
        focus_tp= REDUCTIONS.get(focus_tp, focus_tp)
        if focus_tp in ('button'):
            focus_cid   = '' if len(chds)!=1 else chds[0]
    return  act_cid \
        ,   an_vals \
        ,   focus_cid \
        ,   chds
   #def dlg_wrapper

DLG_CTL_ADD_SET = 26
DLG_PROC_I2S={
0:'DLG_CREATE'
,1:'DLG_FREE'
,5:'DLG_SHOW_MODAL'
,6:'DLG_SHOW_NONMODAL'
,7:'DLG_HIDE'
,8:'DLG_FOCUS'
,9:'DLG_SCALE'
,10:'DLG_PROP_GET'
,11:'DLG_PROP_SET'
,12:'DLG_DOCK'
,13:'DLG_UNDOCK'
,20:'DLG_CTL_COUNT'
,21:'DLG_CTL_ADD'
,26:'DLG_CTL_ADD_SET'
,22:'DLG_CTL_PROP_GET'
,23:'DLG_CTL_PROP_SET'
,24:'DLG_CTL_DELETE'
,25:'DLG_CTL_DELETE_ALL'
,30:'DLG_CTL_FOCUS'
,31:'DLG_CTL_FIND'
,32:'DLG_CTL_HANDLE'
}
_SCALED_KEYS = ('x', 'y', 'w', 'h'
            ,  'w_min', 'w_max', 'h_min', 'h_max'
            ,  'sp_l', 'sp_r', 'sp_t', 'sp_b', 'sp_a'
            )
def _os_scale(id_action, prop=None, index=-1, index2=-1, name=''):
    pass;                      #return prop
    pass;                      #log('prop={}',({k:prop[k] for k in prop if k in ('x','y')}))
    pass;                      #logb=name in ('tolx', 'tofi') and id_action==app.DLG_CTL_PROP_GET
    pass;                      #log('name,prop={}',(name,{k:prop[k] for k in prop if k in ('h','_ready_h')})) if logb else 0
    ppi     = app.app_proc(app.PROC_GET_SYSTEM_PPI, '')
    if ppi==96:
        return prop
    scale   = ppi/96
    pass;                      #log('id_dialog, id_action,scale={}',(id_dialog, DLG_PROC_I2S[id_action],scale))
    if False:pass
    elif id_action in (app.DLG_PROP_SET     , app.DLG_PROP_GET
                      ,app.DLG_CTL_PROP_SET , app.DLG_CTL_PROP_GET
                      ,'scale', 'unscale'):

        def scale_up(prop_dct):
            for k in _SCALED_KEYS:
                if k in prop_dct and '_ready_'+k not in prop_dct:
                    prop_dct[k]   =             round(prop_dct[k] * scale)      # Scale!
        
        def scale_dn(prop_dct):
            for k in _SCALED_KEYS:
                if k in prop_dct and '_ready_'+k not in prop_dct:
                    prop_dct[k]   =             round(prop_dct[k] / scale)      # UnScale!
        
#       pass;                   print('a={}, ?? pr={}'.format(DLG_PROC_I2S[id_action], {k:prop[k] for k in prop if k in _SCALED_KEYS or k=='name'}))
        if False:pass
        elif id_action==app.DLG_PROP_SET:                   scale_up(prop)
        elif id_action==app.DLG_CTL_PROP_SET and -1!=index: scale_up(prop)
        elif id_action==app.DLG_CTL_PROP_SET and ''!=name:  scale_up(prop)
        elif id_action==app.DLG_PROP_GET:                   scale_dn(prop)
        elif id_action==app.DLG_CTL_PROP_GET and -1!=index: scale_dn(prop)
        elif id_action==app.DLG_CTL_PROP_GET and ''!=name:  scale_dn(prop)

        elif id_action==  'scale':                          scale_up(prop)
        elif id_action=='unscale':                          scale_dn(prop)
        pass;                  #print('a={}, ok pr={}'.format(DLG_PROC_I2S[id_action], {k:prop[k] for k in prop if k in _SCALED_KEYS or k=='name'}))
    pass;                      #log('name,prop={}',(name,{k:prop[k] for k in prop if k in ('h','_ready_h')})) if logb else 0
    return prop
   #def _os_scale

gui_height_cache= { 'button'            :0
                  , 'label'             :0
                  , 'linklabel'         :0
                  , 'combo'             :0
                  , 'combo_ro'          :0
                  , 'edit'              :0
                  , 'spinedit'          :0
                  , 'check'             :0
                  , 'radio'             :0
                  , 'checkbutton'       :0
                  , 'filter_listbox'    :0
                  , 'filter_listview'   :0
#                 , 'scrollbar'         :0
                  }
def get_gui_height(ctrl_type):
    """ Return real OS-specific height of some control
             'button'
             'label' 'linklabel'
             'combo' 'combo_ro'
             'edit' 'spinedit'
             'check' 'radio' 'checkbutton'
             'filter_listbox' 'filter_listview'
             'scrollbar'
    """
    global gui_height_cache
    if 0 == gui_height_cache['button']:
        for tpc in gui_height_cache:
            gui_height_cache[tpc]   = app.app_proc(app.PROC_GET_GUI_HEIGHT, tpc)
        pass;                  #log('gui_height_cache={}',(gui_height_cache))
        idd=app.dlg_proc(         0,    app.DLG_CREATE)
        for tpc in gui_height_cache:
            idc=app.dlg_proc(   idd,    app.DLG_CTL_ADD, tpc)
            pass;              #log('tpc,idc={}',(tpc,idc))
            prc = {'name':tpc, 'x':0, 'y':0, 'w':1, 'cap':tpc
                , 'h':gui_height_cache[tpc]}
            if tpc in ('combo' 'combo_ro'):
                prc['items']='item0'
            app.dlg_proc(       idd,    app.DLG_CTL_PROP_SET, index=idc, prop=prc)
        app.dlg_proc(           idd,    app.DLG_PROP_SET, prop={'x':-1000, 'y':-1000, 'w':100, 'h':100})
        app.dlg_proc(           idd,    app.DLG_SHOW_NONMODAL)

        ppi     = app.app_proc(app.PROC_GET_SYSTEM_PPI, '')
        if ppi!=96:
            # Try to scale height of controls
            scale   = ppi/96
            for tpc in gui_height_cache:
                prc     = app.dlg_proc( idd,    app.DLG_CTL_PROP_GET, name=tpc)
                sc_h    = round(prc['h'] * scale)
                app.dlg_proc( idd,    app.DLG_CTL_PROP_SET, name=tpc, prop=dict(h=sc_h))

        for tpc in gui_height_cache:
            prc = app.dlg_proc( idd,    app.DLG_CTL_PROP_GET, name=tpc)
            pass;              #log('prc={}',(prc))
            gui_height_cache[tpc]   = prc['h']
        app.dlg_proc(           idd,    app.DLG_FREE)
        pass;                  #log('gui_height_cache={}',(gui_height_cache))
    
    return gui_height_cache.get(ctrl_type, app.app_proc(app.PROC_GET_GUI_HEIGHT, ctrl_type))
   #def get_gui_height

def dlg_proc_wpr(id_dialog, id_action, prop='', index=-1, index2=-1, name=''):
    """ Wrapper on app.dlg_proc 
        1. To set/get dlg-props in scaled OS
        2. New command DLG_CTL_ADD_SET to set props of created ctrl
        3. Correct prop for ('label', 'button', 'checkbutton'): if no 'h' then set 'h' as OS default
    """
    if id_action==app.DLG_SCALE:
        return
#   if id_dialog==0:
#       print('idd=dlg_proc(0, {})'.format(DLG_PROC_I2S[id_action]))
#   else:
#       print('dlg_proc(idd, {}, index={}, name="{}", prop={}, index2={})'.format(   DLG_PROC_I2S[id_action], 
#           index, name, 
#           '""' if type(prop)==str else prop, 
##           '""' if type(prop)==str else {k:prop[k] for k in prop if k not in ['on_change']}, 
#           index2))
    #print('#dlg_proc id_action='+str(id_action)+' prop='+repr(prop))

    scale_on_set    = id_action in (app.DLG_PROP_SET, app.DLG_CTL_PROP_SET)
    scale_on_get    = id_action in (app.DLG_PROP_GET, app.DLG_CTL_PROP_GET)
    res             = ''
    if id_action==DLG_CTL_ADD_SET:  # Join ADD and SET for a control
        res = ctl_ind = \
        app.dlg_proc(id_dialog, app.DLG_CTL_ADD, name, -1, -1, '')       # type in name
#       if name in ('label', 'button', 'checkbutton') and 'h' not in prop:
#           prop['h'] = app.dlg_proc(id_dialog, app.DLG_CTL_PROP_GET, index=ctl_ind)['h']
        _os_scale(              app.DLG_CTL_PROP_SET, prop, ctl_ind, -1, '')
        app.dlg_proc(id_dialog, app.DLG_CTL_PROP_SET, prop, ctl_ind, -1, '')
    else:
        _os_scale(                    id_action, prop, index, index2, name) if scale_on_set else 0
        res = app.dlg_proc(id_dialog, id_action, prop, index, index2, name)
        pass;                  #log('res={}',({k:res[k] for k in res if k in ('x','y')})) if id_action==app.DLG_PROP_GET else 0
    
    _os_scale(id_action, res, index, index2, name)               if scale_on_get else 0
    return res
   #def dlg_proc_wpr

LMBD_HIDE   = lambda cid,ag:None
class BaseDlgAgent:
    """ 
    Simple helper to use dlg_proc(). See wiki.freepascal.org/CudaText_API#dlg_proc

    Main features:
    - All controls are created once then some of them can be changed via callbacks (attribute 'call').
    - The helper stores config version of form's and control's attributes.
      So the helper can return two versions of attributes: configured and actual (live).
    - Helper marshals complex data ('items' for type=list/combo/..., 'val' for type=memo)
    - Helper can to save/restore form position and sizes to/from file "settings/forms data.json".
      Key for saving is form's 'cap' (default) or a value in call BaseDlgAgent(..., option={'form data key':'smth'})
      If value of 'form data key' is empty then helper doesnt save/restore.

    Format of constructor 
        BaseDlgAgent(ctrls, form=None, focused=None)
            ctrls             [(name,{...})]        To create controls with the names
                                                    All attributes from 
                                                        wiki.freepascal.org/CudaText_API#Control_properties
                                                        excluded: name, callback
            form              {...}                 To configure form
                                                    All attributes from
                                                        wiki.freepascal.org/CudaText_API#Form_properties
            focused           name_to_focus         To set focus
    
    Format of callback for a control
        def callname(name_of_event_control, agent):
            # Reactions
            return None                         #   To hide form
            return {}                           #   To keep controls and form state
            return {'ctrls':  [(name,{...})]    #   To change controls with the names
                   ,'form':   {...}             #   To change form
                   ,'focused':name_to_focus     #   To set focus
                   }                            #   Any key ('ctrls','form','focused') can be ommited.
    Callback cannot add new controls or change type values.
    
    Useful methods of agent
    - agent.cattr(name, '??', live=T)               To get a control actual/configured attribute
    - agent.cattrs(name, ['??'], live=T)            To get dict of a control listed actual/configured attributes
    - agent.fattr('??', live=T)                     To get actual/configured form attribute
    - agent.fattrs(live=T, ['??']=None)             To get actual/configured all/listed form attributes
    
    Tricks
    Automatically set some values for attributes
        - 'act':True    If 'call'   set in a control (not 'button')
        - 'w_min':w     If 'resize' set in the form
        - 'h_min':h     If 'resize' set in the form

    Example. Dialog with two buttons.
    BaseDlgAgent(
        ctrls=[('b1', dict(type='button',             cap='Click', x=0, y= 0, w=100, 
                call=lambda name,ag:{'ctrls':[('b1',{'cap':'OK',             'w':70})]}
               ))
              ,('b2', dict(type='button', cap='Close', x=0, y=30, w=100,
                call=lambda name,ag:None)
               )]
    ,   form=dict(cap='Two buttons', x=0, y=0, w=100, h=60)
    ,   focused='b1'
    ).show()
    """
 
    def activate(self):
        """ Set focus to the form """
        app.dlg_proc(self.id_dlg, app.DLG_FOCUS)
       #def activate
    
    def hide(self):
        app.dlg_proc(self.id_dlg, app.DLG_HIDE)
       #def hide
    
    def show(self, callbk_on_exit=None):
        """ Show the form """
        ed_caller   = ed
        
#       app.dlg_proc(self.id_dlg, app.DLG_SCALE)        #??
#       pass;                   pr_   = dlg_proc_wpr(self.id_dlg, app.DLG_CTL_PROP_GET, name='edch')
#       pass;                   log('exit,pr_={}',('edch', {k:v for k,v in pr_.items() if k in ('h','y')}))
        app.dlg_proc(self.id_dlg, app.DLG_SHOW_MODAL)
#       pass;                   pr_   = dlg_proc_wpr(self.id_dlg, app.DLG_CTL_PROP_GET, name='edch')
#       pass;                   log('exit,pr_={}',('edch', {k:v for k,v in pr_.items() if k in ('h','y')}))
        pass;                  #log('ok DLG_SHOW_MODAL',())
        
        BaseDlgAgent._form_acts('save', id_dlg=self.id_dlg, key4store=self.opts.get('form data key'))
        if callbk_on_exit:  callbk_on_exit(self)
        dlg_proc_wpr(self.id_dlg, app.DLG_FREE)
        
        ed_to_fcs   = ed_caller \
                        if 'on_exit_focus_to_ed' not in self.opts else \
                      self.opts['on_exit_focus_to_ed']
        pass;                  #log('self.opts={}',(self.opts))
        pass;                  #log('ed_to_fcs.get_filename()={}',(ed_to_fcs.get_filename()))
        if ed_to_fcs:
            ed_to_fcs.focus()
#           pass;               log('self.opts={}',(self.opts))
#           self.opts['on_exit_focus_to_ed'].focus()
#       else:
#           ed_caller.focus()
       #def show
        
    def fattr(self, attr, live=True, defv=None):
        """ Return one form property """
        attr= 'focused' if attr=='fid' else attr
        pr  = dlg_proc_wpr(self.id_dlg
                        , app.DLG_PROP_GET)     if live else    self.form
        pass;                  #log('pr={}',(pr))
        rsp = pr.get(attr, defv)
        if live and attr=='focused':
            prf = dlg_proc_wpr(self.id_dlg, app.DLG_CTL_PROP_GET, index=rsp)
            rsp = prf['name'] if prf else None
        return rsp
       #def fattr

    def fattrs(self, live=True, attrs=None):
        """ Return form properties """
        pr  = dlg_proc_wpr(self.id_dlg
                        , app.DLG_PROP_GET)     if live else    self.form
        return pr   if not attrs else   {attr:pr.get(attr) for attr in attrs}
       #def fattrs

    def cattr(self, name, attr, live=True, defv=None):
        """ Return one the control property """
        live= False if attr in ('type',) else live          # Unchangable
        pr  = dlg_proc_wpr(self.id_dlg
                        , app.DLG_CTL_PROP_GET
                        , name=name)            if live else    self.ctrls[name]
        attr    = REDUCTIONS.get(attr, attr)
        if attr not in pr:  return defv
        rsp = pr[attr]
        if not live:        return rsp
        if attr=='val':     return self._take_val(name, rsp, defv)
        return                     self._take_it_cl(name, attr, rsp, defv)
#       return self._take_val(name, rsp, defv)   if attr=='val' and live else    rsp
       #def cattr

    def cattrs(self, name, attrs=None, live=True):
        """ Return the control properties """
        pr  = dlg_proc_wpr(self.id_dlg
                        , app.DLG_CTL_PROP_GET
                        , name=name)            if live else    self.ctrls[name]
        attrs   = attrs if attrs else list(pr.keys())
        pass;                  #log('pr={}',(pr))
        rsp     = {attr:pr.get(attr) for attr in attrs if attr not in ('val','on_change','callback')}
        if 'val' in attrs:
            rsp['val'] = self._take_val(name, pr.get('val')) if live else pr.get('val')
        return rsp
       #def cattrs
       
    def chandle(self, name):
        return app.dlg_proc(self.id_dlg, app.DLG_CTL_HANDLE, name=name)

    def bind_do(self, names=None, gui2data=True):
        names   = names if names else self.binds.keys()
        assert self.bindof
        for name in names:
            if name not in self.binds: continue
            attr    = 'val'
            if gui2data:
                self.bindof.__setattr__(self.binds[name], self.cattr(name, attr))
            else:
                val = self.bindof.__getattr(self.binds[name])
                self.update({'ctrls':[(name, {attr:val})]})
       #def bind_do

    def __init__(self, ctrls, form=None, focused=None, options=None):
        # Fields
        self.opts   = options if options else {}
        
        self.id_dlg = dlg_proc_wpr(0, app.DLG_CREATE)
        self.ctrls  = None                      # Conf-attrs of all controls by name (may be with 'val')
        self.form   = None                      # Conf-attrs of form
#       self.callof = self.opts.get('callof')   # Object for callbacks
        self.bindof = self.opts.get('bindof')   # Object for bind control's values to object's fields
        self.binds  = {}                        # {name:'other obj field name'}
        
        self._setup_base(ctrls, form, focused)

        rtf     = self.opts.get('gen_repro_to_file', False)
        rtf_fn  = rtf if isinstance(rtf, str) else 'repro_dlg_proc.py'
        if rtf:   self._gen_repro_code(tempfile.gettempdir()+os.sep+rtf_fn)
       #def __init__
        
    def _setup_base(self, ctrls, form, focused=None):
        """ Arrange and fill all: controls attrs, form attrs, focus.
            Params
                ctrls   [(id, {})]
                form    {}
                focused id
        """
        #NOTE: DlgAg init
        self.ctrls  = odict(ctrls)
        self.form   = form.copy()     if form   else {}
        
#       if 'checks'=='checks':
#           if focused and focused not in self.ctrls:
#               raise Exception(f('Unknown focused: {}', focused))
        
        # Create controls
        for name, cfg_ctrl in ctrls:
            assert 'type' in cfg_ctrl
            # Create control
            cfg_ctrl.pop('callback', None)
            cfg_ctrl.pop('on_change', None)
            ind_c   = dlg_proc_wpr(self.id_dlg
                        , DLG_CTL_ADD_SET
                        , name=cfg_ctrl['type']
                        , prop=self._prepare_c_pr(name, cfg_ctrl))
            pass;              #cfg_ctrl['_idc']    = ind_c         # While API bug: name isnot work if contorl is in panel
            pass;              #log('ind_c,cfg_ctrl[type]={}',(ind_c,cfg_ctrl['type']))
           #for cnt
        
        if self.form:
            fpr     = self.form
            if fpr.get('resize', False):
                fpr['w_min']    = fpr.get('w_min', fpr['w'])
                fpr['h_min']    = fpr.get('h_min', fpr['h'])
            fpr     = BaseDlgAgent._form_acts('move', form=fpr      # Move and (maybe) resize
                                             , key4store=self.opts.get('form data key'))
            fpr['topmost']      = app.app_api_version()<'1.0.270' or app.app_proc(app.PROC_WINDOW_TOPMOST_GET, '')
#           fpr['topmost']      = True
            dlg_proc_wpr(       self.id_dlg
                            , app.DLG_PROP_SET
                            , prop=fpr)
        
        if focused in self.ctrls:
            self.form['focused']   = focused
            app.dlg_proc(   self.id_dlg
                        , app.DLG_CTL_FOCUS
                        , name=focused)
       #def _setup_base
       
    def _take_val(self, name, liv_val, defv=None):
        tp      = self.ctrls[name]['type']
        old_val = self.ctrls[name].get('val', defv)
        new_val = liv_val
        if False:pass
        elif tp=='memo':
            # For memo: "\t"-separated lines (in lines "\t" must be replaced to chr(2)) 
            if isinstance(old_val, list):
                new_val = [v.replace(chr(2), '\t') for v in liv_val.split('\t')]
               #liv_val = '\t'.join([v.replace('\t', chr(2)) for v in old_val])
            else:
                new_val = liv_val.replace('\t','\n').replace(chr(2), '\t')
               #liv_val = old_val.replace('\t', chr(2)).replace('\r\n','\n').replace('\r','\n').replace('\n','\t')
        elif tp=='checkgroup' and isinstance(old_val, list):
            # For checkgroup: ","-separated checks (values "0"/"1") 
            new_val = liv_val.split(',')
           #in_val = ','.join(in_val)
        elif tp in ['checklistbox', 'checklistview'] and isinstance(old_val, tuple):
            new_val = liv_val.split(';')
            new_val = (new_val[0], new_val[1].split(','))
           #liv_val = ';'.join(old_val[0], ','.join(old_val[1]))
        elif isinstance(old_val, bool): 
            new_val = liv_val=='1'
        elif tp=='listview':
            new_val = -1 if liv_val=='' else int(liv_val)
        elif old_val is not None: 
            new_val = type(old_val)(liv_val)
        return new_val
       #def _take_val

    def _take_it_cl(self, name, attr, liv_val, defv=None):
        tp      = self.ctrls[name]['type']
        old_val = self.ctrls[name].get(attr, defv)
        pass;                  #log('name, attr, isinstance(old_val, str)={}',(name, attr, isinstance(old_val, str)))
        if isinstance(old_val, str):
            # No need parsing - config was by string
            return liv_val
        new_val = liv_val
        
        if attr=='items':
            if tp in ['listview', 'checklistview']:
                # For listview, checklistview: "\t"-separated items.
                #   first item is column headers: title1+"="+size1 + "\r" + title2+"="+size2 + "\r" +...
                #   other items are data: cell1+"\r"+cell2+"\r"+...
                # ([(hd,wd)], [[cells],[cells],])
                header_rows = new_val.split('\t')
                new_val =[[h.split('=')  for h in header_rows[0].split('\r')]
                         ,[r.split('\r') for r in header_rows[1:]]
                         ]
            else:
                # For combo, combo_ro, listbox, checkgroup, radiogroup, checklistbox: "\t"-separated lines
                new_val     = new_val.split('\t')
        
        if attr=='columns':
            # For listview, checklistview: 
            #   "\t"-separated of 
            #       "\r"-separated 
            #           Name, Width, Min Width, Max Width, Alignment (str), Autosize('0'/'1'), Visible('0'/'1')
            # [{nm:str, wd:num, mi:num, ma:num, al:str, au:bool, vi:bool}]
            pass;              #log('new_val={}',repr(new_val))
            new_val= [ci.split('\r')      for ci in new_val.split('\t')]
#           new_val= [ci.split('\r')[:-1] for ci in new_val.split('\t')[:-1]]   # API bug
            pass;              #log('new_val={}',repr(new_val))
            int_sc = lambda s: _os_scale('unscale', {'w':int(s)})['w']
            new_val= [dict(nm=       ci[0]
#                         ,wd=int( ci[1])
#                         ,mi=int( ci[2])
#                         ,ma=int( ci[3])
                          ,wd=int_sc(ci[1])
                          ,mi=int_sc(ci[2])
                          ,ma=int_sc(ci[3])
                          ,au='1'==  ci[4]
                          ,vi='1'==  ci[5]
                          ) for ci in new_val]
            pass;              #log('new_val={}',(new_val))
        
        return new_val
       #def _take_it_cl

    def _prepare_it_vl(self, c_pr, cfg_ctrl, opts={}):
        tp      = cfg_ctrl['type']

        if 'val' in cfg_ctrl        and opts.get('prepare val', True):
            in_val  = cfg_ctrl['val']
            if False:pass
            elif tp=='memo':
                # For memo: "\t"-separated lines (in lines "\t" must be replaced to chr(2)) 
                if isinstance(in_val, list):
                    in_val = '\t'.join([v.replace('\t', chr(2)) for v in in_val])
                else:
                    in_val = in_val.replace('\t', chr(2)).replace('\r\n','\n').replace('\r','\n').replace('\n','\t')
            elif tp=='checkgroup' and isinstance(in_val, list):
                # For checkgroup: ","-separated checks (values "0"/"1") 
                in_val = ','.join(in_val)
            elif tp in ['checklistbox', 'checklistview'] and isinstance(in_val, tuple):
                # For checklistbox, checklistview: index+";"+checks 
                in_val = ';'.join( (str(in_val[0]), ','.join( in_val[1]) ) )
            c_pr['val']     = in_val

        if 'items' in cfg_ctrl        and opts.get('prepare items', True):
            items   = cfg_ctrl['items']
            if isinstance(items, str):
                pass
            elif tp in ['listview', 'checklistview']:
                # For listview, checklistview: "\t"-separated items.
                #   first item is column headers: title1+"="+size1 + "\r" + title2+"="+size2 + "\r" +...
                #   other items are data: cell1+"\r"+cell2+"\r"+...
                # ([(hd,wd)], [[cells],[cells],])
                items   = '\t'.join(['\r'.join(['='.join((hd,sz)) for hd,sz in items[0]])]
                                   +['\r'.join(row) for row in items[1]]
                                   )
                pass;          #log('items={}',repr(items))
            else:
                # For combo, combo_ro, listbox, checkgroup, radiogroup, checklistbox: "\t"-separated lines
                items   = '\t'.join(items)
            c_pr['items']   = items

        if 'cols' in cfg_ctrl        and opts.get('prepare cols', True):
            cols   = cfg_ctrl['cols']
            cfg_ctrl['columns'] = cols
            if isinstance(cols, str):
                pass
            else:
                # For listview, checklistview: 
                #   "\t"-separated of 
                #       "\r"-separated 
                #           Name, Width, Min Width, Max Width, Alignment (str), Autosize('0'/'1'), Visible('0'/'1')
                pass;          #log('cols={}',(cols))
                str_sc = lambda n: str(_os_scale('scale', {'w':n})['w'])
                cols   = '\t'.join(['\r'.join([       cd[    'nm']
#                                             ,str(   cd[    'wd']   )
#                                             ,str(   cd.get('mi' ,0))
#                                             ,str(   cd.get('ma' ,0))
                                              ,str_sc(cd[    'wd']   )
                                              ,str_sc(cd.get('mi' ,0))
                                              ,str_sc(cd.get('ma' ,0))
                                              ,       cd.get('al','')
                                              ,'1' if cd.get('au',False) else '0'
                                              ,'1' if cd.get('vi',True) else '0'
                                              ])
                                    for cd in cols]
                                  )
                pass;          #log('cols={}',repr(cols))
            c_pr['columns'] = cols
            pass;              #log('isinstance(cfg_ctrl[columns], str)={}',(isinstance(cfg_ctrl['columns'], str)))

        return c_pr
       #def _prepare_it_vl

    def _prepare_c_pr(self, name, cfg_ctrl, opts={}):
        pass;                  #log('name, cfg_ctrl={}',(name, cfg_ctrl))
        c_pr    = {k:v for (k,v) in cfg_ctrl.items() if k not in ['call', 'bind', 'items', 'val']}
        c_pr['name'] = name
        tp      = cfg_ctrl['type']

        c_pr    = self._prepare_it_vl(c_pr, cfg_ctrl, opts)

        if cfg_ctrl.get('bind'):
            self.binds[name]    = cfg_ctrl['bind']
        
        if callable(cfg_ctrl.get('call'))        and opts.get('prepare call', True):
            if tp!='button':
                c_pr['act'] = True
            user_callbk = cfg_ctrl['call']
            
            def bda_c_callbk(idd, idc, data):
                pass;              #log('idc,name={}',(idc,name))
                upds = user_callbk(name, self)
                if upds is None:                                        # To hide/close
                    app.dlg_proc(self.id_dlg, app.DLG_HIDE)
                    return
                elif not upds:                                          # No changes
                    return
                self.update( ctrls  =odict(upds.get('ctrls',  []))
                            ,form   =upds.get('form',   {})
                            ,focused=upds.get('focused',None))
               #def bda_c_callbk
            on_what         = 'on_select' \
                                if tp in ('listview', 'treeview') else \
                              'on_click' \
                                if tp in ('linklabel') else \
                              'on_change'
            c_pr[on_what]   = bda_c_callbk
        
        return c_pr
       #def _prepare_c_pr

    def update(self, ctrls={}, form={}, focused=None):
        """ Change some attrs of form/controls """
        pass;                  #log('',())
        pass;                  #log('ctrls={}',(ctrls))
        pass;                  #log('form={}',(form))
        pass;                  #log('focused={}',(focused))
        if form:
            self.form.update(form)
            pass;              #log('form={}',(self.fattrs(live=F)))
            pass;              #log('form={}',(self.fattrs()))
            pass;              #log('form={}',(form))
            dlg_proc_wpr(   self.id_dlg
                        , app.DLG_PROP_SET
                        , prop=form)

        for name, new_ctrl in ctrls.items():
            pass;              #log('name, new_ctrl={}',(name, new_ctrl))
                
            cfg_ctrl= self.ctrls[name]
            cfg_ctrl.update(new_ctrl)
            new_ctrl['type']    = cfg_ctrl['type']
            c_prop  =self._prepare_c_pr(name, new_ctrl, {'ctrls':ctrls})
            pass;              #log('c_prop={}',(c_prop)) if new_ctrl['type']=='listview' else None
            dlg_proc_wpr(   self.id_dlg
                        , app.DLG_CTL_PROP_SET
                        , name=name
                        , prop=c_prop
                        )
        
        if focused in self.ctrls:
            self.form['focused']    = focused
            app.dlg_proc(   self.id_dlg
                        , app.DLG_CTL_FOCUS
                        , name=focused)
       #def _update
    
    def _gen_repro_code(self, rerpo_fn):
        # Repro-code
        l       = '\n'
        cattrs  = [  ('type', 'name', 'tag', 'act')
                    ,('x', 'y', 'w', 'h', 'w_min', 'h_min', 'w_max', 'h_max', 'cap', 'hint', 'p')
                    ,('en', 'vis', 'focused', 'tab_stop', 'tab_order'
                     ,'props', 'ex0', 'ex1', 'ex2', 'ex3', 'ex4', 'ex5', 'ex6', 'ex7', 'ex8', 'ex9'
                     ,'sp_l', 'sp_r', 'sp_t', 'sp_b', 'sp_a', 'a_l', 'a_r', 'a_t', 'a_b', 'align')
                    ,('val', 'items', 'columns')
                    ,('tp', 't', 'b', 'l', 'r', 'tid', 'a')
                    ]
        fattrs  = [  ('x', 'y', 'w', 'h', 'cap', 'tag')
                    ,('resize', 'w_min', 'w_max', 'h_min', 'h_max')
                    ,('vis', 'keypreview')
                    ]
        def out_attrs(pr, attrs, out=''):
            pr          = pr.copy()
            out         += '{'
            afix        = ''
            for ats in attrs:
                apr     =   {k:pr.pop(k) for k in ats if k in pr}
                if apr:
                    out += afix + ', '.join(repr(k) + ':' + repr(apr[k]) for k in ats if k in apr)
                    afix= '\n,'
            apr =           {k:pr.pop(k) for k in pr.copy() if k[0:3]!='on_'}
            if apr:
                out     += afix + repr(apr).strip('{}') 
            for k in pr:
#               pr  = {k:(lambda idd,idc,data:print(repr(k))) for k in pr}
                out     += afix + f('"{}":(lambda idd,idc,data:print("{}"))', k, k)
            out         += '}'
            return out
        srp     =    ''
        srp    +=    'idd=dlg_proc(0, DLG_CREATE)'
        pass;                  #log('app.dlg_proc(self.id_dlg, app.DLG_CTL_COUNT)={}',(app.dlg_proc(self.id_dlg, app.DLG_CTL_COUNT)))
        for idC in range(app.dlg_proc(self.id_dlg, app.DLG_CTL_COUNT)):
            prC = dlg_proc_wpr(self.id_dlg, app.DLG_CTL_PROP_GET, index=idC)
            if ''==prC.get('hint', ''):                 prC.pop('hint', None)
            if ''==prC.get('tag', ''):                  prC.pop('tag', None)
            if ''==prC.get('cap', ''):                  prC.pop('cap', None)
            if ''==prC.get('items', None):              prC.pop('items')
            if prC.get('tab_stop', None):               prC.pop('tab_stop')
            if prC['type'] in ('label',):               prC.pop('tab_stop', None)
            if prC['type'] in ('bevel',):               (prC.pop('tab_stop', None)
                                                        ,prC.pop('tab_order', None))
            if prC['type'] not in ('listview'
                                  ,'checklistview'):    prC.pop('columns', None)
            if prC['type'] in ('label'
                              ,'bevel'
                              ,'button'):               prC.pop('val', None)
            if prC['type'] in ('button'):               prC.pop('act', None)
            if not prC.get('act', False):               prC.pop('act', None)
            if not prC.get('focused', False):           prC.pop('focused', None)
            if prC.get('vis', True):                    prC.pop('vis', None)
            if prC.get('en', True):                     prC.pop('en', None)
            name = prC['name']
            c_pr = self.ctrls[name].copy()
            c_pr = self._prepare_it_vl(c_pr, c_pr)
            prC.update({k:v for k,v in c_pr.items() if k not in ('callback','call')})
            srp+=l+f('idc=dlg_proc(idd, DLG_CTL_ADD,"{}")', prC['type'])
            prC.pop('type', None)
            srp+=l+f('dlg_proc(idd, DLG_CTL_PROP_SET, index=idc, prop={})', out_attrs(prC, cattrs))
        prD     = dlg_proc_wpr(self.id_dlg, app.DLG_PROP_GET)
        prD.update(self.form)
        srp    +=l+f('dlg_proc(idd, DLG_PROP_SET, prop={})', out_attrs(prD, fattrs))
        srp    +=l+f('dlg_proc(idd, DLG_CTL_FOCUS, name="{}")', prD['focused'])
        srp    +=l+  'dlg_proc(idd, DLG_SHOW_MODAL)'
        srp    +=l+  'dlg_proc(idd, DLG_FREE)'
        open(rerpo_fn, 'w', encoding='UTF-8').write(srp)
        pass;                   log(r'exec(open(r"{}", encoding="UTF-8").read())', rerpo_fn)
       #def _gen_repro_code
    
    @staticmethod
    def _form_acts(act, form=None, id_dlg=None, key4store=None):
        """ Save/Restore pos of form """
        pass;                  #log('act, form, id_dlg={}',(act, form, id_dlg))
        CFG_JSON= app.app_path(app.APP_DIR_SETTINGS)+os.sep+'forms data.json'
        stores  = json.loads(open(CFG_JSON).read(), object_pairs_hook=odict) \
                    if os.path.exists(CFG_JSON) and os.path.getsize(CFG_JSON) != 0 else \
                  odict()
        
        def get_form_key(prs):
            fm_cap  = prs['cap']
            fm_cap  = fm_cap[:fm_cap.rindex(' (')]      if ' (' in fm_cap else fm_cap
            fm_cap  = fm_cap[:fm_cap.rindex(' [')]      if ' [' in fm_cap else fm_cap
            return fm_cap #if ' (' not in fm_cap else fm_cap[:fm_cap.rindex(' (')]
        
        if False:pass
        
        if act=='move' and form:
            fm_key  = key4store if key4store else get_form_key(form)
            pass;              #log('fm_key={}',(fm_key))
            if fm_key not in stores:    return form
            prev    = stores[fm_key]
            if not form.get('resize', False):
                prev.pop('w', None)
                prev.pop('h', None)
            form.update(prev)
            pass;              #log('!upd form={}',(form))
            return form
        
        if act=='save' and id_dlg:
            dlg_pr  = dlg_proc_wpr(id_dlg, app.DLG_PROP_GET)
            fm_key  = key4store if key4store else get_form_key(dlg_pr)
            pass;              #log('{}={}', fm_key,{k:v for k,v in dlg_pr.items() if k in ('x','y','w','h')})
            stores[fm_key]  = {k:v for k,v in dlg_pr.items() if k in ('x','y','w','h')}
            open(CFG_JSON, 'w').write(json.dumps(stores, indent=4))
       #def _form_acts
    
   #class BaseDlgAgent

ALI_CL  = app.ALIGN_CLIENT
ALI_LF  = app.ALIGN_LEFT
ALI_RT  = app.ALIGN_RIGHT
ALI_TP  = app.ALIGN_TOP
ALI_BT  = app.ALIGN_BOTTOM
class DlgAgent(BaseDlgAgent):
    """ 
    Helper to use dlg_proc(). See wiki.freepascal.org/CudaText_API#dlg_proc

    Main base features :
    - All controls are created once then some of them can be changed via callbacks (attribute 'call').
    - The helper stores config version of form's and control's attributes.
      So the helper can return two versions of attributes: configured and actual (live).
    - Helper marshals complex data ('items' for type=list/combo/..., 'val' for type=memo)
    - Helper can to save/restore form position and sizes to/from file "settings/forms data.json".

    Main extra features:
    - Helper handles attributes names of controls
        Helper adds short synonyms: 
            cid is name
            tp  is type
            fid is focused
        Helper adds new attributes to simplify config: 
            l,r,t,b,tid,a,aid 
        are translated to live 
            x,y,w,h,a_*,sp*
    - Helper allows to aligns texts from linear controls (by tid attribute)

    Terms
        conf-attr - configured attribute (key and value passed to agent from plugin)
        live-attr - actual attribute     (key and value taked from dlg_proc)
    
    Rules
    1. All conrols have conf-attr 'cid'|'name'. It must be unique.
    2. All conrols have conf-attr 'tp'|'type'. 
        Value of 'tp'|'type' can be any API values or shortened variants from REDUCTIONS.
    3. Conrol position can be set 
        - directly by x,y,w,h
        - computed by enough subset of l,r,w,t,b,h (x=l, y=t, l+w=r, t+h=b)
        - computed by tid (refer to cid|name of control, from which to get t and add a bit to align texts)
    4. Controls values (attribute 'val') can be passed to agent as separate collection:
        - parameter 'vals' to call constructor
        - key 'vals' in dict to return from callback
    5. Need focused control cid can be passed to agent
        - parameter 'fid' to call constructor
        - key 'fid' in dict to return from callback
        - key 'fid' into parameter 'form'
       Live focused control cid can be asked as agent.fattr('fid')
    6. Anchors 'aid' and 'a' are used to plan control's position on form resize. 
        Note: agent's anchors cannot set initial control position (except to center control).
        'aid' points to a tagret: form (empty value, default) or cid.
        'a' has string value which can include character:
            - |     to center           the control by horz/vert with the target
            t T     to anchor top    of the control to top/bottom of the target
            b B     to anchor bottom of the control to top/bottom of the target
            l L     to anchor left   of the control to left/right of the target
            r R     to anchor right  of the control to left/right of the target
        Padding with target (live-attrs 'sp_*') are auto-calculated by initial positions of both elements.
        Examples
            a='|lR'     
    7. Format of callback for a control
        def callname(name_of_event_control, agent):
            'smth'
            return None                         #   To hide form
            return {}                           #   To keep controls and form state
            return {'ctrls':[(nm,{...})]        #   To change controls with pointed names
                   ,'vals':{nm:...}             #   To change controls 'val' with pointed names
                   ,'form':{...}                #   To change form attributes
                   ,'focused':name_to_focus     #   To change focus
                   ,'fid':cid_to_focus          #   To change focus
                   }                            #   Any key ('ctrls','vals','form','fid','focused') can be ommited.
        Callback cannot add/del controls or change cid,type,a,aid
        Callback have to conside the form as it has initial size - agent will recalculate to actual state.
        Useful methods of agent
        - agent.cattr(cid, '??', live=T)        To get a control actual/configured attribute
        - agent.cattrs(cid, ['??'], live=T)     To get dict of a control listed actual/configured attributes
        - agent.cval(cid, live=T)               To get actual/configured 'val' attribute (short of agent.cattr(cid, 'val'))
        - agent.cvals([cid], live=T)            To get dict of actual/configured 'val' the listed attributes
        - agent.fattr('??', live=T)             To get actual/configured form attribute
        - agent.fattrs(live=T, ['??']=None)     To get actual/configured all/listed form attributes
    8. Match of different conf-attr and live-attr keys: 
        cid     name
        tp      type
        l       x 
        r       x+w
        t       y
        b       y+h
        tid     y
        aid     a_*[0]
        a       a_*[1], sp*
        fid     focused
        call    callback
    9. List of same conf-attr and live-attr keys
        w h 
        cap hint 
        props 
        color 
        font_name font_size font_color font
        val
        act
        en vis 
        tag 
        tab_stop tab_order
    10. Tricks
        - 'tid' vertical aligns not a pair of controls themself but text in its. 
          For this it uses platform specific data. The data can be configured by call dlg_valign_consts()
        - If cap of a label starts with '>' then the character is cut and 'prop' set to '1' to right alignment
        - Attributes
            def_bt spinedit url grid at_botttom brdW_fillC_fontC_brdC ro_mono_brd
          are used as temporary and readable version of 'prop'. 
          They can be use to configure as others but key to ask is 'prop'.
        - Attribute 
            sto     tab_stop
          is used as temporary version of 'tab_stop'. 
    
    Example. Horz-resized modal dialog with two buttons. First button is stretched, second is pinned to right.
    DlgAgent(
        ctrls=[('b1', dict(type='button',             cap='Click', x=0, y= 0, w=100, a='lR'
                call=lambda name,ag:{'ctrls':[('b1',{'cap':'OK',             'w':70})]}
               ))
              ,('b2', dict(type='button', cap='Close', x=0, y=30, w=100,             a='LR'
                call=lambda name,ag:None)
               )]
    ,   form=dict(cap='Two buttons', x=0, y=0, w=100, h=60)
    ,   focused='b1'
    ).show()
    """

    def __init__(self, ctrls, vals=None, form=None, fid=None, focused=None, options=None):
        options = options or {}
        super().__init__([], options={k:v for k,v in options.items() if k not in ['gen_repro_to_file']})
        # Inherited Fields
#       self.opts
#       self.id_dlg
#       self.ctrls
#       self.form
        self._setup(ctrls, vals, form, focused or fid)

        rtf     = options.get('gen_repro_to_file', False)
        rtf_fn  = rtf if isinstance(rtf, str) else 'repro_dlg_proc.py'
        if rtf:   self._gen_repro_code(tempfile.gettempdir()+os.sep+rtf_fn)
       #def __init__
        
    def cval(self, cid, live=True, defv=None):
        """ Return the control val property """
        return self.cattr(cid, 'val', live=live, defv=defv)
    def cvals(self, cids, live=True):
        """ Return the controls val property """
        return {cid:self.cattr(cid, 'val', live=live) for cid in cids}
    
    def _setup(self, ctrls, vals=None, form=None, fid=None):
        """ Arrange and fill all: controls static/dinamic attrs, form attrs, focus.
            Params
                ctrls   [{}]
                vals    {cid:v}
                form    {}
                fid     str
        """
        #NOTE: DlgAg init
        self.ctrls  = odict(ctrls)      if isinstance(ctrls, list) else ctrls
        self.form   = form.copy()       if form     else {}

        if 'checks'=='checks':
            no_tids = {cnt['tid'] for cnt in ctrls if 'tid' in cnt and cnt['tid'] not in self.ctrls}
            if no_tids:
                raise Exception(f('No cid for tid: {}', no_tids))
            no_vids = {cid for cid in vals if cid not in self.ctrls} if vals else None
            if no_vids:
                raise Exception(f('No cid for val: {}', no_vids))
#           if fid and fid not in self.ctrls:
#               raise Exception(f('No fid: {}', fid))
        
        if vals:
            for cid, val in vals.items():
                self.ctrls[cid]['val']  = val
        
        # Create controls
        for cid,cfg_ctrl in self.ctrls.items():
            cfg_ctrl.pop('callback', None)
            cfg_ctrl.pop('on_change', None)
#           cid     = cfg_ctrl.get('cid', cfg_ctrl.get('name'))
#           cfg_ctrl['cid']     = cid
#           cfg_ctrl['name']    = cid
            assert 'type' in cfg_ctrl or 'tp'  in cfg_ctrl
            tp      = cfg_ctrl.get('tp',  cfg_ctrl.get('type'))
            cfg_ctrl['tp']      = tp
            cfg_ctrl['type']    = REDUCTIONS.get(tp, tp)
            ind_c   = dlg_proc_wpr(self.id_dlg
                        , DLG_CTL_ADD_SET
                        , name=cfg_ctrl['type']
                        , prop=self._prepare_c_pr(cid, cfg_ctrl))
            pass;              #cfg_ctrl['_idc']    = ind_c         # While API bug: name isnot work if contorl is in panel
           #for cnt

        # Resize callback
        if 'on_resize' in self.form:
            user_callbk = self.form['on_resize']
            def da_rs_callbk(id_dlg, id_ctl=-1, data=''):
                upds    = user_callbk(self)
                if upds:
                    self._update_on_call(upds)
            self.form['on_resize'] = da_rs_callbk
        
        # Resize on start
        fpr     = self.form
        w0      = fpr['w']
        h0      = fpr['h']
        if fpr.get('resize', False):
            self._prepare_anchors()                                 # a,aid -> a_*,sp_*
            fpr['w_min']    = fpr.get('w_min', fpr['w'])
            fpr['h_min']    = fpr.get('h_min', fpr['h'])
        pass;                  #log('fpr is self.form={}',(fpr is self.form))
        fpr     = BaseDlgAgent._form_acts('move', form=fpr)         # Move and (maybe) resize
        pass;                  #log('fpr is self.form={}',(fpr is self.form))
        if 'on_resize' in self.form and \
           (fpr['w'] != w0 or \
            fpr['h'] != h0):
            pass;              #log('fpr[w],fpr[h],w0,h0={}',(fpr['w'], fpr['h'], w0,h0))
            self.form['on_resize'](self)

        fpr['topmost']      = app.app_api_version()<'1.0.270' or app.app_proc(app.PROC_WINDOW_TOPMOST_GET, '')
#       fpr['topmost']      = True
        dlg_proc_wpr(           self.id_dlg
                            , app.DLG_PROP_SET
                            , prop=fpr)                         # Upd live-attrs
        
        fid     = fid   if fid in self.ctrls else     self.form.get('fid')
        if fid in self.ctrls:
            self.form['fid']    = fid                           # Upd conf-attrs
            self.form['focused']= fid
            app.dlg_proc(   self.id_dlg
                        , app.DLG_CTL_FOCUS
                        , name=fid)                             # Upd live-attrs

        pass;                   self._gen_repro_code(tempfile.gettempdir()+os.sep+'repro_dlg_proc.py')     if F else None
       #def _setup

    EXTRA_C_ATTRS   = ['tp','l','t','r','b','tid','a','aid']
    def _prepare_c_pr(self, cid, cfg_ctrl, opts={}):
        pass;                  #log('cid, cfg_ctrl={}',(cid, cfg_ctrl))
#       cid     = cfg_ctrl['cid']
        tp      = cfg_ctrl['type']  # reduced
        DlgAgent._preprocessor(cfg_ctrl, tp)                                # sto -> tab_stop, ... -> props
        c_pr    = super()._prepare_c_pr(cid
                    , {k:v for k,v in cfg_ctrl.items() if k not in DlgAgent.EXTRA_C_ATTRS}
                    , {'prepare call':False})                               # items -> items, val -> val
        c_pr.update(self._prep_pos_attrs(cfg_ctrl, cid, opts.get('ctrls')))                    # l,r,t,b,tid -> x,y,w,h
        pass;                  #log('c_pr={}',(c_pr))

        def get_proxy_cb(u_callbk, event):
            def da_c_callbk(idd, idc, data):
                pass;          #log('ev,idc,cid,data={}',(event,idc,cid,data))
                if tp in ('listview',) and type(data) in (tuple, list):
                    if not data[1]: return  # Skip event "selection loss"
                    # Crutch for Linux! Wait fix in core
                    event_val   = app.dlg_proc(idd, app.DLG_CTL_PROP_GET, index=idc)['val']
                    if event_val!=data[0]:
                        app.dlg_proc(idd, app.DLG_CTL_PROP_SET, index=idc, prop={'val':data[0]})
                pass;          #log('?? u_callbk',())
                upds    = u_callbk(cid, self, data)
                pass;          #log('ok u_callbk upds={}',(upds))
                if upds is None:                                        # To hide/close
                    app.dlg_proc(self.id_dlg, app.DLG_HIDE)
                    return
                elif not upds:                                          # No changes
                    return
                pass;          #log('upds={}',(upds))
                pass;          #log('?? _update_on_call',())
                self._update_on_call(upds)
                pass;          #log('ok _update_on_call',())
               #def da_c_callbk
            return da_c_callbk
           #def get_proxy_cb

        for on_key in [k for k in cfg_ctrl if (k=='call' or k.startswith('on_')) and callable(cfg_ctrl.get(k))]:
            user_callbk     = cfg_ctrl[on_key]
            on_what         =  on_key \
                                if on_key.startswith('on_') else \
                              'on_select' \
                                if tp in ('listview', 'treeview') else \
                              'on_click' \
                                if tp in ('linklabel') else \
                              'on_change'
            pass;              #log('cid,tp,on_what={}',(cid,tp,on_what))
            if tp!='button':
                c_pr['act'] = True
            c_pr[on_what]   = get_proxy_cb(user_callbk, on_what)
#           pass;               log('?? on_what={}',on_what)
#           pass;               log('  c_pr[on_what]()={}',(c_pr[on_what](0,0,'')))
#           pass;               log('ok on_what={}',on_what)
           #for on_key

        if callable(cfg_ctrl.get('menu')):
            c_pr['on_menu'] = lambda idd, idc, data: cfg_ctrl['menu'](cid, self)
        
        pass;                  #log('c_pr={}',(c_pr)) if c_pr['type']=='checkbutton' else 0
        return c_pr
       #def _prepare_c_pr

    def show_menu(self, cid, mn_content, where='+h'):
        """ cid             Control to show menu near it
            mn_content      [{cap:'', tag:'', en:T, ch:F, rd:F, cmd:(lambda ag, tag:''), sub:[]}]
            where           Menu position 
                                '+h' - under the control
                                '+w' - righter the control
        """
        pr      = self.cattrs(cid, ('x','y','w','h'))
        x, y    = pr['x']+(pr['w'] if '+w' in where else 0) \
                , pr['y']+(pr['h'] if '+h' in where else 0)
        pass;                  #log('x, y={}',(x, y))
        prXY    = _os_scale('scale', {'x':x, 'y':y})
        x, y    = prXY['x'], prXY['y']
        pass;                  #log('x, y={}',(x, y))
        x, y    = app.dlg_proc(self.id_dlg, app.DLG_COORD_LOCAL_TO_SCREEN, index=x, index2=y)
        pass;                  #log('x, y={}',(x, y))
        
        def da_mn_callbk(it):
            pass;              #log('it[tag]={}',(it['tag']))
            u_callbk= it['cmd']
            upds    = u_callbk(self, it.get('tag', ''))
            if upds is None:                                        # To hide/close
                app.dlg_proc(self.id_dlg, app.DLG_HIDE)
                return
            if not upds:    return  # No changes
            self._update_on_call(upds)
           #def da_mn_callbk
            
        def fill_mn(mid_prn, its):
            for it in its:
                if it['cap']=='-':
                    app.menu_proc(  mid_prn, app.MENU_ADD, caption='-');
                    continue
                mid =(app.menu_proc(mid_prn, app.MENU_ADD, caption=it['cap'], command= lambda _it=it:da_mn_callbk(_it))     # _it=it solves lambda closure problem
                        if 'cmd' in it else 
                      app.menu_proc(mid_prn, app.MENU_ADD, caption=it['cap'])
                     )
                if 'key' in it and it['key']:
                    app.menu_proc(      mid, app.MENU_SET_HOTKEY            , command=     it['key'])
                if 'en' in it:
                    app.menu_proc(      mid, app.MENU_SET_ENABLED           , command=bool(it['en']))
                if 'ch' in it:
                    app.menu_proc(      mid, app.MENU_SET_CHECKED           , command=bool(it['ch']))
                if 'rd' in it:
                    app.menu_proc(      mid, app.MENU_SET_RADIOITEM         , command=bool(it['rd']))
                if 'sub' in it:
                    fill_mn(mid, it['sub'])
           #def fill_mn
        
        mid_top = app.menu_proc(    0,       app.MENU_CREATE)
        fill_mn(mid_top, mn_content)
        app.menu_proc(              mid_top, app.MENU_SHOW                  , command=f('{},{}', x, y))
       #def show_menu

    def _update_on_call(self, upds):
        if isinstance(upds, tuple) or isinstance(upds, list) :          # Allow to use list of upd data
            upds    = deep_upd(upds)
            pass;      #log('upds={}',(upds))
        ctrls_u = odict(upds.get('ctrls',  []))
        pass;          #log('ctrls_u={}',(ctrls_u))
        vals    = upds.get('vals',   {})
        form    = upds.get('form',   {})
        fid     = upds.get('fid'  , upds.get('focused', form.get('fid', form.get('focused'))))
        if False:pass
        elif vals and not ctrls_u:
            ctrls_u     = { cid_    :  {'val':val} for cid_, val in vals.items()}
        elif vals and     ctrls_u:
            for cid_, val in vals.items():
                if cid_ not in ctrls_u:
                    ctrls_u[cid_]   =  {'val':val}
                else:
                    ctrls_u[cid_]['val']    = val
        for cid_, c in ctrls_u.items():
            pass;      #log('cid_, c={}',(cid_, c))
            c.pop('callback', None)
            c.pop('on_change', None)
            c.pop('call', None)
            c['type']   = self.ctrls[cid_]['type']
        super(DlgAgent,self).update( ctrls  =ctrls_u
                                    ,form   =form
                                    ,focused=fid)
        if fid in self.ctrls:
            self.form['fid']    = fid
       #def _update_on_call
       
    def _prepare_anchors(self):
        """ Translate attrs 'a' 'aid' to 'a_*','sp_*'
            Values for 'a' are str-mask with signs
                'l' 'L'    fixed distanse ctrl-left     to trg-left  or trg-right
                't' 'T'    fixed distanse ctrl-top      to trg-top   or trg-bottom
                'r' 'R'    fixed distanse ctrl-right    to trg-left  or trg-right
                'b' 'B'    fixed distanse ctrl-bottom   to trg-top   or trg-bottom
        """
        fm_w    = self.form['w']
        fm_h    = self.form['h']
        for cid,cnt in self.ctrls.items():
            anc     = cnt.get('a'  , '')
            if not anc: continue
            aid     = cnt.get('aid', cnt.get('p', ''))    # '' anchor to form
            trg_w,  \
            trg_h   = fm_w, fm_h
            if aid in self.ctrls:
                prTrg   = dlg_proc_wpr(self.id_dlg, app.DLG_CTL_PROP_GET, name=aid)
                trg_w,  \
                trg_h   = prTrg['w'], prTrg['h']
            prOld   = dlg_proc_wpr(self.id_dlg, app.DLG_CTL_PROP_GET, name=cid)
            pass;              #prOld   = prOld if prOld else \
                     #dlg_proc_wpr(self.id_dlg, app.DLG_CTL_PROP_GET, index=self.ctrls[cid]['_idc'])    # While API bug: name isnot work if contorl is in panel
            pass;               logb=cid in ('tolx', 'tofi')
            pass;              #nat_prOld=app.dlg_proc(self.id_dlg, app.DLG_CTL_PROP_GET, name=cid)
            pass;              #log('cid,nat-prOld={}',(cid,{k:v for k,v in nat_prOld.items() if k in ('x','y','w','h','_ready_h')})) if logb else 0
            pass;              #log('cid,    prOld={}',(cid,{k:v for k,v in     prOld.items() if k in ('x','y','w','h','_ready_h')})) if logb else 0
            pass;              #log('cid,anc,trg_w,trg_h,prOld={}',(cid,anc,trg_w,trg_h, {k:v for k,v in prOld.items() if k in ('x','y','w','h')})) \
                               #    if logb else 0
            prAnc   = {}
            if '-' in anc:
                # Center by horz
                prAnc.update(dict( a_l=(aid, '-')
                                  ,a_r=(aid, '-')))
            if 'L' in anc and 'R' in anc:
                # Both left/right to form right
                pass;          #log('+L +R') if logb else 0
                prAnc.update(dict( a_l=None                                             # (aid, ']'), sp_l=trg_w-prOld['x']
                                  ,a_r=(aid, ']'), sp_r=trg_w-prOld['x']-prOld['w']))
            if 'L' in anc and 'R' not in anc:
                # Left to form right
                pass;          #log('+L -R') if logb else 0
                prAnc.update(dict( a_l=(aid, '['), sp_l=trg_w-prOld['x']
                                  ,a_r=None))
            if 'l' in anc and 'R' in anc:
                # Left to form left. Right to form right.
                pass;          #log('+l +R') if logb else 0
                prAnc.update(dict( a_l=(aid, '['), sp_l=      prOld['x']
                                  ,a_r=(aid, ']'), sp_r=trg_w-prOld['x']-prOld['w']))
            if '|' in anc:
                # Center by vert
                prAnc.update(dict( a_t=(aid, '-')
                                  ,a_b=(aid, '-')))
            if 'T' in anc and 'B' in anc:
                # Both top/bottom to form bottom
                pass;          #log('+T +B') if logb else 0
                prAnc.update(dict( a_t=None      #, sp_t=trg_h-prOld['y']                # a_t=(aid, ']') - API bug
                                  ,a_b=(aid, ']'), sp_b=trg_h-prOld['y']-prOld['h']))
            elif 'T' in anc and 'B' not in anc:
                # Top to form bottom
                pass;          #log('+T -B') if logb else 0
                prAnc.update(dict( a_t=(aid, ']'), sp_t=trg_h-prOld['y']                # a_t=(aid, ']') - API bug
                                  ,a_b=None))
            if 't' in anc and 'B' in anc:
                # Top to form top. Bottom to form bottom.
                pass;          #log('+t +B') if logb else 0
                prAnc.update(dict( a_t=(aid, '['), sp_t=      prOld['y']
                                  ,a_b=(aid, ']'), sp_b=trg_h-prOld['y']-prOld['h']))
            if prAnc:
                pass;          #log('aid,prAnc={}',(cid, prAnc)) if logb else 0
                cnt.update(prAnc)
                dlg_proc_wpr(self.id_dlg, app.DLG_CTL_PROP_SET, name=cid, prop=prAnc)
#               pass;           pr_   = dlg_proc_wpr(self.id_dlg, app.DLG_CTL_PROP_GET, name=cid)
#               pass;           log('cid,pr_={}',(cid, {k:v for k,v in pr_.items() if k in ('h','y', 'sp_t', 'sp_b', 'a_t', 'a_b')}))
       #def _prepare_anchors

    def _prep_pos_attrs(self, cnt, cid, ctrls4t=None):
        # Position:
        #   t[op] or tid, l[eft] required
        #   w[idth]  >>> r[ight ]=l+w
        #   h[eight] >>> b[ottom]=t+h
        #   b dont need for buttons, edit, labels
#       if not [k for k in cnt.keys() if k in ('l','t','r','b','tid')]:
#           return {k:v for (k,v) in cnt.items() if k in ('x','y','w','h')}

        pass;                  #log('cid, cnt={}',(cid, cnt))
        prP     =  {}
        
        if 'h' not in cnt \
        and cnt['type'] in (  'button', 'checkbutton'
                            , 'label'
                            , 'combo', 'combo_ro'
                            , 'edit', 'spinedit'
                            , 'check', 'radio'
                            , 'filter_listbox', 'filter_listview'
                            ):
            # OS specific control height
            cnt['h']    = get_gui_height(cnt['type'])
            prP['_ready_h'] = True
#           cnt['h']    = app.app_proc(app.PROC_GET_GUI_HEIGHT, cnt['type'])

        if 'l' in cnt:
            prP['x']    = cnt['l']
        if 'r' in cnt and 'x' in prP:
            prP['w']    = cnt['r'] - prP['x']
        if 'w' in cnt:
            prP['w']    = cnt['w']

        if 't' in cnt:
            prP['y']    = cnt['t']
#       t       = cnt.get('t', 0)   if 't' in cnt else  self.cattr(cid, 't', live=False)
        elif 'tid' in cnt:
            ctrls4t = ctrls4t if ctrls4t else self.ctrls
            assert cnt['tid'] in ctrls4t
            # cid for horz-align text
            bs_cnt4t= ctrls4t[   cnt['tid']]
            bs_cnt  = self.ctrls[cnt['tid']]
            bs_tp   = bs_cnt['tp']
            bs_tp   = REDUCTIONS.get(bs_tp, bs_tp)
            tp      = self.ctrls[cid]['tp']
            tp      = REDUCTIONS.get(tp, tp)
            pass;              #log('tp, bs_tp, fit, bs_cnt={}',(tp, bs_tp, fit_top_by_env(tp, bs_tp), bs_cnt))
            t       = bs_cnt4t['t'] + fit_top_by_env(tp, bs_tp)
            prP['y']    = t
        if 'b' in cnt and 'y' in prP:
            prP['h']    = cnt['b'] - prP['y']
        if 'h' in cnt:
            prP['h']    = cnt['h']
            
#       b       = cnt.get('b', t+cnt.get('h', 0)) 

#       l       = cnt['l']          if 'l' in cnt else  self.cattr(cid, 'l', live=False)
#       r       = cnt.get('r', l+cnt.get('w', 0)) 
#       prP     =  dict(x=l, y=t, w=r-l)
#       prP.update(dict(h=cnt.get('h')))    if 0!=cnt.get('h', 0) else 0 
        pass;                  #log('cid, prP={}',(cid, prP))
        return prP
       #def _prep_pos_attrs

    @staticmethod
    def _preprocessor(cnt, tp):
        if 'ali' in cnt:
            cnt['align'] = cnt.pop('ali')
        if 'sp_lr' in cnt:
            cnt['sp_l'] = cnt['sp_r']               = cnt.pop('sp_lr')
        if 'sp_lrt' in cnt:
            cnt['sp_l'] = cnt['sp_r'] = cnt['sp_t'] = cnt.pop('sp_lrt')
        if 'sp_lrb' in cnt:
            cnt['sp_l'] = cnt['sp_r'] = cnt['sp_b'] = cnt.pop('sp_lrb')

        if 'sto' in cnt:
            cnt['tab_stop'] = cnt.pop('sto')
        if 'tor' in cnt:
            cnt['tab_order'] = cnt.pop('tor')
            
        if 'props' in cnt:
            pass
        elif tp=='label' and 'cap' in cnt and cnt['cap'][0]=='>':
            #   cap='>smth' --> cap='smth', props='1' (r-align)
            cnt['cap']  = cnt['cap'][1:]
            cnt['props']= '1'
        elif tp=='label' and    cnt.get('ralign'):
            cnt['props']=       cnt.pop('ralign')
        elif tp=='button' and cnt.get('def_bt') in ('1', True):
            cnt['props']= '1'
        elif tp=='spinedit' and cnt.get('min_max_inc'):
            cnt['props']=       cnt.pop('min_max_inc')
        elif tp=='linklabel' and    cnt.get('url'):
            cnt['props']=           cnt.pop('url')
        elif tp=='listview' and cnt.get('grid'):
            cnt['props']=       cnt.pop('grid')
        elif tp=='tabs' and     cnt.get('at_botttom'):
            cnt['props']=       cnt.pop('at_botttom')
        elif tp=='colorpanel' and   cnt.get('brdW_fillC_fontC_brdC'):
            cnt['props']=           cnt.pop('brdW_fillC_fontC_brdC')
        elif tp in ('edit', 'memo') and cnt.get('ro_mono_brd'):
            cnt['props']=               cnt.pop('ro_mono_brd')

        if 'props' in cnt and app.app_api_version()>='1.0.224':
            # Convert props to ex0..ex9
            #   See 'Prop "ex"' at wiki.freepascal.org/CudaText_API
            lsPr = cnt.pop('props').split(',')
            if False:pass
            elif tp=='button':
                cnt['ex0']  = '1'==lsPr[0]  #bool: default for Enter key
            elif tp in ('edit', 'memo'):
                cnt['ex0']  = '1'==lsPr[0]  #bool: read-only
                cnt['ex1']  = '1'==lsPr[1]  #bool: font is monospaced
                cnt['ex2']  = '1'==lsPr[2]  #bool: show border
            elif tp=='spinedit':
                cnt['ex0']  =  int(lsPr[0]) #int:  min value
                cnt['ex1']  =  int(lsPr[1]) #int:  max value
                cnt['ex2']  =  int(lsPr[2]) #int:  increment
            elif tp=='label':
                cnt['ex0']  = '1'==lsPr[0]  #bool: right aligned
            elif tp=='linklabel':
                cnt['ex0']  = lsPr[0]       #str: URL. Should not have ','. Clicking on http:/mailto: URLs should work, result of clicking on other kinds depends on OS.
            elif tp=='listview':
                cnt['ex0']  = '1'==lsPr[0]  #bool: show grid lines
            elif tp=='tabs':
                cnt['ex0']  = '1'==lsPr[0]  #bool: show tabs at bottom
            elif tp=='colorpanel':
                cnt['ex0']  =  int(lsPr[0]) #int:  border width (from 0)
                cnt['ex1']  =  int(lsPr[1]) #int:  color of fill
                cnt['ex2']  =  int(lsPr[2]) #int:  color of font
                cnt['ex3']  =  int(lsPr[3]) #int:  color of border
            elif tp=='filter_listview':
                cnt['ex0']  = '1'==lsPr[0]  #bool: filter works for all columns
            elif tp=='image':
                cnt['ex0']  = '1'==lsPr[0]  #bool: center picture
                cnt['ex1']  = '1'==lsPr[1]  #bool: stretch picture
                cnt['ex2']  = '1'==lsPr[2]  #bool: allow stretch in
                cnt['ex3']  = '1'==lsPr[3]  #bool: allow stretch out
                cnt['ex4']  = '1'==lsPr[4]  #bool: keep origin x, when big picture clipped
                cnt['ex5']  = '1'==lsPr[5]  #bool: keep origin y, when big picture clipped
            elif tp=='trackbar':
                cnt['ex0']  =  int(lsPr[0]) #int:  orientation (0: horz, 1: vert)
                cnt['ex1']  =  int(lsPr[1]) #int:  min value
                cnt['ex2']  =  int(lsPr[2]) #int:  max value
                cnt['ex3']  =  int(lsPr[3]) #int:  line size
                cnt['ex4']  =  int(lsPr[4]) #int:  page size
                cnt['ex5']  = '1'==lsPr[5]  #bool: reversed
                cnt['ex6']  =  int(lsPr[6]) #int:  tick marks position (0: bottom-right, 1: top-left, 2: both)
                cnt['ex7']  =  int(lsPr[7]) #int:  tick style (0: none, 1: auto, 2: manual)
            elif tp=='progressbar':
                cnt['ex0']  =  int(lsPr[0]) #int:  orientation (0: horz, 1: vert, 2: right-to-left, 3: top-down)
                cnt['ex1']  =  int(lsPr[1]) #int:  min value
                cnt['ex2']  =  int(lsPr[2]) #int:  max value
                cnt['ex3']  = '1'==lsPr[3]  #bool: smooth bar
                cnt['ex4']  =  int(lsPr[4]) #int:  step
                cnt['ex5']  =  int(lsPr[5]) #int:  style (0: normal, 1: marquee)
                cnt['ex6']  = '1'==lsPr[6]  #bool: show text (only for some OSes)
            elif tp=='progressbar_ex':
                cnt['ex0']  =  int(lsPr[0]) #int:  style (0: text only, 1: horz bar, 2: vert bar, 3: pie, 4: needle, 5: half-pie)
                cnt['ex1']  =  int(lsPr[1]) #int:  min value
                cnt['ex2']  =  int(lsPr[2]) #int:  max value
                cnt['ex3']  = '1'==lsPr[3]  #bool: show text
                cnt['ex4']  =  int(lsPr[4]) #int:  color of background
                cnt['ex5']  =  int(lsPr[5]) #int:  color of foreground
                cnt['ex6']  =  int(lsPr[6]) #int:  color of border
            elif tp=='bevel':
                cnt['ex0']  =  int(lsPr[0]) #int:  shape (0: sunken panel, 1: 4 separate lines - use it as border for group of controls, 2: top line, 3: bottom line, 4: left line, 5: right line, 6: no lines, empty space)
            elif tp=='splitter':
                cnt['ex0']  = '1'==lsPr[0]  #bool: beveled style
                cnt['ex1']  = '1'==lsPr[1]  #bool: instant repainting
                cnt['ex2']  = '1'==lsPr[2]  #bool: auto snap to edge
                cnt['ex3']  =  int(lsPr[3]) #int:  min size
       #def _preprocessor

#class DlgAgent


######################################
#NOTE: dlg_valign_consts
######################################
def dlg_valign_consts():
    pass;                      #log('ok')
    rsp     = False
    UP,DN   = '↑↑','↓↓'
    DLG_W,  \
    DLG_H   = 335, 310
    ctrls   = ['check'
              ,'edit'
              ,'button'   
              ,'combo_ro' 
              ,'combo'    
              ,'checkbutton'
              ,'linklabel'
              ,'spinedit'
              ,'radio'
              ]
    ctrls_sp= [('_sp'+str(1+ic),nc) for ic, nc in enumerate(ctrls)]
    fits    = {sp:fit_top_by_env(nc) for sp, nc in ctrls_sp}
    hints   = {sp:nc+': '+str(fits[sp]) for sp, nc in ctrls_sp}

    def save():
        nonlocal rsp
        scam        = app.app_proc(app.PROC_GET_KEYSTATE, '') if app.app_api_version()>='1.0.143' else ''
        if not scam:#aid_m=='save':
            for sp, nc in ctrls_sp:
                fit = fits[sp]
                if fit==fit_top_by_env(nc): continue#for ic, nc
                apx.set_opt('dlg_wrapper_fit_va_for_'+nc, fit)
               #for ic, nc
            fit_top_by_env__clear()
            rsp = True
            return None#break#while
            
        if scam=='c':#aid_m=='c/save': # Report
            rpt = 'env:'+get_desktop_environment()
            rpt+= c13 + c13.join(hints.values())
            aid_r, *_t = dlg_wrapper(_('Report'), 230,310,
                 [dict(cid='rprt',tp='me'    ,t=5   ,l=5 ,h=200 ,w=220)
                 ,dict(           tp='lb'    ,t=215 ,l=5        ,w=220  ,cap=_('Send the report to the address'))
                 ,dict(cid='mail',tp='ed'    ,t=235 ,l=5        ,w=220)
                 ,dict(           tp='lb'    ,t=265 ,l=5        ,w=150  ,cap=_('or post it on'))
                 ,dict(cid='gith',tp='ln-lb' ,t=265 ,l=155      ,w=70   ,cap='GitHub',props='https://github.com/kvichans/cuda_fit_v_alignments/issues')
                 ,dict(cid='-'   ,tp='bt'    ,t=280 ,l=205-80   ,w=80   ,cap=_('Close'))
                 ], dict(rprt=rpt
                        ,mail='kvichans@mail.ru')
                 ,  focus_cid='rprt')
        return {}
       #def save

    def up_dn(ag, cid, sht):
        pass;                  #log('cid,sht={}',(cid,sht))
        sign    = cid[-1]
        sp      = '_sp'+sign
        fits[sp]= fits[sp] + sht
        nonlocal hints
        hints   = {sp:nc+': '+str(fits[sp]) for sp, nc in ctrls_sp}
        return {'ctrls':[(cid ,dict(y=ag.cattr(cid, 'y')+sht ,hint=hints[sp] ))]}
       #def up_dn

    cs      = ctrls
    cnts    = \
            [('lb1' ,dict(tp='lb'    ,t= 10              ,l=  5  ,w=100  ,cap=cs[0]+' ==============='                          ))
            ,('ch1' ,dict(tp='ch'    ,t= 10+fits['_sp1'] ,l=115  ,w=100  ,cap='=================',hint=hints['_sp1']     ,val=F))
            ,('up1' ,dict(tp='bt'    ,t= 10-3            ,l=230  ,w=50   ,cap=UP ,call=lambda cid,ag,d: up_dn(ag,'ch1',-1) ))
            ,('dn1' ,dict(tp='bt'    ,t= 10-3            ,l=280  ,w=50   ,cap=DN ,call=lambda cid,ag,d: up_dn(ag,'ch1', 1) ))
                
            ,('lb2' ,dict(tp='lb'    ,t= 40              ,l=  5  ,w=100  ,cap=cs[1]+' ==============='                          ))
            ,('ed2' ,dict(tp='ed'    ,t= 40+fits['_sp2'] ,l=115  ,w=100                          ,hint=hints['_sp2']     ,val='================='))
            ,('up2' ,dict(tp='bt'    ,t= 40-3            ,l=230  ,w=50   ,cap=UP ,call=lambda cid,ag,d: up_dn(ag,'ed2',-1) ))
            ,('dn2' ,dict(tp='bt'    ,t= 40-3            ,l=280  ,w=50   ,cap=DN ,call=lambda cid,ag,d: up_dn(ag,'ed2', 1) ))
                
            ,('lb3' ,dict(tp='lb'    ,t= 70              ,l=  5  ,w=100  ,cap=cs[2]+' ==============='                          ))
            ,('bt3' ,dict(tp='bt'    ,t= 70+fits['_sp3'] ,l=115  ,w=100  ,cap='=================',hint=hints['_sp3']     ))
            ,('up3' ,dict(tp='bt'    ,t= 70-3            ,l=230  ,w=50   ,cap=UP ,call=lambda cid,ag,d: up_dn(ag,'bt3',-1) ))
            ,('dn3' ,dict(tp='bt'    ,t= 70-3            ,l=280  ,w=50   ,cap=DN ,call=lambda cid,ag,d: up_dn(ag,'bt3', 1) ))
                
            ,('lb4' ,dict(tp='lb'    ,t=100              ,l=  5  ,w=100  ,cap=cs[3]+' ==============='                          ))
            ,('cbo4',dict(tp='cb-ro' ,t=100+fits['_sp4'] ,l=115  ,w=100  ,items=['============='],hint=hints['_sp4']     ,val=0))
            ,('up4' ,dict(tp='bt'    ,t=100-3            ,l=230  ,w=50   ,cap=UP ,call=lambda cid,ag,d: up_dn(ag,'cbo4',-1)))
            ,('dn4' ,dict(tp='bt'    ,t=100-3            ,l=280  ,w=50   ,cap=DN ,call=lambda cid,ag,d: up_dn(ag,'cbo4', 1)))
                
            ,('lb5' ,dict(tp='lb'    ,t=130              ,l=  5  ,w=100  ,cap=cs[4]+' ==============='                          ))
            ,('cb5' ,dict(tp='cb'    ,t=130+fits['_sp5'] ,l=115  ,w=100  ,items=['============='],hint=hints['_sp5']     ,val='============='))
            ,('up5' ,dict(tp='bt'    ,t=130-3            ,l=230  ,w=50   ,cap=UP ,call=lambda cid,ag,d: up_dn(ag,'cb5',-1) ))
            ,('dn5' ,dict(tp='bt'    ,t=130-3            ,l=280  ,w=50   ,cap=DN ,call=lambda cid,ag,d: up_dn(ag,'cb5', 1) ))
                
            ,('lb6' ,dict(tp='lb'    ,t=160              ,l=  5  ,w=100  ,cap=cs[5]+' ==============='                          ))
            ,('chb6',dict(tp='ch-bt' ,t=160+fits['_sp6'] ,l=115  ,w=100  ,cap='==========='      ,hint=hints['_sp6']     ,val=0))
            ,('up6' ,dict(tp='bt'    ,t=160-3            ,l=230  ,w=50   ,cap=UP ,call=lambda cid,ag,d: up_dn(ag,'chb6',-1)))
            ,('dn6' ,dict(tp='bt'    ,t=160-3            ,l=280  ,w=50   ,cap=DN ,call=lambda cid,ag,d: up_dn(ag,'chb6', 1)))
                
            ,('lb7', dict(tp='lb'    ,t=190              ,l=  5  ,w=100  ,cap=cs[6]+' ==============='                          ))
            ,('lnb7',dict(tp='ln-lb' ,t=190+fits['_sp7'] ,l=115  ,w=100  ,cap='=================',props=hints['_sp7']    ))
            ,('up7' ,dict(tp='bt'    ,t=190-3            ,l=230  ,w=50   ,cap=UP ,call=lambda cid,ag,d: up_dn(ag,'lnb7',-1)))
            ,('dn7' ,dict(tp='bt'    ,t=190-3            ,l=280  ,w=50   ,cap=DN ,call=lambda cid,ag,d: up_dn(ag,'lnb7', 1)))
                
            ,('lb8' ,dict(tp='lb'    ,t=220              ,l=  5  ,w=100  ,cap=cs[7]+' 4444444444444444'                         ))
            ,('sp8' ,dict(tp='sp-ed' ,t=220+fits['_sp8'] ,l=115  ,w=100  ,props='0,444444444,1'  ,hint=hints['_sp8']     ,val=444444444))
            ,('up8' ,dict(tp='bt'    ,t=220-3            ,l=230  ,w=50   ,cap=UP ,call=lambda cid,ag,d: up_dn(ag,'sp8',-1) ))
            ,('dn8' ,dict(tp='bt'    ,t=220-3            ,l=280  ,w=50   ,cap=DN ,call=lambda cid,ag,d: up_dn(ag,'sp8', 1) ))
                
            ,('lb9' ,dict(tp='lb'    ,t=250              ,l=  5  ,w=100  ,cap=cs[8]+' ==============='                          ))
            ,('rd9' ,dict(tp='rd'    ,t=250+fits['_sp9'] ,l=115  ,w=100  ,cap='=================',hint=hints['_sp9']     ,val=F))
            ,('up9' ,dict(tp='bt'    ,t=250-3            ,l=230  ,w=50   ,cap=UP ,call=lambda cid,ag,d: up_dn(ag,'rd9',-1) ))
            ,('dn9' ,dict(tp='bt'    ,t=250-3            ,l=280  ,w=50   ,cap=DN ,call=lambda cid,ag,d: up_dn(ag,'rd9', 1) ))
                
            ,('save',dict(tp='bt'    ,t=DLG_H-30         ,l=115  ,w=100  ,cap=_('&Save')     ,call=lambda cid,ag,d: save()
                                                                                ,hint=_('Apply the fittings to controls of all dialogs.'
                                                                                        '\rCtrl+Click  - Show data to mail report.')))
            ,('-'   ,dict(tp='bt'    ,t=DLG_H-30         ,l=230  ,w=100  ,cap=_('Cancel')    ,call=(lambda cid,ag,d: None) ))
            ]
    agent   = DlgAgent( form=dict(cap=_('Adjust vertical alignments'), w=DLG_W, h=DLG_H)
                       ,ctrls=cnts ,fid = '-'
                               #,options={'gen_repro_to_file':'repro_dlg_valign_consts.py'}
            ).show()    #NOTE: dlg_valign
    return rsp
   #def dlg_valign_consts

def get_hotkeys_desc(cmd_id, ext_id=None, keys_js=None, def_ans=''):
    """ Read one or two hotkeys for command 
            cmd_id [+ext_id]
        from 
            settings\keys.json
        Return 
            def_ans                     If no  hotkeys for the command
            'Ctrl+Q'            
            'Ctrl+Q * Ctrl+W'           If one hotkey  for the command
            'Ctrl+Q/Ctrl+T'            
            'Ctrl+Q * Ctrl+W/Ctrl+T'    If two hotkeys for the command
    """
    if keys_js is None:
        keys_json   = app.app_path(app.APP_DIR_SETTINGS)+os.sep+'keys.json'
        keys_js     = apx._json_loads(open(keys_json).read()) if os.path.exists(keys_json) else {}

    cmd_id  = f('{},{}', cmd_id, ext_id) if ext_id else cmd_id
    if cmd_id not in keys_js:
        return def_ans
    cmd_keys= keys_js[cmd_id]
    desc    = '/'.join([' * '.join(cmd_keys.get('s1', []))
                       ,' * '.join(cmd_keys.get('s2', []))
                       ]).strip('/')
    return desc
   #def get_hotkeys_desc

######################################
#NOTE: plugins history
######################################
PLING_HISTORY_JSON  = app.app_path(app.APP_DIR_SETTINGS)+os.sep+'plugin history.json'
def get_hist(key_or_path, default=None, module_name='_auto_detect', to_file=PLING_HISTORY_JSON):
    """ Read from "plugin history.json" one value by string key or path (list of keys).
        Parameters
            key_or_path     Key(s) to navigate in json tree
                            Type: str or [str]
            default         Value to return  if no suitable node in json tree
            module_name     Start node to navigate.
                            If it is '_auto_detect' then name of caller module is used.
                            If it is None then it is skipped.
            to_file         Name of file to read. APP_DIR_SETTING will be joined if no full path.
        
        Return              Found value or default
            
        Examples (caller module is 'plg')
        1. If no "plugin history.json"
                get_hist('k')                   returns None
                get_hist(['p', 'k'], 0)         returns 0
        2. If "plugin history.json" contains 
                {"k":1, "plg":{"k":2, "p":{"m":3}, "t":[0,1]}, "q":{"n":4}}
                get_hist('k', 0, None)          returns 1
                get_hist('k', 0)                returns 0
                get_hist('k', 0, 'plg')         returns 2
                get_hist('k', 0, 'oth')         returns 0
                get_hist(['p','m'], 0)          returns 3
                get_hist(['p','t'], [])         returns [0,1]
                get_hist('q', 0, None)          returns {'n':4}
                get_hist(['q','n'], 0, None)    returns 4
    """
    to_file = to_file   if os.sep in to_file else   app.app_path(app.APP_DIR_SETTINGS)+os.sep+to_file
    if not os.path.exists(to_file):
        pass;                  #log('not exists',())
        return default
    data    = None
    try:
        data    = json.loads(open(to_file).read())
    except:
        pass;                   log('not load: {}',sys.exc_info())
        return default
    if module_name=='_auto_detect':
        # commented is for Py3.5+, fails on Py3.4
        # caller_globals  = inspect.stack()[1].frame.f_globals
        caller_globals  = inspect.stack()[1][0].f_globals
        module_name = inspect.getmodulename(caller_globals['__file__']) \
                        if '__file__' in caller_globals else None
    keys    = [key_or_path] if type(key_or_path)==str   else key_or_path
    keys    = keys          if module_name is None      else [module_name]+keys
    parents,\
    key     = keys[:-1], keys[-1]
    for parent in parents:
        data= data.get(parent)
        if type(data)!=dict:
            pass;               log('not dict parent={}',(parent))
            return default
    return data.get(key, default)
   #def get_hist

def set_hist(key_or_path, value, module_name='_auto_detect', kill=False, to_file=PLING_HISTORY_JSON):
    """ Write to "plugin history.json" one value by key or path (list of keys).
        If any of node doesnot exist it will be added.
        Or remove (if kill) one key+value pair (if suitable key exists).
        Parameters
            key_or_path     Key(s) to navigate in json tree
                            Type: str or [str]
            value           Value to set if suitable item in json tree exists
            module_name     Start node to navigate.
                            If it is '_auto_detect' then name of caller module is used.
                            If it is None then it is skipped.
            kill            Need to remove node in tree.
                            if kill==True parm value is ignored
            to_file         Name of file to write. APP_DIR_SETTING will be joined if no full path.
        
        Return              value (param)   if !kill and modification is successful
                            value (killed)  if  kill and modification is successful
                            None            if  kill and no path in tree (no changes)
                            KeyError        if !kill and path has problem
        Return  value
            
        Examples (caller module is 'plg')
        1. If no "plugin history.json"  it will become
            set_hist('k',0,None)        {"k":0}
            set_hist('k',1)             {"plg":{"k":1}}
            set_hist('k',1,'plg')       {"plg":{"k":1}}
            set_hist('k',1,'oth')       {"oth":{"k":1}}
            set_hist('k',[1,2])         {"plg":{"k":[1,2]}}
            set_hist(['p','k'], 1)      {"plg":{"p":{"k":1}}}
        
        2. If "plugin history.json" contains    {"plg":{"k":1, "p":{"m":2}}}
                                                it will contain
            set_hist('k',0,None)                {"plg":{"k":1, "p":{"m":2}},"k":0}
            set_hist('k',0)                     {"plg":{"k":0, "p":{"m":2}}}
            set_hist('k',0,'plg')               {"plg":{"k":0, "p":{"m":2}}}
            set_hist('n',3)                     {"plg":{"k":1, "p":{"m":2}, "n":3}}
            set_hist(['p','m'], 4)              {"plg":{"k":1, "p":{"m":4}}}
            set_hist('p',{'m':4})               {"plg":{"k":1, "p":{"m":4}}}
            set_hist(['p','m','k'], 1)          KeyError (old m is not branch node)

        3. If "plugin history.json" contains    {"plg":{"k":1, "p":{"m":2}}}
                                                it will contain
            set_hist('k',       kill=True)      {"plg":{       "p":{"m":2}}}
            set_hist('p',       kill=True)      {"plg":{"k":1}}
            set_hist(['p','m'], kill=True)      {"plg":{"k":1, "p":{}}}
            set_hist('n',       kill=True)      {"plg":{"k":1, "p":{"m":2}}}    (nothing to kill)
    """
    to_file = to_file   if os.sep in to_file else   app.app_path(app.APP_DIR_SETTINGS)+os.sep+to_file
    body    = json.loads(open(to_file).read(), object_pairs_hook=odict) \
                if os.path.exists(to_file) and os.path.getsize(to_file) != 0 else \
              odict()

    if module_name=='_auto_detect':
        # commented is for Py3.5+, fails of Py3.4
        # caller_globals  = inspect.stack()[1].frame.f_globals
        caller_globals  = inspect.stack()[1][0].f_globals
        module_name = inspect.getmodulename(caller_globals['__file__']) \
                        if '__file__' in caller_globals else None
    keys    = [key_or_path] if type(key_or_path)==str   else key_or_path
    keys    = keys          if module_name is None      else [module_name]+keys
    parents,\
    key     = keys[:-1], keys[-1]
    data    = body
    for parent in parents:
        if kill and parent not in data:
            return None
        data= data.setdefault(parent, odict())
        if type(data)!=odict:
            raise KeyError()
    if kill:
        if key not in data:
            return None
        value       = data.pop(key)
    else:
        data[key]   =  value
    open(to_file, 'w').write(json.dumps(body, indent=2))
    return value
   #def set_hist
######################################
######################################

def get_translation(plug_file):
    ''' Part of i18n.
        Full i18n-cycle:
        1. All GUI-string in code are used in form
            _('')
        2. These string are extracted from code to
            lang/messages.pot
           with run
            python.exe <python-root>\Tools\i18n\pygettext.py -p lang <plugin>.py
        3. Poedit (or same program) create
            <module>\lang\ru_RU\LC_MESSAGES\<module>.po
           from (cmd "Update from POT")
            lang/messages.pot
           It allows to translate all "strings"
           It creates (cmd "Save")
            <module>\lang\ru_RU\LC_MESSAGES\<module>.mo
        4. <module>.mo can be placed also in dir
            CudaText\data\langpy\ru_RU\LC_MESSAGES\<module>.mo
           The dir is used first.
        5. get_translation uses the file to realize
            _('')
    '''
    lng     = app.app_proc(app.PROC_GET_LANG, '')
    plug_dir= os.path.dirname(plug_file)
    plug_mod= os.path.basename(plug_dir)
    lng_dirs= [
                app.app_path(app.APP_DIR_DATA)  +os.sep+'langpy',
                plug_dir                        +os.sep+'lang',
              ]
    _       =  lambda x: x
    pass;                      #return _
    for lng_dir in lng_dirs:
        lng_mo  = lng_dir+'/{}/LC_MESSAGES/{}.mo'.format(lng, plug_mod)
        if os.path.isfile(lng_mo):
            t   = gettext.translation(plug_mod, lng_dir, languages=[lng])
            _   = t.gettext
            t.install()
            break
    return _
   #def get_translation

_   = get_translation(__file__) # I18N


######################################
#NOTE: misc
######################################
def upd_dict(d1, d2):
    rsp = d1.copy()
    rsp.update(d2)
    return rsp
   #def upd_dict

def deep_upd(dcts):
    pass;                      #log('dcts={}',(dcts))
    if not dcts:
        return dcts
    if isinstance(dcts, dict):
        return dcts

    dct1, *dcts = dcts
    pass;                      #log('dct1, dcts={}',(dct1, dcts))
    rsp   = dct1.copy()
    for dct in dcts:
        for k,v in dct.items():
            if False:pass
            elif k not in rsp:
                rsp[k]  = v
            elif isinstance(rsp[k], dict) and isinstance(v, dict):
                rsp[k].update(v)
            else:
                rsp[k]  = v
    pass;                      #log('rsp={}',(rsp))
    return rsp
   #def deep_upd

def isint(what):    return isinstance(what, int)
   
def ed_of_file_open(op_file):
    if not app.file_open(op_file):
        return None
    for h in app.ed_handles(): 
        op_ed   = app.Editor(h)
        if op_ed.get_filename() and os.path.samefile(op_file, op_ed.get_filename()):
            return op_ed
    return None
   #def ed_of_file_open

if __name__ == '__main__' :     # Tests
    class C:
        def m1(self, p):
            print('m1',p)
        def m2(self, p):
            print('m2',p)

    c = C()
#   print('c.m1',dir(c.m1))
#   print('c',dir(c))
#   print('c.m1.__self__',dir(c.m1.__self__))
    
    c.m1('0')
    rm = c.m1
    rm('0')
    rm(c, '0')

#   DlgAgent(
#       ctrls=[('b1', dict(type='button',             cap='Click', x=0, y= 0, w=100, a='lR',
#               call=lambda name,ag:{'ctrls':[('b1',{'cap':'OK',             'w':70})]}         ##!! new w <> a
#              ))
#             ,('b2', dict(type='button', cap='Close', x=0, y=30, w=100,             a='LR',
#               call=lambda name,ag:None)
#              )]
#   ,   form=dict(cap='Two buttons', x=0, y=0, w=100, h=60, h_max=60, resize=True)
#   ,   focused='b1'
#   ).show()

#   BaseDlgAgent(
#       ctrls=[('b1', dict(type='button', cap='Click', x=0, y= 0, w=100, 
#                          call=lambda name,ag:{'ctrls':[('b1',{'cap':'OK', 'w':70})]}))
#             ,('b2', dict(type='button', cap='Close', x=0, y=30, w=100,
#                          call=lambda name,ag:None))]
#   ,   form=dict(cap='Two buttons', x=0, y=0, w=100, h=60)
#   ,   focused='b1'
#   ).show()

#   pass
#   def test_ask_number(ask, def_val):
#       cnts=[dict(        tp='lb',tid='v',l=3 ,w=70,cap=ask)
#            ,dict(cid='v',tp='ed',t=3    ,l=73,w=70)
#            ,dict(cid='!',tp='bt',t=45   ,l=3 ,w=70,cap='OK',props='1')
#            ,dict(cid='-',tp='bt',t=45   ,l=73,w=70,cap='Cancel')]
#       vals={'v':def_val}
#       while True:
#           btn,vals,fid,chds=dlg_wrapper('Example',146,75,cnts,vals,'v')
#           if btn is None or btn=='-': return def_val
#           if not re.match(r'\d+$', vals['v']): continue
#           return vals['v']
#   ask_number('ask_____________', '____smth')

'''
ToDo
[ ][kv-kv][14may18] Remove keys dlg_wrapper_fit_va_for_* if it's same as def
'''
