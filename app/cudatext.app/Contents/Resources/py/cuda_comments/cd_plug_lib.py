''' Lib for Plugin
Authors:
    Andrey Kvichansky    (kvichans on github.com)
Version:
    '1.0.2 2016-03-22'
Content
    log                 Logger with timing
    get_translation     i18n
    dlg_wrapper         Wrapper for dlg_custom: pack/unpack values, h-align controls
ToDo: (see end of file)
'''

import  sys, os, gettext, logging, inspect
from    time        import perf_counter
import  cudatext        as app
import  cudax_lib       as apx

pass;                           # Logging
pass;                           from pprint import pformat

GAP         = 5
c13,c10,c9  = chr(13),chr(10),chr(9)
REDUCTS = {'lb'     :'label'
        ,  'ln-lb'  :'linklabel'
        ,  'ed'     :'edit'
        ,  'sp-ed'  :'spinedit'
        ,  'me'     :'memo'
        ,  'bt'     :'button'
        ,  'rd'     :'radio'
        ,  'ch'     :'check'
        ,  'ch-bt'  :'checkbutton'
        ,  'ch-gp'  :'checkgroup'
        ,  'rd-gp'  :'radiogroup'
        ,  'cb'     :'combo'
        ,  'cb-ro'  :'combo_ro'
        ,  'lbx'    :'listbox'
        ,  'ch-lbx' :'checklistbox'
        ,  'lvw'    :'listview'
        ,  'ch-lvw' :'checklistview'
        ,  'tabs'   :'tabs'
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

def get_translation(plug_file):
    ''' Part of i18n.
        Full i18n-cycle:
        1. All GUI-string in code are used in form 
            _('')
        2. These string are extracted from code to 
            lang/messages.pot
           with run
            python.exe <pypython-root>\Tools\i18n\pygettext.py -p lang <plugin>.py
        3. Poedit (or same program) create 
            <module>\lang\ru_RU\LC_MESSAGES\<module>.po
           from (cmd "Update from POT") 
            lang/messages.pot
           It allows to translate all "strings"
           It creates (cmd "Save")
            <module>\lang\ru_RU\LC_MESSAGES\<module>.mo
        4. get_translation uses the file to realize
            _('')
    '''
    plug_dir= os.path.dirname(plug_file)
    plug_mod= os.path.basename(plug_dir)
    lng     = app.app_proc(app.PROC_GET_LANG, '')
    lng_mo  = plug_dir+'/lang/{}/LC_MESSAGES/{}.mo'.format(lng, plug_mod)
    if os.path.isfile(lng_mo):
        t   = gettext.translation(plug_mod, plug_dir+'/lang', languages=[lng])
        _   = t.gettext
        t.install()
    else:
        _   =  lambda x: x
    return _

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
        if re.search(process, x):
            return True
    return False

ENV2FITS= {'win':
            {'check'      :-2
            ,'edit'       :-3
            ,'button'     :-4
            ,'combo_ro'   :-4
            ,'combo'      :-3
            ,'checkbutton':-4
            ,'linklabel'  : 0
            ,'spinedit'   :-3
            }
          ,'unity':
            {'check'      :-3
            ,'edit'       :-5
            ,'button'     :-4
            ,'combo_ro'   :-5
            ,'combo'      :-6
            ,'checkbutton':-3
            ,'linklabel'  : 0
            ,'spinedit'   :-5
            }
          ,'mac':
            {'check'      :-1
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
    else:
        fit = fit_top_by_env(what_tp) - fit_top_by_env(base_tp)
    pass;                      #log('what_tp, base_tp, fit={}',(what_tp, base_tp, fit))
    return fit_top_by_env__cash.setdefault((what_tp, base_tp), fit)
   #def fit_top_by_env

def dlg_wrapper(title, w, h, cnts, in_vals={}, focus_cid=None):
    """ Wrapper for dlg_custom. 
        Params
            title, w, h     Title, Width, Height 
            cnts            List of static control properties
                                [{cid:'*', tp:'*', t:1,l:1,w:1,r:1,b;1,h:1, cap:'*', hint:'*', en:'0', props:'*', items:[*], valign_to:'cid'}]
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
            focus           (opt) Control cid for  start focus
        Return
            btn_cid         Clicked/changed control cid
            {'cid':val}     Dict of new values for the same (as in_vals) controls
                                Format of values is same too.
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
                    btn,vals=dlg_wrapper('Example',146,75,cnts,vals,'v')
                    if btn is None or btn=='-': return def_val
                    if not re.match(r'\d+$', vals['v']): continue
                    return vals['v']
    """
    pass;                      #log('in_vals={}',pformat(in_vals, width=120))
    cid2i       = {cnt['cid']:i for i,cnt in enumerate(cnts) if 'cid' in cnt}
    if True:
        # Checks
        no_tids = {cnt['tid']   for   cnt in    cnts    if 'tid' in cnt and  cnt['tid'] not in cid2i}
        if no_tids:
            raise Exception(f('No cid(s) for tid(s): {}', no_tids))
        no_vids = {cid          for   cid in    in_vals if                          cid not in cid2i}
        if no_vids:
            raise Exception(f('No cid(s) for vals: {}', no_vids))
    ctrls_l = []
    for cnt in cnts:
        tp      = cnt['tp']
        tp      = REDUCTS.get(tp, tp)
        if tp=='--':
            # Horz-line
            t   = cnt.get('t')
            l   = cnt.get('l', 0)                   # def: from DlgLeft
            r   = cnt.get('r', l+cnt.get('w', w))   # def: to   DlgRight
            lst = ['type=label']
            lst+= ['cap='+'—'*1000]
            lst+= ['en=0']
            lst+= ['pos={l},{t},{r},0'.format(l=l,t=t,r=r)]
            ctrls_l+= [chr(1).join(lst)]
            continue#for cnt
            
        lst     = ['type='+tp]
        # Simple props
        for k in ['cap', 'hint', 'props']:
            if k in cnt:
                lst += [k+'='+str(cnt[k])]
        # Props with preparation
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
            t       = bs_cnt['t'] + fit_top_by_env(tp, REDUCTS.get(bs_tp, bs_tp))
#           t       = bs_cnt['t'] + top_plus_for_os(tp, REDUCTS.get(bs_tp, bs_tp))
        r       = cnt.get('r', l+cnt.get('w', 0)) 
        b       = cnt.get('b', t+cnt.get('h', 0)) 
        lst    += ['pos={l},{t},{r},{b}'.format(l=l,t=t,r=r,b=b)]
        if 'en' in cnt:
            val     = cnt['en']
            lst    += ['en='+('1' if val in [True, '1'] else '0')]

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

        if 'act' in cnt:    # must be last in lst
            val     = cnt['act']
            lst    += ['act='+('1' if val in [True, '1'] else '0')]
        pass;                  #log('lst={}',lst)
        ctrls_l+= [chr(1).join(lst)]
       #for cnt
    pass;                      #log('ok ctrls_l={}',pformat(ctrls_l, width=120))
    
    ans     = app.dlg_custom(title, w, h, '\n'.join(ctrls_l), cid2i.get(focus_cid, -1))
    if ans is None: return None, None, None   # btn_cid, {cid:v}, [cid]

    btn_i,  \
    vals_ls = ans[0], ans[1].splitlines()
    aid     = cnts[btn_i]['cid']
    # Parse output values
    an_vals = {cid:vals_ls[cid2i[cid]] for cid in in_vals}
    for cid in an_vals:
        cnt     = cnts[cid2i[cid]]
        tp      = cnt['tp']
        tp      = REDUCTS.get(tp, tp)
        in_val  = in_vals[cid]
        an_val  = an_vals[cid]
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
        an_vals[cid]    = an_val
       #for cid
    return  aid, an_vals, [cid for cid in in_vals if in_vals[cid]!=an_vals[cid]]
   #def dlg_wrapper

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

if __name__ == '__main__' :     # Tests
    def test_ask_number(ask, def_val):
        cnts=[dict(        tp='lb',tid='v',l=3 ,w=70,cap=ask)
             ,dict(cid='v',tp='ed',t=3    ,l=73,w=70)
             ,dict(cid='!',tp='bt',t=45   ,l=3 ,w=70,cap='OK',props='1')
             ,dict(cid='-',tp='bt',t=45   ,l=73,w=70,cap='Cancel')]
        vals={'v':def_val}
        while True:
            btn,vals=dlg_wrapper('Example',146,75,cnts,vals,'v')
            if btn is None or btn=='-': return def_val
            if not re.match(r'\d+$', vals['v']): continue
            return vals['v']
    ask_number('ask_____________', '____smth')
