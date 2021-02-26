''' Plugin for CudaText editor
Authors:
    Andrey Kvichansky    (kvichans on github.com)
Version:
    '2.3.14 2021-01-24'
ToDo: (see end of file)
'''

import  re, os, sys, json, collections, itertools, webbrowser, tempfile, html, pickle, time, datetime
from    itertools       import *
from pathlib import PurePath as PPath
from pathlib import     Path
def first_true(iterable, default=False, pred=None):return next(filter(pred, iterable), default) # 10.1.2. Itertools Recipes

import  cudatext            as app
import  cudatext_cmd        as cmds
import  cudax_lib           as apx
from    .cd_plug_lib    import *

d   = dict
odict = collections.OrderedDict

pass;                           LOG     = (-1== 1) or apx.get_opt('_opts_dlg_log',False)    # Do or dont logging.
pass;                           from pprint import pformat
pass;                           pf=lambda d:pformat(d,width=150)
pass;                           pf80=lambda d:pformat(d,width=80)
pass;                           pf60=lambda d:pformat(d,width=60)
pass;                           ##!! waits correction

_   = get_translation(__file__) # I18N

MIN_API_VER     = '1.0.168'
MIN_API_VER_4WR = '1.0.175'     # vis
MIN_API_VER     = '1.0.231'     # listview has prop columns
MIN_API_VER     = '1.0.236'     # p, panel
MIN_API_VER     = '1.0.237'     # STATUSBAR_SET_CELL_HINT
VERSION     = re.split('Version:', __doc__)[1].split("'")[1]
VERSION_V,  \
VERSION_D   = VERSION.split(' ')
MAX_HIST    = apx.get_opt('ui_max_history_edits', 20)
CFG_JSON    = app.app_path(app.APP_DIR_SETTINGS)+os.sep+'cuda_options_editor.json'
HTM_RPT_FILE= str(Path(tempfile.gettempdir()) / 'CudaText_option_report.html')
FONT_LST    = ['default'] \
            + [font 
                for font in app.app_proc(app.PROC_ENUM_FONTS, '')
                if not font.startswith('@')] 
pass;                          #FONT_LST=FONT_LST[:3]

def load_definitions(defn_path_or_json)->list:
    """ Return  
            [{  opt:'opt name'
            ,   def:<def val>
            ,   cmt:'full comment'
            ,   frm:'bool'|'float'|'int'|'str'|     # simple
                    'int2s'|'strs'|'str2s'|         # list/dict
                    'font'|'font-e'|                # font non-empty/can-empty
                    '#rgb'|'#rgb-e'|                # color non-empty/can-empty
                    'hotk'|'file'|'json'|
                    'unk'
            ,   lst:[str]       for frm==ints
            ,   dct:[(num,str)] for frm==int2s
            ,       [(str,str)] for frm==str2s
            ,   chp:'chapter/chapter'
            ,   tgs:['tag',]
            }]
    """
    pass;                      #LOG and log('defn_path_or_json={}',(defn_path_or_json))
    kinfs   = []
    lines   = defn_path_or_json \
                if str==type(defn_path_or_json) else \
              defn_path_or_json.open(encoding='utf8').readlines()
    if lines[0][0]=='[':
        # Data is ready - SKIP parsing
        json_bd = defn_path_or_json \
                    if str==type(defn_path_or_json) else \
                  defn_path_or_json.open(encoding='utf8').read()
        kinfs   = json.loads(json_bd, object_pairs_hook=odict)
        for kinf in kinfs:
            pass;              #LOG and log('opt in kinf={}',('opt' in kinf))
            if isinstance(kinf['cmt'], list):
                kinf['cmt'] = '\n'.join(kinf['cmt'])

        upd_cald_vals(kinfs, '+def')
        for kinf in kinfs:
            kinf['jdc'] = kinf.get('jdc', kinf.get('dct', []))
            kinf['jdf'] = kinf.get('jdf', kinf.get('def', ''))
        return kinfs

    l       = '\n'
    
    #NOTE: parse_raw
    reTags  = re.compile(r' *\((#\w+,?)+\)')
    reN2S   = re.compile(r'^\s*(\d+): *(.+)'    , re.M)
    reS2S   = re.compile(r'^\s*"(\w*)": *(.+)'  , re.M)
#   reLike  = re.compile(r' *\(like (\w+)\)')               ##??
    reFldFr = re.compile(r'\s*Folders from: (.+)')
    def parse_cmnt(cmnt, frm):#, kinfs):  
        tags= set()
        mt  = reTags.search(cmnt)
        while mt:
            tags_s  = mt.group(0)
            tags   |= set(tags_s.strip(' ()').replace('#', '').split(','))
            cmnt    = cmnt.replace(tags_s, '')
            mt      = reTags.search(cmnt)
        dctN= [[int(m.group(1)), m.group(2).rstrip(', ')] for m in reN2S.finditer(cmnt+l)]
        dctS= [[    m.group(1) , m.group(2).rstrip(', ')] for m in reS2S.finditer(cmnt+l)]
        lstF= None
        mt  = reFldFr.search(cmnt)
        if mt:
            from_short  = mt.group(1)
            from_dir    = from_short if os.path.isabs(from_short) else os.path.join(app.app_path(app.APP_DIR_DATA), from_short)
            pass;              #LOG and log('from_dir={}',(from_dir))
            if not os.path.isdir(from_dir):
                log(_('No folder "{}" from\n{}'), from_short, cmnt)
            else:
                lstF    = [d for d in os.listdir(from_dir) if os.path.isdir(from_dir+os.sep+d) and d.strip()]
                lstF    = sorted(lstF)
                pass;          #LOG and log('lstF={}',(lstF))
        frm,\
        lst = ('strs' , lstF)    if lstF else \
              (frm    , []  )
        frm,\
        dct = ('int2s', dctN)    if dctN else \
              ('str2s', dctS)    if dctS else \
              (frm    , []  )
        return cmnt, frm, dct, lst, list(tags)
       #def parse_cmnt
    def jsstr(s):
        return s[1:-1].replace(r'\"','"').replace(r'\\','\\')
    
    reChap1 = re.compile(r' *//\[Section: +(.+)\]')
    reChap2 = re.compile(r' *//\[(.+)\]')
    reCmnt  = re.compile(r' *//(.+)')
    reKeyDV = re.compile(r' *"(\w+)" *: *(.+)')
    reInt   = re.compile(r' *(-?\d+)')
    reFloat = re.compile(r' *(-?\d+\.\d+)')
    reFontNm= re.compile(r'font\w*_name')
    reHotkey= re.compile(r'_hotkey_')
    reColor = re.compile(r'_color$')
    chap    = ''
    pre_cmnt= ''
    pre_kinf= None
    cmnt    = ''
    for line in lines:
        if False:pass
        elif    reChap1.match(line):
            mt= reChap1.match(line)
            chap    = mt.group(1)
            cmnt    = ''
        elif    reChap2.match(line):
            mt= reChap2.match(line)
            chap    = mt.group(1)
            cmnt    = ''
        elif    reCmnt.match(line):
            mt= reCmnt.match(line)
            cmnt   += l+mt.group(1)
        elif    reKeyDV.match(line):
            mt= reKeyDV.match(line)
            key     = mt.group(1)
            dval_s  = mt.group(2).rstrip(', ')
            dfrm,dval= \
                      ('bool', True         )   if dval_s=='true'                       else \
                      ('bool', False        )   if dval_s=='false'                      else \
                      ('float',float(dval_s))   if reFloat.match(dval_s)                else \
                      ('int',  int(  dval_s))   if reInt.match(dval_s)                  else \
                      ('font', dval_s[1:-1] )   if reFontNm.search(key)                 else \
                      ('hotk', dval_s[1:-1] )   if reHotkey.search(key)                 else \
                      ('#rgb', dval_s[1:-1] )   if reColor.search(key)                  else \
                      ('str',  jsstr(dval_s))   if dval_s[0]=='"' and dval_s[-1]=='"'   else \
                      ('unk',  dval_s       )
            dfrm,dval=('#rgb-e',''          )   if dfrm=='#rgb' and dval==''            else \
                      (dfrm,   dval         ) 
            pass;              #LOG and log('key,dval_s,dfrm,dval={}',(key,dval_s,dfrm,dval))
            
            cmnt    = cmnt.strip(l)     if cmnt else pre_cmnt
            ref_frm = cmnt[:3]=='...'
            pre_cmnt= cmnt              if cmnt else pre_cmnt
            pass;              #LOG and log('ref_frm,pre_cmnt,cmnt={}',(ref_frm,pre_cmnt,cmnt))
            cmnt    = cmnt.lstrip('.'+l)

            dfrm    = 'font-e' if dfrm=='font' and _('Empty string is allowed') in cmnt   else dfrm
            
            kinf    = odict()
            kinfs  += [kinf]
            kinf['opt']         = key
            kinf['def']         = dval
            kinf['cmt']         = cmnt.strip()
            kinf['frm']         = dfrm
            if dfrm in ('int','str'):
                cmnt,frm,\
                dct,lst,tags    = parse_cmnt(cmnt, dfrm)#, kinfs)
                kinf['cmt']     = cmnt.strip()
                if frm!=dfrm:
                    kinf['frm'] = frm
                if dct:
                    kinf['dct'] = dct
                if lst:
                    kinf['lst'] = lst
                if tags:
                    kinf['tgs'] = tags
            if dfrm=='font':
                kinf['lst']     = FONT_LST
            if dfrm=='font-e':
                kinf['lst']     = [''] + FONT_LST
            if chap:
                kinf['chp']     = chap
            
            if ref_frm and pre_kinf:
                # Copy frm data from prev oi
                pass;          #LOG and log('Copy frm pre_kinf={}',(pre_kinf))
                kinf[    'frm'] = pre_kinf['frm']
                if 'dct' in pre_kinf:
                    kinf['dct'] = pre_kinf['dct']
                if 'lst' in pre_kinf:
                    kinf['lst'] = pre_kinf['lst']
            
            pre_kinf= kinf.copy()
            cmnt    = ''
       #for line
    pass;                      #open(str(defn_path_or_json)+'.p.json', 'w').write(json.dumps(kinfs,indent=2))
    upd_cald_vals(kinfs, '+def')
    for kinf in kinfs:
        kinf['jdc'] = kinf.get('jdc', kinf.get('dct', []))
        kinf['jdf'] = kinf.get('jdf', kinf.get('def', ''))
    return kinfs
   #def load_definitions

def load_vals(opt_dfns:list, lexr_json='', ed_=None, full=False, user_json='user.json')->odict:
    """ Create reformated copy (as odict) of 
            definitions data opt_dfns (see load_definitions) 
        If ed_ then add
            'fval' 
            for some options
        If full==True then append optitions without definition
            but only with 
            {   opt:'opt name'
            ,   frm:'int'|'float'|'str'
            ,   uval:<value from user.json>
            ,   lval:<value from lexer*.json>
            }}
        Return
            {'opt name':{  opt:'opt name', frm:
        ?   ,   def:, cmt:, dct:, chp:, tgs:
        ?   ,   uval:<value from user.json>
        ?   ,   lval:<value from lexer*.json>
        ?   ,   fval:<value from ed>
            }}
    """
    user_json       = app.app_path(app.APP_DIR_SETTINGS)+os.sep+user_json
    lexr_def_json   = apx.get_def_setting_dir()         +os.sep+lexr_json
    lexr_json       = app.app_path(app.APP_DIR_SETTINGS)+os.sep+lexr_json
    user_vals       = apx._json_loads(open(user_json    , encoding='utf8').read(), object_pairs_hook=odict) \
                        if  os.path.isfile(user_json)     else {}
    lexr_def_vals   = apx._json_loads(open(lexr_def_json, encoding='utf8').read(), object_pairs_hook=odict) \
                        if  os.path.isfile(lexr_def_json) else {}
    lexr_vals       = apx._json_loads(open(lexr_json    , encoding='utf8').read(), object_pairs_hook=odict) \
                        if  os.path.isfile(lexr_json)     else {}
    pass;                      #LOG and log('lexr_vals={}',(lexr_vals))
    pass;                      #LOG and log('lexr_def_vals={}',(lexr_def_vals))

    # Fill vals for defined opt
    pass;                      #LOG and log('no opt={}',([oi for oi in opt_dfns if 'opt' not in oi]))
    oinf_valed  = odict([(oi['opt'], oi) for oi in opt_dfns])
    for opt, oinf in oinf_valed.items():
        if opt in lexr_def_vals:                                # Correct def-vals for lexer
            oinf['dlx']     = True
            oinf['def']     = lexr_def_vals[opt]
            oinf['jdf']     = oinf['def']
        if opt in user_vals:                                    # Found user-val for defined opt
            oinf['uval']    = user_vals[opt]
        if opt in lexr_vals:                                    # Found lexer-val for defined opt
            oinf['lval']    = lexr_vals[opt]
        if ed_ and opt in apx.OPT2PROP:                         # Found file-val for defined opt
            fval            = ed_.get_prop(apx.OPT2PROP[opt])
            oinf['fval']    =fval

    if full:
        # Append item for non-defined opt
        reFontNm    = re.compile(r'font\w*_name')
        def val2frm(val, opt=''):
            pass;              #LOG and log('opt,val={}',(opt,val))
            return  ('bool'     if isinstance(val, bool)    else
                     'int'      if isinstance(val, int)     else
                     'float'    if isinstance(val, float)   else
                     'json'     if isinstance(val, (list, dict))    else
                     'hotk'     if '_hotkey_' in val        else
                     'font'     if isinstance(val, str)     and 
                                   reFontNm.search(val)     else
                     'str')
        for uop,uval in user_vals.items():
            if uop in oinf_valed: continue
            oinf_valed[uop] = odict(
                [   ('opt'  ,uop)
                ,   ('frm'  ,val2frm(uval,uop))
                ,   ('uval' ,uval)
                ]+([('lval' ,lexr_vals[uop])] if uop in lexr_vals else [])
                )
        for lop,lval in lexr_vals.items():
            if lop in oinf_valed: continue
            oinf_valed[lop] = odict(
                [   ('opt'  ,lop)
                ,   ('frm'  ,val2frm(lval,lop))
                ,   ('lval' ,lval)
                ])
    
    upd_cald_vals(oinf_valed)
    upd_cald_vals(oinf_valed, '+def') if lexr_def_vals else None    # To update oi['jdf'] by oi['def']

    return oinf_valed
   #def load_vals

def upd_cald_vals(ois, what=''):
    # Fill calculated attrs
    if '+def' in what:
        for oi in [oi for oi in ois if 'dct' in oi]:
            dct = oi['dct']
            dval= oi['def']
            dc  = odict(dct)
            pass;              #LOG and log('dct={}',(dct))
            oi['jdc']   = [f('({}) {}', vl,   cm      ) for vl,cm in dct]
            oi['jdf']   =  f('({}) {}', dval, dc[dval])
            pass;              #LOG and log('oi={}',(oi))


    # Fill calculated attrs
    if not what or '+clcd' in what:
        for op, oi in ois.items():
            oi['!']     = ('L' if oi.get('dlx') else '') \
                        + ('+!!' if 'def' not in oi and 'lval' in oi   else
                            '+!' if 'def' not in oi and 'uval' in oi   else
                           '!!!' if                     'fval' in oi 
                                    and oi['fval'] != oi.get('lval'
                                                    , oi.get('uval'
                                                    , oi.get( 'def'))) else
                            '!!' if                     'lval' in oi   else
                             '!' if                     'uval' in oi   else
                          '')
            dct         = odict(oi.get('dct', []))
            oi['juvl']  = oi.get('uval', '') \
                            if not dct or 'uval' not in oi else \
                          f('({}) {}', oi['uval'], dct[oi['uval']])
            oi['jlvl']  = oi.get('lval', '') \
                            if not dct or 'lval' not in oi else \
                          f('({}) {}', oi['lval'], dct[oi['lval']])
            oi['jfvl']  = oi.get('fval', '') \
                            if not dct or 'fval' not in oi else \
                          f('({}) {}', oi['fval'], dct[oi['fval']])
   #def upd_cald_vals

#class OptDt:
#   """ Options infos to view/change in dlg.
#       Opt getting is direct - by fields.
#       Opt setting only by methods.
#   """
#
#   def __init__(self
#       , keys_info=None            # Ready data
#       , path_raw_keys_info=''     # default.json
#       , path_svd_keys_info=''     # To save parsed default.json
#       , bk_sets=False             # Create backup of settings before the first change
#       ):
#       self.defn_path  = Path(path_raw_keys_info)
#       self.bk_sets    = bk_sets   # Need to backup
#       self.bk_files   = {}        # Created backup files
#
#       self.opts_defn  = {}        # Meta-info for options: format, comment, dict/list of values, chapter, tags
#       self.ul_opts    = {}        # Total options info for user+cur_lexer
#      #def __init__
#  
#  #class OptDt

_SORT_NO    = -1
_SORT_DN    = 0
_SORT_UP    = 1
_SORT_TSGN  = {_SORT_NO:'', _SORT_UP:'↑', _SORT_DN:'↓'}
_SORT_NSGN  = {-1:'', 0:'', 1:'²', 2:'³'}
_SORT_NSGN.update({n:str(1+n) for n in range(3,10)})
_sort_pfx   = lambda to,num: '' if to==_SORT_NO else _SORT_TSGN[to]+_SORT_NSGN[num]+' '
_next_sort  = lambda to: ((1 + 1+to) % 3) - 1
_inve_sort  = lambda to: 1 - to

sorts_dflt  = lambda cols: [[_SORT_NO, -1] for c in range(cols)]
sorts_sign  = lambda sorts, col: _sort_pfx(sorts[col][0], sorts[col][1])
sorts_on    = lambda sorts, col: sorts[col][0] != _SORT_NO

def sorts_turn(sorts, col, scam=''):
    """ Switch one of sorts """
    max_num = max(tn[1] for tn in sorts) 
    tn_col  = sorts[col]
    if 0:pass
    elif 'c'==scam and tn_col[1]==max_num:          # Turn col with max number
        tn_col[0]   = _next_sort(tn_col[0])
        tn_col[1]   = -1 if tn_col[0]==_SORT_NO else tn_col[1]
    elif 'c'==scam:                                 # Add new or turn other col
        tn_col[0]   = _next_sort(tn_col[0]) if -1==tn_col[1] else _inve_sort(tn_col[0])
        tn_col[1]   = max_num+1             if -1==tn_col[1] else tn_col[1]
    else:#not  scam:                                # Only col
        for cl,tn in enumerate(sorts):  
            tn[0]   = _next_sort(tn_col[0]) if cl==col else _SORT_NO
            tn[1]   = 0                     if cl==col else -1
    return sorts
   #def sorts_turn

def sorts_sort(sorts, tdata):
    """ Sort tdata (must contain only str) by sorts """
    pass;                      #log('tdata={}',(tdata))
    pass;                      #log('sorts={}',(sorts))
    max_num     = max(tn[1] for tn in sorts) 
    if -1==max_num:  return tdata

    def push(lst, v):
        lst.append(v)
        return lst
    prep_str    = lambda s,inv: (chr(0x10FFFF)                              # To move empty to bottom
                                    if not s else 
                                 s
                                    if not inv else 
                                 ''.join(chr(0x10FFFF - ord(c)) for c in s) # 0x10FFFF from chr() doc 
                                )                                 

    td_keys     = [[r] for r in tdata]
    for srt_n in range(1+max_num):
        srt_ctn = first_true(((c,tn) for c,tn in enumerate(sorts)), None
                            ,lambda ntn: ntn[1][1]==srt_n)
        assert srt_ctn is not None
        srt_c   = srt_ctn[0]
        inv     = srt_ctn[1][0]==_SORT_UP
        td_keys = [push(r, prep_str(r[0][srt_c], inv)) for r in td_keys]
    td_keys.sort(key=lambda r: r[1:])
    tdata       = [r[0] for r in td_keys]                                   # Remove appended cols
    return tdata
   #def sorts_sort

class OptEdD:
    SCROLL_W= app.app_proc(app.PROC_GET_GUI_HEIGHT, 'scrollbar') if app.app_api_version()>='1.0.233' else 15
    COL_SEC = 0
    COL_NAM = 1
    COL_OVR = 2
    COL_DEF = 3
    COL_USR = 4
    COL_LXR = 5
    COL_FIL = 6
    COL_LEXR= _('Lexer')
    COL_FILE= _('File "{}"')
    COL_NMS = (_('Section'), _('Option'), '!', _('Default'), _('User'), COL_LEXR, COL_FILE)
    COL_MWS = [   70,           210,       25,    120,         120,       70,         50]   # Min col widths
#   COL_MWS = [   70,           150,       25,    120,         120,       70,         50]   # Min col widths
    COL_N   = len(COL_MWS)
    CMNT_MHT= 60                            # Min height of Comment
    STBR_FLT= 10
    STBR_ALL= 11
    STBR_MSG= 12
    STBR_H  = apx.get_opt('ui_statusbar_height',24)

    FILTER_C= _('&Filter')
    NO_CHAP = _('_no_')
    CHPS_H  = f(_('Choose section to append in "{}".'
                '\rHold Ctrl to add several sections.'
               ), FILTER_C).replace('&', '')
    FLTR_H  = _('Suitable options will contain all specified words.'
              '\r Tips and tricks:'
              '\r • Add "#" to search the words also in comments.'
              '\r • Add "@sec" to show options from section with "sec" in name.'
              '\r   Several sections are allowed.'
              '\r   Click item in menu "Section..." with Ctrl to add it.'
              '\r • To show only overridden options:'
              '\r   - Add "!"   to show only User+Lexer+File.'
              '\r   - Add "!!"  to show only Lexer+File'
              '\r   - Add "!!!" to show only File.'
              '\r • Use "<" or ">" for word boundary.'
              '\r     Example: '
              '\r       size> <tab'
              '\r     selects "tab_size" but not "ui_tab_size" or "tab_size_x".'
              '\r • Alt+L - Clear filter')
    LOCV_C  = _('Go to "{}" in user/lexer config file')
    LOCD_C  = _('Go to "{}" in default config file')
    OPME_H  = _('Edit JSON value')
    TOOP_H  = f(_('Close dialog and open user/lexer settings file'
                  '\rto edit the current option.'
                  '\rSee also menu command'
                  '\r   {}'), f(LOCD_C, '<option>'))
    LIFL_C  = _('Instant filtering')
    FULL_C  = _('Show &all keys in user/lexer configs')

    @staticmethod
    def prep_sorts(sorts):
        M   = OptEdD
        if len(sorts)==len(M.COL_NMS):
            return sorts
        return sorts_dflt(len(M.COL_NMS))

    def __init__(self
        , path_keys_info    =''             # default.json or parsed data (file or list_of_dicts)
        , subset            =''             # To get/set from/to cuda_options_editor.json
        , how               ={}             # Details to work
        ):
        M,m         = self.__class__,self
        
        m.ed        = ed
        m.how       = how
        
        m.defn_path = Path(path_keys_info)  if str==type(path_keys_info) else json.dumps(path_keys_info)

        m.subset    = subset
        m.stores    = get_hist('dlg'
                        , json.loads(open(CFG_JSON).read(), object_pairs_hook=odict)
                            if os.path.exists(CFG_JSON) else odict())
        pass;                  #LOG and log('ok',())
#       m.bk_sets   = m.stores.get(m.subset+'bk_sets'    , False)
        m.lexr_l    = app.lexer_proc(app.LEXER_GET_LEXERS, False)
        m.lexr_w_l  = [f('{} {}'
                        ,'!!' if os.path.isfile(app.app_path(app.APP_DIR_SETTINGS)+os.sep+'lexer '+lxr+'.json') else '  '
                        , lxr) 
                        for lxr in m.lexr_l]
        
        m.cur_op    = m.stores.get(m.subset+'cur_op'    , '')           # Name of current option
        m.col_ws    = m.stores.get(m.subset+'col_ws'    , M.COL_MWS[:])
        m.col_ws    = m.col_ws if M.COL_N==len(m.col_ws) else M.COL_MWS[:]
        m.h_cmnt    = m.stores.get(m.subset+'cmnt_heght', M.CMNT_MHT)
        m.sorts     = m.stores.get(m.subset+'sorts'     , []        )   # Def sorts is no sorts
        m.live_fltr = m.stores.get(m.subset+'live_fltr' , False)        # To filter after each change and no History
        m.cond_hl   = [s for s in m.stores.get(m.subset+'h.cond', []) if s] if not m.live_fltr else []
        m.cond_s    = '' if M.restart_cond is None else M.restart_cond  # String filter
        m.ops_only  = []        # Subset to show (future)
        
        m.sorts     = M.prep_sorts(m.sorts)
        
        m.lexr      = m.ed.get_prop(app.PROP_LEXER_CARET)
        m.all_ops   = m.stores.get(m.subset+'all_ops'   , False)        # Show also options without definition

        m.opts_defn = {}        # Meta-info for options: format, comment, dict of values, chapter, tags
        m.opts_full = {}        # Show all options
        m.chp_tree  = {}        # {'Ui':{ops:[], 'kids':{...}, 'path':'Ui/Tabs'}
        m.pth2chp   = {}        # path-index for m.chp_tree

        # Cache
        m.SKWULFs   = []        # Last filtered+sorted
        m.cols      = []        # Last info about listview columns
        m.itms      = []        # Last info about listview cells
        
#       m.bk_files  = {}
#       m.do_file('backup-user')    if m.bk_sets else 0
        
        m.do_file('load-data')
        
        m.for_ulf   = 'u'       # 'u' for User, 'l' for Lexer, 'f' for File
        m.cur_op    = m.cur_op if m.cur_op in m.opts_full else ''           # First at start
        m.cur_in    = 0 if m.cur_op else -1
        
        m.stbr      = None      # Handle for statusbar_proc
        m.locate_on_exit    = None
        
        m.chng_rpt  = []        # Report of all changes by user
        m.apply_one = m.stores.get(m.subset+'apply_one', False) # Do one call OpsReloadAndApply on exit
        m.apply_need= False                                     # Need to call OpsReloadAndApply
        m.auto4file = m.stores.get(m.subset+'auto4file', True)  # Auto reset file value to over value def/user/lex
       #def __init__
    
    def stbr_act(self, tag=None, val='', opts={}):
        M,m = self.__class__,self
        if not m.stbr:  return 
        app.statusbar_proc(m.stbr, app.STATUSBAR_SET_CELL_TEXT, tag=tag, value=str(val))
       #def stbr_act
    
    def do_file(self, what, data='', opts={}):
        M,m = self.__class__,self
        if False:pass
        elif what=='load-data':
            pass;              #LOG and log('',)
            m.opts_defn = load_definitions(m.defn_path)
            pass;              #LOG and log('m.opts_defn={}',pf([o for o in m.opts_defn]))
            pass;              #LOG and log('m.opts_defn={}',pf([o for o in m.opts_defn if '2s' in o['frm']]))
            m.opts_full = load_vals(m.opts_defn
                                   ,lexr_json='lexer '+m.lexr+'.json'
                                   ,user_json=m.how.get('stor_json', 'user.json')
                                   , ed_=m.ed, full=m.all_ops)
            m.cur_op    = m.cur_op if m.cur_op in m.opts_full else ''
            pass;              #LOG and log('m.opts_full={}',pf(m.opts_full))
            m.do_file('build-chp-tree')
        
        elif what=='build-chp-tree':
            # Build chapter tree
            m.chp_tree  = odict(ops=list(m.opts_full.keys())
                               ,kids=odict()
                               ,path='')  # {chp:{ops:[], kids:{...}, path:'c1/c2'}
            m.pth2chp   = {}                                    # {path:chp}
            for op,oi in m.opts_full.items():
                chp_s   = oi.get('chp', M.NO_CHAP)
                chp_s   = chp_s if chp_s else M.NO_CHAP
                chp_node= m.chp_tree                            # Start root to move
                kids    = chp_node['kids']
                path    =''
                for chp in chp_s.split('/'):
                    # Move along branch and create nodes if need
                    chp_node    = kids.setdefault(chp, odict())
                    path       += ('/'+chp) if path else chp
                    chp_node['path']= path
                    m.pth2chp[path] = chp_node
                    ops_l       = chp_node.setdefault('ops', [])
                    ops_l      += [op]
                    if not ('/'+chp_s).endswith('/'+chp):   # not last
                        kids    = chp_node.setdefault('kids', odict())
            pass;              #LOG and log('m.chp_tree=¶{}',pf60(m.chp_tree))
            pass;              #LOG and log('m.pth2chp=¶{}',pf60(m.pth2chp))
        
        elif what == 'locate_to':
            to_open = data['path']
            find_s  = data['find']
            app.file_open(to_open)      ##!!
            pass;              #log('to_open={}',(to_open))
            pass;              #log('ed.get_filename()={}',(ed.get_filename()))
            m.ag.opts['on_exit_focus_to_ed'] = ed
            # Locate
            user_opt= app.app_proc(app.PROC_GET_FINDER_PROP, '') \
                        if app.app_api_version()>='1.0.248' else \
                      app.app_proc(app.PROC_GET_FIND_OPTIONS, '')   # Deprecated
            pass;              #log('ed_to_fcs.get_filename()={}',(ed_to_fcs.get_filename()))
            pass;              #log('ed.get_filename()={}',(ed.get_filename()))
            pass;              #LOG and log('find_s={!r}',(find_s))
            ed.cmd(cmds.cmd_FinderAction, chr(1).join(['findnext', find_s, '', 'fa']))    # f - From-caret,  a - Wrap
            if app.app_api_version()>='1.0.248':
                app.app_proc(app.PROC_SET_FINDER_PROP, user_opt)
            else:
                app.app_proc(app.PROC_SET_FIND_OPTIONS, user_opt)   # Deprecated
            
        elif what in ('locate-def', 'locate-opt', 'goto-def', 'goto-opt', ):
            if not m.cur_op:
                m.stbr_act(M.STBR_MSG, _('Choose option to find in config file'))
                return False
            oi      = m.opts_full[m.cur_op]
            pass;              #LOG and log('m.cur_op,oi={}',(m.cur_op,oi))
            to_open = ''
            if what in ('locate-opt', 'goto-opt'):
                if 'uval' not in oi and m.for_ulf=='u':
                    m.stbr_act(M.STBR_MSG, f(_('No user value for option "{}"'), m.cur_op))
                    return False
                if 'lval' not in oi and m.for_ulf=='l':
                    m.stbr_act(M.STBR_MSG, f(_('No lexer "{}" value for option "{}"'), m.lexr, m.cur_op))
                    return False
                to_open = 'lexer '+m.lexr+'.json'   if m.for_ulf=='l' else 'user.json'
                to_open = app.app_path(app.APP_DIR_SETTINGS)+os.sep+to_open
            else:
                if 'def' not in oi:
                    m.stbr_act(M.STBR_MSG, f(_('No default for option "{}"'), m.cur_op))
                    return False
                to_open = str(m.defn_path)
            if not os.path.exists(to_open):
                log('No file={}',(to_open))
                return False

            find_s  = f('"{}"', m.cur_op)
            if what in ('goto-def', 'goto-opt'):
                m.locate_on_exit  = d(path=to_open, find=find_s)
                return True #
            m.do_file('locate_to',  d(path=to_open, find=find_s))
            return False
        
       #elif what=='set-dfns':
       #    m.defn_path = data
       #    m.do_file('load-data')
       #    return d(ctrls=odict(m.get_cnts('lvls')))
        
        elif what=='set-lexr':
            m.opts_full = load_vals(m.opts_defn
                                   ,lexr_json='lexer '+m.lexr+'.json'
                                   ,user_json=m.how.get('stor_json', 'user.json')
                                   ,ed_=m.ed, full=m.all_ops)
            return d(ctrls=odict(m.get_cnts('lvls')))

        elif what=='out-rprt':
            if do_report(HTM_RPT_FILE, 'lexer '+m.lexr+'.json', m.ed):
                webbrowser.open_new_tab('file://'         +HTM_RPT_FILE)
                app.msg_status(_('Opened browser with file ')+HTM_RPT_FILE)

        return []
       #def do_file
    
    def _prep_opt(self, opts='', ind=-1, nm=None):
        """ Prepare vars to show info about current option by 
                m.cur_op
                m.lexr
            Return
                {}  vi-attrs
                {}  en-attrs
                {}  val-attrs
                {}  items-attrs
        """
        M,m = self.__class__,self
        if opts=='key2ind':
            opt_nm  = nm if nm else m.cur_op
            m.cur_in= index_1([m.SKWULFs[row][1] for row in range(len(m.SKWULFs))], opt_nm, -1)
            return m.cur_in
        
        if opts=='ind2key':
            opt_in  = ind if -1!=ind else m.ag.cval('lvls')
            m.cur_op= m.SKWULFs[opt_in][1] if -1<opt_in<len(m.SKWULFs) else ''
            return m.cur_op
        
        if opts=='fid4ed':
            if not m.cur_op:    return 'lvls'
            frm = m.opts_full[m.cur_op]['frm']
            fid =   'eded'  if frm in ('str', 'int', 'float')                       else \
                    'edcb'  if frm in ('int2s', 'str2s', 'strs', 'font', 'font-e')  else \
                    'edrf'  if frm in ('bool',)                                     else \
                    'brow'  if frm in ('hotk', 'file', '#rgb', '#rgb-e')            else \
                    'opjs'  if frm in ('json')                                      else \
                    'lvls'
            pass;              #LOG and log('m.cur_op,frm,fid={}',(m.cur_op,frm,fid))
            return fid
        
        pass;                  #LOG and log('m.cur_op, m.lexr={}',(m.cur_op, m.lexr))
        vis,ens,vas,its,bcl = {},{},{},{},{}
        vis['edcl'] = vis['dfcl'] = False
        bcl['edcl'] = bcl['dfcl'] = 0x20000000
#       bcl['eded'] = bcl['dfvl'] = 0x20000000
        
        ens['eded'] = ens['setd']                                                               = False # All un=F
        vis['eded'] = vis['edcb']=vis['edrf']=vis['edrt']=vis['brow']=vis['toop']=vis['opjs']   = False # All vi=F
        vas['eded'] = vas['dfvl']=vas['cmnt']= ''                                                       # All ed empty
        vas['edcb'] = -1
        vas['edrf'] = vas['edrt'] = False
        its['edcb'] = []
        
        ens['dfvl']         = True
        ens['tofi']         = m.cur_op in apx.OPT2PROP
        if m.for_ulf=='l' and m.lexr not in m.lexr_l:
            # Not selected lexer
            vis['eded']     = True
            ens['dfvl']     = False
            return vis,ens,vas,its,bcl
        
        if m.for_ulf=='f' and m.cur_op not in apx.OPT2PROP:
            # No the option for File
            vis['eded']     = True
            ens['dfvl']     = False
            return vis,ens,vas,its,bcl
        
        if not m.cur_op:
            # No current option
            vis['eded']     = True
        else:
            # Current option
            oi              = m.opts_full[m.cur_op]
            pass;              #LOG and log('oi={}',(oi))
            vas['dfvl']     = str(oi.get('jdf' , '')).replace('True', 'true').replace('False', 'false')
            vas['uval']     = oi.get('uval', '')
            vas['lval']     = oi.get('lval', '')
            vas['fval']     = oi.get('fval', '')
            vas['cmnt']     = oi.get('cmt' , '')
            frm             = oi['frm']
            ulfvl_va        = vas['fval'] \
                                if m.for_ulf=='f' else \
                              vas['lval'] \
                                if m.for_ulf=='l' else \
                              vas['uval']                       # Cur val with cur state of "For lexer"
            ens['eded']     = frm not in ('json', 'hotk', 'file')#, '#rgb', '#rgb-e')
            ens['setd']     = frm not in ('json',) and ulfvl_va is not None
            if False:pass
            elif frm in ('json'):
#               vis['toop'] = True
                vis['opjs'] = True
                vis['eded'] = True
                vas['eded'] = str(ulfvl_va)
            elif frm in ('str', 'int', 'float'):
                vis['eded'] = True
                vas['eded'] = str(ulfvl_va)
            elif frm in ('hotk', 'file', '#rgb', '#rgb-e'):
                vis['eded'] = True
                vis['brow'] = True
                vas['eded'] = str(ulfvl_va)
                vis['edcl'] = frm in ('#rgb', '#rgb-e')
                vis['dfcl'] = frm in ('#rgb', '#rgb-e')
                bcl['edcl'] = apx.html_color_to_int(ulfvl_va    ) if frm in ('#rgb', '#rgb-e') and ulfvl_va     else 0x20000000
                bcl['dfcl'] = apx.html_color_to_int(vas['dfvl'] ) if frm in ('#rgb', '#rgb-e') and vas['dfvl']  else 0x20000000
            elif frm in ('bool',):
                vis['edrf'] = True
                vis['edrt'] = True
                vas['edrf'] = ulfvl_va is False
                vas['edrt'] = ulfvl_va is True
            elif frm in ('int2s', 'str2s'):
                vis['edcb'] = True
                ens['edcb'] = True
                its['edcb'] = oi['jdc']
                vas['edcb'] = index_1([k for (k,v) in oi['dct']], ulfvl_va, -1)
                pass;          #LOG and log('ulfvl_va, vas[edcb]={}',(ulfvl_va,vas['edcb']))
            elif frm in ('strs','font','font-e'):
                vis['edcb'] = True
                ens['edcb'] = True
                its['edcb'] = oi['lst']
                vas['edcb'] = index_1(oi['lst'], ulfvl_va, -1)
        
        pass;                  #LOG and log('ulfvl_va={}',(ulfvl_va))
        pass;                  #LOG and log('vis={}',(vis))
        pass;                  #LOG and log('ens={}',(ens))
        pass;                  #LOG and log('vas={}',(vas))
        pass;                  #LOG and log('its={}',(its))
        return vis,ens,vas,its,bcl
       #def _prep_opt

    def show(self
        , title                     # For cap of dlg
        ):
        M,m = self.__class__,self

        def when_exit(ag):
            pass;              #LOG and log('',())
            pass;              #pr_   = dlg_proc_wpr(ag.id_dlg, app.DLG_CTL_PROP_GET, name='edch')
            pass;              #log('exit,pr_={}',('edch', {k:v for k,v in pr_.items() if k in ('x','y')}))
            pass;              #log('cols={}',(ag.cattr('lvls', 'cols')))
            m.col_ws= [ci['wd'] for ci in ag.cattr('lvls', 'cols')]
            m.stores[m.subset+'cmnt_heght'] = m.ag.cattr('cmnt', 'h')
            
            if m.apply_one and m.apply_need:
                ed.cmd(cmds.cmd_OpsReloadAndApply)
            
            if m.locate_on_exit:
                m.do_file('locate_to', m.locate_on_exit)
           #def when_exit

        repro_py    = apx.get_opt('dlg_cuda_options.repro_py')  # 'repro_dlg_opted.py'

        m.dlg_min_w = 10 + sum(M.COL_MWS) + M.COL_N + M.SCROLL_W
        m.dlg_w     = 10 + sum(m.col_ws)  + M.COL_N + M.SCROLL_W
        m.dlg_h     = 380 + m.h_cmnt    +10 + M.STBR_H
#       m.dlg_h     = 270 + m.h_cmnt    +10 + M.STBR_H
        pass;                  #log('m.dlg_w,m.dlg_h={}',(m.dlg_w,m.dlg_h))
        m.ag = DlgAgent(
            form =dict(cap     = title + f(' ({})', VERSION_V)
                      ,resize  = True
                      ,w       = m.dlg_w    ,w_min=m.dlg_min_w
                      ,h       = m.dlg_h
                      ,on_resize=m.do_resize
                      )
        ,   ctrls=m.get_cnts()
        ,   vals =m.get_vals()
        ,   fid  ='cond'
                                ,options = ({
                                    'gen_repro_to_file':repro_py,    #NOTE: repro
                                } if repro_py else {})
        )
        # Select on pre-show. Reason: linux skip selection event after show
        m.ag._update_on_call(m.do_sele('lvls', m.ag))

        m.stbr  = app.dlg_proc(m.ag.id_dlg, app.DLG_CTL_HANDLE, name='stbr')
        app.statusbar_proc(m.stbr, app.STATUSBAR_ADD_CELL               , tag=M.STBR_ALL)
        app.statusbar_proc(m.stbr, app.STATUSBAR_SET_CELL_SIZE          , tag=M.STBR_ALL, value=40)
        app.statusbar_proc(m.stbr, app.STATUSBAR_SET_CELL_ALIGN         , tag=M.STBR_ALL, value='R')
        app.statusbar_proc(m.stbr, app.STATUSBAR_SET_CELL_HINT          , tag=M.STBR_ALL, value=_('Number of all options'))
        app.statusbar_proc(m.stbr, app.STATUSBAR_ADD_CELL               , tag=M.STBR_FLT)
        app.statusbar_proc(m.stbr, app.STATUSBAR_SET_CELL_SIZE          , tag=M.STBR_FLT, value=40)
        app.statusbar_proc(m.stbr, app.STATUSBAR_SET_CELL_ALIGN         , tag=M.STBR_FLT, value='R')
        app.statusbar_proc(m.stbr, app.STATUSBAR_SET_CELL_HINT          , tag=M.STBR_FLT, value=_('Number of shown options'))
        app.statusbar_proc(m.stbr, app.STATUSBAR_ADD_CELL               , tag=M.STBR_MSG)
        app.statusbar_proc(m.stbr, app.STATUSBAR_SET_CELL_AUTOSTRETCH   , tag=M.STBR_MSG, value=True)
        m.stbr_act(M.STBR_ALL, len(m.opts_full))
        m.stbr_act(M.STBR_FLT, len(m.opts_full))

        stor_json   = app.app_path(app.APP_DIR_SETTINGS)+os.sep+m.how.get('stor_json', 'user.json')
        start_mtime = os.path.getmtime(stor_json) if os.path.exists(stor_json) else 0
        
        m.ag.show(when_exit)
        m.ag    = None

        # Save for next using
        m.stores[m.subset+'cur_op']     = m.cur_op
        m.stores[m.subset+'col_ws']     = m.col_ws
        m.stores[m.subset+'sorts']      = m.sorts
        if not m.live_fltr:
            m.stores[m.subset+'h.cond'] = m.cond_hl
        m.stores[m.subset+'all_ops']    = m.all_ops
        set_hist('dlg', m.stores)
        
        return start_mtime != (os.path.getmtime(stor_json) if os.path.exists(stor_json) else 0)
       #def show
    
    def get_cnts(self, what=''):
        M,m = self.__class__,self
        
        reNotWdChar = re.compile(r'\W')
        def test_fltr(fltr_s, op, oi):
            if not fltr_s:                                  return True
            pass;              #LOG and log('fltr_s, op, oi[!]={}',(fltr_s, op, oi['!']))
            if '!!!' in fltr_s and '!!!' not in oi['!']:    return False
            if '!!'  in fltr_s and '!!'  not in oi['!']:    return False
            pass;              #LOG and log('skip !!',())
            if  '!'  in fltr_s and  '!'  not in oi['!']:    return False
            pass;              #LOG and log('skip !',())
            text    = op \
                    + (' '+oi.get('cmt', '') if '#' in fltr_s else '')
            text    = text.upper()
            fltr_s  = fltr_s.replace('!', '').replace('#', '').upper()
            if '<' in fltr_s or '>' in fltr_s:
                text    = '·' + reNotWdChar.sub('·', text)    + '·'
                fltr_s  = ' ' + fltr_s + ' '
                fltr_s  = fltr_s.replace(' <', ' ·').replace('> ', '· ')
            pass;              #LOG and log('fltr_s, text={}',(fltr_s, text))
            return all(map(lambda c:c in text, fltr_s.split()))
           #def test_fltr

        def get_tbl_cols(sorts, col_ws):
            cnms    = list(M.COL_NMS)
            cnms[M.COL_FIL] = f(cnms[M.COL_FIL], m.ed.get_prop(app.PROP_TAB_TITLE))
            cols    = [d(nm=sorts_sign(sorts, c) + cnms[c]
                        ,wd=col_ws[c]
                        ,mi=M.COL_MWS[c]
                        )   for c in range(M.COL_N)]
            cols[M.COL_OVR]['al']   = 'C'
            if m.how.get('hide_fil', False):
                pos_fil = M.COL_NMS.index(M.COL_FILE)
                cols[pos_fil]['vi'] = False
            if m.how.get('hide_lex_fil', False):
                pos_lex = M.COL_NMS.index(M.COL_LEXR)
                pos_fil = M.COL_NMS.index(M.COL_FILE)
                cols[pos_lex]['vi'] = False
                cols[pos_fil]['vi'] = False
            return cols
           #def get_tbl_cols
        
        def get_tbl_data(opts_full, cond_s, ops_only, sorts, col_ws):
            # Filter table data
            pass;              #LOG and log('cond_s={}',(cond_s))
            pass;              #log('opts_full/tab_s={}',({o:oi for o,oi in opts_full.items() if o.startswith('tab_s')}))
            chp_cond    = ''
            chp_no_c    = False
            if  '@' in cond_s:
                # Prepare to match chapters
                chp_cond    = ' '.join([mt.group(1) for mt in re.finditer(r'@([\w/]+)'    , cond_s)]).upper()   # @s+ not empty chp
                chp_cond    = chp_cond.replace(M.NO_CHAP.upper(), '').strip()
                chp_no_c    = '@'+M.NO_CHAP in cond_s
                cond_s      =                                 re.sub(     r'@([\w/]*)', '', cond_s)             # @s* clear @ and cph
            pass;              #log('chp_cond, chp_no_c, cond_s={}',(chp_cond, chp_no_c, cond_s))
            SKWULFs = [  (oi.get('chp','') 
                         ,op
                         ,oi['!']
                         ,str(oi.get('jdf' ,'')).replace('True', 'true').replace('False', 'false')
                         ,str(oi.get('juvl','')).replace('True', 'true').replace('False', 'false')
                         ,str(oi.get('jlvl','')).replace('True', 'true').replace('False', 'false')
                         ,str(oi.get('jfvl','')).replace('True', 'true').replace('False', 'false')
                         ,oi['frm']
                         )
                            for op,oi in opts_full.items()
#                           if  (not chp_cond   or      chp_cond in oi.get('chp', '').upper())
                            if  (not chp_cond   or any((chp_cond in oi.get('chp', '').upper()) for chp_cond in chp_cond.split()))
                            and (not chp_no_c   or not oi.get('chp', ''))
                            and (not cond_s     or test_fltr(cond_s, op, oi))
                            and (not ops_only   or op in ops_only)
                      ]
            # Sort table data
            SKWULFs     = sorts_sort(sorts, SKWULFs)
            # Fill table
            pass;              #LOG and log('M.COL_NMS,col_ws,M.COL_MWS={}',(len(M.COL_NMS),len(col_ws),len(M.COL_MWS)))
            cols    = get_tbl_cols(sorts, col_ws)

            itms    = (list(zip([_('Section'),_('Option'), '', _('Default'), _('User'), _('Lexer'), _('File')], map(str, col_ws)))
                     #,         [ (str(n)+':'+sc,k         ,w    ,dv           ,uv         ,lv          ,fv)    # for debug
                     #,         [ (sc+' '+fm    ,k         ,w    ,dv           ,uv         ,lv          ,fv)    # for debug
                      ,         [ (sc           ,k         ,w    ,dv           ,uv         ,lv          ,fv)    # for user
                        for  n,(   sc           ,k         ,w    ,dv           ,uv         ,lv          ,fv, fm) in enumerate(SKWULFs) ]
                      )
            return SKWULFs, cols, itms
           #def get_tbl_data
           
        if not what or '+lvls' in what:
            m.SKWULFs,\
            m.cols  ,\
            m.itms  = get_tbl_data(m.opts_full, m.cond_s, m.ops_only, m.sorts, m.col_ws)
            if 'stbr' in dir(m):
                m.stbr_act(M.STBR_FLT, len(m.SKWULFs))

        if '+cols' in what:
            pass;              #LOG and log('m.col_ws={}',(m.col_ws))
            m.cols  = get_tbl_cols(m.sorts, m.col_ws)
            pass;              #LOG and log('m.cols={}',(m.cols))
        
        # Prepare [Def]Val data by m.cur_op
        vis,ens,vas,its,bcl = m._prep_opt()
        
        ed_s_c  = _('>Fil&e:')  if m.for_ulf=='f' else \
                  _('>L&exer:') if m.for_ulf=='l' else \
                  _('>Us&er:')
        cnts    = []
        if '+cond' in what:
            cnts   += [0
            ,('cond',d(items=m.cond_hl))
            ][1:]

        if '+cols' in what or '=cols' in what:
            cnts   += [0
            ,('lvls',d(cols=m.cols))
            ][1:]
        if '+lvls' in what or '=lvls' in what:
            cnts   += [0
            ,('lvls',d(cols=m.cols, items=m.itms))
            ][1:]

        tofi_en = not m.how.get('only_for_ul', not ens['tofi'])         # Forbid to switch fo File ops
        if '+cur' in what:
            cnts   += [0
            ,('ed_s',d(cap=ed_s_c                       ,hint=m.cur_op      ))
#           ,('eded',d(vis=vis['eded']                    ,sto=ens['eded']  ,color=bcl['eded']  ))
#           ,('eded',d(vis=vis['eded'],ex0=not ens['eded'],sto=ens['eded']  ,color=bcl['eded']  ))
#           ,('eded',d(vis=vis['eded'],en=ens['eded']                       ,color=bcl['eded']  ))
            ,('eded',d(vis=vis['eded'],en=ens['eded']                       ))
            ,('edcl',d(vis=vis['edcl']                                      ,color=bcl['edcl']  ))
            ,('edcb',d(vis=vis['edcb']                  ,items=its['edcb']  ))
            ,('edrf',d(vis=vis['edrf']                                      ))
            ,('edrt',d(vis=vis['edrt']                                      ))
            ,('brow',d(vis=vis['brow']                                      ))
            ,('toop',d(vis=vis['toop']                                      ))
            ,('opjs',d(vis=vis['opjs']                                      ))
            ,('dfv_',d(                                  hint=m.cur_op      ))
            ,('dfvl',d(                                                     ))
#           ,('dfvl',d(                en=ens['dfvl']                       ,color=bcl['dfvl']  ))
            ,('dfcl',d(vis=vis['dfcl']                                      ,color=bcl['dfcl']  ))
            ,('setd',d(                en=ens['setd']                       ))
            ,('tofi',d(                en=tofi_en                           ))
            ][1:]

        if what and cnts:
            # Part info
            return cnts

        # Full dlg controls info    #NOTE: cnts
        edit_h  = get_gui_height('edit')
        cmnt_t  = m.dlg_h-m.h_cmnt-5-M.STBR_H
        tofi_c  = m.ed.get_prop(app.PROP_TAB_TITLE)
        co_tp   = 'ed' if m.live_fltr else 'cb'
        cnts    = [0                                                                                                                        #
    # Hidden buttons                                                                                                                    
 ,('flt-',d(tp='bt' ,cap='&l'   ,sto=False              ,t=-99,l=0,w=44))  # &l
 ,('fltr',d(tp='bt' ,cap=''     ,sto=False  ,def_bt='1' ,t=-99,l=0,w=44))  # Enter
 ,('srt0',d(tp='bt' ,cap='&1'   ,sto=False              ,t=-99,l=0,w=44))  # &1
 ,('srt1',d(tp='bt' ,cap='&2'   ,sto=False              ,t=-99,l=0,w=44))  # &2
 ,('srt2',d(tp='bt' ,cap='&3'   ,sto=False              ,t=-99,l=0,w=44))  # &3
 ,('srt3',d(tp='bt' ,cap='&4'   ,sto=False              ,t=-99,l=0,w=44))  # &4
 ,('srt4',d(tp='bt' ,cap='&5'   ,sto=False              ,t=-99,l=0,w=44))  # &5
 ,('srt5',d(tp='bt' ,cap='&6'   ,sto=False              ,t=-99,l=0,w=44))  # &6
 ,('srt6',d(tp='bt' ,cap='&7'   ,sto=False              ,t=-99,l=0,w=44))  # &7
 ,('srt-',d(tp='bt' ,cap='&9'   ,sto=False              ,t=-99,l=0,w=44))  # &9
 ,('cws-',d(tp='bt' ,cap='&W'   ,sto=False              ,t=-99,l=0,w=44))  # &w
 ,('cpnm',d(tp='bt' ,cap='&C'   ,sto=False              ,t=-99,l=0,w=44))  # &c
 ,('erpt',d(tp='bt' ,cap='&O'   ,sto=False              ,t=-99,l=0,w=44))  # &o
 ,('apnw',d(tp='bt' ,cap='&Y'   ,sto=False              ,t=-99,l=0,w=44))  # &y
 ,('help',d(tp='bt' ,cap='&H'   ,sto=False              ,t=-99,l=0,w=44))  # &h
    # Top-panel                                                                                                             
 ,('ptop',d(tp='pn' ,h=    270 ,w=m.dlg_w               ,ali=ALI_CL                                                         
                    ,h_min=270                                                                                                  ))
    # Menu                                                                                                                      
 ,('menu',d(tp='bt' ,tid='cond' ,l=-40-5,w=  40 ,p='ptop'   ,cap='&='                                               ,a='LR'     ))  # &=
    # Filter                                                                                                                    
 ,('chps',d(tp='bt' ,tid='cond' ,l=-270 ,r=-180 ,p='ptop'   ,cap=_('+&Section…')    ,hint=M.CHPS_H                  ,a='LR'     ))  # &s
 ,('flt_',d(tp='lb' ,tid='cond' ,l=   5 ,w=  70 ,p='ptop'   ,cap='>'+M.FILTER_C+':' ,hint=M.FLTR_H                              ))  # &f
 ,('cond',d(tp=co_tp,t=  5      ,l=  78 ,r=-270 ,p='ptop'   ,items=m.cond_hl                                        ,a='lR'     ))  #
#,('cond',d(tp='cb' ,t=  5      ,l=  78 ,r=-270 ,p='ptop'   ,items=m.cond_hl                                        ,a='lR'     ))  #
    # Table of keys+values                                                                                                  
 ,('lvls',d(tp='lvw',t= 35,h=160,l=   5 ,r=  -5 ,p='ptop'   ,items=m.itms,cols=m.cols   ,grid='1'                   ,a='tBlR'   ))  #
    # Editors for value                                                                                                         
 ,('ed_s',d(tp='lb' ,t=210      ,l=   5 ,w=  70 ,p='ptop'   ,cap=ed_s_c             ,hint=m.cur_op                  ,a='TB'     ))  # &e 
 ,('eded',d(tp='ed' ,tid='ed_s' ,l=  78 ,r=-270 ,p='ptop'                           ,vis=vis['eded'],ex0=not ens['eded'],a='TBlR'   ))  #
#,('eded',d(tp='ed' ,tid='ed_s' ,l=  78 ,r=-270 ,p='ptop'                           ,vis=vis['eded'],en=ens['eded'] ,a='TBlR'   ))  #
 ,('edcl',d(tp='clr',t=210-2    ,l= 210 ,r=-271 ,p='ptop'   ,h=edit_h-4             ,vis=vis['edcl'],border=True    ,a='TBlR'   ))  #
 ,('edcb',d(tp='cbr',tid='ed_s' ,l=  78 ,r=-270 ,p='ptop'   ,items=its['edcb']      ,vis=vis['edcb']                ,a='TBlR'   ))  #
 ,('edrf',d(tp='ch' ,tid='ed_s' ,l=  78 ,w=  60 ,p='ptop'   ,cap=_('f&alse')        ,vis=vis['edrf']                ,a='TB'     ))  # &a
 ,('edrt',d(tp='ch' ,tid='ed_s' ,l= 140 ,w=  60 ,p='ptop'   ,cap=_('t&rue')         ,vis=vis['edrt']                ,a='TB'     ))  # &r
 ,('brow',d(tp='bt' ,tid='ed_s' ,l=-270 ,w=  90 ,p='ptop'   ,cap=_('&...')          ,vis=vis['brow']                ,a='TBLR'   ))  # &.
 ,('toop',d(tp='bt' ,tid='ed_s' ,l=-270 ,w=  90 ,p='ptop'   ,cap=_('&GoTo')         ,vis=vis['toop'],hint=M.TOOP_H  ,a='TBLR'   ))  # &g
 ,('opjs',d(tp='bt' ,tid='ed_s' ,l=-270 ,w=  90 ,p='ptop'   ,cap=_('E&dit')         ,vis=vis['opjs'],hint=M.OPME_H  ,a='TBLR'   ))  # &d
    # View def-value                                                                                                            
 ,('dfv_',d(tp='lb' ,tid='dfvl' ,l=   5 ,w=  70 ,p='ptop'   ,cap=_('>Defa&ult:')    ,hint=m.cur_op                  ,a='TB'     ))  # &u
#,('dfvl',d(tp='ed' ,t=235      ,l=  78 ,r=-270 ,p='ptop'   ,en=False               ,sto=False                      ,a='TBlR'   ))  #
 ,('dfvl',d(tp='ed' ,t=235      ,l=  78 ,r=-270 ,p='ptop'   ,ex0=True               ,sto=False                      ,a='TBlR'   ))  #
#,('dfvl',d(tp='ed' ,t=235      ,l=  78 ,r=-270 ,p='ptop'   ,ro_mono_brd='1,0,1'    ,sto=False                      ,a='TBlR'   ))  #
 ,('dfcl',d(tp='clr',t=235+1    ,l= 210 ,r=-271 ,p='ptop'   ,h=edit_h-4             ,vis=vis['dfcl'],border=True    ,a='TBlR'   ))  #
 ,('setd',d(tp='bt' ,tid='dfvl' ,l=-270 ,w=  90 ,p='ptop'   ,cap=_('Rese&t')                        ,en=ens['setd'] ,a='TBLR'   ))  # &t
    # For lexer/file                                                                                                            
#,('to__',d(tp='lb' ,tid='ed_s' ,l=-170 ,w=  30 ,p='ptop'   ,cap=_('>For:')                                         ,a='TBLR'   ))  # 
 ,('to__',d(tp='lb' ,tid='ed_s' ,l=-165 ,w=  30 ,p='ptop'   ,cap=_('For:')                                          ,a='TBLR'   ))  # 
 ,('tolx',d(tp='ch' ,tid='ed_s' ,l=-140 ,w=  70 ,p='ptop'   ,cap=_('Le&xer')                                        ,a='TBLR'   ))  # &x
 ,('tofi',d(tp='ch' ,tid='ed_s' ,l=- 90 ,w=  70 ,p='ptop'   ,cap=_('F&ile')         ,hint=tofi_c    ,en=tofi_en     ,a='TBLR'   ))  # &i
 ,('lexr',d(tp='cbr',tid='dfvl' ,l=-165 ,w= 160 ,p='ptop'   ,items=m.lexr_w_l                                       ,a='TBLR'   ))
    # Comment                                                                                                               
 ,('cmsp',d(tp='sp' ,y=cmnt_t-5                         ,ali=ALI_BT,sp_lr=5                                                     ))
 ,('cmnt',d(tp='me' ,t=cmnt_t   ,h=    m.h_cmnt                                                                                 
                                ,h_min=M.CMNT_MHT       ,ali=ALI_BT,sp_lrb=5       ,ro_mono_brd='1,1,1'                         ))
 ,('stbr',d(tp='sb' ,y=-M.STBR_H                                                                                                
                    ,h= M.STBR_H                        ,ali=ALI_BT                                                             ))
                ][1:]
        if 'mac'==get_desktop_environment():
            cnts    = [(cid,cnt) for cid,cnt in cnts if cnt.get('cap', '')[:3]!='srt']
        cnts    = odict(cnts)
        if m.how.get('hide_fil', False):
            for cid in ('tofi',):
                cnts[cid]['vis'] = False
        if m.how.get('hide_lex_fil', False):
            for cid in ('to__', 'tolx', 'lexr', 'tofi'):
                cnts[cid]['vis'] = False
        for cnt in cnts.values():
            if 'l' in cnt:  cnt['l']    = m.dlg_w+cnt['l'] if cnt['l']<0 else cnt['l']
            if 'r' in cnt:  cnt['r']    = m.dlg_w+cnt['r'] if cnt['r']<0 else cnt['r']
            if 'y' in cnt:  cnt['y']    = m.dlg_h+cnt['y'] if cnt['y']<0 else cnt['y']
        
        cnts['menu']['call']            = m.do_menu
        cnts['chps']['call']            = m.do_menu
        cnts['cpnm']['call']            = m.do_menu
        cnts['erpt']['call']            = m.do_menu
        cnts['apnw']['call']            = m.do_menu
        cnts['flt-']['call']            = m.do_fltr
        cnts['fltr']['call']            = m.do_fltr
        if m.live_fltr:
            cnts['cond']['call']        = m.do_fltr
        cnts['lexr']['call']            = m.do_lxfi
        cnts['tolx']['call']            = m.do_lxfi
        cnts['tofi']['call']            = m.do_lxfi
        cnts['lvls']['call']            = m.do_sele
        cnts['lvls']['on_click_header'] = m.do_sort
        cnts['srt0']['call']            = m.do_sort
        cnts['srt1']['call']            = m.do_sort
        cnts['srt2']['call']            = m.do_sort
        cnts['srt3']['call']            = m.do_sort
        cnts['srt4']['call']            = m.do_sort
        cnts['srt5']['call']            = m.do_sort
        cnts['srt6']['call']            = m.do_sort
        cnts['srt-']['call']            = m.do_sort
        cnts['cmsp']['call']            = m.do_cust
        cnts['cws-']['call']            = m.do_cust
        cnts['lvls']['on_click_dbl']    = m.do_dbcl   #lambda idd,idc,data:print('on dbl d=', data)
        cnts['setd']['call']            = m.do_setv
        cnts['edcb']['call']            = m.do_setv
        cnts['edrf']['call']            = m.do_setv
        cnts['edrt']['call']            = m.do_setv
        cnts['brow']['call']            = m.do_setv
        cnts['toop']['call']            = m.do_setv
        cnts['opjs']['call']            = m.do_setv
        cnts['help']['call']            = m.do_help
        return cnts
       #def get_cnts
    
    def get_vals(self, what=''):
        M,m = self.__class__,self
        m.cur_in    = m._prep_opt('key2ind')
        if not what or 'cur' in what:
            vis,ens,vas,its,bcl = m._prep_opt()
        if not what:
            # all
            return dict(cond=m.cond_s
                       ,lvls=m.cur_in
                       ,eded=vas['eded']
                       ,edcb=vas['edcb']
                       ,edrf=vas['edrf']
                       ,edrt=vas['edrt']
                       ,dfvl=vas['dfvl']
                       ,cmnt=vas['cmnt']
                       ,tolx=m.for_ulf=='l'
                       ,tofi=m.for_ulf=='f'
                       ,lexr=m.lexr_l.index(m.lexr)     if m.lexr in m.lexr_l else -1
                       )
        if '+' in what:
            rsp = {}
            if '+lvls' in what:
                rsp.update(dict(
                        lvls=m.cur_in
                        ))
            if '+cur' in what:
                rsp.update(dict(
                        eded=vas['eded']
                       ,edcb=vas['edcb']
                       ,edrf=vas['edrf']
                       ,edrt=vas['edrt']
                       ,dfvl=vas['dfvl']
                       ,cmnt=vas['cmnt']
                       ))
            if '+inlxfi' in what:
                rsp.update(dict(
                        tolx=m.for_ulf=='l'
                       ,tofi=m.for_ulf=='f'
                       ))
            pass;              #LOG and log('rsp={}',(rsp))
            return rsp
                    
        if what=='lvls':
            return dict(lvls=m.cur_in
                       )
        if what=='lvls-cur':
            return dict(lvls=m.cur_in
                       ,eded=vas['eded']
                       ,edcb=vas['edcb']
                       ,edrf=vas['edrf']
                       ,edrt=vas['edrt']
                       ,dfvl=vas['dfvl']
                       ,cmnt=vas['cmnt']
                       )
        if what=='cur':
            return dict(eded=vas['eded']
                       ,edcb=vas['edcb']
                       ,edrf=vas['edrf']
                       ,edrt=vas['edrt']
                       ,dfvl=vas['dfvl']
                       ,cmnt=vas['cmnt']
                       )
       #def get_vals
    
    def do_resize(self, ag):
        M,m = self.__class__,self
        m.stbr_act(M.STBR_MSG, '')
        f_w     = ag.fattr('w')
        l_w     = ag.cattr('lvls', 'w')
        pass;                  #LOG and log('f_w,l_w={}',(f_w,l_w))
        if f_w < m.dlg_min_w:           return []   # fake event
        
        m.col_ws= [ci['wd'] for ci in m.ag.cattr('lvls', 'cols')]
        if f_w == m.dlg_min_w and m.col_ws!=M.COL_MWS:
            return m.do_cust('cws-', ag)

        sum_ws  = sum(m.col_ws)
        pass;                  #LOG and log('l_w,sum_ws={}',(l_w,sum_ws))
        if sum_ws >= (l_w - M.COL_N - M.SCROLL_W):return []   # decrease dlg - need user choice
        
        # Auto increase widths of def-val and user-val cols
        extra   = int((l_w - M.COL_N - M.SCROLL_W - sum_ws)/2)
        pass;                  #LOG and log('extra={}',(extra))
        pass;                  #LOG and log('m.col_ws={}',(m.col_ws))
        m.col_ws[3] += extra
        m.col_ws[4] += extra
        pass;                  #LOG and log('m.col_ws={}',(m.col_ws))
        return d(ctrls=m.get_cnts('+cols'))
       #def do_resize
    
    def do_cust(self, aid, ag, data=''):
        M,m = self.__class__,self
        m.stbr_act(M.STBR_MSG, '')
        pass;                  #LOG and log('aid={}',(aid))
        if False:pass
        elif aid=='cmsp':
            # Splitter moved
            sp_y    = ag.cattr('cmsp', 'y')
            return []
            ##??
            
        elif aid=='cws-':
            # Set def col widths
            m.col_ws    = M.COL_MWS[:]
            m.stores.pop(m.subset+'col_ws', None)
            return d(ctrls=m.get_cnts('+cols'))

        elif aid=='vali':
            if dlg_valign_consts():
                return d(ctrls=m.get_cnts())
            return []

        elif aid=='rslt':
            # Restore dlg/ctrls sizes
            fpr         = ag.fattrs()
            layout      = data
            m.col_ws    = layout.get('col_ws', m.col_ws)
            cmnt_h      = layout.get('cmnt_h', ag.cattr('cmnt', 'h'))
            dlg_h       = layout.get('dlg_h' , fpr['h'])
            dlg_w       = layout.get('dlg_w' , fpr['w'])
            return  d(ctrls=
                         m.get_cnts('+cols')+
                        [('cmnt', d(h=cmnt_h))
                        ,('stbr', d(y=dlg_h))   # Hack to push it at bottom (by Alex)
                    ],form=d(
                         h=dlg_h
                        ,w=dlg_w
                    ))
        elif aid=='svlt':
            # Save dlg/ctrls sizes
            m.col_ws        = [ci['wd'] for ci in m.ag.cattr('lvls', 'cols')]
            layout          = data
            fpr             = ag.fattrs()
            layout['dlg_w'] = fpr['w']
            layout['dlg_h'] = fpr['h']
            layout['cmnt_h']= ag.cattr('cmnt', 'h')
            layout['col_ws']= m.col_ws
       #def do_cust
    
    def do_menu(self, aid, ag, data=''):
        pass;                  #LOG and log('aid={}',(aid))
        M,m = self.__class__,self
        m.stbr_act(M.STBR_MSG, '')

        scam    = app.app_proc(app.PROC_GET_KEYSTATE, '')
        if scam=='c' and aid=='menu':
            return m.do_cust('vali', ag)

        def wnen_menu(ag, tag):
            pass;              #LOG and log('tag={}',(tag))
            if False:pass
            
            elif tag[:3]=='ch:':
                return m.do_fltr('chps', ag, tag[3:])

            elif tag=='srt-':
                return m.do_sort('', ag, -1)
            elif tag[:3]=='srt':
                return m.do_sort('', ag, int(tag[3]))
            
            elif tag=='cws-':
                return m.do_cust(tag, ag)
            elif tag=='vali':
                return m.do_cust(tag, ag)
            
#           elif tag=='lubk':
#               if app.ID_OK != app.msg_box(
#                               _('Restore user settings from backup copy?')
#                               , app.MB_OKCANCEL+app.MB_ICONQUESTION): return []
#               return m.do_file('restore-user')
#           elif tag=='llbk':
#               if app.ID_OK != app.msg_box(
#                               f(_('Restore lexer "{}" settings from backup copy?'), m.lexr)
#                               , app.MB_OKCANCEL+app.MB_ICONQUESTION): return []
#               return m.do_file('restore-lexr')
#           elif tag=='dobk':
#               m.stores[m.subset+'bk_sets'] = m.bk_sets = not m.bk_sets
#               return []
            
        #   elif tag=='dfns':
        #       m.col_ws    = [ci['wd'] for ci in m.ag.cattr('lvls', 'cols')]
        #       new_file    = app.dlg_file(True, m.defn_path.name, str(m.defn_path.parent), 'JSONs|*.json')
        #       if not new_file or not os.path.isfile(new_file):    return []
        #       return m.do_file('set-dfns', new_file)
            elif tag=='full':
                m.col_ws    = [ci['wd'] for ci in m.ag.cattr('lvls', 'cols')]
                m.all_ops   = not m.all_ops
                m.opts_full = load_vals(m.opts_defn
                                       ,lexr_json='lexer '+m.lexr+'.json'
                                       ,user_json=m.how.get('stor_json', 'user.json')
                                       , ed_=m.ed, full=m.all_ops)
                m.cur_op    = m.cur_op if m.cur_op in m.opts_full else ''
                m.do_file('build-chp-tree')
                m.stbr_act(M.STBR_ALL, len(m.opts_full))
                return d(ctrls=odict(m.get_cnts('+lvls +cur')))
            
            if tag=='apex':
                m.apply_one = not m.apply_one
                m.stores[m.subset+'apply_one']  = m.apply_one
            if tag=='apnw':
                ed.cmd(cmds.cmd_OpsReloadAndApply)
            if tag=='aufi':
                m.auto4file = not m.auto4file
                m.stores[m.subset+'auto4file']  = m.auto4file
        
            if tag=='lifl':
                m.stores[m.subset+'live_fltr']  = not m.stores.get(m.subset+'live_fltr' , False)
                M.restart       = True
                M.restart_cond  = ag.cval('cond')
                return None         # Close dlg
        
            elif tag=='cpnm':
                app.app_proc(app.PROC_SET_CLIP, m.cur_op)
            elif tag=='erpt':
                body    = '\n'.join(m.chng_rpt)
                dlg_wrapper(_('Сhange log')       ,  500+10     ,400+10, 
                    [ dict(cid='body',tp='me' ,l=5,w=500  ,t=5,h=400, ro_mono_brd='1,0,0')]
                    , dict(body=body), focus_cid='body')
            elif tag=='locv':
        #       m.do_file('locate-opt')                     # while wait core fix
                if m.do_file('goto-opt'):   return None     #   need close dlg
            elif tag=='locd':
        #       m.do_file('locate-def')                     # while wait core fix
                if m.do_file('goto-def'):   return None     #   need close dlg

            elif tag[:4] in ('rslt', 'rmlt', 'svlt'):
                layouts_l   = m.stores.get(m.subset+'layouts', [])  # [{nm:Nm, dlg_h:H, dlg_w:W, ...}]
                layouts_d   = {lt['nm']:lt for lt in layouts_l}
                lt_i        = int(tag[4:])      if tag[:4] in ('rslt', 'rmlt')  else -1
                layout      = layouts_l[lt_i]   if lt_i>=0                      else None
                if 0:pass
                elif tag[:4]=='rmlt':
                    if  app.ID_OK != app.msg_box(
                                f(_('Remove layout "{}"?'), layout['nm'])
                                , app.MB_OKCANCEL+app.MB_ICONQUESTION): return []
                    del layouts_l[lt_i]
                elif tag=='svlt':
                    nm_tmpl     = _('#{}')
                    layout_nm   = f(nm_tmpl
                                   ,first_true(itertools.count(1+len(layouts_d))
                                            ,pred=lambda n:f(nm_tmpl, n) not in layouts_d))     # First free #N after len()
                    while True:
                        pass;  #LOG and log('layout_nm={!r}',(layout_nm))
                        layout_nm   = app.dlg_input('Name to save current sizes of the dialog and controls', layout_nm)
                        if not layout_nm:   return []
                        layout_nm   = layout_nm.strip()
                        if not layout_nm:   return []
                        if layout_nm in layouts_d and \
                            app.ID_OK != app.msg_box(
                                    f(_('Name "{}" already used. Overwrite?'), layout_nm)
                                    , app.MB_OKCANCEL+app.MB_ICONQUESTION): continue
                        break
                    layout      = None
                    if layout_nm in layouts_d:
                        layout  = layouts_d[layout_nm]  # Overwrite
                    else:
                        layout  = d(nm=layout_nm)       # Create
                        layouts_l+=[layout]
                    # Fill
                    m.do_cust(       'svlt', ag, layout)
                elif tag[:4]=='rslt':
                    return m.do_cust('rslt', ag, layout)
                # Save
                m.stores[m.subset+'layouts']    = layouts_l
                return []
            
            elif tag=='rprt':
                m.do_file('out-rprt')
            elif tag=='help':
                return m.do_help('', ag)
            return []
           #def wnen_menu
        pass;                  #LOG and log('',())

        if aid=='chps':
            def tree2menu(node, chp=''):
                mn_l    = [    d( tag='ch:'+                node['path']
                                , cap=f('{} ({})', chp, len(node['ops'])) 
                                , cmd=wnen_menu)
                              ,d( cap='-')
                          ] if chp else []
                for chp,kid in                              node['kids'].items():
                    mn_l   +=([d( cap=f('{} ({})', chp, len(kid['ops']))               
                                , sub=tree2menu(kid, chp))
                              ]
                                if 'kids' in kid else
                              [d( tag='ch:'+                kid['path']
                                , cap=f('{} ({})', chp, len(kid['ops']))               
                                , cmd=wnen_menu)
                              ]
                             )  
                return mn_l
               #def tree2menu
            mn_its  = tree2menu(m.chp_tree)
            ag.show_menu('chps', mn_its)

        if aid=='apnw': return wnen_menu(ag, aid)
        if aid=='cpnm': return wnen_menu(ag, aid)
        if aid=='erpt': return wnen_menu(ag, aid)
        
        if aid=='menu':
            locv_c  = f(M.LOCV_C, m.cur_op)
            locd_c  = f(M.LOCD_C, m.cur_op)
            lts_l   = m.stores.get(m.subset+'layouts', [])  # [{nm:Nm, dlg_h:H, dlg_w:W, ...}]
            full_en = not m.how.get('only_with_def', False) # Forbid to switch fo User+Lexer ops
            live_fltr=m.stores.get(m.subset+'live_fltr' , False)
            pass;              #lts_l   = [d(nm='Nm1'), d(nm='Nm2')]
            mn_its  = \
    [ d(tag='cpnm'              ,cap=_('&Copy option name')                                     ,key='Alt+C'
    ),d(                         cap='-'
    ),d(                         cap=_('&Layout')           ,sub=
        [ d(tag='svlt'              ,cap=_('&Save current layout...')
        ),d(                         cap='-'
        )]+     (
        [ d(tag='rslt'+str(nlt)     ,cap=f(_('Restore layout "{}"'), lt['nm']))         for nlt, lt in enumerate(lts_l)
        ]+
        [ d(                         cap=_('&Forget layout'),sub=
            [ d(tag='rmlt'+str(nlt)     ,cap=f(_('Forget layout "{}"...'), lt['nm']))   for nlt, lt in enumerate(lts_l)
            ])
        ]       if lts_l else []) +
        [ d(                         cap='-'
        ),d(tag='vali'              ,cap=_('Adjust vertical alignments...')
        ),d(tag='cws-'              ,cap=_('Set default columns &widths')                       ,key='Alt+W'
        )]
    ),d(                         cap=_('&Table')            ,sub=
        [ d(tag='srt'+str(cn)       ,cap=f(_('Sort by column "{}"'), cs.split()[0])
                                                                            ,ch=sorts_on(m.sorts, cn)
                                                                                                ,key='Alt+'+str(1+cn))
                                                            for cn, cs in enumerate(M.COL_NMS)
        ]+
        [ d(                         cap='-'
        ),d(tag='srt-'              ,cap=_('Reset sorting')                                     ,key='Alt+9'
        )]
    ),d(                         cap=_('M&ore')             ,sub=
        [ d(tag='locv'              ,cap=locv_c                             ,en=bool(m.cur_op)
        ),d(tag='locd'              ,cap=locd_c                             ,en=bool(m.cur_op)
        ),d(                         cap='-'
        ),d(tag='erpt'              ,cap=_('Show rep&ort of changes...')                        ,key='Alt+O'
        ),d(tag='apex'              ,cap=_('Apply changes on exit')         ,ch=m.apply_one
        ),d(tag='apnw'              ,cap=_('Appl&y changes now')            ,en=m.apply_need    ,key='Alt+Y'
        ),d(tag='aufi'              ,cap=_('Auto-update FILE options')      ,ch=m.auto4file
        ),d(                         cap='-'
        ),d(tag='lifl'              ,cap=M.LIFL_C                           ,ch=live_fltr
        ),d(                         cap='-'
        ),d(tag='full'              ,cap=M.FULL_C                           ,ch=m.all_ops   ,en=full_en
        )]
    ),d(                         cap='-'
    ),d(    tag='rprt'          ,cap=_('Create HTML &report')
    ),d(                         cap='-'
    ),d(    tag='help'          ,cap=_('&Help...')                                              ,key='Alt+H'
    )]
            pass;              #LOG and log('mn_its=¶{}',pf(mn_its))
            def add_cmd(its):
                for it in its:
                    if 'sub' in it: add_cmd(it['sub'])
                    else:                   it['cmd']=wnen_menu
            add_cmd(mn_its)
            ag.show_menu(aid, mn_its)
        return []
       #def do_menu

    def do_fltr(self, aid, ag, data=''):
        M,m = self.__class__,self
        m.stbr_act(M.STBR_MSG, '')
        m.col_ws= [ci['wd'] for ci in m.ag.cattr('lvls', 'cols')]

        fid     = ag.fattr('fid')
        pass;                  #LOG and log('aid,fid={}',(aid,fid))
        if aid=='fltr' and fid in ('dfvl', 'eded', 'edrf', 'edrt'):
            # Imitate default button
            return m.do_setv('setd' if fid in ('dfvl',)         else
                             'setv' if fid in ('eded',)         else
                             fid    if fid in ('edrf', 'edrt')  else
                             ''
                            , ag)
            
        if aid=='cond':
            pass;              #LOG and log('ag.cval(cond)={}',(ag.cval('cond')))
            m.cond_s    = ag.cval('cond')
            fid         = '' if m.live_fltr else 'lvls'
        if aid=='fltr':
            m.cond_s    = ag.cval('cond')
            m.cond_hl   = add_to_history(m.cond_s, m.cond_hl)       if m.cond_s and not m.live_fltr else m.cond_hl
            fid         = 'lvls'
        if aid=='flt-':
            m.cond_s    = ''
            fid         = 'cond'

        if aid=='chps':
            # Append selected chapter as filter value
            scam        = app.app_proc(app.PROC_GET_KEYSTATE, '')
            path        = '@'+data
            if path not in m.cond_s:
                if scam!='c':
                    m.cond_s= re.sub(r'@([\w/]*)', '', m.cond_s).strip()    # del old 
                m.cond_s    = (m.cond_s+' '+path).strip()                   # add new
                m.cond_hl   = add_to_history(m.cond_s, m.cond_hl)   if not m.live_fltr else m.cond_hl
            fid         = 'cond'

        # Select old/new op
        m.cur_op= m._prep_opt('ind2key')
        ctrls   = m.get_cnts('+lvls')
        m.cur_in= m._prep_opt('key2ind')
        if m.cur_in<0 and m.SKWULFs:
            # Sel top if old hidden
            m.cur_in= 0
            m.cur_op= m._prep_opt('ind2key', ind=m.cur_in)
        return d(ctrls=m.get_cnts('+cond =lvls +cur')
                ,vals =m.get_vals()
                ,form =d(fid=fid)
                )
               
       #def do_fltr
    
    def do_sort(self, aid, ag, col=-1):
        scam    = app.app_proc(app.PROC_GET_KEYSTATE, '')
        pass;                  #LOG and log('col,scam={}',(col,scam))
        pass;                  #return []
        M,m = self.__class__,self
        m.stbr_act(M.STBR_MSG, '')
        m.col_ws= [ci['wd'] for ci in m.ag.cattr('lvls', 'cols')]
        
        if aid=='srt-' or col==-1:
            m.sorts = sorts_dflt(len(M.COL_NMS))
        else:
            col     = int(aid[3]) if aid[:3]=='srt' else col
            pass;              #LOG and log('?? m.sorts={}',(m.sorts))
            m.sorts = sorts_turn(m.sorts, col, scam)
            pass;              #LOG and log('ok m.sorts={}',(m.sorts))

        old_in  = m._prep_opt('key2ind')
        ctrls   = m.get_cnts('+lvls')
        if old_in==0:
            # Set top if old was top
            m.cur_in= 0
            m.cur_op= m._prep_opt('ind2key', ind=m.cur_in)
        else:
            # Save old op
            m.cur_in= m._prep_opt('key2ind')
        return d(ctrls=m.get_cnts('=lvls +cur')
                ,vals =m.get_vals()
                )
       #def do_sort

    def do_sele(self, aid, ag, data=''):
        M,m = self.__class__,self
        m.stbr_act(M.STBR_MSG, '')
        pass;                  #LOG and log('data,m.cur_op,m.cur_in={}',(data,m.cur_op,m.cur_in))
        m.cur_op= m._prep_opt('ind2key')
        pass;                  #LOG and log('m.cur_op,m.cur_in={}',(m.cur_op,m.cur_in))
        pass;                  #log('###m.get_cnts(+cur)={}',(m.get_cnts('+cur')))
        return d(ctrls=odict(m.get_cnts('+cur'))
                ,vals =      m.get_vals('cur')
                )
       #def do_sele
    
    def do_lxfi(self, aid, ag, data=''):
        M,m = self.__class__,self
        m.stbr_act(M.STBR_MSG, '')
        pass;                  #LOG and log('aid={}',(aid))
        m.col_ws= [ci['wd'] for ci in m.ag.cattr('lvls', 'cols')]

        if False:pass
        elif aid in ('tolx', 'tofi'):
            # Changed "For Lexer/File"
            m.for_ulf   = 'l' if aid=='tolx' and ag.cval('tolx') else \
                          'f' if aid=='tofi' and ag.cval('tofi') else \
                          'u'
            fid         = 'lexr' \
                            if m.for_ulf=='l' and m.lexr not in m.lexr_l else \
                          m._prep_opt('fid4ed')
            return d(ctrls=m.get_cnts('+cur')
                    ,vals =m.get_vals('+cur+inlxfi')
                    ,form =d(fid=fid)
                    )
        elif aid=='lexr':
            # Change current lexer
            lexr_n  = ag.cval('lexr')
            m.lexr  = m.lexr_l[lexr_n]      if lexr_n>=0 else ''
            m.cur_op= m._prep_opt('ind2key')
            m.do_file('load-data')
            ctrls   = m.get_cnts('+lvls')
            m.cur_in= m._prep_opt('key2ind')
            if m.cur_in<0 and m.SKWULFs:
                # Sel top if old hidden
                m.cur_in= 0
                m.cur_op= m._prep_opt('ind2key', ind=m.cur_in)
            elif m.cur_in<0:
                m.cur_op= ''
            return d(ctrls=m.get_cnts('=lvls +cur')
                    ,vals =m.get_vals()#'+lvls +cur')
                    )
       #def do_lxfi
    
    def do_dbcl(self, aid, ag, data=''):
        M,m = self.__class__,self
        m.stbr_act(M.STBR_MSG, '')
        pass;                  #LOG and log('data,m.cur_op,m.cur_in={}',(data,m.cur_op,m.cur_in))
        m.col_ws= [ci['wd'] for ci in m.ag.cattr('lvls', 'cols')]

        if aid!='lvls':     return []
        # Dbl-click on lvls cell
        if sum(m.col_ws) > ag.cattr('lvls', 'w') - M.SCROLL_W:
            # Has hor-scrolling
            pass;              #LOG and log('skip as h-scroll',())
            return []
        op_r    = ag.cval('lvls')
        op_c    = next(filter(                              # next(filter())==first_true
                    lambda col_n_sw: col_n_sw[1]>data[0]    # > x from click (x,y)
                  , enumerate(accumulate(m.col_ws))         # (n_col, sum(col<=n))
                  ), [-1, -1
                  ])[0]
        pass;                  #LOG and log('op_r,op_c,m.cur_op,m.cur_in={}',(op_r,op_c,m.cur_op,m.cur_in))
        pass;                  #LOG and log('op_r,op_c={}',(op_r,op_c))
        if False:pass
        elif op_c not in (M.COL_DEF,M.COL_USR,M.COL_LXR,M.COL_FIL):
            return []
        elif -1==op_r:
            pass;              #LOG and log('skip as no opt',())
            return []
        elif -1==op_c:
            pass;              #LOG and log('skip as miss col',())
            return []
        elif M.COL_DEF==op_c:
            return d(form =d(fid='setd'))
        elif M.COL_USR==op_c and m.for_ulf!='u':
            # Switch to user vals
            m.for_ulf   = 'u'
        elif M.COL_LXR==op_c and m.for_ulf!='l':
            # Switch to lexer vals
            m.for_ulf   = 'l'
        elif M.COL_FIL==op_c and m.for_ulf!='f':
            # Switch to lexer vals
            m.for_ulf   = 'f'
        else:
            return []
        pass;                   LOG and log('op_r,op_c,m.for_ulf={}',(op_r,op_c,m.for_ulf))
        return d(ctrls=m.get_cnts('+cur')
                ,vals =m.get_vals('+cur+inlxfi')
                ,form =d(fid=m._prep_opt('fid4ed'))
                )
       #def do_dbcl
    
    def do_setv(self, aid, ag, data=''):
        M,m = self.__class__,self
        m.stbr_act(M.STBR_MSG, '')
        pass;                  #LOG and log('aid,m.cur_op={}',(aid,m.cur_op))
        if not m.cur_op:   return []
        m.col_ws= [ci['wd'] for ci in m.ag.cattr('lvls', 'cols')]

        if aid=='toop':
#           m.do_file('locate-opt')                     # while wait core fix
            if m.do_file('goto-opt'):   return None     #   need close dlg
            return []
        
        trg     = 'lexer '+m.lexr+'.json' if m.for_ulf=='l' else 'user.join'
        key4v   = m.for_ulf+'val'
        op      = m.cur_op
        oi      = m.opts_full[op]
        frm     = oi['frm']
#       if frm=='json':
#           m.stbr_act(M.STBR_MSG, f(_('Edit {!r} to change value'), trg))
#           return []
        dval    = oi.get( 'def')
        uval    = oi.get('uval')
        lval    = oi.get('lval')
        fval    = oi.get('fval')
        ulfvl   = oi.get(key4v ) #fval if m.for_ulf=='f' else lval if m.for_ulf=='l' else uval
        jval    = oi['jlvl']    if m.for_ulf=='l' else \
                  oi['juvl']    if m.for_ulf=='u' else \
                  oi['jfvl']
        scam    = app.app_proc(app.PROC_GET_KEYSTATE, '')
        
        # Get new value
        newv    = None
        erpt_s  = ''
        if False:pass
        
        elif aid=='setd'        and \
             m.for_ulf=='f'     and \
             op in apx.OPT2PROP:
            # Remove from file - set over def/user/lex val
            newv    = oi.get('lval', oi.get('uval', oi.get('def')))
            if newv==ulfvl:
                m.stbr_act(M.STBR_MSG, _('No need changes'))
                return []
            erpt_s  = 'reset-f'
            m.ed.set_prop(apx.OPT2PROP[op], newv)
            
        elif aid=='setd'        and \
             ulfvl is not None  and \
             m.for_ulf!='f':
            # Remove from user/lexer
            if  scam!='c' and \
                app.ID_OK != app.msg_box(f(_('Remove {} option'
                                            '\n   {} = {!r}'
                                            '\n?'), 'LEXER' if m.for_ulf=='l' else 'USER', op, jval)
                                        , app.MB_OKCANCEL+app.MB_ICONQUESTION): return []
            newv= None
        
        elif aid=='brow' and frm in ('hotk', 'file', '#rgb', '#rgb-e'):
            ulfvl_s = '' if ulfvl is None else ulfvl
            m.stbr_act(M.STBR_MSG, f(_('Default value: "{}". Old value: "{}"'), dval, ulfvl_s))
            if frm in ('#rgb', '#rgb-e'):
                ulfvl_s = ulfvl_s if ulfvl_s else dval if frm=='#rgb' else '#fff'
                newv    = app.dlg_color(apx.html_color_to_int(ulfvl_s))
                if newv is None:    return []
                newv    = apx.int_to_html_color(newv)
            else:
                newv= (app.dlg_hotkey(op)                                       if frm=='hotk' else
                       app.dlg_file(False, '', os.path.expanduser(ulfvl_s), '') if frm=='file' else None)
            m.stbr_act(M.STBR_MSG, '')
            if not newv:    return []
        
        elif aid=='opjs':
            newv    = edit_json_as_dict(op, ulfvl, dval, oi.get('cmt' , ''))
            if newv is None:    return []
        
        elif aid=='setv':                   # Add/Set opt for user/lexer/file
            # Enter from edit. Need parse some string
            newv    = m.ag.cval('eded')
            try:
                newv    =   int(newv)   if frm=='int'   else \
                          float(newv)   if frm=='float' else \
                                newv
            except Exception as ex:
                app.msg_box(f(_('Incorrect value. It\'s needed in format: {}'), frm)
                           , app.MB_OK+app.MB_ICONWARNING)
                return d(form=d(fid='eded'))
            if frm=='#rgb' or frm=='#rgb-e' and newv:       # Testing new val
                try:
                    apx.html_color_to_int(newv)
                except Exception as ex:
                    app.msg_box(f(_('Incorrect value. It\'s needed in format: {}'), '#RGB or #RRGGBB')
                               , app.MB_OK+app.MB_ICONWARNING)
                    return d(form=d(fid='eded'))
        elif aid in ('edrf', 'edrt'):       # Add/Set opt for user/lexer/file
            newv    = aid=='edrt'
            newv    = not newv if newv==ulfvl else newv
        elif aid=='edcb':                   # Add/Set opt into user/lexer/file
            pass;              #LOG and log('oi={}',(oi))
            vl_l    = [k for k,v in oi.get('dct', [])]  if 'dct' in oi else oi.get('lst', [])
            pass;              #LOG and log('vl_l={}',(vl_l))
            pass;              #LOG and log('m.ag.cval(edcb)={}',(m.ag.cval('edcb')))
            newv    = vl_l[m.ag.cval('edcb')]
            pass;              #LOG and log('newv={}',(newv))

        # Use new value to change env
        if newv is not None and newv==ulfvl:
            m.stbr_act(M.STBR_MSG, _('No need changes'))
            return []
        
        if m.for_ulf=='f' and newv is not None and op in apx.OPT2PROP:
            # Change for file
            erpt_s  = 'set-f'
            ed.set_prop(apx.OPT2PROP[op], newv)
            
        if m.for_ulf!='f':
            # Change target file
            pass;              #LOG and log('?? do_erpt',())
            erpt_s  =('reset-u' if newv  is None and m.for_ulf=='u' else
                      'reset-l' if newv  is None and m.for_ulf=='l' else
                      'add-u'   if ulfvl is None and m.for_ulf=='u' else
                      'add-l'   if ulfvl is None and m.for_ulf=='l' else
                      'set-u'   if                   m.for_ulf=='u' else
                      'set-l'   if                   m.for_ulf=='l' else '')
            pass;              #LOG and log('?? set_opt',())
            apx.set_opt(op
                       ,newv
                       ,apx.CONFIG_LEV_LEX  if m.for_ulf=='l' else apx.CONFIG_LEV_USER
                       ,ed_cfg  =None
                       ,lexer   =m.lexr     if m.for_ulf=='l' else None
                       ,user_json=m.how.get('stor_json', 'user.json')
                       )

            if not m.apply_one:
                pass;          #LOG and log('?? OpsReloadAndApply',())
                ed.cmd(cmds.cmd_OpsReloadAndApply)
            else:
                m.apply_need    = True
            
        # Use new value to change dlg data
        pass;                  #LOG and log('?? oi={}',(oi))
        pass;                  #LOG and log('?? m.opts_full={}',pf(m.opts_full))
        if False:pass
        elif aid=='setd':
            oi.pop(key4v, None)     if m.for_ulf!='f' else 0
        else:
            pass;              #LOG and log('key4v, newv={}',(key4v, newv))
            oi[key4v] = newv
        pass;                  #LOG and log('oi={}',(oi))
        upd_cald_vals(m.opts_full)
        pass;                  #LOG and log('oi={}',(oi))
        
        jnewv   = oi['jlvl']   if m.for_ulf=='l' else oi['juvl']    if m.for_ulf=='u' else oi['jfvl']
        m.do_erpt(erpt_s, jnewv, jval)
        pass;                  #LOG and log('ok oi={}',(oi))
        pass;                  #LOG and log('ok m.opts_full={}',pf(m.opts_full))
        
        pass;                  #LOG and log('?? get_cnts',())
        
        if m.for_ulf!='f' and m.auto4file and op in apx.OPT2PROP:
            # Change FILE to over
            newv    = oi.get('lval', oi.get('uval', oi.get('def')))
            if newv!=oi.get('fval'):
                erpt_s      = 'reset-f'
                m.ed.set_prop(apx.OPT2PROP[op], newv)
                oi['fval']  = newv
                jval        = oi['jfvl']
                upd_cald_vals(m.opts_full)
                jnewv       = oi['jfvl']
                m.do_erpt('auset-f', jnewv, jval)
        
        pass;                  #LOG and log('m.get_vals(lvls-cur)={}',(m.get_vals('lvls-cur')))
        return d(ctrls=m.get_cnts('+lvls+cur')
                ,vals =m.get_vals('lvls-cur')
                )
       #def do_setv

    def do_erpt(self, what='', jnewv=None, joldv=None):
        pass;                  #LOG and log('what, newv={}',(what, newv))
        M,m = self.__class__,self
        
        if 0==len(m.chng_rpt):
            rpt = f('Starting to change options at {:%Y-%m-%d %H:%M:%S}', datetime.datetime.now())
            m.chng_rpt += [rpt]
        
        oi  = m.opts_full[m.cur_op]
        oldv= None
        rpt = ''
        if 0:pass
        elif what=='reset-f':
            rpt     = f(_('Set FILE option to overridden value {!r}')       ,jnewv)
        elif what=='set-f':
            rpt     = f(_('Set FILE option to {!r}')                        ,jnewv)
        elif what=='auset-f':
            rpt     = f(_('Auto-set FILE option to overridden value {!r}')  ,jnewv)
        elif what=='reset-l':
            rpt     = f(_('Remove LEXER {!r} option')               ,m.lexr       )
        elif what=='set-l':
            rpt     = f(_('Set LEXER {!r} option to {!r}')          ,m.lexr ,jnewv)
        elif what=='add-l':
            rpt     = f(_('Add LEXER {!r} option {!r}')             ,m.lexr ,jnewv)
        elif what=='reset-u':
            rpt     = f(_('Remove USER option')                                   )
        elif what=='set-u':
            rpt     = f(_('Set USER option to {!r}')                        ,jnewv)
        elif what=='add-u':
            rpt     = f(_('Add USER option {!r}')                           ,jnewv)
        else:
            return 
        rpt         = f('{} (from {!r})', rpt, joldv) \
                        if what[:3]!='add' and joldv is not None else rpt
        rpt         = rpt.replace('True', 'true').replace('False', 'false')
        rpt         = m.cur_op + ': '               + rpt
        rpt         = f('{}. ', len(m.chng_rpt))  + rpt
#       print(rpt)
        m.stbr_act(M.STBR_MSG, rpt + _('   [Alt+O - all changes]'))
        m.chng_rpt += [rpt]
       #def do_erpt
    
    def do_help(self, aid, ag, data=''):
        M,m = self.__class__,self
        m.stbr_act(M.STBR_MSG, '')
        pass;                  #LOG and log('',())
        dlg_wrapper('Help'
        ,   680+10, 500+10 
        ,   [d(cid='body', tp='me', l=5, t=5, w=680, h=500, ro_mono_brd='1,1,0')]
        ,   d(      body=   #NOTE: help
                 f(
  _(  'About "{fltr}"'
    '\r '
   )
   +M.FLTR_H+
  _('\r '
    '\rOther tips.'
    '\r • Use ENTER to filter table and to change or reset value.'
    '\r • Use double click on any cell in columns'
    '\r     "{c_usr}"'
    '\r     "{c_lxr}"'
    '\r     "{c_fil}"'
    '\r   to change "{in_lxr}" flag and to put focus on the value field.'
    '\r • Use double click on any cell in column'
    '\r     "{c_def}"'
    '\r   to put focus on "{reset}".'
    '\r • Clicking "{reset}" will ask for confirmation, for user/lexer options.'
    '\r   Hold Ctrl key to skip this confirmation.'
    '\r • Click on a column header sorts data in the column.'
    '\r     Alt+# (# is 1..8) sorts the N column (not on macOS).'
    '\r     Alt+9 resets sorting (not on macOS).'
    '\r     Click with Ctrl allows to sort by several columns.'
    '\r     Clicking with Ctrl on already sorted column does 2-state loop (down, up).'
    '\r     Clicking with Ctrl on already sorted column with maximal sorting index, '
    '\r     does 3-state loop (down, up, off).'
    '\r • Use option "{lifl}" to see instant update of the list after'
    '\r   each changing in the filter field'
    '\r   (otherwise you need to press Enter after changing).'
    '\r   With this option, no history of the filter is kept'
    '\r   (filter combobox has empty dropdown list).'
    '\r • If current list line is scrolled out of view, '
    '\r   you can still see the option name - in the tooltip'
    '\r   of "User" (Lexer/File) label near the value field.'
    '\r • Tooltip shows file name (or tag name), when cursor hovers the checkbox "{tofi}".'
    '\r • Some plugins store their settings into user.json.'
    '\r   So after a while, user.json contains options not present in default.json.'
    '\r   To see all these keys, use option "{full}".'
    '\r • Values in table column "!"'
    '\r     !   option is set in "user.json",'
    '\r     !!  option is set in "lexer NNN.json",'
    '\r     !!! option is set for current file,'
    '\r     L   default value is from "settings_default/lexer NNN.json",'
    '\r     +   not CudaText standard option.'
   )             , c_usr=M.COL_NMS[M.COL_USR]
                 , c_lxr=M.COL_NMS[M.COL_LXR]
                 , c_fil=M.COL_NMS[M.COL_FIL].split()[0]
                 , c_def=M.COL_NMS[M.COL_DEF]
                 , fltr = ag.cattr('flt_', 'cap', live=False).replace('&', '').strip(':')
                 , in_lxr=ag.cattr('tolx', 'cap', live=False).replace('&', '')
                 , reset= ag.cattr('setd', 'cap', live=False).replace('&', '')
                 , tofi = ag.cattr('tofi', 'cap', live=False).replace('&', '')
                 , lifl = M.LIFL_C.replace('&', '')
                 , full = M.FULL_C.replace('&', '')
                 ))
        )
        return []
       #def do_help
    
    restart     = False
    restart_cond= None
   #class OptEdD


def edit_json_as_dict(op, uval, dval, cmnt4v):
    """ Allow user to edit JSON value
    """
    pass;                      #log("op, uval, dval={}",(op, uval, dval))
    newv    = None
    def acts(aid, ag, data=''):
        nonlocal newv
        if False:pass
        elif aid=='defv':
            return d(vals=d(meme=json.dumps(dval, indent=2)),fid='meme')
        elif aid=='undo':
            return d(vals=d(meme=json.dumps(uval, indent=2)),fid='meme')
        elif aid in ('test', 'okok'):
            mejs    = ag.cval('meme')
            pass;              #log("mejs={!r}",(mejs))
            try:
                jsvl    = json.loads(mejs, object_pairs_hook=odict)
            except Exception as ex:
                warn    = str(ex) + c10 + (c10.join('{:>3}|{}'.format(n+1, s.replace(' ','·')) 
                                                    for n,s in enumerate(mejs.split(c10))))
                return d(vals=d(cmnt=warn),fid='meme')
#               app.msg_box(str(ex)
#                   +c10+(c10.join('{:>3}|{}'.format(n+1, s.replace(' ','·')) 
#                                   for n,s in enumerate(mejs.split(c10))))
#                   , app.MB_OK)
#               return d(fid='meme')
            if aid=='okok':
                newv    = jsvl
                return None     # Close
            return d(vals=d(cmnt=cmnt4v),fid='meme')
       #def acts
    DlgAgent(
        form =dict(cap     = f(_('Edit JSON option ({})'), op)
                  ,resize  = True
                  ,w       = 510
                  ,h       = 400
                  )
    ,   ctrls=[0
        ,('meme',d(tp='me'  ,l=  5  ,w=500  ,t=  5  ,h=150                         ,a='tBlR'))
        ,('cmnt',d(tp='me'  ,l=  5  ,w=500  ,t=160  ,h=200 ,ro_mono_brd='1,1,1'    ,a='TBlR'))
        ,('defv',d(tp='bt'  ,l=  5  ,w=110  ,t=370  ,cap=_('Set &default')  ,a='TB'     ,call=acts  ,en=(dval is not None)))
        ,('undo',d(tp='bt'  ,l=120  ,w=110  ,t=370  ,cap=_('&Undo changes') ,a='TB'     ,call=acts))
        ,('test',d(tp='bt'  ,l=285  ,w= 70  ,t=370  ,cap=_('Chec&k')        ,a='TBLR'   ,call=acts))
        ,('cans',d(tp='bt'  ,l=360  ,w= 70  ,t=370  ,cap=_('Cancel')        ,a='TBLR'   ,call=acts))
        ,('okok',d(tp='bt'  ,l=435  ,w= 70  ,t=370  ,cap=_('OK')            ,a='TBLR'   ,call=acts  ,def_bt=True))
                ][1:]
    ,   vals =dict(meme=json.dumps(uval, indent=2)
                  ,cmnt=cmnt4v)
    ,   fid  ='meme'
    ).show()
    return newv
   #def edit_json_as_dict


class Command:
    def dlg_cuda_options(self):
        while True:
            OptEdD.restart    = False
            self._dlg_opt()
            if not OptEdD.restart:    break
       #def dlg_cuda_options

    def _dlg_opt(self):
        if app.app_api_version()<MIN_API_VER:   return app.msg_status(_('Need update CudaText'))
        defs_json   = apx.get_opt('dlg_cuda_options.defs_json', 'default.json')
        defs_json   = defs_json     if os.sep in defs_json else     apx.get_def_setting_dir()+os.sep+defs_json
        OptEdD(
          path_keys_info=defs_json
        , subset='df.'
        ).show(_('CudaText options'))
       #def _dlg_opt
   #class Command

def add_to_history(val:str, lst:list, max_len=MAX_HIST, unicase=False)->list:
    """ Add/Move val to list head. """
    lst_u = [ s.upper() for s in lst] if unicase else lst
    val_u = val.upper()               if unicase else val
    if val_u in lst_u:
        if 0 == lst_u.index(val_u):   return lst
        del lst[lst_u.index(val_u)]
    lst.insert(0, val)
    if len(lst)>max_len:
        del lst[max_len:]
    return lst
   #def add_to_history
    
RPT_HEAD = '''
<html>
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <title>CudaText options</title>
    <style type="text/css">
td, th, body {
    color:          #000;
    font-family:    Verdana, Arial, Helvetica, sans-serif;
    font-size:      12px;
}
table {
    border-width:   1px;
    border-spacing: 2px;
    border-color:   gray;
    border-collapse:collapse;
}
table td, table th{
    border-width:   1px;
    padding:        1px;
    border-style:   solid;
    border-color:   gray;
}
pre {
    margin:         0;
    padding:        0;
}
td.nxt {
    color:          grey;
    word-break:     break-all;
}
td.win {
    font-weight:    bold;
    word-break:     break-all;
}
    </style>
</head>
<body>
'''
RPT_FOOT = '''
</body>
</html>
'''

def do_report(fn, lex='', ed_=ed):
    def hard_word_wrap(text, rmax):
        reShift     = re.compile(r'\s*')
        reHeadTail  = re.compile(r'(.{' + str(rmax) + r'}\S*)\s*(.*)')
        src_lines   = text.splitlines()
        pass;                  #print('src_lines=',src_lines)
        trg_lines   = []
        for line in src_lines:
            pass;              #print('line=', line, 'len=', len(line.rstrip()))
            if len(line.rstrip()) <= rmax: 
                trg_lines.append(line)
                continue
            shift   = reShift.match(line).group(0)
            head,   \
            tail    = reHeadTail.match(line).group(1, 2)
            if not tail:
                tail= line.split()[-1]
                head= line[:-len(tail)]
            pass;              #print('head=', head, 'tail=', tail)
            trg_lines.append(head)
            trg_lines.append(shift+tail)
        pass;                  #print('trg_lines=',trg_lines)
        return '\n'.join(trg_lines)
       #def hard_word_wrap
       
#   lex         = ed_.get_prop(app.PROP_LEXER_CARET)
    def_json    = apx.get_def_setting_dir()         +os.sep+'default.json'
    usr_json    = app.app_path(app.APP_DIR_SETTINGS)+os.sep+'user.json'
    lex_json    = app.app_path(app.APP_DIR_SETTINGS)+os.sep+lex                                 if lex else ''

    def_opts    = apx._get_file_opts(def_json, {},  object_pairs_hook=collections.OrderedDict)
    usr_opts    = apx._get_file_opts(usr_json, {},  object_pairs_hook=collections.OrderedDict)
    lex_opts    = apx._get_file_opts(lex_json, {},  object_pairs_hook=collections.OrderedDict)  if lex else None

    def_opts    = pickle.loads(pickle.dumps(def_opts))                              # clone to pop
    usr_opts    = pickle.loads(pickle.dumps(usr_opts))                              # clone to pop
    lex_opts    = pickle.loads(pickle.dumps(lex_opts))  if lex else {}              # clone to pop

    fil_opts    = {op:ed_.get_prop(pr) for op,pr in apx.OPT2PROP.items()}
#   fil_opts    = get_ovrd_ed_opts(ed)
    cmt_opts    = {}
    # Find Commentary for def opts in def file
    # Rely: _commentary_ is some (0+) lines between opt-line and prev opt-line
    def_body    = open(def_json).read()
    def_body    = def_body.replace('\r\n', '\n').replace('\r', '\n')
    def_body    = def_body[def_body.find('{')+1:]   # Cut head with start '{'
    def_body    = def_body.lstrip()
    for opt in def_opts.keys():
        pos_opt = def_body.find('"{}"'.format(opt))
        cmt     = def_body[:pos_opt].strip()
        cmt     = ('\n\n'+cmt).split('\n\n')[-1]
        cmt     = re.sub('^\s*//', '', cmt, flags=re.M)
        cmt     = cmt.strip()
        cmt_opts[opt]    = html.escape(cmt)
        def_body= def_body[def_body.find('\n', pos_opt)+1:]   # Cut the opt

    with open(fn, 'w', encoding='utf8') as f:
        f.write(RPT_HEAD)
        f.write('<h4>High priority: editor options</h4>\n')
        f.write('<table>\n')
        f.write(    '<tr>\n')
        f.write(    '<th>Option name</th>\n')
        f.write(    '<th>Value in<br>default</th>\n')
        f.write(    '<th>Value in<br>user</th>\n')
        f.write(    '<th>Value in<br>{}</th>\n'.format(lex))                                                            if lex else None
        f.write(    '<th title="{}">Value for file<br>{}</th>\n'.format(ed_.get_filename()
                                              , os.path.basename(ed_.get_filename())))
        f.write(    '<th>Comment</th>\n')
        f.write(    '</tr>\n')
        for opt in fil_opts.keys():
            winner  = 'def'
            winner  = 'usr' if opt in usr_opts else winner
            winner  = 'lex' if opt in lex_opts else winner
            winner  = 'fil' if opt in fil_opts else winner
            f.write(    '<tr>\n')
            f.write(    '<td>{}</td>\n'.format(opt))
            f.write(    '<td class="{}">{}</td>\n'.format('win' if winner=='def' else 'nxt', def_opts.get(opt, '')))
            f.write(    '<td class="{}">{}</td>\n'.format('win' if winner=='usr' else 'nxt', usr_opts.get(opt, '')))
            f.write(    '<td class="{}">{}</td>\n'.format('win' if winner=='lex' else 'nxt', lex_opts.get(opt, '')))    if lex else None
            f.write(    '<td class="{}">{}</td>\n'.format('win' if winner=='fil' else 'nxt', fil_opts.get(opt, '')))
#           f.write(    '<td><pre>{}</pre></td>\n'.format(cmt_opts.get(opt, '')))
            f.write(    '<td><pre>{}</pre></td>\n'.format(hard_word_wrap(cmt_opts.get(opt, ''), 50)))
            f.write(    '</tr>\n')
            def_opts.pop(opt, None)
            usr_opts.pop(opt, None)
            lex_opts.pop(opt, None)                                                                                     if lex else None
        f.write('</table><br/>\n')
        f.write('<h4>Overridden default options</h4>\n')
        f.write('<table>\n')
        f.write(    '<tr>\n')
        f.write(    '<th width="15%">Option name</th>\n')
        f.write(    '<th width="20%">Value in<br>default</th>\n')
        f.write(    '<th width="20%">Value in<br>user</th>\n')
        f.write(    '<th width="10%">Value in<br>{}<br></th>\n'.format(lex))                                            if lex else None
        f.write(    '<th width="35%">Comment</th>\n')
        f.write(    '</tr>\n')
        for opt in def_opts.keys():
            winner  = 'def'
            winner  = 'usr' if opt in usr_opts else winner
            winner  = 'lex' if opt in lex_opts else winner
            winner  = 'fil' if opt in fil_opts else winner
            f.write(    '<tr>\n')
            f.write(    '<td>{}</td>\n'.format(opt))
            f.write(    '<td class="{}">{}</td>\n'.format('win' if winner=='def' else 'nxt', def_opts.get(opt, '')))
            f.write(    '<td class="{}">{}</td>\n'.format('win' if winner=='usr' else 'nxt', usr_opts.get(opt, '')))
            f.write(    '<td class="{}">{}</td>\n'.format('win' if winner=='lex' else 'nxt', lex_opts.get(opt, '')))    if lex else None
            f.write(    '<td><pre>{}</pre></td>\n'.format(hard_word_wrap(cmt_opts.get(opt, ''), 50)))
            f.write(    '</tr>\n')
            usr_opts.pop(opt, None)
            lex_opts.pop(opt, None)                                                                                     if lex else None
        f.write('</table><br/>\n')
        f.write('<h4>Overridden user-only options</h4>')
        f.write('<table>\n')
        f.write(    '<tr>\n')
        f.write(    '<th>Option name</th>\n')
        f.write(    '<th>Value in<br>user</th>\n')
        f.write(    '<th>Value in<br>{}</th>\n'.format(lex))                                                            if lex else None
        f.write(    '<th>Comment</th>\n')
        f.write(    '</tr>\n')
        for opt in usr_opts.keys():
            winner  = 'usr'
            winner  = 'lex' if opt in lex_opts else winner
            f.write(    '<tr>\n')
            f.write(    '<td>{}</td>\n'.format(opt))
            f.write(    '<td class="{}">{}</td>\n'.format('win' if winner=='usr' else 'nxt', usr_opts.get(opt, '')))
            f.write(    '<td class="{}">{}</td>\n'.format('win' if winner=='lex' else 'nxt', lex_opts.get(opt, '')))    if lex else None
            f.write(    '<td><pre>{}</pre></td>\n'.format(cmt_opts.get(opt, '')))
            f.write(    '</tr>\n')
            lex_opts.pop(opt, None)                                                                                     if lex else None
        for opt in lex_opts:
            winner  = 'lex'
            f.write(    '<tr>\n')
            f.write(    '<td>{}</td>\n'.format(opt))
            f.write(    '<td class="{}"></td>  \n'.format('non'))
            f.write(    '<td class="{}">{}</td>\n'.format('win', lex_opts.get(opt, '')))
            f.write(    '<td><pre>{}</pre></td>\n'.format(cmt_opts.get(opt, '')))
            f.write(    '</tr>\n')
            lex_opts.pop(opt, None)
        f.write('</table><br/>\n')
        f.write(RPT_FOOT)
        return True
   #def do_report(fn):

def index_1(cllc, val, defans=-1):
    return cllc.index(val) if val in cllc else defans

if __name__ == '__main__' :     # Tests
    # To start the tests run in Console
    #   exec(open(path_to_the_file, encoding="UTF-8").read())
#   app.app_log(app.LOG_CONSOLE_CLEAR, 'm')
#   for smk in [smk for smk 
#       in  sys.modules                             if 'cuda_options_editor.tests.test_options_editor' in smk]:
#       del sys.modules[smk]        # Avoid old module 
#   import                                              cuda_options_editor.tests.test_options_editor
#   import unittest
#   suite = unittest.TestLoader().loadTestsFromModule(  cuda_options_editor.tests.test_options_editor)
#   unittest.TextTestRunner(verbosity=0).run(suite)

    pass
        
'''
ToDo
[+][kv-kv][02apr17] History for cond
[-][kv-kv][02apr17] ? Chapters list and "chap" attr into kinfo
[-][kv-kv][02apr17] ? Tags list and "tag" attr into kinfo
[-][kv-kv][02apr17] ? Delimeter row in table
[ ][kv-kv][02apr17] "Need restart" in Comments
[+][kv-kv][02apr17] ? Calc Format by Def_val
[ ][kv-kv][02apr17] int_mm for min+max
[+][kv-kv][02apr17] VERS in Title
[+][at-kv][02apr17] 'enum' вместо 'enum_i' 
[ ][kv-kv][02apr17] Save top row in table
[+][kv-kv][03apr17] Show stat in Chap-combo and tags check-list
[-][kv-kv][03apr17] ? Add chap "(No chapter)"
[-][kv-kv][03apr17] ? Add tag "#no_tag"
[+][kv-kv][03apr17] Call opts report
[+][at-kv][04apr17] Format 'font'
[-][at-kv][04apr17] ? FilterListView
[+][at-kv][04apr17] use new default.json
[-][kv-kv][04apr17] Testing for update user.json
[+][kv-kv][04apr17] Restore Sec and Tags
[+][kv-kv][04apr17] ro-combo hitory for Tags
[+][kv-kv][05apr17] Add "default" to fonts if def_val=="default"
[+][at-kv][05apr17] Preview for format=fontmay
[+][kv-kv][06apr17] Spec filter sign: * - to show only modified
[-][kv-kv][06apr17] Format color
[+][kv-kv][24apr17] Sort as Def or as User
[+][kv-kv][05may17] New type "list of str"
[ ][kv-kv][23jun17] ? Filter with tag (part of tag?). "smth #my"
[+][kv-kv][15mar18] ? Filter with all text=key+comment
[+][kv-kv][19mar18] ? First "+" to filter with comment
[-][kv-kv][19mar18] !! Point the fact if value is overed in ed
[?][kv-kv][20mar18] Allow to add/remove opt in user/lex
[?][kv-kv][21mar18] ? Allow to meta keys in user.json: 
                        "_fif_LOG__comment":"Comment for fif_LOG"
[+][kv-kv][22mar18] Set conrol's tab_order to always work Alt+E for "Valu&e"
[ ][kv-kv][26mar18] Use 'editor' for comment
[+][kv-kv][26mar18] Increase w for one col when user increases w of dlg (if no h-scroll)
[+][kv-kv][13apr18] DClick on Def-col - focus to Reset
[-][kv-kv][16apr18] Open in tag for fmt=json
[?][kv-kv][23apr18] ? Show opt from cur line if ed(default.json)
[+][at-kv][03may18] Rework ask to confirm removing user/lex opt
[+][at-kv][04may18] Report to console all changes
[+][at-kv][05may18] Call OpsReloadAndApply
[+][kv-kv][05may18] Rework radio to checks (Linux bug: always set one of radio-buttons)
[-][kv-kv][05may18] Ask "Set also for current file?" if ops is ed.prop
[+][kv-kv][06may18] Menu command "Show changes"
[+][kv-kv][06may18] Show all file opt value. !!! only if val!=over-val
[+][kv-kv][06may18] Rework Sort
[+][kv-kv][14may18] Scale def col widths
[ ][at-kv][14may18] DClick over 1-2-3 is bad
[+][at-kv][14may18] Allow to refresh table on each changing of filter 
[+][at-kv][15may18] Allow to extra sort cols with Ctrl+Click
[ ][kv-kv][04jun18] Cannot select section @Ui after selected @Ui/Tabs
[ ][kv-kv][16jun18] Have 2 filter control to instant and history. Switch by vis
[+][kv-kv][18jun18] More then one chap in filter. Append from menu if Ctrl holds
[+][at-kv][24apr19] Add types: rgb
[ ][at-kv][24apr19] Add type condition: int/float range
[+][kv-kv][25apr19] Hide cols "Lexer" and "File", controls []For and lexer list (by init opt)
[+][kv-kv][25apr19] Allow store other then user.json
[+][kv-kv][25apr19] Return 'was modified' from show()
'''