''' Plugin for CudaText editor
Authors:
    Andrey Kvichansky    (kvichans on github.com)
Version:
    '2.3.15 2021-04-02'
ToDo: (see end of file)
'''

import  re, os, sys, json, collections, itertools, webbrowser, tempfile, html, pickle, time, datetime
from    itertools       import *
from pathlib import PurePath as PPath
from pathlib import     Path as  Path
def first_true(iterable, default=False, pred=None):return next(filter(pred, iterable), default) # 10.1.2. Itertools Recipes

import  cudatext            as app
import  cudatext_cmd        as cmds
import  cudax_lib           as apx
from    cudatext        import ed
#from    .cd_plug_lib    import *

d   = dict
odict = collections.OrderedDict
OptFetchResult = collections.namedtuple('OptFetchResult', 'opts filter sort')

pass;                           LOG     = (-1== 1) or apx.get_opt('_opts_dlg_log',False)    # Do or dont logging.
pass;                           from pprint import pformat
pass;                           pf=lambda d:pformat(d,width=150)
pass;                           pf80=lambda d:pformat(d,width=80)
pass;                           pf60=lambda d:pformat(d,width=60)
pass;                           ##!! waits correction

_   = apx.get_translation(__file__) # I18N

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

NO_CHAP = _('_no_') #TODO MY: handle dupe

pass;                          #FONT_LST=FONT_LST[:3]


def f(s, *args, **kwargs):return s.format(*args, **kwargs)

def log(msg='', *args, **kwargs):
    if args or kwargs:
        msg = msg.format(*args, **kwargs)
    return msg


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
                lstF    = [d for d in os.listdir(from_dir)
                           if os.path.isdir(from_dir+os.sep+d) and d.upper()!='README' and d.strip()]
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


class OptEdD:

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
        #m.stores    = get_hist('dlg'
                        #, json.loads(open(CFG_JSON).read(), object_pairs_hook=odict)
                            #if os.path.exists(CFG_JSON) else odict())
        pass;                  #LOG and log('ok',())
#       m.bk_sets   = m.stores.get(m.subset+'bk_sets'    , False)
        m.lexr_l    = app.lexer_proc(app.LEXER_GET_LEXERS, False)
        m.lexr_w_l  = [f('{} {}'
                        ,'!!' if os.path.isfile(app.app_path(app.APP_DIR_SETTINGS)+os.sep+'lexer '+lxr+'.json') else '  '
                        , lxr)
                        for lxr in m.lexr_l]


        m.lexr      = m.ed.get_prop(app.PROP_LEXER_CARET)
        m.all_ops   = False        # Show also options without definition

        '!!! ALL options'
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

        #m.apply_one = m.stores.get(m.subset+'apply_one', False) # Do one call OpsReloadAndApply on exit
        #m.apply_need= False                                     # Need to call OpsReloadAndApply
        #m.auto4file = m.stores.get(m.subset+'auto4file', True)  # Auto reset file value to over value def/user/lex
        m.auto4file = True  # Auto reset file value to over value def/user/lex
       #def __init__

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
            #m.cur_op    = m.cur_op if m.cur_op in m.opts_full else ''
            pass;              #LOG and log('m.opts_full={}',pf(m.opts_full))
            m.do_file('build-chp-tree')

        elif what=='build-chp-tree':
            # Build chapter tree
            m.chp_tree  = odict(ops=list(m.opts_full.keys())
                               ,kids=odict()
                               ,path='')  # {chp:{ops:[], kids:{...}, path:'c1/c2'}
            m.pth2chp   = {}                                    # {path:chp}
            for op,oi in m.opts_full.items():
                chp_s   = oi.get('chp', NO_CHAP)
                chp_s   = chp_s if chp_s else NO_CHAP
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

        return []
       #def do_file


    def show(self
        , title                     # For cap of dlg
        ):

        '!!! WORK'
        from .dlg import DialogMK2
        opt_man = OptionsMan(self.opts_defn, self.chp_tree, self.on_apply_value)
        # to access from Console:  xxcuda_options_editor_my._dbg_opted.dlg
        self.dlg = DialogMK2(opt_man, title, subset=self.subset, how=self.how)
        self.dlg.show()
        return


    '!!! MY'
    def on_apply_value(self, action, scope, opname, val, lexer=None, apply_=False):
        """ Prepares values for and calls - `do_setv()`
            * action -- set, remove
            * scope -- m.for_ulf -- 'u','l','f' (user, lexer, file)
            * opname -- optio nname -- m.cur_op
            * lexer -- m.lexr
            * apply_ -- apply options - set True on last option  or  on btn_apply
        """
        M,m = self.__class__,self

        if action not in ['set', 'remove']: # dbg
            raise Exception('invalid action for `on_apply_value`: {}'.format(action))

        m.for_ulf = scope
        m.cur_op = opname
        m.lexr = lexer

        if action == 'set':         action_id = 'setv'
        elif action == 'remove':    action_id = 'setd'

        # apply_one == apply only once -- if False - one will not be applied
        m.apply_one = not apply_

        return self.do_setv(aid=action_id, ag=None, data=val)


    def do_setv(self, aid, ag, data):
        M,m = self.__class__,self
        pass;                  #LOG and log('aid,m.cur_op={}',(aid,m.cur_op))
        if not m.cur_op:   return []
        #m.col_ws= [ci['wd'] for ci in m.ag.cattr('lvls', 'cols')]

        cur_val = data

        trg     = 'lexer '+m.lexr+'.json' if m.for_ulf=='l' else 'user.join'
        key4v   = m.for_ulf+'val'
        op      = m.cur_op
        oi      = m.opts_full[op]
        frm     = oi['frm']

        dval    = oi.get( 'def')
        uval    = oi.get('uval')
        lval    = oi.get('lval')
        fval    = oi.get('fval')
        ulfvl   = oi.get(key4v ) #fval if m.for_ulf=='f' else lval if m.for_ulf=='l' else uval
        jval    = oi['jlvl']    if m.for_ulf=='l' else \
                  oi['juvl']    if m.for_ulf=='u' else \
                  oi['jfvl']

        # Get new value
        newv    = None
        erpt_s  = ''
        if False:pass

        elif aid=='setd'  and  m.for_ulf=='f'  and  op in apx.OPT2PROP:
            # Remove from file - set over def/user/lex val
            newv    = oi.get('lval', oi.get('uval', oi.get('def')))
            if newv==ulfvl:
                #m.stbr_act(M.STBR_MSG, _('No need changes'))
                return []
            erpt_s  = 'reset-f'
            m.ed.set_prop(apx.OPT2PROP[op], newv)

        elif aid=='setd'  and  ulfvl is not None  and  m.for_ulf!='f':
            # Remove from user/lexer
            newv= None

        elif aid=='setv':                   # Add/Set opt for user/lexer/file
            # Enter from edit. Need parse some string
            #newv    = m.ag.cval('eded')
            newv    = cur_val
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


        ### Use new value to change env
        if newv is not None and newv==ulfvl:
            #m.stbr_act(M.STBR_MSG, _('No need changes'))
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
        #m.do_erpt(erpt_s, jnewv, jval)
        pass;                  #LOG and log('ok oi={}',(oi))
        pass;                  #LOG and log('ok m.opts_full={}',pf(m.opts_full))

        pass;                  #LOG and log('?? get_cnts',())

        if m.for_ulf!='f' and m.auto4file and op in apx.OPT2PROP:
            # Change FILE to over  (("over" - override?))
            newv    = oi.get('lval', oi.get('uval', oi.get('def')))
            if newv!=oi.get('fval'):
                erpt_s      = 'reset-f'
                m.ed.set_prop(apx.OPT2PROP[op], newv)
                oi['fval']  = newv
                jval        = oi['jfvl']
                upd_cald_vals(m.opts_full)
                jnewv       = oi['jfvl']
                #m.do_erpt('auset-f', jnewv, jval)

        pass;                  #LOG and log('m.get_vals(lvls-cur)={}',(m.get_vals('lvls-cur')))
        #return d(ctrls=m.get_cnts('+lvls+cur')
                #,vals =m.get_vals('lvls-cur')
                #)
       #def do_setv




class OptionsMan:
    """
    ('opt', 'font_name')    # option name
    ('def', 'Consolas')     # current value?
    ('cmt', 'Text font name [has suffix]')      # comment
    ('frm', 'font')     # format?
    ('lst', ['default', 'aakar', 'Abyssinica SIL', 'Ani', ...
    ('chp', 'Font')         # Category  -- ('chp', 'Text/Look')
    ('jdc', [])         # ui display value
    ('jdf', 'Consolas') # current dusplay value
    ('!', '')       # changed?
    ('juvl', '')    # user value?
    ('jlvl', '')    # lexer value?
    ('jfvl', '')    # file value?
    +
    ('dct', [['', 'disabled'], ['a', 'by Alt+click'], ['sa', 'by Shift+Alt+click'], ...
    ('lst', ['common_20x20', 'octicons_20x20'])
    """

    FORMATS_ORDER = {
        'bool'  : 0, # bool
        'float' : 1, # numbers
        'int'   : 1,
        'hotk'  : 2, # strings
        'str'   : 2,
        'font'  : 2,
        'font-e': 2,
        'int2s' : 2,
        'str2s' : 2,
        'strs'  : 2,
    }

    # value and its scope
    # display value
    # name
    def __init__(self, opts_defn, chp_tree, on_apply_value):
        self._opts_defn = opts_defn
        self._chp_tree = chp_tree
        self._on_apply_value = on_apply_value
        self._last_result = None   # OptFetchResult()  -- opts get cache

    @property
    def tree(self):
        """ only for tree structure;  opt values are in _opts_defn
        """
        return self._chp_tree

    def on_opts_change(self):
        """ drop cached results when options change
        """
        self._last_result = None

    def get_list(self, filter_str='', sort_field='', reverse=False):
        """ FLTR_H ...
            sort types: name, ...
        """
        # check if already have this result
        if (self._last_result
                and self._last_result.filter == filter_str
                and self._last_result.sort == sort_field):
            if reverse:
                return self._last_result.opts[::-1]
            return self._last_result.opts[:]


        # filter
        opts = OptionFilter.opt_filter(self._opts_defn, filter_str)
        # sort
        OptionsMan.sort_opts(opts, sort_field)

        # cache result
        self._last_result = OptFetchResult(opts, filter=filter_str, sort=sort_field)
        if reverse:
            return opts[::-1]
        return opts[:]


    def set_opt(self, name, scope, val, lexer=None, apply_=True):
        res = self._on_apply_value(
                'set', scope, name, val, lexer=lexer, apply_=apply_)
        pass;       LOG and print('NOTE: set opt: {}, {}, {}'.format(name, val, scope))

    def reset_opt(self, name, scope, lexer=None, apply_=True):
        res = self._on_apply_value(
                'remove', scope, name, val=None, lexer=lexer, apply_=apply_)
        pass;       LOG and print('NOTE:reseted opt: {}, {}'.format(name, scope))


    @classmethod
    def sort_opts(cls, opts, sort_field):

        def key(opt): #SKIP
            """ returns key value depending on sort field
            """
            if sort_field == 'Value':
                res = cls.get_opt_active_value(opt, is_ui=True)
                if res == ''  and  opt['frm'] in {'bool', 'int', 'float'}:
                    return float('-inf')
                return res

            elif sort_field in {'uval', 'lval', 'fval', 'def'}:
                scope = sort_field[0]
                # cant be None - atleast default
                res = cls.get_opt_scope_value(opt, scope, is_ui=True)
                if res == ''  and  opt['frm'] in {'bool', 'int', 'float'}:
                    return float('-inf')
                return res

            else: # strings
                return opt[sort_field]

        # sort by name first -- so options with same sort-column-value - are sorted
        opts.sort(key=lambda item: item['opt'])

        if not sort_field:
            return

        if sort_field in {'Value', 'uval', 'lval', 'fval', 'def'}:
            # split options into buckets by format
            _nformats = 1 + max(cls.FORMATS_ORDER.values())
            format_opts = [[] for i in range(_nformats)]
            unset_opts = []
            for opt in opts:
                # extract unset options to sort first
                _sort_key_val = key(opt)
                sort_item = (_sort_key_val, opt)

                if sort_field == 'Value':
                    if _sort_key_val == ''  and  opt['!'] == '':
                        unset_opts.append(sort_item)
                        continue
                elif sort_field in {'uval', 'lval', 'fval'}:
                    #if opt.get(field) is None:
                    if opt.get(sort_field) is None:
                        unset_opts.append(sort_item)
                        continue

                _format_key = cls.FORMATS_ORDER[opt['frm']]
                format_opts[_format_key].append(sort_item)
            #end for

            pass;       LOG and print('unset vals: {}'.format(len(unset_opts)))

            opts.clear()
            opts.extend(item[1] for item in unset_opts)
            for i,format_list in enumerate(format_opts):
                is_str_group =  (i == cls.FORMATS_ORDER['str'])  # string group - normalize case
                if is_str_group:
                    key = lambda item: item[0].lower()
                else:
                    key = lambda item: item[0]

                opts.extend(item[1] for item in sorted(format_list, key=key))

        else:
            opts.sort(key=key)


    def get_opt(self, name):
        for opt in self._opts_defn:
            if opt.get('opt') == name:
                return opt

    def get_scope_value(self, name, scope, default=None):
        opt = self.get_opt(name)
        if not opt:
            return default

        val = self.get_opt_scope_value(opt, scope, is_ui=False)
        return val  if val is not None else  default

    @classmethod
    def get_opt_scope_value(cls, opt, scope, is_ui):
        """ if is_ui==False -- result can be None
        """
        if not opt:
            return None

        if scope == 'u': # user
            return opt['juvl']  if is_ui else  opt.get('uval')
        elif scope == 'l': # lexer
            return opt['jlvl']  if is_ui else  opt.get('lval')
        elif scope == 'f': # file
            return opt['jfvl']  if is_ui else  opt.get('fval')
        elif scope == 'def' or scope == 'd': # default
            return opt['jdf']  if is_ui else  opt.get('def')

    @classmethod
    def get_opt_active_value(cls, opt, is_ui, with_scope=False):
        if not opt:
            return None

        override_str = opt['!']
        override_str = override_str.replace('+', '').replace('L', '')
        if override_str == '': # default
            res = opt['jdf']  if is_ui else  opt['def']
            scope = ''

        elif override_str == '!': # user
            res = opt['juvl']  if is_ui else  opt['uval']
            scope = 'u'

        elif override_str == '!!': # lexer
            res = opt['jlvl']  if is_ui else  opt['lval']
            scope = 'l'

        elif override_str == '!!!': # file
            res = opt['jfvl']  if is_ui else  opt['fval']
            scope = 'f'
        else:
            raise Exception('Unsupported override-str:{}\n+ {}'.format(opt['!'], opt))
        #end if

        return (scope, res)  if with_scope else  res

    @classmethod
    def value2uival(cls, opt, val):
        frm = opt['frm']
        if frm in ['int2s', 'str2s']:
            dct = opt['dct']
            for i,(ival,_iname) in enumerate(dct):
                if ival == val:
                    return opt['jdc'][i]
            else:
                raise Exception(f'No matching "dct" value for [{val}] --- {opt}')
        else:
            return val


class OptionFilter:
    @classmethod
    def opt_filter(cls, opts_defn, filter_str):
        if not filter_str:
            return opts_defn[:]

        #print(' >>>filter: {}, {}'.format(filter_str, len(opts_defn)))

        filter_str, chapters = cls._get_chapter_filter(filter_str)
        #print(' >>>    chp: {}, {}'.format(filter_str, chapters))
        opts_defn = cls.chapter_filter(chapters, opts_defn)
        #print(' >>>    chp filted len: {}'.format(len(opts_defn)))
        opts_defn = cls.opt_text_filter(filter_str, opts_defn)
        #print(' >>>    txt filted len: {}'.format(len(opts_defn)))
        return opts_defn

    @classmethod
    def chapter_filter(cls, chapters, opts_defn):
        if not chapters:
            return opts_defn

        return [op for op in opts_defn
                    if any(chp in op.get('chp', '').upper() for chp in chapters)]

    @classmethod
    def opt_text_filter(cls, filter_str, opts_defn):
        if not filter_str:
            return opts_defn
        return [op for op in opts_defn  if cls._filter_opt(filter_str, op['opt'], op)]


    @classmethod
    def _get_chapter_filter(cls, filter_str):
        """ returns (filter_str, [chapters])
        """
        chp_conds    = None
        #chp_no_c    = False    #TODO invesigate
        if  '@' in filter_str:
            # Prepare to match chapters
            chp_conds    = (mt.group(1).upper() for mt in re.finditer(r'@([\w/]+)'    , filter_str))
            chp_conds    = [chp for chp in chp_conds if chp != NO_CHAP.upper()]
            #chp_no_c    = '@'+M.NO_CHAP in filter_str #WTF?
            filter_str      = re.sub('@([\w/]*)', '', filter_str)             # @s* clear @ and cph
        return filter_str, chp_conds


    reNotWdChar = re.compile(r'\W')
    @classmethod
    def _filter_opt(cls, fltr_s, op, oi):
        if not fltr_s:                                  return True
        pass;              #LOG and log('fltr_s, op, oi[!]={}',(fltr_s, op, oi['!']))
        if '!!!' in fltr_s and '!!!' not in oi['!']:    return False
        if '!!'  in fltr_s and '!!'  not in oi['!']:    return False
        pass;              #LOG and log('skip !!',())
        if  '!'  in fltr_s and  '!'  not in oi['!']:    return False
        pass;              #LOG and log('skip !',())
        text    = op + (' '+oi.get('cmt', '')  if '#' in fltr_s else  '')
        text    = text.upper()
        fltr_s  = fltr_s.replace('!', '').replace('#', '').upper()
        if '<' in fltr_s or '>' in fltr_s:
            text    = '·' + cls.reNotWdChar.sub('·', text)    + '·'
            fltr_s  = ' ' + fltr_s + ' '
            fltr_s  = fltr_s.replace(' <', ' ·').replace('> ', '· ')
        pass;              #LOG and log('fltr_s, text={}',(fltr_s, text))
        return all(map(lambda ch:ch in text,  fltr_s.split()))


class Command:
    def dlg_cuda_options(self):
        self._dlg_opt()

    def _dlg_opt(self):
        defs_json   = apx.get_opt('dlg_cuda_options.defs_json', 'default.json')
        defs_json   = defs_json     if os.sep in defs_json else     apx.get_def_setting_dir()+os.sep+defs_json
        opted = OptEdD(path_keys_info=defs_json, subset='df.')
        opted.show(_('CudaText options lite'))
       #def _dlg_opt
   #class Command


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
