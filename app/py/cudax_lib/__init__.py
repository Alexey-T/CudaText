''' Py-extensions for CudaText.
Overrided option tools:
    get_opt(path, def_value=None, level=CONFIG_LEV_ALL, ed=ed)
        Reads option from configs (lexer-override config, then user config).
        Path: simple value (e.g. "tab_size") or "/"-separated path inside JSON tree
    set_opt(path, value, ed=ed, level=CONFIG_LEV_USER)
        Add/Update/Delete option into a config (user-config if no param level).
Comments:
    cmt_toggle_stream
        Add/Remove stream comment (as /*....*/) for cur selection or for cur line (by opt)
    cmt_toggle_line_1st
        Add/Remove comment at 1st pos of line
    cmt_toggle_line_body
        Add/Remove comment at 1st pos of text
Duplicate:
    duplicate
        Dub cur selection or cur line (by opt)
Config menus
    load_main_menu()
Authors:
    Andrey Kvichansky    (kvichans on githab)
Version:
    '0.4.3 2015-11-10'
Wiki: github.com/kvichans/cudax_lib/wiki
ToDo: (see end of file)
'''

import  cudatext        as app
from    cudatext    import ed
import  cudatext_cmd    as cmds
import  os, json, re

# Overrided option tools:
CONFIG_LEV_DEF      = 'def'
CONFIG_LEV_USER     = 'user'
CONFIG_LEV_LEX      = 'lex'
CONFIG_LEV_FILE     = 'file'
CONFIG_LEV_ALL      = 'dulf'
APP_DEFAULT_OPTS    = {}
LAST_FILE_OPTS      = {}

# Menu config
PROC_MENU_TOP       = 'top'
PROC_MENU_TOP_FILE  = 'top-file'
PROC_MENU_TOP_EDIT  = 'top-edit'
PROC_MENU_TOP_SEL   = 'top-sel'
PROC_MENU_TOP_SR    = 'top-sr'
PROC_MENU_TOP_VIEW  = 'top-view'
PROC_MENU_TEXT      = 'text'
PROC_MENU_RECENTS   = 'recents'
PROC_MENU_THEMES    = 'themes'
PROC_MENU_PLUGINS   = 'plugins'

# Localization
CONFIG_MSG_DONT_SET_FILE= 'Cannot set editor properties'
CONFIG_MSG_DONT_SET_PATH= 'Cannot set complex path'
NEED_NEWER_API          = 'Needs newer app version'
COMMENTING              = 'Commenting'
DUPLICATION             = 'Duplication'
ONLY_NORM_SEL_MODE      = '{} works only with normal selection'
CMT_NO_LINE_4LEX        = 'No line comment for lexer "{}"'
CMT_NO_STRM_4LEX        = 'No stream comment for lexer "{}"'
ONLY_SINGLE_CRT         = "{} doesn't work with multi-carets"

pass;                           # Logging
pass;                           import inspect  # stack
pass;                           from pprint import pformat
pass;                           pfrm15=lambda d:pformat(d,width=15)
pass;                           LOG = (-2== 2)  # Do or dont logging.
pass;                           log_gap = ''    # use only into log()

class Command:
    ###############################################
    ## Menus
#   def load_main_menu(self):
#       ''' Reset main menu from config file
#       '''
#       mn_cfg_json = get_opt('config_main_menu', '')
#       pass;                  LOG and log('mn_cfg_json={}',mn_cfg_json)
#       if not mn_cfg_json:    return
#       mn_cfg_json = os.path.join(app.app_path(app.APP_DIR_SETTINGS), mn_cfg_json)
#       mn_cfg      = _json_loads(open(mn_cfg_json).read())
#       pass;                  LOG and log('mn_cfg={}',pfrm15(mn_cfg))
#       mn_items    = mn_cfg["items"]
#       for mn_item in mn_items:
#           for mn_id in mn_item:
#               pass;          #LOG and log('mn_id={}',pfrm15(mn_id))
#               self._reset_menu(mn_id, mn_item[mn_id])
#      #def load_main_menu
#
#   def _reset_menu(self, mn_id, mn_items):
#       pass;                  LOG and log('mn_id, mn_items={}',(mn_id, pfrm15(mn_items)))
#       # Inspect cur menu
#   #   app.app_proc(app.PROC_MENU_ENUM, mn_id)
#       # Clear old items
##       app.app_proc(app.PROC_MENU_CLEAR, mn_id)
#       for mn_item in mn_items:
#           if False:pass
#           elif 'id' in mn_item:
#               # Command!
#           elif 'items' in mn_item:
#               # Submenu!
#               for mn_id in mn_item:
#                   self._reset_menu(mn_id, mn_item[mn_id])
#      #def _reset_menu

    ###############################################
    ## Comments
    def cmt_toggle_line_1st(self):
        return self._cmt_toggle_line('1st')
    def cmt_toggle_line_body(self):
        return self._cmt_toggle_line('bod')
    def _cmt_toggle_line(self, cmt_type, ed_=ed):
        ''' Add/Remove full line comment
            Params
                cmt_type    '1st'    at begin of line
                            'bod'    at first not blank
        '''
        if not _check_API('1.0.108'):    return
        lex         = ed_.get_prop(app.PROP_LEXER_CARET)
        cmt_sgn     = app.lexer_proc(app.LEXER_GET_COMMENT, lex)
        pass;                  #LOG and log('cmt_type, lex, cmt_sgn={}', (cmt_type, lex, cmt_sgn))
        if not cmt_sgn:
            return app.msg_status(CMT_NO_LINE_4LEX.format(lex))
        # Analize
        bEmpSel     = False
        rWrks       = []
        pass;                  #LOG and log('ed_.get_sel_mode(),app.SEL_NORMAL,app.SEL_COLUMN={}', (ed_.get_sel_mode(),app.SEL_NORMAL,app.SEL_COLUMN))
        if False:pass
        elif ed_.get_sel_mode() == app.SEL_NORMAL:
            crts        = ed_.get_carets()
            bEmpSel     = 1==len(crts) and -1==crts[0][3]
            for (cCrt, rCrt ,cEnd, rEnd) in crts:
                (rCrtMin
                ,rCrtMax)   = minmax(rCrt, rEnd if -1!=rEnd else rCrt)
                if -1!=rEnd and rCrt>rEnd and 0==cCrt:
                    rCrtMax = rCrtMax-1    # For direct section along left bound
                rWrks      += list(range(rCrtMin, rCrtMax+1))
        elif ed_.get_sel_mode() == app.SEL_COLUMN:
            (cBgn
            ,rSelBgn
            ,cEnd
            ,rSelEnd)   = ed_.get_sel_rect()
            rWrks       = list(range(rSelBgn, rSelEnd+1))
        if not rWrks:
            rWrks       = [crts[0][1]]
        do_uncmt    = ed_.get_text_line(rWrks[0]).lstrip().startswith(cmt_sgn)
        # Work
        save_bd_col = get_opt('comment_save_column' , False)
        at_min_bd   = get_opt('comment_equal_column', False)
        col_min_bd  = 1000 # infinity
        if at_min_bd:
            for rWrk in rWrks:
                line        = ed_.get_text_line(rWrk)
                pos_body    = line.index(line.lstrip())
                pos_body    = len(line) if 0==len(line.lstrip()) else pos_body
                col_min_bd  = min(pos_body, col_min_bd)
                if 0==col_min_bd:
                    break # for rWrk
        blnks4cmt   = '\t'.expandtabs(len(cmt_sgn))
        pass;                  #LOG and log('rWrks,do_uncmt, save_cols, at_min_bd, col_min_bd={}', (rWrks,do_uncmt,save_bd_col,at_min_bd,col_min_bd))
        for rWrk in rWrks:
            line    = ed_.get_text_line(rWrk)
            pos_body= line.index(line.lstrip())
            pos_body= len(line) if 0==len(line.lstrip()) else pos_body
            pass;              #LOG and log('rWrk,pos_body,line={}', (rWrk,pos_body,line))
            if do_uncmt:
                # Uncomment!
                if not line[pos_body:].startswith(cmt_sgn):
                    # Already no comment
                    continue    #for rWrk
                if False:pass
                elif len(line)==len(cmt_sgn): # and line.startswith(cmt_sgn)
                    line = ''
                elif save_bd_col and (' '==line[0] or
                                      ' '==line[pos_body+len(cmt_sgn)]):
                    # Before or after cmt_sgn must be blank
                    line = line.replace(cmt_sgn, blnks4cmt, 1)
                else:
                    line = line.replace(cmt_sgn, ''       , 1)
            else:
                # Comment!
                if cmt_type=='bod' and line[pos_body:].startswith(cmt_sgn):
                    # Body comment already sets - willnot double it
                    continue    #for rWrk
                if False:pass
                elif cmt_type=='1st' and save_bd_col and line.startswith(blnks4cmt) :
                    line = line.replace(blnks4cmt, cmt_sgn, 1)
               #elif cmt_type=='1st' and save_bd_col #  !line.startswith(blnks4cmt) :
                elif cmt_type=='1st':#  !save_bd_col
                    line = cmt_sgn+line
                elif cmt_type=='bod' and save_bd_col and line.startswith(blnks4cmt):
                    pos_cmnt = col_min_bd if at_min_bd else pos_body
                    pass;          #LOG and log('pos_cmnt={}', (pos_cmnt))
                    if pos_cmnt>=len(cmt_sgn):
                        line = line[:pos_cmnt-len(cmt_sgn)]+cmt_sgn+line[pos_cmnt:             ]
                    else:
                        line = line[:pos_cmnt             ]+cmt_sgn+line[pos_cmnt+len(cmt_sgn):]
                   #line = line[:pos_cmnt-len(cmt_sgn)]+cmt_sgn+line[pos_cmnt:]
                   #line = line[:pos_body-len(cmt_sgn)]+cmt_sgn+line[pos_body:]
               #elif cmt_type=='bod' and save_bd_col #  !line.startswith(blnks4cmt) :
                elif cmt_type=='bod':#  !save_bd_col
                    pos_cmnt = col_min_bd if at_min_bd else pos_body
                    pass;      #LOG and log('pos_cmnt={}', (pos_cmnt))
                    line = line[:pos_cmnt]             +cmt_sgn+line[pos_cmnt:]
                   #line = line[:pos_body]             +cmt_sgn+line[pos_body:]

            pass;              #LOG and log('new line={}', (line))
            ed_.set_text_line(rWrk, line)
            #for rWrk
        bSkip    = get_opt('comment_move_down', True)
        if bEmpSel and bSkip:
            _move_caret_down(cCrt, rCrt)
       #def _cmt_toggle_line

    def cmt_toggle_stream(self):
        ''' '''
        if ed.get_sel_mode() != app.SEL_NORMAL:
            return app.msg_status(ONLY_NORM_SEL_MODE.format(COMMENTING))
        lex     = ed.get_prop(app.PROP_LEXER_CARET)
        ((bgn_sgn
        ,end_sgn)
        ,bOnlyLn)=self._get_cmt_pair(lex)
        if not bgn_sgn:
            return app.msg_status(CMT_NO_STRM_4LEX.format(lex))
        bUseFLn = get_opt('comment_full_line_if_no_sel', True)
        crts    = ed.get_carets()
        pass;                  #LOG and log('lex, get_carets()={}', (lex, crts))
        pass;                  #LOG and log('(bgn_sgn,end_sgn),bOnlyLn,bUseFLn={}', ((bgn_sgn,end_sgn),bOnlyLn,bUseFLn))
        for icrt, (cCrt, rCrt, cEnd, rEnd) in enumerate(crts):
            pass;              #LOG and log('(cCrt, rCrt), (cEnd, rEnd)={}', ((cCrt, rCrt), (cEnd, rEnd)))
            bEmpSel     = -1==rEnd
            bDrtSel     = -1==rEnd or (rCrt, cCrt)>(rEnd, cEnd)
            if False:pass
            elif bEmpSel and (bUseFLn or bOnlyLn):
                # Use full line
                line        = ed.get_text_line(rCrt)
                (cTx1, rTx1), (cTx2, rTx2) = (0, rCrt), (len(line), rCrt)
            elif bOnlyLn: # and not bEmpSel
                # Only full lines
                rTx1, rTx2  = minmax(rCrt, rEnd)
                line    = ed.get_text_line(rTx2)
                (cTx1, rTx1), (cTx2, rTx2) = (0, rTx1), (len(line), rTx2)
            elif bEmpSel: # and not bUseFLn and not bOnlyLn
                continue
            else:
                (rTx1, cTx1), (rTx2, cTx2) = minmax((rCrt, cCrt), (rEnd, cEnd))
            selTx   = ed.get_text_substr(cTx1, rTx1, cTx2, rTx2)
            pass;              #LOG and log('(rTx1, cTx1), (rTx2, cTx2), selTx={}', ((rTx1, cTx1), (rTx2, cTx2), repr(selTx)))
            do_uncmt= selTx.startswith(bgn_sgn) and selTx.endswith(end_sgn)
            pass;              #LOG and log('do_uncmt={}', (do_uncmt))

            if False:pass
            elif not do_uncmt and bOnlyLn:
                # Comment!
                ed.insert(0, rTx2+1, end_sgn+'\n')    #! true insert sequence
                ed.insert(0, rTx1,   bgn_sgn+'\n')    #! true insert sequence
                (cNSel1, rNSel1
                ,cNSel2, rNSel2)    = 0, rTx1, len(end_sgn), rTx2+2
            elif not do_uncmt:
                # Comment!
                ed.insert(cTx2, rTx2, end_sgn)        #! true insert sequence
                ed.insert(cTx1, rTx1, bgn_sgn)        #! true insert sequence
                if False:pass
                elif rTx1==rTx2:
                    # sel into one row
                    (cNSel1, rNSel1
                    ,cNSel2, rNSel2)    = cTx1, rTx1, cTx2+len(bgn_sgn)+len(end_sgn), rTx2
                elif rTx1!=rTx2:
                    # sel ends on diff rows
                    (cNSel1, rNSel1
                    ,cNSel2, rNSel2)    = cTx1, rTx1, cTx2             +len(end_sgn), rTx2

            elif do_uncmt and bOnlyLn:
                # UnComment!
                ed.delete(0, rTx2, 0, rTx2+1)    #! true delete sequence
                ed.delete(0, rTx1, 0, rTx1+1)    #! true delete sequence
                (cNSel1, rNSel1
                ,cNSel2, rNSel2)    = 0, rTx1, len(ed.get_text_line(rTx2-2)), rTx2-2
            elif do_uncmt:
                # UnComment!
                ed.delete(cTx2-len(end_sgn), rTx2, cTx2, rTx2)    #! true delete sequence
                ed.delete(cTx1, rTx1, cTx1+len(bgn_sgn), rTx1)    #! true delete sequence
                if False:pass
                elif rTx1==rTx2:
                    # sel into one row
                    (cNSel1, rNSel1
                    ,cNSel2, rNSel2)    = cTx1, rTx1, cTx2-len(bgn_sgn)-len(end_sgn), rTx2
                elif rTx1!=rTx2:
                    # sel ends on diff rows
                    (cNSel1, rNSel1
                    ,cNSel2, rNSel2)    = cTx1, rTx1, cTx2             -len(end_sgn), rTx2

            pass;              #LOG and log('bDrtSel, (cNSel1, rNSel1), (cNSel2, rNSel2)={}', (bDrtSel, (cNSel1, rNSel1), (cNSel2, rNSel2)))
            if bDrtSel:
                ed.set_caret(cNSel2, rNSel2, cNSel1, rNSel1, app.CARET_SET_INDEX+icrt)
            else:
                ed.set_caret(cNSel1, rNSel1, cNSel2, rNSel2, app.CARET_SET_INDEX+icrt)
           #for icrt
        bSkip    = get_opt('comment_move_down', True)
        if False:pass
        elif 1==len(crts) and bEmpSel and bUseFLn and bSkip:
            _move_caret_down(cCrt, rCrt)
            if bOnlyLn and not do_uncmt:
                crt=ed.get_carets()[0]; _move_caret_down(crt[0], crt[1])
                crt=ed.get_carets()[0]; _move_caret_down(crt[0], crt[1])
        #def cmt_toggle_stream

    def _get_cmt_pair(self, lex):
        ''' Return ((begin_sign, end_sign), only_lines)
                begin_sign    as '/*'
                end_sign      as '*/'
                only_lines    True if each of *_sign must be whole line
        '''
        if lex not in self.pair4lex:
            # Search lex-pair
            def_lexs_json   = os.path.join(get_def_setting_dir()             , 'default_lexers.json')
            usr_lexs_json   = os.path.join(app.app_path(app.APP_DIR_SETTINGS), 'user_lexers.json')
            def_lexs        = _json_loads(open(def_lexs_json).read())
            usr_lexs        = _json_loads(open(usr_lexs_json).read()) if os.path.exists(usr_lexs_json) else {"Comments":{}, "CommentsForLines":{}}
            only_ln        = False
            if False:pass
            elif lex in   usr_lexs["Comments"]:
                pair    = usr_lexs["Comments"].get(lex)
            elif lex in   usr_lexs["CommentsForLines"]:
                pair    = usr_lexs["CommentsForLines"].get(lex)
                only_ln = True
            elif lex in   def_lexs["Comments"]:
                pair    = def_lexs["Comments"].get(lex)
            elif lex in   def_lexs["CommentsForLines"]:
                pair    = def_lexs["CommentsForLines"].get(lex)
                only_ln = True
            else:
                pair    = ['','']
            self.pair4lex[lex] = (pair, only_ln)
        return self.pair4lex[lex]
       #def _get_cmt_pair

    #################################################
    ## Duplicate
    def duplicate(self):
        if ed.get_sel_mode() != app.SEL_NORMAL:
            return app.msg_status(ONLY_NORM_SEL_MODE.format(DUPLICATION))

        crts    = ed.get_carets()
        if len(crts)>1:
            return app.msg_status(ONLY_SINGLE_CRT.format(DUPLICATION))

        (cCrt, rCrt, cEnd, rEnd)    = crts[0]
        bEmpSel = -1==rEnd
        bUseFLn = get_opt('duplicate_full_line_if_no_sel', True)
        bSkip   = get_opt('duplicate_move_down', True)
        if bEmpSel:
            if not bUseFLn:
                return
            # Dup whole row
            row_txt    = ed.get_text_line(rCrt)
            ed.insert(0, rCrt, row_txt+'\n')

            # Move crt to next row
            if bSkip and (rCrt+1)<ed.get_line_count():
                _move_caret_down(cCrt, rCrt)
            return

        (rFr, cFr), (rTo, cTo)  = minmax((rCrt, cCrt), (rEnd, cEnd))
        pass;                  #LOG and log('(cFr , rFr , cTo , rTo) ={}',(cFr , rFr , cTo , rTo))
        sel_txt = ed.get_text_substr(cFr, rFr, cTo, rTo)
        pass;                  #LOG and log('sel_txt={}',repr(sel_txt))
        ed.insert(cFr, rFr, sel_txt)
       #def duplicate

    def __init__(self):
        self.opts     = {}
        self.pair4lex = {}
        #def __init__

    #class Command:

#################################################
## Common APP utils
def version(self):
    ''' Value from module doc
        Version:'value'
    '''
    return re.split('Version:', __doc__)[1].split("'")[1]

def _check_API(ver):
    if app.app_api_version()<ver:
        app.msg_status(NEED_NEWER_API)
        return False
    return True

def get_app_default_opts():
    global APP_DEFAULT_OPTS
    if not APP_DEFAULT_OPTS:
        # Once load def-opts
        def_lexs_json    = os.path.join(get_def_setting_dir(), 'default.json')
        APP_DEFAULT_OPTS = _json_loads(open(def_lexs_json).read())
    return APP_DEFAULT_OPTS
   #def get_app_default_opts

def _get_file_opts(opts_json, def_opts={}):
#   global LAST_FILE_OPTS
    if not os.path.exists(opts_json):
        pass;              #LOG and log('no {}',os.path.basename(opts_json))
        LAST_FILE_OPTS.pop(opts_json, None)
        return def_opts
    mtime_os    = os.path.getmtime(opts_json)
    if opts_json not in LAST_FILE_OPTS:
        pass;              #LOG and log('load "{}" with mtime_os={}',os.path.basename(opts_json), int(mtime_os))
        opts    = _json_loads(open(opts_json).read())
        LAST_FILE_OPTS[opts_json]       = (opts, mtime_os)
    else:
        opts, mtime = LAST_FILE_OPTS[opts_json]
        if mtime_os > mtime:
            pass;          #LOG and log('reload "{}" with mtime, mtime_os={}',os.path.basename(opts_json), (int(mtime), int(mtime_os)))
            opts= _json_loads(open(opts_json).read())
            LAST_FILE_OPTS[opts_json]   = (opts, mtime_os)
    return opts
   #def _get_file_opts

def get_opt(path, def_value=None, lev=CONFIG_LEV_ALL, ed_cfg=ed):
    ''' Overrided options tool.
        Config pairs key:val are read from
            <root>/settings_default/default.json
            <root>/settings/user.json
            <root>/settings/lexer <LEXER-NAME>.json
            ed_cfg props
        Params
            path        Simple value (e.g. "tab_size") or "/"-separated path inside JSON tree
            def_value   For return if no opt for the path into all config files
            lev         Stop finding level
                            CONFIG_LEV_ALL, CONFIG_LEV_DEF, CONFIG_LEV_USER, CONFIG_LEV_LEX, CONFIG_LEV_FILE
            ed_cfg      Ref to editor to point a lexer
        Return          Last found in config files default[/user[/lexer]] or def_value
    '''
    pass;                      #LOG and log('path, def_va, lev, ed_cfg={}',(path, def_value, lev, ed_cfg))
    keys            = path.split('/') if '/' in path else ()
    ans             = def_value

    def_opts        = get_app_default_opts()
    if lev==CONFIG_LEV_DEF:
        ans             = def_opts.get(path, def_value)   if not keys else _opt_for_keys(def_opts, keys, def_value)
        pass;                  #LOG and log('lev=DEF ans={}',(ans))
    else:
        usr_json    = os.path.join(app.app_path(app.APP_DIR_SETTINGS), 'user.json')
        usr_opts    = _get_file_opts(usr_json)
        if lev==CONFIG_LEV_USER:
            pass;              #LOG and log('def_opts(), usr_opts()={}',(def_opts.get(path),usr_opts.get(path)))
            ans         = usr_opts.get(path
                        , def_opts.get(path, def_value))  if not keys else _opt_for_keys(usr_opts, keys
                                                                          ,_opt_for_keys(def_opts, keys, def_value))
            pass;              #LOG and log('lev=USR ans={}',(ans))
        else:
            lex     = ed_cfg.get_prop(app.PROP_LEXER_CARET)
            lex_json= os.path.join(app.app_path(app.APP_DIR_SETTINGS), 'lexer {}.json'.format(lex))
            lex_opts= _get_file_opts(lex_json)
            if lev==CONFIG_LEV_LEX:
                pass;          #LOG and log('def_opts(), usr_opts(), lex_opts()={}',(def_opts.get(path),usr_opts.get(path),lex_opts.get(path)))
                ans     = lex_opts.get(path
                         ,usr_opts.get(path
                         ,def_opts.get(path, def_value))) if not keys else _opt_for_keys(lex_opts, keys
                                                                          ,_opt_for_keys(usr_opts, keys
                                                                          ,_opt_for_keys(def_opts, keys, def_value)))
                pass;          #LOG and log('lev=LEX ans={}',(ans))
            else: # lev in (CONFIG_LEV_ALL, CONFIG_LEV_FILE
                if False:pass
                elif path=='tab_size':
                    ans = ed_cfg.get_prop(app.PROP_TAB_SIZE)
                elif path=='tab_spaces':
                    ans = ed_cfg.get_prop(app.PROP_TAB_SPACES)
                elif path=='unprinted_show':
                    ans = ed_cfg.get_prop(app.PROP_UNPRINTED_SHOW)
                elif path=='unprinted_spaces':
                    ans = ed_cfg.get_prop(app.PROP_UNPRINTED_SPACES)
                elif path=='unprinted_ends':
                    ans = ed_cfg.get_prop(app.PROP_UNPRINTED_ENDS)
                elif path=='unprinted_end_details':
                    ans = ed_cfg.get_prop(app.PROP_UNPRINTED_END_DETAILS)
                elif path=='wrap_mode':
                    ans = ced_cfg.get_prop(app.PROP_WRAP)
                else:
                    pass;      #LOG and log('def_opts(), usr_opts(), lex_opts()={}',(def_opts.get(path),usr_opts.get(path),lex_opts.get(path)))
                    ans = lex_opts.get(path
                         ,usr_opts.get(path
                         ,def_opts.get(path, def_value))) if not keys else _opt_for_keys(lex_opts, keys
                                                                          ,_opt_for_keys(usr_opts, keys
                                                                          ,_opt_for_keys(def_opts, keys, def_value)))
                pass;          #LOG and log('lev=ALL ans={}',(ans))
    return ans if def_value is None else type(def_value)(ans)
   #def get_opt

def set_opt(path, value, lev=CONFIG_LEV_USER, ed_cfg=ed):
    ''' Overrided options tool.
        Config pairs key:val are add/update/delete into
            <root>/settings/user.json
            <root>/settings/lexer <LEXER-NAME>.json
            ed_cfg props
        Params
            path        Simple value (e.g. "tab_size") or "/"-separated path inside JSON tree
            value       Value for setting or deleting
                            None        Delete pair (last key in path)
                            is not None Add or update pair value
            lev         Level for set opt
                            CONFIG_LEV_USER, CONFIG_LEV_LEX, CONFIG_LEV_FILE
            ed_cfg      Ref to editor to point a lexer
        Return          The value (second param) or None if fail
    '''
    if lev==CONFIG_LEV_FILE:
        if value is None:
            # Del! Cannot del from file-lev -- can set as default
            def_opts    = get_app_default_opts()
            value       = def_opts.get(path)
        if False:pass
        elif path=='tab_size':
            ed_cfg.set_prop(app.PROP_TAB_SIZE, value)
        elif path=='tab_spaces':
            ed_cfg.set_prop(app.PROP_TAB_SPACES, value)
        elif path=='unprinted_show':
            ed_cfg.set_prop(app.PROP_UNPRINTED_SHOW, value)
        elif path=='unprinted_spaces':
            ed_cfg.set_prop(app.PROP_UNPRINTED_SPACES, value)
        elif path=='unprinted_ends':
            ed_cfg.set_prop(app.PROP_UNPRINTED_ENDS, value)
        elif path=='unprinted_end_details':
            ed_cfg.set_prop(app.PROP_UNPRINTED_END_DETAILS, value)
        elif path=='wrap_mode':
            ed_cfg.set_prop(app.PROP_WRAP, value)
        else:
            app.msg_status(CONFIG_MSG_DONT_SET_FILE)
            return None # Fail!
        return value

    if lev==CONFIG_LEV_LEX and ed_cfg is None: return None # Fail!
    lex     = ed_cfg.get_prop(app.PROP_LEXER_CARET) if ed_cfg is not None else ''
    cfg_json= os.path.join(app.app_path(app.APP_DIR_SETTINGS), icase(False,''
              ,lev==CONFIG_LEV_USER                          , 'user.json'
              ,lev==CONFIG_LEV_LEX                           , 'lexer {}.json'.format(lex)
                                                             , ''))
    pass;                      #LOG and log('cfg_json={}',(cfg_json))
    if not os.path.exists(cfg_json)  and value is     None:
        return None #?? success or fail?
    if not os.path.exists(cfg_json):#and value is not None
        # First pair for this file
        dct     = {path:value}
        if '/' in path:
            keys= path.split('/')
            dct = {}
            dic = dct
            for ikey in range(len(keys)):
                key     = keys[ikey]
                if ikey+1<len(keys):
                    dic = dic.setdefault(key, {})
                else:
                    dic[key] = value
        open(cfg_json, 'w').write(json.dumps(dct, indent=4))
        return value

    # Try to modify file
    body    = open(cfg_json).read()
    value4js= json.dumps({'':value})[len('{"": '):-1]    # format for json
    if '/' in path:
        # Complex path
        pass;                   app.msg_status(CONFIG_MSG_DONT_SET_PATH)
        pass;                   return None # Fail!
    else:
        # Simple key
        # Assumptions:
        #    one key:val into one row
        re_key_val  = r'^\s*,?\s*"{}"\s*:.+'.format(re.escape(path))
        cre         = re.compile(re_key_val, re.MULTILINE)
        has_pair    = cre.search(body) is not None
        pass;                  #LOG and log('re_key_val, has_pair={}',(re_key_val,has_pair))
        if False:pass
        elif has_pair and value is None:
            # Delete!
            pass;              #LOG and log('del!',)
            body    = cre.sub('', body)
        elif has_pair and value is not None:
            # Update!
            pass;              #LOG and log('upd!',)
            body    = cre.sub('    "{}": {},'.format(path, value4js), body)
        elif not has_pair and value is None:
            # OK
            pass
        elif not has_pair:
            # Add! before end
            pass;              #LOG and log('add!',)
            body    = body.rstrip(' \t\r\n')[:-1].rstrip(' \t\r\n')
            body= body+'{}\n    "{}": {},\n}}'.format(
                         '' if body[-1] in ',{' else ','
                       , path
                       , value4js)
    open(cfg_json, 'w').write(body)
    return value
   #def set_opt

def _move_caret_down(cCrtSmb, rCrt, ed_=ed, id_crt=app.CARET_SET_ONE):
    ''' Caret will be moved to next line with save start column (if next line exists)
        Params
            cCrtSmb     Start pos as symbol number
            rCrt        Start line
            ed_         Editor
            id_crt      CARET_SET_ONE or CARET_SET_INDEX+N for caret with index N
    '''
    pass;                      #LOG and log('cCrtSmb, rCrt, id_crt==app.CARET_SET_ONE={}',(cCrtSmb, rCrt, id_crt==app.CARET_SET_ONE))
    if (rCrt+1)>=ed_.get_line_count():    return
    colCrt  = ed.convert(app.CONVERT_CHAR_TO_COL, cCrtSmb, rCrt  )[0]
    cCrtSmb1= ed.convert(app.CONVERT_COL_TO_CHAR, colCrt,  rCrt+1)[0]
    ed_.set_caret(cCrtSmb1, rCrt+1, id=id_crt)
   #def _move_caret_down

def _json_loads(s, **kw):
    ''' Adapt s for json.loads
            Delete comments
            Delete unnecessary ',' from {,***,} and [,***,]
    '''
    s = re.sub('//.*'   , ''  , s)
    s = re.sub('{\s*,'  , '{' , s)
    s = re.sub(',\s*}'  , '}' , s)
    s = re.sub('\[\s*,' , '[' , s)
    s = re.sub(',\s*\]' , ']' , s)
    pass;                      #LOG and log('s={}',s)
    return json.loads(s, **kw)
    #def _json_loads

def get_def_setting_dir():
    pass;                     #LOG and log('os.path.dirname(app.app_path(app.APP_DIR_SETTINGS))={}', os.path.dirname(app.app_path(app.APP_DIR_SETTINGS)))
    return os.path.join(
                os.path.dirname(app.app_path(app.APP_DIR_SETTINGS))
            ,   'settings_default' )
    #def get_def_setting_dir

def _opt_for_keys(dct_tree, keys=(), def_val=None):
    ''' Get opt as full dct_tree or sub-dict or single value
        Params
            keys    Path for dct_tree
                    (str1, str2, ...)
                    str
            def_val Default for return
        Return      dct_tree if keys==()
                    dct_tree[keys[0]][keys[1]][keys[2]]
                    def_val if any keys[*] not in [sub-]dict
    '''
    if isinstance(keys, str):
        return dct_tree.get(keys, def_val)
    ans     = dct_tree
    for k in keys:
        if not isinstance(ans, dict): return def_val
        if k not in ans:              return def_val
        ans = ans.get(k)
    return ans
    #def _opt_for_keys

def minmax(v1, v2):
    return min(v1, v2), max(v1, v2)

def icase(*pars):
    """ Params    cond1,val1[, cond2,val2, ...[, valElse]...]
        Result    Value for first true cond in pairs otherwise last odd param or None
        Examples
            icase(1==2,'a', 3==3,'b') == 'b'
            icase(1==2,'a', 3==4,'b', 'c') == 'c'
            icase(1==2,'a', 3==4,'b') == None
    """
    for ppos in range(1,len(pars),2) :
        if pars[ppos-1] :
            return pars[ppos]
    return pars[-1] if 1==len(pars)%2 else None
    #def icase

def log(msg='', *args):
    """ en:
        Light print-logger. Commands are included into msg:
            >> << {{    Expand/Narrow/Cancel gap
        Execute msg.format(*args).  So you can insert Format String Syntax into msg.
        Replace '¬' to chr(9), '¶'to chr(10).

        Example.
        1    class C:
        2        def m():
        3            log('qwerty')
        4            log('>>more gap here')
        5            log('v1={}¶v2,v3¬{}',12,('ab',{}))
        6            log('<<less gap at next')
        7            log('QWERTY')
        output
            C.m:3 qwerty
                C.m:4 >>more gap here
                C.m:5 v1=12
            v2,v3    ('ab', {})
                C.m:6 <<less gap at next
            C.m:7 QWERTY
    """
    global log_gap
    lctn    = ''
    if -1==-1: # add "location"
        frCaller= inspect.stack()[1]    # 0-log, 1-need func
        try:
            cls = frCaller[0].f_locals['self'].__class__.__name__ + '.'
        except:
            cls = ''
        fun,ln  = (cls + frCaller[3]).replace('.__init__','()'), frCaller[2]
        lctn    = '{}:{} '.format(fun, ln)

    if 0<len(args):
        msg = msg.format(*args)
    log_gap = log_gap + (chr(9) if '>>' in msg else '')
    msg     = log_gap + lctn + msg.replace('¬',chr(9)).replace('¶',chr(10))

    print(msg)

    log_gap = icase('<<' in msg, log_gap[:-1]
                   ,'{{' in msg, ''
                   ,             log_gap )
    #def log

'''
ToDo
[S][кто-кому][дата] Что сделать
    [S] Состояние: [ ] Не реализовано, [+] Сделано, [-] Не требуется, [?] Нужны уточнения
    [кто-кому] Автор-Исполнитель. Принимает значения: [kv] kvichans, [at] Alexey-T
    [дата] Когда придумано
---------------
[+][kv-kv][27oct15] full-line-if-no-sel for stream-cmt
[+][kv-at][27oct15] Дать способ вертикального перемещения каретки вниз
[+][kv-kv][27oct15] При Cmt1st не вставлять // перед пробелами, а заменять
[ ][kv-kv][27oct15] ! Разрешить Комм и Дубл для режима "много кареток"
[ ][kv-at][27oct15] Дать механизм для Localization
[-][kv-at][27oct15] Дать доступ из плагинов к командам из cudax_lib
[ ][kv-at][27oct15] Спрятать в меню Plugins команды из cudax_lib
[+][kv-kv][28oct15] Применять cmt_toggle_line_(1st|body) к каждой строке в выделении, решение по первой строке
[+][kv-at][28oct15] Включить настройки cudax_lib в общие default.json и user.json
[ ][kv-kv][28oct15] Контролировать, что выделенный фрагмент имеет один Лексер
[+][at-kv][29oct15] Вычислять по всему выделению оптимальную позицию для body - min(body_pos)
[+][kv-kv][29oct15] Брать/использовать комментарии из def_lexs.json\CommentsForLines
[?][kv-at][01nov15] (bug!) При быстром последовательном клике Ctrl+/ переход на новую строку не отрабатывает
[-][kv-kv][01nov15] Разделить set_opt на set_opt и del_opt
[+][kv-kv][02nov15] При stream-comm выделение направлять как исходное
[?][kv-at][02nov15] При stream-comm верт.выделении с неск каретками не давать ed.get_sel_mode()==SEL_COLUMN
[ ][kv-kv][02nov15] При line-comm и добавлении/удалении символов перед выделением происходит смещение выделения. Избавиться!
'''