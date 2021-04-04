""" Py-extensions for CudaText.
Overridden option tools:
    get_opt(path, def_value=None, level=CONFIG_LEV_ALL, ed=ed, lexer='')
        Reads option from configs chain default-user-lexer-file.
        Path: simple value (e.g. "tab_size") or "/"-separated path inside JSON tree
    set_opt(path, value, ed=ed, level=CONFIG_LEV_USER, ed=ed, lexer='')
        Add/Update/Delete option into a config (user-config if no param level).
Duplicate:
    duplicate
        Dub cur selection or cur line (by opt)
Authors:
    Andrey Kvichansky    (kvichans on github)
Version:
    '0.6.10 2020-10-14'
Wiki: github.com/kvichans/cudax_lib/wiki
ToDo: (see end of file)
"""

import  cudatext        as app
from    cudatext    import ed
import  cudatext_cmd    as cmds
import  os, json, re, sys, collections, gettext
odict       = collections.OrderedDict

# Overridden option tools:
CONFIG_LEV_DEF      = 'def'
CONFIG_LEV_USER     = 'user'
CONFIG_LEV_USER_ONLY= 'user-only'
CONFIG_LEV_LEX      = 'lex'
CONFIG_LEV_LEX_ONLY = 'lex-only'
CONFIG_LEV_FILE     = 'file'
CONFIG_LEV_ALL      = 'dulf'
OPT2PROP            = dict(
     caret_after_end            = app.PROP_CARET_VIRTUAL
    ,gutter_show                = app.PROP_GUTTER_ALL
    ,gutter_bookmarks           = app.PROP_GUTTER_BM
    ,gutter_fold                = app.PROP_GUTTER_FOLD
    ,show_cur_column            = app.PROP_HILITE_CUR_COL
    ,show_cur_line              = app.PROP_HILITE_CUR_LINE
    ,show_cur_line_minimal      = app.PROP_HILITE_CUR_LINE_MINIMAL
    ,show_cur_line_only_focused = app.PROP_HILITE_CUR_LINE_IF_FOCUS
    ,indent_auto                = app.PROP_INDENT_AUTO
    ,unindent_keeps_align       = app.PROP_INDENT_KEEP_ALIGN
    ,indent_kind                = app.PROP_INDENT_KIND
    ,indent_size                = app.PROP_INDENT_SIZE
    ,links_hilite               = app.PROP_LINKS_SHOW
    ,links_regex                = app.PROP_LINKS_REGEX
    ,mouse_click_links          = app.PROP_LINKS_CLICKS
    ,show_last_line_on_top      = app.PROP_LAST_LINE_ON_TOP
    ,margin                     = app.PROP_MARGIN
    ,margin_string              = app.PROP_MARGIN_STRING
    ,micromap_show              = app.PROP_MICROMAP
    ,minimap_show               = app.PROP_MINIMAP
    ,ruler_show                 = app.PROP_RULER
    ,tab_size                   = app.PROP_TAB_SIZE
    ,tab_spaces                 = app.PROP_TAB_SPACES
    ,unprinted_ends             = app.PROP_UNPRINTED_ENDS
    ,unprinted_end_details      = app.PROP_UNPRINTED_END_DETAILS
    ,unprinted_show             = app.PROP_UNPRINTED_SHOW
    ,unprinted_spaces           = app.PROP_UNPRINTED_SPACES
    ,unprinted_spaces_trailing  = app.PROP_UNPRINTED_SPACES_TRAILING
    ,wrap_mode                  = app.PROP_WRAP
    ,zebra                      = app.PROP_ZEBRA
    ,zebra_step                 = app.PROP_ZEBRA_STEP
    )

# Localization
CONFIG_MSG_DONT_SET_FILE= 'Cannot set editor properties'
NEED_NEWER_API          = 'Needs newer app version'
DUPLICATION             = 'Duplication'
ONLY_NORM_SEL_MODE      = '{} works only with normal selection'
ONLY_SINGLE_CRT         = "{} doesn't work with multi-carets"

pass;                           # Logging
pass;                           import inspect  # stack
pass;                           from pprint import pformat
pass;                           pfrm15=lambda d:pformat(d,width=15)
pass;                           LOG = (-2==-2)  # Do or dont logging.
pass;                           log_gap = ''    # use only into log()

APP_DEF_LEX_OPTS    = {}
APP_DEFAULT_OPTS    = {}
LAST_FILE_OPTS      = {}
class Command:
    #################################################
    ## Duplicate
    def duplicate(self):
        if ed.get_prop(app.PROP_RO): return

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
        ed.set_caret(cCrt, rCrt, cEnd, rEnd)
       #def duplicate

    #class Command

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

def get_app_default_opts(lexer='', **kw):
    pass;                      #LOG and log('kw={}',kw)
    global APP_DEFAULT_OPTS, APP_DEF_LEX_OPTS
    if not APP_DEFAULT_OPTS:
        # Once load def-opts
        def_json    = os.path.join(get_def_setting_dir(), 'default.json')
        APP_DEFAULT_OPTS = _json_loads(open(def_json, encoding='utf8').read(), **kw)
#       APP_DEFAULT_OPTS = _json_loads(open(def_json, encoding='utf8').read(), object_pairs_hook=collections.OrderedDict, **kw)
#       APP_DEFAULT_OPTS = _json_loads(open(def_json).read(), **kw)
    if not lexer:
        return APP_DEFAULT_OPTS
    lex_json        = os.path.join(get_def_setting_dir(), 'lexer {}.json'.format(lexer))
    if not os.path.exists(lex_json):
        return APP_DEFAULT_OPTS
    lex_opts    = APP_DEF_LEX_OPTS.get(lexer)
    if not lex_opts:
        lex_opts    = _json_loads(open(lex_json, encoding='utf8').read(), **kw)
        def_opts    = APP_DEFAULT_OPTS.copy()
        def_opts.update(lex_opts)
        lex_opts    = def_opts
#       lex_opts    = {**APP_DEFAULT_OPTS, **lex_opts}
        APP_DEF_LEX_OPTS[lexer] = lex_opts
    return lex_opts
   #def get_app_default_opts

def _get_file_opts(opts_json, def_opts={}, **kw):
#   global LAST_FILE_OPTS
    if not os.path.exists(opts_json):
        pass;                  #LOG and log('no {}',os.path.basename(opts_json))
        LAST_FILE_OPTS.pop(opts_json, None)
        return def_opts
    mtime_os    = os.path.getmtime(opts_json)
    if opts_json not in LAST_FILE_OPTS:
        pass;                  #LOG and log('load "{}" with mtime_os={}',os.path.basename(opts_json), int(mtime_os))
        opts    = _json_loads(open(opts_json, encoding='utf8').read(), **kw)
        LAST_FILE_OPTS[opts_json]       = (opts, mtime_os)
    else:
        opts, mtime = LAST_FILE_OPTS[opts_json]
        if mtime_os > mtime:
            pass;              #LOG and log('reload "{}" with mtime, mtime_os={}',os.path.basename(opts_json), (int(mtime), int(mtime_os)))
            opts= _json_loads(open(opts_json, encoding='utf8').read(), **kw)
            LAST_FILE_OPTS[opts_json]   = (opts, mtime_os)
    return opts
   #def _get_file_opts

def get_opt(path, def_value=None, lev=CONFIG_LEV_ALL, ed_cfg=ed, lexer='', user_json='user.json'):
    ''' Overridden options tool.
        Config pairs key:val are read from
            <root>/settings_default/default.json
            <root>/settings/user.json
            <root>/settings/lexer <LEXER-NAME>.json
            ed_cfg props
        Params
            path        Simple value (e.g. "tab_size") or "/"-separated path inside JSON tree
            def_value   For return if no opt for the path into all config files
            lev         Stop level to search in chain default-user-lexer-file
                            CONFIG_LEV_ALL, CONFIG_LEV_DEF, CONFIG_LEV_USER[_ONLY], CONFIG_LEV_LEX[_ONLY], CONFIG_LEV_FILE
            ed_cfg      Ref to editor to point a lexer (over first caret)
                        Used only if lev in (CONFIG_LEV_LEX, CONFIG_LEV_FILE)
            lexer       Explicit lexer name (lexer from ed_cfg not used).
                        Used only if lev==CONFIG_LEV_LEX
        Return          Last found in config chain default-user-lexer-file or def_value
    '''
    pass;                      #LOG and log('path, def_va, lev, ed_cfg, lexer={}',(path, def_value, lev, ed_cfg, lexer))
    keys            = path.split('/') if '/' in path else ()
    ans             = def_value

    def_opts        = get_app_default_opts(lexer=lexer)
    if lev==CONFIG_LEV_DEF:
        ans             = def_opts.get(path, def_value)   if not keys else _opt_for_keys(def_opts, keys, def_value)
        pass;                  #LOG and log('lev=DEF ans={}',(ans))
    else:
        usr_json    = os.path.join(app.app_path(app.APP_DIR_SETTINGS), user_json)
        usr_opts    = _get_file_opts(usr_json)
        if lev in (CONFIG_LEV_USER, CONFIG_LEV_USER_ONLY):
            pass;              #LOG and log('def_opts(), usr_opts()={}',(def_opts.get(path),usr_opts.get(path)))
            ans         = usr_opts.get(path, def_value)   if not keys else _opt_for_keys(usr_opts, keys, def_value) \
                            if lev==CONFIG_LEV_USER_ONLY else \
                          usr_opts.get(path
                        , def_opts.get(path, def_value))  if not keys else _opt_for_keys(usr_opts, keys
                                                                          ,_opt_for_keys(def_opts, keys, def_value))
            pass;              #LOG and log('lev=USR ans={}',(ans))
        else:
            lex     = lexer                                 if lexer    else \
                      ed_cfg.get_prop(app.PROP_LEXER_CARET) if ed_cfg   else ''
            lex_json= os.path.join(app.app_path(app.APP_DIR_SETTINGS), 'lexer {}.json'.format(lex))
            lex_opts= _get_file_opts(lex_json)
            if lev in (CONFIG_LEV_LEX, CONFIG_LEV_LEX_ONLY):
                pass;          #LOG and log('def_opts(), usr_opts(), lex_opts()={}',(def_opts.get(path),usr_opts.get(path),lex_opts.get(path)))
                ans     = lex_opts.get(path, def_value)   if not keys else _opt_for_keys(lex_opts, keys, def_value) \
                            if lev==CONFIG_LEV_LEX_ONLY else \
                          lex_opts.get(path
                         ,usr_opts.get(path
                         ,def_opts.get(path, def_value))) if not keys else _opt_for_keys(lex_opts, keys
                                                                          ,_opt_for_keys(usr_opts, keys
                                                                          ,_opt_for_keys(def_opts, keys, def_value)))
                pass;          #LOG and log('lev=LEX ans={}',(ans))
            else: # lev in (CONFIG_LEV_ALL, CONFIG_LEV_FILE
                if path in OPT2PROP:
                    ans = ed_cfg.get_prop(OPT2PROP[path])
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

def set_opt(path, value, lev=CONFIG_LEV_USER, ed_cfg=ed, lexer='', user_json='user.json'):
    ''' Overridden options tool.
        Config pairs key:val are add/update/delete into
            <root>/settings/user.json
            <root>/settings/lexer <LEXER-NAME>.json
            ed_cfg props
        Params
            path        Simple value (e.g. "tab_size") or "/"-separated path inside JSON tree
            value       Value for setting or deleting
                            None        Delete pair (last key in path)
                            not None    Add or update pair value
            lev         Level for set opt
                            CONFIG_LEV_USER, CONFIG_LEV_LEX, CONFIG_LEV_FILE
                        For CONFIG_LEV_FILE path only from
                            tab_size
                            tab_spaces
                            unprinted_show
                            unprinted_spaces
                            unprinted_ends
                            unprinted_end_details
                            wrap_mode
            ed_cfg      Ref to editor to point a lexer (over first caret)
                        Used only if lev in (CONFIG_LEV_LEX, CONFIG_LEV_FILE)
            lexer       Explicit lexer name (lexer from ed_cfg not used).
        Return          The value (second param) or None if fail
    '''
    if lev==CONFIG_LEV_FILE:
        if value is None:
            # Del! Cannot del from file-lev -- can set as default
            def_opts    = get_app_default_opts(lexer=lexer)
            value       = def_opts.get(path)
        else:
            value       = str(value)
        if path in OPT2PROP:
            ed_cfg.set_prop(OPT2PROP[path], value)
        else:
            app.msg_status(CONFIG_MSG_DONT_SET_FILE)
            return None # Fail!
        return value

    lev = CONFIG_LEV_LEX                                if lexer    else lev
    lex = ''
    if lev==CONFIG_LEV_LEX:
        lex     = lexer                                 if lexer    else \
                  ed_cfg.get_prop(app.PROP_LEXER_CARET) if ed_cfg   else ''
        if not lex: return None # Fail!
    cfg_json= os.path.join(app.app_path(app.APP_DIR_SETTINGS), icase(False,''
              ,lev==CONFIG_LEV_USER                          , user_json
              ,lev==CONFIG_LEV_LEX                           , 'lexer {}.json'.format(lex)
                                                             , ''))
    pass;                      #LOG and log('cfg_json={}',(cfg_json))
    if not os.path.exists(cfg_json)     and value is     None:
        return None # SUCCESS (or fail?)

    kv_dct  = {path:value}
    keys    = path.split('/')
    if '/' in path:
        kv_dct  = {}
        dic     = kv_dct
        for ikey,key in enumerate(keys):
            if ikey+1<len(keys):
                dic = dic.setdefault(key, {})
            else:
                dic[key] = value

    if not os.path.exists(cfg_json)     and value is not None:
        # First pair for this file
        open(cfg_json, 'w', encoding='utf8').write(json.dumps(kv_dct, indent=4))
        return value

    # Try to modify file
    body    = open(cfg_json, encoding='utf8').read()
    value4js= json.dumps({"": value})[len('{"": '):-1]      # Format value as 'after ": " string'
    if '/' in path:
        # Trick:
        #   1. Replace all comments to cmkey:value
        #   2. Parse
        #   3. Modify
        #   4. Dump
        #   5. Replace all cmkey:value to comments
        # Comments
        #   //abc
        #     //xyz
        #   //"a": "\t"
        # Pairs
        # "__c_m_n_t_0__":"  //abc"
        # "__c_m_n_t_1__":"    //xyz"
        # "__c_m_n_t_2__":"  //\"a\": \"\\t\""
        # Comment # N
        #   //smth
        # is replaced to pair
        #   "__c_m_n_t_N__":"SMTH"
        # where
        #   SMTH = repr(r'  //smth')
        def comms2pairs(text):
            co_num  = 0
            def c2p(match):     # Saving: Replace all json comments to spec_key:val_from_comment
                nonlocal co_num
                co_num += 1
                return '"__c_m_n_t_{}__": "{}",'.format(co_num, repr(match.group(0)).replace('"', r'\"'))
            return re.sub(r'^\s*//.*', c2p, text, flags=re.MULTILINE)
        def pairs2comms(text):  # Restoring: Replace all spec_key:val to json comment lines
            def p2c(match):
                return eval(match.group(1))
            return re.sub(r'^\s*"__c_m_n_t_\d+__": "(.*)",?$', p2c, text, flags=re.MULTILINE)
        # 1. Repl   2. Parse
        body_c2p = comms2pairs(body)
        body_c2p = re.sub(r',\s*}$', '}', body_c2p)     # Kill "," after last value
        body_js  = _json_loads(body_c2p, object_pairs_hook=odict)
#       body_js  = json.loads(body_c2p, object_pairs_hook=odict)
        # 3. Modify
        node    = body_js
        kv_node = None
        for ikey,key in enumerate(keys):
            assert isinstance(node, dict)
            if key not in node and value is None:
                return None                             # Nothing to modify
            if ikey+1==len(keys):                       # Last path segment
                if value is None:
                    del node[key]                       # Remove pair
                    remove_empty_nodes(body_js, keys)
                else:
                    log(_('Warning: section is overwrited with simple key.\n\tfile={}\n\tpath={}').format(
                            cfg_json, '/'.join(keys[:ikey+1]), node[key])) \
                        if key in node and isinstance(node[key], dict) else 0
                    if node.get(key)==value:
                        return value                    # No need to modify
                    node[key]   = value                 # Change or Add
            else:                                       # Middle path segment
                if key not in node:
                    node    = node.setdefault(key, {})  # Add section
                elif isinstance(node[key], dict):
                    node    = node[key]                 # Step down
                else:                                   # key is not for section
                    log(_('Warning: simple key is overwrited with section.\n\tfile={}\n\tpath={}\n\told value={}').format(
                            cfg_json, '/'.join(keys[:ikey+1]), node[key]))
                    node[key]   = {}
                    node    = node[key]                 # Step down
           #for ikey,key
        # 4. Dump   5. Repl
        body_pr  = json.dumps(body_js, indent=4)
        body     = pairs2comms(body_pr)
    else:
        # Simple key
        # Assumptions:
        #    one key:val into one row
        sre_key_val = r'^\s*,?\s*"{}"\s*:(.+)'.format(re.escape(path))
        cre_key_val = re.compile(sre_key_val, re.MULTILINE)  # MULTILINE for ^
        mt_key_val  = cre_key_val.search(body)
        has_pair    = mt_key_val is not None
        pass;                  #LOG and log('re_key_val, has_pair={}',(re_key_val,has_pair))
        if False:pass
        elif has_pair and value is None:
            # Delete!
            pass;              #LOG and log('del!',)
            body    = cre_key_val.sub('', body)     # Will empty line
        elif has_pair and value is not None:
            # Update?
            if mt_key_val.group(1).strip(' \t,') == value4js:
                # Skip! Value is same
                return value
            # Update!
            new_pair= '    "{}": {},'.format(path, value4js.replace('\\', r'\\'))
            body    = cre_key_val.sub(new_pair, body)   # NB! backslash escapes in 1st par are processed
        elif not has_pair and value is None:
            # Skip! Nothing to delete
            return value
        elif not has_pair:
            # Add! before end
            pass;              #LOG and log('add!',)
            body    = body.rstrip(' \t\r\n')[:-1].rstrip(' \t\r\n')
            body= body+'{}\n    "{}": {},\n}}'.format(
                         '' if body[-1] in ',{' else ','
                       , path
                       , value4js)
    open(cfg_json, 'w', encoding='utf8').write(body)
    return value
   #def set_opt

def remove_empty_nodes(tree, keys):
    prn_keys= []
    prn     = tree
    for key in keys:
        if key not in prn: break
        prn_keys   += [(prn, key)]
        prn         = prn[key]
    for prn, key in reversed(prn_keys):
        if not prn[key]:
            del prn[key]
   #def remove_empty_nodes

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
#   s = re.sub(r'(^|[^:])//.*'  , r'\1', s)     # :// in http://
#   s = re.sub(r'(^|[^:])//.*'  , r'\1', s, flags=re.MULTILINE)     # :// in http://
    def rm_cm(match):
        line    = match.group(0)
        pos     = 0
        in_str  = False
        while pos<len(line):
            ch  = line[pos]
            if ch=='\\':
                pos += 2
            else:
                if ch=='"':
                    in_str = not in_str
                else:
                    if line[pos:pos+2]=='//' and not in_str:
                        return line[:pos]
                pos += 1
        return line
    s = re.sub(r'^.*//.*$'     , rm_cm, s, flags=re.MULTILINE)     # re.MULTILINE for ^$
    s = re.sub(r'{\s*,'         , r'{' , s)
    s = re.sub(r',\s*}'         , r'}' , s)
    s = re.sub(r'\[\s*,'        , r'[' , s)
    s = re.sub(r',\s*\]'        , r']' , s)
    try:
        ans = json.loads(s, **kw)
    except:
        pass;                   #LOG and log('FAIL: s={}',s)
        pass;                   #LOG and log('sys.exc_info()={}',sys.exc_info())
        log_file    = kw.get('log_file', _get_log_file())
        open(log_file, 'a').write('_json_loads FAIL: s=\n'+s)
        print('ERROR: error on loading json. Log file:', log_file)
        ans = {}
    return ans
    #def _json_loads

def get_tab_by_id(tab_id):
    for h in app.ed_handles():
        try_ed  = app.Editor(h)
        if int(tab_id) == try_ed.get_prop(app.PROP_TAB_ID, ''):
            return try_ed
    return None

def get_groups_count():
    dct = {
        app.GROUPS_ONE      : 1,
        app.GROUPS_2VERT    : 2,
        app.GROUPS_2HORZ    : 2,
        app.GROUPS_3VERT    : 3,
        app.GROUPS_3HORZ    : 3,
        app.GROUPS_3PLUS    : 3,
        app.GROUPS_1P2VERT  : 3,
        app.GROUPS_1P2HORZ  : 3,
        app.GROUPS_4VERT    : 4,
        app.GROUPS_4HORZ    : 4,
        app.GROUPS_4GRID    : 4,
        app.GROUPS_6GRID    : 6
    }
    gr_mode = app.app_proc(app.PROC_GET_GROUPING, '')
    return dct.get(gr_mode, 1)

def get_enabled_lexers():
    return app.lexer_proc(app.LEXER_GET_LEXERS, False)

def choose_avail_lexer(lxr_names):
    """ Choose from lxr_names first enabled lexer """
    all_lxrs  = get_enabled_lexers()
    for lxr in lxr_names:
        if lxr in all_lxrs:
            return lxr
    return ''

def _get_log_file():
    return os.path.join(app.app_path(app.APP_DIR_SETTINGS), 'cudax.log')

def get_def_setting_dir():
    pass;                     #LOG and log('os.path.dirname(app.app_path(app.APP_DIR_SETTINGS))={}', os.path.dirname(app.app_path(app.APP_DIR_SETTINGS)))
    return app.app_path(app.APP_DIR_SETTINGS_DEF)
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

def int_to_html_color(n):
    """
    Convert int to HTML color '#rrggbb'
    """
    s = '%06x' % n
    r, g, b = s[4:], s[2:4], s[:2]
    return '#'+r+g+b
   #def int_to_html_color

def html_color_to_int(s):
    """
    Convert HTML color '#RRGGBB' or '#RGB' to int
    """
    s = s.strip().lstrip('#')
#   while s[0] == '#': s = s[1:]
    if len(s)==3:
        s = s[0]*2 + s[1]*2 + s[2]*2
    if len(s)!=6:
        raise Exception('Incorrect color token: '+s)
    s = s[4:6] + s[2:4] + s[0:2]
    color = int(s, 16)
    return color
   #def html_color_to_int

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

def log(msg='', *args, **kwargs):
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
    if '###' in msg :   # Add stack info
        st_inf  = '\n###'
        for fr in inspect.stack()[2:]:
            try:
                cls = fr[0].f_locals['self'].__class__.__name__ + '.'
            except:
                cls = ''
            fun,ln  = (cls + fr[3]).replace('.__init__','()'), fr[2]
            st_inf += '    {}:{}'.format(fun, ln)
        msg    += st_inf

    if args or kwargs:
        msg = msg.format(*args, **kwargs)
    log_gap = log_gap + (chr(9) if '>>' in msg else '')
    msg     = log_gap + lctn + msg.replace('¬',chr(9)).replace('¶',chr(10))

    _out_h  = kwargs.pop('_out_h', None)
    _out_s  = kwargs.pop('_out_s', None)
    pass;                      #print('_out_h={}, _out_s={}'.format(_out_h, _out_s))
    if False:pass
    elif _out_h:
        _out_h.write(msg+chr(10))
    elif _out_s:
        with open(_out_s, 'a') as _out_h:
            _out_h.write(msg+chr(10))
    else:
        print(msg)

    log_gap = icase('<<' in msg, log_gap[:-1]
                   ,'{{' in msg, ''
                   ,             log_gap )
    #def log


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
        4. <module>.mo can be placed also in dir
            CudaText\data\langpy\ru_RU\LC_MESSAGES\<module>.mo
           The dir is used first.
        5. get_translation uses the file to realize
            _('')
    '''
    lng      = app.app_proc(app.PROC_GET_LANG, '')
    plug_dir = os.path.dirname(plug_file)
    plug_mod = os.path.basename(plug_dir)
    lng_dirs = [
                 app.app_path(app.APP_DIR_DATA)  + os.sep + 'langpy',
                 plug_dir                        + os.sep + 'lang',
               ]
    _        =  lambda x: x
    pass;                      #return _
    for lng_dir in lng_dirs:
        lng_mo = lng_dir+'/{}/LC_MESSAGES/{}.mo'.format(lng, plug_mod)
        if os.path.isfile(lng_mo):
            t = gettext.translation(plug_mod, lng_dir, languages = [lng])
            _ = t.gettext
            t.install()
            break
    return _
   #def get_translation


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
[+][kv-kv][02nov15] При line-comm и добавлении/удалении символов перед выделением происходит смещение выделения. Избавиться!
'''
