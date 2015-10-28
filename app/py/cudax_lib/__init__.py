''' Py-extensions for CudaText.
    Comments:
        cmt_toggle_stream       Add/Remove stream comment (as /*....*/) for cur selection
                                or (if full-line-if-no-sel) for cur line
        cmt_toggle_line_1st     Add/Remove comment at 1st pos of line
        cmt_toggle_line_body    Add/Remove comment at 1st pos of text
    Duplicate:
        duplicate               Dub line or cur selection (if it's not empty)

    Authors:    Andrey Kvichansky    (kvichans on githab)
                ???                  (Alexey-T on githab)

    Version:    '0.1.1 2015-10-28'
    History:
    + ???
    - ???
    * ???

    ToDo:        See end of file
'''

import  cudatext        as app
from    cudatext    import ed
import  cudatext_cmd    as cmds
import  os, json, re

# Localization
NEED_NEWER_API      = 'Needs newer app version'
COMMENTING          = 'Commenting'
DUPLICATION         = 'Duplication'
ONLY_NORM_SEL_MODE  = '{} works only with normal selection'
CMT_NO_LINE_4LEX    = 'No line comment for lexer "{}" '
CMT_NO_STRM_4LEX    = 'No stream comment for lexer "{}'
ONLY_SINGLE_CRT     = '{} doesnt works with many carets/selections'

DEF_OPTS = {
    'cmt':{
        'full-line-if-no-sel':True  # for stream
    ,   'skip-line-after':True      # for cmt_toggle_line_* and 'full-line-if-no-sel':True
    ,   'save_body_col':False       # replace blanks<->cmt to save col of body (PROP_TAB_SPACES==true)? for cmt_toggle_line_(1st|body)
    }
,   'dup':{
        'full-line-if-no-sel':True  # dup cur line
    ,   'skip-line-after':True      # caret to new line
    }
}

pass;                           # Logging
pass;                           import inspect  # stack
pass;                           LOG = False      # Do or dont logging.
pass;                           log_gap = ''    # use only into log()

class Command:
    ###############################################
    ## Comments
    def cmt_toggle_line_1st(self):
        return self._cmt_toggle_line('1st')
    def cmt_toggle_line_body(self):
        return self._cmt_toggle_line('bod')
    def _cmt_toggle_line(self, cmt_type):
        ''' Add/Remove line comment
            Params
                cmt_type    '1st'    at begin of line
                            'bod'    at first not blank
        '''
        if not _check_API('1.0.107'):    return
        if ed.get_sel_mode() != app.SEL_NORMAL:
            return app.msg_status(ONLY_NORM_SEL_MODE.format(COMMENTING))

        lex         = ed.get_prop(app.PROP_LEXER_CARET)
        cmt_sgn     = app.lexer_proc(app.LEXER_GET_COMMENT, lex)
        pass;                   LOG and log('lex, cmt_sgn={}', (lex, cmt_sgn))
        if not cmt_sgn:
            return app.msg_status(CMT_NO_LINE_4LEX.format(lex))
        crts        = ed.get_carets()
        if len(crts)>1:
            return app.msg_status(ONLY_SINGLE_CRT.format(COMMENTING))

        no_tab      = ed.get_prop(app.PROP_TAB_SPACES)    # only blanks
        opts        = self._get_opts('cmt')
        save_bd_col = no_tab and opts.get('save_body_col', False)
        blnks4cmt   = '\t'.expandtabs(len(cmt_sgn))
        (cCrt, rCrt
        ,cEnd, rEnd)= crts[0]
        (rCmtBgn
        ,rCmtEnd)   = minmax(rCrt, rEnd if -1!=rEnd else rCrt)
        uncmtAll    = ed.get_text_line(rCmtBgn).lstrip().startswith(cmt_sgn)
        pass;                   LOG and log('rCmtBgn,rCmtEnd,uncmtAll={}', (rCmtBgn,rCmtEnd,uncmtAll))
        for rCmt in range(rCmtBgn, rCmtEnd+1):
            line    = ed.get_text_line(rCmt)
            pos_body= line.index(line.lstrip())
            pass;               LOG and log('rCmtBgn,rCmtEnd,uncmtAll={}', (rCmtBgn,rCmtEnd,uncmtAll))
            if uncmtAll:
                # Uncomment!
                if not line[pos_body:].startswith(cmt_sgn):
                    # Already no comment
                    continue
                if save_bd_col:
                    line = line.replace(cmt_sgn, blnks4cmt, 1)
                else:
                    line = line.replace(cmt_sgn, ''       , 1)
            else:
                # Comment!
                if cmt_type=='bod' and line[pos_body:].startswith(cmt_sgn):
                    # Body comment already sets - willnot double it
                    continue
                if False:pass
                elif cmt_type=='1st' and save_bd_col and line.startswith(blnks4cmt) :
                    line = line.replace(blnks4cmt, cmt_sgn, 1)
#               elif cmt_type=='1st' and save_bd_col #  !line.startswith(blnks4cmt) :
                elif cmt_type=='1st':#  !save_bd_col
                    line = cmt_sgn+line
                elif cmt_type=='bod' and save_bd_col and line.startswith(blnks4cmt) :
                    line = line.replace(blnks4cmt, cmt_sgn, 1)
                    line = line[:pos_body-len(cmt_sgn)]+cmt_sgn+line[pos_body:]
#               elif cmt_type=='bod' and save_bd_col #  !line.startswith(blnks4cmt) :
                elif cmt_type=='bod':#  !save_bd_col
                    line = line[:pos_body]             +cmt_sgn+line[pos_body:]

            ed.set_text_line(rCmt, line)
            #for rCmt
        if -1==rEnd and opts.get('skip-line-after', True) and (rCrt+1)<ed.get_line_count():
            colCrt  = pos2pos(cCrt  , rCrt,   'smb2col')
            smbCrt1 = pos2pos(colCrt, rCrt+1, 'col2smb')
            ed.set_caret(smbCrt1, rCrt+1)
        #def _cmt_toggle_line

    def cmt_toggle_stream(self):
        ''' '''
        if ed.get_sel_mode() != app.SEL_NORMAL:
            return app.msg_status(ONLY_NORM_SEL_MODE.format(COMMENTING))
        lex     = ed.get_prop(app.PROP_LEXER_CARET)
        (bgn
        ,end)   = self._get_cmt_pair(lex)
        if not bgn:
            return app.msg_status(CMT_NO_STRM_4LEX.format(lex))
        crts    = ed.get_carets()
        pass;                   LOG and log('lex, get_carets()={}', (lex, crts))
        pass;                   LOG and log('bgn,end={}', (bgn,end))
        for icrt, (c1, r1, c2, r2) in enumerate(crts):
            pass;               LOG and log('(r1, c1), (r2, c2)={}', ((r1, c1), (r2, c2)))
            if -1==c2:
                # Empty sel
                continue
            (r1, c1), (r2, c2) = minmax((r1, c1), (r2, c2))
            pass;               LOG and log('(r1, c1), (r2, c2)={}', ((r1, c1), (r2, c2)))
            selTx   = ed.get_text_substr(c1, r1, c2, r2)
            cmted   = selTx.startswith(bgn) and selTx.endswith(end)
            pass;               LOG and log('cmted, selTx={}', (cmted, selTx))

            if False:pass
            elif not cmted and r1==r2:
                # Comment ON, sel into one row
                ed.insert(c2, r2, end)
                ed.insert(c1, r1, bgn)
                ed.set_caret(c1, r1, c2+len(bgn)+len(end), r2, app.CARET_SET_INDEX+icrt)
            elif not cmted and r1!=r2:
                # Comment ON, sel ends on diff rows
                ed.insert(c2, r2, end)
                ed.insert(c1, r1, bgn)
                ed.set_caret(c1, r1, c2         +len(end), r2, app.CARET_SET_INDEX+icrt)

            elif cmted and r1==r2:
                # Comment OFF, sel into one row
                ed.delete(c2-len(end), r2, c2, r2)
                ed.delete(c1, r1, c1+len(bgn), r1)
                ed.set_caret(c1, r1, c2-len(bgn)-len(end), r2, app.CARET_SET_INDEX+icrt)
            elif cmted and r1!=r2:
                # Comment OFF, sel ends on diff rows
                ed.delete(c2-len(end), r2, c2, r2)
                ed.delete(c1, r1, c1+len(bgn), r1)
                ed.set_caret(c1, r1, c2         -len(end), r2, app.CARET_SET_INDEX+icrt)
        #def cmt_toggle_stream

    def _get_cmt_pair(self, lex):
        ''' '''
        if lex not in self.pair4lex:
            # Search lex-pair
            def_lexs_json      = os.path.join(get_def_setting_dir()             , 'default_lexers.json')
            usr_lexs_json      = os.path.join(app.app_path(app.APP_DIR_SETTINGS), 'user_lexers.json')
            def_lexs           = _json_loads(open(def_lexs_json).read())
            usr_lexs           = _json_loads(open(usr_lexs_json).read()) if os.path.exists(usr_lexs_json) else {"Comments":{}}
            self.pair4lex[lex] = usr_lexs["Comments"].get(lex, def_lexs["Comments"].get(lex, ['','']))
        return self.pair4lex[lex]

    #################################################
    ## Duplicate
    def duplicate(self):
        if ed.get_sel_mode() != app.SEL_NORMAL:
            return app.msg_status(ONLY_NORM_SEL_MODE.format(DUPLICATION))

        crts    = ed.get_carets()
        if len(crts)>1:
            return app.msg_status(ONLY_SINGLE_CRT.format(DUPLICATION))

        (cCrt, rCrt, cEnd, rEnd)    = crts[0]
        if -1==cEnd:
            # Empty sel -- dup whole row
            row_txt    = ed.get_text_line(rCrt)
            ed.insert(0, rCrt, row_txt+'\n')

            # Move crt to next row
            if (rCrt+1)<ed.get_line_count():
                colCrt  = pos2pos(cCrt  , rCrt,   'smb2col')
                smbCrt1 = pos2pos(colCrt, rCrt+1, 'col2smb')
                ed.set_caret(smbCrt1, rCrt+1)
            return

        (rFr, cFr), (rTo, cTo)  = minmax((rCrt, cCrt), (rEnd, cEnd))
        #pass;                   LOG and log('(cCrt, rCrt, cEnd, rEnd)={}',(cCrt, rCrt, cEnd, rEnd))
        pass;                   LOG and log('(cFr , rFr , cTo , rTo) ={}',(cFr , rFr , cTo , rTo))
        sel_txt = ed.get_text_substr(cFr, rFr, cTo, rTo)
        pass;                   LOG and log('sel_txt={}',repr(sel_txt))
        ed.insert(cFr, rFr, sel_txt)
        if -1==rEnd: # or rCrt==rEnd:
            # Move crt to next row
            colCrt  = pos2pos(cCrt  , rCrt,   'smb2col')
            smbCrt1 = pos2pos(colCrt, rCrt+1, 'col2smb')
            ed.set_caret(smbCrt1, rCrt+1)

    def version(self):
        ''' Value from module doc
            Version:'value'
        '''
        return re.split('Version:', __doc__)[1].split("'")[1]

    #################################################
    ## Utils
    def _get_opts(self, keys=()):
        ''' Get this module options as dict-tree.
            Load from CudaText\settings\{module-name}.json if this file exists
        '''
        if not self.opts:
            ops_json    = os.path.join(app.app_path(app.APP_DIR_SETTINGS), '{}.json'.format(__name__))
            self.opts   = _json_loads(open(ops_json).read())    if os.path.exists(ops_json) else self.def_opts
        return opt(self.opts, keys)
        #def _get_opts

    def __init__(self):
        self.def_opts = {
                            'cmt':{
                                'full-line-if-no-sel':True
                            ,   'skip-line-after':True
                            }
                        }
        self.opts     = {}
        self.pair4lex = {}
        #def __init__

    #class Command:

#################################################
## Common utils
def _check_API(ver):
    if app.app_api_version()<ver:
        app.msg_status(NEED_NEWER_API)
        return False
    return True

def pos2pos(col_or_smb, row, how):
    '''
        Params
            how    'col2smb'
                   'smb2col'
    '''
    pass;                       LOG and log('col_or_smb, row, how={}', (col_or_smb, row, how))
    return (ed.convert(  app.CONVERT_CHAR_TO_COL, col_or_smb, row)[0] if how=='smb2col' else
            ed.convert(  app.CONVERT_COL_TO_CHAR, col_or_smb, row)[0]                       )
#    tab_sz  = ed.get_prop(app.PROP_TAB_SIZE)
#    line    = ed.get_text_line(row)
#    if False:pass
#    elif how=='smb2col':
#        smb     = col_or_smb
#        #pass;                   LOG and log('line[:smb].count(\t)={}', (line[:smb].count('\t')))
#        return smb + (tab_sz-1)*line[:smb].count('\t')
#    elif how=='col2smb':
#        tabs1   = ''.zfill(tab_sz).replace('0', chr(1))
#        col     = col_or_smb
#        return len(line.replace('\t', tabs1)[:col].replace(tabs1, '\t').replace(chr(1), ''))
#    #def pos2pos

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
    return json.loads(s, **kw)
    #def _json_loads

def get_def_setting_dir():
    #pass;                      LOG and log('os.path.dirname(app.app_path(app.APP_DIR_SETTINGS))={}', os.path.dirname(app.app_path(app.APP_DIR_SETTINGS)))
    return os.path.join(
                os.path.dirname(app.app_path(app.APP_DIR_SETTINGS))
            ,   'settings_default' )
    #def get_def_setting_dir

def opt(dct_tree, keys=(), def_val=None):
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
    #def opt

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
    [S] Состояние. Принимает значения
        [ ] Не реализовано
        [+] Сделано
        [-] Не требуется
        [?] Нужны уточнения
    [кто-кому] Автор-Исполнитель. Принимает значения
        [kv] kvichans
        [at] Alexey-T
    [дата] Когда придумано
---------------
[ ][kv-kv][27oct15] full-line-if-no-sel for stream-cmt
[+][kv-at][27oct15] Дать способ вертикального перемещения каретки вниз
[ ][kv-kv][27oct15] При Cmt1st не вставлять // перед пробелами, а заменять
[ ][kv-kv][27oct15] ! Разрешить Комм и Дубл для режима "много кареток"
[ ][kv-at][27oct15] Дать механизм для Localization
[ ][kv-at][27oct15] Дать доступ из плагинов к командам из cudax_lib
[ ][kv-at][27oct15] Спрятать в меню Plugins команды из cudax_lib
[ ][kv-kv][28oct15] Применять cmt_toggle_line_(1st|body) к каждой строке в выделении, решение по первой строке
[?][kv-at][28oct15] Включить настройки cudax_lib в общие default.json и user.json
[ ][kv-kv][28oct15] Контролировать, что выделенный фрагмент имеет один Лексер
'''