''' Plugin for CudaText editor
Authors:
    Andrey Kvichansky    (kvichans on github.com)
Version:
    '0.8.1 2016-09-13'
ToDo: (see end of file)
'''

import  re, os, json
import  cudatext            as app
from    cudatext        import ed
import  cudatext_cmd        as cmds
import  cudax_lib           as apx
from    .cd_plug_lib    import *

# I18N
_       = get_translation(__file__)

pass;                           LOG     = (-1==-1)  # Do or dont logging.

class Command:
    def __init__(self):
        self.pair4lex = {}
        #def __init__
    
    def edit_strcomment_chars(self):
        lex     = ed.get_prop(app.PROP_LEXER_CARET)
        if not lex: return app.msg_status(_("No lexer"))
        def_lexs_json   = os.path.join(apx.get_def_setting_dir()         , 'default_lexers.json')
        usr_lexs_json   = os.path.join(app.app_path(app.APP_DIR_SETTINGS), 'user_lexers.json')
        def_lexs        = apx._json_loads(open(def_lexs_json).read())
        usr_lexs        = apx._json_loads(open(usr_lexs_json).read()) if os.path.exists(usr_lexs_json) else {"Comments":{}, "CommentsForLines":{}}
        pass;                  #LOG and log('usr_lexs={}',usr_lexs)
        only_ln         = False
        pair_df         = ['','']
        pair            = ['','']
        if False:pass
        elif lex in   def_lexs["Comments"]:
            pair_df = def_lexs["Comments"].get(lex)
        elif lex in   def_lexs["CommentsForLines"]:
            pair_df = def_lexs["CommentsForLines"].get(lex)
            only_ln = True
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
        vals        = dict(stcs=pair[   0]
                          ,stdf=pair_df[0]
                          ,encs=pair[   1]
                          ,endf=pair_df[1]
                          ,full=only_ln   )
        while True:
            btn,vals,chds   = dlg_wrapper(f(_('Stream comment chars for lexer "{}"'), lex), GAP*3+100+165*2, GAP+105+GAP,     #NOTE: dlg-str-cmnt
                 [dict(           tp='lb'   ,t=GAP          ,l=GAP+100+GAP+165  ,w=165  ,cap=_('Default values')    ) #
                 ,dict(           tp='lb'   ,tid='stcs'     ,l=GAP              ,w=100  ,cap=_('&Start chars')      ) # &s
                 ,dict(cid='stcs',tp='ed'   ,t=GAP+20       ,l=GAP+100          ,w=165                              ) # 
                 ,dict(cid='stdf',tp='ed'   ,tid='stcs'     ,l=GAP+100+GAP+165  ,w=165  ,props='1,0,1'              ) #     ro,mono,border
                 ,dict(           tp='lb'   ,tid='encs'     ,l=GAP              ,w=100  ,cap=_('&Finish chars')     ) # &f
                 ,dict(cid='encs',tp='ed'   ,t=GAP+50       ,l=GAP+100          ,w=165                              ) # 
                 ,dict(cid='endf',tp='ed'   ,tid='encs'     ,l=GAP+100+GAP+165  ,w=165  ,props='1,0,1'              ) #     ro,mono,border
                 ,dict(cid='full',tp='ch'   ,t=GAP+80       ,l=GAP+100          ,w=165  ,cap=_('Only f&ull lines')  ) # &u
                 ,dict(cid='!'   ,tp='bt'   ,tid='full'     ,l=GAP+GAP+430-165  ,w=80   ,cap=_('OK'),props='1'      ) #     default
                 ,dict(cid='-'   ,tp='bt'   ,tid='full'     ,l=GAP+GAP+430-80   ,w=80   ,cap=_('Cancel')            )
                 ], vals, focus_cid='stcs')
            pass;              #LOG and log('vals={}',(vals))
            if btn is None or btn=='-': return None
            pair        = [vals['stcs'], vals['encs']]
            only_ln     = vals['full']
            # Checking
            if not pair[0] or not pair[1]:
                app.msg_box(USE_NOT_EMPTY, app.MB_OK)
                continue #while
            break #while 
           #while
           
        #Saving
        usr_lexs["Comments"         if only_ln else "CommentsForLines"].pop(lex, None)
        usr_lexs["CommentsForLines" if only_ln else "Comments"        ][lex] = pair
        open(usr_lexs_json, 'w').write(json.dumps(usr_lexs, indent=2))
        app.msg_status(f(_('File "{}" is updated'), usr_lexs_json))
       #def edit_strcomment_chars
    
    def dlg_config(self):
        save_bd_col = apx.get_opt('comment_save_column'         , False)
        at_min_bd   = apx.get_opt('comment_equal_column'        , False)
        bUseFLn     = apx.get_opt('comment_full_line_if_no_sel' , True)
        bSkip       = apx.get_opt('comment_move_down'           , True)
        
        save_s      = _('(Line commands) Try to save char positions in line after (un)commenting')
        save_h      = _('Try to replace blank(s) to save text positions:'
                        '\rUncommented lines:'
                        '\r····foo1'
                        '\r····foo2'
                        '\rCommented lines:'
                        '\r#···foo1'
                        '\r···#foo2'
                        )
        vert_s      = _('(Line "at non-space") If selected few lines, insert comment at maximal common indent')         
        vert_h      = _('Use max same column to comment:'
                        '\rUncommented lines:'
                        '\r··foo1'
                        '\r····foo2'
                        '\r······foo3'
                        '\rCommented lines:'
                        '\r·#foo1'
                        '\r·#··foo2'
                        '\r·#····foo3'
                        )         
        full_s      = _('(Stream) Comment full line if no selection')                            
        down_s      = _('(All) Move caret to next line')                                         
        aid,vals,chds   = dlg_wrapper(_('Config commenting commands'), 610, 135,     #NOTE: dlg-cmnt
             [dict(cid='save',tp='ch'   ,t=5    ,l=5    ,w=600      ,cap=save_s ,hint=save_h) # 
             ,dict(cid='vert',tp='ch'   ,t=5+25 ,l=5    ,w=600      ,cap=vert_s ,hint=vert_h) # 
             ,dict(cid='full',tp='ch'   ,t=5+50 ,l=5    ,w=600      ,cap=full_s             ) # 
             ,dict(cid='down',tp='ch'   ,t=5+75 ,l=5    ,w=600      ,cap=down_s             ) # 
             ,dict(cid='!'   ,tp='bt'   ,t=105  ,l=610-165-5,w=80   ,cap=_('OK'),props='1'                                                          ) #     default
             ,dict(cid='-'   ,tp='bt'   ,t=105  ,l=610 -80-5,w=80   ,cap=_('Cancel')                                                                )
             ], dict(save=save_bd_col
                    ,vert=at_min_bd
                    ,full=bUseFLn
                    ,down=bSkip
             ), focus_cid='save')
        if aid is None or aid=='-': return
        if vals['save'] != save_bd_col: apx.set_opt('comment_save_column'       , vals['save'])
        if vals['vert'] != at_min_bd:   apx.set_opt('comment_equal_column'      , vals['vert'])
        if vals['full'] != bUseFLn:     apx.set_opt('comment_full_line_if_no_sel',vals['full'])
        if vals['down'] != bSkip:       apx.set_opt('comment_move_down'         , vals['down'])
       #def dlg_config
    
    def cmt_toggle_line_1st(self):
        return self._cmt_toggle_line('bgn', '1st')
    def cmt_add_line_1st(self):
        return self._cmt_toggle_line('add', '1st')
    def cmt_toggle_line_body(self):
        return self._cmt_toggle_line('bgn', 'bod')
    def cmt_add_line_body(self):
        return self._cmt_toggle_line('add', 'bod')
    def cmt_del_line(self):
        return self._cmt_toggle_line('del')
    def _cmt_toggle_line(self, cmt_act, cmt_type='', ed_=ed):
        ''' Add/Remove full line comment
            Params
                cmt_act     'del'   uncomment all lines
                            'add'   comment all lines
                            'bgn'   (un)comment all as toggled first line 
                cmt_type    '1st'   at begin of line
                            'bod'   at first not blank
        '''
#       if not apx._check_API('1.0.108'):    return
        lex         = ed_.get_prop(app.PROP_LEXER_CARET)
        cmt_sgn     = app.lexer_proc(app.LEXER_GET_COMMENT, lex)
        pass;                  #LOG and log('cmt_type, lex, cmt_sgn={}', (cmt_type, lex, cmt_sgn))
        if not cmt_sgn:
            return app.msg_status(f(_('No line comment for lexer "{}"'), lex))
        # Analize
        bEmpSel     = False
        rWrks       = []
        pass;                  #LOG and log('ed_.get_sel_mode(),app.SEL_NORMAL,app.SEL_COLUMN={}', (ed_.get_sel_mode(),app.SEL_NORMAL,app.SEL_COLUMN))
        crts        = ed_.get_carets()
        if False:pass
        elif ed_.get_sel_mode() == app.SEL_NORMAL:
            bEmpSel     = 1==len(crts) and -1==crts[0][3]
            for (cCrt, rCrt ,cEnd, rEnd) in crts:
                (rCrtMin
                ,rCrtMax)   = apx.minmax(rCrt, rEnd if -1!=rEnd else rCrt)
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
        do_uncmt    = ed_.get_text_line(rWrks[0]).lstrip().startswith(cmt_sgn) \
                        if cmt_act=='bgn' else \
                      True \
                        if cmt_act=='del' else \
                      False
        # Work
        save_bd_col = apx.get_opt('comment_save_column' , False)
        at_min_bd   = apx.get_opt('comment_equal_column', False)
        col_min_bd  = 1000 # infinity
        if at_min_bd:
            for rWrk in rWrks:
                line        = ed_.get_text_line(rWrk)
                pos_body    = line.index(line.lstrip())
                pos_body    = len(line) if 0==len(line.lstrip()) else pos_body
                col_min_bd  = min(pos_body, col_min_bd)
                if 0==col_min_bd:
                    break # for rWrk
        blnks4cmt   = ' '*len(cmt_sgn) # '\t'.expandtabs(len(cmt_sgn))
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
        bSkip    = apx.get_opt('comment_move_down', True)
        if bEmpSel and bSkip:
            (cCrt, rCrt, cEnd, rEnd)    = crts[0]
            apx._move_caret_down(cCrt, rCrt)
       #def _cmt_toggle_line

    def cmt_toggle_stream(self):
        ''' '''
        if ed.get_sel_mode() != app.SEL_NORMAL:
            return app.msg_status(f(_('{} works only with normal selection'), _('Commenting')))
        lex     = ed.get_prop(app.PROP_LEXER_CARET)
        ((bgn_sgn
        ,end_sgn)
        ,bOnlyLn)=self._get_cmt_pair(lex)
        if not bgn_sgn:
            return app.msg_status(f(_('No stream comment for lexer "{}"'), lex))
        bUseFLn = apx.get_opt('comment_full_line_if_no_sel', True)
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
                rTx1, rTx2  = apx.minmax(rCrt, rEnd)
                line    = ed.get_text_line(rTx2)
                (cTx1, rTx1), (cTx2, rTx2) = (0, rTx1), (len(line), rTx2)
            elif bEmpSel: # and not bUseFLn and not bOnlyLn
                continue
            else:
                (rTx1, cTx1), (rTx2, cTx2) = apx.minmax((rCrt, cCrt), (rEnd, cEnd))
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
        bSkip    = apx.get_opt('comment_move_down', True)
        if False:pass
        elif 1==len(crts) and bEmpSel and bUseFLn and bSkip:
            apx._move_caret_down(cCrt, rCrt)
            if bOnlyLn and not do_uncmt:
                crt=ed.get_carets()[0]; apx._move_caret_down(crt[0], crt[1])
                crt=ed.get_carets()[0]; apx._move_caret_down(crt[0], crt[1])
        #def cmt_toggle_stream

    def _get_cmt_pair(self, lex):
        ''' Return ((begin_sign, end_sign), only_lines)
                begin_sign    as '/*'
                end_sign      as '*/'
                only_lines    True if each of *_sign must be whole line
        '''
        if lex not in self.pair4lex:
            # Search lex-pair
            def_lexs_json   = os.path.join(apx.get_def_setting_dir()         , 'default_lexers.json')
            usr_lexs_json   = os.path.join(app.app_path(app.APP_DIR_SETTINGS), 'user_lexers.json')
            def_lexs        = apx._json_loads(open(def_lexs_json, encoding='utf8').read())
            usr_lexs        = apx._json_loads(open(usr_lexs_json, encoding='utf8').read()) if os.path.exists(usr_lexs_json) else {"Comments":{}, "CommentsForLines":{}}
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

    
'''
ToDo
[ ][kv-kv][13sep16] Start
'''
