''' Plugin for CudaText editor
Authors:
    Andrey Kvichansky    (kvichans on github.com)
    Alexey Torgashin (CudaText)
Version:
    '0.8.8 2018-03-05'
ToDo: (see end of file)
'''

import  os
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

    def dlg_config(self):
        save_bd_col = apx.get_opt('comment_save_column'         , False)
        at_min_bd   = apx.get_opt('comment_equal_column'        , False)
        bUseFLn     = True
        bSkip       = apx.get_opt('comment_move_down'           , True)

        save_s      = _('(Line commands) Try to keep text position after (un)commenting')
        save_h      = _('Try to replace only blank(s) to keep text positions:'
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
        down_s      = _('(All) Move caret to next line')
        aid,vals,chds   = dlg_wrapper(_('Config commenting commands'), 610, 110,     #NOTE: dlg-cmnt
             [dict(cid='save',tp='ch'   ,t=5    ,l=5    ,w=600      ,cap=save_s ,hint=save_h) #
             ,dict(cid='vert',tp='ch'   ,t=5+25 ,l=5    ,w=600      ,cap=vert_s ,hint=vert_h) #
             ,dict(cid='down',tp='ch'   ,t=5+50 ,l=5    ,w=600      ,cap=down_s             ) #
             ,dict(cid='!'   ,tp='bt'   ,t=80   ,l=610-165-5,w=80   ,cap=_('OK'),ex0='1'                                                          ) #     default
             ,dict(cid='-'   ,tp='bt'   ,t=80   ,l=610 -80-5,w=80   ,cap=_('Cancel')                                                                )
             ], dict(save=save_bd_col
                    ,vert=at_min_bd
                    ,down=bSkip
             ), focus_cid='save')
        if aid is None or aid=='-': return
        if vals['save'] != save_bd_col: apx.set_opt('comment_save_column'       , vals['save'])
        if vals['vert'] != at_min_bd:   apx.set_opt('comment_equal_column'      , vals['vert'])
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
        prop        = app.lexer_proc(app.LEXER_GET_PROP, lex)
        cmt_sgn     = prop['c_line'] if prop else None
        pass;                  #log('cmt_type, lex, cmt_sgn={}', (cmt_type, lex, cmt_sgn))
        if not cmt_sgn:
            return app.msg_status(f(_('No line comment for lexer "{}"'), lex))
        # Analize
        bEmpSel     = False
        rWrks       = []
        bUseRepLns  = app.app_api_version()>='1.0.177'
        y1,y2,lines = (-1, -1, []) if bUseRepLns else (None, None, None) # To use API replace_lines
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
            bUseRepLns  = bUseRepLns and 1==len(crts)
        elif ed_.get_sel_mode() == app.SEL_COLUMN:
            (cBgn
            ,rSelBgn
            ,cEnd
            ,rSelEnd)   = ed_.get_sel_rect()
            rWrks       = list(range(rSelBgn, rSelEnd+1))
        if not rWrks:
            rWrks       = [crts[0][1]]
        pass;                  #log('rWrks={}', (rWrks))
        y1,y2       = (rWrks[0],rWrks[-1]) if bUseRepLns else (y1,y2)
        pass;                  #LOG and log('y1,y2,lines={}', (y1,y2,lines))
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
        pass;                  #log('rWrks,do_uncmt, save_cols, at_min_bd, col_min_bd={}', (rWrks,do_uncmt,save_bd_col,at_min_bd,col_min_bd))
        for rWrk in rWrks:
            line    = ed_.get_text_line(rWrk)
            pos_body= line.index(line.lstrip())
            pos_body= len(line) if 0==len(line.lstrip()) else pos_body
            pass;              #LOG and log('rWrk,pos_body,line={}', (rWrk,pos_body,line))
            if do_uncmt:
                # Uncomment!
                if not line[pos_body:].startswith(cmt_sgn):
                    # Already no comment
                    if bUseRepLns:
                        lines += [line]
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
                    if bUseRepLns:
                        lines += [line]
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
            if bUseRepLns:
                lines += [line]
            else:
                pass;           log('line={}',(line))
                ed_.set_text_line(rWrk, line)
            #for rWrk
        if bUseRepLns:
            pass;              #log('y1, y2, len(lines), lines={}',(y1, y2, len(lines), lines))
            if y1==y2:
                ed_.set_text_line(y1, lines[0])
            else:
                ed_.replace_lines(y1, y2, lines)
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
        bUseFLn = True
        crts    = ed.get_carets()
        pass;                  #LOG and log('lex, get_carets()={}', (lex, crts))
        pass;                  #LOG and log('(bgn_sgn,end_sgn),bOnlyLn,bUseFLn={}', ((bgn_sgn,end_sgn),bOnlyLn,bUseFLn))
        for icrt, (cCrt, rCrt, cEnd, rEnd) in enumerate(crts):
            pass;              #LOG and log('(cCrt, rCrt), (cEnd, rEnd)={}', ((cCrt, rCrt), (cEnd, rEnd)))
            bEmpSel     = -1==rEnd
            bDrtSel     = -1==rEnd or (rCrt, cCrt)>(rEnd, cEnd)
            bEntireLn   = (rEnd>=0) and (cEnd==0) and (cCrt==0)
            bEntireLn1  = bEntireLn and abs(rEnd-rCrt)==1
            bEntireLn2  = bEntireLn and abs(rEnd-rCrt)>1
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
            do_uncmt= selTx.startswith(bgn_sgn) #and selTx.endswith(end_sgn)
                # don't check for ending of selection - for HTML and entire selected line(s)
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
                if bEntireLn1:
                    s = ed.get_text_line(rTx1)
                    ed.set_text_line(rTx1, bgn_sgn+s+end_sgn)
                    (cNSel1, rNSel1
                    ,cNSel2, rNSel2) = (0, rTx1, 0, rTx2)

                elif bEntireLn2:
                    ed.insert(0, rTx2, end_sgn+'\n')
                    ed.insert(0, rTx1, bgn_sgn+'\n')
                    (cNSel1, rNSel1
                    ,cNSel2, rNSel2) = (0, rTx1, 0, rTx2+2)

                else:
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
                if selTx.endswith(end_sgn):
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

                elif bEntireLn1:
                    s = ed.get_text_line(rTx1)
                    if s.startswith(bgn_sgn):
                        s = s[len(bgn_sgn):]
                    if s.endswith(end_sgn):
                        s = s[:-len(end_sgn)]
                    ed.set_text_line(rTx1, s)
                    (cNSel1, rNSel1
                    ,cNSel2, rNSel2) = (0, rTx1, 0, rTx2)

                elif bEntireLn2:
                    ed.delete(0, rTx2-1, 0, rTx2)
                    ed.delete(0, rTx1, 0, rTx1+1)
                    (cNSel1, rNSel1
                    ,cNSel2, rNSel2) = (0, rTx1, 0, rTx2-2)

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
            only_ln = False
            prop = app.lexer_proc(app.LEXER_GET_PROP, lex)
            pair1 = prop['c_str'] if prop else None
            pair2 = prop['c_lined'] if prop else None
            if pair1 is not None:
                pair = pair1
            elif pair2 is not None:
                pair = pair2
                only_ln = True
            else:
                pair = ('', '')
            self.pair4lex[lex] = (pair, only_ln)
        return self.pair4lex[lex]
       #def _get_cmt_pair


'''
ToDo
[ ][kv-kv][13sep16] Start
'''
