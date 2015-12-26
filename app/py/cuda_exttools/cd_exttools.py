''' Plugin for CudaText editor
Authors:
    Andrey Kvichansky    (kvichans on github.com)
Version:
    '0.9.8 2015-12-25'
ToDo: (see end of file)
'''

import  os, json, random, subprocess, shlex, copy, collections
import  cudatext        as app
from    cudatext    import ed
import  cudatext_cmd    as cmds
import  cudax_lib       as apx
from    cudax_lib   import log
from    .encodings  import *

pass;                           # Logging
pass;                           LOG = (-2==-2)  # Do or dont logging.

JSON_FORMAT_VER = '20151209'
EXTS_JSON       = app.app_path(app.APP_DIR_SETTINGS)+os.sep+'exttools.json'

RSLT_NO         = 'Ignore'
RSLT_TO_PANEL   = 'Output panel'
RSLT_TO_PANEL_AP= 'Output panel (append)'
RSLT_TO_NEWDOC  = 'Copy to new document'
RSLT_TO_CLIP    = 'Copy to clipboard'
RSLT_REPL_SEL   = 'Replace selection'
RSLT_N          = 'N'
RSLT_OP         = 'OP'
RSLT_OPA        = 'OPA'
RSLT_ND         = 'ND'
RSLT_CB         = 'CB'
RSLT_SEL        = 'SEL'

SAVS_NOTHING    = 'Nothing'
SAVS_ONLY_CUR   = 'Current document'
SAVS_ALL_DOCS   = 'All documents'
SAVS_N          = 'N'
SAVS_Y          = 'Y'
SAVS_A          = 'A'

FROM_API_VERSION= '1.0.115'
C1      = chr(1)
C2      = chr(2)
POS_FMT = 'pos={l},{t},{r},{b}'.format
GAP     = 5

class Command:
    exts        = []    # Main list [exttools]
    ext4id      = {}    # Derived dict {id:exttool}
    
#   id_menu     = 0
    
    def _fill_ext(self, ext):
        ext.pop('capt', None)
        if not ext['nm']:
            ext['nm']='tool'+str(random.randint(100, 999))
        ext['ddir'] = ext.get('ddir', '')
        ext['shll'] = ext.get('shll', 'N')=='Y' if str(ext.get('shll', 'N'))    in 'NY'            else ext.get('shll', False)
        ext['prms'] = ext.get('prms', '')
        ext['savs'] = ext.get('savs', SAVS_N)
        ext['rslt'] = ext.get('rslt', RSLT_N)   if     ext.get('rslt', RSLT_N)  in self.rslt_vals  else RSLT_N
        ext['encd'] = ext.get('encd', '')
        ext['lxrs'] = ext.get('lxrs', '')
        return ext

    def __init__(self):
        self.savs_caps  = [SAVS_NOTHING, SAVS_ONLY_CUR, SAVS_ALL_DOCS]
        self.savs_vals  = [SAVS_N,       SAVS_Y,        SAVS_A]
        self.savs_v2c   = {SAVS_N:SAVS_NOTHING
                          ,SAVS_Y:SAVS_ONLY_CUR
                          ,SAVS_A:SAVS_ALL_DOCS}
        self.rslt_caps  = [RSLT_NO, RSLT_TO_PANEL, RSLT_TO_PANEL_AP, RSLT_TO_NEWDOC, RSLT_TO_CLIP, RSLT_REPL_SEL]
        self.rslt_vals  = [RSLT_N,  RSLT_OP,       RSLT_OPA,         RSLT_ND,        RSLT_CB,      RSLT_SEL]
        self.rslt_v2c   = {RSLT_N:RSLT_NO
                          ,RSLT_OP:RSLT_TO_PANEL
                          ,RSLT_OPA:RSLT_TO_PANEL_AP
                          ,RSLT_ND:RSLT_TO_NEWDOC
                          ,RSLT_CB:RSLT_TO_CLIP
                          ,RSLT_SEL:RSLT_REPL_SEL}
        
        ver_exts            = apx._json_loads(open(EXTS_JSON).read()) if os.path.exists(EXTS_JSON) else {'ver':JSON_FORMAT_VER, 'list':[]}
        if ver_exts['ver'] < JSON_FORMAT_VER:
            # Adapt to new format
            pass
        self.dlg_prs        = ver_exts.get('dlg_prs', {})
        self.ext4lxr        = ver_exts.get('ext4lxr', {})
        self.exts           = ver_exts['list']
        for ext in self.exts:
            self._fill_ext(ext)
        self.ext4id         = {str(ext['id']):ext for ext in self.exts}
        
        self.last_ext_id    = 0
       #def __init__
       
    def on_start(self, ed_self):
        pass
        self._do_acts(acts='|reg|menu|')
       #def on_start
        
    def _adapt_menu(self):
        ''' Add or change top-level menu ExtTools
        '''
        id_menu     = 0
        if 'exttools_id_menu' in dir(ed):               ##?? dirty hack!
            id_menu = ed.exttools_id_menu               ##?? dirty hack!
            # Clear old
            app.app_proc(app.PROC_MENU_CLEAR, id_menu)
        else:
#       if 0==self.id_menu:
            # Create AFTER Plugins
            top_nms = app.app_proc(app.PROC_MENU_ENUM, 'top').splitlines()
            pass;              #LOG and log('top_nms={}',top_nms)
            plg_ind = top_nms.index('&Plugins|')        ##?? 
            id_menu = app.app_proc( app.PROC_MENU_ADD, '{};{};{};{}'.format('top', 0, '&Tools', 1+plg_ind))
            ed.exttools_id_menu = id_menu               ##?? dirty hack!

        # Fill
        app.app_proc(app.PROC_MENU_ADD, '{};cuda_exttools,dlg_config;{}'.format(  id_menu, 'Con&fig...'))
        app.app_proc(app.PROC_MENU_ADD, '{};cuda_exttools,run_lxr_main;{}'.format(id_menu, 'R&un main lexer tool'))
        if 0==len(self.exts):
            return
        app.app_proc(app.PROC_MENU_ADD, '{};;-'.format(id_menu))
        for ext in self.exts:
            app.app_proc(app.PROC_MENU_ADD, '{};cuda_exttools,run,{};{}'.format(id_menu, ext['id'], ext['nm']))
       #def _adapt_menu
        
    def _do_acts(self, what='', acts='|save|second|reg|keys|menu|'):
        ''' Use exts list '''
        pass;                  #LOG and log('what, acts={}',(what, acts))
        # Save
        if '|save|' in acts:
            open(EXTS_JSON, 'w').write(json.dumps(
                {'ver':JSON_FORMAT_VER
                ,'dlg_prs':self.dlg_prs
                ,'ext4lxr':self.ext4lxr
                ,'list':self.exts
                }, indent=4))
        
        # Secondary data
        if '|second|' in acts:
            self.ext4id     = {str(ext['id']):ext for ext in self.exts}
        
        # Register new subcommands
        if '|reg|' in acts:
            reg_subs        = 'cuda_exttools;run;{}'.format('\n'.join(
                             'exttool: {}\t{}'.format(ext['nm'],ext['id']) 
                                 for ext in self.exts)
                             )
            pass;              #LOG and log('reg_subs={}',reg_subs)
            app.app_proc(app.PROC_SET_SUBCOMMANDS, reg_subs)
        
        # Clear keys.json
        if '|keys|' in acts and ':' in what:
            # Need delete a key 'cuda_exttools,run,NNNNN'
            ext_id      = what[1+what.index(':'):]
            ext_key     = 'cuda_exttools,run,{}'.format(ext_id)
            keys_json   = app.app_path(app.APP_DIR_SETTINGS)+os.sep+'keys.json'
            if not os.path.exists(keys_json): return
            keys        = apx._json_loads(open(keys_json).read())
            pass;              #LOG and log('??? key={}',ext_key)
            if keys.pop(ext_key, None) is not None:
                pass;           LOG and log('UPD keys.json, deleted key={}',ext_key)
                open(keys_json, 'w').write(json.dumps(keys, indent=2))
        
        # [Re]Build menu
        if '|menu|' in acts:
            self._adapt_menu()
       #def _do_acts

    def run_lxr_main(self):
        lxr     = ed.get_prop(app.PROP_LEXER_FILE)
        if lxr not in self.ext4lxr:
            return app.msg_status('No main tool for lexer "{}"'.format(lxr))
        self.run(self.ext4lxr[lxr])
       #def run_lxr_main

    def run(self, ext_id):
        ''' Main (and single) way to run any exttool
        '''
        self.last_ext_id = ext_id
        ext_id  = str(ext_id)
        pass;                  #LOG and log('ext_id={}',ext_id)
        ext     = self.ext4id.get(str(ext_id))
        if ext is None:
            return app.msg_status('No Tool: {}'.format(ext_id))
        nm      = ext['nm']
        lxrs    = ext['lxrs']
        lxr_cur = ed.get_prop(app.PROP_LEXER_FILE)
        lxr_cur = lxr_cur if lxr_cur else '(none)' 
        pass;                   LOG and log('nm="{}", lxr_cur="{}", lxrs="{}"',nm, lxr_cur, lxrs)
        if (lxrs
        and not (','+lxr_cur+',' in ','+lxrs+',')):
            return app.msg_status('Tool "{}" is not suitable for lexer "{}". It works only with "{}"'.format(nm, lxr_cur, lxrs))
        cmnd    = ext['file']
        prms_s  = ext['prms']
        ddir    = ext['ddir']
        pass;                   LOG and log('nm="{}", cmnd="{}", ddir="{}", prms_s="{}"',nm, ddir, cmnd, prms_s)
        
        # Saving
        if SAVS_Y==ext.get('savs', SAVS_N):
            if not app.file_save():  return
        if SAVS_A==ext.get('savs', SAVS_N):
            ed.cmd(cmds.cmd_FileSaveAll)
        
        # Preparing
        file_nm = ed.get_filename()
        (cCrt, rCrt
        ,cEnd, rEnd)    = ed.get_carets()[0]
        prms_l  = shlex.split(prms_s)
        for ind, prm in enumerate(prms_l):
            prm_raw = prm
            prm     = subst_props(prm, file_nm, cCrt, rCrt)
            if prm_raw != prm:
                prms_l[ind] = prm
#               prms_l[ind] = shlex.quote(prm)
           #for ind, prm
        ddir        = subst_props(ddir, file_nm, cCrt, rCrt)

        pass;                   LOG and log('ready prms_l={}',(prms_l))

        val4call  = [cmnd] + prms_l
        pass;                   LOG and log('val4call={}',(val4call))

        # Calling
        rslt    = ext.get('rslt', RSLT_N)
        nmargs  = {'cwd':ddir} if ddir else {}
        if RSLT_N==rslt:
            # Without capture
            subprocess.Popen(val4call, **nmargs)
            return
        
        # With capture
        pass;                  #LOG and log("'Y'==ext.get('shll', 'N')",'Y'==ext.get('shll', 'N'))
        nmargs['stdout']=subprocess.PIPE
        nmargs['stderr']=subprocess.STDOUT
        nmargs['shell'] =ext.get('shll', False)
        pass;                   LOG and log('?? Popen nmargs={}',nmargs)
        pipe    = subprocess.Popen(val4call, **nmargs)
        if pipe is None:
            pass;               LOG and log('fail Popen',)
            app.msg_status('Fail call: {} {}'.format(cmnd, prms_s))
            return
        pass;                   LOG and log('ok Popen',)
        app.msg_status('Call: {} {}'.format(cmnd, prms_s))

        rslt_txt= ''
        if False:pass
        elif rslt in (RSLT_OP, RSLT_OPA):
            ed.cmd(cmds.cmd_ShowPanelOutput)
            ed.focus()
            app.app_log(app.LOG_SET_PANEL, app.LOG_PANEL_OUTPUT)
            if rslt==RSLT_OP:
                app.app_log(app.LOG_CLEAR, '')
        elif rslt ==  'ND':
            app.file_open('')
            
        while True:
            out_ln = pipe.stdout.readline().decode(ext.get('encd', 'utf-8'))
            if 0==len(out_ln): break
            out_ln = out_ln.strip('\r\n')
            pass;              #LOG and log('out_ln={}',out_ln)
            if False:pass
            elif rslt in (RSLT_OP, RSLT_OPA):
                app.app_log(app.LOG_ADD, out_ln)
            elif rslt ==  RSLT_ND:
                ed.set_text_line(-1, out_ln)
            elif rslt in (RSLT_CB, RSLT_SEL):
                rslt_txt+= out_ln + '\n'
           #while True

        rslt_txt= rslt_txt.strip('\n')
        if False:pass
        elif rslt == RSLT_CB:
            app.app_proc(app.PROC_SET_CLIP, rslt_txt)
        elif rslt == RSLT_SEL:
            crts    = ed.get_carets()
            for (cCrt, rCrt, cEnd, rEnd) in crts.reverse():
                if -1!=cEnd:
                    (rCrt, cCrt), (rEnd, cEnd) = apx.minmax((rCrt, cCrt), (rEnd, cEnd))
                    ed.delete(cCrt, rCrt, cEnd, rEnd)
                ed.insert(cCrt, rCrt, rslt_txt)
       #def run
       
    def dlg_config(self):
        return self.dlg_config_list()
    def dlg_config_list(self):
        if app.app_api_version()<FROM_API_VERSION:  return app.msg_status('Need update CudaText')

        keys_json   = app.app_path(app.APP_DIR_SETTINGS)+os.sep+'keys.json'
        keys        = apx._json_loads(open(keys_json).read()) if os.path.exists(keys_json) else {}
        
        ids     = [ext['id'] for ext in self.exts]
        ext_ind = ids.index(self.last_ext_id) if self.last_ext_id in ids else min(0, len(ids)-1)

        GAP2    = GAP*2    
        prs     = self.dlg_prs
        pass;                  #LOG and log('prs={}',prs)
        while True:
            ext_nz_d        = collections.OrderedDict([
                           ('Name'          ,prs.get('nm'  , '150'))
                          ,('Keys'          ,prs.get('keys', '100'))
                          ,('File|>cmd'     ,prs.get('file', '180'))
                          ,('Params'        ,prs.get('prms', '100'))
                          ,('Folder'        ,prs.get('ddir', '100'))
                          ,('Lexers'        ,prs.get('lxrs', 'C50'))
                          ,('Capture'       ,prs.get('rslt', 'C50'))
                          ,('Saving'        ,prs.get('savs', 'C30'))
                          ])
            ACTS_W          = prs.get('w_btn', 90)
            WD_LST, HT_LST  = (sum([int(w.lstrip('LRC')) for w in ext_nz_d.values() if w[0]!='-'])+len(ext_nz_d)
                              ,prs.get('h_list', 300))
            ACTS_T          = [GAP+HT_LST   + GAP*ind+23*(ind-1)     for ind in range(20)]
            ACTS_L          = [GAP+         + GAP*ind+ACTS_W*(ind-1) for ind in range(20)]
            WD_LST_MIN      = GAP*10+ACTS_W*8
            if WD_LST < WD_LST_MIN:
                ext_nz_d['Name']    = str(WD_LST_MIN-WD_LST + int(ext_nz_d['Name']))
                WD_LST              = WD_LST_MIN
            DLG_W, DLG_H    = max(WD_LST, ACTS_L[9])+GAP*4, ACTS_T[3]+GAP
            ext_hnzs    = ['{}={}'.format(nm, '0' if sz[0]=='-' else sz) for (nm,sz) in ext_nz_d.items()]
            ext_vlss    = []
            for ext in self.exts:
                ext_vlss+=[[                                    ext['nm']
                          ,get_keys_desc('cuda_exttools,run',   ext['id'], keys)
                          ,('>' if                              ext['shll'] else '')
                          +                                     ext['file']
                          ,                                     ext['prms']
                          ,                                     ext['ddir']
                          ,                                     ext['lxrs']
                          ,                                     ext['rslt']
                          ,                                     ext['savs']
                          ]]
            pass;              #LOG and log('ext_hnzs={}',ext_hnzs)
            pass;              #LOG and log('ext_vlss={}',ext_vlss)

            ext     = self.exts[ext_ind] if ext_ind in range(len(self.exts)) else None
            pass;              #LOG and log('ext_ind, ext={}',(ext_ind, ext))
            
            ans = app.dlg_custom('Tools'   ,DLG_W, DLG_H, '\n'.join([]
            #LIST
            +[C1.join(['type=label'     ,POS_FMT(l=GAP2,        t=GAP,      r=GAP2+WD_LST+GAP, b=ACTS_T[3])
                      ,'cap=&Tools'
                      ])] # i= 0
            +[C1.join(['type=listview'  ,POS_FMT(l=GAP2,        t=GAP+20,   r=GAP+4+WD_LST,   b=GAP+HT_LST)
                      ,'items=' +'\t'.join(
                            ['\r'.join(ext_hnzs)]
                           +[' \r'.join(ext_vls) for ext_vls in ext_vlss]
                            )
                      ,'val='   +str(ext_ind)  # start sel
                      ])] # i= 1
            # TOOLS ACTS
            +[C1.join(['type=button'    ,POS_FMT(l=ACTS_L[1],    t=ACTS_T[1],   r=ACTS_L[1]+ACTS_W,b=0)
                      ,'cap=&Edit...'
                      ,'props=1'    # default
                      ])] # i= 2
            +[C1.join(['type=button'    ,POS_FMT(l=ACTS_L[2],    t=ACTS_T[1],   r=ACTS_L[2]+ACTS_W,b=0)
                      ,'cap=&Add...'
                      ])] # i= 3
            +[C1.join(['type=button'    ,POS_FMT(l=ACTS_L[1],    t=ACTS_T[2],   r=ACTS_L[1]+ACTS_W,b=0)
                      ,'cap=Clo&ne'
                      ])] # i= 4
            +[C1.join(['type=button'    ,POS_FMT(l=ACTS_L[2],    t=ACTS_T[2],   r=ACTS_L[2]+ACTS_W,b=0)
                      ,'cap=&Delete...'
                      ])] # i= 5
            +[C1.join(['type=button'    ,POS_FMT(l=ACTS_L[4],    t=ACTS_T[1],   r=ACTS_L[4]+ACTS_W,b=0)
                      ,'cap=&Up'
                      ])] # i= 6
            +[C1.join(['type=button'    ,POS_FMT(l=ACTS_L[4],    t=ACTS_T[2],   r=ACTS_L[4]+ACTS_W,b=0)
                      ,'cap=Do&wn'
                      ])] # i= 7
            +[C1.join(['type=button'    ,POS_FMT(l=ACTS_L[6],    t=ACTS_T[1],   r=ACTS_L[7]+ACTS_W,b=0)
                      ,'cap=Set &main for lexers...'
                      ])] # i= 8
            # DLG ACTS
            +[C1.join(['type=button'    ,POS_FMT(l=DLG_W-GAP2-ACTS_W,    t=ACTS_T[1],   r=DLG_W-GAP2,b=0)
                      ,'cap=Ad&just...'
                      ])] # i= 9
            +[C1.join(['type=button'    ,POS_FMT(l=DLG_W-GAP2-ACTS_W,    t=ACTS_T[2],   r=DLG_W-GAP2,b=0)
                      ,'cap=Cl&ose'
                      ])] # i=10
            ), 1)    # start focus
            if ans is None:  break #while
            (ans_i
            ,vals)      = ans
            vals        = vals.splitlines()
            pass;              #LOG and log('ans_i, vals={}',(ans_i, list(enumerate(vals))))
            ans_s       = apx.icase(False,''
                           ,ans_i== 2,'edit'
                           ,ans_i== 3,'add'
                           ,ans_i== 4,'clone'
                           ,ans_i== 5,'del'
                           ,ans_i== 6,'up'
                           ,ans_i== 7,'down'
                           ,ans_i== 8,'main'
                           ,ans_i== 9,'custom'
                           ,ans_i==10,'close'
                           ,'?')
            if ans_s=='close':
                break #while
            if ans_s=='custom': #Custom
                custs   = app.dlg_input_ex(10, 'Custom dialog Tools. Use L,R,C before width to align (empty=L). Use "-" to hide column.'
                    , 'Width of Name    (min 100)'  , prs.get('nm'  , '150')
                    , 'Width of Keys    (min  50)'  , prs.get('keys', '100')
                    , 'Width of File    (min 150)'  , prs.get('file', '180')
                    , 'Width of Params  (min 150)'  , prs.get('prms', '100')
                    , 'Width of Folder  (min 100)'  , prs.get('ddir', '100')
                    , 'Width of Lexers  (min  50)'  , prs.get('lxrs', 'C50')
                    , 'Width of Capture (min  50)'  , prs.get('rslt', 'C50')
                    , 'Width of Saving  (min  30)'  , prs.get('savs', 'C30')
                    , 'List height  (min 200)'      , str(self.dlg_prs.get('h_list', 300))
                    , 'Button width (min 70)'       , str(self.dlg_prs.get('w_btn', 90))
                    )
                if custs is not None:
                    def adapt2min(vmin, cval, can_hide=True):
                        cval    = cval.upper()
                        if can_hide and cval[0]=='-':   return cval
                        cval    = cval.lstrip('-')
                        c1st    = cval[0] if cval[0] in 'LRC' else ''
                        cval    = cval.lstrip('LRC')
                        return    c1st + str(max(vmin, int(cval)))
                    self.dlg_prs['nm']      = adapt2min(100, custs[0], False)
                    self.dlg_prs['keys']    = adapt2min( 50, custs[1], True)
                    self.dlg_prs['file']    = adapt2min(150, custs[2], True)
                    self.dlg_prs['prms']    = adapt2min(150, custs[3], True)
                    self.dlg_prs['ddir']    = adapt2min(100, custs[4], True)
                    self.dlg_prs['lxrs']    = adapt2min( 50, custs[5], True)
                    self.dlg_prs['rslt']    = adapt2min( 50, custs[6], True)
                    self.dlg_prs['savs']    = adapt2min( 30, custs[7], True)
                    self.dlg_prs['h_list']  =       max(200, int(custs[8]))
                    self.dlg_prs['w_btn']   =       max( 70, int(custs[9]))
                    open(EXTS_JSON, 'w').write(json.dumps(
                        {'ver':JSON_FORMAT_VER
                        ,'list':self.exts
                        ,'ext4lxr':self.ext4lxr
                        ,'dlg_prs':self.dlg_prs}, indent=4))
                continue #while

            new_ext_ind = int(vals[ 1])

            if ans_s=='main': #Main lexer tool
                self.dlg_main_tool()
                continue #while
            
            if ans_s=='add': #New
                file4run    = app.dlg_file(True, '!', '', '')   # '!' to disable check "filename exists"
                file4run    = file4run if file4run is not None else ''
                id4ext      = gen_ext_id(self.ext4id)
                ext         = self._fill_ext({'id':id4ext
                                    ,'nm':(os.path.basename(file4run) if file4run else 'tool{}'.format(len(self.exts)))
                                    ,'file':file4run
                                    ,'ddir':os.path.dirname(file4run)})
                ed_ans      = self.dlg_config_prop(ext, keys)
                pass;           LOG and log('fin edit={}',ed_ans)
                if ed_ans is None:
                    continue #while
                self.exts  += [ext]
                ids         = [ext['id'] for ext in self.exts]
                ext_ind     = len(self.exts)-1
                new_ext_ind = ext_ind           ## need?

            if new_ext_ind==-1:
                continue #while
                
            what    = ''
            ext_ind = new_ext_ind
            self.last_ext_id    = ids[ext_ind]
            if False:pass
            
            elif ans_s=='edit':
                pass;          #LOG and log('?? edit self.exts[new_ext_ind]={}',self.exts[new_ext_ind])
                ed_ans  = self.dlg_config_prop(self.exts[new_ext_ind], keys)
                if ed_ans is None or not ed_ans:
                    pass;      #LOG and log('// edit self.exts[new_ext_ind]={}',self.exts[new_ext_ind])
                    continue # while
                pass;          #LOG and log('ok edit self.exts[new_ext_ind]={}',self.exts[new_ext_ind])
                
            elif ans_s=='up' and new_ext_ind>0: #Up
                pass;          #LOG and log('up',())
                (self.exts[new_ext_ind-1]
                ,self.exts[new_ext_ind  ])  = (self.exts[new_ext_ind  ]
                                              ,self.exts[new_ext_ind-1])
                ext_ind = new_ext_ind-1
            elif ans_s=='down' and new_ext_ind<(len(self.exts)-1): #Down
                pass;           LOG and log('dn',())
                (self.exts[new_ext_ind  ]
                ,self.exts[new_ext_ind+1])  = (self.exts[new_ext_ind+1]
                                              ,self.exts[new_ext_ind  ])
                ext_ind = new_ext_ind+1
            
            elif ans_s=='clone': #Clone
                cln_ext     = copy.deepcopy(self.exts[new_ext_ind])
                cln_ext['id']= gen_ext_id(self.ext4id)
                cln_ext['nm']= cln_ext['nm']+' clone'
                self.exts   += [cln_ext]
                ext_ind     = len(self.exts)-1

            elif ans_s=='del': #Del
#               if app.msg_box( 'Delete Tool\n    {}'.format(exkys[new_ext_ind])
                flds    = list(ext_nz_d.keys())
                ext_vls = ext_vlss[new_ext_ind]
                if app.msg_box( 'Delete Tool?\n\n' + '\n'.join(['{}: {}'.format(flds[ind], ext_vls[ind]) 
                                                            for ind in range(len(flds))
                                                       ])
                              , app.MB_YESNO)!=app.ID_YES:
                    continue # while
                what    = 'delete:'+str(self.exts[new_ext_ind]['id'])
                del self.exts[new_ext_ind]
                ext_ind = min(new_ext_ind, len(self.exts)-1)

            pass;              #LOG and log('?? list _do_acts',)
            self._do_acts(what)
           #while True
       #def dlg_config_list
        
    def dlg_main_tool(self):
        lxrs_l  = app.lexer_proc(app.LEXER_GET_LIST, '').splitlines()
        lxrs_l  = [lxr for lxr in lxrs_l if app.lexer_proc(app.LEXER_GET_ENABLED, lxr)]
        lxrs_l += ['(none)']
        nm4ids  = {ext['id']:ext['nm'] for ext in self.exts}
        nms     = [ext['nm'] for ext in self.exts]
        ids     = [ext['id'] for ext in self.exts]
        lxr_ind     = 0
        tool_ind    = 0
        focused     = 1
        while True:
            DLG_W, DLG_H= GAP*3+200*2, GAP*4+300+23*2

            lxrs_enm    = ['"{}"{}'.format(lxr, ' - "{}"'.format(nm4ids[self.ext4lxr[lxr]]) if lxr in self.ext4lxr else '')
                            for lxr in lxrs_l]

            ans = app.dlg_custom('Tool properties'   ,GAP*3+200*2, GAP*4+300+23*2, '\n'.join([]
            # TOOL PROPS
            +[C1.join(['type=label'     ,POS_FMT(l=GAP,         t=3+GAP,   r=GAP+200,        b=0)
                      ,'cap=&Lexer - main tool'
                      ])] # i= 0
            +[C1.join(['type=listbox'   ,POS_FMT(l=GAP,         t=GAP+23,  r=GAP+200,        b=GAP+23+300)
                      ,'items=' +'\t'.join(lxrs_enm)
                      ,'val='   +str(lxr_ind)  # start sel
                      ])] # i= 1
            +[C1.join(['type=label'     ,POS_FMT(l=GAP+200+GAP, t=3+GAP,   r=GAP+200+GAP+200,b=0)
                      ,'cap=&Tools'
                      ])] # i= 2
            +[C1.join(['type=listbox'   ,POS_FMT(l=GAP+200+GAP, t=GAP+23,  r=GAP+200+GAP+200,b=GAP+23+300)
                      ,'items=' +'\t'.join(nms)
                      ,'val='   +str(tool_ind)  # start sel
                      ])] # i= 3
            +[C1.join(['type=button'    ,POS_FMT(l=GAP,         t=GAP+23+300+GAP,r=GAP+90,b=0)
                      ,'cap=&Assign'
                      ])] # i= 4
            +[C1.join(['type=button'    ,POS_FMT(l=GAP+90+GAP,  t=GAP+23+300+GAP,r=GAP+90+GAP+90,b=0)
                      ,'cap=&Break'
                      ])] # i= 5
 
            +[C1.join(['type=button'    ,POS_FMT(l=DLG_W-GAP-80,t=GAP+23+300+GAP,r=DLG_W-GAP,b=0)
                      ,'cap=&Close'
                      ])] # i= 6
            ), focused)    # start focus
            if ans is None:  
                return None
            (ans_i
            ,vals)      = ans
            vals        = vals.splitlines()
            pass;              #LOG and log('ans_i, vals={}',(ans_i, list(enumerate(vals))))
            ans_s       = apx.icase(False,''
                           ,ans_i== 4,'assign'
                           ,ans_i== 5,'break'
                           ,ans_i== 6,'close'
                           )
            lxr_ind     = int(vals[ 1])
            tool_ind    = int(vals[ 3])
            if ans_s=='close':  return
            changed     = False
            if False:pass
            elif (ans_s=='assign' 
            and   lxr_ind in range(len(lxrs_l)) 
            and   tool_ind in range(len(ids))):      #Assign
                self.ext4lxr[lxrs_l[lxr_ind]]   = ids[tool_ind]
                changed = True

            elif (ans_s=='break' 
            and   lxr_ind in range(len(lxrs_l))
            and   lxrs_l[lxr_ind] in self.ext4lxr):  #Break
                del self.ext4lxr[lxrs_l[lxr_ind]]
                changed = True

            if changed:
                open(EXTS_JSON, 'w').write(json.dumps(
                    {'ver':JSON_FORMAT_VER
                    ,'dlg_prs':self.dlg_prs
                    ,'ext4lxr':self.ext4lxr
                    ,'list':self.exts
                    }, indent=4))
           #while True
       #def dlg_main_tool
        
    def dlg_config_prop(self, ext, keys=None):
        if keys is None:
            keys_json   = app.app_path(app.APP_DIR_SETTINGS)+os.sep+'keys.json'
            keys        = apx._json_loads(open(keys_json).read()) if os.path.exists(keys_json) else {}
        kys         = get_keys_desc('cuda_exttools,run', ext['id'], keys)

        ed_ext      = copy.deepcopy(ext)
        
        GAP2            = GAP*2    
        PRP1_W, PRP1_L  = (100, GAP)
        PRP2_W, PRP2_L  = (400, PRP1_L+    PRP1_W)
        PRP3_W, PRP3_L  = (100, PRP2_L+GAP+PRP2_W)
        PROP_T          = [GAP*ind+23*(ind-1) for ind in range(20)]   # max 20 rows
        DLG_W, DLG_H    = PRP3_L+PRP3_W+GAP, PROP_T[14]+GAP
        
        focused         = 1
        while True:
            ed_kys  = get_keys_desc('cuda_exttools,run', ed_ext['id'], keys)
            val_savs= self.savs_vals.index(ed_ext['savs']) if ed_ext is not None else 0
            val_rslt= self.rslt_vals.index(ed_ext['rslt']) if ed_ext is not None else 0
            
            ans = app.dlg_custom('Tool properties'   ,DLG_W, DLG_H, '\n'.join([]
            # TOOL PROPS
            +[C1.join(['type=label'     ,POS_FMT(l=PRP1_L,  t=PROP_T[1]+3,  r=PRP1_L+PRP1_W,b=0)
                      ,'cap=&Name'
                      ])] # i= 0
            +[C1.join(['type=edit'      ,POS_FMT(l=PRP2_L,  t=PROP_T[1],  r=PRP2_L+PRP2_W,b=0)
                      ,'val='+ed_ext['nm']
                      ])] # i= 1
                      
            +[C1.join(['type=label'     ,POS_FMT(l=PRP1_L,  t=PROP_T[2]+3,r=PRP1_L+PRP1_W,b=0)
                      ,'cap=&File name'
                      ])] # i= 2
            +[C1.join(['type=edit'      ,POS_FMT(l=PRP2_L,  t=PROP_T[2],  r=PRP2_L+PRP2_W,b=0)
                      ,'val='+ed_ext['file']
                      ])] # i= 3
            +[C1.join(['type=button'    ,POS_FMT(l=PRP3_L,  t=PROP_T[2]-1,r=PRP3_L+PRP3_W,b=0)
                      ,'cap=&Browse...'
                      ])] # i= 4
                      
            +[C1.join(['type=check'     ,POS_FMT(l=PRP2_L,  t=PROP_T[3]-2,r=PRP1_L+PRP1_W,b=0)
                      ,'cap=&Shell command'
                      ,'val='+('1' if ed_ext['shll'] else '0')
                      ])] # i= 5
                      
            +[C1.join(['type=label'     ,POS_FMT(l=PRP1_L,  t=PROP_T[4]+3,r=PRP1_L+PRP1_W,b=0)
                      ,'cap=&Parameters'
                      ])] # i= 6
            +[C1.join(['type=edit'      ,POS_FMT(l=PRP2_L,  t=PROP_T[4],  r=PRP2_L+PRP2_W,b=0)
                      ,'val='+ed_ext['prms']
                      ])] # i= 7
            +[C1.join(['type=button'    ,POS_FMT(l=PRP3_L,  t=PROP_T[4]-1,r=PRP3_L+PRP3_W,b=0)
                      ,'cap=A&dd...'
                      ])] # i= 8
                      
            +[C1.join(['type=label'     ,POS_FMT(l=PRP1_L,  t=PROP_T[5]+3,r=PRP1_L+PRP1_W,b=0)
                      ,'cap=&Initial folder'
                      ])] # i= 9
            +[C1.join(['type=edit'      ,POS_FMT(l=PRP2_L,  t=PROP_T[5],  r=PRP2_L+PRP2_W,b=0)
                      ,'val='+ed_ext['ddir']
                      ])] # i=10
            +[C1.join(['type=button'    ,POS_FMT(l=PRP3_L,  t=PROP_T[5]-1,r=PRP3_L+PRP3_W,b=0)
                      ,'cap=B&rowse...'
                      ])] # i=11
                      
            +[C1.join(['type=label'     ,POS_FMT(l=PRP1_L,  t=PROP_T[6]+3,r=PRP1_L+PRP1_W,b=0)
                      ,'cap=Lexers'
                      ])] # i=12
            +[C1.join(['type=edit'      ,POS_FMT(l=PRP2_L,  t=PROP_T[6],  r=PRP2_L+PRP2_W,b=0)
                      ,'val='+ed_ext['lxrs']
                      ,'props=1,0,1'    # ro,mono,border
                      ])] # i=13
            +[C1.join(['type=button'    ,POS_FMT(l=PRP3_L,  t=PROP_T[6]-1,r=PRP3_L+PRP3_W,b=0)
                      ,'cap=Le&xers...'
                      ])] # i=14
                      
            +[C1.join(['type=label'     ,POS_FMT(l=PRP1_L,  t=PROP_T[7]+3,r=PRP1_L+PRP1_W,b=0)
                      ,'cap=Main for'
                      ])] # i=15
            +[C1.join(['type=edit'      ,POS_FMT(l=PRP2_L,  t=PROP_T[7],  r=PRP2_L+PRP2_W,b=0)
                      ,'val='+ ','.join([lxr for (lxr,eid) in self.ext4lxr.items() if eid==ed_ext['id']])
                      ,'props=1,0,1'    # ro,mono,border
                      ])] # i=16
            +[C1.join(['type=button'    ,POS_FMT(l=PRP3_L,  t=PROP_T[7]-1,r=PRP3_L+PRP3_W,b=0)
                      ,'cap=Set &main...'
                      ])] # i=17
                      
            +[C1.join(['type=label'     ,POS_FMT(l=PRP1_L,  t=PROP_T[8]+3,r=PRP1_L+PRP1_W,b=0)
                      ,'cap=Sa&ve before'
                      ])] # i=18
            +[C1.join(['type=combo_ro'  ,POS_FMT(l=PRP2_L,  t=PROP_T[8]-1,r=PRP2_L+PRP2_W,b=0)
                      ,'items='+'\t'.join(self.savs_caps)
                      ,'val='   +str(val_savs)  # start sel
                      ])] # i=19
                      
            +[C1.join(['type=label'     ,POS_FMT(l=PRP1_L,  t=PROP_T[9]+3,r=PRP1_L+PRP1_W,b=0)
                      ,'cap=Hotkey'
                      ])] # i=20
            +[C1.join(['type=edit'      ,POS_FMT(l=PRP2_L,  t=PROP_T[9],  r=PRP2_L+PRP2_W,b=0)
                      ,'val='+ed_kys
                      ,'props=1,0,1'    # ro,mono,border
                      ])] # i=21
            +[C1.join(['type=button'    ,POS_FMT(l=PRP3_L,  t=PROP_T[9]-1,r=PRP3_L+PRP3_W,b=0)
                      ,'cap=Assi&gn...'
                      ])] # i=22
                      
            +[C1.join(['type=label'     ,POS_FMT(l=PRP1_L,  t=PROP_T[10]+3,r=PRP1_L+PRP1_W,b=0)
                      ,'cap=&Capture output'
                      ])] # i=23
            +[C1.join(['type=combo_ro'  ,POS_FMT(l=PRP2_L,  t=PROP_T[10]-1,r=PRP2_L+PRP2_W,b=0)
                      ,'items='+'\t'.join(self.rslt_caps)
                      ,'val='   +str(val_rslt)  # start sel
                      ])] # i=24
                      
            +[C1.join(['type=label'     ,POS_FMT(l=PRP1_L,  t=PROP_T[11]+3,r=PRP1_L+PRP1_W,b=0)
                      ,'cap=Encoding'
                      ])] # i=25
            +[C1.join(['type=edit'      ,POS_FMT(l=PRP2_L,  t=PROP_T[11],  r=PRP2_L+PRP2_W,b=0)
                      ,'val='+ed_ext['encd']
                      ,'props=1,0,1'    # ro,mono,border
                      ])] # i=26
            +[C1.join(['type=button'    ,POS_FMT(l=PRP3_L,  t=PROP_T[11]-1,r=PRP3_L+PRP3_W,b=0)
                      ,'cap=S&elect...'
                      ])] # i=27
            # DLG ACTS
            +[C1.join(['type=button'    ,POS_FMT(l=GAP,                 t=PROP_T[13],r=GAP+PRP1_W,b=0)
                      ,'cap=Help'
                      ])] # i=28
            +[C1.join(['type=button'    ,POS_FMT(l=DLG_W-GAP*2-100*2,   t=PROP_T[13],r=DLG_W-GAP*2-100*1,b=0)
                      ,'cap=OK'
                      ,'props=1' #default
                      ])] # i=29
            +[C1.join(['type=button'    ,POS_FMT(l=DLG_W-GAP*1-100*1,   t=PROP_T[13],r=DLG_W-GAP*1-100*0,b=0)
                      ,'cap=Cancel'
                      ])] # i=30
            ), focused)    # start focus
            if ans is None:  
                return None
            (ans_i
            ,vals)      = ans
            vals        = vals.splitlines()
            pass;              #LOG and log('ans_i, vals={}',(ans_i, list(enumerate(vals))))
            ans_s       = apx.icase(False,''
                           ,ans_i== 4,'file'
                           ,ans_i== 8,'prms'
                           ,ans_i==11,'ddir'
                           ,ans_i==14,'lxrs'
                           ,ans_i==17,'main'
                           ,ans_i==22,'hotkeys'
                           ,ans_i==27,'encd'
                           ,ans_i==28,'help'
                           ,ans_i==29,'save'
                           ,ans_i==30,'cancel'
                           ,'?')
            ed_ext['nm']    =   vals[ 1]
            ed_ext['file']  =   vals[ 3]
            ed_ext['shll']  =   vals[ 5]=='1'
            ed_ext['prms']  =   vals[ 7]
            ed_ext['ddir']  =   vals[10]
            ed_ext['lxrs']  =   vals[13]
            ed_ext['savs']  = self.savs_vals[int(
                                vals[19])]
            ed_ext['rslt']  = self.rslt_vals[int(
                                vals[24])]
            ed_ext['encd']  =   vals[26]
                
            if ans_s=='cancel':
                return None
            if ans_s=='save':
                #Checks
                if False:pass
                elif not ed_ext['nm']:
                    app.msg_box('Set name', app.MB_OK)
                    focused = 1
                    continue #while
                elif not ed_ext['file']:
                    app.msg_box('Set file', app.MB_OK)
                    focused = 3
                    continue #while
                    
                pass;          #LOG and log('save    ext={}',ext)
                pass;          #LOG and log('save ed_ext={}',ed_ext)
                if ext==ed_ext and kys==ed_kys:
                    return False
                for fld in ed_ext:
                    ext[fld] = ed_ext[fld]
                pass;          #LOG and log('ok ext={}',ext)
                return True

            if False:pass
            elif ans_s=='help':
                show_help()
                continue #while

            if ans_s=='main': #Main lexer tool
                self.dlg_main_tool()
                continue #while
            
            elif ans_s=='file': #File
                file4run= app.dlg_file(True, '!'+ed_ext['file'], '', '')# '!' to disable check "filename exists"
                if file4run is not None:
                    ed_ext['file'] = file4run
            
            elif ans_s=='ddir': #File
                file4dir= app.dlg_file(True, '!', ed_ext['ddir'], '')   # '!' to disable check "filename exists"
                if file4dir is not None:
                    ed_ext['ddir'] = os.path.dirname(file4dir)

            elif ans_s=='prms': #Append param {*}
                prms_l  =([]
                        +['{FileName}\tFull path']
                        +['{FileDir}\tFolder path, without file name']
                        +['{FileNameOnly}\tFile name only, without folder path']
                        +['{FileNameNoExt}\tFile name without extension and path']
                        +['{FileExt}\tExtension']
                        +['{CurrentLine}\tText of current line']
                        +['{CurrentLineNum}\tNumber of current line']
                        +['{CurrentColumnNum}\tNumber of current column']
                        +['{SelectedText}\tText' ]
                        +['{Interactive}\tText will be asked at each running']
                        +['{InteractiveFile}\tFile name will be asked'])
                prm_i   = app.dlg_menu(app.MENU_LIST_ALT, '\n'.join(prms_l))
                if prm_i is not None:
                    ed_ext['prms'] += (' '+prms_l[prm_i].split('\t')[0])

            elif ans_s=='hotkeys': #Hotkeys
                app.dlg_hotkeys('cuda_exttools,run,'+str(ed_ext['id']))
                keys    = apx._json_loads(open(keys_json).read()) if os.path.exists(keys_json) else {}
            
            elif ans_s=='lxrs': #Lexers only
                lxrs    = ','+ed_ext['lxrs']+','
                lxrs_l  = app.lexer_proc(app.LEXER_GET_LIST, '').splitlines()
                lxrs_l += ['(none)']
                sels    = ['1' if ','+lxr+',' in lxrs else '0' for lxr in lxrs_l]
                crt     = str(sels.index('1') if '1' in sels else 0)
                ans     = app.dlg_custom('Select lexers'   ,GAP+200+GAP, GAP+400+GAP+24+GAP, '\n'.join([]
                +[C1.join(['type=checklistbox'  ,POS_FMT(l=GAP,             t=GAP,          r=GAP+200,   b=GAP+400)
                          ,'items=' +'\t'.join(lxrs_l)
                          ,'val='   + crt+';'+','.join(sels)
                          ])] # i=0
                +[C1.join(['type=button'        ,POS_FMT(l=    200-120,      t=GAP+400+GAP,  r=    200- 60,   b=0)
                          ,'cap=OK'
                          ])] # i=1
                +[C1.join(['type=button'        ,POS_FMT(l=GAP+200- 60,      t=GAP+400+GAP,  r=GAP+200,   b=0)
                          ,'cap=Cancel'
                          ])] # i=2
                ), 0)    # start focus
                if ans is not None and ans[0]==1:
                    crt,sels= ans[1].splitlines()[0].split(';')
                    sels    = sels.strip(',').split(',')
                    lxrs    = [lxr for (ind,lxr) in enumerate(lxrs_l) if sels[ind]=='1']
                    ed_ext['lxrs'] = ','.join(lxrs)
            
            elif ans_s=='encd': #Lexers only
                enc_nms = get_encoding_names()
                enc_ind = app.dlg_menu(app.MENU_LIST_ALT, '\n'.join(enc_nms))
                if enc_ind is not None:
                    ed_ext['encd'] = enc_nms[enc_ind].split('\t')[0]
            
           #while True
       #def dlg_config_prop
        
   #class Command

def subst_props(prm, file_nm, cCrt, rCrt):
    if '{FileName}'         in prm: prm = prm.replace('{FileName}'     ,                          file_nm)
    if '{FileDir}'          in prm: prm = prm.replace('{FileDir}'      ,          os.path.dirname(file_nm))
    if '{FileNameOnly}'     in prm: prm = prm.replace('{FileNameOnly}' ,         os.path.basename(file_nm))
    if '{FileNameNoExt}'    in prm: prm = prm.replace('{FileNameNoExt}','.'.join(os.path.basename(file_nm).split('.')[0:-1]))
    if '{FileExt}'          in prm: prm = prm.replace('{FileExt}'      ,         os.path.basename(file_nm).split('.')[-1])

    if '{CurrentLine}'      in prm: prm = prm.replace('{CurrentLine}'     , ed.get_text_line(rCrt))
    if '{CurrentLineNum}'   in prm: prm = prm.replace('{CurrentLineNum}'  , str(1+rCrt))
    if '{CurrentColumnNum}' in prm: prm = prm.replace('{CurrentColumnNum}', str(1+ed.convert(app.CONVERT_CHAR_TO_COL, cCrt, rCrt)[0]))
    if '{SelectedText}'     in prm: prm = prm.replace('{SelectedText}'    , ed.get_text_sel())

    if '{Interactive}' in prm:
        ans = app.dlg_input('Param for call {}'.format(ext['nm']), '')
        if ans is None: return
        prm = prm.replace('{Interactive}'     , ans)
    if '{InteractiveFile}' in prm:
        ans = app.dlg_file(True, '!', '', '')   # '!' to disable check "filename exists"
        if ans is None: return
        prm = prm.replace('{InteractiveFile}' , ans)
    return prm

def gen_ext_id(ids):
    id4ext      = random.randint(100000, 999999)
    while id4ext in ids:
        id4ext  = random.randint(100000, 999999)
    return id4ext

def get_usage_names():
    return [
        RSLT_NO
    ,   RSLT_TO_PANEL
    ,   RSLT_TO_PANEL_AP
    ,   RSLT_TO_NEWDOC
    ,   RSLT_TO_CLIP
    ,   RSLT_REPL_SEL
    ]
   #def get_usage_names

def show_help():
    l   = chr(13)
    hlp = (''
        +   'In properties'
        +l+ '   Parameters'
        +l+ '   Initial folder'
        +l+ 'the following macros will be are substituted.'
        +l+ ''
        +l+ 'Currently focused file properties:'
        +l+ '   {FileName}         - Full path'
        +l+ '   {FileDir}          - Folder path, without file name'
        +l+ '   {FileNameOnly}     - Name only, without folder path'
        +l+ '   {FileNameNoExt}    - Name without extension and path'
        +l+ '   {FileExt}          - Extension'
        +l+ ''
        +l+ 'Currently focused editor properties (for top caret):'
       #+l+ '   {CurrentWord}'
        +l+ '   {CurrentLine}      - text'
        +l+ '   {CurrentLineNum}   - number'
        +l+ '   {CurrentColumnNum} - number'
        +l+ '   {SelectedText}     - text' 
        +l+ ''
        +l+ 'Prompted:'
        +l+ '   {Interactive}      - Text will be asked at each running'
        +l+ '   {InteractiveFile}  - File name will be asked'
        +'')
    app.dlg_custom( 'Tool Help', GAP*2+500, GAP*3+25+400, '\n'.join([]
        +[C1.join(['type=memo'      ,POS_FMT(l=GAP, t=GAP,          r=GAP+500,      b=GAP+400)
                  ,'val='+hlp
                  ,'props=1,1,1'    # ro,mono,border
                  ] # i=1
         )]
         +[C1.join(['type=button'   ,POS_FMT(l=GAP+500-90, t=GAP+400+GAP,  r=GAP+500, b=0)
                  ,'cap=&Close'
                  ] # i=7
         )]
    ), 1)    # start focus

def get_keys_desc(mdl_mth, id, keys=None):
    if keys is None:
        keys_json   = app.app_path(app.APP_DIR_SETTINGS)+os.sep+'keys.json'
        keys        = apx._json_loads(open(keys_json).read()) if os.path.exists(keys_json) else {}

    cmd_id  = '{},{}'.format(mdl_mth, id)
    cmd_keys= keys.get(cmd_id, {})
    desc    = '/'.join([' * '.join(cmd_keys.get('s1', []))
                       ,' * '.join(cmd_keys.get('s2', []))
                       ]).strip('/')
    return desc
   #def get_keys_desc

'''
ToDo
[ ][kv-kv][09dec15] Run test cmd
'''
