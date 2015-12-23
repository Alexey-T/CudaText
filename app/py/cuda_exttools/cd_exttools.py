''' Plugin for CudaText editor
Authors:
    Andrey Kvichansky    (kvichans on github.com)
Version:
    '0.9.2 2015-12-22'
ToDo: (see end of file)
'''

import  os, json, random, subprocess, shlex, copy
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
        ext['ddir'] = ext.get('ddir', '')
        ext['shll'] = ext.get('shll', 'N')=='Y' if str(ext.get('shll', 'N')) in 'NY'            else ext.get('shll', False)
        ext['prms'] = ext.get('prms', '')
        ext['savs'] = ext.get('savs', 'N')
        ext['rslt'] = ext.get('rslt', 'N')      if     ext.get('rslt', 'N')  in self.rslt_vals  else 'N'
        ext['encd'] = ext.get('encd', '')
        ext['lxrs'] = ext.get('lxrs', '')
        return ext

    def __init__(self):
        self.savs_caps   = ['Nothing', 'Current document', 'All documents']
        self.savs_vals   = ['N',       'Y',                'A']
        self.rslt_caps   = [RSLT_NO, RSLT_TO_PANEL, RSLT_TO_PANEL_AP, RSLT_TO_NEWDOC, RSLT_TO_CLIP, RSLT_REPL_SEL]
        self.rslt_vals   = ['N',     'OP',          'OPA',            'ND',           'CB',         'SEL']
        
        ver_exts            = apx._json_loads(open(EXTS_JSON).read()) if os.path.exists(EXTS_JSON) else {'ver':JSON_FORMAT_VER, 'list':[]}
        if ver_exts['ver'] < JSON_FORMAT_VER:
            # Adapt to new format
            pass
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
        app.app_proc(app.PROC_MENU_ADD, '{};cuda_exttools,dlg_config;{}'.format(id_menu, 'Con&fig...'))
        if 0==len(self.exts):
            return
        app.app_proc(app.PROC_MENU_ADD, '{};;-'.format(id_menu))
        for ext in self.exts:
            app.app_proc(app.PROC_MENU_ADD, '{};cuda_exttools,run,{};{}'.format(id_menu, ext['id'], ext['nm']))
       #def _adapt_menu
        
    def dlg_config(self):
        ''' Show dlg for change exts list.
        '''
        if app.app_api_version()<FROM_API_VERSION:  return app.msg_status('Need update CudaText')
        keys_json   = app.app_path(app.APP_DIR_SETTINGS)+os.sep+'keys.json'
        keys        = apx._json_loads(open(keys_json).read()) if os.path.exists(keys_json) else {}
        
        ids     = [ext['id'] for ext in self.exts]
        ext_ind = ids.index(self.last_ext_id) if self.last_ext_id in ids else -1

        GAP2    = GAP*2    
        ACTS_W          = 100
        WD_LST, HT_LST  = (3*ACTS_W+GAP2, 300)
        PRP1_W, PRP1_L  = (100, GAP2+WD_LST+GAP +GAP2)
        PRP2_W, PRP2_L  = (300, PRP1_L+    PRP1_W)
        PRP3_W, PRP3_L  = (100, PRP2_L+GAP+PRP2_W)
        PROP_T          = [20+GAP       + GAP*ind+23*(ind-1) for ind in range(20)]
        ACTS_T          = [GAP2+HT_LST  + GAP*ind+23*(ind-1) for ind in range(20)]
        ACTS_L          = [GAP+         + GAP*ind+ACTS_W*(ind-1) for ind in range(20)]
        DLG_W, DLG_H    = PRP3_L+PRP3_W+GAP2, ACTS_T[3]+GAP
        while True:
            exkys   = []
            ekeys   = []
            for ext in self.exts:
                ext_cid = 'cuda_exttools,run,{}'.format(ext['id'])
                ext_keys= keys.get(ext_cid, {})
                kys     = '/'.join([' * '.join(ext_keys.get('s1', []))
                                   ,' * '.join(ext_keys.get('s2', []))
                                   ]).strip('/')
                exkys  += [ext['nm'] + (' ['+kys+']' if kys else '')]
                ekeys  +=                 [  kys  ]
            pass;               LOG and log('exkys={}',exkys)
            ext     = self.exts[ext_ind] if ext_ind in range(len(self.exts)) else None
            val_savs= self.savs_vals.index(ext['savs']) if ext is not None else 0
            val_rslt= self.rslt_vals.index(ext['rslt']) if ext is not None else 0
            
            ans = app.dlg_custom('Tools'   ,DLG_W, DLG_H, '\n'.join([]
            #LIST
            +[C1.join(['type=label'     ,POS_FMT(l=GAP2,        t=GAP,      r=GAP2+WD_LST+GAP, b=ACTS_T[3])
                      ,'cap=&Tools'
                      ])] # i= 0
            +[C1.join(['type=listbox'   ,POS_FMT(l=GAP2,        t=GAP+20,   r=GAP+4+WD_LST,   b=GAP+HT_LST)
                      ,'items=' +'\t'.join(exkys)
                      ,'val='   +str(ext_ind)  # start sel
                      ])] # i= 1
            # TOOLS ACTS
            +[C1.join(['type=button'    ,POS_FMT(l=ACTS_L[1],    t=ACTS_T[1],   r=ACTS_L[1]+ACTS_W,b=0)
                      ,'cap=&View prop'
                      ,'props=1'    # default
                      ])] # i= 2
            +[C1.join(['type=button'    ,POS_FMT(l=ACTS_L[2],    t=ACTS_T[1],   r=ACTS_L[2]+ACTS_W,b=0)
                      ,'cap=&Add...'
                      ])] # i= 3
            +[C1.join(['type=button'    ,POS_FMT(l=ACTS_L[3],    t=ACTS_T[1],   r=ACTS_L[3]+ACTS_W,b=0)
                      ,'cap=De&lete...'
                      ])] # i= 4
            +[C1.join(['type=button'    ,POS_FMT(l=ACTS_L[1],    t=ACTS_T[2],   r=ACTS_L[1]+ACTS_W,b=0)
                      ,'cap=Clone'
                      ])] # i= 5
            +[C1.join(['type=button'    ,POS_FMT(l=ACTS_L[2],    t=ACTS_T[2],   r=ACTS_L[2]+ACTS_W,b=0)
                      ,'cap=Up'
                      ])] # i= 6
            +[C1.join(['type=button'    ,POS_FMT(l=ACTS_L[3],    t=ACTS_T[2],   r=ACTS_L[3]+ACTS_W,b=0)
                      ,'cap=Down'
                      ])] # i= 7
            # TOOL PROPS
            +[C1.join(['type=label'     ,POS_FMT(l=PRP2_L,  t=GAP,      r=PRP1_L+GAP+PRP1_W+PRP2_W+PRP3_W+4, b=GAP+PROP_T[11])
                      ,'cap=Tool properties'
                      ])] # i= 8
                      
            +[C1.join(['type=label'     ,POS_FMT(l=PRP1_L,  t=PROP_T[1]+3,  r=PRP1_L+PRP1_W,b=0)
                      ,'cap=&Name'
                      ])] # i= 9
            +[C1.join(['type=edit'      ,POS_FMT(l=PRP2_L,  t=PROP_T[1],  r=PRP2_L+PRP2_W,b=0)
                      ,'val='+(ext['nm'] if ext is not None else '')
                      ])] # i=10
                      
            +[C1.join(['type=label'     ,POS_FMT(l=PRP1_L,  t=PROP_T[2]+3,r=PRP1_L+PRP1_W,b=0)
                      ,'cap=&File name'
                      ])] # i=11
            +[C1.join(['type=edit'      ,POS_FMT(l=PRP2_L,  t=PROP_T[2],  r=PRP2_L+PRP2_W,b=0)
                      ,'val='+(ext['file'] if ext is not None else '')
                      ])] # i=12
            +[C1.join(['type=button'    ,POS_FMT(l=PRP3_L,  t=PROP_T[2]-1,r=PRP3_L+PRP3_W,b=0)
                      ,'cap=&Browse...'
                      ])] # i=13
                      
            +[C1.join(['type=check'     ,POS_FMT(l=PRP2_L,  t=PROP_T[3]-2,r=PRP1_L+PRP1_W,b=0)
                      ,'cap=&Shell command'
                      ,'val='+(('1' if ext['shll'] else '0') if ext is not None else '0')
                      ])] # i=14
                      
            +[C1.join(['type=label'     ,POS_FMT(l=PRP1_L,  t=PROP_T[4]+3,r=PRP1_L+PRP1_W,b=0)
                      ,'cap=&Parameters'
                      ])] # i=15
            +[C1.join(['type=edit'      ,POS_FMT(l=PRP2_L,  t=PROP_T[4],  r=PRP2_L+PRP2_W,b=0)
                      ,'val='+(ext['prms'] if ext is not None else '')
                      ])] # i=16
            +[C1.join(['type=button'    ,POS_FMT(l=PRP3_L,  t=PROP_T[4]-1,r=PRP3_L+PRP3_W,b=0)
                      ,'cap=A&dd...'
                      ])] # i=17
                      
            +[C1.join(['type=label'     ,POS_FMT(l=PRP1_L,  t=PROP_T[5]+3,r=PRP1_L+PRP1_W,b=0)
                      ,'cap=&Initial folder'
                      ])] # i=18
            +[C1.join(['type=edit'      ,POS_FMT(l=PRP2_L,  t=PROP_T[5],  r=PRP2_L+PRP2_W,b=0)
                      ,'val='+(ext['ddir'] if ext is not None else '')
                      ])] # i=19
            +[C1.join(['type=button'    ,POS_FMT(l=PRP3_L,  t=PROP_T[5]-1,r=PRP3_L+PRP3_W,b=0)
                      ,'cap=B&rowse...'
                      ])] # i=20
                      
            +[C1.join(['type=label'     ,POS_FMT(l=PRP1_L,  t=PROP_T[6]+3,r=PRP1_L+PRP1_W,b=0)
                      ,'cap=Lexers'
                      ])] # i=21
            +[C1.join(['type=edit'      ,POS_FMT(l=PRP2_L,  t=PROP_T[6],  r=PRP2_L+PRP2_W,b=0)
                      ,'val='+(ext['lxrs'] if ext is not None else '')
                      ,'props=1,0,1'    # ro,mono,border
                      ])] # i=22
            +[C1.join(['type=button'    ,POS_FMT(l=PRP3_L,  t=PROP_T[6]-1,r=PRP3_L+PRP3_W,b=0)
                      ,'cap=Le&xers...'
                      ])] # i=23
                      
            +[C1.join(['type=label'     ,POS_FMT(l=PRP1_L,  t=PROP_T[7]+3,r=PRP1_L+PRP1_W,b=0)
                      ,'cap=Save &before'
                      ])] # i=24
            +[C1.join(['type=combo_ro'  ,POS_FMT(l=PRP2_L,  t=PROP_T[7]-1,r=PRP2_L+PRP2_W,b=0)
                      ,'items='+'\t'.join(self.savs_caps)
                      ,'val='   +str(val_savs)  # start sel
                      ])] # i=25
                      
            +[C1.join(['type=label'     ,POS_FMT(l=PRP1_L,  t=PROP_T[8]+3,r=PRP1_L+PRP1_W,b=0)
                      ,'cap=Hotkey'
                      ])] # i=26
            +[C1.join(['type=edit'      ,POS_FMT(l=PRP2_L,  t=PROP_T[8],  r=PRP2_L+PRP2_W,b=0)
                      ,'val='+(ekeys[ext_ind] if ext is not None else '')
                      ,'props=1,0,1'    # ro,mono,border
                      ])] # i=27
            +[C1.join(['type=button'    ,POS_FMT(l=PRP3_L,  t=PROP_T[8]-1,r=PRP3_L+PRP3_W,b=0)
                      ,'cap=Assi&gn...'
                      ])] # i=28
                      
            +[C1.join(['type=label'     ,POS_FMT(l=PRP1_L,  t=PROP_T[9]+3,r=PRP1_L+PRP1_W,b=0)
                      ,'cap=&Capture output'
                      ])] # i=29
            +[C1.join(['type=combo_ro'  ,POS_FMT(l=PRP2_L,  t=PROP_T[9]-1,r=PRP2_L+PRP2_W,b=0)
                      ,'items='+'\t'.join(self.rslt_caps)
                      ,'val='   +str(val_rslt)  # start sel
                      ])] # i=30
                      
            +[C1.join(['type=label'     ,POS_FMT(l=PRP1_L,  t=PROP_T[10]+3,r=PRP1_L+PRP1_W,b=0)
                      ,'cap=Encoding'
                      ])] # i=31
            +[C1.join(['type=edit'      ,POS_FMT(l=PRP2_L,  t=PROP_T[10],  r=PRP2_L+PRP2_W,b=0)
                      ,'val='+(ext['encd'] if ext is not None else '')
                      ,'props=1,0,1'    # ro,mono,border
                      ])] # i=32
            +[C1.join(['type=button'    ,POS_FMT(l=PRP3_L,  t=PROP_T[10]-1,r=PRP3_L+PRP3_W,b=0)
                      ,'cap=S&elect...'
                      ])] # i=33
            # DLG ACTS
            +[C1.join(['type=button'    ,POS_FMT(l=PRP3_L-GAP-PRP3_W,   t=ACTS_T[2],r=PRP3_L-GAP,b=0)
                      ,'cap=Help'
                      ])] # i=34
            +[C1.join(['type=button'    ,POS_FMT(l=PRP3_L,              t=ACTS_T[2],r=PRP3_L+PRP3_W,b=0)
                      ,'cap=Cl&ose'
                      ])] # i=35
            ), 1)    # start focus
            if ans is None:  break #while
            (ans_i
            ,vals)      = ans
            vals        = vals.splitlines()
            pass;               LOG and log('ans_i, vals={}',(ans_i, list(enumerate(vals))))
            ans_s       = apx.icase(False,''
                           ,ans_i== 2,'view'
                           ,ans_i== 3,'add'
                           ,ans_i== 4,'del'
                           ,ans_i== 5,'clone'
                           ,ans_i== 6,'up'
                           ,ans_i== 7,'down'
                           ,ans_i==13,'file'
                           ,ans_i==17,'prms'
                           ,ans_i==20,'ddir'
                           ,ans_i==23,'lxrs'
                           ,ans_i==28,'hotkeys'
                           ,ans_i==33,'encd'
                           ,ans_i==34,'help'
                           ,ans_i==35,'close'
                           ,'?')
            new_ext_ind = int(vals[ 1])
            pass;               LOG and log('ext_ind, new_ext_ind={}',(ext_ind, new_ext_ind))
            new_name    =     vals[10]
            new_file    =     vals[12]
            new_shll    =     vals[14]=='1'
            new_prms    =     vals[16]
            new_ddir    =     vals[19]
            new_savs    = int(vals[25])
            new_rslt    = int(vals[30])
            if ext is not None:
                # Save news
                ext['nm'  ] = new_name
                ext['file'] = new_file
                ext['shll'] = new_shll
                ext['prms'] = new_prms
                ext['ddir'] = new_ddir
                ext['savs'] = self.savs_vals[new_savs]
                ext['rslt'] = self.rslt_vals[new_rslt]
                self._do_acts()                         ##?? skip?
                
            if ans_s=='close':  break #while
            if ans_s=='help':
                show_help()
                continue #while
                
            if (ans_s=='view'
            and new_ext_ind in range(len(self.exts))):
                #NB! Only way to select other tool. 
                #    Ignore list selection for any PROP-acts.
                #    Selection will be restored after any PROP-acts
                ext_ind = new_ext_ind
                continue #while

            what    = ''
            changed = False

            # List acts
            if ans_s=='add': #New
                file4run    = app.dlg_file(True, '!', '', '')   # '!' to disable check "filename exists"
                file4run    = file4run if file4run is not None else ''
                id4ext      = gen_ext_id(self.ext4id)
                self.exts   += [self._fill_ext({'id':id4ext
                                ,'nm':(os.path.basename(file4run) if file4run else 'tool{}'.format(len(self.exts)))
                                ,'file':file4run
                                ,'ddir':os.path.dirname(file4run)})]
                ext_ind     = len(self.exts)-1
                changed     = True

            if new_ext_ind not in range(len(self.exts)):
                continue #while
            if False:pass
                
            elif ans_s=='up' and new_ext_ind>0: #Up
                pass;           LOG and log('up',())
                (self.exts[new_ext_ind-1]
                ,self.exts[new_ext_ind  ])  = (self.exts[new_ext_ind  ]
                                              ,self.exts[new_ext_ind-1])
                ext_ind = new_ext_ind-1
                changed = True
            elif ans_s=='down' and new_ext_ind<(len(self.exts)-1): #Down
                pass;           LOG and log('dn',())
                (self.exts[new_ext_ind  ]
                ,self.exts[new_ext_ind+1])  = (self.exts[new_ext_ind+1]
                                              ,self.exts[new_ext_ind  ])
                ext_ind = new_ext_ind+1
                changed = True
            
            elif ans_s=='clone': #Clone
                cln_ext     = copy.deepcopy(self.exts[new_ext_ind])
                cln_ext['id']= gen_ext_id(self.ext4id)
                cln_ext['nm']= cln_ext['nm']+' clone'
                self.exts   += [cln_ext]
                ext_ind     = len(self.exts)-1
                changed     = True

            elif ans_s=='del': #Del
                if app.msg_box( 'Delete Tool\n    {}'.format(exkys[new_ext_ind])
                              , app.MB_YESNO)!=app.ID_YES:  continue # while
                what    = 'delete:'+str(self.exts[new_ext_ind]['id'])
                del self.exts[new_ext_ind]
                ext_ind = min(new_ext_ind, len(self.exts)-1)
                changed = True

            # Prop acts
            if ext_ind not in range(len(self.exts)):
                continue #while
            if False:pass
                
            elif ans_s=='file': #File
                file4run= app.dlg_file(True, '!'+ext['file'], '', '')# '!' to disable check "filename exists"
                if file4run is not None:
                    ext['file'] = file4run
                    changed = True
            elif ans_s=='ddir': #File
                file4dir= app.dlg_file(True, '!', ext['ddir'], '')   # '!' to disable check "filename exists"
                if file4dir is not None:
                    ext['ddir'] = os.path.dirname(file4dir)
                    changed = True

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
                    ext['prms'] += (' '+prms_l[prm_i].split('\t')[0])
                    changed = True


            elif ans_s=='hotkeys': #Hotkeys
                app.dlg_hotkeys('cuda_exttools,run,'+str(ext['id']))
                keys    = apx._json_loads(open(keys_json).read()) if os.path.exists(keys_json) else {}
            
            elif ans_s=='lxrs': #Lexers only
                lxrs    = ','+ext['lxrs']+','
                lxrs_l  = app.lexer_proc(app.LEXER_GET_LIST, '').splitlines()
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
                    ext['lxrs'] = ','.join(lxrs)
                    changed = True
            
            elif ans_s=='encd': #Lexers only
                enc_nms = get_encoding_names()
                enc_ind = app.dlg_menu(app.MENU_LIST_ALT, '\n'.join(enc_nms))
                if enc_ind is not None:
                    ext['encd'] = enc_nms[enc_ind].split('\t')[0]
                    changed = True
            
            if changed or not what:
                self._do_acts(what)
           #while True
       #def dlg_config
       
    def _do_acts(self, what='', acts='|save|second|reg|keys|menu|'):
        ''' Use exts list '''
        pass;                  #LOG and log('what, acts={}',(what, acts))
        # Save
        if '|save|' in acts:
            open(EXTS_JSON, 'w').write(json.dumps({'ver':JSON_FORMAT_VER, 'list':self.exts}, indent=4))
        
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

    def run(self, ext_id):
        ''' Main (and single) way to run any exttool
        '''
        ext_id  = str(ext_id)
        pass;                  #LOG and log('ext_id={}',ext_id)
        ext     = self.ext4id.get(str(ext_id))
        if ext is None:
            return app.msg_status('No Tool: {}'.format(ext_id))
        cmnd    = ext['file']
        prms_s  = ext['prms']
        ddir    = ext['ddir']
        pass;                   LOG and log('nm="{}", cmnd="{}", ddir="{}", prms_s="{}"',ext['nm'], ddir, cmnd, prms_s)
        
        # Saving
        if 'Y'==ext.get('savs', 'N'):
            if not app.file_save():  return
        if 'A'==ext.get('savs', 'N'):
            ed.cmd(cmds.cmd_FileSaveAll)
        
        # Preparing
        file_nm = ed.get_filename()
        (cCrt, rCrt
        ,cEnd, rEnd)    = ed.get_carets()[0]
        prms_l  = shlex.split(prms_s)
        for ind, prm in enumerate(prms_l):
            prm_raw = prm
            prm     = sub_props(prm, file_nm, cCrt, rCrt)
            if prm_raw != prm:
                prms_l[ind] = prm
#               prms_l[ind] = shlex.quote(prm)
           #for ind, prm
        ddir        = sub_props(ddir, file_nm, cCrt, rCrt)

        pass;                   LOG and log('ready prms_l={}',(prms_l))

        val4call  = [cmnd] + prms_l
        pass;                   LOG and log('val4call={}',(val4call))

        # Calling
        nmargs  = {'cwd':ddir} if ddir else {}
#       if 'Y'!=ext.get('capt', 'N'):
        if 'N'==ext.get('rslt', 'N'):
            # Without capture
            subprocess.Popen(val4call, **nmargs)
#           if ddir:
#               subprocess.Popen(val4call, cwd=ddir)
#           else:
#               subprocess.Popen(val4call)
            return
        
        # With capture
        pass;                  #LOG and log("'Y'==ext.get('shll', 'N')",'Y'==ext.get('shll', 'N'))
        nmargs['stdout']=subprocess.PIPE
        nmargs['stderr']=subprocess.STDOUT
        nmargs['shell'] =ext.get('shll', False)
        pass;                   LOG and log('?? Popen nmargs={}',nmargs)
        pipe    = subprocess.Popen(val4call, **nmargs)
#       if ddir:
#           pipe    = subprocess.Popen(val4call, cwd=ddir
#                               , stdout=subprocess.PIPE
#                               , stderr=subprocess.STDOUT
#                              #, universal_newlines = True
#                               , shell=ext.get('shll', False)
#                               )
#       else:
#           pipe    = subprocess.Popen(val4call
#                               , stdout=subprocess.PIPE
#                               , stderr=subprocess.STDOUT
#                              #, universal_newlines = True
#                               , shell=ext.get('shll', False)
#                               )
        if pipe is None:
            pass;               LOG and log('fail Popen',)
            app.msg_status('Fail call: {} {}'.format(cmnd, prms_s))
            return
        pass;                   LOG and log('ok Popen',)
        app.msg_status('Call: {} {}'.format(cmnd, prms_s))

        rslt    = ext.get('rslt', RSLT_TO_PANEL)
        rslt_txt= ''
        if False:pass
        elif rslt in ('OP', 'OPA'):
#       elif rslt in (RSLT_TO_PANEL, RSLT_TO_PANEL_AP):
            ed.cmd(cmds.cmd_ShowPanelOutput)
            ed.focus()
            app.app_log(app.LOG_SET_PANEL, app.LOG_PANEL_OUTPUT)
            if rslt==RSLT_TO_PANEL:
                app.app_log(app.LOG_CLEAR, '')
        elif rslt ==  'ND':
#       elif rslt ==  RSLT_TO_NEWDOC:
            app.file_open('')
            
        while True:
            out_ln = pipe.stdout.readline().decode(ext.get('encd', 'utf-8'))
            if 0==len(out_ln): break
            out_ln = out_ln.strip('\r\n')
            pass;              #LOG and log('out_ln={}',out_ln)
            if False:pass
            elif rslt in ('OP', 'OPA'):
#           elif rslt in (RSLT_TO_PANEL, RSLT_TO_PANEL_AP):
                app.app_log(app.LOG_ADD, out_ln)
            elif rslt ==  'ND':
#           elif rslt ==  RSLT_TO_NEWDOC:
                ed.set_text_line(-1, out_ln)
            elif rslt in ('CB', 'SEL'):
#           elif rslt in (RSLT_TO_CLIP
#                        ,RSLT_REPL_SEL):
                rslt_txt+= out_ln + '\n'
           #while True

        rslt_txt= rslt_txt.strip('\n')
        if False:pass
        elif rslt == 'CB':
#       elif rslt == RSLT_TO_CLIP:
            app.app_proc(app.PROC_SET_CLIP, rslt_txt)
        elif rslt == 'SEL':
#       elif rslt == RSLT_REPL_SEL:
            crts    = ed.get_carets()
            for (cCrt, rCrt, cEnd, rEnd) in crts.reverse():
                if -1!=cEnd:
                    (rCrt, cCrt), (rEnd, cEnd) = apx.minmax((rCrt, cCrt), (rEnd, cEnd))
                    ed.delete(cCrt, rCrt, cEnd, rEnd)
                ed.insert(cCrt, rCrt, rslt_txt)
       #def run
       
   #class Command

def sub_props(prm, file_nm, cCrt, rCrt):
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
         +[C1.join(['type=button'   ,POS_FMT(l=GAP, t=GAP+400+GAP,  r=GAP+500+GAP, b=0)
                  ,'cap=&Close'
                  ] # i=7
         )]
    ), 1)    # start focus

'''
ToDo
[ ][kv-kv][09dec15] Run test cmd
'''
