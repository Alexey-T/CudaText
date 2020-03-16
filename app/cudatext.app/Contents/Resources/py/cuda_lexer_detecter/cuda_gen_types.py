import os
import re
from zipfile import ZipFile

def log(msg='', *args, **kwargs):
    if args or kwargs:
        msg = msg.format(*args, **kwargs)
    print(msg)

def cuda_gen_types(src_dir, trg_file):
    types   = {}
    
    zip_fns = [fn for fn in os.listdir(src_dir)
                  if  fn.endswith('.zip')]
    pass;                       log("zip file count: {}",len(zip_fns))
    for zip_fn in zip_fns:
        with ZipFile(src_dir+os.sep+zip_fn) as zip_f:
            lcf_fns = [fn for fn in zip_f.namelist() 
                          if  fn.endswith('.lcf')]
            pass;              #log("lcf_fns={}",(lcf_fns))
            pass;              #break#for fn
            for lcf_fn in lcf_fns:
                pass;          #log("lcf_fn={}",(lcf_fn))
                lex = lcf_fn[:-4]
                with zip_f.open(lcf_fn) as lcf_f:
                    body    = str(lcf_f.read())
                    body    = body.replace(r"\'", "'").replace(r'\n', '\n').replace(r'\r', '\r')
                    body    = re.sub(r'\s*=\s*', '=', body)
                    body    = re.sub(r"'\s*\+\s*'", '', body)
                    mt      = re.search(r"Extentions='([^']+)'", body)
                    ext_s   = mt.group(1) if mt else ''
                    pass;      #log('ext_s={}',(ext_s))
                    
                    exts    = ext_s.split(' ')
                    for ext in exts:
                        ext_list    = types.setdefault(ext, [])
                        ext_list   += [lex]
                   #with zip_f
               #for lcf_fn
           #with ZipFile
       #for zip_fn
    pass;                      #log("types={}",(types))
    
    with open(trg_file, 'w') as trg_f:
        trg_f.write('TYPES = {')
        srt_keys    = sorted(types.keys())
        for key in srt_keys:
            trg_f.write('\n    "{}": {},'.format(key, types[key]))
        trg_f.write('\n}')
   #def cuda_gen_types

if __name__ == '__main__' :
    pass;                       log('Start')
    src_dir = os.getcwd()
    trg_file= src_dir+os.sep+'lexertypes.py'
    cuda_gen_types(src_dir, trg_file)
    pass;                       log('Finish')
