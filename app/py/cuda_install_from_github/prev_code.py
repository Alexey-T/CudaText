import zipfile
import shutil


def do_unzip(zip_fn, to_folder):
    zip_ref = zipfile.ZipFile(zip_fn, 'r')
    zip_ref.extractall(to_folder)
    zip_ref.close()


def do_copy_dir(src, dest):
    try:
        shutil.copytree(src, dest)
    # Directories are the same
    except shutil.Error as e:
        print('Directory not copied. Error: %s' % e)
    # Any error saying that the directory doesn't exist
    except OSError as e:
        print('Directory not copied. Error: %s' % e)


def do_install_from_dir(dir_from):        
    fn_inf = os.path.join(dir_from, 'install.inf')
    if not os.path.isfile(fn_inf):
        msg_box('Cannot find install.inf in zip', MB_OK+MB_ICONERROR)
        return
        
    subdir = ini_read(fn_inf, 'info', 'subdir', '')
    typ = ini_read(fn_inf, 'info', 'type', '')
    if not subdir:
        msg_box('Cannot find subdir-value in install.inf', MB_OK+MB_ICONERROR)
        return
    if typ!='cudatext-plugin':
        msg_box('Cannot install this addon type: '+typ, MB_OK+MB_ICONERROR)
        return
        
    dir_to = os.path.join(app_path(APP_DIR_PY), subdir)
    print('Installing to: '+dir_to)
        
    dir_trash = os.path.join(app_path(APP_DIR_PY), '__trash')
    if not os.path.isdir(dir_trash):
        os.mkdir(dir_trash)
    if os.path.isdir(dir_to):
        dir_del = os.path.join(dir_trash, subdir)
        while True:
            if not os.path.isdir(dir_del):
                os.rename(dir_to, dir_del)
                break
            dir_del += '_'
        
    do_copy_dir(dir_from, dir_to)
    ed.cmd(cudatext_cmd.cmd_RescanPythonPluginsInfFiles)
    msg_box('Plugin installed to:\n'+dir_to, MB_OK+MB_ICONINFO)
    
