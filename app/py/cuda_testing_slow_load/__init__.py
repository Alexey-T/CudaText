import os, tempfile, time, random
from cudatext import *
import cudatext_cmd as cmds
import cudax_lib

total_lines=1000000;
filename="cuda_undo_test_rand_1M.txt";

def make_file():
    fpath = os.path.join(tempfile.gettempdir(), filename);
    if not os.path.isfile(fpath):
        print('Making temp 500MB file...')
        t1 = time.time();
        open(fpath, "w").writelines(os.urandom(random.randint(245, 255)).hex() + "\n" for _ in range(total_lines));
        print(f"saved to {fpath} in {time.time()-t1:.4f}s")
    

class Command:

    def slow_sc_false(self):
        make_file()
        print('Test scrollbar_themed:false')
        cudax_lib.set_opt('wrap_enabled_max_lines', 1100000);
        cudax_lib.set_opt('wrap_mode', 1);
        ed.cmd(cmds.cmd_OpsReloadAndApply)

        file_open("");
        app_proc(PROC_IDLE, True);
        ed.set_prop(PROP_WRAP, True);
        ed.set_prop(PROP_MODERN_SCROLLBAR, False)
        fpath = os.path.join(tempfile.gettempdir(), filename);
        lines = open(fpath, "r").readlines();

        app_proc(PROC_IDLE, True);
        t1 = time.time(); ed.replace_lines(0, ed.get_line_count()-1, lines); t2 = time.time(); print(f"replace_lines: {t2-t1:.4f}s");
        t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
        t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s");
        del lines;

    def slow_sc_true(self):
        make_file()
        print('Test scrollbar_themed:true')
        cudax_lib.set_opt('wrap_enabled_max_lines', 1100000);
        cudax_lib.set_opt('wrap_mode', 1);
        ed.cmd(cmds.cmd_OpsReloadAndApply)

        file_open("");
        app_proc(PROC_IDLE, True);
        ed.set_prop(PROP_WRAP, True);
        ed.set_prop(PROP_MODERN_SCROLLBAR, True)
        fpath = os.path.join(tempfile.gettempdir(), filename);
        lines = open(fpath, "r").readlines();

        app_proc(PROC_IDLE, True);
        t1 = time.time(); ed.replace_lines(0, ed.get_line_count()-1, lines); t2 = time.time(); print(f"replace_lines: {t2-t1:.4f}s");
        t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
        t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s");
        del lines;
