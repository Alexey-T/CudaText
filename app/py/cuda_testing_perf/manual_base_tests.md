
choose a test corpus:

corpus1: create random text with random lines lenght 490 to 510 char, 1000K lines, 500mb, saved in temp dir
```python
filename="cuda_undo_test_rand_1M.txt";
total_lines=1000000;
deleted_lines=600000;
```

corpus2: create random text with random lines lenght 490 to 510 char, 300K lines, 150mb, saved in temp dir
```python
filename="cuda_undo_test_rand_300K.txt";
total_lines=300000;
deleted_lines=200000;
```

```python
import os, tempfile, random, time; fpath = os.path.join(tempfile.gettempdir(), filename); t1 = time.time(); open(fpath, "w").writelines(os.urandom(random.randint(245, 255)).hex() + "\n" for _ in range(total_lines)); print(f"saved to {fpath} in {time.time()-t1:.4f}s")
```

_______________________________________

### test1 replace_lines

write 500mb rand lines
write 150mb rand lines

- note about real consumed time:after replace_lines finishes in 3.0762s (for 1M lines) it takes 8s to show text and for cpu to return to 0%, and another 8s when i do the first click on text or first scroll, it eats 25% cpu for 8s while app hangs,so real total time is 19s
- to automate the time spent calculation of hang1 and hang2 we can use app_proc(PROC_IDLE, True) to calculate hang1 and ed.action(EDACTION_UPDATE,1) to calculate hang2 as used bellow

- met1: this method is more correct than met2 because it reproduce exactly the test i run manually in cuda console, because when i do it manualy i do: i first start a tab, then i click in console then i run the one line command, when i open the tab cuda had the time to idle, but in met2 i don t use PROC_IDLE so the hang1 and hang2 are both mixed and calculated in the first PROC_IDLE hang1, while met1 show them clearly in diferent time so i can calculate the timinig in better granularity, in MP functions i will use met1

```python
import os, tempfile, time; file_open(""); app_proc(PROC_IDLE, True); ed.set_prop(PROP_WRAP,1); fpath = os.path.join(tempfile.gettempdir(), filename); lines = open(fpath, "r").readlines();

app_proc(PROC_IDLE, True);
t1 = time.time(); ed.replace_lines(0, ed.get_line_count()-1, lines); t2 = time.time(); print(f"replace_lines: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s");
del lines;
```

300k
replace_lines: 0.9491s
Hang1: 2.3201s
Hang2: 0.0090s

1M
replace_lines: 3.0732s
Hang1: 7.8765s
Hang2: 0.0100s



- met2:

```python
import os, tempfile, time; file_open(""); ed.set_prop(PROP_WRAP,1); fpath = os.path.join(tempfile.gettempdir(), filename); lines = open(fpath, "r").readlines();
 
t1 = time.time(); ed.replace_lines(0, ed.get_line_count()-1, lines); t2 = time.time(); print(f"replace_lines: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s");
del lines;
```

300k
replace_lines: 0.8871s
Hang1: 2.4051s
Hang2: 0.0080s

1M
replace_lines: 3.0272s
Hang1: 7.9685s
Hang2: 0.0140s

_______________________________________

### test2 set_text_all

write 500mb rand lines
write 150mb rand lines

```python
import os, tempfile, time; file_open(""); app_proc(PROC_IDLE, True); ed.set_prop(PROP_WRAP,1); fpath = os.path.join(tempfile.gettempdir(), filename); text = open(fpath, "r").read();

app_proc(PROC_IDLE, True);
t1 = time.time(); ed.set_text_all(text); t2 = time.time(); print(f"set_text_all: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s");
del text
```

300k
set_text_all: 3.1442s
Hang1: 2.3361s
Hang2: 0.0180s

1M
set_text_all: 9.4745s
Hang1: 8.1685s
Hang2: 0.0090s


_______________________________________

### test3 undo/redo and delete (all lines)

write 500mb rand lines, select all, delete it, then undo
write 150mb rand lines, select all, delete it, then undo

```python
import os, tempfile, time, cudatext_cmd as c; file_open(""); app_proc(PROC_IDLE, True); ed.set_prop(PROP_WRAP,1); fpath = os.path.join(tempfile.gettempdir(), filename); 
lines = open(fpath, "r").readlines(); ed.replace_lines(0, ed.get_line_count()-1, lines); del lines; ed.set_caret(0, ed.get_line_count(), 0, 0); 

app_proc(PROC_IDLE, True);
t1 = time.time(); ed.cmd(c.cCommand_TextDeleteSelection); t2 = time.time(); print(f"Delete: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s");

app_proc(PROC_IDLE, True);
t1 = time.time(); ed.cmd(c.cCommand_Undo); t2 = time.time(); print(f"Undo: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s");

app_proc(PROC_IDLE, True);
t1 = time.time(); ed.cmd(c.cCommand_Redo); t2 = time.time(); print(f"Redo: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s"); 
```

300k
Delete: 0.8140s
Hang1: 0.1080s
Hang2: 0.0110s
Undo: 2.6962s
Hang1: 2.2021s
Hang2: 0.0210s
Redo: 0.8801s
Hang1: 0.1050s
Hang2: 0.0080s

1M
Delete: 2.8732s
Hang1: 0.4750s
Hang2: 0.0100s
Undo: 9.3815s
Hang1: 7.7334s
Hang2: 0.0180s
Redo: 2.9832s
Hang1: 0.1680s
Hang2: 0.0050s

_______________________________________

### test4 undo/redo and delete (partial lines)

write 500mb (1M) rand line, select 600k line, delete it, then undo
write 150mb (300k) rand line, select 200k line, delete it, then undo

```python
import os, tempfile, time, cudatext_cmd as c; file_open(""); app_proc(PROC_IDLE, True); ed.set_prop(PROP_WRAP,1); fpath = os.path.join(tempfile.gettempdir(), filename);
lines = open(fpath, "r").readlines(); ed.replace_lines(0, ed.get_line_count()-1, lines); del lines; ed.set_caret(0, deleted_lines, 0, 0);

app_proc(PROC_IDLE, True);
t1 = time.time(); ed.cmd(c.cCommand_TextDeleteSelection); t2 = time.time(); print(f"Delete: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s");

app_proc(PROC_IDLE, True);
t1 = time.time(); ed.cmd(c.cCommand_Undo); t2 = time.time(); print(f"Undo: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s");

app_proc(PROC_IDLE, True);
t1 = time.time(); ed.cmd(c.cCommand_Redo); t2 = time.time(); print(f"Redo: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s"); 
```

300k
Delete: 0.6060s
Hang1: 0.0800s
Hang2: 0.0240s
Undo: 0.5390s
Hang1: 0.0860s
Hang2: 0.0210s
Redo: 0.6260s
Hang1: 0.0920s
Hang2: 0.0110s

1M
Delete: 1.7321s
Hang1: 3.0562s
Hang2: 0.0200s
Undo: 5.5503s
Hang1: 7.7214s
Hang2: 0.0080s
Redo: 1.6831s
Hang1: 3.1082s
Hang2: 0.0170s

_______________________________________

### test5 file_open

open 500mb file
set wrap_enabled_max_lines to 1100000 in user.json manually
set wrap_mode to 1 in user.json manually

```python
import os, cudatext as app, cudatext_cmd as cmds, cudax_lib;
cudax_lib.set_opt('wrap_enabled_max_lines', 1100000); cudax_lib.set_opt('wrap_mode', 1); file_open(os.path.join(app_path(APP_DIR_SETTINGS), 'user.json')) and (ed.cmd(cmds.cmd_FileSave), ed.cmd(cmds.cmd_FileClose));
import tempfile, time; fpath = os.path.join(tempfile.gettempdir(), filename); t1 = time.time(); file_open(fpath); t2 = time.time(); print(f"file_open: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s"); 
```

300k
file_open: 2.9142s
Hang1: 0.0430s
Hang2: 0.0190s

1M
file_open: 8.8475s
Hang1: 0.0550s
Hang2: 0.0190s


____________________________________________

**CudaText performance results**

All times in seconds.  
**Total** = Command time + Hang1 + Hang2 (the real perceived cost).

### Corpus 1 – 300K lines (~150 MB)

| Test                 | Command       | Time   | Hang1  | Hang2  | **Total**  |
| -------------------- | ------------- | ------ | ------ | ------ | ---------- |
| **1. replace_lines** | replace_lines | 0.9491 | 2.3201 | 0.0090 | **3.2782** |
| **2. set_text_all**  | set_text_all  | 3.1442 | 2.3361 | 0.0180 | **5.4983** |
| **3. undo all**      | Delete        | 0.8140 | 0.1080 | 0.0110 | **0.9330** |
|                      | Undo          | 2.6962 | 2.2021 | 0.0210 | **4.9193** |
|                      | Redo          | 0.8801 | 0.1050 | 0.0080 | **0.9931** |
| **4. undo partial**  | Delete        | 0.6060 | 0.0800 | 0.0240 | **0.7100** |
|                      | Undo          | 0.5390 | 0.0860 | 0.0210 | **0.6460** |
|                      | Redo          | 0.6260 | 0.0920 | 0.0110 | **0.7290** |
| **5. file_open**     | file_open     | 2.9142 | 0.0430 | 0.0190 | **2.9762** |

### Corpus 2 – 1M lines (~500 MB)

| Test                 | Command       | Time   | Hang1  | Hang2  | **Total**   |
| -------------------- | ------------- | ------ | ------ | ------ | ----------- |
| **1. replace_lines** | replace_lines | 3.0732 | 7.8765 | 0.0100 | **10.9597** |
| **2. set_text_all**  | set_text_all  | 9.4745 | 8.1685 | 0.0090 | **17.6520** |
| **3. undo all**      | Delete        | 2.8732 | 0.4750 | 0.0100 | **3.3582**  |
|                      | Undo          | 9.3815 | 7.7334 | 0.0180 | **17.1329** |
|                      | Redo          | 2.9832 | 0.1680 | 0.0050 | **3.1562**  |
| **4. undo partial**  | Delete        | 1.7321 | 3.0562 | 0.0200 | **4.8083**  |
|                      | Undo          | 5.5503 | 7.7214 | 0.0080 | **13.2797** |
|                      | Redo          | 1.6831 | 3.1082 | 0.0170 | **4.8083**  |
| **5. file_open**     | file_open     | 8.8475 | 0.0550 | 0.0190 | **8.9215**  |

### Quick observations
- **replace_lines** is much faster than **set_text_all** on both sizes.
- The expensive part is almost always the **Hang1** after the main command (especially on 1M lines).
- **file_open** has almost no hang.
- Partial undo (test4) is cheaper than full undo (test3), as expected.

___
manual benchmark done on an Intel Core i7 CPU M 640 @ 2.80GHz (2026-09-11).
baselines were measured with wrap enabled.
you will have to revert the original wrap_enabled_max_lines and wrap_mode manually in user.json
