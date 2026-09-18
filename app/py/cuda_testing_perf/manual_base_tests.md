

manual benchmark done on an Intel Core i7 CPU M 640 @ 2.80GHz (2026-09-11).
baselines were measured with wrap enabled.

_______________________________________

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
import os, tempfile, random, time; fpath = os.path.join(tempfile.gettempdir(), filename); t1 = time.time(); open(fpath, "w").writelines(os.urandom(random.randint(245, 255)).hex() + "\n" for _ in range(total_lines)); print(f"saved to {fpath} in {time.time()-t1:.4f}s");
```

_______________________________________
### MP1: replace_lines with scrollbar_themed=False
write 500mb rand lines
expected results: MP1 and MP2 and MP3 must consume the same time

test an old bug, now it s fixed: in the past when scrollbar_themed is false the Hang2 start happening and consumes 7s, this was happenening with replace_lines and set_text_all 

- note about real consumed time:after replace_lines finishes in 3.0762s (for 1M lines) it takes 8s to show text and for cpu to return to 0%, and another 8s when i do the first click on text or first scroll, it eats 25% cpu for 8s while app hangs,so real total time is 19s
- to automate the time spent calculation of hang1 and hang2 we can use app_proc(PROC_IDLE, True) to calculate hang1 and ed.action(EDACTION_UPDATE,1) to calculate hang2 as used bellow

- PROC_IDLE before replace_lines is important:
met1: app_proc(PROC_IDLE, True); ed.replace_lines(0, ed.get_line_count()-1, lines);
met2: ed.replace_lines(0, ed.get_line_count()-1, lines);
met1 is more correct than met2 because it reproduce exactly the test i run manually in cuda console, because when i do it manualy i do: i first start a tab, then i click in console then i run the one line command, when i open the tab cuda had the time to idle, but in met2 i don t use PROC_IDLE so the hang1 and hang2 are both mixed and calculated in the first PROC_IDLE hang1, while met1 show them clearly in diferent time so i can calculate the timinig in better granularity, in MP functions i use met1 too


```python
import os, cudatext_cmd as cmds;
app_proc(PROC_CONFIG_READ, '{"scrollbar_themed": false, "wrap_enabled_max_lines": 1100000, "wrap_mode": 1}');

import os, tempfile, time; file_open(""); app_proc(PROC_IDLE, True); ed.set_prop(PROP_WRAP,1); fpath = os.path.join(tempfile.gettempdir(), filename); lines = open(fpath, "r").readlines();

app_proc(PROC_IDLE, True); ed.action(EDACTION_UPDATE,1);
t1 = time.time(); ed.replace_lines(0, ed.get_line_count()-1, lines); t2 = time.time(); print(f"replace_lines: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s");
del lines;
```

replace_lines: 2.9202s
Hang1: 0.1220s
Hang2: 0.0330s

_______________________________________
### MP2: replace_lines with scrollbar_themed=True
write 500mb rand lines
expected results: MP1 and MP2 and MP3 must consume the same time

```python
import os, cudatext_cmd as cmds;
app_proc(PROC_CONFIG_READ, '{"scrollbar_themed": true, "wrap_enabled_max_lines": 1100000, "wrap_mode": 1}');

import os, tempfile, time; file_open(""); app_proc(PROC_IDLE, True); ed.set_prop(PROP_WRAP,1); fpath = os.path.join(tempfile.gettempdir(), filename); lines = open(fpath, "r").readlines();

app_proc(PROC_IDLE, True); ed.action(EDACTION_UPDATE,1);
t1 = time.time(); ed.replace_lines(0, ed.get_line_count()-1, lines); t2 = time.time(); print(f"replace_lines: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s");
del lines;
```

replace_lines: 2.9182s
Hang1: 0.1230s
Hang2: 0.0180s

_______________________________________
### MP3: set_text_all
write 500mb rand lines
expected results: MP1 and MP2 and MP3 must consume the same time

```python
import os, cudatext_cmd as cmds;
app_proc(PROC_CONFIG_READ, '{"scrollbar_themed": true, "wrap_enabled_max_lines": 1100000, "wrap_mode": 1}');

import os, tempfile, time; file_open(""); app_proc(PROC_IDLE, True); ed.set_prop(PROP_WRAP,1); fpath = os.path.join(tempfile.gettempdir(), filename); text = open(fpath, "r").read();

app_proc(PROC_IDLE, True); ed.action(EDACTION_UPDATE,1);
t1 = time.time(); ed.set_text_all(text); t2 = time.time(); print(f"set_text_all: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s");
del text
```

set_text_all: 3.0872s
Hang1: 0.1940s
Hang2: 0.0410s

_______________________________________
### MP4: delete (all lines) then undo/redo
write 500mb rand lines, select all, delete it, then undo

```python
import os, cudatext_cmd as cmds;
app_proc(PROC_CONFIG_READ, '{"scrollbar_themed": true, "wrap_enabled_max_lines": 1100000, "wrap_mode": 1}');

import os, tempfile, time, cudatext_cmd as c; file_open(""); app_proc(PROC_IDLE, True); ed.set_prop(PROP_WRAP,1); fpath = os.path.join(tempfile.gettempdir(), filename); 
lines = open(fpath, "r").readlines(); ed.replace_lines(0, ed.get_line_count()-1, lines); del lines; ed.set_caret(0, ed.get_line_count(), 0, 0); 

app_proc(PROC_IDLE, True); ed.action(EDACTION_UPDATE,1);
t1 = time.time(); ed.cmd(c.cCommand_TextDeleteSelection); t2 = time.time(); print(f"Delete: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s");

app_proc(PROC_IDLE, True); ed.action(EDACTION_UPDATE,1);
t1 = time.time(); ed.cmd(c.cCommand_Undo); t2 = time.time(); print(f"Undo: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s");

app_proc(PROC_IDLE, True); ed.action(EDACTION_UPDATE,1);
t1 = time.time(); ed.cmd(c.cCommand_Redo); t2 = time.time(); print(f"Redo: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s"); 
```

Delete: 1.8931s
Hang1: 0.3380s
Hang2: 0.0170s
Undo: 3.1482s
Hang1: 0.0750s
Hang2: 0.0170s
Redo: 2.1631s
Hang1: 0.0470s
Hang2: 0.0090s

_______________________________________
### MP5: delete (partial lines) then undo/redo
write 500mb (1M) rand line, select 600k line, delete it, then undo

```python
import os, cudatext_cmd as cmds;
app_proc(PROC_CONFIG_READ, '{"scrollbar_themed": true, "wrap_enabled_max_lines": 1100000, "wrap_mode": 1}');

import os, tempfile, time, cudatext_cmd as c; file_open(""); app_proc(PROC_IDLE, True); ed.set_prop(PROP_WRAP,1); fpath = os.path.join(tempfile.gettempdir(), filename);
lines = open(fpath, "r").readlines(); ed.replace_lines(0, ed.get_line_count()-1, lines); del lines; ed.set_caret(0, deleted_lines, 0, 0);

app_proc(PROC_IDLE, True); ed.action(EDACTION_UPDATE,1);
t1 = time.time(); ed.cmd(c.cCommand_TextDeleteSelection); t2 = time.time(); print(f"Delete: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s");

app_proc(PROC_IDLE, True); ed.action(EDACTION_UPDATE,1);
t1 = time.time(); ed.cmd(c.cCommand_Undo); t2 = time.time(); print(f"Undo: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s");

app_proc(PROC_IDLE, True); ed.action(EDACTION_UPDATE,1);
t1 = time.time(); ed.cmd(c.cCommand_Redo); t2 = time.time(); print(f"Redo: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s"); 
```

Delete: 1.6681s
Hang1: 0.3300s
Hang2: 0.0340s
Undo: 2.3641s
Hang1: 0.1020s
Hang2: 0.0410s
Redo: 1.8261s
Hang1: 0.1090s
Hang2: 0.0390s

_______________________________________
### MP6: undo / redo (fair compare)
test real undo redo of big text where undo and redo both move big text for a fair compare between both
this test performs a second replace_lines that inserts marker lines ("111\n" at the start and "222\n" at the end) so the subsequent undo/redo moves a large amount of text.
expected results: undo and redo must consume the same time

```python
import os, cudatext_cmd as cmds;
app_proc(PROC_CONFIG_READ, '{"scrollbar_themed": true, "wrap_enabled_max_lines": 1100000, "wrap_mode": 1}');

import os, tempfile, time, cudatext_cmd as c; file_open(""); app_proc(PROC_IDLE, True); ed.set_prop(PROP_WRAP,1); fpath = os.path.join(tempfile.gettempdir(), filename);
lines = open(fpath, "r").readlines();
ed.replace_lines(0, ed.get_line_count()-1, lines);
ed.replace_lines(0, ed.get_line_count()-1, ["111\n"] + lines + ["222\n"]);
del lines;

app_proc(PROC_IDLE, True); ed.action(EDACTION_UPDATE,1);
t1 = time.time(); ed.cmd(c.cCommand_Undo); t2 = time.time(); print(f"Undo: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s");

app_proc(PROC_IDLE, True); ed.action(EDACTION_UPDATE,1);
t1 = time.time(); ed.cmd(c.cCommand_Redo); t2 = time.time(); print(f"Redo: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s"); 

```

Undo: 7.4064s
Hang1: 0.4510s
Hang2: 0.0230s
Redo: 7.4594s
Hang1: 0.1490s
Hang2: 0.0180s

____________________________________________
### MP7: file_open with scrollbar_themed=False
test an old bug, now it s fixed: when scrollbar_themed is false file_open time doubles 
expected results: MP7 and MP8 must consume the same time

```python
import os, cudatext_cmd as cmds;
app_proc(PROC_CONFIG_READ, '{"scrollbar_themed": false, "wrap_enabled_max_lines": 1100000, "wrap_mode": 1}');

app_proc(PROC_IDLE, True); ed.action(EDACTION_UPDATE,1);
import tempfile, time; fpath = os.path.join(tempfile.gettempdir(), filename); t1 = time.time(); file_open(fpath); t2 = time.time(); print(f"file_open: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s"); 
```

file_open: 2.5111s
Hang1: 0.0030s
Hang2: 0.0160s

____________________________________________
### MP8: file_open with scrollbar_themed=True
expected results: MP7 and MP8 must consume the same time

```python
import os, cudatext_cmd as cmds;
app_proc(PROC_CONFIG_READ, '{"scrollbar_themed": true, "wrap_enabled_max_lines": 1100000, "wrap_mode": 1}');

app_proc(PROC_IDLE, True); ed.action(EDACTION_UPDATE,1);
import tempfile, time; fpath = os.path.join(tempfile.gettempdir(), filename); t1 = time.time(); file_open(fpath); t2 = time.time(); print(f"file_open: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s"); 
```

file_open: 2.5391s
Hang1: 0.0030s
Hang2: 0.0180s

____________________________________________
### MP9: undo text with color markers
write 500mb rand lines 1M lines, mark 500k line, replace all with v, undo 

```python
import os, cudatext_cmd as cmds;
app_proc(PROC_CONFIG_READ, '{"scrollbar_themed": true, "wrap_enabled_max_lines": 1100000, "wrap_mode": 1}');

import os, tempfile, time; file_open(""); app_proc(PROC_IDLE, True); ed.set_prop(PROP_WRAP,1); fpath = os.path.join(tempfile.gettempdir(), filename); text = open(fpath, "r").read();

ed.set_text_all(text);
[ed.attr(MARKERS_ADD,tag=11,x=0,y=i,len=480,color_bg=0x00FF00)for i in range(0,total_lines,2)];
ed.replace_lines(0, ed.get_line_count()-1, ["v"]);

app_proc(PROC_IDLE, True); ed.action(EDACTION_UPDATE,1);
t1 = time.time(); ed.cmd(cmds.cCommand_Undo); t2 = time.time(); print(f"Undo: {t2-t1:.4f}s");
t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s");

del text
```

Undo: 3.7122s
Hang1: 0.0740s
Hang2: 0.0280s


