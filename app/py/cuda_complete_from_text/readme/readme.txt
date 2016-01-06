Plugin for CudaText.
handles auto-complete command (Ctrl+Space). gives completion listbox with list 
of words from current file, which start with the current word (before caret). 
eg, if you typed "wr", it may give "writeln", "write" etc.

plugin has options: 
- minimal word len
- lexers list (for which to work)
- case-sensitive

to edit options, open plugin's source (by Addon Manager) and options are 
at the top of __init__.py.


Author: Alexey (Cudatext)
