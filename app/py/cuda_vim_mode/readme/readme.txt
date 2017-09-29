plugin for CudaText.
if activated (via command in Plugins), it activates Vim key bindings, initially in Vim command mode.
not all Vim key are supported.

a) in Vim insertion mode, all keys work like usual in CudaText, only Esc goes to command mode.
b) supported keys in Vim command mode:

  hjkl - caret movement
  w - go to next word (jumps not exactly like Vim, but like CudaText command "go to next word")
  b - go to previous word (same note)
  e - go to end of word
  a - enter Insertion mode after moving caret right
  i - enter Insertion mode at current pos
  x - delete char right (like Delete key)
  X - delete char left (like Backspace key)


author: Alexey (CudaText)
license: MIT
