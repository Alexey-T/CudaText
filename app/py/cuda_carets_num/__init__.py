from cudatext import *

class Command:
    def run(self):
        carets = ed.get_carets()
        if len(carets)<2: 
            msg_box('Place several carets first', MB_OK)
            return

        s = dlg_input_ex(4, 'Carets Numbering', 
          'Starting number:', '1', 
          'Digits:', '1',
          'Text before numbers:', '',
          'Text after numbers:', '')
        if not s: return
        try:
            n_start = int(s[0])
        except:
            msg_box('Incorrect number entered: '+s[0], MB_OK)
            return
        try:
            n_len = int(s[1])
        except:
            msg_box('Incorrect number entered: '+s[1], MB_OK)
            return
            
        text_before = s[2]
        text_after = s[3]
        
        carets = list(reversed(carets))
        
        ed.lock()
        for i, caret in enumerate(carets):
            num = (len(carets)-i+n_start-1)
            text = text_before + ('%0'+str(n_len)+'d') % num + text_after
            ed.insert(caret[0], caret[1], text)
        ed.unlock()
            
        msg_status('Inserted %d numbers' % len(carets))
        