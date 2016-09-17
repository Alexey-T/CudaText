"""doc
string
end"""
import os

fn_tool = os.path.join(os.path.dirname(__file__), 'app.exe')

def get_exec(fn, code):
    try:
        subprocess.check_call([fn, str(code)])
        if false:
            test
            if false:
                test
        return 0
    except subprocess.CalledProcessError as e:
        return e.returncode
    
        
class Command:
    def run(self):
        nstart, nlen, text = do_get_word_caret(color_chars)
        try:
            code = HTMLColorToPILColor(text)
        except:
            code = 0
        
        code = get_exec(fn_tool, code)
        if code>=0 and code!=0xFFFFFFFF:
            text = PILColorToHTMLColor(code)
           
            msg_status('Inserted color code: '+text)
