#
# CSS utilities for SynWrite plugins
# Author: Alexey Torgashin (UVViewsoft.com)
# License: MIT
#

import re

re_brackets = re.compile(r'{.*?}', re.S)
re_class = re.compile(r'[\w\-]*\.[\w\-]+')
re_id = re.compile(r'[\w\-]*\#[\w\-]+')

re_link_tag = re.compile(r'<link \b [^>]*? \b type="text/css" [^>]*? >', 
                         re.S + re.I + re.X)
re_link_href = re.compile(r'\b href="(.+?)"', 
                          re.S + re.I + re.X)
                 
re_style_tag = re.compile(r'<style \b [^>]*? \b type="text/css" .*? </style>',
                          re.S + re.I + re.X)                 
re_style_href = re.compile(r'@import \s+ "(.+?)"',
                           re.S + re.I + re.X)                 


def html_find_style_content(text):
    m = re_style_tag.search(text)
    if m:
        return text[m.start():m.end()]
    return ''

def html_find_tagname(text, ncaret):
    npos = text.rfind('<', 0, ncaret)
    if npos<0:
        return ''
    npos2 = text.find(' ', npos)
    if npos2<0:
        return ''
    return text[npos+1:npos2]

def html_find_css_filenames(text):
    result = []
    
    # find all <link> tags
    names = re_link_tag.findall(text)
    for name in names:
        m = re_link_href.search(name)
        if m:
            result += [m.group(1)]
    if result:
        return result
    
    # find single <style> tag
    m = re_style_tag.search(text)
    if m:
        text2 = text[m.start():m.end()]
        m = re_style_href.search(text2)
        if m:
            return [m.group(1)]
    
    # nothing found    
    return []

def get_class_tagname(s):
    n = s.find('.')
    if n>=0:
        return s[:n]
    n = s.find('#')
    if n>=0:
        return s[:n]
    return ''

def strip_class(s):
    tagname = get_class_tagname(s)
    return s[len(tagname)+1:]
    
def is_class_with_tagname(s, tagname):
    _tagname = get_class_tagname(s)
    return _tagname=='' or _tagname==tagname 

def css_find_classes(text, tagname, mode_class):
    t = re_brackets.sub('', text)
    regex = re_class if mode_class else re_id
    res = regex.findall(t)
    items = [strip_class(item) for item in res if is_class_with_tagname(item, tagname)]
    items = sorted(list(set(items)))
    return items
   
#--------------------------------------------
# tests


test_htm = """
<html>
<HEAD>
<meta http-equiv="Content-Type" content="text/html; charset=windows-1251" />
<link rel="StyleSheet" href="style1.css" type="text/css" media="screen" />
<title>Dialog</title>
</HEAD>
<body>
"""
test_htm2 = """
<html>
<HEAD>
<meta http-equiv="Content-Type" content="text/html; charset=windows-1251" />
<link rel="StyleSheet" type="text/css" href="/www/path/style2.css" media="screen" />
<title>Dialog</title>
</HEAD>
<body>
"""

test_css = """
.nameCom1, .nameCom2, tag.nameA1 { 
.aaaa
.cccc
}
tag.nameA2, notname { .name55 .name66 }
tagA.nameAA, tagB.nameBB { .name55 .name66 }
"""

test_htm_ok = """
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=windows-1251" />
<LINK REL=StyleSheet HREF="style.css" TYPE="text/css" MEDIA=screen>
<title>FAQ</title>
</head>
<body>

<h1>Frequently Asked Questions</h1>
<hr>
"""

test_htm_many = """
<LINK REL=StyleSheet HREF="styleA.css" TYPE="text/css" MEDIA=screen>
<LINK REL=StyleSheet HREF="styleB.css" TYPE="text/css" MEDIA=screen>
<LINK REL=StyleSheet TYPE="text/css" HREF="styleC.css" MEDIA=screen>
"""

test_css_ok = """
h3{
    font-size: 16px;
}
.code{
  font-family: "Courier New", Courier, monospace;
  font-size: 14px;
}
.inlinecode{
  font-family: "Courier New", Courier, monospace;
}
"""

if __name__=='__main__':
    print('classes "tag":', css_find_classes(test_css, 'tag', True))
    print('classes "tagA":', css_find_classes(test_css, 'tagA', True))
    print('classes "tagNN":', css_find_classes(test_css, 'tagNN', True))
    print('classes none text:', css_find_classes('tttttt', 'tagNN', True))
    print('classes ok text:', css_find_classes(test_css_ok, 'a', True))

    print('css file1:', html_find_css_filenames(test_htm))
    print('css file2:', html_find_css_filenames(test_htm2))
    print('css file_ok:', html_find_css_filenames(test_htm_ok))
    print('css file_many:', html_find_css_filenames(test_htm_many))
    #test_real = open(r'D:\THelp\en\_t.html').read()
    #print('css file_real:', html_find_css_filenames(test_real))
    
    print('tagname:', html_find_tagname('  <tag href=wwwwwwwwwwwwwwwwwwwwwwwww', 20))
