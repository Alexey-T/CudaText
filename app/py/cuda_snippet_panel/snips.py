
def parse_usual_snip(s):

    if '=' in s:
        name, text = s.split('=', maxsplit=1)
    else:
        name, text = s, s
    return {'kind': 'line', 'name': name, 'text': text}

