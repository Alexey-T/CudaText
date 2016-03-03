def string_to_color(s):
    """
    #RRGGBB or #RGB to integer
    """
    s = s.strip()
    while s[0] == '#': s = s[1:]
    if len(s)==3:
        s = s[0]*2 + s[1]*2 + s[2]*2
    if len(s)!=6:
        raise Exception('Incorrect color token: '+s)
    s = s[-2:] + s[2:4] + s[:2]
    color = int(s, 16)
    return color
