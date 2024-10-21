def str_to_numeric_key(s):
    nlen = len(s)
    i = 0
    r = []
    while True:
        j = i
        while (j<nlen) and not s[j].isdigit():
            j += 1
        ss = s[i:j]
        if r and not ss:
            break
        r.append(ss)
        k = j
        while (k<nlen) and s[k].isdigit():
            k += 1
        ss = s[j:k]
        if not ss:
            break
        r.append(int(ss))
        i = k
    return r

if __name__=='__main__':
    s='11d22d33.'
    print(str_to_numeric_tuple(s))
    s='abc'
    print(str_to_numeric_tuple(s))
    s='abc22'
    print(str_to_numeric_tuple(s))
    s='33'
    print(str_to_numeric_tuple(s))
    s=''
    print(str_to_numeric_tuple(s))
