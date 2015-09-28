#params: byte, not str

def uni_decode(data):
    return data.decode('unicode-escape').encode('utf8')

def uni_encode(data):
    return data.decode('utf8').encode('unicode-escape')
