import os

def enum_dir(dir):

    if not os.path.isdir(dir):
        return []    
    l = sorted(os.listdir(dir))
    l = [os.path.join(dir, i) for i in l]
    return l

'''
https://stackoverflow.com/questions/436220/determine-the-encoding-of-text-in-python
'''

def bom_type(file):
    """
    returns file encoding string for open() function
    example:
        bom = bom_type(f)
        open(f, encoding=bom, errors='ignore')
    """

    f = open(file, 'rb')
    b = f.read(4)
    f.close()

    if (b[0:3] == b'\xef\xbb\xbf'):
        return "utf8"

    # Python automatically detects endianness if utf-16 bom is present
    # write endianness generally determined by endianness of CPU
    if (b[0:2] == b'\xfe\xff') or \
       (b[0:2] == b'\xff\xfe'):
        return "utf16"

    if (b[0:4] == b'\xfe\xff\x00\x00') or \
       (b[0:4] == b'\x00\x00\xff\xfe'):
        return "utf32"

    # If BOM is not provided, then assume its the codepage
    #     used by your operating system
    return "utf8"
    # For the United States its: cp1252


def open_read(f):

    bom = bom_type(f)
    return open(f, 'r', encoding=bom, errors='ignore').read()
