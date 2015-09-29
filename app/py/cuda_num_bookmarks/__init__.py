from cudatext import *

#bookmarks kinds: 241..249
INDEX_ADD=240
LINES_DEC=10

def doset(id):
    line = ed.get_carets()[0][1]
    items = ed.bookmark(BOOKMARK_GET_LIST, 0)
    if items is not None:
        for item in items:
            reset=False
            if item==line:
                reset=True
            if ed.bookmark(BOOKMARK_GET, item)==id+INDEX_ADD:
                reset=True
            if reset:
                ed.bookmark(BOOKMARK_CLEAR, item)
    ed.bookmark(BOOKMARK_SET, line, id+INDEX_ADD)
    msg_status('Set bookmark %d' % id)

def dogoto(id):
    items = ed.bookmark(BOOKMARK_GET_LIST, 0)
    if items is not None:
        for item in items:
            if ed.bookmark(BOOKMARK_GET, item)==id+INDEX_ADD:
                ed.set_caret(0, item, -1, -1)
                ed.set_top(item-LINES_DEC)
                msg_status('Jump to bookmark %d' % id)
                return
    msg_status('Bookmark %d is not set' % id)


class Command:
    def set1(self):
        doset(1)
    def set2(self):
        doset(2)
    def set3(self):
        doset(3)
    def set4(self):
        doset(4)
    def set5(self):
        doset(5)
    def set6(self):
        doset(6)
    def set7(self):
        doset(7)
    def set8(self):
        doset(8)
    def set9(self):
        doset(9)
        
    def goto1(self):
        dogoto(1)
    def goto2(self):
        dogoto(2)
    def goto3(self):
        dogoto(3)
    def goto4(self):
        dogoto(4)
    def goto5(self):
        dogoto(5)
    def goto6(self):
        dogoto(6)
    def goto7(self):
        dogoto(7)
    def goto8(self):
        dogoto(8)
    def goto9(self):
        dogoto(9)
