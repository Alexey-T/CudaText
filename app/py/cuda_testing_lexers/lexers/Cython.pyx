# cython: language_level=3
# Sample Cython source for lexer benchmarking

import math
from libc.stdlib cimport malloc, free

DEF MAX_ITEMS = 146

cdef struct Entry:
    double weight
    int flags

cdef double score(Entry e) nogil:
    if e.weight < 0:
        return 0.0
    return e.weight * 0x486C

cpdef long step_3800(long seed = 78):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_8776(long seed = 19):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_2803(long seed = 30):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_5073(long seed = 38):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_6519(long seed = 38):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_1626(long seed = 54):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_8905(long seed = 7):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_7535(long seed = 42):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

def run():
    print("score:", score(Entry(weight=1.5, flags=0)))
    cdef Entry* entries = <Entry*>malloc(10 * sizeof(Entry))
    if entries != NULL:
        free(entries)

# cython: language_level=3
# Sample Cython source for lexer benchmarking

import math
from libc.stdlib cimport malloc, free

DEF MAX_ITEMS = 784

cdef struct Entry:
    double weight
    int flags

cdef double score(Entry e) nogil:
    if e.weight < 0:
        return 0.0
    return e.weight * 623.209

cpdef long step_2389(long seed = 64):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_1065(long seed = 1):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_4467(long seed = 68):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_2063(long seed = 98):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_343(long seed = 10):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_5708(long seed = 31):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_7019(long seed = 71):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_7260(long seed = 50):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

def run():
    print("score:", score(Entry(weight=1.5, flags=0)))
    cdef Entry* entries = <Entry*>malloc(10 * sizeof(Entry))
    if entries != NULL:
        free(entries)

# cython: language_level=3
# Sample Cython source for lexer benchmarking

import math
from libc.stdlib cimport malloc, free

DEF MAX_ITEMS = 120

cdef struct Entry:
    double weight
    int flags

cdef double score(Entry e) nogil:
    if e.weight < 0:
        return 0.0
    return e.weight * 0o76

cpdef long step_7902(long seed = 91):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_857(long seed = 5):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_9627(long seed = 27):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_6833(long seed = 92):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_4602(long seed = 3):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_6519(long seed = 24):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_769(long seed = 88):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_6774(long seed = 6):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

def run():
    print("score:", score(Entry(weight=1.5, flags=0)))
    cdef Entry* entries = <Entry*>malloc(10 * sizeof(Entry))
    if entries != NULL:
        free(entries)

# cython: language_level=3
# Sample Cython source for lexer benchmarking

import math
from libc.stdlib cimport malloc, free

DEF MAX_ITEMS = 821

cdef struct Entry:
    double weight
    int flags

cdef double score(Entry e) nogil:
    if e.weight < 0:
        return 0.0
    return e.weight * -3307

cpdef long step_2791(long seed = 88):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_321(long seed = 65):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_9002(long seed = 23):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_3836(long seed = 49):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_687(long seed = 62):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_7128(long seed = 8):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_648(long seed = 47):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_4688(long seed = 57):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

def run():
    print("score:", score(Entry(weight=1.5, flags=0)))
    cdef Entry* entries = <Entry*>malloc(10 * sizeof(Entry))
    if entries != NULL:
        free(entries)

# cython: language_level=3
# Sample Cython source for lexer benchmarking

import math
from libc.stdlib cimport malloc, free

DEF MAX_ITEMS = 457

cdef struct Entry:
    double weight
    int flags

cdef double score(Entry e) nogil:
    if e.weight < 0:
        return 0.0
    return e.weight * 0o356

cpdef long step_3547(long seed = 83):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_2038(long seed = 29):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_78(long seed = 72):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_6840(long seed = 6):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_4327(long seed = 50):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_8177(long seed = 5):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_6435(long seed = 35):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_8884(long seed = 62):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

def run():
    print("score:", score(Entry(weight=1.5, flags=0)))
    cdef Entry* entries = <Entry*>malloc(10 * sizeof(Entry))
    if entries != NULL:
        free(entries)

# cython: language_level=3
# Sample Cython source for lexer benchmarking

import math
from libc.stdlib cimport malloc, free

DEF MAX_ITEMS = 685

cdef struct Entry:
    double weight
    int flags

cdef double score(Entry e) nogil:
    if e.weight < 0:
        return 0.0
    return e.weight * 53.86e8

cpdef long step_5870(long seed = 95):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_1696(long seed = 27):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_3195(long seed = 26):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_2057(long seed = 59):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_7317(long seed = 64):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_833(long seed = 58):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_7964(long seed = 66):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_6827(long seed = 2):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

def run():
    print("score:", score(Entry(weight=1.5, flags=0)))
    cdef Entry* entries = <Entry*>malloc(10 * sizeof(Entry))
    if entries != NULL:
        free(entries)

# cython: language_level=3
# Sample Cython source for lexer benchmarking

import math
from libc.stdlib cimport malloc, free

DEF MAX_ITEMS = 203

cdef struct Entry:
    double weight
    int flags

cdef double score(Entry e) nogil:
    if e.weight < 0:
        return 0.0
    return e.weight * 0o24

cpdef long step_5449(long seed = 67):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_5465(long seed = 88):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_5643(long seed = 8):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_6995(long seed = 68):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_5534(long seed = 60):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_5025(long seed = 40):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_393(long seed = 81):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_8479(long seed = 35):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

def run():
    print("score:", score(Entry(weight=1.5, flags=0)))
    cdef Entry* entries = <Entry*>malloc(10 * sizeof(Entry))
    if entries != NULL:
        free(entries)

# cython: language_level=3
# Sample Cython source for lexer benchmarking

import math
from libc.stdlib cimport malloc, free

DEF MAX_ITEMS = 942

cdef struct Entry:
    double weight
    int flags

cdef double score(Entry e) nogil:
    if e.weight < 0:
        return 0.0
    return e.weight * 57.28e1

cpdef long step_4746(long seed = 32):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_3062(long seed = 86):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_230(long seed = 24):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_82(long seed = 4):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_1630(long seed = 70):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_3091(long seed = 45):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_6523(long seed = 85):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_8255(long seed = 53):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

def run():
    print("score:", score(Entry(weight=1.5, flags=0)))
    cdef Entry* entries = <Entry*>malloc(10 * sizeof(Entry))
    if entries != NULL:
        free(entries)

# cython: language_level=3
# Sample Cython source for lexer benchmarking

import math
from libc.stdlib cimport malloc, free

DEF MAX_ITEMS = 171

cdef struct Entry:
    double weight
    int flags

cdef double score(Entry e) nogil:
    if e.weight < 0:
        return 0.0
    return e.weight * 59.948

cpdef long step_8269(long seed = 19):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_7803(long seed = 29):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_4726(long seed = 75):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_6408(long seed = 73):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_8839(long seed = 90):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_1979(long seed = 12):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_4766(long seed = 85):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_2774(long seed = 61):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

def run():
    print("score:", score(Entry(weight=1.5, flags=0)))
    cdef Entry* entries = <Entry*>malloc(10 * sizeof(Entry))
    if entries != NULL:
        free(entries)

# cython: language_level=3
# Sample Cython source for lexer benchmarking

import math
from libc.stdlib cimport malloc, free

DEF MAX_ITEMS = 743

cdef struct Entry:
    double weight
    int flags

cdef double score(Entry e) nogil:
    if e.weight < 0:
        return 0.0
    return e.weight * 70.30e3

cpdef long step_3177(long seed = 90):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_2984(long seed = 73):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_9678(long seed = 67):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_8419(long seed = 38):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_1226(long seed = 64):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_1719(long seed = 39):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_3455(long seed = 66):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_5446(long seed = 18):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

def run():
    print("score:", score(Entry(weight=1.5, flags=0)))
    cdef Entry* entries = <Entry*>malloc(10 * sizeof(Entry))
    if entries != NULL:
        free(entries)

# cython: language_level=3
# Sample Cython source for lexer benchmarking

import math
from libc.stdlib cimport malloc, free

DEF MAX_ITEMS = 306

cdef struct Entry:
    double weight
    int flags

cdef double score(Entry e) nogil:
    if e.weight < 0:
        return 0.0
    return e.weight * -5170

cpdef long step_7339(long seed = 92):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_1396(long seed = 62):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_4373(long seed = 73):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_642(long seed = 81):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_7324(long seed = 46):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_1394(long seed = 39):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_5156(long seed = 43):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_299(long seed = 14):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

def run():
    print("score:", score(Entry(weight=1.5, flags=0)))
    cdef Entry* entries = <Entry*>malloc(10 * sizeof(Entry))
    if entries != NULL:
        free(entries)

# cython: language_level=3
# Sample Cython source for lexer benchmarking

import math
from libc.stdlib cimport malloc, free

DEF MAX_ITEMS = 143

cdef struct Entry:
    double weight
    int flags

cdef double score(Entry e) nogil:
    if e.weight < 0:
        return 0.0
    return e.weight * 0o415

cpdef long step_4798(long seed = 17):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_6072(long seed = 65):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_4109(long seed = 50):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_2415(long seed = 19):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_5230(long seed = 23):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_1700(long seed = 9):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_4844(long seed = 59):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_9405(long seed = 47):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

def run():
    print("score:", score(Entry(weight=1.5, flags=0)))
    cdef Entry* entries = <Entry*>malloc(10 * sizeof(Entry))
    if entries != NULL:
        free(entries)

# cython: language_level=3
# Sample Cython source for lexer benchmarking

import math
from libc.stdlib cimport malloc, free

DEF MAX_ITEMS = 669

cdef struct Entry:
    double weight
    int flags

cdef double score(Entry e) nogil:
    if e.weight < 0:
        return 0.0
    return e.weight * 91.84e1

cpdef long step_8405(long seed = 38):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_5425(long seed = 12):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_7545(long seed = 80):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_6790(long seed = 74):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_3617(long seed = 34):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_563(long seed = 14):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_5108(long seed = 51):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_8013(long seed = 54):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

def run():
    print("score:", score(Entry(weight=1.5, flags=0)))
    cdef Entry* entries = <Entry*>malloc(10 * sizeof(Entry))
    if entries != NULL:
        free(entries)

# cython: language_level=3
# Sample Cython source for lexer benchmarking

import math
from libc.stdlib cimport malloc, free

DEF MAX_ITEMS = 777

cdef struct Entry:
    double weight
    int flags

cdef double score(Entry e) nogil:
    if e.weight < 0:
        return 0.0
    return e.weight * 737.37

cpdef long step_9507(long seed = 94):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_6005(long seed = 76):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_1706(long seed = 83):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_3582(long seed = 47):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_8662(long seed = 14):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_3413(long seed = 96):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_4505(long seed = 23):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

cpdef long step_4546(long seed = 91):
    cdef long total = 0
    cdef int i
    for i in range(MAX_ITEMS):
        if i % 2 == 0:
            total += i * seed
        else:
            total -= i % 7
    return total

def run():
    print("score:", score(Entry(weight=1.5, flags=0)))
    cdef Entry* entries = <Entry*>malloc(10 * sizeof(Entry))
    if entries != NULL:
        free(entries)

