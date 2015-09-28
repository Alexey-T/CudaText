#!/usr/bin/env python

from __future__ import print_function
import os.path
import sys
from optparse import OptionParser
from pyfiglet import Figlet
from subprocess import Popen, PIPE

__version__ = '0.1'


def dump(text):
    for line in text.split('\n'):
        print(repr(line))


def main():
    parser = OptionParser(version=__version__)

    parser.add_option('-s', '--show', action='store_true', default=False,
                      help='pause at each failure and compare output '
                           '(default: %default)')

    opts, args = parser.parse_args()

    f = Figlet()

    ok = 0
    fail = 0
    failed = []
    skip = ['runic']  # known bug..

    for font in f.getFonts():
        if font in skip:
            continue

        f.setFont(font=font)

        outputPyfiglet = f.renderText('foo')

        fontpath = os.path.join('pyfiglet', 'fonts', font)
        if os.path.isfile(fontpath + '.flf'):
            cmd = ('figlet', '-d', 'pyfiglet/fonts', '-f', font, 'foo')
        elif os.path.isfile(fontpath + '.tlf'):
            cmd = ('toilet', '-d', 'pyfiglet/fonts', '-f', font, 'foo')
        else:
            raise Exception('Missing font file: '+fontpath)

        p = Popen(cmd, bufsize=1, stdout=PIPE)
        outputFiglet = p.communicate()[0].decode('UTF-8')

        # Our TLF rendering isn't perfect, yet
        strict = os.path.isfile(fontpath + '.flf')
        if not strict:
            outputPyfiglet = outputPyfiglet.strip('\n')
            outputFiglet = outputFiglet.strip('\n')

        if outputPyfiglet == outputFiglet:
            print('[OK] %s' % font)
            ok += 1
            continue

        print('[FAIL] %s' % font)
        fail += 1
        failed.append(font)

        if opts.show is True:
            print('[PYTHON] *** %s\n\n' % font)
            dump(outputPyfiglet)
            print('[FIGLET] *** %s\n\n' % font)
            dump(outputFiglet)
            raw_input()

    print('OK = %d, FAIL = %d' % (ok, fail))
    if len(failed) > 0:
        print('FAILED = %s' % repr(failed))

    return 0


if __name__ == '__main__':
    sys.exit(main())
